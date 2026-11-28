(ns tolglow.studio.view-tabs
  "Views of one show in tabs (#669).

  Each tab holds one Studio view (Perform, Rig, Compose, …) of the same show.
  Tabs are a strip inside this one page, not separate pages or native windows,
  so every tab shares the page's single state connection, its preview engine
  and its Room window, and opening a tab never starts anything: no socket, no
  worker, no device and no output. Choosing a tab navigates to its view, and
  navigating (sidebar, shortcut, history) changes the active tab's view.

  Tabs belong to the device, like the panel layouts of #668. A browser keeps
  them in local storage. The Mac app's loopback port changes on every launch,
  which would give each launch an empty storage origin, so the app keeps them
  per show folder and passes them back in `window.tolglowStudioWindow`."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.page-routes :as pages]
            [tolglow.studio.show-window :as show-window]))

;; ---------------------------------------------------------------------------
;; Model (pure)

(def max-tabs 12)

(def max-closed 10)

(def ^:private links (into {} (map (juxt :route identity)) pages/navigation))

(defn view? [view] (contains? links view))

(defn label [view] (get-in links [view :label] (some-> view name)))

(defn short-label [view]
  (let [{:keys [label short-label]} (get links view)] (or short-label label (some-> view name))))

(defn icon [view] (get-in links [view :icon] :layout-grid))

(defn initial
  "One tab showing `view`."
  [view]
  {:tabs [{:id 1 :view (if (view? view) view :perform)}] :active 1 :next-id 2 :closed []})

(defn tab-id [id] (str "view-tab-" id))

(defn active-tab [{:keys [tabs active]}] (some #(when (= active (:id %)) %) tabs))

(defn active-view [state] (:view (active-tab state)))

(defn- index-of [tabs id] (first (keep-indexed (fn [i tab] (when (= id (:id tab)) i)) tabs)))

(defn normalize
  "Repair stored or received tab state: unknown views and duplicate or invalid
  IDs are dropped, the count is bounded and a missing active tab becomes the
  first. Nothing usable gives one tab on `fallback`."
  [state fallback]
  (let [tabs (->> (:tabs state)
                  (keep (fn [{:keys [id view]}]
                          (let [view (if (string? view) (keyword view) view)]
                            (when (and (pos-int? id) (view? view)) {:id id :view view}))))
                  (reduce (fn [[seen out] tab]
                            (if (seen (:id tab)) [seen out] [(conj seen (:id tab)) (conj out tab)]))
                          [#{} []])
                  second
                  (take max-tabs)
                  vec)]
    (if (empty? tabs)
      (initial fallback)
      (let [ids (set (map :id tabs))
            top (apply max ids)]
        {:tabs tabs
         :active (if (contains? ids (:active state)) (:active state) (:id (first tabs)))
         :next-id (max (inc top) (if (pos-int? (:next-id state)) (:next-id state) 0))
         :closed (->> (:closed state)
                      (keep #(let [view (if (string? %) (keyword %) %)] (when (view? view) view)))
                      (take-last max-closed) vec)}))))

(defn open
  "A new tab on `view` (the active tab's view when nil), after the active tab
  and selected. At the limit the state is returned unchanged."
  [state view]
  (let [{:keys [tabs active next-id]} state
        view (if (view? view) view (active-view state))]
    (if (>= (count tabs) max-tabs)
      state
      (let [at (inc (or (index-of tabs active) (dec (count tabs))))]
        (assoc state
               :tabs (into (conj (subvec tabs 0 at) {:id next-id :view view}) (subvec tabs at))
               :active next-id :next-id (inc next-id))))))

(defn close
  "Remove a tab, keeping at least one. Closing the active tab selects its right
  neighbour, or its left one when it was last. The view can be reopened."
  [state id]
  (let [{:keys [tabs active]} state
        at (index-of tabs id)]
    (if (or (nil? at) (<= (count tabs) 1))
      state
      (let [remaining (into (subvec tabs 0 at) (subvec tabs (inc at)))]
        (assoc state
               :tabs remaining
               :active (if (= id active) (:id (nth remaining (min at (dec (count remaining))))) active)
               :closed (vec (take-last max-closed (conj (:closed state []) (:view (nth tabs at))))))))))

(defn reopen
  "Reopen the most recently closed view in a new tab."
  [state]
  (if-let [view (peek (:closed state))]
    (let [opened (open state view)]
      (if (identical? opened state) state (update opened :closed pop)))
    state))

(defn select [state id]
  (if (index-of (:tabs state) id) (assoc state :active id) state))

(defn step
  "Select the tab `delta` places from the active one, wrapping."
  [{:keys [tabs active] :as state} delta]
  (let [n (count tabs)]
    (if (< n 2) state
        (assoc state :active (:id (nth tabs (mod (+ (or (index-of tabs active) 0) delta) n)))))))

(defn follow
  "The router reached `view`. Normally the active tab now shows it. On the
  first route of the page (`initial?`) a tab already showing that view is
  selected instead, so a deep link does not overwrite a restored tab."
  [state view initial?]
  (cond
    (not (view? view)) state
    (= view (active-view state)) state
    :else (if-let [existing (and initial? (some #(when (= view (:view %)) %) (:tabs state)))]
            (assoc state :active (:id existing))
            (update state :tabs (fn [tabs] (mapv #(if (= (:active state) (:id %)) (assoc % :view view) %) tabs))))))

(defn encode
  "Stored JSON for tabs. `saved-at` (epoch ms) lets a reader prefer the newer
  of the app's and the page's copies."
  [state & [saved-at]]
  (js/JSON.stringify
   (clj->js {:version 1 :savedAt (or saved-at 0) :active (:active state) :next-id (:next-id state)
             :tabs (mapv (fn [{:keys [id view]}] {:id id :view (name view)}) (:tabs state))
             :closed (mapv name (:closed state))})))

(defn decode
  "Stored tab state from its JSON text; anything unreadable is nil."
  [text]
  (try
    (let [{:keys [version tabs] :as data} (js->clj (js/JSON.parse text) :keywordize-keys true)]
      (when (and (map? data) (= 1 version) (vector? tabs))
        (update data :closed #(if (vector? %) % []))))
    (catch :default _ nil)))

;; ---------------------------------------------------------------------------
;; App-db

(defn state [db] (:view-tabs db))

(defn install
  "Install saved tabs (or one tab on the current view) into a fresh app-db."
  [db saved]
  (let [view (get-in db [:ui :view] :perform)]
    (assoc db :view-tabs (cond-> (normalize saved view) saved (assoc :restored? true)))))

(defn restored-view
  "The active tab's view when tabs were restored from this device."
  [db]
  (when (:restored? (state db)) (active-view (state db))))

(defn follow-route
  "Update the tabs for a route the router reached."
  [db view initial?]
  (cond-> db (state db) (update :view-tabs follow view initial?)))

(defn store-fx
  "Effect entry that saves the tabs and reports them to the Mac app."
  [db]
  (when-let [tabs (state db)] [:view-tabs/store tabs]))

;; ---------------------------------------------------------------------------
;; Effects. Storage and the host bridge stay here, outside app-db.

(def storage-key "tolglow.studio.view-tabs")

(defn- storage []
  (try (.-localStorage js/window) (catch :default _ nil)))

(rf/reg-cofx :view-tabs/saved
  (fn [cofx _]
    (assoc cofx :view-tabs/saved
           (some-> (show-window/newest-text (try (:tabs (show-window/host-config)) (catch :default _ nil))
                                            (try (some-> (storage) (.getItem storage-key)) (catch :default _ nil)))
                   decode))))

(rf/reg-fx :view-tabs/store
  (fn [tabs]
    (let [text (encode tabs (.now js/Date))]
      (try (some-> (storage) (.setItem storage-key text)) (catch :default _ nil))
      (show-window/post! {:type "tabs" :tabs text :count (count (:tabs tabs))
                          :view (some-> (active-view tabs) name) :label (label (active-view tabs))}))))

(defonce ^:private host-listener (atom nil))

(rf/reg-fx :view-tabs/listen
  (fn [_]
    ;; The Mac app's New Tab, Close Tab and tab-cycling menu items arrive as
    ;; `tolglow-view-tabs` events, like the Room window's placement replies.
    (when-let [previous @host-listener] (.removeEventListener js/window "tolglow-view-tabs" previous))
    (let [listener (fn [event]
                     (let [action (some-> event .-detail (aget "action"))]
                       (when (string? action) (rf/dispatch [:view-tabs/host action]))))]
      (reset! host-listener listener)
      (.addEventListener js/window "tolglow-view-tabs" listener))))

(rf/reg-fx :view-tabs/focus
  (fn [id]
    (r/after-render #(some-> (.getElementById js/document (tab-id id)) (.focus)))))

;; ---------------------------------------------------------------------------
;; Events

(defn- changed
  "Effects after a tab operation: save, and navigate when the view changed."
  [db & [focus?]]
  (let [view (active-view (state db))]
    {:db db
     :fx (cond-> [(store-fx db)]
           (not= view (get-in db [:ui :view])) (conj [:studio/navigate [:push view]])
           focus? (conj [:view-tabs/focus (:active (state db))]))}))

(defn- operate [db f & args]
  (if (state db) (changed (apply update db :view-tabs f args)) {:db db}))

(rf/reg-event-fx :view-tabs/select (fn [{:keys [db]} [_ id]] (operate db select id)))

(rf/reg-event-fx :view-tabs/open (fn [{:keys [db]} [_ view]] (operate db open view)))

(rf/reg-event-fx :view-tabs/close (fn [{:keys [db]} [_ id]] (operate db close id)))

(rf/reg-event-fx :view-tabs/reopen (fn [{:keys [db]} _] (operate db reopen)))

(rf/reg-event-fx :view-tabs/step
  (fn [{:keys [db]} [_ delta focus?]]
    (if (state db) (changed (update db :view-tabs step delta) focus?) {:db db})))

(rf/reg-event-fx :view-tabs/host
  (fn [{:keys [db]} [_ action]]
    (case action
      "new" (operate db open nil)
      "close" (operate db close (:active (state db)))
      "reopen" (operate db reopen)
      "next" (operate db step 1)
      "previous" (operate db step -1)
      ;; File › Show Folder and Checkpoints… (#742): the same dialog as Restore show.
      "checkpoints" {:dispatch [:checkpoints/open]}
      nil)))

;; ---------------------------------------------------------------------------
;; Views

(defn- key-down [tabs index event]
  (let [n (count tabs)
        target (case (.-key event)
                 "ArrowRight" (mod (inc index) n)
                 "ArrowLeft" (mod (dec index) n)
                 "Home" 0
                 "End" (dec n)
                 nil)]
    (cond
      target (do (.preventDefault event) (rf/dispatch [:view-tabs/step (- target index) true]))
      (and (= "Delete" (.-key event)) (> n 1))
      (do (.preventDefault event) (rf/dispatch [:view-tabs/close (:id (nth tabs index))])))))

(defn- title-text [db]
  (let [view (or (active-view (state db)) (get-in db [:ui :view]))
        show (or (get-in db [:window :show]) (get-in db [:show :rig :name]))]
    (str/join " · " (remove str/blank? [(label view) show "Tolglow Studio"]))))

(defn- title-publisher
  "Keep the document title on the active view and show, which is what a
  browser tab and the Mac window display."
  []
  (let [publish! (fn [this]
                   (let [[_ text] (r/argv this)]
                     (when (not= text (.-title js/document)) (set! (.-title js/document) text))))]
    (r/create-class
     {:display-name "view-tabs-title"
      :component-did-mount publish!
      :component-did-update publish!
      :reagent-render (fn [_] nil)})))

(defn panel-attributes
  "Attributes that make the main region the active tab's panel."
  [db]
  (when-let [active (:active (state db))]
    {:role "tabpanel" :aria-labelledby (tab-id active)}))

(defn strip
  "The tab strip above the active view. The tablist holds only its tabs; a
  tab's × is a pointer shortcut hidden from assistive technology, and the
  accessible Close tab button (and Delete in the tab list) sit outside it."
  [db]
  (let [{:keys [tabs active closed] :as tabs-state} (state db)
        several? (> (count tabs) 1)]
    (when tabs-state
      [:nav#view-tabs.view-tabs {:aria-label "Views of this show"}
       [title-publisher (title-text db)]
       [:div.view-tab-list {:role "tablist" :aria-label "Open views"}
        (doall
         (for [[index {:keys [id view]}] (map-indexed vector tabs)
               :let [selected? (= id active)]]
           ^{:key id}
           [:button.view-tab
            {:type "button" :role "tab" :id (tab-id id) :aria-selected selected?
             :class (when selected? "selected") :data-view (name view)
             :aria-controls "main" :tab-index (if selected? 0 -1) :title (label view)
             :aria-keyshortcuts (when several? "Delete")
             :on-click #(rf/dispatch [:view-tabs/select id])
             :on-key-down #(key-down tabs index %)}
            [:span.view-tab-icon {:aria-hidden true
                                  :style {"--nav-icon" (str "url('/studio/icons/lucide/" (name (icon view)) ".svg')")}}]
            [:span.view-tab-label (short-label view)]
            (when several?
              [:span.view-tab-close {:aria-hidden true :title "Close tab"
                                     :on-click (fn [event]
                                                 (.stopPropagation event)
                                                 (rf/dispatch [:view-tabs/close id]))}
               "×"])]))]
       [:button#view-tab-new.view-tab-action
        {:type "button" :aria-label "New tab" :title "New tab on this view"
         :disabled (>= (count tabs) max-tabs) :on-click #(rf/dispatch [:view-tabs/open nil])}
        [:span {:aria-hidden true} "+"]]
       (when several?
         [:button#view-tab-close.view-tab-action
          {:type "button" :aria-label (str "Close " (label (active-view tabs-state)) " tab") :title "Close tab"
           :on-click #(rf/dispatch [:view-tabs/close active])}
          [:span {:aria-hidden true} "×"]])
       (when (seq closed)
         [:button#view-tab-reopen.view-tab-action.text-button
          {:type "button" :title (str "Reopen " (label (peek closed)))
           :on-click #(rf/dispatch [:view-tabs/reopen])}
          "Reopen " (short-label (peek closed))])])))
