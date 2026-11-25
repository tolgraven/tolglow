(ns tolglow.studio.operator-history
  (:require [clojure.string :as str]
            [reagent.core :as r]))

(defn- label [value]
  (cond
    (nil? value) "—"
    (keyword? value) (subs (str value) 1)
    (string? value) value
    :else (pr-str value)))

(defn- node-label [{:keys [parent-id parameter]}]
  (if (some? parent-id) (label parameter) "History start"))

(defn- scopes [{:keys [cue-id cue-label effect-id effect-label]}]
  (keep (fn [[kind id name]]
          (when-let [identity (or id name)]
            {:key [kind identity] :label (or name (label id))}))
        [[:cue cue-id cue-label] [:effect effect-id effect-label]]))

(defn- ancestor-ids [by-id ids]
  (loop [pending (seq ids), visited #{}]
    (if-let [[id & remaining] (seq pending)]
      (let [node (get by-id id)]
        (if (or (nil? node) (contains? visited id))
          (recur remaining visited)
          (recur (cond-> remaining (some? (:parent-id node)) (conj (:parent-id node)))
                 (conj visited id))))
      visited)))

(defn- branch-lanes [nodes children]
  (first
   (reduce (fn [[lanes next-lane] {:keys [id parent-id]}]
             (cond
               (nil? parent-id) [(assoc lanes id 0) next-lane]
               (= id (:id (first (get children parent-id))))
               [(assoc lanes id (get lanes parent-id 0)) next-lane]
               :else [(assoc lanes id next-lane) (inc next-lane)]))
           [{} 1] nodes)))

(def ^:private last-visible-lane 6)

(defn- guide-rows [nodes children lanes]
  (let [lane-of #(min last-visible-lane (get lanes (:id %) 0))]
    (loop [remaining (seq nodes), rails {}, rows []]
      (if-let [{:keys [id parent-id] :as node} (first remaining)]
        (let [lane (lane-of node)
              next-lanes (map lane-of (get children id))
              continuing (if (some? parent-id) (update rails lane (fnil dec 0)) rails)
              outgoing (into {} (filter (comp pos? val))
                             (reduce #(update %1 %2 (fnil inc 0)) continuing next-lanes))
              guides (keep (fn [track]
                             (let [above? (contains? rails track)
                                   below? (contains? outgoing track)]
                               (cond
                                 (and above? below?) [track "through"]
                                 above? [track "end"]
                                 below? [track "start"])))
                           (range (inc last-visible-lane)))
              fork-width (- (reduce max lane next-lanes) lane)]
          (recur (next remaining) outgoing
                 (conj rows (assoc node :lane lane :guides guides :fork-width fork-width))))
        rows))))

(defn outline
  "Order entries by creation sequence within each branch and preserve ancestor
  context in a cue or effect filter. Lanes depend on the full retained tree."
  [{:keys [nodes current-id]} filter-key]
  (let [ordered (sort-by (juxt :sequence :id) nodes)
        by-id (into {} (map (juxt :id identity)) ordered)
        scopes-by-id (into {} (map (juxt :id scopes)) ordered)
        filters (->> (map #(get scopes-by-id (:id %)) ordered)
                     (mapcat identity)
                     (reduce #(assoc %1 (:key %2) %2) {})
                     vals
                     (sort-by (juxt :label (comp pr-str :key)))
                     vec)
        filter-key (when (some #(= filter-key (:key %)) filters) filter-key)
        matches (into #{} (keep (fn [[id options]]
                                  (when (some #(= filter-key (:key %)) options) id)))
                      scopes-by-id)
        visible (if filter-key (ancestor-ids by-id matches) (set (keys by-id)))
        children (group-by :parent-id ordered)
        visible-children (group-by :parent-id (filter #(contains? visible (:id %)) ordered))
        nodes (mapcat (partial tree-seq #(seq (get visible-children (:id %)))
                               #(get visible-children (:id %)))
                      (get visible-children nil))
        rows (guide-rows nodes visible-children (branch-lanes ordered children))
        ids (mapv :id rows)]
    {:rows rows :ids ids :indices (zipmap ids (range)) :by-id by-id
     :children visible-children :applied (ancestor-ids by-id [current-id])
     :filter-key filter-key :matches matches :filters filters}))

(defn navigation-target [{:keys [ids indices by-id children]} id key]
  (let [index (get indices id)]
    (when (some? index)
      (case key
        "ArrowUp" (get ids (dec index))
        "ArrowDown" (get ids (inc index))
        "ArrowLeft" (:parent-id (get by-id id))
        "ArrowRight" (:id (first (get children id)))
        "Home" (first ids)
        "End" (peek ids)
        nil))))

(defn update-interaction
  "Store this reducer's result in app-db. Identity should change on a show or
  runtime replacement so a reused node number cannot inherit an old preview."
  [state projection [action value]]
  (let [{:keys [identity current-id position]} projection
        state (if (= identity (:identity state)) state {:identity identity})]
    (case action
      :focus (assoc state :focus-id value)
      :preview (assoc (dissoc state :conflict :message :error?) :focus-id value
                      :preview {:target-id value :expected-cursor current-id :expected-history-position position})
      :clear-preview (dissoc state :preview :conflict)
      :filter (-> state (assoc :filter-key value) (dissoc :focus-id :preview :conflict))
      state)))

(defn- apply-block [{:keys [identity current-id position]} state {:keys [pending? connection-message conflict]} by-id]
  (let [{:keys [target-id expected-cursor expected-history-position]} (:preview state)
        target (get by-id target-id)]
    (cond
      pending? "Applying history…"
      connection-message connection-message
      (string? conflict) conflict
      conflict "The live show changed. Refresh the preview before applying."
      (nil? target) "This step is no longer in the retained history."
      (:unavailable-reason target) (:unavailable-reason target)
      (not= identity (:identity state)) "The show changed. Select a new step."
      (or (not= current-id expected-cursor) (not= position expected-history-position))
      "The live history changed. Select this step again to refresh the preview."
      (not (contains? by-id current-id)) "The current step is unavailable."
      (= current-id target-id) "This is the current step.")))

(defn apply-request
  "Return the guarded request for an explicit Apply. The runtime still owns
  command validation and can reject a request when its live state has changed."
  [projection state status by-id]
  (when (and (:preview state) (nil? (apply-block projection state status by-id)))
    (:preview state)))

(defn commands
  "Guard each action with the cursor and history position the operator saw. A fork needs an
  explicitly previewed direct child before Redo can choose a branch."
  [{:keys [current-id position nodes] :as projection} state status]
  (let [by-id (into {} (map (juxt :id identity)) nodes)
        current (get by-id current-id)
        children (filterv #(= current-id (:parent-id %)) nodes)
        preview (apply-request projection state status by-id)
        ready? (and (:identity projection) current (some? position)
                    (not (:pending? status)) (not (:connection-message status)))
        redo-id (if (= 1 (count children))
                  (:id (first children))
                  (some #(when (= (:target-id preview) (:id %)) (:id %)) children))
        request (fn [id]
                  (when (and ready? (contains? by-id id) (nil? (get-in by-id [id :unavailable-reason])))
                    {:identity (:identity projection) :expected-cursor current-id
                     :expected-history-position position :target-id id}))]
    {:history/undo (request (:parent-id current))
     :history/redo (request redo-id)
     :history/select (request (:target-id preview))
     :choose-redo? (and ready? (> (count children) 1) (nil? redo-id))}))

(defn- reveal! [scroll row]
  (when (and scroll row)
    (let [viewport (.getBoundingClientRect scroll)
          bounds (.getBoundingClientRect row)]
      (when (or (< (.-top bounds) (.-top viewport)) (> (.-bottom bounds) (.-bottom viewport)))
        (set! (.-scrollTop scroll)
              (+ (.-scrollTop scroll) (- (.-top bounds) (.-top viewport))
                 (/ (- (.-height bounds) (.-height viewport)) 2)))))))

(defn- navigate! [event model id]
  (let [key (.-key event)]
    (when (and (contains? #{"ArrowUp" "ArrowDown" "ArrowLeft" "ArrowRight" "Home" "End"} key)
               (not (or (.-altKey event) (.-ctrlKey event) (.-metaKey event))))
      (.preventDefault event)
      (when-let [target (navigation-target model id key)]
        (let [scroll (.closest (.-currentTarget event) ".operator-history-scroll")]
          (when-let [row (.querySelector scroll (str "[data-history-node='" target "']"))]
            (.focus row #js {:preventScroll true})
            (reveal! scroll row)))))))

(defn- gutter [{:keys [lane guides fork-width]}]
  [:span.operator-history-gutter {:aria-hidden true}
   (for [[track extent] guides]
     ^{:key track}
     [:span.operator-history-rail {:data-extent extent :style {"--history-lane" track}}])
   (when (pos? fork-width)
     [:span.operator-history-fork {:style {"--history-lane" lane "--history-fork-width" fork-width}}])
   [:span.operator-history-dot {:style {"--history-lane" lane}}]])

(defn- row [model node current-id focus-id preview-id dispatch-local!]
  (let [{:keys [id parent-id actor cue-label effect-label unavailable-reason]} node
        current? (= id current-id)
        preview? (= id preview-id)
        context? (and (:filter-key model) (not (contains? (:matches model) id)))
        parent (get-in model [:by-id parent-id])
        targets (distinct (remove nil? [cue-label effect-label]))
        status (cond
                 current? "Current"
                 unavailable-reason "Unavailable"
                 (contains? (:applied model) id) "Applied"
                 :else "Alternate")]
    [:li.operator-history-entry
     [:button.operator-history-row {:type "button"
                                    :data-history-node id
                                    :data-state status
                                    :aria-current (when current? "true")
                                    :aria-pressed preview?
                                    :tab-index (if (= id focus-id) 0 -1)
                                    :on-focus #(dispatch-local! [:focus id])
                                    :on-click #(dispatch-local! [:preview id])
                                    :on-key-down #(navigate! % model id)}
      [gutter node]
      [:span.operator-history-entry-copy
       [:strong (node-label node)]
       (when (or actor (seq targets))
         [:span.operator-history-meta (str/join " · " (cond-> (vec targets) actor (conj (label actor))))])
       (when parent
         [:span.sr-only (str "Step " id ", after " (node-label parent) ", step " parent-id)])]
      [:span.operator-history-states
       [:span status]
       (when context? [:span "Context"])
       (when preview? [:strong "Preview"])]]]))

(defn- filter-select [{:keys [filter-key filters]} dispatch-local!]
  (when (seq filters)
    (let [indexed (map-indexed (fn [i option] (assoc option :value (str i))) filters)]
      [:label.operator-history-filter "Filter history"
       [:select {:value (or (some #(when (= filter-key (:key %)) (:value %)) indexed) "")
                 :on-change #(let [value (.. % -target -value)]
                               (dispatch-local! [:filter (some (fn [option]
                                                               (when (= value (:value option)) (:key option)))
                                                             indexed)]))}
        [:option {:value ""} "All edits"]
        (for [[kind group] [[:cue "Cues"] [:effect "Effects"]]
              :let [options (filter #(= kind (first (:key %))) indexed)
                    counts (frequencies (map :label options))]
              :when (seq options)]
          ^{:key kind} [:optgroup {:label group}
                        (for [{:keys [key value] name :label} options]
                          ^{:key (pr-str key)}
                          [:option {:value value}
                           (if (> (get counts name) 1) (str name " · " (label (second key))) name)])])]])))

(defn- clear-preview! [event dispatch-local!]
  (let [panel (.closest (.-currentTarget event) ".operator-history")]
    (when-let [row (.querySelector panel "[data-history-node][tabindex='0']")]
      (.focus row #js {:preventScroll true})))
  (dispatch-local! [:clear-preview]))

(defn- details [{:keys [projection view-state apply-status command! dispatch-local!]} by-id request]
  (let [preview (:preview view-state)
        target (get by-id (:target-id preview))
        message (when (and preview (nil? (:message apply-status))
                           (not (or (:pending? apply-status) (:connection-message apply-status))))
                  (apply-block projection view-state apply-status by-id))]
    [:aside.operator-history-details {:aria-label "History preview"}
     [:h3 (if target (str "Preview · " (node-label target)) "History preview")]
     (cond
       (some? (:parent-id target))
       [:dl.operator-history-values
        [:div [:dt "Before edit"] [:dd (label (:before target))]]
        [:div [:dt "After edit"] [:dd (label (:after target))]]]
       target [:p "Starting point of the retained history."]
       (nil? preview) [:p "Select a step to inspect its recorded change."])
     [:div.operator-history-warning {:role "status"} message]
     (when preview
       [:div.operator-history-actions
        [:button.button {:type "button" :on-click #(clear-preview! % dispatch-local!)} "Clear preview"]
        [:button.button.primary {:type "button" :disabled (nil? request)
                                 :on-click #(command! :history/select request)}
         "Apply to show"]])]))

(defn- close! [event]
  (.close (.closest (.-currentTarget event) "dialog")))

(defn panel
  [{:keys [projection view-state apply-status dispatch-local! command! on-close] :as props}]
  (r/with-let [scroll (volatile! nil)
               revealed (volatile! nil)
               show! (fn [node] (when (and node (not (.-open node))) (.showModal node)))]
    (let [{:keys [identity current-id retention]} projection
          view-state (when (= identity (:identity view-state)) view-state)
          {:keys [rows by-id indices filter-key] :as model} (outline projection (:filter-key view-state))
          {:history/keys [undo redo select] :keys [choose-redo?]} (commands projection view-state apply-status)
          focus-id (or (some #(when (contains? indices %) %) [(:focus-id view-state) current-id])
                       (:id (first rows)))
          preview-id (get-in view-state [:preview :target-id])
          reveal-key [identity filter-key focus-id]
          message (cond
                    (:connection-message apply-status) (:connection-message apply-status)
                    (:pending? apply-status) "Applying history…"
                    (:message apply-status) (:message apply-status)
                    choose-redo? "Preview a child of the current step to choose a redo branch.")]
      (when (not= reveal-key @revealed)
        (vreset! revealed reveal-key)
        (r/after-render #(when-let [element @scroll]
                           (reveal! element (.querySelector element "[data-history-node][tabindex='0']")))))
      [:dialog.operator-history {:aria-labelledby "operator-history-title" :ref show! :on-close on-close}
       [:header.operator-history-header
        [:div
         [:h2#operator-history-title "Operator history"]
         (when-let [{:keys [retained limit pruned?]} retention]
           [:p.operator-history-retention (str retained " / " limit " retained steps"
                                              (when pruned? " · Older steps pruned"))])]
        [:button.icon-button.operator-history-close {:type "button" :aria-label "Close operator history"
                                                      :on-click close!} "×"]]
       [:div.operator-history-toolbar
        [:div.operator-history-navigation {:role "group" :aria-label "History commands"}
         [:button.button {:type "button" :disabled (nil? undo) :on-click #(command! :history/undo undo)} "Undo"]
         [:button.button {:type "button" :disabled (nil? redo) :on-click #(command! :history/redo redo)} "Redo"]]
        [filter-select model dispatch-local!]]
       [:div.operator-history-feedback {:role "status" :data-error (boolean (:error? apply-status))} message]
       (if (seq rows)
         [:div.operator-history-body
          [:div.operator-history-scroll {:ref #(vreset! scroll %)}
           [:ol.operator-history-outline {:role "list" :aria-label "History steps"}
            (for [{:keys [id] :as node} rows]
              ^{:key id} [row model node current-id focus-id preview-id dispatch-local!])]]
          [details (assoc props :view-state view-state) by-id select]]
         [:p.operator-history-empty (if projection "No operator history recorded." "History is unavailable.")])])))
