(ns tolglow.studio.surfaces
  "Control surfaces, links and their session boxes (#705, #706). Studio sends
  the same :control-surface/* owner commands as the REPL and devices and reads
  the committed :control-surfaces projection."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [tolglow.studio.projection :as projection]))

(def ^:private ui-path [:ui :surfaces])

(defn- show-view [db] (get-in db [:server-projection :view]))

(defn- connected? [db] (and (get-in db [:connection :connected?]) (not (:awaiting-full? db))))

(defn surfaces-view [db] (:control-surfaces (show-view db)))

(defn- wire-id [value] (if (keyword? value) (subs (str value) 1) (str value)))

(defn on-surface
  "Cells shown by an attached member: {:groups {group-id label}
  :instances {wire instance-id label}}, labelled with the surface and strip.
  Instance keys use the text IDs of Studio's presented instances."
  [surfaces]
  (reduce (fn [found {:keys [surface-id cells]}]
            (reduce (fn [found {:keys [group-id instance-id position]}]
                      (let [label (str surface-id " " (inc (:x position)))]
                        (cond-> found
                          group-id (assoc-in [:groups group-id] label)
                          instance-id (assoc-in [:instances (wire-id instance-id)] label))))
                    found cells))
          {:groups {} :instances {}}
          (mapcat :members (:logical surfaces))))

(rf/reg-sub :surfaces/view (fn [db _] (surfaces-view db)))

(rf/reg-sub :surfaces/on-surface :<- [:surfaces/view] (fn [surfaces _] (on-surface surfaces)))

(rf/reg-sub :surfaces/status (fn [db _] (get-in db ui-path)))

(defn- command [db body]
  (let [id (str (random-uuid))]
    {:db (assoc-in db (conj ui-path :pending) id)
     :fx [[:studio/http {:path "/api/command" :typed? true :raw-response? true :timeout-ms 6500
                         :body (merge (projection/identity-of (show-view db))
                                      {:command-id id :source {:adapter :web :session-id (:session db)}}
                                      body)
                         :on-success [:surfaces/received id]
                         :on-failure [:surfaces/failed id]}]]}))

(rf/reg-event-fx :surfaces/bank
  (fn [{:keys [db]} [_ target viewport move]]
    (when (connected? db)
      (command db (cond-> {:op :control-surface/bank :target target :viewport viewport}
                    (keyword? move) (assoc :move move)
                    (map? move) (merge move))))))

(def link-choices
  "The Studio link form's choices: the shared :control-surface/link payload
  for a leader and one more surface."
  {"right" {:mode :extend :direction :right} "left" {:mode :extend :direction :left}
   "mirror" {:mode :mirror} "follow" {:mode :follow :offset {:x 1 :y 0}}})

(defn link-payload [link-id leader other choice]
  (let [{:keys [mode direction offset]} (get link-choices choice)]
    (cond-> {:op :control-surface/link :link-id link-id :mode mode
             :members [{:surface-id leader} (cond-> {:surface-id other} offset (assoc :offset offset))]}
      direction (assoc :direction direction))))

(rf/reg-event-fx :surfaces/link
  (fn [{:keys [db]} [_ link-id leader other choice]]
    (when (and (connected? db) (not (str/blank? link-id)) (not= leader other) (contains? link-choices choice))
      (command db (link-payload link-id leader other choice)))))

(rf/reg-event-fx :surfaces/unlink
  (fn [{:keys [db]} [_ link-id]]
    (when (connected? db) (command db {:op :control-surface/unlink :link-id link-id}))))

(rf/reg-event-fx :surfaces/follow
  ;; Studio selected a group or cue. With follow on, each logical surface whose
  ;; content holds it reveals it; the owner leaves a visible cell where it is.
  (fn [{:keys [db]} [_ selected]]
    (let [surfaces (surfaces-view db)
          ;; A presented instance ID resolves to the exact typed cue ID.
          cell-key (if (= :instance (first selected))
                     (some (fn [{:keys [id cue-id]}] (when (and cue-id (= (second selected) (wire-id id))) [:cue cue-id]))
                           (:instances (show-view db)))
                     selected)]
      (when (and cell-key (connected? db) (get-in surfaces [:settings :follow-selection?]))
        {:fx (vec (for [{:keys [id viewport]} (:logical surfaces)]
                    [:dispatch [:surfaces/reveal id viewport cell-key]]))}))))

(rf/reg-event-fx :surfaces/reveal
  (fn [{:keys [db]} [_ target viewport cell-key]]
    (when (connected? db)
      (assoc (command db {:op :control-surface/bank :target target :viewport viewport :reveal cell-key})
             :db (assoc-in db (conj ui-path :quiet) true)))))

(rf/reg-event-fx :surfaces/follow-toggle
  (fn [{:keys [db]} [_ enabled?]]
    (when (connected? db)
      {:fx [[:studio/http {:path "/api/settings" :timeout-ms 6000
                           :body {:scope "controllers"
                                  :values {:surfaces (assoc (get-in (show-view db) [:settings :controllers :surfaces] {})
                                                            :follow-selection? (boolean enabled?))}}
                           :on-success [:surfaces/received nil]
                           :on-failure [:surfaces/failed nil]}]]})))

(rf/reg-event-db :surfaces/received
  (fn [db [_ id _receipt]]
    (cond-> db
      (= id (get-in db (conj ui-path :pending))) (update-in ui-path dissoc :pending :error :quiet))))

(rf/reg-event-db :surfaces/failed
  (fn [db [_ id message]]
    (if (or (nil? id) (= id (get-in db (conj ui-path :pending))))
      (update-in db ui-path #(cond-> (dissoc % :pending :quiet)
                               ;; A follow reveal for a cell outside a surface is not an error to show.
                               (not (:quiet %)) (assoc :error message)))
      db)))

(defn range-label
  "Human strip range of a logical surface, such as \"Strips 1–16 of 20\"."
  [{:keys [window span size]}]
  (let [first-cell (inc (:x window)) last-cell (min (:columns size) (+ (:x window) (:columns span)))]
    (if (pos? (:columns size))
      (str "Strips " first-cell "–" (max first-cell last-cell) " of " (:columns size))
      "No strips")))

(defn- member-names [mode members]
  (str/join (if (= :mirror mode) " = " " · ")
            (map (fn [{:keys [surface-id attached?]}] (str surface-id (when-not attached? " (unavailable)"))) members)))

(defn session-box
  "Bank buttons and the strip range for one logical surface."
  [{:keys [id viewport mode window limit members] :as logical} disabled?]
  (let [bank #(rf/dispatch [:surfaces/bank id viewport %])]
    [:div.surface-box {:data-surface id :data-mode (name mode)}
     [:span.surface-box-name {:title (member-names mode members)}
      (str id (when (not= :single mode) (str " · " (name mode))))]
     [:span.surface-box-range {:role "status"} (range-label logical)]
     [:span.surface-box-buttons
      [:button.surface-box-button {:type "button" :aria-label (str "First bank on " id)
                                   :disabled (or disabled? (zero? (:x window))) :on-click #(bank :first)} "⏮"]
      [:button.surface-box-button {:type "button" :aria-label (str "Previous bank on " id)
                                   :disabled (or disabled? (zero? (:x window))) :on-click #(bank :previous)} "‹"]
      [:button.surface-box-button {:type "button" :aria-label (str "Next bank on " id)
                                   :disabled (or disabled? (>= (:x window) (:x limit))) :on-click #(bank :next)} "›"]
      [:button.surface-box-button {:type "button" :aria-label (str "Last bank on " id)
                                   :disabled (or disabled? (>= (:x window) (:x limit))) :on-click #(bank :last)} "⏭"]]]))

(defn- link-form [surfaces disabled?]
  (let [unlinked (keep (fn [{:keys [id link?]}] (when-not link? id)) (:logical surfaces))
        attached (set (keep #(when (:attached? %) (:id %)) (:surfaces surfaces)))
        choices (filterv attached unlinked)]
    (when (< 1 (count choices))
      [:form.surface-link-form
       {:on-submit (fn [event]
                     (.preventDefault event)
                     (let [fields (js/FormData. (.-currentTarget event))
                           first-id (.get fields "first") second-id (.get fields "second")
                           choice (.get fields "mode")]
                       (when (not= first-id second-id)
                         (rf/dispatch [:surfaces/link (str first-id "+" second-id) first-id second-id choice]))))}
       [:select {:name "first" :aria-label "First surface" :default-value (first choices)}
        (for [id choices] ^{:key id} [:option {:value id} id])]
       [:select {:name "mode" :aria-label "How to link" :default-value "right"}
        [:option {:value "right"} "extended to the right by"] [:option {:value "left"} "extended to the left by"]
        [:option {:value "mirror"} "mirrored by"] [:option {:value "follow"} "followed one page right by"]]
       [:select {:name "second" :aria-label "Second surface" :default-value (second choices)}
        (for [id choices] ^{:key id} [:option {:value id} id])]
       [:button.button {:type "submit" :disabled disabled?} "Link"]])))

(defn panel
  "Session boxes for every logical surface, links, and the follow setting."
  []
  (let [surfaces @(rf/subscribe [:surfaces/view])
        {:keys [pending error]} @(rf/subscribe [:surfaces/status])
        disabled? (boolean pending)]
    (when (seq (:surfaces surfaces))
      [:section.surface-panel {:aria-label "Control surfaces"}
       [:header.surface-panel-heading
        [:h3 "Surfaces"]
        [:label.checkbox-label
         [:input {:type "checkbox" :checked (boolean (get-in surfaces [:settings :follow-selection?]))
                  :on-change #(rf/dispatch [:surfaces/follow-toggle (.. % -target -checked)])}]
         "Follow selection"]]
       (when error [:p.surface-panel-error {:role "alert"} error])
       (for [logical (:logical surfaces)]
         ^{:key (pr-str [(:id logical) (:viewport logical)])}
         [:div.surface-panel-row
          [session-box logical disabled?]
          (when (:link? logical)
            [:button.surface-unlink {:type "button" :disabled disabled?
                                     :on-click #(rf/dispatch [:surfaces/unlink (:id logical)])} "Unlink"])])
       [link-form surfaces disabled?]])))
