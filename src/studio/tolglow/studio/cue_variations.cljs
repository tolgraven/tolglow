(ns tolglow.studio.cue-variations
  "Studio controls for the shared running-cue variation grid and saved row."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [tolglow.studio.events :as events]
            [tolglow.studio.parameter-widgets :as widgets]
            [tolglow.transit :as transit]))

(defn- session [db]
  (select-keys (:show db) [:epoch :runtime-id]))

(defn- path [instance-id] [::controls instance-id])

(defn- current? [db instance-id token identity]
  (and (= identity (session db)) (= token (get-in db (conj (path instance-id) :token)))))

(rf/reg-sub ::state
  (fn [db [_ instance-id]]
    (let [state (get-in db (path instance-id))]
      (when (= (:identity state) (session db)) state))))

(rf/reg-event-fx ::load
  (fn [{:keys [db]} [_ instance-id layout row-page]]
    (let [token (str (random-uuid)) identity (session db)
          key (path instance-id)]
      {:db (assoc-in db key {:identity identity :token token :pending? true})
       :studio/http {:path "/api/cue-variations/preview" :typed? true :raw-response? true
                     :body (merge identity {:instance-id instance-id :layout layout :row-page row-page})
                     :on-success [::loaded instance-id token identity]
                     :on-failure [::failed instance-id token identity]}})))

(rf/reg-event-db ::loaded
  (fn [db [_ instance-id token identity view]]
    (if (current? db instance-id token identity)
      (update-in db (path instance-id) assoc :pending? false :view view)
      db)))

(rf/reg-event-db ::failed
  (fn [db [_ instance-id token identity message]]
    (if (current? db instance-id token identity)
      (update-in db (path instance-id) assoc :pending? false :error message)
      db)))

(rf/reg-event-fx ::commit
  (fn [{:keys [db]} [_ instance-id op fields]]
    (let [{:keys [view pending? identity]} (get-in db (path instance-id))]
      (when (and view (not pending?) (= identity (session db)))
        (let [token (str (random-uuid))
              basis (select-keys view [:basis :layout :seed :row-page])
              payload (merge identity basis fields {:op op :instance-id instance-id :command-id token})]
          {:db (update-in db (path instance-id) assoc :pending? true :error nil :token token)
           :studio/http {:path "/api/command" :typed? true :body payload
                         :on-success [::committed instance-id token identity]
                         :on-failure [::failed instance-id token identity]}})))))

(rf/reg-event-fx ::committed
  (fn [{:keys [db]} [_ instance-id token identity]]
    (when (current? db instance-id token identity)
      (let [{:keys [layout row-page]} (get-in db (conj (path instance-id) :view))]
        {:dispatch [::load instance-id layout row-page]}))))

(defn- value-text [value]
  (if (transit/fraction? value) (str value) (widgets/value-label {} value)))

(defn- choice-label [{:keys [param value params]}]
  (if param (value-text value)
      (str/join ", " (map (fn [[param value]] (str (widgets/label param) " " (value-text value)))
                         (sort-by key params)))))

(defn- ratio-entry [{:keys [param value]} pending? commit!]
  (let [[numerator denominator] (cond (transit/fraction? value) [(:numerator value) (:denominator value)]
                                      (and (integer? value) (pos? value)) [value 1])
        submit! (fn [event]
                  (.preventDefault event)
                  (let [fields (js/FormData. (.-currentTarget event))
                        n (js/Number (.get fields "numerator")) d (js/Number (.get fields "denominator"))]
                    (when (and (js/Number.isSafeInteger n) (js/Number.isSafeInteger d) (pos? n) (pos? d))
                      (commit! :cue-variation/ratio-set {:param param :numerator n :denominator d}))))]
    [:form.live-variation-actions {:on-submit submit! :aria-label (str (widgets/label param) " beat ratio")}
     [:strong (widgets/label param)]
     [:input {:type "number" :required true :min 1 :step 1 :name "numerator" :aria-label "Numerator"
              :default-value numerator :disabled pending?}]
     [:span "/"]
     [:input {:type "number" :required true :min 1 :step 1 :name "denominator" :aria-label "Denominator"
              :default-value denominator :disabled pending?}]
     [:button {:disabled pending? :type "submit"} "Apply ratio"]]))

(defn panel [instance]
  (let [instance-id (:id instance)
        {:keys [view pending? error]} @(rf/subscribe [::state instance-id])
        {:keys [layout rows row-page row-pages ratio-mode cells ratio-parameters]} view
        load! #(rf/dispatch [::load instance-id %1 %2])
        commit! #(rf/dispatch [::commit instance-id %1 %2])
        cell-at (into {} (map (juxt :position identity)) cells)
        row-name (into {} (map (juxt :y :param)) rows)]
    [:details.live-cue-variations
     {:on-toggle #(when (and (.. % -currentTarget -open) (nil? view) (not pending?)) (load! :full 0))}
     [:summary "Cue variations"]
     [:div.live-variation-actions
      [:button {:type "button" :disabled pending? :aria-pressed (= :full layout) :on-click #(load! :full 0)} "Full states"]
      [:button {:type "button" :disabled pending? :aria-pressed (= :rows layout) :on-click #(load! :rows 0)} "Parameter rows"]
      [:button {:type "button" :disabled pending? :on-click #(load! (or layout :full) (or row-page 0))} "Refresh"]
      (when view
        [:button {:type "button" :disabled pending?
                  :on-click #(commit! :cue-variation/ratio-mode {:mode (if (= :free ratio-mode) :preferred :free)})}
         (if (= :free ratio-mode) "Ratios · Free" "Ratios · Preferred")])]
     (when error [:p {:role "alert"} error])
     (when pending? [:p {:role "status"} "Updating cue variations"])
     (when view
       [:<>
        (for [{:keys [param value] :as ratio} ratio-parameters]
          ^{:key (str param value)} [ratio-entry ratio pending? commit!])
        (when (and (= :rows layout) (< 1 row-pages))
          [:div.live-variation-actions
           [:button {:type "button" :disabled (or pending? (zero? row-page)) :on-click #(load! :rows (dec row-page))} "Previous rows"]
           [:button {:type "button" :disabled (or pending? (>= (inc row-page) row-pages)) :on-click #(load! :rows (inc row-page))} "Next rows"]])
        [:div.live-variation-grid {:aria-label "Generated variations"}
         (for [y (range 7 0 -1) :when (some #(contains? cell-at [% y]) (range 8))]
           ^{:key y} [:div.live-variation-row
                      [:span (widgets/label (get row-name y))]
                      (for [x (range 8) :let [position [x y] cell (get cell-at position)]]
                        ^{:key x} [:button {:type "button" :disabled (or pending? (nil? cell))
                                            :title (choice-label cell)
                                            :on-click #(commit! :cue-variation/apply {:position position})}
                                   (when cell (if (= :rows layout) (choice-label cell) (inc (+ x (* 8 (dec y))))))])])]
        [:div.live-variation-actions
         [:strong "Saved states"]
         [:button {:type "button" :disabled pending? :on-click #(commit! :cue-variation/append {})} "Append current"]]
        [:div.live-variation-grid {:aria-label "Eight saved states"}
         [:div.live-variation-row
          [:span "Slot"]
          (for [slot (range 8) :let [position [slot 0] cell (get cell-at position)]]
            ^{:key slot} [:div.live-variation-actions
                          [:button {:type "button" :disabled (or pending? (nil? cell) (:stale? cell))
                                    :title (if (:stale? cell) "Saved state needs the earlier cue schema" (choice-label cell))
                                    :aria-label (str "Recall slot " (inc slot))
                                    :on-click #(commit! :cue-variation/apply {:position position})} (inc slot)]
                          (when cell
                            [:button {:type "button" :disabled pending? :aria-label (str "Overwrite slot " (inc slot))
                                      :on-click #(commit! :cue-variation/overwrite {:slot slot})} "Save"])])]]])]))
