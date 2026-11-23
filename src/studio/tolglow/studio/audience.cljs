(ns tolglow.studio.audience
  "Operate the enrolled audience zones. The existing Connections stream owns all telemetry."
  (:require [re-frame.core :as rf]
            [tolglow.studio.events :as events]))

(rf/reg-sub ::mix
  (fn [db _] (get-in db [:server-projection :view :connection-status :audience])))

(defn- session [db]
  [(get-in db [:show :runtime-id]) (get-in db [:show :epoch]) (get-in db [:connections :generation])])

(rf/reg-sub ::operations
  (fn [db _] (get-in db [:connections :audience-operations (session db)])))

(rf/reg-event-fx ::change
  (fn [{:keys [db]} [_ id path body]]
    (let [token (session db)]
      {:db (assoc-in db [:connections :audience-operations token id] body)
       :studio/http {:path path :body (assoc body :device-id id) :typed? true
                     :on-success [::complete token id] :on-failure [::failed token id]}})))

(rf/reg-event-db ::complete
  (fn [db [_ token id _]]
    (if (= token (session db))
      (update-in db [:connections :audience-operations token] dissoc id)
      db)))

(rf/reg-event-db ::failed
  (fn [db [_ token id error]]
    (if (= token (session db))
      (assoc-in db [:connections :audience-operations token id] (events/error-message error))
      db)))

(defn- select! [id included?]
  (rf/dispatch [::change id "/api/sensors/audience-zone" {:included? included?}]))

(defn- arm! [id session-id armed?]
  (rf/dispatch [::change id "/api/sensors/arm" {:session-id session-id :armed? armed?}]))

(defn- level [title value]
  [:div.audience-level [:dt title] [:dd [:strong value]]])

(defn- crowd-levels [{:keys [audience-count dance-effort pulse-count]}]
  (let [people (or audience-count 0)
        wearers (or pulse-count 0)
        effort (js/Math.round (* 100 (or dance-effort 0)))]
    [:dl.audience-levels
     [level "Dancers" people]
     [:div.audience-level [:dt "Dance effort"]
      [:dd [:strong (str effort "%")]
       [:meter {:aria-label "Crowd dance effort" :value effort :min 0 :max 100}]]]
     [level "Pulse sharing" (str wearers " / " people)]]))

(def status-label {:waiting "Waiting for gateway" :ready "Joined" :armed "Armed"
                   :expired "Signal expired" :unavailable "Unavailable"})

(defn- zone-row [{:keys [id label status session-id]} selected? operation]
  (let [pending? (map? operation)
        checked? (if (and pending? (contains? operation :included?)) (:included? operation) selected?)]
    [:li.audience-zone {:data-zone id :data-status (name status)}
     [:label.audience-inclusion
      [:input {:type "checkbox" :checked checked? :disabled (or pending? (and (= :unavailable status) (not selected?)))
               :aria-label (str "Include " label " in audience mix")
               :on-change #(select! id (not checked?))}]
      [:span [:strong label] [:small (get status-label status)]]]
     (when (#{:armed :ready} status)
       [:button.button {:type "button" :disabled pending? :on-click #(arm! id session-id (= :ready status))
                        :aria-label (str (if (= :armed status) "Disarm " "Arm ") label)}
        (if (= :armed status) "Disarm" "Arm")])
     (when (string? operation) [:p.connection-error {:role "alert"} operation])]))

(defn panel []
  (let [{:keys [gateways levels zones] :as mix} @(rf/subscribe [::mix])
        operations @(rf/subscribe [::operations])
        selected (set gateways)]
    [:section#audience-zones.panel {:aria-labelledby "audience-heading"}
     [:div.audience-heading [:h2#audience-heading "Audience"]
      (when (seq zones) [:span.small.muted (str (count selected) " selected")])]
     (cond
       (nil? mix) [:p.audience-empty "Connecting to audience sensors…"]
       (empty? zones) [:p.audience-empty "Enroll a gateway with audience-count and dance-effort channels to add a zone."]
       :else [:<>
              [crowd-levels levels]
              [:ul.audience-zones {:aria-label "Enrolled audience zones"}
               (for [{:keys [id] :as zone} zones]
                 ^{:key id} [zone-row zone (contains? selected id) (get operations id)])]])]))
