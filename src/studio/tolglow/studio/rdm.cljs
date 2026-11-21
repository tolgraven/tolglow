(ns tolglow.studio.rdm
  "RDM fixture management through guarded re-frame actions and owned host jobs."
  (:require [clojure.string :as str] [re-frame.core :as rf] [reagent.core :as r]
            [tolglow.studio.events]))

(def defaults {:identity nil :generation 0 :busy? false :snapshot {} :error nil :notice nil
               :host "127.0.0.1" :port "9010" :universe "1" :uid "" :sub-device "0"
               :fixture-id "" :address nil :personality nil :label nil :pid "0x0060" :bytes "" :command "get"})
(defn identity-of [db] (select-keys (:show db) [:runtime-id :epoch]))
(defn state [db]
  (let [saved (merge defaults (get-in db [:ui :rdm]))]
    (if (= (:identity saved) (identity-of db)) saved (assoc defaults :identity (identity-of db)))))
(defn current-request? [db token]
  (= token [(:identity (state db)) (:generation (state db))]))
(defn lookup [m id] (or (get m id) (get m (keyword (str id))) (when (re-matches #"\d+" (str id)) (get m (js/Number id)))))
(defn key-text [value] (if (keyword? value) (name value) (str value)))
(defn device [s] {:universe (js/Number (:universe s)) :uid (:uid s) :sub-device (js/Number (:sub-device s))})
(defn device-key [s] (str (:universe s) "/" (str/lower-case (:uid s)) "/" (:sub-device s)))
(defn observation [s] (lookup (get-in s [:snapshot :observations]) (device-key s)))
(defn hex-bytes [text]
  (let [parts (if (str/blank? text) [] (str/split (str/trim text) #"\s+"))]
    (when-not (and (<= (count parts) 231) (every? #(re-matches #"[0-9a-fA-F]{2}" %) parts))
      (throw (js/Error. "Use up to 231 hex bytes, separated by spaces: 00 ff")))
    (mapv #(js/parseInt % 16) parts)))

(defn request-fx [db operation payload]
  (let [s (-> (state db) (update :generation inc) (assoc :busy? true :error nil))
        token [(:identity s) (:generation s)]
        body (when (not= :load operation)
               (merge {:runtime-id (get-in db [:show :runtime-id]) :expected-epoch (get-in db [:show :epoch])
                       :session-id (get-in s [:snapshot :session-id])} payload))]
    {:db (assoc-in db [:ui :rdm] s)
     :studio/http (cond-> {:path (if (= :load operation) "/api/rdm" (str "/api/rdm/" (name operation)))
                           :on-success [:rdm/reply token operation] :on-failure [:rdm/failed token]}
                    body (assoc :body body))}))

(rf/reg-sub :rdm/state (fn [db _] (state db)))
(rf/reg-event-db :rdm/set
  (fn [db [_ key value]]
    (assoc-in db [:ui :rdm] (cond-> (assoc (state db) key value)
                            (#{:host :port} key) (assoc :config-loaded? true)))))
(rf/reg-event-fx :rdm/load
  (fn [{:keys [db]} _]
    (when (and (get-in db [:show :runtime-id]) (not (:busy? (state db)))) (request-fx db :load nil))))
(rf/reg-event-fx :rdm/action
  (fn [{:keys [db]} [_ operation payload]]
    (when-not (:busy? (state db)) (request-fx db operation payload))))
(rf/reg-event-fx :rdm/reply
  (fn [{:keys [db]} [_ token operation result]]
    (when (current-request? db token)
      (let [s (assoc (state db) :busy? false)]
        (if (= :load operation)
          {:db (assoc-in db [:ui :rdm] (cond-> (assoc s :snapshot result)
                                        (not (:config-loaded? s))
                                        (assoc :host (get-in result [:config :host] "127.0.0.1")
                                               :port (str (get-in result [:config :port] 9010)) :config-loaded? true)))}
          {:db (assoc-in db [:ui :rdm] (assoc s :notice (case operation
                                                        :connect "RDM session enabled. Discovery and device requests are explicit."
                                                        :adopt "Observed address adopted into the show patch."
                                                        :associate "Fixture linked. Its show patch is unchanged."
                                                        :disconnect "RDM session disabled."
                                                        (if (:job-id result) "Device job queued." "Saved."))))
           :dispatch [:rdm/load]})))))
(rf/reg-event-db :rdm/failed
  (fn [db [_ token error]]
    (if (current-request? db token) (assoc-in db [:ui :rdm] (assoc (state db) :busy? false :error (str error))) db)))
(rf/reg-event-db :rdm/error (fn [db [_ error]] (assoc-in db [:ui :rdm] (assoc (state db) :error error))))
(rf/reg-event-db :rdm/select-fixture
  (fn [db [_ fixture]]
    (let [s (state db) binding (lookup (get-in s [:snapshot :bindings]) (:id fixture))]
      (assoc-in db [:ui :rdm] (merge s {:fixture-id (:id fixture) :universe (str (:universe fixture))
                                      :uid (:uid binding "") :sub-device (str (:sub-device binding 0))
                                      :address nil :personality nil :label nil})))))

(defn- input! [key event] (rf/dispatch-sync [:rdm/set key (.. event -target -value)]))
(defn- action! [operation payload] (rf/dispatch [:rdm/action operation payload]))
(defn- command! [s parameter value] (action! :command (merge (device s) {:command :set :parameter parameter :value value})))
(defn- field [s key title type]
  [:label title [:input {:aria-label title :type type :value (get s key "") :on-change #(input! key %)}]])
(defn- value-view [entry]
  (if (= "ack" (:status entry))
    (let [value (:value entry)] (if (or (map? value) (vector? value)) (pr-str value) (str value)))
    (or (:reason entry) (:status entry) "Not read")))

(defn panel []
  (r/with-let [_ (rf/dispatch [:rdm/load])
               timer (js/setInterval #(when-not (.-hidden js/document) (rf/dispatch [:rdm/load])) 1000)]
    (let [s @(rf/subscribe [:rdm/state]) snapshot (:snapshot s) observation (observation s)
          info (get-in observation [:fields :device-info :value])
          enabled? (:connected? snapshot) disabled? (or (:busy? s) (not enabled?))
          discovered (lookup (:discovered snapshot) (:universe s))]
      [:section.panel.rdm-panel {:aria-label "RDM fixture management"}
       [:div.panel-top [:span.section-label "RIG · RDM FIXTURES"]
        [:span.rdm-badge (if enabled? "SESSION ENABLED" "OFFLINE")]]
       [:div.rdm-intro [:h2 "Know every fixture."]
        [:p "Discover device identities, inspect configuration, and send RDM controls through OLA. Saved observations remain available offline."]]
       [:div.rdm-connection
        [field s :host "OLA host" "text"] [field s :port "OLA TCP port" "number"]
        [:button.button {:disabled (:busy? s) :on-click #(if enabled? (action! :disconnect {})
                                                         (action! :connect {:config {:host (:host s) :port (js/Number (:port s))}}))}
         (if enabled? "Disable RDM" "Enable RDM")]]
       (when (:error s) [:p.rdm-error {:role "alert"} (:error s)])
       (when (:notice s) [:p.rdm-notice {:role "status"} (:notice s)])
       [:div.rdm-columns
        [:div.rdm-show-fixtures [:h3 "Show fixtures"]
         [:p.small.muted "Select a fixture to link its RDM identity. Linking does not move its DMX patch."]
         (for [fixture (:fixtures snapshot)]
           ^{:key (:id fixture)}
           [:button.rdm-fixture {:class (when (= (:id fixture) (:fixture-id s)) "selected")
                                 :on-click #(rf/dispatch [:rdm/select-fixture fixture])}
            [:strong (:name fixture (:id fixture))]
            [:span (str "U" (:universe fixture) " · " (:address fixture))]
            [:small (or (:uid (lookup (:bindings snapshot) (:id fixture))) "Not linked")]])]
        [:div.rdm-device
         [:h3 "Discover & inspect"]
         [:div.rdm-fields [field s :universe "RDM universe" "number"]
          [:button.button {:disabled disabled? :on-click #(action! :discover {:universe (js/Number (:universe s)) :full? true})} "Discover"]]
         (when (seq discovered)
           [:label "Discovered device"
            [:select {:aria-label "Discovered device" :value (:uid s) :on-change #(input! :uid %)}
             [:option {:value ""} "Choose a device"]
             (for [uid discovered] ^{:key uid} [:option {:value uid} uid])]])
         [:div.rdm-fields [field s :uid "Device UID" "text"] [field s :sub-device "Subdevice" "number"]]
         [:div.rdm-actions
          [:button.button {:disabled (or disabled? (str/blank? (:uid s))) :on-click #(action! :read (device s))} "Read device"]
          [:button.button {:disabled (or (:busy? s) (str/blank? (:fixture-id s)) (str/blank? (:uid s)))
                           :on-click #(action! :associate {:fixture-id (:fixture-id s) :device (device s)})} "Link selected fixture"]
          [:button.text-button {:disabled (or (:busy? s) (str/blank? (:fixture-id s)))
                                :on-click #(action! :unassociate {:fixture-id (:fixture-id s)})} "Unlink"]]
         (when observation
           [:div.rdm-readback
            [:h3 "Observed configuration"]
            [:dl (for [[title key] [["Manufacturer" :manufacturer-label] ["Model" :model-description]
                                    ["Device label" :device-label] ["Software" :software-label]]]
                   ^{:key key} [:div [:dt title] [:dd [value-view (get-in observation [:fields key])]]])
             [:div [:dt "Footprint / subdevices"] [:dd (str (:footprint info "—") " slots / " (:sub-device-count info "—"))]]
             [:div [:dt "Physical address / personality"] [:dd (str (:address info "—") " / " (:personality info "—"))]]]
            [:div.rdm-fields
             [field (assoc s :address (or (:address s) (str (:address info "")))) :address "New DMX address" "number"]
             [:button.button {:disabled disabled? :on-click #(command! s :address (js/Number (or (:address s) (:address info))))} "Set device address"]]
            [:div.rdm-fields
             [:label "Device personality"
              [:select {:aria-label "Device personality" :value (or (:personality s) (str (:personality info "")))
                        :on-change #(input! :personality %)}
               (for [[number entry] (:personalities observation)]
                 ^{:key (str number)} [:option {:value (key-text number)}
                                      (str (key-text number) " · " (get-in entry [:value :name] "Unavailable")
                                           " · " (get-in entry [:value :footprint] "?") " slots")])]]
             [:button.button {:disabled disabled? :on-click #(command! s :personality (js/Number (or (:personality s) (:personality info))))} "Set personality"]]
            [:div.rdm-fields
             [field (assoc s :label (or (:label s) (get-in observation [:fields :device-label :value] ""))) :label "New device label" "text"]
             [:button.button {:disabled disabled? :on-click #(command! s :device-label (or (:label s) (get-in observation [:fields :device-label :value] "")))} "Set label"]]
            [:div.rdm-actions
             [:button.button {:disabled disabled? :on-click #(command! s :identify true)} "Identify on"]
             [:button.button {:disabled disabled? :on-click #(command! s :identify false)} "Identify off"]
             [:button.button {:disabled (or (:busy? s) (str/blank? (:fixture-id s)))
                              :on-click #(action! :adopt {:fixture-id (:fixture-id s) :device (device s)
                                                         :observed-at-ms (:observed-at-ms observation)})} "Adopt observed address"]]
            [:p.small.muted "Device writes are read back. Adopting updates the show patch separately and checks for overlap. A personality's channel meanings still come from your fixture definition."]
            (when (seq (:sensors observation))
              [:div.rdm-sensors [:h3 "Sensors"]
               (for [[i sensor] (map-indexed vector (:sensors observation))]
                 ^{:key i} [:div [:strong (get-in sensor [:definition :value :description] (str "Sensor " i))]
                            [:span [value-view (:value sensor)]]])])
            [:details [:summary "Supported parameters & raw readback"] [:pre (pr-str observation)]]])
         [:details.rdm-advanced [:summary "Advanced GET / SET"]
          [:p.small.muted "Use the fixture's documented PID and raw hexadecimal payload. SET sends a real device command."]
          [:div.rdm-fields [field s :pid "Parameter ID" "text"]
           [:label "Command class" [:select {:aria-label "Command class" :value (:command s) :on-change #(input! :command %)}
                                     [:option {:value "get"} "GET"] [:option {:value "set"} "SET"]]]]
          [field s :bytes "Parameter bytes (hex)" "text"]
          [:button.button {:disabled disabled? :on-click #(try (action! :command (merge (device s) {:pid (js/Number (:pid s))
                                                                                                   :command (keyword (:command s)) :data (hex-bytes (:bytes s))}))
                                                              (catch :default e (rf/dispatch [:rdm/error (.-message e)])))} "Send command"]]]]
       [:div.rdm-jobs [:h3 "Device activity"]
        (if (empty? (:jobs snapshot)) [:p.small.muted "No device requests in this session."]
          (for [job (reverse (:jobs snapshot))]
            ^{:key (:id job)} [:div.rdm-job
             [:span (str (:operation job) " · " (:status job))]
             (when (#{"queued" "running"} (:status job))
               [:button.text-button {:on-click #(action! :cancel {:job-id (:id job)})} "Cancel"])
             (when-let [error (:error job)] [:p.rdm-error (:message error)])
             (when-let [reason (get-in job [:result :reason])] [:p.rdm-error (str "Device response: " reason)])
             (when-let [verification (get-in job [:result :verification :status])]
               [:p {:class (when (not= "verified" verification) "rdm-error")} (str "Readback: " verification)])
             (when (:result job) [:details [:summary "Result"] [:pre (pr-str (:result job))]])]))]
       [:p.rdm-footer.small.muted "Requires an OLA plugin and interface with RDM support. Direct Art-Net RDM is not yet implemented."]])
    (finally (js/clearInterval timer))))
