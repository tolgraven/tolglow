(ns tolglow.studio.rdm
  "RDM fixture management through guarded re-frame actions and owned host jobs."
  (:require [clojure.string :as str] [re-frame.core :as rf]
            [tolglow.studio.state-socket]
            [tolglow.studio.show-window :as show-window]
            [tolglow.studio.events]
            [tolglow.studio.pretty :as pretty]))

(def defaults {:generation 0 :busy? false :error nil :notice nil :form {}
               :universe "1" :uid "" :sub-device "0" :fixture-id ""
               :address nil :personality nil :label nil :pid "0x0060" :bytes "" :command "get"})

(def connection-fields #{:host :port :controller-uid :bind-host :local-port :bind-index :physical-port
                         :timeout-ms :job-timeout-ms})

(def query [:rdm/status])

(defn identity-of [db] (select-keys (:show db) [:runtime-id :epoch]))

(defn- ui [db]
  (let [id (identity-of db) local (get-in db [:ui :rdm])]
    (if (= id (:identity local)) local (assoc defaults :identity id))))

(defn- settings-form [transport config]
  (let [defaults (merge {:timeout-ms 2000 :job-timeout-ms 30000}
                        (if (= :art-net transport)
                          {:host "" :port 6454 :local-port 6454 :physical-port 1 :bind-index 1}
                          {:host "127.0.0.1" :port 9010}))
        settings (if (= transport (:transport config :ola)) (merge defaults config) defaults)]
    (into {} (map (fn [[k v]] [k (str v)])) (select-keys settings connection-fields))))

(defn- discovery-inventory [snapshot universe]
  (let [uids (get-in snapshot [:discovered universe])
        pending? (and (:connected? snapshot)
                      (some (fn [{:keys [operation device status]}]
                              (and (= :discover operation)
                                   (= universe (:universe device))
                                   (#{:queued :running} status)))
                            (:jobs snapshot)))
        device-count (count uids)]
    {:uids uids
     :status-text (cond
                    pending? (str "Discovery pending for universe " universe ".")
                    (nil? uids) (str "No RDM inventory recorded for universe " universe ".")
                    (empty? uids) (str "Last inventory for universe " universe " listed no RDM devices.")
                    :else (str device-count (if (= 1 device-count) " RDM device" " RDM devices")
                               " in the last inventory for universe " universe "."))}))

(defn- view [local snapshot]
  (let [config (:config snapshot)
        draft (when-not (:connected? snapshot) local)
        transport (or (:transport draft) (:transport config) :ola)
        universe (parse-long (:universe local))]
    (merge local (settings-form transport config) (:form draft)
           {:snapshot snapshot :transport transport
            :inventory (when (and snapshot universe) (discovery-inventory snapshot universe))})))

(defn state
  "The RDM form over the pushed status. remote maps queries to results, as the
  :studio/remote coeffect supplies them."
  [db remote]
  (view (ui db) (get-in remote [query :value])))

(defn current-request? [db token]
  (= token [(:identity (ui db)) (:generation (ui db))]))

(defn key-text [value] (if (keyword? value) (subs (str value) 1) (str value)))

(defn device [s] {:universe (parse-long (:universe s)) :uid (:uid s) :sub-device (parse-long (:sub-device s))})

(defn device-key [s] (str (:universe s) "/" (str/lower-case (:uid s)) "/" (:sub-device s)))

(defn observation [s] (get-in s [:snapshot :observations (device-key s)]))

(defn config [s]
  (let [common {:transport (:transport s) :host (:host s) :port (parse-long (:port s))
                :timeout-ms (parse-long (:timeout-ms s)) :job-timeout-ms (parse-long (:job-timeout-ms s))}]
    (if (= :art-net (:transport s))
      (cond-> (assoc common :local-port (parse-long (:local-port s))
                     :bind-index (parse-long (:bind-index s)) :physical-port (parse-long (:physical-port s)))
        (not (str/blank? (:bind-host s))) (assoc :bind-host (:bind-host s))
        (not (str/blank? (:controller-uid s))) (assoc :controller-uid (:controller-uid s)))
      common)))

(defn- hex-number [text] (js/parseInt text 16))

(defn hex-bytes [text]
  (let [parts (if (str/blank? text) [] (str/split (str/trim text) #"\s+"))]
    (when-not (and (<= (count parts) 231) (every? #(re-matches #"[0-9a-fA-F]{2}" %) parts))
      (throw (js/Error. "Use up to 231 hex bytes, separated by spaces: 00 ff")))
    (mapv hex-number parts)))

(defn request-fx [db remote operation payload]
  (let [local (-> (ui db) (update :generation inc) (assoc :busy? true :error nil))
        token [(:identity local) (:generation local)]
        body (merge {:runtime-id (get-in db [:show :runtime-id]) :expected-epoch (get-in db [:show :epoch])
                     :session-id (get-in (state db remote) [:snapshot :session-id])} payload)]
    {:db (assoc-in db [:ui :rdm] local)
     :studio/http {:path (str "/api/rdm/" (name operation)) :body body :typed? true
                   :on-success [:rdm/reply token operation] :on-failure [:rdm/failed token]}}))

(rf/reg-sub :rdm/ui (fn [db _] (ui db)))
(rf/reg-sub :rdm/state :<- [:rdm/ui] :<- [:studio/remote query] (fn [[local snapshot] _] (view local snapshot)))
(rf/reg-event-db :rdm/set
  (fn [db [_ k v]]
    (assoc-in db [:ui :rdm]
              (cond (= :transport k) (assoc (ui db) :transport (keyword v) :form {})
                    (connection-fields k) (assoc-in (ui db) [:form k] v)
                    :else (assoc (ui db) k v)))))
(rf/reg-event-fx :rdm/action
  [(rf/inject-cofx :studio/remote)]
  (fn [{:keys [db remote]} [_ operation payload]]
    (when-not (:busy? (ui db)) (request-fx db remote operation payload))))
(defn- notice [operation result]
  (case operation
    :connect "RDM session enabled. Discovery and device requests are explicit."
    :adopt "Observed address adopted into the show patch."
    :associate "Fixture linked. Its show patch is unchanged."
    :disconnect "RDM session disabled."
    (if (:job-id result) "Device job queued." "Saved.")))
(rf/reg-event-db :rdm/reply
  (fn [db [_ token operation result]]
    (if (current-request? db token)
      (assoc-in db [:ui :rdm] (cond-> (assoc (ui db) :busy? false :notice (notice operation result))
                                (= :connect operation) (dissoc :form :transport)))
      db)))
(rf/reg-event-db :rdm/failed
  (fn [db [_ token error]]
    (if (current-request? db token) (assoc-in db [:ui :rdm] (assoc (ui db) :busy? false :error (str error))) db)))
(rf/reg-event-db :rdm/error (fn [db [_ error]] (assoc-in db [:ui :rdm] (assoc (ui db) :error error))))
(rf/reg-event-fx :rdm/select-fixture
  [(rf/inject-cofx :studio/remote)]
  (fn [{:keys [db remote]} [_ fixture]]
    (let [binding (get-in (state db remote) [:snapshot :bindings (:id fixture)])]
      {:db (assoc-in db [:ui :rdm] (merge (ui db) {:fixture-id (:id fixture) :universe (str (:universe fixture))
                                                    :uid (:uid binding "") :sub-device (str (:sub-device binding 0))
                                                    :address nil :personality nil :label nil}))})))

(defn- input! [key event] (rf/dispatch-sync [:rdm/set key (.. event -target -value)]))

(defn- action! [operation payload] (rf/dispatch [:rdm/action operation payload]))

(defn- command! [s parameter value] (action! :command (merge (device s) {:command :set :parameter parameter :value value})))

(defn- field [s key title type]
  [:label title [:input {:aria-label title :type type :value (get s key "") :on-change #(input! key %)}]])

(defn- pid-number [text]
  (if (re-matches #"0[xX][0-9a-fA-F]+" text) (hex-number (subs text 2)) (parse-long text)))

(defn- value-view [entry]
  (if (= :ack (:status entry))
    (let [value (:value entry)] (if (or (map? value) (vector? value)) (pr-str value) (str value)))
    (if-let [reason (or (:reason entry) (:status entry))] (name reason) "Not read")))

(defn- connection-form [s enabled?]
  (let [direct? (= :art-net (:transport s))]
    [:div.rdm-session
     [:fieldset.rdm-connection {:disabled (or enabled? (:busy? s))}
      [:label "Transport"
       [:select {:aria-label "RDM transport" :value (name (:transport s)) :on-change #(input! :transport %)}
        [:option {:value "ola"} "OLA"] [:option {:value "art-net"} "Direct Art-Net"]]]
      [field s :host (if direct? "Gateway IPv4 address" "OLA host") "text"]
      [field s :port (if direct? "Gateway UDP port" "OLA TCP port") "number"]
      (when direct? [field s :controller-uid "Controller UID · required for GET / SET" "text"])
      [:details.rdm-options [:summary "Connection settings"]
       [:div.rdm-fields
        (when direct? [field s :bind-host "Local IPv4 address" "text"])
        (when direct? [field s :local-port "Local UDP port" "number"])
        (when direct? [field s :bind-index "Gateway bind index" "number"])
        (when direct? [field s :physical-port "Port within binding" "number"])
        [field s :timeout-ms "Request timeout (ms)" "number"]
        [field s :job-timeout-ms "Job timeout (ms)" "number"]]]]
     (let [reason (when-not enabled? @(rf/subscribe [:show-window/preview-only]))]
       [:<>
        [show-window/device-note reason]
        [:button#rdm-enable.button (show-window/device-control
                                    {:disabled (or (:busy? s) (nil? (:snapshot s)))
                                     :on-click #(if enabled? (action! :disconnect {}) (action! :connect {:config (config s)}))}
                                    reason)
         (if enabled? "Disable RDM" "Enable RDM")]])]))

(defn panel []
  (let [s @(rf/subscribe [:rdm/state]) snapshot (:snapshot s) observation (observation s)
        info (get-in observation [:fields :device-info :value])
        enabled? (:connected? snapshot)
        discovery-disabled? (or (:busy? s) (not enabled?))
        missing-uid? (and enabled? (= :art-net (:transport s)) (str/blank? (:controller-uid s)))
        disabled? (or discovery-disabled? missing-uid?)
        {:keys [uids status-text]} (:inventory s)]
    [:section.panel.rdm-panel {:aria-label "RDM fixture management"}
       [:div.panel-top [:span.section-label "RIG · RDM FIXTURES"]
        [:span.rdm-badge (cond enabled? "SESSION ENABLED" snapshot "OFFLINE" :else "CONNECTING")]]
       [:div.rdm-intro [:h2 "Fixture management"]
        [:p "Choose a gateway to discover devices or send commands. Saved observations are available offline."]]
       [connection-form s enabled?]
       (when missing-uid? [:p.rdm-notice "Discovery is enabled. Disable RDM and add a controller UID to send GET or SET."])
       (when (:error s) [:p.rdm-error {:role "alert"} (:error s)])
       (when (:notice s) [:p.rdm-notice {:role "status"} (:notice s)])
       [:div.rdm-columns
        [:div.rdm-show-fixtures [:h3 "Show fixtures"]
         [:p.small.muted "Select a fixture to link its RDM identity. Linking does not move its DMX patch."]
         (for [fixture (:fixtures snapshot)]
           ^{:key (:id fixture)}
           [:button.rdm-fixture {:class (when (= (:id fixture) (:fixture-id s)) "selected")
                                 :on-click #(rf/dispatch [:rdm/select-fixture fixture])}
            [:strong (:name fixture (key-text (:id fixture)))]
            [:span (str "U" (:universe fixture) " · " (:address fixture))]
            [:small (or (:uid (get (:bindings snapshot) (:id fixture))) "Not linked")]])]
        [:div.rdm-device
         [:h3 "Discover & inspect"]
         [:div.rdm-fields [field s :universe "RDM universe" "number"]
          [:div.rdm-actions
           [:button.button {:disabled discovery-disabled? :on-click #(action! :discover {:universe (parse-long (:universe s)) :full? false})} "Read device list"]
           [:button.button {:disabled discovery-disabled? :on-click #(action! :discover {:universe (parse-long (:universe s)) :full? true})} "Discover"]]]
         (when status-text [:p.small.muted {:role "status"} status-text])
         (when (seq uids)
           [:label "Discovered device"
            [:select {:aria-label "Discovered device" :value (:uid s) :on-change #(input! :uid %)}
             [:option {:value ""} "Choose a device"]
             (for [uid uids] ^{:key uid} [:option {:value uid} uid])]])
         [:div.rdm-fields [field s :uid "Device UID" "text"] [field s :sub-device "Subdevice" "number"]]
         [:div.rdm-actions
          [:button.button {:disabled (or disabled? (str/blank? (:uid s))) :on-click #(action! :read (device s))} "Read device"]
          [:button.button {:disabled (or (:busy? s) (empty? (key-text (:fixture-id s))) (str/blank? (:uid s)))
                           :on-click #(action! :associate {:fixture-id (:fixture-id s) :device (device s)})} "Link selected fixture"]
          [:button.text-button {:disabled (or (:busy? s) (empty? (key-text (:fixture-id s))))
                                :on-click #(action! :unassociate {:fixture-id (:fixture-id s)})} "Unlink"]]
         (when observation
           [:div.rdm-readback
            [:h3 "Observed configuration"]
            [:dl (for [[title key] [["Manufacturer" :manufacturer-label] ["Model" :model-description]
                                    ["Device label" :device-label] ["Software" :software-label]]]
                   ^{:key key} [:div [:dt title] [:dd [value-view (get-in observation [:fields key])]]])
             [:div [:dt "Identify"] [:dd {:aria-label "Observed identify"}
                                        (case (get-in observation [:fields :identify :value]) true "On" false "Off" "Not read")]]
             [:div [:dt "Footprint / subdevices"] [:dd (str (:footprint info "—") " slots / " (:sub-device-count info "—"))]]
             [:div [:dt "Physical address / personality"] [:dd (str (:address info "—") " / " (:personality info "—"))]]]
            [:div.rdm-fields
             [field (assoc s :address (or (:address s) (str (:address info "")))) :address "New DMX address" "number"]
             [:button.button {:disabled disabled? :on-click #(command! s :address (parse-long (str (or (:address s) (:address info)))))} "Set device address"]]
            [:div.rdm-fields
             [:label "Device personality"
              [:select {:aria-label "Device personality" :value (or (:personality s) (str (:personality info "")))
                        :on-change #(input! :personality %)}
               (for [[number entry] (:personalities observation)]
                 ^{:key (str number)} [:option {:value (key-text number)}
                                      (str (key-text number) " · " (get-in entry [:value :name] "Unavailable")
                                           " · " (get-in entry [:value :footprint] "?") " slots")])]]
             [:button.button {:disabled disabled? :on-click #(command! s :personality (parse-long (str (or (:personality s) (:personality info)))))} "Set personality"]]
            [:div.rdm-fields
             [field (assoc s :label (or (:label s) (get-in observation [:fields :device-label :value] ""))) :label "New device label" "text"]
             [:button.button {:disabled disabled? :on-click #(command! s :device-label (or (:label s) (get-in observation [:fields :device-label :value] "")))} "Set label"]]
            [:div.rdm-actions
             [:button.button {:disabled disabled? :on-click #(command! s :identify true)} "Identify on"]
             [:button.button {:disabled disabled? :on-click #(command! s :identify false)} "Identify off"]
             [:button.button {:disabled (or (:busy? s) (empty? (key-text (:fixture-id s))))
                              :on-click #(action! :adopt {:fixture-id (:fixture-id s) :device (device s)
                                                         :observed-at-ms (:observed-at-ms observation)})} "Adopt observed address"]]
            [:p.small.muted "Device writes are read back. Adopting updates the show patch separately and checks for overlap. A personality's channel meanings still come from your fixture definition."]
            (when (seq (:sensors observation))
              [:div.rdm-sensors [:h3 "Sensors"]
               (for [[i sensor] (map-indexed vector (:sensors observation))]
                 ^{:key i} [:div [:strong (get-in sensor [:definition :value :description] (str "Sensor " i))]
                            [:span [value-view (:value sensor)]]])])
            [:details [:summary "Supported parameters & raw readback"] [:pre (pretty/edn observation)]]])
         [:details.rdm-advanced [:summary "Advanced GET / SET"]
          [:p.small.muted "Use the fixture's documented PID and raw hexadecimal payload. SET sends a real device command."]
          [:div.rdm-fields [field s :pid "Parameter ID" "text"]
           [:label "Command class" [:select {:aria-label "Command class" :value (:command s) :on-change #(input! :command %)}
                                     [:option {:value "get"} "GET"] [:option {:value "set"} "SET"]]]]
          [field s :bytes "Parameter bytes (hex)" "text"]
          [:button.button {:disabled disabled? :on-click #(try (action! :command (merge (device s) {:pid (pid-number (:pid s))
                                                                                                   :command (keyword (:command s)) :data (hex-bytes (:bytes s))}))
                                                              (catch :default e (rf/dispatch [:rdm/error (.-message e)])))} "Send command"]]]]
       [:div.rdm-jobs [:h3 "Device activity"]
        (if (empty? (:jobs snapshot)) [:p.small.muted "No device requests in this session."]
          (for [job (reverse (:jobs snapshot))]
            ^{:key (:id job)} [:div.rdm-job
             [:span (str (name (:operation job)) " · " (name (:status job)))]
             (when (#{:queued :running} (:status job))
               [:button.text-button {:on-click #(action! :cancel {:job-id (:id job)})} "Cancel"])
             (when-let [error (:error job)] [:p.rdm-error (:message error)])
             (when-let [reason (get-in job [:result :reason])] [:p.rdm-error (str "Device response: " (name reason))])
             (when-let [verification (get-in job [:result :verification :status])]
               [:p {:class (when (not= :verified verification) "rdm-error")} (str "Readback: " (name verification))])
             (when (:result job) [:details [:summary "Result"] [:pre (pretty/edn (:result job))]])]))]
       [:p.rdm-footer.small.muted (if (= :art-net (:transport s))
                                   "Direct Art-Net uses universes 1–32768. Read device list uses the gateway’s cache. Discover starts a physical scan."
                                   "Choose an OLA universe patched to an RDM-capable interface.")]]))
