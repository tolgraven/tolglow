(ns tolglow.studio.sensors
  "Owned browser motion acquisition; conditioning and lighting remain server-side."
  (:require ["@capacitor/motion" :refer [Motion]]
            [cljs.reader :as reader]
            [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.events :as events]
            [tolglow.studio.pretty :as pretty]))

(def radians (/ js/Math.PI 180))

(def sample-period-ms 50)

(def sample-freshness-ms 250)

(defn finite? [x] (and (number? x) (js/Number.isFinite x)))

(defn- measured [out value key scale]
  (if (finite? value) (assoc out key (* scale value)) out))

(defn normalize-motion [sample]
  (reduce (fn [out [path key scale]] (measured out (get-in sample path) key scale)) {}
          [[[:acceleration :x] :accel-x 1] [[:acceleration :y] :accel-y 1] [[:acceleration :z] :accel-z 1]
           [[:rotationRate :beta] :rotation-x radians] [[:rotationRate :gamma] :rotation-y radians]
           [[:rotationRate :alpha] :rotation-z radians]]))

(defn normalize-orientation [sample]
  (reduce (fn [out [key target]] (measured out (get sample key) target radians)) {}
          [[:alpha :orientation-alpha] [:beta :orientation-beta] [:gamma :orientation-gamma]]))

(defn fresh-values [values now]
  (into {} (keep (fn [[key {:keys [value at]}]]
                  (when (and (finite? value) (<= 0 (- now at) sample-freshness-ms)) [key value]))) values))

(def preview-config
  {:version 1 :timeout-ms 500
   :channels [{:id :tilt :input :orientation-beta :calibrated? true :smoothing-ms 40
               :transforms [{:op :wrap :period (* 2 js/Math.PI)} {:op :deadband :width 0.04}
                            {:op :map-range :in [-0.7 0.7] :out [0 1]} {:op :curve :kind :smoothstep}]}
              {:id :acceleration :input {:op :magnitude :inputs [:accel-x :accel-y :accel-z]} :smoothing-ms 20}
              {:id :rotation :input {:op :magnitude :inputs [:rotation-x :rotation-y :rotation-z]} :smoothing-ms 20}]
   :bindings [] :triggers [] :gestures []})

(def defaults {:visible? false :generation 0 :device-id nil :label "Browser motion" :paired? false
               :capture :stopped :session-id nil :pending #{} :polling? false :status nil :error nil
               :values {} :received 0 :sent 0 :age-ms nil :permission nil
               :mapping {:instance-id "" :param "" :cue-id ""} :source (pretty/edn preview-config)
               :osc-host "127.0.0.1" :osc-port "9001"})

(defn state [db] (merge defaults (:sensors db)))

(defn token [db] [(get-in db [:show :runtime-id]) (get-in db [:show :epoch]) (:generation (state db))])

(defn current? [db request-token] (= request-token (token db)))

(defn source-state [s] (some #(when (= (:device-id s) (:device-id %)) %) (get-in s [:status :sources])))

(defn instance-params [db id]
  (when-let [instance (some #(when (= id (:id %)) %) (get-in db [:show :instances]))]
    (or (get-in instance [:definition :params])
        (:params (some #(when (= (:effect-id instance) (:id %)) %) (get-in db [:show :effects]))))))

(defn mapping-config [db]
  (let [{:keys [instance-id param cue-id]} (:mapping (state db))
        schemas (instance-params db instance-id)
        entry (some (fn [[key schema]] (when (= (events/wire-id key) param) [key schema])) schemas)
        [parameter schema] entry
        numeric? (contains? #{:number :integer "number" "integer"} (:type schema))
        bounds (:bounds schema)]
    (when (and (seq instance-id) (not (and entry numeric? (= 2 (count bounds)) (every? finite? bounds))))
      (throw (js/Error. "Choose a current numeric parameter with finite bounds.")))
    (cond-> preview-config
      (seq instance-id)
      (-> (update :channels conj {:id :parameter :input :orientation-beta :calibrated? true :smoothing-ms 40
                                  :transforms [{:op :wrap :period (* 2 js/Math.PI)} {:op :deadband :width 0.04}
                                               {:op :map-range :in [-0.7 0.7] :out bounds}]})
          (assoc :bindings [{:id :parameter :channel :parameter :schema (update schema :type keyword)
                             :command {:op :param/set :instance-id instance-id :param (keyword parameter)}}]))
      (seq cue-id)
      (assoc :gestures [{:id :accent :type :primary-action :cooldown {:beats 0.25}
                        :command {:op :cue/launch :cue-id (keyword cue-id)
                                  :launch {:mode :one-shot :duration {:beats 0.125}}}}]))))

;; Tokens, listener handles and in-flight transport live only at this IO boundary.
(defonce resources (atom {:credentials {} :run nil}))

(defn- alive? [run-id] (= run-id (get-in @resources [:run :id])))

(defn- ignore-error [promise] (when promise (.catch (js/Promise.resolve promise) (fn [_] nil))))

(defn- disarm! [run]
  (when (:session-id run)
    (ignore-error
      (js/fetch "/api/sensors/arm"
                #js {:method "POST" :keepalive true :headers #js {"Content-Type" "application/json"}
                     :body (js/JSON.stringify (clj->js {:device-id (:device-id run) :session-id (:session-id run)
                                                       :armed? false :runtime-id (:runtime-id run) :expected-epoch (:epoch run)}))}))))

(defn stop-capture! [forget?]
  (let [run (:run @resources)]
    (swap! resources assoc :run nil)
    (when-let [timer (:timer run)] (js/clearInterval timer))
    (when-let [controller (:controller run)] (.abort controller))
    (doseq [handle (:handles run)] (ignore-error (js-invoke handle "remove")))
    (doseq [[target name listener] (:listeners run)] (.removeEventListener target name listener))
    (when run (disarm! run))
    (when forget? (swap! resources assoc :credentials {}))))
(rf/reg-fx :sensors/stop stop-capture!)
(rf/reg-fx :sensors/disarm disarm!)
(rf/reg-fx :sensors/store-token (fn [[device-id credential]] (swap! resources assoc-in [:credentials device-id] credential)))
(defn permission-request [constructor]
  (if (and constructor (fn? (aget constructor "requestPermission")))
    (js-invoke constructor "requestPermission")
    (js/Promise.resolve (if constructor "available" "unavailable"))))

(defn event-values [kind sample]
  ((if (= kind :motion) normalize-motion normalize-orientation)
   (if (= kind :motion)
     {:acceleration (let [v (aget sample "acceleration")] (when v {:x (aget v "x") :y (aget v "y") :z (aget v "z")}))
      :rotationRate (let [v (aget sample "rotationRate")] (when v {:alpha (aget v "alpha") :beta (aget v "beta") :gamma (aget v "gamma")}))}
     {:alpha (aget sample "alpha") :beta (aget sample "beta") :gamma (aget sample "gamma")})))

(defn- receive! [id kind sample]
  (when (alive? id)
    (let [now (.now js/performance) values (event-values kind sample)]
      (when (seq values)
        (swap! resources update :run
               (fn [run] (-> run (update :received inc) (assoc :dirty? true :last-at now)
                             (update :values merge (into {} (map (fn [[k value]] [k {:value value :at now}])) values)))))))))

(defn- send-sample! [id]
  (when (alive? id)
    (let [run (:run @resources) now (.now js/performance)
          values (fresh-values (:values run) now)
          age (when (:last-at run) (- now (:last-at run)))]
      (when (>= (- now (:published-at run 0)) 200)
        (swap! resources assoc-in [:run :published-at] now)
        (rf/dispatch [:sensors/readings (:token run) (:session-id run)
                      {:values values :received (:received run) :sent (:sent run) :age-ms age}]))
      (when (and (not (:sending? run)) (:dirty? run) (or (seq values) (seq (:gestures run))))
        (let [controller (js/AbortController.) timeout (js/setTimeout #(.abort controller) 2000)
              packet {:version 1 :device-id (:device-id run) :session-id (:session-id run)
                      :sample-id (:sequence run) :time-ms now :reference-frame "browser-device"
                      :runtime-id (:runtime-id run) :pairing-token (:credential run)
                      :values values :gestures (:gestures run [])}]
          (swap! resources update :run #(-> % (assoc :sending? true :dirty? false :gestures [] :controller controller) (update :sequence inc)))
          (-> (js/fetch "/api/sensors/sample" #js {:method "POST" :headers #js {"Content-Type" "application/json"}
                                                    :signal (.-signal controller) :body (js/JSON.stringify (clj->js packet))})
              (.then (fn [response] (-> (.json response)
                                        (.then (fn [data]
                                                 (when (or (not (.-ok response)) (= "rejected" (aget data "status")) (false? (aget data "accepted?")) (true? (aget data "overflow?")))
                                                   (throw (js/Error. (or (aget data "message") (aget data "reason") "Sensor sample rejected")))))))))
              (.then (fn [_] (when (alive? id) (swap! resources update-in [:run :sent] inc))))
              (.catch (fn [error]
                        (when (alive? id)
                          (stop-capture! false)
                          (rf/dispatch [:sensors/capture-failed (:token run) (events/error-message error)]))))
              (.finally (fn [] (js/clearTimeout timeout)
                          (when (alive? id) (swap! resources update :run dissoc :sending? :controller))))))))))

(defn start-capture! [{:keys [device-id request-token session-id] :as options}]
  (stop-capture! false)
  (let [[runtime-id epoch] request-token credential (get-in @resources [:credentials device-id])
        id session-id]
    (cond
      (not (.-isSecureContext js/window)) (rf/dispatch [:sensors/capture-failed request-token "Motion requires HTTPS or localhost."])
      (nil? credential) (rf/dispatch [:sensors/capture-failed request-token "Pair this browser before enabling motion."])
      :else
      (try
        ;; Both permission calls occur now, inside the original user gesture.
        (let [motion (permission-request (aget js/window "DeviceMotionEvent"))
              orientation (permission-request (aget js/window "DeviceOrientationEvent"))]
          (swap! resources assoc :run {:id id :session-id session-id :device-id device-id :runtime-id runtime-id :epoch epoch
                                      :token request-token :credential credential :handles [] :listeners [] :received 0 :sent 0
                                      :sequence 0 :values {} :gestures []})
          (-> (js/Promise.all #js [motion orientation])
              (.then (fn [permissions]
                       (when (alive? id)
                         (let [motion? (contains? #{"granted" "available"} (aget permissions 0))
                               orientation? (contains? #{"granted" "available"} (aget permissions 1))]
                           (when-not (or motion? orientation?) (throw (js/Error. "Motion access was denied or is unavailable in this browser.")))
                           (doseq [[enabled? type kind] [[motion? "accel" :motion] [orientation? "orientation" :orientation]]]
                             (when enabled?
                               (-> (js-invoke Motion "addListener" type #(receive! id kind %))
                                   (.then (fn [handle]
                                            (if (alive? id) (swap! resources update-in [:run :handles] conj handle)
                                                (ignore-error (js-invoke handle "remove")))))
                                   (.catch (fn [error] (when (alive? id) (stop-capture! false)
                                                       (rf/dispatch [:sensors/capture-failed request-token (events/error-message error)])))))))
                           (let [hidden #(when (.-hidden js/document) (rf/dispatch-sync [:sensors/stop]))
                                 pagehide #(rf/dispatch-sync [:sensors/stop])
                                 timer (js/setInterval #(send-sample! id) sample-period-ms)]
                             (.addEventListener js/document "visibilitychange" hidden)
                             (.addEventListener js/window "pagehide" pagehide)
                             (swap! resources update :run assoc :timer timer :listeners [[js/document "visibilitychange" hidden] [js/window "pagehide" pagehide]]))
                           (rf/dispatch [:sensors/started request-token session-id (js->clj permissions)])))))
              (.catch (fn [error] (when (alive? id) (stop-capture! false)
                                   (rf/dispatch [:sensors/capture-failed request-token (events/error-message error)]))))))
        (catch :default error (rf/dispatch [:sensors/capture-failed request-token (events/error-message error)]))))))
(rf/reg-fx :sensors/start start-capture!)
(rf/reg-fx :sensors/pulse
  (fn [_] (when (:run @resources)
            (swap! resources update :run #(-> % (assoc :dirty? true)
                                               (update :gestures (fn [gestures] (vec (take-last 16 (conj gestures {:id (str (random-uuid)) :type :primary-action :phase :pulse})))) ))))))

(defn- request [db path body success]
  (cond-> {:path path :on-success [success (token db)] :on-failure [:sensors/failed (token db)]}
    (some? body) (assoc :body body)))
(rf/reg-sub :sensors/state (fn [db _] (state db)))
(rf/reg-sub :sensors/show (fn [db _] (select-keys (:show db) [:instances :effects :cues])))
(rf/reg-fx :sensors/browser-id
  (fn [generation]
    (let [id (try
               (let [stored (.getItem js/localStorage "tolglow.motion-device-id")
                     id (if (and stored (re-matches #"browser-[A-Za-z0-9-]{1,80}" stored)) stored (str "browser-" (random-uuid)))]
                 (.setItem js/localStorage "tolglow.motion-device-id" id) id)
               (catch :default _ (str "browser-" (random-uuid))))]
      (rf/dispatch [:sensors/identity generation id]))))
(rf/reg-event-fx :sensors/identity
  (fn [{:keys [db]} [_ generation id]]
    (when (= generation (:generation (state db)))
      {:db (assoc-in db [:sensors :device-id] (or (:device-id (state db)) id))
       :dispatch [:sensors/poll]})))
(rf/reg-event-fx :sensors/open
  (fn [{:keys [db]} _]
    (let [s (state db)]
      (cond-> {:db (assoc db :sensors (assoc s :visible? true :polling? false))}
        (:device-id s) (assoc :dispatch [:sensors/poll])
        (nil? (:device-id s)) (assoc :sensors/browser-id (:generation s))))))
(rf/reg-event-fx :sensors/close
  (fn [{:keys [db]} _]
    {:db (update db :sensors #(-> (merge defaults %) (assoc :visible? false :capture :stopped :polling? false :pending #{} :values {} :age-ms nil :arm-intent? false) (update :generation inc)))
     :sensors/stop false}))
(rf/reg-event-fx :sensors/runtime-change
  (fn [{:keys [db]} _]
    (let [s (state db)]
      {:db (assoc db :sensors (assoc defaults :visible? (:visible? s) :device-id (:device-id s) :label (:label s)
                                    :source (:source s) :generation (inc (:generation s))))
       :sensors/stop true :dispatch (when (:visible? s) [:sensors/poll])})))
(rf/reg-event-db :sensors/edit (fn [db [_ path value]] (assoc-in db (into [:sensors] path) value)))
(rf/reg-event-fx :sensors/poll
  (fn [{:keys [db]} _]
    (when (and (:visible? (state db)) (not (:polling? (state db))))
      (if-not (get-in db [:show :runtime-id]) {:dispatch-later [{:ms 200 :dispatch [:sensors/poll]}]}
        {:db (assoc-in db [:sensors :polling?] true)
         :studio/http (request db "/api/sensors" nil :sensors/loaded)}))))
(rf/reg-event-fx :sensors/loaded
  (fn [{:keys [db]} [_ t data]]
    (when (current? db t)
      {:db (-> db (assoc-in [:sensors :status] data) (assoc-in [:sensors :polling?] false))
       :dispatch-later (when (:visible? (state db)) [{:ms 750 :dispatch [:sensors/poll]}])})))
(rf/reg-event-fx :sensors/failed
  (fn [{:keys [db]} [_ t error]]
    (when (current? db t)
      {:db (update db :sensors #(assoc % :pending #{} :polling? false :error (events/error-message error)))
       :dispatch-later (when (:visible? (state db)) [{:ms 1500 :dispatch [:sensors/poll]}])})))
(rf/reg-event-fx :sensors/pair
  (fn [{:keys [db]} _]
    (let [s (state db)]
      {:db (update db :sensors #(assoc % :error nil :pending #{:pair}))
       :studio/http (request db "/api/sensors/pair" {:device-id (:device-id s) :label (:label s)} :sensors/paired)})))
(rf/reg-event-fx :sensors/paired
  (fn [{:keys [db]} [_ t result]]
    (when (current? db t)
      (if-let [credential (:pairing-token result)]
        {:db (update db :sensors #(assoc % :paired? true :pending #{} :error nil))
         :sensors/store-token [(:device-id result) credential]
         :dispatch [:sensors/save-mapping]}
        {:db (assoc-in db [:sensors :error] "Pairing did not return a credential. Pair again.")}))))
(rf/reg-event-fx :sensors/start
  (fn [{:keys [db]} _]
    (let [s (state db) session (str (random-uuid))]
      (when (and (:paired? s) (= :stopped (:capture s)))
        {:db (update db :sensors #(assoc % :capture :requesting :error nil :session-id session :values {} :received 0 :sent 0))
         :sensors/start {:device-id (:device-id s) :session-id session :request-token (token db)}}))))
(rf/reg-event-db :sensors/started
  (fn [db [_ t session permission]]
    (if (and (current? db t) (= session (:session-id (state db))) (= :requesting (:capture (state db))))
      (update db :sensors #(assoc % :capture :listening :permission permission)) db)))
(rf/reg-event-db :sensors/capture-failed
  (fn [db [_ t error]] (if (current? db t) (update db :sensors #(assoc % :capture :stopped :error error :values {} :age-ms nil :arm-intent? false)) db)))
(rf/reg-event-db :sensors/readings
  (fn [db [_ t session readings]]
    (if (and (current? db t) (= session (:session-id (state db))) (= :listening (:capture (state db))))
      (update db :sensors merge readings) db)))
(rf/reg-event-fx :sensors/stop
  (fn [{:keys [db]} _]
    {:db (update db :sensors #(assoc (merge defaults %) :capture :stopped :values {} :age-ms nil :arm-intent? false)) :sensors/stop false}))
(rf/reg-event-fx :sensors/save-mapping
  (fn [{:keys [db]} [_ advanced?]]
    (try
      (let [s (state db) config (if advanced? (reader/read-string (:source s)) (mapping-config db))]
        {:db (update db :sensors #(assoc % :error nil :source (pretty/edn config) :pending #{:configure}))
         :studio/http (request db "/api/sensors/configure" {:device-id (:device-id s) :source (pr-str config)} :sensors/ack)})
      (catch :default error {:db (assoc-in db [:sensors :error] (events/error-message error))}))))
(rf/reg-event-fx :sensors/action
  (fn [{:keys [db]} [_ action value]]
    (let [s (state db) source (source-state s)
          body (case action
                 :arm {:device-id (:device-id s) :session-id (:session-id s) :armed? (boolean value)}
                 :calibrate {:device-id (:device-id s)}
                 :osc/start {:host (:osc-host s) :port (js/Number (:osc-port s))}
                 :osc/stop {})]
      (when (and (not (and (= action :calibrate) (:armed? source)))
                 (not (and (= action :arm) value (or (not= :listening (:capture s)) (empty? (:values s))))))
        {:db (update db :sensors #(assoc % :error nil :pending #{action} :arm-intent? (if (= action :arm) (boolean value) (:arm-intent? %))))
         :studio/http (cond-> (request db (str "/api/sensors/" (events/wire-id action)) body :sensors/ack)
                        (= action :arm) (assoc :on-success [:sensors/armed (token db)
                                                          {:device-id (:device-id s) :session-id (:session-id s)
                                                           :runtime-id (get-in db [:show :runtime-id]) :epoch (get-in db [:show :epoch])}
                                                          (boolean value)]))}))))
(rf/reg-event-fx :sensors/armed
  (fn [{:keys [db]} [_ t run armed? _]]
    (let [s (state db) still-owned? (and (current? db t) (= (:session-id run) (:session-id s))
                                      (= :listening (:capture s)) (:arm-intent? s))]
      (cond-> {}
        (current? db t) (assoc :db (update db :sensors #(assoc % :pending #{} :error nil)) :dispatch [:sensors/poll])
        (and armed? (not still-owned?)) (assoc :sensors/disarm run)))))
(rf/reg-event-fx :sensors/ack
  (fn [{:keys [db]} [_ t _]]
    (when (current? db t) {:db (update db :sensors #(assoc % :pending #{} :error nil)) :dispatch [:sensors/poll]})))
(rf/reg-event-fx :sensors/accent
  (fn [{:keys [db]} _]
    (when (and (:armed? (source-state (state db))) (= :listening (:capture (state db)))) {:sensors/pulse true})))

(defn- edit! [path value] (rf/dispatch-sync [:sensors/edit path value]))

(defn- number-text [value] (if (finite? value) (.toFixed value 3) "—"))

(defn sensor-panel [db s]
  (let [source (source-state s) armed? (boolean (:armed? source)) pending? (boolean (seq (:pending s)))
        listening? (= :listening (:capture s)) available? (boolean (seq (:values s)))
        mapping (:mapping s) schemas (instance-params db (:instance-id mapping))]
    [:section.sensor-panel.panel {:aria-labelledby "sensor-title"}
     [:div.panel-heading [:div [:div.eyebrow "CONDUCT WITH MOTION"] [:h2#sensor-title "Phone & Watch"]]
      [:span.status-pill {:class (when armed? "connected")} (if armed? "Armed" "Disarmed")]]
     [:p.muted "Tilt your phone to shape a parameter. Calibrate your resting position, then arm the mapping."]
     (when (:error s) [:p.connection-error {:role "alert"} (:error s)])
     [:div.sensor-columns
      [:div.sensor-acquisition
       [:label "Source name" [:input {:aria-label "Motion source name" :value (:label s) :disabled (or (:paired? s) pending?)
                                       :on-change #(edit! [:label] (.. % -target -value))}]]
       [:div.button-row
        [:button.button {:type "button" :disabled (or (nil? (:status s)) (:paired? s) pending?) :on-click #(rf/dispatch [:sensors/pair])}
         (if (:paired? s) "Browser paired" "Pair this browser")]
        [:button.button.primary {:type "button" :disabled (or (not (:paired? s)) (not= :stopped (:capture s)))
                                  :on-click #(rf/dispatch-sync [:sensors/start])} "Enable motion"]
        [:button.button {:type "button" :disabled (= :stopped (:capture s)) :on-click #(rf/dispatch-sync [:sensors/stop])} "Stop motion"]]
       [:p.sensor-feed-status {:role "status"}
        (case (:capture s) :requesting "Requesting sensor access…"
              :listening (if available? (str "Receiving " (count (:values s)) " motion channels") "Listening · no sensor readings received")
              "Motion capture is stopped")]
       [:div.sensor-readings
        (if available?
          (for [[k value] (sort-by (comp str first) (:values s))]
            ^{:key k} [:div [:span (str/replace (name k) "-" " ")] [:strong (number-text value)]
                       [:small (cond (str/starts-with? (name k) "accel") "m/s²" (str/starts-with? (name k) "rotation") "rad/s" :else "rad")]])
          [:p.small.muted "Available axes appear here when the device sends them."])]
       [:p.small.muted (str (:received s) " readings · " (:sent s) " packets · up to 20 Hz")]
       [:div.button-row
        [:button.button {:disabled (or pending? armed? (not available?)) :on-click #(rf/dispatch [:sensors/action :calibrate])} "Calibrate resting position"]
        [:button.button {:disabled (or pending? (not listening?) (not available?))
                         :class (when armed? "danger") :on-click #(rf/dispatch [:sensors/action :arm (not armed?)])}
         (if armed? "Disarm motion" "Arm motion")]]
       (when (seq (:baseline source)) [:p.small.muted "Resting position calibrated."])]
      [:div.sensor-mapping
       [:label "Control an active effect"
        [:select {:aria-label "Motion target instance" :value (:instance-id mapping)
                  :on-change #(do (edit! [:mapping :instance-id] (.. % -target -value)) (edit! [:mapping :param] ""))}
         [:option {:value ""} "Readings only"]
         (for [instance (get-in db [:show :instances])]
           ^{:key (:id instance)} [:option {:value (:id instance)} (str (or (get-in instance [:definition :name]) (:cue-id instance)) " · " (:id instance))])]]
       (when (seq (:instance-id mapping))
         [:label "Tilt controls"
          [:select {:aria-label "Motion target parameter" :value (:param mapping) :on-change #(edit! [:mapping :param] (.. % -target -value))}
           [:option {:value ""} "Choose a parameter…"]
           (for [[key schema] schemas :when (and (contains? #{"number" "integer" :number :integer} (:type schema)) (:bounds schema))]
             ^{:key key} [:option {:value (events/wire-id key)} (or (:name schema) (events/wire-id key))])]])
       [:label "Accent button cue"
        [:select {:aria-label "Motion accent cue" :value (:cue-id mapping) :on-change #(edit! [:mapping :cue-id] (.. % -target -value))}
         [:option {:value ""} "No accent cue"]
         (for [cue (get-in db [:show :cues])] ^{:key (:id cue)} [:option {:value (:id cue)} (:name cue)])]]
       [:div.button-row
        [:button.button {:disabled (or (not (:paired? s)) pending? armed?) :on-click #(rf/dispatch [:sensors/save-mapping])} "Save motion mapping"]
        [:button.button.primary {:disabled (or (not armed?) (not listening?) (str/blank? (:cue-id mapping)))
                                  :on-click #(rf/dispatch [:sensors/accent])} "Accent"]]
       [:p.small.muted "Tilt spans −40° to +40° around your resting position. Sensor capture stops when you leave this view or hide the page."]
       [:details [:summary "Advanced conditioning & gestures"]
        [:textarea.sensor-source {:aria-label "Sensor mapping EDN" :value (:source s) :spell-check false :on-change #(edit! [:source] (.. % -target -value))}]
        [:button.button {:disabled (or (not (:paired? s)) pending? armed?) :on-click #(rf/dispatch [:sensors/save-mapping true])} "Save EDN mapping"]]]]
     [:details.sensor-native [:summary "Native phone / Watch receiver"]
      [:p.small.muted "The iPhone relay sends paired motion and Watch actions to this receiver. Choose a reachable bind address for another device."]
      [:div.form-row
       [:label "Bind address" [:input {:aria-label "Sensor OSC bind address" :value (:osc-host s) :on-change #(edit! [:osc-host] (.. % -target -value))}]]
       [:label "UDP port" [:input {:aria-label "Sensor OSC port" :type "number" :min 1 :max 65535 :value (:osc-port s) :on-change #(edit! [:osc-port] (.. % -target -value))}]]]
      [:div.button-row
       [:button.button {:disabled pending? :on-click #(rf/dispatch [:sensors/action :osc/start])} "Start OSC receiver"]
       [:button.button {:disabled pending? :on-click #(rf/dispatch [:sensors/action :osc/stop])} "Stop OSC receiver"]]
      [:pre (pretty/edn (get-in s [:status :udp]))]]]))

(def panel
  (r/create-class {:display-name "Phone motion"
                   :component-did-mount (fn [_] (rf/dispatch [:sensors/open]))
                   :component-will-unmount (fn [_] (rf/dispatch-sync [:sensors/close]))
                   :reagent-render (fn [_] [sensor-panel {:show @(rf/subscribe [:sensors/show])} @(rf/subscribe [:sensors/state])])}))
