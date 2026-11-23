(ns tolglow.rig.rdm
  "Explicit RDM jobs and portable rig observations. No IO occurs on require,
  construction, status reads, or model transitions."
  (:require [clojure.string :as str]
            [afterglow.rdm.artnet :as artnet]
            [afterglow.rdm.codec :as codec]
            [afterglow.reactive :as graph]
            [afterglow.rdm.transport :as transport]
            [afterglow.render.frame :as render])
  (:import [java.util UUID]
           [java.util.concurrent ThreadPoolExecutor TimeUnit ArrayBlockingQueue Executors ThreadFactory
            FutureTask ScheduledExecutorService RejectedExecutionException]))

(def state-key :tolglow.rdm/state)
(defn saved [model] (get-in model [:extensions state-key] {}))
(defn session-current? [manager id]
  (let [session (:session @(:state manager))]
    (and session (= id (:id session)) (= (:identity session) ((:identity! manager))))))
(defn wire-id [value] (if (keyword? value) (subs (str value) 1) (str value)))
(defn device-key [{:keys [universe uid sub-device] :or {sub-device 0}}]
  (str universe "/" (codec/uid uid) "/" sub-device))
(defn configuration [config]
  (let [{:keys [transport timeout-ms job-timeout-ms] :as c}
        (merge {:transport :ola :timeout-ms 2000 :job-timeout-ms 30000} config)
        transport (keyword transport)]
    (when-not (and (integer? timeout-ms) (<= 100 timeout-ms 10000)
                   (integer? job-timeout-ms) (<= 1000 job-timeout-ms 60000))
      (codec/fail! "Invalid RDM request or job timeout" {}))
    (let [endpoint (case transport
                     :art-net (artnet/configuration c)
                     :ola (let [{:keys [host port]} (merge {:host "127.0.0.1" :port 9010} c)]
                            (when-not (and (string? host) (<= 1 (count host) 255)
                                           (codec/uint? 16 port) (pos? port))
                              (codec/fail! "OLA RDM requires a host and TCP port" {}))
                            {:host host :port port})
                     (codec/fail! "Unknown RDM transport" {:transport transport}))]
      (assoc endpoint :transport transport :timeout-ms timeout-ms :job-timeout-ms job-timeout-ms))))

(defn- fixture-for [model id]
  (or (some #(when (= (wire-id (:id %)) (wire-id id)) %) (get-in model [:rig :fixtures]))
      (codec/fail! "Show fixture not found" {:fixture-id id})))

(defn- proposed-fixture [model fixture observation address footprint]
  (when-not (and (integer? address) (<= 1 address 512) (integer? footprint) (<= 1 footprint 512)
                 (<= (+ address footprint -1) 512))
    (codec/fail! "RDM address and footprint must fit one DMX universe" {}))
  ;; compile-patch validates the advertised footprint against every coarse and fine offset.
  (let [candidate (assoc fixture :address address :footprint footprint)]
    (render/compile-patch (mapv #(if (= (:id fixture) (:id %)) candidate %) (get-in model [:rig :fixtures])))
    candidate))

(defn validate-write!
  "Check a linked fixture's proposed address/personality before physical SET.
  The command never silently rewrites the show's patch."
  [model request]
  (let [request (codec/typed-command request)
        key (device-key request)
        binding (some (fn [[id device]] (when (= key (device-key device)) id)) (:bindings (saved model)))
        observation (get-in (saved model) [:observations key])]
    (when (and (= :set (:command request)) (#{0xf0 0xe0} (:pid request)))
      (let [_ (when-not (= (if (= 0xf0 (:pid request)) 2 1) (count (:data request)))
                (codec/fail! "Malformed address/personality SET bytes" {}))
            info (get-in observation [:fields :device-info :value])
            address (if (= 0xf0 (:pid request)) (codec/u16 (:data request) 0) (:address info))
            footprint (if (= 0xe0 (:pid request))
                        (get-in observation [:personalities (first (:data request)) :value :footprint])
                        (:footprint info))]
        (when-not (and info footprint)
          (codec/fail! "Read device configuration and the selected personality before changing its patch" {}))
        (when binding (proposed-fixture model (fixture-for model binding) observation address footprint))
        (when-not (and (integer? address) (integer? footprint) (<= 1 address 512)
                       (<= 1 footprint 512) (<= (+ address footprint -1) 512))
          (codec/fail! "Proposed RDM patch exceeds its universe" {}))))
    request))

(defn update-model [model {:keys [operation config device observation uids universe fixture-id] :as payload}]
  (let [state (saved model)
        next-model
        (case operation
          :configure (assoc-in model [:extensions state-key :config] (configuration config))
          :discovered (do (when-not (and (codec/uint? 16 universe) (vector? uids) (<= (count uids) 512))
                            (codec/fail! "Invalid bounded discovery result" {}))
                          (assoc-in model [:extensions state-key :discovered universe] (mapv codec/uid uids)))
          :observe (let [key (device-key device)
                         observations (assoc (:observations state {}) key observation)
                         observations (into {} (take-last 512 (sort-by (comp :observed-at-ms val) observations)))]
                     (assoc-in model [:extensions state-key :observations] observations))
          :associate (let [fixture (fixture-for model fixture-id)
                           device (select-keys (codec/validate-command (merge {:pid 0x60} device)) [:uid :universe :sub-device])
                           key (device-key device)]
                       (when-not (= (:universe fixture) (:universe device))
                         (codec/fail! "The show fixture and RDM device must use the same universe" {}))
                       (when (some (fn [[id other]] (and (not= id (:id fixture)) (= key (device-key other)))) (:bindings state))
                         (codec/fail! "This RDM device is already linked to another show fixture" {}))
                       (assoc-in model [:extensions state-key :bindings (:id fixture)] device))
          :unassociate (update-in model [:extensions state-key :bindings] dissoc (:id (fixture-for model fixture-id)))
          :adopt (let [fixture (fixture-for model fixture-id)
                       binding (get-in state [:bindings (:id fixture)])
                       _ (when-not (and device binding (= (device-key device) (device-key binding)))
                           (codec/fail! "Link the inspected device before adopting its address" {}))
                       observation (get-in state [:observations (device-key binding)])
                       info (get-in observation [:fields :device-info :value])]
                   (when-not (and info (= :ack (get-in observation [:fields :device-info :status])))
                     (codec/fail! "Read the associated device before adopting its address" {}))
                   (when-not (= (:observed-at-ms payload) (:observed-at-ms observation))
                     (codec/fail! "Device readback changed; review its current address before adoption" {}))
                   (let [candidate (proposed-fixture model fixture observation (:address info) (:footprint info))]
                     (update-in model [:rig :fixtures] #(mapv (fn [entry] (if (= (:id fixture) (:id entry)) candidate entry)) %))))
          (codec/fail! "Unknown RDM model operation" {:operation operation}))]
    (update next-model :revision inc)))

(defn- daemon-factory [name]
  (reify ThreadFactory (newThread [_ run] (doto (Thread. run name) (.setDaemon true)))))

(defn session-state [] (atom {:session nil :jobs [] :closed? false}))

(defn create
  "Create an inert session manager. model!/publish! are explicit host boundaries;
  open! is the external transport boundary."
  [{:keys [model! identity! publish! open! state] :or {open! transport/open!}}]
  {:state (or state (session-state)) :lock (Object.) :active (atom {})
   :model! model! :identity! identity! :publish! publish! :open! open!
   :executor (ThreadPoolExecutor. 1 1 0 TimeUnit/MILLISECONDS (ArrayBlockingQueue. 8) (daemon-factory "tolglow-rdm"))
   :timer (Executors/newSingleThreadScheduledExecutor (daemon-factory "tolglow-rdm-deadlines"))})

(defn- close-work! [work]
  (doseq [[_ {:keys [future close!]}] work]
    (when future (.cancel ^java.util.concurrent.Future future true))
    (when-let [close @close!] (try (close) (catch Exception _)))))

(defn disconnect!
  ([manager] (disconnect! manager nil))
  ([manager expected-session]
  (let [work (locking (:lock manager)
               (when (or (nil? expected-session) (= expected-session (:session @(:state manager))))
                 (swap! (:state manager)
                        #(-> % (assoc :session nil)
                             (update :jobs (fn [jobs] (mapv (fn [job] (if (#{:queued :running} (:status job))
                                                                                     (assoc job :status :cancelled) job)) jobs)))))
                 (first (swap-vals! (:active manager) (constantly {})))))]
    (close-work! work)
    (.purge ^ThreadPoolExecutor (:executor manager))
    {:status :disconnected})))

(defn reconcile! [manager]
  (when-let [session (:session @(:state manager))]
    (when-not (= (:identity session) ((:identity! manager))) (disconnect! manager session))))

(defn close! [manager]
  (swap! (:state manager) assoc :closed? true)
  (disconnect! manager)
  (.shutdownNow ^ThreadPoolExecutor (:executor manager))
  (.shutdownNow ^ScheduledExecutorService (:timer manager)) nil)

(defn connect! [manager config identity]
  (let [config (configuration config)
        session {:id (str (UUID/randomUUID)) :identity identity :config config}
        retired (locking (:lock manager)
                  (when (or (:closed? @(:state manager)) (not= identity ((:identity! manager))))
                    (codec/fail! "RDM session changed before connection admission" {}))
                  (swap! (:state manager) assoc :session session :jobs [] :epoch (:expected-epoch identity))
                  (first (reset-vals! (:active manager) {})))]
    (close-work! retired)
    (.purge ^ThreadPoolExecutor (:executor manager))
    ((:publish! manager) identity {:operation :configure :config config :rdm-session-id (:id session)})
    {:status :connected :session-id (:id session) :config config}))

(defn disconnect-session! [manager id]
  (let [expected (:session @(:state manager))]
    (when-not (and expected (= id (:id expected)))
      (codec/fail! "This RDM session has already been replaced" {}))
    (disconnect! manager expected)))

(defn- status-view [state epoch saved fixtures]
  (let [session (:session state)
        current? (= epoch (:epoch state))
        connected? (boolean (and current? session (not (:closed? state))))
        config (or (when connected? (:config session)) (:config saved) (configuration {}))]
    (merge saved
           {:connected? connected? :session-id (when connected? (:id session))
            :config config :transport (:transport config :ola) :jobs (if current? (:jobs state) [])
            :fixtures (mapv #(select-keys % [:id :name :universe :address :channels :footprint :fixture-type-id]) fixtures)})))

(defn status [manager]
  (let [model ((:model! manager))]
    (status-view @(:state manager) (:expected-epoch ((:identity! manager)))
                 (saved model) (get-in model [:rig :fixtures]))))

(defn status-inputs [{:keys [db]} _]
  (let [model @db] [(:epoch model) (saved model) (get-in model [:rig :fixtures])]))

(defn observed-status [{:keys [cofx subscribe]} _]
  (let [[epoch saved fixtures] @(subscribe [::status-inputs])]
    (status-view (cofx ::session-state) epoch saved fixtures)))

(defn register-status! [system state]
  (graph/reg-ref! system ::session-state state)
  (doseq [[id handler] [[::status-inputs #'status-inputs] [:rdm/status #'observed-status]]]
    (graph/reg-sub! system id @handler
                    {:id id :kind :sub :implementation {:type :var :symbol (str (symbol handler))}}))
  system)

(defn- record-job! [manager id change]
  (swap! (:state manager) update :jobs
         #(mapv (fn [job] (if (= id (:id job)) (merge job change) job)) %)))

(defn- read-value! [request! device parameter data deadline current?]
  (let [pid (get codec/pids parameter)
        response (transport/command! request! (merge device {:command :get :pid pid :data data}) deadline current?)]
    (cond-> (select-keys response [:status :data :reason :nack-code :response-code])
      (= :ack (:status response)) (assoc :value (codec/decode-value pid (:data response))))))

(defn- read-device! [request! device deadline current?]
  (let [fields (into {} (for [parameter [:device-info :manufacturer-label :model-description :device-label
                                        :software-label :supported-parameters :personality :address :identify]]
                          [parameter (read-value! request! device parameter [] deadline current?)]))
        info (get-in fields [:device-info :value])
        supported (set (get-in fields [:supported-parameters :value]))
        personalities (when (supported 0xe1)
                        (into {} (for [number (range 1 (inc (min 32 (:personality-count info 0))))]
                                   [number (read-value! request! device :personality-description [number] deadline current?)])))
        sensors (when (supported 0x200)
                  (mapv (fn [number]
                          {:definition (read-value! request! device :sensor-definition [number] deadline current?)
                           :value (when (supported 0x201) (read-value! request! device :sensor-value [number] deadline current?))})
                        (range (min 32 (:sensor-count info 0)))))]
    {:device device :fields fields :personalities personalities :sensors sensors
     :truncated? (or (> (:personality-count info 0) 32) (> (:sensor-count info 0) 32))
     :observed-at-ms (System/currentTimeMillis)}))

(defn cancel! [manager id]
  (let [work (locking (:lock manager)
               (let [work (select-keys @(:active manager) [id])]
                 (swap! (:active manager) dissoc id) work))]
    (close-work! work)
    (record-job! manager id {:status :cancelled})
    (.purge ^ThreadPoolExecutor (:executor manager))
    {:status :cancelled :job-id id}))

(defn- checked-request [manager session operation {:keys [universe full?] :as payload}]
  (let [direct? (= :art-net (get-in session [:config :transport]))]
    (when-not (if direct? (and (integer? universe) (<= 1 universe 32768)) (codec/uint? 16 universe))
      (codec/fail! "Universe outside the selected RDM transport's range" {:universe universe}))
    (when (and direct? (not= operation :discover) (nil? (get-in session [:config :controller-uid])))
      (codec/fail! "Configure an individual controller UID before sending GET or SET" {}))
    (case operation
      :discover (do (when-not (or (nil? full?) (boolean? full?)) (codec/fail! "Full discovery needs a boolean" {}))
                    payload)
      :read (codec/validate-command (merge {:pid 0x60} payload))
      :command (validate-write! ((:model! manager)) payload)
      (codec/fail! "Unknown RDM job" {:operation operation}))))

(def ^:private verified-fields {0xf0 :address 0xe0 :personality 0x82 :device-label 0x1000 :identify})

(defn- verify-readback [request observation]
  (let [{:keys [pid data]} request
        field (verified-fields pid)
        readback (get-in observation [:fields field])
        actual (if (= field :personality) (get-in readback [:value :current]) (:value readback))
        expected (case field :address (codec/u16 data 0) :personality (first data) (codec/decode-value pid data))]
    {:status (cond (not= :ack (:status readback)) :unavailable (= expected actual) :verified :else :mismatch)
     :expected expected :actual actual}))

(defn- run-operation! [manager {:keys [operation device request current? session]} request! deadline]
  (let [publish! (fn [change]
                   (when (current?)
                     ((:publish! manager) (:identity session) (assoc change :rdm-session-id (:id session)))))
        observe! (fn []
                   (let [obs (read-device! request! device deadline current?)]
                     (publish! {:operation :observe :observation obs :device device})
                     obs))]
    (case operation
      :discover (let [table (request! "ForceDiscovery" request)]
                  (when (> (count (:uids table)) 512) (codec/fail! "Discovery exceeds 512-device limit" {}))
                  (publish! (assoc table :operation :discovered))
                  table)
      :read (observe!)
      :command (let [request (validate-write! ((:model! manager)) request)
                     response (transport/command! request! request deadline current?)]
                 (cond
                   (not= :ack (:status response)) response
                   (and (= :set (:command request)) (verified-fields (:pid request)))
                   (let [obs (observe!)]
                     (assoc response :readback obs :verification (verify-readback request obs)))
                   :else (assoc response :value (codec/decode-value (:pid request) (:data response))))))))

(defn- run-job! [manager {:keys [session current? close-ref id] :as job}]
  (let [config (:config session)
        ms (:job-timeout-ms config)
        deadline (+ (System/nanoTime) (* 1000000 ms))
        expire! #(when-let [close! @close-ref] (close!))
        register! (fn [close!] (reset! close-ref close!) (when-not (current?) (close!)))
        timer (.schedule ^ScheduledExecutorService (:timer manager) ^Runnable expire! (long ms) TimeUnit/MILLISECONDS)]
    (try
      (when-not (current?) (throw (InterruptedException. "Stale RDM session")))
      (record-job! manager id {:status :running})
      (let [{:keys [request! close!]} ((:open! manager) config deadline register!)]
        (try
          (let [result (run-operation! manager job request! deadline)]
            (record-job! manager id {:status (if (current?) :completed :cancelled) :result result}))
          (finally (close!))))
      (catch InterruptedException _ (record-job! manager id {:status :cancelled}))
      (catch Exception error
        (record-job! manager id {:status (if (current?) :failed :cancelled)
                                 :error {:message (.getMessage error) :details (ex-data error)}}))
      (finally (.cancel timer false) (swap! (:active manager) dissoc id)))))

(defn submit!
  "Admit bounded asynchronous discovery/read/command work to the current session."
  [manager operation payload]
  (let [session (:session @(:state manager))
        identity ((:identity! manager))
        request (checked-request manager session operation payload)
        device (select-keys request [:universe :uid :sub-device])
        id (str (UUID/randomUUID))
        close-ref (atom nil)
        current? #(and (contains? @(:active manager) id) (= session (:session @(:state manager)))
                       (= identity ((:identity! manager))))
        job {:operation operation :request request :session session :device device :id id :close-ref close-ref :current? current?}
        task (FutureTask. ^java.util.concurrent.Callable #(run-job! manager job))]
    (locking (:lock manager)
      (when-not (and session (not (:closed? @(:state manager))) (= identity (:identity session))
                     (= (:session-id payload) (:id session)) (= session (:session @(:state manager))))
        (codec/fail! "Open a fresh RDM session before sending commands" {}))
      (when (>= (count @(:active manager)) 8) (codec/fail! "RDM queue is full" {}))
      (swap! (:active manager) assoc id {:future task :close! close-ref})
      (swap! (:state manager) update :jobs #(vec (take-last 32 (conj % {:id id :operation operation :device device :status :queued}))))
      (try (.execute ^ThreadPoolExecutor (:executor manager) task)
           (catch RejectedExecutionException error (swap! (:active manager) dissoc id) (throw error))))
    {:status :queued :job-id id}))
