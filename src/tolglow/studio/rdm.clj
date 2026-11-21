(ns tolglow.studio.rdm
  "Explicit RDM jobs and portable rig observations. No IO occurs on require,
  construction, status reads, or model transitions."
  (:require [clojure.string :as str]
            [tolglow.studio.rdm.codec :as codec]
            [tolglow.studio.rdm.transport :as transport]
            [afterglow.render.frame :as render])
  (:import [java.util UUID]
           [java.util.concurrent ThreadPoolExecutor TimeUnit ArrayBlockingQueue Executors ThreadFactory
            FutureTask ScheduledExecutorService RejectedExecutionException]))

(def state-key :tolglow.rdm/state)
(defn saved [model] (get-in model [:extensions state-key] {}))
(defn wire-id [value] (if (keyword? value) (subs (str value) 1) (str value)))
(defn device-key [{:keys [universe uid sub-device] :or {sub-device 0}}]
  (str universe "/" (codec/uid uid) "/" sub-device))
(defn configuration [config]
  (let [c (merge {:host "127.0.0.1" :port 9010 :timeout-ms 2000 :job-timeout-ms 30000} config)]
    (when-not (and (string? (:host c)) (<= 1 (count (:host c)) 255)
                   (integer? (:port c)) (<= 1 (:port c) 65535)
                   (integer? (:timeout-ms c)) (<= 100 (:timeout-ms c) 10000)
                   (integer? (:job-timeout-ms c)) (<= 1000 (:job-timeout-ms c) 60000))
      (codec/fail! "OLA RDM needs a host, TCP port and bounded request/job deadlines" {}))
    (select-keys c [:host :port :timeout-ms :job-timeout-ms])))

(defn- fixture-for [model id]
  (or (some #(when (= (wire-id (:id %)) (wire-id id)) %) (get-in model [:rig :fixtures]))
      (codec/fail! "Show fixture not found" {:fixture-id id})))

(defn- proposed-fixture [model fixture observation address footprint]
  (when-not (and (integer? address) (<= 1 address 512) (integer? footprint) (<= 1 footprint 512)
                 (<= (+ address footprint -1) 512))
    (codec/fail! "RDM address and footprint must fit one DMX universe" {}))
  (let [largest-channel (reduce max 0 (flatten (vals (:channels fixture))))
        candidate (assoc fixture :address address :footprint footprint)]
    (when (> largest-channel footprint)
      (codec/fail! "This personality is smaller than the show's channel definition; update the fixture type first" {}))
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
      (let [info (get-in observation [:fields :device-info :value])
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
                         (codec/fail! "The show fixture and RDM device must use the same OLA universe" {}))
                       (when (some (fn [[id other]] (and (not= id (:id fixture)) (= key (device-key other)))) (:bindings state))
                         (codec/fail! "This RDM device is already linked to another show fixture" {}))
                       (assoc-in model [:extensions state-key :bindings (:id fixture)] device))
          :unassociate (update-in model [:extensions state-key :bindings] dissoc (:id (fixture-for model fixture-id)))
          :adopt (let [fixture (fixture-for model fixture-id)
                       device (get-in state [:bindings (:id fixture)])
                       observation (when device (get-in state [:observations (device-key device)]))
                       info (get-in observation [:fields :device-info :value])]
                   (when-not (and device info (= :ack (get-in observation [:fields :device-info :status])))
                     (codec/fail! "Read the associated device before adopting its address" {}))
                   (let [candidate (proposed-fixture model fixture observation (:address info) (:footprint info))]
                     (update-in model [:rig :fixtures] #(mapv (fn [entry] (if (= (:id fixture) (:id entry)) candidate entry)) %))))
          (codec/fail! "Unknown RDM model operation" {:operation operation}))]
    (update next-model :revision inc)))

(defn- daemon-factory [name]
  (reify ThreadFactory (newThread [_ run] (doto (Thread. run name) (.setDaemon true)))))

(defn create
  "Create an inert session manager. model!/publish! are explicit host boundaries;
  open! is replaceable only at the external OLA transport boundary for tests."
  [{:keys [model! identity! publish! open!] :or {open! transport/open!}}]
  {:state (atom {:session nil :jobs [] :closed? false}) :lock (Object.) :active (atom {})
   :model! model! :identity! identity! :publish! publish! :open! open!
   :executor (ThreadPoolExecutor. 1 1 0 TimeUnit/MILLISECONDS (ArrayBlockingQueue. 8) (daemon-factory "tolglow-rdm"))
   :timer (Executors/newSingleThreadScheduledExecutor (daemon-factory "tolglow-rdm-deadlines"))})

(defn- close-work! [work]
  (doseq [[_ {:keys [future close!]}] work]
    (when future (.cancel ^java.util.concurrent.Future future true))
    (when-let [close @close!] (try (close) (catch Exception _)))))

(defn disconnect! [manager]
  (let [work (locking (:lock manager)
               (swap! (:state manager) assoc :session nil)
               (first (swap-vals! (:active manager) (constantly {}))))]
    (close-work! work)
    {:status :disconnected}))

(defn reconcile! [manager]
  (when-let [session (:session @(:state manager))]
    (when-not (= (:identity session) ((:identity! manager))) (disconnect! manager))))

(defn close! [manager]
  (swap! (:state manager) assoc :closed? true)
  (disconnect! manager)
  (.shutdownNow ^ThreadPoolExecutor (:executor manager))
  (.shutdownNow ^ScheduledExecutorService (:timer manager)) nil)

(defn connect! [manager config]
  (let [config (configuration config)]
    (disconnect! manager)
    (locking (:lock manager)
      (when (:closed? @(:state manager)) (codec/fail! "RDM manager is closed" {}))
      (let [session {:id (str (UUID/randomUUID)) :identity ((:identity! manager)) :config config}]
        (swap! (:state manager) assoc :session session :jobs [])
        ((:publish! manager) (:identity session) {:operation :configure :config config})
        {:status :connected :session-id (:id session) :config config}))))

(defn status [manager]
  (let [state @(:state manager) model ((:model! manager)) session (:session state)
        connected? (and (not (:closed? state)) session (= (:identity session) ((:identity! manager))))]
    (merge (saved model)
           {:connected? (boolean connected?) :session-id (when connected? (:id session))
            :config (or (:config session) (:config (saved model)) (configuration {}))
            :transport :ola :direct-artnet? false :jobs (:jobs state)
            :fixtures (mapv #(select-keys % [:id :name :universe :address :channels :footprint :fixture-type-id]) (get-in model [:rig :fixtures]))})))

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
    {:status :cancelled :job-id id}))

(defn submit!
  "Admit bounded asynchronous discovery/read/command work. Only a current,
  explicitly opened session can send requests. All commits remain epoch guarded."
  [manager operation payload]
  (let [session (:session @(:state manager)) identity ((:identity! manager))
        request (case operation
                  :discover (do (when-not (codec/uint? 16 (:universe payload)) (codec/fail! "Invalid OLA universe" {})) payload)
                  :read (codec/validate-command (merge {:pid 0x60} payload))
                  :command (validate-write! ((:model! manager)) payload)
                  (codec/fail! "Unknown RDM job" {:operation operation}))
        device (select-keys request [:universe :uid :sub-device])
        id (str (UUID/randomUUID)) close-ref (atom nil)
        current? #(and (contains? @(:active manager) id)
                       (= session (:session @(:state manager))) (= identity ((:identity! manager))))
        task (FutureTask.
              ^java.util.concurrent.Callable
              (fn []
                (let [deadline (+ (System/nanoTime) (* 1000000 (get-in session [:config :job-timeout-ms])))
                      timeout (.schedule ^ScheduledExecutorService (:timer manager)
                                         ^Runnable #(when-let [close @close-ref] (close))
                                         (long (get-in session [:config :job-timeout-ms])) TimeUnit/MILLISECONDS)]
                  (try
                    (when-not (current?) (throw (InterruptedException. "Stale RDM session")))
                    (record-job! manager id {:status :running})
                    (let [{request! :request! close! :close!}
                          ((:open! manager) (:config session) deadline
                           (fn [close!] (reset! close-ref close!) (when-not (current?) (close!))))]
                      (try
                        (let [result
                              (case operation
                                :discover (let [result (request! "ForceDiscovery" request)]
                                            (when (> (count (:uids result)) 512) (codec/fail! "Discovery exceeds 512-device limit" {}))
                                            (when (current?) ((:publish! manager) identity (assoc result :operation :discovered))) result)
                                :read (let [observation (read-device! request! device deadline current?)]
                                        (when (current?) ((:publish! manager) identity {:operation :observe :device device :observation observation})) observation)
                                :command (let [response (transport/command! request! request deadline current?)]
                                           (if (and (= :set (:command request)) (= :ack (:status response))
                                                    (#{0xf0 0xe0 0x82 0x1000} (:pid request)))
                                             (let [observation (read-device! request! device deadline current?)]
                                               (when (current?) ((:publish! manager) identity {:operation :observe :device device :observation observation}))
                                               (assoc response :readback observation))
                                             (cond-> response (= :ack (:status response))
                                               (assoc :value (codec/decode-value (:pid request) (:data response)))))))]
                          (record-job! manager id {:status (if (current?) :completed :cancelled) :result result}))
                        (finally (close!))))
                    (catch InterruptedException _ (record-job! manager id {:status :cancelled}))
                    (catch Exception error
                      (record-job! manager id {:status (if (current?) :failed :cancelled)
                                              :error {:message (.getMessage error) :details (ex-data error)}}))
                    (finally (.cancel timeout false) (swap! (:active manager) dissoc id))))))]
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
