(ns tolglow.studio.state-stream
  "Push committed snapshots to Studio over Transit WebSockets."
  (:require [ring.websocket :as ws]
            [tolglow.studio.origin :as origin]
            [tolglow.studio.projection :as projection]
            [tolglow.studio.query-stream :as queries]
            [tolglow.transit :as transit])
  (:import [java.util UUID]
           [java.util.concurrent Semaphore]
           [java.util.concurrent.atomic AtomicLong AtomicReference]))

(def protocol "tolglow-state/2")
(def ^:private max-clients 32)

(defn- identity-of [view] (select-keys view [:runtime-id :show-id :epoch]))

(defn- message [base {:keys [token view]}]
  (let [identity (identity-of view)
        header {:protocol protocol :identity identity :token token}
        changes (when (and base (= identity (identity-of (:view base))))
                  (projection/typed-changes (:view base) view))]
    (if (some? changes)
      (assoc header :kind :patch :base-token (:token base) :changes changes)
      (assoc header :kind :full :base-token nil :view view))))

(defn- wake! [hub]
  (let [^Semaphore wake (:wake hub)]
    (when (zero? (.availablePermits wake)) (.release wake))))

(defn publish!
  "Called after commit. Hands ownership of a read-only state reference to the
  stream worker. Projection and sockets never run on the publishing thread."
  [hub committed]
  (.set ^AtomicReference (:latest hub) committed)
  (when (seq @(:clients hub)) (wake! hub)))

(defn- release-unused-interest! [hub]
  (when-let [interest @(:status-interest hub)]
    (when-not (some (fn [[_ client]] (:interest @client)) @(:clients hub))
      ((:detach! interest))
      (reset! (:status-interest hub) nil))))

(defn- set-interest! [hub socket enabled?]
  (when-not (and (boolean? enabled?) (:observe-status! hub))
    (throw (ex-info "Invalid Connections subscription" {})))
  (locking (:clients hub)
    (when-let [client (get @(:clients hub) socket)]
      (when (not= enabled? (boolean (:interest @client)))
        (let [interest (when enabled?
                         (or @(:status-interest hub)
                             (let [version (AtomicLong.)
                                   detach! ((:observe-status! hub) #(do (.incrementAndGet version) (wake! hub)))
                                   active {:version version :detach! detach! :snapshot (volatile! nil)
                                           :projection (volatile! nil)}]
                               (reset! (:status-interest hub) active))))]
          ;; Each change of scope starts a full acknowledged baseline. Old ACKs
          ;; cannot promote a frame produced for a different scope.
          (swap! client #(-> % (dissoc :pending :base) (assoc :interest interest)))
          (release-unused-interest! hub)
          (wake! hub))))))

(defn- release-queries! [hub before after]
  (when-let [release! (:release-query! hub)]
    (doseq [[query producer] before
            :when (and (contains? (:scoped-queries hub) (first query)) (not (contains? after query)))]
      (release! producer))))

(defn- wanted-queries [hub]
  (locking (:clients hub)
    (into #{} (mapcat #(vals (:queries @%))) (vals @(:clients hub)))))

(defn- query-demand-changed! [hub client removed]
  (try
    (queries/release-unused-graph-leases! (:query-store hub) #(wanted-queries hub) removed)
    (finally
      (when client (swap! client update :query-releases dec))
      (wake! hub))))

(defn- set-queries! [hub socket desired]
  (when-not (:query-store hub) (throw (ex-info "Signal streaming unavailable" {})))
  (when-let [[client removed] (locking (:clients hub)
                               (when-let [client (get @(:clients hub) socket)]
                                 (let [prior (:queries @client {})]
                                   (when-not (= desired (set (keys prior)))
                                     (let [next (into {} (for [q desired]
                                                           [q (or (get prior q) ((get (:scoped-queries hub) (first q) identity) q))]))
                                           removed (into #{} (keep (fn [[q scoped]]
                                                                    (when-not (contains? next q) scoped))) prior)]
                                       (swap! client #(cond-> (-> % (assoc :queries next) (dissoc :pending :base))
                                                        (seq removed) (-> (update :query-releases (fnil inc 0))
                                                                          (update :removed-queries (fnil into #{}) removed))))
                                       (release-queries! hub prior next)
                                       [client removed])))))]
    (if (seq removed) (query-demand-changed! hub client removed) (wake! hub))))

(defn- forget! [hub socket]
  (let [[removed prior] (locking (:clients hub)
                          (let [removed (get (first (swap-vals! (:clients hub) dissoc socket)) socket)
                                prior (:queries (some-> removed deref))]
                            (release-unused-interest! hub)
                            (when (seq prior) (release-queries! hub prior {}))
                            [removed prior]))]
    (when (seq prior) (query-demand-changed! hub nil (vals prior)))
    removed))

(defn- retire! [hub socket code reason]
  (when (forget! hub socket)
    (try (ws/close socket code reason) (catch Exception _))))

(defn- ready? [state]
  (not (or (:sending state) (:pending state) (pos? (:query-releases state 0)))))

(defn- offer! [hub socket client before target cache]
  (let [base (:base before)
        token (:token target)]
    (when (and target (not= token (:token base)))
      (let [checkpoint? (>= (long (:deltas before 0)) (:checkpoint-every hub))
            base (when-not checkpoint? base)
            key [token (:token base)]
            encoded (or (get @cache key)
                        (let [wire (transit/encode (message base target))]
                          (vswap! cache assoc key wire)
                          wire))]
        (when (compare-and-set! client before (-> before
                                                (dissoc :removed-queries)
                                                (assoc :pending target :sending token :deltas (if base (inc (:deltas before 0)) 0))))
          (ws/send socket encoded
                   (fn []
                     (swap! client #(if (= token (:sending %)) (dissoc % :sending) %))
                     (wake! hub))
                   (fn [_] (retire! hub socket 1011 "State delivery failed"))))))))

(defn- new-target [view] {:token (str (UUID/randomUUID)) :view view})

(defn- common-target [hub source]
  (let [last @(:projected hub)]
    (if (identical? source (:source last)) (:target last)
        (let [view ((:project hub) source)
              target (if (= view (get-in last [:target :view])) (:target last) (new-target view))]
          (vreset! (:projected hub) {:source source :target target})
          target))))

(defn- connections-target [hub interest source common]
  (let [version (.get ^AtomicLong (:version interest))
        last @(:projection interest)]
    (if (and (= version (:version last)) (identical? common (:common last))) (:target last)
        (let [snapshot @(:snapshot interest)
              project (if (= version (:version snapshot)) (:project snapshot)
                          (let [project ((:capture-status hub))]
                            (vreset! (:snapshot interest) {:version version :project project})
                            project))
              status (project source)
              target (when status
                       (if (and (identical? common (:common last))
                                (= status (get-in last [:target :view :connection-status])))
                         (:target last)
                         (new-target (assoc (:view common) :connection-status status))))]
          (vreset! (:projection interest) {:common common :version version :target target})
          target))))

(defn- query-target [hub source client leases common]
  (let [wanted (:queries client)
        query-only? (:query-only? client)
        base (:base client)
        values (into {} (keep (fn [[q scoped]] (when-let [lease (get leases scoped)] [q (queries/read-result lease source)]))) wanted)]
    (cond
      query-only?
      (let [model (:model source)
            identity (assoc (select-keys model [:epoch :show-id]) :runtime-id (:runtime-id hub))]
        (when-not (and (= identity (identity-of (:view base))) (= values (get-in base [:view :subscriptions])))
          (new-target (assoc identity
                             :revision (:revision model)
                             :frame-number (get model :frame-number 0)
                             :subscriptions values))))

      (empty? wanted) @common

      :else
      (let [target @common]
        (when (and target (or (not= (:token target) (:common-token base))
                              (not= values (get-in base [:view :subscriptions]))))
          (assoc (new-target (assoc (:view target) :subscriptions values)) :common-token (:token target)))))))

(defn- deliver-latest! [hub]
  (when-let [source (.get ^AtomicReference (:latest hub))]
    (let [common (delay (common-target hub source))
          clients (mapv (fn [[socket client]] [socket client @client]) @(:clients hub))
          store (:query-store hub)
          leases (when store (queries/reconcile-current! store source #(wanted-queries hub)))
          retiring (when store (queries/retiring-queries store))
          cache (volatile! {})]
      (doseq [[socket client before] clients
              :when (and (ready? before)
                         (not-any? #(contains? retiring %) (:removed-queries before))
                         (every? #(contains? leases %) (vals (:queries before))))]
        (try
          (let [target (if-let [interest (:interest before)]
                         (delay (connections-target hub interest source @common))
                         common)]
            (offer! hub socket client before (query-target hub source before leases target) cache))
          (catch Exception _ (retire! hub socket 1011 "Could not project show state")))))))

(defn- run-worker [hub]
  (try
    (while (not @(:closed? hub))
      (.acquire ^Semaphore (:wake hub))
      (.drainPermits ^Semaphore (:wake hub))
      (when-not @(:closed? hub) (deliver-latest! hub))
      ;; Includes send preparation time in the cadence; never catch up by
      ;; bursting several state messages after a stall.
      (Thread/sleep (long (:period-ms hub))))
    (catch InterruptedException _)))

(defn create
  "Create an inert stream service. Starts one daemon worker on first connection."
  ([project heartbeat!] (create project heartbeat! {}))
  ([project heartbeat! {:keys [frame-rate observe-status! capture-status get-graph runtime-id checkpoint-every client-fault! surfaces observation! scoped-queries release-query!]
                        :or {frame-rate 40 checkpoint-every 240}}]
   (when-not (pos-int? checkpoint-every) (throw (ex-info "Checkpoint interval must be positive" {})))
   (let [hub {:scoped-queries scoped-queries :release-query! release-query! :observation! observation!
              :client-faults (atom []) :report-fault! client-fault! :runtime-id runtime-id :checkpoint-every checkpoint-every :observe-status! observe-status! :capture-status capture-status :status-interest (atom nil)
              :period-ms (long (Math/ceil (/ 1000.0 (min 40 frame-rate))))
              :project project :heartbeat! heartbeat! :clients (atom {}) :closed? (atom false)
              :projected (volatile! nil) :latest (AtomicReference.) :wake (Semaphore. 0)}]
     (let [hub (cond-> hub get-graph (assoc :query-store (queries/create get-graph #(wake! hub) surfaces)))]
       (assoc hub :worker (delay (doto (Thread. #(run-worker hub) "tolglow-state-stream")
                                 (.setDaemon true) (.start))))))))

(defn close! [hub]
  (locking (:closed? hub)
    (reset! (:closed? hub) true)
    (when (realized? (:worker hub)) (.interrupt ^Thread @(:worker hub))))
  (doseq [socket (keys @(:clients hub))] (retire! hub socket 1001 "Studio stopped"))
  (when-let [store (:query-store hub)] (queries/close! store)))

(defn- grant-lease! [hub session clock]
  ;; An acknowledged stream may run at 40Hz. One renewal per second suffices
  ;; for the existing 10-second held-cue lease.
  (let [now (System/nanoTime) last @clock]
    (when (and (or (nil? last) (>= (- now last) 1000000000))
               (compare-and-set! clock last now))
      ((:heartbeat! hub) session))))

(defn- client-fault! [hub client session fault]
  (when-not (and (map? fault) (#{:event :source :subscription} (:kind fault))
                 (or (keyword? (:id fault)) (and (string? (:id fault)) (<= 1 (count (:id fault)) 128)))
                 (string? (:message fault))
                 (every? #(or (nil? %) (string? %) (keyword? %))
                         (map fault [:stack :cue-id :instance-id])))
    (throw (ex-info "Invalid Studio fault" {})))
  (let [now (System/currentTimeMillis)
        before (first (swap-vals! client (fn [c] (if (>= (- now (:fault-at c 0)) 100)
                                                   (assoc c :fault-at now) c))))]
    (when (>= (- now (:fault-at before 0)) 100)
      (let [event (assoc (select-keys fault [:kind :id :message :stack :cue-id :instance-id])
                         :type :studio/handler-failed :session-id session :at now)]
        (swap! (:client-faults hub) #(vec (take-last 64 (conj % event))))
        (when-let [report! (:report-fault! hub)] (report! event))))))

(defn- receive-observation! [hub socket message]
  (when-let [accept! (:observation! hub)]
    (locking (:clients hub)
      (when-let [client (get @(:clients hub) socket)]
        (when-let [producer (get (:queries @client) (:query message))]
          (accept! producer message))))))

(defn- receive! [hub socket session lease-clock text]
  (when-let [client (get @(:clients hub) socket)]
    (try
      (when-not (and (string? text) (<= (count text) 8192))
        (throw (ex-info "Invalid stream control" {})))
      (let [{:keys [op token enabled? queries fault] :as message} (transit/decode text)]
        (case op
          :ack (let [[old _] (swap-vals! client
                                (fn [{:keys [pending] :as state}]
                                  (if (and pending (= token (:token pending)))
                                    (-> state (assoc :base pending) (dissoc :pending)) state)))]
                 (when (and (:pending old) (= token (get-in old [:pending :token])))
                   (grant-lease! hub session lease-clock)
                   (wake! hub)))
          :observation (receive-observation! hub socket message)
          :resync (do (swap! client dissoc :base :pending) (wake! hub))
          :client-fault (client-fault! hub client session fault)
          :queries (set-queries! hub socket (queries/validate queries))
          :connections (set-interest! hub socket enabled?)
          :heartbeat (grant-lease! hub session lease-clock)
          (throw (ex-info "Unknown stream control" {}))))
      (catch Exception _ (retire! hub socket 1008 "Invalid stream control")))))

(defn response [hub request]
  (let [session (get-in request [:query-params "session"])
        lease-clock (atom nil)]
    (cond
      @(:closed? hub) {:status 503 :body "Studio stopped"}
      (not (origin/same-origin? request)) {:status 403 :body "Cross-origin state stream"}
      (not (ws/upgrade-request? request)) {:status 426 :headers {"Upgrade" "websocket"} :body "WebSocket upgrade required"}
      (not (and (string? session) (<= 1 (count session) 128))) {:status 400 :body "Missing Studio session"}
      :else
      {::ws/listener
       {:on-open (fn [socket]
                   (locking (:closed? hub)
                     (if (or @(:closed? hub) (>= (count @(:clients hub)) max-clients))
                       (ws/close socket 1013 "State stream unavailable")
                       (do (swap! (:clients hub) assoc socket (atom {:query-only? (= "queries" (get-in request [:query-params "view"]))}))
                           (force (:worker hub))
                           (wake! hub)))))
        :on-message #(receive! hub %1 session lease-clock %2)
        :on-close (fn [socket _ _] (forget! hub socket))
        :on-error (fn [socket _] (retire! hub socket 1011 "State stream closed"))}})))
