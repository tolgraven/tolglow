(ns tolglow.studio.state-stream
  "Push committed snapshots to Studio over Transit WebSockets."
  (:require [ring.websocket :as ws]
            [tolglow.studio.projection :as projection]
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

(defn- forget! [hub socket]
  (locking (:clients hub)
    (let [removed (get (first (swap-vals! (:clients hub) dissoc socket)) socket)]
      (release-unused-interest! hub)
      removed)))

(defn- retire! [hub socket code reason]
  (when (forget! hub socket)
    (try (ws/close socket code reason) (catch Exception _))))

(defn- ready? [state] (not (or (:sending state) (:pending state))))

(defn- offer! [hub socket client before target cache]
  (let [base (:base before)
        token (:token target)]
    (when (and target (not= token (:token base)))
      (let [key [token (:token base)]
            encoded (or (get @cache key)
                        (let [wire (transit/encode (message base target))]
                          (vswap! cache assoc key wire)
                          wire))]
        (when (compare-and-set! client before (assoc before :pending target :sending token))
          (ws/send socket encoded
                   (fn []
                     (swap! client #(if (= token (:sending %)) (dissoc % :sending) %))
                     (wake! hub))
                   (fn [_] (retire! hub socket 1011 "State delivery failed"))))))))

(defn- new-target [view] {:token (str (UUID/randomUUID)) :view view})

(defn- common-target [hub source]
  (let [last @(:projected hub)]
    (if (identical? source (:source last)) (:target last)
        (let [target (new-target ((:project hub) source))]
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

(defn- deliver-latest! [hub]
  (when-let [source (.get ^AtomicReference (:latest hub))]
    (let [common (delay (common-target hub source))
          cache (volatile! {})]
      (doseq [[socket client] @(:clients hub)
              :let [before @client] :when (ready? before)]
        (try
          (offer! hub socket client before
                  (if-let [interest (:interest before)]
                    (connections-target hub interest source @common)
                    @common) cache)
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
  ([project heartbeat! {:keys [frame-rate observe-status! capture-status] :or {frame-rate 40}}]
   (let [hub {:observe-status! observe-status! :capture-status capture-status :status-interest (atom nil)
              :period-ms (long (Math/ceil (/ 1000.0 (min 40 frame-rate))))
              :project project :heartbeat! heartbeat! :clients (atom {}) :closed? (atom false)
              :projected (volatile! nil) :latest (AtomicReference.) :wake (Semaphore. 0)}]
     (assoc hub :worker (delay (doto (Thread. #(run-worker hub) "tolglow-state-stream")
                                 (.setDaemon true) (.start)))))))

(defn close! [hub]
  (locking (:closed? hub)
    (reset! (:closed? hub) true)
    (when (realized? (:worker hub)) (.interrupt ^Thread @(:worker hub))))
  (doseq [socket (keys @(:clients hub))] (retire! hub socket 1001 "Studio stopped")))

(defn- grant-lease! [hub session clock]
  ;; An acknowledged stream may run at 40Hz. One renewal per second suffices
  ;; for the existing 10-second held-cue lease.
  (let [now (System/nanoTime) last @clock]
    (when (and (or (nil? last) (>= (- now last) 1000000000))
               (compare-and-set! clock last now))
      ((:heartbeat! hub) session))))

(defn- receive! [hub socket session lease-clock text]
  (when-let [client (get @(:clients hub) socket)]
    (try
      (when-not (and (string? text) (<= (count text) 2048))
        (throw (ex-info "Invalid stream control" {})))
      (let [{:keys [op token enabled?]} (transit/decode text)]
        (case op
          :ack (let [[old _] (swap-vals! client
                                (fn [{:keys [pending] :as state}]
                                  (if (and pending (= token (:token pending)))
                                    (-> state (assoc :base pending) (dissoc :pending)) state)))]
                 (when (and (:pending old) (= token (get-in old [:pending :token])))
                   (grant-lease! hub session lease-clock)
                   (wake! hub)))
          :resync (do (swap! client dissoc :base :pending) (wake! hub))
          :connections (set-interest! hub socket enabled?)
          :heartbeat (grant-lease! hub session lease-clock)
          (throw (ex-info "Unknown stream control" {}))))
      (catch Exception _ (retire! hub socket 1008 "Invalid stream control")))))

(defn same-origin? [{:keys [headers scheme]}]
  (if-let [origin (get headers "origin")]
    (try
      (let [uri (java.net.URI. origin)]
        (and (#{"http" "https"} (.getScheme uri))
             (or (nil? scheme) (= (name scheme) (.getScheme uri)))
             (= (.getAuthority uri) (get headers "host"))))
      (catch Exception _ false))
    true))

(defn response [hub request]
  (let [session (get-in request [:query-params "session"])
        lease-clock (atom nil)]
    (cond
      @(:closed? hub) {:status 503 :body "Studio stopped"}
      (not (same-origin? request)) {:status 403 :body "Cross-origin state stream"}
      (not (ws/upgrade-request? request)) {:status 426 :headers {"Upgrade" "websocket"} :body "WebSocket upgrade required"}
      (not (and (string? session) (<= 1 (count session) 128))) {:status 400 :body "Missing Studio session"}
      :else
      {::ws/listener
       {:on-open (fn [socket]
                   (locking (:closed? hub)
                     (if (or @(:closed? hub) (>= (count @(:clients hub)) max-clients))
                       (ws/close socket 1013 "State stream unavailable")
                       (do (swap! (:clients hub) assoc socket (atom {}))
                           (force (:worker hub))
                           (wake! hub)))))
        :on-message #(receive! hub %1 session lease-clock %2)
        :on-close (fn [socket _ _] (forget! hub socket))
        :on-error (fn [socket _] (retire! hub socket 1011 "State stream closed"))}})))
