(ns tolglow.studio.state-stream
  "Push committed snapshots to Studio over Transit WebSockets."
  (:require [ring.websocket :as ws]
            [tolglow.studio.projection :as projection]
            [tolglow.studio.transit :as transit])
  (:import [java.util UUID]
           [java.util.concurrent Semaphore]
           [java.util.concurrent.atomic AtomicReference]))

(def protocol "tolglow-state/2")
(def ^:private max-clients 32)
(def ^:private period-ms 25)

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

(defn- retire! [hub socket code reason]
  (when (get (first (swap-vals! (:clients hub) dissoc socket)) socket)
    (try (ws/close socket code reason) (catch Exception _))))

(defn- ready? [state] (not (or (:sending state) (:pending state))))

(defn- send-next! [hub socket client target cache]
  (let [before @client
        base (:base before)
        target @target]
    (when (and (ready? before) (not= (:token target) (:token base)))
      (let [encoded (or (get @cache (:token base))
                        (let [wire (transit/encode (message base target))]
                          (vswap! cache assoc (:token base) wire)
                          wire))]
        ;; A resync or disconnect can race encoding. Only offer a delta if its
        ;; client still owns the acknowledged baseline from which we built it.
        (when (compare-and-set! client before (assoc before :pending target :sending (:token target)))
          (ws/send socket encoded
                   (fn []
                     (swap! client #(if (= (:token target) (:sending %)) (dissoc % :sending) %))
                     (wake! hub))
                   (fn [_] (retire! hub socket 1011 "State delivery failed"))))))))

(defn- deliver-latest! [hub]
  (let [source (.get ^AtomicReference (:latest hub))
        last @(:projected hub)
        ;; One projection per commit across all connections and ACKs. A paused
        ;; receiver costs one retained baseline and at most one message.
        target (delay
                 (if (identical? source (:source last)) (:target last)
                     (let [next {:token (str (UUID/randomUUID)) :view ((:project hub) source)}]
                       (vreset! (:projected hub) {:source source :target next})
                       next)))
        cache (volatile! {})]
    (when source
      (doseq [[socket client] @(:clients hub) :when (ready? @client)]
        (try (send-next! hub socket client target cache)
             (catch Exception _ (retire! hub socket 1011 "Could not project show state")))))))

(defn- run-worker [hub]
  (try
    (while (not @(:closed? hub))
      (.acquire ^Semaphore (:wake hub))
      (.drainPermits ^Semaphore (:wake hub))
      (when-not @(:closed? hub) (deliver-latest! hub))
      ;; Includes send preparation time in the cadence; never catch up by
      ;; bursting several state messages after a stall.
      (Thread/sleep period-ms))
    (catch InterruptedException _)))

(defn create
  "Create an inert stream service. Starts one daemon worker on first connection."
  [project heartbeat!]
  (let [hub {:project project :heartbeat! heartbeat! :clients (atom {}) :closed? (atom false)
             :projected (volatile! nil) :latest (AtomicReference.) :wake (Semaphore. 0)}]
    (assoc hub :worker (delay (doto (Thread. #(run-worker hub) "tolglow-state-stream")
                                (.setDaemon true) (.start))))))

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
      (let [{:keys [op token]} (transit/decode text)]
        (case op
          :ack (let [[old _] (swap-vals! client
                                (fn [{:keys [pending] :as state}]
                                  (if (and pending (= token (:token pending)))
                                    (-> state (assoc :base pending) (dissoc :pending)) state)))]
                 (when (and (:pending old) (= token (get-in old [:pending :token])))
                   (grant-lease! hub session lease-clock)
                   (wake! hub)))
          :resync (do (swap! client dissoc :base :pending) (wake! hub))
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
        :on-close (fn [socket _ _] (swap! (:clients hub) dissoc socket))
        :on-error (fn [socket _] (retire! hub socket 1011 "State stream closed"))}})))
