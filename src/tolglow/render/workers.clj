(ns tolglow.render.workers
  "Admission and asynchronous socket I/O for a show's distributed render pool."
  (:require [afterglow.render.distributed :as pool]
            [ring.websocket :as ws]
            [tolglow.studio.access :as access]
            [tolglow.studio.origin :as origin]
            [tolglow.transit :as transit])
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest]
           [java.util.concurrent ArrayBlockingQueue ScheduledThreadPoolExecutor ThreadFactory TimeUnit]))

;; A result and its Transit base64 representation fit Jetty's default 64 KiB
;; incoming message limit. The coordinator can distribute larger jobs in slices.
(def max-result-floats 8192)
(def ^:private max-clients 16)

(defn- credential [value]
  (when (and (string? value) (<= 24 (count value) 1024)) (.getBytes ^String value StandardCharsets/UTF_8)))

(defn create
  ([pool operator] (create pool operator {}))
  ([pool operator {:keys [guest-bridge verification] :or {verification :full}}]
   (when (and pool (or (credential operator) guest-bridge))
     (let [operator-bytes (credential operator) bridge (credential guest-bridge)]
       (when (or (and guest-bridge (not bridge)) (and operator (not operator-bytes))
                 (and bridge operator-bytes (MessageDigest/isEqual bridge operator-bytes))
                 (and bridge (not (contains? pool/verification-policies verification))))
         (throw (ex-info "Invalid compute credentials or guest verification mode" {})))
       {:pool pool :token operator-bytes :bridge-token bridge :verification verification
        :clients (atom {}) :closed? (atom false)
        :io (doto (ScheduledThreadPoolExecutor. 1
                    ^ThreadFactory (reify ThreadFactory
                                     (newThread [_ f] (doto (Thread. f "tolglow-compute-socket-io") (.setDaemon true)))))
              (.setRemoveOnCancelPolicy true))}))))

(defn admission
  "Validate credentials held by the Studio worker or stage bridge. The host
  chooses guest verification. A guest never receives either credential."
  [hub {:keys [op token caps]}]
  (let [bytes (credential token) limit (:max-result-floats caps)
        matches? #(and bytes % (MessageDigest/isEqual bytes %))]
    (when (and (= :hello op) (pos-int? limit))
      (let [base {:max-result-floats (min limit max-result-floats)}]
        (cond
          (matches? (:bridge-token hub))
          (when (and (= true (:guest? caps)) (string? (:subject caps)) (<= 1 (count (:subject caps)) 128))
            (assoc base :verification (:verification hub) :subject (:subject caps)))
          (and (matches? (:token hub)) (not (contains? caps :guest?))) base)))))

(defn- retire! [{:keys [clients pool]} socket code reason]
  (when-let [client (get (first (swap-vals! clients dissoc socket)) socket)]
    (locking client
      (reset! (:closed? client) true)
      (when-let [timeout @(:timer client)] (.cancel ^java.util.concurrent.Future timeout false))
      (.clear ^ArrayBlockingQueue (:out client))
      (when-let [id @(:id client)] (pool/disconnect! pool id)))
    (try (ws/close socket code reason) (catch Exception _))))

(defn close! [hub]
  (when hub
    (locking hub (reset! (:closed? hub) true))
    (doseq [socket (keys @(:clients hub))] (retire! hub socket 1001 "Studio stopped"))
    (.shutdownNow ^ScheduledThreadPoolExecutor (:io hub))))

(declare send-next!)

(defn- schedule-send! [{:keys [io] :as hub} socket client]
  (when (and (not @(:closed? client)) (compare-and-set! (:sending? client) false true))
    (.execute ^ScheduledThreadPoolExecutor io ^Runnable #(send-next! hub socket client))))

(defn- send-next! [hub socket {:keys [out closed? sending?] :as client}]
  (when-not @closed?
    (try
      (if-let [packet (.poll ^ArrayBlockingQueue out)]
        (ws/send socket (transit/encode packet)
                 (fn [] (reset! sending? false) (schedule-send! hub socket client))
                 (fn [_] (retire! hub socket 1011 "Compute write failed")))
        (do (reset! sending? false)
            (when-not (.isEmpty ^ArrayBlockingQueue out) (schedule-send! hub socket client))))
      (catch Exception _ (retire! hub socket 1011 "Compute write failed")))))

(defn- offer! [hub socket client packet]
  (if (and (not @(:closed? client)) (.offer ^ArrayBlockingQueue (:out client) packet))
    (when @(:id client) (schedule-send! hub socket client))
    (throw (ex-info "Compute peer unavailable" {}))))

(defn- open! [hub socket]
  (locking hub
    (if (or @(:closed? hub) (>= (count @(:clients hub)) max-clients))
      (ws/close socket 1013 "Compute unavailable")
      (let [client {:id (atom nil) :closed? (atom false) :sending? (atom false)
                    :timer (atom nil) :out (ArrayBlockingQueue. 4)}]
        (swap! (:clients hub) assoc socket client)
        (reset! (:timer client)
                (.schedule ^ScheduledThreadPoolExecutor (:io hub)
                           ^Runnable #(locking client
                                        (when-not @(:id client)
                                          (retire! hub socket 1008 "Compute handshake expired")))
                           5 TimeUnit/SECONDS))))))

(defn- admit! [hub socket client hello]
  (let [caps (or (admission hub hello) (throw (ex-info "Compute admission denied" {})))]
    ;; Queue welcome before work, release the writer only after successful pool membership.
    (.add ^ArrayBlockingQueue (:out client)
          (cond-> {:op :welcome} (:verification caps) (assoc :verification (:verification caps))))
    (let [id (pool/connect! (:pool hub) caps (partial offer! hub socket client))]
      (reset! (:id client) id)
      (schedule-send! hub socket client)
      (when-let [timer @(:timer client)] (.cancel ^java.util.concurrent.Future timer false)))))

(defn- receive! [hub socket message]
  (when-let [client (get @(:clients hub) socket)]
    (try
      (locking client
        (when-not @(:closed? client)
          (let [id @(:id client)
                cap (if id 65536 2048)]
            (when-not (and (string? message) (<= (count message) cap))
              (throw (ex-info "Invalid compute message" {})))
            (let [{:keys [op values] :as packet} (transit/decode message)]
              (if id
                (do
                  (when-not (and (#{:ready :result :error :keepalive} op)
                                 (or (not= :result op)
                                     (and (bytes? values) (<= (alength ^bytes values) (* 4 max-result-floats)))))
                    (throw (ex-info "Unexpected compute response" {})))
                  (when-not (= op :keepalive)
                    (pool/receive! (:pool hub) id (select-keys packet [:op :id :job :values])))
                  (when-not (pool/connected? (:pool hub) id)
                    (throw (ex-info "Worker lease ended" {}))))
                (admit! hub socket client packet))))))
      (catch Exception _ (retire! hub socket 1008 "Compute session rejected")))))

(defn response [hub {:keys [scheme remote-addr headers] :as request}]
  (cond
    (nil? hub) {:status 503 :body "Distributed rendering unavailable"}
    (not (and (seq (get headers "origin")) (origin/same-origin? request)))
    {:status 403 :body "Same-origin Studio session required"}
    (not (or (and (origin/loopback-host? remote-addr)
                  (origin/loopback-host? (.getHost (java.net.URI. (get headers "origin")))))
             (and (= :https scheme) (::access/operator? request))))
    {:status 403 :body "Authenticated HTTPS Studio session required"}
    (not (ws/upgrade-request? request)) {:status 426 :headers {"Upgrade" "websocket"} :body "WebSocket required"}
    :else
    {::ws/listener {:on-open #(open! hub %)
                    :on-message #(receive! hub %1 %2)
                    :on-close (fn [socket _ _] (retire! hub socket 1000 "Compute disconnected"))
                    :on-error (fn [socket _] (retire! hub socket 1011 "Compute disconnected"))}}))
