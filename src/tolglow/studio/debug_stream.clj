(ns tolglow.studio.debug-stream
  "Bounded Transit event history and opt-in raw universe inspection."
  (:require [ring.websocket :as ws]
            [tolglow.studio.state-stream :as state-stream]
            [tolglow.studio.transit :as wire])
  (:import [java.util Date UUID]
           [java.util.concurrent Semaphore]))

(def protocol "tolglow-debug/1")
(def ^:private history-size 512)
(def ^:private batch-size 64)
(def ^:private max-clients 12)

(defn- named-id [v]
  (when (or (keyword? v) (and (string? v) (<= (count v) 192))) v))

(def ^:private display-fields
  [:id :device-id :source-id :instance-id :cue-id :effect-id :effect-version :param
   :time-ms :observed-at-ms :sequence :confidence :stage :status :operation :reason :code
   :gesture-id :watcher-id :registration-id :active? :armed? :index :crossed :number
   :command-id :note :velocity :beat :rms :energy :relative-energy :kick-rms :inference])

(defn- display-event [stamp subscribers {:keys [time type category data emitter] :as event}]
  ;; Deliberately project metadata only. A cue parameter or handler argument can
  ;; contain arbitrary show data, user-entered source, chat or credentials.
  {:timestamp stamp :time time :category category :type type
   :emitter {:kind (:kind emitter) :id (named-id (:id emitter))}
   :subscribers (when (some? subscribers) (mapv named-id (take 128 subscribers)))
   :data (reduce (fn [out k]
                   (let [v (get data k)]
                     (if (or (number? v) (boolean? v) (named-id v)) (assoc out k v) out))) {} display-fields)})

(defn- wake! [{:keys [^Semaphore wake]}]
  (when (zero? (.availablePermits wake)) (.release wake)))

(defn observing? [hub] (realized? (:worker hub)))

(defn- append-batch [history events skipped]
  (let [start (:high history)
        latest (+ start (count events))
        tail (take-last history-size events)
        first-id (inc (- latest (count tail)))
        retained (into (:rows history)
                       (map-indexed (fn [index event] (assoc event :id (+ first-id index))) tail))]
    {:high (+ latest skipped) :rows (if (> (count retained) history-size)
                                      (into [] (take-last history-size retained))
                                      retained)}))

(defn commit!
  "Called in owner commit order. No networking or encoding on this thread."
  [hub time semantic matches trace]
  (when (observing? hub)
    (when (or (seq semantic) (seq (:events trace)) (pos? (:skipped trace 0)))
      (let [stamp (Date.)
            rows (into (mapv (fn [i event]
                               (display-event stamp (get matches i (:subscribers event)) event))
                             (range) semantic)
                       (map #(display-event stamp (:subscribers %) (assoc % :time time))) (:events trace))]
        (swap! (:history hub) append-batch rows (:skipped trace 0))))
    (when (seq @(:clients hub)) (wake! hub))))

(defn- event-message [hub state]
  (when-not (:events-pending state)
    (let [{:keys [high rows]} @(:history hub)
          after (:after state)]
      (when (or (:initial? state) (< after high))
        (let [events (into [] (comp (drop-while #(<= (:id %) after)) (take batch-size)) rows)
              cursor (if (= batch-size (count events)) (:id (peek events)) high)]
          {:kind :events :cursor cursor :skipped (- cursor after (count events)) :events events})))))

(defn- dmx-message [hub state]
  (when (and (some? (:universe state)) (nil? (:dmx-pending state)))
    (let [{:keys [model output]} ((:frame! hub))
          epoch (:epoch model) frame (:frame-number model 0)
          universe (:universe state) key [epoch frame (:revision model) universe]]
      (when (not= key (:last-dmx state))
        (let [packet (get-in output [:universes universe])]
          [key {:kind :dmx :token (str (UUID/randomUUID)) :epoch epoch :frame-number frame
                :universe universe :bytes (when packet (aclone ^bytes packet))}])))))

(defn- retire! [hub socket code message]
  (when (get (first (swap-vals! (:clients hub) dissoc socket)) socket)
    (try (ws/close socket code message) (catch Exception _))))

(defn- send-next! [hub socket client]
  (let [state @client]
    (when-not (:sending state)
      (let [dmx (delay (dmx-message hub state))
            events (delay (event-message hub state))
            [key message] (if (= :events (:last-kind state))
                            (or @dmx [nil @events])
                            (if-let [msg @events] [nil msg] @dmx))
            kind (:kind message)]
        (when message
          (let [pending (cond-> (assoc state :sending true :last-kind kind)
                          (= kind :events) (assoc :initial? false :events-pending (:cursor message))
                          (= kind :dmx) (assoc :dmx-pending (:token message) :last-dmx key))]
            (when (compare-and-set! client state pending)
              (ws/send socket (wire/encode (assoc message :protocol protocol :runtime-id (:id hub)))
                       (fn [] (swap! client dissoc :sending) (wake! hub))
                       (fn [_] (retire! hub socket 1011 "Debug delivery failed"))))))))))

(defn- run-worker [hub]
  (try
    (while (not @(:closed? hub))
      (.acquire ^Semaphore (:wake hub))
      (.drainPermits ^Semaphore (:wake hub))
      (doseq [[socket client] @(:clients hub)]
        (try (send-next! hub socket client)
             (catch Exception _ (retire! hub socket 1011 "Debug projection failed"))))
      (Thread/sleep 25))
    (catch InterruptedException _)))

(defn create [id frame!]
  (let [hub {:id id :frame! frame! :history (atom {:high 0 :rows []})
             :clients (atom {}) :closed? (atom false) :wake (Semaphore. 0)}]
    (assoc hub :worker (delay (doto (Thread. #(run-worker hub) "tolglow-debug-stream")
                                (.setDaemon true) (.start))))))

(defn close! [hub]
  (locking (:closed? hub)
    (reset! (:closed? hub) true)
    (when (observing? hub) (.interrupt ^Thread @(:worker hub))))
  (doseq [socket (keys @(:clients hub))] (retire! hub socket 1001 "Studio stopped")))

(defn- control! [hub socket text]
  (try
    (when-not (and (string? text) (<= (count text) 1024))
      (throw (ex-info "Expected a bounded control message" {})))
    (let [{:keys [op cursor universe token]} (wire/decode text)]
      (when-let [client (@(:clients hub) socket)]
        (case op
          :ack (swap! client #(if (and (integer? cursor) (= cursor (:events-pending %)))
                                (-> % (assoc :after cursor) (dissoc :events-pending)) %))
          :dmx/subscribe (if (and (integer? universe) (<= 0 universe Integer/MAX_VALUE))
                           (swap! client #(-> % (assoc :universe universe) (dissoc :last-dmx :dmx-pending)))
                           (throw (ex-info "Invalid universe" {})))
          :dmx/unsubscribe (swap! client dissoc :universe :last-dmx :dmx-pending)
          :dmx/ack (swap! client #(if (and (string? token) (= token (:dmx-pending %))) (dissoc % :dmx-pending) %))
          (throw (ex-info "Unknown control" {})))
        (wake! hub)))
    (catch Exception _ (retire! hub socket 1008 "Invalid debug stream control"))))

(defn response [hub request]
  (let [{:strs [runtime after]} (:query-params request)
        since (when after (parse-long after))]
    (cond
      @(:closed? hub) {:status 503 :body "Studio stopped"}
      (not (state-stream/same-origin? request)) {:status 403 :body "Cross-origin debug stream"}
      (not (ws/upgrade-request? request)) {:status 426 :headers {"Upgrade" "websocket"} :body "WebSocket required"}
      (and after (not (and since (<= 0 since)))) {:status 400 :body "Invalid cursor"}
      :else
      {::ws/listener
       {:on-open (fn [socket]
                   (locking (:closed? hub)
                     (if (or @(:closed? hub) (>= (count @(:clients hub)) max-clients))
                       (ws/close socket 1013 "Debug stream unavailable")
                       (let [high (:high @(:history hub))]
                         (swap! (:clients hub) assoc socket
                                (atom {:after (if (and (= runtime (:id hub)) since (<= since high)) since 0)
                                       :initial? true}))
                         (force (:worker hub))
                         (wake! hub)))))
        :on-message #(control! hub %1 %2)
        :on-close (fn [socket _ _] (swap! (:clients hub) dissoc socket))
        :on-error (fn [socket _] (retire! hub socket 1011 "Debug stream closed"))}})))
