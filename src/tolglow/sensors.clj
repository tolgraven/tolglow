(ns tolglow.sensors
  "Owned sensor admission/mailbox resources around immutable model sensor state.
  Pair credentials and sockets stay here; mappings, calibration and progress stay
  in model :sensors. The host commits returned models/commands at its frame boundary."
  (:require [afterglow.control.sensors :as core]
            [afterglow.time :as musical-time]
            [tolglow.agents.credentials :as credentials]
            [tolglow.integrations.audience-invitation :as stage]
            [tolglow.sensors.osc :as osc])
  (:import [java.util UUID]
           [java.security MessageDigest]
           [java.nio.charset StandardCharsets]))

(defn- fail! [code message] (throw (ex-info message {:sensors/error code})))
(defn- identifier? [x] (and (string? x) (<= 1 (count x) 128)))
(defn- finite? [x] (and (number? x) (Double/isFinite (double x))))
(def empty-state {:version 1 :sources {}})

(defn validate-state [state]
  (when-not (and (map? state) (= 1 (:version state)) (= #{:version :sources} (set (keys state)))
                 (map? (:sources state)) (<= (count (:sources state)) 16))
    (fail! :invalid-state "Sensor state needs version1 and at most16 sources"))
  (doseq [[id source] (:sources state)]
    (when-not (and (identifier? id) (map? source)
                   (every? #{:config :pairing :armed-intent? :state} (keys source))
                   (boolean? (:armed-intent? source)) (map? (:pairing source))
                   (every? #{:credential-ref :label} (keys (:pairing source)))
                   (credentials/valid-reference? (get-in source [:pairing :credential-ref]))
                   (or (nil? (get-in source [:pairing :label])) (identifier? (get-in source [:pairing :label])))
                   (= id (get-in source [:state :device-id])))
      (fail! :invalid-state "Sensor source needs logical pairing reference and owned state"))
    (core/validate-config (:config source)) (core/validate-state (:state source)))
  state)

(defn- owned-commands [model device-id session-id commands]
  (mapv (fn [command]
          (let [captured (get-in model [:instances (:instance-id command) :source])
                source (if (and (= :sensor (:adapter captured)) (= device-id (:device-id captured)))
                         (select-keys captured [:adapter :device-id :session-id])
                         {:adapter :sensor :device-id device-id :session-id session-id})]
            (assoc command :source source))) commands))

(defn prepare-restored-model
  "After the host remaps instance IDs, discard unacknowledged old-epoch callbacks.
  Known held IDs remain until first advance! emits exact release intents under
  the replacement manager's disarmed ownership. Calibration/config are preserved."
  [model]
  (validate-state (or (:sensors model) empty-state))
  (if-not (:sensors model) model
      (update-in model [:sensors :sources]
                 (fn [sources]
                   (into {} (map (fn [[id source]]
                                   [id (update-in source [:state :holds :gestures]
                                                  #(into {} (filter (fn [[_ hold]] (:instance-id hold))) %))])) sources)))))

(defn- device-events [id events]
  (mapv #(assoc % :device-id id) events))

(defn configure
  "Pure source admission/update. Return release intents when replacing an active
  mapping. A pairing comes from pair!; no credential secret belongs in this model."
  ([model device-id config] (configure model device-id config (get-in model [:sensors :sources device-id :pairing])))
  ([model device-id config pairing]
   (core/validate-config config)
   (let [existing (get-in model [:sensors :sources device-id :state])
         state (or existing (core/new-state {:device-id device-id :session-id "unobserved"
                                             :meter (get-in model [:transport :meter])}))
         released (core/set-armed state false)
         sensors (assoc-in (or (:sensors model) empty-state) [:sources device-id]
                           {:config config :pairing pairing :armed-intent? false :state (:state released)})]
     (validate-state sensors)
     {:model (assoc model :sensors sensors) :commands (owned-commands model device-id (:session-id state) (:commands released)) :events (device-events device-id (:events released))})))

(defn- live [manager]
  (let [state @(:state manager)] (when (:closed? state) (fail! :closed "Sensor manager is closed")) state))

(declare restore!)
(defn create-manager
  "Inert unless a saved model requests local credential lookup. No sockets or
  physical sources start. :credential-store is the shared lazy local vault.
  Explicit :now-ms is monotonic receipt time, independent of show transport."
  ([] (create-manager {}))
  ([{:keys [credential-store now-ms token-fn queue-limit drain-limit model audience-helper]
     :or {audience-helper (System/getenv "TOLGLOW_AUDIENCE_BRIDGE") now-ms #(/ (System/nanoTime) 1000000.0) token-fn #(str (UUID/randomUUID)) queue-limit 256 drain-limit 16}}]
   (when-not (and (ifn? now-ms) (ifn? token-fn)
                  (integer? queue-limit) (<= 1 queue-limit 1024)
                  (integer? drain-limit) (<= 1 drain-limit 32))
     (fail! :invalid-options "Sensor manager needs bounded queue/drain options"))
   (let [manager {:lock (Object.) :credential-store credential-store :now-ms now-ms :token-fn token-fn
                  :queue-limit queue-limit :drain-limit drain-limit :stage (stage/create audience-helper)
                  :state (atom {:closed? false :generation 0 :devices {} :queue [] :udp nil
                                :metrics {:accepted 0 :rejected 0 :overflow 0}})}]
     (when model (restore! manager model)) manager)))

(defn pair!
  "Explicitly create/replace one pairing credential. Return token once to the
  caller; configure the returned :pairing on the model using configure."
  [manager device-id & [label]]
  (when-not (and (identifier? device-id) (or (nil? label) (identifier? label)))
    (fail! :invalid-pairing "Pairing needs bounded device ID and optional label"))
  (let [store (:credential-store manager)
        generation (locking (:lock manager)
                     (let [state (live manager)]
                       (when (and (not (contains? (:devices state) device-id)) (>= (count (:devices state)) 16))
                         (fail! :full "Sensor source limit reached"))
                       (:generation state)))]
    (when-not (every? ifn? [(:put! store) (:get! store) (:delete! store)])
      (fail! :missing-store "Pairing requires the owned credential store"))
    (let [token ((:token-fn manager))
          _ (when-not (and (string? token) (<= 16 (count token) 512)) (fail! :invalid-token "Pairing token generator returned invalid data"))
          reference ((:put! store) token)
          pairing (cond-> {:credential-ref reference} label (assoc :label label))]
      (when-not (credentials/valid-reference? reference) (fail! :invalid-store "Credential store returned invalid reference"))
      (try
        (locking (:lock manager)
          (let [state (live manager)]
            (when-not (= generation (:generation state)) (fail! :stale-generation "Sensor pairing ownership changed"))
            (when (and (not (contains? (:devices state) device-id)) (>= (count (:devices state)) 16))
              (fail! :full "Sensor source limit reached"))
            (swap! (:state manager)
                   (fn [state] (-> state
                                   (assoc-in [:devices device-id] {:token token :credential-ref reference :armed? false})
                                   (update :queue #(filterv (fn [entry] (not= device-id (get-in entry [:observation :device-id]))) %)))))
            (let [{:keys [devices udp]} @(:state manager)]
              (stage/stop! (:stage manager) device-id)
              (stage/inputs! (:stage manager) devices udp))))
        {:device-id device-id :pairing pairing :pairing-token token}
        (catch Throwable error
          (try ((:delete! store) reference) (catch Throwable cleanup (.addSuppressed error cleanup)))
          (throw error))))))

(defn restore!
  "Load pairing refs before publishing replacements; restored sessions remain
  disarmed. Mutable queues, receiver sockets and old session ownership do not resume."
  [manager model]
  (let [sensors (validate-state (or (:sensors model) empty-state))
        store (:credential-store manager)
        devices (into {} (map (fn [[id source]]
                               (let [reference (get-in source [:pairing :credential-ref])
                                     token (when (ifn? (:get! store)) ((:get! store) reference))]
                                 [id {:credential-ref reference :token token :armed? false}]))) (:sources sensors))
        old (locking (:lock manager)
              (live manager)
              (let [udp (:udp @(:state manager))]
                (stage/stop! (:stage manager))
                (stage/inputs! (:stage manager) devices nil)
                (swap! (:state manager) #(-> % (update :generation inc) (assoc :devices devices :queue [] :udp nil))) udp))]
    (when old ((:close! old)))
    {:restored? true :sources (count devices)}))

(defn- token-matches? [left right]
  (and (string? left) (string? right)
       (MessageDigest/isEqual (.getBytes ^String left StandardCharsets/UTF_8) (.getBytes ^String right StandardCharsets/UTF_8))))

(defn ingest!
  "Authenticate and enqueue one observation. Missing/retired ownership or a stale
  sequence is rejected without exposing token data. No show command executes here."
  [manager observation]
  (let [observation (osc/validate-observation observation)
        now ((:now-ms manager)) id (:device-id observation) session (:session-id observation)]
    (when-not (finite? now) (fail! :invalid-clock "Receipt clock must be finite"))
    (locking (:lock manager)
      (let [state (live manager) device (get-in state [:devices id])
            same? (= session (:session-id device))
            reason (cond
                     (not (token-matches? (:token device) (:pairing-token observation))) :unauthorized
                     (and (:armed? device) (not same?)) :owned-session
                     (and same? (<= (:sample-id observation) (:last-sample-id device -1))) :stale-sample
                     (and same? (< (:time-ms observation) (:device-time-ms device 0))) :stale-device-time
                     :else nil)]
        (if reason
          (do (swap! (:state manager) update-in [:metrics :rejected] inc) {:accepted? false :reason reason})
          (let [safe (dissoc observation :pairing-token)
                overflow? (>= (count (:queue state)) (:queue-limit manager))
                next (-> (cond-> state overflow? (update :devices #(into {} (map (fn [[key value]] [key (assoc value :armed? false)])) %)))
                         (assoc-in [:devices id] (merge device {:session-id session :last-sample-id (:sample-id observation)
                                                               :device-time-ms (:time-ms observation) :received-at-ms now
                                                               :latest safe :armed? (and same? (:armed? device) (not overflow?))}))
                         (update-in [:metrics :accepted] inc)
                         (cond-> overflow? (update-in [:metrics :overflow] inc))
                         (update :queue #(conj (if overflow? [] %) {:observation safe :received-at-ms now})))]
            (reset! (:state manager) next)
            {:accepted? true :overflow? overflow?}))))))

(defn status [manager]
  (let [state @(:state manager)]
    {:closed? (:closed? state) :generation (:generation state) :queued (count (:queue state)) :metrics (:metrics state)
     :udp (when-let [udp (:udp state)] {:port (:port udp) :status ((:status udp))})
     :devices (into {} (map (fn [[id device]] [id (assoc (select-keys device [:session-id :armed? :last-sample-id :received-at-ms])
                                                        :credential-ready? (boolean (:token device)))])) (:devices state))}))

(defn latest [manager device-id] (get-in (live manager) [:devices device-id :latest]))

(defn arm!
  "Select an already observed session explicitly. Show model and returned intents
  must be committed by the caller; a runtime arm flag alone never enables output."
  [manager model device-id session-id armed?]
  (when-not (boolean? armed?) (fail! :invalid-armed "Armed must be boolean"))
  (let [source (get-in model [:sensors :sources device-id])]
    (when-not source (fail! :unknown-source "Configure the paired source first"))
    (locking (:lock manager)
      (let [device (get-in (live manager) [:devices device-id])
            session-id (or session-id (:session-id device) (get-in source [:state :session-id]))]
        (when (and armed? (or (not= session-id (:session-id device)) (nil? (:latest device))
                             (> (- ((:now-ms manager)) (:received-at-ms device)) (get-in source [:config :timeout-ms] 500))))
          (fail! :unobserved-session "Arming needs a fresh observed session"))
        (let [old (:state source)
              released (if (= session-id (:session-id old)) {:state old :commands [] :events []}
                           (core/set-armed old false))
              state (if (= session-id (:session-id old)) (:state released)
                        (assoc (core/new-state {:device-id device-id :session-id session-id
                                               :meter (get-in model [:transport :meter])})
                               :baseline (:baseline old) :holds (get-in released [:state :holds])
                               :next-gesture (:next-gesture old)))
              state (cond-> state armed? (assoc :last-sample-id (:last-sample-id device)
                                                :last-device-time-ms (:device-time-ms device)
                                                :last-time-ms (:time-ms model) :last-beat (get-in model [:transport :beat] 0)))
              armed (core/set-armed state armed?)]
          (swap! (:state manager) #(-> % (assoc-in [:devices device-id :armed?] armed?)
                                        (update :queue (fn [queue] (filterv (fn [entry] (not= device-id (get-in entry [:observation :device-id]))) queue)))))
          {:model (-> model (assoc-in [:sensors :sources device-id :state] (:state armed))
                      (assoc-in [:sensors :sources device-id :armed-intent?] armed?))
           :commands (into (owned-commands model device-id (:session-id old) (:commands released))
                           (owned-commands model device-id session-id (:commands armed)))
           :events (device-events device-id (into (vec (:events released)) (:events armed)))})))))

(defn calibrate [manager model device-id]
  (let [source (get-in model [:sensors :sources device-id]), observation (latest manager device-id)]
    (when-not (and source observation) (fail! :unobserved-session "Calibration needs observed data"))
    (when (:armed-intent? source) (fail! :armed "Disarm before calibration"))
    (let [state (assoc (core/new-state {:device-id device-id :session-id (:session-id observation)
                                       :meter (get-in model [:transport :meter])})
                       :baseline (:values observation) :holds (get-in source [:state :holds])
                       :next-gesture (get-in source [:state :next-gesture]))]
      {:model (assoc-in model [:sensors :sources device-id :state] state) :commands [] :events [{:type :sensor/calibrated :device-id device-id}]})))

(defn acknowledge [model device-id gesture-id ack]
  (if-let [state (get-in model [:sensors :sources device-id :state])]
    (let [result (core/acknowledge state gesture-id ack)]
      {:model (assoc-in model [:sensors :sources device-id :state] (:state result))
       :commands (owned-commands model device-id (:session-id state) (:commands result)) :events (device-events device-id (:events result))})
    {:model model :commands [] :events []}))

(defn advance!
  "Drain bounded observations into a pure model using authoritative frame clocks.
  Returns semantic commands/events; host stamps/reduces them and feeds actual held
  launch receipts through acknowledge. No command callback or hardware IO runs here."
  [manager model clock]
  (validate-state (or (:sensors model) empty-state))
  (let [clock (musical-time/frame-clock clock)
        _ (when-not (and (finite? (:time-ms clock)) (<= 0 (:time-ms clock)) (finite? (:beat clock)))
            (fail! :invalid-clock "Sensor admission needs authoritative time and beat"))
        _ (musical-time/normalize-meter (:meter clock))
        now ((:now-ms manager))
        {:keys [devices packets]}
        (locking (:lock manager)
          (let [state (live manager) n (min (:drain-limit manager) (count (:queue state)))
                packets (subvec (:queue state) 0 n)]
            (swap! (:state manager) update :queue #(subvec % n))
            {:devices (:devices state) :packets packets}))
        initial
        (reduce-kv
         (fn [acc id source]
           (let [state (:state source) device (devices id)
                 permitted? (and (:armed? device) (:armed-intent? source) (= (:session-id device) (:session-id state)))
                 result (if (and (:armed? state) (not permitted?)) (core/set-armed state false)
                            {:state state :commands [] :events []})]
             (-> acc (assoc-in [:model :sensors :sources id :state] (:state result))
                 (update :commands into (owned-commands (:model acc) id (:session-id state) (:commands result)))
                 (update :events into (device-events id (:events result))))))
         {:model model :commands [] :events []} (get-in model [:sensors :sources] {}))]
    (let [result
          (reduce
     (fn [acc {:keys [observation received-at-ms]}]
       (let [id (:device-id observation) source (get-in acc [:model :sensors :sources id]) device (devices id)]
         (if (or (nil? source) (not= (:session-id observation) (:session-id device))
                 (> (- now received-at-ms) (get-in source [:config :timeout-ms] 500))) acc
             (let [old (:state source)
                   new-session? (not= (:session-id old) (:session-id observation))
                   released (if new-session? (core/set-armed old false) {:state old :commands [] :events []})
                   state (if new-session?
                           (assoc (core/new-state (merge observation {:meter (:meter clock)})) :baseline (:baseline old)
                                  :holds (get-in released [:state :holds]) :next-gesture (:next-gesture old))
                           (:state released))
                   sample (-> observation (assoc :device-time-ms (:time-ms observation))
                              (merge (select-keys clock [:time-ms :beat :bpm :meter :playing?])))
                   result (try (core/step state (:config source) sample)
                               (catch Exception _
                                 (update (core/set-armed state false) :events conj
                                         {:type :sensor/input-error :device-id id :reason :invalid-conditioned-value})))]
               (-> acc (assoc-in [:model :sensors :sources id :state] (:state result))
                   (update :commands into (owned-commands (:model acc) id (:session-id old) (:commands released)))
                   (update :commands into (owned-commands (:model acc) id (:session-id state) (:commands result)))
                   (update :events into (device-events id (concat (:events released) (:events result))))))))) initial packets)
          result (reduce-kv
                  (fn [acc id source]
                    (let [expired (core/expire (:state source) (:config source) clock)]
                      (-> acc (assoc-in [:model :sensors :sources id :state] (:state expired))
                          (update :commands into (owned-commands (:model acc) id (get-in source [:state :session-id]) (:commands expired)))
                          (update :events into (device-events id (:events expired))))))
                  result (get-in result [:model :sensors :sources] {}))]
      (locking (:lock manager)
        (when-not (:closed? @(:state manager))
          (doseq [[id source] (get-in result [:model :sensors :sources])
                  :when (not (get-in source [:state :armed?]))]
            (when (= (get-in source [:state :session-id]) (get-in @(:state manager) [:devices id :session-id]))
              (swap! (:state manager) assoc-in [:devices id :armed?] false)))))
      result)))

(defn start-osc! [manager options]
  (let [generation (:generation (live manager))
        receiver (osc/open! (assoc options :receive! #(when (= generation (:generation @(:state manager))) (ingest! manager %))))]
    (try (locking (:lock manager)
           (let [state (live manager)]
             (when (or (:udp state) (not= generation (:generation state))) (fail! :stale-resource "Receiver ownership changed"))
             (swap! (:state manager) assoc :udp receiver)
             (stage/inputs! (:stage manager) (:devices state) receiver)))
         receiver
         (catch Throwable error ((:close! receiver)) (throw error)))))

(defn stop-osc! [manager]
  (let [receiver (locking (:lock manager)
                   (let [{:keys [udp devices]} (live manager)]
                     (stage/stop! (:stage manager))
                     (stage/inputs! (:stage manager) devices nil)
                     (swap! (:state manager) assoc :udp nil)
                     udp))]
    (when receiver ((:close! receiver))) {:stopped? true}))

(defn start-stage! [manager options]
  (locking (:lock manager)
    (let [{:keys [devices udp]} (live manager)
          device (get devices (:device-id options))]
      (when (:armed? device) (fail! :armed "Disarm this sensor before starting its stage."))
      (stage/start! (:stage manager) options udp (:token device)))))

(defn export-state [model] (validate-state (or (:sensors model) empty-state)))
(defn close! [manager]
  (let [receiver (locking (:lock manager)
                   (let [state @(:state manager)]
                     (when-not (:closed? state)
                       (stage/stop! (:stage manager))
                       (stage/inputs! (:stage manager) {} nil)
                       (swap! (:state manager) #(-> % (update :generation inc) (assoc :closed? true :devices {} :queue [] :udp nil)))
                       (:udp state))))]
    (when receiver ((:close! receiver))) {:closed? true}))
