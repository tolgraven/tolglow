(ns tolglow.integrations.live-sync
  "Live clip/control adapter for a per-show Afterglow synchronization session."
  (:require [afterglow.sync :as sync]
            [afterglow.sync.config :as config]
            [afterglow.sync.session :refer [fail! error-data emit! apply-clock! commands! run-callbacks
                                            enqueue-io! queue! ingest!]]
            [afterglow.sync.midi :as midi]
            [afterglow.time :refer [finite-number?] :rename {finite-number? finite?}]
            [tolglow.integrations.ableton :as ableton]
            [tolglow.integrations.live-parameters :as live-params]
            [tolglow.integrations.live-sync-config :as settings]
            [tolglow.integrations.live-transport :as transport])
  (:import [java.util UUID]))

(defn- bridge [config]
  (ableton/new-bridge (assoc (select-keys (:live config) [:mappings :parameters :channel :mode :launch-grid]) :id :live)))

(defn parameter-routing
  "Snapshot destinations currently owned by a running Live or mapped MIDI input."
  [manager]
  (let [{:keys [active status config bridge]} @(:state manager)]
    (when (and (= :running status) active (= :lighting (:mode bridge)) (seq (:parameters bridge)))
      (let [resources @(:resources active)
            ports (cond-> []
                    (:live resources) (conj (get-in config [:live :source-id]))
                    (and (:midi resources) (get-in config [:midi :control?])) (conj (get-in config [:midi :source-id])))]
        (when (seq ports)
          {:parameters (:parameters bridge) :channel (:channel bridge)
           :bridge-id (get-in config [:live :bridge-id]) :source-ids ports})))))

(defn ingest-live!
  "Admit a companion bridge callback only for the configured bridge and current
  explicit run handshake. Queued callbacks from an older show/run cannot become
  commands in a new run. This is logical ownership, not network authentication.
  Return false for stale/duplicate/unconnected callbacks or full input queues."
  [manager payload]
  (let [run (:active @(:state manager)) config (get-in @(:state manager) [:config :live])
        kind (when (or (keyword? (:kind payload)) (string? (:kind payload))) (keyword (name (:kind payload))))
        valid? (and run (= 1 (:protocol-version payload)) (:enabled? config)
                    (:live @(:resources run)) (= (:bridge-id config) (:bridge-id payload))
                    (= (:id run) (:sync-session-id payload))
                    (string? (:event-id payload)) (<= 1 (count (:event-id payload)) 256)
                    (case kind :status (map? (:status payload)) (:midi :run-clip :parameter-device) (map? (:event payload))
                          (:parameter :parameter-error) (map? (:sample payload))
                          :handoff (#{:live :lighting "live" "lighting"} (:mode payload)) false))]
    (if-not valid? false
      (let [event (case kind
                    :status {:type :live/status :status (:status payload) :time-ms ((get-in manager [:options :now-ms]))}
                    :midi {:type :live/midi :time-ms ((get-in manager [:options :now-ms]))
                           :event (-> (:event payload) (update :type #(if (string? %) (keyword %) %))
                                                                    (update :origin #(if (string? %) (keyword %) %)))}
                    ;; Run-track playback (#683): decoded by the show's run-clip player.
                    :run-clip {:type :live/run-clip :time-ms ((get-in manager [:options :now-ms]))
                               :event (update (:event payload) :watch-session #(or % (:sync-session-id payload)))}
                    ;; Parameter-device slot values (#625): decoded by the show's
                    ;; live-devices owner step.
                    :parameter-device {:type :live/parameter-device :time-ms ((get-in manager [:options :now-ms]))
                                       :event (update (:event payload) :watch-session #(or % (:sync-session-id payload)))}
                    (:parameter :parameter-error) {:type (if (= kind :parameter) :live/parameter :live/parameter-error)
                                :sample (:sample payload) :event-id (:event-id payload)
                                :time-ms ((get-in manager [:options :now-ms]))}
                    :handoff (when-let [envelope! (:envelope! (run-callbacks run))]
                               {:type :live/handoff :mode (some-> (:mode payload) name keyword) :envelope (envelope!)}))]
        (locking (:lock manager)
          (let [seen @(:callback-ids run) id (:event-id payload)]
            (if (or (nil? event) (some #{id} seen) (not (identical? run (:active @(:state manager))))) false
                (if (queue! manager run event)
                  (do (reset! (:callback-ids run) (vec (take-last 256 (conj seen id)))) true)
                  false))))))))

(defn record! [manager command acknowledgement]
  (ingest! manager {:type :record/command :command command :ack acknowledgement}))

(defn handoff!
  "Hand the surface to mode (:live or :lighting): the bridge releases held
  activations and tells the companion. With request-id, the companion's
  receipt for that handoff is kept for handoff-receipt."
  ([manager mode envelope] (handoff! manager mode envelope nil))
  ([manager mode envelope request-id]
   (ingest! manager (cond-> {:type :live/handoff :mode (if (string? mode) (keyword mode) mode) :envelope envelope}
                      request-id (assoc :request-id request-id)))))

(defn- remember-handoff! [manager id receipt]
  (swap! (:state manager) update :handoff-receipts #(vec (take-last 16 (conj (or % []) [id receipt])))))

(defn companion-capture
  "What the companion said about its capture host in its last status:
  {:capture-host track-id-or-nil :capture-playing? bool}, or nil before a
  status that carries it. A companion that reloaded has forgotten its host
  until setup runs again (#680)."
  [manager]
  (let [status (:recording @(:state manager))]
    (when (contains? status :capture-host)
      {:capture-host (:capture-host status) :capture-playing? (true? (:capture-playing? status))})))

(defn handoff-receipt
  "The companion's receipt for a handoff! request-id, or nil while none has
  arrived. The last 16 are kept."
  [manager request-id]
  (some (fn [[id receipt]] (when (= id request-id) receipt)) (:handoff-receipts @(:state manager))))
(declare receive-ack!)
(defn acknowledge! [manager note acknowledgement envelope]
  (receive-ack! manager note acknowledgement envelope))

(defn- bridge-step! [manager step]
  (locking (:lock manager)
    (let [result (step (:bridge @(:state manager)))]
      (swap! (:state manager) assoc :bridge (:state result)) result)))

(defn- receive-ack! [manager note acknowledgement envelope]
  (let [resolved
        (locking (:lock manager)
          (let [{:keys [active bridge]} @(:state manager)
                {:keys [command-id sync-run-id]} acknowledgement]
            (when (and active (or (nil? sync-run-id) (= (:id active) sync-run-id))
                       (ableton/awaiting-launch? bridge note command-id))
              (let [result (ableton/acknowledge-launch bridge note acknowledgement envelope)]
                (swap! (:state manager) assoc :bridge (:state result))
                [active (:commands result)]))))]
    (when resolved (commands! manager (first resolved) (second resolved)))
    (boolean resolved)))

(defn- release-held! [manager run]
  (when-let [envelope! (:envelope! (run-callbacks run))]
    (try
      (let [envelope (envelope!)
            result (locking (:lock manager)
                     (let [state @(:state manager) result (ableton/handoff (:bridge state) :live envelope)]
                       (swap! (:state manager) assoc :bridge (bridge (:config state)))
                       result))]
        (commands! manager run (:commands result)))
      (catch Exception error (emit! manager run {:type :sync/error :error (error-data error)})))))

(defn- live-request! [manager run id op payload]
  (let [{:keys [transport handle]} (:live @(:resources run))]
    (if-not transport
      (do (emit! manager run {:type :live/execution :id id :op op :status :rejected :error {:code :not-connected}}) false)
      (enqueue-io! manager run
                   (fn []
                     (let [start ((get-in manager [:options :now-ms]))
                           status-version @(:status-version run)
                           response (try ((:request! transport) handle id op
                                          (cond-> payload (= op :inspect)
                                            (assoc :sync-session-id (:id run)
                                                   :expected-bridge-id (get-in @(:state manager) [:config :live :bridge-id]))))
                                         (catch Exception error {:id id :status :indeterminate :error (error-data error)}))
                           end ((get-in manager [:options :now-ms]))]
                       (queue! manager run {:type :live/response :op op :response response :payload payload
                                            :time-ms (/ (+ start end) 2.0) :received-at-ms end :round-trip-ms (- end start)
                                            :status-version status-version})
                       (when (= op :inspect) (reset! (:poll-pending? run) false))))))))

(defn bridge-connected?
  "Has the running Live bridge opened its companion connection? Until then,
  companion-transport refuses operations as :not-connected."
  [manager]
  (let [{:keys [active]} @(:state manager)]
    (boolean (and active (get-in @(:resources active) [:live :transport])))))

(defn companion-transport
  "A live-connection transport ({:open! :request! :close!}) that sends each
  operation through the running Live bridge's own companion connection, under
  the bridge's sync session and in order with its status polls and recording
  writes. The companion accepts operations only from the session of its last
  inspect handshake, so a second connection with its own session would take
  the companion's callbacks from the bridge, and the bridge's next poll would
  take them back between that connection's operations. Each request blocks the
  calling thread until its receipt (never call it from the show owner).
  Refused as never sent: without a running bridge (:not-connected), with a
  full queue, or when the bridge stops or rebinds before the operation left
  its queue (:not-sent, reported at once). No receipt within the wait after
  the operation started is indeterminate."
  [manager]
  {:open! (fn [_] :live-bridge)
   :close! (fn [_] nil)
   :request! (fn [_ id op payload]
               (let [{:keys [active config]} @(:state manager)
                     {:keys [transport handle]} (when active (:live @(:resources active)))
                     reply (promise)
                     ;; :queued until the IO thread claims it (:running) or the
                     ;; waiter gives it up (:abandoned); exactly one wins.
                     claim (atom :queued)
                     generation (some-> active :binding-generation deref)
                     wait-ms (+ 1000 (* 4 (get-in config [:live :timeout-ms] 2000)))
                     not-sent (fn [message] (ex-info message {:live/not-sent? true :live/code :not-sent}))
                     dropped? (fn []
                                (let [^java.util.concurrent.ExecutorService io (:io active)]
                                  (or (not (identical? active (:active @(:state manager))))
                                      (not= generation @(:binding-generation active))
                                      (and io (.isShutdown io)))))]
                 (when-not transport
                   (throw (ex-info "Connect the Live bridge first" {:live/not-sent? true})))
                 (when-not (enqueue-io! manager active
                                        (fn [] (when (compare-and-set! claim :queued :running)
                                                 (deliver reply (try ((:request! transport) handle id op payload)
                                                                     (catch Exception error error))))))
                   (throw (ex-info "The Live bridge's request queue is full" {:live/not-sent? true})))
                 (loop [waited 0]
                   (let [result (deref reply 25 ::waiting)]
                     (cond
                       (instance? Exception result) (throw result)
                       (not= ::waiting result) result
                       (and (= :queued @claim) (dropped?) (compare-and-set! claim :queued :abandoned))
                       (throw (not-sent "The Live bridge stopped or rebound before sending the operation"))
                       (>= waited wait-ms)
                       (throw (if (compare-and-set! claim :queued :abandoned)
                                (not-sent "The Live bridge did not reach the operation in time; it was not sent")
                                (ex-info "No receipt from the Live bridge in time" {:wait-ms wait-ms})))
                       :else (recur (+ waited 25)))))))})

(defn edit-observed-parameter! [manager op id]
  (when-not (live-params/valid-id? id)
    (fail! :invalid-live-parameter-id "Use a unique name of up to 64 letters, numbers, dots, hyphens or underscores"))
  (let [{:keys [active config]} @(:state manager)
        table (get-in config [:live :observed-parameters])]
    (when-not active (fail! :not-connected "Start the Live connection before changing observed sources"))
    (case op
      :learn-parameter
      (do (when (or (contains? table id) (>= (count table) live-params/max-parameters))
            (fail! :parameter-in-use "Choose an unused source name or forget an existing parameter"))
          (live-request! manager active (str (UUID/randomUUID)) :describe-selected-parameter {:id id}))
      :forget-parameter (queue! manager active {:type :live/forget-parameter :id id}))))

(defn- update-parameter-watchers! [manager run]
  (let [table (get-in @(:state manager) [:config :live :observed-parameters])
        generation (str (UUID/randomUUID))]
    (reset! (:parameter-generation run) generation)
    (swap! (:state manager) assoc :parameter-watch nil)
    (live-request! manager run generation :watch-parameters {:sources table :generation generation})))

(defn place-in-arrangement! [manager beat request-id]
  (when-not (and (finite? beat) (<= 0 beat) (string? request-id) (<= 1 (count request-id) 128))
    (fail! :invalid-placement "Placement requires a nonnegative song beat and a stable request ID"))
  (let [{:keys [active recording]} @(:state manager)]
    (when-not (and active (:destination-id recording)) (fail! :unpaired "Pair a MIDI clip before placing it"))
    (live-request! manager active request-id :place-in-arrangement
                   {:destination-id (:destination-id recording) :song-beat beat})))

(defn- clip-beat-at [{:keys [beat loop-start loop-end looping?]} delta]
  (when (finite? beat)
    (let [position (+ beat delta)]
      (if (and looping? (every? finite? [loop-start loop-end]) (< loop-start loop-end)
               (or (and (< beat loop-end) (<= loop-end position))
                   (and (<= loop-start beat) (< beat loop-end) (< position loop-start))))
        (+ loop-start (mod (- position loop-start) (- loop-end loop-start)))
        position))))

(defn- recording-sample [latest previous at-ms]
  (when (and latest (every? latest [:track-armed? :recording? :writable?]))
    (if (and previous (< at-ms (:sampled-at-ms latest)))
      (when (= (:destination-id previous) (:destination-id latest)) previous)
      latest)))

(defn- recording-at [latest previous at-ms freshness-ms]
  (when-let [sample (recording-sample latest previous at-ms)]
    (let [elapsed (- at-ms (:sampled-at-ms sample))
          {:keys [playing? bpm]} (:clock sample)]
      (when (and (<= (Math/abs (double elapsed)) freshness-ms)
                 (<= (Math/abs (double (- at-ms (:sampled-at-ms latest)))) freshness-ms))
        (let [delta (if playing? (* (/ elapsed 60000.0) (or bpm 120)) 0.0)]
          (cond-> (assoc sample :beat (clip-beat-at sample delta))
            (finite? (:song-beat sample)) (update :song-beat + delta)))))))

(defn- forward-jump? [previous current]
  (when (and previous current (<= (:at-ms previous) (:at-ms current)))
    (let [elapsed-ms (+ (- (:at-ms current) (:at-ms previous))
                        (/ (+ (:round-trip-ms previous) (:round-trip-ms current)) 2.0))
          ;; LOM Song.tempo can automate anywhere in 20–999 BPM between polls.
          ;; Use the full range and allow a quarter beat of measurement error.
          max-progress (+ 1/4 (* 999 (/ elapsed-ms 60000.0)))]
      (> (- (:beat current) (:beat previous)) max-progress))))

(defn- process-status! [manager run result at-ms round-trip-ms polled?]
  (let [config (:config @(:state manager)) source (get-in config [:live :source-id])
        transport (:clock result)
        recording (assoc result :sampled-at-ms at-ms :round-trip-ms round-trip-ms)]
    (when-not (and (map? result) (finite? at-ms) (<= 0 at-ms)
                   (finite? round-trip-ms) (<= 0 round-trip-ms)
                   (every? #(or (nil? %) (finite? %)) ((juxt :beat :song-beat :loop-start :loop-end) result))
                   (or (nil? transport) (and (map? transport) (finite? (:bpm transport)) (pos? (:bpm transport)))))
      (fail! :invalid-live-status "Invalid Live status timing"))
    (when transport
      (let [sample (when (and polled? (finite? (:beat transport)))
                     {:at-ms at-ms :round-trip-ms round-trip-ms :beat (:beat transport)})
            previous @(:polled-position run)
            clock (merge transport {:type :clock :kind :ableton-live :source-id source :time-ms at-ms
                                    :confidence (max 0.0 (- 1.0 (/ round-trip-ms (/ 60000.0 (:bpm transport 120)))))})]
        (apply-clock! manager run (cond-> clock (forward-jump? previous sample) (assoc :discontinuity? true)))
        (when (or sample (:discontinuity? transport))
          (reset! (:polled-position run) sample))))
    (when (and (:mode result) (compare-and-set! (:setup-mode? run) false true)
               (not= (keyword (:mode result)) (get-in config [:live :mode])))
      (live-request! manager run (str (UUID/randomUUID)) :handoff {:mode (get-in config [:live :mode])}))
    (when-let [rack (get-in result [:destination :rack-device-index])]
      (let [pair [(:destination-id result) rack]]
        (when (not= pair @(:mirrored-pair run))
          (when-let [pads (:pads (run-callbacks run))]
            (doseq [page (partition-all 64 (pads (get-in config [:live :mappings])))]
              (live-request! manager run (str (UUID/randomUUID)) :pad-page {:pads (vec page)})))
          (reset! (:mirrored-pair run) pair))))
    (when (and (true? (get-in result [:capabilities :observe-parameters?]))
               (compare-and-set! (:setup-watchers? run) false true))
      (update-parameter-watchers! manager run))
    (swap! (:state manager) (fn [state] (assoc state :previous-recording (:recording state) :recording recording)))
    (emit! manager run {:type :live/status :status recording})))

(defn- midi-control! [manager run event]
  (let [state @(:state manager) callbacks (run-callbacks run) envelope (when-let [provider (:envelope! callbacks)] (provider))]
    (when envelope
      (let [controller (ableton/parameter-controller (:bridge state) event)
            mapping (get-in state [:bridge :parameters controller])
            instance (when (and mapping (not (:disabled? mapping)) (:resolve-instance! callbacks))
                       ((:resolve-instance! callbacks) (:cue-id mapping)))
            source (cond-> {:sync-run-id (:id run) :source-id (:source-id event)}
                     (contains? event :note) (assoc :note (:note event))
                     (and (= :note-on (:type event)) (pos? (or (:velocity event) 0))) (assoc :velocity (:velocity event)))
            result (bridge-step! manager #(ableton/midi->commands % event (assoc envelope :source source) instance))]
        (doseq [notice (:events result)] (emit! manager run notice))
        (commands! manager run (:commands result))))))

(defn- surface-changed! [run mode]
  (when-let [changed! (:surface-mode! (run-callbacks run))] (changed! mode)))

(defn- receive-handoff! [manager run {:keys [mode envelope request-id]}]
  (let [result (bridge-step! manager #(ableton/handoff % mode envelope))]
    (swap! (:state manager) assoc-in [:config :live :mode] mode)
    (commands! manager run (:commands result))
    (doseq [event (:events result)] (emit! manager run event))
    (surface-changed! run mode)
    (let [id (or request-id (str (UUID/randomUUID)))]
      ;; Only the receipt of the latest handoff may roll the surface back.
      (swap! (:state manager) assoc :last-handoff-id id)
      (when-not (and (:live @(:resources run))
                     (live-request! manager run id :handoff {:mode mode}))
        (when request-id (remember-handoff! manager request-id {:id request-id :status :rejected :error {:code :not-connected}}))))))

(defn- receive-recording! [manager run {:keys [command ack]} at]
  (let [{:keys [config recording previous-recording]} @(:state manager)
        {:keys [record-route poll-ms]} (:live config)
        observed (recording-at recording previous-recording (get command :time-ms at) (max 1500 (* 3 poll-ms)))
        plan (bridge-step! manager #(ableton/record-command % command ack observed))]
    (when (seq (if (= :midi record-route) (:midi plan) (:intents plan)))
      (enqueue-io!
       manager run
       (fn []
         (let [result (try (transport/execute-plan! @(:resources run) record-route plan observed command ack)
                           (catch Exception error
                             {:status :indeterminate :error (error-data error) :command-id (:command-id command)}))]
           (queue! manager run {:type :live/executed :result result})))))))

(defn- remember-learned-parameter! [manager run id descriptor]
  (let [spec (live-params/descriptor descriptor)
        sources (get-in @(:state manager) [:config :live :observed-parameters])]
    (when (and (not (contains? sources id)) (< (count sources) live-params/max-parameters))
      (swap! (:state manager) assoc-in [:config :live :observed-parameters id] spec)
      (update-parameter-watchers! manager run))))

(defn- receive-parameter! [manager run {:keys [type event-id time-ms sample]}]
  (let [live (get-in @(:state manager) [:config :live])
        id (live-params/wire-id (:id sample))
        generation @(:parameter-generation run)]
    (when (and generation (= generation (:generation sample))
               (contains? (:observed-parameters live) id))
      (if (= :live/parameter type)
        (when-let [observation (live-params/observation live event-id time-ms sample)]
          (emit! manager run observation))
        (when-let [reason (and (string? (:reason sample)) (:reason sample))]
          (let [diagnostic {:id id :reason (subs reason 0 (min 512 (count reason)))}]
            (swap! (:state manager) update-in [:parameter-watch :skipped] (fnil conj []) diagnostic)
            (emit! manager run (assoc diagnostic :type :live/parameter-lost))))))))

(defn- receive-response! [manager run {:keys [op response status-version time-ms round-trip-ms payload]}]
  (when (and (= :executed (:status response)) (#{:inspect :configure-destination} op))
    (if (= status-version @(:status-version run))
      (process-status! manager run (:result response) time-ms round-trip-ms true)
      (swap! (:state manager) update-in [:metrics :stale-live-statuses] (fnil inc 0))))
  (when (= :executed (:status response))
    (case op
      :watch-parameters (when (= @(:parameter-generation run) (:id response))
                          (swap! (:state manager) assoc :parameter-watch (:result response)))
      :describe-selected-parameter (remember-learned-parameter! manager run (:id payload) (:result response))
      nil))
  (when (= :handoff op)
    (let [kw #(if (string? %) (keyword %) %)
          response (cond-> (update response :status kw) (get-in response [:error :code]) (update-in [:error :code] kw))]
    (remember-handoff! manager (:id response) response)
    ;; The companion refused or lost a handoff to Live: Tolglow takes the
    ;; surface back, so both sides agree on lighting. A lost one also tells
    ;; the companion, once.
    (when (and (not= :executed (:status response)) (= :live (some-> (:mode payload) name keyword))
               (= :live (get-in @(:state manager) [:config :live :mode]))
               (= (:id response) (:last-handoff-id @(:state manager))))
      (when-let [envelope (some-> (:envelope! (run-callbacks run)) (apply []))]
        (let [result (bridge-step! manager #(ableton/handoff % :lighting envelope))]
          (commands! manager run (:commands result))
          (doseq [event (:events result)] (emit! manager run event))))
      (swap! (:state manager) assoc-in [:config :live :mode] :lighting)
      (surface-changed! run :lighting)
      (when (= :indeterminate (:status response))
        (live-request! manager run (str (UUID/randomUUID)) :handoff {:mode :lighting})))))
  (when (#{:place-in-arrangement :describe-selected-parameter :watch-parameters} op)
    (swap! (:state manager) assoc :last-execution response))
  (if (= :inspect op)
    (when-not (= :executed (:status response))
      (swap! (:state manager) assoc :last-error (:error response)))
    (emit! manager run (assoc response :type :live/execution :op op))))

(defn- receive! [manager run event at]
  (case (:type event)
    :midi/control (midi-control! manager run (update (:event event) :time-ms #(or % at)))
    :live/status (do (process-status! manager run (:status event) (get event :time-ms at) (get event :round-trip-ms 0.0) false)
                     (swap! (:status-version run) inc))
    (:live/parameter :live/parameter-error) (receive-parameter! manager run event)
    :live/forget-parameter (do (swap! (:state manager) update-in [:config :live :observed-parameters] dissoc (:id event))
                               (update-parameter-watchers! manager run))
    :live/midi (let [at (:time-ms event at)
                     message (assoc (:event event) :source-id (get-in @(:state manager) [:config :live :source-id]) :time-ms at)
                     capture-playback! (:capture-playback! (run-callbacks run))]
                 (when-not (= :tolglow-feedback (:origin message))
                   (midi/observe-midi! manager run message :live :live-playback at))
                 ;; Clip playback from a capture track (#680) belongs to the
                 ;; show's capture decoder, never also to the pad mappings.
                 (when-not (and capture-playback! (true? (capture-playback! message)))
                   (midi-control! manager run message)))
    :live/run-clip (when-let [run-clip! (:run-clip! (run-callbacks run))] (run-clip! (:event event)))
    :live/parameter-device (when-let [device! (:parameter-device! (run-callbacks run))] (device! (:event event)))
    :live/acknowledgement (receive-ack! manager (:note event) (:ack event) (:envelope event))
    :live/handoff (receive-handoff! manager run event)
    :record/command (receive-recording! manager run event at)
    :live/executed (do (swap! (:state manager) assoc :last-execution (:result event))
                       (emit! manager run (assoc (:result event) :type :live/execution)))
    :live/response (receive-response! manager run event)))


(defn- prepare-state [state _phase]
  (assoc state :bridge (bridge (:config state)) :recording nil :previous-recording nil :parameter-watch nil))

(defn- run-state []
  {:status-version (atom 0) :parameter-generation (atom nil) :setup-watchers? (atom false) :setup-mode? (atom false) :mirrored-pair (atom nil)
   :poll-pending? (atom false) :next-poll (atom 0) :callback-ids (atom []) :polled-position (atom nil)})

(defn- tick! [manager run now]
  (when (and (:live @(:resources run)) (>= now @(:next-poll run))
             (compare-and-set! (:poll-pending? run) false true))
    (reset! (:next-poll run) (+ now (get-in @(:state manager) [:config :live :poll-ms])))
    (when-not (live-request! manager run (str (UUID/randomUUID)) :inspect {})
      (reset! (:poll-pending? run) false))))

(defn- lifecycle! [manager run phase]
  (case phase
    :opened (when (:live @(:resources run))
              (tick! manager run ((get-in manager [:options :now-ms])))
              (when-let [pairing (get-in @(:state manager) [:config :live :pairing])]
                (live-request! manager run (str (UUID/randomUUID)) :configure-destination pairing)))
    :rebind (do (reset! (:callback-ids run) []) (reset! (:poll-pending? run) false)
                (reset! (:polled-position run) nil)
                (reset! (:setup-watchers? run) false) (reset! (:parameter-generation run) nil))
    :stopping (release-held! manager run)))

(defn- adapter [factory]
  {:defaults settings/defaults :normalize settings/normalize
   :transport (fn [settings _ run-id] (factory (assoc settings :sync-session-id run-id)))
   :prepare-state prepare-state :run-state run-state :lifecycle! lifecycle! :tick! tick!
   :status #(select-keys % [:recording :last-execution :parameter-watch])
   :events #{:live/parameter :live/parameter-error :live/forget-parameter :midi/control :live/status :live/midi :live/run-clip :live/parameter-device :live/acknowledgement :live/handoff
             :record/command :live/executed :live/response}
   :receive! receive!})

(defn validate-config [data]
  (config/validate-config {:live (adapter transport/transport)} data))

(defn validate-state [data]
  (config/validate-state {:live (adapter transport/transport)} data))

(defn create-manager
  "Create an inert Afterglow synchronization session with Tolglow's Live adapter."
  ([] (create-manager {}))
  ([options]
   (sync/create-manager (assoc-in options [:adapters :live] (adapter (:live-factory options transport/transport))))))
