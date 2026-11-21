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
  (ableton/new-bridge (assoc (select-keys (:live config) [:mappings :parameters :channel :mode]) :id :live)))

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
                    (case kind :status (map? (:status payload)) :midi (map? (:event payload))
                          (:parameter :parameter-error) (map? (:sample payload))
                          :handoff (#{:live :lighting "live" "lighting"} (:mode payload)) false))]
    (if-not valid? false
      (let [event (case kind
                    :status {:type :live/status :status (:status payload) :time-ms ((get-in manager [:options :now-ms]))}
                    :midi {:type :live/midi :event (-> (:event payload) (update :type #(if (string? %) (keyword %) %))
                                                                    (update :origin #(if (string? %) (keyword %) %)))}
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
(defn handoff! [manager mode envelope]
  (ingest! manager {:type :live/handoff :mode (if (string? mode) (keyword mode) mode) :envelope envelope}))
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
          (let [state @(:state manager)
                matches? (fn [entry run-id]
                           (and (some? (:command-id acknowledgement))
                                (get-in entry [:held note])
                                (= (:command-id acknowledgement) (get-in entry [:held note :command-id]))
                                (or (nil? (:sync-run-id acknowledgement)) (= run-id (:sync-run-id acknowledgement)))))
                current (:active state)
                retired-index (first (keep-indexed (fn [index entry] (when (matches? (:bridge entry) (:id entry)) index)) (:retired state)))]
            (cond
              (and current (matches? (:bridge state) (:id current)))
              (let [result (ableton/acknowledge-launch (:bridge state) note acknowledgement envelope)]
                (swap! (:state manager) assoc :bridge (:state result)) [current (:commands result)])
              (some? retired-index)
              (let [retired (get-in state [:retired retired-index])
                    result (ableton/acknowledge-launch (:bridge retired) note acknowledgement envelope)]
                (swap! (:state manager) update :retired
                       #(filterv (comp seq :held :bridge) (assoc % retired-index (assoc retired :bridge (:state result)))))
                [retired (:commands result)])
              :else nil)))]
    (when resolved (commands! manager (first resolved) (second resolved)))
    (boolean resolved)))

(defn- release-held! [manager run]
  (when-let [envelope! (:envelope! (run-callbacks run))]
    (try
      (let [envelope (envelope!)
            result (locking (:lock manager)
                     (let [state @(:state manager) result (ableton/handoff (:bridge state) :live envelope)]
                       (swap! (:state manager)
                              #(cond-> (assoc % :bridge (bridge (:config state)))
                                 (seq (get-in result [:state :held]))
                                 (update :retired conj {:id (:id run) :callbacks (run-callbacks run) :bridge (:state result)})))
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

(defn- recording-at [recording at-ms freshness-ms]
  (when (and recording (finite? (:sampled-at-ms recording)) (<= (Math/abs (double (- at-ms (:sampled-at-ms recording)))) freshness-ms))
    (let [bpm (get-in recording [:clock :bpm] 120) playing? (get-in recording [:clock :playing?])
          delta (if playing? (* (/ (- at-ms (:sampled-at-ms recording)) 60000.0) bpm) 0.0)
          beat (when (finite? (:beat recording)) (+ (:beat recording) delta))
          low (:loop-start recording) high (:loop-end recording)
          beat (if (and beat (:looping? recording) (every? finite? [low high]) (< low high)) (+ low (mod (- beat low) (- high low))) beat)]
      (cond-> (assoc recording :beat beat)
        (finite? (:song-beat recording)) (update :song-beat + delta)))))

(defn- process-status! [manager run result at-ms round-trip-ms]
  (let [config (:config @(:state manager)) source (get-in config [:live :source-id])
        transport (:clock result)
        recording (assoc result :sampled-at-ms at-ms :round-trip-ms round-trip-ms)]
    (when-not (and (map? result) (finite? at-ms) (<= 0 at-ms)
                   (finite? round-trip-ms) (<= 0 round-trip-ms)
                   (every? #(or (nil? %) (finite? %)) ((juxt :beat :song-beat :loop-start :loop-end) result))
                   (or (nil? transport) (and (map? transport) (finite? (:bpm transport)) (pos? (:bpm transport)))))
      (fail! :invalid-live-status "Invalid Live status timing"))
    (when transport
      (apply-clock! manager run (merge transport {:type :clock :kind :ableton-live :source-id source :time-ms at-ms
                                                :confidence (max 0.0 (- 1.0 (/ round-trip-ms (/ 60000.0 (:bpm transport 120)))))})))
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
    (swap! (:state manager) assoc :recording recording)
    (emit! manager run {:type :live/status :status recording})))

(defn- midi-control! [manager run event]
  (let [state @(:state manager) callbacks (run-callbacks run) envelope (when-let [provider (:envelope! callbacks)] (provider))]
    (when envelope
      (let [controller (ableton/parameter-controller (:bridge state) event)
            mapping (get-in state [:bridge :parameters controller])
            instance (when (and mapping (not (:disabled? mapping)) (:resolve-instance! callbacks))
                       ((:resolve-instance! callbacks) (:cue-id mapping)))
            result (bridge-step! manager #(ableton/midi->commands % event envelope instance))
            commands (mapv #(update % :source merge
                                    (cond-> {:sync-run-id (:id run) :source-id (or (:source-id event) (get-in state [:config :live :source-id]))}
                                      (contains? event :note) (assoc :note (:note event)))) (:commands result))]
        (commands! manager run commands)))))

(defn- receive-handoff! [manager run {:keys [mode envelope]}]
  (let [result (bridge-step! manager #(ableton/handoff % mode envelope))]
    (swap! (:state manager) assoc-in [:config :live :mode] mode)
    (commands! manager run (:commands result))
    (doseq [event (:events result)] (emit! manager run event))
    (when (:live @(:resources run))
      (live-request! manager run (str (UUID/randomUUID)) :handoff {:mode mode}))))

(defn- receive-recording! [manager run {:keys [command ack]} at]
  (let [{:keys [config recording]} @(:state manager)
        {:keys [record-route poll-ms]} (:live config)
        observed (recording-at recording (get command :time-ms at) (max 1500 (* 3 poll-ms)))
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
      (process-status! manager run (:result response) time-ms round-trip-ms)
      (swap! (:state manager) update-in [:metrics :stale-live-statuses] (fnil inc 0))))
  (when (= :executed (:status response))
    (case op
      :watch-parameters (when (= @(:parameter-generation run) (:id response))
                          (swap! (:state manager) assoc :parameter-watch (:result response)))
      :describe-selected-parameter (remember-learned-parameter! manager run (:id payload) (:result response))
      nil))
  (when (#{:place-in-arrangement :describe-selected-parameter :watch-parameters} op)
    (swap! (:state manager) assoc :last-execution response))
  (if (= :inspect op)
    (when-not (= :executed (:status response))
      (swap! (:state manager) assoc :last-error (:error response)))
    (emit! manager run (assoc response :type :live/execution :op op))))

(defn- receive! [manager run event at]
  (case (:type event)
    :midi/control (midi-control! manager run (:event event))
    :live/status (do (process-status! manager run (:status event) (get event :time-ms at) (get event :round-trip-ms 0.0))
                     (swap! (:status-version run) inc))
    (:live/parameter :live/parameter-error) (receive-parameter! manager run event)
    :live/forget-parameter (do (swap! (:state manager) update-in [:config :live :observed-parameters] dissoc (:id event))
                               (update-parameter-watchers! manager run))
    :live/midi (let [message (:event event)]
                 (when-not (= :tolglow-feedback (:origin message))
                   (midi/observe-midi! manager run message :live :live-playback (get event :time-ms at)))
                 (midi-control! manager run message))
    :live/acknowledgement (receive-ack! manager (:note event) (:ack event) (:envelope event))
    :live/handoff (receive-handoff! manager run event)
    :record/command (receive-recording! manager run event at)
    :live/executed (do (swap! (:state manager) assoc :last-execution (:result event))
                       (emit! manager run (assoc (:result event) :type :live/execution)))
    :live/response (receive-response! manager run event)))


(defn- prepare-state [state phase]
  (when (and (= phase :start) (>= (count (:retired state)) 16))
    (fail! :pending-acknowledgements "Resolve pending launch acknowledgements before opening more synchronization sessions"))
  (cond-> (assoc state :bridge (bridge (:config state)) :recording nil :parameter-watch nil)
    (or (= phase :rebind) (nil? (:retired state))) (assoc :retired [])))

(defn- run-state []
  {:status-version (atom 0) :parameter-generation (atom nil) :setup-watchers? (atom false) :setup-mode? (atom false) :mirrored-pair (atom nil)
   :poll-pending? (atom false) :next-poll (atom 0) :callback-ids (atom [])})

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
                (reset! (:setup-watchers? run) false) (reset! (:parameter-generation run) nil))
    :stopping (release-held! manager run)))

(defn- adapter [factory]
  {:defaults settings/defaults :normalize settings/normalize
   :transport (fn [settings _ run-id] (factory (assoc settings :sync-session-id run-id)))
   :prepare-state prepare-state :run-state run-state :lifecycle! lifecycle! :tick! tick!
   :status #(select-keys % [:recording :last-execution :parameter-watch])
   :events #{:live/parameter :live/parameter-error :live/forget-parameter :midi/control :live/status :live/midi :live/acknowledgement :live/handoff
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
