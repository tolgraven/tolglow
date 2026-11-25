(ns tolglow.composition
  "Pure bridge from reusable slot editors to owned control-model instances.
  The host supplies milliseconds and, for beat sequences, transport :beat/:bpm.
  No clock, transport or output is started by loading this namespace."
  (:require [tolglow.spatial :as spatial]
            [afterglow.render.layers :as layers]
            [afterglow.time :as time]
            [clojure.string :as str]
            [tolglow.registry :as registry]
            [tolglow.composition.sequence :as sequence]
            [tolglow.composition.macro :as macro]))

(defn- fail! [path message]
  (throw (ex-info message {:errors [{:path path :message message}]})))

(defn- identifier? [id]
  (or (keyword? id) (and (string? id) (not (str/blank? id)))))

(defn- positive-time? [duration]
  (and (registry/finite-number? duration) (pos? duration)))

(defn- sequence-spec [definition]
  (merge (select-keys definition [:max-transitions])
         {:steps (:slots definition) :timebase (:timebase definition :beats)
          :mode (:loop definition :once) :seed (:seed definition 0)}))

(defn- validate-cue-slot! [model index slot]
  (let [cue (get-in model [:registry :cues (:cue-id slot)])
        effect (registry/cue-effect (:registry model) cue)
        launch (:launch slot {})
        mode (:mode launch :persistent)]
    (when-not (and cue effect)
      (fail! [:slots index] "Cue slot needs a registered cue/effect reference"))
    (when-not (and (map? (:params slot)) (map? launch)
                   (map? (:params launch {})) (#{:persistent :held :timed :one-shot} mode))
      (fail! [:slots index :launch] "Slot needs parameter maps and a supported launch mode"))
    (when (and (contains? slot :targets) (not= :previous (:targets slot)))
      (fail! [:slots index :targets] "Use :previous to include the targets of earlier layer members"))
    (when (contains? slot :transforms)
      (try (layers/validate-transforms! (:transforms slot))
           (catch clojure.lang.ExceptionInfo cause
             (fail! [:slots index :transforms] (.getMessage cause)))))
    (doseq [key [:duration-ms :duration-beats] :when (contains? slot key)]
      (when-not (positive-time? (get slot key))
        (fail! [:slots index key] "Slot duration must be positive and finite")))
    (doseq [[path duration] [[[:slots index :duration] (:duration slot)]
                             [[:slots index :launch :duration] (:duration launch)]]
            :when (some? duration)]
      (when (or (seq (time/validate-duration duration))
                (not (pos? (val (first duration)))))
        (fail! path "Duration needs one positive finite musical or millisecond value")))
    (when (and (contains? launch :duration-ms) (not (positive-time? (:duration-ms launch))))
      (fail! [:slots index :launch :duration-ms] "Launch duration must be positive and finite"))
    (when (and (#{:timed :one-shot} mode)
               (not (or (:duration launch) (:duration cue) (:duration effect)
                        (positive-time? (or (:duration-ms launch) (:duration-ms cue) (:duration-ms effect))))))
      (fail! [:slots index :launch :duration-ms] "Timed/one-shot launches need a declared duration"))
    (let [errors (registry/validate-params (:params effect {})
                                            (merge (:params cue {}) (:saved-params cue {}) (:params slot) (:params launch {})))]
      (when (seq errors)
        (throw (ex-info "Invalid slot parameter values"
                        {:errors (mapv #(update % :path (fn [path] (into [:slots index] path))) errors)}))))))

(defn- validate-slot! [model mode index slot]
  (when-not (and (map? slot) (identifier? (:id slot)))
    (fail! [:slots index :id] "Slot needs a stable ID"))
  (let [empty? (= :empty (:kind slot))]
    (when (and (= :macro mode) (or empty? (contains? slot :muted?)))
      (fail! [:slots index] "Empty and muted steps require a sequence"))
    (when-not empty?
      (validate-cue-slot! model index slot))))

(defn- assign-slot-ids
  "Keep authored slot IDs. Give each unidentified slot the first free
  \"slot-N\" from its index, so adding after a deletion cannot collide."
  [slots]
  (let [taken (set (keep :id slots))]
    (first
     (reduce (fn [[result taken] [i slot]]
               (if (contains? slot :id)
                 [(conj result slot) taken]
                 (let [id (first (remove taken (map #(str "slot-" %) (iterate inc i))))]
                   [(conj result (assoc slot :id id)) (conj taken id)])))
             [[] taken] (map-indexed vector slots)))))

(defn save
  "Validate and save a macro or sequence. New sequences default to one-beat slots.
  :duration maps support beats, bars, phrases and explicit milliseconds. Slot overrides and
  cue references validate before a new model is returned. Running executions
  retain their captured definition; restart explicitly to use an edited one."
  [model definition]
  (when-not (and (map? definition) (registry/data? definition)
                 (identifier? (:id definition)) (#{:macro :sequence} (:mode definition))
                 (vector? (:slots definition)) (seq (:slots definition))
                 (every? map? (:slots definition))
                 (#{:ms :beats} (:timebase definition :beats)))
    (fail! [] "Composition needs portable data, an ID, mode, timebase and nonempty slot vector"))
  (let [opacity (get definition :opacity 1)]
    (when-not (and (registry/finite-number? opacity) (<= 0 opacity 1))
      (fail! [:opacity] "Layer opacity must be between zero and one")))
  (let [timebase (:timebase (sequence/normalize-spec {:steps (:slots definition)
                                                        :timebase (:timebase definition)}))
        duration {:duration (if (= :beats timebase) {:beats 1} {:ms 1000})}
        slots (mapv (fn [i slot]
                      (merge {:params {}}
                             (when (and (= :sequence (:mode definition))
                                        (not-any? #(contains? slot %) [:duration :duration-ms :duration-beats])) duration) slot))
                    (range) (assign-slot-ids (:slots definition)))
        definition (assoc definition :timebase timebase :slots slots)]
    (doseq [[i slot] (map-indexed vector slots)] (validate-slot! model (:mode definition) i slot))
    (if (= :sequence (:mode definition))
      (let [errors (sequence/validate-sequence (sequence-spec definition))]
        (when (seq errors) (throw (ex-info "Invalid sequence" {:errors errors}))))
      (macro/launch-plan (:id definition) slots))
    (-> model (assoc-in [:compositions (:id definition)] definition) (update :revision inc))))

(defn- time! [time-ms]
  (when-not (and (registry/finite-number? time-ms) (not (neg? time-ms)))
    (fail! [:time-ms] "Composition time must be finite and nonnegative"))
  time-ms)

(defn- owned-tokens [model execution]
  (->> (keys (:composition-tokens model))
       (filter #(= (:owner-id execution) (first %)))
       (sort-by pr-str) vec))

(defn- discard-execution [model id execution]
  (-> model
      (update :composition-tokens #(apply dissoc % (owned-tokens model execution)))
      (update :composition-runs dissoc id)))

(defn- perform [model time-ms definition intent]
  (let [token (:token intent) id (get-in model [:composition-tokens token])
        launch? (= :composition/launch (:op intent))
        command (cond-> {:op (if launch? :cue/launch :instance/end)
                         :command-id (str "composition:" (pr-str token) ":" (name (:op intent)))
                         :source {:adapter :composition :session-id (:owner-id intent)}
                         :show-id (:show-id model) :epoch (:epoch model) :time-ms time-ms}
                  launch? (assoc :cue-id (:cue-id intent) :launch (:launch intent))
                  launch? (assoc-in [:source :native-layer]
                                    (merge {:id (:owner-id intent) :member (:member intent)
                                            :opacity (:opacity definition 1)}
                                           (select-keys (get-in definition [:slots (:member intent)]) [:transforms :targets])))
                  (not launch?) (assoc :instance-id id))]
    (if (and (not launch?) (not (get-in model [:instances id])))
      (update model :composition-tokens dissoc token)
      (let [{:keys [model ack]} (spatial/apply-command model command)]
        (when (= :rejected (:status ack)) (throw (ex-info "Composition command rejected" {:ack ack})))
        (if launch? (assoc-in model [:composition-tokens token] (:instance-id ack))
            (update model :composition-tokens dissoc token))))))

(defn stop
  "Release only this execution's captured instance IDs, then discard its tokens.
  Stale executions from an earlier show epoch are discarded without commands."
  [model id time-ms]
  (time! time-ms)
  (if-let [execution (get-in model [:composition-runs id])]
    (let [stale? (not= (:epoch execution (:epoch model)) (:epoch model))
          intents (when-not stale?
                    (mapv #(hash-map :op :composition/release :token % :owner-id (:owner-id execution))
                          (owned-tokens model execution)))
          stopped (reduce #(perform %1 time-ms (:definition execution) %2) model intents)]
      (-> (discard-execution stopped id execution) (update :revision inc)))
    model))

(defn- transport-clock [model time-ms]
  {:time-ms time-ms :beat (get-in model [:transport :beat])
   :bpm (get-in model [:transport :bpm]) :meter (get-in model [:transport :meter]) :playing? (get-in model [:transport :playing?] true)})

(defn start
  "Start a saved definition, replacing only its previous owned execution.
  Macro slots launch immediately. Sequences launch on advance; starting while
  paused waits for transport play. Beat sequences require host transport beat."
  [model id time-ms]
  (time! time-ms)
  (let [definition (get-in model [:compositions id])]
    (when-not definition (fail! [:id] "Composition does not exist"))
    (doseq [[i slot] (map-indexed vector (:slots definition))]
      (validate-slot! model (:mode definition) i slot))
    (let [model (stop model id time-ms)
          owner (str (:epoch model) ":" (:revision model) ":" (pr-str id))
          spec (sequence-spec definition)
          sequence? (= :sequence (:mode definition))
          playing? (get-in model [:transport :playing?] true)
          clock (if (= :beats (:timebase spec)) (transport-clock model time-ms) time-ms)
          initial (when sequence? (sequence/initial-state spec clock owner))
          plan (when-not sequence? (macro/launch-plan owner (:slots definition)))
          model (if plan (reduce #(perform %1 time-ms definition %2) model (:intents plan)) model)
          run (cond-> {:owner-id owner :epoch (:epoch model) :definition definition :started-at-ms time-ms
                        :playback {:kind (:mode definition) :playing? (if sequence? playing? true)
                                   :started? (not sequence?) :slot-count (count (:slots definition))}}
                sequence? (assoc :spec spec :state initial
                                 :clock {:last-time-ms time-ms :logical-time-ms time-ms :playing? playing?}))]
      (-> model
          (assoc-in [:composition-runs id] run)
          (update :revision inc)))))

(defn- advance-clock [model execution time-ms]
  (let [previous (:clock execution)
        playing? (get-in model [:transport :playing?] true)
        _ (when (< time-ms (:last-time-ms previous))
            (fail! [:time-ms] "Composition clock cannot move backwards"))
        delta (- time-ms (:last-time-ms previous))
        logical (+ (:logical-time-ms previous) (if (:playing? previous) delta 0))
        next {:last-time-ms time-ms :logical-time-ms logical :playing? playing?}]
    (when-not (registry/finite-number? logical)
      (fail! [:time-ms] "Composition logical time exceeds finite milliseconds"))
    {:clock next
     :sample (if (= :beats (get-in execution [:spec :timebase]))
               (transport-clock model time-ms) {:time-ms logical :playing? playing?})}))

(defn advance
  "Advance every owned sequence at explicit host time. Beat phase comes directly
  from model transport :beat; tempo changes affect estimates, not beat history.
  Millisecond sequences accumulate only intervals whose preceding transport state
  was playing, so pause/resume cannot burst through skipped wall-clock steps.
  Runs expose :playback with current slot/cue/index/phase and remaining time."
  [model time-ms]
  (time! time-ms)
  (reduce
   (fn [model [id execution]]
     (cond
       (not= (:epoch execution (:epoch model)) (:epoch model))
       (-> (discard-execution model id execution) (update :revision inc))

       (:spec execution)
       (let [spec (:spec execution)
             {:keys [clock sample]} (advance-clock model execution time-ms)
             {:keys [state intents done? step phase remaining-ms remaining-beats]}
             (sequence/advance spec (:state execution) sample)
             waiting? (and (not (:playing? clock)) (not (get-in execution [:playback :started?])))
             intents (if waiting? [] intents)
             state (if waiting? (:state execution) state)
             model (reduce #(perform %1 time-ms (:definition execution) %2) model intents)]
         (if done? (discard-execution model id execution)
             (assoc-in model [:composition-runs id]
                       (assoc execution :state state :clock clock
                              :playback {:kind :sequence :timebase (:timebase spec :ms)
                                         :playing? (:playing? clock) :started? (not waiting?)
                                         :slot-count (count (:steps spec)) :index (:step-index state)
                                         :slot-id (:id step) :cue-id (:cue-id step)
                                         :step-kind (when step (:kind step :cue)) :muted? (boolean (:muted? step))
                                         :phase (some-> phase double)
                                         :remaining-ms (when (:playing? clock) (some-> remaining-ms double))
                                         :remaining-beats (some-> remaining-beats double) :done? done?}))))

       :else model)) model (sort-by (comp pr-str key) (:composition-runs model))))
