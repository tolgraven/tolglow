(ns tolglow.composition
  "Pure bridge from reusable slot editors to owned control-model instances.
  The host supplies milliseconds and, for beat sequences, transport :beat/:bpm.
  No clock, transport or output is started by loading this namespace."
  (:require [tolglow.spatial :as spatial]
            [afterglow.render.layers :as layers]
            [afterglow.time :as time]
            [clojure.string :as str]
            [tolglow.authoring.card-layout :as card-layout]
            [tolglow.registry :as registry]
            [tolglow.composition.sequence :as sequence]
            [tolglow.composition.macro :as macro]))

(defn- fail! [path message]
  (throw (ex-info message {:errors [{:path path :message message}]})))

(def macro-terms
  "How validation names a composition and its slots. A template group
  (`tolglow.template-groups`) passes its own terms, so its errors say
  \"member\" and use :members paths without rewriting this text."
  {:noun "macro" :item "slot" :items :slots})

(def reserved-prefix
  "Composition IDs starting with this name a template group placement's run
  (`tolglow.template-groups/run-id`). A saved macro or sequence may not use
  it, so it can never take over a placement's run."
  "template-group/")

(defn reserved-id?
  "Whether `id` (a keyword or string) starts with `reserved-prefix`."
  [id]
  (let [n (if (keyword? id) (subs (str id) 1) id)]
    (and (string? n) (str/starts-with? n reserved-prefix))))

(defn- identifier? [id]
  (or (keyword? id) (and (string? id) (not (str/blank? id)))))

(defn- positive-time? [duration]
  (and (registry/finite-number? duration) (pos? duration)))

(defn- sequence-spec [definition]
  (merge (select-keys definition [:max-transitions])
         {:steps (:slots definition) :timebase (:timebase definition :beats)
          :mode (:loop definition :once) :seed (:seed definition 0)}))

(defn- validate-cue-slot! [model {:keys [item items]} index slot]
  (let [cue (get-in model [:registry :cues (:cue-id slot)])
        effect (registry/cue-effect (:registry model) cue)
        launch (:launch slot {})
        mode (:mode launch :persistent)
        Item (str/capitalize item)]
    (when-not (and cue effect)
      (fail! [items index] (str "A " item " needs a registered cue/effect reference")))
    (when-not (and (map? (:params slot)) (map? launch)
                   (map? (:params launch {})) (#{:persistent :held :timed :one-shot} mode))
      (fail! [items index :launch] (str Item " needs parameter maps and a supported launch mode")))
    (when (and (contains? slot :targets) (not= :previous (:targets slot)))
      (fail! [items index :targets] (str "Use :previous to include the targets of earlier " item "s in the layer")))
    (when (contains? slot :transforms)
      (try (layers/validate-transforms! (:transforms slot))
           (catch clojure.lang.ExceptionInfo cause
             (fail! [items index :transforms] (.getMessage cause)))))
    (doseq [key [:duration-ms :duration-beats] :when (contains? slot key)]
      (when-not (positive-time? (get slot key))
        (fail! [items index key] (str Item " duration must be positive and finite"))))
    (doseq [[path duration] [[[items index :duration] (:duration slot)]
                             [[items index :launch :duration] (:duration launch)]]
            :when (some? duration)]
      (when (or (seq (time/validate-duration duration))
                (not (pos? (val (first duration)))))
        (fail! path "Duration needs one positive finite musical or millisecond value")))
    (when (and (contains? launch :duration-ms) (not (positive-time? (:duration-ms launch))))
      (fail! [items index :launch :duration-ms] "Launch duration must be positive and finite"))
    (when (and (#{:timed :one-shot} mode)
               (not (or (:duration launch) (:duration cue) (:duration effect)
                        (positive-time? (or (:duration-ms launch) (:duration-ms cue) (:duration-ms effect))))))
      (fail! [items index :launch :duration-ms] "Timed/one-shot launches need a declared duration"))
    (let [errors (registry/validate-params (:params effect {})
                                            (merge (:params cue {}) (:saved-params cue {}) (:params slot) (:params launch {})))]
      (when (seq errors)
        (throw (ex-info (str "Invalid " item " parameter values")
                        {:errors (mapv #(update % :path (fn [path] (into [items index] path))) errors)}))))))

(defn- validate-slot! [model {:keys [noun item items] :as terms} mode index slot]
  (when-not (and (map? slot) (identifier? (:id slot)))
    (fail! [items index :id] (str (str/capitalize item) " needs a stable ID")))
  (let [empty? (= :empty (:kind slot))]
    (when (and (= :macro mode) (or empty? (:muted? slot)))
      (fail! [items index] (str "A " noun " " item " needs a cue and cannot be muted; empty and muted steps belong to sequences")))
    (when-not empty?
      (validate-cue-slot! model terms index slot))))

(defn- assign-slot-ids
  "Keep authored slot IDs. Give each unidentified slot the first free
  \"slot-N\" from its index, so adding after a deletion cannot collide.
  Taken IDs compare by name, matching the JSON boundary's keyword IDs."
  [slots]
  (let [taken (set (map sequence/step-key (keep :id slots)))]
    (first
     (reduce (fn [[result taken] [i slot]]
               (if (contains? slot :id)
                 [(conj result slot) taken]
                 (let [id (first (remove taken (map #(str "slot-" %) (iterate inc i))))]
                   [(conj result (assoc slot :id id)) (conj taken id)])))
             [[] taken] (map-indexed vector slots)))))

(defn- canonical-cards
  "Cards naming each slot by the slot's own ID, so a keyword ID from the JSON
  boundary and the stored string ID stay one value. Invalid entries are left
  for validation to report."
  [cards slots]
  (let [ids (into {} (map (juxt (comp card-layout/slot-key :id) :id)) slots)]
    (if (vector? cards)
      (mapv (fn [card]
              (if-let [id (and (map? card) (ids (card-layout/slot-key (:slot-id card))))]
                (assoc card :slot-id id)
                card))
            (card-layout/normalize cards))
      cards)))

(def projection-keys
  "Keys the compositions projection adds for views; a definition read back
  from a snapshot and saved again drops them."
  [:running? :playback :layer-id :member-slots])

(defn save
  "Validate and save a macro or sequence. New sequences default to one-beat slots.
  :duration maps support beats, bars, phrases and explicit milliseconds. Slot overrides and
  cue references validate before a new model is returned. A macro's optional
  :cards arrange its member cards (`tolglow.authoring.card-layout`): a save
  that omits :cards keeps the saved cards whose slot still names the same cue,
  and an explicit `:cards nil` clears them. Running executions
  retain their captured definition; restart explicitly to use an edited one.
  IDs starting with `reserved-prefix` are refused. `opts`:
  :terms names the composition and its slots in errors (`macro-terms`), and
  :placement? true validates a template group placement's derived run, whose
  ID uses the reserved prefix (`tolglow.template-groups`)."
  ([model definition] (save model definition {}))
  ([model definition {:keys [terms placement?] :or {terms macro-terms}}]
  (when (and (not placement?) (map? definition) (reserved-id? (:id definition)))
    (fail! [:id] (str "Composition IDs starting with " reserved-prefix " name template group placements")))
  (when-not (and (map? definition) (registry/data? definition)
                 (identifier? (:id definition)) (#{:macro :sequence} (:mode definition))
                 (vector? (:slots definition)) (seq (:slots definition))
                 (every? map? (:slots definition))
                 (#{:ms :beats} (:timebase definition :beats)))
    (fail! [] "Composition needs portable data, an ID, mode, timebase and nonempty slot vector"))
  (let [opacity (get definition :opacity 1)]
    (when-not (and (registry/finite-number? opacity) (<= 0 opacity 1))
      (fail! [:opacity] "Layer opacity must be between zero and one")))
  (when (and (contains? definition :cards) (not= :macro (:mode definition)))
    (fail! [:cards] "Only a macro arranges its cards; other effects keep standard slots"))
  (let [definition (apply dissoc definition projection-keys)
        timebase (:timebase (sequence/normalize-spec {:steps (:slots definition)
                                                        :timebase (:timebase definition)}))
        duration {:duration (if (= :beats timebase) {:beats 1} {:ms 1000})}
        slots (mapv (fn [i slot]
                      (merge {:params {}}
                             (when (and (= :sequence (:mode definition))
                                        (not-any? #(contains? slot %) [:duration :duration-ms :duration-beats])) duration) slot))
                    (range) (assign-slot-ids (:slots definition)))
        saved (get-in model [:compositions (:id definition)])
        cards (if (contains? definition :cards)
                (canonical-cards (:cards definition) slots)
                (when (= :macro (:mode definition))
                  (card-layout/carry (:slots saved) (:cards saved) slots)))
        definition (cond-> (-> definition (assoc :timebase timebase :slots slots) (dissoc :cards))
                     (and (some? cards) (not (and (sequential? cards) (empty? cards)))) (assoc :cards cards))]
    (when-let [errors (card-layout/errors (:mode definition) slots (:cards definition) terms)]
      (throw (ex-info (str "Invalid " (:noun terms) " cards") {:errors errors})))
    (doseq [[i slot] (map-indexed vector slots)] (validate-slot! model terms (:mode definition) i slot))
    (if (= :sequence (:mode definition))
      (let [errors (sequence/validate-sequence (sequence-spec definition))]
        (when (seq errors) (throw (ex-info "Invalid sequence" {:errors errors}))))
      (macro/launch-plan (:id definition) slots))
    (-> model (assoc-in [:compositions (:id definition)] definition) (update :revision inc)))))

(defn append-macro-cue
  "Append a cue slot to an existing saved macro in the current model. Running
  executions keep the definition they captured at launch."
  [model macro-id cue-id]
  (let [definition (get-in model [:compositions macro-id])
        slots (:slots definition)]
    (when-not (= :macro (:mode definition))
      (fail! [:macro-id] "Choose an existing saved macro"))
    (let [used-ids (into #{} (map :id) slots)
          slot-id (first (remove used-ids (map #(str "slot-" %) (iterate inc (count slots)))))]
      (save model (update definition :slots conj {:id slot-id :cue-id cue-id})))))

(defn set-cards
  "Change a saved macro's card arrangement (#795). The change is one of
  `{:cards [{:slot-id :size} ...]}` (the whole arrangement in card order, or
  nil to return every member card to the default size in slot order),
  `{:resize {:slot-id :size}}` or `{:move {:slot-id :offset}}`. A resize or
  move applies to the arrangement the show holds now, so edits from Studio,
  the REPL and a controller never overwrite each other with a stale copy.
  Validated like a save; running executions keep launching their slots in
  slot order. `opts` are `save`'s."
  ([model macro-id change] (set-cards model macro-id change {}))
  ([model macro-id change {:keys [terms] :or {terms macro-terms} :as opts}]
  (let [definition (get-in model [:compositions macro-id])
        {:keys [slots cards]} definition
        forms (filter #(contains? change %) [:cards :resize :move])]
    (when-not (= :macro (:mode definition))
      (fail! [:id] (str "Choose an existing saved " (:noun terms))))
    (when-not (= 1 (count forms))
      (fail! [] "Send exactly one of :cards, :resize or :move"))
    (let [{:keys [slot-id size offset]} (or (:resize change) (:move change))
          next (case (first forms)
                 :cards (:cards change)
                 :resize (if-let [size (card-layout/size size)]
                           (card-layout/resize slots cards slot-id size)
                           (fail! [:resize :size] "A card size is :small, :medium or :large"))
                 :move (if (integer? offset)
                         (card-layout/move slots cards slot-id offset)
                         (fail! [:move :offset] "A move needs a whole number of places")))]
      (when (and (#{:resize :move} (first forms))
                 (not-any? #(= (card-layout/slot-key slot-id) (card-layout/slot-key (:id %))) slots))
        (fail! [(first forms) :slot-id] (str "A card names a " (:item terms) " of this " (:noun terms))))
      (save model (assoc definition :cards next) opts)))))

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

(def max-faults
  "Recent refused composition launches kept in :composition-faults."
  32)

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
        (cond
          ;; A refused slot launch is a recorded fault; the run keeps playing.
          (and launch? (= :rejected (:status ack)))
          (update model :composition-faults
                  (fn [faults] (vec (take-last max-faults
                                               (conj (vec faults)
                                                     (merge (select-keys ack [:code :message])
                                                            {:type :composition/launch-refused :owner-id (:owner-id intent)
                                                             :member (:member intent) :cue-id (:cue-id intent)
                                                             :epoch (:epoch model) :time-ms time-ms}))))))

          (= :rejected (:status ack)) (throw (ex-info "Composition command rejected" {:ack ack}))
          launch? (assoc-in model [:composition-tokens token] (:instance-id ack))
          :else (update model :composition-tokens dissoc token))))))

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

(declare start-definition)

(defn start
  "Start a saved definition, replacing only its previous owned execution.
  Macro slots launch immediately. Sequences launch on advance; starting while
  paused waits for transport play. Beat sequences require host transport beat."
  [model id time-ms]
  (time! time-ms)
  (let [definition (get-in model [:compositions id])]
    (when-not definition (fail! [:id] "Composition does not exist"))
    (start-definition model definition time-ms)))

(defn start-definition
  "Start `definition`, an already saved shape (see `save`) that need not be
  in :compositions, as the execution under its :id. A template group
  placement (`tolglow.template-groups`) starts its derived macro this way, so
  it shares ownership, stop and restore with saved macros."
  [model definition time-ms]
  (time! time-ms)
  (let [id (:id definition)]
    (doseq [[i slot] (map-indexed vector (:slots definition))]
      (validate-slot! model macro-terms (:mode definition) i slot))
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
