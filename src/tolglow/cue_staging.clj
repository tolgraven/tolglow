(ns tolglow.cue-staging
  "Staged cue launches (#477). An operator opens a stage for a cue, adjusts
  its parameters, then launches it with those values now or on a grid
  boundary, or cancels it without launching.

  A stage renders nothing and is no instance. Its parameters are history
  targets of their own kind, [:staged-param stage-id param], which Afterglow's
  edit history reads and writes through the hooks in `edit-targets`. Gestures
  and history travel therefore share the operator history (#510) with
  running cues, and their writes emit :cue-stage/param-changed, never
  :param/changed. A launch hands the stage's targets to the launched instance
  (control/retarget-edit-history), so its edits stay undoable on the running
  cue. A cancel discards them (control/remove-edit-history-targets).

  The launch is the ordinary :cue/launch, admitted now or through the grid
  queue, so its order follows the same intake rules as every other launch.
  The queued command carries no values: they are read from the stage when it
  fires, after any history travel or definition revision in between.

  A stage follows its cue: `refresh` re-resolves its values against the
  cue's current effect definition (clamping a narrowed bound) and marks it
  :stale when the cue or effect is gone.

  Operator-facing commands, submitted with `show/submit!`:
  - `{:op :cue-stage/open :cue-id id}` with optional `:stage-id` (a string;
    the command ID by default), `:params` overrides and `:launch` options;
  - `{:op :cue-stage/edit :stage-id id :param p :value v}` records one edit
    from a trusted operator (one history step for numeric parameters);
  - `:param/set` and `:gesture/*` with `:stage-id` in place of `:instance-id`;
  - `{:op :cue-stage/launch :stage-id id}`, with optional `:grid`;
  - `{:op :cue-stage/cancel :stage-id id}`.
  Agents have no staging tool; staging is an operator rehearsal surface."
  (:require [afterglow.control.edit-history :as edit-history]
            [afterglow.control.model :as control]
            [afterglow.control.params :as params]
            [tolglow.cue-timing :as cue-timing]))

(def ^:private path [:control ::stages])

(def stage-limit "Open stages per show epoch." 16)

(def target-kind "The edit history target kind of a staged parameter." :staged-param)

(def operations #{:cue-stage/open :cue-stage/edit :cue-stage/launch :cue-stage/cancel})

(def domain-operations
  "Operations admitted as ordinary domain commands by the show reducer.
  :cue-stage/set is the translated form of :param/set with :stage-id."
  #{:cue-stage/open :cue-stage/cancel :cue-stage/set})

(def ^:private gesture-operations #{:gesture/begin :gesture/sample :gesture/end :gesture/cancel})

(def ^:private launch-modes #{:persistent :timed :one-shot})

(defn- fail! [code message & [details]]
  (throw (ex-info message (cond-> {:control/error code} details (assoc :details details)))))

(defn- stage-id? [x] (and (string? x) (<= 1 (count x) 128)))

(defn target
  "The history target of one staged parameter."
  [stage-id param]
  [target-kind stage-id param])

(defn- state [model]
  (let [state (get-in model path)]
    (when (= (:epoch model) (:epoch state)) state)))

(defn- raw-stage [model stage-id] (get-in (state model) [:stages stage-id]))

(defn- effective
  "A grid launch that missed or was refused returns its stage to editing and
  leaves the outcome in :last-launch."
  [model {:keys [status launch-command-id] :as stage}]
  (let [receipt (when (= :scheduled status) (cue-timing/receipt model launch-command-id))]
    (if (and (= :scheduled status) (not= :pending (:status receipt)))
      (-> stage
          (assoc :status (if (:stale stage) :stale :staged)
                 :last-launch (cond-> {:command-id launch-command-id :status (or (:status receipt) :unknown)}
                                (:reason receipt) (assoc :reason (:reason receipt))
                                (:code receipt) (assoc :code (:code receipt))
                                (:due-beat receipt) (assoc :due-beat (double (:due-beat receipt)))))
          (dissoc :launch-command-id))
      stage)))

(defn stage
  "One stage in the current epoch with its effective status, or nil."
  [model stage-id]
  (some->> (raw-stage model stage-id) (effective model)))

(defn stages
  "Stages in the current epoch, in opening order."
  [model]
  (->> (vals (:stages (state model)))
       (sort-by :sequence)
       (mapv #(effective model %))))

(defn- effect-definition [model cue]
  (if-let [version (:effect-version cue)]
    (or (get-in model [:registry :versions [:effects (:effect-id cue) version]])
        (let [current (get-in model [:registry :effects (:effect-id cue)])]
          (when (= version (:version current)) current)))
    (get-in model [:registry :effects (:effect-id cue)])))

(defn- current? [model {:keys [cue-id cue-version effect-version]}]
  (let [cue (get-in model [:registry :cues cue-id])]
    (and cue (= cue-version (:version cue)) (= effect-version (:version (effect-definition model cue))))))

(defn- editable? [model stage]
  (and stage (= :staged (:status stage)) (current? model stage)))

(defn- stage-targets [model stage-id]
  (filterv (fn [[kind id]] (and (= target-kind kind) (= stage-id id)))
           (edit-history/tracked-targets (get-in model [:control :edit-history]))))

;; History hooks. A scheduled stage keeps its cells, so history travel across
;; its steps still works while the launch waits; the launch reads the values
;; when it fires.

(defn- cell [model [_ stage-id param]]
  (let [stage (stage model stage-id)]
    (when (and stage (#{:staged :scheduled} (:status stage)) (current? model stage))
      {:value (get-in stage [:params param]) :schema (get-in stage [:definition :params param])})))

(defn- write [model [_ stage-id param] value]
  (let [schema (get-in (raw-stage model stage-id) [:definition :params param])]
    (when-not (params/valid-value? schema value)
      (fail! :invalid-params "Value does not satisfy the staged parameter's schema" {:param param}))
    {:model (update-in model (conj path :stages stage-id) #(-> % (assoc-in [:params param] value) (dissoc :revised)))
     :events [{:type :cue-stage/param-changed :stage-id stage-id :param param :value value}]}))

(defn- display [model [_ stage-id]]
  (let [{:keys [cue-id effect-id definition]} (raw-stage model stage-id)]
    (cond-> {}
      cue-id (assoc :cue-id cue-id :cue-label (or (get-in model [:registry :cues cue-id :name]) (name cue-id)))
      effect-id (assoc :effect-id effect-id :effect-label (or (:name definition) (name effect-id))))))

(def edit-targets
  "Afterglow edit target hooks for staged parameters."
  {target-kind {:cell cell :write write :display display}})

(defn with-edit-targets
  "Add the staged-parameter hooks to an operator authority."
  [authority]
  (cond-> authority (:operator-id authority) (assoc ::control/edit-targets edit-targets)))

(defn unavailable-reason
  "Why a staged-parameter history target cannot be travelled, or nil."
  [model [_ stage-id param :as target]]
  (let [stage (stage model stage-id)]
    (cond
      (nil? stage) "The staged cue was launched or cancelled."
      (= :stale (:status stage)) (str "The staged cue is stale: " (get-in stage [:stale :message]))
      (not (current? model stage)) "The staged cue's definition changed."
      (nil? (cell model target)) "The staged parameter is no longer available.")))

(defn- rejected [model command code message]
  {:model model :events []
   :ack (merge (select-keys model [:show-id :epoch :revision])
               {:status :rejected :command-id (:command-id command) :code code :message message})})

(defn- accepted? [result] (= :accepted (get-in result [:ack :status])))

(defn- agent-source? [payload] (#{:agent "agent"} (get-in payload [:source :adapter])))

;; Following the cue's definition.

(defn- revised-params
  "Keep each staged value the new schema accepts. A numeric value outside a
  narrowed bound is clamped; anything else falls back to the cue's value."
  [schemas defaults staged]
  (into {} (map (fn [[param schema]]
                  (let [value (get staged param)
                        [low high] (:bounds schema)]
                    [param (cond
                             (and (contains? staged param) (params/valid-value? schema value)) value
                             (and (number? value) (#{:number :integer} (:type schema)) (number? low) (number? high)
                                  (params/valid-value? schema (max low (min high value))))
                             (max low (min high value))
                             :else (get defaults param))])))
        schemas))

(defn- stale [model stage-id code message]
  (let [{:keys [status launch-command-id]} (raw-stage model stage-id)]
    (-> (cond-> model
          (= :scheduled status) (cue-timing/withdraw launch-command-id :stage-stale))
        (update-in (conj path :stages stage-id) assoc :stale {:code code :message message}
                   :status (if (= :scheduled status) :scheduled :stale)))))

(defn- refresh-stage [model {:keys [id cue-id params] :as stage}]
  (let [cue (get-in model [:registry :cues cue-id])
        effect (when cue (effect-definition model cue))]
    (cond
      (:stale stage) model
      (nil? cue) (stale model id :cue-deleted "The cue was deleted.")
      (nil? effect) (stale model id :effect-unavailable "The cue's effect is not published.")
      (current? model stage) model
      :else
      (let [schemas (get effect :params {})
            defaults (try (params/resolve-values schemas (merge (:params cue) (:saved-params cue)))
                          (catch clojure.lang.ExceptionInfo _ nil))]
        (if (nil? defaults)
          (stale model id :invalid-params "The cue's parameters no longer resolve.")
          (let [next-params (revised-params schemas defaults params)
                adjusted (sort-by str (filter #(not= (get params %) (get next-params %))
                                              (into (set (keys params)) (keys next-params))))
                {:keys [status launch-command-id]} stage
                ;; A waiting launch never fires values the operator did not see:
                ;; it is withdrawn and the stage returns to editing.
                withdraw? (and (seq adjusted) (= :scheduled status)
                               (= :pending (:status (cue-timing/receipt model launch-command-id))))]
            (reduce control/touch-edit-target
                    (cond-> (update-in (cond-> model withdraw? (cue-timing/withdraw launch-command-id :stage-revised))
                                       (conj path :stages id) assoc
                                       :definition effect :effect-id (:id effect) :effect-version (:version effect)
                                       :cue-version (:version cue) :params next-params)
                      (seq adjusted)
                      (assoc-in (conj path :stages id :revised)
                                {:effect-version (:version effect)
                                 :adjusted (into {} (map (fn [param] [param [(get params param) (get next-params param)]]))
                                                 adjusted)}))
                    (map #(target id %) adjusted))))))))

(defn refresh
  "Follow each stage's cue: adopt a revised effect definition, clamping
  values its bounds no longer allow, or mark the stage stale (withdrawing a
  pending launch) when the cue or effect is gone. Changed values advance
  their history generations, and :revised records each as [old new] until the
  next edit or launch. A waiting launch whose values change is withdrawn
  (:stage-revised), returning the stage to editing. Cheap when nothing is
  staged."
  [model]
  (if-let [current (seq (vals (:stages (state model))))]
    (reduce refresh-stage model current)
    model))

;; Commands.

(defn- open [model {:keys [cue-id command-id time-ms source] :as command}]
  (let [stage-id (get command :stage-id command-id)
        launch (get command :launch {})
        overrides (get command :params {})
        current (state model)
        cue (get-in model [:registry :cues cue-id])
        effect (when cue (effect-definition model cue))]
    (when-not (stage-id? stage-id)
      (fail! :invalid-stage "A stage ID is a string of 1 to 128 characters"))
    (when (get-in current [:stages stage-id])
      (fail! :stage-exists "That stage is already open"))
    (when (>= (count (:stages current)) stage-limit)
      (fail! :too-many-stages (str "At most " stage-limit " cues can be staged at once")))
    (when-not cue (fail! :unknown-cue "Cue does not exist" {:cue-id cue-id}))
    (when-not effect (fail! :unknown-effect "Effect is not published"))
    (when-not (and (map? launch) (map? overrides)) (fail! :invalid-launch "Launch options and parameters must be maps"))
    (when (contains? launch :params) (fail! :invalid-launch "Stage parameters go in :params"))
    (when-not (launch-modes (get launch :mode :persistent))
      (fail! :invalid-launch "A staged cue launches persistent, timed or one-shot"))
    (let [values (try (params/resolve-values (get effect :params {})
                                             (merge (:params cue) (:saved-params cue) overrides))
                      (catch clojure.lang.ExceptionInfo error
                        (fail! :invalid-params "Invalid parameters" (ex-data error))))
          sequence (get current :sequence 0)
          stage {:id stage-id :sequence sequence :cue-id cue-id :cue-version (:version cue)
                 :effect-id (:id effect) :effect-version (:version effect) :definition effect
                 :params values :launch launch :status :staged :opened-at-ms time-ms :source source}]
      {:model (assoc-in model path (-> (or current {})
                                       (assoc :epoch (:epoch model) :sequence (inc sequence))
                                       (assoc-in [:stages stage-id] stage)))
       :events [{:type :cue-stage/opened :stage-id stage-id :cue-id cue-id}]
       :result {:stage-id stage-id :cue-id cue-id}})))

(defn- set-param
  "A direct write to a staged value, outside the history, like :param/set."
  [model {:keys [stage-id param value]}]
  (let [current (stage model stage-id)
        schema (get-in current [:definition :params param])]
    (when-not (editable? model current) (fail! :stage-unavailable "That cue is not staged for editing"))
    (when-not (and (keyword? param) schema) (fail! :invalid-params "The staged cue has no such parameter"))
    (when-not (params/valid-value? schema value)
      (fail! :invalid-params "Invalid parameters" {:errors [{:path [param] :message "Value does not satisfy parameter schema"}]}))
    (let [changed? (not= value (get-in current [:params param]))]
      {:model (cond-> (update-in model (conj path :stages stage-id) #(-> % (assoc-in [:params param] value) (dissoc :revised)))
                changed? (control/touch-edit-target (target stage-id param)))
       :events (if changed? [{:type :cue-stage/param-changed :stage-id stage-id :param param :value value}] [])
       :result {:stage-id stage-id}})))

(defn- cancel
  "Discard a stage and its history steps. A pending grid launch is withdrawn
  here, so every path that cancels a stage stops its launch."
  [model {:keys [stage-id]}]
  (let [{:keys [status launch-command-id]} (raw-stage model stage-id)]
    (when-not (stage model stage-id) (fail! :unknown-stage "No staged cue has that ID"))
    {:model (-> (cond-> model
                  (= :scheduled status) (cue-timing/withdraw launch-command-id :stage-cancelled))
                (update-in (conj path :stages) dissoc stage-id)
                (control/remove-edit-history-targets (stage-targets model stage-id)))
     :events [{:type :cue-stage/cancelled :stage-id stage-id}]
     :result {:stage-id stage-id}}))

(defn apply-operation
  "Domain reducer for :cue-stage/open, :cue-stage/cancel and :cue-stage/set."
  [model {:keys [op] :as command}]
  (case op
    :cue-stage/open (open model command)
    :cue-stage/cancel (cancel model command)
    :cue-stage/set (set-param model command)))

(defn routes?
  "Whether a parameter or gesture command names a stage."
  [{:keys [op] :as payload}]
  (and (or (gesture-operations op) (= :param/set op)) (contains? payload :stage-id)))

(defn gesture-target
  "The history target a gesture payload addresses."
  [{:keys [stage-id instance-id param]}]
  (if stage-id (target stage-id param) [:instance-param instance-id param]))

(defn route
  "Admit a parameter or gesture command that names a stage through
  `apply-command` ([model payload])."
  [model {:keys [op stage-id param command-id] :as payload} apply-command]
  (let [cached? (get-in model [:control :history command-id])]
    (cond
      (agent-source? payload)
      (rejected model payload :agent-refused "Agents cannot stage cues")

      (contains? payload :instance-id)
      (rejected model payload :invalid-command "Address either a stage or an instance")

      (not (and (stage-id? stage-id) (keyword? param)))
      (rejected model payload :invalid-stage "A staged command needs a stage ID and a parameter")

      (and (not cached?) (not (editable? model (stage model stage-id))))
      (rejected model payload :stage-unavailable "That cue is not staged for editing")

      (= :param/set op)
      (apply-command model (assoc payload :op :cue-stage/set))

      :else
      (apply-command model (-> payload (dissoc :stage-id :param) (assoc :edit-target (target stage-id param)))))))

(defn prepare-launch
  "Give a stage's own queued launch the stage's values as it fires."
  [model {:keys [op stage-id command-id] :as payload}]
  (let [{:keys [status launch-command-id params]} (when (= :cue/launch op) (raw-stage model stage-id))]
    (if (and (= :scheduled status) (= command-id launch-command-id))
      (assoc-in payload [:launch :params] params)
      payload)))

(defn complete-launch
  "Finish a stage when its own accepted launch commits, wherever it fired.
  The stage's history follows the launched instance."
  [result {:keys [op stage-id command-id]}]
  (let [model (:model result)
        current (when (and stage-id (= :cue/launch op) (accepted? result) (not (:replayed? result)))
                  (raw-stage model stage-id))
        instance-id (get-in result [:ack :instance-id])]
    (if (and current (= command-id (:launch-command-id current)) instance-id)
      (-> result
          (assoc :model (reduce (fn [model [_ _ param :as from]]
                                  (control/retarget-edit-history model from [:instance-param instance-id param]))
                                (update-in model (conj path :stages) dissoc stage-id)
                                (stage-targets model stage-id)))
          (update :ack assoc :stage-id stage-id))
      result)))

(defn- envelope [payload]
  (select-keys payload [:show-id :epoch :source]))

(defn- settle-gestures
  "Close every open gesture on the stage under its own owner before the
  launch: an ordinary gesture commits, a temporary one rolls back."
  [model stage-id payload apply-command]
  (let [open (sort-by (comp pr-str key)
                      (filter (fn [[[kind id]]] (and (= target-kind kind) (= stage-id id)))
                              (edit-history/open-gestures (get-in model [:control :edit-history]))))]
    (reduce (fn [model [index [target {:keys [actor control-id gesture-id last-sequence temporary?]}]]]
              (let [command (cond-> (merge (envelope payload)
                                           {:op (if temporary? :gesture/cancel :gesture/end) :edit-target target
                                            :control-id control-id :gesture-id gesture-id
                                            :command-id (str (:command-id payload) "/settle/" index)})
                              (not temporary?) (assoc :last-sequence last-sequence))
                    result (apply-command model command (with-edit-targets {:operator-id actor}))]
                (if (accepted? result) (:model result) model)))
            model (map-indexed vector open))))

(defn- launch [model {:keys [stage-id command-id grid raw] :as payload} {:keys [command schedule]}]
  (let [cached (get-in model [:control :history command-id])
        queued (cue-timing/queued-command model command-id)
        current (stage model stage-id)]
    (cond
      (not (stage-id? stage-id))
      (rejected model payload :invalid-stage "A stage ID is a string of 1 to 128 characters")

      ;; A retry is the same launch only if it was this stage's.
      (or cached queued (cue-timing/receipt model command-id))
      (cond
        (= stage-id (get-in cached [:command :stage-id]))
        {:model model :events [] :ack (assoc (:ack cached) :stage-id stage-id) :replayed? true}

        (and queued (= stage-id (:stage-id queued)))
        {:model model :events [] :ack (assoc (cue-timing/receipt model command-id) :duplicate? true :stage-id stage-id)}

        :else (rejected model payload :command-id-conflict "Command ID reused with different data"))

      (nil? current) (rejected model payload :unknown-stage "No staged cue has that ID")

      (= :stale (:status current))
      (rejected model payload (get-in current [:stale :code] :stage-stale) (get-in current [:stale :message]))

      (= :scheduled (:status current))
      (rejected model payload :launch-pending "This staged cue is already scheduled to launch")

      :else
      (let [settled (settle-gestures model stage-id payload command)
            frozen (raw-stage settled stage-id)
            request (merge (envelope payload)
                           {:op :cue/launch :cue-id (:cue-id frozen) :stage-id stage-id :command-id command-id
                            :launch (merge {:mode :persistent} (:launch frozen))})
            scheduled (update-in settled (conj path :stages stage-id) #(-> % (assoc :status :scheduled :launch-command-id command-id)
                                                                         (dissoc :last-launch :revised)))
            result (if (and (some? grid) (not= :off grid))
                     (schedule scheduled (cond-> {:command request :grid grid} raw (assoc :raw raw)))
                     (command scheduled request nil))]
        (if (#{:accepted :pending :fired} (get-in result [:ack :status]))
          result
          (assoc result :model model))))))

(defn- edit [model {:keys [stage-id param value command-id control-id] :as payload} authority apply-command]
  (let [current (stage model stage-id)
        cached (get-in model [:control :history command-id])
        recorded (or (get-in model [:control :history (str command-id "/sample") :command]) (:command cached))
        schema (get-in current [:definition :params param])]
    (cond
      (not (and (stage-id? stage-id) (keyword? param) (some? command-id)))
      (rejected model payload :invalid-command "A staged edit needs a stage, a parameter and a command ID")

      cached
      (if (and (#{(target stage-id param)} (get recorded :edit-target (target (:stage-id recorded) (:param recorded))))
               (= value (:value recorded)))
        {:model model :events [] :ack (:ack cached) :replayed? true}
        (rejected model payload :command-id-conflict "Command ID reused with different data"))

      (nil? (:operator-id authority))
      (rejected model payload :operator-required "Edit history requires a trusted operator")

      (not (editable? model current))
      (rejected model payload :stage-unavailable "That cue is not staged for editing")

      (nil? schema)
      (rejected model payload :invalid-params "The staged cue has no such parameter")

      ;; Only numeric parameters have history steps; other types are set directly.
      (not (#{:number :integer} (:type schema)))
      (apply-command model (merge (envelope payload) {:op :cue-stage/set :stage-id stage-id :param param
                                                      :value value :command-id command-id}) nil)

      :else
      (let [gesture (merge (envelope payload) {:edit-target (target stage-id param)
                                               :control-id (or control-id "cue-stage/edit") :gesture-id command-id})
            authority (with-edit-targets authority)
            steps [(assoc gesture :op :gesture/begin :command-id (str command-id "/begin"))
                   (assoc gesture :op :gesture/sample :sequence 1 :value value :command-id (str command-id "/sample"))
                   (assoc gesture :op :gesture/end :last-sequence 1 :command-id command-id)]
            outcome (reduce (fn [{:keys [model events]} step]
                              (let [result (apply-command model step authority)]
                                (if (accepted? result)
                                  {:model (:model result) :events (into events (:events result)) :ack (:ack result)}
                                  (reduced (assoc result :failed? true)))))
                            {:model model :events []} steps)]
        (if (:failed? outcome)
          {:model model :events [] :ack (assoc (:ack outcome) :command-id command-id)}
          outcome)))))

(defn dispatch
  "Owner admission for :cue-stage/* commands. Host functions: :command
  ([model payload authority]) admits through the show reducer and :schedule
  ([model request]) queues a grid launch."
  [model {:keys [op] :as payload} authority {:keys [command] :as host}]
  (let [payload (cond-> payload (nil? (:command-id payload)) (assoc :command-id (str (random-uuid))))
        model (refresh model)]
    (if (agent-source? payload)
      (rejected model payload :agent-refused "Agents cannot stage cues")
     (case op
      :cue-stage/edit (edit model payload authority command)
      :cue-stage/launch (launch model payload host)
      (:cue-stage/open :cue-stage/cancel) (command model payload nil)))))

(defn projection
  "Staged cues as Studio, Push and the REPL read them. A stage has no
  instance ID; its parameters are history targets of kind :staged-param."
  [model]
  (mapv (fn [{:keys [id cue-id effect-id definition params launch status launch-command-id stale revised last-launch]}]
          (let [receipt (when launch-command-id (cue-timing/receipt model launch-command-id))]
            (cond-> {:stage-id id :cue-id cue-id
                     :cue-label (or (get-in model [:registry :cues cue-id :name]) cue-id)
                     :swatch (get-in model [:registry :cues cue-id :swatch])
                     :effect-id effect-id :effect-label (or (:name definition) effect-id)
                     :status status :params params :schema (get definition :params {}) :launch launch}
              stale (assoc :stale stale)
              revised (assoc :revised revised)
              last-launch (assoc :last-launch last-launch)
              (= :scheduled status)
              (assoc :launch-command-id launch-command-id
                     :due-beat (some-> (:due-beat receipt) double) :grid (:grid receipt)))))
        (stages model)))

(defn without-stages
  "Drop live stages, and their history steps, from a saved or restored model."
  [model]
  (let [targets (filterv #(= target-kind (first %))
                         (edit-history/tracked-targets (get-in model [:control :edit-history])))]
    (-> (control/remove-edit-history-targets model targets)
        (update :control dissoc ::stages))))
