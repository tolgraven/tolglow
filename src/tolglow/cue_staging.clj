(ns tolglow.cue-staging
  "Staged cue launches (#477). An operator opens a stage for a cue, adjusts
  its parameters, then launches it with those values now or on a grid
  boundary, or cancels it without launching.

  A stage renders nothing. Its parameter edits use the shared operator
  history (#510): while a history, gesture or parameter command addresses a
  stage, the stage is presented to the ordinary command reducer as a running
  instance with a reserved ID, and its values return to the stage afterwards.
  A launch rekeys the stage's history to the launched instance, so its edits
  stay undoable on the running cue. A cancel removes them from the tree and
  returns the cursor to the step before them. The launch itself is the
  ordinary `:cue/launch`, admitted immediately or through the grid queue, so
  its order follows the same intake rules as every other launch.

  Operator-facing commands, submitted with `show/submit!`:
  - `{:op :cue-stage/open :cue-id id}` with optional `:stage-id` (a string;
    the command ID by default), `:params` overrides and `:launch` options;
  - `{:op :cue-stage/edit :stage-id id :param p :value v}` records one edit
    from a trusted operator (one history step for numeric parameters);
  - `:param/set` and `:gesture/*` with `:stage-id` in place of `:instance-id`;
  - `{:op :cue-stage/launch :stage-id id}`, with optional `:grid`;
  - `{:op :cue-stage/cancel :stage-id id}`."
  (:require [afterglow.control.params :as params]
            [tolglow.cue-timing :as cue-timing]))

(def ^:private path [:control ::stages])

(def stage-limit "Open stages per show epoch." 16)

(def ^:private id-prefix "stage:")

(def operations #{:cue-stage/open :cue-stage/edit :cue-stage/launch :cue-stage/cancel})

(def domain-operations
  "Operations admitted as ordinary domain commands by the show reducer."
  #{:cue-stage/open :cue-stage/cancel})

(def ^:private parameter-operations
  #{:param/set :gesture/begin :gesture/sample :gesture/end :gesture/cancel})

(def ^:private history-operations #{:history/undo :history/redo :history/select})

(def ^:private launch-modes #{:persistent :timed :one-shot})

(defn- fail! [code message & [details]]
  (throw (ex-info message (cond-> {:control/error code} details (assoc :details details)))))

(defn- stage-id? [x] (and (string? x) (<= 1 (count x) 128)))

(defn stage-instance-id
  "The reserved instance ID a stage's parameters carry in commands and the
  operator history. It never names a launched instance."
  [stage-id]
  (str id-prefix stage-id))

(defn stage-id-of
  "The stage named by a reserved instance ID, or nil."
  [instance-id]
  (when (and (string? instance-id) (.startsWith ^String instance-id id-prefix))
    (subs instance-id (count id-prefix))))

(defn- state [model]
  (let [state (get-in model path)]
    (when (= (:epoch model) (:epoch state)) state)))

(defn- effective [model {:keys [status launch-command-id] :as stage}]
  ;; A grid launch that missed or was refused returns its stage to editing.
  (if (and (= :scheduled status)
           (not= :pending (:status (cue-timing/receipt model launch-command-id))))
    (-> stage (assoc :status :staged) (dissoc :launch-command-id))
    stage))

(defn stage
  "One stage in the current epoch with its effective status, or nil."
  [model stage-id]
  (some->> (get-in (state model) [:stages stage-id]) (effective model)))

(defn stages
  "Stages in the current epoch, in opening order."
  [model]
  (->> (vals (:stages (state model)))
       (sort-by :sequence)
       (mapv #(effective model %))))

(defn- staged-instance [{:keys [id cue-id effect-id definition params opened-at-ms]}]
  {:id (stage-instance-id id) :cue-id cue-id :effect-id effect-id :effect-version (:version definition)
   :definition definition :params params :phase :running :state {} :priority 0 :order -1
   :launch-mode :persistent :started-at-ms opened-at-ms ::stage-id id})

(defn materialize
  "Present editable stages to the command reducer as running instances."
  [model]
  (reduce (fn [model stage]
            (if (= :staged (:status stage))
              (assoc-in model [:instances (stage-instance-id (:id stage))] (staged-instance stage))
              model))
          model (stages model)))

(defn- dematerialize [before after]
  (let [ids (into [] (keep (fn [[id instance]] (when (::stage-id instance) id))) (:instances after))
        instances (apply dissoc (:instances after) ids)
        model (reduce (fn [model id]
                        (let [{::keys [stage-id] :keys [params]} (get-in after [:instances id])]
                          (if (get-in model (conj path :stages stage-id))
                            (assoc-in model (conj path :stages stage-id :params) params)
                            model)))
                      after ids)]
    (cond-> (assoc model :instances (if (= instances (:instances before)) (:instances before) instances))
      ;; A gesture on a stage may hold its modulated parameter; the stage owns no modulation.
      (seq (get-in model [:modulation :holds]))
      (update-in [:modulation :holds] #(apply dissoc % ids)))))

(defn- rejected [model command code message]
  {:model model :events []
   :ack (merge (select-keys model [:show-id :epoch :revision])
               {:status :rejected :command-id (:command-id command) :code code :message message})})

(defn- accepted? [result] (= :accepted (get-in result [:ack :status])))

(defn- through
  "Apply one ordinary command with editable stages materialized."
  [model payload apply-command]
  (let [materialized (materialize model)
        result (apply-command materialized payload)]
    (if (identical? (:model result) materialized)
      (assoc result :model model)
      (update result :model #(dematerialize model %)))))

(defn routes?
  "Whether a command must see stages: a parameter or gesture command that
  names a stage, or history travel while a stage is open for editing."
  [model {:keys [op] :as payload}]
  (or (and (parameter-operations op) (contains? payload :stage-id))
      (and (history-operations op) (some #(= :staged (:status %)) (stages model)))))

(defn route
  "Admit a routed command through `apply-command` ([model payload])."
  [model {:keys [stage-id command-id] :as payload} apply-command]
  (if-not (contains? payload :stage-id)
    (through model payload apply-command)
    (let [translated (-> payload (dissoc :stage-id) (assoc :instance-id (stage-instance-id stage-id)))
          cached? (get-in model [:control :history command-id])]
      (cond
        (contains? payload :instance-id)
        (rejected model payload :invalid-command "Address either a stage or an instance")

        (not (stage-id? stage-id))
        (rejected model payload :invalid-stage "A stage ID is a string of 1 to 128 characters")

        (and (not cached?) (not= :staged (:status (stage model stage-id))))
        (rejected model payload :stage-unavailable "That cue is no longer staged for editing")

        :else (through model translated apply-command)))))

(defn- effect-definition [model cue]
  (if-let [version (:effect-version cue)]
    (or (get-in model [:registry :versions [:effects (:effect-id cue) version]])
        (let [current (get-in model [:registry :effects (:effect-id cue)])]
          (when (= version (:version current)) current)))
    (get-in model [:registry :effects (:effect-id cue)])))

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
          stage {:id stage-id :sequence sequence :cue-id cue-id :effect-id (:id effect)
                 :definition effect :params values :launch launch :status :staged
                 :opened-at-ms time-ms :source source}]
      {:model (assoc-in model path (-> (or current {})
                                       (assoc :epoch (:epoch model) :sequence (inc sequence))
                                       (assoc-in [:stages stage-id] stage)))
       :events [{:type :cue-stage/opened :stage-id stage-id :cue-id cue-id}]
       :result {:stage-id stage-id :cue-id cue-id}})))

(defn- splice-history
  "Remove edits to cancelled targets. Their children attach to the nearest
  remaining ancestor; the cursor moves to it too."
  [{:keys [nodes cursor] :as history} targets]
  (let [dead (into #{} (keep (fn [[id node]] (when (contains? targets (:target node)) id))) nodes)
        live (fn [id] (loop [id id] (if (contains? dead id) (recur (get-in nodes [id :parent])) id)))]
    (if (empty? dead)
      history
      (cond-> (-> history
                  (assoc :nodes (into {} (keep (fn [[id node]]
                                                 (when-not (dead id)
                                                   [id (cond-> node (some? (:parent node)) (update :parent live))])))
                                      nodes)
                         :cursor (live cursor))
                  (update :generations #(apply dissoc % targets))
                  (update :position (fnil inc 0)))
        (:open history) (update :open #(apply dissoc % targets))))))

(defn- stage-targets [model stage-id]
  (let [instance-id (stage-instance-id stage-id)
        history (get-in model [:control :edit-history])]
    (into #{} (filter (fn [[kind id]] (and (= :instance-param kind) (= instance-id id))))
          (concat (keys (:generations history)) (keys (:open history))))))

(defn- cancel [model {:keys [stage-id]}]
  (when-not (stage model stage-id) (fail! :unknown-stage "No staged cue has that ID"))
  (let [targets (stage-targets model stage-id)]
    {:model (cond-> (update-in model (conj path :stages) dissoc stage-id)
              (seq targets) (update-in [:control :edit-history] splice-history targets))
     :events [{:type :cue-stage/cancelled :stage-id stage-id}]
     :result {:stage-id stage-id}}))

(defn apply-operation
  "Domain reducer for :cue-stage/open and :cue-stage/cancel."
  [model {:keys [op] :as command}]
  (case op
    :cue-stage/open (open model command)
    :cue-stage/cancel (cancel model command)))

(defn- rekey [target from to]
  (let [[kind id param] target]
    (if (and (= :instance-param kind) (= from id)) [kind to param] target)))

(defn- rekey-history [history from to]
  (let [rekey-keys #(into {} (map (fn [[target v]] [(rekey target from to) v])) %)]
    (cond-> (-> history
                (update :nodes update-vals #(if (:target %) (update % :target rekey from to) %))
                (update :generations rekey-keys))
      (:open history) (update :open rekey-keys))))

(defn complete-launch
  "Finish a stage when its own accepted launch commits, wherever it fired.
  The stage's history follows the launched instance."
  [result {:keys [op stage-id command-id]}]
  (let [model (:model result)
        current (when (and stage-id (= :cue/launch op) (accepted? result) (not (:replayed? result)))
                  (get-in (state model) [:stages stage-id]))
        instance-id (get-in result [:ack :instance-id])]
    (if (and current (= command-id (:launch-command-id current)) instance-id)
      (-> result
          (assoc :model (cond-> (update-in model (conj path :stages) dissoc stage-id)
                          (get-in model [:control :edit-history])
                          (update-in [:control :edit-history] rekey-history (stage-instance-id stage-id) instance-id)))
          (update :ack assoc :stage-id stage-id))
      result)))

(defn- envelope [payload]
  (select-keys payload [:show-id :epoch :source]))

(defn- settle-gestures
  "Close every open gesture on the stage under its own actor before the
  values freeze: an ordinary gesture commits, a temporary one rolls back."
  [model stage-id payload apply-command]
  (let [instance-id (stage-instance-id stage-id)
        open (sort-by (comp pr-str key)
                      (filter (fn [[[kind id]]] (and (= :instance-param kind) (= instance-id id)))
                              (get-in model [:control :edit-history :open])))]
    (reduce (fn [model [index [[_ _ param] {:keys [actor control-id gesture-id last-sequence temporary?]}]]]
              (let [op (if temporary? :gesture/cancel :gesture/end)
                    command (cond-> (merge (envelope payload)
                                           {:op op :instance-id instance-id :param param :control-id control-id
                                            :gesture-id gesture-id
                                            :command-id (str (:command-id payload) "/settle/" index)})
                              (not temporary?) (assoc :last-sequence last-sequence))
                    result (through model command #(apply-command %1 %2 {:operator-id actor}))]
                (if (accepted? result) (:model result) model)))
            model (map-indexed vector open))))

(defn- launch [model {:keys [stage-id command-id grid raw] :as payload} {:keys [command schedule]}]
  (let [cached (get-in model [:control :history command-id])
        receipt (cue-timing/receipt model command-id)
        current (stage model stage-id)]
    (cond
      (not (stage-id? stage-id))
      (rejected model payload :invalid-stage "A stage ID is a string of 1 to 128 characters")

      (or cached receipt)
      (if (= stage-id (get-in cached [:command :stage-id] (when receipt stage-id)))
        (if cached
          {:model model :events [] :ack (assoc (:ack cached) :stage-id stage-id) :replayed? true}
          {:model model :events [] :ack (assoc receipt :duplicate? true)})
        (rejected model payload :command-id-conflict "Command ID reused with different data"))

      (nil? current) (rejected model payload :unknown-stage "No staged cue has that ID")

      (= :scheduled (:status current))
      (rejected model payload :launch-pending "This staged cue is already scheduled to launch")

      :else
      (let [settled (settle-gestures model stage-id payload command)
            frozen (get-in (state settled) [:stages stage-id])
            request (merge (envelope payload)
                           {:op :cue/launch :cue-id (:cue-id frozen) :stage-id stage-id :command-id command-id
                            :launch (assoc (merge {:mode :persistent} (:launch frozen)) :params (:params frozen))})
            scheduled (update-in settled (conj path :stages stage-id) assoc
                                 :status :scheduled :launch-command-id command-id)
            result (if (and (some? grid) (not= :off grid))
                     (schedule scheduled (cond-> {:command request :grid grid} raw (assoc :raw raw)))
                     (command scheduled request nil))]
        (if (#{:accepted :pending :fired} (get-in result [:ack :status]))
          result
          (assoc result :model model))))))

(defn- edit [model {:keys [stage-id param value command-id control-id] :as payload} authority apply-command]
  (let [current (stage model stage-id)
        instance-id (stage-instance-id stage-id)
        cached (get-in model [:control :history command-id])
        schema (get-in current [:definition :params param])
        base (merge (envelope payload) {:instance-id instance-id :param param})]
    (cond
      (not (and (stage-id? stage-id) (keyword? param) (some? command-id)))
      (rejected model payload :invalid-command "A staged edit needs a stage, a parameter and a command ID")

      cached
      (if (= [instance-id param] ((juxt :instance-id :param) (:command cached)))
        {:model model :events [] :ack (:ack cached) :replayed? true}
        (rejected model payload :command-id-conflict "Command ID reused with different data"))

      (nil? (:operator-id authority))
      (rejected model payload :operator-required "Edit history requires a trusted operator")

      (not= :staged (:status current))
      (rejected model payload :stage-unavailable "That cue is no longer staged for editing")

      (nil? schema)
      (rejected model payload :invalid-params "The staged cue has no such parameter")

      ;; Only numeric parameters have history steps; other types are set directly.
      (not (#{:number :integer} (:type schema)))
      (through model (assoc base :op :param/set :value value :command-id command-id)
               #(apply-command %1 %2 nil))

      :else
      (let [gesture (assoc base :control-id (or control-id "cue-stage/edit") :gesture-id command-id)
            run #(apply-command %1 %2 authority)
            materialized (materialize model)
            steps [(assoc gesture :op :gesture/begin :command-id (str command-id "/begin"))
                   (assoc gesture :op :gesture/sample :sequence 1 :value value :command-id (str command-id "/sample"))
                   (assoc gesture :op :gesture/end :last-sequence 1 :command-id command-id)]
            outcome (reduce (fn [{:keys [model events]} step]
                              (let [result (run model step)]
                                (if (accepted? result)
                                  {:model (:model result) :events (into events (:events result)) :ack (:ack result)}
                                  (reduced (assoc result :failed? true)))))
                            {:model materialized :events []} steps)]
        (if (:failed? outcome)
          {:model model :events [] :ack (assoc (:ack outcome) :command-id command-id)}
          (update outcome :model #(dematerialize model %)))))))

(defn dispatch
  "Owner admission for :cue-stage/edit, :cue-stage/launch and the
  scheduled-launch half of :cue-stage/cancel. Host functions:
  :command ([model payload authority]) admits through the show reducer, :schedule ([model request]) queues a grid launch and
  :cancel-launch ([model command-id]) withdraws a pending grid launch."
  [model {:keys [op] :as payload} authority {:keys [command cancel-launch] :as host}]
  (let [payload (cond-> payload (nil? (:command-id payload)) (assoc :command-id (str (random-uuid))))]
    (case op
      :cue-stage/edit (edit model payload authority command)
      :cue-stage/launch (launch model payload host)
      :cue-stage/open (command model payload nil)
      :cue-stage/cancel
      (let [{:keys [status launch-command-id]} (stage model (:stage-id payload))
            withdrawn (if (= :scheduled status) (cancel-launch model launch-command-id) model)
            result (command withdrawn payload nil)]
        (if (accepted? result) result (assoc result :model model))))))

(defn projection
  "Staged cues as Studio, Push and the REPL read them."
  [model]
  (mapv (fn [{:keys [id cue-id effect-id definition params launch status launch-command-id]}]
          (let [receipt (when launch-command-id (cue-timing/receipt model launch-command-id))]
            (cond-> {:stage-id id :instance-id (stage-instance-id id) :cue-id cue-id
                     :cue-label (or (get-in model [:registry :cues cue-id :name]) cue-id)
                     :effect-id effect-id :effect-label (or (:name definition) effect-id)
                     :status status :params params :schema (get definition :params {}) :launch launch}
              (= :scheduled status)
              (assoc :launch-command-id launch-command-id
                     :due-beat (some-> (:due-beat receipt) double) :grid (:grid receipt)))))
        (stages model)))

(defn without-stages
  "Drop live stages from a saved or restored model."
  [model]
  (update model :control dissoc ::stages))
