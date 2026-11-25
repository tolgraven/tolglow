(ns tolglow.show.scene-draft
  "Private declarative Room audition and a candidate transition for the show owner."
  (:require [afterglow.render.frame :as render]
            [afterglow.runtime.engine :as engine]
            [tolglow.compiler :as compiler]
            [tolglow.effects.prefabs :as prefabs]
            [tolglow.registry :as registry]
            [tolglow.spatial :as spatial]
            [tolglow.spatial.targets :as targets]))

(def ^:private max-operations 32)

(def ^:private operation-fields
  {:registry/publish #{:kind :definition}
   :cue/put #{:cue :expected-version}
   :cue/launch #{:cue-id :launch}
   :effect/launch #{:effect-id :launch}
   :instance/end #{:instance-id :force?}})

(defn- fail! [code message details]
  (throw (ex-info message (assoc details :code code))))

(defn- valid-id? [id]
  (and (string? id) (<= 1 (count id) 96)))

(defn- check-request! [{:keys [op kind launch] :as request}]
  (when-not (and (map? request) (contains? operation-fields op)
                 (every? (conj (operation-fields op) :op) (keys request))
                 (registry/data? request)
                 (or (not= :registry/publish op) (= :effects kind))
                 (or (not (contains? request :launch)) (map? launch))
                 (or (not (contains? launch :targets)) (vector? (:targets launch)))
                 (not= :held (:mode launch)))
    (fail! :invalid-draft-operation "This operation cannot be staged in a Room draft" {:op op})))

(defn- private-model [model]
  (let [candidate (-> (select-keys model [:show-id :epoch :revision :time-ms :last-frame-time-ms
                                          :last-frame-beat :frame-number :registry :instances :pages
                                          :rig :variables :transport :master :blackout? :settings :extensions])
                      (assoc :control (merge (select-keys (:control model) [:next-order :next-instance :history-limit])
                                             {:history {} :history-order []})))]
    (when-not (registry/data? candidate)
      (fail! :non-portable-model "Room audition requires an immutable data snapshot" {}))
    candidate))

(defn- definitions [model {:keys [op definition cue cue-id effect-id]}]
  (let [registry (:registry model)
        cue (or cue (get-in registry [:cues cue-id]))]
    (case op
      :registry/publish [:effects definition]
      :cue/put [:cues cue]
      :cue/launch [:effects (registry/cue-effect registry cue)]
      :effect/launch [:effects (get-in registry [:effects effect-id])]
      nil)))

(defn- validate-targets! [model ids]
  (when (seq ids)
    (let [known (into #{} (map :id) (get-in model [:rig :fixtures]))]
      (when-let [unknown (seq (remove known ids))]
        (fail! :unknown-draft-target "A staged target is absent from the fixture patch"
               {:target-ids (vec unknown)})))))

(defn- validate-authoring! [model {:keys [op] :as request}]
  (when-let [[kind definition] (definitions model request)]
    (when (#{:registry/publish :cue/put} op)
      (when-let [errors (seq (registry/validate-definition kind definition))]
        (fail! :invalid-draft-definition "A staged definition is invalid" {:errors (vec errors)})))
    (when (and (= kind :effects) definition)
      (validate-targets! model (into (:targets definition [])
                                     (mapcat :targets)
                                     (registry/output-entries (:outputs definition))))
      (when (= op :registry/publish)
        (compiler/compile-effect definition)))))

(defn- operation [model id phase time-ms step request]
  (let [command (->> (merge request
                            {:show-id (:show-id model) :epoch (:epoch model) :time-ms time-ms
                             :source {:adapter :scene-draft :session-id id}
                             :command-id (str "room-draft:" (name phase) ":" id ":" step)})
                     (prefabs/prepare-command model))
        _ (when (= phase :stage)
            (validate-authoring! model command)
            (validate-targets! model (get-in command [:launch :targets])))
        selected (when-let [selector (get-in command [:cue :selector])]
                   (try (targets/resolve-ids model selector)
                        (catch clojure.lang.ExceptionInfo error
                          (fail! :invalid-draft-target (ex-message error) (assoc (ex-data error) :step step)))))
        {:keys [ack events] next-model :model} (spatial/apply-command model command)
        instance (get-in next-model [:instances (:instance-id ack)])
        launch? (#{:cue/launch :effect/launch} (:op request))]
    (when (or (not= :accepted (:status ack)) (empty? events))
      (fail! (get ack :code :duplicate-draft-command)
             (or (:message ack) "A staged operation could not be applied")
             {:step step :op (:op request) :operation-ack ack}))
    {:model next-model :ack ack :targets (if launch? (:targets instance) selected)}))

(defn- instance-stamp [instance]
  (assoc (select-keys instance [:id :cue-id :definition :targets :started-at-ms :order :phase])
         :system (select-keys (:system instance) [:attack :release :paused? :opacity :gain])))

(defn- ack [model status details]
  (merge (select-keys model [:show-id :epoch :revision]) {:status status} details))

(defn- stage [model id revision requests]
  (try
    (when-not (and (valid-id? id) (vector? requests) (<= (count requests) max-operations))
      (fail! :invalid-draft "Provide a draft ID and up to 32 staged operations" {}))
    (let [base (private-model model)
          {:keys [model steps]}
          (reduce (fn [{:keys [model steps]} [step request]]
                    (check-request! request)
                    (let [instance-id (:instance-id request)
                          stamp (when instance-id (instance-stamp (get-in model [:instances instance-id])))]
                      (when (and instance-id (not (get-in base [:instances instance-id])))
                        (fail! :unknown-draft-instance "Only committed instances can be released by ID"
                               {:step step :instance-id instance-id}))
                      (let [{:keys [model targets]} (operation model id :stage (:time-ms model) step request)]
                        {:model model :steps (conj steps {:request request :targets targets :instance stamp})})))
                  {:model base :steps []}
                  (map-indexed vector requests))]
      {:ack (ack base :accepted {:draft-id id :draft-revision revision})
       :draft {:id id :revision revision :base base :steps steps :model model}})
    (catch clojure.lang.ExceptionInfo error
      (let [details (ex-data error)]
        {:ack (ack model :rejected (merge {:code :invalid-draft-operation} details
                                         {:message (ex-message error)}))}))))

(defn begin
  "Capture an editable draft from the committed show. Re-staging a draft uses
  the same base rather than inheriting later edits from the live show."
  [model id requests]
  (stage model id 0 requests))

(defn revise [draft requests]
  (stage (:base draft) (:id draft) (inc (:revision draft)) requests))

(defn sample
  "Render private preview data at a supplied logical time and beat. Return
  optical values only. Frame packets, intents and callbacks never escape."
  [{:keys [id revision model]} time-ms beat]
  (let [fixtures (get-in model [:rig :fixtures])
        private (assoc-in model [:transport :beat] beat)
        result (engine/advance private {:time-ms time-ms :fixtures fixtures} compiler/compile-effect)
        frame (render/resolve-contributions fixtures (:contributions result)
                                            (select-keys model [:master :blackout?]))]
    {:id id :kind :audition :draft-revision revision :show-id (:show-id model) :epoch (:epoch model)
     :time-ms time-ms :beat beat :output (select-keys frame [:values :target-order])
     :fault-events (:fault-events result)}))

(defn apply-draft
  "Return one complete candidate transition for an existing show owner. The
  caller supplies a transaction ID, expected draft revision and commit time.
  Any conflict/rejection returns the input model, including its pending events."
  [live {:keys [base steps id revision]} {:keys [transaction-id expected-draft-revision]} time-ms]
  (try
    (when-not (and base (seq steps) (valid-id? transaction-id))
      (fail! :invalid-draft "An existing draft and a transaction ID are required" {}))
    (when-not (= revision expected-draft-revision)
      (fail! :stale-draft "The draft changed before application" {:draft-revision revision}))
    (let [fences [[:show-id :wrong-show] [:epoch :stale-epoch] [:rig :stale-patch]
                  [:registry :stale-registry]]
          changed (some (fn [[field code]] (when-not (= (get base field) (get live field)) code)) fences)]
      (when changed
        (fail! changed "The draft's show, rig or definition dependencies changed" {:draft-id id})))
    (when-not (= (prefabs/catalog base) (prefabs/catalog live))
      (fail! :stale-prefabs "The draft's material library changed" {:draft-id id}))
    (let [{:keys [model receipts]}
          (reduce (fn [{:keys [model receipts]} [step {:keys [request targets instance]}]]
                    (when (and instance (not= instance (instance-stamp (get-in model [:instances (:instance-id request)]))))
                      (fail! :stale-instance "A staged instance changed or ended" {:step step :instance-id (:instance-id request)}))
                    (let [{next-model :model applied :ack resolved :targets}
                          (operation model transaction-id :apply time-ms step request)]
                      (when-not (= targets resolved)
                        (fail! :stale-targets "A staged selector resolved to different fixtures"
                               {:step step :expected targets :actual resolved}))
                      {:model next-model :receipts (conj receipts applied)}))
                  {:model live :receipts []}
                  (map-indexed vector steps))]
      {:model model
       :ack (ack model :accepted {:draft-id id :transaction-id transaction-id :operations receipts})})
    (catch clojure.lang.ExceptionInfo error
      {:model live
       :ack (ack live :rejected (merge {:code :invalid-draft-commit} (ex-data error)
                                      {:draft-id id :message (ex-message error)}))})))
