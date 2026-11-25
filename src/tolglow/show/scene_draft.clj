(ns tolglow.show.scene-draft
  "Data for a Room audition: a staged scene on a private copy of the show and
  the single show-owner transaction that applies it. The owner renders the
  private model through its own frame path; nothing here reads clocks,
  renders, sends output or calls back into adapters."
  (:require [afterglow.control.model :as control]
            [afterglow.runtime.events :as staging]
            [afterglow.time :as musical-time]
            [tolglow.compiler :as compiler]
            [tolglow.effects.prefabs :as prefabs]
            [tolglow.registry :as registry]
            [tolglow.spatial :as spatial]
            [tolglow.spatial.targets :as targets]))

(def operations
  "Owner commands for a Room audition. Stage and discard leave the show model
  untouched. Apply is one transaction with one command ID and one revision."
  #{:scene-draft/stage :scene-draft/apply :scene-draft/discard})

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

(def ^:private private-keys
  "Show data the audition frame reads. Owner bookkeeping, retry history and
  pending events stay with the live show."
  [:show-id :epoch :revision :time-ms :last-frame-time-ms :last-frame-beat :frame-number
   :registry :instances :pages :rig :variables :transport :master :blackout? :settings
   :extensions :modulation :responses :compositions :composition-runs :composition-tokens
   :waveforms])

(defn- private-model [model]
  (let [candidate (-> (select-keys model private-keys)
                      (assoc :control (merge (select-keys (:control model) [:next-order :next-instance :history-limit])
                                             {:history {} :history-order []})))]
    (when-not (registry/data? candidate)
      (fail! :non-portable-model "Room audition requires an immutable data snapshot"
             {:fields (vec (remove #(registry/data? (get candidate %)) (keys candidate)))}))
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

(defn- operation
  "Admit one staged request through the shared command path. Staging and
  applying use the same source, so both timelines create identical instances."
  [model draft-id phase time-ms step request]
  (let [command (->> (merge request
                            {:show-id (:show-id model) :epoch (:epoch model) :time-ms time-ms
                             :source {:adapter :scene-draft :session-id draft-id}
                             :command-id (str "scene-draft:" draft-id ":" step)})
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
    {:model next-model :ack ack :events events :targets (if launch? (:targets instance) selected)}))

(defn- instance-stamp [instance]
  (assoc (select-keys instance [:id :cue-id :definition :targets :started-at-ms :order :phase])
         :system (select-keys (:system instance) [:attack :release :paused? :opacity :gain])))

(defn- ack [model status details]
  (merge (select-keys model [:show-id :epoch :revision]) {:status status} details))

(defn- rejection [model error]
  {:ack (ack model :rejected (merge {:code :invalid-draft-operation} (ex-data error)
                                    {:message (ex-message error)}))})

(defn- stage* [model id revision requests time-ms]
  (when-not (and (valid-id? id) (vector? requests) (<= (count requests) max-operations))
    (fail! :invalid-draft "Provide a draft ID and up to 32 staged operations" {}))
  (let [base (private-model model)
        {staged :model steps :steps}
        (reduce (fn [{:keys [model steps]} [step request]]
                  (check-request! request)
                  (let [instance-id (:instance-id request)
                        stamp (when instance-id (instance-stamp (get-in model [:instances instance-id])))]
                    (when (and instance-id (not (get-in base [:instances instance-id])))
                      (fail! :unknown-draft-instance "Only committed instances can be released by ID"
                             {:step step :instance-id instance-id}))
                    (let [{:keys [model targets]} (operation model id :stage time-ms step request)]
                      {:model model :steps (conj steps {:request request :targets targets :instance stamp})})))
                {:model base :steps []}
                (map-indexed vector requests))]
    {:ack (ack base :accepted {:draft-id id :draft-revision revision})
     :draft {:id id :revision revision :staged-at-ms time-ms :base base :steps steps
             :model (assoc staged :revision (:revision base))}}))

(defn stage
  "Stage requests against the committed show at logical time-ms. Each staging
  captures a fresh private copy, so a revision sees the live show it replaces.
  A different draft must be discarded first. Returns {:ack} and, when
  accepted, the next {:draft}. Rejection leaves the current draft in place."
  [model current {:keys [draft-id operations] :as request} time-ms]
  (try
    (cond
      (and current (not= draft-id (:id current)))
      (fail! :draft-active "Discard the current Room draft first" {:draft-id (:id current)})

      (and current (contains? request :expected-draft-revision)
           (not= (:expected-draft-revision request) (:revision current)))
      (fail! :stale-draft "The draft changed before this edit" {:draft-revision (:revision current)})

      :else (stage* model draft-id (if current (inc (:revision current)) 0) operations time-ms))
    (catch clojure.lang.ExceptionInfo error
      (rejection model error))))

(def ^:private fences
  [[:show-id :wrong-show] [:epoch :stale-epoch] [:rig :stale-patch] [:registry :stale-registry]])

(defn changed-dependency
  "The refusal code of the first show dependency that changed since the draft
  was staged, or nil while it can still be applied. Apply refuses with the
  same code, so an audition can report it before anyone applies."
  [live {:keys [base]}]
  (or (some (fn [[field code]] (when-not (= (get base field) (get live field)) code)) fences)
      (when-not (= (prefabs/catalog base) (prefabs/catalog live)) :stale-prefabs)))

(defn- check-current! [live {:keys [base steps id revision] :as draft} expected-revision]
  (when-not (and base (seq steps))
    (fail! :empty-draft "Stage at least one operation before applying" {:draft-id id}))
  (when-not (= revision expected-revision)
    (fail! :stale-draft "The draft changed before application" {:draft-revision revision}))
  (when-let [changed (changed-dependency live draft)]
    (fail! changed (if (= :stale-prefabs changed)
                     "The draft's material library changed"
                     "The draft's show, rig or definition dependencies changed")
           {:draft-id id})))

(defn- replay
  "Run every staged step on the live model at one logical time."
  [live {:keys [id steps]} time-ms]
  (reduce (fn [{:keys [model events receipts]} [step {:keys [request targets instance]}]]
            (when (and instance (not= instance (instance-stamp (get-in model [:instances (:instance-id request)]))))
              (fail! :stale-instance "A staged instance changed or ended" {:step step :instance-id (:instance-id request)}))
            (let [{next-model :model applied :ack resolved :targets step-events :events}
                  (operation model id :apply time-ms step request)]
              (when-not (= targets resolved)
                (fail! :stale-targets "A staged selector resolved to different fixtures"
                       {:step step :expected targets :actual resolved}))
              {:model next-model :events (into events step-events)
               :receipts (conj receipts (select-keys applied [:status :instance-id]))}))
          {:model live :events [] :receipts []}
          (map-indexed vector steps)))

(defn- transaction-operation [draft]
  (fn [live {:keys [expected-draft-revision time-ms]}]
    (try
      (check-current! live draft expected-draft-revision)
      ;; Steps run as private sub-operations. The enclosing admission records
      ;; one history entry, one revision and one batch of events.
      (let [pending? (contains? live ::staging/pending)
            {:keys [model events receipts]} (replay (staging/finish live) draft time-ms)]
        {:model (cond-> (-> model
                            (assoc :revision (:revision live))
                            (assoc-in [:control :history] (get-in live [:control :history]))
                            (assoc-in [:control :history-order] (get-in live [:control :history-order])))
                  pending? (assoc ::staging/pending (staging/pending live)))
         :events (conj events {:type :scene-draft/applied :draft-id (:id draft)
                               :draft-revision (:revision draft) :operations (count receipts)})
         :result {:draft-id (:id draft) :draft-revision (:revision draft) :operations receipts}})
      (catch clojure.lang.ExceptionInfo error
        (let [{:keys [code] :as details} (ex-data error)]
          (throw (ex-info (ex-message error)
                          {:control/error (or code :invalid-draft-commit)
                           :details (dissoc details :code)})))))))

(defn apply-draft
  "Apply a staged draft through the owner's command admission as one
  transaction. The command carries :command-id, :source, :time-ms and
  :expected-draft-revision. Any stale dependency or failed step rejects the
  whole transaction and returns the input model, including pending events."
  [live draft command]
  (let [result (control/apply-domain-command live (assoc command :op :scene-draft/apply)
                                             (transaction-operation draft))]
    (cond-> result
      (= :rejected (get-in result [:ack :status]))
      (update :ack #(merge % (:details %) {:draft-id (:id draft)})))))

(defn grid-boundary
  "Return the beat of the next boundary of grid ({:beats n}, {:bars n} or
  {:phrases n}) at or after beat, measured from beat zero in the transport meter."
  [grid {:keys [beat meter]}]
  (when-not (and (map? grid) (= 1 (count grid)) (empty? (musical-time/validate-duration grid))
                 (not (contains? grid :ms)) (pos? (val (first grid))) (number? beat))
    (throw (ex-info "Quantized apply needs one positive beat, bar or phrase grid" {:code :invalid-grid :grid grid})))
  (let [meter (musical-time/normalize-meter meter)
        interval (musical-time/duration->beats grid meter)
        lower (* interval (Math/floor (/ (double beat) interval)))]
    (if (== beat lower) lower (+ lower interval))))

(defn boundary-time
  "The logical time at which the transport crossed boundary-beat, given its
  sample at time-ms. Never earlier than floor-ms or later than time-ms."
  [{:keys [beat bpm]} time-ms boundary-beat floor-ms]
  (let [elapsed (if (and (number? bpm) (pos? bpm)) (/ (* (- beat boundary-beat) 60000.0) bpm) 0.0)]
    (min time-ms (max floor-ms (- time-ms elapsed)))))

(defn summary
  "Portable projection of a draft for Studio and the REPL."
  [{:keys [id revision staged-at-ms steps]}]
  {:id id :revision revision :staged-at-ms staged-at-ms
   :operations (mapv (fn [{:keys [request targets]}]
                       (cond-> (select-keys request [:op :cue-id :effect-id :instance-id :kind])
                         (:definition request) (assoc :definition-id (get-in request [:definition :id]))
                         (:cue request) (assoc :cue-id (get-in request [:cue :id]))
                         (seq targets) (assoc :targets targets)))
                     steps)})
