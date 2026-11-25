(ns tolglow.controllers.content-browser-host
  "Push content and confirmed show restore through the active show owner.

  Exact launch and attach still require the frame owner to check the selected
  registry version when it resolves the cue or binds the waveform."
  (:require [tolglow.controllers.content-browser :as browser]
            [tolglow.show :as show]
            [tolglow.show.recovery :as recovery]))

(def ^:private checkpoint-handles
  {:named :checkpoint/named :recovery :checkpoint/recovery})

(defn- session [model]
  (select-keys model [:show-id :epoch]))

(defn- checkpoint-descriptors [saves {:keys [show-id checkpoints]}]
  (let [name (some #(when (= show-id (:show-id %)) (:name %)) saves)]
    (into [] (keep (fn [[kind handle]]
                     (let [modified (get-in checkpoints [kind :modified-at-ms])]
                       (when (nat-int? modified)
                         {:show-id show-id :source-kind kind :handle handle
                          :name name :revision modified :checkpoint? true}))))
          checkpoint-handles)))

(defn- source-key [{:keys [show-id source-kind]}] [show-id source-kind])

(defn- reject [reason] {:status :rejected :reason reason})

(defn- current-session? [runtime runtime-id selection]
  (and @(:running runtime) (= (:id runtime) runtime-id) (map? selection)
       (= (session (:model @(:state runtime))) (session selection))))

(defn- capture [runtime saves]
  (when-not (vector? saves)
    (throw (ex-info "Expected a vector of configured save descriptors" {})))
  (let [before (:model @(:state runtime))
        status (show/recovery-status runtime)
        model (:model @(:state runtime))
        owned (when (and (= (session before) (session model)) (= (:show-id model) (:show-id status)))
                (checkpoint-descriptors saves status))
        owned-keys (into #{} (map source-key) owned)]
    {:model model
     :saves (into (or owned []) (comp (remove #(contains? owned-keys (source-key %)))
                                     (map #(dissoc % :checkpoint?))) saves)}))

(defn project
  "Project one browser page for a runtime. Configured saves are the trusted
  descriptor vector accepted by content-browser/catalog. The active owner's
  available checkpoint takes precedence for the same show and source kind.
  Call from the UI or display worker to keep checkpoint status reads off the
  frame owner."
  [runtime configured-saves view]
  (let [{:keys [model saves]} (capture runtime configured-saves)]
    (merge (browser/project (browser/catalog model saves) view)
           (session model) {:runtime-id (:id runtime)})))

(defn- resolve-file [runtime {:keys [checkpoint? show-id source-kind path revision]}]
  (if checkpoint?
    (when-let [file (some-> (:recovery runtime) deref (recovery/latest-file show-id source-kind))]
      (when (= revision (.lastModified file)) file))
    path))

(defn- select-save [runtime {:keys [model saves]} {:keys [runtime-id selection] :as request}]
  (let [plan (browser/plan model saves (assoc request :op :load))]
    (if-let [{:keys [target-show-id source-kind handle revision]} (get-in plan [:request :payload])]
      (let [source (some #(when (= [target-show-id source-kind] (source-key %)) %) saves)
            scope [runtime-id (:show-id selection) (:epoch selection)
                   target-show-id source-kind handle revision (boolean (:checkpoint? source))]
            snapshot (recovery/read-selected-source #(resolve-file runtime source) scope)]
        (cond
          (not (current-session? runtime runtime-id selection)) (reject :stale-session)
          (nil? snapshot) (reject :stale-save-source)
          :else (assoc snapshot :target-show-id target-show-id)))
      plan)))

(defn preview
  "Describe a selected row from project. Pass its exact :selection and the
  enclosing :runtime-id. Pass a projected :source to preview that saved file
  and receive a path-free :confirmation. Preview only reads on the caller."
  [runtime configured-saves {:keys [runtime-id selection source] :as request}]
  (if-not (current-session? runtime runtime-id selection)
    (reject :stale-session)
    (let [{:keys [model saves] :as captured} (capture runtime configured-saves)
          preview (browser/plan model saves {:op :preview :selection selection})]
      (cond
        (not (current-session? runtime runtime-id selection)) (reject :stale-session)
        (or (nil? source) (= :rejected (:status preview))) preview
        :else
        (let [{:keys [token] :as selected} (select-save runtime captured request)]
          (if token
            (assoc preview :confirmation (assoc (select-keys request [:runtime-id :selection :source]) :token token))
            selected))))))

(defn confirm-load!
  "Confirm an exact saved file using preview's :confirmation. Configured
  descriptors may include a trusted :path, which never comes from the
  confirmation. The import receives frozen bytes at confirmation, and the
  frame owner rechecks the epoch before restoring. On :submitted, deref
  :receipt for the import result."
  [runtime configured-saves {:keys [runtime-id selection token] :as confirmation}]
  (cond
    (not (current-session? runtime runtime-id selection)) (reject :stale-session)
    (not (and (string? token) (re-matches #"[0-9a-f]{64}" token))) (reject :confirmation-required)
    :else
    (let [selected (select-save runtime (capture runtime configured-saves) confirmation)]
      (cond
        (= :rejected (:status selected)) selected
        (not= token (:token selected)) (reject :stale-save-source)
        :else {:status :submitted
               :receipt (show/submit! runtime :import {:runtime-id runtime-id
                                                       :expected-epoch (:epoch selection)
                                                       :expected-source-show-id (:target-show-id selected)
                                                       :source (:source selected)})}))))
