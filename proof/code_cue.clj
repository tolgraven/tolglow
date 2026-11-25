(ns proof.code-cue
  "Headless proof of a source-owned cue and its validation/admission boundary."
  (:require [afterglow.control.model :as control]
            [afterglow.render.frame :as render]
            [afterglow.runtime.engine :as engine]
            [sci.core :as sci]
            [tolglow.compiler.batch :as batch]
            [tolglow.registry :as registry])
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest]
           [java.util HexFormat]))

(defn- fail! [reason message]
  (throw (ex-info message {:reason reason})))

(defn- source-bytes [source]
  (when-not (and (string? source) (<= (count source) 16384))
    (fail! :source-size "Use a Clojure buffer of at most 16,384 UTF-8 bytes."))
  (let [bytes (.getBytes ^String source StandardCharsets/UTF_8)]
    (when (> (alength bytes) 16384)
      (fail! :source-size "Use a Clojure buffer of at most 16,384 UTF-8 bytes."))
    bytes))

(defn- sha256 [source]
  (.formatHex (HexFormat/of) (.digest (MessageDigest/getInstance "SHA-256") (source-bytes source))))

(defn- guest []
  ;; No show namespaces, application vars, host class bindings or file loader.
  (sci/init {:allow '[let let* if + - * / min max get assoc merge vector hash-map
                      throw ex-info]}))

(defn- parse-draft [context source]
  (source-bytes source)
  (let [reader (sci/reader source)
        form (sci/parse-next context reader)]
    (when (= ::sci/eof form)
      (fail! :missing-form "Enter one Clojure expression."))
    (when-not (= ::sci/eof (sci/parse-next context reader))
      (fail! :extra-form "Use one Clojure expression for this cue."))
    form))

(defn syntax-diagnostics
  "Reader diagnostics for an editor draft. This path never evaluates the form."
  [source]
  (try
    (parse-draft (guest) source)
    []
    (catch Exception error
      [(merge {:message (ex-message error)} (select-keys (ex-data error) [:line :column :reason]))])))

(defn- cue-basis [model cue-id]
  (let [registry (:registry model)
        cue (get-in registry [:cues cue-id])
        effect-id (:effect-id cue)
        effect (get-in registry [:effects effect-id])
        active (filter #(= effect-id (:effect-id %)) (vals (:instances model)))]
    (when-not (and (:code-cue? cue) (nil? (:effect-version cue))
                   (= cue-id (:code/cue-id effect)) (string? (:code/source effect))
                   (not-any? #(and (= effect-id (:effect-id %)) (not= cue-id (:id %)))
                             (vals (:cues registry)))
                   (every? #(and (= cue-id (:cue-id %)) (= :running (:phase %))
                                 (= :preserve-state (:reload-policy %))
                                 (= (:version effect) (:effect-version %))) active))
      (fail! :cue-source-unavailable "Select a source-owned cue with a compatible running revision."))
    {:show-id (:show-id model) :epoch (:epoch model)
     :cue-id cue-id :cue-version (:version cue) :effect-id effect-id :effect-version (:version effect)
     :source (:code/source effect) :fixtures (get-in model [:rig :fixtures])
     :instances (mapv #(select-keys % [:id :targets :params]) (sort-by :id active))}))

(defn open-source [model cue-id]
  (dissoc (cue-basis model cue-id) :instances :fixtures))

(defn previous-source [model cue-id version]
  (let [{:keys [effect-id effect-version]} (cue-basis model cue-id)
        entry (get-in model [:registry :versions [:effects effect-id version]])]
    (when (and (integer? version) (< version effect-version) (= cue-id (:code/cue-id entry)))
      (:code/source entry))))

(defn- publish [model definition command-id]
  (let [result (control/apply-command model {:op :registry/publish :kind :effects :definition definition
                                             :show-id (:show-id model) :epoch (:epoch model) :time-ms (:time-ms model)
                                             :source :code-editor :command-id command-id})]
    (when-not (= :accepted (get-in result [:ack :status]))
      (throw (ex-info "Registry refused the candidate." {:reason :publication-rejected :ack (:ack result)})))
    result))

(defn- candidate-model [model {:keys [cue-id instances]} definition]
  (let [model (assoc model :instances (select-keys (:instances model) (map :id instances)))
        model (:model (publish model definition (str (random-uuid))))]
    (if (seq instances)
      model
      (let [result (control/apply-command model {:op :cue/launch :cue-id cue-id :show-id (:show-id model)
                                                 :epoch (:epoch model) :time-ms (:time-ms model)
                                                 :source :code-editor :command-id (str (random-uuid))
                                                 :launch {:mode :persistent :reload-policy :preserve-state}})]
        (when-not (= :accepted (get-in result [:ack :status]))
          (throw (ex-info "Candidate cue could not launch in preview." {:reason :preview-launch :ack (:ack result)})))
        (:model result)))))

(defn- exercise [model {:keys [fixtures] :as basis} definition]
  (let [candidate (candidate-model model basis definition)
        time-ms (max (:time-ms candidate) (:last-frame-time-ms candidate 0))
        frame (engine/advance candidate {:time-ms time-ms :fixtures fixtures}
                              (batch/compiler-for-rig fixtures))
        instances (vals (get-in frame [:model :instances]))]
    (when (or (empty? instances)
              (some #(or (:fault %) (:reload-fault %) (not= (:version definition) (:effect-version %))) instances)
              (some #(#{:effect/failed :effect/reload-failed} (:type %)) (:intents frame)))
      (fail! :preview-failed "The candidate did not produce a healthy frame."))
    (:values (render/resolve-contributions fixtures (:contributions frame)))))

(defn check-draft
  "For the trusted fixture harness. A host must run guest evaluation in a
  bounded worker process before exposing this operation to submitted drafts."
  [model cue-id source]
  (let [{:keys [effect-id effect-version] :as basis} (cue-basis model cue-id)
        context (guest)
        form (parse-draft context source)
        content (sci/eval-form context form)]
    (when-not (and (map? content) (every? #{:name :params :outputs} (keys content)))
      (fail! :invalid-definition "Return an effect map with name, parameters and outputs."))
    (let [definition (assoc content :id effect-id :version (inc effect-version)
                            :code-cue? true :auto? false :code/cue-id cue-id :code/source source)
          errors (registry/validate-definition :effects definition)]
      (when (seq errors)
        (throw (ex-info "Invalid effect definition." {:reason :invalid-definition :errors errors})))
      {:basis basis :source-sha256 (sha256 source) :definition definition
       :preview-values (exercise model basis definition)})))

(defn apply-checked
  "Consume a host-held candidate and the exact current buffer through the
  model owner's command boundary. Never accept a candidate from a client."
  [model cue-id source {:keys [basis source-sha256 definition]}]
  (when-not (and (= source-sha256 (sha256 source)) (= source (:code/source definition)))
    (fail! :draft-changed "The buffer changed after the preview check."))
  (when-not (= basis (cue-basis model cue-id))
    (fail! :stale-cue "The cue, running instance, rig or show changed after the preview check."))
  (publish model definition (str (random-uuid))))
