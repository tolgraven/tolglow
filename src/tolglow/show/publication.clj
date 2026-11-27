(ns tolglow.show.publication
  "Definition publications checked once, off the frame owner (#784).

  A `:definition` message publishes the revision `revision` derives from the
  payload and the registry. Its pure checks (`check`: schema/registry
  validation, the effect compile and the modulation period) depend only on
  that revision, so with a worker they run before the owner sees the message,
  against the last committed model. The owner derives the revision again
  against its current model and adopts the worker's verdict when both
  revisions are equal; it then only decides acceptance against the current
  registry (version order, dependent cues) and commits. Without a worker the
  owner runs `check` itself, once."
  (:require [tolglow.authoring.output-patch :as output-patch]
            [tolglow.compiler :as compiler]
            [tolglow.effects.prefabs :as prefabs]
            [tolglow.registry :as registry]))

(defn revision
  "The revision a :definition `payload` publishes against `model`: an output
  patch applied to the current revision, and the next version when the payload
  names its expected version or none. Throws as the owner refuses a stale or
  malformed edit."
  [model {:keys [kind definition] :as payload}]
  (let [previous (get-in model [:registry kind (:id definition)])
        _ (when (and (contains? payload :expected-version) (not= (:expected-version payload) (:version previous)))
            (throw (ex-info "Definition changed since editing began" {:current-version (:version previous)})))
        definition (if-let [edit (:output-patch payload)]
                     (do (when-not (and (= kind :effects) previous (contains? payload :expected-version))
                           (throw (ex-info "An output patch needs the current effect revision" {})))
                         (output-patch/apply-patch previous edit)) definition)]
    (cond-> definition
      (or (contains? payload :expected-version) (nil? (:version definition)))
      (assoc :version (inc (get previous :version 0))))))

(defn captured
  "`definition` as its publication compiles and stores it: an effect snapshots
  the shared prefab stages it uses from `model`."
  [model kind definition]
  (if (= :effects kind) (prefabs/capture (prefabs/catalog model) definition) definition))

(defn check
  "The pure checks of publishing `definition` (captured as `captured`), in the
  owner's order: validation, then an effect's compile, then its modulation
  period. Returns {:definition :captured :failure :errors}: :failure is the
  exception the owner throws (its message and data become the refusal), and
  :errors the period errors the publication is refused with.

  The definition is validated once. An effect without shared stages compiles
  the same object without validating it again; one that captured stages
  differs from what was validated, so its compile validates the capture."
  [kind definition captured]
  (try
    (let [errors (registry/validate-definition kind definition)]
      (when (seq errors) (throw (ex-info "Invalid definition" {:errors errors})))
      (when (= :effects kind)
        (binding [compiler/*validated* (when (identical? definition captured) captured)]
          (compiler/compile-effect captured)))
      {:definition definition :captured captured :failure nil
       :errors (or (compiler/publication-period-errors kind captured) [])})
    (catch Exception failure
      {:definition definition :captured captured :failure failure})))

(defn prepare
  "Derive and check a :definition `payload` against `model`, as a worker does
  before the owner sees it. Returns the `check` result, or {:unchecked true}
  when the revision cannot be derived against this model (the owner derives
  it again and refuses it, or checks it once more) or checking failed with a
  VM error, which the owner then meets itself."
  [model {:keys [kind] :as payload}]
  (try
    (let [definition (revision model payload)
          result (check kind definition (captured model kind definition))]
      (if (instance? InterruptedException (:failure result)) {:unchecked true} result))
    (catch Throwable _ {:unchecked true})))

(defn verdict-for
  "`prepared`'s verdict when it checked exactly `definition` and `captured`,
  else nil."
  [prepared definition captured]
  (when (and (not (:unchecked prepared))
             (= definition (:definition prepared))
             (= captured (:captured prepared)))
    prepared))
