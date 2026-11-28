(ns tolglow.param-nudge
  "The owner-resolved relative parameter edit (#813):

    {:op :param/nudge :instance-id id :param k :detents n :component i :fine? b}

  moves one number, or one component of a numeric :vector, by n detents of
  its step, resolved under the owner lock against the committed value. Two
  editors that change different components of one vector at once (a Push
  encoder and a Studio drag) therefore never send each other's component
  back, and every detent counts even when commands queue up. The show turns
  the nudge into the ordinary `param/set` of the resulting whole value, so it
  is validated, recorded and treated as a human override exactly like one.

  A step is the component schema's :step, else a hundredth of its range;
  :fine? is one DMX code of the patch (Afterglow's patch-step, as the Push
  scalar encoders use), else a tenth of the step. Bounds clamp, integer
  components round. :component is required for a vector and refused for a
  number.

  A nudge whose result equals the committed value (a spin past a bound) is
  accepted without committing anything: no revision, no history, no Live
  recording. The committed param/set keeps the nudge's arguments under
  :nudge, so a command ID reused with other arguments is refused as a
  conflict instead of replaying the first."
  (:require [afterglow.control.parameter-edit :as parameter-edit]
            [tolglow.authoring.parameter-controls :as controls]))

(def max-detents
  "Largest |detents| one nudge may carry."
  1024)

(defn component-schemas
  "One number schema per component of a numeric :vector: its declared
  component schema, else the vector's bounds and unit, with bounds
  intersected with the vector's own :bounds (the range every element must
  satisfy) and the vector's :step. nil for a vector whose component range
  is empty."
  [schema]
  (let [outer (controls/bounds schema)
        step (when (and (number? (:step schema)) (pos? (:step schema))) (:step schema))
        components (mapv (fn [component]
                           (let [inner (controls/bounds component)
                                 bounds (cond (and outer inner) [(max (first outer) (first inner))
                                                                 (min (second outer) (second inner))]
                                              :else (or inner outer))]
                             (cond-> (dissoc component :bounds)
                               bounds (assoc :bounds bounds)
                               step (assoc :step step))))
                         (controls/component-schemas schema))]
    (when (every? (fn [{[low high] :bounds}] (or (nil? low) (<= low high))) components)
      components)))

;; The last patch-step cache: the per-fixture channel widths of the last
;; fixtures list a fine nudge read, reused while the list is identical, as the
;; Push scalar encoders keep it per Shift gesture. Pure memoization on an
;; immutable value; a different list replaces it.
(defonce ^:private patch-widths (atom nil))

(defn- fine-bits [model instance param]
  (let [[cache bits] (parameter-edit/patch-step @patch-widths model instance param)]
    (reset! patch-widths cache)
    bits))

(defn- refuse [message details]
  (throw (ex-info message {:control/error :invalid-nudge :details details})))

(defn- clamp [x [low high]] (max low (min high x)))

(defn- nudge-number
  "x moved by detents of schema's step (fine: one DMX code at dmx-bits, else
  a tenth of the step), clamped and rounded as schema says."
  [schema x detents fine? dmx-bits]
  (let [x (if-let [b (controls/bounds schema)] (clamp x b) x)
        integer? (= :integer (:type schema))
        x (if integer? (long (Math/round (double x))) (double x))
        edited (parameter-edit/edit schema x detents {:mode :relative :fine? (boolean fine?) :dmx-bits dmx-bits})]
    (if integer? (long edited) (double edited))))

(defn resolve-value
  "The value a nudge commits: the committed number or vector with the
  addressed number moved. Throws ExceptionInfo with :control/error
  :invalid-nudge for a nudge that does not address a running number or
  numeric vector component."
  [model {:keys [instance-id param component detents fine?]}]
  (let [instance (get-in model [:instances instance-id])
        schema (get-in instance [:definition :params param])
        value (get-in instance [:params param])]
    (when-not instance (refuse "Unknown instance" {:instance-id instance-id}))
    (when-not schema (refuse "Unknown parameter" {:param param}))
    (when-not (and (integer? detents) (not (zero? detents)) (<= (Math/abs (long detents)) max-detents))
      (refuse "A nudge needs a non-zero whole number of detents" {:detents detents :max max-detents}))
    (when-not (or (nil? fine?) (boolean? fine?)) (refuse ":fine? is a boolean" {:fine? fine?}))
    (let [bits (when fine? (fine-bits model instance param))]
      (case (:type schema)
        (:number :integer)
        (do (when (some? component) (refuse "A number has no components" {:component component}))
            (when-not (number? value) (refuse "The parameter has no numeric value" {:param param}))
            (nudge-number schema value detents fine? bits))

        :vector
        (let [schemas (component-schemas schema)
              target (get schemas component)]
          (when-not (and (integer? component) target)
            (refuse "A vector nudge needs the index of one of its components"
                    {:component component :size (count schemas)}))
          (when-not (and (vector? value) (= (count value) (count schemas)) (number? (nth value component)))
            (refuse "The vector has no numeric value for that component" {:param param}))
          (let [edited (nudge-number (assoc target :type (or (:type target) :number)) (nth value component)
                                     detents fine? bits)]
            (if (== edited (nth value component)) value (assoc value component edited))))

        (refuse "Only numbers and numeric vectors can be nudged" {:type (:type schema)})))))

(defn- arguments [payload] (select-keys payload [:component :detents :fine?]))

(defn as-set
  "What `show/command` admits for a nudge payload: the param/set of its
  resolved value, carrying the nudge's arguments as :nudge; the payload
  marked ::unchanged with its :value when the result equals the committed
  value; or the payload unchanged when it cannot be resolved (refuse! then
  rejects it through the shared receipt). previous is the command already
  recorded under the payload's command ID: a replay commits the value it
  resolved then, and other arguments make the admitted command differ from
  the recorded one, which the receipt check refuses."
  [model payload previous]
  (let [committed (get-in model [:instances (:instance-id payload) :params (:param payload)])
        value (if (= :param/set (:op previous))
                (:value previous)
                (try (resolve-value model payload)
                     (catch clojure.lang.ExceptionInfo error
                       (if (:control/error (ex-data error)) ::refused (throw error)))))]
    (cond
      (= ::refused value) payload
      (and (nil? previous) (= value committed)) (assoc payload ::unchanged true :value value)
      :else (-> (apply dissoc payload (keys (arguments payload)))
                (assoc :op :param/set :value value :nudge (arguments payload))))))

(defn unchanged-receipt
  "The receipt of a nudge that changes nothing: accepted at the current
  revision, with nothing committed. The show identity and source are
  checked as for any command."
  [model {:keys [command-id show-id epoch source value]}]
  (let [base {:command-id command-id :show-id (:show-id model) :epoch (:epoch model) :revision (:revision model)}
        refuse (fn [code message] (assoc base :status :rejected :code code :message message))]
    (cond
      (not= show-id (:show-id model)) (refuse :wrong-show "Command targets a different show")
      (not= epoch (:epoch model)) (refuse :stale-epoch "Show epoch changed")
      (nil? source) (refuse :invalid-command "Command needs a source")
      :else (assoc base :status :accepted :changed? false :value value))))

(defn refuse!
  "The domain operation for a nudge as-set could not resolve: re-resolve it
  to throw the reason inside the shared receipt."
  [model command]
  (resolve-value model command)
  (refuse "The nudge could not be resolved" {}))
