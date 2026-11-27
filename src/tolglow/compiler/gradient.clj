(ns tolglow.compiler.gradient
  "Sample a typed `:gradient` parameter as linear RGB inside the shared numeric IR.

  `{:op :gradient :key k :args [position]}` reads gradient parameter `k` at a
  scalar position: a constant, a fixture coordinate, another parameter or any
  numeric expression. Colours interpolate component by component between
  neighbouring stops, on the stored linear values, which is how Studio's ramp
  draws the same stops. Before the first stop and after the last the end
  colours hold (`:extend :clamp`, the default). `:extend :repeat` wraps the
  position into [0,1) and `:extend :mirror` reflects it back and forth.

  The node lowers to ordinary Afterglow IR: one clamped mix per segment, read
  from inputs that `expander` derives from the resolved parameter value. The
  scalar, batch and GLSL backends therefore read it unchanged. Only sampled
  gradients are expanded, at most once per changed value, never per fixture.
  The schema's stop capacity (`:max-stops`, default 8) fixes the number of
  segments, so the per-fixture cost grows with the capacity, not with the live
  value. A live value with more stops is resampled evenly (`fit-stops`)."
  (:require [afterglow.render.ir :as ir]
            [tolglow.registry :as registry]))

(def input-namespace
  "Namespace of the derived per-frame stop inputs. Authored parameter IDs may not use it."
  "tolglow.gradient")

(def extends #{:clamp :repeat :mirror})

(def max-scale
  "Largest inverse segment width. A narrower gap acts as a hard step, and the
  value stays finite in float32 GLSL."
  1.0e30)

(defn gradients
  "Each gradient parameter of a schema map, in a stable order, with the index
  that names its derived inputs and its stop capacity."
  [schemas]
  (into [] (comp (filter #(= :gradient (:type (val %))))
                 (map-indexed (fn [index [key schema]]
                                {:key key :index index :capacity (registry/gradient-capacity schema)})))
        (sort-by (comp str key) schemas)))

(defn input-key
  "The derived parameter carrying one field of stop `stop` of the gradient at
  `index` in `gradients`. Indices keep distinct parameter IDs apart."
  [index stop field]
  (keyword input-namespace (str "g" index "." stop "." (name field))))

(defn gradient-node? [expression]
  (and (map? expression) (= :gradient (:op expression))))

(defn sampled-keys
  "The parameter keys that some gradient node in `definition` samples. Reads
  the definition data only, including blocks, prefabs and shader uniforms."
  [definition]
  (into #{} (comp (filter gradient-node?) (map :key))
        (tree-seq coll? #(if (map? %) (vals %) (seq %)) definition)))

(defn- fail! [path message]
  (throw (ex-info message {:errors [{:path path :message message}]})))

(defn- const [value] {:op :const :value (double value)})

(defn- input [index stop field type] {:op :param :key (input-key index stop field) :type type})

(defn- op [operator & args] {:op operator :args (vec args)})

(defn- extended [extend position]
  (case extend
    :clamp position
    :repeat (op :fract position)
    ;; 1 - |1 - 2 fract(t/2)| runs 0→1 over [0,1] and back to 0 over [1,2].
    :mirror (op :subtract (const 1.0)
                (op :abs (op :subtract (const 1.0)
                             (op :multiply (const 2.0)
                                 (op :fract (op :multiply position (const 0.5)))))))))

(defn misuse-message
  "Why a gradient cannot be read as a plain parameter, naming the supported form."
  [key]
  (str "A :gradient parameter is not a number; sample it with {:op :gradient :key "
       (pr-str key) " :args [position]}"))

(defn lower
  "Lower a gradient node whose position argument is already prepared IR.
  Returns a vec3 expression. Errors name the authored path."
  [schemas {:keys [key args] :as node} path]
  (let [schema (get schemas key)
        extend (:extend node :clamp)]
    (when-not (every? #{:op :key :args :extend} (keys node))
      (fail! path "A gradient sample takes :key, :args [position] and an optional :extend"))
    (when-not schema (fail! (conj path :key) "Gradient sample references an unknown parameter"))
    (when-not (= :gradient (:type schema))
      (fail! (conj path :key)
             (str "Gradient samples read a :gradient parameter; " (pr-str key) " is "
                  (pr-str (:type schema)) ". Read other parameters with {:op :param}")))
    (when-not (contains? extends extend)
      (fail! (conj path :extend) "Gradient :extend must be :clamp, :repeat or :mirror"))
    (when-not (and (vector? args) (= 1 (count args)))
      (fail! (conj path :args) "A gradient sample takes exactly one position argument in :args"))
    (when (> (count (:default schema)) (registry/gradient-capacity schema))
      (fail! (conj path :key)
             (str "Gradient " (pr-str key) " has " (count (:default schema)) " default stops but samples "
                  (registry/gradient-capacity schema) "; declare :max-stops up to " registry/max-gradient-stops)))
    (let [position (first args)
          errors (ir/validate-expression position)]
      (when (seq errors)
        (fail! (into (conj path :args 0) (:path (first errors))) (:message (first errors))))
      (when-not (= :number (ir/expression-type position))
        (fail! (conj path :args 0) "A gradient position must be a scalar number"))
      (let [index (some #(when (= key (:key %)) (:index %)) (gradients schemas))
            spatial? (some #(#{:position :surface-uv} (:op %)) (tree-seq #(vector? (:args %)) :args position))
            ;; Multiplying by a floating 1.0 lets an integer fixture coordinate
            ;; keep the typed batch path; every later node is floating anyway.
            t (extended extend (if spatial? (op :multiply position (const 1.0)) position))
            colour (reduce (fn [colour stop]
                             (op :mix colour (input index stop :color :vec3)
                                 (op :clamp (op :multiply (op :subtract t (input index stop :start :number))
                                                (input index stop :scale :number))
                                     (const 0.0) (const 1.0))))
                           (input index 0 :color :vec3)
                           (range 1 (registry/gradient-capacity schema)))]
        ;; Stops are within [0,1]; the clamp keeps rounding inside the unit range.
        (op :clamp colour (const 0.0) (const 1.0))))))

(defn- colour [stop] (mapv double (:color stop)))

(defn fit-stops
  "Stops that `capacity` segments can draw. Up to `capacity` stops stay exact.
  More are resampled at `capacity` evenly spaced positions from the first stop
  to the last, so the result moves continuously as the value moves and never
  depends on which stops happen to be nearly redundant. Linear time."
  [stops capacity]
  (let [stops (vec stops)
        n (count stops)]
    (cond
      (<= n capacity) stops
      (= 1 capacity) [(first stops)]
      :else
      (let [from (double (:position (first stops)))
            to (double (:position (peek stops)))
            step (/ (- to from) (dec capacity))]
        (loop [j 0, i 0, out (transient [])]
          (if (= j capacity)
            (persistent! out)
            (let [q (if (= j (dec capacity)) to (+ from (* j step)))
                  ;; Advance to the segment [i, i+1] that holds q.
                  i (loop [i i] (if (and (< i (- n 2)) (> q (double (:position (nth stops (inc i)))))) (recur (inc i)) i))
                  {p0 :position c0 :color} (nth stops i)
                  {p1 :position c1 :color} (nth stops (inc i))
                  f (max 0.0 (min 1.0 (/ (- q (double p0)) (- (double p1) p0))))]
              (recur (inc j) i
                     (conj! out {:position q
                                 :color (if (= j (dec capacity)) (colour (peek stops))
                                            (mapv #(+ (* (- 1.0 f) (double %1)) (* f (double %2))) c0 c1))})))))))))

(defn- stop-inputs
  "Flat [key value ...] derived inputs for one gradient value."
  [{:keys [index capacity]} stops]
  (let [stops (fit-stops stops capacity)
        n (count stops)
        last-colour (colour (peek stops))]
    (loop [out (transient [(input-key index 0 :color) (colour (first stops))]), stop 1]
      (if (= stop capacity)
        (persistent! out)
        (recur (if (< stop n)
                 (let [from (double (:position (nth stops (dec stop))))
                       to (double (:position (nth stops stop)))]
                   (-> out
                       (conj! (input-key index stop :color)) (conj! (colour (nth stops stop)))
                       (conj! (input-key index stop :start)) (conj! from)
                       (conj! (input-key index stop :scale)) (conj! (min max-scale (/ 1.0 (- to from))))))
                 ;; Unused segments keep the last colour with a zero fraction.
                 (-> out
                     (conj! (input-key index stop :color)) (conj! last-colour)
                     (conj! (input-key index stop :start)) (conj! 0.0)
                     (conj! (input-key index stop :scale)) (conj! 0.0)))
               (inc stop))))))

(defn expander
  "Return params -> params adding the derived inputs of each sampled gradient.
  Each gradient remembers its last value and inputs, so an unchanged value
  (identical or equal) costs a lookup and a few assocs. The memo is a pure
  cache of a deterministic function: it changes no result, it is private to
  one compiled step, and a racing worker at worst recomputes the same inputs."
  [gradients]
  (if (empty? gradients)
    identity
    (let [memos (mapv (fn [gradient] [gradient (volatile! nil)]) gradients)]
      (fn [params]
        (persistent!
         (reduce (fn [out [gradient memo]]
                   (let [value (get params (:key gradient))
                         [seen inputs] @memo
                         inputs (if (and (some? seen) (or (identical? seen value) (= seen value)))
                                  inputs
                                  (let [inputs (stop-inputs gradient value)]
                                    (vreset! memo [value inputs])
                                    inputs))]
                     (loop [out out, i 0]
                       (if (< i (count inputs))
                         (recur (assoc! out (nth inputs i) (nth inputs (inc i))) (+ i 2))
                         out))))
                 (transient params) memos))))))

(defn reduced-gradients
  "Sampled gradients whose value holds more stops than the schema samples, as
  {key {:from stops :to capacity}}. `params` may omit defaulted values. Empty
  when every value fits; the definition is only walked when one does not."
  [definition params]
  (let [schemas (:params definition)
        stops (fn [key] (count (get params key (get-in schemas [key :default]))))
        over (filter #(> (stops (:key %)) (:capacity %)) (gradients schemas))]
    (if (empty? over)
      {}
      (let [sampled (sampled-keys definition)]
        (into {} (for [{:keys [key capacity]} over :when (contains? sampled key)]
                   [key {:from (stops key) :to capacity}]))))))

(defn validate-schemas
  "Refuse parameter IDs that would collide with derived stop inputs."
  [schemas]
  (when-let [key (some #(when (and (keyword? %) (= input-namespace (namespace %))) %) (keys schemas))]
    (fail! [:params key] (str "Parameter IDs in the " input-namespace " namespace are reserved"))))
