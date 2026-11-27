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
  from per-frame inputs that `with-stops` derives from the resolved parameter
  value. The scalar, batch and GLSL backends therefore read it unchanged, and a
  frame expands each gradient once rather than once per fixture. The schema's
  stop capacity (`:max-stops`, default 8) fixes the number of segments, so the
  per-fixture cost grows with the capacity, not with the live value."
  (:require [afterglow.render.ir :as ir]
            [clojure.string :as str]
            [tolglow.registry :as registry]))

(def input-namespace
  "Namespace of the derived per-frame stop inputs. Authored parameter IDs may not use it."
  "tolglow.gradient")

(def extends #{:clamp :repeat :mirror})

(defn input-key
  "The derived parameter that carries one field of stop `index` of gradient `key`."
  [key index field]
  (keyword input-namespace (str (str/replace (subs (str key) 1) "/" "__") "@" index "." (name field))))

(defn gradient-node? [expression]
  (and (map? expression) (= :gradient (:op expression))))

(defn- fail! [path message]
  (throw (ex-info message {:errors [{:path path :message message}]})))

(defn- const [value] {:op :const :value (double value)})

(defn- input [key index field type] {:op :param :key (input-key key index field) :type type})

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
      (let [t (extended extend position)
            colour (reduce (fn [colour index]
                             (op :mix colour (input key index :color :vec3)
                                 (op :clamp (op :multiply (op :subtract t (input key index :start :number))
                                                (input key index :scale :number))
                                     (const 0.0) (const 1.0))))
                           (input key 0 :color :vec3)
                           (range 1 (registry/gradient-capacity schema)))]
        ;; Stops are within [0,1]; the clamp keeps rounding inside the unit range.
        (op :clamp colour (const 0.0) (const 1.0))))))

(defn gradients
  "Each gradient parameter of a schema map with its stop capacity, in a stable order."
  [schemas]
  (into [] (comp (filter #(= :gradient (:type (val %))))
                 (map (fn [[key schema]] [key (registry/gradient-capacity schema)])))
        (sort-by (comp str key) schemas)))

(defn- colour [stop] (mapv double (:color stop)))

(defn- removal-error
  "How far stop `i` sits from the ramp its neighbours would draw without it."
  [stops i]
  (let [{p0 :position c0 :color} (nth stops (dec i))
        {p :position c :color} (nth stops i)
        {p1 :position c1 :color} (nth stops (inc i))
        f (/ (- (double p) p0) (- (double p1) p0))]
    (reduce max (map (fn [a x b] (Math/abs (- (double x) (+ (* (- 1.0 f) a) (* f b))))) c0 c c1))))

(defn simplify
  "Reduce ordered stops to at most `capacity`, dropping the interior stop that
  changes the ramp least (the earliest on a tie) until it fits. The end stops
  remain unless the capacity is a single solid colour, which keeps the first."
  [stops capacity]
  (loop [stops (vec stops)]
    (cond
      (<= (count stops) capacity) stops
      (<= (count stops) 2) (subvec stops 0 capacity)
      :else (let [i (apply min-key #(removal-error stops %) (reverse (range 1 (dec (count stops)))))]
              (recur (into (subvec stops 0 i) (subvec stops (inc i))))))))

(defn- add-stops! [inputs [key capacity] stops]
  (let [stops (simplify stops capacity)
        n (count stops)
        segments (dec capacity)
        last-colour (colour (peek stops))]
    (loop [inputs (assoc! inputs (input-key key 0 :color) (colour (first stops))), index 1]
      (if (> index segments)
        inputs
        (recur (if (< index n)
                 (let [from (double (:position (nth stops (dec index))))
                       to (double (:position (nth stops index)))]
                   (-> inputs
                       (assoc! (input-key key index :color) (colour (nth stops index)))
                       (assoc! (input-key key index :start) from)
                       (assoc! (input-key key index :scale) (/ 1.0 (- to from)))))
                 ;; Unused segments keep the last colour with a zero fraction.
                 (-> inputs
                     (assoc! (input-key key index :color) last-colour)
                     (assoc! (input-key key index :start) 0.0)
                     (assoc! (input-key key index :scale) 0.0)))
               (inc index))))))

(defn with-stops
  "Add the derived stop inputs of each `[key capacity]` gradient to resolved
  `params`. Call once per frame after parameters resolve; values are doubles.
  A value with more stops than its capacity is sampled through `simplify`."
  [gradients params]
  (if (empty? gradients)
    params
    (persistent! (reduce (fn [inputs gradient] (add-stops! inputs gradient (get params (first gradient))))
                         (transient params) gradients))))

(defn validate-schemas
  "Refuse parameter IDs that would collide with derived stop inputs."
  [schemas]
  (when-let [key (some #(when (and (keyword? %) (= input-namespace (namespace %))) %) (keys schemas))]
    (fail! [:params key] (str "Parameter IDs in the " input-namespace " namespace are reserved"))))
