(ns tolglow.authoring.output-patch
  "Change a single output expression in a published effect. Its surrounding
  selectors, blend, defaults, version history and exact numeric values survive.")

(defn rows [definition]
  (let [outputs (:outputs definition)]
    (if (map? outputs)
      (mapv (fn [[attribute value]] {:ref attribute :attribute attribute :value value})
            (sort-by (comp str key) outputs))
      (mapv (fn [i row] (assoc row :ref i)) (range) outputs))))

(defn sources [expression]
  (into [] (distinct)
        (keep #(when (and (map? %) (= :param (:op %))) (:key %))
              (tree-seq map? :args expression))))

(defn width [schema]
  (case (:type schema)
    (:number :integer) 1
    :color 3
    :vector (:size schema)
    nil))

(defn output-width [attribute]
  (if (#{:aim :direction :color} attribute) 3 1))

(defn- fail! [message] (throw (ex-info message {})))

(defn- finite? [v] (and (number? v) #?(:clj (Double/isFinite (double v)) :cljs (js/Number.isFinite v))))

(defn apply-patch
  "Sources is one parameter of the output's width, or three scalars in XYZ/RGB
  order. Scale and offset are scalar coefficients applied componentwise.
  Clamping uses optional user-chosen numeric bounds. Evaluated by the normal IR."
  [definition {:keys [ref sources clamp] :as patch}]
  (let [row (first (filter #(= ref (:ref %)) (rows definition)))
        dims (mapv #(width (get-in definition [:params %])) sources)
        size (output-width (:attribute row))
        scale (get patch :scale 1) offset (get patch :offset 0)]
    (when-not (and definition row (map? patch)
                   (every? #{:ref :sources :scale :offset :clamp} (keys patch)))
      (fail! "Select an existing effect output."))
    (when-not (and (vector? sources) (or (= [size] dims) (and (= size 3) (= [1 1 1] dims))))
      (fail! "Choose compatible input parameters for every component."))
    (when-not (and (finite? scale) (finite? offset)
                   (or (nil? clamp) (and (vector? clamp) (= 2 (count clamp))
                                         (every? finite? clamp) (apply <= clamp))))
      (fail! "Scale, offset and clamp bounds must be finite and ordered."))
    (let [nodes (mapv (fn [id] {:op :param :key id}) sources)
          input (if (= 1 (count nodes)) (first nodes) {:op :vec3 :args nodes})
          scaled (if (= scale 1) input {:op :multiply :args [input scale]})
          shifted (if (zero? offset) scaled {:op :add :args [scaled offset]})
          expression (if clamp {:op :clamp :args [shifted (first clamp) (second clamp)]} shifted)
          path (if (map? (:outputs definition)) [:outputs ref] [:outputs ref :value])]
      (assoc-in definition path expression))))
