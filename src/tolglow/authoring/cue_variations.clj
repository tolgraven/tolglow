(ns tolglow.authoring.cue-variations
  "Seeded audition choices and an ordered bank for one cue. Callers supply the
  cue's selected effect schema and authored :params, apart from runtime controls."
  (:require [tolglow.automation.random :as random]
            [tolglow.registry :as registry]))

(def preferred-components [1 2 3 4 6 8])

(def bank-capacity 8)

(defn- exact-components [value]
  (cond (ratio? value) [(numerator value) (denominator value)]
        (integer? value) [value 1]))

(defn- beat-ratio? [schema]
  (and (= :number (:type schema))
       (or (= :beat-ratio (:semantic/type schema))
           (#{:beats :bars :phrases} (:unit schema)))))

(defn- component-bounds? [bounds]
  (and (vector? bounds) (= 2 (count bounds))
       (every? #(and (integer? %) (pos? %)) bounds) (apply <= bounds)))

(defn ratio-schema?
  "Whether a numeric parameter has bounded beat-ratio editing metadata."
  [{:keys [bounds ratio-components] :as schema}]
  (and (beat-ratio? schema) (some? bounds)
       (or (not (contains? schema :ratio-components))
           (and (map? ratio-components)
                (= #{:numerator :denominator} (set (keys ratio-components)))
                (every? component-bounds? (vals ratio-components))))))

(defn- valid-value? [schema value]
  (try (registry/valid-value? schema value)
       ;; A nonterminating fraction cannot satisfy the registry's decimal comparison.
       (catch ArithmeticException _ false)))

(defn- exact-decimal [value]
  (if (decimal? value) (rationalize value) value))

(defn valid-beat-ratio-components?
  "Check input components before reducing the fraction, including hard bounds
  and the optional :ratio-components numerator/denominator limits."
  [schema numerator denominator mode]
  (when-not (#{:preferred :free} mode)
    (throw (ex-info "Select :preferred or :free beat ratios" {:ratio-mode mode})))
  (let [legal? (fn [components]
                 (every? (fn [[component limit]]
                           (let [[low high] (get-in schema [:ratio-components limit])]
                             (and (integer? component) (pos? component)
                                  (or (nil? low) (<= low component high))
                                  (or (= :free mode) (some #{component} preferred-components)))))
                         (map vector components [:numerator :denominator])))]
    (and (ratio-schema? schema) (legal? [numerator denominator])
         (let [value (/ numerator denominator)]
           (and (legal? (exact-components value)) (valid-value? schema value))))))

(defn valid-beat-ratio?
  "Check an exact positive ratio and its reduced components against the schema."
  [schema value mode]
  (boolean (when-let [[n d] (exact-components value)]
             (valid-beat-ratio-components? schema n d mode))))

(defn- validate-ratio-params! [schemas values]
  (doseq [[param schema] schemas
          :when (or (= :beat-ratio (:semantic/type schema)) (contains? schema :ratio-components))]
    (when-not (and (valid-beat-ratio? schema (:default schema) :free)
                   (valid-beat-ratio? schema (get values param) :free))
      (throw (ex-info "Invalid exact beat ratio or component limits" {:param param}))))
  values)

(defn- resolved-params [schemas current]
  (when-not (and (map? schemas) (every? keyword? (keys schemas)) (map? current))
    (throw (ex-info "Supply authored parameter schemas and a parameter map" {})))
  (validate-ratio-params! schemas (registry/resolve-params schemas current)))

(defn- settings [seed options]
  (let [limit (:count options 8) mode (:ratio-mode options :preferred)]
    (when-not (and (integer? seed) (integer? limit) (<= 1 limit 64) (#{:preferred :free} mode))
      (throw (ex-info "Variations need an integer seed, 1–64 choices and a beat-ratio mode"
                      {:seed seed :count limit :ratio-mode mode})))
    [limit mode]))

(defn- sample-bounds [param {:keys [type bounds recommended random]}]
  (let [range (or (:range random) recommended
                  (when (and (#{:level :dimmer} param) (= :number type) bounds)
                    (let [[low high] bounds]
                      [(+ (* 0.85 low) (* 0.15 high)) (+ (* 0.15 low) (* 0.85 high))]))
                  bounds (when (= :color type) [0 1]))]
    (when (and range (not (and (vector? range) (= 2 (count range))
                               (every? registry/finite-number? range)
                               (apply <= (map exact-decimal range)))))
      (throw (ex-info "Variation range must have ordered finite bounds" {:param param :range range})))
    (when range
      (let [[low high] (map exact-decimal range)
            [hard-low hard-high] (map exact-decimal (or bounds range))]
        (if (= :color type) [(max low hard-low 0) (min high hard-high 1)]
            [(max low hard-low) (min high hard-high)])))))

(defn- local-bounds [[low high] current max-change]
  (if max-change
    (let [current (exact-decimal current) max-change (exact-decimal max-change)]
      [(max low (-' current max-change)) (min high (+' current max-change))])
    [low high]))

(defn- samples [low high steps]
  (when (<= low high)
    (map (fn [step]
           (let [weight (/ step steps)]
             (+' (*' (- 1 weight) low) (*' weight high))))
         (range (inc steps)))))

(defn- free-ratios [schema current [low high]]
  (let [denominator-bounds (get-in schema [:ratio-components :denominator])
        [numerator-low numerator-high] (get-in schema [:ratio-components :numerator])
        denominators (if denominator-bounds
                       (map bigint (apply samples (conj denominator-bounds 16)))
                       (range 1 17))
        denominators (distinct (into (vec denominators)
                                     (keep #(second (exact-components %)) [current (:default schema)])))]
    (for [denominator denominators
          :let [low (cond-> (*' (rationalize low) denominator) numerator-low (max numerator-low))
                high (cond-> (*' (rationalize high) denominator) numerator-high (min numerator-high))]
          numerator (map bigint (samples low high 16))]
      (/ numerator denominator))))

(defn- ratio-choices [schema current mode bounds]
  (when (and bounds (ratio-schema? schema))
    (filter #(valid-beat-ratio? schema % mode)
            (case mode
              :preferred (for [n preferred-components d preferred-components] (/ n d))
              :free (free-ratios schema current bounds)))))

(defn- color-choices [current bounds]
  (let [[low high] bounds
        middle (mapv #(max low (min high (exact-decimal %))) current)
        ;; Mixing toward neutral anchors changes lightness in linear RGB.
        anchors (concat [(repeat 3 low) (repeat 3 high)]
                        [(mapv #(+' low (*' % (-' high low))) [1.0 0.35 0.05])
                         (mapv #(+' low (*' % (-' high low))) [0.1 0.45 1.0])])]
    (for [anchor anchors weight [0.18 0.35 0.55 0.75]]
      (mapv #(+ (* (- 1 weight) %1) (* weight %2)) middle anchor))))

(defn- compound-choices [current bounds max-change]
  (let [grids (mapv (fn [value]
                      (let [[low high] (local-bounds bounds value max-change)]
                        (vec (samples (double low) (double high) 16))))
                    current)]
    (when (every? seq grids)
      (for [step (range 17)]
        (mapv (fn [index grid] (nth grid (mod (+ step (* 5 index)) 17))) (range) grids)))))

(defn- same-value? [type current candidate]
  (case type
    (:integer :number) (== (exact-decimal current) candidate)
    (:color :vector :range) (every? true? (map #(== (exact-decimal %1) %2) current candidate))
    (= current candidate)))

(defn- candidates [param schema current mode]
  (let [{:keys [type random values]} schema
        bounds (sample-bounds param schema)
        max-change (:max-change random)
        scalar-bounds (when (and bounds (number? current)) (local-bounds bounds current max-change))
        choices (cond
                  (beat-ratio? schema)
                  (ratio-choices schema current mode scalar-bounds)

                  (= :enum type) values
                  (= :boolean type) [false true]
                  (nil? bounds) []
                  (= :integer type) (map bigint (apply samples (conj (mapv rationalize scalar-bounds) 32)))
                  (= :number type) (apply samples (conj (mapv double scalar-bounds) 32))
                  (= :color type) (color-choices current bounds)
                  (= :range type) (map #(vec (sort %)) (compound-choices current bounds max-change))
                  :else (compound-choices current bounds max-change))]
    (into []
          (comp (remove #(same-value? type current %))
                (filter #(valid-value? schema %))
                (filter (fn [value]
                          (or (#{:enum :boolean} type)
                              (let [before (if (vector? current) current [current])
                                    after (if (vector? value) value [value])]
                                (every? true? (map (fn [previous candidate]
                                                     (let [[low high] (local-bounds bounds previous max-change)]
                                                       (<= low candidate high))) before after))))))
                (distinct))
          choices)))

(defn beat-ratio-choices
  "Return ordered exact ratios sampled within the authored bounds and mode,
  including the current ratio when it is allowed. The pool is bounded even
  when a Free-mode schema permits large numerator or denominator ranges."
  [schema current mode]
  (when (ratio-schema? schema)
    (let [choices (ratio-choices schema current mode (:bounds schema))]
      (vec (sort (distinct (cond-> choices (valid-beat-ratio? schema current mode) (conj current))))))))

(defn edit-beat-ratio
  "Project knob detents or a normalized absolute position onto ordered exact
  ratios. The ratio mode determines the candidate set independently of fine
  tuning. Return nil for parameters outside the bounded beat-ratio contract."
  [schema current input mode ratio-mode]
  (when (and (ratio-schema? schema) (registry/finite-number? current))
    (let [choices (beat-ratio-choices schema current ratio-mode)
          anchor (exact-decimal current)
          index (count (take-while #(< % anchor) choices))
          exact? (and (< index (count choices)) (== anchor (nth choices index)))
          target (if (= :absolute mode)
                   (Math/round (* (double input) (dec (count choices))))
                   (+ index (long input) (if (and (not exact?) (pos? input)) -1 0)))]
      {:value (if (seq choices) (nth choices (max 0 (min (dec (count choices)) target))) current)})))

(defn- shuffled-choices [seed param schema current mode]
  (if (false? (get-in schema [:random :enabled?])) [seed []]
      (random/shuffle-values seed (candidates param schema current mode))))

(defn parameter-variants
  "Return {:seed :variants} for one explicitly selected authored parameter.
  Each prospective choice contains only :param and :value, plus :kind :generated.
  Options are :count (default 8) and :ratio-mode (default :preferred). Free mode
  samples a bounded set of candidates while valid-beat-ratio? checks any ratio."
  [schemas current param seed options]
  (let [[limit mode] (settings seed options)
        params (resolved-params schemas current)
        schema (get schemas param)]
    (when-not schema (throw (ex-info "Unknown authored cue parameter" {:param param})))
    (let [[seed values] (shuffled-choices seed param schema (get params param) mode)]
      {:seed seed :variants (mapv #(hash-map :kind :generated :param param :value %) (take limit values))})))

(defn full-variants
  "Return distinct full authored states as {:seed :variants}. Default eligibility
  is :random {:enabled? true}. Pass :keys to select parameters explicitly.
  Explicitly disabled parameters keep their supplied values. Each variant uses
  :kind :generated. Options also accept :count and :ratio-mode as above."
  [schemas current seed options]
  (let [[limit mode] (settings seed options)
        params (resolved-params schemas current)
        selected (:keys options (filter #(true? (get-in schemas [% :random :enabled?])) (keys schemas)))]
    (when-not (every? #(contains? schemas %) selected)
      (throw (ex-info "Unknown authored cue parameter in variation selection" {:keys selected})))
    (let [[seed rows]
          (reduce (fn [[seed rows] param]
                    (let [schema (get schemas param)
                          current (get params param)
                          [seed values] (shuffled-choices seed param schema current mode)
                          base-valid? (or (not (ratio-schema? schema)) (valid-beat-ratio? schema current mode))]
                      [seed (cond-> rows (seq values) (conj [param values base-valid?]))]))
                  [seed []] (sort (distinct selected)))
          diagonal (for [slot (range limit)]
                     (reduce (fn [result [param values]]
                               (assoc result param (nth values (mod slot (count values))))) params rows))
          combinations (reduce (fn [states [param values base-valid?]]
                                 (for [state states value (cond-> values base-valid? (conj (get params param)))]
                                   (assoc state param value))) [params] rows)]
      {:seed seed :variants (into [] (comp (remove #{params}) (distinct) (take limit)
                                            (map #(hash-map :kind :generated :params %)))
                                  (concat diagonal combinations))})))

(defn- checked-bank [bank]
  (let [bank (if (nil? bank) [] bank)]
    (when-not (and (vector? bank) (<= (count bank) bank-capacity) (every? map? bank))
      (throw (ex-info "Cue variation bank must hold up to eight parameter maps" {})))
    bank))

(defn- exact-state [schemas params]
  (when-not (and (map? params) (= (set (keys schemas)) (set (keys params))))
    (throw (ex-info "Saved variation must match the cue's authored parameter keys" {})))
  (resolved-params schemas params))

(defn- check-slot! [slot]
  (when-not (and (integer? slot) (<= 0 slot) (< slot bank-capacity))
    (throw (ex-info "Saved variation slot must be 0–7" {:slot slot}))))

(defn append
  "Save a complete authored parameter map in the next slot. A full bank evicts
  the oldest appended entry and shifts the seven remaining entries left."
  [bank schemas params]
  (let [bank (checked-bank bank) params (exact-state schemas params)]
    (conj (if (= bank-capacity (count bank)) (into [] (next bank)) bank) params)))

(defn overwrite
  "Replace an occupied zero-based slot in place, retaining its append order."
  [bank schemas slot params]
  (check-slot! slot)
  (let [bank (checked-bank bank)]
    (when-not (< slot (count bank))
      (throw (ex-info "Cannot overwrite an empty saved variation slot" {:slot slot})))
    (assoc bank slot (exact-state schemas params))))

(defn recall
  "Return {:kind :saved :slot :params}, or nil for an empty zero-based slot.
  Revalidate occupied slots against the cue's current schema before returning."
  [bank schemas slot]
  (check-slot! slot)
  (when-let [params (get (checked-bank bank) slot)]
    {:kind :saved :slot slot :params (exact-state schemas params)}))
