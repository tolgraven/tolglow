(ns tolglow.automation.random
  "Explicit seeded randomness and bounded schema-aware parameter choices."
  (:require [tolglow.registry :as registry]))

(defn- seed-word ^long [seed]
  (when-not (integer? seed) (throw (ex-info "Seed must be an integer" {:seed seed})))
  (long (mod seed 4294967296)))

(defn- advance-word ^long [^long word]
  (bit-and 0xffffffff (+ (* 1664525 word) 1013904223)))

(defn- seed-value [seed word]
  ;; Preserve the numeric type when a saved show supplies an arbitrary-precision seed.
  (if (int? seed) word (bigint word)))

(defn next-random
  "Portable 32-bit LCG. Returns [next-seed unit-value], with value in [0,1).
  This is a reproducibility primitive, not cryptographic randomness."
  [seed]
  (let [word (advance-word (seed-word seed))]
    [(seed-value seed word) (/ (double word) 4294967296.0)]))

(defn choose [seed values]
  (when-not (seq values) (throw (ex-info "Cannot choose from an empty collection" {})))
  (let [[seed value] (next-random seed)] [seed (nth (vec values) (int (* value (count values))))]))

(defn derive-seed
  "Stable 32-bit substream seed from a parent seed and identity parts, using
  FNV-1a over the printed parts. A choice keyed by its own identity (a cue, a
  parameter, a section boundary) stays the same when unrelated choices are
  added or removed. Parts must print stably: keywords, strings and numbers."
  [seed & parts]
  (seed-word seed)
  (let [^bytes data (.getBytes ^String (pr-str (into [seed] parts)) "UTF-8")
        h (loop [i 0 h 2166136261]
            (if (= i (alength data)) h
                (recur (inc i) (bit-and 0xffffffff (* (bit-xor h (bit-and 0xff (aget data i))) 16777619)))))
        ;; FNV-1a alone leaves neighbouring IDs (index 1, 2, 3...) on a lattice
        ;; that the LCG's first draw preserves. The MurmurHash3 finalizer
        ;; spreads every input bit over the whole word.
        h (bit-xor h (unsigned-bit-shift-right h 16))
        h (bit-and 0xffffffff (unchecked-multiply h 0x85ebca6b))
        h (bit-xor h (unsigned-bit-shift-right h 13))
        h (bit-and 0xffffffff (unchecked-multiply h 0xc2b2ae35))]
    (bit-xor h (unsigned-bit-shift-right h 16))))

(defn choose-weighted
  "Choose one entry with probability proportional to (weight entry). Weights
  are finite and nonnegative with a positive total. Returns [next-seed entry]
  and consumes exactly one draw."
  [seed entries weight]
  (let [entries (vec entries)
        weights (mapv weight entries)]
    (when-not (and (seq entries) (every? #(and (registry/finite-number? %) (not (neg? %))) weights)
                   (pos? (reduce + weights)))
      (throw (ex-info "Weighted choice needs finite nonnegative weights with a positive total" {:weights weights})))
    (let [[seed x] (next-random seed)
          target (* x (reduce + weights))
          ;; Rounding can leave target at the total; never pick a zero weight.
          last-positive (last (keep-indexed (fn [i w] (when (pos? w) i)) weights))]
      (loop [i 0 total 0.0]
        (let [total (+ total (nth weights i))]
          (if (or (and (pos? (nth weights i)) (< target total)) (= i last-positive))
            [seed (nth entries i)]
            (recur (inc i) total)))))))

(defn shuffle-values
  "Fisher-Yates using only supplied seed. Returns [next-seed vector]."
  [seed values]
  (let [n (count values)]
    (if (< n 2) [seed (vec values)]
      ;; This private copy is never mutated after publication. Each swap
      ;; consumes exactly one LCG word in the same Fisher-Yates order.
      (let [items (object-array values)]
        (loop [word (seed-word seed) i (dec n)]
          (if (zero? i) [(seed-value seed word) (vec items)]
            (let [word (advance-word word)
                  j (int (* (/ (double word) 4294967296.0) (inc i)))
                  item (aget items i)]
              (aset items i (aget items j))
              (aset items j item)
              (recur word (dec i)))))))))

(defn- numeric-choice [seed spec current]
  (let [bounds (or (get-in spec [:random :range]) (:recommended spec) (:bounds spec)
                   (when (= :color (:type spec)) [0.0 1.0]))
        _ (when-not (and (vector? bounds) (= 2 (count bounds))
                         (every? registry/finite-number? bounds) (apply <= bounds))
            (throw (ex-info "Random numeric parameter needs a valid range" {:schema spec})))
        [hard-low hard-high] (or (:bounds spec) bounds)
        max-change (get-in spec [:random :max-change])
        low (max hard-low (first bounds) (if max-change (- current max-change) hard-low))
        high (min hard-high (second bounds) (if max-change (+ current max-change) hard-high))
        low (if (= :integer (:type spec)) (long (Math/ceil (double low))) low)
        high (if (= :integer (:type spec)) (long (Math/floor (double high))) high)]
    (when (> low high) (throw (ex-info "Randomization constraints have no valid value" {:low low :high high})))
    (let [[seed x] (next-random seed)
          distribution (get-in spec [:random :distribution] :uniform)
          [seed x] (case distribution
                     :uniform [seed x]
                     :triangular (let [[seed y] (next-random seed)] [seed (/ (+ x y) 2)])
                     (throw (ex-info "Unknown random distribution" {:distribution distribution})))
          value (if (= :integer (:type spec))
                  (+ low (long (Math/floor (* x (inc (- high low))))))
                  (+ low (* x (- high low))))]
      [seed value])))

(defn randomize-value
  "Return [seed value]. Eligibility is opt-in via :random {:enabled? true}.
  Numeric choices honor hard/recommended/custom bounds and maximum change.
  Range pairs are sorted after sampling, colors/vectors sampled componentwise."
  [seed spec current]
  (when-not (registry/valid-value? spec current)
    (throw (ex-info "Current value violates schema" {:value current :schema spec})))
  (if-not (true? (get-in spec [:random :enabled?])) [seed current]
    (let [[seed value]
          (case (:type spec)
            (:number :integer) (numeric-choice seed spec current)
            :boolean (choose seed [false true])
            :enum (choose seed (:values spec))
            (:color :vector :range)
            (let [[seed values]
                  (reduce (fn [[seed values] x]
                            (let [[seed value] (numeric-choice seed (assoc spec :type :number
                                                                        :bounds (:bounds spec (when (= :color (:type spec)) [0 1]))) x)]
                              [seed (conj values value)])) [seed []] current)]
              [seed (if (= :range (:type spec)) (vec (sort values)) values)])
            (throw (ex-info "Unsupported random parameter type" {:type (:type spec)})))]
      (when-not (registry/valid-value? spec value)
        (throw (ex-info "Randomized result violates schema" {:schema spec :value value})))
      [seed value])))

(defn randomize-params
  "Resolve defaults and randomize eligible keys in stable key order. Optional
  :keys restricts selection. Returns {:seed :params}; never mutates originals."
  ([schemas values seed] (randomize-params schemas values seed {}))
  ([schemas values seed options]
   (let [params (registry/resolve-params schemas values)
         selected (if (contains? options :keys) (set (:keys options)) (set (keys schemas)))]
     (reduce
      (fn [{:keys [seed params]} k]
        (if-not (selected k) {:seed seed :params params}
                (let [[seed value] (randomize-value seed (get schemas k) (get params k))]
                  {:seed seed :params (assoc params k value)})))
      {:seed seed :params params} (sort-by str (keys schemas))))))
