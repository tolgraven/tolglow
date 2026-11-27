(ns tolglow.visualization.waveform-shapes
  "Waveform shape choices drawn as glyphs. The server samples each glyph from
  the registered waveform the engine runs (`tolglow.signals/glyph`) and ships
  it in the waveform catalog. This namespace only matches a schema's choices to
  that catalog and lays a glyph out, so Studio and the Push display draw the
  same shapes from the same data."
  (:require [clojure.string :as str]))

(def aliases
  "Legacy oscillator names for registered waveforms."
  {:square :pulse :sawtooth :saw})

(def max-choices 16)

(defn- tag [id]
  (cond (keyword? id) id
        (and (string? id) (not (str/blank? id))) (keyword id)
        :else nil))

(defn- finite? [x]
  #?(:clj (and (number? x) (Double/isFinite (double x)))
     :cljs (and (number? x) (js/isFinite x))))

(defn- magnitude [x] #?(:clj (Math/abs (double x)) :cljs (js/Math.abs x)))

(defn- glyph? [glyph]
  (and (sequential? glyph) (<= 2 (count glyph)) (every? finite? glyph)))

(def ^:private last-index
  "The index of the last catalog seen. Every parameter row, display slot and
  Live device slot asks about the same catalog value, so it is built once per
  catalog change instead of once per question."
  (atom nil))

(defn catalog-index
  "Catalog entries `{:id :name :glyph}` with a drawable glyph, by waveform ID."
  [catalog]
  (let [[seen index] @last-index]
    (if (and (some? seen) (identical? seen catalog))
      index
      (let [index (into {} (keep (fn [entry] (when-let [id (tag (:id entry))] (when (glyph? (:glyph entry)) [id entry]))))
                        catalog)]
        (reset! last-index [catalog index])
        index))))

(defn waveform-id
  "The catalog waveform a choice value names, directly or by a legacy alias."
  [index value]
  (when-let [id (tag value)]
    (cond (contains? index id) id
          (contains? index (aliases id)) (aliases id))))

(defn- schema-choices [schema]
  (cond (seq (:choices schema)) (mapv #(if (map? %) % {:value %}) (:choices schema))
        (seq (:values schema)) (mapv #(hash-map :value %) (:values schema))
        (seq (:options schema)) (mapv #(hash-map :value %) (:options schema))))

(defn choices
  "`[{:value :label :waveform :glyph}]` when every choice of a choice or enum
  schema names a waveform of `catalog`, by a choice's `:id` or by its value.
  Otherwise nil, so a geometry `:shape` or any other enum stays text. Values
  are the schema's own, ready to send back unchanged."
  [schema catalog]
  (let [index (catalog-index catalog)
        items (schema-choices schema)]
    (when (and (seq index) (<= 1 (count items) max-choices))
      (let [resolved (mapv (fn [item]
                             (when-let [id (waveform-id index (or (:id item) (:value item)))]
                               (let [entry (get index id)]
                                 {:value (:value item)
                                  :label (or (:label item) (:name entry) (name id))
                                  :waveform id
                                  :glyph (vec (:glyph entry))})))
                           items)]
        (when (every? some? resolved) resolved)))))

(defn- same-choice? [a b]
  (if (and (number? a) (number? b))
    (== a b)
    (and (some? a) (= (or (tag a) a) (or (tag b) b)))))

(defn selected-index
  "The index of the choice holding `value`. A number between numeric choices
  takes the nearest, ties going to the later one, like a registered selector."
  [shapes value]
  (or (first (keep-indexed (fn [index shape] (when (same-choice? value (:value shape)) index)) shapes))
      (when (and (finite? value) (every? (comp finite? :value) shapes) (seq shapes))
        (first (reduce (fn [[best distance] [index shape]]
                         (let [d (magnitude (- value (:value shape)))]
                           (if (<= d distance) [index d] [best distance])))
                       [0 ##Inf] (map-indexed vector shapes))))))

(defn- clamp [x] (max 0.0 (min 1.0 x)))

(defn- coordinate [x]
  (let [rounded (/ (Math/round (* 100.0 (double x))) 100.0)]
    (if (== rounded (Math/floor rounded)) (str (long rounded)) (str rounded))))

(defn points
  "SVG polyline points that fit a glyph in a `width` × `height` box with
  `pad` on every side. Values are the waveform's share of its range, 0…1."
  [glyph width height pad]
  (when (glyph? glyph)
    (let [n (dec (count glyph))
          w (- width (* 2 pad)) h (- height (* 2 pad))]
      (str/join " " (map-indexed (fn [i v]
                                   (str (coordinate (+ pad (* w (/ i n)))) ","
                                        (coordinate (+ pad (* h (- 1.0 (clamp v)))))))
                                 glyph)))))
