(ns tolglow.rig
  "Pure validation shared by rig authoring, rendering admission and show files.
  Unknown portable scene metadata is preserved; validation never fills defaults."
  (:require [clojure.string :as str]
            [afterglow.render.frame :as render]
            [afterglow.render.atmosphere :as atmosphere]
            [tolglow.rig.surfaces :as surfaces]))

(defn- error [path code message] {:path path :code code :message message})
(defn- finite? [value] (and (number? value) (Double/isFinite (double value))))
(defn- positive? [value] (and (finite? value) (pos? value)))
(defn- vec3? [value] (and (vector? value) (= 3 (count value)) (every? finite? value)))
(defn- identifier? [value] (or (keyword? value) (and (string? value) (not (str/blank? value)))))

(defn- portable-errors [value path depth]
  (cond
    (>= depth 128) [(error path :nonportable "Rig data exceeds the nesting limit")]
    (or (nil? value) (string? value) (keyword? value) (boolean? value) (char? value)) []
    (number? value) (if (finite? value) [] [(error path :nonfinite "Rig numbers must be finite")])
    (and (map? value) (not (record? value)))
    (mapcat (fn [[key child]]
              (concat (portable-errors key (conj path :map-key) (inc depth))
                      (portable-errors child (conj path key) (inc depth)))) value)
    (or (vector? value) (set? value) (list? value))
    (mapcat (fn [[index child]] (portable-errors child (conj path index) (inc depth)))
            (map-indexed vector value))
    :else [(error path :nonportable "Rig data must be portable EDN, without functions, refs, records or Java objects")]))

(defn errors
  "Return structured errors relative to the rig root. :fixtures is required;
  :room and :trusses are optional. An explicit room needs all positive finite
  dimensions. Trusses need unique stable IDs, finite positions and positive
  lengths; optional :rotation-deg contains three finite Euler angles."
  [rig]
  (if-not (and (map? rig) (not (record? rig)))
    [(error [] :invalid-rig "Rig must be a portable map")]
    (vec
     (concat
      (portable-errors rig [] 0)
      (let [fixtures (:fixtures rig)]
        (cond
          (not (vector? fixtures)) [(error [:fixtures] :invalid-fixtures "Rig fixtures must be a vector")]
          (not-every? map? fixtures)
          (for [[index fixture] (map-indexed vector fixtures) :when (not (map? fixture))]
            (error [:fixtures index] :invalid-fixture "Fixture must be a map"))
          :else
          (map #(-> % (assoc :code :invalid-patch) (update :path (fn [path] (or path [:fixtures]))))
               (render/validate-patch fixtures))))
      (when (some #(contains? (:room rig) %) [:air-changes-per-hour :fog-half-life-s :measured-fog])
        (try (atmosphere/plan (:room rig) {:fixtures []}) nil
             (catch Exception e [(error [:room] :invalid-atmosphere (.getMessage e))])))
      (when (contains? rig :room)
        (if-not (map? (:room rig))
          [(error [:room] :invalid-room "Room must be a map with width, height and depth")]
          (for [axis [:width :height :depth] :when (not (positive? (get-in rig [:room axis])))]
            (error [:room axis] :invalid-room "Room dimensions must be positive finite metres"))))
      (let [invalid (surfaces/surface-errors rig)
            room (:room rig)]
        (if (or (seq invalid) (not (vector? (:fixtures rig)))
                (and room (not (and (map? room) (every? positive? ((juxt :width :height :depth) room))))))
          invalid
          (surfaces/tracking-errors rig)))
      (when (contains? rig :trusses)
        (let [trusses (:trusses rig)]
          (if-not (vector? trusses)
            [(error [:trusses] :invalid-trusses "Trusses must be a vector")]
            (concat
             (when-not (= (count trusses) (count (distinct (map :id trusses))))
               [(error [:trusses] :duplicate-truss-id "Truss IDs must be unique")])
             (mapcat
              (fn [[index truss]]
                (let [path [:trusses index]]
                  (if-not (map? truss)
                    [(error path :invalid-truss "Truss must be a map")]
                    (concat
                     (when-not (identifier? (:id truss))
                       [(error (conj path :id) :invalid-truss-id "Truss needs a stable string or keyword ID")])
                     (when-not (vec3? (:position truss))
                       [(error (conj path :position) :invalid-position "Truss position must contain three finite metre coordinates")])
                     (when-not (positive? (:length truss))
                       [(error (conj path :length) :invalid-length "Truss length must be positive finite metres")])
                     (when (and (contains? truss :rotation-deg) (not (vec3? (:rotation-deg truss))))
                       [(error (conj path :rotation-deg) :invalid-rotation "Truss rotation must contain three finite degree angles")])))))
              (map-indexed vector trusses))))))))))

(defn validate-rig
  "Return the exact input rig when valid; otherwise throw ex-info with :errors.
  No geometry, imported metadata or identifier types are rewritten."
  [rig]
  (let [problems (errors rig)]
    (when (seq problems) (throw (ex-info "Invalid rig" {:rig/error :invalid-rig :errors problems})))
    rig))
