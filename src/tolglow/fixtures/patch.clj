(ns tolglow.fixtures.patch
  "Expand Tolglow's ordered patch groups into physical fixture instances.
  Show files and the Afterglow renderer consume the expanded rig."
  (:require [tolglow.fixtures.heads :as heads]
            [tolglow.rig :as rig]))

(defn- fail [message data] (throw (ex-info message data)))

(defn- factory? [x] (or (fn? x) (and (var? x) (fn? @x))))

(defn- definition [registry factories spec]
  (let [[source & args] (if (vector? spec) spec [spec])
        value (or (get factories source) (get registry source) source)
        result (if (factory? value) (apply value args) value)]
    (when-not (and (map? result) (or (not (seq args)) (factory? value)))
      (fail "Unknown fixture definition or builder" {:fixture source}))
    result))

(defn- degrees [angle]
  (cond (number? angle) (Math/toDegrees (double angle))
        (and (seq? angle) (= 'afterglow.transform/degrees (first angle))
             (= 2 (count angle)) (number? (second angle))) (second angle)
        :else (fail "Expected a numeric angle or (afterglow.transform/degrees n)" {:angle angle})))

(defn- placement [entry]
  (if-not (vector? entry) [entry {}]
    (let [[source & positioning] entry
          [xyz options] (split-with #(or (nil? %) (number? %)) positioning)
          opts (cond (empty? options) {}
                     (and (= 1 (count options)) (map? (first options))) (first options)
                     (and (even? (count options)) (every? keyword? (take-nth 2 options))) (apply hash-map options)
                     :else (fail "Fixture placement needs an options map or keyword pairs" {:entry entry}))
          radians (select-keys opts [:x-rotation :y-rotation :z-rotation])]
      (when (or (> (count xyz) 3) (and (seq xyz) (:position opts))
                (and (seq radians) (:rotation-deg opts)))
        (fail "Fixture placement has conflicting coordinates or angles" {:entry entry}))
      [source (cond-> (apply dissoc opts (keys radians))
                (seq xyz) (assoc :position (mapv #(or % 0) (take 3 (concat xyz (repeat 0)))))
                (seq radians) (assoc :rotation-deg
                                     (mapv #(degrees (get radians % 0))
                                           [:x-rotation :y-rotation :z-rotation])))])))

(defn- expand-group [registry factories [group {:keys [universe start-universe offset list type] :or {offset 1}}]]
  (when-not (and (keyword? group) (vector? list)
                 (not= (some? universe) (some? start-universe))
                 (pos-int? (or universe start-universe)) (integer? offset) (<= 1 offset 512))
    (fail "Patch group needs a name, :list, :universe or :start-universe, and a valid :offset" {:group group}))
  (loop [items (seq list) index 1 slot (+ (* 512 (dec (or universe start-universe))) (dec offset)) result []]
    (if-not items result
      (let [[spec opts] (placement (first items))
            id (get opts :id (keyword (namespace group) (str (name group) "-" index)))
            template (definition registry factories spec)
            template (merge {:id (if (keyword? spec) spec id) :version 1} template)
            opts (assoc opts :id id :universe (inc (quot slot 512)) :address (inc (mod slot 512)))
            {:keys [fixtures footprint]} (heads/targets template opts slot)
            groups (vec (distinct (remove nil? [group type])))]
        (let [next-slot (+ slot footprint)
              ;; :start-universe advances each fixture to another universe at
              ;; the same offset. Large fixtures may occupy several universes.
              next-slot (if start-universe
                          (+ slot (* 512 (inc (quot (dec footprint) 512))))
                          next-slot)]
          (recur (next items) (inc index) next-slot
                 (into result (map #(assoc % :groups groups)) fixtures)))))))

(defn expand
  "Pass a full fixture vector or a map of groups with :list, :offset, and
  :universe or :start-universe. A list may contain a keyword, builder function,
  or [fixture x y z & options]. Resolve names from the explicit registry and
  trusted builder map. Validation includes all groups before returning."
  ([registry patches] (expand registry {} patches))
  ([registry factories patches] (expand registry factories patches []))
  ([registry factories patches order]
   (let [fixtures (cond (vector? patches) patches
                        (map? patches) (into [] (mapcat (partial expand-group registry factories))
                                             (map (fn [k] [k (get patches k)])
                                                  (distinct (concat (filter #(contains? patches %) order) (keys patches)))))
                        :else (fail "Expected a full fixture vector or named patch groups" {}))]
     (:fixtures (rig/validate-rig {:fixtures fixtures})))))
