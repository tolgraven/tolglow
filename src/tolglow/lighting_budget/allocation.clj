(ns tolglow.lighting-budget.allocation
  "Allocate normalized source drives against simultaneous fixture and watt limits.")

(def scope-defaults {:limit 1.0 :standby-level 0.0 :max-active nil :strategy :proportional :fixture-order []})
(def defaults {:enabled? false :unrated-watts 1.0 :groups {} :global nil})

(defn- finite? [x] (and (number? x) (Double/isFinite (double x))))
(defn- unit? [x] (and (finite? x) (<= 0 x 1)))
(defn- id? [x] (or (keyword? x) (and (string? x) (not (.isBlank ^String x)))))

(defn- validate-scope [rule]
  (let [{:keys [limit standby-level max-active strategy fixture-order] :as normalized} (merge scope-defaults rule)]
    (when-not (and (map? rule) (every? (set (keys scope-defaults)) (keys rule))
                   (unit? limit) (unit? standby-level) (or (nil? max-active) (nat-int? max-active))
                   (#{:proportional :priority} strategy) (vector? fixture-order)
                   (every? id? fixture-order) (= (count fixture-order) (count (distinct fixture-order))))
      (throw (ex-info "Invalid lighting budget rule" {:rule rule})))
    normalized))

(defn settings [opts]
  (let [{:keys [enabled? unrated-watts groups global] :as normalized} (merge defaults opts)]
    (when-not (and (map? opts) (every? (set (keys defaults)) (keys opts)) (boolean? enabled?)
                   (finite? unrated-watts) (pos? unrated-watts) (map? groups) (every? id? (keys groups)))
      (throw (ex-info "Invalid lighting budget settings" {:settings opts})))
    (cond-> (update normalized :groups update-vals validate-scope)
      (some? global) (assoc :global (validate-scope global)))))

(defn- sum [f xs] (reduce (fn [n x] (+ n (double (f x)))) 0.0 xs))

(defn scopes
  "Each group uses its selected scene members when present, otherwise its patch members.
  A source can appear in multiple scopes. Groups trim in ID order, then the rig trims
  the result. Every earlier constraint therefore still holds."
  [config fixtures scene-groups rows]
  (let [order (into [] (comp (map :id) (filter #(contains? rows %))) fixtures)
        patch-groups (reduce (fn [result {:keys [id groups]}]
                               (if (contains? rows id)
                                 (reduce #(update %1 %2 (fnil conj []) id) result groups) result)) {} fixtures)
        members (fn [id]
                  (if-let [ids (:fixture-ids (get scene-groups id))]
                    (filterv (set ids) order)
                    (get patch-groups id [])))
        scope (fn [path rule ids]
                (let [rule (merge scope-defaults rule)
                      requested (:fixture-order rule)
                      present (set ids)
                      ranks (set requested)]
                  {:path path :rule rule
                   :ids (if (seq requested)
                          (into (filterv present requested) (remove ranks) ids) ids)
                   :capacity-watts (sum #(get-in rows [% :watts]) ids)}))]
    (cond-> (mapv (fn [id] (scope [:groups id] (get-in config [:groups id]) (members id)))
                   (sort-by (juxt str pr-str) (keys (:groups config))))
      (:global config) (conj (scope [:global] (:global config) order)))))

(defn- allocate-scope [drives rows {:keys [ids rule capacity-watts]}]
  (let [{:keys [limit standby-level max-active strategy]} rule
        active? #(> (double (drives %)) (double standby-level))
        active (filterv active? ids)
        selected (if max-active (take max-active active) active)
        watts #(get-in rows [% :watts])
        cost (fn [drive-fn id] (* (watts id) (drive-fn id)))
        available (* limit capacity-watts)
        sought (sum (partial cost drives) ids)]
    (if (and (<= sought available) (or (nil? max-active) (<= (count active) max-active)))
      drives
      (let [floor #(min standby-level (drives %))
            minimum (sum (partial cost floor) ids)
            remaining (max 0.0 (- available minimum))
            floor-scale (if (> minimum available) (/ available minimum) 1.0)
            floor-drives (reduce #(assoc %1 %2 (* floor-scale (floor %2))) drives ids)
            extra #(- (drives %) (floor %))]
        (case strategy
          :proportional
          (let [want (sum (partial cost extra) selected)
                scale (if (pos? want) (min 1.0 (/ remaining want)) 0.0)]
            (reduce #(update %1 %2 + (* scale (extra %2))) floor-drives selected))
          :priority
          (first
           (reduce (fn [[result left] id]
                     (let [w (watts id) granted (min left (* w (extra id)))]
                       [(update result id + (if (pos? w) (/ granted w) (extra id)))
                        (max 0.0 (- left granted))]))
                   [floor-drives remaining] selected)))))))

(defn allocate [scopes rows]
  (reduce #(allocate-scope %1 rows %2) (update-vals rows :emitter-drive) scopes))

(defn shortfall [rows allocated]
  (reduce-kv (fn [total id {:keys [emitter-drive watts]}]
               (+ total (* watts (max 0.0 (- emitter-drive (get allocated id 0.0)))))) 0.0 rows))

(defn report [scopes rows actual]
  (reduce
   (fn [result {:keys [path ids rule capacity-watts]}]
     (let [watts #(get-in rows [% :watts])
           drive #(get-in actual [% :emitter-drive] 0.0)
           requested #(get-in rows [% :emitter-drive])
           above? #(> (double %) (+ 1e-10 (double (:standby-level rule))))]
       (assoc-in result path
                 {:capacity-watts capacity-watts :limit-watts (* capacity-watts (:limit rule))
                  :requested-watts (sum #(* (watts %) (requested %)) ids)
                  :used-watts (sum #(* (watts %) (drive %)) ids)
                  :active-count (count (filter #(above? (drive %)) ids)) :source-count (count ids)
                  :shed-count (count (filter #(and (above? (requested %)) (not (above? (drive %)))) ids))
                  :unrated-count (count (filter #(nil? (get-in rows [% :rated-watts])) ids))}))) {} scopes))
