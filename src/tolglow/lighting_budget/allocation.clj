(ns tolglow.lighting-budget.allocation
  "Allocate normalized source drives against simultaneous fixture and watt limits."
  (:require [tolglow.autopilot.signal :as signal]))

(def scope-defaults {:limit 1.0 :standby-level 0.0 :standby-mode :shed :max-active nil :strategy :proportional :fixture-order []})
(def defaults {:enabled? false :unrated-watts 1.0 :groups {} :global nil})

(defn- finite? [x] (and (number? x) (Double/isFinite (double x))))
(defn- unit? [x] (and (finite? x) (<= 0 x 1)))
(defn- limit? [x]
  (or (unit? x)
      (and (map? x) (:cue-id x) (unit? (:default x)) (signal/valid? x))))

(defn- id? [x] (or (keyword? x) (and (string? x) (not (.isBlank ^String x)))))

(defn- validate-scope [rule]
  (let [{:keys [limit standby-level standby-mode max-active strategy fixture-order] :as normalized} (merge scope-defaults rule)]
    (when-not (and (map? rule) (every? (set (keys scope-defaults)) (keys rule))
                   (limit? limit) (unit? standby-level) (#{:shed :all} standby-mode) (or (nil? max-active) (nat-int? max-active))
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

(defn scopes
  "Compile stable membership and priority order for each group and finally the rig."
  [config fixtures scene-groups source-indices ^doubles watts rated]
  (let [order (into [] (comp (map :id) (filter #(contains? source-indices %))) fixtures)
        patch-groups (reduce (fn [result {:keys [id groups]}]
                               (if (contains? source-indices id)
                                 (reduce #(update %1 %2 (fnil conj []) id) result groups) result)) {} fixtures)
        members (fn [id]
                  (if-let [ids (:fixture-ids (get scene-groups id))]
                    (filterv (set ids) order) (get patch-groups id [])))
        scope (fn [path rule ids]
                (let [rule (merge scope-defaults rule)
                      priorities (:fixture-order rule)
                      ids (if (seq priorities)
                            (into (filterv (set ids) priorities) (remove (set priorities)) ids) ids)
                      indices (int-array (map source-indices ids))]
                  {:path path :rule rule :ids ids :indices indices
                   :capacity-watts (reduce (fn [sum i] (+ sum (aget watts (int i)))) 0.0 indices)
                   :unrated-count (count (remove rated ids))}))]
    (cond-> (mapv (fn [id] (scope [:groups id] (get-in config [:groups id]) (members id)))
                   (sort-by (juxt str pr-str) (keys (:groups config))))
      (:global config) (conj (scope [:global] (:global config) order)))))

(defn- allocate-scope!
  "Only writes the privately owned granted array. Uses the same pass order
  for priority shedding and floor reservation regardless of frame format."
  [^doubles granted ^doubles watts ^doubles light {:keys [indices rule capacity-watts]}]
  (let [^ints indices indices
        n (alength indices)
        floor (double (:standby-level rule))
        max-active (long (or (:max-active rule) n))
        cap (* (double (:limit rule)) (double capacity-watts))
        priority? (= :priority (:strategy rule))]
    (loop [k 0 requested 0.0 base 0.0 eligible-extra 0.0 active 0]
      (if (< k n)
        (let [i (aget indices k) drive (aget granted i) w (aget watts i)
              extra (max 0.0 (- drive floor))
              visible? (or (nil? light) (pos? (aget light i)))]
          (recur (inc k) (+ requested (* w drive)) (+ base (* w (min floor drive)))
                 (+ eligible-extra (if (or (not visible?) (< active max-active)) (* w extra) 0.0))
                 (if (and visible? (pos? extra)) (inc active) active)))
        (when (or (> requested cap) (> active max-active))
          (let [floor-scale (if (> base cap) (/ cap base) 1.0)
                headroom (max 0.0 (- cap base))
                share (if (pos? eligible-extra) (min 1.0 (/ headroom eligible-extra)) 0.0)]
            (loop [j 0 slots max-active left headroom]
              (when (< j n)
                (let [i (aget indices j) drive (aget granted i) w (aget watts i)
                      extra (max 0.0 (- drive floor))
                      visible? (or (nil? light) (pos? (aget light i)))
                      eligible? (and (pos? extra) (or (not visible?) (pos? slots)))
                      above (cond (not eligible?) 0.0
                                  (not priority?) (* share extra)
                                  (zero? w) extra
                                  :else (min extra (/ left w)))]
                  (aset granted i (+ (* floor-scale (min drive floor)) above))
                  (recur (inc j) (if (and eligible? visible?) (dec slots) slots) (max 0.0 (- left (* w above)))))))))))))

(defn allocate [scopes watts light ^doubles requested]
  (let [granted (aclone requested)]
    (doseq [scope scopes] (allocate-scope! granted watts light scope))
    granted))

(defn shortfall ^double [^doubles watts ^doubles requested ^doubles granted]
  (loop [i 0 cost 0.0]
    (if (= i (alength watts)) cost
      (recur (inc i) (+ cost (* (aget watts i) (max 0.0 (- (aget requested i) (aget granted i)))))))))

(defn report [scopes ^doubles watts ^doubles requested ^doubles actual ^doubles before-light ^doubles after-light]
  (reduce
   (fn [result {:keys [path indices rule capacity-watts unrated-count]}]
     (let [^ints indices indices n (alength indices) threshold (+ 1e-10 (double (:standby-level rule)))]
       (loop [k 0 before 0.0 after 0.0 active 0 shed 0]
         (if (= k n)
           (assoc-in result path {:capacity-watts capacity-watts :limit-watts (* capacity-watts (:limit rule))
                                  :requested-watts before :used-watts after :active-count active
                                  :source-count n :shed-count shed :unrated-count unrated-count})
           (let [i (aget indices k) w (aget watts i) req (aget requested i) out (aget actual i)
                 lit? (and (> out threshold) (or (nil? after-light) (pos? (aget after-light i))))
                 wanted? (and (> req threshold) (or (nil? before-light) (pos? (aget before-light i))))]
             (recur (inc k) (+ before (* w req)) (+ after (* w out))
                    (if lit? (inc active) active)
                    (if (and wanted? (not lit?)) (inc shed) shed))))))) {} scopes))
