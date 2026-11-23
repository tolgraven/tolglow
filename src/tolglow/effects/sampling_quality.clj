(ns tolglow.effects.sampling-quality
  "Identify authored timing edges that an automatic interpolation window can erase."
  (:require [tolglow.registry :as registry]))

(defn- nodes [expression]
  (tree-seq #(seq (:args %)) :args expression))

(defn- time-dependent? [expression]
  (some #(#{:time :beat :bar :phrase :tween} (:op %)) (nodes expression)))

(defn- stepped-tween? [{:keys [op spec]}]
  (when (= op :tween)
    (let [frames (:keyframes spec)
          stepped? (fn [[a b]]
                     (let [curve (:curve a)]
                       (and (not= (:value a) (:value b))
                            (= :step (if (map? curve) (:type curve) curve)))))]
      (or (some stepped? (partition 2 1 frames))
          (and (= :loop (:mode spec))
               (not= (:value (first frames)) (:value (peek frames))))))))

(defn- timed-edge? [{:keys [op args] :as node}]
  (or (and (#{:floor :fract} op) (some time-dependent? args))
      (stepped-tween? node)))

(defn transient-output?
  "A discontinuity in a time-bearing subtree can turn a one-frame flash into
  darkness or a longer false flash. Spatial quantization on its own is inert."
  [definition]
  (boolean (some (fn [{:keys [value]}] (some timed-edge? (nodes value)))
                 (registry/output-entries (:outputs definition)))))

(defn prepare
  "Cache once per captured revision, bounded by currently active effects.
  An unchanged active/version set returns the existing cache verbatim."
  [prior epoch versions instances]
  (if (and (= epoch (:epoch prior)) (= versions (:versions prior))) prior
    (let [previous (when (= epoch (:epoch prior)) (:definitions prior))
          capture (fn [state id {:keys [effect-id effect-version definition]}]
                    (let [ref [effect-id effect-version]
                          entry (or (get-in state [:definitions ref]) (get previous ref))
                          entry (if (and entry (identical? definition (:definition entry))) entry
                                  {:definition definition :transient? (transient-output? definition)})]
                      (cond-> (assoc-in state [:definitions ref] entry)
                        (:transient? entry) (update :protected conj id)
                        (get-in definition [:keyframes :max-interval])
                        (assoc-in [:ceilings id] (:keyframes definition)))))]
      (reduce-kv capture {:epoch epoch :versions versions :protected #{} :ceilings {} :definitions {}} instances))))

(defn protect [samples ids]
  (reduce (fn [out id]
            (if (contains? out id) (assoc-in out [id :predictive?] false) out))
          samples ids))
