(ns tolglow.lighting-levels
  "Publish measured light into show variables and the ordinary committed event
  stream. Percentages describe emitter output after rendering and master."
  (:require [afterglow.runtime.events :as events]))

(def group-event-types
  [:lighting/group-seeking :lighting/group-at-target :lighting/group-limited :lighting/group-released])

(def rig-event-types [:lighting/rig-level :lighting/rig-unavailable])

(defn- group-event [status]
  (case status
    :at-target :lighting/group-at-target
    :seeking :lighting/group-seeking
    (:over-target :limited :unlit) :lighting/group-limited))

(defn- publish-group [previous m id {:keys [actual target status]}]
  (let [old (get previous id)]
    (if (and (= actual (:actual old)) (= target (:target old)) (= status (:status old)))
      m
      (let [level {:actual actual :target target :status status}]
        (cond-> (assoc-in m [:variables :lighting/groups id] level)
          (not= status (:status old))
          (events/collect :lighting {:kind :fixture-group :id id}
            [(assoc level :type (group-event status) :previous-status (:status old) :group-id id)]))))))

(defn- retire-group [m id old]
  (-> m
      (update-in [:variables :lighting/groups] dissoc id)
      (events/collect :lighting {:kind :fixture-group :id id}
        [(assoc old :type :lighting/group-released :group-id id :previous-status (:status old) :status :released)])))

(defn- rig-status [{:keys [mode target utilization gain]}]
  (cond
    (not= mode :following) mode
    (< (abs (- utilization target)) 0.01) :at-target
    (and (< utilization target) (== (double gain) 1.0)) :limited
    :else :seeking))

(defn- percent [n] (when (number? n) (Math/round (* 100.0 (double n)))))

(defn- publish-rig [m meter]
  (let [old (get-in m [:variables :lighting/rig])]
    (cond
      meter
      (let [{:keys [utilization target mode]} meter
            status (rig-status meter)]
        (if (and (= utilization (:actual old)) (= target (:target old)) (= mode (:mode old)) (= status (:status old)))
          m
          (let [level {:actual utilization :target target :mode mode :status status}]
            (cond-> (assoc-in m [:variables :lighting/rig] level)
              (or (not= (percent utilization) (percent (:actual old))) (not= status (:status old))
                  (not= mode (:mode old)))
              (events/collect :lighting {:kind :lighting-rig :id (:show-id m)} [(assoc level :type :lighting/rig-level :previous-status (:status old))])))))
      old
      (-> m (update :variables dissoc :lighting/rig)
          (events/collect :lighting {:kind :lighting-rig :id (:show-id m)} [{:type :lighting/rig-unavailable :previous-status (:status old)}]))
      :else m)))

(defn publish
  "Thread measurements from the accepted software frame into its candidate
  model. The owner commits variables and admits the associated events together."
  [model groups meter]
  (let [vars (:variables model)
        previous (:lighting/groups vars)]
    (if (or (seq groups) (seq previous) meter (:lighting/rig vars))
      (let [updated (reduce-kv (partial publish-group previous) model groups)
            updated (reduce-kv (fn [m id old] (if (contains? groups id) m (retire-group m id old))) updated previous)]
        (publish-rig updated meter))
      model)))
