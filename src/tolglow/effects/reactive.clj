(ns tolglow.effects.reactive
  "Show-owned leases and immutable inputs for authored dependency-driven effects."
  (:require [afterglow.reactive :as graph]
            [afterglow.runtime.reuse :as reuse]))

(defn project-inputs [bindings context]
  (if-some [values (get-in context [:frame ::values])]
    (assoc-in context [:frame :reactive-inputs]
              (into {} (keep (fn [[alias binding]]
                               (when-let [value (find values binding)] [alias (val value)])))
                    bindings))
    context))

(defn input-sample [{:keys [source subscribe]} [_ [kind id]]]
  (try
    (let [value (case kind
                  :source @(source id)
                  :subscription @(subscribe id))]
      (when-not (reuse/immutable-inputs? [value])
        (throw (ex-info "Reactive effect inputs must be bounded immutable values" {:binding [kind id]})))
      [value])
    (catch clojure.lang.ExceptionInfo error
      (when-not (#{::graph/unknown-source ::graph/unknown-subscription} (:type (ex-data error)))
        (throw error)))))

(defn register! [system]
  (graph/reg-sub! system ::input input-sample
                  {:id ::input :kind :sub
                   :implementation {:type :var :symbol "tolglow.effects.reactive/input-sample"}}
                  {:on-error :fallback})
  system)

(defn export-registry [system]
  (update (graph/export-registry system) :registrations
          (fn [definitions]
            (into [] (remove #(and (= :sub (:kind %)) (= ::input (:id %)))) definitions))))

(defn create [] (volatile! {:leases {}}))

(defn- bindings-for [model]
  (into #{} (mapcat (comp vals :inputs :reactive :definition)) (vals (:instances model))))

(defn- close-lease! [owner binding]
  (when-let [close! (get-in @owner [:leases binding])]
    (vswap! owner update :leases dissoc binding)
    (close!)))

(defn publish!
  "Retain the input queries demanded by committed instances, then capture their
  values from the same graph publication used by semantic actions and tracing."
  [owner system model publish]
  (locking owner
    (let [bindings (bindings-for model)
          active (get @owner :leases)]
      (vswap! owner dissoc :snapshot)
      (doseq [binding (keys active) :when (not (contains? bindings binding))]
        (close-lease! owner binding))
      (doseq [binding bindings :when (not (contains? active binding))]
        (let [lease (graph/acquire! system [::input binding] (constantly nil))]
          (vswap! owner assoc-in [:leases binding] (:close! lease))))
      ;; Capture the current representation even when graph equality suppressed
      ;; a notification. Existing user queries retain their normal cache semantics.
      (doseq [binding bindings] (graph/invalidate! system [::input binding]))
      (let [publication (publish system model)]
        (when (seq bindings)
          (let [values (into {} (keep (fn [binding]
                                        (when-some [slot (get-in publication [:values [:subscription [::input binding]]])]
                                          [binding (first slot)]))) bindings)]
            (vswap! owner assoc :snapshot {:epoch (:epoch model) ::values values})))
        publication))))

(defn frame-inputs [owner epoch]
  (let [snapshot (:snapshot @owner)]
    (when (= epoch (:epoch snapshot))
      (select-keys snapshot [::values]))))

(defn close! [owner]
  (locking owner
    (vswap! owner dissoc :snapshot)
    (run! #(close-lease! owner %) (keys (:leases @owner)))))
