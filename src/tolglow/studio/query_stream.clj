(ns tolglow.studio.query-stream
  "Reference-counted websocket demand for the show's registered reactive signals.
  Only the delivery worker installs leases; graph callbacks publish immutable
  values and wake it. A disconnected socket never owns the underlying graph."
  (:require [afterglow.reactive :as reactive]
            [tolglow.registry :as registry]))

(def defaults
  #{:clock/beat :clock/beat-in-bar :clock/bar :clock/phrase :clock/phase
    :runtime/keyframe :runtime/block-trace :runtime/block-library :spatial/scene :spatial/body :spatial/members :rdm/status})

(defn create
  ([get-graph wake!] (create get-graph wake! {}))
  ([get-graph wake! surfaces]
   {:get-graph get-graph :wake! wake! :state (atom {}) :surfaces surfaces}))

(defn read-result [lease source]
  (if-let [sample! (:sample! lease)]
    (let [source (if-let [current (:source! lease)] (current) source)
          last @(:last lease)]
      (if (and last (identical? source (:source last))) (:value last)
        (let [value (try {:value (sample! source)}
                         (catch Exception _ {:error :unavailable}))]
          (vreset! (:last lease) {:source source :value value})
          value)))
    @(:result lease)))

(defn validate [queries]
  (when-not (and (vector? queries) (<= (count queries) 32)
                 (every? #(and (vector? %) (<= 1 (count %) 16) (keyword? (first %))) queries))
    (throw (ex-info "Expected up to 32 named subscription queries" {})))
  (set queries))

(defn- close-lease! [lease]
  (when-let [detach! (:detach! lease)] (detach!))
  (when-let [close! (:close! lease)] (close!)))

(defn close! [store]
  (let [[before _] (locking store (reset-vals! (:state store) {:closed? true}))]
    (doseq [lease (vals (:leases before))] (close-lease! lease))))

(defn- surface-query? [store query]
  (contains? (:surfaces store) (first query)))

(defn retiring-queries [store]
  (:retiring @(:state store)))

(defn release-unused-graph-leases!
  "Retire unobserved Afterglow queries independently of projection delivery."
  [store wanted-queries candidates]
  (let [state (:state store)
        retired (locking store
                  (let [before @state wanted (wanted-queries)
                        unused (filterv #(and (contains? (:leases before) %)
                                              (not (contains? wanted %))
                                              (not (surface-query? store %)))
                                        candidates)]
                    (when (seq unused)
                      (reset! state (-> before
                                        (update :leases #(apply dissoc % unused))
                                        (update :wanted #(apply disj % unused))
                                        (update :missing #(apply disj % unused))
                                        (update :retiring (fnil into #{}) unused)))
                      (select-keys (:leases before) unused))))
        failure (reduce (fn [first-error [query lease]]
                          (try
                            (close-lease! lease)
                            first-error
                            (catch Exception error (or first-error error))
                            (finally
                              (locking store
                                (when-not (:closed? @state)
                                  (swap! state update :retiring disj query)))
                              ((:wake! store)))))
                        nil retired)]
    (when failure (throw failure))))

(defn- open-graph-lease [system query allowed? wake!]
  (if-not allowed?
    {:result (atom {:error :not-exposed})}
    (let [absent (Object.) result (atom absent)]
      (try
        (let [{:keys [value close! available?]} (reactive/acquire! system query
                                       (fn [value] (reset! result {:value value}) (wake!)))]
          ;; A newer publish may already have notified us after acquire returned.
          (compare-and-set! result absent (if available? {:value value} {:error :unavailable}))
          {:result result :close! close!})
        (catch Exception e
          {:await-registration? (= :afterglow.reactive/unknown-subscription (:type (ex-data e)))
           :result (atom {:error :unavailable :message (or (.getMessage e) "Subscription unavailable")})})))))

(defn- open-lease [store system query public]
  (if-let [open! (get (:surfaces store) (first query))]
    (let [{:keys [observe!] :as lease} (open! query)]
      (cond-> (assoc lease :last (volatile! nil))
        observe! (assoc :detach! (observe! (:wake! store)))))
    (open-graph-lease system query (contains? public (first query)) (:wake! store))))

(defn reconcile-current!
  "Share leases across sockets. Changing show identity, graph or public-query
  policy invalidates previous acquisitions, including unavailable queries."
  [store source wanted-queries]
  (let [[leases retired]
        (locking store
          (let [state (:state store) before @state
                ;; Opening while its old watcher is still alive would reuse
                ;; the retired query's cached value and in-flight producer.
                wanted (reduce disj (wanted-queries) (:retiring before))]
            (when-not (or (:closed? before) (and (empty? wanted) (empty? (:leases before))))
              (let [model (:model source)
                    configured (get-in model [:settings :studio :public-queries])
                    public (if (registry/valid-public-query-ids? configured) (into defaults configured) defaults)
                    identity (select-keys model [:show-id :epoch])
                    system (when (seq wanted)
                             ;; The runtime can close its graph during a concurrent shutdown.
                             (try ((:get-graph store)) (catch Exception _ nil)))
                    compatible? (and (identical? system (:system before)) (= identity (:identity before))
                                     (= public (:public before)))
                    prior (:leases before)
                    newly-registered (when (and compatible? system)
                                       (into #{} (filter #(reactive/registered-sub? system (first %))) (:missing before)))
                    keep? #(and compatible? (contains? wanted %) (not (contains? newly-registered %)))]
                (if (and compatible? (empty? newly-registered) (= wanted (:wanted before)))
                  [prior nil]
                  (let [{surfaces true graph false}
                        (group-by #(surface-query? store (key %)) (remove #(keep? (key %)) prior))]
                    (doseq [[_ lease] surfaces] (close-lease! lease))
                    (let [leases (into {} (map (fn [q] [q (or (when (keep? q) (get prior q))
                                                             (open-lease store system q public))])
                                               wanted))]
                      (reset! state {:system system :identity identity :leases leases :wanted wanted :public public
                                     :retiring (:retiring before)
                                     :missing (into #{} (keep (fn [[q lease]] (when (:await-registration? lease) q))) leases)})
                      [leases graph])))))))]
    (doseq [[_ lease] retired] (close-lease! lease))
    leases))

(defn reconcile! [store source wanted]
  (reconcile-current! store source (constantly wanted)))
