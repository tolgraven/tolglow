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
    (let [last @(:last lease) version (some-> (:version lease) deref)]
      (if (and (identical? source (:source last)) (= version (:version last))) (:value last)
        (let [value (try {:value (sample! source)}
                         (catch Exception _ {:error :unavailable}))]
          (vreset! (:last lease) {:source source :version version :value value})
          value)))
    @(:result lease)))

(defn validate [queries]
  (when-not (and (vector? queries) (<= (count queries) 32)
                 (every? #(and (vector? %) (<= 1 (count %) 16) (keyword? (first %))) queries))
    (throw (ex-info "Expected up to 32 named subscription queries" {})))
  (set queries))

(defn- close-lease! [lease]
  (when-let [close! (:close! lease)] (close!)))

(defn close! [store]
  (locking store
    (let [[before _] (reset-vals! (:state store) {:closed? true})]
      (doseq [lease (vals (:leases before))] (close-lease! lease)))))

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
    (let [{:keys [version close!] :as lease} (open! query)
          key (Object.)]
      (when version (add-watch version key (fn [_ _ _ _] ((:wake! store)))))
      (assoc lease :last (volatile! nil)
             :close! #(do (when version (remove-watch version key)) (when close! (close!)))))
    (open-graph-lease system query (contains? public (first query)) (:wake! store))))

(defn reconcile!
  "Share leases across sockets. Changing show identity, graph or public-query
  policy invalidates previous acquisitions, including unavailable queries."
  [store source wanted]
  (locking store
    (let [state (:state store) before @state]
      (when-not (or (:closed? before) (and (empty? wanted) (empty? (:leases before))))
        (let [model (:model source)
              configured (get-in model [:settings :studio :public-queries])
              public (if (registry/valid-public-query-ids? configured) (into defaults configured) defaults)
              identity (select-keys model [:show-id :epoch])
              system (when (seq wanted)
                       ;; A runtime can close its graph during a concurrent shutdown.
                       ;; Return unavailable until the next committed graph is present.
                       (try ((:get-graph store)) (catch Exception _ nil)))
              compatible? (and (identical? system (:system before)) (= identity (:identity before))
                               (= public (:public before)))
              prior (:leases before)
              newly-registered (when (and compatible? system)
                                 (into #{} (filter #(reactive/registered-sub? system (first %))) (:missing before)))
              keep? #(and compatible? (contains? wanted %) (not (contains? newly-registered %)))]
          (if (and compatible? (empty? newly-registered) (= wanted (:wanted before)))
            prior
            (do
              (doseq [[query lease] prior :when (not (keep? query))] (close-lease! lease))
              (let [leases (into {} (map (fn [q] [q (or (when (keep? q) (get prior q))
                                                       (open-lease store system q public))])
                                         wanted))]
                (reset! state {:system system :identity identity :leases leases :wanted wanted :public public
                               :missing (into #{} (keep (fn [[q lease]] (when (:await-registration? lease) q))) leases)})
                leases))))))))
