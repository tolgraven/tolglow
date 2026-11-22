(ns tolglow.studio.query-stream
  "Reference-counted websocket demand for the show's registered reactive signals.
  Only the delivery worker installs leases; graph callbacks publish immutable
  values and wake it. A disconnected socket never owns the underlying graph."
  (:require [afterglow.reactive :as reactive]))

(def defaults
  #{:clock/beat :clock/beat-in-bar :clock/bar :clock/phrase :clock/phase
    :spatial/body :spatial/members})

(defn create [get-graph wake!]
  {:get-graph get-graph :wake! wake! :state (atom {})})

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

(defn- open-lease [system query allowed? wake!]
  (if-not allowed?
    {:result (atom {:error :not-exposed})}
    (let [absent (Object.) result (atom absent)]
      (try
        (let [{:keys [value close!]} (reactive/acquire! system query
                                       (fn [value] (reset! result {:value value}) (wake!)))]
          ;; A newer publish may already have notified us after acquire returned.
          (compare-and-set! result absent {:value value})
          {:result result :close! close!})
        (catch Exception e
          {:result (atom {:error :unavailable :message (or (.getMessage e) "Subscription unavailable")})})))))

(defn reconcile!
  "Share leases across sockets. Changing show identity, graph or public-query
  policy invalidates previous acquisitions, including unavailable queries."
  [store source wanted]
  (locking store
    (let [state (:state store) before @state]
      (when-not (or (:closed? before) (and (empty? wanted) (empty? (:leases before))))
        (let [model (:model source)
              public (into defaults (get-in model [:settings :studio :public-queries]))
              identity (select-keys model [:show-id :epoch])
              system (when (seq wanted)
                       ;; A runtime can close its graph during a concurrent shutdown.
                       ;; Return unavailable until the next committed graph is present.
                       (try ((:get-graph store)) (catch Exception _ nil)))
              compatible? (and (identical? system (:system before)) (= identity (:identity before))
                               (= public (:public before)))
              prior (:leases before)
              keep? #(and compatible? (contains? wanted %))]
          (if (and compatible? (= wanted (:wanted before)))
            prior
            (do
              (doseq [[query lease] prior :when (not (keep? query))] (close-lease! lease))
              (let [leases (into {} (map (fn [q] [q (or (when compatible? (get prior q))
                                                       (open-lease system q (contains? public (first q)) (:wake! store)))])
                                         wanted))]
                (reset! state {:system system :identity identity :leases leases :wanted wanted :public public})
                leases))))))))
