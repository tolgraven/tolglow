(ns tolglow.studio.query-stream
  "Reference-counted websocket demand for the show's registered reactive signals.
  Only the delivery worker installs leases; graph callbacks publish immutable
  values and wake it. A disconnected socket never owns the underlying graph."
  (:require [afterglow.reactive :as reactive]
            [tolglow.registry :as registry])
  (:import [java.lang System$Logger System$Logger$Level]
           [java.util.concurrent Executors ThreadFactory TimeUnit]))

(def defaults
  #{:clock/beat :clock/beat-in-bar :clock/bar :clock/phrase :clock/phase
    :runtime/keyframe :runtime/block-trace :runtime/block-library :spatial/scene :spatial/body :spatial/members :rdm/status})

(def ^:private ^System$Logger logger (System/getLogger "tolglow.studio.query-stream"))

(def ^:private close-wait-ms 1000)

(def ^:private unavailable-lease {:result (delay {:error :unavailable})})

(defn create
  ([get-graph wake!] (create get-graph wake! {}))
  ([get-graph wake! surfaces]
   {:get-graph get-graph :wake! wake! :state (atom {}) :surfaces surfaces}))

(defn read-result [lease source]
  (if-let [sample! (:sample! lease)]
    (let [slot (:last lease)]
      (locking slot
        (let [last @slot]
          (when-not (= ::retired last)
            (let [source (if-let [current (:source! lease)] (current) source)]
              (if (and last (identical? source (:source last))) (:value last)
                (let [value (try {:value (sample! source)}
                                 (catch Exception _ {:error :unavailable}))]
                  (vreset! slot {:source source :value value})
                  value)))))))
    @(:result lease)))

(defn validate [queries]
  (when-not (and (vector? queries) (<= (count queries) 32)
                 (every? #(and (vector? %) (<= 1 (count %) 16) (keyword? (first %))) queries))
    (throw (ex-info "Expected up to 32 named subscription queries" {})))
  (set queries))

(defn- close-lease! [lease]
  ;; A delivery pass may already hold this lease. Finish any active sample
  ;; and prevent that pass from starting a new one after cleanup.
  (when-let [slot (:last lease)]
    (locking slot (vreset! slot ::retired)))
  (try
    (when-let [detach! (:detach! lease)] (detach!))
    (finally
      (when-let [close! (:close! lease)] (close!)))))

(defn- close-lease-safely! [query lease]
  (try
    (close-lease! lease)
    true
    (catch Throwable error
      (.log logger System$Logger$Level/WARNING (str "Could not retire Studio query " (first query)) error)
      false)))

(defn- await-retirements! [completions]
  (let [deadline (+ (System/nanoTime) (.toNanos TimeUnit/MILLISECONDS close-wait-ms))]
    (try
      (when-not (every? #(deref % (max 0 (.toMillis TimeUnit/NANOSECONDS (- deadline (System/nanoTime)))) false)
                        completions)
        (.log logger System$Logger$Level/WARNING "Studio query cleanup is still running after shutdown"))
      (catch InterruptedException _ (.interrupt (Thread/currentThread))))))

(defn- surface-query? [store query]
  (contains? (:surfaces store) (first query)))

(defn retiring-queries [store]
  (set (keys (:retiring @(:state store)))))

(defn owns-query-work? [store thread]
  (let [{:keys [reconciling retiring]} @(:state store)]
    (or (identical? thread (:thread reconciling))
        (some #(identical? thread @(:owner %)) (vals retiring)))))

(defn- claim-retirements [leases]
  (into {} (map (fn [[query lease]] [query {:lease lease :owner (atom nil) :done (promise)}])) leases))

(defn- finish-retirement! [store [query {:keys [lease owner done] :as retirement}]]
  ;; Shutdown can take queued entries from a socket or delivery pass. The
  ;; thread already executing an entry keeps ownership of that callback.
  (when (compare-and-set! owner nil (Thread/currentThread))
    (let [closed? (close-lease-safely! query lease)]
      (locking store
        (swap! (:state store)
               (fn [state]
                 (cond-> state
                   (identical? retirement (get-in state [:retiring query])) (update :retiring dissoc query)
                   (and (not closed?) (not (:closed? state)))
                   (update :failed-retirements (fnil conj #{}) query)))))
      (deliver done true)
      ((:wake! store)))))

(defn close!
  ([store] (close! store true))
  ([store wait?] (close! store wait? []))
  ([store wait? cleanups]
   (let [[retiring reconciling] (when store
                                 (locking store
                                   (let [state (:state store) before @state]
                                     (when-not (:closed? before)
                                       (let [retiring (merge (:retiring before) (claim-retirements (:leases before)))]
                                         (reset! state {:closed? true :retiring retiring :reconciling (:reconciling before)})
                                         [retiring (:reconciling before)])))))
         completions (mapv (fn [_] (promise)) cleanups)]
     (let [reentrant? (and store (owns-query-work? store (Thread/currentThread)))
           queued (remove (fn [[_ entry]] (some? @(:owner entry))) retiring)]
       (when (or (seq queued) (seq cleanups))
         (let [executor (Executors/newFixedThreadPool
                          4 (reify ThreadFactory
                              (newThread [_ task]
                                (doto (Thread. task "tolglow-query-cleanup") (.setDaemon true)))))]
           (doseq [entry queued]
             (.execute executor ^Runnable #(finish-retirement! store entry)))
           (doseq [[cleanup! done] (map vector cleanups completions)]
             (.execute executor ^Runnable #(try (cleanup!) (finally (deliver done true)))))
           (.shutdown executor)))
       (when (and wait? (not reentrant?))
         (await-retirements! (cond-> (into completions (map :done) (vals retiring))
                              reconciling (conj (:done reconciling)))))))
   nil))

(defn release-unused-leases!
  "Retire unobserved graph and surface queries independently of projection delivery."
  [store wanted-queries candidates]
  (let [state (:state store)
        retired (locking store
                  (let [before @state]
                    (when-not (:closed? before)
                      (let [wanted (wanted-queries)
                            unused (filterv #(and (contains? (:leases before) %)
                                                  (not (contains? wanted %)))
                                            candidates)]
                        (when (seq unused)
                          (let [retired (claim-retirements (select-keys (:leases before) unused))]
                            (reset! state (-> before
                                              (update :leases #(apply dissoc % unused))
                                              (update :wanted #(apply disj % unused))
                                              (update :missing #(apply disj % unused))
                                              (update :retiring merge retired)))
                            retired))))))]
    (doseq [entry retired] (finish-retirement! store entry))))

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

(defn- fence-failed-retirement! [store query]
  (locking store
    (let [state (:state store)]
      (when-not (:closed? @state)
        (swap! state update :failed-retirements (fnil conj #{}) query)))))

(defn- open-lease [store system query public]
  (if-let [open! (get (:surfaces store) (first query))]
    (try
      (let [{:keys [observe!] :as lease} (assoc (open! query) :last (volatile! nil))]
        (try
          (cond-> lease
            (and observe! (not (:closed? @(:state store)))) (assoc :detach! (observe! (:wake! store))))
          (catch Exception error
            ;; Opening can allocate resources before installing its observer.
            (close-lease-safely! query lease)
            (fence-failed-retirement! store query)
            (throw error))))
      (catch Exception error
        (.log logger System$Logger$Level/WARNING (str "Could not open Studio query " (first query)) error)
        unavailable-lease))
    (open-graph-lease system query (contains? public (first query)) (:wake! store))))

(defn- current-demand [store wanted-queries]
  (reduce disj (wanted-queries) (keys (:retiring @(:state store)))))

(defn- plan-reconciliation! [store model system requested wanted-queries]
  (locking store
    (let [state (:state store) before @state]
      (when-not (:closed? before)
        (let [wanted (into (empty requested) (filter (current-demand store wanted-queries)) requested)
              configured (get-in model [:settings :studio :public-queries])
              public (if (registry/valid-public-query-ids? configured) (into defaults configured) defaults)
              identity (select-keys model [:show-id :epoch])
              compatible? (and (identical? system (:system before)) (= identity (:identity before))
                               (= public (:public before)))
              prior (:leases before)
              newly-registered (when (and compatible? system)
                                 (into #{} (filter #(reactive/registered-sub? system (first %))) (:missing before)))
              kept (if compatible? (reduce disj (into #{} (filter wanted) (:wanted before)) newly-registered) #{})]
          (when-not (and compatible? (empty? newly-registered) (= wanted (:wanted before))
                         (= (count wanted) (count prior)))
            (let [retired (claim-retirements
                            (remove (fn [[q _]]
                                      (or (contains? kept q)
                                          (and (contains? wanted q) (not (surface-query? store q)))))
                                    prior))
                  leases (apply dissoc prior (keys retired))
                  open (reduce disj (reduce disj wanted kept) (keys retired))]
              ;; Keep superseded graph leases live until their replacements
              ;; acquire. Removed and superseded surfaces retire before reuse.
              (reset! state (assoc before :system system :identity identity :public public
                                          :leases leases :wanted kept
                                          :retiring (merge (:retiring before) retired)
                                          :missing (into #{} (filter kept) (:missing before))))
              [public retired open])))))))

(defn- acquire-current! [store system public query wanted-queries]
  (let [state (:state store)]
    (when (locking store
            (and (not (:closed? @state)) (contains? (current-demand store wanted-queries) query)))
      (let [lease (if (contains? (:failed-retirements @state) query) unavailable-lease
                     (open-lease store system query public))
            installed (locking store
                        (when (and (not (:closed? @state)) (contains? (current-demand store wanted-queries) query))
                          (let [prior (get-in @state [:leases query])]
                            (swap! state (fn [s] (-> s
                                                    (assoc-in [:leases query] lease)
                                                    (update :wanted conj query)
                                                    (update :missing (if (:await-registration? lease) conj disj) query))))
                            [prior])))]
        (if installed
          (when-let [prior (first installed)] (close-lease-safely! query prior))
          (when-not (close-lease-safely! query lease)
            (fence-failed-retirement! store query)))))))

(defn reconcile-current!
  "Share leases across sockets. Changing show identity, graph or public-query
  policy invalidates previous acquisitions, including unavailable queries."
  [store source wanted-queries]
  (let [state (:state store)
        [requested done] (locking store
                           (when-not (or (:closed? @state) (:reconciling @state))
                             (let [requested (current-demand store wanted-queries)]
                               (when (or (seq requested) (seq (:leases @state)))
                                 (let [done (promise)]
                                   (swap! state assoc :reconciling {:thread (Thread/currentThread) :done done})
                                   [requested done])))))]
    (when done
      (try
        (let [system (when (seq requested)
                       ;; The runtime can close its graph during shutdown.
                       (try ((:get-graph store)) (catch Exception _ nil)))
              [public retired open] (plan-reconciliation! store (:model source) system requested wanted-queries)]
          (doseq [entry retired] (finish-retirement! store entry))
          (doseq [query open] (acquire-current! store system public query wanted-queries))
          (let [{:keys [leases wanted]} @state]
            (into {} (filter #(contains? wanted (key %))) leases)))
        (finally
          (locking store (swap! state dissoc :reconciling))
          (deliver done true))))))

(defn reconcile! [store source wanted]
  (reconcile-current! store source (constantly wanted)))
