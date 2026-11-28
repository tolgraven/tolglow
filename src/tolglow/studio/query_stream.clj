(ns tolglow.studio.query-stream
  "Reference-counted websocket demand for the show's registered reactive signals.
  Only the delivery worker installs leases; graph callbacks publish immutable
  values and wake it. A disconnected socket never owns the underlying graph.

  Surface leases with a :sample! function sample on a small bounded pool, never
  on the delivery worker: at most one sample per lease runs at a time, and each
  committed source is sampled at most once. A slow sample is awaited for a
  bounded deadline and then delivered late as a :stale result (#906)."
  (:require [afterglow.reactive :as reactive]
            [tolglow.registry :as registry])
  (:import [java.lang System$Logger System$Logger$Level]
           [java.util.concurrent Executors RejectedExecutionException SynchronousQueue ThreadFactory
            ThreadPoolExecutor TimeUnit]
           [java.util.concurrent.atomic AtomicInteger]))

(def defaults
  #{:clock/beat :clock/beat-in-bar :clock/bar :clock/phrase :clock/phase
    :runtime/keyframe :runtime/block-trace :runtime/block-library :spatial/scene :spatial/body :spatial/members :rdm/status})

(def ^:private ^System$Logger logger (System/getLogger "tolglow.studio.query-stream"))

(def ^:private close-wait-ms 1000)

(def ^:private unavailable-lease {:result (delay {:error :unavailable})})

(def ^:private sampler-threads
  "The most surface samples that run at once across one store. A further
  sample is refused and counted as :busy until a sampler returns."
  8)

(def default-sample-wait-ms
  "How long a delivery pass waits for the samples it started before it sends
  their clients the previous result marked :stale."
  50)

(def ^:private sample-counts {:completed 0 :failed 0 :late 0 :skipped 0 :busy 0})

(defn- sampler-executor ^ThreadPoolExecutor []
  ;; No queue: a sample either gets a thread now or is refused. Idle threads
  ;; exit after a second, so a store without demand holds none.
  (ThreadPoolExecutor. 0 (int sampler-threads) 1 TimeUnit/SECONDS (SynchronousQueue.)
                       (reify ThreadFactory
                         (newThread [_ task] (doto (Thread. task "tolglow-query-sample") (.setDaemon true))))))

(defn create
  "Create a query store. Shutdown can retry a surface's detach callback once
  after an exception. Surface close callbacks run once even if they throw.
  :sample-wait-ms bounds how long a delivery pass waits for a surface sample."
  ([get-graph wake!] (create get-graph wake! {}))
  ([get-graph wake! surfaces] (create get-graph wake! surfaces {}))
  ([get-graph wake! surfaces {:keys [sample-wait-ms] :or {sample-wait-ms default-sample-wait-ms}}]
   {:get-graph get-graph :wake! wake! :state (atom {}) :surfaces surfaces
    :sampler (delay (sampler-executor)) :sample-wait-ms sample-wait-ms :totals (atom sample-counts)}))

;; Surface sampling ------------------------------------------------------------
;;
;; A sampled lease's :last slot holds, under its own lock, the last completed
;; sample (:source, :frame, :value), the running :flight, :late? when a sample
;; overran the deadline, :busy? when the pool refused the latest start, and its
;; :counts. ::retired fences the lease; :retiring? defers that fence, with the
;; lease's retirement, until the running sample returns.

(defn- count-in [state outcome]
  (update-in state [:counts outcome] (fnil inc 0)))

(defn- total! [store outcome]
  (swap! (:totals store) update outcome inc))

(defn- finish-sample! [store lease flight outcome]
  (let [slot (:last lease)
        on-time? (<= (- (System/nanoTime) (:started flight))
                     (.toNanos TimeUnit/MILLISECONDS (:sample-wait-ms store)))
        kind (if (:error outcome) :failed :completed)
        retire! (locking slot
                  (let [state @slot]
                    (if (:retiring? state)
                      (do (vreset! slot ::retired) (:on-retire state))
                      (do (vreset! slot (cond-> (-> state
                                                    (assoc :source (:source flight) :frame (:frame flight) :value outcome)
                                                    (dissoc :flight)
                                                    (count-in kind))
                                          on-time? (assoc :late? false)))
                          nil))))]
    (total! store kind)
    (deliver (:done flight) true)
    (if retire! (retire!) ((:wake! store)))))

(defn- run-sample! [store lease flight]
  (finish-sample! store lease flight
                  (try {:value ((:sample! lease) (:source flight))}
                       (catch Throwable _ {:error :unavailable}))))

(defn- begin-flight! [store lease source committed]
  (let [slot (:last lease)
        [flight late?] (locking slot
                         (let [state @slot]
                           (cond
                             (or (not (map? state)) (:retiring? state)) nil
                             (identical? source (:source state)) nil

                             (:flight state)
                             ;; Never a second concurrent sample of one lease.
                             (do (when-not (or (identical? source (:source (:flight state)))
                                               (identical? source (:skipped state)))
                                   (vswap! slot #(-> % (assoc :skipped source) (count-in :skipped)))
                                   (total! store :skipped))
                                 nil)

                             :else
                             (let [flight {:source source :frame (get-in committed [:model :frame-number])
                                           :started (System/nanoTime) :done (promise)}]
                               (vswap! slot assoc :flight flight :busy? false)
                               [flight (:late? state)]))))
        started? (when flight
                   (try (.execute ^ThreadPoolExecutor @(:sampler store) ^Runnable #(run-sample! store lease flight))
                        true
                        (catch RejectedExecutionException _
                          (locking slot
                            (when (map? @slot)
                              (vswap! slot #(-> % (dissoc :flight) (assoc :busy? true) (count-in :busy)))))
                          (total! store :busy)
                          false)))]
    {:lease lease :source source :flight (when started? flight) :await? (and started? (not late?))}))

(defn- begin-sample! [store lease committed]
  (if-not (:sample! lease)
    {:lease lease}
    (let [source (try (if-let [current (:source! lease)] (current) committed)
                      (catch Exception _ ::source-failed))]
      (if (= ::source-failed source)
        {:lease lease :failed? true}
        (begin-flight! store lease source committed)))))

(defn sample!
  "Start this pass's sample of each leased surface, keyed like leases. Graph
  leases need no sample. Returns entries for result and await-samples!."
  [store leases committed]
  (into {} (map (fn [[query lease]] [query (begin-sample! store lease committed)])) leases))

(defn awaiting
  "The queries whose fresh samples a pass should wait for."
  [entries]
  (into #{} (keep (fn [[query entry]] (when (:await? entry) query))) entries))

(defn await-samples!
  "Wait, at most the store's sampling deadline, for the samples this pass
  started. A sample that overruns is counted late, and its lease is not
  waited for again until one of its samples finishes inside the deadline."
  [store entries]
  (let [deadline (+ (System/nanoTime) (.toNanos TimeUnit/MILLISECONDS (:sample-wait-ms store)))]
    (doseq [{:keys [lease flight await?]} (vals entries) :when await?]
      (let [remaining (max 0 (.toMillis TimeUnit/NANOSECONDS (- deadline (System/nanoTime))))]
        (when-not (deref (:done flight) remaining false)
          (let [slot (:last lease)
                counted? (locking slot
                           (when (and (map? @slot) (identical? flight (:flight @slot)))
                             (vswap! slot #(-> % (assoc :late? true) (count-in :late)))
                             true))]
            (when counted? (total! store :late))))))))

(defn- stale-marker [state]
  (cond-> {:reason (if (:busy? state) :busy :late)}
    (:frame state) (assoc :frame-number (:frame state))))

(defn result
  "A query's result for this pass: the graph value, the fresh sample of this
  pass's source, or the last completed sample (or :error :unavailable) with
  :stale {:reason :late|:busy :frame-number n} while sampling is behind.
  nil once the lease retires."
  [{:keys [lease source failed?]}]
  (cond
    failed? {:error :unavailable}
    (not (:sample! lease)) @(:result lease)
    :else
    (let [state (let [slot (:last lease)] (locking slot @slot))]
      (when (and (map? state) (not (:retiring? state)))
        (if (identical? source (:source state))
          (:value state)
          (assoc (or (:value state) {:error :unavailable}) :stale (stale-marker state)))))))

(defn- defer-retirement!
  "Fence lease against new samples. While a sample runs, retire! runs on its
  thread once it returns, and this answers true. Otherwise answers false."
  [lease retire!]
  (if-let [slot (:last lease)]
    (locking slot
      (let [state @slot]
        (cond
          (and (map? state) (:retiring? state)) true
          (and (map? state) (:flight state)) (do (vswap! slot assoc :retiring? true :on-retire retire!) true)
          :else (do (vreset! slot ::retired) false))))
    false))

(defn sampling-status
  "Totals of completed, failed, late, skipped and busy surface samples, and
  for each leased surface whether a sample is in flight and the :stale marker
  Studio receives while it is behind. Graph leases are not sampled."
  [store]
  (let [state @(:state store)
        status (fn [lease]
                 (let [s (let [slot (:last lease)] (locking slot @slot))]
                   (if (map? s)
                     (cond-> (merge {:counts (merge sample-counts (:counts s)) :in-flight? (boolean (:flight s))}
                                    (select-keys s [:frame]))
                       (or (:late? s) (:busy? s)) (assoc :stale (stale-marker s))
                       (:retiring? s) (assoc :retiring? true))
                     {:retired? true})))
        sampler @(:sampler store)]
    {:totals @(:totals store)
     :threads {:active (.getActiveCount ^ThreadPoolExecutor sampler) :live (.getPoolSize ^ThreadPoolExecutor sampler)
               :max sampler-threads}
     :queries (into {}
                    (keep (fn [[query lease]] (when (:sample! lease) [query (status lease)])))
                    (concat (:leases state) (map (fn [[q r]] [q (:lease r)]) (:retiring state))))}))

(defn validate [queries]
  (when-not (and (vector? queries) (<= (count queries) 32)
                 (every? #(and (vector? %) (<= 1 (count %) 16) (keyword? (first %))) queries))
    (throw (ex-info "Expected up to 32 named subscription queries" {})))
  (set queries))

(defn- cleanup-safely! [query phase cleanup!]
  (try
    (when cleanup! (cleanup!))
    true
    (catch Throwable error
      (.log logger System$Logger$Level/WARNING (str "Could not " phase " Studio query " (first query)) error)
      false)))

(defn- detach-lease! [query lease]
  ;; Prevent a delivery pass from starting a sample after detachment. Callers
  ;; defer retirement past a running sample, so none runs now.
  (when-let [slot (:last lease)]
    (locking slot (vreset! slot ::retired)))
  (cleanup-safely! query "detach" (:detach! lease)))

(defn- await-retirements! [deadline completions]
  (try
    (when-not (every? #(deref % (max 0 (.toMillis TimeUnit/NANOSECONDS (- deadline (System/nanoTime)))) false)
                      completions)
      (.log logger System$Logger$Level/WARNING "Studio query cleanup is still running after shutdown"))
    (catch InterruptedException _ (.interrupt (Thread/currentThread)))))

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

(defn- remove-retirement [state query retirement]
  (if (and retirement (identical? retirement (get-in state [:retiring query])))
    (update state :retiring dissoc query)
    state))

(defn- complete-retirement! [store query lease retirement detached?]
  (let [released? (cleanup-safely! query "close" (:close! lease))
        failed-detach (when-not detached? (:detach! lease))
        state (:state store)
        ;; Keep a throwing lease fenced. Only its failed detach handle is
        ;; eligible to run again, since close can have arbitrary side effects.
        retry? (locking store
                 (let [before @state]
                   (if (and (:closed? before) failed-detach)
                     true
                     (do (reset! state (cond-> (remove-retirement before query retirement)
                                       (and (not (:closed? before)) (not (and detached? released?)))
                                       (assoc-in [:failed-retirements query] failed-detach)))
                         false))))]
    (when retry?
      (cleanup-safely! query "retry detach for" failed-detach)
      (locking store (swap! state remove-retirement query retirement)))
    (when retirement
      (deliver (:done retirement) true)
      ((:wake! store)))))

(defn- finish-retirement! [store [query {:keys [lease owner] :as retirement}]]
  ;; Shutdown can take queued entries from a socket or delivery pass. The
  ;; thread already executing an entry keeps ownership of that callback.
  (when (compare-and-set! owner nil (Thread/currentThread))
    (let [retire! #(complete-retirement! store query lease retirement (detach-lease! query lease))]
      ;; A blocked sample keeps its lease, and its close, until it returns.
      ;; The sampling thread then owns the retirement; this thread moves on.
      (if (defer-retirement! lease (fn [] (reset! owner (Thread/currentThread)) (retire!)))
        (compare-and-set! owner (Thread/currentThread) ::sampling)
        (retire!)))))

(defn- cleanup-executor ^ThreadPoolExecutor [^String name]
  (doto ^ThreadPoolExecutor (Executors/newFixedThreadPool
                             4 (reify ThreadFactory
                                 (newThread [_ task] (doto (Thread. task name) (.setDaemon true)))))
    ;; If detachment blocks indefinitely, completed release workers can still exit.
    (.setKeepAliveTime 1 TimeUnit/SECONDS)
    (.allowCoreThreadTimeOut true)))

(defn- submit-cleanups! [^ThreadPoolExecutor executor cleanups]
  (mapv (fn [cleanup!]
          (let [done (promise)]
            (.execute executor ^Runnable #(try (cleanup!) (finally (deliver done true))))
            done))
        cleanups))

(defn- start-terminal-cleanups! [store retiring detachments releases]
  (let [queued (filterv (fn [[_ {:keys [owner]}]] (nil? @owner)) retiring)]
    (when (or (seq queued) (seq detachments) (seq releases))
      (let [detach-executor (cleanup-executor "tolglow-query-detach")
            release-executor (cleanup-executor "tolglow-query-release")
            pending-detachments (AtomicInteger. (count queued))
            completions (into (submit-cleanups! detach-executor detachments)
                              (submit-cleanups! release-executor releases))]
        (doseq [[query {:keys [lease owner] :as retirement}] queued]
          (.execute detach-executor
                    ^Runnable #(try
                                 (when (and (compare-and-set! owner nil (Thread/currentThread))
                                            (or (not (defer-retirement!
                                                       lease (fn []
                                                               (reset! owner (Thread/currentThread))
                                                               (complete-retirement! store query lease retirement
                                                                                     (detach-lease! query lease)))))
                                                (do (compare-and-set! owner (Thread/currentThread) ::sampling) false)))
                                   (let [detached? (detach-lease! query lease)]
                                     (.execute release-executor
                                               ^Runnable (fn []
                                                           (reset! owner (Thread/currentThread))
                                                           (complete-retirement! store query lease retirement detached?)))))
                                 (finally
                                   (when (zero? (.decrementAndGet pending-detachments))
                                     (.shutdown release-executor))))))
        (.shutdown detach-executor)
        (when (empty? queued) (.shutdown release-executor))
        completions))))

(defn close!
  "Close admission and, when requested, wait up to one second across all
  cleanup. Detachment and release use separate bounded pools. A phase can
  still be held up by its own callbacks, which keep ownership after the wait."
  ([store] (close! store true))
  ([store wait?] (close! store wait? {}))
  ([store wait? {:keys [detachments releases]}]
   (let [deadline (+ (System/nanoTime) (.toNanos TimeUnit/MILLISECONDS close-wait-ms))
         [retiring reconciling failed] (when store
                                        (locking store
                                          (let [state (:state store) before @state]
                                            (when-not (:closed? before)
                                              (let [retiring (merge (:retiring before) (claim-retirements (:leases before)))]
                                                (reset! state {:closed? true :retiring retiring :reconciling (:reconciling before)})
                                                [retiring (:reconciling before) (:failed-retirements before)])))))
         reentrant? (and store (owns-query-work? store (Thread/currentThread)))
         detachments (into (vec detachments)
                           (keep (fn [[query detach!]]
                                   (when detach! #(cleanup-safely! query "retry detach for" detach!))))
                           failed)
         completions (start-terminal-cleanups! store retiring detachments releases)]
     ;; Running samples finish on their own threads; idle ones exit.
     (when store (.shutdown ^ThreadPoolExecutor @(:sampler store)))
     (when (and wait? (not reentrant?))
       (await-retirements! deadline (cond-> (into (vec completions) (map :done) (vals retiring))
                                      reconciling (conj (:done reconciling))))))
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
        (swap! state assoc-in [:failed-retirements query] nil)))))

(defn- open-lease [store system query public]
  (if-let [open! (get (:surfaces store) (first query))]
    (try
      (let [{:keys [observe!] :as lease} (assoc (open! query) :last (volatile! {:source ::none}))]
        (try
          (cond-> lease
            (and observe! (not (:closed? @(:state store)))) (assoc :detach! (observe! (:wake! store))))
          (catch Exception error
            ;; The observer must undo a partial registration if it throws
            ;; before returning a detach callback.
            (complete-retirement! store query lease nil (detach-lease! query lease))
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
          (when-let [prior (first installed)] (cleanup-safely! query "close" (:close! prior)))
          (complete-retirement! store query lease nil (detach-lease! query lease)))))))

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
