(ns tolglow.spatial.sampling
  "Sample one venue volume over contiguous fixture ranges on workers owned by
  the show. Afterglow's own samplers evaluate every range, so the arithmetic for
  a fixture is the serial arithmetic. The hits are then replayed in the serial
  insertion order, which gives the same maps in the same iteration order."
  (:require [afterglow.spatial :as physics]
            [afterglow.spatial.field :as field]
            [afterglow.spatial.geometry :as geometry])
  (:import [java.io Closeable]
           [java.util ArrayList]
           [java.util.concurrent ArrayBlockingQueue Callable ExecutionException Future FutureTask
            RejectedExecutionException ThreadFactory ThreadPoolExecutor TimeUnit]
           [java.util.concurrent.atomic AtomicLong]))

(def default-min-fixtures
  "A patch with fewer fixtures stays on the serial path."
  4096)

(def thread-prefix "tolglow-spatial-field-")

(defn default-workers
  "Leave one processor for the publisher. The calling render thread counts as a worker."
  []
  (max 1 (dec (.availableProcessors (Runtime/getRuntime)))))

(defrecord Sampler [^ThreadPoolExecutor executor workers partitions min-fixtures]
  Closeable
  (close [_]
    (when executor
      (.shutdownNow executor)
      (try
        (.awaitTermination executor 1 TimeUnit/SECONDS)
        (catch InterruptedException _
          (.interrupt (Thread/currentThread)))))))

(defn- pool ^ThreadPoolExecutor [threads queue-size]
  (let [serial (AtomicLong. 0)
        ^ThreadFactory factory (reify ThreadFactory
                                 (newThread [_ task]
                                   (doto (Thread. ^Runnable task (str thread-prefix (.incrementAndGet serial)))
                                     (.setDaemon true))))]
    (ThreadPoolExecutor. (int threads) (int threads) 0 TimeUnit/MILLISECONDS
                         (ArrayBlockingQueue. (int queue-size)) factory)))

(defn create
  "Create the show's field sampler without starting a thread.

  :workers is the number of threads sampling one volume, counting the caller.
  It defaults to the available processors minus one. The sampler owns
  workers - 1 daemon threads, and 1 keeps every patch on the serial path.
  :partitions is the number of contiguous fixture ranges (default 2 x workers).
  :min-fixtures is the smallest patch that is split (default 4096). Close the
  sampler with the show."
  ([] (create nil))
  ([options]
   (when-not (and (or (nil? options) (map? options))
                  (every? #{:workers :partitions :min-fixtures} (keys options)))
     (throw (ex-info "Invalid spatial sampling option" {:options options})))
   (let [workers (if (nil? (:workers options)) (default-workers) (:workers options))
         _ (when-not (and (integer? workers) (<= 1 workers 256))
             (throw (ex-info "Invalid spatial sampling option" {:option :workers :value workers})))
         partitions (if (nil? (:partitions options)) (* 2 workers) (:partitions options))
         min-fixtures (if (nil? (:min-fixtures options)) default-min-fixtures (:min-fixtures options))]
     (doseq [[option value limit] [[:partitions partitions 4096] [:min-fixtures min-fixtures Integer/MAX_VALUE]]]
       (when-not (and (integer? value) (<= 1 value limit))
         (throw (ex-info "Invalid spatial sampling option" {:option option :value value}))))
     (->Sampler (when (> workers 1) (pool (dec workers) (* 2 partitions)))
                workers partitions min-fixtures))))

(defn parallel?
  "True when the sampler owns workers and the patch reaches its threshold."
  [sampler fixture-count]
  (boolean (and sampler (:executor sampler) (>= fixture-count (:min-fixtures sampler)))))

(defn- ranges
  "Split n rows into at most parts contiguous, non-empty [start end) ranges."
  [n parts]
  (let [parts (min n parts)]
    (mapv (fn [i] [(quot (* i n) parts) (quot (* (inc i) n) parts)]) (range parts))))

(defn partition-rig
  "Prepare the contiguous slices of one patch. Cache the result with its rig.

  Position weights are inserted in the rig's X rank order, so :position slices
  cut the ranked rows. Beam weights are inserted in patch order, so :optical
  slices cut the fixture vector. A patch with a repeated ID has no slices and
  stays on the serial path."
  [fixtures rig parts]
  (let [fixtures (vec fixtures)
        rows (geometry/candidates rig nil)
        plan {:rig rig :parts parts}]
    (if-not (and (= (count rows) (count fixtures))
                 (= (count rows) (count (into #{} (map first) rows))))
      plan
      (assoc plan
             :sliced? true
             :position (mapv (fn [[start end]]
                               (let [slice (subvec rows start end)
                                     points (mapv (fn [[id position]] {:id id :position position}) slice)]
                                 {:rows slice :rig (geometry/prepare-rig points)}))
                             (ranges (count rows) parts))
             :optical (mapv (fn [[start end]]
                              (let [slice (subvec fixtures start end)]
                                {:fixtures slice :rig (physics/prepare-rig slice)}))
                            (ranges (count fixtures) parts))))))

(defn- position-hits
  "Position weights of one ranked slice as [id weight ...] in rank order."
  [volume {:keys [rig rows]}]
  (let [weights (:weights (field/positions volume rig))
        hits (ArrayList.)]
    (when (pos? (count weights))
      (run! (fn [row]
              (let [id (nth row 0)]
                (when-some [weight (get weights id)]
                  (.add hits id)
                  (.add hits weight))))
            rows))
    hits))

(defn- beam-hits
  "Optical weights of one patch slice as [id weight aim ...] in patch order."
  [sample-slice volume fixtures]
  (let [{:keys [weights aims]} (sample-slice volume)
        hits (ArrayList.)]
    (when (pos? (count weights))
      (run! (fn [fixture]
              (let [id (:id fixture)]
                (when-some [weight (get weights id)]
                  (.add hits id)
                  (.add hits weight)
                  (.add hits (get aims id)))))
            fixtures))
    hits))

(defn- add-positions [weights ^ArrayList hits]
  (let [n (.size hits)]
    (loop [i 0 weights weights]
      (if (< i n)
        (recur (+ i 2) (assoc! weights (.get hits i) (.get hits (+ i 1))))
        weights))))

(defn- merge-hits
  "Repeat the serial sampler's inserts: ranked positions first, then beams in
  patch order, where a head keeps the greater of its weights."
  [position-slices beam-slices optical?]
  (let [initial (persistent! (reduce add-positions (transient {}) position-slices))]
    (if-not optical?
      {:weights initial :aims {}}
      (let [aims (volatile! (transient {}))
            add-beams (fn [weights ^ArrayList hits]
                        (let [n (.size hits)]
                          (loop [i 0 weights weights]
                            (if (< i n)
                              (let [id (.get hits i)
                                    weight (.get hits (+ i 1))
                                    aim (.get hits (+ i 2))]
                                (when aim (vswap! aims assoc! id aim))
                                (recur (+ i 3) (assoc! weights id (max weight (get weights id 0.0)))))
                              weights))))]
        {:weights (persistent! (reduce add-beams (transient initial) beam-slices))
         :aims (persistent! @aims)}))))

(defn- await-task
  "Wait until the task has finished, even when this thread is interrupted."
  [^Future task]
  (loop [interrupted? false]
    (let [outcome (try
                    {:value (.get task)}
                    (catch InterruptedException _ nil)
                    (catch ExecutionException error {:failure (.getCause error)}))]
      (if outcome
        (assoc outcome :interrupted? interrupted?)
        (recur true)))))

(defn- run-all
  "Run every thunk and return the values in thunk order. No thunk is still
  running when this returns or throws. The first failure in thunk order is
  rethrown as the worker threw it."
  [^ThreadPoolExecutor executor thunks]
  (let [tasks (mapv (fn [thunk] (FutureTask. ^Callable thunk)) thunks)]
    (when executor
      ;; A rejected task, or a failure to start a worker thread, leaves the
      ;; remaining tasks unstarted. This thread runs them below and then waits
      ;; for every submitted task, so nothing is still running on return.
      (try
        (run! (fn [task] (.execute executor ^Runnable task)) tasks)
        (catch RejectedExecutionException _)
        (catch OutOfMemoryError error
          (when-not (some-> (.getMessage error) (.contains "native thread"))
            (throw error)))))
    ;; Workers take the head of the queue. This thread claims unstarted work
    ;; from the tail. A FutureTask runs once, so the other side skips it. A
    ;; full or closing pool therefore still completes the frame here.
    (run! (fn [task] (.run ^FutureTask task)) (rseq tasks))
    (let [outcomes (mapv await-task tasks)]
      (when executor (.clear (.getQueue executor)))
      (when (some :interrupted? outcomes) (.interrupt (Thread/currentThread)))
      (when-let [failure (some :failure outcomes)] (throw failure))
      (mapv :value outcomes))))

(defn sample
  "Return the serial sampler's {:weights :aims} for one volume.

  slices comes from partition-rig. optical-samplers holds one sampler per
  optical slice of the committed frame when the volume selects beams or aim,
  and is nil for a position-only volume."
  [sampler {:keys [position optical]} volume optical-samplers]
  ;; The serial path validates the volume before it reads a fixture.
  (field/prepare volume)
  (let [selection (get volume :selection #{:position})
        beams (when optical-samplers (assoc volume :selection (disj selection :position)))
        position-tasks (if (contains? selection :position)
                         (mapv (fn [slice] #(position-hits volume slice)) position)
                         [])
        beam-tasks (if beams
                     (mapv (fn [slice sample-slice] #(beam-hits sample-slice beams (:fixtures slice)))
                           optical optical-samplers)
                     [])
        hits (run-all (:executor sampler) (into position-tasks beam-tasks))
        split (count position-tasks)]
    (merge-hits (subvec hits 0 split) (subvec hits split) (some? beams))))
