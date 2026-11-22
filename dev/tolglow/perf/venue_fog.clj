(ns tolglow.perf.venue-fog
  "Headless venue fog cost on the full show and independent output paths."
  (:require [afterglow.output.scheduler :as scheduler]
            [clojure.string :as str]
            [tolglow.bench-show :as bench]
            [tolglow.show :as show])
  (:import [com.sun.management ThreadMXBean]
           [java.lang.management ManagementFactory]
           [java.util Arrays]))

(def ^:private sources 4)
(def ^:private period-ms 25)
(def ^:private ^ThreadMXBean metrics (ManagementFactory/getThreadMXBean))

(defn- create-case [base pixels tracked? independent?]
  (let [universe (inc (quot (+ (* 4 pixels) 511) 512))
        jets (mapv (fn [i]
                     (cond-> {:id (str "fog-" i) :address (inc i) :universe universe
                              :channels {:fog 1} :position [i 0 0]}
                       tracked? (assoc :fogger {:flow-m3-min 60 :mixing-seconds 8})))
                   (range sources))
        sink (bench/capture-sink)
        clock (volatile! 0)
        rt (show/create-runtime {:model (update-in base [:rig :fixtures] into jets)
                                 :workers 2 :workspace-options nil :sink sink
                                 :independent-output? independent? :physical-now-ms #(deref clock)})]
    (try
      (let [effect {:id :fog-profile :version 1 :targets (mapv :id jets) :outputs {:fog 0.5}}
            replies (mapv #(show/submit! rt %)
                          [{:op :registry/publish :kind :effects :definition effect}
                           {:op :cue/put :cue {:id :fog-profile :effect-id :fog-profile}}
                           {:op :cue/launch :cue-id :fog-profile}])]
        (show/tick! rt 0)
        (when independent? (scheduler/publish! (:publisher rt)))
        (when-not (every? #(= :accepted (:status (deref %))) replies)
          (throw (ex-info "Fog benchmark cue setup failed" {:receipts (mapv deref replies)})))
        {:runtime rt :sink sink :clock clock :tracked? tracked? :independent? independent? :universe universe})
      (catch Exception e (show/stop! rt) (throw e)))))

(defn- run-frame! [{:keys [runtime clock independent?]}]
  (let [now (vswap! clock + period-ms)
        start (System/nanoTime)]
    (show/tick! runtime now)
    (if independent?
      (let [id (.getId (Thread/currentThread))
            cpu-start (.getCurrentThreadCpuTime metrics)
            alloc-start (.getThreadAllocatedBytes metrics id)
            publish-start (System/nanoTime)]
        (scheduler/publish! (:publisher runtime))
        (let [end (System/nanoTime)]
          [(/ (- end start) 1e6) (/ (- end publish-start) 1e6)
           (/ (- (.getCurrentThreadCpuTime metrics) cpu-start) 1e6)
           (- (.getThreadAllocatedBytes metrics id) alloc-start)]))
      [(/ (- (System/nanoTime) start) 1e6)])))

(defn- verify! [{:keys [runtime sink clock tracked? universe]}]
  (let [{:keys [status output]} @(:state runtime)
        packets @(:latest sink)
        ;; The capture encodes each universe as an ArtDMX packet, sorted by port.
        packet ^bytes (last packets)
        levels (mapv #(bit-and 255 (aget packet (+ 18 %))) (range sources))
        state (:atmosphere output)]
    (when-not (and (= universe (count packets)) (every? #{127 128} levels)
                   (not-any? status [:error :output-error :reactive-error])
                   (if tracked?
                     (and (= sources (count (:sources state)))
                          (pos? (:mixed-density state))
                          (every? true?
                                  (map (fn [source level]
                                         (< (abs (- (:emitted-m3 source) (* (/ @clock 1000.0) (/ level 255.0)))) 1e-6))
                                       (:sources state) levels)))
                     (nil? state)))
      (throw (ex-info "Incorrect packet or fog integral" {:levels levels :fog state :status status})))))

(defn- thread-counts [^ThreadMXBean bean ids]
  (let [sum #(reduce + (map (fn [id] (max 0 (% (long id)))) ids))]
    [(sum #(.getThreadCpuTime bean %)) (sum #(.getThreadAllocatedBytes bean %))]))

(defn- sample [case frames]
  (let [^ThreadMXBean bean (ManagementFactory/getThreadMXBean)
        owner (.getId (Thread/currentThread))
        ids (filterv (fn [id]
                       (let [info (.getThreadInfo bean (long id))]
                         (and info (or (= id owner) (str/starts-with? (.getThreadName info) "afterglow-runtime-")))))
                     (.getAllThreadIds bean))
        [cpu-before bytes-before] (thread-counts bean ids)
        samples (mapv (fn [_] (run-frame! case)) (range frames))
        [cpu-after bytes-after] (thread-counts bean ids)]
    (verify! case)
    {:samples samples :cpu-ms (/ (- cpu-after cpu-before) 1e6) :bytes (- bytes-after bytes-before)}))

(defn- summarize [batches independent?]
  (let [samples (mapcat :samples batches)
        frames (count samples)]
    (cond-> {:frames frames :wall-ms (bench/distribution (map first samples))
             :over-25-ms (count (filter #(> (first %) period-ms) samples))
             :cpu-ms-per-frame (/ (reduce + (map :cpu-ms batches)) frames)
             :bytes-per-frame (double (/ (reduce + (map :bytes batches)) frames))}
      independent? (assoc :publisher-ms (bench/distribution (map second samples))
                          :publisher-over-25-ms (count (filter #(> (second %) period-ms) samples))
                          :publisher-cpu-ms (bench/distribution (map #(nth % 2) samples))
                          :publisher-bytes (bench/distribution (map #(nth % 3) samples))))))

(defn- compare-block [a b n]
  (let [measurement-a (sample a n)
        measurement-b (sample b n)
        capture #(deref (get-in % [:sink :latest]))
        packets-a (capture a)
        packets-b (capture b)]
    (when-not (and (= (count packets-a) (count packets-b))
                   (every? true? (map #(Arrays/equals ^bytes %1 ^bytes %2) packets-a packets-b)))
      (throw (ex-info "Tracking fog changed ArtDMX output" {:frame-time @(:clock a)})))
    {(:tracked? a) measurement-a (:tracked? b) measurement-b}))

(defn profile [pixels warmup samples]
  (let [model (:model (bench/corpus pixels 0))]
    (into {}
          (for [independent? [false true]]
            (let [a (create-case model pixels false independent?)]
              (try
                (let [b (create-case model pixels true independent?)]
                  (try
                    (dotimes [_ warmup] (run-frame! a) (run-frame! b))
                    ;; Reverse the order every block so JVM warmup and external load
                    ;; do not consistently favor one configuration.
                    (let [results (mapv (fn [i block]
                                          (if (even? i) (compare-block a b (count block))
                                              (compare-block b a (count block))))
                                        (range) (partition-all 25 (range samples)))]
                      [(if independent? :independent :direct)
                       {:without-fog-model (summarize (map #(get % false) results) independent?)
                        :with-fog-model (summarize (map #(get % true) results) independent?)}])
                    (finally (show/stop! (:runtime b)))))
                (finally (show/stop! (:runtime a)))))))))

(defn -main [& args]
  (let [[pixels warmup samples] (if (empty? args) [2048 100 200] (mapv parse-long args))]
    (when-not (and (or (empty? args) (= 3 (count args))) (every? pos-int? [pixels warmup samples]))
      (throw (ex-info "Expected pixel, warmup and sample counts" {:args args})))
    (when-not (and (.isCurrentThreadCpuTimeSupported metrics) (.isThreadCpuTimeEnabled metrics)
                   (.isThreadAllocatedMemorySupported metrics) (.isThreadAllocatedMemoryEnabled metrics))
      (throw (ex-info "The fog profiler needs enabled HotSpot CPU and allocation counters" {})))
    (try
      (prn {:tolglow (bench/revision ".") :afterglow (bench/revision (or (System/getenv "AFTERGLOW_ROOT") "../afterglow"))
            :java (System/getProperty "java.version") :cpu (bench/cpu-model)
            :max-heap (.maxMemory (Runtime/getRuntime)) :pixels pixels :sources sources
            :warmup warmup :samples samples :results (profile pixels warmup samples)})
      (finally (shutdown-agents)))))
