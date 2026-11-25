(ns tolglow.qa.capture-mixed-stage-output
  "Measure the mixed-stage show scheduler against an in-memory ArtDMX sink."
  (:require [afterglow.output.artnet :as artnet]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [tolglow.effects.cadence :as cadence]
            [tolglow.qa.mixed-stage :as mixed]
            [tolglow.show :as show])
  (:import [java.lang.management ManagementFactory]
           [java.time Duration Instant]
           [java.util Arrays]
           [jdk.jfr Recording]
           [jdk.jfr.consumer RecordingFile]))

(def ^:private vm-events
  ["jdk.GCPhasePause" "jdk.GarbageCollection" "jdk.SafepointBegin"
   "jdk.SafepointStateSynchronization" "jdk.SafepointEnd"
   "jdk.ExecuteVMOperation" "jdk.ZAllocationStall"])

(defn- verify-jvm! []
  (let [runtime (Runtime/getRuntime)
        processors (.availableProcessors runtime)
        heap (.maxMemory runtime)
        collectors (mapv #(.getName %) (ManagementFactory/getGarbageCollectorMXBeans))
        expected-processors (some-> (System/getProperty "tolglow.capture.processors") parse-long)
        expected-heap (some-> (System/getProperty "tolglow.capture.heap.bytes") parse-long)]
    (println (str "Effective show JVM: " processors " processors, " heap " bytes max heap, "
                  (first collectors)))
    (flush)
    (when-not (and (= processors expected-processors) (= heap expected-heap)
                   (some #(.startsWith ^String % "ZGC") collectors))
      (throw (ex-info "Capture JVM configuration mismatch"
                      {:expected {:processors expected-processors :heap-max-bytes expected-heap :gc "ZGC"}
                       :actual {:processors processors :heap-max-bytes heap :gc-names collectors}})))
    {:jvm-processors processors :heap-max-bytes heap :gc-names collectors}))

(defn- millis [^Instant instant]
  (+ (* 1000.0 (.getEpochSecond instant)) (/ (.getNano instant) 1.0e6)))

(defn- read-recording [path]
  (let [selected? (set vm-events)]
    (with-open [recording (RecordingFile. path)]
      (loop [events []]
        (if (.hasMoreEvents recording)
          (let [event (.readEvent recording)
                name (.getName (.getEventType event))]
            (recur (cond-> events
                     (selected? name) (conj {:kind name :start (millis (.getStartTime event))
                                             :end (millis (.getEndTime event))}))))
          events)))))

(defn- watch-changes! [state key rows]
  (add-watch state key
    (fn [_ _ old new]
      (when-let [row (key new)]
        (when-not (identical? row (key old))
          (swap! rows conj row))))))

(defn- capture! [file seconds jvm]
  (let [epoch (System/currentTimeMillis)
        origin (System/nanoTime)
        clock #(+ epoch (/ (- (System/nanoTime) origin) 1.0e6))
        sends (atom [])
        renders (atom [])
        missed (atom [])
        output (atom [])
        recording-path (.toPath (file "timing.jfr"))
        host (show/start! {:model (mixed/model) :workspace-options nil :workers 2 :batch? true
                           :frame-rate 40 :adaptive-keyframes {:enabled? true} :physical-now-ms clock}
               (fn [_]
                 {:description {:type :capture :network false}
                  :send! (fn [universes id]
                           (let [at (clock)
                                 packets (mapv (fn [[universe data]] (artnet/artdmx-packet universe 1 data))
                                               (sort-by key universes))]
                             (swap! sends conj {:at at :id id :universes (count packets)
                                                :varying-bytes (mapv #(Arrays/hashCode ^bytes %) packets)})))
                  :close! (fn [])}))
        publisher (:publisher host)]
    (try
      (with-open [jfr (Recording.)]
        (mixed/start-look! host (constantly nil))
        (watch-changes! (:timeline publisher) :last renders)
        (watch-changes! (:timeline publisher) :last-missed-wake missed)
        (watch-changes! (:status publisher) :output-timing output)
        (doseq [event vm-events]
          (.withThreshold (.enable jfr ^String event) (Duration/ofNanos 0)))
        (.start jfr)
        (Thread/sleep 5000)
        (spit (file "ready") (str (clock)))
        (println "Mixed stage scheduler warmed. Output is capture-only.")
        (flush)
        (let [expires (+ (clock) (* 1000 seconds))]
          (while (and (not (.exists (file "done"))) (< (clock) expires))
            (Thread/sleep 250)))
        (.stop jfr)
        (.dump jfr recording-path))
      (let [receipt (assoc jvm :epoch epoch :sends @sends
                          :renders (mapv #(select-keys % [:slot :deadline-ms :cost-ms :ready-at-ms :finished-at-ms
                                                         :slack-ms :start-at-ms :started-at-ms :lead-ms :wake-lag-ms]) @renders)
                          :output (mapv #(select-keys % [:slot :deadline-ms :frame-id :source-time-ms :started-at-ms
                                                         :finished-at-ms :sent-at-ms :sent-by-ms :expired-before-send?
                                                         :failed?]) @output)
                          :misses @missed :cadence (cadence/status host))]
        (spit (file "show.json") (json/write-str (assoc receipt :vm-events (read-recording recording-path)))))
      (finally
        (show/stop! host)
        (shutdown-agents)))))

(defn -main [directory & [seconds-text]]
  (let [seconds (if seconds-text (parse-long seconds-text) 70)
        file #(io/file directory %)]
    (when-not (and directory (.isDirectory (io/file directory)) (pos-int? seconds)
                   (not-any? #(.exists (file %)) ["ready" "done" "show.json" "timing.jfr"]))
      (throw (ex-info "Supply a fresh capture directory and a positive duration in seconds"
                      {:directory directory :seconds seconds-text})))
    (capture! file seconds (verify-jvm!))))
