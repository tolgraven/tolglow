(ns tolglow.perf.jfr-summary
  "Summarise a JFR recording by application frame: CPU samples by first and
  inclusive Tolglow/Afterglow function, allocation weight by first application
  frame and allocated class. Library frames (clojure.lang, java.*) are skipped
  when choosing the first application frame."
  (:require [clojure.string :as str])
  (:import [jdk.jfr.consumer RecordedEvent RecordedFrame RecordingFile]))

(defn- application-frame [^RecordedFrame frame]
  (let [type (.getName (.getType (.getMethod frame)))]
    (when (or (str/starts-with? type "tolglow.") (str/starts-with? type "afterglow."))
      (-> type (str/replace #"\$fn__\d+.*$" "") (str/replace #"\$reify__\d+.*$" "/reify")
          (str/replace #"\$" "/") (str/replace #"_" "-")))))

(defn- top-frame [^RecordedEvent event]
  (when-let [stack (.getStackTrace event)]
    (let [frame ^RecordedFrame (first (.getFrames stack))]
      (when frame (str (.getName (.getType (.getMethod frame))) "." (.getName (.getMethod frame)))))))

(defn- frames [^RecordedEvent event]
  (when-let [stack (.getStackTrace event)] (keep application-frame (.getFrames stack))))

(defn summary
  "Return {:cpu-samples n :first {...} :inclusive {...} :allocation-first {...} :allocation-class {...}}."
  [path]
  (with-open [file (RecordingFile. (.toPath (java.io.File. ^String path)))]
    (loop [out {:cpu-samples 0 :allocation-bytes 0}]
      (if-not (.hasMoreEvents file)
        out
        (let [event (.readEvent file)
              type (.getName (.getEventType event))]
          (recur
           (case type
             "jdk.ExecutionSample"
             (let [fs (frames event)]
               (-> out (update :cpu-samples inc)
                   (update-in [:first (or (first fs) "(no application frame)")] (fnil inc 0))
                   (update-in [:leaf (top-frame event)] (fnil inc 0))
                   (update :inclusive #(reduce (fn [m f] (update m f (fnil inc 0))) % (distinct fs)))))
             "jdk.ObjectAllocationSample"
             (let [weight (.getLong event "weight")
                   fs (frames event)]
               (-> out (update :allocation-bytes + weight)
                   (update-in [:allocation-first (or (first fs) "(no application frame)")] (fnil + 0) weight)
                   (update-in [:allocation-inclusive] #(reduce (fn [m f] (update m f (fnil + 0) weight)) % (distinct fs)))
                   (update-in [:allocation-class (.getName (.getClass event "objectClass"))] (fnil + 0) weight)))
             out)))))))

(defn- top [m n] (take n (sort-by (comp - val) m)))

(defn -main [path & [n]]
  (let [n (or (some-> n parse-long) 25)
        s (summary path)
        pct (fn [x total] (format "%5.1f%%" (* 100.0 (/ (double x) (max 1 total)))))]
    (println "CPU samples" (:cpu-samples s) " sampled allocation" (format "%.1f MB" (/ (:allocation-bytes s) 1048576.0)))
    (doseq [[title k total] [["Leaf method" :leaf (:cpu-samples s)]
                             ["First application frame (CPU)" :first (:cpu-samples s)]
                             ["Inclusive application frame (CPU)" :inclusive (:cpu-samples s)]
                             ["First application frame (allocation)" :allocation-first (:allocation-bytes s)]
                             ["Inclusive application frame (allocation)" :allocation-inclusive (:allocation-bytes s)]
                             ["Allocated class" :allocation-class (:allocation-bytes s)]]]
      (println) (println "##" title)
      (doseq [[k v] (top (get s k) n)] (println (pct v total) k)))))

(defn stacks
  "Top n leaf-first stacks (depth frames) for events of `event-type` whose
  stack contains a frame matching `pattern`. Allocation events weigh bytes."
  [path event-type pattern depth n]
  (with-open [file (RecordingFile. (.toPath (java.io.File. ^String path)))]
    (loop [out {}]
      (if-not (.hasMoreEvents file)
        (take n (sort-by (comp - val) out))
        (let [event (.readEvent file)]
          (recur
           (if (= event-type (.getName (.getEventType event)))
             (let [names (some->> (.getStackTrace event) .getFrames
                                  (map (fn [^RecordedFrame f]
                                         (str (.getName (.getType (.getMethod f))) "." (.getName (.getMethod f)) ":" (.getLineNumber f)))))]
               (if (some #(re-find pattern %) names)
                 (update out (vec (take depth names)) (fnil + 0)
                         (if (= event-type "jdk.ObjectAllocationSample") (.getLong event "weight") 1))
                 out))
             out)))))))
