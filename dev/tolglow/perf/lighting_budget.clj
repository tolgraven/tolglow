(ns tolglow.perf.lighting-budget
  "Measure just the post-composition budget on changing completed compact RGBW frames.
  Packet and preview projection, allocation and both DMX measurements are included."
  (:require [afterglow.render.frame :as frame]
            [tolglow.bench-show :as bench]
            [tolglow.lighting-budget :as budget]
            [tolglow.lighting-capacity :as capacity])
  (:import [com.sun.management ThreadMXBean]
           [java.lang.management ManagementFactory]
           [java.util Arrays]))

(defn- pixel [i]
  {:id (keyword (str "pixel-" i)) :universe (inc (quot i 128)) :address (inc (* 4 (mod i 128)))
   :max-watts 1.0 :channels {:red 1 :green 2 :blue 3 :white 4}})

(defn- source-frame [patch red]
  (let [assignments (mapv (fn [{:keys [id]}]
                            {:target-id id :attribute :color :value [red 0.9 0.7]})
                          (:fixtures patch))]
    (frame/resolve-frame patch assignments {:include-values? false :compact-values? true})))

(defn- verify! [row maximum-active]
  (when-not (and (= maximum-active (:active-count row))
                 (< 0 (:used-watts row) (:requested-watts row))
                 (<= (:used-watts row) (:limit-watts row)))
    (throw (ex-info "Unexpected captured power or active source count" row))))

(defn profile [pixels warmup samples]
  (let [fixtures (mapv pixel (range pixels))
        patch (frame/compile-patch fixtures)
        plan (capacity/prepare patch)
        maximum-active (quot pixels 2)
        config (budget/settings {:enabled? true :global {:limit 0.35 :max-active maximum-active :standby-level 0.02}})
        context (budget/prepare-frame-context nil {:settings {:lighting-budget config}} patch plan)
        raw [(source-frame patch 0.8) (source-frame patch 1.0)]
        cap (fn [i] (budget/apply-frame context (nth raw (bit-and i 1))))
        ^ThreadMXBean counters (ManagementFactory/getThreadMXBean)
        thread (.getId (Thread/currentThread))
        times (double-array samples)]
    (when-not (and (.isCurrentThreadCpuTimeSupported counters) (.isThreadAllocatedMemoryEnabled counters))
      (throw (ex-info "The budget profiler requires HotSpot CPU and allocation counters" {})))
    (dotimes [i warmup] (cap i))
    (let [start-cpu (.getCurrentThreadCpuTime counters)
          start-bytes (.getThreadAllocatedBytes counters thread)]
      (dotimes [i samples]
        (let [start (System/nanoTime) frame (cap i)]
          (aset times i (/ (- (System/nanoTime) start) 1e6))
          (verify! (get-in frame [:lighting-budget :global]) maximum-active)))
      (let [cpu (- (.getCurrentThreadCpuTime counters) start-cpu)
            bytes (- (.getThreadAllocatedBytes counters thread) start-bytes)]
        (Arrays/sort times)
        {:pixels pixels :warmup warmup :samples samples :budget (:global config)
         :cpu-ms-per-frame (/ cpu (* samples 1e6))
         :p95-ms (aget times (dec (int (Math/ceil (* 0.95 samples)))))
         :allocated-bytes-per-frame (/ (double bytes) samples)
         :frames-over-25-ms (count (filter #(> % 25) times))
         :captured (mapv #(get-in (cap %) [:lighting-budget :global]) [0 1])}))))

(defn -main [& args]
  (let [[pixels warmup samples] (if (seq args) (mapv parse-long args) [2048 50 125])]
    (when-not (and (pos-int? pixels) (> pixels 1) (nat-int? warmup) (pos-int? samples))
      (throw (ex-info "Expected pixels, warmup frames and measured frames" {:args args})))
    (try
      (prn (assoc (profile pixels warmup samples)
                  :tolglow (bench/revision ".")
                  :afterglow (bench/revision (or (System/getenv "AFTERGLOW_ROOT") "../afterglow"))
                  :java (System/getProperty "java.version") :cpu (bench/cpu-model)))
      (finally (shutdown-agents)))))
