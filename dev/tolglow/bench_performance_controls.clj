(ns tolglow.bench-performance-controls
  "Measure full owner ticks with output response, allocation and activity enabled."
  (:require [clojure.string :as str]
            [tolglow.bench-show :as bench]
            [tolglow.show :as show])
  (:import [com.sun.management ThreadMXBean]
           [java.lang.management ManagementFactory]))

(defn- worker-ids [^ThreadMXBean bean]
  (into #{} (keep (fn [id]
                    (when-let [info (.getThreadInfo bean (long id))]
                      (when (str/starts-with? (.getThreadName info) "afterglow-runtime-") id))))
        (.getAllThreadIds bean)))

(defn- worker-totals [^ThreadMXBean bean ^longs ids]
  {:cpu-ns (reduce + (.getThreadCpuTime bean ids))
   :allocated-bytes (reduce + (.getThreadAllocatedBytes bean ids))})

(defn- gc-delta [before after]
  (into {} (map (fn [[collector counters]]
                  [collector (merge-with - counters (get before collector))])) after))

(defn profile [pixels movers warmup frames]
  (let [model (:model (bench/corpus pixels movers))
        max-active (max 1 (quot (+ pixels movers) 2))
        ^ThreadMXBean bean (ManagementFactory/getPlatformMXBean ThreadMXBean)
        existing-workers (worker-ids bean)
        rt (show/create-runtime {:model (assoc model :settings
                                           {:lighting-activity {:enabled? true}
                                            :output-response {:enabled? true :global {:attack {:beats 1/8} :release {:beats 1/2}}}
                                            :lighting-budget {:enabled? true :global {:limit 0.30 :max-active max-active
                                                                                      :standby-level 0.03}}})
                                 :workers 2 :sink (bench/capture-sink) :workspace-options nil})
        clock (volatile! 0)
        thread-id (.getId (Thread/currentThread))
        tick! #(show/tick! rt (vswap! clock + 25))]
    (try
      (dotimes [_ warmup] (tick!))
      (let [workers (long-array (remove existing-workers (worker-ids bean)))
            gc-start (bench/gc-snapshot)
            worker-start (worker-totals bean workers)
            samples (mapv (fn [_]
                            (let [wall (System/nanoTime) cpu (.getCurrentThreadCpuTime bean)
                                  allocated (.getThreadAllocatedBytes bean thread-id)]
                              (tick!)
                              [(/ (- (System/nanoTime) wall) 1e6) (/ (- (.getCurrentThreadCpuTime bean) cpu) 1e6)
                               (- (.getThreadAllocatedBytes bean thread-id) allocated)]))
                          (range frames))
            worker-delta (merge-with - (worker-totals bean workers) worker-start)
            gc (gc-delta gc-start (bench/gc-snapshot))
            snapshot @(:state rt)
            usage (get-in snapshot [:output :lighting-budget :global])
            score (get-in snapshot [:output :lighting-activity :scene :score])
            ^booleans aim (get-in snapshot [:output :compact :fields :aim :valid])
            owner-bytes (bench/distribution (map peek samples))
            worker-bytes (/ (double (:allocated-bytes worker-delta)) frames)]
        (assert (and (nil? (get-in snapshot [:status :error])) (number? score) (<= 0 score 1)))
        (assert (= (count (:instances model)) (count (get-in snapshot [:model :instances]))))
        (assert (not-any? :fault (vals (get-in snapshot [:model :instances]))))
        (assert (every? #(aget aim (int %)) (range pixels (+ pixels movers))))
        (assert (and (= (+ pixels movers) (:source-count usage)) (<= (:active-count usage) max-active)
                     (pos? (:used-watts usage)) (<= (:used-watts usage) (+ 1e-7 (:limit-watts usage)))))
        {:pixels pixels :moving-heads movers :warmup warmup :frames frames
         :wall-ms (bench/distribution (map first samples))
         :owner-cpu-ms (bench/distribution (map second samples))
         :owner-allocated-bytes owner-bytes
         :worker-threads (alength workers)
         :worker-cpu-ms-per-frame (/ (:cpu-ns worker-delta) 1e6 frames)
         :worker-allocated-bytes-per-frame worker-bytes
         :owner-and-worker-allocated-bytes-per-frame (+ (:mean owner-bytes) worker-bytes)
         :gc-during-measurement gc
         :over-25-ms (count (filter #(> (first %) 25.0) samples))
         :last-budget usage :last-activity score})
      (finally (show/stop! rt)))))

(defn -main [& args]
  (try
    (let [[pixels movers warmup frames] (if (seq args) (mapv parse-long args) [2048 64 180 240])]
      (prn (profile pixels movers warmup frames)))
    (finally (shutdown-agents))))
