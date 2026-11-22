(ns tolglow.bench-performance-controls
  "Measure full owner ticks with output response, allocation and activity enabled."
  (:require [tolglow.bench-show :as bench]
            [tolglow.show :as show])
  (:import [com.sun.management ThreadMXBean]
           [java.lang.management ManagementFactory]))

(defn profile [pixels movers warmup frames]
  (let [model (:model (bench/corpus pixels movers))
        max-active (quot (+ pixels movers) 2)
        rt (show/create-runtime {:model (assoc model :settings
                                           {:lighting-activity {:enabled? true}
                                            :output-response {:enabled? true :global {:attack {:beats 1/8} :release {:beats 1/2}}}
                                            :lighting-budget {:enabled? true :global {:limit 0.30 :max-active max-active
                                                                                      :standby-level 0.03}}})
                                 :workers 2 :sink (bench/capture-sink) :workspace-options nil})
        clock (volatile! 0)
        bean (ManagementFactory/getPlatformMXBean ThreadMXBean)
        thread-id (.getId (Thread/currentThread))
        tick! #(show/tick! rt (vswap! clock + 25))]
    (try
      (dotimes [_ warmup] (tick!))
      (let [samples (mapv (fn [_]
                            (let [wall (System/nanoTime) cpu (.getCurrentThreadCpuTime bean)
                                  allocated (.getThreadAllocatedBytes bean thread-id)]
                              (tick!)
                              [(/ (- (System/nanoTime) wall) 1e6) (/ (- (.getCurrentThreadCpuTime bean) cpu) 1e6)
                               (- (.getThreadAllocatedBytes bean thread-id) allocated)]))
                          (range frames))
            snapshot @(:state rt)
            usage (get-in snapshot [:output :lighting-budget :global])
            score (get-in snapshot [:output :lighting-activity :scene :score])]
        (assert (and (nil? (get-in snapshot [:status :error])) (number? score) (<= 0 score 1)))
        (assert (and (<= (:active-count usage) max-active) (pos? (:used-watts usage))
                     (<= (:used-watts usage) (+ 1e-7 (:limit-watts usage)))))
        {:pixels pixels :moving-heads movers :warmup warmup :frames frames
         :wall-ms (bench/distribution (map first samples))
         :owner-cpu-ms (bench/distribution (map second samples))
         :owner-allocated-bytes (bench/distribution (map peek samples))
         :over-25-ms (count (filter #(> (first %) 25.0) samples))
         :last-budget usage :last-activity score})
      (finally (show/stop! rt)))))

(defn -main [& args]
  (try
    (let [[pixels movers warmup frames] (if (seq args) (mapv parse-long args) [2048 64 60 120])]
      (prn (profile pixels movers warmup frames)))
    (finally (shutdown-agents))))
