(ns tolglow.bench-activity-opacity
  "Profile scoring a continuously changing alpha mix over a dense RGBW strip.
  Run lein with-profile +benchmark run -m tolglow.bench-activity-opacity
  [pixel-count warmup samples]. Each tick renders fresh output before timing
  the scorer so that the reported cost belongs to activity measurement."
  (:require [afterglow.render.frame :as frame]
            [tolglow.lighting-activity :as activity]
            [tolglow.lighting-capacity :as capacity])
  (:import [com.sun.management ThreadMXBean]
           [java.lang.management ManagementFactory]))

(defn- batch [id targets red blend]
  {:kind :batch :source {:effect-id id :effect-version 1}
   :batch {:target-ids targets :blend blend
           :outputs {:red {:type :number :components 1 :values (double-array (repeat (count targets) red))}}}})

(defn- distribution [samples]
  (let [ordered (vec (sort samples))
        n (count ordered)]
    {:median (nth ordered (quot n 2)) :p95 (nth ordered (int (* 0.95 n)))}))

(defn profile [pixels warmup samples]
  {:pre [(pos-int? pixels) (nat-int? warmup) (pos-int? samples)]}
  (let [fixtures (mapv (fn [i]
                         {:id (str "pixel-" i) :universe (inc (quot (* 4 i) 512)) :address (inc (mod (* 4 i) 512))
                          :groups [:strip] :channels {:red 1 :green 2 :blue 3 :white 4}}) (range pixels))
        patch (frame/compile-patch fixtures)
        meter (capacity/prepare patch)
        targets (:target-order patch)
        base (batch :background targets 0.2 :replace)
        wash (batch :wash targets 0.75 :replace)
        flash (batch :flash (subvec targets 0 (max 1 (quot pixels 2))) 0.2 :add)
        ^ThreadMXBean bean (ManagementFactory/getThreadMXBean)]
    (loop [i 0 prior nil cpu-ms [] allocated []]
      (if (= i (+ warmup samples))
        {:pixels pixels :warmup warmup :frames samples
         :owner-cpu-ms (distribution cpu-ms) :allocated-bytes (distribution allocated)}
        (let [phase (* 0.071 i)
              opacity (* 0.5 (+ 1.0 (Math/sin phase)))
              blocks [base (assoc-in wash [:batch :opacity] opacity)
                      (assoc-in flash [:batch :opacity] (* 0.5 (+ 1.0 (Math/cos (* 1.3 phase)))))]
              output (frame/resolve-contributions patch blocks {:include-values? false :compact-values? true})
              start (.getCurrentThreadCpuTime bean)
              bytes (.getCurrentThreadAllocatedBytes bean)
              next (activity/advance-meter prior patch meter output blocks (* i 25))
              elapsed (/ (- (.getCurrentThreadCpuTime bean) start) 1e6)
              used (- (.getCurrentThreadAllocatedBytes bean) bytes)]
          (assert (number? (get-in next [:report :effects :wash :score])))
          (recur (inc i) (:state next) (cond-> cpu-ms (>= i warmup) (conj elapsed))
                 (cond-> allocated (>= i warmup) (conj used))))))))

(defn -main [& args]
  (try (prn (apply profile (if (seq args) (mapv parse-long args) [2048 60 120])))
       (finally (shutdown-agents))))
