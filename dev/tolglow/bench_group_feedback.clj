(ns tolglow.bench-group-feedback
  "Alternate two warmed preview owners running the same generated light program."
  (:require [tolglow.demo :as demo]
            [tolglow.lighting-scenes :as scenes]
            [tolglow.show :as show])
  (:import [com.sun.management ThreadMXBean]
           [java.lang.management ManagementFactory]
           [java.util Arrays]))

(defn- strip [size]
  (mapv (fn [i]
          (let [slot (* 3 i)]
            {:id (keyword (str "px-" i)) :groups [:strip]
             :universe (inc (quot slot 512)) :address (inc (mod slot 512))
             :channels {:red 1 :green 2 :blue 3}}))
        (range size)))

(defn- measure [^ThreadMXBean bean tid f]
  (let [start (System/nanoTime)
        cpu (.getCurrentThreadCpuTime bean)
        allocated (.getThreadAllocatedBytes bean (long tid))]
    (f)
    [(/ (- (System/nanoTime) start) 1e6)
     (/ (- (.getCurrentThreadCpuTime bean) cpu) 1e6)
     (- (.getThreadAllocatedBytes bean (long tid)) allocated)]))

(defn- summarize [rows]
  (into {}
        (map-indexed
         (fn [column key]
           (let [values (vec (sort (map #(nth % column) rows)))]
             [key {:p50 (values (quot (count values) 2))
                   :p95 (values (int (* 0.95 (count values))))}]))
         [:wall-ms :owner-cpu-ms :owner-allocated-bytes])))

(defn run
  ([size samples] (run size samples :local))
  ([size samples brightness-reference]
  (let [model (assoc-in (demo/model) [:rig :fixtures] (strip size))
        live (show/create-runtime {:model model})
        time (volatile! 0)
        tick #(show/tick! % (vswap! time + 25))
        bean (ManagementFactory/getPlatformMXBean ThreadMXBean)
        tid (.getId (Thread/currentThread))]
    (try
      (dotimes [_ 12] (tick live))
      (let [reply (show/submit! live :lighting-scene {:group-id :strip :op :apply :target 0.4 :brightness-reference brightness-reference :effect-ids []})
            activation (measure bean tid #(tick live))]
        (assert (= :accepted (:status @reply)))
        (dotimes [_ 42] (tick live))
        (let [settled (:model @(:state live))
              base (show/create-runtime {:model (update settled :extensions dissoc :tolglow.lighting-scenes/control)})
              owners [[:feedback live] [:same-look base]]]
          (try
            (dotimes [_ 42] (tick live) (tick base))
            (let [rows (reduce (fn [result i]
                                 (reduce (fn [m [name rt]]
                                           (update m name conj (measure bean tid #(tick rt))))
                                         result (if (even? i) owners (rseq owners))))
                               {:feedback [] :same-look []} (range samples))
                  baseline (get-in @(:state base) [:output :universes])]
              (assert (every? (fn [[u buffer]] (Arrays/equals ^bytes buffer ^bytes (baseline u)))
                              (get-in @(:state live) [:output :universes])))
              {:heads size :samples-per-owner samples
               :target 0.4 :brightness-reference brightness-reference :actual (get-in (scenes/groups settled) [:strip :actual])
               :first-scene (zipmap [:wall-ms :owner-cpu-ms :owner-allocated-bytes] activation)
               :steady (update-vals rows summarize)})
            (finally (show/stop! base)))))
      (finally (show/stop! live))))))

(defn -main [& [size samples reference]]
  (prn (run (Long/parseLong (or size "2048")) (Long/parseLong (or samples "60")) (keyword (or reference "local"))))
  (shutdown-agents))
