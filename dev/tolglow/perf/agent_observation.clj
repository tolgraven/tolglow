(ns tolglow.perf.agent-observation
  "Cost and size of the agent observation, tolglow-agent-obs/1 (#849).

  Two shows through the real owner (`show/tick!`, capture sink, no devices):
  the mixed performance rig (1,000 RGBW pixels, 48 dimmers, 48 pars, 16
  moving heads, 16 cue layers with beat LFOs) and the R1o red show on the
  2023 gig rig under auto. After each owner tick, on this reader thread,
  the harness times `show/agent-observation` for every profile as a delta
  against the previous token and as a fresh snapshot, and `show/snapshot`
  (the Studio projection) for comparison. Ticks are outside the timer.

  Token counts are estimates (UTF-8 bytes / 3), not tokenizer measurements.

      $W/bin/repl run tolglow.perf.agent-observation [samples] [output.edn]"
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [tolglow.agent-observation :as ao]
            [tolglow.automation.red-show :as red-show]
            [tolglow.perf.mixed-rig :as mixed]
            [tolglow.show :as show])
  (:import [java.lang.management ManagementFactory]))

(defn- percentile [xs p]
  (let [v (vec (sort xs))]
    (when (seq v) (v (min (dec (count v)) (int (Math/floor (* p (count v)))))))))

(defn- stats [xs]
  {:n (count xs) :mean (when (seq xs) (/ (reduce + xs) (double (count xs))))
   :p50 (percentile xs 0.5) :p95 (percentile xs 0.95) :max (when (seq xs) (reduce max xs))})

(defn- timed [f]
  (let [start (System/nanoTime) value (f)]
    [(/ (- (System/nanoTime) start) 1.0e6) value]))

(defn- size [envelope]
  (let [text (ao/encode envelope)] {:bytes (ao/utf8-size text) :tokens (ao/estimate-tokens text)}))

(defn- sample-show
  "Warm up, then per frame: tick (untimed), then time each read."
  [rt start-frame warmup samples]
  (doseq [i (range start-frame (+ start-frame warmup))] (show/tick! rt (* i 25.0)))
  (let [tokens (atom (into {} (for [p [:tick :compact :full]] [p (get (show/agent-observation rt {:profile p :session (name p)}) "token")])))
        rows (vec (for [i (range (+ start-frame warmup) (+ start-frame warmup samples))]
                    (do (show/tick! rt (* i 25.0))
                        (into {}
                              (concat
                               (for [p [:tick :compact :full]]
                                 (let [[ms env] (timed #(show/agent-observation rt {:profile p :session (name p) :base (get @tokens p)}))]
                                   (swap! tokens assoc p (get env "token"))
                                   [[p :delta] (assoc (size env) :ms ms :kind (get env "kind") :changes (count (get env "changes")))]))
                               (for [p [:tick :compact :full]]
                                 (let [[ms env] (timed #(show/agent-observation rt {:profile p}))]
                                   [[p :snapshot] (assoc (size env) :ms ms)]))
                               [[[:studio :snapshot] {:ms (first (timed #(show/snapshot rt)))}]])))))]
    (into (sorted-map)
          (for [k (keys (first rows))]
            [k (cond-> {:ms (stats (map #(get-in % [k :ms]) rows))}
                 (:bytes (get (first rows) k))
                 (assoc :bytes (stats (map #(get-in % [k :bytes]) rows))
                        :tokens (stats (map #(get-in % [k :tokens]) rows)))
                 (:changes (get (first rows) k))
                 (assoc :changes (stats (map #(get-in % [k :changes]) rows))
                        :kinds (frequencies (map #(get-in % [k :kind]) rows))))]))))

(defn- mixed-rig [samples]
  (let [opts {:dimmers 48 :pars 48 :movers 16 :layers 16}
        {:keys [model counts]} (mixed/corpus opts 1000)
        rt (show/create-runtime {:model model :workspace-options nil :workers 2 :sink {:send! (fn [_ _])}})]
    (try
      (show/tick! rt 0.0)
      {:show :mixed-rig :fixtures counts :instances (count (get-in @(:state rt) [:model :instances]))
       :results (sample-show rt 1 200 samples)}
      (finally (show/stop! rt)))))

(defn- red-show [samples]
  (let [rt (show/create-runtime {:model (red-show/model) :workspace-options nil :sink {:send! (fn [_ _])}})]
    (try
      (show/tick! rt 0.0)
      (let [reply (show/submit! rt :auto (red-show/auto-config 217))]
        (show/tick! rt 25.0)
        (deref reply 2000 nil))
      {:show :red-show-gig-2023 :results (sample-show rt 2 200 samples)}
      (finally (show/stop! rt)))))

(defn- rig-cost
  "Cold cost of the rig block on a 2,048-fixture rig: `rig-section` alone, the
  first observation per profile on a fresh store (the unpaged rig is built
  once and shared), and two `inspect` pages."
  []
  (let [fixtures (mapv (fn [i] {:id (format "f-%04d" i) :position [(double (mod i 40)) (* 0.1 (mod i 7)) (double (quot i 40))]
                                :channels {:red 1 :green 2 :blue 3}})
                       (range 2048))
        model (assoc (red-show/model) :rig {:fixtures fixtures})
        input {:runtime-id "r" :frame-rate 40 :state {:model model :output {}}}
        section-ms (vec (for [_ (range 5)] (first (timed #(ao/rig-section (:rig model) {})))))
        store (ao/create-store)
        first-ms (into {} (for [p [:compact :full :tick]] [p (first (timed #(ao/observe store input {:profile p})))]))
        page-ms (vec (for [o [0 10]] (first (timed #(ao/inspect store input ["rig" "fixtures"] {:offset o :limit 10})))))]
    {:fixtures 2048 :rig-section-ms section-ms :first-observation-ms first-ms :inspect-ms page-ms}))

(defn -main [& [samples output]]
  (let [samples (if samples (parse-long samples) 400)
        report {:benchmark :agent-observation :version 1 :measured-at (str (java.time.Instant/now))
                :java (System/getProperty "java.version")
                :processors (.availableProcessors (Runtime/getRuntime))
                :load-average (.getSystemLoadAverage (ManagementFactory/getOperatingSystemMXBean))
                :caps ao/caps
                :notes ["Times are one show/agent-observation call on the reader thread after an untimed owner tick (25 ms show frames)."
                        "Delta rows are against the previous frame's token; snapshot rows have no base."
                        "Tokens are UTF-8 bytes / 3, the research estimate; not a tokenizer count."]
                :rig-cost (rig-cost)
                :runs [(mixed-rig samples) (red-show samples)]}]
    (if output
      (do (io/make-parents output)
          (with-open [w (io/writer output)] (binding [*out* w pprint/*print-right-margin* 160] (pprint/pprint report))))
      (binding [pprint/*print-right-margin* 160] (pprint/pprint report)))))
