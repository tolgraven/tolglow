(ns tolglow.perf.adaptive-keyframes
  "Compare per-frame and adaptive samples on two spatial fields and a one-head control."
  (:require [afterglow.control.model :as control]
            [clojure.java.io :as io]
            [tolglow.bench-show :as bench]
            [tolglow.effects.cadence :as cadence]
            [tolglow.show :as show]))

(def ^:private policy {:enabled? true :headroom 0.4 :min-cost-ms 0.5 :pressure-frames 3})

(defn- spatial-wave [axis]
  (let [position {:op :position :axis axis}
        wave (reduce (fn [node op] {:op op :args [node]}) position
                     [:sin :cos :sin :cos :sin :cos :sin :cos])
        field {:op :multiply :args [0.25 wave]}
        musical {:op :multiply :args [0.004 {:op :beat}]}]
    {:op :add :args [0.4 field musical]}))

(defn- install-effect [model {:keys [id] :as effect}]
  (-> model
      (bench/command [id :definition] {:op :registry/publish :kind :effects :definition (assoc effect :version 1)})
      (bench/command [id :launch] {:op :effect/launch :effect-id id})))

(defn- scene [fixtures]
  (reduce install-effect (control/new-show {:show-id :profile :rig {:fixtures fixtures}})
          [{:id :spatial-red :outputs {:red (spatial-wave :x)}}
           {:id :spatial-blue :outputs {:blue (spatial-wave :z)}}
           {:id :single-green :targets [(:id (first fixtures))] :outputs {:green 0.23}}]))

(defn- measure-frame [runtime sink preview-indices reference index]
  (vreset! (:latest sink) [])
  (let [start (System/nanoTime)
        frame (bench/host-step! runtime sink preview-indices index)
        elapsed (/ (- (System/nanoTime) start) 1.0e6)]
    (when (empty? (:packets frame))
      (throw (ex-info "Missing ArtDMX capture" {:index index})))
    (when reference
      (try
        (bench/check! (nth reference index) frame)
        (catch Exception cause
          (throw (ex-info "Adaptive show disagreed with per-frame evaluation"
                          {:index index :cadence (cadence/status runtime)} cause)))))
    [(when-not reference (dissoc frame :stages-ms)) elapsed]))

(defn- effect-counters [instances initial {:keys [instance-id] :as row}]
  [(get-in instances [instance-id :effect-id])
   (-> row
       (dissoc :instance-id)
       (update :evaluations - (get-in initial [instance-id :evaluations] 0))
       (update :sampled-frames - (get-in initial [instance-id :sampled-frames] 0)))])

(defn- capture [model stride warmup samples reference]
  (let [sink (bench/capture-sink)
        runtime (show/create-runtime {:model model :sink sink :workspace-options nil :workers 2
                                      :adaptive-keyframes (assoc policy :max-stride stride)})
        preview-indices (range (min 3 (count (get-in model [:rig :fixtures]))))
        step (partial measure-frame runtime sink preview-indices reference)]
    (try
      (let [warmed (mapv step (range warmup))
            initial (into {} (map (juxt :instance-id identity)) (:effects (cadence/status runtime)))
            rows (mapv step (range warmup (+ warmup samples)))
            ms (mapv second rows)
            instances (get-in @(:state runtime) [:model :instances])
            costs (into {} (map (partial effect-counters instances initial)) (:effects (cadence/status runtime)))]
        {:frames (when-not reference (into (mapv first warmed) (map first) rows))
         :measurements {:max-stride stride :instances costs :elapsed-ms (bench/distribution ms)
                        :over-25ms (count (filter #(< 25.0 %) ms))}})
      (finally (show/stop! runtime)))))

(defn- measurements [pixels warmup samples]
  (let [model (scene (:fixtures (bench/corpus pixels 0 :sculpture)))
        full (capture model 1 warmup samples nil)
        adaptive (capture model 8 warmup samples (:frames full))]
    {:pixels pixels :warmup warmup :samples samples :headroom (:headroom policy)
     :runs [(:measurements full) (:measurements adaptive)]}))

(defn -main [& args]
  (let [args (or (seq args) ["4000" "24" "80"])
        [pixels warmup samples :as counts] (mapv parse-long (take 3 args))
        output (nth args 3 nil)]
    (when-not (and (<= 3 (count args) 4) (every? pos-int? counts))
      (throw (ex-info "Expected positive pixel, warmup and frame counts followed by an optional EDN file" {:args args})))
    (try
      (let [report (measurements pixels warmup samples)]
        (if output
          (do (io/make-parents output) (spit output (str (pr-str report) "\n")))
          (prn report)))
      (finally (shutdown-agents)))))
