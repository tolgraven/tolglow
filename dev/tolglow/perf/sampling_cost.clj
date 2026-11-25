(ns tolglow.perf.sampling-cost
  "Measure worker-thread CPU per effect on the real compact show renderer."
  (:require [afterglow.control.model :as control]
            [afterglow.runtime.keyframes :as keyframes]
            [clojure.java.io :as io]
            [tolglow.bench-show :as bench]
            [tolglow.show :as show])
  (:import [java.lang.management ManagementFactory ThreadMXBean]))

(def ^:private frame-ms 25)

(defn- op [kind & args] {:op kind :args (vec args)})

(defn- expression [depth]
  (let [phase (op :add (op :multiply 0.15 {:op :position :axis :x}) {:op :beat})
        wave #(op :sin (op :add (op :multiply 1.25 %) 0.25))]
    (nth (iterate wave phase) depth)))

(defn- build [fixtures pixels stride depth]
  (let [e (expression depth)
        light #(op :add 0.5 (op :multiply 0.45 %))
        programs [{:id :wash
                   :outputs {:color (op :vec3 (light (op :sin e)) (light (op :cos e))
                                        (light (op :sin (op :multiply 0.3 e))))}}
                  {:id :steer :targets (mapv :id (subvec fixtures pixels))
                   :outputs {:aim (op :vec3 e (op :multiply 1.2 e) 3.0)}}]]
    (reduce (fn [model {:keys [id] :as program}]
              (let [definition (cond-> (assoc program :version 1)
                                 (> stride 1) (assoc :keyframes {:interval {:ms (* frame-ms stride)}}))]
                (-> model
                    (bench/command [id :publish] {:op :registry/publish :kind :effects :definition definition})
                    (bench/command [id :launch] {:op :effect/launch :effect-id id}))))
            (control/new-show {:rig {:fixtures fixtures}}) programs)))

(defn- measure-worker [evaluate ^ThreadMXBean bean rows stride step context]
  ;; Observe the same worker before and after the complete effect operation.
  ;; The existing sampler callback identifies frames that constructed endpoints.
  (let [calls (volatile! 0)
        original (get-in context [:frame :measure-effect])
        count! (fn [id eligible? calculate]
                 (vswap! calls inc)
                 (original id eligible? calculate))
        input (assoc-in context [:frame :measure-effect] count!)
        start (.getCurrentThreadCpuTime bean)
        result (evaluate step input)
        elapsed (/ (- (.getCurrentThreadCpuTime bean) start) 1e6)
        stage (cond (= stride 1) :direct (pos? @calls) :endpoint :else :interpolate)]
    (swap! rows update-in [(get-in context [:instance :effect-id]) stage] (fnil conj []) elapsed)
    result))

(defn- summarize [groups frames stride]
  (let [samples (mapcat val groups)
        endpoints (count (:endpoint groups))]
    (when-not (and (= frames (count samples))
                   (if (= stride 1)
                     (= frames (count (:direct groups)))
                     (and (= (quot frames stride) endpoints)
                          (= (- frames endpoints) (count (:interpolate groups))))))
      (throw (ex-info "Missing worker measurements or unexpected keyframe cadence"
                      {:frames frames :stride stride :counts (update-vals groups count)})))
    (into {:combined (bench/distribution samples)}
          (map (fn [[stage xs]] [stage (assoc (bench/distribution xs) :n (count xs))])) groups)))

(defn- replay [bean rig pixels depth stride warmup frames]
  (let [runtime (show/create-runtime
                  {:model (build rig pixels stride depth) :workers 2 :batch? true
                   :adaptive-keyframes {:enabled? true :max-stride 1}
                   :workspace-options nil :worker-timeout-ms 10000})
        rows (atom {})
        tick! (fn [i]
                (show/tick! runtime (* i frame-ms))
                (let [{:keys [status fault-events]} @(:state runtime)]
                  (when (or (:error status) (seq fault-events))
                    (throw (ex-info "Worker replay fault" {:frame i :status status :faults fault-events})))))]
    (try
      (dotimes [i warmup] (tick! i))
      ;; This standalone replay owns the JVM while installing the worker probe.
      (with-redefs [keyframes/evaluate (partial measure-worker keyframes/evaluate bean rows stride)]
        (doseq [i (range warmup (+ warmup frames))] (tick! i)))
      (when-not (= #{:wash :steer} (set (keys @rows)))
        (throw (ex-info "Expected work from both the wash and moving-head paths" {:effects (keys @rows)})))
      (update-vals @rows #(summarize % frames stride))
      (finally (show/stop! runtime)))))

(defn measure []
  (let [bean (ManagementFactory/getThreadMXBean)
        pixels 1024 movers 64 warmup 40 frames 100
        rig (:fixtures (bench/corpus pixels movers :sculpture))]
    (when-not (and (.isCurrentThreadCpuTimeSupported bean) (.isThreadCpuTimeEnabled bean))
      (throw (ex-info "This replay requires enabled worker-thread CPU accounting" {})))
    {:pixels pixels :movers movers :warmup warmup :frames frames :frame-ms frame-ms
     :runs (vec (for [depth [1 8] stride [1 4 4 1]]
                  {:depth depth :stride stride :effects (replay bean rig pixels depth stride warmup frames)}))}))

(defn -main [output]
  (try
    (let [report (measure)]
      (io/make-parents output)
      (spit output (str (pr-str report) "\n"))
      (prn report))
    (finally (shutdown-agents))))
