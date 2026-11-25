(ns tolglow.perf.compound-layers
  "Compare actual two-worker show frames for the same rig and 24 animated cues."
  (:require [afterglow.control.model :as control]
            [tolglow.bench-movers :as movers]
            [tolglow.bench-movers-reference :as reference]
            [tolglow.bench-show :as bench]
            [tolglow.composition :as composition]
            [tolglow.show :as show])
  (:import [java.util Arrays]))

(def ^:private lanes [:individual :opaque-compound :transformed-compound])

(def ^:private transforms
  [{:attribute :aim :value {:op :add :args [{:op :field :attribute :aim} [0.35 0.15 -0.75]]}}
   {:attribute :color :value {:op :rotate-hue :args [{:op :field :attribute :color} 1/6]}}])

(defn- launch [model effect]
  (let [{:keys [id]} effect
        {:keys [model ack]} (control/apply-command model {:op :cue/launch :cue-id id :command-id id
                                                         :source :benchmark :time-ms 0
                                                         :show-id (:show-id model) :epoch (:epoch model)})]
    (when-not (= :accepted (:status ack))
      (throw (ex-info "Benchmark cue launch failed" {:cue-id id :ack ack})))
    model))

(defn- make-show [fixtures effects lane]
  (let [initial (reduce (fn [model {:keys [id priority] :as effect}]
                          (-> model
                              (assoc-in [:registry :effects id] effect)
                              (assoc-in [:registry :cues id] {:id id :effect-id id :priority priority :version 1})))
                        (control/new-show {:show-id "compound-profile" :rig {:fixtures fixtures}
                                           :transport {:beat 0 :bpm 120 :playing? true}})
                        effects)]
    (if (= lane :individual)
      (reduce launch initial effects)
      (let [transform? (= lane :transformed-compound)
            slots (mapv (fn [{:keys [id priority]}]
                          (cond-> {:cue-id id}
                            (and transform? (= 20 priority)) (assoc :launch {:system {:opacity 0.7}} :transforms transforms)))
                        (drop 8 effects))]
        (-> (reduce launch initial (take 8 effects))
            (composition/save {:id :compound :mode :macro :slots slots :opacity (if transform? 0.65 1)})
            (composition/start :compound 0))))))

(defn- packets-equal? [a b]
  (and (seq a) (= (count a) (count b))
       (every? true? (map #(Arrays/equals ^bytes %1 ^bytes %2) a b))))

(defn- measure-frame [runtime i allocated]
  (let [bytes-before (when allocated (allocated))
        start (System/nanoTime)
        _ (show/tick! runtime (* 25.0 (inc i)))
        elapsed (/ (- (System/nanoTime) start) 1e6)
        bytes (when allocated (- (allocated) bytes-before))
        {:keys [model output status]} @(:state runtime)]
    (when (or (:error status) (:output-error status) (not= 24 (count (:instances model)))
              (some :fault (vals (:instances model))) (empty? (:universes output)))
      (throw (ex-info "Compound benchmark frame failed" {:frame i :status status})))
    {:ms elapsed :bytes bytes :packets (deref (get-in runtime [:sink :latest]))}))

(defn- sample [runtimes i allocated]
  (let [order (take (count lanes) (drop (mod i (count lanes)) (cycle lanes)))
        frames (into {} (map (fn [lane] [lane (measure-frame (runtimes lane) i allocated)]) order))
        individual (get-in frames [:individual :packets])]
    (when-not (and (packets-equal? individual (get-in frames [:opaque-compound :packets]))
                   (not (packets-equal? individual (get-in frames [:transformed-compound :packets]))))
      (throw (ex-info "Compound did not produce the expected DMX frame" {:frame i})))
    (update-vals frames #(dissoc % :packets))))

(defn- summarize [rows]
  (cond-> {:ms (bench/distribution (map :ms rows))
           :over-25-ms (count (filter #(> (:ms %) 25.0) rows))}
    (:bytes (first rows)) (assoc :bytes (bench/distribution (map :bytes rows)))))

(defn profile [warmup samples]
  (let [fixtures (reference/fixtures 64)
        effects (reference/definitions fixtures)
        runtimes (volatile! {})]
    (try
      (doseq [lane lanes]
        (let [runtime (show/create-runtime {:model (make-show fixtures effects lane) :workers 2 :batch? true
                                            :sink (bench/capture-sink)})]
          (vswap! runtimes assoc lane runtime)
          (run! (:compile runtime) effects)))
      (dotimes [i warmup] (sample @runtimes i nil))
      (let [allocated (movers/allocation-reader)
            rows (mapv #(sample @runtimes (+ warmup %) allocated) (range samples))]
        (into {} (map (fn [lane] [lane (summarize (map lane rows))]) lanes)))
      (finally (run! show/stop! (vals @runtimes))))))

(defn -main [& args]
  (let [[warmup samples] (if (empty? args) [80 200] (mapv parse-long args))]
    (when-not (and (#{0 2} (count args)) (pos-int? warmup) (pos-int? samples))
      (throw (ex-info "Use optional positive warmup and sample counts" {:args args})))
    (try
      (prn {:tolglow (bench/revision ".") :afterglow (bench/revision (or (System/getenv "AFTERGLOW_ROOT") "../afterglow"))
            :java (System/getProperty "java.version") :cpu (bench/cpu-model) :max-heap (.maxMemory (Runtime/getRuntime))
            :heads 64 :effects 24 :workers 2 :warmup warmup :samples samples :results (profile warmup samples)})
      (finally (shutdown-agents)))))
