(ns tolglow.perf.admission-cost
  "Compare the release governor's raw sample ranking with measured whole-worker CPU savings."
  (:require [afterglow.control.model :as control]
            [afterglow.runtime.cadence :as governor]
            [afterglow.runtime.keyframes :as keyframes]
            [clojure.java.io :as io]
            [tolglow.bench-show :as bench]
            [tolglow.effects.cadence :as cadence]
            [tolglow.perf.multirate-quality :as quality]
            [tolglow.show :as show])
  (:import [java.lang.management ManagementFactory ThreadMXBean]))

(def ^:private candidates [:wash :steer])
(defn- op [kind & args] {:op kind :args (vec args)})

(defn- scene [fixtures pixels]
  (let [beat (op :multiply 0.008 {:op :beat})
        ;; Time is affine. Changing the interval changes cost without intentionally
        ;; changing the scene. These three spatial fields exercise real per-head work.
        field (fn [axis]
                (let [nest #(op :sin (op :add 1.2 %))
                      wave (nth (iterate nest {:op :position :axis axis}) 8)]
                  (op :add 0.35 (op :multiply 0.25 wave) beat)))
        presets (quality/programs fixtures pixels)
        wash {:id :wash :targets (mapv :id (subvec fixtures 1 pixels))
              :outputs {:color (op :vec3 (field :x) (field :y) (field :z))}}
        steer {:id :steer :targets (mapv :id (subvec fixtures pixels))
               :outputs {:aim (op :vec3 (op :add 1.3 beat) 0.75 (op :add 3.25 beat))}}
        install (fn [m {:keys [id] :as definition}]
                  (let [m (-> m
                              (bench/command [id :publish] {:op :registry/publish :kind :effects
                                                            :definition (assoc definition :version 1)})
                              (bench/command [id :cue] {:op :cue/put :cue {:id id :effect-id id}}))]
                    (if (= id :shutter) m (bench/command m [id :start] {:op :cue/launch :cue-id id}))))]
    (reduce install (control/new-show {:show-id :admission :rig {:fixtures fixtures}})
            (into [wash steer] (filter #(#{:shutter :pulse} (:id %)) presets)))))

(defn- force-intervals [begin rates runtime before model]
  (let [intervals (into {} (keep (fn [[id {:keys [effect-id]}]]
                                   (when-let [stride (rates effect-id)]
                                     (when (> stride 1) [id {:ms (* quality/period-ms stride)}]))))
                        (:instances model))]
    (assoc-in (begin runtime before model) [:inputs :keyframe-intervals] intervals)))

(defn- timed-sample [^ThreadMXBean bean readings observer id eligible? calculate]
  (letfn [(timed []
            (let [start (.getCurrentThreadCpuTime bean) t (System/nanoTime)
                  result (calculate)
                  wall-ns (- (System/nanoTime) t)
                  cpu-ns (- (.getCurrentThreadCpuTime bean) start)]
              (vswap! readings (fn [[calls cpu wall]] [(inc calls) (+ cpu cpu-ns) (+ wall wall-ns)]))
              result))]
    (observer id eligible? timed)))

(defn- worker-sample [evaluate ^ThreadMXBean bean samples rates step context]
  (let [effect (get-in context [:instance :effect-id])]
    (if-not (contains? rates effect)
      (evaluate step context)
      (let [observer (get-in context [:frame :measure-effect])
            readings (volatile! [0 0 0])
            ctx (assoc-in context [:frame :measure-effect] (partial timed-sample bean readings observer))
            start (.getCurrentThreadCpuTime bean)
            result (evaluate step ctx)
            entire (/ (- (.getCurrentThreadCpuTime bean) start) 1e6)
            [calls cpu wall] @readings
            stage (cond (= 1 (rates effect)) :direct (pos? calls) :endpoint :else :interpolate)]
        (swap! samples update effect (fnil conj [])
               {:stage stage :worker-ms entire :sample-cpu-ms (/ cpu 1e6) :sample-wall-ms (/ wall 1e6)})
        result))))

(defn- summarize [rows stride frames]
  (let [groups (group-by :stage rows)
        count-at #(count (groups %))
        expected (if (= stride 1) (= frames (count-at :direct))
                     (and (= (quot frames stride) (count-at :endpoint))
                          (= (- frames (quot frames stride)) (count-at :interpolate))))
        measurements (fn [xs]
                       (into {:n (count xs)}
                             (map (fn [k] [k (bench/distribution (map k xs))]))
                             [:worker-ms :sample-cpu-ms :sample-wall-ms]))]
    (when-not expected
      (throw (ex-info "Unexpected worker cadence" {:stride stride :counts (update-vals groups count)})))
    (assoc (update-vals groups measurements) :combined (measurements rows))))

(defn- check-frame! [fixtures pixels expected frame]
  (let [d (quality/compare-frame fixtures pixels expected frame)]
    (when (or (pos? (:dmx-max d)) (> (:rgb-max d) 1e-9) (> (:aim-max-m d) 1e-9)
              (some d [:shutter-error? :missed-pulse? :false-pulse? :lifecycle-error?]))
      (throw (ex-info "Candidate changed show output" d)))))

(defn- tick! [runtime sink i]
  (let [shot (when (zero? (mod i 17))
               (show/submit! runtime {:command-id (str "shot-" i) :op :cue/launch :cue-id :shutter
                                      :launch {:mode :one-shot :duration {:beats 1/4}}}))]
    (vreset! (:latest sink) [])
    (show/tick! runtime (* i quality/period-ms))
    (when (or (empty? @(:latest sink)) (and shot (not= :accepted (:status @shot)))
              (seq (:fault-events @(:state runtime))))
      (throw (ex-info "Missing output, shot or failed worker" {:frame i})))))

(defn- capture [runtime sink fixtures pixels warmup frames oracle]
  (letfn [(read-at [j]
            (tick! runtime sink (+ warmup j))
            (let [frame (quality/read-frame @(:state runtime) fixtures pixels)]
              (if oracle (do (check-frame! fixtures pixels (nth oracle j) frame) nil) frame)))]
    (into [] (keep read-at) (range frames))))

(defn- replay [bean {:keys [model fixtures pixels]} rates warmup frames oracle]
  (let [sink (bench/capture-sink)
        runtime (show/create-runtime {:model model :sink sink :workspace-options nil :workers 2 :batch? true
                                      :worker-timeout-ms 10000 :adaptive-keyframes {:enabled? true :max-stride 1}})
        rows (atom {})]
    (try
      (with-redefs [cadence/begin (partial force-intervals cadence/begin rates)]
        (dotimes [i warmup] (tick! runtime sink i))
        (with-redefs [keyframes/evaluate (partial worker-sample keyframes/evaluate bean rows rates)]
          (let [observed (capture runtime sink fixtures pixels warmup frames oracle)]
            {:frames observed
             :statistics (into {} (map (fn [id] [id (summarize (@rows id) (rates id) frames)])) candidates)})))
      (finally (show/stop! runtime)))))

(defn- decision [full slow eligible floor-ms]
  (let [ids (zipmap eligible (repeat 1))
        raw (into {} (map (fn [id] [id (get-in full [id :combined :sample-wall-ms :mean])])) eligible)
        sample (update-vals raw #(hash-map :calls 1 :total-ms % :predictive? true))
        pressure {:epoch 1 :versions ids :samples sample :period-ms quality/period-ms :elapsed-ms 32.0}
        opts (governor/settings {:min-cost-ms floor-ms :pressure-frames 2 :max-stride 2})
        policy (nth (iterate #(governor/advance % pressure opts) nil) 3)
        saving (fn [id] (- (get-in full [id :combined :worker-ms :mean])
                           (get-in slow [id :combined :worker-ms :mean])))
        gain (into {} (map (juxt identity saving)) eligible)
        best (first (sort-by (juxt (comp - gain) name)
                             (filter #(and (>= (raw %) floor-ms) (pos? (gain %))) eligible)))]
    {:release (ffirst (:keyframe-intervals policy)) :net best :calibration-saving-ms gain :floor-ms floor-ms}))

(defn- paired-decisions [calibration eligible threshold]
  (mapv (fn [pair]
          (let [{:keys [full both]} (into {} pair)]
            (decision full both eligible threshold)))
        (partition 2 calibration)))

(defn- held-out-savings [runs]
  (mapv (fn [trio]
          (let [{:keys [full] :as data} (into {} trio)]
            (into {} (map (fn [id]
                            [id (- (get-in full [id :combined :worker-ms :mean])
                                   (get-in data [id id :combined :worker-ms :mean]))]) candidates))))
        (partition 3 runs)))

(defn measure [pixels movers warmup frames]
  (let [bean (ManagementFactory/getThreadMXBean)
        _ (when-not (and (.isCurrentThreadCpuTimeSupported bean) (.isThreadCpuTimeEnabled bean))
            (throw (ex-info "Thread CPU measurement is unavailable" {})))
        rig (:fixtures (bench/corpus pixels movers :sculpture))
        fixtures (into (subvec rig 0 pixels) (map #(assoc-in % [:channels :shutter] 12)) (subvec rig pixels))
        show {:fixtures fixtures :pixels pixels :model (scene fixtures pixels)}
        schedules {:full {:wash 1 :steer 1} :both {:wash 2 :steer 2}
                   :wash {:wash 2 :steer 1} :steer {:wash 1 :steer 2}}
        run (fn [id reference] (replay bean show (schedules id) warmup frames reference))
        {oracle :frames} (run :full nil)
        pulse (mapv :pulse oracle) shutter (mapv :shutter oracle)
        _ (when-not (and (seq (filter #(> % 0.5) pulse)) (> (count (partition-by identity shutter)) 1))
            (throw (ex-info "Measured interval needs a lit pulse and a shutter edge" {})))
        observe (fn [id] [id (:statistics (run id oracle))])
        ;; Counterbalance calibration order. Choose before executing held-out runs.
        calibration (mapv observe [:full :both :both :full])
        decisions (into {} (for [[name ids] {:mixed candidates :aim-only [:steer]}]
                             [name {:release-default (paired-decisions calibration ids (:min-cost-ms governor/defaults))
                                    :low-threshold (paired-decisions calibration ids 0.0)}]))
        holdout (mapv observe [:full :wash :steer :steer :wash :full])]
    {:pixels pixels :movers movers :warmup warmup :frames frames :calibration calibration :decisions decisions
     :holdout holdout :held-out-saving-ms (held-out-savings holdout)
     :pulse pulse :shutter shutter :bpm 120 :frame-period-ms quality/period-ms}))

(defn -main [file & args]
  (let [[pixels movers warmup frames] (if (seq args) (mapv parse-long args) [1024 128 48 80])]
    (when-not (and file (or (empty? args) (= 4 (count args))) (every? pos-int? [pixels movers warmup frames])
                   (> pixels 1) (even? frames) (>= frames 20))
      (throw (ex-info "Supply FILE [pixels movers warmup even-frames>=20]" {})))
    (try
      (let [report (measure pixels movers warmup frames)]
        (io/make-parents file)
        (spit file (pr-str report))
        (prn (:decisions report)))
      (finally (shutdown-agents)))))
