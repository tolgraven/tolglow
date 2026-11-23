(ns tolglow.perf.stride-break-even
  "Measure complete predictive worker costs under a maximum permitted interval."
  (:require [clojure.java.io :as io]
            [tolglow.perf.admission-cost :as admission])
  (:import [java.lang.management ManagementFactory]))

(def strides [1 2 4 8])
(defn- op [name & args] {:op name :args (vec args)})

(defn- spatial-target [depth]
  (let [beat (op :multiply 0.008 {:op :beat})
        field (fn [axis offset]
                (let [nest #(op :sin (op :add 1.2 %))
                      wave (nth (iterate nest {:op :position :axis axis}) depth)]
                  (op :add offset (op :multiply 0.4 wave) beat)))]
    (op :vec3 (field :x 1.3) (field :y 0.75) (field :z 3.25))))

(defn estimate [direct endpoint blend stride]
  (if (= 1 stride) direct
      (/ (+ endpoint (* (dec stride) blend)) stride)))

(defn first-saving-stride
  "Return the finest allowed interval whose estimated cost is strictly below
  full rate. Keep full rate if every candidate inside the cap loses."
  [costs cap]
  (or (first (filter #(and (<= % cap) (< (costs %) (costs 1))) (rest strides))) 1))

(defn- predicted [direct stride-two statistic]
  (let [cost #(get-in %1 [%2 :worker-ms statistic])
        value #(estimate (cost direct :direct) (cost stride-two :endpoint)
                         (cost stride-two :interpolate) %)]
    (into (sorted-map) (map (juxt identity value)) strides)))

(defn- measured [rows statistic]
  (into (sorted-map)
        (map (fn [[stride stats]]
               [stride (if (= 1 stride) (get-in stats [:direct :worker-ms statistic])
                           (estimate nil (get-in stats [:endpoint :worker-ms statistic])
                                     (get-in stats [:interpolate :worker-ms statistic]) stride))]))
        rows))

(defn- choices [costs]
  (into (sorted-map) (map (fn [cap] [cap (first-saving-stride costs cap)])) strides))

(defn- calibrated [runs]
  (mapv (fn [pair]
          (let [lanes (into {} pair)]
            (into {} (for [statistic [:p50 :mean]
                           :let [costs (predicted (lanes 1) (lanes 2) statistic)]]
                       [statistic {:cost-ms costs :by-cap (choices costs)}]))))
        (partition 2 runs)))

(defn- held-out [runs]
  (mapv (fn [round]
          (into {} (for [statistic [:p50 :mean] :let [costs (measured round statistic)]]
                     [statistic {:cost-ms costs :by-cap (choices costs)}])))
        (partition 4 runs)))

(defn compare-decisions
  "Compare training choices with an independent run, including a rule that
  would retain full rate as soon as the first stride-two trial fails."
  [trained observed]
  (let [actual (:cost-ms observed)
        choice (fn [cap]
                 (let [from-model (get-in trained [:by-cap cap])
                       first-profitable (get-in observed [:by-cap cap])
                       after-first-trial (if (and (>= cap 2) (< (actual 2) (actual 1))) 2 1)]
                   [cap {:predicted from-model :first-measured-saving first-profitable
                         :stop-after-two after-first-trial
                         :missed-by-stopping? (and (= 1 after-first-trial) (> first-profitable 2))
                         :measured-saving-ms (- (actual 1) (actual from-model))}]))]
    (into (sorted-map) (map choice) strides)))

(defn with-validation [data]
  (let [validate (fn [trained observed]
                   (into {} (map (fn [stat] [stat (compare-decisions (trained stat) (observed stat))]))
                         [:p50 :mean]))]
    (assoc data :validation (mapv validate (:predictions data) (:held-out data)))))

(defn- scenario [bean work effect warmup frames]
  (let [run (fn [stride oracle]
              (admission/replay bean work (assoc {:wash 1 :steer 1} effect stride) warmup frames oracle))
        reference (:frames (run 1 nil))
        sample (fn [stride] [stride (get-in (run stride reference) [:statistics effect])])
        calibration (mapv sample [1 2 2 1])
        predictions (calibrated calibration)
        ;; Decisions are frozen before testing the four intervals in both orders.
        trials (mapv sample [1 2 4 8 8 4 2 1])
        pulse (mapv :pulse reference) shutter (mapv :shutter reference)]
    (when-not (and (some #(> % 0.5) pulse) (> (count (partition-by identity shutter)) 1))
      (throw (ex-info "The calibration window must include pulse and shutter transitions" {})))
    {:effect effect :calibration calibration :predictions predictions
     :trials trials :held-out (held-out trials) :pulse pulse :shutter shutter}))

(defn measure [pixels movers warmup frames]
  (let [bean (ManagementFactory/getThreadMXBean)
        _ (when-not (and (.isCurrentThreadCpuTimeSupported bean) (.isThreadCpuTimeEnabled bean))
            (throw (ex-info "This benchmark requires worker-thread CPU accounting" {})))
        shared (admission/workload pixels movers)
        cases [[:wash :wash shared] [:shared-aim :steer shared]
               [:spatial-aim :steer (admission/workload pixels movers (spatial-target 2))]
               [:deep-spatial-aim :steer (admission/workload pixels movers (spatial-target 8))]]]
    {:pixels pixels :movers movers :warmup warmup :frames frames :bpm 120 :period-ms 25
     :cases (into {} (map (fn [[name effect work]] [name (with-validation (scenario bean work effect warmup frames))])) cases)}))

(defn -main [output & args]
  (let [[pixels movers warmup frames] (if (seq args) (mapv parse-long args) [1024 128 48 80])]
    (when-not (and output (or (empty? args) (= 4 (count args))) (every? pos-int? [pixels movers warmup frames])
                   (> pixels 1) (>= frames 24) (zero? (mod frames 8)))
      (throw (ex-info "Supply OUTPUT [pixels movers warmup frames]; frames must be a multiple of eight, at least 24" {})))
    (try
      (let [report (measure pixels movers warmup frames)]
        (io/make-parents output)
        (spit output (pr-str report))
        (prn (update-vals (:cases report) #(select-keys % [:effect :predictions :held-out]))))
      (finally (shutdown-agents)))))
