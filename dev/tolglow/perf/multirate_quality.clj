(ns tolglow.perf.multirate-quality
  "Replay real emitted color, 3D aim, discrete DMX and one-shot cue lifetimes."
  (:require [afterglow.control.model :as control]
            [afterglow.render.compute :as compute]
            [afterglow.render.frame :as frame]
            [clojure.java.io :as io]
            [tolglow.bench-show :as bench]
            [tolglow.show :as show]
            [tolglow.perf.multirate-metrics :as metrics])
  (:import [java.lang.management ManagementFactory]
           [com.sun.management OperatingSystemMXBean]))

(def period-ms 25.0)
(def policies
  [[:full {:wash 1 :steer 1 :pulse 1}]
   [:uniform-4 {:wash 4 :steer 4 :pulse 4}]
   [:selective {:wash 4 :steer 2 :pulse 1}]])

(defn- op [name & args] {:op name :args (vec args)})
(def beat {:op :beat})

(defn- oscillation [rate phase]
  (op :add 0.5 (op :multiply 0.45 (op :sin (op :add (op :multiply rate beat) phase)))))

(defn programs [fixtures pixels]
  (let [movers (subvec fixtures pixels)
        phase (op :multiply 0.15 {:op :position :axis :x})
        pulse (op :floor (op :add 0.12 (op :fract (op :add (op :multiply 2.0 beat) 0.04))))]
    [{:id :wash :outputs {:color (op :vec3 (oscillation 0.6 phase) (oscillation 1.1 phase) (oscillation 2.0 phase))}}
     {:id :steer :targets (mapv :id movers)
      :outputs {:aim (op :vec3 (op :multiply 3.0 (op :sin (op :add beat phase)))
                          (op :add 1.0 (oscillation 1.3 phase)) (op :multiply 4.0 (oscillation 0.7 phase)))}}
     {:id :pulse :targets [(:id (first fixtures))] :outputs {:dimmer pulse :color [1 1 1]}}
     {:id :shutter :targets [(:id (first movers))] :duration {:beats 1/4} :outputs {:shutter 1.0}}]))

(defn- scene [fixtures pixels rates]
  (letfn [(install [model definition]
            (let [id (:id definition) stride (get rates id 1)
                  effect (cond-> (assoc definition :version 1)
                           (> stride 1) (assoc :keyframes {:interval {:beats (/ stride 20)}}))
                  registered (-> model
                                 (bench/command [id :publish] {:op :registry/publish :kind :effects :definition effect})
                                 (bench/command [id :cue] {:op :cue/put :cue {:id id :effect-id id}}))]
              (if (= :shutter id) registered
                (bench/command registered [id :launch] {:op :cue/launch :cue-id id}))))]
    (reduce install (control/new-show {:show-id :multirate :rig {:fixtures fixtures}})
            (programs fixtures pixels))))

(defn- shutter-byte [output fixture]
  (let [slot (+ (dec (:address fixture)) (dec (get-in fixture [:channels :shutter])))
        data (get-in output [:universes (+ (:universe fixture) (quot slot 512))])]
    (bit-and 255 (aget ^bytes data (mod slot 512)))))

(defn read-frame [state fixtures pixels]
  (let [{:keys [model output status]} state
        rows (if-let [compact (:compact output)]
               (frame/project-compact compact (range (count fixtures)))
               (mapv #(get-in output [:values (:id %)]) fixtures))
        active (filter #(= :shutter (:cue-id %)) (vals (:instances model)))]
    (when (or (:error status) (:output-error status) (some nil? (map :emitted-color rows)))
      (throw (ex-info "Incomplete benchmark frame" {:status status})))
    {:dmx (:universes output) :rgb (double-array (mapcat :emitted-color rows))
     :aim (mapv :aim (subvec rows pixels))
     :pulse (first (:emitted-color (first rows)))
     :shutter (shutter-byte output (nth fixtures pixels))
     :lifecycle (mapv (juxt :cue-id :phase :started-at-ms) active)
     :intents (mapv #(select-keys % [:type :cue-id :effect-id :reason]) (:last-intents status))}))

(defn- replay [fixtures pixels rates warmup frames backend batch?]
  (let [sink (bench/capture-sink)
        runtime (show/create-runtime {:model (scene fixtures pixels rates) :workers 2 :batch? batch?
                                      :adaptive-keyframes {:enabled? false}
                                      :compute-backend backend :movement-backend backend
                                      :workspace-options nil :worker-timeout-ms 10000 :sink sink})
        os (ManagementFactory/getOperatingSystemMXBean)
        cpu-ns (if (instance? OperatingSystemMXBean os)
                 #(.getProcessCpuTime ^OperatingSystemMXBean os)
                 (constantly nil))
        step (fn [i]
               (let [shot (when (zero? (mod i 17))
                            (show/submit! runtime {:command-id (str "shot-" i) :op :cue/launch :cue-id :shutter
                                                   :launch {:mode :one-shot :duration {:beats 1/4}}}))
                     _ (vreset! (:latest sink) [])
                     cpu-start (cpu-ns)
                     start (System/nanoTime)
                     _ (show/tick! runtime (* i period-ms))
                     elapsed (/ (- (System/nanoTime) start) 1e6)
                     cpu-ms (when cpu-start (/ (- (cpu-ns) cpu-start) 1e6))]
                 (when (or (empty? @(:latest sink)) (and shot (not= :accepted (:status @shot))))
                   (throw (ex-info "Missing ArtDMX or rejected shot" {:frame i :receipt (when shot @shot)})))
                 (when (>= i warmup)
                   (assoc (read-frame @(:state runtime) fixtures pixels) :elapsed-ms elapsed :cpu-ms cpu-ms))))]
    (try
      (let [samples (into [] (keep step) (range (+ warmup frames)))]
        {:samples samples :backend (compute/status (:compute runtime)) :movement (compute/movement-status (:compute runtime) (:output @(:state runtime)))})
      (finally (show/stop! runtime)))))

(defn compare-frame [fixtures pixels expected actual]
  (let [origin (map :position (subvec fixtures pixels))
        target (:aim expected) observed (:aim actual)]
    (merge (metrics/rgb-error (:rgb expected) (:rgb actual))
           {:dmx-max (metrics/dmx-error (:dmx expected) (:dmx actual))
            :beam-max-deg (reduce max 0.0 (map metrics/angle origin target observed))
            :aim-max-m (reduce max 0.0 (map metrics/distance target observed))
            :missed-pulse? (and (> (:pulse expected) 0.5) (<= (:pulse actual) 0.5))
            :false-pulse? (and (<= (:pulse expected) 0.5) (> (:pulse actual) 0.5))
            :shutter-error? (not= (:shutter actual) (:shutter expected))
            :lifecycle-error? (not= (select-keys actual [:lifecycle :intents]) (select-keys expected [:lifecycle :intents]))})))

(defn- report [expected run fixtures pixels]
  (let [actual (:samples run)
        rows (mapv (partial compare-frame fixtures pixels) expected actual)
        elapsed (map :elapsed-ms actual)
        process-cpu (keep :cpu-ms actual)
        dist (bench/distribution elapsed)
        count-flag #(count (filter % rows))
        trace {:pulse (mapv :pulse actual) :beam-error-deg (mapv :beam-max-deg rows) :tick-ms (vec elapsed)}]
    {:trace trace :rgb-rmse (Math/sqrt (/ (reduce + (map :rgb-mse rows)) (count rows)))
     :rgb-max (reduce max (map :rgb-max rows)) :oklab-max (reduce max (map :oklab-max rows))
     :dmx-max (reduce max (map :dmx-max rows))
     :beam-max-deg (reduce max (map :beam-max-deg rows)) :aim-max-m (reduce max (map :aim-max-m rows))
     :reference-pulse-frames (count (filter #(> (:pulse %) 0.5) expected))
     :missed-pulse-frames (count-flag :missed-pulse?) :false-pulse-frames (count-flag :false-pulse?)
     :reference-shutter-edges (count (filter (fn [[a b]] (not= (:shutter a) (:shutter b))) (partition 2 1 expected)))
     :shutter-mismatch-frames (count-flag :shutter-error?) :lifecycle-mismatch-frames (count-flag :lifecycle-error?)
     :cpu-ms-per-tick (when (seq process-cpu) (/ (reduce + process-cpu) (count process-cpu)))
     :wall-tick-ms dist :p95-headroom-ms (- period-ms (:p95 dist)) :late-frames (count (filter #(> % period-ms) elapsed))
     :compute (:backend run) :movement (:movement run)}))

(defn verify!
  "Discrete transitions must survive every policy. Continuous error is reported
  separately. Full-rate and pulse-aware lanes must retain every short flash."
  [{:keys [lanes] :as result}]
  (doseq [[lane stats] lanes]
    (let [checks (cond-> [:shutter-mismatch-frames :lifecycle-mismatch-frames]
                   (not= lane :uniform-4) (into [:missed-pulse-frames :false-pulse-frames]))]
      (when-not (and (pos? (:reference-shutter-edges stats)) (every? #(zero? (get stats %)) checks))
        (throw (ex-info "Discrete output fidelity regression" {:lane lane :stats (select-keys stats checks)})))))
  result)

(defn measure [pixels movers warmup frames backend]
  (let [{:keys [fixtures]} (bench/corpus pixels movers :sculpture)
        rig (into (subvec fixtures 0 pixels)
                  (map #(assoc-in % [:channels :shutter] 12)) (subvec fixtures pixels))
        [full & rest] policies
        run! #(replay rig pixels (second %) warmup frames backend true)
        reference (:samples (replay rig pixels (second full) warmup frames :cpu false))
        baseline (run! full)]
    (when-not (and (seq (filter #(> (:pulse %) 0.5) reference))
                   (seq (filter #(pos? (:shutter %)) reference)))
      (throw (ex-info "Measured window must exercise actual pulses and shutter changes" {})))
    {:pixels pixels :movers movers :warmup warmup :frames frames :frame-period-ms period-ms :bpm 120
     :reference :cpu-scalar-full-rate
     :reference-pulse (mapv :pulse reference) :reference-shutter (mapv :shutter reference)
     :lanes (into {(first full) (report reference baseline rig pixels)}
                  (map (fn [lane] [(first lane) (report reference (run! lane) rig pixels)]))
                  (conj (vec rest) [:full-repeat (second full)]))}))

(defn -main [& [file pixels movers warmup frames backend]]
  (let [[pixels movers warmup frames] (mapv parse-long [(or pixels "1024") (or movers "64")
                                                        (or warmup "40") (or frames "80")])]
    (when-not (and file (#{nil "cpu" "native-gpu"} backend) (every? pos-int? [pixels movers warmup frames]) (<= 20 frames))
      (throw (ex-info "Use OUTPUT.edn [pixels movers warmup frames cpu|native-gpu] and measure at least one beat" {})))
    (try
      (let [result (measure pixels movers warmup frames (keyword (or backend "cpu")))]
        (io/make-parents file) (spit file (str (pr-str result) "\n"))
        (verify! result) (prn (update result :lanes update-vals #(dissoc % :trace))))
      (finally (shutdown-agents)))))
