(require '[clojure.string :as str]
         '[tolglow.bench-autopilot :as bench]
         '[tolglow.bench-show :as bench-show]
         '[tolglow.show :as show]
         '[tolglow.autopilot :as autopilot]
         '[tolglow.autopilot.controller :as controller]
         '[tolglow.lighting-activity :as activity]
         '[afterglow.output.artnet :as artnet])
(import '[com.sun.management ThreadMXBean]
        '[java.lang.management ManagementFactory]
        '[java.util Arrays]
        '[java.io BufferedOutputStream FileOutputStream])
(def candidate-meter (ns-resolve 'tolglow.lighting-activity 'advance-scene-meter))
(load-string (str/replace-first (slurp "/tmp/tolglow-413-activity-master.clj")
                               "(ns tolglow.lighting-activity" "(ns tolglow.lighting-activity-baseline"))
(def baseline-meter @(ns-resolve 'tolglow.lighting-activity-baseline 'advance-meter))
(def candidate @candidate-meter)
(def ^ThreadMXBean bean (ManagementFactory/getPlatformMXBean ThreadMXBean))
(defn gc-delta [before]
  (merge-with #(merge-with - %1 %2) (bench-show/gc-snapshot) before))
(defn packets [number ut]
  (mapv (fn [[u ^bytes payload]] (artnet/artdmx-packet u (inc (mod number 255)) payload)) (sort-by key ut)))
(defn check-packets! [expected actual case-number i]
  (when-not (and (= (count expected) (count actual))
                 (every? true? (map #(Arrays/equals ^bytes %1 ^bytes %2) expected actual)))
    (throw (ex-info "ArtDMX capture differs" {:case case-number :frame i}))))
(defn output-record! [reference reference? case-number i run packets]
  (if reference?
    (vswap! reference conj {:packets packets :run run})
    (let [expected (nth @reference i)]
      (check-packets! (:packets expected) packets case-number i)
      (when-not (= (:run expected) run)
        (throw (ex-info "Conductor run differs" {:case case-number :frame i}))))))
(def stage-times (volatile! nil))
(defn measure-stage [k f]
  (fn [& args]
    (if-not @stage-times
      (apply f args)
      (let [w (System/nanoTime) c (.getCurrentThreadCpuTime bean)
            result (apply f args)]
        (vswap! stage-times assoc k {:wall (- (System/nanoTime) w) :cpu (- (.getCurrentThreadCpuTime bean) c)})
        result))))
(defn stage-distributions [samples]
  (into {} (map (fn [[k measurements]]
                  [k {:calls (count measurements)
                      :wall-ms (bench-show/distribution (map #(/ (:wall %) 1e6) measurements))
                      :owner-cpu-ms (bench-show/distribution (map #(/ (:cpu %) 1e6) measurements))}]))
        (apply merge-with into
               (keep (fn [{:keys [stages]}] (when stages (update-vals stages vector))) samples))))
(defn run-case [model warmup measured variant case-number reference file-path]
  (let [sink (#'bench/counted-capture)
        before-workers (#'bench/worker-ids bean)
        rt (show/create-runtime {:model model :sink sink :workers 2 :workspace-options nil :worker-timeout-ms 5000})
        previous (volatile! nil)]
    (try
      (with-redefs-fn {candidate-meter (measure-stage :meter (case variant :baseline baseline-meter :candidate candidate))
                        #'autopilot/prepare (measure-stage :prepare @#'autopilot/prepare)
                        #'autopilot/observe (measure-stage :observe @#'autopilot/observe)
                        #'controller/advance (measure-stage :controller @#'controller/advance)}
        (fn []
          (with-open [out (if file-path (BufferedOutputStream. (FileOutputStream. file-path)) (java.io.OutputStream/nullOutputStream))]
            (let [read-run #(get-in @(:state rt) [:model :extensions :tolglow.autopilot/run])
                  frame! (fn [i workers]
                           (let [counted @(:counter sink)
                                 record? (some? workers)
                                 _ (vreset! stage-times (when record? {}))
                                 before (when record? (#'bench/thread-sample bean workers))
                                 _ (show/tick! rt (* 25 (inc i)))
                                 timing (when record? (merge-with - (#'bench/thread-sample bean workers) before))
                                 run (read-run)
                                 decision (get-in run [:decision :decision])
                                 decision? (not= decision @previous)
                                 [number ut] @(:latest sink)
                                 captured (packets number ut)]
                             (vreset! previous decision)
                             (#'bench/check-frame! rt sink counted)
                             (output-record! reference (zero? case-number) case-number i run captured)
                             (when (and file-path record?)
                               (doseq [^bytes packet captured] (.write ^java.io.OutputStream out packet)))
                             (when record? (assoc timing :decision? decision? :frame (inc i) :decision decision :stages @stage-times))))]
              (dotimes [i warmup] (frame! i nil))
              (let [workers (long-array (clojure.set/difference (#'bench/worker-ids bean) before-workers))
                    before-gc (bench-show/gc-snapshot)
                    before-jit (.getTotalCompilationTime (ManagementFactory/getCompilationMXBean))
                    samples (mapv #(frame! % workers) (range warmup (+ warmup measured)))
                    groups (group-by :decision? samples)
                    output (#'bench/final-output rt 2048 64 :autopilot)]
                (assert (= 2 (alength workers)))
                (assert (= (* 18 (+ warmup measured)) @(:datagrams sink)))
                {:variant variant :run case-number :warmup warmup :frames measured :verified-ticks (+ warmup measured)
                 :capture-frames @(:counter sink) :datagrams @(:datagrams sink) :probe output
                 :ordinary (assoc (#'bench/distributions (get groups false)) :stages (stage-distributions (get groups false)))
                 :decision (assoc (#'bench/distributions (get groups true)) :stages (stage-distributions (get groups true)))
                 :gc (gc-delta before-gc)
                 :jit-compilation-ms (- (.getTotalCompilationTime (ManagementFactory/getCompilationMXBean)) before-jit)
                 :samples samples})))))
      (finally (show/stop! rt)))))
(let [[w n out-dir] *command-line-args*
      warmup (parse-long w) measured (parse-long n)
      model (#'bench/performance-model 2048 64)
      ref (volatile! [])]
  (try
    (doseq [[case-number variant] (map-indexed vector [:baseline :candidate :candidate :baseline])]
      (let [result (run-case model warmup measured variant case-number ref
                             (when (= case-number 1) (str out-dir "/candidate.artdmx")))]
        (spit (str out-dir "/paired-" case-number ".edn") (pr-str result))
        (prn (dissoc result :samples))
        (flush)))
    (finally (shutdown-agents))))
