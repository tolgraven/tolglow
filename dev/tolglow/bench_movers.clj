(ns tolglow.bench-movers
  "Complete animated moving-head shows and separately instrumented render phases."
  (:require [afterglow.control.model :as control]
            [afterglow.runtime.engine :as engine]
            [afterglow.runtime.executor :as executor]
            [afterglow.render.frame :as render]
            [tolglow.compiler :as compiler]
            [tolglow.compiler.batch :as batch]
            [tolglow.show :as show]
            [tolglow.bench-show :as bench]
            [tolglow.bench-movers-reference :as reference]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str])
  (:import [java.lang.management ManagementFactory]))

(defn- options [args]
  (when (odd? (count args)) (throw (ex-info "Each option needs a value" {})))
  (let [result (reduce (fn [m [key value]]
                        (case key
                          "--heads" (assoc m :heads (mapv parse-long (str/split value #",")))
                          "--workers" (assoc m :workers (mapv parse-long (str/split value #",")))
                          "--warmup" (assoc m :warmup (parse-long value))
                          "--samples" (assoc m :samples (parse-long value))
                          "--output" (assoc m :output value)
                          (throw (ex-info "Unknown moving-head profiler option" {:option key}))))
                      {:heads [64 128] :workers [1 2] :warmup 40 :samples 120} (partition 2 args))]
    (when-not (and (seq (:heads result)) (every? #(and (int? %) (<= 8 % 2048)) (:heads result))
                   (seq (:workers result)) (every? #(and (int? %) (<= 1 % 16)) (:workers result))
                   (pos-int? (:warmup result)) (pos-int? (:samples result)))
      (throw (ex-info "Invalid moving-head profiler counts" {:options result})))
    result))

(defn- apply-command [model command]
  (let [result (control/apply-command model
                 (merge {:show-id (:show-id model) :epoch (:epoch model) :time-ms (:time-ms model)
                         :source :moving-head-profile} command))]
    (when-not (= :accepted (get-in result [:ack :status]))
      (throw (ex-info "Moving-head profile command rejected" {:ack (:ack result)})))
    (:model result)))

(defn- corpus [heads]
  (let [fixtures (reference/fixtures heads) definitions (reference/definitions fixtures)
        ;; Deliberately launch in reverse priority order. The engine must restore
        ;; low→mid→high order regardless of launch or worker completion order.
        model (reduce (fn [model definition]
                        (let [id (:id definition)]
                          (-> model
                              (apply-command {:command-id (str id "-definition") :op :registry/publish :kind :effects :definition definition})
                              (apply-command {:command-id (str id "-cue") :op :cue/put
                                              :cue {:id id :effect-id id :priority (:priority definition)}})
                              (apply-command {:command-id (str id "-launch") :op :cue/launch :cue-id id}))))
                      (control/new-show {:show-id "animated-moving-head-profile" :rig {:fixtures fixtures}})
                      (reverse definitions))]
    {:model model :fixtures fixtures :definitions definitions :patch (render/compile-patch fixtures)
     :indices (vec (range heads))}))

(defn- commands [i]
  (mapv (fn [layer alpha]
          {:command-id (str "opacity-" i "-" layer) :op :group/apply
           :selector {:effect-ids (mapv #(reference/effect-id layer %) (range 8))}
           :action {:op :instance/opacity-set :value alpha}})
        (range 3) (reference/alphas i)))

(defn- millis [a b] (/ (- b a) 1e6))

(defn- pipeline-runner [{:keys [model fixtures definitions patch indices]} typed? mapper counters]
  (let [factory (if typed? (batch/compiler-for-rig fixtures) compiler/compile-effect)
        compiled (into {} (map (juxt identity factory)) definitions)
        initial (render/resolve-frame patch [] {:include-values? (not typed?) :compact-values? true})
        state (volatile! {:model model :output initial}) sink (bench/capture-sink)]
    {:close (fn []) :initial-preview (render/project-compact (:compact initial) indices)
     :run (fn [i]
            (when counters (vreset! counters {:aim 0 :pack 0}))
            (let [start (System/nanoTime) time-ms (* 25.0 (inc i))
                  model (-> (:model @state)
                            (assoc-in [:transport :beat] (* 0.05 (inc i)))
                            (assoc-in [:transport :time-ms] time-ms))
                  model (reduce apply-command model (commands i))
                  after-admission (System/nanoTime)
                  result (engine/advance model {:time-ms time-ms :epoch (:epoch model) :fixtures fixtures} compiled mapper)
                  after-engine (System/nanoTime)
                  output (render/resolve-contributions patch (:contributions result)
                            {:master 1.0 :include-values? (not typed?) :compact-values? true
                             :previous-compact (:compact (:output @state))})
                  after-render (System/nanoTime)
                  preview (render/project-compact (:compact output) indices)
                  after-preview (System/nanoTime)
                  packets ((:send! sink) (:universes output) (get-in result [:model :frame-number]))
                  end (System/nanoTime)
                  stages {:command-admission (millis start after-admission) :engine (millis after-admission after-engine)
                          :render (millis after-engine after-render) :all-head-preview (millis after-render after-preview)
                          :packet-encoding (millis after-preview end)}
                  stages (if counters
                           (assoc stages :aim-solving (/ (:aim @counters) 1e6)
                                  :fixture-pack-and-preview (/ (- (:pack @counters) (:aim @counters)) 1e6)
                                  :render-reduction-and-dispatch (- (:render stages) (/ (:pack @counters) 1e6))) stages)]
              (vreset! state {:model (:model result) :output output})
              {:model (:model result) :intents (:intents result) :packets packets :preview preview
               :universes (:universes output) :frame-ms (millis start end) :stages-ms stages}))}))

(defn- host-runner [{:keys [model indices definitions]} typed? workers]
  (let [sink (bench/capture-sink)
        runtime (show/create-runtime {:model model :workers workers :batch? typed? :sink sink :workspace-options nil})]
    ;; Match the pipeline lane's prepared compiler cache. Compilation belongs
    ;; to construction/warmup, not the engine's first worker deadline.
    (doseq [definition definitions] ((:compile runtime) definition))
    {:close #(show/stop! runtime)
     :initial-preview (mapv (fn [fixture] {:id (:id fixture) :pan 0.0 :tilt 0.0}) (get-in model [:rig :fixtures]))
     :run (fn [i]
            (let [start (System/nanoTime)
                  replies (mapv #(show/submit! runtime %) (commands i))
                  _ (show/tick! runtime (* 25.0 (inc i)))
                  end (System/nanoTime)
                  {:keys [model output status]} @(:state runtime)]
              (when (or (:error status) (:output-error status) (:reactive-error status)
                        (some #(not= :accepted (:status @%)) replies))
                (throw (ex-info "Moving-head host did not commit a healthy frame"
                                {:status status :frame-index i :typed? typed? :workers workers})))
              {:model model :intents (:last-intents status) :packets @(:latest sink)
               :preview (if (:compact output) (render/project-compact (:compact output) indices)
                            (mapv (fn [id]
                                    (let [view (get (:values output) id)]
                                      (merge (select-keys view [:id :emitted-color :intensity :aim :direction])
                                             (select-keys (:channels view) [:pan :tilt :focus :zoom])))) (:target-order output)))
               :universes (:universes output) :frame-ms (millis start end) :stages-ms {}}))}))

(defn- comparable [result]
  ;; Compact buffers also contain zero-filled strobe slots where the rich view
  ;; has no authored strobe. Keep semantically meaningful fields in both lanes.
  (update result :preview #(mapv (fn [view] (select-keys view [:id :pan :tilt :focus :zoom :emitted-color :intensity :aim :direction])) %)))

(defn- oracle [runner total fixtures]
  (try
    (loop [i 0 previous (:initial-preview runner) frames []]
      (if (= i total) frames
          (let [frame (comparable ((:run runner) i))
                proof (try (reference/check-frame! fixtures (get-in frame [:model :transport :beat])
                                                  (reference/alphas i) previous (:preview frame) (:universes frame))
                           (catch Exception error
                             (throw (ex-info (.getMessage error)
                                             (assoc (ex-data error) :frame-index i :alphas (reference/alphas i)
                                                    :beat (get-in frame [:model :transport :beat])) error))))]
            (recur (inc i) (:preview frame) (conj frames (assoc frame :proof proof))))))
    (finally ((:close runner)))))

(defn- allocation-reader []
  (let [bean (ManagementFactory/getThreadMXBean)
        ids (filterv (fn [id] (let [info (.getThreadInfo bean id)]
                               (and info (or (= id (.getId (Thread/currentThread)))
                                             (str/starts-with? (.getThreadName info) "afterglow-runtime-")))))
                     (.getAllThreadIds bean))]
    (when (and (instance? com.sun.management.ThreadMXBean bean)
               (.isThreadAllocatedMemoryEnabled ^com.sun.management.ThreadMXBean bean))
      #(reduce + (map (fn [id] (max 0 (.getThreadAllocatedBytes ^com.sun.management.ThreadMXBean bean id))) ids)))))

(defn- measure [runner expected fixtures {:keys [warmup samples]}]
  (try
    (let [previous (volatile! (:initial-preview runner))
          check (fn [i result]
                  (bench/check! (nth expected i) result)
                  (let [proof (reference/check-frame! fixtures (get-in result [:model :transport :beat])
                                                      (reference/alphas i) @previous (:preview result) (:universes result))]
                    (vreset! previous (:preview result)) proof))]
      (dotimes [i warmup] (check i (comparable ((:run runner) i))))
      (let [allocated (allocation-reader) gc-before (bench/gc-snapshot)
            observations (mapv (fn [i]
                                 (let [before (when allocated (allocated)) actual (comparable ((:run runner) i))
                                       after (when allocated (allocated)) proof (check i actual)]
                                   {:frame-number (get-in actual [:model :frame-number])
                                    :beat (get-in actual [:model :transport :beat]) :alphas (reference/alphas i)
                                    :ms (:frame-ms actual) :bytes (when allocated (- after before))
                                    :stages-ms (:stages-ms actual) :proof proof})) (range warmup (+ warmup samples)))]
        {:milliseconds (bench/distribution (map :ms observations))
         :frames-over-25ms (count (filter #(> (:ms %) 25.0) observations))
         :allocated-bytes (when allocated (bench/distribution (map :bytes observations)))
         :stages-ms (into {} (for [stage (keys (:stages-ms (first observations)))]
                              [stage (bench/distribution (map #(get-in % [:stages-ms stage]) observations))]))
         :gc-before gc-before :gc-after (bench/gc-snapshot) :observations observations}))
    (finally ((:close runner)))))

(defn- instrumented [f]
  (let [counters (volatile! {:aim 0 :pack 0})
        timed (fn [key original] (fn [& args]
                                  (let [start (System/nanoTime)]
                                    (try (apply original args)
                                         (finally (vswap! counters update key + (- (System/nanoTime) start)))))))]
    ;; Separate diagnostic pass in this owned JVM. Never use these wrappers for
    ;; host/deadline results; per-fixture timers add overhead to this pass.
    (with-redefs-fn {#'render/resolve-geometry (timed :aim @#'render/resolve-geometry)
                    #'render/pack-target! (timed :pack @#'render/pack-target!)} #(f counters))))

(defn- profile [heads opts]
  (let [{:keys [fixtures] :as data} (corpus heads) total (+ (:warmup opts) (:samples opts))
        expected-pipeline (oracle (pipeline-runner data false mapv nil) total fixtures)
        expected-host (oracle (host-runner data false 1) total fixtures)]
    {:heads heads :effects (count (:definitions data)) :universes (count (:universes (:patch data)))
     :unique-positions (count (set (map :position fixtures))) :unique-rotations (count (set (map :rotation-deg fixtures)))
     :runs (into [{:scope :pipeline :execution :serial
                   :result (measure (pipeline-runner data true mapv nil) expected-pipeline fixtures opts)}]
                 (mapcat (fn [workers]
                           (with-open [pool (executor/create {:workers workers :queue-size 32 :timeout-ms 5000})]
                             [{:scope :pipeline :workers workers
                               :result (measure (pipeline-runner data true (executor/mapper pool) nil) expected-pipeline fixtures opts)}
                              {:scope :host-tick :workers workers
                               :result (measure (host-runner data true workers) expected-host fixtures opts)}
                              {:scope :instrumented-render-phases :workers workers
                               :result (instrumented #(measure (pipeline-runner data true (executor/mapper pool) %) expected-pipeline fixtures opts))}])) (:workers opts)))}))

(defn -main [& args]
  (try
    (reference/self-check!)
    (let [opts (options args)
          report {:benchmark :animated-moving-heads :version 1 :measured-at (str (java.time.Instant/now))
                  :source-revisions {:tolglow (bench/revision ".") :afterglow (bench/revision (System/getenv "AFTERGLOW_ROOT"))}
                  :source-working-trees {:tolglow (bench/source-status ".") :afterglow (bench/source-status (System/getenv "AFTERGLOW_ROOT"))}
                  :environment {:java (System/getProperty "java.version") :clojure (clojure-version)
                                :os (System/getProperty "os.name") :architecture (System/getProperty "os.arch")
                                :cpu-model (bench/cpu-model) :processors (.availableProcessors (Runtime/getRuntime))
                                :cgroup-cpu (when (.isFile (io/file "/sys/fs/cgroup/cpu.max"))
                                              (str/trim (slurp "/sys/fs/cgroup/cpu.max")))
                                :heap-bytes (.maxMemory (Runtime/getRuntime))
                                :jvm-arguments (vec (.getInputArguments (ManagementFactory/getRuntimeMXBean)))}
                  :options (dissoc opts :output)
                  :notes ["64/128 differently placed and rotated RGBW moving heads, 24 real running effects, three competing priority layers launched in reverse priority order."
                          "Logical 25ms ticks at 120BPM; targets vary by fixture position/group/layer and musical beat. Opacity cycles exact 0/1, rising/falling and simultaneous mixtures."
                          "Every head, including indices 16 onward, is checked against an independent matrix/blend/calibration/channel oracle and scalar full-model/intent/ArtDMX reference."
                          "Host timing includes enqueue/admission, actual headless show tick, rendering, reactive publication and ArtDMX capture. All-head oracle projection happens afterward, outside host duration."
                          "Pipeline timing includes real advancing engine state, prior compact movement history, all-head projection and packet encoding."
                          "Both pipeline and host compiler caches are prepared before warmup. Cold definition compilation is not a measured frame phase."
                          "Instrumented-render-phases is a separate diagnostic pass: nested fixture timers report aim solving and remaining channel packing/preview work, with timer overhead. Never substitute its timing for uninstrumented deadlines."
                          "Allocated bytes cover owner/warmed effect-worker threads including result extraction; GC spans outside-timer reference checks."
                          "No browser, GPU, scheduler pacing/jitter, actual network or physical output. Frame budget counts describe captured CPU work, not proven real-time output."]
                  :results (mapv (fn [heads]
                                   (binding [*out* *err*] (println "Profiling animated moving heads:" heads))
                                   (try (profile heads opts)
                                        (catch Exception error
                                          {:heads heads :failed? true
                                           :failure {:message (.getMessage error)
                                                     :details (select-keys (ex-data error)
                                                               [:frame-index :alphas :beat :fixture :field :expected :actual
                                                                :status :typed? :workers :model-equal? :intents-equal? :preview-equal?])}}))) (:heads opts))}]
      (if-let [path (:output opts)]
        (do (io/make-parents path)
            (with-open [writer (io/writer path)] (binding [*out* writer pprint/*print-right-margin* 220] (pprint/pprint report)))
            (println "Wrote" path))
        (pprint/pprint report))
      (when (some :failed? (:results report))
        (throw (ex-info "Moving-head workload failed; report retains completed cases and failure details"
                        {:failed-head-counts (mapv :heads (filter :failed? (:results report)))}))))
    (finally (shutdown-agents))))
