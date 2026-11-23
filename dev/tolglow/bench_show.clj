(ns tolglow.bench-show
  "Reproducible mixed-rig engine and complete host profiling. No network output."
  (:require [afterglow.control.model :as control]
            [afterglow.runtime.engine :as engine]
            [afterglow.runtime.executor :as executor]
            [afterglow.render.frame :as render]
            [tolglow.compiler :as compiler]
            [tolglow.compiler.batch :as batch]
            [afterglow.output.artnet :as artnet]
            [afterglow.output.scheduler :as scheduler]
            [tolglow.show :as show]
            [tolglow.perf.venue-rig :as venue]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.pprint :as pprint]
            [clojure.string :as str])
  (:import [java.lang.management ManagementFactory]
           [java.util Arrays]))

(defn- options [args]
  (when (odd? (count args)) (throw (ex-info "Every option requires a value" {})))
  (let [opts (reduce (fn [m [k v]]
                       (case k
                         "--heads" (assoc m :heads (mapv parse-long (str/split v #",")))
                         "--movers" (assoc m :movers (mapv parse-long (str/split v #",")))
                         "--workers" (assoc m :workers (mapv parse-long (str/split v #",")))
                         "--warmup" (assoc m :warmup (parse-long v))
                         "--samples" (assoc m :samples (parse-long v))
                         "--layout" (assoc m :layout (keyword v))
                         "--scope" (assoc m :scope (keyword v))
                         "--output" (assoc m :output v)
                         (throw (ex-info "Unknown option" {:option k}))))
                     {:heads [100 1000 10000] :movers [0 8] :workers [1 2]
                      :warmup 40 :samples 120 :scope :all :layout :grid} (partition 2 args))]
    (when-not (and (every? pos-int? (:heads opts)) (seq (:heads opts))
                   (every? nat-int? (:movers opts)) (seq (:movers opts))
                   (every? #(and (pos-int? %) (<= % 32)) (:workers opts))
                   (pos-int? (:warmup opts)) (pos-int? (:samples opts))
                   (#{:all :pipeline :host} (:scope opts))
                   (#{:grid :sculpture} (:layout opts)))
      (throw (ex-info "Invalid benchmark counts or scope" {:options opts})))
    opts))

(defn- command [model id payload]
  (let [result (control/apply-command model
                 (merge {:show-id (:show-id model) :epoch (:epoch model) :time-ms 0
                         :command-id (str id) :source :performance-profile} payload))]
    (when-not (= :accepted (get-in result [:ack :status]))
      (throw (ex-info "Benchmark command rejected" {:ack (:ack result)})))
    (:model result)))

(defn- patch-address [offset]
  {:universe (inc (quot offset 512)) :address (inc (mod offset 512))})

(defn- pixel [layout i]
  (assoc (patch-address (* 4 i))
         :id (str "pixel-" i)
         :position (if (= layout :sculpture)
                     (venue/pixel-position i)
                     [(double (mod i 100)) 0.0 (double (quot i 100))])
         :channels {:red 1 :green 2 :blue 3 :white 4}))

(defn- moving-head [layout pixels movers i]
  (merge (patch-address (+ (* 4 pixels) (* 12 i)))
         (if (= layout :sculpture)
           (venue/mover-pose i movers)
           {:position [(* 1.0 i) 4.0 -3.0] :rotation-deg [0.0 0.0 0.0]})
         {:id (str "mover-" i) :footprint 12
          :channels {:red 1 :green 2 :blue 3 :white 4 :dimmer 5
                     :pan [6 7] :tilt [8 9] :focus 10 :zoom 11}}))

(defn corpus
  ([pixels movers] (corpus pixels movers :grid))
  ([pixels movers layout]
   (let [pixel-fixtures (mapv (partial pixel layout) (range pixels))
         moving-fixtures (mapv (partial moving-head layout pixels movers) (range movers))
         fixtures (into pixel-fixtures moving-fixtures)
         definitions (cond->
                       [{:id :spatial :version 1 :targets (mapv :id pixel-fixtures)
                         :params {:level {:type :number :default 0.5 :bounds [0 1]}}
                         :outputs {:color (venue/spatial-color layout) :white 0.1}}
                        {:id :gain :version 1 :params {:level {:type :number :default 0.8 :bounds [0 1]}}
                         :envelope {:attack {:beats 1/2} :release {:beats 1}}
                         :outputs {:dimmer {:op :param :key :level}}}
                        {:id :accent :version 1 :targets (mapv :id (take 16 pixel-fixtures))
                         :outputs {:dimmer {:op :tween :spec {:keyframes [{:at-ms 0 :value 0.3}
                                                                        {:at-ms 10000 :value 0.7}]}}}}]
                       (pos? movers)
                       (conj {:id :movement :version 1 :targets (mapv :id moving-fixtures)
                              :outputs {:aim {:op :vec3 :args [{:op :multiply :args [3.0 {:op :sin :args [{:op :bar}]}]}
                                                              1.5 4.0]}
                                        :color [0.5 0.1 0.7] :focus 0.45 :zoom 0.3}}))
         model (reduce (fn [m definition]
                         (let [id (:id definition)]
                           (-> m (command [id :definition] {:op :registry/publish :kind :effects :definition definition})
                               (command [id :cue] {:op :cue/put :cue {:id id :effect-id id}})
                               (command [id :launch] {:op :cue/launch :cue-id id}))))
                       (control/new-show {:show-id "master-performance" :rig {:fixtures fixtures}}) definitions)]
     {:model model :fixtures fixtures :definitions definitions :patch (render/compile-patch fixtures)
      :pixels pixels :movers movers :layout layout
      :preview-indices (vec (concat (range (min 16 pixels)) (range pixels (+ pixels movers))))})))

(defn capture-sink []
  (let [latest (volatile! [])]
    {:latest latest :description {:type :artdmx-capture :network false}
     :send! (fn [universes _] (vreset! latest (mapv (fn [[u b]] (artnet/artdmx-packet u 1 b)) (sort-by key universes))))
     :close! (fn [])}))

(defn- approximately= [a b]
  (cond
    (= a b) true
    (and (number? a) (number? b)) (< (Math/abs (- (double a) (double b))) 1.0e-9)
    (and (map? a) (map? b) (= (set (keys a)) (set (keys b))))
    (every? (fn [[k v]] (approximately= v (get b k))) a)
    (and (sequential? a) (sequential? b) (= (count a) (count b))) (every? true? (map approximately= a b))
    :else false))

(defn check! [expected actual]
  (when-not (and (= (:model expected) (:model actual))
                 (= (:intents expected) (:intents actual))
                 (= (count (:packets expected)) (count (:packets actual)))
                 (every? true? (map #(Arrays/equals ^bytes %1 ^bytes %2) (:packets expected) (:packets actual)))
                 (approximately= (:preview expected) (:preview actual)))
    (throw (ex-info "Frame differs from scalar engine/renderer oracle"
                    {:model-equal? (= (:model expected) (:model actual))
                     :intents-equal? (= (:intents expected) (:intents actual))
                     :preview-equal? (approximately= (:preview expected) (:preview actual))}))))

(defn- pipeline [{:keys [model fixtures patch preview-indices]} compile mapper sink rich?]
  (let [start (System/nanoTime)
        evaluated (engine/advance model {:time-ms 1250.0 :beat 2.5 :fixtures fixtures :epoch (:epoch model)} compile mapper)
        after-engine (System/nanoTime)
        output (render/resolve-contributions patch (:contributions evaluated)
                                            {:master (:master model) :include-values? rich? :compact-values? true})
        after-render (System/nanoTime)
        preview (render/project-compact (:compact output) preview-indices)
        after-preview (System/nanoTime)
        packets ((:send! sink) (:universes output) (get-in evaluated [:model :frame-number]))
        end (System/nanoTime)]
    {:model (:model evaluated) :intents (:intents evaluated) :packets packets :preview preview
     :stages-ms {:engine (/ (- after-engine start) 1e6) :render (/ (- after-render after-engine) 1e6)
                 :preview (/ (- after-preview after-render) 1e6) :packet-encoding (/ (- end after-preview) 1e6)}}))

(defn distribution [values]
  (let [xs (vec (sort values)) n (count xs) at #(nth xs (dec (max 1 (long (Math/ceil (* % n))))))]
    {:p50 (at 0.5) :p95 (at 0.95) :p99 (at 0.99) :min (first xs) :max (peek xs)
     :mean (/ (reduce + 0.0 xs) n)}))

(defn gc-snapshot []
  (into {} (for [collector (ManagementFactory/getGarbageCollectorMXBeans)]
             [(.getName collector) {:collections (.getCollectionCount collector) :ms (.getCollectionTime collector)}])))

(defn measure
  "Time run after warmup; verify each result against expected outside its measured interval."
  [run expected {:keys [warmup samples]}]
  (dotimes [i warmup] (check! (expected i) (run i)))
  (let [bean (ManagementFactory/getThreadMXBean)
        owner (.getId (Thread/currentThread))
        ids (vec (filter (fn [id] (let [info (.getThreadInfo bean id)]
                                    (and info (or (= owner id) (str/starts-with? (.getThreadName info) "afterglow-runtime-")))))
                         (.getAllThreadIds bean)))
        allocations? (and (instance? com.sun.management.ThreadMXBean bean)
                           (.isThreadAllocatedMemorySupported ^com.sun.management.ThreadMXBean bean)
                           (.isThreadAllocatedMemoryEnabled ^com.sun.management.ThreadMXBean bean))
        allocated #(when allocations? (reduce + (map (fn [id] (max 0 (.getThreadAllocatedBytes ^com.sun.management.ThreadMXBean bean (long id)))) ids)))
        cpu? (and (.isThreadCpuTimeSupported bean) (.isThreadCpuTimeEnabled bean))
        cpu #(when cpu? (reduce + (map (fn [id] (max 0 (.getThreadCpuTime bean (long id)))) ids)))
        gc-before (gc-snapshot)
        observations (mapv (fn [offset]
                             (let [i (+ warmup offset) bytes-before (allocated) cpu-before (cpu) start (System/nanoTime)
                                   actual (run i) elapsed (/ (- (System/nanoTime) start) 1e6) cpu-after (cpu) bytes-after (allocated)]
                               (check! (expected i) actual)
                               {:ms elapsed :cpu-ms (when cpu? (/ (- cpu-after cpu-before) 1e6))
                                :bytes (when allocations? (- bytes-after bytes-before)) :stages-ms (:stages-ms actual)}))
                           (range samples))
        milliseconds (distribution (map :ms observations))]
    {:milliseconds milliseconds :cpu-ms (when cpu? (distribution (map :cpu-ms observations)))
     :frames-per-second (/ 1000.0 (:mean milliseconds))
     :over-25ms (count (filter #(> (:ms %) 25.0) observations))
     :allocated-bytes (when allocations? (distribution (map :bytes observations))) :allocation-thread-count (count ids)
     :stages-ms (into {} (for [stage (keys (:stages-ms (first observations)))]
                           [stage (distribution (map #(get-in % [:stages-ms stage]) observations))]))
     :gc-before gc-before :gc-after (gc-snapshot) :observations observations}))

(defn- cached-compiler [factory definitions]
  (let [compiled (into {} (map (juxt identity factory)) definitions)]
    #(or (get compiled %) (throw (ex-info "Uncaptured benchmark definition" {})))))

(defn- pipeline-bench [{:keys [fixtures definitions] :as data} opts]
  (let [scalar (cached-compiler compiler/compile-effect definitions)
        typed (cached-compiler (batch/compiler-for-rig fixtures) definitions)
        expected (pipeline data scalar mapv (capture-sink) true)]
    (into [{:scope :pipeline :execution :serial
            :result (measure (fn [_] (pipeline data typed mapv (capture-sink) false)) (constantly expected) opts)}]
          (for [workers (:workers opts)]
            (with-open [^java.io.Closeable pool (executor/create {:workers workers :queue-size 16 :timeout-ms 5000})]
              {:scope :pipeline :execution :workers :workers workers
               :result (measure (fn [_] (pipeline data typed (executor/mapper pool) (capture-sink) false)) (constantly expected) opts)})))))

(defn host-step! [runtime sink preview-indices i]
  (let [now (* 25.0 (inc i))
        reply (when (zero? (mod i 8))
                (show/submit! runtime {:command-id (str "frame-" i) :op :master/set
                                       :value (+ 0.5 (* 0.01 (mod i 40)))}))
        start (System/nanoTime)
        _ (show/tick! runtime now)
        ticked (System/nanoTime)
        _ (when (:independent-output? runtime)
            (scheduler/publish! (:publisher runtime)))
        published (System/nanoTime)]
    (let [{:keys [model output status]} @(:state runtime)]
      (when (or (:error status) (:output-error status) (:reactive-error status)
                (and reply (not= :accepted (:status @reply))))
        (throw (ex-info "Host benchmark did not produce a successful frame" {:status status})))
      (let [preview (if (:compact output)
                      (render/project-compact (:compact output) preview-indices)
                      (mapv #(get (:values output) (nth (:target-order output) %)) preview-indices))]
        {:model model :intents (:last-intents status) :packets @(:latest sink) :preview preview
         :stages-ms {:tick (/ (- ticked start) 1e6)
                     :publish (/ (- published ticked) 1e6)
                     :preview (/ (- (System/nanoTime) published) 1e6)}}))))

(defn projected-host-step!
  "Advance one show and capture freshly presented packets and comparable preview fields."
  [runtime sink clock indices i]
  (vreset! clock (* 25.0 (inc i)))
  (vreset! (:latest sink) [])
  (let [frame (host-step! runtime sink indices i)]
    (when (empty? (:packets frame))
      (throw (ex-info "Expected a fresh ArtDMX capture" {:frame i})))
    (update frame :preview
            (fn [values]
              (mapv #(select-keys % [:id :pan :tilt :focus :zoom :strobe
                                     :emitted-color :intensity :aim :direction]) values)))))

(defn- host-bench [{:keys [model preview-indices layout]} opts]
  (let [clock (volatile! 0.0)
        options {:model model :workspace-options nil :physical-now-ms #(deref clock)}
        step! (fn [rt sink i] (projected-host-step! rt sink clock preview-indices i))
        sink (capture-sink)
        oracle (show/create-runtime (assoc options :workers 1 :sink sink :batch? false :worker-timeout-ms 10000))
        expected (try
                   (mapv #(step! oracle sink %) (range (+ (:warmup opts) (:samples opts))))
                   (finally (show/stop! oracle)))]
    (vec (for [workers (:workers opts)
               independent? (if (= layout :sculpture) [false true] [false])]
           (let [sink (capture-sink)
                 runtime (show/create-runtime (assoc options :workers workers :sink sink
                                                     :independent-output? independent?))]
             (try
               {:scope :host-tick :execution :workers :workers workers
                :output (if independent? :independent :direct)
                :result (measure #(step! runtime sink %) #(nth expected %) opts)}
               (finally (show/stop! runtime))))))))

(defn revision [directory]
  (let [r (shell/sh "git" "rev-parse" "HEAD" :dir directory)] (when (zero? (:exit r)) (str/trim (:out r)))))

(defn source-status [directory]
  (let [r (shell/sh "git" "status" "--short" "--" "src" "resources" "dev" "bin" "project.clj" :dir directory)]
    (when (zero? (:exit r)) (str/trim (:out r)))))

(defn- system-file [path]
  ;; Some container procfs files reject FileInputStream.available().
  (when (.isFile (io/file path))
    (let [result (shell/sh "cat" path)] (when (zero? (:exit result)) (:out result)))))

(defn cpu-model []
  (or (some #(when (str/starts-with? % "model name") (str/trim (second (str/split % #":" 2))))
            (str/split-lines (or (system-file "/proc/cpuinfo") "")))
      (when (= "Mac OS X" (System/getProperty "os.name"))
        (let [result (shell/sh "sysctl" "-n" "machdep.cpu.brand_string")]
          (when (zero? (:exit result)) (str/trim (:out result)))))
      "unavailable"))

(defn -main [& args]
  (try
    (let [opts (options args)
          report {:benchmark :master-mixed-show :version 1 :measured-at (str (java.time.Instant/now))
                  :source-revisions {:tolglow (revision ".") :afterglow (revision (or (System/getenv "AFTERGLOW_ROOT") "../afterglow"))}
                  :source-working-trees {:tolglow (source-status ".") :afterglow (source-status (or (System/getenv "AFTERGLOW_ROOT") "../afterglow"))}
                  :environment {:java (System/getProperty "java.version") :clojure (clojure-version)
                                :os (System/getProperty "os.name") :architecture (System/getProperty "os.arch")
                                :processors (.availableProcessors (Runtime/getRuntime)) :heap-bytes (.maxMemory (Runtime/getRuntime))
                                :jvm-arguments (vec (.getInputArguments (ManagementFactory/getRuntimeMXBean)))
                                :cpu-model (cpu-model)
                                :cgroup-cpu (when (.isFile (io/file "/sys/fs/cgroup/cpu.max")) (str/trim (system-file "/sys/fs/cgroup/cpu.max")))}
                  :options (dissoc opts :output)
                  :notes ["Every packet byte, model and intent is checked against scalar compiler/general renderer; preview tolerance 1e-9."
                          "Pipeline replays one captured frame; stage timings include timer overhead."
                          "Host ticks advance musical time, admission/master commands, effects, rendering, reactive publication and ArtDMX capture; construction/compilation warmup excluded."
                          "No sockets or physical devices. Async scheduler jitter, external browser/UI load and packet delivery are excluded."
                          "Sculpture host runs also tick the independent publisher at a deterministic 25 ms cadence; no thread scheduling jitter is simulated."
                          "Allocation includes owner plus warmed runtime worker threads; GC counts cover measured loop including outside-timing oracle checks."]
                  :results (vec (for [pixels (:heads opts) movers (:movers opts)
                                      :let [data (corpus pixels movers (:layout opts))]]
                                  (do (binding [*out* *err*] (println "Profiling" pixels "pixels +" movers "moving heads"))
                                      {:pixels pixels :moving-heads movers :effects (count (:definitions data))
                                       :universes (count (get-in data [:patch :universes]))
                                       :runs (vec (concat (when (#{:all :pipeline} (:scope opts)) (pipeline-bench data opts))
                                                          (when (#{:all :host} (:scope opts)) (host-bench data opts))))})))}]
      (if-let [file (:output opts)]
        (do (io/make-parents file) (with-open [writer (io/writer file)]
                                   (binding [*out* writer pprint/*print-right-margin* 220] (pprint/pprint report)))
            (println "Wrote" file))
        (pprint/pprint report)))
    (finally (shutdown-agents))))
