(ns tolglow.bench-pipeline
  "Actual new-model/compiler/engine/render/ArtDMX capture benchmark; no sockets."
  (:require [afterglow.control.model :as control]
            [afterglow.runtime.engine :as engine]
            [afterglow.runtime.executor :as executor]
            [afterglow.render.frame :as render]
            [tolglow.compiler :as compiler]
            [tolglow.compiler.batch :as batch]
            [tolglow.composition.response :as response]
            [tolglow.studio.output :as output]
            [clojure.pprint :as pprint]
            [clojure.string :as str])
  (:import [java.lang.management ManagementFactory]
           [java.util Arrays]))

(defn- options [args]
  (when (odd? (count args)) (throw (ex-info "Every option requires a value" {})))
  (let [options (reduce (fn [result [option value]]
                          (case option
                            "--heads" (assoc result :heads (mapv #(Long/parseLong %) (str/split value #",")))
                            "--warmup" (assoc result :warmup (Long/parseLong value))
                            "--samples" (assoc result :samples (Long/parseLong value))
                            (throw (ex-info "Unknown option" {:option option}))))
                        {:heads [1000 10000] :warmup 60 :samples 200} (partition 2 args))]
    (when-not (and (seq (:heads options)) (every? pos-int? (:heads options))
                   (pos-int? (:warmup options)) (pos-int? (:samples options)))
      (throw (ex-info "Benchmark counts must be positive" {})))
    options))

(defn- command [model id payload]
  (let [result (control/apply-command model (merge {:show-id (:show-id model) :epoch (:epoch model)
                                                   :command-id (pr-str id) :source :benchmark :time-ms 0} payload))]
    (when-not (= :accepted (get-in result [:ack :status]))
      (throw (ex-info "Benchmark setup command rejected" {:ack (:ack result)})))
    (:model result)))

(defn- setup [heads]
  (let [fixtures (mapv (fn [index]
                         {:id (str "pixel-" index) :position [(double index) 0.0 0.0]
                          :universe (inc (quot (* index 4) 512)) :address (inc (mod (* index 4) 512))
                          :channels {:red 1 :green 2 :blue 3 :white 4}}) (range heads))
        definitions [{:id :spatial :version 1 :params {:level {:type :number :default 0.5 :bounds [0 1]}}
                      :outputs {:color {:op :vec3 :args [{:op :multiply :args [0.5 {:op :add :args [1.0 {:op :sin :args [{:op :add :args [{:op :multiply :args [{:op :position :axis :x} 0.05]}
                                                                                                                                                        {:op :multiply :args [{:op :bar} (* 2 Math/PI)]}]}]}]}]}
                                                         {:op :param :key :level} 0.25]}
                                :white 0.1}}
                     {:id :gain :version 1 :params {:level {:type :number :default 0.8 :bounds [0 1]}}
                      :envelope {:attack {:beats 1/2} :release {:beats 1}}
                      :outputs {:dimmer {:op :param :key :level}}}
                     {:id :accent :version 1 :targets (mapv #(str "pixel-" %) (range (min 16 heads)))
                      :outputs {:dimmer {:op :tween :spec {:keyframes [{:at-ms 0 :value 0.3}
                                                                                    {:at-ms 2000 :value 0.7}]}}}}]
        initial (control/new-show {:show-id "pipeline-benchmark" :rig {:fixtures fixtures}})
        model (reduce (fn [model definition]
                        (let [id (:id definition)]
                          (-> model
                              (command [id :publish] {:op :registry/publish :kind :effects :definition definition})
                              (command [id :cue] {:op :cue/put :cue {:id id :effect-id id}})
                              (command [id :launch] {:op :cue/launch :cue-id id :launch {:mode :persistent}})))) initial definitions)
        responses (response/compile-middleware [{:id :response :select {:effect-ids [:gain]}
                                                 :params [:level] :operation :multiply :range [0.95 0.95]
                                                 :envelope {} :state {:started-at-ms 0}}])]
    {:fixtures fixtures :patch (render/compile-patch fixtures) :definitions definitions
     :model model :responses responses
     :frame {:time-ms 1250.0 :beat 2.5 :fixtures fixtures :epoch (:epoch model) :show-id (:show-id model)}}))

(defn- cached-compiler [factory definitions middleware]
  (let [compiled (into {} (map (fn [definition] [definition (comp (factory definition) middleware)])) definitions)]
    (fn [definition]
      (or (get compiled definition)
          (throw (ex-info "Benchmark only uses precompiled captured definitions" {}))))))

(defn- capture-sink []
  {:description {:type :artdmx-packet-capture :network false}
   :send! (fn [universes _frame-number]
            (mapv (fn [[universe values]] (output/artdmx-packet universe 1 values)) (sort-by key universes)))})

(defn- run-frame [{:keys [model frame patch]} compile map-work sink]
  (let [evaluated (engine/advance model frame compile map-work)
        output (render/resolve-contributions patch (:contributions evaluated)
                                              {:master (:master model) :blackout? (:blackout? model)
                                               :include-values? false :compact-values? true})
        preview (render/project-compact (:compact output) (range (min 16 (count (:target-order output)))))
        packets ((:send! sink) (:universes output) (get-in evaluated [:model :frame-number]))]
    {:model (:model evaluated) :intents (:intents evaluated) :preview preview :packets packets}))

(defn- matches? [expected actual]
  (and (= (:model expected) (:model actual)) (= (:intents expected) (:intents actual))
       (= (count (:packets expected)) (count (:packets actual)))
       (= (count (:preview expected)) (count (:preview actual)))
       (every? true? (map #(Arrays/equals ^bytes %1 ^bytes %2) (:packets expected) (:packets actual)))
       ;; Preview may differ at float64 rounding boundaries; physical bytes must
       ;; match exactly. Compare all published scalar/vector fields with tolerance.
       (every? true? (map (fn [a b]
                            (and (= (set (keys a)) (set (keys b)))
                                 (every? (fn [[key value]]
                                           (let [other (get b key)]
                                             (cond (= key :id) (= value other)
                                                   (number? value) (< (Math/abs (double (- value other))) 1.0e-9)
                                                   (vector? value) (every? #(< (Math/abs (double %)) 1.0e-9) (map - value other))
                                                   :else (= value other)))) a)))
                         (:preview expected) (:preview actual)))))

(defn- distribution [values]
  (let [ordered (vec (sort values)), n (count values)
        at (fn [p] (nth ordered (dec (max 1 (long (Math/ceil (* p n)))))))]
    {:p50 (at 0.5) :p95 (at 0.95) :p99 (at 0.99) :min (first ordered) :max (peek ordered)}))

(defn- measure [label run expected {:keys [warmup samples]}]
  (dotimes [_ warmup]
    (when-not (matches? expected (run)) (throw (ex-info "Warmup differs from scalar pipeline oracle" {:stage label}))))
  (let [bean (ManagementFactory/getThreadMXBean)
        owner-id (.getId (Thread/currentThread))
        thread-ids (vec (keep (fn [id]
                               (let [info (.getThreadInfo bean (long id))]
                                 (when (and info (or (= owner-id id) (str/starts-with? (.getThreadName info) "afterglow-runtime-"))) id)))
                             (.getAllThreadIds bean)))
        allocation? (and (instance? com.sun.management.ThreadMXBean bean)
                          (.isThreadAllocatedMemorySupported ^com.sun.management.ThreadMXBean bean)
                          (.isThreadAllocatedMemoryEnabled ^com.sun.management.ThreadMXBean bean))
        allocated (fn [] (when allocation?
                           (reduce + (map #(max 0 (.getThreadAllocatedBytes ^com.sun.management.ThreadMXBean bean (long %))) thread-ids))))
        observations (mapv (fn [_]
                             (let [before (allocated), start (System/nanoTime), result (run)
                                   ms (/ (- (System/nanoTime) start) 1.0e6), after (allocated)]
                               (when-not (matches? expected result)
                                 (throw (ex-info "Measured output differs from scalar pipeline oracle" {:stage label})))
                               {:ms ms :bytes (when (and before after) (- after before))})) (range samples))]
    {:stage label :warmup warmup :samples samples :milliseconds (distribution (map :ms observations))
     :allocation-thread-count (count thread-ids)
     :allocated-bytes (when allocation? (distribution (map :bytes observations)))
     :samples-ms (mapv :ms observations) :samples-allocated-bytes (when allocation? (mapv :bytes observations))}))

(defn benchmark [heads opts]
  (let [{:keys [fixtures definitions responses] :as data} (setup heads)
        scalar (cached-compiler compiler/compile-effect definitions responses)
        typed (cached-compiler (batch/compiler-for-rig fixtures) definitions responses)
        sink (capture-sink)
        expected (run-frame data scalar mapv sink)]
    (with-open [^java.io.Closeable pool (executor/create {:workers 2 :queue-size 8 :timeout-ms 5000})]
      {:heads heads :channels (* 4 heads) :universes (count (get-in data [:patch :universes]))
       :effects 3 :batch-effects 2 :ordinary-effects 1 :ordinary-targets (min heads 16) :preview-targets (min heads 16)
       :packet-bytes (reduce + (map alength (:packets expected)))
       :stages [(measure :scalar-engine-through-artdmx-capture #(run-frame data scalar mapv sink) expected opts)
                (measure :batch-engine-through-artdmx-capture #(run-frame data typed mapv sink) expected opts)
                (measure :batch-two-workers-through-artdmx-capture #(run-frame data typed (executor/mapper pool) sink) expected opts)]})))

(defn -main [& args]
  (try
    (let [opts (options args)]
      (pprint/pprint
       {:benchmark :new-runtime-through-artdmx-capture :version 1
        :measured-at (str (java.time.Instant/now))
        :environment {:java-version (System/getProperty "java.version") :clojure-version (clojure-version)
                      :processors (.availableProcessors (Runtime/getRuntime)) :heap-bytes (.maxMemory (Runtime/getRuntime))
                      :jvm-arguments (vec (.getInputArguments (ManagementFactory/getRuntimeMXBean)))}
        :notes ["Real control commands publish definitions/cues and launch three instances before timing."
                "Each measured replay advances the real engine using cached definitions, response middleware, musical transport/envelope and an ordinary tween fallback for 16 heads."
                "Measured work includes contribution merge, fixture/DMX resolution, owned compact preview, 16-head UI projection, actual ArtDMX packet encoding and injected capture sink."
                "All model/intent results and every packet byte are compared with the scalar compiler pipeline outside timing; preview uses absolute tolerance 1e-9."
                "Allocations sum the owner and warmed afterglow-runtime worker threads; unrelated JVM thread allocations are excluded."
                "Same captured input frame is replayed; command admission, clock advance, auto/sequence planning, hot reload compilation, HTTP serialization, full studio tick lock/queue, network and physical devices are excluded."
                "Sequence is fixed at 1 for deterministic capture. No socket is opened. CPU batching is not a GPU or SIMD claim."]
        :results (mapv #(benchmark % opts) (:heads opts))}))
    (finally (shutdown-agents))))
