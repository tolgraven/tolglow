(ns tolglow.perf.mixed-rig
  "Complete mixed-rig host profile through `show/tick!` with an ArtDMX capture.

  One rig holds single-channel dimmers, RGB pars with a dimmer, RGBW pixel
  strips and pan/tilt RGBW moving heads. Many cues run at once on overlapping
  targets at several priorities, with attack/release envelopes, a keyframe
  tween and beat-synchronized LFO bindings on cue parameters. Every measured
  frame is compared with a serial scalar oracle runtime (`:batch? false`, one
  worker): full model, intents, every packet byte and selected preview fields.

  No sockets, devices or `live/start-demo!`. The publisher is not ticked: the
  measured interval is one owner cycle (`show/tick!`)."
  (:require [afterglow.control.model :as control]
            [afterglow.render.frame :as render]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [tolglow.bench-show :as bench]
            [tolglow.show :as show])
  (:import [java.lang.management ManagementFactory]
           [java.time Duration]
           [jdk.jfr Recording]))

(defn- options [args]
  (when (odd? (count args)) (throw (ex-info "Every option requires a value" {})))
  (let [ints #(mapv parse-long (str/split % #","))
        opts (reduce (fn [m [k v]]
                       (case k
                         "--pixels" (assoc m :pixels (ints v))
                         "--dimmers" (assoc m :dimmers (parse-long v))
                         "--pars" (assoc m :pars (parse-long v))
                         "--movers" (assoc m :movers (parse-long v))
                         "--layers" (assoc m :layers (parse-long v))
                         "--workers" (assoc m :workers (ints v))
                         "--warmup" (assoc m :warmup (parse-long v))
                         "--samples" (assoc m :samples (parse-long v))
                         "--jfr" (assoc m :jfr v)
                         "--output" (assoc m :output v)
                         (throw (ex-info "Unknown option" {:option k}))))
                     {:pixels [1000] :dimmers 48 :pars 48 :movers 16 :layers 16
                      :workers [2] :warmup 200 :samples 400}
                     (partition 2 args))]
    (when-not (and (seq (:pixels opts)) (every? pos-int? (:pixels opts))
                   (every? nat-int? (map opts [:dimmers :pars :movers :layers]))
                   (every? #(<= 1 % 32) (:workers opts))
                   (pos-int? (:warmup opts)) (pos-int? (:samples opts)))
      (throw (ex-info "Invalid mixed-rig options" {:options opts})))
    opts))

(defn- address [offset]
  {:universe (inc (quot offset 512)) :address (inc (mod offset 512))})

(def ^:private level {:level {:type :number :default 0.5 :bounds [0 1]}})

(defn- x-phase [scale] {:op :multiply :args [{:op :position :axis :x} scale]})

(defn- fract-beat [rate offset]
  {:op :fract :args [{:op :add :args [{:op :multiply :args [{:op :beat} rate]} offset]}]})

(defn- scaled [expression] {:op :multiply :args [{:op :param :key :level} expression]})

(defn fixtures
  "Patch in this order: pixels (4 ch), dimmers (1), pars (4), movers (12).
  Fixtures cross universe boundaries."
  [{:keys [pixels dimmers pars movers]}]
  (let [offset (volatile! 0)
        place (fn [footprint m]
                (let [a (address @offset)] (vswap! offset + footprint) (merge a m)))]
    {:pixels (mapv #(place 4 {:id (str "pixel-" %) :position [(double (mod % 100)) 0.0 (double (quot % 100))]
                              :channels {:red 1 :green 2 :blue 3 :white 4}})
                   (range pixels))
     :dimmers (mapv #(place 1 {:id (str "dimmer-" %) :position [(* 0.5 %) 6.0 0.0] :channels {:dimmer 1}})
                    (range dimmers))
     :pars (mapv #(place 4 {:id (str "par-" %) :position [(* 0.5 %) 5.0 -2.0]
                            :channels {:red 1 :green 2 :blue 3 :dimmer 4}})
                 (range pars))
     :movers (mapv #(place 12 {:id (str "mover-" %) :position [(* 1.0 %) 4.0 -3.0] :rotation-deg [0.0 0.0 0.0]
                               :footprint 12
                               :channels {:red 1 :green 2 :blue 3 :white 4 :dimmer 5
                                          :pan [6 7] :tilt [8 9] :focus 10 :zoom 11}})
                   (range movers))}))

(defn- every-nth [n offset xs] (vec (take-nth n (drop offset xs))))

(defn definitions
  "Effects and their cue priorities. Launch order is the vector order."
  [{:keys [layers]} {:keys [pixels dimmers pars movers]}]
  (let [ids #(mapv :id %)
        all-dimmable (ids (concat dimmers pars movers))]
    (cond-> [[{:id :pixel-field :version 1 :targets (ids pixels) :params level
               :outputs {:color {:op :vec3 :args [{:op :multiply :args [0.5 {:op :add :args [1.0 {:op :sin :args [{:op :add :args [(x-phase 0.05) {:op :bar}]}]}]}]}
                                                  {:op :param :key :level} 0.25]}
                         :white 0.1}} 0]
             [{:id :pixel-overlay :version 1 :targets (every-nth 2 0 (ids pixels)) :params level
               :envelope {:attack {:beats 1/2} :release {:beats 1}}
               :outputs {:color {:op :vec3 :args [(scaled (fract-beat 0.5 (x-phase 0.02))) 0.1 {:op :param :key :level}]}}} 1]
             [{:id :accent :version 1 :targets (vec (take 16 (ids pixels)))
               :outputs {:white {:op :tween :spec {:keyframes [{:at-ms 0 :value 0.3} {:at-ms 10000 :value 0.7}]}}}} 2]
             [{:id :dimmer-chase :version 1 :targets (ids dimmers) :params level
               :outputs {:dimmer (scaled (fract-beat 1.0 (x-phase 0.1)))}} 0]
             [{:id :dimmer-wave :version 1 :targets (every-nth 2 1 (ids dimmers)) :params level
               :outputs {:dimmer (scaled {:op :multiply :args [0.5 {:op :add :args [1.0 {:op :sin :args [{:op :bar}]}]}]})}} 1]
             [{:id :par-wash :version 1 :targets (ids pars) :params level
               :outputs {:color {:op :vec3 :args [{:op :param :key :level} 0.2 0.6]} :dimmer 0.8}} 0]
             [{:id :par-accent :version 1 :targets (every-nth 4 0 (ids pars)) :params level
               :envelope {:attack {:beats 1/4} :release {:beats 1/2}}
               :outputs {:color [1.0 1.0 1.0] :dimmer (scaled (fract-beat 2.0 0.0))}} 2]
             [{:id :gain :version 1 :targets all-dimmable :params {:level {:type :number :default 0.8 :bounds [0 1]}}
               :envelope {:attack {:beats 1/2} :release {:beats 1}}
               :outputs {:dimmer {:op :param :key :level}}} 3]]
      (seq movers)
      (into [[{:id :movement :version 1 :targets (ids movers) :params level
               :outputs {:aim {:op :vec3 :args [{:op :multiply :args [3.0 {:op :sin :args [{:op :bar}]}]} 1.5 4.0]}
                         :color {:op :vec3 :args [0.5 {:op :param :key :level} 0.7]} :white 0.2
                         :focus 0.45 :zoom 0.3}} 0]
             [{:id :mover-pulse :version 1 :targets (every-nth 2 0 (ids movers)) :params level
               :outputs {:dimmer (scaled (fract-beat 0.25 (x-phase 0.125)))}} 1]])
      true
      (into (for [i (range layers)
                  :let [group (nth [pixels dimmers pars (if (seq movers) movers pars)] (mod i 4))
                        targets (every-nth (+ 2 (mod i 3)) (mod i 2) (ids group))]
                  :when (seq targets)]
              [{:id (keyword (str "layer-" i)) :version 1 :targets targets :params level
                :outputs (if (= 0 (mod i 4))
                           {:white (scaled (fract-beat (+ 0.25 (* 0.125 i)) (x-phase 0.01)))}
                           {:dimmer (scaled (fract-beat (+ 0.25 (* 0.125 i)) (x-phase 0.03)))})}
               (- 4 (mod i 5))])))))

(def ^:private modulated
  "Cue IDs whose :level parameter is driven by a beat-synchronized LFO."
  #{:pixel-field :pixel-overlay :dimmer-chase :par-wash :movement :layer-1 :layer-2 :layer-5})

(defn corpus [opts pixel-count]
  (let [groups (fixtures (assoc opts :pixels pixel-count))
        rig (vec (mapcat groups [:pixels :dimmers :pars :movers]))
        defs (definitions opts groups)
        model (reduce (fn [m [{:keys [id] :as definition} priority]]
                        (-> m
                            (bench/command [id :definition] {:op :registry/publish :kind :effects :definition definition})
                            (bench/command [id :cue] {:op :cue/put :cue {:id id :effect-id id :priority priority}})
                            (bench/command [id :launch] {:op :cue/launch :cue-id id})))
                      (control/new-show {:show-id "mixed-rig-performance" :rig {:fixtures rig}})
                      defs)
        index (into {} (map-indexed (fn [i f] [(:id f) i])) rig)
        preview (vec (distinct (concat (map index (take 8 (map :id (:pixels groups))))
                                       (map index (take 4 (map :id (:dimmers groups))))
                                       (map index (take 4 (map :id (:pars groups))))
                                       (map index (map :id (:movers groups)))))) ]
    {:model model :rig rig :definitions (mapv first defs) :priorities (mapv second defs)
     :counts (update-vals groups count) :preview-indices preview
     :universes (count (:universes (render/compile-patch rig)))}))

(defn- bind-modulation! [runtime model]
  (let [binds (for [[id instance] (sort-by key (:instances model))
                    :when (modulated (:cue-id instance))]
                (show/submit! runtime :modulation-bind
                              {:instance-id id :param :level
                               :binding {:type :lfo :waveform (if (even? (count (name (:cue-id instance)))) :saw :triangle)
                                         :period {:beats (if (#{:pixel-field :movement} (:cue-id instance)) 4 1)}
                                         :from 0.1 :to 0.9}}))
        binds (doall binds)]
    (show/tick! runtime 0.0)
    (doseq [reply binds]
      (when-not (= :accepted (:status (deref reply 2000 {:status :timeout})))
        (throw (ex-info "Modulation binding rejected" {:reply (deref reply 0 nil)}))))
    (count binds)))

(defn- runtime-for [model clock sink overrides]
  (let [runtime (show/create-runtime (merge {:model model :workspace-options nil :physical-now-ms #(deref clock)
                                             :sink sink}
                                            overrides))]
    {:runtime runtime :bindings (bind-modulation! runtime model)}))

(defn- with-jfr [file run]
  (if-not file
    (run)
    (let [recording (doto (Recording. (jdk.jfr.Configuration/getConfiguration "profile"))
                      (.setToDisk true)
                      (.setDestination (.toPath (io/file file))))]
      ;; One-millisecond Java samples: the default 10-20 ms leaves too few per frame.
      (-> (.enable recording "jdk.ExecutionSample") (.withPeriod (Duration/ofMillis 1)))
      (-> (.enable recording "jdk.ObjectAllocationSample") (.with "throttle" "5000/s"))
      (io/make-parents file)
      (.start recording)
      (try (run)
           (finally (.stop recording) (.close recording))))))

(defn host-run
  "Measure the host tick for each worker count against the serial scalar oracle."
  [{:keys [model preview-indices counts]} opts]
  (let [clock (volatile! 0.0)
        steps (+ (:warmup opts) (:samples opts))
        step! (fn [rt sink i] (bench/projected-host-step! rt sink clock preview-indices i))
        oracle-sink (bench/capture-sink)
        {oracle :runtime bound :bindings} (runtime-for model clock oracle-sink {:workers 1 :batch? false :worker-timeout-ms 10000})
        expected (try (mapv #(step! oracle oracle-sink %) (range steps))
                      (finally (show/stop! oracle)))]
    (vec (for [workers (:workers opts)]
           (let [sink (bench/capture-sink)
                 _ (vreset! clock 0.0)
                 {:keys [runtime]} (runtime-for model clock sink {:workers workers})]
             (try
               (let [result (with-jfr (when (:jfr opts) (str (:jfr opts) "-p" (:pixels counts) "-w" workers ".jfr"))
                              #(bench/measure (fn [i] (step! runtime sink i)) (fn [i] (nth expected i)) opts))]
                 {:scope :host-tick :workers workers :modulation-bindings bound
                  :result (-> result
                              (dissoc :observations)
                              (assoc :allocated-mb-per-frame
                                     (some-> (:allocated-bytes result) (update-vals #(/ % 1048576.0)))))})
               (finally (show/stop! runtime))))))))

(defn -main [& args]
  (let [opts (options args)
        afterglow (or (System/getenv "AFTERGLOW_ROOT") "../afterglow")
        report {:benchmark :mixed-rig-host :version 1 :measured-at (str (java.time.Instant/now))
                :source-revisions {:tolglow (bench/revision ".") :afterglow (bench/revision afterglow)}
                :source-working-trees {:tolglow (bench/source-status ".") :afterglow (bench/source-status afterglow)}
                :environment {:java (System/getProperty "java.version") :clojure (clojure-version)
                              :os (System/getProperty "os.name") :architecture (System/getProperty "os.arch")
                              :processors (.availableProcessors (Runtime/getRuntime))
                              :heap-bytes (.maxMemory (Runtime/getRuntime))
                              :jvm-arguments (vec (.getInputArguments (ManagementFactory/getRuntimeMXBean)))
                              :cpu-model (bench/cpu-model)
                              :load-average (.getSystemLoadAverage (ManagementFactory/getOperatingSystemMXBean))}
                :options (dissoc opts :output)
                :notes ["Measured interval: one show/tick! (command admission every 8th frame, clock, modulation, effects, render, reactive publication, ArtDMX capture encoding)."
                        "Excluded: publisher thread and scheduler jitter, packet delivery, Studio subscribers, browser, physical output, oracle comparison (outside the timer)."
                        "Allocation counts the owner thread plus warmed afterglow-runtime and spatial worker threads."
                        "GC counters span the measured loop including oracle comparisons."]
                :results (vec (for [pixels (:pixels opts)
                                    :let [data (corpus opts pixels)]]
                                (do (binding [*out* *err*]
                                      (println "Mixed rig" (:counts data) "cues" (count (:definitions data))))
                                    {:fixtures (:counts data) :cues (count (:definitions data))
                                     :universes (:universes data)
                                     :runs (host-run data opts)})))}]
    (if-let [file (:output opts)]
      (do (io/make-parents file)
          (with-open [writer (io/writer file)]
            (binding [*out* writer pprint/*print-right-margin* 200] (pprint/pprint report)))
          (println "Wrote" file))
      (pprint/pprint report))))
