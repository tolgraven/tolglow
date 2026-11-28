(ns tolglow.perf.pixel-modifiers
  "Measure what one colour modifier adds to a complete show tick on a large
  pixel rig (#908). Every case runs the same positional wash over 2,048 RGB
  pixels; each modifier case adds one native layer modifier at a higher
  priority over all of them. The cases are interleaved frame by frame, two
  effect workers render each case and an in-memory ArtDMX sink captures the
  frame. Each modifier case must change the wash's bytes on every frame.

  `general` runs repeat each modifier case on Afterglow's general native layer
  path (runtime option `:typed-layers? false`) and require byte-equal packets
  every frame; a revision without the typed path ignores the option.

    AFTERGLOW_ROOT=../afterglow lein with-profile +benchmark run -m tolglow.perf.pixel-modifiers [warmup samples [pixels]]"
  (:require [afterglow.control.model :as control]
            [clojure.string :as str]
            [tolglow.bench-show :as bench]
            [tolglow.effects :as fx]
            [tolglow.show :as show])
  (:import [java.lang.management ManagementFactory]
           [java.util Arrays]))

(def ^:private tau (* 2.0 Math/PI))

(defn- op [operator & args] {:op operator :args (mapv fx/expression args)})

(def ^:private beat {:op :beat :clock :absolute})

(def ^:private x (fx/position :x))

(def ^:private y (fx/position :y))

(def ^:private z (fx/position :z))

(def ^:private p fx/param)

(defn- number [default low high] {:type :number :default default :bounds [low high]})

(defn- sine [cycles] (fx/add 0.5 (fx/multiply 0.5 (op :sin (fx/multiply tau cycles)))))

(defn- fract [value] (op :fract value))

(defn- floor [value] (op :floor value))

(defn pixels
  "RGB pixels in a 64-column grid, 11 m wide and 5 m tall, three channels each."
  [n]
  (mapv (fn [i]
          (let [column (mod i 64) row (quot i 64) offset (* 3 i)]
            {:id (str "pixel-" i) :universe (inc (quot offset 510)) :address (inc (mod offset 510))
             :position [(- (* 11.0 (/ column 63.0)) 5.5) (* 5.0 (/ row (max 1.0 (dec (Math/ceil (/ n 64.0)))))) 0.0]
             :channels {:red 1 :green 2 :blue 3}}))
        (range n)))

(def ^:private wash-color
  "A moving positional wash with varied hue and saturation, so the HSV
  filters meet greys, primaries and mixes."
  (fx/rgb (sine (fx/add (fx/multiply x 0.11) (fx/divide beat 8.0)))
          (sine (fx/subtract (fx/multiply y 0.17) (fx/divide beat 6.0)))
          (fx/add 0.2 (fx/multiply 0.6 (sine (fx/add (fx/multiply x -0.07) (fx/multiply y 0.05) (fx/divide beat 16.0)))))))

(defn- bloom-band
  "The legacy bloom measure, as in `tolglow.showcase.looks/bloom-keyhole`."
  []
  (let [dx (fx/subtract x (p :center-x))
        dy (fx/subtract y (p :center-y))
        distance (fx/divide (op :sqrt (fx/add (fx/multiply dx dx) (fx/multiply dy dy))) (p :reach))
        fraction (fx/mix (p :min) (p :max) (sine (fx/subtract (fx/divide beat (p :beats)) 0.25)))
        outside (fx/subtract (op :abs (fx/subtract distance fraction)) (fx/multiply 0.5 (p :width)))]
    (fx/clamp (fx/subtract 1.0 (fx/divide outside (fx/add (fx/multiply (p :halo) (p :width)) 1.0e-4))))))

(def ^:private bloom-params
  {:beats (number 8.0 0.25 64.0) :min (number 0.0 0.0 1.5) :max (number 1.0 0.0 1.5)
   :width (number 0.3 0.0 1.0) :halo (number 0.5 0.0 2.0) :center-x (number 0.0 -20.0 20.0)
   :center-y (number 0.3 -20.0 20.0) :reach (number 4.8 0.5 40.0)})

(def ^:private previous-color {:op :previous :attribute :color :default [0.0 0.0 0.0]})

(defn- filter-layer [operator]
  {:transforms [{:attribute :color :value {:op operator :args [previous-color {:op :incoming :attribute :color}]}}]})

(defn- hash3
  "Dave Hoskins' hash without sine, as in Tolglow #920's darkening sparkle."
  [a b c]
  (let [h (fract (fx/add (fx/multiply a 0.1031) (fx/multiply b 0.11369) (fx/multiply c 0.13787)))]
    (fract (fx/multiply 7.13 h (fx/add h 33.33)))))

(defn- sparkle-level
  "A seeded sparkle: each pixel has `:chance` of a spark per sixteenth, fading
  over `:fade` beats. The same expression as #920's `sparkle-shade`."
  []
  (let [time (fx/multiply beat 4.0)
        slot (floor time)
        within (fract time)
        head-hash (fx/add (fx/multiply x 12.9898) (fx/multiply y 78.233) (fx/multiply z 37.719))
        spark (fn [age]
                (let [cell (fx/subtract slot age)
                      cell (fx/subtract cell (fx/multiply 4096.0 (floor (fx/divide cell 4096.0))))
                      drawn (hash3 head-hash (fx/add cell 0.5) (fx/add (p :seed) 0.25))
                      lit (floor (fx/clamp (fx/add 1.0 (fx/subtract (p :chance) drawn)) 0.0 1.0))
                      beats (fx/divide (fx/add within age) 4.0)]
                  (fx/multiply lit (fx/clamp (fx/subtract 1.0 (fx/divide beats (p :fade)))))))]
    (apply op :max (map (comp spark double) (range 6)))))

(defn- grade [level]
  (fx/rgb (fx/multiply (p :dim) level) (fx/multiply (p :desaturate) level) (fract (fx/multiply (p :hue-shift) level))))

(def ^:private shade-params
  {:dim (number 0.6 0.0 1.0) :desaturate (number 0.3 0.0 1.0) :hue-shift (number 0.1 -0.5 0.5)})

(defn- definitions [targets]
  (let [mask (fx/mix (fx/subtract 1.0 (p :keyhole-opacity)) 1.0 (bloom-band))]
    {:wash {:id :wash :version 1 :targets targets :priority 10 :params {}
            :outputs {:color (fx/clamp wash-color)}}
     ;; As tolglow.showcase.looks/bloom-keyhole.
     :keyhole {:id :keyhole :version 1 :targets targets :priority 50
               :params (assoc bloom-params :keyhole-opacity (number 0.85 0.0 1.0))
               :layer {:transforms [{:attribute :color
                                     :value {:op :multiply :args [previous-color {:op :incoming :attribute :color}]}}]}
               :outputs {:color (fx/clamp (fx/rgb mask mask mask))}}
     ;; As the composition hue transform, without writing colour of its own.
     :rotate-hue {:id :rotate-hue :version 1 :targets targets :priority 50 :params {}
                  :layer {:transforms [{:attribute :color :value {:op :rotate-hue :args [previous-color 1/6]}}]}}
     ;; #920's shading bloom.
     :shade {:id :shade :version 1 :targets targets :priority 50 :params (merge bloom-params shade-params)
             :layer (filter-layer :color-grade) :outputs {:color (grade (bloom-band))}}
     ;; #920's colour guard with a restricted palette.
     :guard {:id :guard :version 1 :targets targets :priority 50
             :params {:min-saturation (number 0.55 0.0 1.0) :hue-center (number 0.08 0.0 1.0)
                      :hue-restriction (number 0.6 0.0 1.0)}
             :layer (filter-layer :color-guard)
             :outputs {:color (fx/rgb (p :min-saturation) (p :hue-restriction) (p :hue-center))}}
     ;; #920's darkening sparkle, above the guard.
     :sparkle {:id :sparkle :version 1 :targets targets :priority 60
               :params (merge {:chance (number 0.15 0.0 1.0) :fade (number 0.75 0.1 1.5) :seed (number 656.0 0.0 9999.0)}
                              shade-params {:dim (number 0.85 0.0 1.0)})
               :layer (filter-layer :color-grade) :outputs {:color (grade (sparkle-level))}}}))

(def cases
  "Each case's cues, launched in order."
  {:wash [:wash]
   :multiply [:wash :keyhole]
   :rotate-hue [:wash :rotate-hue]
   :color-grade [:wash :shade]
   :color-guard [:wash :guard]
   :guard-sparkle [:wash :guard :sparkle]})

(defn- launch [model id]
  (let [{:keys [model ack]} (control/apply-command model {:op :cue/launch :cue-id id :command-id (name id)
                                                         :source :benchmark :time-ms 0
                                                         :show-id (:show-id model) :epoch (:epoch model)})]
    (when-not (= :accepted (:status ack))
      (throw (ex-info "Benchmark cue launch failed" {:cue-id id :ack ack})))
    model))

(defn- make-show [fixtures effects cues]
  (reduce launch
          (reduce (fn [model {:keys [id priority] :as effect}]
                    (-> model
                        (assoc-in [:registry :effects id] (dissoc effect :priority))
                        (assoc-in [:registry :cues id] {:id id :effect-id id :priority priority :version 1})))
                  (control/new-show {:show-id "pixel-modifiers" :rig {:fixtures fixtures}
                                     :transport {:beat 0 :bpm 120 :playing? true}})
                  (vals effects))
          cues))

;; As tolglow.bench-movers/allocation-reader, which needs the benchmark profile.
(defn- allocation-reader
  "Aggregate allocated bytes of this thread and the show's effect workers."
  []
  (let [bean (ManagementFactory/getThreadMXBean)
        ids (filterv (fn [id] (let [info (.getThreadInfo bean id)]
                               (and info (or (= id (.getId (Thread/currentThread)))
                                             (str/starts-with? (.getThreadName info) "afterglow-runtime-")))))
                     (.getAllThreadIds bean))]
    (when (and (instance? com.sun.management.ThreadMXBean bean)
               (.isThreadAllocatedMemoryEnabled ^com.sun.management.ThreadMXBean bean))
      #(reduce + (map (fn [id] (max 0 (.getThreadAllocatedBytes ^com.sun.management.ThreadMXBean bean id))) ids)))))

(defn- packets-equal? [a b]
  (and (seq a) (= (count a) (count b))
       (every? true? (map #(Arrays/equals ^bytes %1 ^bytes %2) a b))))

(defn- measure-frame [runtime cues i allocated]
  (let [bytes-before (when allocated (allocated))
        start (System/nanoTime)
        _ (show/tick! runtime (* 25.0 (inc i)))
        elapsed (/ (- (System/nanoTime) start) 1e6)
        bytes (when allocated (- (allocated) bytes-before))
        {:keys [model output status]} @(:state runtime)]
    (when (or (:error status) (:output-error status) (not= (count cues) (count (:instances model)))
              (some :fault (vals (:instances model))) (empty? (:universes output)))
      (throw (ex-info "Pixel modifier benchmark frame failed"
                      {:frame i :status status :faults (keep :fault (vals (:instances model)))})))
    {:ms elapsed :bytes bytes :packets (deref (get-in runtime [:sink :latest]))}))

(defn- lanes [general?]
  (into (vec (keys cases))
        (when general? (map #(keyword (str (name %) "-general")) (remove #{:wash} (keys cases))))))

(defn- case-of [lane] (keyword (str/replace (name lane) #"-general$" "")))

(defn- sample [runtimes lane-order i allocated]
  (let [order (take (count lane-order) (drop (mod i (count lane-order)) (cycle lane-order)))
        frames (into {} (map (fn [lane] [lane (measure-frame (runtimes lane) (cases (case-of lane)) i allocated)])) order)
        wash (get-in frames [:wash :packets])]
    (doseq [lane lane-order :when (not= :wash lane)
            :let [packets (get-in frames [lane :packets])]]
      (when (packets-equal? wash packets)
        (throw (ex-info "A modifier left the wash unchanged" {:frame i :lane lane})))
      (when (not= lane (case-of lane))
        (when-not (packets-equal? packets (get-in frames [(case-of lane) :packets]))
          (throw (ex-info "The typed and general native layer paths produced different DMX" {:frame i :lane lane})))))
    (update-vals frames #(dissoc % :packets))))

(defn- summarize [rows]
  (cond-> {:ms (bench/distribution (map :ms rows))
           :over-25-ms (count (filter #(> (:ms %) 25.0) rows))}
    (:bytes (first rows)) (assoc :bytes (bench/distribution (map :bytes rows)))))

(defn profile
  "Interleave every case for `warmup` then `samples` frames. With `general?`
  each modifier case also runs on the general native layer path."
  ([warmup samples] (profile warmup samples 2048 true))
  ([warmup samples n general?]
   (let [fixtures (pixels n)
         effects (definitions (mapv :id fixtures))
         lane-order (lanes general?)
         runtimes (volatile! {})]
     (try
       (doseq [lane lane-order]
         (let [cues (cases (case-of lane))
               runtime (show/create-runtime (cond-> {:model (make-show fixtures effects cues) :workers 2 :batch? true
                                                     :sink (bench/capture-sink)}
                                              (not= lane (case-of lane)) (assoc :typed-layers? false)))]
           (vswap! runtimes assoc lane runtime)
           (run! (:compile runtime) (map #(dissoc (effects %) :priority) cues))))
       (dotimes [i warmup] (sample @runtimes lane-order i nil))
       (let [allocated (allocation-reader)
             rows (mapv #(sample @runtimes lane-order (+ warmup %) allocated) (range samples))
             results (into {} (map (fn [lane] [lane (summarize (map lane rows))])) lane-order)
             wash (get-in results [:wash :ms :p50])]
         {:results results
          :added-median-ms (into {} (map (fn [lane] [lane (- (get-in results [lane :ms :p50]) wash)]))
                                 (remove #{:wash} lane-order))})
       (finally (run! show/stop! (vals @runtimes)))))))

(defn- load-average []
  (let [bean (ManagementFactory/getOperatingSystemMXBean)]
    (.getSystemLoadAverage bean)))

(defn -main [& args]
  (let [[warmup samples n] (if (empty? args) [100 300 2048] (mapv parse-long args))
        n (or n 2048)]
    (when-not (and (#{0 2 3} (count args)) (pos-int? warmup) (pos-int? samples) (pos-int? n))
      (throw (ex-info "Use optional positive warmup and sample counts, then an optional pixel count" {:args args})))
    (try
      (let [load-before (load-average)
            result (profile warmup samples n true)]
        (prn (merge {:tolglow (bench/revision ".")
                     :afterglow (bench/revision (or (System/getenv "AFTERGLOW_ROOT") "../afterglow"))
                     :java (System/getProperty "java.version") :vm (System/getProperty "java.vm.name")
                     :cpu (bench/cpu-model) :os (str (System/getProperty "os.name") " " (System/getProperty "os.version"))
                     :processors (.availableProcessors (Runtime/getRuntime)) :max-heap (.maxMemory (Runtime/getRuntime))
                     :load-average {:before load-before :after (load-average)}
                     :pixels n :workers 2 :warmup warmup :samples samples}
                    result)))
      (finally (shutdown-agents)))))
