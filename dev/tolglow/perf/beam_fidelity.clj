(ns tolglow.perf.beam-fidelity
  "Compare emitted pan/tilt and the projected spotlight under different sampling intervals."
  (:require [afterglow.control.model :as control]
            [afterglow.render.frame :as frame]
            [afterglow.render.optics :as optics]
            [clojure.java.io :as io]
            [tolglow.bench-show :as bench]
            [tolglow.perf.multirate-metrics :as metrics]
            [tolglow.perf.beam-admission :as admission]
            [tolglow.show :as show]))

(def ^:private ground {:id :floor :point [0 0 0] :normal [0 1 0] :u-axis [1 0 0]})

(def ^:private warmup 40)

(def ^:private frames 160)

(def ^:private frame-ms 25)

(def ^:private bpm 120)

(def ^:private groups [[:near 5.0] [:near 40.0] [:far 5.0] [:far 40.0]])

(defn- group-for [i] (nth groups (mod i (count groups))))

(defn- op [name & args] {:op name :args (vec args)})

(defn- rig []
  (let [fixtures (:fixtures (bench/corpus 0 64 :sculpture))]
    (mapv (fn [i f]
            (let [cohort (quot i 4)]
              ;; Every four fixtures use the same phase at different lens widths and target ranges.
              (assoc f :position [(* 1.3 (mod cohort 4)) 6.0 (+ (* 2.0 (quot cohort 4)) (* 0.32 (mod i 4)))]
                     :lens-radius-m 0.005 :beam-angle (second (group-for i)) :rotation-deg [0 0 0])))
          (range) fixtures)))

(defn- program [distance]
  (let [axis (fn [k] {:op :position :axis k})
        phase (op :multiply 0.25 (axis :x))
        orbit (fn [shape speed radius]
                (op :multiply radius (op shape (op :add (op :multiply speed {:op :beat}) phase))))]
    (op :vec3 (op :add (axis :x) (orbit :sin 5.0 0.55))
        (op :subtract (axis :y) distance) (op :add (axis :z) (orbit :cos 4.0 0.32)))))

(defn- model [fixtures stride target]
  (reduce
   (fn [model [group distance]]
     (let [targets (keep-indexed (fn [i f] (when (= group (first (group-for i))) (:id f))) fixtures)
           effect (cond-> {:id group :version 1 :targets (vec targets)
                           :outputs {:aim (target distance) :dimmer 1.0 :color [1 1 1]}}
                    (> stride 1) (assoc :keyframes {:interval {:ms (* frame-ms stride)}}))]
       (-> model
           (bench/command [group :define] {:op :registry/publish :kind :effects :definition effect})
           (bench/command [group :start] {:op :effect/launch :effect-id group}))))
   (control/new-show {:rig {:fixtures fixtures}}) [[:near 0.7] [:far 6.0]]))

(defn- channel [universes fixture slot]
  (let [offset (+ (dec (:address fixture)) (dec slot))
        universe (+ (:universe fixture) (quot offset 512))]
    (bit-and 255 (aget ^bytes (get universes universe) (mod offset 512)))))

(defn- axis-bytes [universes fixture axis]
  (mapv #(channel universes fixture %) (get-in fixture [:channels axis])))

(def ^:private calibration-levels [0 (/ 255 65535) (/ 256 65535) (/ 1 255) 1/2 1])

(def ^:private axis-encodings
  {[[0 0] [0 253] [0 254] [1 0] [127 128] [255 0]]
   {:name :fractional-255 :coarse-weight 255 :max-code 65025}
   [[0 0] [0 255] [1 0] [1 1] [128 0] [255 255]]
   {:name :unsigned-16 :coarse-weight 256 :max-code 65535}})

(defn- decode-axis [{:keys [coarse-weight max-code]} [coarse fine]]
  (/ (+ (* coarse-weight coarse) fine) (double max-code)))

(defn- calibrated-encoding []
  ;; The pan pair straddles universes. Raw byte expectations come from each
  ;; published channel contract, independently of this replay's decoder.
  (let [fixture {:id :probe :universe 1 :address 512 :channels {:pan [1 2] :tilt [3 4]}}
        patch (frame/compile-patch [fixture])
        probes (mapv (fn [value]
                       (let [rows (mapv (fn [axis] {:target-id :probe :attribute axis :value (double value)})
                                        [:pan :tilt])
                             raw (:universes (frame/resolve-frame patch rows))]
                         (mapv #(axis-bytes raw fixture %) [:pan :tilt]))) calibration-levels)
        bytes (mapv first probes)
        encoding (axis-encodings bytes)]
    (when-not (and encoding (= bytes (mapv second probes)))
      (throw (ex-info "Unrecognized pan/tilt coarse and fine encoding" {:levels calibration-levels :probes probes})))
    (doseq [[level pair] (map vector calibration-levels bytes)]
      (let [decoded (decode-axis encoding pair)]
        (when (> (abs (- decoded (double level))) (+ (/ 0.5 (:max-code encoding)) 1e-12))
          (throw (ex-info "Decoded axis exceeds half a quantization step"
                          {:encoding (:name encoding) :level level :bytes pair :decoded decoded})))))
    (assoc encoding :levels calibration-levels :wire-bytes bytes)))

(defn- observed-beam [encoding universes fixture]
  (let [pose (frame/fixture-pose fixture {:pan (decode-axis encoding (axis-bytes universes fixture :pan))
                                         :tilt (decode-axis encoding (axis-bytes universes fixture :tilt))})
        beam (optics/fixture-beam fixture {:pose pose})
        area (optics/beam-footprint beam ground)]
    (when-not area (throw (ex-info "Fixture beam missed the benchmark floor" {:fixture (:id fixture) :pose pose})))
    {:direction (:direction beam) :beam-angle (:beam-angle beam) :area area}))

(defn- replay [encoding fixtures stride target]
  (let [runtime (show/create-runtime {:model (model fixtures stride target) :batch? true :workers 2 :worker-timeout-ms 10000
                                      :adaptive-keyframes {:enabled? false} :workspace-options nil})]
    (try
      (into [] (keep (fn [i]
                       (show/tick! runtime (* frame-ms i))
                       (let [{:keys [status fault-events output]} @(:state runtime)]
                         (when (or (:error status) (seq fault-events))
                           (throw (ex-info "Moving-head replay failed" {:frame i :status status :faults fault-events})))
                         (when (>= i warmup)
                           (mapv (partial observed-beam encoding (:universes output)) fixtures)))))
            (range (+ warmup frames)))
      (finally (show/stop! runtime)))))

(defn- displacement [expected actual]
  (let [degrees (metrics/angle [0 0 0] (:direction expected) (:direction actual))
        {:keys [center major-axis minor-axis major-radius-m minor-radius-m]} (:area expected)
        destination (get-in actual [:area :center])
        delta (mapv - destination center)
        along (fn [axis radius] (/ (reduce + (map * axis delta)) radius))]
    {:degrees degrees :beam-radii (/ degrees (* 0.5 (:beam-angle expected)))
     :floor-metres (metrics/distance center destination)
     :footprint-radii (Math/hypot (along major-axis major-radius-m) (along minor-axis minor-radius-m))}))

(defn- summarize [rows]
  (into {} (for [field [:degrees :beam-radii :floor-metres :footprint-radii]]
             [field (bench/distribution (map field rows))])))

(defn- compare-frames [reference candidate]
  (let [deltas (for [[i [r c]] (map-indexed vector (map vector reference candidate))
                     j (range (count r))]
                 {:group (group-for j) :frame i :error (displacement (r j) (c j))})]
    (into {}
          (for [[g rows] (group-by :group deltas)]
            [g {:all (summarize (map :error rows))
                :first-bar (summarize (keep #(when (< (:frame %) 80) (:error %)) rows))
                :second-bar (summarize (keep #(when (>= (:frame %) 80) (:error %)) rows))}]))))

(defn- trace [samples]
  (mapv (fn [row] (mapv #(select-keys (:area %) [:center :major-axis :major-radius-m :minor-radius-m])
                          (subvec row 0 4))) samples))

(defn- compare-calibrated [lanes limit]
  (into {}
        (for [g groups]
          (let [below? #(<= (get-in lanes [% :errors g :first-bar :footprint-radii :max]) limit)
                stride (last (filter below? [1 2 4 8]))]
            [g {:stride stride :first-bar-max-radii (get-in lanes [stride :errors g :first-bar :footprint-radii :max])
                :second-bar-max-radii (get-in lanes [stride :errors g :second-bar :footprint-radii :max])}]))))

(defn- fast-orbit [depth]
  (let [axis (fn [k] {:op :position :axis k})
        swing (op :multiply 0.55 (op :sin (op :multiply (* 10 Math/PI) {:op :beat})))]
    (op :vec3 (op :add (axis :x) swing) (op :subtract (axis :y) depth)
        (op :add (axis :z) 0.25))))

(defn- midpoint-comparison [encoding fixtures limit]
  ;; At 120 BPM this continuous orbit completes one cycle in four output frames.
  ;; Sample the same first four fixtures used by the footprint recording.
  (let [reference (replay encoding fixtures 1 fast-orbit)
        candidate (replay encoding fixtures 4 fast-orbit)]
    (into {}
          (for [j (range 4)]
            (let [radii (map #(get (displacement (nth %1 j) (nth %2 j)) :footprint-radii)
                             reference candidate)
                  windows (mapv (fn [r] {:midpoint (nth r 2) :worst (reduce max r)}) (partition 4 radii))]
              [(nth groups j) {:windows (count windows)
                               :worst-midpoint (reduce max (map :midpoint windows))
                               :worst-frame (reduce max (map :worst windows))
                               :false-admissions (count (filter #(and (<= (:midpoint %) limit)
                                                                      (> (:worst %) limit)) windows))}])))))

(defn- check-upper [plans stride measured]
  (let [upper (reduce max (map #(admission/max-error % stride (* frame-ms (/ bpm 60000.0))) plans))]
    (when (or (not (Double/isFinite upper)) (> measured (+ upper 1e-9)))
      (throw (ex-info "Beam curvature bound failed on the measured replay"
                      {:stride stride :upper upper :observed measured})))
    {:upper-radii upper :worst-observed-radii measured}))

(defn- compare-curvature [fixtures encoding lanes alias-results limit]
  (into {}
        (for [[depth-class _ :as group] groups]
          (let [depth (if (= :near depth-class) 0.7 6.0)
                heads (keep-indexed #(when (= group (group-for %1)) %2) fixtures)
                plans (fn [program] (mapv #(admission/prepare (program depth) % encoding) heads))
                orbit (plans program)
                strides (into (sorted-map)
                              (for [stride [1 2 4 8]]
                                [stride (check-upper orbit stride
                                          (get-in lanes [stride :errors group :all :footprint-radii :max]))]))
                allowed (keep (fn [[stride result]] (when (<= (:upper-radii result) limit) stride)) strides)]
            [group {:stride (last allowed) :candidates strides
                    :fast-orbit (check-upper (plans fast-orbit) 4 (get-in alias-results [group :worst-frame]))}]))))

(defn measure [limit]
  (let [encoding (calibrated-encoding) fixtures (rig) reference (replay encoding fixtures 1 program)
        alias-results (midpoint-comparison encoding fixtures limit)
        lanes (into {} (for [stride [1 2 4 8]]
                         (let [candidate (replay encoding fixtures stride program)]
                           [stride (cond-> {:errors (compare-frames reference candidate)}
                                     (= stride 4) (assoc :footprints (trace candidate)))])))]
    (doseq [g groups]
      (when-not (zero? (get-in lanes [1 :errors g :all :footprint-radii :max]))
        (throw (ex-info "Full-rate DMX movement replay diverged" {:group g}))))
    (let [p95 #(get-in lanes [4 :errors % :all :footprint-radii :p95])]
      (when-not (and (pos? (p95 [:far 40.0]))
                     (> (p95 [:near 5.0]) (p95 [:near 40.0]) (p95 [:far 40.0])))
        (throw (ex-info "Fixture optics or the moving target did not affect the replay" {}))))
    {:movers (count fixtures) :frames frames :bpm bpm :period-ms frame-ms :groups groups
     :axis-encoding encoding :radius-budget limit :midpoint-comparison alias-results
     :curvature-admission (compare-curvature fixtures encoding lanes alias-results limit)
     :first-bar-policy (compare-calibrated lanes limit)
     :fixture-positions (mapv :position (subvec fixtures 0 4))
     :reference-footprints (trace reference) :lanes lanes}))

(defn -main [output & [radius-budget]]
  (let [limit (parse-double (or radius-budget "0.25"))]
    (when-not (and limit (Double/isFinite limit) (pos? limit))
      (throw (ex-info "Provide a positive finite fraction of the reference footprint radius" {})))
    (try
      (let [fork (io/file (or (System/getenv "AFTERGLOW_ROOT") "../afterglow"))
            result (cond-> (measure limit)
                     (.exists (io/file fork ".git")) (assoc :afterglow-revision (bench/revision fork)))]
        (io/make-parents output)
        (spit output (str (pr-str result) "\n"))
        (prn (select-keys result [:movers :frames :afterglow-revision :axis-encoding :radius-budget :first-bar-policy :midpoint-comparison])))
      (finally (shutdown-agents)))))
