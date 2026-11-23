(ns tolglow.perf.beam-fidelity
  "Compare emitted pan/tilt and the projected spotlight under different sampling intervals."
  (:require [afterglow.control.model :as control]
            [afterglow.render.frame :as frame]
            [afterglow.render.optics :as optics]
            [clojure.java.io :as io]
            [tolglow.bench-show :as bench]
            [tolglow.perf.multirate-metrics :as metrics]
            [tolglow.show :as show]))

(def ^:private ground {:id :floor :point [0 0 0] :normal [0 1 0] :u-axis [1 0 0]})
(def ^:private warmup 40)
(def ^:private frames 160)
(def ^:private frame-ms 25)
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

(defn- normalized-axis [universes fixture axis]
  (let [[msb lsb] (get-in fixture [:channels axis])
        coarse (channel universes fixture msb) fine (channel universes fixture lsb)]
    (/ (+ (bit-shift-left coarse 8) fine) 65535.0)))

(defn- observed-beam [universes fixture]
  (let [pose (frame/fixture-pose fixture {:pan (normalized-axis universes fixture :pan)
                                         :tilt (normalized-axis universes fixture :tilt)})
        beam (optics/fixture-beam fixture {:pose pose})
        area (optics/beam-footprint beam ground)]
    (when-not area (throw (ex-info "Fixture beam missed the benchmark floor" {:fixture (:id fixture) :pose pose})))
    {:direction (:direction beam) :beam-angle (:beam-angle beam) :area area}))

(defn- replay [fixtures stride target]
  (let [runtime (show/create-runtime {:model (model fixtures stride target) :batch? true :workers 2 :worker-timeout-ms 10000
                                      :adaptive-keyframes {:enabled? false} :workspace-options nil})]
    (try
      (into [] (keep (fn [i]
                       (show/tick! runtime (* frame-ms i))
                       (let [{:keys [status fault-events output]} @(:state runtime)]
                         (when (or (:error status) (seq fault-events))
                           (throw (ex-info "Moving-head replay failed" {:frame i :status status :faults fault-events})))
                         (when (>= i warmup)
                           (mapv (partial observed-beam (:universes output)) fixtures)))))
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

(defn- midpoint-comparison [fixtures limit]
  ;; At 120 BPM this continuous orbit completes one cycle in four output frames.
  ;; Sample the same first four fixtures used by the footprint recording.
  (let [reference (replay fixtures 1 fast-orbit)
        candidate (replay fixtures 4 fast-orbit)]
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

(defn measure [limit]
  (let [fixtures (rig) reference (replay fixtures 1 program)
        lanes (into {} (for [stride [1 2 4 8]]
                         (let [candidate (replay fixtures stride program)]
                           [stride (cond-> {:errors (compare-frames reference candidate)}
                                     (= stride 4) (assoc :footprints (trace candidate)))])))]
    (doseq [g groups]
      (when-not (zero? (get-in lanes [1 :errors g :all :footprint-radii :max]))
        (throw (ex-info "Full-rate DMX movement replay diverged" {:group g}))))
    (let [p95 #(get-in lanes [4 :errors % :all :footprint-radii :p95])]
      (when-not (and (pos? (p95 [:far 40.0]))
                     (> (p95 [:near 5.0]) (p95 [:near 40.0]) (p95 [:far 40.0])))
        (throw (ex-info "Fixture optics or the moving target did not affect the replay" {}))))
    {:movers (count fixtures) :frames frames :bpm 120 :period-ms frame-ms :groups groups
     :radius-budget limit :midpoint-comparison (midpoint-comparison fixtures limit)
     :first-bar-policy (compare-calibrated lanes limit)
     :fixture-positions (mapv :position (subvec fixtures 0 4))
     :reference-footprints (trace reference) :lanes lanes}))

(defn -main [output & [radius-budget]]
  (let [limit (parse-double (or radius-budget "0.25"))]
    (when-not (and limit (Double/isFinite limit) (pos? limit))
      (throw (ex-info "Provide a positive finite fraction of the reference footprint radius" {})))
    (try
      (let [result (measure limit)]
        (io/make-parents output)
        (spit output (str (pr-str result) "\n"))
        (prn (select-keys result [:movers :frames :radius-budget :first-bar-policy :midpoint-comparison])))
      (finally (shutdown-agents)))))
