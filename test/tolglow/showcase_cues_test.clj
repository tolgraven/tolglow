(ns tolglow.showcase-cues-test
  "The showcase cues mined from tolglow-web (#656): searchlight drift, twin
  audio cones, the one-control Run macro and the matrix chase. Each runs on a
  real show with a manual clock and a capture sink. The tests read the frame
  output at known beats, stop and restart the transport, change the seed and
  feed the cones a deterministic bass passage through the real capture path."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.audio-lfo :as audio-lfo]
            [tolglow.audio.fixtures :as fixtures]
            [tolglow.audio.shape :as shape]
            [tolglow.connections :as connections]
            [tolglow.show :as show]
            [tolglow.showcase :as showcase]
            [tolglow.showcase.looks :as looks]
            [tolglow.showcase.rig :as rig]
            [tolglow.showcase.timeline :as timeline]
            [tolglow.visualization.parameter-views :as parameter-views]))

(def ^:private beat-ms (/ 60000.0 showcase/bpm))

(defn- capture-sink [frames]
  {:description {:type :capture :network false}
   :send! (fn [universes _]
            (swap! frames conj (into (sorted-map) (map (fn [[universe data]] [universe (mapv #(bit-and 255 %) data)]))
                                     universes)))
   :close! (fn [])})

(defn- session
  "Launch `cue` with `params` on tick 0 of a showcase show and tick every
  `frame-ms` until `end-ms`. `commands` maps a tick time to payloads submitted
  just before that tick. Returns one captured DMX frame and one row per tick:
  {:t :beat :values}, where values are the committed output rows."
  [{:keys [cue params seed frame-ms end-ms commands] :or {params {} seed showcase/default-seed frame-ms 25}}]
  (let [frames (atom [])
        runtime (show/create-runtime {:model (showcase/model {:seed seed}) :workspace-options nil
                                      :worker-timeout-ms 60000 :sink (capture-sink frames)})]
    (try
      (show/submit! runtime {:op :cue/launch :cue-id cue :launch {:params params}})
      (let [rows (mapv (fn [t]
                         (doseq [payload (get commands t)] (show/submit! runtime payload))
                         (show/tick! runtime t)
                         (let [snapshot (show/snapshot runtime)]
                           (when-let [fault (some :fault (:instances snapshot))]
                             (throw (ex-info "A showcase cue faulted" {:fault fault})))
                           {:t t :beat (get-in snapshot [:transport :beat]) :values (get-in snapshot [:output :values])}))
                       (range 0 end-ms frame-ms))]
        {:frames @frames :rows rows})
      (finally (show/stop! runtime)))))

(defn- after-beat
  "Rows from `beat` on, past the cue's attack."
  [rows beat]
  (filter #(>= (:beat %) beat) rows))

(defn- close? [tolerance a b]
  (if (number? a)
    (<= (Math/abs (- (double a) (double b))) tolerance)
    (and (= (count a) (count b)) (every? true? (map (partial close? tolerance) a b)))))

(defn- distance [a b] (Math/sqrt (reduce + (map #(let [d (- %1 %2)] (* d d)) a b))))

(def ^:private new-cues [:searchlight-drift :twin-cones :run :matrix-chase])

;; ------------------------------------------------------------------ parameters

(deftest the-new-cues-declare-typed-bounded-parameters-for-their-widgets
  (let [model (showcase/model)
        schema (fn [cue param] (get-in model [:registry :effects cue :params param]))
        widget (fn [cue param] (parameter-views/resting-view (schema cue param)))]
    (is (= looks/after-hours-cues (set new-cues)))
    (doseq [cue new-cues
            [param {:keys [type bounds] :as s}] (get-in model [:registry :effects cue :params])]
      (testing (str cue " " param)
        (is (string? (:name s)) "every parameter has a label")
        (when (#{:number :integer :vector} type)
          (is (vector? bounds) "every numeric parameter has hard bounds"))))
    (testing "musical lengths are beats and bars, distances metres, angles degrees"
      (is (= :beats (:unit (schema :searchlight-drift :min-beats)) (:unit (schema :matrix-chase :step))))
      (is (= :beat-ratio (:semantic/type (schema :matrix-chase :step))))
      (is (= :bars (:unit (schema :twin-cones :bars)) (:unit (schema :twin-cones :sway-bars))))
      (is (= :m (:unit (schema :searchlight-drift :center)) (:unit (schema :twin-cones :radius))))
      (is (= :deg (:unit (schema :matrix-chase :angle))))
      (is (= :pressure (:control/axis (schema :run :drive)) (:control/axis (schema :twin-cones :energy)))))
    (testing "each parameter opens the widget of its type"
      (is (= :spatial (widget :searchlight-drift :center)))
      (is (= :color (widget :searchlight-drift :tint) (widget :twin-cones :tint-a) (widget :matrix-chase :palette)))
      (is (= :gradient (:type (schema :matrix-chase :palette))))
      (is (= :controls (widget :run :drive) (widget :matrix-chase :angle))))))

;; ------------------------------------------------------------- frame output

(deftest searchlight-drift-wanders-inside-its-box-settles-and-follows-its-seed
  (let [{:keys [frames rows]} (session {:cue :searchlight-drift :end-ms 15000})
        rows (vec (after-beat rows 0.5))
        aims (fn [beam] (mapv #(get-in % [:values beam :aim]) rows))
        center [0.0 0.8 1.6] extent [4.2 0.8 2.2]]
    (doseq [beam rig/beams]
      (testing (name beam)
        (is (every? (fn [aim] (every? true? (map #(<= (Math/abs (- %1 %2)) (+ %3 1.0e-9)) aim center extent)))
                    (aims beam))
            "every aim point stays in centre +/- extent")
        (is (< (reduce max (map distance (aims beam) (rest (aims beam)))) 0.6)
            "a new move starts from where the last one settled, so the aim never jumps")
        (is (some zero? (map distance (aims beam) (rest (aims beam)))) "it settles between moves")))
    (testing "every head keeps its own move lengths and phase"
      (let [moving (fn [beam] (mapv #(pos? (distance %1 %2)) (aims beam) (rest (aims beam))))]
        (is (= (count rig/beams) (count (distinct (map moving rig/beams)))))))
    (testing "the seed decides the wander"
      (let [again (session {:cue :searchlight-drift :end-ms 2500})
            other (session {:cue :searchlight-drift :end-ms 2500 :seed 7})]
        (is (= (subvec frames 0 100) (:frames again)))
        (is (not= (:frames again) (:frames other)))))))

(defn- angle-around [[x _ z] cx depth] (Math/atan2 (- z depth) (- x cx)))

(defn- unwrap [delta] (cond (> delta Math/PI) (- delta (* 2 Math/PI)) (< delta (- Math/PI)) (+ delta (* 2 Math/PI)) :else delta))

(deftest twin-cones-kick-on-the-beat-counter-rotate-and-sway-in-opposite-tints
  (testing "with no audio follow the brightness is a kick that decays from every beat"
    (let [{:keys [rows]} (session {:cue :twin-cones :params {:follow 0.0} :end-ms 4000})]
      (doseq [{:keys [beat values]} (after-beat rows 2.0)]
        (is (close? 1.0e-9 (+ 0.15 (* 0.85 (Math/exp (* -3.0 (- beat (Math/floor beat))))))
                    (get-in values [:mover-1 :intensity]))
            (str "beat " beat)))))
  (testing "with full follow it is the energy alone"
    (let [{:keys [rows]} (session {:cue :twin-cones :params {:follow 1.0 :energy 0.3} :end-ms 2000})]
      (is (every? #(close? 1.0e-9 0.405 (get-in % [:values :mover-3 :intensity])) (after-beat rows 2.0)))))
  (let [{:keys [rows]} (session {:cue :twin-cones :params {:follow 1.0 :energy 1.0} :end-ms 6000})
        rows (vec (after-beat rows 2.0))
        turn (fn [mover cx] (let [angles (mapv #(angle-around (get-in % [:values mover :aim]) cx 0.4) rows)]
                              (reduce + (map #(unwrap (- %2 %1)) angles (rest angles)))))]
    (testing "the stage-left pair turns one way and the stage-right pair the other"
      (is (every? neg? [(turn :mover-1 -1.8) (turn :mover-2 -1.8)]))
      (is (every? pos? [(turn :mover-3 1.8) (turn :mover-4 1.8)]))
      (let [beats (- (:beat (peek rows)) (:beat (first rows)))]
        (is (every? #(close? 1.0e-6 (* 2.0 Math/PI (/ beats 8.0)) (Math/abs %)) (map turn rig/movers [-1.8 -1.8 1.8 1.8]))
            "a two-bar turn is one circle every eight beats")))
    (testing "the two sides sway between the tints in opposite phase"
      (is (every? #(close? 1.0e-9 [1.0 0.45 1.55] (mapv + (get-in % [:values :mover-1 :emitted-color])
                                                        (get-in % [:values :mover-4 :emitted-color])))
                  rows)))))

(defn- lit? [row id] (some #(> % 1.0e-6) (get-in row [:values id :emitted-color])))

(deftest run-drives-level-hue-whiteness-and-a-beat-locked-strobe-from-one-control
  (let [play (fn [drive] (vec (after-beat (:rows (session {:cue :run :params {:drive drive} :frame-ms 5 :end-ms 4000}))
                                          3.9)))
        dim (play 0.0) middle (play 0.5) full (play 1.0)
        intensity (fn [rows] (get-in (first rows) [:values :wash-1 :intensity]))
        colour (fn [rows id] (get-in (first (filter #(lit? % id) rows)) [:values id :emitted-color]))
        whiteness (fn [[r g b]] (/ (min r g b) (max r g b)))
        rising (fn [rows] (count (filter true? (map #(and (not (lit? %1 :wash-1)) (lit? %2 :wash-1)) rows (rest rows)))))
        four-beats (fn [rows] (filterv #(< (:beat %) 8.0) rows))]
    (testing "the level climbs from the floor"
      (is (close? 1.0e-9 0.2 (intensity dim)))
      (is (close? 1.0e-9 (+ 0.2 (* 0.8 (Math/sqrt 0.5))) (intensity middle)))
      (is (close? 1.0e-9 1.0 (intensity full))))
    (testing "the hue fans out across the room as drive rises"
      (is (= (colour dim :wash-1) (colour dim :wash-2)))
      (is (not (close? 0.05 (colour middle :wash-1) (colour middle :wash-2)))))
    (testing "the colour whitens"
      (is (< (whiteness (colour dim :wash-3)) 0.01))
      (is (< 0.7 (whiteness (colour full :wash-3)))))
    (testing "below the strobe threshold nothing flashes; at full drive four flashes a beat"
      (is (every? #(lit? % :wash-1) dim))
      (is (every? #(lit? % :wash-1) middle))
      (is (= 16 (rising (four-beats full)))))))

(defn- column-levels [row]
  (mapv (fn [tube] (mapv #(reduce max (get-in row [:values (keyword (str "tube-" tube "-head-" %)) :emitted-color] [0.0]))
                         (range 1 11)))
        (range 1 9)))

(deftest matrix-chase-steps-whole-columns-or-rows-one-cell-per-step
  (let [steps (fn [rows] (->> rows (partition-by #(Math/floor (/ (:beat %) 0.5))) (map first) (drop 1) (take 8)))
        lit-columns (fn [row] (set (keep-indexed (fn [i column] (when (pos? (reduce max column)) i)) (column-levels row))))]
    (testing "at 0 degrees whole columns light, one column further each step"
      (let [chase (steps (:rows (session {:cue :matrix-chase :params {:tail 1.0 :spacing 4.0} :end-ms 3000})))]
        (doseq [row chase]
          (is (every? #(apply = (map pos? %)) (column-levels row)) "a column is all on or all off")
          (is (= 2 (count (lit-columns row))) "one column in every four of eight"))
        (is (every? (fn [[a b]] (= (set (map #(mod (inc %) 8) (lit-columns a))) (lit-columns b)))
                    (partition 2 1 chase)))))
    (testing "at 90 degrees whole rows light"
      (let [row (first (steps (:rows (session {:cue :matrix-chase :params {:angle 90.0 :tail 1.0} :end-ms 1500}))))
            rows-lit (apply map vector (column-levels row))]
        (is (every? (fn [row] (every? #(close? 1.0e-9 (first row) %) row)) rows-lit) "a row is the same on every tube")
        (is (every? #{4} (let [lit (keep-indexed #(when (pos? (first %2)) %1) rows-lit)] (map - (rest lit) lit)))
            "lit rows are four apart")))
    (testing "the palette colours the rows bottom to top"
      (let [row (first (steps (:rows (session {:cue :matrix-chase :params {:angle 90.0 :spacing 1.0 :tail 1.0} :end-ms 1500}))))
            [bottom top] (map #(get-in row [:values (keyword (str "tube-1-head-" %)) :emitted-color]) [1 10])]
        (is (> (first bottom) (* 5 (first top))) "red at the bottom")
        (is (> (second top) (* 5 (second bottom))) "cyan at the top")))))

;; --------------------------------------------------------- transport stop

(deftest every-new-cue-holds-on-transport-stop-and-resumes-without-catch-up
  (doseq [cue new-cues]
    (testing (name cue)
      (let [straight (:frames (session {:cue cue :end-ms 3000}))
            paused (:frames (session {:cue cue :end-ms 4000
                                      :commands {1000 [{:op :transport/set :transport {:playing? false}}]
                                                 2000 [{:op :transport/set :transport {:playing? true}}]}}))
            frame (fn [frames t] (nth frames (long (/ t 25))))]
        (is (= 1 (count (distinct (map #(frame paused %) (range 1000 2000 25)))))
            "every frame of the stop is the frame it stopped on")
        (is (= (frame straight 1000) (frame paused 1999)))
        (is (every? #(= (frame straight (- % 1000)) (frame paused %)) (range 2000 4000 25))
            "after play every frame is the one a second earlier without the stop")))))

;; ------------------------------------------------------------- audio follow

(defn- pcm-source [^bytes bytes chunk closed]
  (let [position (atom 0)]
    {:started-at-ms 0.0 :description {:name "Injected passage"}
     :read! (fn [out offset length]
              (let [n (min length chunk (- (alength bytes) @position))]
                (if (zero? n) -1 (do (System/arraycopy bytes @position out offset n) (swap! position + n) n))))
     :close! #(swap! closed inc)}))

(deftest twin-cones-follow-the-bass-bars-of-a-captured-passage
  (let [closed (atom 0)
        bytes (fixtures/pcm16-bytes (fixtures/passage :delay-synth {:bpm (double showcase/bpm) :bars 4 :sample-rate 8000.0}))
        ;; Capture timestamps start at 0 ms and the passage lasts 7.5 s, so
        ;; the show's clock starts at 8 s with beat 0 at 0 ms, as a show that
        ;; has been playing since the passage began.
        start 8000.0
        model (-> (showcase/model) (assoc :time-ms start)
                  (update :transport assoc :time-ms start :beat (/ start beat-ms)))
        runtime (show/create-runtime {:model model :workspace-options nil :worker-timeout-ms 60000
                                      :audio-options {:open-source! (fn [_] (pcm-source bytes 640 closed))}})
        lfo-id (:lfo-id looks/bass-lfo)
        energy (fn [table beat] (shape/table-value table (/ beat 4.0)))
        reading (fn []
                  (let [snapshot (show/snapshot runtime)
                        beat (get-in snapshot [:transport :beat])]
                    {:beat beat :intensity (get-in snapshot [:output :values :mover-1 :intensity])
                     :lfo (get-in @(:state runtime) [:model :audio-lfos :lfos lfo-id])}))]
    (try
      (let [at! (fn [t kind payload] (let [reply (show/submit! runtime kind payload)]
                                       (show/tick! runtime t)
                                       (deref reply 5000 {:status :timeout})))
            launch (at! 8000 :command {:op :cue/launch :cue-id :twin-cones :launch {:params {:follow 1.0}}})
            bound (at! 8025 :modulation-bind {:instance-id (:instance-id launch) :param :energy
                                              :binding (get-in looks/sources [:twin-cones :energy])})]
        (is (= :accepted (:status launch) (:status bound)))
        (doseq [t (range 8050 9000 25)] (show/tick! runtime t))
        (testing "before any audio the LFO rests on a sine and the cones breathe on it"
          (let [{:keys [beat intensity lfo]} (reading)]
            (is (= :waiting (:status lfo)))
            (is (= (audio-lfo/rest-shape :sine 16) (:to lfo)))
            (is (close? 1.0e-9 (+ 0.15 (* 0.85 (energy (:to lfo) beat))) intensity))))
        (let [handle (connections/start-audio! (:connections runtime) {:default? true :sample-rate 8000.0 :block-frames 400}
                                               (show/audio-callbacks runtime))]
          (let [result (deref (:result handle) 5000 ::timeout)]
            (is (= :completed (:status result)) (pr-str result))))
        ;; The owner admits the shape on its next frame and morphs to it over two beats.
        (doseq [t (range 9000 10500 25)] (show/tick! runtime t))
        (testing "a whole bar of bass becomes the shape the cones follow"
          (let [{:keys [beat intensity lfo]} (reading)]
            (is (= :ok (:status lfo)))
            (is (pos? (:commits lfo)))
            (is (not= (audio-lfo/rest-shape :sine 16) (:to lfo)))
            (is (close? 1.0e-9 1.0 (audio-lfo/progress lfo beat)) "the morph has finished")
            (is (close? 1.0e-9 (+ 0.15 (* 0.85 (energy (:to lfo) beat))) intensity))))
        (is (= 1 @closed) "the injected source was closed"))
      (finally (show/stop! runtime)))))

;; ------------------------------------------------------------- the timeline

(deftest the-after-hours-timeline-replays-identically-and-plays-every-new-cue
  (let [probe (fn [snapshot] {:running (into #{} (map :cue-id) (:instances snapshot))
                              :faults (vec (keep :fault (:instances snapshot)))
                              :ramps (into #{} (map (comp :param :destination)) (get-in snapshot [:schedule :ramps]))})
        first-run (showcase/replay {:timeline showcase/after-hours :snapshot! probe})
        second-run (showcase/replay {:timeline showcase/after-hours})
        probes (:snapshots first-run)
        bar-frame (fn [bar] (nth probes (long (Math/round (/ (* bar 4 beat-ms) 25)))))
        status (:status first-run)
        prepared (timeline/prepare showcase/after-hours (showcase/model))]
    (is (empty? (timeline/validate showcase/after-hours (showcase/model))))
    (is (= 1202 (count (:dmx first-run))))
    (is (= (:dmx first-run) (:dmx second-run)) "the same seed gives the same DMX")
    (is (:done? status))
    (is (= (+ (count (timeline/posts prepared 0.0 0)) (count (:direct prepared))) (:accepted status)))
    (is (empty? (:rejected status)))
    (is (empty? (:missed status)))
    (is (empty? (:outcomes status)) "every scheduled event fired and every ramp completed")
    (is (empty? (mapcat :faults probes)))
    (is (every? (reduce into #{} (map :running probes)) new-cues))
    (is (= #{:haze :wash-tide :searchlight-drift :matrix-chase} (:running (bar-frame 1))))
    (is (contains? (:running (bar-frame 3)) :twin-cones))
    (is (= #{:drive :angle} (:ramps (bar-frame 5))))
    (is (contains? (:ramps (bar-frame 9)) :extent) "a vector parameter ramps too")
    (is (contains? (:running (bar-frame 12.1)) :blinder-hit))
    (is (empty? (:running (peek probes))))
    (testing "another seed moves the searchlights elsewhere"
      (is (not= (:dmx (showcase/replay {:timeline showcase/after-hours :to-bar 2 :seed 7}))
                (subvec (:dmx first-run) 0 (count (:dmx (showcase/replay {:timeline showcase/after-hours :to-bar 2})))))))))

(deftest the-application-module-plays-a-named-timeline
  (let [runtime (show/create-runtime {:model (showcase/model) :workspace-options nil :worker-timeout-ms 60000})]
    (try
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown showcase timeline"
                            (showcase/showcase-timeline {:modules {:show runtime}} {:timeline :missing})))
      (let [{:keys [value stop!]} (showcase/showcase-timeline {:modules {:show runtime}} {:timeline :after-hours :loop false})]
        (is (= :after-hours (:timeline (showcase/status (:player value)))))
        (stop!))
      (finally (show/stop! runtime)))))
