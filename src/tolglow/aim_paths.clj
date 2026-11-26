(ns tolglow.aim-paths
  "Portable world-space aim paths compiled into ordinary musical tween effects.
  Drawing and publication are separate: this namespace never runs a fixture."
  (:require [clojure.string :as str]
            [afterglow.render.frame :as frame]
            [afterglow.time :as time]
            [tolglow.compiler :as compiler]
            [tolglow.registry :as registry]
            [tolglow.composition.tween :as tween]))

(def max-points 256)

(def ^:dynamic *committed-values*
  "ID -> committed output row lookup, bound by the runtime owner for one cycle.
  Nil outside an owner cycle, which skips only the start-wrap launch check."
  nil)

(defn- fail! [path message]
  (throw (ex-info message {:errors [{:path path :message message}]})))

(defn- identifier? [id]
  (or (keyword? id) (and (string? id) (not (str/blank? id)))))

(defn- distance [a b]
  (Math/sqrt (reduce + (map (fn [x y] (let [d (- x y)] (* d d))) a b))))

(defn- wire-id [id] (if (keyword? id) (subs (str id) 1) (str id)))

(defn next-version [registry kind id]
  (inc (reduce max (get-in registry [kind id :version] 0)
               (for [[[k entry-id revision] _] (:versions registry)
                     :when (and (= k kind) (= entry-id id))] revision))))

(def ^:private axes [:pan :tilt])

(defn- solve-point [patch fixtures point previous index]
  (let [assignments (mapv (fn [fixture] {:target-id (:id fixture) :attribute :aim :value point}) fixtures)
        values (:values (try (frame/resolve-frame patch assignments {:previous-values previous})
                             (catch clojure.lang.ExceptionInfo cause
                               (fail! [:points index] (.getMessage cause)))))]
    (doseq [[id output] values]
      (when-not (:aim-reachable? output)
        (fail! [:points index] (str "Fixture " id " cannot reach this aim point"))))
    values))

(defn- canonical [x]
  (cond (map? x) (into (sorted-map-by #(compare (pr-str %1) (pr-str %2)))
                       (map (fn [[k v]] [k (canonical v)])) x)
        (sequential? x) (mapv canonical x)
        (number? x) (double x)
        :else x))

(defn head-fingerprint
  "Digest of the mount and calibration a synchronized move was solved against.
  Numbers compare as doubles, so EDN and JSON round trips keep the digest."
  [fixture]
  (let [text (pr-str (canonical (select-keys fixture [:position :rotation-deg :calibration])))
        digest (.digest (java.security.MessageDigest/getInstance "SHA-256") (.getBytes ^String text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and 255 %)) digest))))

(defn- estimate-ms
  "Current-tempo estimate of a duration through the show transport."
  [transport duration]
  (let [{:keys [unit value] :as normalized} (time/normalize-duration duration (:meter transport))]
    (if (or (= unit :ms) (zero? value))
      (double value)
      (time/estimate-ms normalized (:bpm transport)))))

(defn- synchronized-move [model fixtures [start end] requested committed]
  (let [rates (mapv #(get-in % [:calibration :motor-rate-deg-per-sec]) fixtures)]
    (doseq [[fixture rate] (map vector fixtures rates)]
      (when-not (every? (fn [axis] (let [value (get rate axis)]
                                     (and (registry/finite-number? value) (pos? value)))) axes)
        (fail! [:targets] (str "Fixture " (:id fixture) " needs positive calibrated pan and tilt rates in degrees per second"))))
    (let [patch (frame/compile-patch fixtures)
          ;; Start from the committed pose when the runtime has one, so the start
          ;; wrap matches the wrap a prepositioning aim cue resolves from history.
          history (into {} (keep (fn [{:keys [id]}] (when-let [row (some-> committed (apply [id]))] [id row]))) fixtures)
          from (solve-point patch fixtures start history 0)
          to (solve-point patch fixtures end from 1)
          minimum-ms (reduce max 0.0
                             (for [[fixture rate] (map vector fixtures rates)
                                   :let [id (:id fixture)
                                         first-pose (frame/fixture-pose fixture (get from id))
                                         last-pose (frame/fixture-pose fixture (get to id))]
                                   [axis degrees] [[:pan :pan-deg] [:tilt :tilt-deg]]]
                               (/ (* 1000.0 (abs (- (last-pose degrees) (first-pose degrees)))) (rate axis))))
          _ (when-not (and (registry/finite-number? minimum-ms) (> minimum-ms 1.0e-9))
              (fail! [:points] "The selected heads need mechanical travel between the two points"))
          requested-ms (try (estimate-ms (:transport model) requested)
                            (catch clojure.lang.ExceptionInfo _
                              (fail! [:duration] "A musical minimum needs a finite, positive show tempo")))
          musical? (and (not (contains? requested :ms)) (pos? requested-ms) (>= requested-ms minimum-ms))
          arrival (if musical? requested {:ms (max requested-ms minimum-ms)})
          [unit span] (first arrival)]
      {:duration arrival
       :duration-ms (if musical? requested-ms (:ms arrival))
       :minimum-travel-ms minimum-ms
       :outputs (vec (for [{:keys [id]} fixtures axis axes]
                       {:attribute axis :targets [id]
                        :value {:op :tween
                                :spec {:mode :once :keyframes [{:at {unit 0} :value (get-in from [id axis])}
                                                              {:at {unit span} :value (get-in to [id axis])}]}}}))})))

(defn- launched-effect [model {:keys [op cue-id effect-id]}]
  (if (= op :cue/launch)
    (when-let [cue (or (get-in model [:registry :cues cue-id])
                       (some #(when (= (wire-id cue-id) (wire-id (:id %))) %) (vals (get-in model [:registry :cues]))))]
      (if-let [version (:effect-version cue)]
        (or (get-in model [:registry :versions [:effects (:effect-id cue) version]])
            (let [current (get-in model [:registry :effects (:effect-id cue)])]
              (when (= version (:version current)) current)))
        (get-in model [:registry :effects (:effect-id cue)])))
    (get-in model [:registry :effects effect-id])))

(defn- wrap-problem [fixtures committed {:keys [attribute targets value]}]
  (let [fixture (get fixtures (wire-id (first targets)))
        current (when (and committed fixture) (committed (:id fixture)))
        start (get-in value [:spec :keyframes 0 :value])]
    (when (and (= :pan attribute) fixture (number? (:pan current)) (number? start))
      (let [degrees #(:pan-deg (frame/fixture-pose fixture {:pan % :tilt 0.5}))
            [low high] (sort [(degrees 0.0) (degrees 1.0)])
            now (degrees (:pan current))
            baked (degrees start)
            nearer? (fn [k] (let [alternative (+ baked (* 360.0 k))]
                              (and (<= (- low 1.0e-6) alternative (+ high 1.0e-6))
                                   (< (+ (abs (- now alternative)) 1.0e-6) (abs (- now baked))))))]
        (when (some nearer? [-2 -1 1 2])
          [:start-wrap-mismatch
           (str "Fixture " (:id fixture) " is on another pan wrap than this path's start; republish it from the current pose")])))))

(defn launch-problem
  "Nil when a launch may proceed, otherwise [code message]. A synchronized aim
  path is refused when a head's mount or calibration changed since it was
  solved, when the current tempo makes its musical arrival faster than the
  calibrated motor rates, or when a head's committed pan is nearer another
  wrap of the start pose than the published one. Committed values are an
  optional ID -> output row lookup (a map works), read only for such paths."
  [model command committed]
  (let [effect (when (#{:cue/launch :effect/launch} (:op command)) (launched-effect model command))
        {:keys [rig-fingerprint arrival-duration minimum-travel-ms] :as editor} (:editor effect)
        fixtures (into {} (map (fn [fixture] [(wire-id (:id fixture)) fixture])) (get-in model [:rig :fixtures]))]
    (when (= :synchronized-aim-path (:type editor))
      (or (when-not (and (map? rig-fingerprint) (seq rig-fingerprint)
                         (every? (fn [[id digest]] (= digest (some-> (get fixtures (wire-id id)) head-fingerprint)))
                                 rig-fingerprint))
            [:stale-calibration "A head's mount or calibration changed after this synchronized path was solved; republish it"])
          (when (and (map? arrival-duration) (not (contains? arrival-duration :ms)) (number? minimum-travel-ms)
                     (< (try (estimate-ms (:transport model) arrival-duration) (catch clojure.lang.ExceptionInfo _ 0.0))
                        (- minimum-travel-ms 1.0e-6)))
            [:tempo-exceeds-motor-rate "At the current tempo this musical arrival is faster than the calibrated motor rates; republish it"])
          (some (partial wrap-problem fixtures committed) (:outputs effect))))))

(defn hold-launch-tempo
  "Pin each freshly launched musical synchronized move to the interval its
  arrival took at launch. Takes and returns an apply-command result. The
  interval comes from the instance's captured meter and the transport tempo of
  that launch, and is kept as the instance's ::compiler/launch-tempo state, so
  a later tempo change cannot command the heads faster than the launch check
  admitted. Replayed commands launch nothing and are left alone."
  [result]
  (reduce (fn [result {:keys [type instance]}]
            (let [{:keys [arrival-duration] :as editor} (get-in instance [:definition :editor])
                  bpm (get-in result [:model :transport :bpm])]
              (if (and (= :instance/launched type) (= :synchronized-aim-path (:type editor))
                       (map? arrival-duration) (not (contains? arrival-duration :ms))
                       (registry/finite-number? bpm) (pos? bpm))
                (let [interval (time/normalize-duration arrival-duration (:timing-meter instance))]
                  (assoc-in result [:model :instances (:id instance) :state ::compiler/launch-tempo]
                            {:beats (:value interval) :ms (time/estimate-ms interval bpm)}))
                result)))
          result (:events result)))

(defn definition
  "Create a revision of an aim-only effect for selected moving heads.
  Points are world-space metres; chord distance determines musical keyframe
  spacing. Loop, ping-pong and once reuse the standard tween and instance clocks.
  Opt-in :sync-arrival? plans a two-point move in calibrated motor coordinates.
  Original points/settings remain portable editor metadata in the definition.
  An optional ID -> committed output row lookup seeds the synchronized start
  pan wrap."
  ([model spec] (definition model spec nil))
  ([model {:keys [id name points targets curve closed? expected-version]
           :or {curve :linear closed? false} :as spec} committed]
   (when-not (identifier? id) (fail! [:id] "Give the path an effect ID"))
   (when-not (and (vector? points) (<= 2 (count points) max-points)
                  (every? (fn [point]
                            (and (vector? point) (= 3 (count point))
                                 (every? (fn [n] (and (registry/finite-number? n) (<= (abs n) 10000))) point))) points))
     (fail! [:points] "Draw 2..256 finite world-space points within 10 km of the rig origin"))
   (let [previous (or (get-in model [:registry :effects id])
                      (some #(when (= (wire-id id) (wire-id (:id %))) %) (vals (get-in model [:registry :effects]))))
         sync? (:sync-arrival? spec (get-in previous [:editor :sync-arrival?] false))
         mode (:mode spec (if sync? :once :loop))
         duration (:duration spec (if sync? {:beats 0} {:bars 1}))
         _ (when-not (boolean? sync?) (fail! [:sync-arrival?] "Synchronized arrival needs a boolean flag"))
         _ (when-not (and (boolean? closed?) (#{:once :loop :ping-pong} mode))
             (fail! [:mode] "Choose once, loop or ping-pong and an explicit closed-path flag"))
         _ (when (and sync? (or closed? (not= :once mode) (not= :linear curve)))
             (fail! [:sync-arrival?] "Synchronized arrival needs two points, linear easing, and an open once path"))
         _ (when (or (seq (time/validate-duration duration))
                     (and (not sync?) (not (pos? (val (first duration))))))
             (fail! [:duration] (if sync? "Synchronized arrival takes an optional nonnegative musical or millisecond minimum"
                                  "Choose a positive beat, bar, phrase or explicit millisecond duration")))
         _ (tween/curve curve 0.5)
         fixtures (into {} (map (fn [fixture] [(wire-id (:id fixture)) fixture])) (get-in model [:rig :fixtures]))
         _ (when-not (and (vector? targets) (seq targets) (every? identifier? targets))
             (fail! [:targets] "Select at least one moving head"))
         targets (into [] (distinct)
                       (map (fn [id]
                              (let [fixture (get fixtures (wire-id id))]
                                (when-not (and fixture (get-in fixture [:channels :pan]) (get-in fixture [:channels :tilt]))
                                  (fail! [:targets] (str "Fixture " id " has no patched pan/tilt channels")))
                                (:id fixture))) targets))
         id (or (:id previous) id)
         _ (when (and (contains? spec :expected-version) (not= expected-version (:version previous)))
             (fail! [:expected-version] "The effect changed after this path was opened"))
         _ (when (and previous (not (#{:aim-path :synchronized-aim-path} (get-in previous [:editor :type]))))
             (fail! [:id] "That effect ID belongs to a different effect; choose a new ID"))
         points (reduce (fn [result point]
                          (if (and (seq result) (< (distance (peek result) point) 1.0e-6)) result (conj result point))) [] points)
         editor-points points
         points (if (and closed? (> (distance (first points) (peek points)) 1.0e-6))
                  (conj points (first points)) points)
         lengths (vec (reductions + 0.0 (map distance points (rest points))))
         total (peek lengths)
         _ (when-not (pos? total) (fail! [:points] "The path needs at least two different points"))
         _ (when (and sync? (not= 2 (count points)))
             (fail! [:sync-arrival?] "Synchronized arrival needs two points, linear easing, and an open once path"))
         target-fixtures (mapv #(get fixtures (wire-id %)) targets)
         move (when sync? (synchronized-move model target-fixtures points duration committed))
         [unit span] (first duration)
         frames (mapv (fn [point length]
                        {:at {unit (* span (/ length total))} :value point :curve curve}) points lengths)
         version (next-version (:registry model) :effects id)]
     (cond-> {:id id :version version :name (or name (wire-id id)) :params {}
              :targets targets :categories [:movement]
              :outputs (or (:outputs move) {:aim {:op :tween :spec {:mode mode :keyframes frames}}})
              :editor (cond-> {:type (if sync? :synchronized-aim-path :aim-path)
                               :points editor-points :duration duration
                               :mode mode :curve curve :closed? closed?}
                        sync? (assoc :sync-arrival? true :arrival-duration (:duration move)
                                     :arrival-duration-ms (:duration-ms move)
                                     :minimum-travel-ms (:minimum-travel-ms move)
                                     :rig-fingerprint (into {} (map (fn [fixture] [(wire-id (:id fixture)) (head-fingerprint fixture)]))
                                                            target-fixtures)))}
       (= mode :once) (assoc :duration (or (:duration move) duration))))))
