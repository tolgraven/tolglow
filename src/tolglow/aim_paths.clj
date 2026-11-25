(ns tolglow.aim-paths
  "Portable world-space aim paths compiled into ordinary musical tween effects.
  Drawing and publication are separate: this namespace never runs a fixture."
  (:require [clojure.string :as str]
            [afterglow.render.frame :as frame]
            [afterglow.time :as time]
            [tolglow.registry :as registry]
            [tolglow.composition.tween :as tween]))

(def max-points 256)

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

(defn- synchronized-move [fixtures [start end] requested-ms]
  (let [rates (mapv #(get-in % [:calibration :motor-rate-deg-per-sec]) fixtures)]
    (doseq [[fixture rate] (map vector fixtures rates)]
      (when-not (every? (fn [axis] (let [value (get rate axis)]
                                     (and (registry/finite-number? value) (pos? value)))) axes)
        (fail! [:targets] (str "Fixture " (:id fixture) " needs positive calibrated pan and tilt rates in degrees per second"))))
    (let [patch (frame/compile-patch fixtures)
          from (solve-point patch fixtures start {} 0)
          to (solve-point patch fixtures end from 1)
          minimum-ms (reduce max 0.0
                             (for [[fixture rate] (map vector fixtures rates)
                                   :let [id (:id fixture)
                                         first-pose (frame/fixture-pose fixture (get from id))
                                         last-pose (frame/fixture-pose fixture (get to id))]
                                   [axis degrees] [[:pan :pan-deg] [:tilt :tilt-deg]]]
                               (/ (* 1000.0 (abs (- (last-pose degrees) (first-pose degrees)))) (rate axis))))
          duration-ms (max (double requested-ms) minimum-ms)]
      (when-not (and (registry/finite-number? duration-ms) (pos? duration-ms))
        (fail! [:duration] "The endpoints and calibrated rates need a finite, positive travel time"))
      {:duration {:ms duration-ms}
       :outputs (vec (for [{:keys [id]} fixtures axis axes]
                       {:attribute axis :targets [id]
                        :value {:op :tween
                                :spec {:mode :once :keyframes [{:at {:ms 0} :value (get-in from [id axis])}
                                                              {:at {:ms duration-ms} :value (get-in to [id axis])}]}}}))})))

(defn definition
  "Create a revision of an aim-only effect for selected moving heads.
  Points are world-space metres; chord distance determines musical keyframe
  spacing. Loop, ping-pong and once reuse the standard tween and instance clocks.
  Opt-in :sync-arrival? plans a two-point move in calibrated motor coordinates.
  Original points/settings remain portable editor metadata in the definition."
  [model {:keys [id name points targets curve closed? expected-version]
          :or {curve :linear closed? false} :as spec}]
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
        duration (:duration spec (if sync? {:ms 0} {:bars 1}))
        _ (when-not (boolean? sync?) (fail! [:sync-arrival?] "Synchronized arrival needs a boolean flag"))
        _ (when-not (and (boolean? closed?) (#{:once :loop :ping-pong} mode))
            (fail! [:mode] "Choose once, loop or ping-pong and an explicit closed-path flag"))
        _ (when (and sync? (or closed? (not= :once mode) (not= :linear curve) (not= 2 (count points))))
            (fail! [:sync-arrival?] "Synchronized arrival needs two points, linear easing, and an open once path"))
        _ (when (or (seq (time/validate-duration duration))
                    (if sync? (not (contains? duration :ms)) (not (pos? (val (first duration))))))
            (fail! [:duration] (if sync? "Synchronized arrival uses an optional minimum duration in milliseconds"
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
        move (when sync? (synchronized-move (mapv #(get fixtures (wire-id %)) targets) points (:ms duration)))
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
                       sync? (assoc :sync-arrival? true :arrival-duration (:duration move)))}
      (and (= mode :once) (not sync?)) (assoc :duration duration))))
