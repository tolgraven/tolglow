(ns tolglow.aim-paths
  "Portable world-space aim paths compiled into ordinary musical tween effects.
  Drawing and publication are separate: this namespace never runs a fixture."
  (:require [clojure.string :as str]
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

(defn definition
  "Create a revision of an aim-only effect for selected moving heads.
  Points are world-space metres; chord distance determines musical keyframe
  spacing. Loop, ping-pong and once reuse the standard tween and instance clocks.
  Original points/settings remain portable editor metadata in the definition."
  [model {:keys [id name points targets duration mode curve closed? expected-version]
          :or {duration {:bars 1} mode :loop curve :linear closed? false} :as spec}]
  (when-not (identifier? id) (fail! [:id] "Give the path an effect ID"))
  (when-not (and (vector? points) (<= 2 (count points) max-points)
                 (every? (fn [point]
                           (and (vector? point) (= 3 (count point))
                                (every? (fn [n] (and (registry/finite-number? n) (<= (abs n) 10000))) point))) points))
    (fail! [:points] "Draw 2..256 finite world-space points within 10 km of the rig origin"))
  (when-not (and (boolean? closed?) (#{:once :loop :ping-pong} mode))
    (fail! [:mode] "Choose once, loop or ping-pong and an explicit closed-path flag"))
  (when (or (seq (time/validate-duration duration)) (not (pos? (val (first duration)))))
    (fail! [:duration] "Choose a positive beat, bar, phrase or explicit millisecond duration"))
  (tween/curve curve 0.5)
  (let [fixtures (into {} (map (fn [fixture] [(wire-id (:id fixture)) fixture])) (get-in model [:rig :fixtures]))
        _ (when-not (and (vector? targets) (seq targets) (every? identifier? targets))
            (fail! [:targets] "Select at least one moving head"))
        targets (mapv (fn [id]
                        (let [fixture (get fixtures (wire-id id))]
                          (when-not (and fixture (get-in fixture [:channels :pan]) (get-in fixture [:channels :tilt]))
                            (fail! [:targets] (str "Fixture " id " has no patched pan/tilt channels")))
                          (:id fixture))) (distinct targets))
        previous (or (get-in model [:registry :effects id])
                     (some #(when (= (wire-id id) (wire-id (:id %))) %) (vals (get-in model [:registry :effects]))))
        id (or (:id previous) id)
        _ (when (and (contains? spec :expected-version) (not= expected-version (:version previous)))
            (fail! [:expected-version] "The effect changed after this path was opened"))
        _ (when (and previous (not= :aim-path (get-in previous [:editor :type])))
            (fail! [:id] "That effect ID belongs to a different effect; choose a new ID"))
        points (reduce (fn [result point]
                         (if (and (seq result) (< (distance (peek result) point) 1.0e-6)) result (conj result point))) [] points)
        points (if (and closed? (> (distance (first points) (peek points)) 1.0e-6))
                 (conj points (first points)) points)
        lengths (vec (reductions + 0.0 (map distance points (rest points))))
        total (peek lengths)
        _ (when-not (pos? total) (fail! [:points] "The path needs at least two different points"))
        [unit span] (first duration)
        frames (mapv (fn [point length]
                       {:at {unit (* span (/ length total))} :value point :curve curve}) points lengths)
        version (inc (reduce max (:version previous 0)
                             (for [[[kind entry-id v] _] (get-in model [:registry :versions])
                                   :when (and (= kind :effects) (= entry-id id))] v)))]
    (cond-> {:id id :version version :name (or name (wire-id id)) :params {}
             :targets targets :categories [:movement]
             :outputs {:aim {:op :tween :spec {:mode mode :keyframes frames}}}
             :editor {:type :aim-path :points points :duration duration :mode mode :curve curve :closed? closed?}}
      (= mode :once) (assoc :duration duration))))
