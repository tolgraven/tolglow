(ns tolglow.autopilot.target-response
  "Slew a conductor's two setpoints independently of the fixture output response."
  (:require [afterglow.time :as time]
            [tolglow.composition.tween :as tween]))

(def ^:private axes [:brightness :activity])

(defn validate! [stages]
  (when-not (or (nil? stages)
                (and (map? stages) (every? #{:attack :release} (keys stages))
                     (every? (fn [stage]
                               (or (and (= #{:blend} (when (map? stage) (set (keys stage))))
                                        (time/finite-number? (:blend stage)) (<= 0 (:blend stage) 1))
                                   (empty? (time/validate-duration stage))))
                             (vals stages))))
    (throw (ex-info "Target response needs attack/release durations or a frame blend in [0,1]"
                    {:target-response stages})))
  stages)

(defn- unit [x]
  (when-not (time/finite-number? x)
    (throw (ex-info "Expected a finite energy target or measured level" {:value x})))
  (double (max 0 (min 1 x))))

(defn- coord-changed? [cmp a b k]
  (and (some? (get a k)) (some? (get b k)) (cmp (get a k) (get b k))))

(defn- follow-axis [prior clock from-clock goal initial stages]
  (let [current (:value prior (if (some? initial) (unit initial) goal))
        stage (get stages (if (> goal current) :attack :release))]
    (cond
      (or (== current goal) (nil? stage)) {:value goal}
      (contains? stage :blend)
      {:value (tween/interpolate current goal (if from-clock (:blend stage) 0.0))}
      :else
      (let [continuing? (and (= goal (:goal prior)) (= stage (:stage prior)))
            duration (if continuing? (:duration prior) (time/normalize-duration stage (:meter clock)))
            left (if continuing? (:remaining prior) (:value duration))
            elapsed (if from-clock (- (time/coordinate clock (:unit duration))
                                       (time/coordinate from-clock (:unit duration))) 0)
            fraction (if (pos? left) (min 1.0 (/ elapsed left)) 1.0)]
        (if (>= fraction 1.0) {:value goal}
          {:value (unit (tween/interpolate current goal fraction)) :goal goal :stage stage
           :duration duration :remaining (max 0 (- left elapsed))})))))

(defn advance
  "Advance normalized brightness/activity targets from the last committed
  setpoints. A duration reaches a held request exactly; :blend closes a fraction
  of the gap once per distinct running frame. A new segment starts from the last
  output. Musical segments retain the meter captured on retarget. Initial levels
  are scoped measurements; an absent initial axis starts at the requested value.
  Pass the returned :state on the next tick. It contains only portable data."
  [previous clock requested initial stages]
  (validate! stages)
  (let [clock (assoc (time/frame-clock clock) :epoch (:epoch clock))
        requested (into {} (map (fn [axis] [axis (unit (get requested axis))])) axes)]
    (if (empty? stages) {:state nil :targets requested}
      (let [before (:clock previous)
            reset? (or (not= (:epoch before) (:epoch clock))
                       (some #(coord-changed? < clock before %) [:time-ms :beat]))
            prior (when-not reset? previous)
            last-clock (:clock prior)
            advancing? (and last-clock (not= false (:playing? clock))
                            (not= false (:playing? last-clock))
                            (if (and (:time-ms clock) (:time-ms last-clock))
                              (coord-changed? > clock last-clock :time-ms)
                              (coord-changed? > clock last-clock :beat)))
            values (if (and prior (not advancing?)) (:axes prior)
                     (into {} (map (fn [axis]
                                     [axis (follow-axis (get-in prior [:axes axis]) clock
                                                        (when advancing? last-clock) (get requested axis)
                                                        (get initial axis) stages)])) axes))]
        {:state {:clock clock :axes values} :targets (update-vals values :value)}))))
