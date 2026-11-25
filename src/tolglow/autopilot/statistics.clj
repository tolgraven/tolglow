(ns tolglow.autopilot.statistics
  "Time-weighted lighting energy over an explicit musical or elapsed-time window."
  (:require [afterglow.time :as time]))

(def ^:private bucket-count 64)

(def ^:private max-edge-spans 16)

(def ^:private bins 10)

(defn- unit? [x] (and (time/finite-number? x) (<= 0 x 1)))

(defn- hist-bin [level] (min (dec bins) (int (* bins (double level)))))

(def ^:private zero-histogram (vec (repeat bins 0.0)))

(defn- observation [levels targets tolerance axis]
  (let [level (axis levels) target (axis targets)
        band (if (map? tolerance) (axis tolerance) tolerance)]
    (when-not (every? unit? [level target band])
      (throw (ex-info "Energy measurements, targets and tolerances must be in [0,1]"
                      {:axis axis :level level :target target :tolerance band})))
    [(double level) (<= (abs (- (double level) (double target))) (double band))]))

(defn- add-observation [moments [level hit?] duration]
  (let [level (double level) duration (double duration)]
    {:min (min level (double (:min moments level))) :max (max level (double (:max moments level)))
     :sum (+ (double (:sum moments 0)) (* duration level))
     :squares (+ (double (:squares moments 0)) (* duration level level))
     :lit (+ (double (:lit moments 0)) (if (pos? level) duration 0))
     :hits (+ (double (:hits moments 0)) (if hit? duration 0))
     :histogram (update (or (:histogram moments) zero-histogram) (hist-bin level) + duration)}))

(defn- add-span [bucket start end observations]
  (let [[brightness activity] observations
        spans (:spans bucket)
        [previous-start _ previous-levels] (peek spans)
        spans (cond
                (and bucket (nil? spans)) nil
                (= observations previous-levels) (conj (pop spans) [previous-start end observations])
                (< (count spans) max-edge-spans) (conj (or spans []) [start end observations]))
        duration (- end start)]
    {:from (:from bucket start) :to end :spans spans
     :brightness (add-observation (:brightness bucket) brightness duration)
     :activity (add-observation (:activity bucket) activity duration)}))

(defn- integrate
  "Coordinates are measured in buckets. Long gaps visit at most one window."
  [buckets position end observations]
  (let [cutoff (- end bucket-count)
        buckets (if (and (seq buckets) (<= (:to (first buckets)) cutoff))
                  (into [] (drop-while #(<= (:to %) cutoff)) buckets) buckets)]
    (loop [result buckets start (max cutoff position)]
      (if (>= start end) result
        (let [boundary (inc (Math/floor (double start)))
              stop (min end boundary)
              prior (peek result)
              append? (and prior (= start (:to prior)) (= (Math/floor (double start)) (Math/floor (double (:from prior)))))
              b (add-span (when append? prior) start stop observations)]
          (recur (conj (if append? (pop result) result) b) stop))))))

(defn- trim-edge
  "Keep exact samples at the left edge. For very dense input, at most the
  oldest 1/64 of the window uses its bucket's time-weighted distribution."
  [bucket cutoff]
  (cond
    (>= (:from bucket) cutoff) [bucket 1.0]
    (:spans bucket)
    [(reduce (fn [result [a b levels]]
               (if (> b cutoff) (add-span result (max a cutoff) b levels) result))
             nil (:spans bucket)) 1.0]
    :else [bucket (/ (- (:to bucket) cutoff) (- (:to bucket) (:from bucket)))]))

(defn- summarize [buckets cutoff duration axis instant]
  (if (zero? duration)
    {:instant instant :mean instant :min instant :max instant :stddev 0.0
     :lit-fraction 0.0 :target-hit-fraction 0.0 :histogram zero-histogram}
    (let [histogram (double-array bins)]
      (loop [remaining (seq buckets) first? true
             sum 0.0 squares 0.0 lit 0.0 hits 0.0 lowest 1.0 highest 0.0]
        (if-let [bucket (first remaining)]
          (let [[bucket weight] (if first? (trim-edge bucket cutoff) [bucket 1.0])
                m (axis bucket) weight (double weight) bins-in (:histogram m)]
            (dotimes [i bins]
              (aset histogram i (+ (aget histogram i) (* weight (double (nth bins-in i))))))
            (recur (next remaining) false
                   (+ sum (* weight (double (:sum m)))) (+ squares (* weight (double (:squares m))))
                   (+ lit (* weight (double (:lit m)))) (+ hits (* weight (double (:hits m))))
                   (min lowest (double (:min m))) (max highest (double (:max m)))))
          (let [mean (/ sum duration)]
            {:instant instant :mean (min 1.0 (max 0.0 mean)) :min lowest :max highest
             :stddev (Math/sqrt (max 0.0 (- (/ squares duration) (* mean mean))))
             :lit-fraction (min 1.0 (/ lit duration)) :target-hit-fraction (min 1.0 (/ hits duration))
             :histogram (mapv #(/ (double %) duration) histogram)}))))))

(defn- shift-bucket [bucket shift]
  (cond-> (-> bucket (update :from - shift) (update :to - shift))
    (:spans bucket) (update :spans (fn [spans]
                                    (mapv (fn [[a b values]] [(- a shift) (- b shift) values]) spans)))))

(defn advance
  "For each axis (:brightness and :activity), accumulate the last delivered
  value and its contemporary target until the next observation. Durations are
  weighted by elapsed beats or ms, never by the number of frames. :coverage
  measures how much of a full window has been observed. A paused clock stops
  accumulation. A backward seek, changed epoch or changed width starts fresh.
  :histogram holds ten equally spaced time-fraction bins including exact 1
  in the final bin. :lit-fraction is the fraction strictly above zero.

  Pass the returned :state into the next call. State is portable values only.
  A longer gap assumes the previous output was held during the gap."
  [previous clock window levels targets tolerance]
  (let [clock (assoc (time/frame-clock clock) :epoch (:epoch clock))
        {:keys [unit value]} (time/normalize-duration window (:meter clock))
        now (double (time/coordinate clock unit))
        _ (when-not (pos? value) (throw (ex-info "Rolling window must be positive" {:window window})))
        readings [(observation levels targets tolerance :brightness)
                  (observation levels targets tolerance :activity)]
        compatible? (and (= window (:window previous)) (= value (:width previous))
                         (= (:epoch clock) (get-in previous [:clock :epoch]))
                         (every? (fn [k] (let [old (get-in previous [:clock k]) current (get clock k)]
                                           (or (nil? old) (nil? current) (>= current old)))) [:beat :time-ms]))
        previous (when compatible? previous)
        before (double (:position previous 0.0))
        running? (and previous (get-in previous [:clock :playing?] true))
        elapsed (if running? (- now (time/coordinate (:clock previous) unit)) 0)
        full-gap? (>= elapsed value)
        shift (if (> before 1000000.0) (- (Math/floor before) bucket-count) 0.0)
        before (if full-gap? 0.0 (- before shift))
        end (if full-gap? (double bucket-count) (+ before (* bucket-count (/ (double elapsed) (double value)))))
        buckets (cond full-gap? [] (pos? shift) (mapv #(shift-bucket % shift) (:buckets previous))
                      :else (or (:buckets previous) []))
        buckets (integrate buckets before end (:readings previous))
        state {:window window :width value :clock clock :position end :buckets buckets :readings readings}
        duration (min (double bucket-count) end)
        cutoff (- end bucket-count)]
    {:state state
     :report {:window window :coverage (/ duration bucket-count)
              :brightness (summarize buckets cutoff duration :brightness (ffirst readings))
              :activity (summarize buckets cutoff duration :activity (first (second readings)))}}))
