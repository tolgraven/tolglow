(ns tolglow.audio.drift
  "Delay-locked alignment of a capture device's sample clock to the host clock
  (#912). Capture stamps block N at origin + cursor / sample-rate. A device
  whose crystal runs slow or fast against the host puts those stamps behind or
  ahead of the real capture time without bound: 25 ppm slow is 1.5 ms after a
  minute and 90 ms after an hour.

  The tracker watches the delay from each block's nominal end to its arrival,
  `received-at-ms - end`. Its floor is the irreducible delivery latency plus the
  accumulated drift; jitter only ever adds to it. Each segment of audio (one
  second by default) contributes its minimum delay, a line fitted through the
  minima of the last segments gives the drift rate, and the delay floor of the
  first segments, with the drift taken out, anchors it. The correction applied
  to a block's timestamps follows that fitted drift, moving at most a quarter
  sample per block, so consecutive blocks stay contiguous for every consumer
  (they treat a gap over half a sample as a discontinuity).

  A rate beyond `:bound-ppm`, or a correction that falls more than
  `:residual-limit-ms` behind its target, makes timing invalid instead of
  drifting silently. The tracker is plain data, a pure function of the
  positions and arrival times it is given; the capture worker owns it.")

(def defaults
  "Segment length in ms of audio, segments in the fit window, segments that
  anchor the latency floor, the largest believable drift, the largest
  correction step per block in samples, and how far the correction may trail
  its target before timing is invalid."
  {:segment-ms 1000.0 :segments 16 :anchor-segments 4
   :bound-ppm 250.0 :slew-samples 0.25 :residual-limit-ms 2.0})

(defn new-tracker
  "A tracker for one capture run at sample-rate. options override `defaults`."
  ([sample-rate] (new-tracker sample-rate {}))
  ([sample-rate options]
   (let [options (merge defaults options)]
     (when-not (and (number? sample-rate) (<= 8000 sample-rate 192000))
       (throw (ex-info "Drift tracking needs an 8–192 kHz sample rate" {:sample-rate sample-rate})))
     {:options options :slew-ms (* 1000.0 (/ (double (:slew-samples options)) (double sample-rate)))
      :segment nil :history [] :anchors [] :fit nil
      :target-ms 0.0 :correction-ms 0.0 :state :measuring})))

(defn- fit-line
  "Least-squares line through [x y] points: {:rate :intercept}."
  [points]
  (let [n (double (count points))
        mx (/ (reduce + (map first points)) n)
        my (/ (reduce + (map second points)) n)
        sxx (reduce + (map (fn [[x _]] (* (- x mx) (- x mx))) points))
        sxy (reduce + (map (fn [[x y]] (* (- x mx) (- y my))) points))
        rate (if (pos? sxx) (/ sxy sxx) 0.0)]
    {:rate rate :intercept (- my (* rate mx))}))

(defn- refit [{:keys [history anchors options] :as tracker}]
  (if (< (count history) (:anchor-segments options))
    tracker
    (let [{:keys [rate intercept]} (fit-line history)
          ;; The delay floor at sample 0: the best anchor segment with the
          ;; drift up to it taken out. Jitter only raises a segment's minimum.
          baseline (reduce min (map (fn [[x y]] (- y (* rate x))) anchors))]
      (assoc tracker :fit {:rate rate :intercept intercept :baseline baseline}))))

(defn- close-segment [{:keys [segment options] :as tracker}]
  (let [point [(:at segment) (:min segment)]]
    (refit (cond-> (assoc tracker :segment nil
                          :history (vec (take-last (:segments options) (conj (:history tracker) point))))
             (< (count (:anchors tracker)) (:anchor-segments options)) (update :anchors conj point)))))

(defn observe
  "Advance the tracker by one block. position-ms is the block's nominal end in
  ms of audio since the run's origin, (cursor + frames) / sample-rate × 1000;
  delay-ms is its arrival time minus its nominal end timestamp. Returns the
  tracker with `:correction-ms`, the offset to add to this block's nominal
  timestamps."
  [{:keys [options slew-ms] :as tracker} position-ms delay-ms]
  (let [position (double position-ms) delay (double delay-ms)
        segment (:segment tracker)
        segment (if (and segment (<= (:min segment) delay))
                  segment
                  (assoc (or segment {:start position}) :min delay :at position))
        tracker (assoc tracker :segment segment)
        tracker (if (>= (- position (:start segment)) (:segment-ms options)) (close-segment tracker) tracker)
        {:keys [rate intercept baseline] :as fit} (:fit tracker)
        target (if fit (- (+ intercept (* rate position)) baseline) 0.0)
        correction (let [c (double (:correction-ms tracker))]
                     (+ c (max (- slew-ms) (min slew-ms (- target c)))))
        ;; The delay floor grows when the device is slow: report the device
        ;; clock's rate against the host, negative when slow.
        ppm (when fit (* -1.0e6 rate))
        state (cond (nil? fit) :measuring
                    (> (Math/abs (double ppm)) (:bound-ppm options)) :out-of-bound
                    (> (Math/abs (- target correction)) (:residual-limit-ms options)) :behind
                    :else :locked)]
    (assoc tracker :target-ms target :correction-ms correction :state state :ppm ppm)))

(defn valid?
  "False while the measured drift is beyond the bound or the correction trails
  its target by more than the residual limit."
  [tracker]
  (not (#{:out-of-bound :behind} (:state tracker))))

(defn metrics
  "The tracker as capture metrics: state, the device clock's drift against
  the host in ppm (negative when slow; nil while measuring), and the
  correction applied to the newest block and its target, in ms (positive
  moves timestamps later)."
  [{:keys [state ppm correction-ms target-ms history options]}]
  {:tracking? true :state state :ppm ppm :correction-ms correction-ms :target-ms target-ms
   :segments (count history) :bound-ppm (:bound-ppm options)})
