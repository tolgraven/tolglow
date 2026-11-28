(ns tolglow.audio.drift
  "Delay-locked alignment of a capture device's sample clock to the host clock
  (#912). Capture stamps block N at origin + cursor / sample-rate. A device
  whose crystal runs slow or fast against the host puts those stamps behind or
  ahead of the real capture time without bound: 25 ppm slow is 1.5 ms after a
  minute and 90 ms after an hour.

  The tracker watches the delay from each block's newest captured frame to its
  arrival: `received-at-ms - end - buffered`, where buffered is what the line
  still holds unread. Its floor is the delivery latency plus the accumulated
  drift; jitter and backlog only ever add to it. Each segment of audio (one
  second by default) contributes its minimum delay. A line fitted through the
  minima of the last segments is the drift, and the latency floor at the start
  anchors it: the target correction is the fitted floor now minus that
  baseline. The baseline also falls when a fit window of later segments shows
  a floor at least half a step lower once the integrated drift is taken out,
  since a low delay can only be real.

  A floor that moves by more than `:step-ms` (or four times the floor's own
  scatter about the line) from one segment to the next is a latency step or a
  backlog, not drift: a drifting clock moves the floor by a
  fraction of a millisecond per second. Such a segment is held out of the fit;
  when `:step-segments` in a row agree, the history and the baseline are moved
  by the step (re-anchored) so the correction carries on unchanged, and a
  segment back on the line drops the held ones as a passing backlog.

  The correction applied to a block's timestamps follows the target, moving at
  most `:slew-samples` per block, so consecutive blocks stay contiguous for
  every consumer (they treat a gap over half a sample as a discontinuity), and
  it never puts a block's end after its arrival. That caps the drift it can
  follow at slew-samples / block-frames: the bound is the smaller of
  `:bound-ppm` and 80% of that capacity.

  Timing is invalid while the drift is beyond the bound or the correction
  trails its target by more than `:behind-ms`, with hysteresis: it leaves
  out-of-bound below 90% of the bound and behind below `:recovered-ms`, and an
  invalid state holds for at least one segment. The tracker is plain data, a
  pure function of the positions and arrival times it is given; the capture
  worker owns it.")

(def defaults
  "Segment length in ms of audio, segments in the fit window, segments that
  anchor the latency floor, segments before a rate is judged against the
  bound, and later segments the baseline may fall to, the
  largest believable drift, the largest correction step per block in samples,
  the floor change that counts as a step and how many segments confirm one,
  and the behind/recovered thresholds on how far the correction may trail its
  target."
  {:segment-ms 1000.0 :segments 16 :anchor-segments 4 :judge-segments 8 :baseline-segments 300
   :bound-ppm 250.0 :slew-samples 0.4 :step-ms 1.5 :step-segments 2
   :behind-ms 2.0 :recovered-ms 1.0 :skew-margin-ms 0.1})

(def max-block-frames
  "The largest block a drifting device clock may use: at 1024 frames a 0.4
  sample step still follows 390 ppm, so the 250 ppm bound holds."
  1024)

(defn capacity-ppm
  "The largest drift the correction can follow at a block size."
  [block-frames slew-samples]
  (* 1.0e6 (/ (double slew-samples) (double block-frames))))

(defn new-tracker
  "A tracker for one capture run at sample-rate and block-frames. options
  override `defaults`."
  ([sample-rate block-frames] (new-tracker sample-rate block-frames {}))
  ([sample-rate block-frames options]
   (let [options (merge defaults options)]
     (when-not (and (number? sample-rate) (<= 8000 sample-rate 192000))
       (throw (ex-info "Drift tracking needs an 8–192 kHz sample rate" {:sample-rate sample-rate})))
     (when-not (and (integer? block-frames) (<= 1 block-frames max-block-frames))
       (throw (ex-info (str "A drifting device clock needs blocks of at most " max-block-frames " frames")
                       {:block-frames block-frames})))
     {:options options
      :bound-ppm (min (:bound-ppm options) (* 0.8 (capacity-ppm block-frames (:slew-samples options))))
      :slew-ms (* 1000.0 (/ (double (:slew-samples options)) (double sample-rate)))
      :segment nil :history [] :anchors [] :lows [] :suspects [] :fit nil
      :drift-ms 0.0 :position-ms 0.0 :baseline nil
      :target-ms 0.0 :correction-ms 0.0 :state :measuring :invalid-since nil
      :steps 0 :last-step-ms nil :clamps 0})))

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

(defn- floor-at
  "The delay floor the history predicts at a position: the fit, or while
  measuring a line through the segments so far; nil before two."
  [{:keys [fit history]} position]
  (when-let [{:keys [rate intercept]} (or fit (when (>= (count history) 2) (fit-line history)))]
    (+ intercept (* rate position))))

(defn- refit [{:keys [history anchors lows options] :as tracker}]
  (if (< (count history) (:anchor-segments options))
    tracker
    (let [{:keys [rate] :as fit} (fit-line history)
          first-fit? (nil? (:fit tracker))
          ;; The drift integrated so far: back-filled from the first fit, then
          ;; accumulated block by block, never from the baseline.
          drift (if first-fit? (* rate (:position-ms tracker)) (:drift-ms tracker))
          ;; The latency floor at the start: the best anchor with the drift up
          ;; to it taken out, or any later segment that shows a lower floor.
          anchored (reduce min (map (fn [[x y]] (- y (* rate x))) anchors))
          ;; A later floor counts when a fit window of segments in a row agrees
          ;; (their median) and lies at least half a step below the anchor, so
          ;; neither one lucky segment nor the scatter of a quantised floor
          ;; pulls the baseline down.
          sustained (reduce min ##Inf (map (fn [run] (nth (sort (map second run)) (quot (count run) 2)))
                                           (partition (:segments options) 1 lows)))
          baseline (if (< sustained (- anchored (* 0.5 (:step-ms options)))) sustained anchored)]
      (assoc tracker :fit fit :drift-ms drift :baseline baseline))))

(defn- shift [points delta] (mapv (fn [[x y]] [x (+ y delta)]) points))

(defn- re-anchor
  "Move the history, anchors and baseline by a confirmed step, so the
  target carries on unchanged, and take the held segments in."
  [{:keys [suspects] :as tracker} delta]
  (-> tracker
      (update :history shift delta) (update :anchors shift delta) (update :lows shift delta)
      (update :baseline #(some-> % (+ delta)))
      (update :steps inc) (assoc :last-step-ms delta :suspects [])
      (update :history #(vec (take-last (get-in tracker [:options :segments]) (into % (map pop suspects)))))
      refit))

(defn- admit [{:keys [options] :as tracker} point]
  (let [tracker (update tracker :history #(vec (take-last (:segments options) (conj % point))))
        tracker (cond-> tracker
                  (< (count (:anchors tracker)) (:anchor-segments options)) (update :anchors conj point)
                  ;; A later floor, with the drift integrated up to it taken out.
                  (:fit tracker)
                  (update :lows #(let [[at floor] point
                                       drift (- (:drift-ms tracker) (* (:rate (:fit tracker)) (- (:position-ms tracker) at)))]
                                   (vec (take-last (:baseline-segments options) (conj % [at (- floor drift)]))))))]
    (refit tracker)))

(defn- step-threshold
  "A floor change that counts as a step: `:step-ms`, or four times the
  median distance of the fitted segments from their line when the floor is
  noisier than that."
  [{:keys [history options] :as tracker}]
  (let [residuals (when (>= (count history) 3)
                    (sort (map (fn [[x y]] (Math/abs (double (- y (floor-at tracker x))))) history)))]
    (max (:step-ms options) (if residuals (* 4.0 (nth residuals (quot (count residuals) 2))) 0.0))))

(defn- close-segment
  "Admit a closed segment's minimum, or hold it out as a possible step."
  [{:keys [segment suspects options] :as tracker}]
  (let [point [(:at segment) (:min segment)]
        predicted (floor-at tracker (first point))
        deviation (when predicted (- (second point) predicted))
        tracker (assoc tracker :segment nil)]
    (if (and deviation (> (Math/abs (double deviation)) (step-threshold tracker)))
      (let [suspects (conj suspects (conj point deviation))
            deviations (map peek suspects)
            agree? (and (apply = (map pos? deviations))
                        (<= (- (reduce max deviations) (reduce min deviations))
                            (max (:step-ms options) (* 0.25 (Math/abs (double deviation))))))]
        (cond (not agree?) (assoc tracker :suspects [(conj point deviation)])
              (>= (count suspects) (:step-segments options))
              (re-anchor (assoc tracker :suspects suspects) (/ (reduce + deviations) (count deviations)))
              :else (assoc tracker :suspects suspects)))
      ;; Back on the line: held segments were a passing backlog.
      (admit (assoc tracker :suspects []) point))))

(defn- next-state
  "Classify with hysteresis; an invalid state holds for at least one segment."
  [{:keys [state invalid-since options bound-ppm history]} fit ppm residual position]
  (let [ppm (Math/abs (double (or ppm 0.0)))
        residual (Math/abs (double residual))
        ;; A rate is judged only once the fit spans enough segments.
        out? (and (>= (count history) (:judge-segments options))
                  (if (= :out-of-bound state) (>= ppm (* 0.9 bound-ppm)) (> ppm bound-ppm)))
        behind? (if (= :behind state) (>= residual (:recovered-ms options)) (> residual (:behind-ms options)))
        wanted (cond (nil? fit) :measuring out? :out-of-bound behind? :behind :else :locked)
        holding? (and (#{:out-of-bound :behind} state) (not (#{:out-of-bound :behind} wanted))
                      invalid-since (< (- position invalid-since) (:segment-ms options)))]
    (if holding? state wanted)))

(defn observe
  "Advance the tracker by one block. position-ms is the block's nominal end in
  ms of audio since the run's origin, (cursor + frames) / sample-rate × 1000;
  delay-ms is its arrival time minus its nominal end timestamp, less what the
  source still buffers. Returns the tracker with `:correction-ms`, the offset to
  add to this block's nominal timestamps."
  [{:keys [options slew-ms] :as tracker} position-ms delay-ms]
  (let [position (double position-ms) delay (double delay-ms)
        step (- position (:position-ms tracker))
        tracker (cond-> (assoc tracker :position-ms position)
                  (:fit tracker) (update :drift-ms + (* (:rate (:fit tracker)) step)))
        segment (:segment tracker)
        segment (if (and segment (<= (:min segment) delay))
                  segment
                  (assoc (or segment {:start position}) :min delay :at position))
        tracker (assoc tracker :segment segment)
        tracker (if (>= (- position (:start segment)) (:segment-ms options)) (close-segment tracker) tracker)
        {:keys [fit baseline]} tracker
        target (if fit (- (floor-at tracker position) baseline) 0.0)
        slewed (let [c (double (:correction-ms tracker))]
                 (+ c (max (- slew-ms) (min slew-ms (- target c)))))
        ;; Never put the block's end after its arrival.
        ceiling (- delay (:skew-margin-ms options))
        correction (min slewed ceiling)
        ;; The delay floor grows when the device is slow: report the device
        ;; clock's rate against the host, negative when slow.
        ppm (when fit (* -1.0e6 (:rate fit)))
        state (next-state tracker fit ppm (- target correction) position)
        invalid? #{:out-of-bound :behind}]
    (assoc tracker :target-ms target :correction-ms correction :state state :ppm ppm
           :clamps (cond-> (:clamps tracker) (< ceiling slewed) inc)
           :invalid-since (cond (not (invalid? state)) nil
                                (invalid? (:state tracker)) (:invalid-since tracker)
                                :else position))))

(defn valid?
  "False while the measured drift is beyond the bound or the correction trails
  its target too far (see the namespace docstring for the hysteresis)."
  [tracker]
  (not (#{:out-of-bound :behind} (:state tracker))))

(defn metrics
  "The tracker as capture metrics: state, the device clock's drift against
  the host in ppm (negative when slow; nil while measuring), the correction
  applied to the newest block and its target in ms (positive moves timestamps
  later), the effective bound, and the latency steps it re-anchored on and the
  corrections it clamped to keep a block before its arrival."
  [{:keys [state ppm correction-ms target-ms history bound-ppm steps last-step-ms clamps]}]
  {:tracking? true :state state :ppm ppm :correction-ms correction-ms :target-ms target-ms
   :segments (count history) :bound-ppm bound-ppm :steps steps :last-step-ms last-step-ms :clamps clamps})
