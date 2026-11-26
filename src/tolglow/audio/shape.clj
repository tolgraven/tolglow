(ns tolglow.audio.shape
  "Pure band isolation and beat-windowed shape extraction for audio-shaped
  LFOs (#676). An extractor is immutable data: each call copies the small
  filter state, ring buffer and envelope bins it advances, so a returned state
  is never mutated later. Nothing here opens a device, reads a clock or
  publishes to a show; the capture worker feeds blocks and the show owner
  commits the results."
  (:require [afterglow.time :as time]
            [tolglow.integrations.audio :as audio]))

(def bands
  "Named analysis bands in Hz. :full applies no filter."
  {:sub [20.0 60.0] :bass [60.0 250.0] :low-mid [250.0 800.0]
   :mid [800.0 2500.0] :high [2500.0 8000.0] :full nil})

(def extract-modes
  "`:envelope` takes the band's RMS level across the whole window, one value per
  shape point, so the cycle is the window's rhythm and dynamics. `:cycle` takes
  one pitch period of the band-isolated waveform at the window's end, averaged
  over up to eight consecutive periods and rotated so its fundamental starts at
  zero phase rising."
  #{:envelope :cycle})

(def point-range [8 63])

(defn- fail! [message data] (throw (ex-info message (assoc data :type ::invalid-shape))))

(defn- finite? [x] (and (number? x) (Double/isFinite (double x))))

(defn band-range
  "Return [low high] Hz for a band name or an explicit pair, nil for :full."
  [band]
  (cond
    (contains? bands band) (get bands band)
    (and (vector? band) (= 2 (count band)) (every? finite? band)
         (<= 10.0 (first band)) (< (first band) (second band)) (<= (second band) 20000.0))
    (mapv double band)
    :else (fail! "Band must be :sub, :bass, :low-mid, :mid, :high, :full or [low high] Hz within 10..20000"
                 {:band band})))

(defn band-label
  "A short human label: the band name with its edges in Hz."
  [band]
  (let [[low high] (band-range band)
        hz #(if (>= % 1000.0) (str (/ (Math/round (/ % 100.0)) 10.0) " kHz") (str (Math/round (double %)) " Hz"))]
    (str (if (keyword? band) (name band) "band") (when low (str " " (hz low) "–" (hz high))))))

;; Filter bank ------------------------------------------------------------------

(defn- biquad
  "RBJ cookbook Butterworth section [b0 b1 b2 a1 a2], normalised by a0."
  [kind frequency sample-rate]
  (let [w0 (/ (* 2.0 Math/PI frequency) sample-rate)
        cosw (Math/cos w0)
        alpha (/ (Math/sin w0) (* 2.0 (Math/sqrt 0.5)))
        a0 (+ 1.0 alpha)
        [b0 b1 b2] (case kind
                     :low [(/ (- 1.0 cosw) 2.0) (- 1.0 cosw) (/ (- 1.0 cosw) 2.0)]
                     :high [(/ (+ 1.0 cosw) 2.0) (- (+ 1.0 cosw)) (/ (+ 1.0 cosw) 2.0)])]
    (mapv #(/ (double %) a0) [b0 b1 b2 (* -2.0 cosw) (- 1.0 alpha)])))

(defn filter-design
  "Fourth-order band-pass as cascaded Linkwitz–Riley sections: two Butterworth
  high-passes at the low edge and two low-passes at the high edge (−6 dB at
  each edge, 24 dB/octave outside). An edge at or above 0.45 × the sample rate
  is left open. :full has no sections."
  [band sample-rate]
  (when-not (and (finite? sample-rate) (<= 8000 sample-rate 192000))
    (fail! "Sample rate must be 8–192 kHz" {:sample-rate sample-rate}))
  (if-let [[low high] (band-range band)]
    (vec (concat (when (< low (* 0.45 sample-rate)) (repeat 2 (biquad :high low sample-rate)))
                 (when (< high (* 0.45 sample-rate)) (repeat 2 (biquad :low high sample-rate)))))
    []))

(defn filter-samples
  "Run samples through the design from state (two doubles per section,
  transposed direct form II). Returns [filtered new-state]; neither input is
  modified."
  [design state ^doubles samples]
  (let [sections (count design)
        n (alength samples)
        out (double-array n)
        z (double-array (* 2 sections))
        coefficients (double-array (mapcat identity design))]
    (dotimes [i (min (count state) (alength z))] (aset z i (double (nth state i))))
    (dotimes [i n]
      (loop [s 0 x (aget samples i)]
        (if (= s sections)
          (aset out i x)
          (let [c (* 5 s) zi (* 2 s)
                y (+ (* (aget coefficients c) x) (aget z zi))]
            (aset z zi (- (+ (* (aget coefficients (+ c 1)) x) (aget z (inc zi)))
                          (* (aget coefficients (+ c 3)) y)))
            (aset z (inc zi) (- (* (aget coefficients (+ c 2)) x) (* (aget coefficients (+ c 4)) y)))
            (recur (inc s) y)))))
    [out (vec z)]))

;; Cycle analysis ----------------------------------------------------------------

(defn- ring-sample
  "Linear interpolation at fractional index position in a chronological array."
  ^double [^doubles values ^double position]
  (let [n (alength values)
        i (long (Math/floor position))
        f (- position i)
        a (aget values (max 0 (min (dec n) i)))
        b (aget values (max 0 (min (dec n) (inc i))))]
    (+ a (* f (- b a)))))

(defn- fundamental-phase
  "Phase in cycles of the first harmonic of one periodic cycle of n points,
  measured as A·sin(2π(p + phase))."
  [values]
  (let [n (count values)
        [s c] (reduce (fn [[s c] [j v]]
                        (let [angle (/ (* 2.0 Math/PI j) n)]
                          [(+ s (* v (Math/sin angle))) (+ c (* v (Math/cos angle)))]))
                      [0.0 0.0] (map-indexed vector values))]
    (/ (Math/atan2 c s) (* 2.0 Math/PI))))

(defn- cycle-points
  "Average periods of length period (samples) ending at the array end,
  starting offset cycles into the first, resampled to points values."
  [^doubles values period periods offset points]
  (let [start (- (alength values) (* (+ periods 1) period))]
    (mapv (fn [j]
            (/ (reduce + (map (fn [k] (ring-sample values (+ start (* period (+ k offset (/ j (double points)))))))
                              (range periods)))
               periods))
          (range points))))

(defn normalize
  "Map values into 0..1. :range stretches min..max; :peak divides by the
  maximum absolute value so silence stays at zero. Returns nil when there is no
  usable span."
  [values mode]
  (let [low (reduce min values) high (reduce max values)]
    (case mode
      :range (when (> (- high low) 1.0e-9) (mapv #(/ (- % low) (- high low)) values))
      :peak (let [peak (max (Math/abs (double low)) (Math/abs (double high)))]
              (when (> peak 1.0e-12) (mapv #(/ (Math/abs (double %)) peak) values))))))

(defn analyze-cycle
  "Find the fundamental of a chronological band-limited window with YIN and
  return {:shape :frequency-hz :confidence :periods :input} or a status map
  {:status :low-confidence ...}. frequency-range bounds the period search."
  [^doubles window sample-rate [f-min f-max] points min-confidence]
  (let [estimate (audio/estimate-pitch window sample-rate f-min f-max 0.3)]
    (if-not estimate
      {:status :low-confidence :confidence 0.0}
      (let [{:keys [frequency confidence]} estimate
            period (/ sample-rate frequency)
            periods (long (max 1 (min 8 (dec (Math/floor (/ (alength window) period))))))
            provisional (cycle-points window period periods 0.0 points)
            ;; Rotate so the fundamental starts at zero phase, rising.
            offset (let [phase (- (fundamental-phase (let [mean (/ (reduce + provisional) points)]
                                                       (mapv #(- % mean) provisional))))]
                     (- phase (Math/floor phase)))
            aligned (cycle-points window period periods offset points)
            mean (/ (reduce + aligned) points)
            shape (normalize (mapv #(- % mean) aligned) :range)
            preview (let [span (min (alength window) (long (Math/ceil (* 2 period))))
                          start (- (alength window) span)]
                      (mapv #(ring-sample window (+ start (* (dec span) (/ % 127.0)))) (range 128)))]
        (cond
          (nil? shape) {:status :low-confidence :confidence confidence :frequency-hz frequency}
          (< confidence min-confidence) {:status :low-confidence :confidence confidence :frequency-hz frequency
                                         :input {:kind :pcm :values preview :duration-ms (* 2000.0 (/ period sample-rate))}}
          :else {:status :ok :shape shape :frequency-hz frequency :confidence confidence :periods periods
                 :input {:kind :pcm :values preview :duration-ms (* 2000.0 (/ period sample-rate))}})))))

(defn analyze-envelope
  "Shape a window from per-point band RMS. Empty points take the previous
  point's level. Peak normalisation keeps quiet points near zero."
  [squares counts]
  (let [levels (reduce (fn [out [sq n]]
                         (conj out (if (pos? n) (Math/sqrt (/ sq n)) (or (peek out) 0.0))))
                       [] (map vector squares counts))
        shape (normalize levels :peak)]
    (if shape
      {:status :ok :shape shape :input {:kind :envelope :values levels}}
      {:status :silent})))

;; Extractor ---------------------------------------------------------------------

(def default-spec
  {:band :bass :extract :envelope :window {:bars 1} :points 32
   :silence-db -50.0 :min-confidence 0.6})

(defn validate-spec
  "Return [] or path/message errors for an extractor spec."
  [spec]
  (let [{:keys [band extract window points silence-db min-confidence]} (merge default-spec spec)
        problem (fn [path message] [{:path path :message message}])]
    (vec (concat
          (when-not (try (band-range band) true (catch clojure.lang.ExceptionInfo _ false))
            (problem [:band] "Band must be a named band or [low high] Hz"))
          (when-not (extract-modes extract) (problem [:extract] "Extract :envelope or :cycle"))
          (when-not (and (map? window) (= 1 (count window)) (#{:beats :bars :phrases} (ffirst window))
                         (finite? (val (first window))) (pos? (val (first window))))
            (problem [:window] "Window needs positive beats, bars or phrases"))
          (when-not (and (integer? points) (<= (first point-range) points (second point-range)))
            (problem [:points] "Shape points must be an integer from 8 to 63"))
          (when-not (and (finite? silence-db) (<= -120 silence-db 0))
            (problem [:silence-db] "Silence threshold must be −120..0 dBFS"))
          (when-not (and (finite? min-confidence) (<= 0 min-confidence 1))
            (problem [:min-confidence] "Minimum confidence must be 0..1"))))))

(defn- frequency-search [band sample-rate]
  (let [[low high] (or (band-range band) [30.0 2000.0])]
    [(max 20.0 low) (min high 2000.0 (* 0.25 sample-rate))]))

(defn- ring-size [band sample-rate]
  (let [[f-min] (frequency-search band sample-rate)
        needed (long (Math/ceil (* 4.0 (/ sample-rate f-min))))]
    (long (min 16384 (max 1024 (Long/highestOneBit (dec (* 2 needed))))))))

(defn new-extractor
  "Fresh extractor for one spec at one sample rate. A window that opens part
  way through is discarded, so extraction begins at the next window edge."
  [spec sample-rate]
  (let [errors (validate-spec spec)]
    (when (seq errors) (throw (ex-info "Invalid audio shape spec" {:errors errors :type ::invalid-shape}))))
  (let [spec (merge default-spec spec)
        design (filter-design (:band spec) sample-rate)
        size (ring-size (:band spec) sample-rate)]
    {:spec spec :sample-rate (double sample-rate) :design design
     :filter (vec (repeat (* 2 (count design)) 0.0))
     :ring (double-array size) :ring-filled 0 :ring-at 0
     :window nil :seq 0 :status :waiting}))

(defn- window-geometry
  "Window length and origin in transport beats for the clock's meter."
  [spec clock]
  (let [meter (time/normalize-meter (:meter clock))
        length (double (time/duration->beats (:window spec) meter))
        bar-origin (double (or (:bar-origin-beat clock) 0.0))
        origin (if (= :phrases (ffirst (:window spec)))
                 (double (or (:phrase-origin-beat clock) bar-origin))
                 bar-origin)]
    [length origin]))

(defn- playing-clock? [clock]
  (and (map? clock) (not (false? (:playing? clock)))
       (every? finite? [(:time-ms clock) (:beat clock) (:bpm clock)])
       (< 0 (:bpm clock) 1000)))

(defn- chronological
  "Copy of the ring's filled samples, oldest first."
  ^doubles [^doubles ring filled at]
  (let [size (alength ring) n (min filled size) out (double-array n)
        start (mod (- at n) size)]
    (dotimes [i n] (aset out i (aget ring (mod (+ start i) size))))
    out))

(defn- finish-window
  "Turn a complete accumulated window into an extraction."
  [{:keys [spec sample-rate] :as state} window ring filled at]
  (let [{:keys [extract points silence-db min-confidence band]} spec
        rms (Math/sqrt (/ (:sum-sq window) (max 1 (:n window))))
        level-db (* 20.0 (Math/log10 (max rms 1.0e-9)))
        meta {:seq (inc (:seq state)) :level-db level-db
              :window {:index (:index window) :start-beat (:start-beat window) :end-beat (:end-beat window)
                       :samples (:n window)}}]
    (merge meta
           (cond
             (< level-db silence-db) {:status :silent}
             (= :envelope extract) (analyze-envelope (:squares window) (:counts window))
             :else (let [^doubles values (chronological ring filled at)]
                     (if (< (alength values) 256)
                       {:status :low-confidence :confidence 0.0}
                       (analyze-cycle values sample-rate (frequency-search band sample-rate)
                                      points min-confidence)))))))

(defn- open-window [index length origin points complete?]
  {:index index :start-beat (+ origin (* index length)) :end-beat (+ origin (* (inc index) length))
   :complete? complete? :sum-sq 0.0 :n 0
   :squares (vec (repeat points 0.0)) :counts (vec (repeat points 0))})

(defn advance
  "Feed one captured mono block. block has :samples (doubles, -1..1),
  :sample-rate, :started-at-ms, and :clock (transport at capture: :time-ms,
  :beat, :bpm, :playing?, :meter, optional :bar-origin-beat). A stopped or
  missing transport, invalid timing or a discontinuity drops the open window:
  nothing is extracted for audio that is not one contiguous playing window,
  and after play resumes the first extraction is the next whole window.
  Returns {:state :extractions}."
  [state {:keys [samples sample-rate started-at-ms clock timing-valid? discontinuity?]
          :or {timing-valid? true}}]
  (when-not (= (double sample-rate) (:sample-rate state))
    (fail! "Block sample rate differs from the extractor" {:sample-rate sample-rate}))
  (let [^doubles samples samples
        n (alength samples)
        [filtered filter-state] (filter-samples (:design state) (if discontinuity? [] (:filter state)) samples)
        ^doubles filtered filtered
        ^doubles old-ring (:ring state)
        size (alength old-ring)
        ring (aclone old-ring)
        running? (and timing-valid? (not discontinuity?) (playing-clock? clock) (finite? started-at-ms))
        points (get-in state [:spec :points])]
    (if-not running?
      ;; Keep the filter warm and the ring current; drop the window.
      (let [at (:ring-at state)]
        (dotimes [i n] (aset ring (mod (+ at i) size) (aget filtered i)))
        {:state (assoc state :filter filter-state :ring ring :ring-at (mod (+ at n) size)
                       :ring-filled (min size (+ (:ring-filled state) n)) :window nil
                       :status (if (playing-clock? clock) :invalid-timing :paused))
         :extractions []})
      (let [[length origin] (window-geometry (:spec state) clock)
            length (double length) origin (double origin)
            rate (/ (double (:bpm clock)) 60000.0)
            beat0 (+ (double (:beat clock)) (* (- (double started-at-ms) (double (:time-ms clock))) rate))
            step (* rate (/ 1000.0 (double sample-rate)))
            ;; Accumulators for the open window live in private arrays for the
            ;; block and are written back as vectors when it ends.
            squares (double-array points) counts (long-array points)
            load! (fn [window]
                    (dotimes [p points]
                      (aset squares p (double (nth (:squares window) p)))
                      (aset counts p (long (nth (:counts window) p)))))
            store (fn [window sum-sq n]
                    (assoc window :sum-sq sum-sq :n n :squares (vec squares) :counts (vec counts)))
            fresh (fn [index complete?]
                    (java.util.Arrays/fill squares 0.0)
                    (java.util.Arrays/fill counts 0)
                    (open-window index length origin points complete?))]
        (when-let [window (:window state)] (load! window))
        (loop [i 0 window (:window state)
               sum-sq (double (:sum-sq (:window state) 0.0)) wn (long (:n (:window state) 0))
               at (long (:ring-at state)) filled (long (:ring-filled state))
               seq (long (:seq state)) extractions []]
          (if (= i n)
            {:state (assoc state :filter filter-state :ring ring :ring-at at :ring-filled filled
                           :window (when window (store window sum-sq wn)) :seq seq
                           :status (if (and window (:complete? window)) :listening :waiting))
             :extractions extractions}
            (let [beat (+ beat0 (* i step))
                  index (long (Math/floor (/ (- beat origin) length)))]
              (if (and window (= index (:index window)))
                (let [y (aget filtered i)
                      point (long (max 0 (min (dec points) (Math/floor (* points (/ (- beat (double (:start-beat window))) length))))))]
                  (aset squares point (+ (aget squares point) (* y y)))
                  (aset counts point (inc (aget counts point)))
                  (aset ring at y)
                  (recur (inc i) window (+ sum-sq (* y y)) (inc wn) (mod (inc at) size) (min size (inc filled))
                         seq extractions))
                ;; An edge: close the window that ended, then reprocess sample i
                ;; in the new one. A jump in either direction starts incomplete.
                (let [contiguous? (and window (= index (inc (:index window))))
                      extraction (when (and contiguous? (:complete? window))
                                   (finish-window (assoc state :seq seq) (store window sum-sq wn) ring filled at))
                      ;; A window opened on its first sample is whole too.
                      at-edge? (<= (- beat (+ origin (* index length))) (* 1.5 step))]
                  (recur i (fresh index (boolean (or contiguous? at-edge?))) 0.0 0 at filled
                         (if extraction (inc seq) seq)
                         (cond-> extractions extraction (conj extraction))))))))))))

(defn table-value
  "Periodic linear interpolation of a shape table at phase in cycles."
  ^double [table phase]
  (let [n (count table)
        u (* n (- (double phase) (Math/floor (double phase))))
        i (long (Math/floor u))
        f (- u i)
        a (double (nth table (mod i n)))
        b (double (nth table (mod (inc i) n)))]
    (+ a (* f (- b a)))))
