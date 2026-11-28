(ns tolglow.audio.shape
  "Band isolation and beat-windowed shape extraction for audio-shaped LFOs
  (#676), split by thread. `advance-bank` is the capture thread's share: one
  filter and sample ring per distinct band, each LFO's window accumulation, and
  an immutable job when a window closes. `finish-job` is pure and does the
  window-end analysis (bounded, decimated YIN for cycles) on a separate worker.
  Nothing here opens a device, reads a clock or publishes to a show."
  (:require [afterglow.time :as time]))

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
    (cond (= :full band) "full range"
          (keyword? band) (str (name band) " " (hz low) "–" (hz high))
          :else (str (hz low) "–" (hz high)))))

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
  modified. A non-finite sample is read as 0.0, so one NaN or infinity cannot
  poison the filter state for the rest of the run."
  [design state ^doubles samples]
  (let [sections (count design)
        n (alength samples)
        out (double-array n)
        z (double-array (* 2 sections))
        coefficients (double-array (mapcat identity design))]
    (dotimes [i (min (count state) (alength z))] (aset z i (double (nth state i))))
    (dotimes [i n]
      (loop [s 0 x (let [v (aget samples i)] (if (Double/isFinite v) v 0.0))]
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

(defn decimate
  "Average each run of factor samples into one, aligned to the array end. The
  boxcar is the anti-alias filter; callers decimate only band-limited audio."
  ^doubles [^doubles values factor]
  (let [factor (long factor)]
    (if (= 1 factor)
      values
      (let [n (quot (alength values) factor)
            out (double-array n)
            offset (- (alength values) (* n factor))]
        (dotimes [i n]
          (let [base (+ offset (* i factor))]
            (aset out i (/ (double (loop [k 0 sum 0.0] (if (= k factor) sum (recur (inc k) (+ sum (aget values (+ base k)))))))
                           (double factor)))))
        out))))

(defn- difference
  "Squared difference of the last span samples against those lag earlier."
  ^double [^doubles values ^long span ^long lag]
  (let [end (alength values) start (- end span)]
    (loop [j start sum 0.0]
      (if (= j end) sum
          (let [delta (- (aget values j) (aget values (- j lag)))]
            (recur (inc j) (+ sum (* delta delta))))))))

(def yin-threshold
  "A dip in the normalised difference below this picks the period; otherwise
  the deepest dip in range does."
  0.2)

(defn yin
  "Bounded YIN over the last 2 × max-lag samples of values: the cumulative-mean
  normalised difference d′ for lags 1..max-lag. Returns {:lag fractional
  :aperiodicity d′} for the first local minimum below `yin-threshold` at or
  above min-lag, else the deepest one in range, or nil when values are too
  short. Confidence is 1 − aperiodicity."
  [^doubles values min-lag max-lag]
  (let [min-lag (long (max 2 min-lag)) max-lag (long max-lag)]
    (when (and (< min-lag max-lag) (>= (alength values) (* 2 max-lag)))
      (let [cmnd (double-array (+ 2 max-lag))]
        (aset cmnd 0 1.0)
        (loop [lag 1 total 0.0]
          (when (<= lag (inc max-lag))
            (let [d (if (> lag max-lag) (aget cmnd max-lag) (difference values max-lag lag))
                  total (+ total d)]
              (when (<= lag max-lag)
                (aset cmnd lag (if (zero? total) 1.0 (/ (* d lag) total))))
              (recur (inc lag) total))))
        (aset cmnd (inc max-lag) (aget cmnd max-lag))
        (let [dips (filter #(and (<= (aget cmnd %) (aget cmnd (dec %))) (<= (aget cmnd %) (aget cmnd (inc %))))
                           (range min-lag max-lag))
              lag (or (first (filter #(< (aget cmnd %) yin-threshold) dips))
                      (when (seq dips) (apply min-key #(aget cmnd %) dips)))]
          (when lag
            (let [left (aget cmnd (dec lag)) center (aget cmnd lag) right (aget cmnd (inc lag))
                  divisor (* 2.0 (- (+ left right) (* 2.0 center)))
                  delta (if (zero? divisor) 0.0 (max -0.5 (min 0.5 (/ (- left right) divisor))))]
              {:lag (+ lag delta) :aperiodicity (max 0.0 (min 1.0 center))})))))))

(defn- refine-period
  "Sharpen a coarse period (in samples of values) by the raw difference over
  lags within ± radius, with parabolic interpolation."
  [^doubles values period radius]
  (let [low (long (max 2 (Math/floor (- period radius))))
        high (long (Math/ceil (+ period radius)))
        span (long (min (- (alength values) (inc high)) (* 2 high)))]
    (if (or (< span 16) (>= high (alength values)))
      period
      (let [d (fn [lag] (difference values span lag))
            best (apply min-key d (range low (inc high)))]
        (if (or (= best low) (= best high))
          (double best)
          (let [left (d (dec best)) center (d best) right (d (inc best))
                divisor (* 2.0 (- (+ left right) (* 2.0 center)))]
            (+ best (if (zero? divisor) 0.0 (max -0.5 (min 0.5 (/ (- left right) divisor)))))))))))

(defn analysis-plan
  "How a cycle window is searched: the frequency range, the decimation factor
  applied before YIN (one for an unfiltered band), and the decimated lag range.
  YIN costs about max-lag² operations."
  [band sample-rate]
  (let [[low high] (band-range band)
        [f-min f-max] (if low
                        [(max 20.0 low) (min high (* 0.25 sample-rate))]
                        [30.0 (min 2000.0 (* 0.25 sample-rate))])
        factor (if low (long (max 1 (min 16 (Math/floor (/ sample-rate (* 8.0 f-max)))))) 1)
        rate (/ sample-rate factor)]
    {:frequency-range [f-min f-max] :factor factor
     :min-lag (long (Math/floor (/ rate f-max))) :max-lag (long (Math/ceil (/ rate f-min)))}))

(defn analyze-cycle
  "Find the fundamental of a chronological band-limited window and return
  {:status :ok :shape :frequency-hz :confidence :periods :input} or a
  {:status :low-confidence} map. Bounded YIN runs on the decimated end of the
  window, the period is refined at the full rate, and up to eight periods are
  averaged. Confidence is 1 − YIN aperiodicity; below min-confidence the shape
  is withheld."
  [^doubles window sample-rate band points min-confidence]
  (let [{:keys [factor min-lag max-lag]} (analysis-plan band sample-rate)
        tail (let [needed (min (alength window) (* factor 2 max-lag))]
               (java.util.Arrays/copyOfRange window (int (- (alength window) needed)) (int (alength window))))
        estimate (yin (decimate tail factor) min-lag max-lag)]
    (if-not estimate
      {:status :low-confidence :confidence 0.0}
      (let [confidence (- 1.0 (:aperiodicity estimate))
            period (refine-period window (* factor (:lag estimate)) (max 1 factor))
            frequency (/ sample-rate period)
            periods (long (max 1 (min 8 (dec (Math/floor (/ (alength window) period))))))
            preview (let [span (min (alength window) (long (Math/ceil (* 2 period))))
                          start (- (alength window) span)]
                      (mapv #(ring-sample window (+ start (* (dec span) (/ % 127.0)))) (range 128)))
            input {:kind :pcm :values preview :duration-ms (* 2000.0 (/ period sample-rate))}]
        (if (< confidence min-confidence)
          {:status :low-confidence :confidence confidence :frequency-hz frequency :input input}
          (let [provisional (cycle-points window period periods 0.0 points)
                ;; Rotate so the fundamental starts at zero phase, rising.
                offset (let [phase (- (fundamental-phase (let [mean (/ (reduce + provisional) points)]
                                                           (mapv #(- % mean) provisional))))]
                         (- phase (Math/floor phase)))
                aligned (cycle-points window period periods offset points)
                mean (/ (reduce + aligned) points)
                shape (normalize (mapv #(- % mean) aligned) :range)]
            (if shape
              {:status :ok :shape shape :frequency-hz frequency :confidence confidence :periods periods :input input}
              {:status :low-confidence :confidence confidence :frequency-hz frequency :input input})))))))

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

;; Extractor bank ----------------------------------------------------------------

(def default-spec
  {:band :bass :extract :envelope :window {:bars 1} :points 32
   :silence-db -50.0 :min-confidence 0.8})

(def default-tail-guard
  "How far before the bar line a cycle's analysed tail ends, in beats, when
  the spec has no `:tail-guard` (#912). Audio that leads the transport by less
  than this keeps the next bar's first attack out of the tail."
  {:beats 0.03125})

(def tail-guard-limits
  "The guard in milliseconds is kept within these bounds whatever the tempo:
  at least 5 ms at a fast tempo, and at most what every band's ring holds."
  {:min-ms 5.0 :max-ms 60.0 :max-beats 0.125})

(defn tail-guard-ms
  "The cycle tail guard in milliseconds at a tempo: the spec's `:tail-guard`
  beats (default 1/32 beat), clamped to `tail-guard-limits`."
  [spec bpm]
  (let [beats (double (:beats (or (:tail-guard spec) default-tail-guard)))]
    (max (:min-ms tail-guard-limits) (min (:max-ms tail-guard-limits) (* beats (/ 60000.0 (double bpm)))))))

(defn validate-spec
  "Return [] or path/message errors for an extractor spec."
  [spec]
  (let [{:keys [band extract window points silence-db min-confidence tail-guard]} (merge default-spec spec)
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
            (problem [:min-confidence] "Minimum confidence must be 0..1"))
          (when-not (or (nil? tail-guard)
                        (and (map? tail-guard) (= [:beats] (keys tail-guard)) (finite? (:beats tail-guard))
                             (< 0 (:beats tail-guard)) (<= (:beats tail-guard) (:max-beats tail-guard-limits))))
            (problem [:tail-guard] "Tail guard is {:beats x} with x above 0 and at most 1/8"))))))

(defn ring-size
  "Samples a band keeps: at least four periods of the lowest frequency, and
  YIN's 2 × max-lag (at the full rate) plus the longest tail guard, so the
  analysed tail always fits before the guard (#912). A power of two from 1024
  to 32768; at 48 kHz this is 8192 for the full range and 16384 for :sub."
  [band sample-rate]
  (let [{:keys [frequency-range factor max-lag]} (analysis-plan band sample-rate)
        periods (long (Math/ceil (* 4.0 (/ sample-rate (first frequency-range)))))
        tail (+ (* 2 factor max-lag) (long (Math/ceil (* sample-rate (/ (:max-ms tail-guard-limits) 1000.0)))))
        needed (max periods tail)]
    (long (min 32768 (max 1024 (Long/highestOneBit (dec (* 2 needed))))))))

(defn new-bank
  "An empty extractor bank for one sample rate. A bank is owned by one thread,
  the capture worker: it keeps one band filter and one sample ring per distinct
  band, shared by every LFO on that band, and each LFO's open window. Its
  primitive buffers are mutated in place and never leave it; window-end jobs
  carry fresh copies."
  [sample-rate]
  (when-not (and (finite? sample-rate) (<= 8000 sample-rate 192000))
    (fail! "Sample rate must be 8–192 kHz" {:sample-rate sample-rate}))
  {:sample-rate (double sample-rate) :fronts {} :lfos {}})

(defn- new-front [band sample-rate]
  (let [design (filter-design band sample-rate)]
    {:band band :design design :coefficients (double-array (mapcat identity design))
     :filter (double-array (* 2 (count design))) :ring (double-array (ring-size band sample-rate))
     :at 0 :filled 0}))

(defn- filter-block!
  "Filter samples through a front's sections, updating its state in place."
  ^doubles [{:keys [design ^doubles coefficients ^doubles filter]} ^doubles samples]
  (let [sections (count design) n (alength samples) out (double-array n)]
    (dotimes [i n]
      (loop [s 0 x (let [v (aget samples i)] (if (Double/isFinite v) v 0.0))]
        (if (= s sections)
          (aset out i x)
          (let [c (* 5 s) zi (* 2 s)
                y (+ (* (aget coefficients c) x) (aget filter zi))]
            (aset filter zi (- (+ (* (aget coefficients (+ c 1)) x) (aget filter (inc zi)))
                               (* (aget coefficients (+ c 3)) y)))
            (aset filter (inc zi) (- (* (aget coefficients (+ c 2)) x) (* (aget coefficients (+ c 4)) y)))
            (recur (inc s) y)))))
    out))

(defn- ring-tail
  "A fresh chronological copy of the front's ring followed by the first upto
  samples of filtered, without its last guard samples: everything captured
  more than guard samples before sample upto, at most the ring's length less
  the guard."
  ^doubles [{:keys [^doubles ring at filled]} ^doubles filtered upto guard]
  (let [size (alength ring)
        upto (long upto)
        from-ring (min (long filled) (- size (min size upto)))
        from-block (min size upto)
        kept (max 0 (- (+ from-ring from-block) (long guard)))
        ring-part (min kept from-ring)
        block-part (- kept ring-part)
        out (double-array kept)
        start (mod (- (long at) from-ring) size)]
    (dotimes [i ring-part] (aset out i (aget ring (mod (+ start i) size))))
    (System/arraycopy filtered (int (- upto from-block)) out (int ring-part) (int block-part))
    out))

(defn- write-ring [{:keys [^doubles ring at filled] :as front} ^doubles filtered]
  (let [size (alength ring) n (alength filtered)]
    (if (>= n size)
      (System/arraycopy filtered (int (- n size)) ring 0 (int size))
      (dotimes [i n] (aset ring (mod (+ (long at) i) size) (aget filtered i))))
    (assoc front :at (if (>= n size) 0 (mod (+ (long at) n) size)) :filled (min size (+ (long filled) n)))))

(defn- new-lfo [spec digest points]
  {:spec spec :digest digest :window nil :seq 0 :status :waiting
   :squares (double-array points) :counts (long-array points) :sum-sq 0.0 :n 0})

(defn- reconcile
  "Fit the bank to the requested specs ({id spec}, each with an optional
  :digest): new or changed specs start fresh, missing ones are dropped, and
  a front lives exactly as long as an LFO uses its band."
  [bank specs]
  (let [lfos (into {} (for [[id spec] specs
                            :let [digest (get spec :digest spec)
                                  current (get-in bank [:lfos id])]]
                        [id (if (= digest (:digest current)) current
                                (let [spec (merge default-spec (dissoc spec :digest))]
                                  (new-lfo spec digest (:points spec))))]))
        bands (set (map (comp :band :spec) (vals lfos)))]
    (assoc bank :lfos lfos
           :fronts (into {} (for [band bands]
                              [band (or (get-in bank [:fronts band]) (new-front band (:sample-rate bank)))])))))

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

(defn- window-job
  "The immutable hand-off for a closed window. A cycle job carries a fresh copy
  of the band's samples up to the tail guard before the window edge (#912);
  nothing else holds it. edge-beat is the transport beat of sample at-sample,
  the window's first sample after the edge, and step the beats per sample."
  [id {:keys [spec digest] :as lfo} window sum-sq n ^doubles squares ^longs counts front filtered at-sample sample-rate
   edge-beat step bpm]
  (let [cycle? (= :cycle (:extract spec))
        guard-ms (when cycle? (tail-guard-ms spec bpm))
        guard (if cycle? (long (Math/round (* (double guard-ms) (/ (double sample-rate) 1000.0)))) 0)]
    (cond-> {:id id :spec spec :digest digest :seq (inc (:seq lfo)) :sample-rate sample-rate
             :window (assoc (select-keys window [:index :start-beat :end-beat]) :samples n)
             :analysed-until-beat (- (double edge-beat) (* guard (double step)))
             :sum-sq sum-sq :n n}
      (= :envelope (:extract spec)) (assoc :squares (vec squares) :counts (vec counts))
      cycle? (assoc :samples (ring-tail front filtered at-sample guard)
                    :tail-guard {:beats (* guard (double step)) :ms (* 1000.0 (/ guard (double sample-rate)))}))))

(defn- advance-lfo
  "Run one LFO's window accumulation over a filtered block. Returns [lfo jobs]."
  [id lfo front ^doubles filtered {:keys [started-at-ms clock sample-rate]}]
  (let [spec (:spec lfo)
        points (long (:points spec))
        ^doubles squares (:squares lfo) ^longs counts (:counts lfo)
        [length origin] (window-geometry spec clock)
        length (double length) origin (double origin)
        rate (/ (double (:bpm clock)) 60000.0)
        beat0 (+ (double (:beat clock)) (* (- (double started-at-ms) (double (:time-ms clock))) rate))
        step (* rate (/ 1000.0 (double sample-rate)))
        n (alength filtered)
        open! (fn [index complete?]
                (java.util.Arrays/fill squares 0.0)
                (java.util.Arrays/fill counts 0)
                {:index index :start-beat (+ origin (* index length)) :end-beat (+ origin (* (inc index) length))
                 :complete? complete?})]
    (loop [i 0 window (:window lfo)
           windex (long (:index (:window lfo) Long/MIN_VALUE))
           wstart (double (:start-beat (:window lfo) 0.0))
           sum-sq (double (:sum-sq lfo)) wn (long (:n lfo))
           seq (long (:seq lfo)) jobs []]
      (if (= i n)
        [(assoc lfo :window window :sum-sq sum-sq :n wn :seq seq
                :status (if (and window (:complete? window)) :listening :waiting))
         jobs]
        (let [beat (+ beat0 (* i step))
              index (long (Math/floor (/ (- beat origin) length)))]
          (if (and window (= index windex))
            (let [y (aget filtered i)
                  point (long (max 0 (min (dec points) (Math/floor (* points (/ (- beat wstart) length))))))]
              (aset squares point (+ (aget squares point) (* y y)))
              (aset counts point (inc (aget counts point)))
              (recur (inc i) window windex wstart (+ sum-sq (* y y)) (inc wn) seq jobs))
            ;; An edge: hand off the window that ended, then reprocess sample i
            ;; in the new one. A jump in either direction starts incomplete.
            (let [contiguous? (and window (= index (inc windex)))
                  job (when (and contiguous? (:complete? window))
                        (window-job id (assoc lfo :seq seq) window sum-sq wn squares counts front filtered i sample-rate
                                    beat step (:bpm clock)))
                  ;; A window opened on its first sample is whole too.
                  at-edge? (<= (- beat (+ origin (* index length))) (* 1.5 step))
                  opened (open! index (boolean (or contiguous? at-edge?)))]
              (recur i opened index (double (:start-beat opened)) 0.0 0
                     (if job (inc seq) seq) (cond-> jobs job (conj job))))))))))

(defn advance-bank
  "Feed one captured mono block to every requested LFO. specs maps an LFO ID
  to its extractor spec (with an optional :digest identity); block has
  :samples (-1..1), :sample-rate, :started-at-ms and :clock (transport at
  capture: :time-ms, :beat, :bpm, :playing?, :meter, optional bar and phrase
  origins). Each distinct band is filtered once into its shared ring. A stopped
  or missing transport, invalid timing or a discontinuity drops every open
  window: nothing is shaped from audio that is not one contiguous playing
  window, and after play resumes the first job is the next whole window.

  This is the capture thread's whole share: accumulation and copies. It
  returns {:bank :jobs}; a job is finished by `finish-job`, on another thread."
  [bank specs {:keys [samples sample-rate started-at-ms clock timing-valid? discontinuity?]
               :or {timing-valid? true} :as block}]
  (let [bank (if (= (double sample-rate) (:sample-rate bank)) bank (new-bank sample-rate))
        bank (reconcile bank (or specs {}))
        running? (and timing-valid? (not discontinuity?) (playing-clock? clock) (finite? started-at-ms))
        status (if running? nil (if (playing-clock? clock) :invalid-timing :paused))]
    (reduce-kv
     (fn [{:keys [bank jobs]} band front]
       (when discontinuity? (java.util.Arrays/fill ^doubles (:filter front) 0.0))
       (let [filtered (filter-block! front samples)
             [bank jobs] (reduce-kv
                          (fn [[bank jobs] id lfo]
                            (if (not= band (get-in lfo [:spec :band]))
                              [bank jobs]
                              (if running?
                                (let [[lfo new-jobs] (advance-lfo id lfo front filtered block)]
                                  [(assoc-in bank [:lfos id] lfo) (into jobs new-jobs)])
                                [(assoc-in bank [:lfos id] (assoc lfo :window nil :sum-sq 0.0 :n 0 :status status))
                                 jobs])))
                          [bank jobs] (:lfos bank))]
         {:bank (assoc-in bank [:fronts band] (write-ring front filtered)) :jobs jobs}))
     {:bank bank :jobs []} (:fronts bank))))

(defn finish-job
  "Turn a window job into an extraction. Pure and potentially costly (cycle
  mode runs YIN): call it off the capture thread. The extraction reports the
  transport beat its analysis reads up to, `:analysed-until-beat`, and for a
  cycle the `:tail-guard` before the window edge in beats and ms (#912)."
  [{:keys [spec digest seq window sum-sq n squares counts samples sample-rate analysed-until-beat tail-guard]}]
  (let [{:keys [extract points silence-db min-confidence band]} spec
        rms (Math/sqrt (/ sum-sq (max 1 n)))
        level-db (* 20.0 (Math/log10 (max rms 1.0e-9)))]
    (merge (cond-> {:seq seq :level-db level-db :window window :spec-digest digest}
             analysed-until-beat (assoc :analysed-until-beat analysed-until-beat)
             tail-guard (assoc :tail-guard tail-guard))
           (cond
             (< level-db silence-db) {:status :silent}
             (= :envelope extract) (analyze-envelope squares counts)
             (< (alength ^doubles samples) 256) {:status :low-confidence :confidence 0.0}
             :else (analyze-cycle samples sample-rate band points min-confidence)))))

;; Single-LFO convenience ------------------------------------------------------------

(defn new-extractor
  "A bank with one LFO, for tests, the REPL and offline replay. A window that
  opens part way through is discarded, so extraction begins at the next edge."
  [spec sample-rate]
  (let [errors (validate-spec spec)]
    (when (seq errors) (throw (ex-info "Invalid audio shape spec" {:errors errors :type ::invalid-shape}))))
  (assoc (new-bank sample-rate) ::spec spec :status :waiting))

(defn advance
  "Feed one block to a `new-extractor` bank and finish its jobs inline.
  Returns {:state :extractions}; :status of the state is the LFO's."
  [state block]
  (when-not (= (double (:sample-rate block)) (:sample-rate state))
    (fail! "Block sample rate differs from the extractor" {:sample-rate (:sample-rate block)}))
  (let [{:keys [bank jobs]} (advance-bank state {::one (::spec state)} block)]
    {:state (assoc bank :status (get-in bank [:lfos ::one :status]))
     :extractions (mapv finish-job jobs)}))

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
