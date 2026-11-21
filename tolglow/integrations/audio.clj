(ns tolglow.integrations.audio
  "Experimental audio carrier and worker-side analysis. Arrays are privately
  allocated; no sound device, callback, wall clock or render thread is accessed.
  Pitch is monophonic. Polyphonic/plugin returns can be ambiguous; confidence
  gates the parameter update instead of producing an arbitrary value jump.")

(defn- finite? [x] (and (number? x) (Double/isFinite (double x))))
(defn- clamp [x low high] (max low (min high x)))
(defn- settings [options]
  (let [{:keys [sample-rate frequency-range bounds amplitude] :as opts}
        (merge {:sample-rate 48000.0 :frequency-range [80.0 2000.0]
                :bounds [0.0 1.0] :amplitude 0.5 :noise-floor 1.0e-5
                :pitch-threshold 0.15} options)
        [f-min f-max] frequency-range [low high] bounds]
    (when-not (and (finite? sample-rate) (pos? sample-rate)
                   (every? finite? [f-min f-max low high amplitude])
                   (< 0 f-min f-max (/ sample-rate 2.0)) (< low high)
                   (<= 0 amplitude 1))
      (throw (ex-info "Invalid audio sample rate, frequency range, bounds or amplitude" {})))
    opts))

(defn value->frequency
  "Logarithmically map a parameter value into the configured carrier range."
  [value options]
  (let [{[low high] :bounds [f-min f-max] :frequency-range} (settings options)]
    (when-not (finite? value) (throw (ex-info "Carrier value must be finite" {})))
    (* f-min (Math/pow (/ f-max f-min) (clamp (/ (- value low) (- high low)) 0.0 1.0)))))

(defn frequency->value
  "Inverse carrier mapping, clamped to schema bounds. Invalid pitch is rejected."
  [frequency options]
  (let [{[low high] :bounds [f-min f-max] :frequency-range} (settings options)]
    (when-not (and (finite? frequency) (pos? frequency))
      (throw (ex-info "Pitch must be finite and positive" {})))
    (+ low (* (- high low) (clamp (/ (Math/log (/ frequency f-min))
                                    (Math/log (/ f-max f-min))) 0.0 1.0)))))

(defn sine-block
  "Return {:samples double-array :state next-state}. State carries phase,
  frequency and amplitude across blocks. Frequency/amplitude ramp over the block
  to avoid discontinuities; output is newly owned and inputs are never modified."
  [options state value sample-count]
  (let [{:keys [sample-rate amplitude] :as opts} (settings options)
        _ (when-not (and (integer? sample-count) (<= 1 sample-count 1048576))
            (throw (ex-info "Carrier block size must be 1..1048576" {})))
        target (value->frequency value opts)
        from (get state :frequency target)
        from-amplitude (get state :amplitude amplitude)
        phase (get state :phase 0.0)
        _ (when-not (and (every? finite? [from from-amplitude phase])
                         (< 0 from (/ sample-rate 2)) (<= 0 from-amplitude 1))
            (throw (ex-info "Invalid carrier state" {})))
        out (double-array sample-count)
        final-phase (loop [index 0 phase (double phase)]
                      (if (= index sample-count)
                        phase
                        (let [fraction (/ (inc index) (double sample-count))
                              frequency (+ from (* fraction (- target from)))
                              level (+ from-amplitude (* fraction (- amplitude from-amplitude)))]
                          (aset-double out index (* level (Math/sin phase)))
                          (recur (inc index) (mod (+ phase (* 2.0 Math/PI (/ frequency sample-rate)))
                                                  (* 2.0 Math/PI))))))]
    {:samples out :state {:phase final-phase :frequency target :amplitude amplitude}}))

(defn- pitch [^doubles values sample-rate f-min f-max threshold]
  ;; YIN cumulative mean normalized difference with parabolic lag refinement.
  (let [n (alength values)
        max-lag (min (quot n 2) (long (Math/ceil (/ sample-rate f-min))))
        min-lag (max 2 (long (Math/floor (/ sample-rate f-max))))
        span (- n max-lag)
        cmnd (double-array (inc max-lag))]
    (aset-double cmnd 0 1.0)
    (loop [lag 1 total 0.0]
      (when (<= lag max-lag)
        (let [difference (loop [index 0 sum 0.0]
                           (if (= index span) sum
                               (let [delta (- (aget values index) (aget values (+ index lag)))]
                                 (recur (inc index) (+ sum (* delta delta))))))
              total (+ total difference)]
          (aset-double cmnd lag (if (zero? total) 1.0 (/ (* difference lag) total)))
          (recur (inc lag) total))))
    (when-let [lag (first (filter #(and (< (aget cmnd %) threshold)
                                        (<= (aget cmnd %) (aget cmnd (dec %)))
                                        (< (aget cmnd %) (aget cmnd (inc %))))
                                 (range min-lag max-lag)))]
      (let [left (aget cmnd (dec lag)) center (aget cmnd lag) right (aget cmnd (inc lag))
            divisor (* 2.0 (- (+ left right) (* 2.0 center)))
            delta (if (zero? divisor) 0.0 (/ (- left right) divisor))
            refined (+ lag (clamp delta -0.5 0.5))]
        {:frequency (/ sample-rate refined) :confidence (clamp (- 1.0 center) 0.0 1.0)}))))

(defn- spectrum [^doubles samples sample-rate]
  ;; Radix-2 FFT of a Hann-windowed, zero-padded block. Both work arrays are
  ;; private; callers receive values, never references to input sample storage.
  (let [input-n (alength samples)
        n (loop [n 1] (if (>= n input-n) n (recur (* n 2))))
        real (double-array n) imag (double-array n)]
    (dotimes [index input-n]
      (aset-double real index (* (aget samples index)
                                (* 0.5 (- 1.0 (Math/cos (/ (* 2.0 Math/PI index) (dec input-n))))))))
    (loop [i 1 j 0]
      (when (< i n)
        (let [j (loop [j j bit (bit-shift-right n 1)]
                  (if (not (zero? (bit-and j bit)))
                    (recur (bit-xor j bit) (bit-shift-right bit 1))
                    (bit-xor j bit)))]
          (when (< i j)
            (let [value (aget real i)] (aset-double real i (aget real j)) (aset-double real j value)))
          (recur (inc i) j))))
    (loop [size 2]
      (when (<= size n)
        (let [angle (/ (* -2.0 Math/PI) size) half (quot size 2)
              wr-step (Math/cos angle) wi-step (Math/sin angle)]
          (doseq [start (range 0 n size)]
            (loop [offset 0 wr 1.0 wi 0.0]
              (when (< offset half)
                (let [a (+ start offset) b (+ a half)
                      tr (- (* wr (aget real b)) (* wi (aget imag b)))
                      ti (+ (* wr (aget imag b)) (* wi (aget real b)))
                      ar (aget real a) ai (aget imag a)]
                  (aset-double real a (+ ar tr)) (aset-double imag a (+ ai ti))
                  (aset-double real b (- ar tr)) (aset-double imag b (- ai ti))
                  (recur (inc offset) (- (* wr wr-step) (* wi wi-step))
                         (+ (* wr wi-step) (* wi wr-step))))))))
        (recur (* size 2))))
    (let [power (mapv (fn [index] (+ (* (aget real index) (aget real index))
                                    (* (aget imag index) (aget imag index))))
                      (range 1 (inc (quot n 2))))
          total (reduce + 0.0 power)
          mean (/ total (count power))]
      {:power power
       :centroid-hz (if (zero? total) 0.0
                       (/ (reduce + (map-indexed #(* %2 (/ (* (inc %1) sample-rate) n)) power)) total))
       :flatness (if (zero? mean) 0.0
                    (/ (Math/exp (/ (reduce + (map #(Math/log (max 1.0e-30 %)) power)) (count power))) mean))})))

(defn- residual-ratio [^doubles samples sample-rate frequency]
  ;; Least-squares fit of sine and cosine: invariant to carrier gain and phase.
  ;; Residual quantifies departure from that reference, not plugin identity.
  (let [n (alength samples)
        [ss cc sc xs xc energy sum-s sum-c sum-x]
        (loop [i 0 ss 0.0 cc 0.0 sc 0.0 xs 0.0 xc 0.0 energy 0.0 sum-s 0.0 sum-c 0.0 sum-x 0.0]
          (if (= i n) [ss cc sc xs xc energy sum-s sum-c sum-x]
              (let [phase (/ (* 2.0 Math/PI frequency i) sample-rate)
                    s (Math/sin phase) c (Math/cos phase) x (aget samples i)]
                (recur (inc i) (+ ss (* s s)) (+ cc (* c c)) (+ sc (* s c))
                       (+ xs (* x s)) (+ xc (* x c)) (+ energy (* x x))
                       (+ sum-s s) (+ sum-c c) (+ sum-x x)))))
        ;; Center the reference bases too: a finite sine window need not have
        ;; zero mean. This fits a constant offset without treating it as texture.
        ss (- ss (/ (* sum-s sum-s) n)) cc (- cc (/ (* sum-c sum-c) n))
        sc (- sc (/ (* sum-s sum-c) n)) xs (- xs (/ (* sum-x sum-s) n))
        xc (- xc (/ (* sum-x sum-c) n)) energy (- energy (/ (* sum-x sum-x) n))
        det (- (* ss cc) (* sc sc))]
    (if (or (zero? energy) (< (Math/abs det) 1.0e-12))
      0.0
      (let [a (/ (- (* xs cc) (* xc sc)) det)
            b (/ (- (* xc ss) (* xs sc)) det)]
        (clamp (/ (- energy (+ (* a xs) (* b xc))) energy) 0.0 1.0)))))

(defn analyze-block
  "Analyze isolated mono audio. Returns RMS/peak, YIN pitch/confidence, bounded
  decoded value, FFT centroid/flatness/flux and gain-invariant fitted-sine residual.
  :reference-frequency aligns texture comparison with the emitted carrier.
  :previous-spectrum enables normalized positive spectral flux. Latency is the
  full analysis-window duration, before routing/processing latency. Run off-thread."
  [samples options]
  (let [{:keys [sample-rate frequency-range noise-floor pitch-threshold] :as opts} (settings options)
        values (double-array samples) n (alength values)
        _ (when-not (and (<= 64 n 16384) (every? finite? values)
                         (finite? noise-floor) (not (neg? noise-floor))
                         (finite? pitch-threshold) (< 0 pitch-threshold 1))
            (throw (ex-info "Analysis needs 64..16384 finite samples and valid thresholds" {})))
        mean (/ (reduce + 0.0 values) n)
        centered (double-array (map #(- % mean) values))
        rms (Math/sqrt (/ (reduce + 0.0 (map #(* % %) values)) n))
        peak (reduce max 0.0 (map #(Math/abs (double %)) values))
        _ (when-not (finite? rms) (throw (ex-info "Sample energy exceeds numeric range" {})))
        detected (when (> rms noise-floor)
                   (pitch centered sample-rate (first frequency-range) (second frequency-range) pitch-threshold))
        frequency (:frequency detected)
        reference (or (:reference-frequency opts) frequency)
        _ (when (and reference (not (and (finite? reference) (< 0 reference (/ sample-rate 2)))))
            (throw (ex-info "Reference frequency must be below Nyquist" {})))
        spec (spectrum centered sample-rate)
        power (:power spec)
        previous (:previous-spectrum opts)
        normalized (fn [xs] (let [total (reduce + 0.0 xs)] (mapv #(/ % (max 1.0e-30 total)) xs)))
        flux (when (and previous (= (count previous) (count power)))
               (reduce + 0.0 (map #(max 0.0 (- %1 %2)) (normalized power) (normalized previous))))]
    {:rms rms :peak peak :dc mean :pitch-hz frequency :pitch-confidence (get detected :confidence 0.0)
     :value (when frequency (frequency->value frequency opts))
     :spectral-centroid-hz (:centroid-hz spec) :spectral-flatness (:flatness spec)
     :spectral-flux flux :spectral-residual (when reference (residual-ratio centered sample-rate reference))
     :reference-frequency reference :spectrum power :window-samples n :sample-rate sample-rate
     :window-latency-ms (* 1000.0 (/ n sample-rate)) :continuity :block}))

(defn new-tail
  "Capture an explicit target {:show-id :epoch :instance-id :param}. Caller must
  retain the corresponding effect/route; this function does not resurrect it."
  [target time-ms options]
  (when-not (and (every? #(contains? target %) [:show-id :epoch :instance-id :param])
                 (finite? time-ms) (<= 0 time-ms))
    (throw (ex-info "Tail needs exact target identity and logical time" {})))
  (let [opts (merge {:max-tail-ms 10000.0 :silence-hold-ms 500.0 :noise-floor 1.0e-4
                     :confidence-min 0.8 :bounds [0.0 1.0]} options)
        [low high] (:bounds opts)]
    (when-not (and (every? #(and (finite? %) (pos? %)) ((juxt :max-tail-ms :silence-hold-ms) opts))
                   (finite? (:noise-floor opts)) (<= 0 (:noise-floor opts))
                   (finite? (:confidence-min opts)) (<= 0 (:confidence-min opts) 1)
                   (every? finite? [low high]) (< low high))
      (throw (ex-info "Invalid tail thresholds" {})))
    {:target target :started-at-ms time-ms :last-time-ms time-ms :silent-since nil
     :last-value nil :options opts}))

(defn advance-tail
  "Retain echo/reverb response after source release until sustained silence or
  max duration. Emits param commands only for confident finite values, targeting
  the captured instance. Epoch/show mismatches expire the tail. No return audio
  is automatically fed back into the sine encoder."
  [tail features time-ms envelope]
  (when-not (and (finite? time-ms) (>= time-ms (:last-time-ms tail))
                 (finite? (:rms features)) (<= 0 (:rms features)))
    (throw (ex-info "Tail frames need monotonic time and finite nonnegative RMS" {})))
  (let [{:keys [target started-at-ms options]} tail
        silent? (<= (:rms features) (:noise-floor options))
        silent-since (when silent? (or (:silent-since tail) time-ms))
        stale? (not= (select-keys target [:show-id :epoch]) (select-keys envelope [:show-id :epoch]))
        expired? (or stale? (>= (- time-ms started-at-ms) (:max-tail-ms options))
                     (and silent-since (>= (- time-ms silent-since) (:silence-hold-ms options))))
        value (when (and (not silent?) (finite? (:value features))
                          (finite? (:pitch-confidence features))
                          (>= (:pitch-confidence features) (:confidence-min options)))
                (clamp (:value features) (first (:bounds options)) (second (:bounds options))))]
    {:active? (not expired?)
     :tail (when-not expired? (cond-> (assoc tail :last-time-ms time-ms :silent-since silent-since)
                               value (assoc :last-value value)))
     :reason (when expired? (cond stale? :stale-epoch
                                 (>= (- time-ms started-at-ms) (:max-tail-ms options)) :max-duration
                                 :else :silence))
     :commands (if (and (not expired?) (some? value))
                 [(merge envelope target {:op :param/set :value value :time-ms time-ms
                                          :source {:type :audio-return :origin :processed-audio}})] [])}))
