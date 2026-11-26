(ns tolglow.audio.fixtures
  "Deterministic synthetic passages for audio-shaped LFO checks (#676). Each
  passage is a pure function of sample index, tempo and seed and follows a
  transport that starts at beat 0, time 0 and plays at a steady tempo. They are
  test material, not instrument models.

  - `:dry-sine` a steady 110 Hz sine.
  - `:delay-synth` a bass synth alternating saw and square bars, through a
    3/8-beat feedback delay and a small reverb, so tails cross window edges.
  - `:guitar-phrase` Karplus–Strong plucks for one bar, then a silent bar.")

(def passages [:dry-sine :delay-synth :guitar-phrase])

(defn- beat-of [index sample-rate bpm] (* (/ index sample-rate) (/ bpm 60.0)))

(defn- dry-sine [n sample-rate _bpm _seed]
  (let [out (double-array n)]
    (dotimes [i n] (aset out i (* 0.5 (Math/sin (/ (* 2.0 Math/PI 110.0 i) sample-rate)))))
    out))

(defn- comb
  "Feedback comb in place of a reverb tap; returns a new array."
  ^doubles [^doubles input ^long delay ^double feedback]
  (let [n (alength input) out (double-array n)]
    (dotimes [i n]
      (aset out i (+ (aget input i) (if (>= i delay) (* feedback (aget out (- i delay))) 0.0))))
    out))

(defn- allpass ^doubles [^doubles input ^long delay ^double gain]
  (let [n (alength input) out (double-array n)]
    (dotimes [i n]
      (let [x (aget input i)
            delayed-x (if (>= i delay) (aget input (- i delay)) 0.0)
            delayed-y (if (>= i delay) (aget out (- i delay)) 0.0)]
        (aset out i (+ (* (- gain) x) delayed-x (* gain delayed-y)))))
    out))

(defn- delay-synth [n sample-rate bpm _seed]
  (let [dry (double-array n)
        beat-samples (/ (* 60.0 sample-rate) bpm)]
    (loop [i 0 phase 0.0 low 0.0]
      (when (< i n)
        (let [beat (beat-of i sample-rate bpm)
              bar (long (Math/floor (/ beat 4.0)))
              step (long (Math/floor beat))
              frequency (if (even? step) 55.0 82.5)
              into-beat (- beat step)
              envelope (* (min 1.0 (/ (* into-beat beat-samples) (* 0.005 sample-rate)))
                          (Math/exp (* -3.0 into-beat)))
              raw (if (even? bar) (- (* 2.0 phase) 1.0) (if (< phase 0.5) 1.0 -1.0))
              ;; One-pole low-pass softens the oscillator.
              low (+ low (* 0.08 (- raw low)))]
          (aset dry i (* 0.45 envelope low))
          (recur (inc i) (let [p (+ phase (/ frequency sample-rate))] (- p (Math/floor p))) low))))
    (let [delay (long (Math/round (* 0.375 beat-samples)))
          ^doubles echoed (let [out (double-array n)]
                   (dotimes [i n]
                     (aset out i (+ (aget dry i) (if (>= i delay) (* 0.45 (aget out (- i delay))) 0.0))))
                   out)
          scale (/ sample-rate 48000.0)
          taps (map #(long (* scale %)) [1557 1617 1491 1422])
          wet (let [sums (double-array n)]
                (doseq [d taps]
                  (let [^doubles c (comb echoed d 0.78)] (dotimes [i n] (aset sums i (+ (aget sums i) (* 0.25 (aget c i))))))) sums)
          ^doubles wet (allpass (allpass wet (long (* scale 225)) 0.5) (long (* scale 556)) 0.5)
          out (double-array n)]
      (dotimes [i n] (aset out i (max -1.0 (min 1.0 (+ (* 0.7 (aget echoed i)) (* 0.18 (aget wet i)))))))
      out)))

(defn- guitar-phrase [n ^double sample-rate ^double bpm seed]
  (let [out (double-array n)
        random (java.util.Random. (long seed))
        ;; Plucks in beats within each two-bar phrase, and their pitches.
        plucks [[0.0 82.41] [1.5 110.0] [2.0 146.83] [3.0 110.0]]
        phrase-beats 8.0
        total-beats (beat-of n sample-rate bpm)]
    (doseq [phrase (range (long (Math/ceil (/ total-beats phrase-beats))))
            [at frequency] plucks
            :let [start (long (Math/round (* (/ (+ (* phrase phrase-beats) at) (/ bpm 60.0)) sample-rate)))
                  ;; Each note is damped before the silent bar begins.
                  stop (min n (long (Math/round (* (/ (+ (* phrase phrase-beats) 4.0) (/ bpm 60.0)) sample-rate))))
                  period (max 2 (long (Math/round (/ sample-rate frequency))))
                  string (double-array period)]
            :when (< start stop)]
      (dotimes [k period] (aset string k (- (* 2.0 (.nextDouble random)) 1.0)))
      (loop [i start k 0]
        (when (< i stop)
          (let [a (aget string (mod k period)) b (aget string (mod (inc k) period))
                v (* 0.996 0.5 (+ a b))]
            (aset string (mod k period) v)
            (aset out i (max -1.0 (min 1.0 (+ (aget out i) (* 0.35 a)))))
            (recur (inc i) (inc k))))))
    out))

(defn passage
  "Mono PCM for a named passage. Options: :sample-rate (48000), :bpm (120),
  :bars (8), :seed (676). Returns a fresh double array in -1..1."
  [kind {:keys [sample-rate bpm bars seed] :or {sample-rate 48000.0 bpm 120.0 bars 8 seed 676}}]
  (when-not (and (<= 8000 sample-rate 192000) (<= 40 bpm 300) (pos-int? bars) (<= bars 64))
    (throw (ex-info "Passage needs 8–192 kHz, 40–300 BPM and 1–64 bars" {:sample-rate sample-rate :bpm bpm :bars bars})))
  (let [n (long (Math/round (* bars 4.0 (/ 60.0 bpm) sample-rate)))]
    (case kind
      :dry-sine (dry-sine n (double sample-rate) (double bpm) seed)
      :delay-synth (delay-synth n (double sample-rate) (double bpm) seed)
      :guitar-phrase (guitar-phrase n (double sample-rate) (double bpm) seed)
      (throw (ex-info "Unknown passage" {:kind kind})))))

(defn pcm16-bytes
  "Little-endian signed-16 mono bytes for a passage, as a capture source reads."
  ^bytes [^doubles samples]
  (let [out (byte-array (* 2 (alength samples)))]
    (dotimes [i (alength samples)]
      (let [v (long (Math/round (* 32767.0 (max -1.0 (min 1.0 (aget samples i))))))]
        (aset-byte out (* 2 i) (unchecked-byte (bit-and v 255)))
        (aset-byte out (inc (* 2 i)) (unchecked-byte (bit-shift-right v 8)))))
    out))

(defn wav-bytes
  "A 16-bit mono PCM WAV file image of a passage."
  ^bytes [^doubles samples sample-rate]
  (let [data (pcm16-bytes samples)
        buffer (doto (java.nio.ByteBuffer/allocate (+ 44 (alength data)))
                 (.order java.nio.ByteOrder/LITTLE_ENDIAN))
        rate (long sample-rate)]
    (.put buffer (.getBytes "RIFF" "US-ASCII")) (.putInt buffer (+ 36 (alength data)))
    (.put buffer (.getBytes "WAVEfmt " "US-ASCII")) (.putInt buffer 16) (.putShort buffer (short 1))
    (.putShort buffer (short 1)) (.putInt buffer (int rate)) (.putInt buffer (int (* 2 rate)))
    (.putShort buffer (short 2)) (.putShort buffer (short 16))
    (.put buffer (.getBytes "data" "US-ASCII")) (.putInt buffer (alength data)) (.put buffer data)
    (.array buffer)))
