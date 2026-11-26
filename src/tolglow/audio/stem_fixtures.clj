(ns tolglow.audio.stem-fixtures
  "Deterministic synthesized stems with known onset times (#675): a kick,
  snare, bass and the #676 Karplus–Strong guitar phrase on four channels, each with bleed from its
  loud neighbours, an optional dropout on one stem and trailing silence. Tests,
  harnesses and the REPL use the same render, as arrays, interleaved signed-16
  PCM, an injectable capture source or a WAV file. Nothing here opens a device."
  (:require [tolglow.audio.fixtures :as fixtures]
            [tolglow.audio.stems :as stems])
  (:import [java.io ByteArrayInputStream File]
           [java.util Random]
           [javax.sound.sampled AudioFileFormat$Type AudioFormat AudioInputStream AudioSystem]))

(def stem-layout
  [{:id :kick :channel 1 :preset :kick}
   {:id :snare :channel 2 :preset :snare}
   {:id :bass :channel 3 :preset :bass}
   {:id :guitar :channel 4 :preset :guitar}])

(def bass-notes [55.0 55.0 65.41 49.0])

(def guitar-plucks
  "Beats of the plucks in each two-bar phrase of tolglow.audio.fixtures'
  :guitar-phrase passage: one bar of notes, then a silent bar."
  [0 3/2 2 3])

(defn pattern
  "Beat offsets of each drum and bass note within one 4/4 bar."
  []
  {:kick [0 1 2 3] :snare [1 3] :bass [1/2 3/2 5/2 7/2]})

(defn- ms-per-beat [bpm] (/ 60000.0 bpm))

(defn- add-kick! [^doubles out start sample-rate amplitude]
  (let [n (min (- (alength out) start) (long (* 0.35 sample-rate)))]
    (loop [i 0 phase 0.0]
      (when (< i n)
        (let [t (/ i (double sample-rate))
              hz (+ 45.0 (* 75.0 (Math/exp (/ (- t) 0.03))))
              v (* amplitude (Math/exp (/ (- t) 0.09)) (Math/sin phase))]
          (aset out (+ start i) (+ (aget out (+ start i)) v))
          (recur (inc i) (+ phase (/ (* 2.0 Math/PI hz) sample-rate))))))))

(defn- add-snare! [^doubles out start sample-rate amplitude ^Random random]
  (let [n (min (- (alength out) start) (long (* 0.25 sample-rate)))]
    (dotimes [i n]
      (let [t (/ i (double sample-rate))
            noise (* 0.65 (- (* 2.0 (.nextDouble random)) 1.0) (Math/exp (/ (- t) 0.07)))
            tone (* 0.35 (Math/sin (* 2.0 Math/PI 190.0 t)) (Math/exp (/ (- t) 0.04)))]
        (aset out (+ start i) (+ (aget out (+ start i)) (* amplitude (+ noise tone))))))))

(defn- add-tone! [^doubles out start sample-rate hz length-s amplitude]
  (let [n (min (- (alength out) start) (long (* length-s sample-rate)))
        attack (* 0.004 sample-rate) release (* 0.02 sample-rate)]
    (dotimes [i n]
      (let [t (/ i (double sample-rate))
            gain (min 1.0 (/ i attack) (/ (- n i) release))
            v (/ (+ (Math/sin (* 2.0 Math/PI hz t)) (* 0.5 (Math/sin (* 4.0 Math/PI hz t)))
                    (* 0.25 (Math/sin (* 6.0 Math/PI hz t)))) 1.75)]
        (aset out (+ start i) (+ (aget out (+ start i)) (* amplitude gain v)))))))

(defn render
  "Render the four stems. Options: :bpm (120), :bars (4), :sample-rate
  (48000), :lead-in-beats (1, silence before the first downbeat),
  :tail-beats (4, silence for every stem at the end), :bleed-db (-24, each
  neighbour mixed into every channel), :seed (675, snare noise) and
  :dropout {:stem :bass :from-beat 8 :to-beat 12} which silences that stem's
  own signal (its bleed into others remains) over transport beats.
  :hats {:peak-db -20} adds a fifth stem on channel 5 with no bleed in or
  out: a noise hit on every eighth note at that peak level, half of them
  landing on kicks, to check a quiet instrument on its own channel.

  Returns {:sample-rate :bpm :frames :stems :samples {id doubles}
  :onsets {id [{:beat transport-beat :ms time}]}}. A transport playing from
  beat 0 at 0 ms at :bpm puts every onset exactly on its listed beat."
  ([] (render {}))
  ([{:keys [bpm bars sample-rate lead-in-beats tail-beats bleed-db seed dropout hats]
     :or {bpm 120.0 bars 4 sample-rate 48000 lead-in-beats 1 tail-beats 4 bleed-db -24.0 seed 675}}]
   (let [beat-ms (ms-per-beat bpm)
         total-beats (+ lead-in-beats (* 4 bars) tail-beats)
         frames (long (Math/ceil (* sample-rate (/ (* total-beats beat-ms) 1000.0))))
         random (Random. (long seed))
         dry (into {} (map (fn [{:keys [id]}] [id (double-array frames)])) stem-layout)
         at (fn [beat] (long (Math/round (* sample-rate (/ (* beat beat-ms) 1000.0)))))
         onsets (volatile! (zipmap (map :id stem-layout) (repeat [])))
         dropped? (fn [id beat] (and dropout (= id (:stem dropout)) (<= (:from-beat dropout) beat) (< beat (:to-beat dropout))))]
     (doseq [bar (range bars) [id offsets] (pattern) [index offset] (map-indexed vector offsets)
             :let [beat (+ lead-in-beats (* 4 bar) offset)
                   start (at beat)]
             :when (and (< start frames) (not (dropped? id beat)))]
       (case id
         :kick (add-kick! (dry :kick) start sample-rate 0.8)
         :snare (add-snare! (dry :snare) start sample-rate 0.6 random)
         :bass (add-tone! (dry :bass) start sample-rate (nth bass-notes index) (* 0.35 (/ beat-ms 1000.0)) 0.5))
       (vswap! onsets update id conj {:beat (double beat) :ms (* beat beat-ms)}))
     ;; The guitar is the shared #676 phrase, placed after the lead-in: plucks
     ;; for a bar, then a silent bar, so its level is a continuous control
     ;; with clean releases.
     (let [phrase ^doubles (fixtures/passage :guitar-phrase {:sample-rate (double sample-rate) :bpm (double bpm)
                                                               :bars bars :seed seed})
           offset (at lead-in-beats) guitar ^doubles (dry :guitar)]
       (dotimes [i (min (alength phrase) (- frames offset))]
         (let [beat (+ lead-in-beats (/ (* 1000.0 i) (* sample-rate beat-ms)))]
           (when-not (dropped? :guitar beat)
             (aset guitar (+ offset i) (aget phrase i)))))
       (doseq [phrase-start (range 0 (* 4 bars) 8) pluck guitar-plucks
               :let [beat (+ lead-in-beats phrase-start pluck)]
               :when (and (< beat (+ lead-in-beats (* 4 bars))) (not (dropped? :guitar beat)))]
         (vswap! onsets update :guitar conj {:beat (double beat) :ms (* beat beat-ms)})))
     (let [bleed (Math/pow 10.0 (/ bleed-db 20.0))
           mixed (into {} (map (fn [{:keys [id]}]
                                 (let [own ^doubles (dry id) out (double-array frames)
                                       others (mapv dry (remove #{id} (map :id stem-layout)))]
                                   (dotimes [i frames]
                                     (aset out i (+ (aget own i)
                                                    (* bleed (reduce + (map (fn [^doubles o] (aget o i)) others))))))
                                   [id out])))
                       stem-layout)]
       (cond-> {:sample-rate sample-rate :bpm (double bpm) :frames frames :stems stem-layout
                :samples mixed :onsets @onsets}
         hats (as-> rendered
                (let [out (double-array frames) noise (Random. (long (inc seed)))
                      peak (Math/pow 10.0 (/ (:peak-db hats) 20.0))
                      beats (map #(+ lead-in-beats (/ % 2)) (range (* 8 bars)))]
                  (doseq [beat beats :let [start (at beat)]]
                    (dotimes [i (min (- frames start) (long (* 0.06 sample-rate)))]
                      (let [t (/ i (double sample-rate))]
                        (aset out (+ start i) (+ (aget out (+ start i))
                                                 (* peak (- (* 2.0 (.nextDouble noise)) 1.0) (Math/exp (/ (- t) 0.015))))))))
                  (-> rendered
                      (update :stems conj {:id :hats :channel 5 :preset :hats})
                      (assoc-in [:samples :hats] out)
                      (assoc-in [:onsets :hats] (mapv (fn [beat] {:beat (double beat) :ms (* beat beat-ms)}) beats))))))))))

(defn pcm16
  "Interleave rendered stems into little-endian signed-16 PCM, channel order
  following each stem's :channel, through the shared #676 encoder."
  [{:keys [frames samples stems]}]
  (let [channels (reduce max (map :channel stems))
        by-channel (into {} (map (fn [{:keys [id channel]}] [channel (samples id)])) stems)
        interleaved (double-array (* channels frames))]
    (doseq [channel (range 1 (inc channels))
            :let [^doubles source (by-channel channel)]
            :when source]
      (dotimes [frame frames]
        (aset interleaved (+ (dec channel) (* channels frame)) (aget source frame))))
    (fixtures/pcm16-bytes interleaved)))

(defn memory-source
  "Injectable capture source over interleaved PCM bytes, in the capture
  manager's {:read! :close!} contract. before-read! (optional) receives the
  frame offset of each read, so a test can tick the show in step with audio."
  ([bytes channels chunk-frames] (memory-source bytes channels chunk-frames nil))
  ([^bytes bytes channels chunk-frames before-read!]
   (let [position (atom 0) frame-size (* 2 channels)]
     {:started-at-ms 0.0 :description {:name "Synthesized stems"}
      :read! (fn [out offset length]
               (when before-read! (before-read! (quot @position frame-size)))
               (let [n (min length (* frame-size chunk-frames) (- (alength bytes) @position))]
                 (if (zero? n) -1
                     (do (System/arraycopy bytes @position out offset n) (swap! position + n) n))))
      :close! (fn [])})))

(defn write-wav!
  "Write a render as a multichannel 16-bit WAV so the same stems can be
  auditioned or replayed through capture's :file input."
  [rendered path]
  (let [bytes (pcm16 rendered) channels (reduce max (map :channel (:stems rendered)))
        format (AudioFormat. (float (:sample-rate rendered)) 16 (int channels) true false)]
    (with-open [stream (AudioInputStream. (ByteArrayInputStream. bytes) format (long (:frames rendered)))]
      (AudioSystem/write stream AudioFileFormat$Type/WAVE (File. (str path))))
    {:path (str path) :channels channels :frames (:frames rendered) :sample-rate (:sample-rate rendered)}))

(defn capture-config
  "Capture configuration that reads these stems: four channels, the ordinary
  analyzer on the kick channel and each stem on its own channel."
  ([rendered] (capture-config rendered {}))
  ([rendered overrides]
   (merge {:default? true :sample-rate (double (:sample-rate rendered)) :channels (reduce max (map :channel (:stems rendered)))
           :input-channel 1
           :block-frames 480 :stems (:stems rendered)}
          overrides)))

(defn replay
  "Run a render through the pure stem detectors with a transport playing
  from beat 0 at its tempo. Returns tolglow.audio.stems/replay's result."
  ([rendered] (replay rendered {}))
  ([{:keys [sample-rate bpm samples stems]} {:keys [block-frames stem-overrides] :or {block-frames 480}}]
   (stems/replay (stems/validate-config (mapv #(merge % (get stem-overrides (:id %))) stems) (reduce max (map :channel stems)))
                 {:samples samples :sample-rate sample-rate :block-frames block-frames
                  :clock-at (fn [ms] {:time-ms ms :beat (/ ms (ms-per-beat bpm)) :bpm bpm :playing? true})})))
