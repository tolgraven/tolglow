(ns tolglow.integrations.section-tempo-test
  "Causal section boundaries across tempo changes and transport stops (#805).
  The real analyzer hears generated music that follows a transport whose tempo
  steps inside the 70–180 BPM design range. Confirmed boundaries must stay on
  the phrase grid the music changes on."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.integrations.analysis :as analysis])
  (:import [java.util Random]))

(def ^:private rate 8000.0)

(def ^:private block-frames 80)

(def ^:private frame-ms
  "The owner commits the transport once a frame, and the capture worker reads
  the latest committed transport as each block arrives."
  (/ 1000.0 60.0))

(def ^:private phrase-beats 16)

(defn- voice
  "The timeline QA's synthetic music (dev/tolglow/qa/timeline.clj): two bass
  notes alternate every beat and swap order every phrase, so the bass pattern
  changes on each phrase line, with a mid stab and noise hats on top."
  [beat seconds ^Random noise]
  (let [index (long (Math/floor beat))
        fraction (- beat index)
        notes (if (even? (quot index phrase-beats)) [48.0 84.0] [84.0 48.0])
        hz (nth notes (mod index 2))
        bass (* 0.34 (+ 0.6 (* 0.4 (Math/exp (* -4.0 fraction)))) (Math/sin (* 2.0 Math/PI hz seconds)))
        stab (if (odd? index)
               (* 0.3 (Math/exp (* -6.0 fraction)) (Math/sin (* 2.0 Math/PI 660.0 seconds)))
               0.0)
        half (- (* 2.0 beat) (Math/floor (* 2.0 beat)))
        hat (* 0.22 (Math/exp (* -18.0 half)) (- (* 2.0 (.nextDouble noise)) 1.0))]
    (+ bass stab hat)))

(defn- segment-beat [{:keys [time-ms beat bpm playing?]} at-ms]
  (if playing? (+ beat (* (- at-ms time-ms) (/ bpm 60000.0))) beat))

(defn- transport
  "A tempo map from [time-ms changes] steps. Each step changes :bpm or
  :playing? and starts a segment at the beat the previous segment reached,
  moved by :jump beats for a clock correction, seek or loop."
  [steps]
  (reduce (fn [segments [at-ms {:keys [jump] :or {jump 0.0} :as changes}]]
            (let [previous (peek segments)]
              (conj segments (merge previous (dissoc changes :jump)
                                    {:time-ms at-ms :beat (+ jump (segment-beat previous at-ms))}))))
          [{:time-ms 0.0 :beat 0.0 :bpm 120.0 :playing? true}]
          steps))

(defn- segment-at [segments at-ms]
  (reduce (fn [found segment] (if (<= (:time-ms segment) at-ms) segment (reduced found))) segments))

(defn- beat-at [segments at-ms]
  (segment-beat (segment-at segments at-ms) at-ms))

(defn- analyze-transport
  "Run the real analyzer for duration-ms of music that follows the tempo map.
  The capture worker reads the transport the owner committed last as each
  block arrives; :clock-error-ms, if given, returns a per-block error in the
  beat of that snapshot, as an external clock's estimate has. Returns the
  confirmed boundary beats in order and the final analyzer state."
  [steps duration-ms & {:keys [block-frames clock-error-ms source]
                        :or {block-frames 80 clock-error-ms (constantly 0.0) source :manual}}]
  (let [segments (transport steps)
        noise (Random. 805)
        frames (long (* rate (/ duration-ms 1000.0)))
        frame-time #(* 1000.0 (/ % rate))]
    (loop [cursor 0 state (analysis/new-analyzer {:sections {:beats-per-window 2}}) boundaries []]
      (if (>= cursor frames)
        {:boundaries boundaries :state state}
        (let [arrived (+ (frame-time (+ cursor block-frames)) 4.0)
              committed (* frame-ms (Math/floor (/ arrived frame-ms)))
              segment (segment-at segments committed)
              error-beats (* (clock-error-ms) (/ (:bpm segment) 60000.0))
              clock {:time-ms committed :beat (+ error-beats (segment-beat segment committed)) :bpm (:bpm segment)
                     :playing? (:playing? segment) :source source :source-epoch 0 :confidence 1.0}
              samples (double-array (for [i (range block-frames)
                                          :let [frame (+ cursor i)]]
                                      (voice (beat-at segments (frame-time frame)) (/ frame rate) noise)))
              result (analysis/analyze state {:samples samples :sample-rate rate
                                              :started-at-ms (frame-time cursor) :received-at-ms arrived
                                              :clock clock})]
          (recur (+ cursor block-frames) (:state result)
                 (into boundaries (comp (filter #(= :audio/section-change (:type %))) (map :boundary-beat))
                       (:events result))))))))

(defn- confirmed-boundaries [steps duration-ms & options]
  (:boundaries (apply analyze-transport steps duration-ms options)))

(defn- ms-at-beat [bpm beat] (* beat (/ 60000.0 bpm)))

(defn- tempo-step
  "Boundaries when the tempo steps from `from` to `to` at step-beat, heard
  until beat 52: phrase lines 16 and 32 before or around the step and 48 after."
  [from to step-beat & options]
  (let [step-ms (ms-at-beat from step-beat)]
    (apply confirmed-boundaries [[0.0 {:bpm from}] [step-ms {:bpm to}]]
           (+ step-ms (ms-at-beat to (- 52 step-beat))) options)))

(deftest a-tempo-step-keeps-confirmed-boundaries-on-the-phrase-grid
  (testing "120 to 96 BPM, stepping anywhere inside the windows that confirm phrase line 32"
    ;; Before #805 the boundary at 32 was confirmed at 34 when the step fell at
    ;; beat 34.5 or 34.875: the bass pitch is measured over 160 ms that ended
    ;; before each hop, so its smear across a note change filled a different
    ;; share of a half-beat slot after the step and the repeat test failed.
    (doseq [step-beat (range 32.0 36.01 0.125)]
      (is (= [16.0 32.0 48.0] (tempo-step 120.0 96.0 step-beat)) (str "step at beat " step-beat))))
  (testing "Steps across the 70–180 BPM range"
    (doseq [[from to] [[96.0 120.0] [120.0 70.0] [70.0 180.0] [180.0 70.0] [180.0 128.0] [128.0 140.0]]
            step-beat [33.0 34.25 34.5 34.75 35.5]]
      (is (= [16.0 32.0 48.0] (tempo-step from to step-beat)) (str from " to " to " BPM at beat " step-beat)))))

(deftest boundaries-sit-on-phrase-lines-at-both-ends-of-the-tempo-range
  ;; At 180 BPM a half-beat slot is 167 ms. Before #805 the pitch smear filled
  ;; so much of each slot that no boundary was confirmed at all.
  (doseq [bpm [70.0 180.0]]
    (is (= [16.0 32.0 48.0] (confirmed-boundaries [[0.0 {:bpm bpm}]] (ms-at-beat bpm 52)))
        (str bpm " BPM"))))

(deftest a-transport-stop-around-a-tempo-step-resumes-on-the-phrase-grid
  (let [stopped-for-ms 1500.0
        run (fn [stop-beat to]
              (let [stop-ms (ms-at-beat 120.0 stop-beat)
                    resume-ms (+ stop-ms stopped-for-ms)]
                (confirmed-boundaries [[stop-ms {:playing? false}] [(+ stop-ms 700.0) {:bpm to}]
                                       [resume-ms {:playing? true}]]
                                      (+ resume-ms (ms-at-beat to (- 68 stop-beat))))))]
    (testing "A stop clears a pending change; the next phrase line is still found"
      (is (= [16.0 48.0 64.0] (run 33.3 96.0))))
    (testing "A stop after a confirmation keeps the comparison on the grid"
      (is (= [16.0 32.0 48.0 64.0] (run 36.5 96.0)))
      (is (= [16.0 32.0 48.0 64.0] (run 40.7 150.0))))))

(deftest audio-heard-before-a-tempo-step-keeps-the-tempo-it-was-heard-at
  ;; A 256 ms capture block arrives after the step with the new tempo, while
  ;; most of its hops were heard before it. Each hop's beat comes from the
  ;; transport segment it fell in, not from extrapolating the newest snapshot.
  (doseq [[from to] [[120.0 96.0] [96.0 120.0] [180.0 70.0] [70.0 180.0]]
          step-beat [32.25 33.5 34.75 35.5]]
    (is (= [16.0 32.0 48.0] (tempo-step from to step-beat :block-frames 2048))
        (str from " to " to " BPM at beat " step-beat " in 2048-frame blocks"))))

(deftest a-jittered-external-clock-keeps-a-bounded-history
  ;; Each Link-like snapshot is up to 0.3 ms off the line, so each starts a
  ;; segment. The history must stay a plain vector of at most 64 segments: a
  ;; trimmed subvec would keep every segment ever added behind it.
  (let [jitter (Random. 7)
        {:keys [boundaries state]}
        (analyze-transport [] 45000.0 :source :link
                           :clock-error-ms #(* 0.3 (- (* 2.0 (.nextDouble jitter)) 1.0)))
        tempo (get-in state [:section-state :tempo])]
    (is (= [16.0 32.0 48.0 64.0 80.0] boundaries))
    (is (instance? clojure.lang.PersistentVector tempo))
    (is (<= 2 (count tempo) 64))
    (is (= 1 (count (get-in (analyze-transport [] 9000.0) [:state :section-state :tempo])))
        "a clock exactly on its line keeps one segment")))

(deftest clock-corrections-seeks-and-loops-stay-on-the-phrase-grid
  (let [at #(ms-at-beat 120.0 %)]
    (testing "A small forward phase correction at constant tempo keeps the comparison"
      (is (= [16.0 32.0 48.0] (confirmed-boundaries [[(at 34.5) {:jump 0.03}]] (at 52.5)))))
    (testing "A correction backwards or past 1/8 beat restarts the comparison, then finds the next line"
      ;; Phrase line 32 was waiting for its confirming window [34, 36).
      (doseq [jump [-0.03 0.25]]
        (is (= [16.0 48.0] (confirmed-boundaries [[(at 34.5) {:jump jump}]] (at 52.5)))
            (str "corrected by " jump " beat"))))
    (testing "A loop back over a phrase line confirms that line again"
      (is (= [16.0 32.0 32.0 48.0] (confirmed-boundaries [[(at 40.0) {:jump -16.0}]] (at 68.0)))))
    (testing "A seek forward finds the next phrase line after it"
      (is (= [16.0 48.0] (confirmed-boundaries [[(at 21.0) {:jump 19.0}]] (at 36.0)))))))
