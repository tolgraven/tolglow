(ns tolglow.integrations.sections-test
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.integrations.analysis :as analysis]
            [tolglow.integrations.sections :as sections]))

(def ^:private rate 8000)

(def ^:private samples-per-beat 4000)

(def ^:private beat-ms 500.0)

(def ^:private clock {:time-ms 0.0 :beat 0.0 :bpm 120.0 :source :midi-clock :epoch 1})

(def ^:private phrase-a [48.0 64.0 84.0 104.0])

(def ^:private phrase-b [84.0 104.0 48.0 64.0])

(defn- sine [hz phase seconds]
  (Math/sin (+ phase (* 2.0 Math/PI hz seconds))))

(defn- fixture [{:keys [notes amplitudes gain dropout transient]
                 :or {notes (constantly phrase-a) amplitudes (constantly [0.30 0.30 0.30 0.30])
                      gain (constantly 1.0)}}]
  (let [rng (java.util.Random. 514)
        phases (into {} (map (fn [hz] [hz (* 2.0 Math/PI (.nextDouble rng))])
                             (conj phrase-a 700.0)))]
    (fn [index]
      (let [beat (quot index samples-per-beat)
            beat-position (/ index (double samples-per-beat))
            seconds (/ (rem index samples-per-beat) (double rate))
            slot (mod beat 4)
            note (nth (notes beat) slot)
            bass (* (nth (amplitudes beat) slot) (sine note (get phases note) seconds))
            kick (* (Math/exp (/ (- seconds) 0.018))
                    (+ (* 0.08 (sine 56.0 0.0 seconds)) (* 0.14 (sine 1690.0 0.0 seconds))))
            texture (* 0.025 (sine 700.0 (get phases 700.0) seconds))]
        (if (and dropout (<= (first dropout) beat-position) (< beat-position (second dropout)))
          0.0
          (+ (* (gain beat) (+ bass kick texture))
             (if (and transient (<= transient beat-position (+ transient 0.02)))
               (* 0.7 (sine 53.0 0.0 seconds)) 0.0)))))))

(defn- run-pcm [signal beats block-sizes]
  ;; The analyzer returns its latest hop. Blocks up to one hop (80 samples)
  ;; preserve each feature, including blocks that cross a hop boundary.
  (let [length (long (* beats samples-per-beat))]
    (loop [index 0 block-sizes (cycle block-sizes) analyzer (analysis/new-analyzer)
           detector (sections/new-detector) last-feature-ms nil windows [] events []]
      (if (= index length)
        {:windows windows :events events}
        (let [end (min length (+ index (first block-sizes)))
              started-at-ms (* 1000.0 (/ index rate))
              received-at-ms (+ 2.0 (* 1000.0 (/ end rate)))
              result (analysis/analyze analyzer {:samples (double-array (map signal (range index end)))
                                                 :sample-rate rate :clock clock
                                                 :started-at-ms started-at-ms :received-at-ms received-at-ms})
              {:keys [time-ms window-latency-ms] :as features} (:features result)
              observation (when (and time-ms (not= time-ms last-feature-ms))
                            (sections/observe-hop detector
                                                  {:beat (/ (+ time-ms (/ window-latency-ms 2.0)) beat-ms)
                                                   :available-at-beat (/ received-at-ms beat-ms)
                                                   :features features}))]
          (recur end (rest block-sizes) (:state result) (or (:state observation) detector)
                 time-ms (cond-> windows (:window observation) (conj (:window observation)))
                 (cond-> events (:event observation) (conj (:event observation)))))))))

(defn- window-at [result beat]
  (some #(when (== beat (:start-beat %)) %) (:windows result)))

(deftest equal-level-bass-note-order-confirms-on-the-existing-beat-grid
  (let [signal (fixture {:notes #(if (< % 8) phrase-a phrase-b)})
        result (run-pcm signal 24 [80])
        irregular (run-pcm signal 24 [17 29 73 20 67])
        [event] (:events result)
        comparable #(dissoc % :available-at-beat)]
    (is (= 1 (count (:events result))))
    (is (= :audio/section-change (:type event)))
    (is (= [:bass-pattern] (get-in event [:evidence :drivers])))
    (is (> (get-in event [:evidence :distance :bass]) 0.7))
    (is (< (get-in event [:evidence :distance :accents]) 0.03))
    (is (< (Math/abs (- (get-in event [:level :current-rms])
                        (get-in event [:level :previous-rms]))) 0.002))
    (is (<= 0.45 (:confidence event) 1.0))
    (is (= 8.0 (:boundary-beat event) (:confirmation-delay-beats event)))
    (is (= 16.0 (:confirmed-at-beat event)))
    (is (= 4000.0 (* beat-ms (:confirmation-delay-beats event))))
    (is (<= 1.99 (* beat-ms (- (:available-at-beat event) (:confirmed-at-beat event))) 2.01))
    (is (empty? (:events (run-pcm signal 15.98 [80]))))
    (is (= (:windows result) (:windows irregular)))
    (is (= (mapv comparable (:events result)) (mapv comparable (:events irregular))))))

(deftest equal-level-accent-order-can-change-with-stable-bass-pitch
  (let [result (run-pcm (fixture {:notes (constantly [64.0 64.0 64.0 64.0])
                                :amplitudes #(if (< % 8) [0.34 0.13 0.34 0.13]
                                                [0.13 0.34 0.13 0.34])}) 20 [80])
        [event] (:events result)]
    (is (= 1 (count (:events result))))
    (is (= [:groove-accents] (get-in event [:evidence :drivers])))
    (is (< (get-in event [:evidence :distance :bass]) 0.05))
    (is (> (get-in event [:evidence :distance :accents]) 0.4))
    (is (< (Math/abs (- (get-in event [:level :current-rms])
                        (get-in event [:level :previous-rms]))) 0.002))
    (is (= 8.0 (:boundary-beat event) (:confirmation-delay-beats event)))))

(deftest gain-missing-pitch-and-isolated-novelty-do-not-confirm-a-new-section
  (testing "A gain step changes RMS without changing the normalized pattern"
    (let [result (run-pcm (fixture {:gain #(if (< % 8) 1.0 0.5)}) 20 [80])
          before (window-at result 4) after (window-at result 12)]
      (is (empty? (:events result)))
      (is (<= 0.49 (/ (:rms after) (:rms before)) 0.51))))
  (testing "A short dropout lowers confidence, and an isolated transient does not confirm"
    (let [result (run-pcm (fixture {:dropout [8.5 9.0] :transient 12.1}) 20 [80])
          before (window-at result 4) dropout (window-at result 8)]
      (is (empty? (:events result)))
      (is (false? (:eligible? dropout)))
      (is (< (:pitch-coverage dropout) (:pitch-coverage before)))
      (is (< (:confidence dropout) (:confidence before)))))
  (testing "One changed window followed by the reference never confirms"
    (let [result (run-pcm (fixture {:notes #(if (<= 8 % 11) phrase-b phrase-a)}) 20 [80])]
      (is (true? (:eligible? (window-at result 8))))
      (is (empty? (:events result)))))
  (testing "An unqualified window breaks confirmation even if a similar candidate follows"
    (let [result (run-pcm (fixture {:notes #(if (or (<= 8 % 11) (<= 16 % 19)) phrase-b phrase-a)
                                   :dropout [12.0 12.5]}) 24 [80])]
      (is (false? (:eligible? (window-at result 12))))
      (is (empty? (:events result))))))

(deftest discontinuity-clears-comparison-and-requires-a-fresh-grid-window
  (let [features {:rms 0.2 :bass-pitch-hz 64.0 :bass-pitch-confidence 0.99}
        observe (fn [state beat] (:state (sections/observe-hop state {:beat beat :features features})))
        state (reduce observe (sections/new-detector {:anchor-beat 1 :beats-per-window 2})
                      (map #(+ 1.0 (/ % 20.0)) (range 42)))
        gap (observe state 3.25)
        seek (:state (sections/observe-hop state {:beat 0.0 :features features :discontinuity? true}))]
    (is (:reference state))
    (is (nil? (:reference gap)))
    (is (false? (get-in gap [:active :complete-start?])))
    (is (nil? (:reference seek)))
    (is (thrown? clojure.lang.ExceptionInfo (observe state 0.0)))))
