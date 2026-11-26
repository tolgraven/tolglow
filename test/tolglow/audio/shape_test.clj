(ns tolglow.audio.shape-test
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.audio.fixtures :as fixtures]
            [tolglow.audio.shape :as shape]))

(def sample-rate 48000.0)

(defn- tone [frequency seconds & [phase amplitude]]
  (let [n (long (* seconds sample-rate)) out (double-array n)]
    (dotimes [i n]
      (aset out i (* (or amplitude 0.5) (Math/sin (+ (or phase 0.0) (/ (* 2.0 Math/PI frequency i) sample-rate))))))
    out))

(defn- rms [^doubles values from]
  (Math/sqrt (/ (reduce + (map #(* % %) (drop from values))) (- (alength values) from))))

(defn- clock [bpm & [overrides]] (merge {:time-ms 0.0 :beat 0.0 :bpm bpm :playing? true} overrides))

(defn- feed
  "Feed samples in blocks with a clock per block start (ms). Returns the
  extractions and the final state."
  [state ^doubles samples {:keys [block clock-at] :or {block 480}}]
  (loop [start 0 state state out []]
    (if (>= start (alength samples))
      {:state state :extractions out}
      (let [end (min (alength samples) (+ start block))
            started (* 1000.0 (/ start sample-rate))
            {:keys [state extractions]}
            (shape/advance state {:samples (java.util.Arrays/copyOfRange samples (int start) (int end))
                                  :sample-rate sample-rate :started-at-ms started
                                  :clock (clock-at started)})]
        (recur end state (into out extractions))))))

(defn- close? [tolerance a b] (every? true? (map #(<= (Math/abs (- (double %1) (double %2))) tolerance) a b)))

(deftest the-band-filter-passes-its-band-and-rejects-neighbours
  (let [design (shape/filter-design :bass sample-rate)
        through (fn [frequency] (rms (first (shape/filter-samples design [] (tone frequency 0.5))) 4800))]
    (is (= 4 (count design)) "two high-pass and two low-pass Butterworth sections")
    (is (< (Math/abs (* 20 (Math/log10 (/ (through 120.0) (rms (tone 120.0 0.5) 4800))))) 1.0)
        "a 120 Hz tone passes the bass band within 1 dB")
    (is (< (* 20 (Math/log10 (/ (through 2000.0) (rms (tone 2000.0 0.5) 4800)))) -40.0)
        "2 kHz is more than 40 dB down")
    (is (< (* 20 (Math/log10 (/ (through 15.0) (rms (tone 15.0 2.0) 24000)))) -30.0)
        "15 Hz rumble is rejected")
    (is (= [] (shape/filter-design :full sample-rate)))
    (testing "state carries across blocks exactly"
      (let [whole (first (shape/filter-samples design [] (tone 90.0 0.1)))
            [a state] (shape/filter-samples design [] (java.util.Arrays/copyOfRange (tone 90.0 0.1) 0 2000))
            [b _] (shape/filter-samples design state (java.util.Arrays/copyOfRange (tone 90.0 0.1) 2000 4800))]
        (is (= (vec whole) (into (vec a) b)))))))

(deftest a-cycle-is-one-aligned-period-of-the-band
  (let [points 32
        sine (mapv #(+ 0.5 (* 0.5 (Math/sin (/ (* 2.0 Math/PI %) points)))) (range points))
        analyze #(shape/analyze-cycle % sample-rate [60.0 250.0] points 0.6)]
    (doseq [phase [0.0 1.0 2.5 4.0]]
      (let [{:keys [status shape frequency-hz confidence periods]} (analyze (tone 110.0 0.1 phase))]
        (is (= :ok status))
        (is (< (Math/abs (- 110.0 frequency-hz)) 0.2))
        (is (> confidence 0.99))
        (is (= 8 periods))
        (is (close? 0.02 sine shape) (str "the fundamental starts at zero phase whatever the input phase " phase))))
    (testing "a saw keeps its edge after alignment"
      (let [n 4800 saw (double-array n)]
        (dotimes [i n] (aset saw i (- (* 2.0 (mod (/ (* 110.0 i) sample-rate) 1.0)) 1.0)))
        (let [{:keys [shape]} (analyze saw)
              steps (map - (rest shape) shape)]
          (is (< (apply min steps) -0.9) "one falling edge per cycle")
          (is (= 1 (count (filter #(< % -0.5) steps)))))))
    (testing "noise has no period"
      (let [random (java.util.Random. 1) noise (double-array 4800)]
        (dotimes [i 4800] (aset noise i (- (.nextDouble random) 0.5)))
        (is (= :low-confidence (:status (analyze noise))))))))

(deftest an-envelope-follows-the-beats-of-its-window
  (let [bpm 120.0
        n (long (* 4.1 sample-rate)) ; two bars and the edge after them
        bursts (double-array n)]
    (dotimes [i n]
      (let [beat (* (/ i sample-rate) (/ bpm 60.0))
            into-beat (- beat (Math/floor beat))]
        (aset bursts i (* 0.5 (Math/exp (* -6.0 into-beat)) (Math/sin (/ (* 2.0 Math/PI 110.0 i) sample-rate))))))
    (let [{:keys [extractions]} (feed (shape/new-extractor {:band :bass :extract :envelope :points 32} sample-rate)
                                      bursts {:clock-at (fn [_] (clock bpm))})
          [first-bar] extractions]
      (is (= [0 1] (mapv #(get-in % [:window :index]) extractions)) "a window opened on its first sample is whole")
      (is (= {:index 0 :start-beat 0.0 :end-beat 4.0 :samples 96000} (:window first-bar)))
      (is (= :ok (:status first-bar)))
      (is (= [0 8 16 24] (keep-indexed (fn [j v] (when (> v 0.9) j)) (:shape first-bar)))
          "each beat's attack lands on the same shape point")
      (is (< (nth (:shape first-bar) 7) 0.1) "and decays before the next"))))

(deftest windows-follow-the-tempo-across-the-operating-range
  (doseq [bpm [70.0 120.0 180.0]]
    (let [bar-ms (* 4 (/ 60000.0 bpm))
          samples (fixtures/passage :dry-sine {:bpm bpm :bars 3})
          {:keys [extractions]} (feed (shape/new-extractor {:band :bass :extract :envelope} sample-rate)
                                      samples {:clock-at (fn [_] (clock bpm))})]
      (is (= [0 1] (mapv #(get-in % [:window :index]) extractions)) (str bpm " BPM"))
      (is (every? #(<= (Math/abs (- (get-in % [:window :samples]) (* sample-rate (/ bar-ms 1000.0)))) 1)
                  extractions)
          "each window holds one bar of samples"))))

(deftest silence-and-quiet-bands-are-reported-not-shaped
  (let [quiet (double-array (long (* 2.2 sample-rate)))
        loud-elsewhere (tone 3000.0 2.2)
        run #(:extractions (feed (shape/new-extractor {:band :bass :extract %2} sample-rate)
                                 %1 {:clock-at (fn [_] (clock 120.0))}))]
    (is (= [:silent] (mapv :status (run quiet :envelope))))
    (is (= [:silent] (mapv :status (run quiet :cycle))))
    (is (= [:silent] (mapv :status (run loud-elsewhere :envelope))) "a loud neighbour outside the band is silence here")))

(deftest a-stopped-transport-drops-the-window-and-resumes-at-the-next-edge
  (let [bpm 120.0
        samples (fixtures/passage :delay-synth {:bpm bpm :bars 7})
        ;; Play 0..5 s (beat 10), stop for 2 s, then play on from beat 10.
        clock-at (fn [ms]
                   (cond (< ms 5000.0) (clock bpm)
                         (< ms 7000.0) (clock bpm {:time-ms ms :beat 10.0 :playing? false})
                         :else (clock bpm {:time-ms 7000.0 :beat 10.0})))
        {:keys [extractions state]} (feed (shape/new-extractor {:band :bass :extract :envelope} sample-rate)
                                          samples {:clock-at clock-at})]
    (is (= [0 1 3 4] (mapv #(get-in % [:window :index]) extractions))
        "bar 2 straddled the stop: dropped, with no catch-up extraction for the stopped audio")
    (is (= [[0.0 4.0] [4.0 8.0] [12.0 16.0] [16.0 20.0]]
           (mapv (comp (juxt :start-beat :end-beat) :window) extractions)))
    (is (= [1 2 3 4] (mapv :seq extractions)))
    (is (= :listening (:status state)))
    (testing "the stopped state is reported while it lasts"
      (is (= :paused (:status (:state (shape/advance (shape/new-extractor {} sample-rate)
                                                     {:samples (double-array 480) :sample-rate sample-rate
                                                      :started-at-ms 0.0 :clock (clock bpm {:playing? false})}))))))))

(deftest extraction-does-not-depend-on-block-size-or-mutate-earlier-states
  (let [samples (fixtures/passage :delay-synth {:bpm 132.0 :bars 3})
        run (fn [block mode]
              (:extractions (feed (shape/new-extractor {:band :bass :extract mode} sample-rate)
                                  samples {:block block :clock-at (fn [_] (clock 132.0))})))]
    (doseq [mode [:envelope :cycle]]
      (let [a (run 480 mode) b (run 1024 mode)]
        (is (= (mapv :window a) (mapv :window b)))
        (is (every? true? (map #(close? 1.0e-6 (:shape %1) (:shape %2)) a b)) (str mode))))
    (testing "advancing an old state again gives the same result"
      (let [state (:state (feed (shape/new-extractor {:extract :cycle} sample-rate)
                                (java.util.Arrays/copyOfRange samples 0 100000)
                                {:clock-at (fn [_] (clock 132.0))}))
            block {:samples (java.util.Arrays/copyOfRange samples 100000 100480) :sample-rate sample-rate
                   :started-at-ms (* 1000.0 (/ 100000 sample-rate)) :clock (clock 132.0)}
            once (shape/advance state block)
            twice (shape/advance state block)]
        (is (= (vec (:ring (:state once))) (vec (:ring (:state twice)))))
        (is (= (dissoc (:state once) :ring) (dissoc (:state twice) :ring)))))))

(deftest specs-are-validated
  (is (= [] (shape/validate-spec {})))
  (is (= [[:band] [:extract] [:window] [:points] [:silence-db] [:min-confidence]]
         (mapv :path (shape/validate-spec {:band :treble :extract :fft :window {:ms 500} :points 64
                                           :silence-db 3 :min-confidence 2}))))
  (is (thrown? clojure.lang.ExceptionInfo (shape/new-extractor {:points 4} sample-rate)))
  (is (= "bass 60 Hz–250 Hz" (shape/band-label :bass)))
  (is (= [100.0 900.0] (shape/band-range [100 900]))))

(deftest table-value-interpolates-one-periodic-cycle
  (let [table [0.0 1.0 0.5 0.5]]
    (is (= 0.0 (shape/table-value table 0.0)))
    (is (= 0.5 (shape/table-value table 0.125)))
    (is (= 0.125 (shape/table-value table 0.9375)) "the last point wraps to the first")
    (is (close? 1.0e-12 (repeat 2 (shape/table-value table 0.3))
                [(shape/table-value table 2.3) (shape/table-value table -0.7)]))))
