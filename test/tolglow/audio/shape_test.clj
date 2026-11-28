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
        (is (= (vec whole) (into (vec a) b)))))
    (testing "a non-finite sample does not poison the filter state"
      (let [input (doto (tone 90.0 0.1) (aset 100 Double/NaN) (aset 200 Double/POSITIVE_INFINITY))
            [out state] (shape/filter-samples design [] input)]
        (is (every? #(Double/isFinite %) out))
        (is (every? #(Double/isFinite %) state))))))

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
        (is (every? true? (map #(close? 1.0e-6 (:shape %1) (:shape %2)) a b)) (str mode))))))

(defn- bank-run
  "Feed samples to a bank in 480-frame blocks; return the bank, every job and
  the per-block jobs."
  [specs ^doubles samples bpm]
  (loop [start 0 bank (shape/new-bank sample-rate) jobs []]
    (if (>= start (alength samples))
      {:bank bank :jobs jobs}
      (let [end (min (alength samples) (+ start 480))
            result (shape/advance-bank bank specs {:samples (java.util.Arrays/copyOfRange samples (int start) (int end))
                                                   :sample-rate sample-rate :started-at-ms (* 1000.0 (/ start sample-rate))
                                                   :clock (clock bpm)})]
        (recur end (:bank result) (into jobs (:jobs result)))))))

(deftest lfos-on-one-band-share-its-filter-and-ring
  (let [samples (fixtures/passage :delay-synth {:bpm 120.0 :bars 2})
        specs {:a {:band :bass :extract :cycle} :b {:band :bass :extract :envelope}
               :c {:band :sub :extract :cycle :window {:beats 2}}}
        {:keys [bank jobs]} (bank-run specs (java.util.Arrays/copyOfRange samples 0 (+ 96000 4800)) 120.0)]
    (is (= #{:bass :sub} (set (keys (:fronts bank)))) "one front per distinct band")
    (is (= [[:a 1] [:b 1] [:c 1] [:c 2]] (sort (map (juxt :id :seq) jobs))))
    (testing "an envelope job carries its bins; a cycle job a fresh copy of the band up to the edge"
      (let [job (some #(when (= :a (:id %)) %) jobs)
            ^doubles copy (:samples job)
            before (vec copy)]
        (is (nil? (:squares job)))
        (is (= (- 8192 750) (alength copy)) "the bass ring less the 15.6 ms tail guard at 120 BPM, no more")
        (shape/advance-bank bank specs {:samples (double-array 480 0.25) :sample-rate sample-rate
                                        :started-at-ms 2100.0 :clock (clock 120.0)})
        (is (= before (vec copy)) "later blocks never touch a handed-off copy")
        (is (= 32 (count (:squares (some #(when (= :b (:id %)) %) jobs)))))))
    (testing "an LFO removed from the specs is dropped with its band"
      (let [{:keys [bank]} (shape/advance-bank bank {:b (:b specs)} {:samples (double-array 480) :sample-rate sample-rate
                                                                      :started-at-ms 2100.0 :clock (clock 120.0)})]
        (is (= [:b] (keys (:lfos bank))))
        (is (= [:bass] (keys (:fronts bank))))))
    (testing "finishing a job is the same analysis as the single-LFO path"
      (is (= :ok (:status (shape/finish-job (some #(when (= :a (:id %)) %) jobs))))))))

(deftest the-cycle-tail-ends-a-guard-before-the-bar-line
  (testing "1/32 beat by default, never under 5 ms or over 60 ms (#912)"
    (is (= 26.785714285714285 (shape/tail-guard-ms {} 70.0)))
    (is (= 15.625 (shape/tail-guard-ms {} 120.0)))
    (is (< (Math/abs (- 10.4167 (shape/tail-guard-ms {} 180.0))) 1.0e-4))
    (is (= 5.0 (shape/tail-guard-ms {} 400.0)))
    (is (= 5.0 (shape/tail-guard-ms {:tail-guard {:beats 0.001}} 120.0)))
    (is (= 60.0 (shape/tail-guard-ms {:tail-guard {:beats 0.125}} 70.0))))
  (testing "every band's ring holds YIN's tail and the longest guard"
    (doseq [band (keys shape/bands) rate [44100.0 48000.0 96000.0 192000.0]]
      (let [{:keys [factor max-lag]} (shape/analysis-plan band rate)]
        (is (<= (+ (* 2 factor max-lag) (* rate 0.06)) (shape/ring-size band rate)) (pr-str band rate))))
    (is (= [8192 16384] [(shape/ring-size :full 48000.0) (shape/ring-size :sub 48000.0)]) "the 48 kHz rings are unchanged"))
  (testing "the extraction reports the beat its analysis reads up to"
    (doseq [bpm [70.0 120.0 180.0]]
      (let [samples (fixtures/passage :delay-synth {:bpm bpm :bars 2})
            {:keys [extractions]} (feed (shape/new-extractor {:extract :cycle :band :full} sample-rate) samples
                                        {:clock-at (fn [_] (clock bpm))})
            {:keys [window analysed-until-beat tail-guard status]} (first extractions)
            step (/ bpm 60.0 sample-rate)]
        (is (= :ok status) (str bpm))
        (is (= (Math/round (* sample-rate (/ (shape/tail-guard-ms {} bpm) 1000.0)))
               (Math/round (* sample-rate (/ (:ms tail-guard) 1000.0)))) (str bpm))
        (is (< (Math/abs (- (:beats tail-guard) (/ 1.0 32.0))) step) (str bpm))
        (is (<= 0.0 (- analysed-until-beat (- (:end-beat window) (:beats tail-guard))) step)
            (str bpm " " analysed-until-beat))))
    (let [{:keys [extractions]} (feed (shape/new-extractor {:extract :envelope} sample-rate)
                                      (fixtures/passage :delay-synth {:bpm 120.0 :bars 2}) {:clock-at (fn [_] (clock 120.0))})]
      (is (nil? (:tail-guard (first extractions))) "an envelope reads its whole window")
      (is (<= 0.0 (- (:analysed-until-beat (first extractions)) 4.0) (/ 2.0 sample-rate)))))
  (testing "a downbeat that leads the transport by 8 ms stays out of the tail"
    (let [samples (fixtures/passage :delay-synth {:bpm 120.0 :bars 3})
          run (fn [spec] (:extractions (feed (shape/new-extractor (merge {:extract :cycle :band :full} spec) sample-rate) samples
                                             {:clock-at (fn [_] (clock 120.0 {:time-ms 8.0}))})))]
      (is (every? #(and (= :ok (:status %)) (< (Math/abs (- 82.5 (:frequency-hz %))) 1.0)) (run {})))
      (is (every? #(= :low-confidence (:status %)) (run {:tail-guard {:beats 0.01}}))
          "with a 5 ms guard the next bar's first attack is in the tail"))))

(deftest window-end-analysis-is-bounded-and-decimated
  (testing "YIN's lag range, after decimating band-limited audio"
    (is (= {:frequency-range [20.0 60.0] :factor 16 :min-lag 50 :max-lag 150} (shape/analysis-plan :sub 48000.0)))
    (is (= 16 (:factor (shape/analysis-plan :bass 48000.0))))
    (is (= 1 (:factor (shape/analysis-plan :full 48000.0))) "an unfiltered band is never decimated")
    (is (= 1600 (:max-lag (shape/analysis-plan :full 48000.0)))))
  (testing "the period is refined at the full rate"
    (doseq [[band frequency] [[:sub 41.0] [:bass 97.0] [:full 233.0]]]
      (let [{:keys [frequency-hz status]} (shape/analyze-cycle (tone frequency 0.4) sample-rate band 32 0.8)]
        (is (= :ok status) (str band))
        (is (< (Math/abs (- frequency frequency-hz)) (* 0.002 frequency)) (str band " " frequency-hz)))))
  (testing "confidence is 1 − aperiodicity, and the default threshold rejects noise"
    (let [random (java.util.Random. 7) noise (double-array 24000)]
      (dotimes [i 24000] (aset noise i (- (.nextDouble random) 0.5)))
      (doseq [band [:bass :mid :full]]
        (let [{:keys [status confidence]} (shape/analyze-cycle noise sample-rate band 32 (:min-confidence shape/default-spec))]
          (is (= :low-confidence status) (str band))
          (is (< confidence (:min-confidence shape/default-spec)) (str band " " confidence))))
      (is (> (:confidence (shape/analyze-cycle (tone 110.0 0.2) sample-rate :bass 32 0.8)) 0.98)))))

(deftest specs-are-validated
  (is (= [] (shape/validate-spec {})))
  (is (= [[:band] [:extract] [:window] [:points] [:silence-db] [:min-confidence] [:tail-guard]]
         (mapv :path (shape/validate-spec {:band :treble :extract :fft :window {:ms 500} :points 64
                                           :silence-db 3 :min-confidence 2 :tail-guard {:ms 10}}))))
  (doseq [guard [{:beats 0} {:beats 0.2} {:beats -0.01} {:beats 0.05 :ms 3} 0.03]]
    (is (= [[:tail-guard]] (mapv :path (shape/validate-spec {:tail-guard guard}))) (pr-str guard)))
  (is (= [] (shape/validate-spec {:tail-guard {:beats 0.125}})))
  (is (thrown? clojure.lang.ExceptionInfo (shape/new-extractor {:points 4} sample-rate)))
  (is (= "bass 60 Hz–250 Hz" (shape/band-label :bass)))
  (is (= "full range" (shape/band-label :full)))
  (is (= "100 Hz–1.5 kHz" (shape/band-label [100 1500])))
  (is (= [100.0 900.0] (shape/band-range [100 900]))))

(deftest table-value-interpolates-one-periodic-cycle
  (let [table [0.0 1.0 0.5 0.5]]
    (is (= 0.0 (shape/table-value table 0.0)))
    (is (= 0.5 (shape/table-value table 0.125)))
    (is (= 0.125 (shape/table-value table 0.9375)) "the last point wraps to the first")
    (is (close? 1.0e-12 (repeat 2 (shape/table-value table 0.3))
                [(shape/table-value table 2.3) (shape/table-value table -0.7)]))))
