(ns tolglow.integrations.analysis-test
  (:require [clojure.test :refer :all]
            [tolglow.integrations.analysis :as analysis]))

(def sample-rate 48000.0)

(def clock {:time-ms 0.0 :beat 0.0 :bpm 120.0 :source :midi-clock :epoch 1 :confidence 0.9})

(defn close? [a b tolerance] (< (Math/abs (double (- a b))) tolerance))

(defn sine [frequency amplitude time-ms]
  (* amplitude (Math/sin (/ (* 2.0 Math/PI frequency time-ms) 1000.0))))

(defn kick-train [time-ms]
  (let [local (mod (- time-ms 1000.0) 500.0)]
    (if (and (>= time-ms 1000) (< local 160))
      (sine 65.0 (* 0.8 (Math/exp (/ (- local) 45.0))) local) 0.0)))

(defn run-signal
  ([signal duration-ms] (run-signal signal duration-ms {} 480 clock))
  ([signal duration-ms options block-size transport]
   (let [length (long (* sample-rate (/ duration-ms 1000.0)))]
     (loop [position 0 state (analysis/new-analyzer options) events []]
       (if (= position length)
         {:state state :events events :features (:features state)}
         (let [n (min block-size (- length position))
               start (* 1000.0 (/ position sample-rate))
               input (double-array (map #(signal (* 1000.0 (/ (+ position %) sample-rate))) (range n)))
               result (analysis/analyze state {:samples input :sample-rate sample-rate
                                               :started-at-ms start
                                               :received-at-ms (+ start (* 1000.0 (/ n sample-rate)) 2.0)
                                               :clock transport})]
           (recur (+ position n) (:state result) (into events (:events result)))))))))

(defn events-of [result type] (filterv #(= type (:type %)) (:events result)))

(def ^:private section-rate 8000)

(def ^:private section-beat-samples 4000)

(def ^:private section-origin-ms 2000.0)

(def ^:private section-clock
  {:time-ms 2500.0 :beat 1.0 :bpm 120.0 :source :midi-clock :source-epoch 3
   :session-id "transport-a" :confidence 0.9 :playing? true})

(defn- section-pcm [index]
  (let [beat (quot index section-beat-samples)
        frequencies (if (< beat 8) [48.0 64.0 84.0 104.0] [84.0 104.0 48.0 64.0])
        local-ms (* 1000.0 (/ (rem index section-beat-samples) section-rate))]
    (+ (sine (nth frequencies (mod beat 4)) 0.3 local-ms)
       (sine 76.0 (* 0.08 (Math/exp (/ (- local-ms) 18.0))) local-ms)
       (sine 700.0 0.025 local-ms))))

(defn- run-section-pcm [state start end block-sizes transport]
  (let [capture-ms #(+ section-origin-ms (* 1000.0 (/ % section-rate)))]
    (loop [index start block-sizes (cycle block-sizes) state state events []]
      (if (= index end)
        {:state state :events events :features (:features state)}
        (let [next-index (min end (+ index (first block-sizes)))
              received-at-ms (+ 2.0 (capture-ms next-index))
              result (analysis/analyze state {:samples (double-array (map section-pcm (range index next-index)))
                                              :sample-rate section-rate :started-at-ms (capture-ms index)
                                              :received-at-ms received-at-ms
                                              :clock (if (fn? transport) (transport received-at-ms) transport)})]
          (recur next-index (rest block-sizes) (:state result) (into events (:events result))))))))

(deftest opt-in-sections-consume-every-hop-inside-arbitrary-pcm-blocks
  (let [size (* 24 section-beat-samples)
        options {:sections {}}
        run #(run-section-pcm (analysis/new-analyzer %) 0 size %2 section-clock)
        large (run options [65536])
        irregular (run options [193 1199 71 4100])
        reanchored (run-section-pcm (analysis/new-analyzer options) 0 size [64001]
                                    (fn [arrived-at-ms]
                                      (assoc section-clock :time-ms arrived-at-ms
                                             :beat (/ (- arrived-at-ms section-origin-ms) 500.0))))
        disabled (run {} [65536])
        without-arrival #(dissoc % :observed-at-ms :latency-ms :available-at-beat)
        changes (events-of large :audio/section-change)
        [event] changes
        feel (get-in large [:features :audio/section])]
    (is (= 1 (count changes)))
    (is (= 1 (:version event)))
    (is (= [:bass-pattern] (get-in event [:evidence :drivers])))
    (is (= 8.0 (:boundary-beat event) (:confirmation-delay-beats event)))
    (is (= 16.0 (:confirmed-at-beat event)))
    (is (= 10000.0 (:time-ms event) (:confirmed-at-ms event)))
    (is (= 194.0 (:latency-ms event)))
    (is (= :confirmation-hop-end (get-in event [:timing :basis])))
    (is (= :midi-clock (:clock-source event)))
    (is (= 3 (:clock-epoch event)))
    (is (= "transport-a" (:clock-session-id event)))
    (is (<= 0.45 (:confidence event) 0.9))
    ;; The reference is the first window, which lacks the hop that ends on its
    ;; start, so the level matches to a fraction of a percent, not exactly.
    (is (close? (:current-rms (:level event)) (:previous-rms (:level event)) 1.0e-4))
    (is (= feel (get-in irregular [:features :audio/section])))
    (is (= (select-keys event (keys feel)) feel))
    (is (= (range 1 (inc (count (:events large)))) (map :sequence (:events large))))
    (is (= (mapv without-arrival (:events large)) (mapv without-arrival (:events irregular))))
    (is (= (mapv without-arrival changes)
           (mapv without-arrival (events-of reanchored :audio/section-change))))
    (is (empty? (events-of disabled :audio/section-change)))
    (is (not (contains? (:features disabled) :audio/section)))
    (is (= (mapv #(dissoc % :sequence) (:events disabled))
           (mapv #(dissoc % :sequence) (remove (comp #{:audio/section-change} :type) (:events large)))))))

(deftest section-clock-loss-or-provenance-change-fences-a-pending-candidate
  (let [transition (* 13 section-beat-samples)
        end (* 18 section-beat-samples)
        prefix (run-section-pcm (analysis/new-analyzer {:sections {}}) 0 transition [1359] section-clock)
        clocks [nil (assoc section-clock :playing? false) (assoc section-clock :stale? true)
                (assoc section-clock :confidence 0.1) (dissoc section-clock :confidence)
                (dissoc section-clock :source-epoch) (assoc section-clock :source-epoch 4)
                (assoc section-clock :source :link) (assoc section-clock :session-id "transport-b")
                (assoc section-clock :session-id {:invalid :identity})]]
    (is (empty? (events-of prefix :audio/section-change)))
    (is (= 1 (count (events-of (run-section-pcm (:state prefix) transition end [1359] section-clock)
                               :audio/section-change))))
    (doseq [transport clocks]
      (let [short (run-section-pcm (:state prefix) transition (inc transition) [1] transport)
            resumed (run-section-pcm (:state short) (inc transition) end [1359] section-clock)]
        (is (empty? (:events short)))
        (is (empty? (events-of resumed :audio/section-change)))))))

(deftest same-source-phase-jumps-and-loss-clear-confirmed-section-evidence
  (let [position (* 17 section-beat-samples)
        confirmed (run-section-pcm (analysis/new-analyzer {:sections {}}) 0 position [4000] section-clock)]
    (is (= 8.0 (get-in confirmed [:features :audio/section :boundary-beat])))
    (doseq [[transport length] [[nil 1]
                                [(update section-clock :beat inc) 80]
                                [(update section-clock :beat dec) 80]]]
      (let [reset (run-section-pcm (:state confirmed) position (+ position length) [80] transport)]
        (is (empty? (events-of reset :audio/section-change)))
        (is (not (contains? (:features reset) :audio/section)))))))

(deftest section-options-are-explicit-and-validated
  (doseq [bad [true nil {:unknown true} {:beats-per-window 0} {:anchor-beat Double/NaN}]]
    (is (thrown? clojure.lang.ExceptionInfo (analysis/new-analyzer {:sections bad}))))
  (is (= {:anchor-beat 1 :beats-per-window 2}
         (get-in (analysis/new-analyzer {:sections {:anchor-beat 1 :beats-per-window 2}})
                 [:options :sections]))))

(deftest above-full-scale-pcm-publishes-a-bounded-control-signal
  (let [result (analysis/analyze (analysis/new-analyzer)
                                {:samples (double-array (repeat 4800 2.5))
                                 :sample-rate sample-rate :started-at-ms 0 :received-at-ms 100})
        events (events-of result :audio/energy)]
    (is (= 2.5 (get-in result [:features :rms])))
    (is (= 1.0 (get-in result [:features :audio/energy])))
    (is (seq events))
    (is (every? #(= 1.0 (:audio/energy %)) events))))

(deftest kick-onsets-align-with-clock-without-changing-tempo
  (let [initial-clock clock result (run-signal kick-train 4000)
        kicks (events-of result :audio/kick) beats (events-of result :audio/beat)]
    (is (= 6 (count kicks) (count beats)))
    (is (= [2 3 4 5 6 7] (mapv :beat beats)))
    (doseq [[expected kick beat] (map vector (range 1000 4000 500) kicks beats)]
      (is (<= expected (:time-ms kick) (+ expected 20)))
      (is (close? 5.0 (:phase-error-ms beat) 0.001))
      (is (close? 7.0 (:latency-ms kick) 0.001))
      (is (= :adaptive-band-onset (:inference kick)))
      (is (= :kick-confirmed-clock (:inference beat)))
      (is (= :midi-clock (:clock-source beat)))
      (is (= 1 (:clock-epoch beat)))
      (is (<= 0 (:confidence beat) (:confidence kick) 1)))
    (is (= initial-clock clock))
    (is (every? #(= 120.0 (:bpm %)) beats))
    (is (nil? (:bpm (:state result))))
    (is (= (range 1 (inc (count (:events result)))) (map :sequence (:events result))))))

(deftest clock-phase-confidence-and-running-state-gate-beat-events
  (doseq [transport [nil (assoc clock :beat 0.5) (assoc clock :confidence 0.1)
                     (assoc clock :running? false) (assoc clock :playing? false :running? true)]]
    (let [result (run-signal kick-train 1600 {} 480 transport)]
      (is (= 2 (count (events-of result :audio/kick))))
      (is (empty? (events-of result :audio/beat)))))
  (let [result (run-signal kick-train 1600 {} 480 (assoc clock :beat 0.5))]
    (is (every? #(false? (get-in % [:alignment :aligned?])) (events-of result :audio/kick)))))

(deftest fixed-hop-processing-is-independent-of-input-chunk-boundaries
  (let [a (run-signal kick-train 2500 {} 480 clock)
        b (run-signal kick-train 2500 {} 127 clock)
        strip-arrival #(dissoc % :observed-at-ms :latency-ms)]
    (is (= (mapv strip-arrival (:events a)) (mapv strip-arrival (:events b))))
    (is (= (dissoc (:features a) :input-age-ms) (dissoc (:features b) :input-age-ms)))
    (is (= (:filter-delay (:state a)) (:filter-delay (:state b))))
    (is (= (dissoc (:state a) :last-end-ms :features) (dissoc (:state b) :last-end-ms :features)))))

(deftest silence-initial-calibration-and-steady-bass-do-not-retrigger
  (let [silent (run-signal (constantly 0.0) 2000)
        steady (run-signal #(sine 65.0 0.7 %) 2000)
        sudden (run-signal #(if (< % 1000) 0.0 (sine 65.0 0.7 (- % 1000))) 3000)]
    (is (empty? (events-of silent :audio/kick)))
    (is (empty? (events-of silent :audio/breakdown-start)))
    (is (zero? (get-in silent [:features :rms])))
    (is (= -240.0 (get-in silent [:features :energy-dbfs])))
    (is (empty? (events-of steady :audio/kick)))
    (is (= 1 (count (events-of sudden :audio/kick))))
    (is (every? #(<= 0 (:confidence %) 1) (:events sudden)))))

(deftest high-frequency-transients-and-subthreshold-noise-are-not-kicks
  (let [hi (run-signal #(if (< % 1000) 0.0
                           (let [local (mod (- % 1000) 500)]
                             (if (< local 100) (sine 6000 (* 0.6 (Math/exp (/ (- local) 30.0))) local) 0.0))) 2500)
        quiet (run-signal #(* 0.001 (kick-train %)) 2500)]
    (is (empty? (events-of hi :audio/kick)))
    (is (empty? (events-of quiet :audio/kick)))))

(deftest refractory-window-suppresses-closely-spaced-pulses
  (let [signal (fn [ms]
                 (reduce + (for [start [1000 1080 1200]
                                 :let [local (- ms start)] :when (<= 0 local 40)]
                             (sine 65.0 (* 0.8 (Math/exp (/ (- local) 10.0))) local))))
        kicks (events-of (run-signal signal 1500) :audio/kick)]
    (is (= 2 (count kicks)))
    (is (close? 1005.0 (:time-ms (first kicks)) 0.001))
    (is (close? 1205.0 (:time-ms (second kicks)) 0.001))))

(deftest relative-energy-breakdowns-use-confirmation-and-recovery-hysteresis
  (let [result (run-signal #(sine 1000.0 (cond (< % 2000) 0.5 (< % 4000) 0.002 :else 0.5) %) 6000)
        starts (events-of result :audio/breakdown-start) ends (events-of result :audio/breakdown-end)
        start (first starts) end (first ends)]
    (is (= 1 (count starts) (count ends)))
    (is (<= 700 (:confirmation-ms start) 711))
    (is (<= 150 (:confirmation-ms end) 161))
    (is (< 2000 (:time-ms start) (:confirmed-at-ms start) 4000))
    (is (< 4000 (:time-ms end) (:confirmed-at-ms end) 4500))
    (is (> (:latency-ms start) 700))
    (is (= :relative-energy-hysteresis (:inference start)))
    (is (false? (get-in result [:features :breakdown?])))
    (is (= :relative-signal-energy-not-emotion (get-in result [:features :interpretation])))
    (is (nil? (get-in result [:features :mood]))))
  (let [brief-drop (run-signal #(sine 1000.0 (if (<= 2000 % 2150) 0.002 0.5) %) 4000)]
    (is (empty? (events-of brief-drop :audio/breakdown-start)))))

(deftest analyzer-state-and-inputs-remain-owned-by-the-caller
  (let [state (analysis/new-analyzer) before state
        values (double-array (repeat 480 0.2)) values-before (vec values)
        block {:samples values :sample-rate sample-rate :started-at-ms 0.0 :received-at-ms 12.0}
        a (analysis/analyze state block) b (analysis/analyze state block)]
    (is (= a b))
    (is (= before state))
    (is (= values-before (vec values)))
    (is (= [0.0 0.0 0.0 0.0] (:filter-delay (:state (analysis/analyze (analysis/new-analyzer)
                                                                 (assoc block :samples (double-array 480)))))))
    (is (every? #(not (or (instance? clojure.lang.IAtom %) (.isArray (class %))))
                (filter some? (tree-seq coll? seq (:state a)))))))

(deftest gaps-and-sample-rate-changes-recalibrate-without-false-transitions
  (let [a (analysis/analyze (analysis/new-analyzer)
                            {:samples (double-array 480) :sample-rate sample-rate :started-at-ms 0 :received-at-ms 12})
        b (analysis/analyze (:state a)
                            {:samples (double-array (repeat 480 0.8)) :sample-rate sample-rate
                             :started-at-ms 1000 :received-at-ms 1012})
        c (analysis/analyze (:state b)
                            {:samples (double-array 441) :sample-rate 44100.0
                             :started-at-ms 1010 :received-at-ms 1022})]
    (is (false? (:discontinuity? a)))
    (is (:discontinuity? b))
    (is (:discontinuity? c))
    (is (false? (get-in b [:features :calibrated?])))
    (is (empty? (events-of b :audio/kick)))
    (is (empty? (events-of c :audio/breakdown-start)))
    (is (< (get-in a [:state :sequence]) (get-in b [:state :sequence]) (get-in c [:state :sequence])))))

(deftest invalid-blocks-and-clock-are-rejected-before-events
  (let [state (analysis/new-analyzer)
        block {:samples (double-array 480) :sample-rate sample-rate :started-at-ms 0 :received-at-ms 12}]
    (doseq [bad [(assoc block :samples [Double/NaN]) (assoc block :samples [])
                 (assoc block :samples 480) (assoc block :samples (repeat 0.0))
                 (assoc block :sample-rate 0) (assoc block :received-at-ms 9)
                 (assoc block :started-at-ms -1) (assoc block :clock {:bpm 120})
                 (assoc block :clock (assoc clock :confidence 2))]]
      (is (thrown? clojure.lang.ExceptionInfo (analysis/analyze state bad))))
    (let [next-state (:state (analysis/analyze state block))]
      (is (thrown? clojure.lang.ExceptionInfo (analysis/analyze next-state block))))
    (is (thrown? clojure.lang.ExceptionInfo (analysis/new-analyzer {:kick-band-hz [200 100]})))
    (is (thrown? clojure.lang.ExceptionInfo (analysis/new-analyzer {:typo 1})))
    (is (thrown? clojure.lang.ExceptionInfo (analysis/new-analyzer {:hop-ms 0})))))

(deftest partial-hop-waits-for-enough-samples
  (let [result (analysis/analyze (analysis/new-analyzer)
                                 {:samples [0.1] :sample-rate sample-rate
                                  :started-at-ms 0.0 :received-at-ms 1.0})]
    (is (nil? (:features result)))
    (is (empty? (:events result)))
    (is (= 1 (get-in result [:state :hop :n])))))

(deftest subscriptions-are-explicit-composable-and-isolate-handler-errors
  (let [received (atom [])
        empty-bus (analysis/new-subscriptions)
        subscribed (-> empty-bus
                       (analysis/subscribe :a-kicks [:audio/kick] #(swap! received conj [:kick (:sequence %)]))
                       (analysis/subscribe :b-broken :all (fn [_] (throw (ex-info "handler failed" {}))))
                       (analysis/subscribe :c-all :all #(swap! received conj [:all (:sequence %)])))
        events [{:type :audio/kick :sequence 1} {:type :audio/energy :sequence 2}]
        _ (is (empty? @received))
        result (analysis/publish! subscribed events)]
    (is (= {} empty-bus))
    (is (= [[:kick 1] [:all 1] [:all 2]] @received))
    (is (= 3 (:delivered result)))
    (is (= [:b-broken :b-broken] (mapv :subscription-id (:errors result))))
    (is (= 1 (:delivered (analysis/publish! (analysis/unsubscribe subscribed :c-all) events))))
    (is (thrown? clojure.lang.ExceptionInfo (analysis/subscribe empty-bus :invalid [] identity)))))

(defn- measured-tone [hz block-size]
  (let [rate 8000 n 3200]
    (loop [position 0 state (analysis/new-analyzer)]
      (if (= position n) (:features state)
        (let [end (min n (+ position block-size))
              samples (mapv #(if (zero? hz) 0.0 (* 0.5 (Math/sin (/ (* 2.0 Math/PI hz %) rate))))
                            (range position end))
              result (analysis/analyze state {:samples samples :sample-rate rate
                                               :started-at-ms (/ position 8.0) :received-at-ms (/ end 8.0)})]
          (recur end (:state result)))))))

(deftest band-levels-and-sub-bass-pitch-survive-arbitrary-block-partitions
  (let [sub (measured-tone 41.2 64)
        irregular (measured-tone 41.2 193)
        mid (measured-tone 700 80)
        high (measured-tone 3300 80)
        silent (measured-tone 0 51)]
    (is (> (:low-rms sub) (:mid-rms sub) (:high-rms sub)))
    (is (> (:mid-rms mid) (:low-rms mid)))
    (is (> (:high-rms high) (:low-rms high)))
    (is (< (Math/abs (- 41.2 (:bass-pitch-hz sub))) 0.6))
    (is (> (:bass-pitch-confidence sub) 0.95))
    (is (= (select-keys sub [:low-rms :mid-rms :high-rms :bass-pitch-hz])
           (select-keys irregular [:low-rms :mid-rms :high-rms :bass-pitch-hz])))
    (doseq [tone [190 300 450 700 1510 3300]]
      (is (nil? (:bass-pitch-hz (measured-tone tone 80))) (str tone " Hz must not open the bass gate")))
    (is (nil? (:bass-pitch-hz silent)))
    (is (zero? (:bass-pitch-confidence silent)))))
