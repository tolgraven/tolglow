(ns tolglow.composition.sequence-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.composition.sequence :as sequence]))

(def spec {:steps [{:id :a :cue-id :red :duration-ms 100}
                   {:id :b :cue-id :blue :duration-ms 100}
                   {:id :c :cue-id :green :duration-ms 100}]})

(defn sampled-steps [spec times]
  (loop [state (sequence/initial-state spec 0 "owner") times times result []]
    (if-let [time (first times)]
      (let [next (sequence/advance spec state time)]
        (recur (:state next) (rest times) (conj result (get-in next [:step :id])))) result)))

(deftest once-loop-and-ping-pong-have-defined-boundaries
  (is (= [:a :b :c nil] (sampled-steps spec [0 100 200 300])))
  (is (= [:a :b :c :a] (sampled-steps (assoc spec :mode :loop) [0 100 200 300])))
  (is (= [:a :b :c :b :a :b] (sampled-steps (assoc spec :mode :ping-pong) [0 100 200 300 400 500])))
  (is (= [:a :a :a] (sampled-steps (assoc spec :mode :ping-pong :steps [(first (:steps spec))]) [0 100 200]))))

(deftest seeded-shuffle-visits-each-slot-once-per-cycle
  (let [spec (assoc spec :mode :shuffle :seed 55)
        order (sampled-steps spec (range 0 900 100))]
    (is (= order (sampled-steps spec (range 0 900 100))))
    (doseq [cycle (partition 3 order)] (is (= #{:a :b :c} (set cycle)))))
  (let [random (assoc spec :mode :random :seed 19)]
    (is (= (sampled-steps random (range 0 500 100))
           (sampled-steps random (range 0 500 100))))))

(deftest repeated-tick-and-missed-steps-do-not-duplicate-launches
  (let [start (sequence/advance spec (sequence/initial-state spec 0 "show-1") 0)
        same (sequence/advance spec (:state start) 0)
        skipped (sequence/advance spec (:state same) 250)
        stopped (sequence/stop (:state skipped))]
    (is (= 1 (count (:intents start))))
    (is (empty? (:intents same)))
    (is (= [:composition/release :composition/launch] (mapv :op (:intents skipped))))
    (is (= [:red :green] (mapv :cue-id [(first (:intents start)) (last (:intents skipped))])))
    (is (= ["show-1" 2] (get-in stopped [:intents 0 :token])))
    (is (empty? (:intents (sequence/stop (:state stopped)))))))

(deftest invalid-time-and-unbounded-catch-up-are-rejected
  (is (thrown? clojure.lang.ExceptionInfo (sequence/initial-state (assoc spec :steps []) 0 :x)))
  (let [spec (assoc spec :mode :loop :max-transitions 2)
        state (sequence/initial-state spec 0 :x)]
    (is (thrown? clojure.lang.ExceptionInfo (sequence/advance spec state 1000)))
    (is (thrown? clojure.lang.ExceptionInfo (sequence/advance spec state -1)))))

(def beat-spec
  {:timebase :beats :mode :loop
   :steps [{:id :a :cue-id :red :duration-beats 1}
           {:id :b :cue-id :blue :duration-beats 2}]})

(deftest beat-clock-follows-musical-position-across-tempo-and-pause
  (let [start (sequence/initial-state beat-spec {:time-ms 0 :beat 8 :bpm 120} :beats)
        a (sequence/advance beat-spec start {:time-ms 0 :beat 8 :bpm 120})
        slower (sequence/advance beat-spec (:state a) {:time-ms 250 :beat 8.5 :bpm 60})
        b (sequence/advance beat-spec (:state slower) {:time-ms 750 :beat 9 :bpm 60})
        pause (sequence/advance beat-spec (:state b) {:time-ms 1000 :beat 9.25 :bpm 60 :playing? false})
        resume (sequence/advance beat-spec (:state pause) {:time-ms 5000 :beat 9.25 :bpm 60 :playing? true})
        next-a (sequence/advance beat-spec (:state resume) {:time-ms 6750 :beat 11 :bpm 60})]
    (is (= :a (get-in slower [:step :id])))
    (is (= 0.5 (:phase slower)))
    (is (= 500.0 (:remaining-ms slower)))
    (is (empty? (:intents slower)))
    (is (= :b (get-in b [:step :id])))
    (is (= 2000.0 (:remaining-ms b)))
    (is (= 0.125 (:phase pause) (:phase resume)))
    (is (nil? (:remaining-ms pause)))
    (is (empty? (:intents resume)))
    (is (= :a (get-in next-a [:step :id])))
    (is (= 12 (get-in next-a [:state :step-end-beat])))))

(deftest beat-clock-rejects-ambiguous-duration-and-backward-seeks
  (is (seq (sequence/validate-sequence (assoc-in beat-spec [:steps 0 :duration-ms] 100))))
  (is (thrown? clojure.lang.ExceptionInfo (sequence/initial-state beat-spec 0 :beats)))
  (let [state (sequence/initial-state beat-spec {:time-ms 100 :beat 4 :bpm 120} :beats)]
    (is (thrown? clojure.lang.ExceptionInfo
                 (sequence/advance beat-spec state {:time-ms 200 :beat 3.5 :bpm 120})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (sequence/advance beat-spec state {:time-ms 200 :beat 4.5 :bpm 0})))))

(deftest numeric-millisecond-api-remains-compatible-with-explicit-clock-map
  (let [state (sequence/initial-state spec 0 :old)]
    (is (= (sequence/advance spec state 50)
           (sequence/advance spec state {:time-ms 50})))))

(deftest canonical-bars-phrases-and-fractions-use-launch-meter
  (let [spec {:steps [{:id :fraction :cue-id :red :duration {:beats 1/2}}
                     {:id :bar :cue-id :blue :duration {:bars 1}}
                     {:id :phrase :cue-id :green :duration {:phrases 1}}]}
        clock {:time-ms 0 :beat 8 :bpm 120 :meter {:beats-per-bar 3 :bars-per-phrase 2}}
        state (sequence/initial-state spec clock :musical)
        first (sequence/advance spec state clock)
        next (sequence/advance spec (:state first) (assoc clock :time-ms 500 :beat 8.5 :bpm 60))
        phrase (sequence/advance spec (:state next) (assoc clock :time-ms 900 :beat 11.5 :meter {:beats-per-bar 7}))
        paused (sequence/advance spec (:state phrase) (assoc clock :time-ms 90000 :beat 11.5 :playing? false))]
    (is (= :fraction (get-in first [:step :id])))
    (is (= 1/2 (:remaining-beats first)))
    (is (= :bar (get-in next [:step :id])))
    (is (= 3000.0 (:remaining-ms next)))
    (is (= :phrase (get-in phrase [:step :id])))
    (is (== 6 (:remaining-beats phrase)))
    (is (= (:phase phrase) (:phase paused)))
    (is (nil? (:remaining-ms paused)))
    (is (:done? (sequence/advance spec (:state paused) (assoc clock :time-ms 91000 :beat 17.5))))))

(deftest default-step-is-one-beat-and-canonical-ms-remains-explicit
  (is (= 1 (get-in (sequence/normalize-spec {:steps [{:id :x :cue-id :red}]}) [:steps 0 :duration-beats])))
  (let [spec {:steps [{:id :x :cue-id :red :duration {:ms 100}}]}
        state (sequence/initial-state spec 0 :ms)]
    (is (= :ms (:timebase state)))
    (is (:done? (sequence/advance spec state 100))))
  (is (seq (sequence/validate-sequence {:steps [{:id :x :cue-id :red :duration {:beats 0}}]}))))
