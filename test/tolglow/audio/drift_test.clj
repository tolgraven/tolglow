(ns tolglow.audio.drift-test
  "The capture boundary's delay-locked drift tracker (#912), driven with
  explicit positions and arrival times: steady drift, jitter, latency steps,
  backlogs, an elevated start, block-size capacity, the bound's hysteresis and
  the arrival clamp."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.audio.drift :as drift]))

(def ^:private sample-rate 48000.0)

(defn- run
  "Feed blocks of a device whose clock runs ppm against the host, delivered
  latency-ms (plus seeded jitter up to jitter-ms, plus (extra block real-end-ms))
  after their last sample. Returns every tracker state with :s, the real time
  in seconds, and :error, the corrected end timestamp minus the real one."
  [ppm seconds & [{:keys [latency-ms jitter-ms seed block-frames extra ppm-at]
                   :or {latency-ms 3.0 jitter-ms 0.0 seed 1 block-frames 480 extra (fn [_ _] 0.0)}}]]
  (let [random (java.util.Random. seed)
        ;; ppm-at gives the device's rate at an audio position in seconds.
        ppm-at (or ppm-at (constantly ppm))
        block-ms (* 1000.0 (/ block-frames sample-rate))]
    (loop [block 1 tracker (drift/new-tracker sample-rate block-frames) real 0.0 out []]
      (if (> (* block block-ms) (* 1000.0 seconds))
        out
        (let [position (* block block-ms)
              real-end (+ real (/ block-ms (+ 1.0 (* 1.0e-6 (ppm-at (/ position 1000.0))))))
              arrival (+ real-end latency-ms (* jitter-ms (.nextDouble random)) (extra block real-end))
              tracker (drift/observe tracker position (- arrival position))]
          (recur (inc block) tracker real-end
                 (conj out (assoc tracker :s (/ real-end 1000.0)
                                  :error (- (+ position (:correction-ms tracker)) real-end)))))))))

(defn- after [states seconds] (filter #(>= (:s %) seconds) states))

(defn- max-error [states] (reduce max 0.0 (map #(Math/abs (double (:error %))) states)))

(defn- invalid-seconds [states block-frames]
  (* (count (remove drift/valid? states)) (/ block-frames sample-rate)))

(defn- flips [states] (count (filter true? (map not= (map drift/valid? states) (rest (map drift/valid? states))))))

(deftest a-steady-drift-is-measured-and-corrected
  (doseq [ppm [-100.0 -25.0 25.0 100.0]]
    (let [states (run ppm 120.0)
          settled (drop 3000 states)
          last-state (peek states)]
      (is (= :measuring (:state (first states))) "the first segments only measure")
      (is (every? #(= :locked (:state %)) settled) (str ppm))
      (is (< (Math/abs (- ppm (:ppm last-state))) 0.5) (str ppm " measured " (:ppm last-state)))
      (is (every? #(< (Math/abs (:error %)) 0.01) settled) (str ppm " ms off the real sample times"))
      (is (< (Math/abs (- (* -1.0e-6 ppm 120000.0) (:correction-ms last-state))) 0.05)
          "after two minutes the correction is the accumulated drift"))))

(deftest a-genuine-change-of-drift-rate-is-followed-not-re-anchored
  (doseq [[label ppm-at] [["0 to +240 ppm at 60 s" #(if (< % 60.0) 0.0 240.0)]
                          ["0 to -240 ppm at 60 s" #(if (< % 60.0) 0.0 -240.0)]
                          ["100 to 0 ppm at 60 s" #(if (< % 60.0) 100.0 0.0)]
                          ["a sweep from 0 to 240 ppm over 60 s" #(* 4.0 (max 0.0 (min 60.0 (- % 30.0))))]]]
    (let [states (run 0.0 180.0 {:ppm-at ppm-at :jitter-ms 1.0 :seed 5})]
      (is (< (max-error (after states 120.0)) 0.2) label)
      (is (zero? (:steps (peek states))) label)
      (is (every? #(= :locked (:state %)) (after states 120.0)) label))))

(deftest the-correction-steps-at-most-0.4-sample-per-block
  (let [limit (/ 400.0 sample-rate)
        states (run 200.0 30.0)
        steps (map (fn [a b] (Math/abs (- (:correction-ms b) (:correction-ms a)))) states (rest states))]
    (is (every? #(<= % (+ limit 1.0e-12)) steps))
    (is (some #(< (- limit 1.0e-9) %) steps) "catching up after the first estimate moves at the limit")))

(deftest delivery-jitter-only-raises-the-floor
  (let [states (run -50.0 300.0 {:jitter-ms 4.0 :seed 912})
        settled (drop 3000 states)]
    (is (every? #(= :locked (:state %)) settled))
    (is (< (Math/abs (+ 50.0 (:ppm (peek states)))) 10.0) (str (:ppm (peek states))))
    (is (every? #(< (Math/abs (:error %)) 1.0) settled) "within 1 ms of the real sample times")
    (is (< (max-error (after (run 0.0 120.0 {:jitter-ms 3.0 :seed 7}) 30.0)) 0.3) "no drift: a small bias at most")
    (is (= (map :correction-ms states) (map :correction-ms (run -50.0 300.0 {:jitter-ms 4.0 :seed 912})))
        "deterministic for the same arrivals")))

(deftest latency-changes-are-not-drift
  (testing "an elevated delay floor over the first seconds (cold start, buffer fill)"
    (doseq [seconds [1.0 4.5]]
      (let [states (run 50.0 120.0 {:extra (fn [_ t] (if (< t (* 1000.0 seconds)) 8.0 0.0))})]
        (is (< (max-error (after states 30.0)) 0.2) (str seconds " s elevated"))
        (is (every? drift/valid? (after states 10.0))))))
  (testing "a latency step up at 60 s is re-anchored, not slewed into the timestamps"
    (doseq [step [2.0 3.0 5.0]]
      (let [states (run 0.0 120.0 {:extra (fn [_ t] (if (> t 60000.0) step 0.0))})]
        (is (< (max-error (after states 30.0)) 0.2) (str "+" step " ms"))
        (is (zero? (invalid-seconds (after states 30.0) 480)) (str "+" step " ms keeps timing valid"))
        (is (= 1 (:steps (peek states))))
        (is (< (Math/abs (- step (:last-step-ms (peek states)))) 0.1)))))
  (testing "a 1 ms change is below the step threshold: it is absorbed, at most that far off"
    (let [states (run 20.0 150.0 {:extra (fn [_ t] (if (> t 60000.0) -1.0 0.0))})]
      (is (< (max-error (after states 30.0)) 1.5))
      (is (every? drift/valid? (after states 10.0)))))
  (testing "reads quantised by a 512-frame device period: the floor's scatter is not a drop"
    (let [period (/ 512000.0 48000.0)
          states (run 50.0 120.0 {:extra (fn [_ t] (- (* period (Math/ceil (/ t period))) t))})]
      (is (< (max-error (after states 30.0)) 0.6))
      (is (every? drift/valid? (after states 10.0)))))
  (testing "a 20 ms backlog for three seconds"
    (let [states (run 0.0 120.0 {:extra (fn [_ t] (if (< 60000.0 t 63000.0) 20.0 0.0))})]
      (is (< (max-error (after states 30.0)) 0.2))
      (is (zero? (invalid-seconds (after states 30.0) 480)))))
  (testing "a 200 ms stall and its burst"
    (let [states (run 50.0 120.0 {:extra (fn [_ t] (let [d (- t 60000.0)] (if (< 0 d 200.0) (- 200.0 d) 0.0)))})]
      (is (< (max-error (after states 30.0)) 0.2))
      (is (every? drift/valid? (after states 30.0))))))

(deftest the-bound-follows-what-the-block-size-can-correct
  (is (= 250.0 (:bound-ppm (drift/new-tracker sample-rate 480))))
  (is (= 250.0 (:bound-ppm (drift/new-tracker sample-rate drift/max-block-frames))))
  (is (< (:bound-ppm (drift/new-tracker sample-rate 1024 {:slew-samples 0.25})) 200.0)
      "a smaller step lowers the bound to 80% of what it can follow")
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"at most 1024" (drift/new-tracker sample-rate 2048)))
  (testing "100 ppm at the largest allowed block stays locked and within 1 ms"
    (let [states (run 100.0 120.0 {:block-frames drift/max-block-frames :jitter-ms 2.0})]
      (is (every? #(= :locked (:state %)) (after states 15.0)))
      (is (< (max-error (after states 15.0)) 1.0)))))

(deftest the-bound-has-hysteresis
  (let [wobble (fn [block _] (* 0.4 (Math/sin (/ block 37.0))))
        near (run 245.0 300.0 {:jitter-ms 2.0 :seed 3 :extra wobble})
        beyond (run 255.0 300.0 {:jitter-ms 2.0 :seed 3 :extra wobble})]
    (is (<= (flips (after near 30.0)) 2) "245 ppm with a wobbling floor no longer flaps")
    (is (every? drift/valid? (after (run 245.0 300.0 {:jitter-ms 2.0 :seed 3}) 30.0))
        "245 ppm with ordinary jitter stays valid")
    (is (<= (flips (after beyond 30.0)) 2))
    (is (= :out-of-bound (:state (peek beyond)))))
  (testing "an invalid state holds for at least one segment"
    (let [tracker (assoc (drift/new-tracker sample-rate 480) :state :out-of-bound :invalid-since 1000.0
                         :fit {:rate 0.0 :intercept 3.0} :baseline 3.0 :history [[0.0 3.0] [500.0 3.0]])]
      (is (= :out-of-bound (:state (drift/observe tracker 1500.0 3.0))))
      (is (= :locked (:state (drift/observe tracker 2000.0 3.0))))))
  (testing "a correction too far behind its target is invalid until within 1 ms"
    (let [tracker (assoc (drift/new-tracker sample-rate 480) :fit {:rate 0.0 :intercept 5.0} :baseline 0.0
                         :history [[0.0 5.0] [500.0 5.0]])
          behind (drift/observe tracker 1000.0 50.0)]
      (is (= :behind (:state behind)))
      (is (false? (drift/valid? behind)))
      (is (= :behind (:state (drift/observe (assoc behind :correction-ms 3.5 :invalid-since -5000.0) 1010.0 50.0)))
          "1.5 ms behind is still behind")
      (is (= :locked (:state (drift/observe (assoc behind :correction-ms 4.5 :invalid-since -5000.0) 1010.0 50.0)))))))

(deftest a-drift-beyond-the-bound-makes-timing-invalid
  (let [states (run -1000.0 30.0)]
    (is (= :out-of-bound (:state (peek states))))
    (is (false? (drift/valid? (peek states))))
    (is (< 990.0 (- (:ppm (peek states))) 1010.0))
    (is (= {:tracking? true :state :out-of-bound :bound-ppm 250.0 :segments 16 :steps 0}
           (select-keys (drift/metrics (peek states)) [:tracking? :state :bound-ppm :segments :steps]))))
  (is (thrown? clojure.lang.ExceptionInfo (drift/new-tracker 1000.0 480))))

(deftest the-correction-never-puts-a-block-after-its-arrival
  (let [tracker (assoc (drift/new-tracker sample-rate 480) :fit {:rate 0.0 :intercept 4.0} :baseline 0.0
                       :history [[0.0 4.0] [500.0 4.0]] :correction-ms 3.0)
        clamped (drift/observe tracker 1000.0 2.0)]
    (is (= 1.9 (:correction-ms clamped)) "the block's delay less 0.1 ms")
    (is (= 1 (:clamps clamped))))
  (testing "a step on a low-latency path never pushes a corrected end past its arrival"
    (let [states (run 0.0 120.0 {:latency-ms 0.3 :extra (fn [_ t] (if (> t 60000.0) -0.25 0.0))})]
      (is (every? #(< (:error %) (- 0.3 (if (> (:s %) 60.0) 0.25 0.0))) states)))))
