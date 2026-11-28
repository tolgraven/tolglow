(ns tolglow.audio.drift-test
  "The capture boundary's delay-locked drift tracker (#912), driven with
  explicit positions and arrival times."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.audio.drift :as drift]))

(def ^:private sample-rate 48000.0)

(def ^:private block-ms 10.0)

(defn- run
  "Feed blocks of a device whose clock runs ppm against the host, delivered
  latency-ms (plus seeded jitter up to jitter-ms) after their last sample.
  Returns every tracker state with the real error of each block's corrected
  end timestamp."
  [ppm seconds & [{:keys [latency-ms jitter-ms seed] :or {latency-ms 3.0 jitter-ms 0.0 seed 1}}]]
  (let [random (java.util.Random. seed)
        rate (+ 1.0 (* 1.0e-6 ppm))]
    (loop [block 1 tracker (drift/new-tracker sample-rate) out []]
      (if (> (* block block-ms) (* 1000.0 seconds))
        out
        (let [position (* block block-ms)
              real-end (/ position rate)
              arrival (+ real-end latency-ms (* jitter-ms (.nextDouble random)))
              tracker (drift/observe tracker position (- arrival position))]
          (recur (inc block) tracker
                 (conj out (assoc tracker :position position
                                  :error (- (+ position (:correction-ms tracker)) real-end)))))))))

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

(deftest the-correction-never-steps-more-than-a-quarter-sample-per-block
  (let [quarter (/ 250.0 sample-rate)
        states (run 200.0 30.0)
        steps (map (fn [a b] (Math/abs (- (:correction-ms b) (:correction-ms a)))) states (rest states))]
    (is (every? #(<= % (+ quarter 1.0e-12)) steps))
    (is (some #(< (- quarter 1.0e-9) %) steps) "catching up after the first estimate moves at the limit")))

(deftest delivery-jitter-only-raises-the-floor
  (let [states (run -50.0 300.0 {:jitter-ms 4.0 :seed 912})
        settled (drop 3000 states)]
    (is (every? #(= :locked (:state %)) settled))
    (is (< (Math/abs (+ 50.0 (:ppm (peek states)))) 10.0) (str (:ppm (peek states))))
    (is (every? #(< (Math/abs (:error %)) 1.0) settled) "within 1 ms of the real sample times")
    (is (= (map :correction-ms states) (map :correction-ms (run -50.0 300.0 {:jitter-ms 4.0 :seed 912})))
        "deterministic for the same arrivals")))

(deftest a-drift-beyond-the-bound-makes-timing-invalid
  (let [states (run -1000.0 30.0)]
    (is (= :out-of-bound (:state (peek states))))
    (is (false? (drift/valid? (peek states))))
    (is (< 990.0 (- (:ppm (peek states))) 1010.0))
    (is (= {:tracking? true :state :out-of-bound :bound-ppm 250.0 :segments 16}
           (select-keys (drift/metrics (peek states)) [:tracking? :state :bound-ppm :segments]))))
  (testing "a correction too far behind its target is invalid until it catches up"
    (let [tracker (assoc (drift/new-tracker sample-rate) :fit {:rate 0.0 :intercept 5.0 :baseline 0.0})
          behind (drift/observe tracker 1000.0 5.0)]
      (is (= :behind (:state behind)))
      (is (false? (drift/valid? behind)))))
  (is (thrown? clojure.lang.ExceptionInfo (drift/new-tracker 1000.0))))
