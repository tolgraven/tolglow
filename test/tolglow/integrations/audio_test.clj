(ns tolglow.integrations.audio-test
  (:require [clojure.test :refer :all]
            [tolglow.integrations.audio :as audio]))

(def opts {:sample-rate 48000.0 :frequency-range [80.0 2000.0] :bounds [-10.0 10.0] :amplitude 0.4})

(defn close? [a b tolerance] (< (Math/abs (- a b)) tolerance))

(defn sine [frequency amplitude n]
  (double-array (map #(* amplitude (Math/sin (/ (* 2 Math/PI frequency %) 48000.0))) (range n))))

(deftest logarithmic-carrier-mapping-round-trips-and-clamps
  (doseq [value [-10 -5 0 5 10]]
    (is (close? value (audio/frequency->value (audio/value->frequency value opts) opts) 1.0e-9)))
  (is (= 80.0 (audio/value->frequency -100 opts)))
  (is (= 2000.0 (audio/value->frequency 100 opts)))
  (is (thrown? clojure.lang.ExceptionInfo (audio/frequency->value 0 opts)))
  (is (thrown? clojure.lang.ExceptionInfo (audio/value->frequency Double/NaN opts))))

(deftest sine-carrier-retains-phase-between-owned-blocks
  (let [a (audio/sine-block opts {} 0 128)
        before (vec (:samples a))
        b (audio/sine-block opts (:state a) 0 128)
        combined (audio/sine-block opts {} 0 256)]
    (is (= (vec (:samples combined)) (into before (:samples b))))
    (is (= before (vec (:samples a))))
    (is (not (identical? (:samples a) (:samples b))))
    (is (every? #(<= (Math/abs %) 0.4) (:samples b)))))

(deftest actual-pitch-analysis-recovers-an-isolated-sine-sweep
  (doseq [frequency [110.0 220.0 440.0 880.0]]
    (let [samples (sine frequency 0.4 4096)
          original (vec samples)
          result (audio/analyze-block samples (assoc opts :reference-frequency frequency))]
      (is (close? frequency (:pitch-hz result) 1.0))
      (is (> (:pitch-confidence result) 0.95))
      (is (close? (/ 0.4 (Math/sqrt 2)) (:rms result) 0.005))
      (is (close? frequency (:spectral-centroid-hz result) 20.0))
      (is (< (:spectral-residual result) 0.001))
      (is (= original (vec samples)))
      (is (close? 85.333333 (:window-latency-ms result) 0.001)))))

(deftest texture-residual-distinguishes-gain-from-added-harmonics
  (let [clean (audio/analyze-block (sine 440 0.3 4096) (assoc opts :reference-frequency 440))
        loud (audio/analyze-block (sine 440 0.6 4096) (assoc opts :reference-frequency 440))
        distorted (double-array (map + (sine 440 0.4 4096) (sine 880 0.2 4096)))
        result (audio/analyze-block distorted (assoc opts :reference-frequency 440 :previous-spectrum (:spectrum clean)))]
    (is (close? (* 2 (:rms clean)) (:rms loud) 1.0e-9))
    (is (close? (:spectral-residual clean) (:spectral-residual loud) 1.0e-9))
    (is (> (:spectral-residual result) 0.15))
    (is (> (:spectral-centroid-hz result) (:spectral-centroid-hz clean)))
    (is (> (:spectral-flux result) 0))))

(deftest silence-and-invalid-samples-do-not-produce-pitch-jumps
  (let [result (audio/analyze-block (double-array 2048) opts)]
    (is (= 0.0 (:rms result)))
    (is (nil? (:pitch-hz result)))
    (is (nil? (:value result)))
    (is (= 0.0 (:pitch-confidence result))))
  (is (thrown? clojure.lang.ExceptionInfo (audio/analyze-block (repeat 64 Double/NaN) opts))))

(def target {:show-id :main :epoch 1 :instance-id "1:old" :param :level})

(def envelope {:show-id :main :epoch 1 :command-id "audio/1"})

(def loud {:rms 0.2 :pitch-confidence 0.99 :value 0.5})

(def silent {:rms 0.0 :pitch-confidence 0.0 :value nil})

(deftest tails-retain-delayed-pulses-with-exact-instance-ownership
  (let [tail (audio/new-tail target 0 {:max-tail-ms 1000 :silence-hold-ms 200})
        a (audio/advance-tail tail loud 0 envelope)
        b (audio/advance-tail (:tail a) silent 100 envelope)
        echo (audio/advance-tail (:tail b) loud 250 envelope)
        quiet (audio/advance-tail (:tail echo) silent 500 envelope)
        ended (audio/advance-tail (:tail quiet) silent 701 envelope)]
    (is (= "1:old" (-> a :commands first :instance-id)))
    (is (:active? echo))
    (is (nil? (get-in echo [:tail :silent-since])))
    (is (= :silence (:reason ended)))
    (is (empty? (:commands ended)))
    (is (= :max-duration (:reason (audio/advance-tail tail loud 1000 envelope))))
    (is (= :stale-epoch (:reason (audio/advance-tail tail loud 1 (assoc envelope :epoch 2)))))
    (is (empty? (:commands (audio/advance-tail tail (assoc loud :pitch-confidence 0.1) 1 envelope))))))
