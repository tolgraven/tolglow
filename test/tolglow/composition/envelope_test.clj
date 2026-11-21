(ns tolglow.composition.envelope-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.composition.envelope :as envelope]))

(deftest musical-envelopes-keep-their-clock-domain
  (let [spec {:attack {:bars 1} :release {:beats 1/2}}
        meter {:beats-per-bar 7/2 :bars-per-phrase 4}
        state (envelope/initial-state {:time-ms 100 :beat 8 :meter meter})
        normalized (envelope/normalize spec meter)]
    (is (= :beats (:timebase normalized)))
    (is (= 7/2 (:attack-length normalized)))
    (is (not (contains? normalized :attack-ms)))
    (is (= 0.5 (:value (envelope/sample spec state {:time-ms 9999 :beat 39/4}))))
    (let [released (envelope/release spec state {:time-ms 9999 :beat 39/4})]
      (is (= 0.25 (:value (envelope/sample spec released {:time-ms 10000 :beat 10}))))
      (is (:done? (envelope/sample spec released {:time-ms 10001 :beat 41/4}))))))

(deftest ambiguous-envelope-stage-is-rejected
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Choose one duration"
                       (envelope/compile-envelope {:attack {:beats 1} :attack-ms 500}))))

(deftest early-release-is-continuous-and-idempotent
  (let [spec {:attack-ms 100 :decay-ms 100 :sustain 0.4 :release-ms 200}
        initial (envelope/initial-state 1000)
        released (envelope/release spec initial 1050)]
    (is (= 0.5 (:value (envelope/sample spec initial 1050))))
    (is (= 0.5 (:value (envelope/sample spec released 1050))))
    (is (= 0.25 (:value (envelope/sample spec released 1150))))
    (is (= {:value 0.0 :done? true} (envelope/sample spec released 1250)))
    (is (= released (envelope/release spec released 1100)))))

(deftest adsr-boundaries-and-zero-length-stages
  (let [spec {:attack-ms 100 :decay-ms 100 :sustain 0.4}
        state (envelope/initial-state 0)]
    (is (= 1.0 (:value (envelope/sample spec state 100))))
    (is (= 0.7 (:value (envelope/sample spec state 150))))
    (is (= 0.4 (:value (envelope/sample spec state 200)))))
  (is (= 1.0 (:value (envelope/sample {} (envelope/initial-state 0) 0))))
  (is (:done? (envelope/sample {} (envelope/release {} (envelope/initial-state 0) 5) 5))))

(deftest retrigger-can-continue-from-current-response
  (let [spec {:attack-ms 100 :release-ms 100}
        state (envelope/initial-state 0)]
    (is (= 0.5 (:initial-level (envelope/retrigger spec state 50 :continue))))
    (is (= 0.0 (:initial-level (envelope/retrigger spec state 50 :restart))))))

(deftest musical-attack-and-release-freeze-and-capture-meter
  (let [spec {:attack {:bars 1} :decay {:beats 1/2} :sustain 0.4 :release {:phrases 1/2}}
        start (envelope/initial-state {:time-ms 0 :beat 8 :meter {:beats-per-bar 3 :bars-per-phrase 2}})
        halfway {:time-ms 750 :beat 9.5 :bpm 240}
        released (envelope/release spec start halfway)]
    (is (= 0.5 (:value (envelope/sample spec start halfway))))
    (is (= 0.5 (:value (envelope/sample spec start {:time-ms 50000 :beat 9.5 :playing? false}))))
    (is (= 1.0 (:value (envelope/sample spec start {:beat 11 :meter {:beats-per-bar 7}}))))
    (is (= 0.4 (:value (envelope/sample spec start {:beat 11.5}))))
    (is (= 0.25 (:value (envelope/sample spec released {:beat 11}))))
    (is (= {:value 0.0 :done? true} (envelope/sample spec released {:beat 12.5})))
    (is (= released (envelope/release spec released {:beat 11})))))

(deftest canonical-ms-and-invalid-mixed-domains
  (is (= 0.5 (:value (envelope/sample {:attack {:ms 100}} (envelope/initial-state 0) 50))))
  (is (thrown? clojure.lang.ExceptionInfo
               (envelope/compile-envelope {:attack {:beats 1} :release {:ms 500}})))
  (is (thrown? clojure.lang.ExceptionInfo
               (envelope/sample {:attack {:beats 1}} (envelope/initial-state 0) 500))))
