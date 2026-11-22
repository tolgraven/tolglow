(ns tolglow.autopilot.signal-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.autopilot.signal :as signal]
            [tolglow.signals :as waves]))

(def model {:time-ms 4500 :transport {:beat 9 :bpm 120}
            :variables {:audio/energy 0.75}
            :instances {"one" {:id "one" :phase :running :order 1 :cue-id :wash :params {:boost 1.2}}
                        "two" {:id "two" :phase :running :order 3 :cue-id :wash :params {:boost 1.5}
                               :started-at-ms 4000 :started-at-beat 8}}
            :settings {:autopilot {:signals {:bass {:variable [:audio/energy]}
                                            :four {:waveform :saw :period {:beats 4}}}}}})
(defn- near? [a b] (< (abs (- a b)) 1e-8))

(deftest named-show-variables-cue-params-and-numeric-stacks
  (let [gain {:cue-id :wash :param :boost :default 1}
        stack {:op :mix :args [{:signal :four} gain {:signal :bass}]}]
    (is (signal/valid? stack))
    (is (= 1.5 (signal/value model gain)))
    (is (= 1.0 (signal/value (dissoc model :instances) gain)))
    (is (near? (+ (* 0.25 0.25) (* 0.75 1.5)) (signal/value model stack)))
    (is (= -3.0 (signal/value model {:variable [:missing] :default -3})))
    (is (= 1.1 (signal/value model {:op :divide :args [1 0] :default 1.1})))
    (is (signal/valid-bank? (get-in model [:settings :autopilot :signals])))
    (is (not (signal/valid-bank? {:a {:signal :b} :b {:signal :a}})))
    (is (not (signal/valid-bank? {:a {:signal :missing}})))
    (is (not (signal/valid? {:waveform :saw :period {:beats 0}})))))

(deftest waveform-phase-uses-global-or-private-launch-coordinate
  (let [wave {:waveform :saw :period {:beats 3}}
        opts {:start-clock {:beat 8 :time-ms 4000} :instance-id "two"}
        local (assoc wave :phase :local)
        paused (assoc-in model [:instances "two" :system]
                         {:paused? true :paused-clock {:time-ms 4250 :beat 8.5}})]
    (is (near? 0 (signal/value model wave opts)))
    (is (near? 1/3 (signal/value model local opts)))
    (is (near? 1/6 (signal/value paused local opts)))
    (is (near? 1/3 (signal/value model local (dissoc opts :instance-id))))
    (is (near? 0.25 (signal/value model {:signal :four} opts)))))

(deftest registered-waveform-and-dynamic-depth-share-the-render-template
  (let [source {:id :pair :version 1 :name "Offset" :range [-1 1]
                :params {} :expression {:op :subtract
                                        :args [{:op :param :key :signal/phase} {:op :const :value 1.0}]}}
        wave {:waveform :pair :period {:ms 9000} :range [0 {:variable [:audio/energy]}]}
        rig (assoc model :waveforms (waves/publish (waves/new-registry) source))
        prepared (signal/prepare nil (:waveforms rig))]
    (is (signal/valid? wave))
    (is (near? 0.1875 (signal/value rig wave)))
    (is (near? 0.1875 (signal/value rig wave {:prepared prepared})))
    (is (identical? prepared (signal/prepare prepared (:waveforms rig))))
    (is (= 2.0 (signal/value rig {:waveform :pulse :period {:beats 4} :range [0 2]
                                  :params {:duty {:signal :bass}}})))))
