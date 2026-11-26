(ns tolglow.studio.audio-lfo-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [clojure.string :as str]
            [tolglow.studio.audio-lfo :as audio-lfo]))

(deftest the-form-reads-the-projected-configuration
  (let [config {:band "low-mid" :extract "cycle" :window {:bars 2} :morph {:bars 0.5}
                :smoothing 0.25 :on-silence "release" :name "Guitar"}]
    (is (= {:band "low-mid" :extract "cycle" :window "bars-2" :morph "bars-1/2" :smoothing "0.25" :on-silence "release"}
           (audio-lfo/form-of config)))
    (testing "an unlisted duration falls back to the first choice"
      (is (= "beats-1" (audio-lfo/duration-key audio-lfo/windows {:bars 3}))))))

(deftest a-form-becomes-the-same-command-the-repl-sends
  (is (= {:fields {:lfo-id :guitar
                   :config {:band :low-mid :extract :cycle :window {:bars 2} :morph {:bars 0.5}
                            :smoothing 0.25 :on-silence :release}}}
         (audio-lfo/configure-payload "guitar" {:band "low-mid" :extract "cycle" :window "bars-2" :morph "bars-1/2"
                                                :smoothing "0.25" :on-silence "release"})))
  (is (re-find #"Smoothing" (:error (audio-lfo/configure-payload "guitar" {:smoothing "2"})))))

(deftest window-labels-read-as-music
  (is (= "1 bar" (audio-lfo/window-label {:bars 1})))
  (is (= "2 beats" (audio-lfo/window-label {"beats" 2})))
  (is (= "—" (audio-lfo/window-label nil))))

(deftest new-ids-come-from-the-name-and-stay-unique
  (is (= "bass-shape" (audio-lfo/new-lfo-id "Bass Shape" [])))
  (is (= "bass-shape-2" (audio-lfo/new-lfo-id "Bass shape" [{:id "bass-shape"}])))
  (is (= "kick-band" (audio-lfo/new-lfo-id "kick/band" []))))

(deftest traces-scale-into-the-chart-box
  (is (nil? (audio-lfo/trace-points [0.5] false)) "one point is no trace")
  (is (= "0.00,38.00 50.00,20.00 100.00,2.00" (audio-lfo/trace-points [0.0 0.5 1.0] false)))
  (testing "signed source samples centre on zero and fill the height at their peak"
    (let [points (audio-lfo/trace-points [0.0 0.25 -0.25] true)]
      (is (= ["0.00,20.00" "50.00,2.00" "100.00,38.00"] (str/split points #" ")))))
  (is (= "0.00,38.00 100.00,2.00" (audio-lfo/trace-points [0.0 js/NaN 1.0] false)) "non-finite samples are dropped"))
