(ns tolglow.studio.push-display-test
  (:require [cljs.test :refer-macros [deftest is]]
            [re-frame.db :as db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [tolglow.studio.push-display :as display]
            [tolglow.studio.parameter-widgets :as widgets]))

(deftest passive-display-snapshot-is-bounded-and-emits-no-effects
  (reset! db/app-db {:unrelated :preserved})
  (let [view {:parameters (repeat 20 {:name "Level" :value 0.5}) :cells (range 16) :labels (range 10)
              :cue {:name "Ember"} :effect {:envelope {:attack {:beats 0.5}}} :ignored "not display data"}
        result (:effects (interceptor/execute [:push-display/update view]
                           (remove #(= :do-fx (:id %)) (registrar/get-handler :event :push-display/update))))]
    (is (= #{:db} (set (keys result))))
    (is (= :preserved (get-in result [:db :unrelated])))
    (is (= [8 8 8] (mapv #(count (get-in result [:db :push-display %])) [:parameters :cells :labels])))
    (is (nil? (get-in result [:db :push-display :ignored])))
    (is (= {:attack {:beats 0.5}} (get-in result [:db :push-display :effect :envelope])))))

(deftest typed-values-share-labels-with-web-controls
  (is (= "Triangle" (widgets/value-label {:choices [{:value 1 :label "Triangle"}]} 1)))
  (is (= "On" (widgets/value-label {:type :boolean} true)))
  (is (= "0.5 · -1" (widgets/value-label {:type :vector} [0.5 -1])))
  (is (= "0.5 beats" (widgets/duration-label {:beats 0.5})))
  (is (= "rgb(255,0,128)" (widgets/color-css [2 -1 0.5])))
  (is (= 0.75 (widgets/proportion {:bounds [-1 1]} 0.5)))
  (is (= 1 (widgets/proportion {:bounds [0 1]} 2)))
  (is (nil? (widgets/proportion {:bounds [1 1]} 1))))

(deftest explicit-audio-mode-keeps-the-actual-capture-projection
  (let [waveform {:bins [{:min -0.5 :max 0.4 :rms 0.2 :bands {:low 0.8}}] :sample-end 512}
        view (display/normalize-view {:display-mode "audio" :audio-waveform waveform
                                      :parameters [{:id :level :value 0.7}]})]
    (is (= "audio" (:display-mode view)))
    (is (= waveform (:audio-waveform view)))
    (is (= 1 (count (:parameters view))))
    (is (nil? (:audio-waveform (display/normalize-view {}))))))

(deftest traces-require-real-finite-samples
  (is (nil? (widgets/sample-points nil)))
  (is (nil? (widgets/sample-points [0 js/NaN 1])))
  (is (= "0,16 100,16" (widgets/sample-points [1 1])))
  (is (= "0,28 50,4 100,28" (widgets/sample-points [{:value 0} {:value 1} {:value 0}]))))

(deftest rotary-controls-preserve-range-units-discrete-steps-and-bipolar-zero
  (let [bipolar {:type :number :bounds [-180 180] :unit :degrees}
        model (widgets/dial-model bipolar 90)]
    (is (= 0.75 (:amount model)))
    (is (= 0.5 (:origin model)))
    (is (:bipolar? model))
    (is (= "degrees" (widgets/unit-label bipolar)))
    (is (= [-180 180] (:bounds model)))
    (is (= 0.5 (widgets/pointer-proportion 0 -1)))
    (is (= -180 (widgets/keyboard-value bipolar 90 "Home")))
    (is (= 180 (widgets/keyboard-value bipolar 90 "End")))
    (is (= 180 (widgets/keyboard-value bipolar 179 "ArrowUp")))
    (is (nil? (widgets/keyboard-value bipolar 90 "Enter"))))
  (is (= 3 (widgets/normalized-value {:type :integer :bounds [0 8]} 0.4)))
  (is (= 9 (count (:ticks (widgets/dial-model {:type :integer :bounds [0 8]} 3)))))
  (is (nil? (widgets/dial-model {:bounds [1 1]} 1)))
  (is (nil? (widgets/dial-model {:bounds [0 1]} js/NaN))))

(deftest waveform-choices-take-precedence-over-numeric-storage-type
  (is (= :choice (widgets/parameter-kind {:type :number :choices [{:value 0 :label "Sine"}]})))
  (is (= :integer (widgets/parameter-kind {:type "integer"})))
  (is (= :color (widgets/parameter-kind {:type :color}))))

(deftest captured-audio-is-bounded-and-color-comes-from-real-band-energy
  (let [bin {:min -0.5 :max 0.75 :rms 0.3 :bands {:low 1 :mid 0 :high 0}}]
    (is (= [bin] (widgets/audio-bins {:bins [bin]})))
    (is (= "rgb(68,145,235)" (widgets/audio-bin-color (:bands bin))))
    (is (= "rgb(245,163,70)" (widgets/audio-bin-color {:mid 1}))))
  (is (nil? (widgets/audio-bins {:bins []})))
  (is (nil? (widgets/audio-bins {:bins [{:min 0.8 :max 0.2 :rms 0.5}]})))
  (is (nil? (widgets/audio-bins {:bins [{:min -1 :max js/NaN :rms 0.5}]})))
  (is (nil? (widgets/audio-bins {:bins (vec (repeat 513 {:min 0 :max 0 :rms 0}))})))
  (is (= 1 (count (widgets/audio-bins {:bins [{:min 0 :max 0 :rms 0}]})))))
