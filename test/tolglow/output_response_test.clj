(ns tolglow.output-response-test
  (:require [afterglow.control.model :as model]
            [afterglow.render.emission :as light]
            [afterglow.render.frame :as frame]
            [clojure.test :refer [deftest is]]
            [tolglow.output-response :as response]))

(def fixtures [{:id :front :groups [:wash :front] :universe 0 :address 1 :channels {:dimmer 1 :red 2 :blue 3 :strobe 4}}
               {:id :back :groups [:wash] :universe 0 :address 5 :channels {:dimmer 1}}])
(def patch (frame/compile-patch fixtures))
(defn source [front back]
  (frame/resolve-frame patch
    [{:target-id :front :attribute :red :value front}
     {:target-id :front :attribute :strobe :value 0.8}
     {:target-id :back :attribute :dimmer :value back}] {:include-values? false :compact-values? true}))
(def empty-frame (frame/resolve-frame patch [] {}))
(def show (model/new-show {}))
(def opts {:enabled? true :global {:attack {:beats 1/2} :release {:beats 1/2}}
           :groups {:front {:release {:bars 1}}}})
(defn tick [previous opts input ms beat & [model]]
  (let [m (-> (or model show) (assoc :time-ms ms)
              (assoc-in [:transport :beat] beat)
              (assoc-in [:settings :output-response] (response/settings opts)))]
    (response/advance (:state previous) m patch input false (or (:master m) 1.0) nil)))
(defn value [result fixture attr]
  (get (light/emitted-levels (get-in patch [:by-id fixture]) (:frame result)) attr))
(defn near? [a b] (< (abs (- a b)) 0.008))

(deftest a-rendered-change-and-its-release-follow-the-selected-musical-clock
  (let [zero (tick nil opts empty-frame 0 0)
        rise (tick zero opts (source 1.0 1.0) 250 1/2)
        removed (tick rise opts empty-frame 500 1)
        hold (tick removed opts empty-frame 1000 1)
        e (Math/exp -1.0)]
    (is (near? (- 1 e) (value rise :front :red)))
    (is (near? (* (- 1 e) (Math/exp -0.125)) (value removed :front :red)))
    (is (near? (* (- 1 e) e) (value removed :back :dimmer)))
    (is (= (get-in removed [:state :levels]) (get-in hold [:state :levels])))
    (is (zero? (bit-and 255 (aget ^bytes (get-in removed [:frame :universes 0]) 3))))
    (is (< 0.0 (value removed :front :red)))))

(deftest frame-rate-independent-and-operator-capped
  (let [lit (source 1.0 1.0)
        zero (tick nil opts empty-frame 0 0)
        one (tick zero opts lit 1000 1)
        forty (reduce #(tick %1 opts lit (* %2 25) (/ %2 40)) zero (range 1 41))
        m (assoc-in (assoc show :time-ms 1000) [:settings :output-response] opts)
        blackout (response/advance (:state forty) (assoc m :blackout? true) patch empty-frame false)
        low (tick forty opts empty-frame 1100 1.1 (assoc show :master 0.125))]
    (is (= (value one :front :red) (value forty :front :red)))
    (is (near? 0.125 (value low :front :red)))
    (is (identical? empty-frame (:frame blackout)))
    (is (nil? (:state blackout)))
    (is (identical? lit (:frame (response/advance nil show patch lit false))))))

(deftest cue-parameters-and-variables-change-the-actual-response
  (let [opts {:enabled? true :groups {:front {:attack {:beats {:cue-id :control :param :rise :default 1}}
                                               :release {:ms {:variable [:feel :fall]}}}}}
        active (assoc show :instances {"knob" {:id "knob" :cue-id :control :phase :running :order 1 :params {:rise 2.0}}})
        initial (tick nil opts empty-frame 0 0 active)
        rise (tick initial opts (source 1.0 1.0) 500 1 active)
        immediate (tick rise opts (source 1.0 1.0) 1000 2 (assoc-in active [:instances "knob" :params :rise] 0))
        fall (tick immediate opts empty-frame 1500 3 (assoc-in active [:variables :feel :fall] 500))]
    (is (near? (- 1 (Math/exp -0.5)) (value rise :front :red)))
    (is (= 1.0 (value rise :back :dimmer)))
    (is (= 1.0 (value immediate :front :red)))
    (is (near? (Math/exp -1.0) (value fall :front :red)))))

(deftest a-color-change-at-constant-dimmer-fades-through-both-colors
  (let [options {:enabled? true :global {:attack {:ms 250} :release {:ms 250}}}
        red (source 1.0 0.0)
        blue (frame/resolve-frame patch [{:target-id :front :attribute :blue :value 1.0}] {})
        m (-> show (assoc :time-ms 0) (assoc-in [:settings :output-response] options))
        seeded (response/advance nil m patch red false 1 red)
        mixed (tick seeded options blue 250 1)]
    (is (near? (Math/exp -1.0) (value mixed :front :red)))
    (is (near? (- 1 (Math/exp -1.0)) (value mixed :front :blue)))
    (is (= 1.0 (:dimmer (light/levels (get-in patch [:by-id :front]) (:frame mixed)))))))

(deftest reject-invalid-bindings-and-duration-units
  (doseq [x [{:seconds 1} {:beats -1} {:ms Double/NaN} {:beats {:variable []}}
             {:beats {:cue-id :ctl}} {:ms {:variable [:bad] :default -2}}]]
    (is (thrown? Exception (response/settings {:enabled? true :global {:attack x}})))))
