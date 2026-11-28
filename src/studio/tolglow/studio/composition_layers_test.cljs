(ns tolglow.studio.composition-layers-test
  (:require [cljs.test :refer-macros [deftest is]]
            [tolglow.studio.composition-layers :as layers]
            [tolglow.studio.events-test :refer [base transition]]
            [tolglow.transit :as transit]))

(deftest composition-transforms-round-trip-through-the-same-re-frame-save-as-slot-controls
  (let [update-db (fn [db e] (:db (transition db e)))
        initial (assoc-in base [:ui :composition] {:id "t" :mode "macro" :slots [{:cue-id "ember"}]})
        db (reduce update-db initial [[:studio/ui-set [:composition :opacity] 0.4]
                                      [:composition-layer/add 0 :beat-hue]
                                      [:composition-layer/add 0 :ratio]
                                      [:composition-layer/value 0 1 [:value :args 1] 8]
                                      [:composition-layer/move 0 1 0]
                                      [:composition-layer/include-previous 0 true]])
        body (get-in (transition db [:studio/composition-save true]) [:studio/http :body])
        program (get-in body [:slots 0 :transforms])]
    (is (= 0.4 (:opacity body)))
    (is (= :previous (get-in body [:slots 0 :targets])))
    (is (= [:ratio :beat-hue] (mapv (comp first layers/preset-for) program)))
    (is (= [1 8] (get-in program [0 :value :args])))
    (doseq [denominator [0 -1 0.25]]
      (is (= db (update-db db [:composition-layer/value 0 0 [:value :args 1] denominator]))))
    (is (= 1 (count (get-in (update-db db [:composition-layer/remove 0 0])
                            [:ui :composition :slots 0 :transforms]))))))

(deftest reopening-preserves-the-exact-transit-program
  (let [ratio (transit/->BeatFraction "1" "3")
        steps [{:attribute :beat-ratio :value ratio}]
        typed {:id :t :slots [{:cue-id :ember :targets :previous :transforms steps}]}
        shown {:id "t" :running? true :slots [{:cue-id "ember" :targets "previous"
                                              :transforms [{:attribute "beat-ratio" :value (/ 1 3)}]}]}
        db (assoc-in base [:server-projection :view :compositions] [typed])
        opened (:db (transition db [:studio/composition-open shown]))
        saved (get-in (transition opened [:studio/composition-save false]) [:studio/http :body])]
    (is (not (contains? saved :running?)))
    (is (= :previous (get-in saved [:slots 0 :targets])))
    (is (= steps (get-in saved [:slots 0 :transforms])))))

(deftest colour-filter-presets-save-editable-settings-for-the-afterglow-operators
  (let [update-db (fn [db e] (:db (transition db e)))
        initial (assoc-in base [:ui :composition] {:id "t" :mode "macro" :slots [{:cue-id "ember"}]})
        db (reduce update-db initial [[:composition-layer/add 0 :guard]
                                      [:composition-layer/add 0 :grade]
                                      [:composition-layer/value 0 0 [:value :args 1 1] 0.75]
                                      [:composition-layer/value 0 0 [:value :args 1 2] 0.08]
                                      [:composition-layer/value 0 1 [:value :args 1 1] 1]])
        program (get-in (transition db [:studio/composition-save true]) [:studio/http :body :slots 0 :transforms])]
    (is (= [:guard :grade] (mapv (comp first layers/preset-for) program)))
    (is (= [:color-guard :color-grade] (mapv (comp :op :value) program)))
    (is (= [[0.55 0.75 0.08] [0.5 1 0]] (mapv #(get-in % [:value :args 1]) program)))
    (is (= :previous (get-in program [0 :value :args 0 :op])))))
