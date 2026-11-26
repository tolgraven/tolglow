(ns tolglow.studio.knob-presets-test
  (:require [cljs.test :refer-macros [deftest is]]
            [clojure.string :as str]
            ["react-dom/server" :refer [renderToStaticMarkup]]
            [re-frame.db :as rf-db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [reagent.core :as r]
            [tolglow.studio.knob-presets :as presets]))

(def slots
  [{:slot 0 :state :selected :matches? false :values {:look 0.2 :rate 0.1}
    :knobs [{:knob-id :look :name "Look" :value 0.2 :current 0.5 :text "0.200"}
            {:knob-id :rate :value 0.1 :missing? true :text "0.100"}]}
   {:slot 1 :state "stored" :matches? true :values {:look 0.8}
    :knobs [{:knob-id :look :name "Look" :value 0.8 :current 0.8 :text "0.800"}]}
   {:slot 2 :state :empty}])

(deftest slot-actions-carry-the-shown-contents
  (is (= [[:save "Save" {:op :knob-preset/capture :slot 2 :expected nil}]] (presets/slot-actions (nth slots 2))))
  (is (= [{:op :knob-preset/recall :slot 1 :expected {:look 0.8}}
          {:op :knob-preset/capture :slot 1 :overwrite? true :expected {:look 0.8}}
          {:op :knob-preset/clear :slot 1 :expected {:look 0.8}}]
         (mapv last (presets/slot-actions (nth slots 1)))))
  (is (= :stored (presets/slot-state (nth slots 1))) "wire strings read as keywords")
  (is (= "Look 0.200 · rate 0.100 (removed)" (presets/slot-summary (first slots))))
  (is (= "Empty" (presets/slot-summary (nth slots 2)))))

(deftest the-strip-marks-selected-moved-and-sends-the-owner-command
  (let [db {:show {:epoch 3 :runtime-id "r" :knobs {:knobs [{:id :look}]} :knob-presets {:slots slots}}
            :connection {:connected? true}}
        html (renderToStaticMarkup (r/as-element [presets/panel db]))
        result (do (reset! rf-db/app-db db)
                   (:effects (interceptor/execute [::presets/send {:op :knob-preset/recall :slot 1 :expected {:look 0.8}}]
                                                  (remove #(= :do-fx (:id %)) (registrar/get-handler :event ::presets/send)))))
        [effect request] (first (:fx result))]
    (is (str/includes? html "data-state=\"selected\" data-moved=\"true\""))
    (is (str/includes? html "selected · moved"))
    (is (str/includes? html "aria-label=\"Recall Look 2\""))
    (is (= 3 (count (re-seq #"<li" html))))
    (is (= :studio/send-command effect))
    (is (= [:knob-preset/recall {:slot 1 :expected {:look 0.8}} 3]
           [(:op request) (:fields request) (:epoch request)]))
    (is (nil? (presets/panel {:show {:knobs {:knobs []} :knob-presets {:slots [{:slot 0 :state :empty}]}}}))
        "hidden until there is a knob or a saved look")))
