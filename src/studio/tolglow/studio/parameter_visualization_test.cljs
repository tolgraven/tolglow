(ns tolglow.studio.parameter-visualization-test
  (:require [cljs.test :refer-macros [deftest is]]
            [tolglow.studio.parameter-visualization :as visual]
            [tolglow.studio.push-display :as display]))

(deftest spatial-defaults-follow-the-value-domain
  (is (= :spatial (visual/selected-mode {:type "vector" :size 3} nil)))
  (is (= :orientation (visual/spatial-kind {:type :vector :size 3 :semantic/type "orientation"})))
  (is (= :spatial (visual/selected-mode {:type "color"} "auto")))
  (is (= :history (visual/selected-mode {:type :number :visualization :history} :auto)))
  (is (= :controls (visual/selected-mode {:type :vector :size 4} :auto)))
  (is (= :controls (visual/selected-mode {:type :enum} :history)))
  (is (nil? (visual/components {:type :vector} [0 js/NaN 0])))
  (is (nil? (visual/components {:type :color} nil)))
  (is (= [0 0.5 1] (visual/components {:type :color} [0 0.5 1]))))

(defn history-view [id n v]
  {:display-mode "parameters" :runtime-id "one" :instance-id id :epoch 1 :definition-version 3
   :visualization-clock {:time-ms (* 250 n) :beat (/ n 2)} :history-beats 1 :parameter-view "history"
   :parameters [{:id "point" :schema {:type "vector" :size 3 :bounds [-1 1]}
                 :base-value [0 0 0] :effective-value v}]})

(deftest history-is-owned-by-the-visible-bank-and-uses-real-component-samples
  (let [raw (history-view "a" 0 [0 0 0])
        seed (display/observe-view nil raw)
        view (reduce display/observe-view seed
                     (map #(history-view "a" % [0.5 0.2 1]) (range 1 10)))
        history (get-in view [:parameter-observations "point"])]
    (is (= 10 (count history)))
    (is (= [0.5 0.2 1] (:values (peek history))))
    (is (= 3 (count (:paths (visual/chart-paths history 2 {:type :vector :bounds [-1 1]})))))
    (is (not= (:paths (visual/chart-paths history 0.5 {})) (:paths (visual/chart-paths history 16 {}))))
    (is (= 1 (count (get-in (display/observe-view view (history-view "b" 12 [1 0 0])) [:parameter-observations "point"]))))
    (is (nil? (:parameter-observations (display/observe-view view (assoc raw :display-mode :audio)))))
    (is (nil? (:parameter-observations (display/observe-view view (assoc raw :target-picker {:open? true})))))
    (is (map? (get-in (display/observe-view view (assoc raw :parameter-view :auto)) [:parameter-observations "point"])))
    (is (empty? (:parameter-observations (display/observe-view view (assoc raw :parameters [])))))))

(deftest sampling-is-bounded-and-clock-rewinds-reset-the-trace
  (let [feed #(visual/append-sample %1 {:time-ms (double %2) :beat (* 0.002 %2)} [1])
        result (reduce feed [] (range 0 60000 10))
        held (visual/append-sample result {:time-ms 60004 :beat (:beat (peek result))} [0])]
    (is (= 256 (count result)))
    (is (= 100 (- (:t (peek result)) (:t (nth result 254)))))
    (is (= 256 (count held)))
    (is (= [0] (:values (peek held))))
    (is (= result (visual/append-sample result {:beat 100} [3])))
    (is (= 1 (count (feed held 0))))))

(deftest missing-components-create-gaps-at-their-own-timestamps
  (let [samples [{:beat 0 :values [0]} {:beat 1 :values nil} {:beat 2 :values [1]}]
        trace (visual/chart-paths samples 2 {:type :number :bounds [0 1]})]
    (is (= ["M0,41M100,3"] (:paths trace)))
    (is (nil? (visual/chart-paths [{:beat 2 :values nil}] 4 {})))))

(deftest zoom-interpolates-only-when-observations-bracket-the-left-edge
  (let [schema {:type :number :bounds [0 1]}
        data [{:beat 0 :values [0]} {:beat 2 :values [1]}]]
    (is (= ["M0,22L100,3"] (:paths (visual/chart-paths data 1 schema))))
    (is (= ["M100,3"] (:paths (visual/chart-paths (assoc-in data [0 :values] nil) 1 schema))))))

(deftest spatial-frames-interpolate-between-adaptive-samples
  (let [clock #(hash-map :time-ms % :sample-period-ms 200)
        start (visual/spatial-keyframe nil (clock 0) [170 0 0])
        second (visual/spatial-keyframe start (clock 200) [-170 20 60])
        schema {:type :vector :size 3 :semantic/type :orientation}]
    (is (identical? second (visual/spatial-keyframe second (clock 260) [23 23 23])))
    (is (= [180 10 30] (visual/spatial-value schema second 300)))
    (is (= [js/Math.PI 0 0]
           (visual/spatial-value (assoc schema :unit :radians)
                                 (assoc second :from [3 0 0] :to [-3 0 0]) 300)))
    (is (= [-170 20 60] (visual/spatial-value schema second 400)))
    (is (nil? (:to (visual/spatial-keyframe second (clock 270) nil))))
    (is (= [-20 0 0] (:from (visual/spatial-keyframe second (clock 0) [-20 0 0]))))))

(deftest schema-initiated-history-can-be-closed-and-reopened
  (let [schema {:type :number :visualization :history}
        closed (visual/history-toggle schema :auto)]
    (is (= :controls (visual/selected-mode schema closed)))
    (is (= :history (visual/history-toggle schema closed)))))
