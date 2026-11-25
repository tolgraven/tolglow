(ns tolglow.studio.param-visualization-test
  (:require [cljs.test :refer-macros [deftest is]]
            [tolglow.studio.param-visualization :as visual]
            [tolglow.studio.push-display :as display]
            [tolglow.studio.parameter-widgets :as widgets]
            [tolglow.visualization.parameter-views :as views]))

(deftest spatial-defaults-follow-the-value-domain
  (is (= :spatial (visual/selected-mode {:type "vector" :size 3} nil)))
  (is (= :orientation (visual/spatial-kind {:type :vector :size 3 :semantic/type "orientation"})))
  (is (= :color (visual/selected-mode {:type "color"} "auto")))
  (is (nil? (visual/spatial-kind {:type :color})))
  (is (= :history (visual/selected-mode {:type :number :visualization :history} :auto)))
  (is (= :controls (visual/selected-mode {:type :vector :size 4} :auto)))
  (is (= :controls (visual/selected-mode {:type :enum} :history)))
  (is (nil? (visual/components {:type :vector} [0 js/NaN 0])))
  (is (nil? (visual/components {:type :color} nil)))
  (is (= [0 0.5 1] (visual/components {:type :color} [0 0.5 1]))))

(deftest colour-parameters-draw-the-colour-widget-on-push-and-studio
  (let [schemas [{:type "color"} {:type :color} {:type :vector :size 3 :semantic/type :rgb :bounds [0 1]}]
        value [1 0.42 0.12]]
    (doseq [schema schemas
            preference [nil "auto" "spatial" "controls" "history"]
            state [nil "active" "held" "pending"]
            :let [parameter {:id "tint" :param "tint" :schema schema :value value :source-state state
                             :component 0 :component-label "hue"}
                  view {:parameter-view preference :parameter-observations {"tint" [{:beat 0 :values value}]}}
                  [component & args] (visual/push view parameter)
                  [touched] (visual/push view parameter true)
                  history? (and (= "history" preference) (= "active" state))]]
      (is (not= visual/space component) (str "Push " (pr-str schema) " " preference " " state))
      (is (not= visual/space touched))
      (if history?
        (is (= visual/history-chart component))
        (do (is (= widgets/color-view component))
            (is (= [value {:selected "hue"}] args)))))
    (doseq [schema schemas]
      (is (= :color (visual/monitor-kind schema)))
      (is (= [:color :history] (views/studio-choices schema)))
      (doseq [choice [:auto :spatial :controls]]
        (let [mode (views/studio-view schema choice)]
          (is (= :color mode))
          (is (= [widgets/color-view value] (visual/observed-view mode schema value {} (atom nil) 4)))))
      (is (= :history (views/studio-view schema :history))))
    (is (= visual/space (first (visual/push {} {:schema {:type :vector :size 3 :bounds [-1 1]} :value [0 0 0]}))))
    (is (= [:spatial :history] (views/studio-choices {:type :vector :size 3})))))

(deftest colour-tracks-follow-the-component-a-control-edits
  (let [orange [1 0.5 0]
        rgb (widgets/color-tracks orange "g")
        hsv (widgets/color-tracks orange "saturation")
        near? (fn [a b] (< (js/Math.abs (- a b)) 1e-9))]
    (is (= [:red :green :blue] (mapv :id rgb)))
    (is (= [false true false] (mapv :selected? rgb)))
    (is (= [1 0.5 0] (mapv :amount rgb)))
    (is (= "linear-gradient(90deg,rgb(255,0,0),rgb(255,255,0))" (:gradient (second rgb))))
    (is (= [:hue :saturation :brightness] (mapv :id hsv)))
    (is (near? (/ 30 360) (:amount (first hsv))))
    (is (= [1 1] (mapv :amount (rest hsv))))
    (is (= "linear-gradient(90deg,rgb(255,255,255),rgb(255,128,0))" (:gradient (second hsv))))
    (is (every? true? (map near? orange (widgets/hsv-rgb (widgets/hsv orange)))))
    (is (near? 1 (widgets/component-amount orange "saturation" 0)))
    (is (= 0.5 (widgets/component-amount orange "g" 1)))
    (is (= 0 (widgets/component-amount orange nil 2)))
    (is (nil? (widgets/component-amount [1 js/NaN 0] "hue" 0)))
    (is (= :div.parameter-color (first (widgets/color-view orange))))
    (is (= :span.parameter-no-sample (first (widgets/color-view nil))))
    (is (= "linear-gradient(90deg,rgb(255,0,0) 0%,rgb(0,0,255) 100%)"
           (widgets/gradient-stops-css [{:position 0 :color [1 0 0]} {:position 1 :color [0 0 1]}])))
    (is (= :div.parameter-color.gradient
           (first (widgets/color-view [{:position 0 :color [1 0 0]} {:position 1 :color [0 0 1]}]))))))

(deftest observed-captions-retain-destination-precision
  (doseq [[bits value expected] [[8 (/ 127 255) "0.498"]
                                 [8 (/ 128 255) "0.502"]
                                 [16 (/ 32767 65535) "0.49999"]
                                 [16 (/ 32768 65535) "0.50001"]
                                 [nil 0.000123456789 "0.000123"]]]
    (let [schema {:type :number :bounds [0 1] :dmx-bits bits}
          point [value 0 1]
          [_ _ _ caption] (visual/space (assoc schema :type :vector :size 3) point point)
          [_ _ [_ _ readings] _ axis] (visual/history-chart schema [{:beat 0 :values [value]}] 4)]
      (is (= [:figcaption (str expected " · 0 · 1")] caption))
      (is (= expected (peek (first readings))))
      (is (= [:figcaption [:span "0 … 1"] [:span "4 beats"]] axis)))))

(deftest push-slots-show-supplied-signals-until-touched-and-label-rounded-components
  (let [level {:id "level" :schema {:type :number :bounds [0 1] :unit "%"} :value 0.25
               :preview-samples [0 0.5 1 0.5]}
        [_ trace [reading]] (visual/push {:parameter-view "controls"} level)
        point {:type :vector :size 3 :bounds [-1 1] :dmx-bits 16}
        [_ _ [_ attrs & spans]] (visual/history-chart point [{:beat 0 :values [0.123456 -0.5 1]}] 4 nil nil true)
        [_ {:keys [title]} [_ label] shown] (ffirst spans)]
    (is (= :svg.parameter-sparkline (first trace)))
    (is (= :output.push-signal-value reading))
    (is (= widgets/typed-display (first (visual/push {:parameter-view "controls"} level true))))
    (is (= widgets/typed-display (first (visual/push {:parameter-view "controls"} (dissoc level :preview-samples)))))
    (is (= {:class "push-reading" :data-components 3} attrs))
    (is (= ["X" "0.123"] [label shown]))
    (is (= "X 0.12346" title) "the title keeps destination precision")))

(deftest dmx-domain-labels-distinguish-narrow-bounds
  (let [bounds [0.50001 0.50004]
        point [0.50002 0.50002 0.50002]
        data [{:beat 0 :values [0.50002]}]]
    (doseq [[bits expected] [[16 "0.50001 … 0.50004"] [nil "0.5 … 0.5"]]]
      (let [schema {:type :number :bounds bounds :dmx-bits bits}
            [_ _ _ _ domain] (visual/space (assoc schema :type :vector :size 3) point point)
            [_ _ _ _ axis] (visual/history-chart schema data 4)]
        (is (= [:small.parameter-domain expected] domain))
        (is (= [:figcaption [:span expected] [:span "4 beats"]] axis))))))

(defn history-view [id n v]
  {:display-mode "parameters" :runtime-id "one" :instance-id id :epoch 1 :definition-version 3
   :visualization-clock {:time-ms (* 250 n) :beat (/ n 2)} :history-beats 1 :parameter-view "history"
   :parameters [{:id "point" :schema {:type "vector" :size 3 :bounds [-1 1]}
                 :base-value [0 0 0] :effective-value v :source-state "active"}]})

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

(deftest a-manual-held-or-failed-control-keeps-its-widget-without-reading-history
  (let [raw (history-view "a" 0 [1 0 0])
        first-frame (display/observe-view nil raw)
        draw (fn [state frame] (display/observe-view first-frame (assoc-in frame [:parameters 0 :source-state] state)))]
    (doseq [state [nil "held" "pending" "error"]]
      (let [frame (draw state raw) row (first (:parameters frame))]
        (is (empty? (:parameter-observations frame)))
        (is (= visual/space (first (visual/push frame row))))
        (is (= widgets/typed-display (first (visual/push frame (assoc row :schema {:type :number})))))))
    (is (= 2 (count (get-in (draw "active" (history-view "a" 1 [1 0 0])) [:parameter-observations "point"]))))
    (is (= 1 (count (get-in (draw "active" (assoc-in (history-view "a" 1 [0 1 0]) [:parameters 0 :sample-fixture] {:id :rear}))
                             [:parameter-observations "point"]))))))

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

(deftest studio-can-retain-each-committed-sample-and-scroll-it-at-display-speed
  (let [clock (fn [n] {:time-ms (* 25 n) :beat (/ n 16) :source :show :sample-period-ms 400})
        history (reduce #(visual/append-sample %1 (clock %2) [(mod %2 2)] 0 1024) [] (range 4))
        schema {:type :number :bounds [0 1]}]
    (is (= [0 25 50 75] (mapv :t history)))
    (is (= {:beats 0.0625 :duration 25 :offset 1.5625} (visual/history-motion history 4)))
    (is (= ["M-100,41L0,3L100,41"]
           (:paths (visual/chart-paths (subvec history 0 3) 0.0625 schema 0.0625))))
    (is (nil? (visual/history-motion (assoc-in history [3 :source] :new-show) 4)))
    (is (nil? (visual/history-motion (assoc-in history [3 :beat] 0) 4)))))

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
