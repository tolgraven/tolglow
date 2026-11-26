(ns tolglow.studio.param-visualization-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [tolglow.studio.param-visualization :as visual]
            [tolglow.studio.push-display :as display]
            [tolglow.studio.parameter-widgets :as widgets]
            [tolglow.visualization.parameter-views :as views]
            [tolglow.visualization.vector-space :as vector-space]
            [tolglow.visualization.waveform-shapes :as shapes]))

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
            (is (= [value {:selected "hue" :schema schema}] args)))))
    (doseq [schema schemas]
      (is (= :color (visual/monitor-kind schema)))
      (is (= [:color :history] (views/studio-choices schema)))
      (doseq [choice [:auto :spatial :controls]]
        (let [mode (views/studio-view schema choice)]
          (is (= :color mode))
          (is (= [widgets/color-view value {:schema schema}] (visual/observed-view mode schema value {} (atom nil) 4)))))
      (is (= :history (views/studio-view schema :history))))
    (is (= visual/space (first (visual/push {} {:schema {:type :vector :size 3 :bounds [-1 1]} :value [0 0 0]}))))
    (is (= [:spatial :history] (views/studio-choices {:type :vector :size 3})))))

(def colour-layout-samples
  "A known value in each colour layout, the swatch it draws and its track letters."
  [[:rgb [1 0.5 0] "rgb(255,128,0)" ["R" "G" "B"]]
   [:rgba [0 0 1 0.5] "rgb(0,0,255)" ["R" "G" "B"]]
   [:rgbw [0.5 0 0 0.5] "rgb(255,128,128)" ["R" "G" "B" "W"]]
   [:hsl [(/ 1 3) 1 0.5] "rgb(0,255,0)" ["H" "S" "L"]]
   [:hsla [(/ 2 3) 1 0.25 0.5] "rgb(0,0,128)" ["H" "S" "L"]]
   [:hsv [0 1 1] "rgb(255,0,0)" ["H" "S" "V"]]])

(defn- swatch-and-labels [form]
  (let [nodes (tree-seq sequential? seq form)
        tagged (fn [tag] (filter #(and (vector? %) (= tag (first %))) nodes))
        swatch (first (tagged :div.color-swatch))]
    {:background (get-in swatch [1 :style :background])
     :tag (some #(when (and (vector? %) (= :small (first %))) (second %)) swatch)
     :labels (mapv second (tagged :span.color-channel-name))}))

(deftest each-colour-layout-draws-its-own-colour-on-studio
  (doseq [[layout value css letters] colour-layout-samples
          :let [schema {:type :vector :size (count value) :semantic/type layout :bounds [0 1]}
                [view & args] (visual/observed-view :color schema value {} (atom nil) 4)
                drawn (swatch-and-labels (apply view args))]]
    (is (= css (:background drawn)) (str layout))
    (is (= letters (:labels drawn)) (str layout))
    (is (= (when (#{:rgba :hsla} layout) "α 0.5") (:tag drawn)) (str layout))
    (is (= (get {:rgb ["R" "G" "B"] :rgba ["R" "G" "B" "A"] :rgbw ["R" "G" "B" "W"]
                 :hsl ["H" "S" "L"] :hsla ["H" "S" "L" "A"] :hsv ["H" "S" "V"]} layout)
           (views/component-labels schema)) (str layout)))
  (is (= ["R" "G" "B" "A"] (views/component-labels {:type :color})))
  (is (= {:background "rgb(255,128,128)" :tag "W 0.5" :labels ["H" "S" "V"]}
         (swatch-and-labels (widgets/color-view [0.5 0 0 0.5] {:schema {:semantic/type "rgbw"} :selected "hue"})))
      "a Push hue encoder on an RGBW colour keeps the white amount on the swatch")
  (let [hsl (widgets/color-tracks [(/ 1 3) 0.5 0.5] "lightness" :hsl)]
    (is (= [:hue :saturation :lightness] (mapv :id hsl)))
    (is (= [false false true] (mapv :selected? hsl)))
    (is (= "linear-gradient(90deg,rgb(0,0,0),rgb(64,191,64),rgb(255,255,255))" (:gradient (last hsl)))
        "the lightness track passes through the colour at one half")
    (is (= 0.5 (widgets/component-amount [(/ 1 3) 0.5 0.5] "l" 0 :hsl)))
    (is (= 0.25 (widgets/component-amount [0.25 1 1] "hue" 0 :hsv)) "an HSV hue is read, not derived from RGB")
    (is (= 0.75 (widgets/component-amount [-0.25 1 1] "hue" 0 :hsv)) "a hue track marks the wrapped hue the swatch draws")))

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

(defn- nodes
  "Every hiccup vector under a rendered form, components left unexpanded."
  [form]
  (filter vector? (tree-seq sequential? seq form)))

(defn- tagged [form tag] (filter #(= tag (first %)) (nodes form)))

(deftest the-vector-space-draws-the-shared-geometry-and-the-magnitude
  (let [schema {:type :vector :size 3 :bounds [-1 1]}
        [_ _ svg _ _ magnitude :as figure] (visual/space schema [0.6 0 0.8] [0.6 0 0.8])
        labels (tagged svg :text.space-axis-label)
        walls (tagged svg :polygon)
        depth (tagged svg :polygon.space-depth)
        arrow (vector-space/arrow [[-1 1] [-1 1] [-1 1]] [0.6 0 0.8])
        [_ arrow-model] (first (filter #(and (fn? (first %)) (map? (second %)) (:head (second %))) (nodes svg)))]
    (is (= :figure.parameter-space (first figure)))
    (is (= "0 0 96 80" (get-in svg [1 :view-box])))
    (is (= ["X" "Y" "Z"] (map last labels)))
    (is (= (map :label vector-space/axes) (map (comp (juxt :x :y) second) labels))
        "each letter sits on the projected centre of its high face")
    (is (= 3 (count walls)))
    (is (= 3 (count (distinct (map #(get-in % [1 :style :fill]) walls)))) "each wall has its own shade")
    (is (= (map #(get-in % [1 :points]) walls) (map #(get-in % [1 :points]) depth))
        "every wall darkens toward the far corner")
    (is (= (:at vector-space/far-corner)
           ((juxt :cx :cy) (second (first (tagged svg :radialGradient#space-depth))))))
    (is (= arrow arrow-model) "the arrow is the shared model for the plotted value")
    (is (= [:small.space-magnitude {:style {"--extent" (:extent arrow)} :title "Magnitude 1 within -1 … 1"}
            [:b "‖v‖"] "1"]
           magnitude))
    (is (= "0.6, 0, 0.8, range -1 … 1, magnitude 1" (get-in svg [1 :aria-label])))
    (testing "an orientation draws its rotated axes and reports no magnitude"
      (let [turned (visual/space (assoc schema :semantic/type :orientation :bounds [-180 180]) [10 20 30] [10 20 30])]
        (is (nil? (get turned 5)))
        (is (empty? (filter #(and (fn? (first %)) (map? (second %)) (:head (second %))) (nodes turned))))))
    (testing "labels follow the schema and the Push reading keeps its components"
      (let [[_ _ named reading] (visual/space (assoc schema :component-labels ["Pan" "Tilt" "Roll"]) [0 0 0] [0 0 0] true)]
        (is (= ["Pan" "Tilt" "Roll"] (map last (tagged named :text.space-axis-label))))
        (is (= :figcaption.push-reading (first reading)))))))

(def common-time {:meter {:beats-per-bar 4 :bars-per-phrase 4}})

(deftest push-signal-previews-draw-the-grid-of-their-supplied-cycle-window
  (let [level {:id "level" :param "level" :schema {:type :number :bounds [0 1]} :value 0.25
               :preview-samples [0 0.5 1 0.5]
               :signal-timing {:period-beats 4 :start-beat 0 :span-beats 8}}
        view (display/observe-view nil {:parameters [level] :musical-timing common-time})
        grid (get-in view [:parameter-grids "level" :preview])
        [_ [_ attrs [draw spec _ y1 y2]] _ chip] (visual/push view (first (:parameters view)))
        [tag marks lines] (widgets/grid-lines grid #(* 100 (:position %)) 0 32)]
    (is (= [:beat :bar] (:units grid)))
    (is (= [widgets/grid-lines grid 0 32] [draw spec y1 y2]))
    (is (= "Signal preview from supplied samples, period 1 bar" (:aria-label attrs)))
    (is (= [:small.grid-period {:data-unit "bar" :title "Signal period 1 bar"} "1 bar"] chip))
    (is (= [:g.musical-grid {:data-minor "beat" :data-major "bar"}] [tag marks]))
    (is (= ["bar" "beat" "beat" "beat" "bar" "beat" "beat" "beat" "bar"] (map (comp :class second) lines)))
    (is (= [0 12.5 25 37.5 50 62.5 75 87.5 100] (map (comp :x1 second) lines)))
    (testing "the signal's own meter wins over the transport meter"
      (let [own (assoc-in level [:signal-timing :meter] {:beats-per-bar 2 :bars-per-phrase 4})
            projected (display/observe-view nil {:parameters [own] :musical-timing common-time})]
        (is (= [:bar :phrase] (get-in projected [:parameter-grids "level" :preview :units])))))
    (testing "a preview without supplied timing or meter draws no grid"
      (is (nil? (:parameter-grids (display/observe-view nil {:parameters [(dissoc level :signal-timing)]
                                                             :musical-timing common-time}))))
      (is (nil? (:parameter-grids (display/observe-view nil {:parameters [level]}))))
      (is (= 3 (count (visual/push {} level))) "no period chip without a grid")
      (is (nil? (widgets/grid-lines nil identity 0 1)))
      (is (nil? (widgets/grid-period {:units [:beat]}))))))

(deftest history-grids-follow-the-observed-window-the-source-period-and-the-transport
  (let [history [{:beat 10 :values [0]} {:beat 12 :values [1]}]
        coarse #(remove (comp #{:sixteenth} :unit) (:lines %))
        beat (visual/chart-grid history 4 0 common-time 1)
        bar (visual/chart-grid history 4 0 common-time 4)
        phrase (visual/chart-grid [{:beat 32 :values [0]}] 16 0 common-time 16)
        led (visual/chart-grid history 4 1 common-time 4)
        offset (visual/chart-grid history 4 0 (assoc common-time :bar-origin-beat 1) 4)]
    (is (= [:sixteenth :beat :bar :phrase] (:units beat)))
    (is (= 17 (count (:lines beat))))
    (is (= [[0 :bar] [25 :beat] [50 :beat] [75 :beat] [100 :bar]] (map (juxt :x :unit) (coarse beat))))
    (is (= [:beat :bar :phrase] (:units bar)))
    (is (= [[0 :bar] [25 :beat] [50 :beat] [75 :beat] [100 :bar]] (map (juxt :x :unit) (:lines bar))))
    (is (= [:bar :phrase] (:units phrase)))
    (is (= [[0 :phrase] [25 :bar] [50 :bar] [75 :bar] [100 :phrase]] (map (juxt :x :unit) (:lines phrase))))
    (is (= [-25 0 25 50 75 100] (map :x (:lines led))) "the scrolling lead extends the grid to the left")
    (is (= [[25 :bar] [50 :beat] [75 :beat] [100 :beat]] (map (juxt :x :unit) (rest (:lines offset)))))
    (is (= [:beat :bar :phrase] (:units (visual/chart-grid history 4 0 common-time nil)))
        "without a source period the window chooses the unit")
    (is (nil? (visual/chart-grid history 4 0 {} 1)) "no transport meter, no grid")
    (is (nil? (visual/chart-grid [] 4 0 common-time 1)))
    (let [[_ _ _ [_ _ [_ _ [draw spec]]] caption chip] (visual/history-chart {:type :number :bounds [0 1]} history 4 nil nil true bar)]
      (is (= [widgets/grid-lines bar] [draw spec]))
      (is (= [:figcaption [:span "0 … 1"] [:span "4 beats"]] caption))
      (is (= [:small.grid-period {:data-unit "bar" :title "Signal period 1 bar"} "1 bar"] chip)))
    (is (nil? (get (visual/history-chart {:type :number :bounds [0 1]} history 4) 5)) "no period, no chip")))

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

(deftest push-history-projects-its-grid-with-each-supplied-view
  (let [frame (fn [n] (-> (history-view "a" n [0.5 0.2 1])
                          (assoc :musical-timing common-time :history-beats 4)
                          (assoc-in [:parameters 0 :source-period-beats] 4)))
        view (reduce display/observe-view nil (map frame (range 0 20)))
        grid (get-in view [:parameter-grids "point" :history])
        [component _ _ _ _ _ _ drawn] (visual/push view (first (:parameters view)))]
    (is (= 9.5 (:beat (peek (get-in view [:parameter-observations "point"])))))
    (is (= [5.5 4] ((juxt :start-beat :span-beats) grid)))
    (is (= [[6 :beat] [7 :beat] [8 :bar] [9 :beat]] (map (juxt :beat :unit) (:lines grid))))
    (is (= [12.5 37.5 62.5 87.5] (map :x (:lines grid))))
    (is (= {:unit :bar :count 1 :beats 4} (:period grid)))
    (is (= [visual/history-chart grid] [component drawn]))
    (is (nil? (:parameter-grids (display/observe-view view (dissoc (frame 20) :musical-timing)))))))

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

;; The catalog shape a Push view or a Studio snapshot carries after JSON.
(def glyph-catalog
  [{:id "pulse" :name "Pulse" :glyph [1 1 0 0 1]} {:id "saw" :name "Saw" :glyph [0 0.25 0.5 0.75 0]}
   {:id "sine" :name "Sine" :glyph [0.5 1 0.5 0 0.5]} {:id "triangle" :name "Triangle" :glyph [0 0.5 1 0.5 0]}])

(deftest push-shape-enums-draw-the-registered-glyphs
  (let [schema {:type "enum" :choices ["sine" "saw" "square"]}
        parameter {:id "shape" :param "shape" :schema schema :value "saw"}
        [component glyphs value] (visual/push {:waveform-glyphs glyph-catalog} parameter)
        figure (widgets/shape-view glyphs value)
        strip (tagged figure :span)]
    (is (= widgets/shape-view component))
    (is (= ["Sine" "Saw" "Pulse"] (map :label glyphs)) "the legacy square name draws the registered pulse")
    (is (= [0 0.25 0.5 0.75 0] (:glyph (second glyphs))))
    (is (= [:div.shape-choice-value "Saw"] (second figure)))
    (is (= [widgets/shape-glyph [0 0.25 0.5 0.75 0] {:width 100 :height 34}] (nth figure 2))
        "the selected shape draws large from its catalog glyph")
    (is (= [nil "selected" nil] (map #(get-in % [1 :class]) strip)))
    (is (= (map :glyph glyphs) (map #(second (nth % 2)) strip)))
    (is (= [:span.typed-choice-position "2 / 3"] (last figure)))
    (is (= :svg.shape-glyph (first (widgets/shape-glyph [0 1 0]))))
    (testing "without a catalog, or for any other enum, the choice stays text"
      (is (= widgets/typed-display (first (visual/push {} parameter))))
      (is (= widgets/typed-display
             (first (visual/push {:waveform-glyphs glyph-catalog}
                                 (assoc parameter :schema {:type "enum" :values ["sphere" "rounded-box"]}))))))))

(defn- key-event [key buttons]
  #js {:key key :preventDefault (fn [])
       :currentTarget #js {:parentNode #js {:children (into-array buttons)}}})

(deftest studio-shape-pickers-are-radio-groups-of-glyphs
  (let [shapes (shapes/choices {:type "enum" :values ["sine" "saw" "square"]} glyph-catalog)
        sent (atom [])
        focused (atom nil)
        buttons (mapv (fn [i] #js {:focus #(reset! focused i)}) (range 3))
        picker (widgets/shape-picker "Shape" shapes "saw" #(swap! sent conj %))
        radios (tagged picker :button)
        handler (fn [index event] ((get-in (nth radios index) [1 :on-key-down]) event))]
    (is (= {:role "radiogroup" :aria-label "Shape"} (second picker)))
    (is (= ["radio" "radio" "radio"] (map #(get-in % [1 :role]) radios)))
    (is (= [false true false] (map #(get-in % [1 :aria-checked]) radios)))
    (is (= [-1 0 -1] (map #(get-in % [1 :tab-index]) radios)) "only the selection is in the tab order")
    (is (= [[widgets/shape-glyph [0.5 1 0.5 0 0.5]] [:span "Sine"]] (drop 2 (first radios))))
    ((get-in (first radios) [1 :on-click]))
    (handler 1 (key-event "ArrowRight" buttons))
    (handler 2 (key-event "ArrowRight" buttons))
    (handler 0 (key-event "ArrowLeft" buttons))
    (handler 1 (key-event "End" buttons))
    (is (= ["sine" "square" "sine" "square" "square"] @sent) "the schema's own values go back, wrapping at the ends")
    (is (= 2 @focused))
    (handler 1 (key-event "a" buttons))
    (is (= 5 (count @sent)) "other keys do nothing")))
