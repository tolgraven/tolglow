(ns tolglow.studio.parameter-visualization
  "Passive visualizations of published parameter values. Geometry uses normalized
  coordinates only for display. History keeps the observed beat and components."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.parameter-widgets :as widgets]
            [tolglow.studio.visibility :refer [watch-visibility]]))

(def zooms [0.5 1 2 4 8 16])
(def colors ["#e8a083" "#aed39a" "#89b9e0" "#cfb2dd"])
(defn- tag [id] (when (or (keyword? id) (string? id)) (keyword id)))

(defn components [schema value]
  (let [value (if (= :color (widgets/parameter-kind schema)) (widgets/color-components value) value)]
    (cond
      (widgets/finite? value) [value]
      (and (sequential? value) (<= 1 (count value) 4) (every? widgets/finite? value)) (vec value))))

(defn spatial-kind [schema]
  (case (widgets/parameter-kind schema)
    :color :color
    :vector (when (= 3 (:size schema))
              (case (tag (:semantic/type schema))
                (:orientation :rotation) :orientation
                :direction :direction
                :position))
    nil))

(defn view-kind [schema preference]
  (let [mode (or (tag preference) :auto)]
    (cond
      (#{:choice :enum :boolean} (widgets/parameter-kind schema)) :controls
      (= mode :history) :history
      (= mode :controls) :controls
      (spatial-kind schema) :spatial
      :else :controls)))

(defn- bounds [schema values]
  (or (when (= :color (spatial-kind schema)) [0 1])
      (widgets/numeric-range schema)
      (let [extent (max 1 (reduce max 0 (map abs values)))
            scale (js/Math.pow 2 (js/Math.ceil (js/Math.log2 extent)))]
        [(- scale) scale])))

(defn- unit-value [[low high] v] (max 0 (min 1 (/ (- v low) (- high low)))))
(defn- screen
  "Oblique camera, fixed portrait-free aspect for the 960 × 160 hardware row."
  [[x y z]]
  [(+ 55 (* 25 (- x z)))
   (+ 36 (* -21 y) (* 10 (+ x z)))])

(defn- points [positions]
  (str/join " " (map (fn [p] (str/join "," (screen p))) positions)))

(defn- segment [from to attrs]
  (let [[x1 y1] (screen from) [x2 y2] (screen to)]
    [:line (assoc attrs :x1 x1 :y1 y1 :x2 x2 :y2 y2)]))

(defn- rotate-euler [[x y z] [rx ry rz]]
  (let [y1 (- (* y (js/Math.cos rx)) (* z (js/Math.sin rx)))
        z1 (+ (* y (js/Math.sin rx)) (* z (js/Math.cos rx)))
        x2 (+ (* x (js/Math.cos ry)) (* z1 (js/Math.sin ry)))
        z2 (- (* z1 (js/Math.cos ry)) (* x (js/Math.sin ry)))]
    [(- (* x2 (js/Math.cos rz)) (* y1 (js/Math.sin rz)))
     (+ (* x2 (js/Math.sin rz)) (* y1 (js/Math.cos rz))) z2]))

(defn- orientation [schema xyz]
  (let [rad? (#{:rad :radians} (tag (or (:unit schema) (:units schema))))
        angles (mapv #(if rad? % (* % (/ js/Math.PI 180))) xyz)]
    [:g (for [[i axis] (map-indexed vector [[0.85 0 0] [0 0.85 0] [0 0 0.85]])]
          ^{:key i} [segment [0 0 0] (rotate-euler axis angles)
                     {:stroke (colors i) :stroke-width 2.4 :stroke-linecap "round"}])]))

(defn spatial-keyframe
  "Keep two observed endpoints when the visual scheduler requests fewer samples.
  Sampling changes the target. Presentation reads this pair at each frame."
  [previous clock values]
  (let [{:keys [time-ms source sample-period-ms]} clock
        period (if (widgets/finite? sample-period-ms) sample-period-ms 0)
        t0 (:t previous)
        start {:source source :t time-ms :period period :from values :to values}]
    (cond
      (and (= t0 time-ms) (= source (:source previous)) (= period (:period previous)) (= values (:to previous))) previous
      (or (nil? previous) (nil? values) (nil? (:to previous)) (not (widgets/finite? time-ms))
          (not= source (:source previous)) (not= period (:period previous))
          (not (widgets/finite? (:t previous))) (< time-ms (:t previous)) (<= period 100)) start
      (>= (- time-ms (:t previous)) period) (assoc start :from (:to previous))
      :else previous)))

(defn spatial-value [schema keyframe time-ms]
  (let [{:keys [t period from to]} keyframe]
    (if (and (= (count from) (count to)) (seq from) (> period 100)
             (every? widgets/finite? [t time-ms]) (not= from to))
      (let [progress (max 0 (min 1 (/ (- time-ms t) period)))
            turn (when (= :orientation (spatial-kind schema))
                   (if (#{:rad :radians} (tag (or (:unit schema) (:units schema)))) (* 2 js/Math.PI) 360))
            blend (fn [a b]
                    (let [delta (- b a) step (if turn (- (mod (+ delta (/ turn 2)) turn) (/ turn 2)) delta)]
                      (if (= 1 progress) b (+ a (* progress step)))))]
        (mapv blend from to))
      to)))

(defn space [schema value plotted]
  (let [actual (components schema value)
        xyz (some->> plotted (take 3) vec)
        kind (spatial-kind schema)
        color? (= :color kind)
        labels (or (:component-labels schema) (if color? ["R" "G" "B"] ["X" "Y" "Z"]))]
    (if-not (and kind (= 3 (count xyz)))
      [:span.parameter-no-sample {:role "status"} "Awaiting value"]
      (let [domain (bounds schema xyz)
            point (mapv #(- (* 2 (unit-value domain %)) 1) xyz)
            [x _ z] point
            [px py] (screen point)
            tint (if color? (widgets/color-css xyz) "var(--push-accent, #bdd49f)")
            caption (str (widgets/number-label (first domain)) " … " (widgets/number-label (second domain)))]
        [:figure.parameter-space {:aria-label (str (name kind) " in three dimensions")}
         [:svg {:view-box "0 0 110 79" :role "img" :aria-label (str (str/join ", " xyz) ", range " caption)}
          [:polygon.space-wall {:points (points [[-1 -1 -1] [-1 1 -1] [-1 1 1] [-1 -1 1]])}]
          [:polygon.space-wall.side {:points (points [[-1 -1 -1] [-1 1 -1] [1 1 -1] [1 -1 -1]])}]
          [:polygon.space-floor {:points (points [[-1 -1 -1] [1 -1 -1] [1 -1 1] [-1 -1 1]])}]
          (for [n [-0.5 0 0.5]]
            ^{:key n} [:g.space-grid [segment [n -1 -1] [n -1 1] {}] [segment [-1 -1 n] [1 -1 n] {}]])
          (if (= kind :orientation)
            [orientation schema xyz]
            [:<> [segment [x -1 z] point {:class "space-drop"}]
             [segment (if color? [-1 -1 -1] (mapv #(- (* 2 (unit-value domain %)) 1) [0 0 0]))
              point {:stroke tint :stroke-width 1.9}]
             [:circle {:cx px :cy py :r 4 :fill tint :stroke "#e8eee0" :stroke-width 1}]])
          (for [[i [tx ty]] (map-indexed vector [[92 36] [55 8] [14 36]])]
            ^{:key i} [:text.space-axis {:x tx :y ty :fill (colors i)} (get labels i)])]
         [:figcaption (str/join " · " (map widgets/number-label (take 3 actual)))]
         [:small.parameter-domain caption]]))))

(defn append-sample [previous clock values]
  (let [{:keys [time-ms beat]} clock
        sample {:t time-ms :beat beat :values values :source (:source clock)}
        last-sample (peek previous)]
    (cond
      (not (every? widgets/finite? [time-ms beat])) previous
      (nil? last-sample) [sample]
      (or (not= (:source clock) (:source last-sample)) (< time-ms (:t last-sample)) (< beat (:beat last-sample))) [sample]
      (= beat (:beat last-sample)) (if (= values (:values last-sample)) previous (conj (pop previous) sample))
      (< (- time-ms (:t last-sample)) (max 100 (or (:sample-period-ms clock) 0))) previous
      :else (let [samples (conj previous sample)]
              (if (> (count samples) 256) (into [] (subvec samples 1)) samples)))))

(defn- window-samples [history start]
  (let [[earlier visible] (split-with #(< (:beat %) start) history)
        a (last earlier) b (first visible)
        av (:values a) bv (:values b)]
    (if (and a b (seq av) (= (count av) (count bv)) (< start (:beat b)))
      (let [t (/ (- start (:beat a)) (- (:beat b) (:beat a)))
            blend (fn [x y] (+ x (* t (- y x))))]
        (cons {:beat start :values (mapv blend av bv)} visible))
      visible)))

(defn chart-paths [history window schema]
  (let [last-beat (:beat (peek history))
        start (when (number? last-beat) (- last-beat window))
        values (when start (window-samples history start))
        all (mapcat :values values)]
    (when (seq all)
      (let [[low high] (or (widgets/numeric-range schema)
                           (when (= :color (widgets/parameter-kind schema)) [0 1])
                           [(apply min all) (apply max all)])
            row (fn [axis]
                  (reduce (fn [[path pen?] {:keys [beat values]}]
                            (if-some [v (get values axis)]
                              [(str path (if pen? "L" "M") (* 100 (/ (- beat start) window)) ","
                                    (if (= low high) 22 (- 41 (* 38 (unit-value [low high] v))))) true]
                              [path false]))
                          ["" false] values))]
        {:bounds [low high] :paths (mapv (comp first row) (range (apply max (map #(count (:values %)) values))))}))))

(defn history-chart [schema history window]
  (let [{:keys [bounds paths]} (chart-paths history window schema)]
    [:figure.parameter-history {:aria-label "Observed parameter history"}
     [:div.history-reading
      (for [[i v] (map-indexed vector (:values (peek history)))]
        ^{:key i} [:span {:style {:color (colors i)}}
                   (when (> (count paths) 1)
                     (str (get (or (:component-labels schema)
                                   (if (= :color (widgets/parameter-kind schema)) ["R" "G" "B" "A"] ["X" "Y" "Z" "W"])) i) " "))
                   (widgets/number-label v)])]
     [:svg {:view-box "0 0 100 44" :preserve-aspect-ratio "none" :role "img"
            :aria-label (str "Last " window " beats")}
      (for [x [0 25 50 75 100]]
        ^{:key x} [:line.time-grid {:x1 x :x2 x :y1 0 :y2 44}])
      (for [[i d] (map-indexed vector paths)]
        ^{:key i} [:path {:d d :stroke (colors i) :fill "none" :stroke-width 1.6
                          :vector-effect "non-scaling-stroke"}])]
     [:figcaption [:span (if bounds (str (widgets/number-label (first bounds)) " … " (widgets/number-label (second bounds))) "Awaiting samples")]
      [:span (str window " beats")]]]))

(defn selected-mode [schema preference]
  (view-kind schema (if (#{:auto nil} (tag preference)) (:visualization schema) preference)))

(defn- observe-parameter [previous clock preference {:keys [schema effective-value value param id]}]
  (let [id (or param id) values (components schema (if (some? effective-value) effective-value value))]
    (case (selected-mode schema preference)
      :history [id (append-sample (get previous id []) clock values)]
      :spatial [id (spatial-keyframe (get previous id) clock values)]
      nil)))

(defn update-push [previous next view]
  (let [identity (mapv #(get view %) [:runtime-id :epoch :instance-id :definition-version])
        preference (:parameter-view view) clock (:visualization-clock view)
        same? (and (= identity (:parameter-identity previous)) (= preference (:parameter-view previous)))
        retained (when same? (:parameter-observations previous))
        visible? (and clock (#{nil :parameters "parameters"} (:display-mode next))
                      (not (get-in next [:target-picker :open?])))
        entries (when visible?
                  (into {} (keep #(observe-parameter retained clock preference %)) (:parameters next)))]
    (cond-> (assoc next :parameter-view preference :history-beats (:history-beats view) :visualization-clock clock)
      (seq entries) (assoc :parameter-identity identity :parameter-observations entries))))

(defn push [view {:keys [schema effective-value value param id] :as parameter}]
  (let [shown (if (some? effective-value) effective-value value)]
    (case (selected-mode schema (:parameter-view view))
      :spatial [space schema shown
                (if-let [keyframe (get-in view [:parameter-observations (or param id)])]
                  (spatial-value schema keyframe (get-in view [:visualization-clock :time-ms]))
                  (components schema shown))]
      :history [history-chart schema (get-in view [:parameter-observations (or param id)])
                (or (some #{(:history-beats view)} zooms) 4)]
      [widgets/typed-display parameter])))

(rf/reg-sub ::clock
  (fn [db _]
    (let [show (:show db)]
      {:time-ms (or (get-in show [:transport :time-ms]) (:time-ms show))
       :beat (get-in show [:transport :beat])
       :source [(:runtime-id show) (:epoch show)]
       :sample-period-ms (get-in show [:visualization-clock :sample-period-ms])})))

(defn- record! [this]
  (let [[_ mode schema value clock observed] (r/argv this)
        retain (if (= mode :spatial) spatial-keyframe append-sample)]
    (swap! observed retain clock (components schema value))))

(def observed-visual
  (r/create-class
   {:component-did-mount record!
    :component-did-update (fn [this _] (record! this))
    :reagent-render (fn [mode schema value clock observed window]
                      (if (= mode :spatial)
                        [space schema value (spatial-value schema @observed (:time-ms clock))]
                        [history-chart schema @observed window]))}))

(defn- visible-visual [mode schema value window]
  (r/with-let [observed (r/atom (when (= mode :history) []))]
    [observed-visual mode schema value @(rf/subscribe [::clock]) observed window]))

(defn studio [schema value]
  (r/with-let [choice (r/atom :auto) window (r/atom 4)]
    (let [mode (selected-mode schema @choice)]
      [:div.parameter-visualizer
       [:div.parameter-views
        (when (spatial-kind schema)
          [:button {:type "button" :aria-pressed (= mode :spatial) :on-click #(reset! choice :spatial)} "3D"])
        [:button {:type "button" :aria-pressed (= mode :history) :on-click #(swap! choice (fn [c] (if (= c :history) :auto :history)))} "History"]
        (when (= mode :history)
          [:label "Window " [:select {:value @window :aria-label "Parameter history beat window"
                                     :on-change #(reset! window (js/Number (.. % -target -value)))}
                            (for [zoom zooms] ^{:key zoom} [:option {:value zoom} (str zoom " beats")])]])]
       (when (#{:spatial :history} mode)
         [watch-visibility
          (fn [visible?] (when visible?
                           ^{:key mode} [visible-visual mode schema value @window]))])])))
