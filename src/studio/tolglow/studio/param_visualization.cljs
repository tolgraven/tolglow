(ns tolglow.studio.param-visualization
  "Passive visualizations of published parameter values. Geometry uses normalized
  coordinates only for display. History keeps the observed beat and components.
  The shared parameter-views projection chooses the widget for each type."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.parameter-widgets :as widgets]
            [tolglow.studio.visibility :refer [watch-visibility]]
            [tolglow.visualization.musical-grid :as musical-grid]
            [tolglow.visualization.parameter-views :as views]))

(def zooms [0.5 1 2 4 8 16])

(def colors ["#e8a083" "#aed39a" "#89b9e0" "#cfb2dd"])

(defn- tag [id] (when (or (keyword? id) (string? id)) (keyword id)))

(defn components [schema value]
  (let [value (if (= :color (views/color-kind schema)) (widgets/color-components value) value)]
    (cond
      (widgets/finite? value) [value]
      (and (sequential? value) (<= 1 (count value) 4) (every? widgets/finite? value)) (vec value))))

(def spatial-kind views/spatial-kind)

(def monitor-kind views/monitor-view)

(def view-kind views/view-kind)

(defn- bounds [schema values]
  (or (widgets/numeric-range schema)
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

(defn- compact-number [schema axis value]
  (let [component (get (:component-schemas schema) axis schema)]
    (cond
      (#{8 16} (:dmx-bits component)) (widgets/number-label component value)
      (widgets/finite? value) (str (js/Number (.toPrecision value 3)))
      :else "—")))

(defn- component-reading [schema labels axis value push?]
  (let [component (get (:component-schemas schema) axis schema)
        exact (widgets/number-label component value)
        label (get labels axis)
        shown (if (and push? labels (widgets/finite? value))
                (widgets/number-label component (js/Number (.toFixed value 3)))
                (compact-number schema axis value))]
    [:span {:style {:color (colors axis)} :title (str label " " exact)}
     (when label (if push? [:b label] (str label " "))) shown]))

(defn- domain-label [schema [low high]]
  (let [display (when (#{8 16} (:dmx-bits schema)) schema)]
    (str (widgets/number-label display low) " … " (widgets/number-label display high))))

(defn component-label [{:keys [schema component component-label]}]
  (if (and (spatial-kind schema) (int? component) (<= 0 component 2))
    (get (or (:component-labels schema) ["X" "Y" "Z"]) component)
    component-label))

(defn space [schema value plotted & [push?]]
  (let [actual (components schema value)
        xyz (some->> plotted (take 3) vec)
        kind (spatial-kind schema)
        labels (or (:component-labels schema) ["X" "Y" "Z"])]
    (if-not (and kind (= 3 (count xyz)))
      [:span.parameter-no-sample {:role "status"} "Awaiting value"]
      (let [domain (bounds schema xyz)
            point (mapv #(- (* 2 (unit-value domain %)) 1) xyz)
            [x _ z] point
            [px py] (screen point)
            tint "var(--push-accent, #bdd49f)"
            caption (domain-label schema domain)]
        [:figure.parameter-space {:aria-label (str (name kind) " in three dimensions")
                                  :data-dmx-bits (:dmx-bits schema)}
         [:svg {:view-box "0 0 110 79" :role "img" :aria-label (str (str/join ", " xyz) ", range " caption)}
          [:polygon.space-wall {:points (points [[-1 -1 -1] [-1 1 -1] [-1 1 1] [-1 -1 1]])}]
          [:polygon.space-wall.side {:points (points [[-1 -1 -1] [-1 1 -1] [1 1 -1] [1 -1 -1]])}]
          [:polygon.space-floor {:points (points [[-1 -1 -1] [1 -1 -1] [1 -1 1] [-1 -1 1]])}]
          (for [n [-0.5 0 0.5]]
            ^{:key n} [:g.space-grid [segment [n -1 -1] [n -1 1] {}] [segment [-1 -1 n] [1 -1 n] {}]])
          (if (= kind :orientation)
            [orientation schema xyz]
            [:<> [segment [x -1 z] point {:class "space-drop"}]
             [segment (mapv #(- (* 2 (unit-value domain %)) 1) [0 0 0])
              point {:stroke tint :stroke-width 1.9}]
             [:circle {:cx px :cy py :r 4 :fill tint :stroke "#e8eee0" :stroke-width 1}]])
          (for [[i [tx ty]] (map-indexed vector [[92 36] [55 8] [14 36]])]
            ^{:key i} [:text.space-axis {:x tx :y ty :fill (colors i)} (get labels i)])]
         (if push?
           [:figcaption.push-reading
            (for [[axis value] (map-indexed vector (take 3 actual))]
              (with-meta (component-reading schema labels axis value true) {:key axis}))]
           [:figcaption (str/join " · " (map-indexed #(compact-number schema %1 %2) (take 3 actual)))])
         [:small.parameter-domain caption]]))))

(defn append-sample
  ([previous clock values]
   (append-sample previous clock values (max 100 (or (:sample-period-ms clock) 0)) 256))
  ([previous clock values interval-ms capacity]
   (let [{:keys [time-ms beat]} clock
         sample {:t time-ms :beat beat :values values :source (:source clock)}
         last-sample (peek previous)]
     (cond
       (not (every? widgets/finite? [time-ms beat])) previous
       (nil? last-sample) [sample]
       (or (not= (:source clock) (:source last-sample)) (< time-ms (:t last-sample)) (< beat (:beat last-sample))) [sample]
       (= beat (:beat last-sample)) (if (= values (:values last-sample)) previous (conj (pop previous) sample))
       (< (- time-ms (:t last-sample)) interval-ms) previous
       :else (let [samples (conj previous sample)]
               (if (> (count samples) capacity) (into [] (subvec samples 1)) samples))))))

(defn- window-samples [history start]
  (let [[earlier visible] (split-with #(< (:beat %) start) history)
        a (last earlier) b (first visible)
        av (:values a) bv (:values b)]
    (if (and a b (seq av) (= (count av) (count bv)) (< start (:beat b)))
      (let [t (/ (- start (:beat a)) (- (:beat b) (:beat a)))
            blend (fn [x y] (+ x (* t (- y x))))]
        (cons {:beat start :values (mapv blend av bv)} visible))
      visible)))

(defn chart-paths [history window schema & [lead]]
  (let [last-beat (:beat (peek history))
        start (when (number? last-beat) (- last-beat window))
        values (when start (window-samples history (- start (or lead 0))))
        all (mapcat :values values)]
    (when (seq all)
      (let [[low high] (or (widgets/numeric-range schema)
                           (when (= :color (views/color-kind schema)) [0 1])
                           [(apply min all) (apply max all)])
            row (fn [axis]
                  (reduce (fn [[path pen?] {:keys [beat values]}]
                            (if-some [v (get values axis)]
                              [(str path (if pen? "L" "M") (* 100 (/ (- beat start) window)) ","
                                    (if (= low high) 22 (- 41 (* 38 (unit-value [low high] v))))) true]
                              [path false]))
                          ["" false] values))]
        {:bounds [low high] :paths (mapv (comp first row) (range (apply max (map #(count (:values %)) values))))}))))

(defn chart-grid
  "The musical grid behind a history chart, from the transport timing the show
  supplied. The window ends at the last observed beat and can extend left by
  the scrolling lead. Each line gains its x in chart units. No timing, no grid."
  [history window lead timing period-beats]
  (let [last-beat (:beat (peek history))
        lead (or lead 0)]
    (when (and (map? (:meter timing)) (widgets/finite? last-beat))
      (let [start (- last-beat window)]
        (some-> (musical-grid/grid (assoc (select-keys timing [:meter :bar-origin-beat :phrase-origin-beat])
                                          :start-beat (- start lead) :span-beats (+ window lead)
                                          :period-beats (when (widgets/finite? period-beats) period-beats)
                                          :aligned? true))
                (update :lines (partial mapv #(assoc % :x (* 100 (/ (- (:beat %) start) window))))))))))

(defn signal-grid
  "The musical grid of a sampled cycle preview. The show supplies the window in
  beats and the meter the period was measured in."
  [timing signal-timing]
  (when (widgets/finite? (:period-beats signal-timing))
    (when-let [meter (or (:meter signal-timing) (:meter timing))]
      (musical-grid/grid (assoc (select-keys signal-timing [:start-beat :span-beats :period-beats]) :meter meter)))))

(defn history-chart [schema history window & [attach lead push? grid]]
  (let [{:keys [bounds paths]} (chart-paths history window schema lead)
        values (:values (peek history))
        labels (when (> (count paths) 1)
                 (or (:component-labels schema)
                     (if (= :color (views/color-kind schema)) ["R" "G" "B" "A"] ["X" "Y" "Z" "W"])))]
    [:figure.parameter-history {:aria-label "Observed parameter history" :data-dmx-bits (:dmx-bits schema)}
     [:div.history-reading {:class (when push? "push-reading") :data-components (count values)}
      (for [[i v] (map-indexed vector values)]
        (with-meta (component-reading schema labels i v push?) {:key i}))]
     [:svg {:view-box "0 0 100 44" :preserve-aspect-ratio "none" :role "img" :style {:overflow "hidden"}
            :aria-label (str "Last " window " beats")}
      [:g {:ref attach}
       [widgets/grid-lines grid :x 0 44]
       (for [[i d] (map-indexed vector paths)]
         ^{:key i} [:path {:d d :stroke (colors i) :fill "none" :stroke-width 1.6
                           :vector-effect "non-scaling-stroke"}])]]
     [:figcaption [:span (if bounds (domain-label schema bounds) "Awaiting samples")]
      [:span (str window " beats")]]
     (widgets/grid-period grid)]))

(defn history-motion [observations window]
  (when (<= 2 (count observations))
    (let [prior (nth observations (- (count observations) 2)) last (peek observations)
          beats (- (:beat last) (:beat prior)) elapsed (- (:t last) (:t prior))]
      (when (and (= (:source prior) (:source last)) (< 0 beats window) (<= 1 elapsed 1000))
        {:beats beats :duration elapsed :offset (* 100 (/ beats window))}))))

(defn- scrolling-history [schema history window timing period-beats]
  (r/with-let [element (atom nil) animation (atom nil) seen (atom nil)
               attach (fn [node] (reset! element node) nil)]
    (let [current (peek history) position [(:source current) window (:beat current)]
          {:keys [beats duration offset]} (history-motion history window)]
      (r/after-render
        (fn []
          (when-let [node @element]
            (when (not= @seen position)
              (when-let [active @animation] (.cancel active))
              (let [[source prior-window prior-beat] @seen]
                (when (and offset (= source (:source current)) (= prior-window window)
                           (< prior-beat (:beat current))
                           (not (.-matches (js/matchMedia "(prefers-reduced-motion: reduce)")))
                           (fn? (aget node "animate")))
                  (reset! animation (.animate node
                                      #js [#js {:transform (str "translateX(" offset "px)")}
                                           #js {:transform "translateX(0px)"}]
                                      #js {:duration duration :easing "linear"}))))
              (reset! seen position)))))
      [history-chart schema history window attach beats false
       (chart-grid history window beats timing period-beats)])
    (finally (when-let [active @animation] (.cancel active)))))

(def selected-mode views/selected-mode)

(def ^:private push-mode views/push-view)

(defn- observe-parameter [previous clock preference {:keys [schema effective-value value param id] :as parameter}]
  (when (= :active (tag (:source-state parameter)))
    (let [id (or param id) values (components schema (if (some? effective-value) effective-value value))]
      (case (push-mode parameter preference)
        :history [id (append-sample (get previous id []) clock values)]
        :spatial [id (spatial-keyframe (get previous id) clock values)]
        nil))))

(defn- history-window [beats] (or (some #{beats} zooms) 4))

(defn- push-grids
  "Project the grid of each visible signal once per supplied view: the cycle
  preview from its own timing and the history from the observed window."
  [timing window observations parameters]
  (into {} (keep (fn [{:keys [param id signal-timing source-period-beats]}]
                   (let [key (or param id)
                         history (get observations key)
                         preview (signal-grid timing signal-timing)
                         plotted (when (vector? history) (chart-grid history window 0 timing source-period-beats))
                         grids (cond-> {} preview (assoc :preview preview) plotted (assoc :history plotted))]
                     (when (seq grids) [key grids]))))
        parameters))

(defn update-push [previous next view]
  (let [identity (mapv #(get view %) [:runtime-id :epoch :instance-id :definition-version])
        preference (:parameter-view view) clock (:visualization-clock view)
        same? (and (= identity (:parameter-identity previous)) (= preference (:parameter-view previous)))
        previous-fixtures (into {} (map (juxt #(or (:param %) (:id %)) :sample-fixture)) (:parameters previous))
        retained (when same?
                   (select-keys (:parameter-observations previous)
                                (keep (fn [{:keys [param id sample-fixture]}]
                                        (let [key (or param id)]
                                          (when (= sample-fixture (get previous-fixtures key)) key)))
                                      (:parameters next))))
        visible? (and clock (#{nil :parameters "parameters"} (:display-mode next))
                      (not (get-in next [:target-picker :open?])))
        entries (when visible?
                  (into {} (keep #(observe-parameter retained clock preference %)) (:parameters next)))
        timing (:musical-timing view)
        grids (push-grids timing (history-window (:history-beats view)) entries (:parameters next))]
    (cond-> (assoc next :parameter-view preference :history-beats (:history-beats view) :visualization-clock clock
                   :musical-timing timing)
      (seq grids) (assoc :parameter-grids grids)
      (seq entries) (assoc :parameter-identity identity :parameter-observations entries))))

(defn push [view {:keys [schema effective-value value param id preview-samples] :as parameter} & [touched?]]
  (let [shown (if (some? effective-value) effective-value value)]
    (case (push-mode parameter (:parameter-view view))
      :color [widgets/color-view shown {:selected (:component-label parameter)}]
      :spatial [space schema shown
                (if-let [keyframe (get-in view [:parameter-observations (or param id)])]
                  (spatial-value schema keyframe (get-in view [:visualization-clock :time-ms]))
                  (components schema shown)) true]
      :history [history-chart schema (get-in view [:parameter-observations (or param id)])
                (history-window (:history-beats view)) nil nil true
                (get-in view [:parameter-grids (or param id) :history])]
      (let [grid (get-in view [:parameter-grids (or param id) :preview])]
        (if-let [trace (when (and (not touched?) (#{:number :integer :choice :enum} (widgets/parameter-kind schema)))
                         (widgets/sparkline preview-samples grid))]
          (into [:div.push-signal
                 trace
                 [:output.push-signal-value (widgets/value-label schema shown)
                  (when-let [unit (widgets/unit-label schema)] [:small unit])]]
                (remove nil? [(widgets/grid-period grid)]))
          [widgets/typed-display parameter])))))

(rf/reg-sub ::clock
  (fn [db _]
    (let [show (:show db)]
      {:time-ms (or (get-in show [:transport :time-ms]) (:time-ms show))
       :beat (get-in show [:transport :beat])
       :source [(:runtime-id show) (:epoch show)]
       :sample-period-ms (get-in show [:visualization-clock :sample-period-ms])
       ;; The committed transport meter and origins place the musical grid.
       :timing (select-keys (:transport show) [:meter :bar-origin-beat :phrase-origin-beat])})))

(defn- record! [this]
  (let [[_ mode schema value clock observed] (r/argv this)
        values (components schema value)]
    (case mode
      :spatial (swap! observed spatial-keyframe clock values)
      ;; A colour shows the committed value itself and keeps no samples.
      :color nil
      ;; The browser already has these committed samples. The cue-pad sampling
      ;; governor governs additional host work, including Push snapshots.
      (swap! observed append-sample clock values 0 1024))))

(defn observed-view
  "The mounted Studio scope for one committed sample of a parameter. Options
  carry the period of the driving source for the musical grid."
  [mode schema value clock observed window & [options]]
  (case mode
    :spatial [space schema value (spatial-value schema @observed (:time-ms clock))]
    :color [widgets/color-view value]
    [scrolling-history schema @observed window (:timing clock) (:period-beats options)]))

(def observed-visual
  (r/create-class
   {:component-did-mount record!
    :component-did-update (fn [this _] (record! this))
    :reagent-render observed-view}))

(defn- visible-visual [mode schema value window options]
  (r/with-let [observed (r/atom (when (= mode :history) []))]
    (let [sample (if (fn? value) (value) value)]
      (if (some? sample)
        [observed-visual mode schema sample @(rf/subscribe [::clock]) observed window options]
        [:span.block-trace-empty "Awaiting sample…"]))))

(defn monitor
  "A single visible scope per performance strip. Sampling starts only when the
  scope intersects the viewport and stops when it leaves."
  [schema value & [options]]
  (let [mode (views/monitor-view schema)]
    [watch-visibility
     (fn [visible?]
       (when visible?
         ^{:key mode} [visible-visual mode schema value 4 options]))]))

(def history-toggle views/history-toggle)

(def ^:private view-titles {:spatial "3D" :color "Colour" :history "History"})

(defn studio [schema value & [options]]
  (r/with-let [choice (r/atom :auto) window (r/atom 4)]
    (let [mode (views/studio-view schema @choice)]
      [:div.parameter-visualizer
       [:div.parameter-views
        (for [view (views/studio-choices schema)]
          ^{:key view}
          [:button {:type "button" :aria-pressed (= mode view)
                    :on-click (if (= :history view)
                                #(swap! choice (partial history-toggle schema))
                                #(reset! choice view))}
           (view-titles view)])
        (when (= mode :history)
          [:label "Window " [:select {:value @window :aria-label "Parameter history beat window"
                                     :on-change #(reset! window (js/Number (.. % -target -value)))}
                            (for [zoom zooms] ^{:key zoom} [:option {:value zoom} (str zoom " beats")])]])]
       (when (#{:spatial :color :history} mode)
         [watch-visibility
          (fn [visible?] (when visible?
                           ^{:key mode} [visible-visual mode schema value @window options]))])])))
