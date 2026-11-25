(ns tolglow.studio.parameter-widgets
  "Pure presentation shared by the web inspector and device displays."
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [tolglow.studio.numeric-display :as numeric]
            [tolglow.visualization.musical-grid :as musical-grid]
            [tolglow.visualization.parameter-views :as views]
            [thi.ng.color.core :as color]))

(defn label [value]
  (str/replace (cond (keyword? value) (name value) (nil? value) "—" :else (str value)) #"[-_]" " "))

(def finite? numeric/finite?)

(def number-label numeric/number-label)

(defn value-label [schema value]
  (or (some #(when (= value (:value %)) (or (:label %) (label (:id %)))) (:choices schema))
      (cond (boolean? value) (if value "On" "Off")
            (number? value) (number-label schema value)
            (sequential? value) (str/join " · " (map #(if (number? %) (number-label schema %) (label %)) value))
            :else (label value))))

(defn color-components [value]
  (if (satisfies? color/IRGBConvert value)
    (let [{:keys [r g b a]} (color/as-rgba value)] [r g b a])
    value))

(defn color-css [value]
  (cond
    (satisfies? color/ICSSConvert value) (:col (color/as-css value))
    (and (string? value) (re-matches #"#[a-fA-F0-9]{3}(?:[a-fA-F0-9]{3})?(?:[a-fA-F0-9]{2})?" value)) value
    (and (sequential? value) (<= 3 (count value)) (every? finite? (take 3 value)))
    (str "rgb(" (str/join "," (map #(js/Math.round (* 255 (max 0 (min 1 %)))) (take 3 value))) ")")
    :else "#b6c9a9"))

(defn swatch-css [value]
  (if (string? value) value (color-css value)))

(defn proportion [schema value]
  (let [[low high] (:bounds schema)]
    (when (and (finite? value) (finite? low) (finite? high) (< low high))
      (max 0 (min 1 (/ (- value low) (- high low)))))))

(defn sample-points
  "Bound and normalize real supplied samples. Missing/nonfinite samples produce
  no trace, and a constant signal remains centered. No synthetic samples."
  [samples]
  (let [values (->> samples (take 256) (map #(if (map? %) (:value %) %)) vec)]
    (when (and (< 1 (count values)) (every? finite? values))
      (let [low (apply min values) high (apply max values) span (- high low)]
        (str/join " " (map-indexed (fn [i value]
                                    (str (* 100 (/ i (dec (count values)))) ","
                                         (if (pos? span) (- 28 (* 24 (/ (- value low) span))) 16))) values))))))

(defn grid-lines
  "Draw a projected musical grid. `x` places a line in chart units; the unit
  of each line is its class, so the stylesheet owns the tint of a beat, a bar
  and a phrase."
  [spec x y1 y2]
  (when (seq (:lines spec))
    [:g.musical-grid {:data-minor (some-> (:minor spec) name) :data-major (some-> (:major spec) name)}
     (for [[index line] (map-indexed vector (:lines spec))]
       ^{:key index} [:line {:class (name (:unit line)) :x1 (x line) :x2 (x line) :y1 y1 :y2 y2
                             :vector-effect "non-scaling-stroke"}])]))

(defn grid-period
  "The signal period of a grid in its largest whole unit, tinted like its lines."
  [spec]
  (when-let [text (musical-grid/measure-text (:period spec))]
    [:small.grid-period {:data-unit (name (get-in spec [:period :unit])) :title (str "Signal period " text)} text]))

(defn- chart-position [line] (* 100 (:position line)))

(defn sparkline
  "A supplied signal trace. An optional projected grid draws behind it."
  [samples & [grid]]
  (when-let [points (sample-points samples)]
    [:svg.parameter-sparkline {:view-box "0 0 100 32" :preserve-aspect-ratio "none" :role "img"
                               :aria-label (str "Signal preview from supplied samples"
                                                (when-let [period (musical-grid/measure-text (:period grid))]
                                                  (str ", period " period)))}
     [grid-lines grid chart-position 0 32]
     [:polyline {:points points :fill "none" :stroke "currentColor" :stroke-width 1.6 :vector-effect "non-scaling-stroke"}]]))

(defn duration-label [duration]
  (if-let [[unit amount] (when (map? duration) (first duration))]
    (str (number-label amount) " " (label unit)) "—"))

(defn category-tags [categories colors]
  [:span.category-tags
   (for [category (take 8 categories)]
     ^{:key (str category)}
     [:span.category-tag {:style {"--category-color" (swatch-css (or (get colors category)
                                                                    (get colors (keyword (str category))) "#a8b99a"))}}
      (label category)])])

(def parameter-kind views/parameter-kind)

(defn unit-label [schema]
  (when-let [unit (or (:unit schema) (:units schema))] (label unit)))

(defn numeric-range [schema]
  (let [[low high] (:bounds schema)]
    (when (and (finite? low) (finite? high) (< low high)) [low high])))

(defn normalized-value [schema amount]
  (when-let [[low high] (numeric-range schema)]
    (let [value (+ low (* (max 0 (min 1 amount)) (- high low)))]
      (if (= :integer (parameter-kind schema)) (max low (min high (js/Math.round value))) value))))

(defn keyboard-value [schema value key]
  (when-let [[low high] (numeric-range schema)]
    (let [step (if (= :integer (parameter-kind schema)) 1 (/ (- high low) 100))
          next (case key ("ArrowRight" "ArrowUp") (+ value step)
                     ("ArrowLeft" "ArrowDown") (- value step)
                     "PageUp" (+ value (* 10 step)) "PageDown" (- value (* 10 step))
                     "Home" low "End" high nil)]
      (when (some? next) (max low (min high next))))))

(defn pointer-proportion
  "A 270-degree dial with its inactive gap at the bottom. No clock or state."
  [x y]
  (let [degrees (mod (+ 360 (* (/ 180 js/Math.PI) (js/Math.atan2 y x))) 360)
        angle (if (< degrees 90) (+ degrees 360) degrees)]
    (max 0 (min 1 (/ (- angle 135) 270)))))

(defn dial-point [amount radius]
  (let [angle (* (/ js/Math.PI 180) (+ 135 (* 270 amount)))]
    [(+ 50 (* radius (js/Math.cos angle))) (+ 44 (* radius (js/Math.sin angle)))]))

(defn arc-path [start end radius]
  (let [[x y] (dial-point start radius) [a b] (dial-point end radius)]
    (str "M" x "," y " A" radius "," radius " 0 " (if (> (* 270 (js/Math.abs (- end start))) 180) 1 0)
         " " (if (< start end) 1 0) " " a "," b)))

(defn dial-model [schema value]
  (when-let [[low high] (numeric-range schema)]
    (when-let [amount (proportion schema value)]
      (let [discrete? (= :integer (parameter-kind schema))
            count (if (and discrete? (<= (- high low) 12)) (inc (- high low)) 11)]
        {:amount amount :origin (if (< low 0 high) (proportion schema 0) 0)
         :bipolar? (< low 0 high) :bounds [low high]
         :ticks (mapv #(/ % (max 1 (dec count))) (range count))}))))

(defn rotary
  "Shared numeric geometry. Base/effective markers require supplied values;
  the component never generates modulation or samples an effect."
  [schema value {:keys [base-value effective-value strip?]}]
  (let [shown (if (finite? effective-value) effective-value value)
        model (dial-model schema shown) base (proportion schema base-value)
        unit (unit-label schema)]
    [:svg.parameter-dial {:view-box "0 0 100 94" :role "img"
                          :aria-label (str (value-label schema shown) (when unit (str " " unit))
                                           (when-let [[low high] (:bounds model)] (str ", range " low " to " high)))}
     (when model
       [:<>
        (for [[index position] (map-indexed vector (:ticks model))
              :let [[x y] (dial-point position 38) [a b] (dial-point position 40)]]
          ^{:key index} [:line.dial-tick {:x1 x :y1 y :x2 a :y2 b}])
        [:path.dial-track {:d (arc-path 0 1 32)}]
        (when (not= (:origin model) (:amount model))
          [:path.dial-value {:d (arc-path (:origin model) (:amount model) 32)}])
        (when (:bipolar? model)
          (let [[x y] (dial-point (:origin model) 27) [a b] (dial-point (:origin model) 37)]
            [:line.dial-zero {:x1 x :y1 y :x2 a :y2 b}]))
        (when (and base (not= base (:amount model)))
          (let [[x y] (dial-point base 27) [a b] (dial-point base 37)]
            [:line.dial-baseline {:x1 x :y1 y :x2 a :y2 b}
             [:title (str "Base " (number-label schema base-value))]]))
        (let [[x y] (dial-point (:amount model) 32)]
          [:circle.dial-position {:cx x :cy y :r 3}])
        [:text.dial-bound {:x 5 :y 90 :text-anchor "start"} (number-label (first (:bounds model)))]
        [:text.dial-bound {:x 95 :y 90 :text-anchor "end"} (number-label (second (:bounds model)))]])
     [:text.dial-number {:x 50 :y (if (and strip? (nil? unit)) 54 48) :text-anchor "middle"} (number-label schema shown)]
     (when (or unit (not strip?))
       [:text.dial-unit {:x 50 :y 63 :text-anchor "middle"} (or unit (if (= :integer (parameter-kind schema)) "steps" "value"))])]))

(defn rotary-control [schema value change! options]
  (let [[low high] (numeric-range schema)
        change-pointer! (fn [event]
                          (let [bounds (.getBoundingClientRect (.-currentTarget event))
                                x (- (.-clientX event) (.-left bounds) (/ (.-width bounds) 2))
                                y (- (.-clientY event) (.-top bounds) (* (.-height bounds) (/ 44 94)))]
                            (when-let [next (normalized-value schema (pointer-proportion x y))] (change! next))))]
    [:div.parameter-rotary-control
     {:role "slider" :tab-index 0 :aria-label (or (:name options) (:name schema) "Parameter")
      :aria-valuemin low :aria-valuemax high :aria-valuenow value
      :aria-valuetext (str (number-label schema value) (when-let [unit (unit-label schema)] (str " " unit)))
      :on-key-down (fn [event] (when-let [next (keyboard-value schema value (.-key event))]
                                (.preventDefault event)
                                (if-let [key-change! (:key-change options)] (key-change! (.-key event)) (change! next))))
      :on-pointer-down (fn [event]
                         (when (or (= "touch" (.-pointerType event)) (= 0 (.-button event)))
                           (.preventDefault event) (.focus (.-currentTarget event))
                           (.setPointerCapture (.-currentTarget event) (.-pointerId event))
                           (change-pointer! event)))
      :on-pointer-move (fn [event]
                         (when (.hasPointerCapture (.-currentTarget event) (.-pointerId event))
                           (change-pointer! event)))
      :on-pointer-up (fn [event]
                       (when (.hasPointerCapture (.-currentTarget event) (.-pointerId event))
                         (.releasePointerCapture (.-currentTarget event) (.-pointerId event))))}
     [rotary schema value options]]))

(defn axes-control [title schemas values change!]
  (r/with-let [popover-id (str "axes-" (random-uuid))]
    [:div.axis-control
     [:button.axis-trigger {:type "button" :popoverTarget popover-id :aria-haspopup "dialog"
                            :aria-label (str "Edit " title " components")}
      (for [[i schema] (map-indexed vector schemas)]
        ^{:key i} [:span [:b (:axis-label schema)] (number-label schema (get values i))])]
     [:div.axis-popover {:id popover-id :popover "auto" :role "dialog" :aria-label (str title " components")}
      [:header [:strong title]
       [:button.text-button {:type "button" :popoverTarget popover-id :popoverTargetAction "hide"} "Done"]]
      [:div.axis-dials
       (for [[i schema] (map-indexed vector schemas)
             :let [v (get values i) axis (:axis-label schema) text (str title " " axis)]]
         ^{:key i}
         [:div.axis-component
          [:label axis]
          (when (numeric-range schema)
            [rotary-control schema v #(change! i %) {:name (str text " rotary")}])
          [numeric/number-input {:display-schema schema :type "number" :aria-label (str text " value") :value (or v "") :step "any"
                   :min (first (:bounds schema)) :max (second (:bounds schema))
                   :on-change (fn [e] (let [n (.. e -target -valueAsNumber)]
                                        (when (and (finite? n) (.. e -target -validity -valid))
                                          (change! i n))))}]])]]]))

(defn vector-control [title schema values change!]
  (let [labels (:component-labels schema ["X" "Y" "Z" "W"])
        axes (mapv (fn [i] (assoc schema :type "number" :axis-label (get labels i (str (inc i)))))
                   (range (or (:size schema) (count values))))]
    [axes-control title axes values change!]))

(defn- component-track [title schema value color]
  [:div.typed-component {:style (when color {"--widget-accent" color})}
   [:span.typed-component-name title]
   [:div.typed-track
    [:i {:style {:width (str (* 100 (or (proportion schema value) 0)) "%")}}]]
   [:output (number-label schema value)]])

(defn- unit [value] (if (finite? value) (max 0 (min 1 value)) 0))

(defn hsv
  "Hue, saturation and brightness of RGB components, each in [0,1]. These are
  the components a Push encoder edits on a colour."
  [[r g b]]
  (let [hi (max r g b) lo (min r g b) delta (- hi lo)
        hue (cond (zero? delta) 0
                  (= hi r) (mod (/ (- g b) delta) 6)
                  (= hi g) (+ 2 (/ (- b r) delta))
                  :else (+ 4 (/ (- r g) delta)))]
    [(/ hue 6) (if (zero? hi) 0 (/ delta hi)) hi]))

(defn hsv-rgb [[h s v]]
  (let [sector (* 6 (- h (js/Math.floor h)))
        channel (fn [n] (let [k (mod (+ n sector) 6)]
                          (- v (* v s (max 0 (min k (- 4 k) 1))))))]
    [(channel 5) (channel 3) (channel 1)]))

(def ^:private color-spaces
  {:rgb [[:red "R"] [:green "G"] [:blue "B"]]
   :hsv [[:hue "H"] [:saturation "S"] [:brightness "V"]]})

(def ^:private component-names
  {"r" :red "g" :green "b" :blue "red" :red "green" :green "blue" :blue
   "h" :hue "s" :saturation "v" :brightness "hue" :hue "saturation" :saturation "brightness" :brightness})

(defn color-component
  "The colour component a control names, such as a Push encoder's `hue`."
  [id]
  (when (or (keyword? id) (string? id)) (get component-names (str/lower-case (name id)))))

(defn- gradient-css [stops]
  (str "linear-gradient(90deg," (str/join "," (map color-css stops)) ")"))

(defn color-tracks
  "Three component tracks for RGB components. Each gradient shows the colours
  that component alone would produce, with the current amount marked. A hue,
  saturation or brightness selection switches the tracks to that space."
  [components selected]
  (let [rgb (mapv unit (take 3 components))
        [h s v :as polar] (hsv rgb)
        component (color-component selected)
        space (if (some #(= component (first %)) (:hsv color-spaces)) :hsv :rgb)
        amounts (if (= :hsv space) polar rgb)
        stops (fn [index]
                (if (= :hsv space)
                  (case index
                    0 (mapv #(hsv-rgb [(/ % 6) (max s 0.6) (max v 0.6)]) (range 7))
                    1 [(hsv-rgb [h 0 v]) (hsv-rgb [h 1 v])]
                    [(hsv-rgb [h s 0]) (hsv-rgb [h s 1])])
                  [(assoc rgb index 0) (assoc rgb index 1)]))]
    (mapv (fn [index [id label]]
            {:id id :label label :amount (nth amounts index) :selected? (= id component)
             :gradient (gradient-css (stops index))})
          (range) (get color-spaces space))))

(defn component-amount
  "The amount of the colour component a control names, else the indexed RGB one."
  [components selected index]
  (when (and (<= 3 (count components)) (every? finite? (take 3 components)))
    (or (some #(when (:selected? %) (:amount %)) (color-tracks components selected))
        (get components index))))

(defn gradient-stops-css
  "CSS for authored gradient stops, or nil when the value is not a gradient."
  [value]
  (when (and (sequential? value) (seq value) (every? #(and (map? %) (finite? (:position %))) value))
    (if (= 1 (count value))
      (color-css (:color (first value)))
      (str "linear-gradient(90deg,"
           (str/join "," (map #(str (color-css (:color %)) " " (* 100 (unit (:position %))) "%") value)) ")"))))

(defn color-view
  "The colour widget shared by Studio and Push: a swatch of the value and one
  gradient track per component. Authored gradient stops draw as one ramp."
  [value & [{:keys [selected]}]]
  (if-let [ramp (gradient-stops-css value)]
    [:div.parameter-color.gradient {:role "img" :aria-label (str "Gradient with " (count value) " stops")}
     [:div.color-swatch {:style {:background ramp}}]
     [:div.color-readout (str (count value) (if (= 1 (count value)) " stop" " stops"))]]
    (let [components (vec (take 4 (color-components value)))]
      (if-not (and (<= 3 (count components)) (every? finite? components))
        [:span.parameter-no-sample {:role "status"} "Awaiting value"]
        (let [tracks (color-tracks components selected)
              readout (str/join " · " (map #(number-label {:bounds [0 1]} %) (take 3 components)))]
          [:div.parameter-color {:role "img" :aria-label (str "Colour, red green blue " readout)
                                 :data-space (if (#{:hue :saturation :brightness} (:id (first tracks))) "hsv" "rgb")}
           [:div.color-swatch {:style {:background (color-css components)}}
            (when (and (= 4 (count components)) (not= 1 (nth components 3)))
              [:small (str "α " (number-label (nth components 3)))])]
           [:div.color-channels
            (for [{:keys [id label amount gradient selected?]} tracks]
              ^{:key id}
              [:div.color-channel {:class (when selected? "selected") :data-component (name id)}
               [:span.color-channel-name label]
               [:div.color-track {:style {:background gradient}}
                [:i {:style {:left (str (* 100 (unit amount)) "%")}}]]
               [:output (number-label {:bounds [0 1]} amount)]])]
           [:div.color-readout [:b "RGB"] readout]])))))

(defn- choice-view [schema value]
  (let [choices (vec (or (:choices schema) (map #(hash-map :value % :label (label %)) (or (:values schema) (:options schema)))))
        selected (first (keep-indexed #(when (= value (:value %2)) %1) choices))
        count-choices (count choices)
        start (max 0 (min (max 0 (- count-choices 3)) (dec (or selected 0))))]
    [:div.typed-choice
     [:div.typed-choice-value (value-label schema value)]
     [:div.typed-choice-options
      (for [[index choice] (map-indexed vector (take 3 (drop start choices)))]
        ^{:key index} [:span {:class (when (= value (:value choice)) "selected")}
                      (or (:label choice) (label (:id choice)) (label (:value choice)))])]
     [:span.typed-choice-position (str (if (some? selected) (inc selected) "—") " / " count-choices)]]))

(defn typed-display [{:keys [schema value preview-samples base-value effective-value component-label]}]
  (let [kind (parameter-kind schema) shown (if (some? effective-value) effective-value value)]
    [:div.typed-parameter {:data-type (name kind)}
     (case kind
       (:number :integer) [rotary schema value {:base-value base-value :effective-value effective-value}]
       (:choice :enum) [choice-view schema shown]
       :boolean [:div.typed-boolean [:div.typed-switch {:class (when shown "on")} [:i]]
                 [:strong (if shown "On" "Off")] [:span "toggle"]]
       (:color :gradient) [color-view shown {:selected component-label}]
       :range (let [[low high] shown a (proportion schema low) b (proportion schema high)]
                [:div.typed-range
                 [:strong (str (number-label low) " — " (number-label high))]
                 [:div.typed-range-track
                  (when (and a b) [:i {:style {:left (str (* 100 a) "%") :width (str (* 100 (- b a)) "%")}}])]
                 [:div.typed-range-bounds [:span (number-label (first (:bounds schema)))]
                  [:span (number-label (second (:bounds schema)))]]
                 [:span.typed-range-unit (or (unit-label schema) "range")]])
       :vector [:div.typed-vector
                (for [[index component] (map-indexed vector (take 4 shown))]
                  ^{:key index} [component-track (get (or (:component-labels schema) ["X" "Y" "Z" "W"]) index)
                                 schema component nil])
                [:div.typed-range-bounds [:span (number-label (first (:bounds schema)))]
                 [:span (number-label (second (:bounds schema)))]]]
       [:div.typed-choice-value (value-label schema shown)])
     (when (seq preview-samples) [sparkline preview-samples])]))

(def audio-band-colors {:low [68 145 235] :mid [245 163 70] :high [235 242 239]})

(defn audio-bin-color [bands]
  (let [weights (mapv #(let [v (get bands %)] (if (and (finite? v) (pos? v)) v 0)) [:low :mid :high])
        total (reduce + weights)]
    (if (zero? total) "rgb(100,115,108)"
      (str "rgb(" (str/join "," (for [channel (range 3)]
                                   (js/Math.round (/ (reduce + (map * weights (map #(get (get audio-band-colors %) channel) [:low :mid :high]))) total)))) ")"))))

(defn audio-bins [waveform]
  (let [bins (:bins waveform)]
    (when (and (vector? bins) (<= 1 (count bins) 512)
               (every? (fn [{:keys [min max rms]}]
                         (and (finite? min) (finite? max) (<= -1 min max 1)
                              (finite? rms) (<= 0 rms 1))) bins)) bins)))

(defn audio-waveform [waveform]
  (if-let [bins (audio-bins waveform)]
    [:div.audio-waveform {:data-source (:source-id waveform) :data-sample-end (:sample-end waveform)
                          :aria-label "Captured audio waveform"}
     [:svg {:view-box "0 0 512 100" :preserve-aspect-ratio "none" :role "img"
             :aria-label (str "Captured PCM, " (count bins) " peak windows; fixed amplitude minus one to one")}
      [:line.audio-waveform-zero {:x1 0 :x2 512 :y1 50 :y2 50}]
      (for [[index {:keys [bands] minimum :min maximum :max}] (map-indexed vector bins)
            :when (or (not= minimum maximum) (not (zero? maximum)))
            :let [column-width (/ 512 (count bins))
                  x (* column-width index) y (- 50 (* 48 maximum))
                  width (max 0.5 (- column-width 0.3))
                  color (audio-bin-color bands)]]
        (if (= minimum maximum)
          ^{:key index} [:line {:x1 x :x2 (+ x width) :y1 y :y2 y :stroke color}]
          ^{:key index} [:rect {:x x :y y :width width :height (* 48 (- maximum minimum)) :fill color}]))
      (when (and (finite? (:duration-ms waveform)) (pos? (:duration-ms waveform)))
        (for [[index marker] (map-indexed vector (take 128 (:beat-markers waveform)))
              :when (and (finite? (:offset-ms marker)) (<= 0 (:offset-ms marker) (:duration-ms waveform)))
              :let [x (* 512 (/ (:offset-ms marker) (:duration-ms waveform)))]]
          ^{:key index} [:line {:x1 x :x2 x :y1 0 :y2 100 :class (if (:bar? marker) "audio-bar-marker" "audio-beat-marker")}]))]
     [:div.audio-waveform-legend
      [:span.audio-band-low "Low"] [:span.audio-band-mid "Mid"] [:span.audio-band-high "High"]
      [:span (str (number-label (:duration-ms waveform)) " ms · " (number-label (:sample-rate waveform)) " Hz")]]]
    [:div.audio-waveform-empty "No captured audio samples"]))
