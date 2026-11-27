(ns tolglow.studio.parameter-widgets
  "Pure presentation shared by the web inspector and device displays."
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [tolglow.authoring.parameter-controls :as controls]
            [tolglow.studio.numeric-display :as numeric]
            [tolglow.visualization.control-geometry :as geometry]
            [tolglow.visualization.musical-grid :as musical-grid]
            [tolglow.visualization.parameter-views :as views]
            [tolglow.visualization.waveform-shapes :as shapes]
            [thi.ng.color.core :as color]))

(def label controls/label)

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

(def unit-label controls/unit-label)

(def numeric-range controls/bounds)

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

(defn- ember-dial
  "The tolglow-web knob face: a rust pie wedge from the origin to the value on
  a dark surface, the bottom gap in slate, coloured limit lines, a glowing bar
  pointer and a dark centre bullet."
  [id schema shown model base base-value unit]
  (let [{:keys [amount origin]} model
        url #(str "url(#" id "-" % ")")]
    [:<>
     [:defs
      [:linearGradient {:id (str id "-fill") :x1 "0%" :y1 "30%" :x2 "50%" :y2 "100%"}
       ;; Rust by default; a ring's sub-knob fills in its ring colour.
       [:stop {:offset "0%" :style {:stop-color "var(--ember-from, rgb(160 75 65 / .85))"}}]
       [:stop {:offset "100%" :style {:stop-color "var(--ember-to, rgb(190 45 40 / .7))"}}]]
      [:linearGradient {:id (str id "-surface") :x1 "0" :y1 "0" :x2 "0" :y2 "1"}
       [:stop {:offset "0%" :stop-color "rgb(50,56,61)"}]
       [:stop {:offset "100%" :stop-color "rgb(29,32,36)"}]]
      [:filter {:id (str id "-glow") :x "-60%" :y "-30%" :width "220%" :height "160%"}
       [:feDropShadow {:dx -2.5 :dy 0 :stdDeviation 2.6 :flood-color "rgb(150,70,45)" :flood-opacity 0.75}]]]
     [:circle.ember-surface {:cx 50 :cy 44 :r 40 :fill (url "surface")}]
     [:path.ember-gap {:d (arc-path 1 (+ 1 (/ 90 geometry/sweep)) 20)}]
     (when (not= origin amount)
       [:path.ember-fill {:d (arc-path origin amount 20) :stroke (url "fill")}])
     (let [[x y] (dial-point 0 40)] [:line.ember-limit.low {:x1 50 :y1 44 :x2 x :y2 y}])
     (let [[x y] (dial-point 1 40)] [:line.ember-limit.high {:x1 50 :y1 44 :x2 x :y2 y}])
     (when (and base (not= base amount))
       (let [[x y] (dial-point base 30) [a b] (dial-point base 40)]
         [:line.dial-baseline {:x1 x :y1 y :x2 a :y2 b}
          [:title (str "Base " (number-label schema base-value))]]))
     [:rect.ember-pointer {:x -3 :y -39 :width 6 :height 32 :rx 3 :filter (url "glow")
                           :transform (str "translate(50 44) rotate(" (geometry/pointer-rotation amount) ")")}]
     [:circle.ember-halo {:cx 50 :cy 44 :r 11}]
     [:circle.ember-bullet {:cx 50 :cy 44 :r 7.5}]
     [:text.ember-number {:x 50 :y 80 :text-anchor "middle"} (number-label schema shown)]
     (when unit [:text.ember-unit {:x 50 :y 90 :text-anchor "middle"} unit])
     [:text.dial-bound {:x 5 :y 90 :text-anchor "start"} (number-label (first (:bounds model)))]
     [:text.dial-bound {:x 95 :y 90 :text-anchor "end"} (number-label (second (:bounds model)))]]))

(defn- standard-dial [schema shown model base base-value unit strip?]
  [:<>
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
     [:text.dial-unit {:x 50 :y 63 :text-anchor "middle"} (or unit (if (= :integer (parameter-kind schema)) "steps" "value"))])])

(defn rotary
  "Shared numeric geometry. Base/effective markers require supplied values;
  the component never generates modulation or samples an effect. `:skin`
  :ember draws the tolglow-web knob face over the same 270-degree geometry."
  [schema value {:keys [base-value effective-value strip? skin]}]
  (r/with-let [id (str "knob-" (random-uuid))]
    (let [shown (if (finite? effective-value) effective-value value)
          model (dial-model schema shown) base (proportion schema base-value)
          unit (unit-label schema)
          ember? (and model (= :ember skin))]
      [:svg.parameter-dial {:view-box "0 0 100 94" :role "img" :data-skin (when ember? "ember")
                            :aria-label (str (value-label schema shown) (when unit (str " " unit))
                                             (when-let [[low high] (:bounds model)] (str ", range " low " to " high)))}
       (if ember?
         [ember-dial id schema shown model base base-value unit]
         [standard-dial schema shown model base base-value unit strip?])])))

(defn- capture! [event]
  (when (or (= "touch" (.-pointerType event)) (= 0 (.-button event)))
    (.preventDefault event)
    (.focus (.-currentTarget event))
    (.setPointerCapture (.-currentTarget event) (.-pointerId event))
    true))

(defn- captured? [event] (.hasPointerCapture (.-currentTarget event) (.-pointerId event)))

(defn- release! [event]
  (when (captured? event) (.releasePointerCapture (.-currentTarget event) (.-pointerId event))))

(defn wheel-value
  "One wheel event on an Ember knob, as {:value :remainder}, or nil when it
  does not turn the knob. The turn is proportional to vertical scroll
  distance; an integer carries the distance remainder until it makes a whole
  step."
  [schema value pixels remainder]
  (when-let [[low high] (numeric-range schema)]
    (when (and (finite? value) (finite? pixels))
      (if (= :integer (parameter-kind schema))
        (let [[steps remainder] (geometry/wheel-steps (or remainder 0) pixels)]
          {:value (max low (min high (+ value steps))) :remainder remainder})
        (some->> (proportion schema value) (#(geometry/wheel-amount % pixels)) (normalized-value schema)
                 (hash-map :remainder 0 :value))))))

(defn rotary-control
  "A rotary that edits. Dragging sets the absolute angle and a drag past
  either end lands on its bound. With the :ember skin a fresh press in the
  bottom gap holds the value, and the wheel turns a focused knob in proportion
  to vertical scroll distance. Unfocused knobs, horizontal and Shift+wheel
  scrolling leave the page to scroll. Keys step like a native slider."
  [schema value change! options]
  (r/with-let [latest (atom nil)
               node (atom nil)
               ;; The value this knob last sent and the value it was turned
               ;; from, so several wheel events inside one frame accumulate.
               sent (atom nil)
               remainder (atom 0)
               ;; React registers wheel listeners as passive; the ember knob
               ;; must cancel page scrolling, so it owns a non-passive one.
               on-wheel (fn [event]
                          (let [{:keys [schema value change! options]} @latest
                                pixels (geometry/wheel-pixels (.-deltaX event) (.-deltaY event) (.-deltaMode event))]
                            (when (and (= :ember (:skin options)) pixels
                                       (identical? @node (.-activeElement js/document)))
                              (.preventDefault event)
                              (let [from (if (= value (:from @sent)) (:to @sent) value)]
                                (when-let [{next :value carried :remainder} (wheel-value schema from pixels @remainder)]
                                  (reset! remainder carried)
                                  (when (not= next from)
                                    (reset! sent {:from value :to next})
                                    (change! next)))))))
               attach! (fn [element]
                         (some-> @node (.removeEventListener "wheel" on-wheel))
                         (reset! node element)
                         (some-> element (.addEventListener "wheel" on-wheel #js {:passive false})))]
    (reset! latest {:schema schema :value value :change! change! :options options})
    (let [[low high] (numeric-range schema)
          ember? (= :ember (:skin options))
          change-pointer! (fn [event mode]
                            (let [bounds (.getBoundingClientRect (.-currentTarget event))
                                  x (- (.-clientX event) (.-left bounds) (/ (.-width bounds) 2))
                                  y (- (.-clientY event) (.-top bounds) (* (.-height bounds) (/ 44 94)))
                                  ;; Scale to view-box units so the dead zone has one size.
                                  scale (/ 100 (max 1 (.-width bounds)))]
                              (when-let [amount (geometry/dial-amount (* scale x) (* scale y) (when ember? mode))]
                                (when-let [next (normalized-value schema amount)] (change! next)))))]
      [:div.parameter-rotary-control
       {:role "slider" :tab-index 0 :aria-label (or (:name options) (:name schema) "Parameter")
        :ref attach! :data-skin (when ember? "ember")
        :aria-valuemin low :aria-valuemax high :aria-valuenow value
        :aria-valuetext (str (number-label schema value) (when-let [unit (unit-label schema)] (str " " unit)))
        :on-key-down (fn [event] (when-let [next (keyboard-value schema value (.-key event))]
                                  (.preventDefault event)
                                  (if-let [key-change! (:key-change options)] (key-change! (.-key event)) (change! next))))
        :on-pointer-down (fn [event] (when (capture! event) (change-pointer! event :press)))
        :on-pointer-move (fn [event] (when (captured? event) (change-pointer! event :drag)))
        :on-pointer-up release!
        :on-lost-pointer-capture release!}
       [rotary schema value options]])
    (finally (some-> @node (.removeEventListener "wheel" on-wheel)))))

(defn vertical-fader
  "A vertical fader for a bounded number: a rust fill from the low end (or
  zero on a bipolar range), a glowing thumb carrying its value and, on an
  integer span of sixteen or fewer, one tick per step with every fourth
  heavier. Pressing anywhere on the track jumps there; keys step like a
  native slider. Base and effective values are supplied, never sampled."
  [schema value change! {:keys [name base-value effective-value key-change skin]}]
  (when-let [axis (geometry/axis nil schema)]
    (let [[low high] (:bounds axis)
          shown (if (finite? effective-value) effective-value value)
          amount (or (geometry/amount axis shown) 0)
          origin (geometry/origin axis)
          base (geometry/amount axis base-value)
          pct #(str (* 100 %) "%")
          change-pointer! (fn [event]
                            (let [track (.querySelector (.-currentTarget event) ".fader-track")
                                  bounds (.getBoundingClientRect track)]
                              (when-let [a (geometry/fader-amount (- (.-clientY event) (.-top bounds)) (.-height bounds))]
                                (change! (geometry/value axis a)))))]
      [:div.parameter-fader
       {:role "slider" :tab-index 0 :aria-orientation "vertical" :aria-label (or name (:name schema) "Parameter")
        :data-skin (some-> skin clojure.core/name)
        :aria-valuemin low :aria-valuemax high :aria-valuenow value
        :aria-valuetext (str (number-label schema value) (when-let [unit (unit-label schema)] (str " " unit)))
        :on-key-down (fn [event] (when-let [next (keyboard-value schema value (.-key event))]
                                  (.preventDefault event)
                                  (if key-change (key-change (.-key event)) (change! next))))
        :on-pointer-down (fn [event] (when (capture! event) (change-pointer! event)))
        :on-pointer-move (fn [event] (when (captured? event) (change-pointer! event)))
        :on-pointer-up release!
        :on-lost-pointer-capture release!}
       [:span.fader-bound.high (number-label high)]
       [:div.fader-track
        (for [{:keys [value major?] tick :amount} (geometry/ticks axis)]
          ^{:key value} [:i.fader-tick {:class (when major? "major") :style {:bottom (pct tick)}}])
        (when (< low 0 high) [:i.fader-zero {:style {:bottom (pct origin)}}])
        [:i.fader-fill {:style {:bottom (pct (min origin amount)) :height (pct (js/Math.abs (- amount origin)))}}]
        (when (and base (not= base amount))
          [:i.fader-base {:style {:bottom (pct base)} :title (str "Base " (number-label schema base-value))}])
        [:span.fader-thumb {:style {:bottom (pct amount)}}
         [:output (number-label schema shown)]]]
       [:span.fader-bound.low (number-label low)]])))

(defn- axis-schema
  "A number schema for one axis, so a component fader or knob keeps its unit."
  [{:keys [bounds integer?]} component]
  (assoc (select-keys component [:unit :units :dmx-bits :name]) :type (if integer? :integer :number) :bounds bounds))

(defn- pad-text [axes values]
  (str/join ", " (map (fn [axis v] (str (:label axis) " " (number-label (axis-schema axis {}) v))) axes values)))

(defn xy-pad
  "An XY pad editing the first two components of a vector. Up is high. The
  pointer marks the shown (effective) value and a ghost marks the base the
  pad edits when a source drives it. Pressing anywhere jumps there; dragging
  follows the pointer, clamped to the bounds; arrow keys move one axis at a
  time, Shift ten steps. change! receives {index value} for the components
  a gesture sets; key! receives an index and a slider key, so the caller
  steps the component's current value and repeated presses accumulate."
  [{:keys [title axes values base change! key! skin]}]
  ;; One pointer owns the pad; a second finger on it is ignored until the
  ;; first lifts. A Z fader is a separate element with its own pointer.
  (r/with-let [owner (atom nil)]
  (let [[x-axis y-axis] axes
        edit (vec (or base values))
        shown (vec (or values base))
        position (geometry/pad-position axes shown)
        base-position (when base (geometry/pad-position axes base))
        pct #(str (* 100 %) "%")
        change-pointer! (fn [event]
                          (let [bounds (.getBoundingClientRect (.-currentTarget event))]
                            (when-let [amounts (geometry/pad-amounts (- (.-clientX event) (.-left bounds))
                                                                     (- (.-clientY event) (.-top bounds))
                                                                     (.-width bounds) (.-height bounds))]
                              (change! (geometry/pad-changes axes amounts)))))]
    [:div.parameter-xy
     {:role "slider" :tab-index 0 :aria-roledescription "XY pad" :data-skin (some-> skin name)
      :aria-label (str title " pad")
      :aria-valuemin (first (:bounds x-axis)) :aria-valuemax (second (:bounds x-axis))
      :aria-valuenow (first edit)
      :aria-valuetext (pad-text (take 2 axes) edit)
      :on-key-down (fn [event]
                     (when-let [[index key] (geometry/pad-key (.-key event) (.-shiftKey event))]
                       (.preventDefault event)
                       (key! index key)))
      :on-pointer-down (fn [event]
                         (when (and (nil? @owner) (capture! event))
                           (reset! owner (.-pointerId event))
                           (change-pointer! event)))
      :on-pointer-move (fn [event]
                         (when (and (= @owner (.-pointerId event)) (captured? event)) (change-pointer! event)))
      :on-pointer-up (fn [event] (when (= @owner (.-pointerId event)) (reset! owner nil) (release! event)))
      :on-lost-pointer-capture (fn [event] (when (= @owner (.-pointerId event)) (reset! owner nil)))}
     [:i.xy-grid]
     (when (< (first (:bounds x-axis)) 0 (second (:bounds x-axis)))
       [:i.xy-zero.vertical {:style {:left (pct (geometry/origin x-axis))}}])
     (when (< (first (:bounds y-axis)) 0 (second (:bounds y-axis)))
       [:i.xy-zero.horizontal {:style {:bottom (pct (geometry/origin y-axis))}}])
     (when-let [[px py] position]
       [:<>
        [:i.xy-cross.vertical {:style {:left (pct px)}}]
        [:i.xy-cross.horizontal {:style {:bottom (pct py)}}]])
     (when (and base-position (not= base-position position))
       (let [[bx by] base-position]
         [:i.xy-base {:style {:left (pct bx) :bottom (pct by)} :title (str "Base " (pad-text (take 2 axes) base))}]))
     (when-let [[px py] position]
       [:i.xy-pointer {:style {:left (pct px) :bottom (pct py)}}])
     [:span.xy-label.x (:label x-axis)]
     [:span.xy-label.y (:label y-axis)]])))

(defn rings-glyph
  "Nested rings, one per sub-parameter, outermost first, each a 270-degree
  arc filled from its origin to its value. The :ember skin sets them on a
  dark disc and shades each arc along its sweep in its own ring colour."
  [title axes values & [skin]]
  (r/with-let [id (str "rings-" (random-uuid))]
    (let [ember? (= :ember skin)
          url #(str "url(#" id "-" % ")")]
      [:svg.parameter-rings-glyph {:view-box "0 0 100 94" :role "img" :data-skin (when ember? "ember")
                                   :aria-label (str title ": " (pad-text axes values))}
       (when ember?
         [:<>
          [:defs
           [:radialGradient {:id (str id "-disc") :cx "45%" :cy "38%" :r "60%"}
            [:stop {:offset "0%" :stop-color "rgb(52,58,63)"}]
            [:stop {:offset "100%" :stop-color "rgb(22,25,28)"}]]
           [:radialGradient {:id (str id "-dot") :cx "35%" :cy "30%" :r "70%"}
            [:stop {:offset "0%" :stop-color "#fbfaf0"}]
            [:stop {:offset "100%" :stop-color "#a9ab9c"}]]]
          [:circle.rings-disc {:cx 50 :cy 44 :r 43 :fill (url "disc")}]])
       (for [{:keys [index radius width amount origin]} (geometry/rings axes values)]
         ^{:key index}
         [:g {:class (str "ring ring-" index)}
          (when ember?
            ;; Inside the ring group, so the stops read its --ring colour.
            [:defs
             [:linearGradient {:id (str id "-sweep-" index) :gradientUnits "userSpaceOnUse" :x1 8 :y1 80 :x2 92 :y2 80}
              [:stop {:offset "0%" :style {:stop-color "color-mix(in srgb, var(--ring) 45%, #101214)"}}]
              [:stop {:offset "60%" :style {:stop-color "var(--ring)"}}]
              [:stop {:offset "100%" :style {:stop-color "color-mix(in srgb, var(--ring) 70%, #ffffff)"}}]]])
          [:path.ring-track {:d (arc-path 0 1 radius) :stroke-width width}]
          (when (and amount (not= amount origin))
            [:path.ring-value {:d (arc-path origin amount radius) :stroke-width width
                               :stroke (when ember? (url (str "sweep-" index)))}])
          (when amount
            (let [[x y] (dial-point amount radius)]
              [:circle.ring-position {:cx x :cy y :r (/ width 2) :fill (when ember? (url "dot"))}]))])])))

(defn- ring-legend [axes values]
  [:span.rings-legend
   (for [[i axis] (map-indexed vector axes)]
     ^{:key i} [:span {:class (str "ring-" i)} [:b (:label axis)] (number-label (axis-schema axis {}) (get values i))])])

(defn vector-ring-axes
  "The ring axes of a vector the shared description draws as nested rings,
  else nil."
  [schema]
  (when (= :rings (controls/compound-widget schema))
    (let [labels (views/component-labels schema)
          axes (mapv (fn [i c] (geometry/axis (get labels i (str (inc i))) c)) (range) (controls/component-schemas schema))]
      (when (every? some? axes) axes))))

(defn ring-knob
  "Several sub-parameters of one parameter in one knob of nested rings.
  Pressing it opens one knob per sub-parameter; Done or Escape closes them
  and returns focus to the rings."
  [{:keys [title axes components values base change-component! key! skin]}]
  (r/with-let [open? (r/atom false)
               panel-id (str "rings-" (random-uuid))
               trigger (atom nil)
               close! (fn [] (reset! open? false) (some-> @trigger .focus))]
    (let [edit (vec (or base values)) shown (vec (or values base))]
      [:div.parameter-rings {:class (when @open? "open")}
       [:button.rings-trigger {:type "button" :ref #(reset! trigger %)
                               :aria-expanded @open? :aria-controls panel-id
                               :aria-label (str "Edit " title " components")
                               :on-click #(swap! open? not)
                               :on-key-down #(when (and @open? (= "Escape" (.-key %))) (.preventDefault %) (close!))}
        [rings-glyph title axes shown skin]
        [ring-legend axes shown]]
       (when @open?
         [:div.rings-components {:id panel-id :role "group" :aria-label (str title " components")
                                 :on-key-down #(when (= "Escape" (.-key %)) (.stopPropagation %) (close!))}
          (for [[i axis] (map-indexed vector axes)
                :let [schema (axis-schema axis (get components i))
                      text (str title " " (:label axis))]]
            ^{:key i}
            [:div.rings-component {:class (str "ring-" i)}
             [:label (:label axis)]
             [rotary-control schema (get edit i) #(change-component! i %)
              {:name (str text " rotary") :skin skin :base-value (get edit i) :effective-value (get shown i)
               :key-change #(key! i schema %)}]])
          [:button.text-button {:type "button" :on-click close!} "Done"]])])))

(defn ring-components
  "A touched ring knob on a device display: one rotary per sub-parameter, as
  the encoders under it edit them."
  [title axes values]
  [:div.ring-components {:role "group" :aria-label (str title " components")}
   (for [[i axis] (map-indexed vector axes)]
     ^{:key i}
     [:div.ring-component {:class (str "ring-" i)}
      [:small (:label axis)]
      [rotary (axis-schema axis {}) (get values i) {}]])])

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

(defn compound-control
  "The widget the shared description names for a numeric vector
  (`controls/compound-widget`): an XY pad, an XY pad with a Z fader, or
  nested rings. Each keeps the component popover with exact entries beside
  it. `components` are the component schemas and `labels` their names;
  `values` are shown (effective) and `base` the values being edited.
  `set!` takes {index value}, `set-component!` an index and a value, and
  `key!` an index, the component's schema and a slider key; without `key!`
  keys step the rendered value. The caller sends the same `param/set`
  commands as any other control."
  [{:keys [title widget components labels values base set! set-component! key! skin]}]
  (let [axes (mapv (fn [i c] (geometry/axis (get labels i (str (inc i))) c)) (range) components)
        edit (vec (or base values)) shown (vec (or values base))
        axis-schemas (mapv (fn [axis c] (when axis (axis-schema axis c))) axes components)
        key! (or key! (fn [i schema key]
                        (when-let [next (keyboard-value schema (get edit i) key)] (set-component! i next))))
        exact [axes-control title (mapv (fn [i c] (assoc c :type (or (:type c) :number) :axis-label (get labels i (str (inc i)))))
                                        (range) components)
               edit set-component!]]
    (if (or (some nil? axes) (not (#{:xy :xyz :rings} widget)))
      exact
      (case widget
        :rings [:div.compound-control {:data-widget "rings"}
                [ring-knob {:title title :axes axes :components components :values shown :base edit
                            :change-component! set-component! :key! key! :skin skin}]]
        [:div.compound-control {:data-widget (name widget)}
         [xy-pad {:title title :axes (subvec axes 0 2) :values shown :base edit :change! set! :skin skin
                  :key! #(key! %1 (nth axis-schemas %1) %2)}]
         (when (= :xyz widget)
           (let [z (nth axes 2) schema (nth axis-schemas 2)]
             [vertical-fader schema (get edit 2) #(set-component! 2 %)
              {:name (str title " " (:label z)) :base-value (get edit 2) :effective-value (get shown 2) :skin skin
               :key-change #(key! 2 schema %)}]))
         exact]))))

(defn vector-compound
  "compound-control for a :vector parameter's own schema."
  [{:keys [schema] :as props}]
  [compound-control (assoc props :widget (controls/compound-widget schema)
                           :components (controls/component-schemas schema)
                           :labels (views/component-labels schema))])

(defn- component-track [title schema value color]
  [:div.typed-component {:style (when color {"--widget-accent" color})}
   [:span.typed-component-name title]
   [:div.typed-track
    [:i {:style {:width (str (* 100 (or (proportion schema value) 0)) "%")}}]]
   [:output (number-label schema value)]])

(defn- unit [value] (if (finite? value) (max 0 (min 1 value)) 0))

(def hsv
  "Hue, saturation and brightness of RGB components, each in [0,1]. These are
  the components a Push encoder edits on a colour."
  views/rgb->hsv)

(def hsv-rgb views/hsv->rgb)

(def ^:private track-spaces
  "The tracks a colour layout draws: its colour components, without alpha."
  {:rgb :rgb :rgba :rgb :rgbw :rgbw :hsl :hsl :hsla :hsl :hsv :hsv})

(def ^:private component-names
  {"r" :red "g" :green "b" :blue "w" :white "red" :red "green" :green "blue" :blue "white" :white
   "h" :hue "s" :saturation "v" :brightness "l" :lightness
   "hue" :hue "saturation" :saturation "brightness" :brightness "lightness" :lightness})

(defn color-component
  "The colour component a control names, such as a Push encoder's `hue`."
  [id]
  (when (or (keyword? id) (string? id)) (get component-names (str/lower-case (name id)))))

(defn- gradient-css [stops]
  (str "linear-gradient(90deg," (str/join "," (map color-css stops)) ")"))

(defn- rgb [space components] (subvec (views/color-rgba space components) 0 3))

(defn- visible-hue
  "The other amounts of a hue track, raised so every hue shows."
  [space [h s x]]
  [h (max s 0.6) (if (= :hsl space) (min 0.65 (max 0.35 x)) (max x 0.6))])

(defn color-tracks
  "One component track per colour component of the layout (RGB by default),
  without alpha. Each gradient shows the colours that component alone would
  produce, with the current amount marked. A hue, saturation or brightness
  selection outside the layout switches the tracks to that space."
  [components selected & [layout]]
  (let [layout (or layout :rgb)
        own (track-spaces layout :rgb)
        component (color-component selected)
        space (cond (some #{component} (views/color-layouts own)) own
                    (#{:hue :saturation :brightness} component) :hsv
                    :else own)
        ids (views/color-layouts space)
        amounts (if (= space own)
                  ;; A hue wraps, as on the swatch; other amounts clamp.
                  (mapv (fn [index]
                          (let [amount (get components index)]
                            (if (and (= :hue (nth ids index)) (finite? amount)) (mod amount 1) (unit amount))))
                        (range (count ids)))
                  (hsv (rgb layout components)))
        stops (fn [index]
                (if (= :hue (nth ids index))
                  (mapv #(rgb space (assoc (visible-hue space amounts) index (/ % 6))) (range 7))
                  ;; Lightness passes through the full colour at one half on its way to white.
                  (mapv #(rgb space (assoc amounts index %)) (if (= :lightness (nth ids index)) [0 0.5 1] [0 1]))))]
    (mapv (fn [index id]
            {:id id :label (views/component-letters id) :amount (nth amounts index) :selected? (= id component)
             :gradient (gradient-css (stops index))})
          (range) ids)))

(defn component-amount
  "The amount of the colour component a control names, else the indexed one."
  [components selected index & [layout]]
  (when (and (<= 3 (count components)) (every? finite? (take 3 components)))
    (or (some #(when (:selected? %) (:amount %)) (color-tracks components selected layout))
        (get components index))))

(defn gradient-stops-css
  "CSS for authored gradient stops, or nil when the value is not a gradient."
  [value]
  (when (and (sequential? value) (seq value) (every? #(and (map? %) (finite? (:position %))) value))
    (if (= 1 (count value))
      (color-css (:color (first value)))
      (str "linear-gradient(90deg,"
           (str/join "," (map #(str (color-css (:color %)) " " (* 100 (unit (:position %))) "%") value)) ")"))))

(def ^:private neutral {:alpha 1 :white 0})

(defn color-view
  "The colour widget shared by Studio and Push: a swatch of the value and one
  gradient track per component. Components are read in the layout the schema
  declares. Authored gradient stops draw as one ramp."
  [value & [{:keys [selected schema]}]]
  (if-let [ramp (gradient-stops-css value)]
    [:div.parameter-color.gradient {:role "img" :aria-label (str "Gradient with " (count value) " stops")}
     [:div.color-swatch {:style {:background ramp}}]
     [:div.color-readout (str (count value) (if (= 1 (count value)) " stop" " stops"))]]
    (let [layout (views/color-space schema)
          ids (views/color-layouts layout)
          components (vec (take (count ids) (color-components value)))]
      (if-not (and (<= 3 (count components)) (every? finite? components))
        [:span.parameter-no-sample {:role "status"} "Awaiting value"]
        (let [tracks (color-tracks components selected layout)
              shown (rgb layout components)
              readout (str/join " · " (map #(number-label {:bounds [0 1]} %) shown))
              extra (get ids 3)
              tracked (set (map :id tracks))]
          [:div.parameter-color {:role "img"
                                 :aria-label (str "Colour, " (str/join ", " (map #(str (name %1) " " (number-label {:bounds [0 1]} %2)) ids components)))
                                 :data-layout (name layout)
                                 :data-space (name (some (fn [[space ids]] (when (= ids (mapv :id tracks)) space)) views/color-layouts))}
           [:div.color-swatch {:style {:background (color-css shown)}}
            (when (and extra (= 4 (count components)) (not (tracked extra)) (not= (neutral extra) (nth components 3)))
              [:small (str (if (= :alpha extra) "α" (views/component-letters extra)) " " (number-label (nth components 3)))])]
           [:div.color-channels
            (for [{:keys [id label amount gradient selected?]} tracks]
              ^{:key id}
              [:div.color-channel {:class (when selected? "selected") :data-component (name id)}
               [:span.color-channel-name label]
               [:div.color-track {:style {:background gradient}}
                [:i {:style {:left (str (* 100 (unit amount)) "%")}}]]
               [:output (number-label {:bounds [0 1]} amount)]])]
           [:div.color-readout [:b "RGB"] readout]])))))

(defn shape-glyph
  "One registered waveform cycle as supplied by the server catalog. Purely
  decorative: the choice's label carries its name."
  [glyph & [{:keys [width height] :or {width 40 height 16}}]]
  (when-let [points (shapes/points glyph width height 1.5)]
    [:svg.shape-glyph {:view-box (str "0 0 " width " " height) :preserve-aspect-ratio "none" :aria-hidden true
                       :focusable false}
     [:polyline {:points points :fill "none" :stroke "currentColor" :stroke-width 1.4
                 :stroke-linejoin "round" :vector-effect "non-scaling-stroke"}]]))

(defn shape-view
  "A waveform choice on a device display: the selected shape large, then a
  strip of the neighbouring choices with the selection marked."
  [shapes value]
  (let [selected (shapes/selected-index shapes value)
        count-shapes (count shapes)
        start (max 0 (min (max 0 (- count-shapes 4)) (dec (or selected 0))))
        current (when selected (get shapes selected))]
    [:div.shape-choice
     [:div.shape-choice-value (if current (:label current) (label value))]
     (if current
       [shape-glyph (:glyph current) {:width 100 :height 34}]
       [:div.shape-choice-empty])
     [:div.shape-choice-strip
      (for [[index shape] (map-indexed vector (take 4 (drop start shapes)))]
        ^{:key index} [:span {:class (when (= (+ start index) selected) "selected") :title (:label shape)}
                       [shape-glyph (:glyph shape) {:width 24 :height 12}]])]
     [:span.typed-choice-position (str (if (some? selected) (inc selected) "—") " / " count-shapes)]]))

(defn shape-picker
  "Choose a waveform by its shape. A radio group of registered glyphs; arrow
  keys move the selection like a native radio group."
  [title shapes value change!]
  (let [selected (shapes/selected-index shapes value)
        last-index (dec (count shapes))
        move! (fn [event index]
                (let [target (cond (< index 0) last-index (> index last-index) 0 :else index)
                      buttons (.. event -currentTarget -parentNode -children)]
                  (.preventDefault event)
                  (change! (:value (get shapes target)))
                  (some-> (aget buttons target) .focus)))]
    [:div.shape-picker {:role "radiogroup" :aria-label title}
     (for [[index shape] (map-indexed vector shapes)]
       ^{:key index}
       [:button {:type "button" :role "radio" :aria-checked (= index selected)
                 :tab-index (if (= index (or selected 0)) 0 -1)
                 :on-click #(change! (:value shape))
                 :on-key-down #(case (.-key %)
                                 ("ArrowRight" "ArrowDown") (move! % (inc index))
                                 ("ArrowLeft" "ArrowUp") (move! % (dec index))
                                 "Home" (move! % 0)
                                 "End" (move! % last-index)
                                 nil)}
        [shape-glyph (:glyph shape)]
        [:span (:label shape)]])]))

(defn- choice-view [schema value]
  (let [choices (vec (or (some->> (:choices schema) seq (mapv #(if (map? %) % {:value % :label (label %)})))
                         (map #(hash-map :value % :label (label %)) (or (:values schema) (:options schema)))))
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
       (:color :gradient) [color-view shown {:selected component-label :schema schema}]
       :range (let [[low high] shown a (proportion schema low) b (proportion schema high)]
                [:div.typed-range
                 [:strong (str (number-label low) " — " (number-label high))]
                 [:div.typed-range-track
                  (when (and a b) [:i {:style {:left (str (* 100 a) "%") :width (str (* 100 (- b a)) "%")}}])]
                 [:div.typed-range-bounds [:span (number-label (first (:bounds schema)))]
                  [:span (number-label (second (:bounds schema)))]]
                 [:span.typed-range-unit (or (unit-label schema) "range")]])
       :vector (if-let [axes (vector-ring-axes schema)]
                 [:div.typed-rings
                  [rings-glyph (or (:name schema) "Components") axes (vec shown)]
                  [ring-legend axes (vec shown)]]
                 [:div.typed-vector
                  (for [[index component] (map-indexed vector (take 4 shown))]
                    ^{:key index} [component-track (get (views/component-labels schema) index)
                                   schema component nil])
                  [:div.typed-range-bounds [:span (number-label (first (:bounds schema)))]
                   [:span (number-label (second (:bounds schema)))]]])
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
