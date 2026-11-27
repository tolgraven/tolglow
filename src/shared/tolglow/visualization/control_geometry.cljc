(ns tolglow.visualization.control-geometry
  "Geometry of the shared parameter widgets as data: the XY pad, the vertical
  fader, the nested-ring knob and the knob skins. Each function maps between a
  pointer, key or wheel position and a parameter value inside the bounds the
  parameter description declares. Studio draws these and dispatches the
  resulting values through the same `param/set` command the REPL submits.
  Nothing here reads a clock, the DOM or a device.")

(defn- finite? [x]
  #?(:clj (and (number? x) (Double/isFinite (double x)))
     :cljs (and (number? x) (js/isFinite x))))

(defn- round [x] #?(:clj (Math/round (double x)) :cljs (js/Math.round x)))

(defn- atan2 [y x] #?(:clj (Math/atan2 (double y) (double x)) :cljs (js/Math.atan2 y x)))

(def ^:private pi #?(:clj Math/PI :cljs js/Math.PI))

(defn clamp01 [x] (if (finite? x) (max 0 (min 1 x)) 0))

(defn axis
  "One editable dimension: its label, [low high] bounds and whether it holds
  whole numbers. nil when the bounds are missing or empty."
  [label {:keys [bounds type]}]
  (let [[low high] bounds]
    (when (and (finite? low) (finite? high) (< low high))
      {:label label :bounds [low high] :integer? (contains? #{:integer "integer"} type)})))

(defn amount
  "Where a value sits between an axis's bounds, from 0 to 1, or nil."
  [{[low high] :bounds} value]
  (when (and (finite? value) (finite? low) (finite? high) (< low high))
    (clamp01 (/ (- value low) (- high low)))))

(defn value
  "The axis value at an amount from 0 to 1, clamped, and rounded for an
  integer axis."
  [{[low high] :bounds integer? :integer?} amount]
  (let [v (+ low (* (clamp01 amount) (- high low)))]
    (if integer? (max low (min high (round v))) v)))

(defn origin
  "The amount a bipolar axis fills from: its zero, else its low end."
  [{[low high] :bounds :as axis}]
  (if (< low 0 high) (amount axis 0) 0))

(defn pad-amounts
  "The [x y] amounts of a pointer at offset (x, y) inside a pad of the given
  size. Up is high, as on a fader; positions outside the pad clamp to its edge."
  [x y width height]
  (when (and (finite? x) (finite? y) (finite? width) (finite? height) (pos? width) (pos? height))
    [(clamp01 (/ x width)) (clamp01 (- 1 (/ y height)))]))

(defn pad-changes
  "The components a pad gesture sets, as {index value}: the pointer's amounts
  on the first two axes. Later components are left to their own controls, so
  a Z fader moved at the same time keeps its value."
  [axes [ax ay]]
  {0 (value (nth axes 0) ax) 1 (value (nth axes 1) ay)})

(defn pad-position
  "The [x y] amounts that draw the first two values on a pad, or nil."
  [axes values]
  (let [x (amount (nth axes 0 nil) (nth values 0 nil))
        y (amount (nth axes 1 nil) (nth values 1 nil))]
    (when (and x y) [x y])))

(defn fader-amount
  "The amount of a vertical fader at offset y from its top: the top is high."
  [y height]
  (when (and (finite? y) (finite? height) (pos? height))
    (clamp01 (- 1 (/ y height)))))

(defn- floor [x] #?(:clj (Math/floor (double x)) :cljs (js/Math.floor x)))

(defn- ceil [x] #?(:clj (Math/ceil (double x)) :cljs (js/Math.ceil x)))

(defn ticks
  "Tick marks of an integer axis spanning at most sixteen steps: one per whole
  number inside the bounds, and every fourth tick counted from the low end
  heavier, as on the tolglow-web slider. nil otherwise."
  [{[low high] :bounds integer? :integer? :as axis}]
  (when (and axis integer? (<= (- high low) 16))
    (let [first-tick (long (ceil low)) last-tick (long (floor high))]
      (mapv (fn [n] {:value n :amount (amount axis n) :major? (zero? (mod (- n first-tick) 4))})
            (range first-tick (inc last-tick))))))

(defn pad-key
  "The component and slider key one pad key press moves, as [index key]:
  Left and Right step the first axis, Up and Down the second, and Shift makes
  it a large (Page) step. nil for other keys. The caller applies the slider
  key to the component's current value, so repeated presses accumulate."
  [key large?]
  (case key
    "ArrowRight" [0 (if large? "PageUp" "ArrowUp")]
    "ArrowLeft" [0 (if large? "PageDown" "ArrowDown")]
    "ArrowUp" [1 (if large? "PageUp" "ArrowUp")]
    "ArrowDown" [1 (if large? "PageDown" "ArrowDown")]
    nil))

;; A 270-degree dial with its 90-degree gap at the bottom, centred on (50, 44)
;; of a 100 × 94 view box, as `rotary` in Studio draws it.

(def sweep 270)

(defn dial-amount
  "The amount at a pointer offset (x, y) from the dial centre, y down.
  Without a mode, or with :drag, a pointer in the bottom gap clamps to the
  nearer end, so a drag past either end lands exactly on its bound. :drag
  and :press hold the value (nil) on the centre, where there is no angle;
  :press, a fresh press, also holds in the gap: the Ember dead zone."
  ([x y] (dial-amount x y nil))
  ([x y mode]
   (when (and (finite? x) (finite? y))
     (let [degrees (mod (+ 360 (* (/ 180 pi) (atan2 y x))) 360)
           angle (if (< degrees 90) (+ degrees 360) degrees)
           raw (/ (- angle 135) sweep)]
       (cond
         (nil? mode) (clamp01 raw)
         (< (+ (* x x) (* y y)) 16) nil
         (<= 0 raw 1) raw
         (= :drag mode) (clamp01 raw))))))

(def wheel-line-px
  "CSS pixels of one wheel line (WheelEvent deltaMode 1)."
  20)

(def wheel-page-px
  "CSS pixels of one wheel page (WheelEvent deltaMode 2)."
  400)

(def wheel-sweep-px
  "Vertical scroll distance that turns a continuous knob across its range."
  1000)

(def wheel-step-px
  "Vertical scroll distance of one step of an integer knob: a three-line
  mouse notch."
  60)

(defn wheel-pixels
  "The vertical distance of one wheel event in CSS pixels, positive when
  scrolling up (which raises a knob). nil when the event has no vertical part
  or is mostly horizontal, so strip scrolling and Shift+wheel pass through."
  [delta-x delta-y delta-mode]
  (when (and (finite? delta-y) (not (zero? delta-y))
             (or (not (finite? delta-x)) (<= (Math/abs (double delta-x)) (Math/abs (double delta-y)))))
    (* -1 delta-y (case delta-mode 1 wheel-line-px 2 wheel-page-px 1))))

(defn wheel-amount
  "The amount of a continuous knob after `pixels` of wheel distance: the turn
  is proportional to distance, so many small trackpad events turn it as far
  as one large mouse event of the same total."
  [current pixels]
  (when (and (finite? pixels) (not (zero? pixels)))
    (clamp01 (+ (clamp01 current) (/ pixels wheel-sweep-px)))))

(defn wheel-steps
  "[steps remainder] of an integer knob: the whole steps in the carried
  remainder plus this event's pixels. Reversing direction drops the carried
  remainder."
  [remainder pixels]
  (let [carried (if (neg? (* remainder pixels)) 0 remainder)
        total (+ carried pixels)
        steps (long (quot total wheel-step-px))]
    [steps (- total (* steps wheel-step-px))]))

(defn pointer-rotation
  "Rotation in degrees of a pointer bar drawn pointing up from the centre."
  [amount]
  (- (* sweep (clamp01 amount)) 135))

(def ring-colors
  "One tint per ring, outermost first, matching the component colours of the
  3D vector widget."
  ["#e8a083" "#aed39a" "#89b9e0" "#cfb2dd"])

(defn rings
  "Nested rings for up to four sub-parameters, outermost first. Each ring has
  its radius, the amount it fills to and the amount it fills from (zero on a
  bipolar axis)."
  [axes values]
  (let [n (min 4 (count axes))
        outer 38 gap (if (<= n 2) 10 8)]
    (vec (for [i (range n)
               :let [axis (nth axes i) v (nth values i nil)]
               :when axis]
           {:index i :label (:label axis) :radius (- outer (* i gap)) :width (- gap 2)
            :amount (amount axis v) :origin (origin axis) :color (nth ring-colors i)}))))

(def knob-skins
  "Knob looks an operator can choose for Studio. `:ember` is the tolglow-web
  knob: a rust pie wedge on dark slate with a glowing bar pointer."
  [{:id :standard :label "Standard"} {:id :ember :label "Ember"}])

(defn knob-skin
  "A stored preference as a known skin, else :standard."
  [preference]
  (let [id (cond (keyword? preference) preference (string? preference) (keyword preference))]
    (or (some #(when (= id (:id %)) id) knob-skins) :standard)))
