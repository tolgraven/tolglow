(ns tolglow.authoring.parameter-controls
  "One description of how a parameter is edited: its label, unit, bounds,
  readout precision, input step and control shape. Studio's parameter rows
  and the Live parameter device (#625, `tolglow.integrations.live-devices`)
  both read it, so a parameter shows the same name, the same number of
  decimals and the same kind of control in Studio and in Live. The REPL can
  call `control` directly. Nothing here reads a clock or a device."
  (:require [clojure.string :as str]
            [afterglow.control.resolution :as resolution]
            [tolglow.visualization.waveform-shapes :as shapes]))

(defn- tag [x] (cond (keyword? x) x (and (string? x) (not (str/blank? x))) (keyword x)))

(defn finite? [x]
  #?(:clj (and (number? x) (Double/isFinite (double x)))
     :cljs (and (number? x) (js/Number.isFinite x))))

(defn label
  "An identifier as an operator reads it: hyphens and underscores become spaces."
  [value]
  (str/replace (cond (keyword? value) (name value) (nil? value) "—" :else (str value)) #"[-_]" " "))

(defn title
  "The control's name: the schema's :name, else the parameter ID as a label."
  [parameter schema]
  (or (:name schema) (label parameter)))

(defn unit-label [schema]
  (when-let [unit (or (:unit schema) (:units schema))] (label unit)))

(defn bounds
  "The hard [low high] range of a numeric control, or nil."
  [schema]
  (let [[low high] (:bounds schema)]
    (when (and (finite? low) (finite? high) (< low high)) [low high])))

(defn channel-step
  "The value change of one code of the finest DMX channel the parameter
  reaches on this patch (:dmx-bits, annotated by the host), or nil."
  [{:keys [dmx-bits] :as schema}]
  (resolution/fine-step dmx-bits (or (bounds schema) [0 1])))

(defn- log10 [x] #?(:clj (Math/log10 (double x)) :cljs (js/Math.log10 x)))

(defn- ceil [x] #?(:clj (Math/ceil (double x)) :cljs (js/Math.ceil x)))

(defn decimal-places
  "Decimals an operator readout shows. A normalized 16-bit control needs five
  places to tell adjacent DMX codes apart; without a patched channel, three."
  [schema]
  (if-let [step (channel-step schema)]
    (long (max 0 (min 15 (ceil (- (log10 step))))))
    3))

(defn input-step
  "The step of a continuous slider: one for integers, else one DMX code, else
  a thousandth of the range."
  [schema]
  (if (= :integer (tag (:type schema)))
    1
    (or (channel-step schema)
        (when-let [[low high] (bounds schema)] (/ (- high low) 1000)))))

(defn waveform-selector? [schema]
  (= :waveform-selector (tag (:semantic/type schema))))

(defn control-type
  "The control family Studio draws: :waveform (a registered waveform
  selector), :shape (choices that each name a catalog waveform, drawn as
  glyphs), :choice (listed choices), or the schema's storage type (:boolean,
  :enum, :color, :gradient, :vector, :range, :integer, :number)."
  [schema catalog]
  (cond
    (waveform-selector? schema) :waveform
    (shapes/choices schema catalog) :shape
    (seq (:choices schema)) :choice
    :else (or (tag (:type schema)) :number)))

(defn expression-type
  "The numeric IR type an expression reads from the parameter with
  `{:op :param}`: :number, :vec2, :vec3 or :vec4, else nil. A :gradient has
  none; `{:op :gradient :key k :args [position]}` samples it as a :vec3. The
  compiler types parameter reads with this, and Studio's block builder offers a
  parameter only to inputs of the same type."
  [schema]
  (case (tag (:type schema))
    (:number :integer) :number
    :color :vec3
    :range :vec2
    :vector (let [size (:size schema)]
              (when (and (integer? size) (<= 2 size 4)) (keyword (str "vec" size))))
    nil))

(defn widget
  "How a bounded number is drawn: :fader when the schema asks for one, else
  :rotary. Unbounded numbers are a :field."
  [schema]
  (cond (not (bounds schema)) :field
        (= :fader (tag (:ui/widget schema))) :fader
        :else :rotary))

(defn component-count
  "How many components a :vector holds: its :size, else its component schemas
  or labels."
  [schema]
  (let [size (:size schema)]
    (cond (integer? size) size
          (seq (:component-schemas schema)) (count (:component-schemas schema))
          (seq (:component-labels schema)) (count (:component-labels schema)))))

(defn component-schemas
  "One numeric schema per component of a :vector: its declared
  :component-schemas entry, else the vector's own bounds, unit and DMX width."
  [schema]
  (let [shared (-> (select-keys schema [:bounds :unit :units :dmx-bits]) (assoc :type :number))]
    (mapv (fn [i] (merge shared (get (:component-schemas schema) i)))
          (range (or (component-count schema) 0)))))

(def compound-widgets
  "Ways a numeric vector is drawn: an XY pad, an XY pad with a Z fader beside
  it, nested rings that open into one knob per component, or a row of
  component readouts that opens into rotaries."
  #{:xy :xyz :rings :axes})

(def ^:private color-vectors #{:color :colour :rgb :rgbw :rgba :hsl :hsla :hsv})

(defn compound-widget
  "How a :vector parameter is edited, or nil for any other type or a colour
  vector (drawn as a swatch). Two bounded components are an :xy pad, three an
  :xyz pad and fader, four nested :rings. `:ui/widget` may ask for :rings or
  :axes on any size, or :xy/:xyz on the size they fit. A vector with an
  unbounded component, or of another size, is :axes."
  [schema]
  (when (and (= :vector (tag (:type schema)))
             (not (color-vectors (tag (:semantic/type schema)))))
    (let [n (component-count schema)
          bounded? (and n (<= 2 n 4) (every? bounds (component-schemas schema)))
          asked (tag (:ui/widget schema))]
      (cond
        (not bounded?) :axes
        (#{:rings :axes} asked) asked
        (and (= :xy asked) (= 2 n)) :xy
        (and (= :xyz asked) (= 3 n)) :xyz
        :else (case n 2 :xy 3 :xyz :rings)))))

(def slot-sizes
  "The standard slots a parameter card occupies on a surface's card grid
  (#795), as [columns rows] of one shared slot unit. A surface places the
  cards with `tolglow.authoring.slot-grid/pack` on one column and row unit;
  it changes its column count with its width, never a card's geometry."
  {:compact [1 1] :wide [2 1] :large [2 2]})

(defn slot-size
  "The standard slot a control description (from `control`) occupies:
  :compact for a knob, fader, toggle, choice, colour swatch, plain field or
  rings; :wide for a waveform or shape picker, a gradient, a range and a
  vector of component readouts; :large for an XY pad and an XY pad with a Z
  fader. Every card of one widget type gets the same slot. A card never
  changes slot while it is used: opened rings and plots are drawn over or
  beside the grid, so nothing repacks under the operator's hand."
  [{:keys [type widget]}]
  (cond
    (#{:xy :xyz} widget) :large
    (= :axes widget) :wide
    ;; A vector drawn without a compound widget (a colour vector) shows its
    ;; component readouts.
    (and (= :vector type) (nil? widget)) :wide
    (#{:waveform :shape :gradient :range} type) :wide
    :else :compact))

(defn slot-span
  "[columns rows] of a slot size, defaulting to :compact."
  [size]
  (get slot-sizes size (:compact slot-sizes)))

(defn choices
  "[{:value :label}] for a :choice, :shape, :waveform or :enum control, in
  order, else nil. Values are the schema's own."
  [schema catalog]
  (or (when-let [shaped (shapes/choices schema catalog)]
        (mapv #(select-keys % [:value :label]) shaped))
      (when (seq (:choices schema))
        (mapv (fn [c] (let [c (if (map? c) c {:value c})]
                        {:value (:value c) :label (or (:label c) (label (or (:id c) (:value c))))}))
              (:choices schema)))
      (when-let [values (seq (or (:values schema) (:options schema)))]
        (mapv (fn [v] {:value v :label (label v)}) values))))

(defn control
  "The control description of one parameter. catalog is the waveform catalog
  (`[{:id :name :glyph}]`), used only to recognise waveform choices. :slot is
  the standard card slot (`slot-size`) every surface lays it out in."
  ([parameter schema] (control parameter schema nil))
  ([parameter schema catalog]
   (let [type (control-type schema catalog)
         listed (choices schema catalog)
         description (cond-> {:param parameter :label (title parameter schema) :type type
                              :places (decimal-places schema)}
                       (unit-label schema) (assoc :unit (unit-label schema))
                       (bounds schema) (assoc :bounds (bounds schema))
                       (input-step schema) (assoc :step (input-step schema))
                       (#{:number :integer} type) (assoc :widget (widget schema))
                       (compound-widget schema) (assoc :widget (compound-widget schema))
                       (and listed (#{:waveform :shape :choice :enum} type)) (assoc :choices listed)
                       (:dmx-bits schema) (assoc :dmx-bits (:dmx-bits schema)))]
     (assoc description :slot (slot-size description)))))
