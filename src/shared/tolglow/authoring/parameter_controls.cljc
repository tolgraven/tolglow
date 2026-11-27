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
  (`[{:id :name :glyph}]`), used only to recognise waveform choices."
  ([parameter schema] (control parameter schema nil))
  ([parameter schema catalog]
   (let [type (control-type schema catalog)
         listed (choices schema catalog)]
     (cond-> {:param parameter :label (title parameter schema) :type type
              :places (decimal-places schema)}
       (unit-label schema) (assoc :unit (unit-label schema))
       (bounds schema) (assoc :bounds (bounds schema))
       (input-step schema) (assoc :step (input-step schema))
       (#{:number :integer} type) (assoc :widget (widget schema))
       (and listed (#{:waveform :shape :choice :enum} type)) (assoc :choices listed)
       (:dmx-bits schema) (assoc :dmx-bits (:dmx-bits schema))))))
