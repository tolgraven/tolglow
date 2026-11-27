(ns tolglow.effects
  "Composable Clojure authoring helpers that return the existing portable DSL.
  No clocks, mutable state, IO or second evaluator. Publish the returned data
  through the ordinary registry and compile it with tolglow.compiler."
  (:require [tolglow.registry :as registry]
            [tolglow.compiler :as compiler]
            [tolglow.oscillator :as oscillator]
            [afterglow.time :as time]))

(defn- fail! [path message]
  (throw (ex-info message {:errors [{:path path :message message}]})))

(defn constant
  "A finite numeric/vector constant. Helper literals use doubles for the typed
  compiler; an explicitly supplied IR map preserves its authored numeric type."
  [value]
  (cond
    (registry/finite-number? value) {:op :const :value (double value)}
    (and (vector? value) (<= 2 (count value) 4) (every? registry/finite-number? value))
    {:op :const :value (mapv double value)}
    :else (fail! [:value] "Expected a finite number or a numeric vector of size 2..4")))

(defn expression [value]
  (if (and (map? value) (keyword? (:op value)) (registry/data? value)) value (constant value)))

(defn param
  "Reference an explicit parameter schema by keyword. Compiler resolves its type."
  [key]
  (when-not (keyword? key) (fail! [:key] "Parameter references need keyword IDs"))
  {:op :param :key key})

(defn- operation [op values] {:op op :args (mapv expression values)})

(defn add [& values] (operation :add values))

(defn subtract ([value] (operation :subtract [value])) ([a b] (operation :subtract [a b])))

(defn multiply [& values] (operation :multiply values))

(defn divide [a b] (operation :divide [a b]))

(defn clamp ([value] (clamp value 0.0 1.0)) ([value low high] (operation :clamp [value low high])))

(defn mix [a b amount] (operation :mix [a b amount]))

(defn xyz [x y z] (operation :vec3 [x y z]))

(defn rgb [r g b] (xyz r g b))

(defn gradient
  "Sample gradient parameter `key` at a scalar `position`, giving linear RGB.
  Positions outside the stops hold the end colours; pass {:extend :repeat} to
  wrap the position or {:extend :mirror} to reflect it. Colours interpolate
  component by component between stops, as Studio's ramp draws them."
  ([key position] (gradient key position {}))
  ([key position {:keys [extend] :as options}]
   (when-not (keyword? key) (fail! [:key] "Gradient references need keyword IDs"))
   (when-not (and (map? options) (every? #{:extend} (keys options)))
     (fail! [] "Gradient options take only :extend"))
   (cond-> {:op :gradient :key key :args [(expression position)]}
     extend (assoc :extend extend))))

(defn position [axis]
  (when-not (#{:x :y :z} axis) (fail! [:axis] "Position axis must be :x, :y or :z"))
  {:op :position :axis axis})

(defn parameter
  "Infer a parameter schema from a default and optional schema metadata. Numbers
  default to normalized :number bounds [0,1]; RGB vectors infer :color and a
  vector of {:position :color} stops infers :gradient. Explicit :type supports
  integer/vector/range, and :values selects enum."
  ([default] (parameter default {}))
  ([default options]
   (when-not (map? options) (fail! [:parameter] "Parameter options must be a map"))
   (let [type (or (:type options)
                  (cond (:values options) :enum (number? default) :number
                        (boolean? default) :boolean
                        (and (vector? default) (seq default) (every? map? default)) :gradient
                        (and (vector? default) (= 3 (count default))) :color
                        (vector? default) :vector))
         floating? (#{:number :color :vector :range} type)
         default (cond
                   floating?
                   (cond (registry/finite-number? default) (double default)
                         (and (vector? default) (every? registry/finite-number? default)) (mapv double default)
                         :else default)
                   ;; Gradient stops use doubles like other helper literals.
                   (and (= :gradient type) (vector? default)
                        (every? #(and (map? %) (registry/finite-number? (:position %))
                                      (vector? (:color %)) (every? registry/finite-number? (:color %))) default))
                   (mapv #(-> % (update :position double) (update :color (partial mapv double))) default)
                   :else default)
         schema (merge (cond-> {:type type}
                         (= :number type) (assoc :bounds [0.0 1.0])
                         (= :vector type) (assoc :size (count default)))
                       options {:default default})
         problems (registry/validate-parameter schema)]
     (when (seq problems) (throw (ex-info "Invalid parameter shorthand" {:errors problems})))
     schema)))

(def ^:private default-control-axes
  {:level :pressure :dimmer :pressure :velocity :strike :pan :glide
   :tilt :slide :zoom :slide :focus :slide})

(defn params
  "Normalize a parameter map. A number/boolean is a default; [default low high]
  is a bounded number shorthand. Use parameter for RGB/vector defaults or enums.
  Existing complete schema maps are retained and validated."
  [entries]
  (when-not (map? entries) (fail! [:params] "Parameters must be a map"))
  (into {}
        (for [[key value] entries]
          (do
            (when-not (keyword? key) (fail! [:params key] "Parameter IDs must be keywords"))
            (let [schema (cond
                           (map? value) value
                           (and (vector? value) (= 3 (count value)) (every? registry/finite-number? value))
                           (parameter (first value) {:bounds [(second value) (nth value 2)]})
                           :else (parameter value))
                  schema (cond-> schema
                           (and (#{:number :integer} (:type schema)) (default-control-axes key)
                                (not (contains? schema :control/axis)))
                           (assoc :control/axis (default-control-axes key)))
                  problems (registry/validate-parameter schema [:params key])]
              (when (seq problems) (throw (ex-info "Invalid parameter shorthand" {:errors problems})))
              [key schema])))))

(defn cycles
  "Continuous cycle count for a musical duration, default one bar. :phase is
  measured in cycles. Use :phase-reference :launch for the cue's elapsed beats
  and captured meter, or :transport for the shared beat and current meter.
  The older :clock :elapsed/:absolute selects a coordinate on the effect clock."
  ([] (cycles {:bars 1} {}))
  ([period] (cycles period {}))
  ([period {:keys [clock phase phase-reference] :or {clock :elapsed phase 0.0} :as options}]
   (let [problems (time/validate-duration period)]
     (when (seq problems) (throw (ex-info "Invalid oscillator period" {:errors problems}))))
   (when-not (pos? (val (first period))) (fail! [:period] "Oscillator period must be positive"))
   (if (contains? options :phase-reference)
     (do
       (when-not (oscillator/phase-references phase-reference)
         (fail! [:phase-reference] "Choose :launch or :transport"))
       (when (contains? options :clock) (fail! [:clock] "Choose one oscillator clock"))
       (when-not (oscillator/musical-period? period) (fail! [:period] "A phase reference needs beats, bars or phrases"))
       (let [oscillator {:op :oscillator-phase :phase-reference phase-reference :period period}]
         (if (time/finite-number? phase) (assoc oscillator :phase phase)
             (add oscillator phase))))
     (do
       (when-not (#{:elapsed :absolute} clock) (fail! [:clock] "Clock must be :elapsed or :absolute"))
       (let [[unit amount] (first period)
             op ({:beats :beat :bars :bar :phrases :phrase :ms :time} unit)]
         (add (divide {:op op :clock clock} amount) phase))))))

(def ^:private wave-options #{:period :clock :phase-reference :phase :low :high :duty})

(defn- options! [options allowed]
  (when-not (and (map? options) (every? allowed (keys options)))
    (fail! [] (str "Unknown or invalid options; expected " (pr-str (sort allowed))))) options)

(defn phase-accumulator
  "Cycles of a cue-local phase that advances each frame by the cue's elapsed
  beats since the last frame divided by `period`. Unlike `cycles`, `period`
  may be an expression of parameters: an edit changes the rate from the phase
  already reached instead of jumping to elapsed / new period. The phase starts
  at zero on launch, holds while the transport is stopped and is kept in the
  instance's state under `id`, a keyword unique within the effect. Options:
  :unit :beats (default), :bars or :phrases for the period."
  ([id period] (phase-accumulator id period {}))
  ([id period {:keys [unit] :as options}]
   (options! options #{:unit})
   (when-not (keyword? id) (fail! [:id] "A phase accumulator needs a keyword ID"))
   (when-not (or (nil? unit) (#{:beats :bars :phrases} unit)) (fail! [:unit] "Unit must be :beats, :bars or :phrases"))
   (cond-> {:op :phase-accumulator :id id :period (expression period)}
     unit (assoc :unit unit))))

(defn- wave-range [options value] (mix (:low options 0.0) (:high options 1.0) value))

(defn- cycle-expression [options] (cycles (:period options {:bars 1}) options))

(defn sine
  "Normalized sine: .5, 1, .5, 0 at successive quarter cycles. Options include
  :period, :phase-reference, :phase (cycles) and output :low/:high."
  ([] (sine {}))
  ([options]
   (options! options wave-options)
   (wave-range options (clamp (add 0.5 (multiply 0.5 (operation :sin [(multiply (* 2.0 Math/PI) (cycle-expression options))])))))))

(defn triangle
  "Triangle: 0, .5, 1, .5 at successive quarter cycles."
  ([] (triangle {}))
  ([options]
   (options! options wave-options)
   (wave-range options (subtract 1.0 (operation :abs [(subtract (multiply 2.0 (operation :fract [(cycle-expression options)])) 1.0)])))))

(defn saw
  "Rising saw: 0, .25, .5, .75, then wraps to 0."
  ([] (saw {}))
  ([options]
   (options! options wave-options)
   (wave-range options (operation :fract [(cycle-expression options)]))))

(defn pulse
  "Hard pulse, high for the first :duty fraction of each cycle (default .5).
  Duty may be an expression and is clamped to [0,1]. At the falling boundary the
  pulse is low. Physical fixtures need appropriate rate/intensity authoring."
  ([] (pulse {}))
  ([options]
   (options! options wave-options)
   (wave-range options
               (subtract 1.0 (operation :floor [(add (operation :fract [(cycle-expression options)])
                                                    (subtract 1.0 (clamp (:duty options 0.5))))])))))

(defn wave [shape options]
  ((case shape :sine sine :triangle triangle :saw saw :pulse pulse
         (fail! [:shape] "Wave shape must be :sine, :triangle, :saw or :pulse")) options))

(defn position-wave
  "Add position/wavelength cycles along :axis (default :x). Wavelength is a
  nonzero signed distance in metres, default 4. Positive wavelength moves the
  pattern toward the negative axis as time advances; negative reverses it."
  ([shape] (position-wave shape {}))
  ([shape {:keys [axis wavelength] :or {axis :x wavelength 4.0} :as options}]
   (options! options (conj wave-options :axis :wavelength))
   (when-not (and (registry/finite-number? wavelength) (not (zero? wavelength)))
     (fail! [:wavelength] "Wavelength must be a nonzero finite distance in metres"))
   (wave shape (-> options (dissoc :axis :wavelength)
                   (assoc :phase (add (:phase options 0.0) (divide (position axis) wavelength)))))))

(defn output
  "One ordered attribute assignment. Options: :blend and :targets. Geometry
  attributes use world coordinates; other attributes must evaluate in [0,1]."
  ([attribute value] (output attribute value {}))
  ([attribute value options]
   (options! options #{:blend :targets})
   (when-not (keyword? attribute) (fail! [:attribute] "Output attribute must be a keyword"))
   (merge {:attribute attribute :value (expression value) :blend :replace} options)))

(defn color ([value] (color value {})) ([value options] (output :color (clamp value) options)))

(defn intensity ([value] (intensity value {})) ([value options] (output :dimmer (clamp value) options)))

(defn aim ([point] (aim point {})) ([point options] (output :aim point options)))

(defn zoom ([value] (zoom value {})) ([value options] (output :zoom (clamp value) options)))

(defn focus ([value] (focus value {})) ([value options] (output :focus (clamp value) options)))

(defn compose
  "Combine assignment descriptors or vectors of descriptors in their exact order.
  Repeated attributes remain separate layers, including :add and :max blends."
  [& layers]
  (vec (mapcat (fn [layer]
                 (cond (and (map? layer) (:attribute layer)) [layer]
                       (and (vector? layer) (every? #(and (map? %) (:attribute %)) layer)) layer
                       :else (fail! [:outputs] "Compose expects output descriptors or vectors of descriptors"))) layers)))

(defn output-categories
  "Stable semantic categories from the union of all output layers. These tags
  are independent of cue colors/swatch and include composite assignments."
  [outputs]
  (let [attributes (set (map :attribute (registry/output-entries outputs)))]
    (cond-> []
      (or (attributes :color) (some attributes registry/emitter-attributes)) (conj :color)
      (attributes :dimmer) (conj :intensity)
      (some attributes [:aim :direction :pan :tilt]) (conj :movement)
      (some attributes [:zoom :focus :iris]) (conj :beam-shape)
      (attributes :strobe) (conj :strobe)
      (attributes :fog) (conj :atmosphere))))

(defn effect
  "Build and validate a portable definition. Options include schemas/shorthands
  in :params, :outputs, targets, envelope, duration and version. Extra output
  layers append in order. Defaults: version 1 and one-bar one-shot duration.
  Compilation validates data but does not evaluate or launch the effect."
  [id options & layers]
  (when-not (map? options) (fail! [] "Effect options must be a map"))
  (when (and (contains? options :outputs) (not (or (map? (:outputs options)) (vector? (:outputs options)))))
    (fail! [:outputs] "Effect outputs must be an attribute map or ordered vector"))
  (let [outputs (mapv #(update % :value expression)
                      (into (registry/output-entries (:outputs options {})) (apply compose layers)))
        attributes (set (map :attribute outputs))
        emitter? (or (attributes :color) (some attributes registry/emitter-attributes))
        moving? (some attributes [:aim :direction :pan :tilt])
        capabilities (cond-> [] emitter? (conj :emission) (attributes :dimmer) (conj :intensity)
                       moving? (conj :movement) (some attributes [:zoom :focus]) (conj :beam-shape))
        definition (merge {:version 1 :duration {:bars 1} :capabilities capabilities :modifiers []
                           :requires (if (and moving? (not emitter?)) [:emission] [])}
                          options {:id id :params (params (:params options {})) :outputs outputs
                                   :categories (output-categories outputs)})]
    (compiler/compile-plan definition)
    definition))

(defn- wash-params [options]
  (merge {:level (parameter (:level options 0.7))
          :tint (parameter (:color options [1.0 0.04 0.01]) {:type :color})}
         (:params options)))

(defn wash
  "A reusable linear-RGB color and dimmer effect with editable :tint/:level params."
  ([id] (wash id {}))
  ([id options]
   (effect id (assoc (dissoc options :color :level) :params (wash-params options))
           (color (param :tint)) (intensity (param :level)))))

(defn breathing-wash
  "Color wash with musical intensity modulation. :wave holds oscillator options
  plus optional :shape (default sine), :axis and :wavelength for spatial motion."
  ([id] (breathing-wash id {}))
  ([id options]
   (let [wave-options (:wave options {}) shape (:shape wave-options :sine)
         wave-options (dissoc wave-options :shape)
         oscillator (if (some #(contains? wave-options %) [:axis :wavelength])
                      (position-wave shape wave-options) (wave shape wave-options))]
     (effect id (assoc (dissoc options :color :level :wave) :params (wash-params options)
                      :modifiers (vec (distinct (concat (:modifiers options) [:oscillator shape]
                                                        (when (some #(contains? wave-options %) [:axis :wavelength]) [:spatial])))))
             (color (param :tint)) (intensity (multiply (param :level) oscillator))))))

(defn aim-sweep
  "Move a world-space target left/right around :center, with editable centre,
  width and zoom. Width is metres (default 4, bounds 0..12). This movement-only
  recipe requires an emitting cue; it does not add color or intensity."
  ([id] (aim-sweep id {}))
  ([id options]
   (let [schemas (merge {:center (parameter (:center options [0.0 0.0 0.0]) {:type :vector :size 3})
                         :width (parameter (:width options 4.0) {:bounds (:width-bounds options [0.0 12.0]) :control/axis :glide})
                         :zoom (parameter (:zoom options 0.2))}
                        (:params options))
         oscillator (sine (merge {:period {:bars 2} :low -1.0 :high 1.0} (:wave options)))
         target (add (param :center) (xyz (multiply (param :width) oscillator) 0.0 0.0))]
     (effect id (assoc (dissoc options :center :width :width-bounds :zoom :wave) :params schemas
                      :modifiers (vec (distinct (concat (:modifiers options) [:oscillator :sine]))))
             (aim target) (zoom (param :zoom))))))
