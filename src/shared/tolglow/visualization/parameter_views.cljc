(ns tolglow.visualization.parameter-views
  "Choose the visual for a typed parameter. Studio and the Push display read
  this one projection, so a parameter type opens the same widget on both.
  A colour draws as a swatch with component gradients. Only a three-component
  position, direction or orientation draws in the 3D vector space.")

(defn- tag [id] (when (or (keyword? id) (string? id)) (keyword id)))

(defn parameter-kind
  "The control family of a schema. Listed choices win over the storage type."
  [schema]
  (if (seq (:choices schema))
    :choice
    (or (tag (:type schema)) :number)))

(def color-semantics
  "Component layouts that describe a colour when a plain vector carries them."
  #{:color :colour :rgb :rgbw :rgba :hsl :hsla :hsv})

(defn color-kind
  "`:color` for colour components, `:gradient` for colour stops, else nil."
  [schema]
  (case (parameter-kind schema)
    :color :color
    :gradient :gradient
    :vector (when (color-semantics (tag (:semantic/type schema))) :color)
    nil))

(def color-layouts
  "The components of each colour layout, in order. Every component is a unit
  amount. A hue is a fraction of a turn, as on a Push hue encoder."
  {:rgb [:red :green :blue] :rgba [:red :green :blue :alpha] :rgbw [:red :green :blue :white]
   :hsl [:hue :saturation :lightness] :hsla [:hue :saturation :lightness :alpha]
   :hsv [:hue :saturation :brightness]})

(def component-letters
  {:red "R" :green "G" :blue "B" :alpha "A" :white "W"
   :hue "H" :saturation "S" :lightness "L" :brightness "V"})

(defn color-space
  "The layout of a colour's components: its declared `:semantic/type`, else RGB
  with an optional alpha."
  [schema]
  (let [declared (tag (:semantic/type schema))]
    (if (contains? color-layouts declared) declared :rgba)))

(defn component-labels
  "Letters for a parameter's components: authored labels, its colour layout's,
  else X Y Z W."
  [schema]
  (or (:component-labels schema)
      (when (= :color (color-kind schema)) (mapv component-letters (color-layouts (color-space schema))))
      ["X" "Y" "Z" "W"]))

(defn- clamp [value] (max 0 (min 1 value)))

(defn rgb->hsv
  "Hue, saturation and brightness of unit RGB amounts."
  [[r g b]]
  (let [hi (max r g b) lo (min r g b) delta (- hi lo)
        hue (cond (zero? delta) 0
                  (= hi r) (mod (/ (- g b) delta) 6)
                  (= hi g) (+ 2 (/ (- b r) delta))
                  :else (+ 4 (/ (- r g) delta)))]
    [(/ hue 6) (if (zero? hi) 0 (/ delta hi)) hi]))

(defn hsv->rgb [[h s v]]
  (let [sector (* 6 (mod h 1))
        channel (fn [n] (let [k (mod (+ n sector) 6)]
                          (- v (* v s (max 0 (min k (- 4 k) 1))))))]
    [(channel 5) (channel 3) (channel 1)]))

(defn hsl->rgb [[h s l]]
  (let [a (* s (min l (- 1 l)))
        channel (fn [n] (let [k (mod (+ n (* 12 (mod h 1))) 12)]
                          (- l (* a (max -1 (min (- k 3) (- 9 k) 1))))))]
    [(channel 0) (channel 8) (channel 4)]))

(defn color-rgba
  "Red, green and blue unit amounts and the alpha of finite colour components
  in a layout. A hue wraps, other amounts clamp to [0,1]. The white channel adds
  to each RGB channel, as the light of an RGBW fixture does. A missing alpha is
  opaque and a missing white is off."
  [layout components]
  (let [[a b c d] (vec components)
        [a b c] [(or a 0) (or b 0) (or c 0)]
        d (some-> d clamp)]
    (case layout
      (:hsl :hsla) (conj (hsl->rgb [a (clamp b) (clamp c)]) (if (= :hsla layout) (or d 1) 1))
      :hsv (conj (hsv->rgb [a (clamp b) (clamp c)]) 1)
      :rgbw (conj (mapv #(clamp (+ (clamp %) (or d 0))) [a b c]) 1)
      :rgb [(clamp a) (clamp b) (clamp c) 1]
      [(clamp a) (clamp b) (clamp c) (or d 1)])))

(defn spatial-kind
  "The spatial meaning of a three-component vector. A colour is never spatial."
  [schema]
  (when (and (= :vector (parameter-kind schema)) (= 3 (:size schema)) (not (color-kind schema)))
    (case (tag (:semantic/type schema))
      (:orientation :rotation) :orientation
      :direction :direction
      :position)))

(defn view-kind
  "`:color`, `:spatial`, `:history` or `:controls` for a schema and a requested
  view. A request for the 3D space on a colour still opens the colour widget."
  [schema preference]
  (let [mode (or (tag preference) :auto)
        kind (parameter-kind schema)
        color (color-kind schema)]
    (cond
      (#{:choice :enum :boolean} kind) :controls
      (= :gradient color) :color
      (= mode :history) :history
      (= mode :controls) (if color :color :controls)
      color :color
      (spatial-kind schema) :spatial
      :else :controls)))

(defn selected-mode
  "The view for an operator preference. Automatic follows the schema's own
  `:visualization` request."
  [schema preference]
  (view-kind schema (if (#{:auto nil} (tag preference)) (:visualization schema) preference)))

(defn resting-view
  "The view of a parameter that has no live history to plot."
  [schema]
  (view-kind schema :auto))

(defn push-view
  "The Push encoder slot. History needs an active source; a held, pending or
  manual value keeps the resting widget of its type."
  [{:keys [schema source-state]} preference]
  (let [mode (selected-mode schema preference)]
    (if (and (= :history mode) (not= :active (tag source-state)))
      (resting-view schema)
      mode)))

(defn studio-view
  "The Perform visual under one control for the operator's chosen view."
  [schema choice]
  (selected-mode schema choice))

(defn studio-choices
  "The views Perform offers for a parameter, resting view first."
  [schema]
  (case (resting-view schema)
    :spatial [:spatial :history]
    :color (if (= :gradient (color-kind schema)) [:color] [:color :history])
    [:history]))

(defn monitor-view
  "The single scope of a compact Perform strip."
  [schema]
  (let [kind (resting-view schema)]
    (if (#{:spatial :color} kind) kind :history)))

(defn history-toggle
  "Open the history, or return to the controls when it is already open."
  [schema preference]
  (if (= :history (selected-mode schema preference)) :controls :history))
