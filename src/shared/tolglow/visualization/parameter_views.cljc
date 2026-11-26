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
