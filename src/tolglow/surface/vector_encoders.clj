(ns tolglow.surface.vector-encoders
  "Spread a vector parameter over several encoders (#813). Touching the
  encoder of a numeric vector that Studio draws as a ring, XY or XY+Z
  control opens a takeover: its components go onto consecutive encoders,
  one component each, starting at the touched encoder (or as far right as
  they fit), even over encoders the bank already uses. The takeover closes
  when no taken encoder has been touched for :linger-ms, which lets a
  finger move from one component's encoder to the next. The bank itself is
  never written, so the encoders return to it exactly.

  Each detent on a taken encoder sends the owner-resolved relative edit
  `{:op :param/nudge :component i :detents n :fine? b}` (tolglow.param-nudge).
  The show moves that component of the committed vector by the component's
  step within its bounds and commits it as the ordinary `param/set`, so every
  detent counts however commands queue, a Studio edit to another component
  is never sent back, and a Push write is a human override like any other.

  Nothing here knows a MIDI address, an LED value or a device state shape.
  Encoders are slots 0-7 in the device's left-to-right order; the device
  mapping (tolglow.surface.push-2, tolglow.surface.push-3) decodes its events
  to slots, says which parameter a slot shows, and turns the LED intents into
  bytes. The state map lives under one add-on key of the device: :open, the
  running takeover, and :held, slots whose physical touch the takeover took
  (their lifts and turns stay its own until they lift, even after it closes)."
  (:require [tolglow.authoring.parameter-controls :as controls]
            [tolglow.param-nudge :as param-nudge]
            [tolglow.visualization.control-geometry :as geometry]
            [tolglow.visualization.parameter-views :as views]))

(def initial-state {:open nil :held #{}})

(def default-linger-ms
  "How long a takeover stays open after its last touch lifts: time to move a
  finger to the next component's encoder. A device boundary, so
  milliseconds."
  1000)

(def max-components 8)

;; ---------------------------------------------------------------------------
;; Which parameters spread

(defn spread
  "The components of a parameter the takeover spreads, or nil. A numeric
  :vector drawn as a compound control (:rings, :xy, :xyz or :axes, see
  parameter-controls/compound-widget) with two to eight components spreads;
  a colour vector, a scalar or any other type does not. Each component has
  the number schema a nudge edits it with (param-nudge/component-schemas:
  its own bounds within the vector's, and the vector's :step), a label and
  the colour its ring has in Studio."
  [param schema]
  (when (and (controls/compound-widget schema)
             (<= 2 (or (controls/component-count schema) 0) max-components))
    (when-let [schemas (param-nudge/component-schemas schema)]
      (let [labels (views/component-labels schema)]
        {:param param
         :name (controls/title param schema)
         :components (vec (map-indexed
                           (fn [i component]
                             {:index i
                              :label (str (get labels i (inc i)))
                              :schema component
                              :color (nth geometry/ring-colors (mod i (count geometry/ring-colors)))})
                           schemas))}))))

(defn window
  "The first slot of n consecutive slots, out of width, that holds slot:
  slot itself when the components fit to its right, else as far right as
  they fit."
  [slot n width]
  (max 0 (min slot (- width n))))

;; ---------------------------------------------------------------------------
;; Touches and lifts

(defn open?
  [state]
  (some? (:open state)))

(defn slots
  "The slots of the running takeover, in component order."
  [{:keys [open]}]
  (when open (vec (range (:start open) (+ (:start open) (count (:components open)))))))

(defn component-at
  "The component index a slot edits in the running takeover, or nil."
  [{:keys [open]} slot]
  (when (and open (<= (:start open) slot) (< slot (+ (:start open) (count (:components open)))))
    (- slot (:start open))))

(defn owns?
  "Whether an event on slot belongs to the takeover: a taken slot, or a
  slot still physically held from a touch the takeover took."
  [state slot]
  (boolean (or (component-at state slot) (contains? (:held state) slot))))

(defn touch
  "A touch on slot. Inside a running takeover it is one of its touches.
  Otherwise, candidate (from spread, plus :instance-id and :basis, what the
  device checks later to see the takeover is still valid) opens one when its
  window of width slots holds no slot in busy (touched outside the
  takeover). A finger still held from an earlier takeover inside the new
  window counts as one of its touches. Returns the new state, or nil when
  the touch is not the takeover's."
  [state slot candidate busy width]
  (cond
    (component-at state slot)
    (-> state (update-in [:open :touched] conj slot) (update :open dissoc :until) (update :held conj slot))

    (and candidate (not (open? state)) (not (contains? (:held state) slot)))
    (let [n (count (:components candidate))
          start (window slot n width)
          taken (set (range start (+ start n)))]
      (when (and (<= n width) (not-any? taken (disj (set busy) slot)))
        (-> state
            (assoc :open (assoc (select-keys candidate [:instance-id :param :name :components :basis])
                                :start start :origin slot
                                :touched (into #{slot} (filter taken) (:held state))))
            (update :held conj slot))))))

(defn lift
  "A lift on slot. Returns the new state, or nil when the slot is not the
  takeover's. The last lift starts the linger."
  [state slot time-ms linger-ms]
  (when (owns? state slot)
    (let [state (update state :held disj slot)]
      (if (component-at state slot)
        (let [state (update-in state [:open :touched] disj slot)]
          (cond-> state
            (empty? (get-in state [:open :touched])) (assoc-in [:open :until] (+ time-ms linger-ms))))
        state))))

(defn close
  "Close the takeover. Held touches stay held until they lift."
  [state]
  (assoc state :open nil))

(defn expire
  "Close a takeover whose linger has run out by time-ms."
  [state time-ms]
  (let [until (get-in state [:open :until])]
    (if (and until time-ms (>= time-ms until)) (close state) state)))

(defn refresh
  "Before an input or a feedback: close the takeover when valid? says it no
  longer applies (a changed launch, bank or slot), then when its linger has
  run out."
  [state valid? time-ms]
  (cond-> state
    (and (open? state) (not (valid? (:open state)))) close
    true (expire time-ms)))

(defn forget-touches
  "A fresh show or a stop: close the takeover and keep only the held
  slots, whose lifts must still be swallowed."
  [state]
  (assoc (close (or state initial-state)) :held (or (:held state) #{})))

;; ---------------------------------------------------------------------------
;; Turns

(defn turn
  "Detents on slot. A taken slot returns {:state :commands [param/nudge]}
  for its component; a slot still held after the takeover closed swallows
  the turn. An untouched turn keeps the takeover open for another linger.
  Returns nil when the slot is not the takeover's."
  [state slot detents fine? time-ms linger-ms]
  (when (owns? state slot)
    (if-let [i (component-at state slot)]
      (let [{:keys [open]} state
            state (cond-> state
                    (empty? (:touched open)) (assoc-in [:open :until] (+ time-ms linger-ms)))]
        {:state state
         :commands (if (and (integer? detents) (not (zero? detents)))
                     [{:op :param/nudge :instance-id (:instance-id open) :param (:param open)
                       :component i :detents detents :fine? (boolean fine?)}]
                     [])})
      {:state state :commands []})))

;; ---------------------------------------------------------------------------
;; Feedback

(defn- committed [model {:keys [instance-id param]}]
  (get-in model [:instances instance-id :params param]))

(defn- amount [schema v]
  (when-let [[low high] (controls/bounds schema)]
    (when (number? v) (/ (- (double (max low (min high v))) low) (- high low)))))

(defn- text [schema v]
  (if (number? v)
    (if (= :integer (:type schema))
      (str (long v))
      (format (str "%." (controls/decimal-places schema) "f") (double v)))
    "—"))

(defn projection
  "What the display shows for the running takeover: the parameter, the
  first slot and one entry per component with its slot, label, value,
  readout text, position in its bounds (nil if unbounded), ring colour and
  whether its encoder is touched. nil when closed."
  [{:keys [open]} model]
  (when open
    (let [value (committed model open)]
      {:instance-id (:instance-id open) :param (:param open) :name (:name open) :start (:start open)
       :components (mapv (fn [{:keys [index label schema color]}]
                           (let [slot (+ (:start open) index)
                                 v (when (vector? value) (nth value index nil))]
                             {:encoder slot :component index :label label :value v :text (text schema v)
                              :amount (amount schema v) :color color
                              :touched? (contains? (:touched open) slot)}))
                         (:components open))})))

(defn decorate
  "A device display view with the takeover over its encoder columns: the
  taken columns of :parameters become one row per component (the vector
  parameter with its :component, so Studio's Push display draws the group
  as one compound control), the takeover's touches are the view's touched
  parameters, and :vector-encoders carries the projection. A closed
  takeover returns the view unchanged."
  [view state model]
  (if-let [{:keys [instance-id param components] :as shown} (projection state model)]
    (let [{:keys [open]} state
          schema (get-in model [:instances instance-id :definition :params param])
          value (committed model open)
          width (max (count (:parameters view)) (inc (:encoder (peek components))))
          base (into (vec (:parameters view)) (repeat (- width (count (:parameters view))) nil))
          rows (reduce (fn [rows {:keys [encoder component label]}]
                         (assoc rows encoder {:id param :param param :name (:name open) :value value :schema schema
                                              :component component :component-label label :vector-component? true}))
                       base components)
          touches (vec (for [{:keys [encoder component touched?]} components :when touched?]
                         {:instance-id instance-id :param param :component component :encoder encoder}))]
      (cond-> (assoc view :parameters rows :vector-encoders shown)
        (seq touches) (assoc :touched-parameter (first touches) :touched-parameters touches)))
    view))

(defn led-intents
  "{slot {:component i :color \"#rrggbb\" :touched? b}} for the running
  takeover's slots; the device turns them into LED bytes."
  [{:keys [open] :as state}]
  (into {} (for [slot (slots state)
                 :let [i (component-at state slot)]]
             [slot {:component i :color (get-in open [:components i :color])
                    :touched? (contains? (:touched open) slot)}])))
