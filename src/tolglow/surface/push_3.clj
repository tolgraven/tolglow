(ns tolglow.surface.push-3
  "Push 3 on the Tolglow surface layer: the device mapping around the
  Afterglow Push 3 User Mode profile. It knows the captured pad and button
  addresses, the pad orientation (Push pad IDs count rows from the bottom;
  viewport rows count from the top) and the LED bytes. Grid behaviour,
  content, commands and the shared selection are device-independent
  (tolglow.surface.cue-grid, tolglow.surface.push).

  It wraps the Afterglow profile instead of replacing it; each part is
  opt-in and every control it does not claim behaves as before.
  - :cue-grid (#545): buttons bound under :cue-grid {:buttons {id action}}
    open Select or Reorder and page the grid; while a mode is open the grid
    owns new pad presses, and each pad's release goes to whoever owned its
    press. There is one selection: the surface's committed :selected
    ([:instance id]) and the launch the Push encoders and display edit are
    kept equal (tolglow.surface.push). :cue-variation-pads? is refused
    alongside :cue-grid: both own the pads.
  - Pad overlay LEDs (#547): with the cue variation pad overlay
    (:cue-variation-pads?), the pads it shows are lit (knob looks by slot
    state, the other layouts by their colours) and turned off when it closes.
  - :capture-button id (#671): a captured, unbound User Mode button that runs
    the host's :capture! once per press, outside the session lock.
  - :vector-encoders {:encoders [id ...] :buttons [id ...] :linger-ms n
    :leds {...}} (#813): touching an encoder bound ({:param k} in
    :encoder-bindings) to a ring, XY or XY+Z vector spreads its components
    over the captured :encoders, listed left to right, even over encoders
    bound to other parameters (tolglow.surface.vector-encoders). The
    bindings are never changed, so the encoders return to them exactly when
    the takeover closes. Optional :buttons, one captured unbound button per
    encoder, light in each component's colour, white while touched.

  LEDs: Afterglow sends no Push 3 pad LEDs, so this namespace lights only
  the pads it shows and turns them off afterwards. The default bytes follow
  the Push 2 MIDI interface (note-on velocity is a palette index, the note-on
  channel picks the animation, note-off is black, button CC value is the
  brightness). That is unverified on Push 3 (#545 hardware checklist);
  :cue-grid {:leds {...}} and :pad-leds replace any part of default-leds."
  (:require [afterglow.control.devices.push-3 :as push-3]
            [afterglow.control.views :as views]
            [tolglow.controllers.push-variations :as push-variations]
            [tolglow.surface.cue-grid :as cue-grid]
            [tolglow.surface.descriptor :as descriptor]
            [tolglow.surface.push :as push]
            [tolglow.surface.vector-encoders :as vectors]))

(def viewport :grid)

(def geometry (get-in descriptor/push-3-grid [:viewports :grid]))

(def default-leds
  "LED encoding, Push 2 MIDI interface scheme (unverified on Push 3).
  :palette maps velocity -> [r g b]; the nearest entry is sent.
  :animations are note-on channels; :button-on/:button-off are CC values."
  {:palette {0 [0 0 0] 122 [1 1 1] 123 [0.5 0.5 0.5] 124 [0.25 0.25 0.25]
             125 [0 0 1] 126 [0 1 0] 127 [1 0 0]}
   :animations {:static 0 :pulse 9 :blink 14}
   :button-on 127 :button-off 0})

(def knob-look-leds
  "Pad intents for the knob-look slots (#547), one per slot state, chosen so
  each state is a different entry of the default palette: dim grey, green,
  white and blue."
  {:empty {:color [0.25 0.25 0.25]} :stored {:color [0 1 0]}
   :selected {:color [1 1 1]} :selected-moved {:color [0 0 1]}})

(def ^:private own-keys
  "Each add-on keeps its own state key: the grid ::push/grid and its LEDs
  ::sent, the overlay LEDs ::overlay-sent, Capture ::push/capture, and the
  vector takeover ::push/vector, its button LEDs ::vector-sent and the
  display view it last decorated ::vector-shown."
  [::push/grid ::sent ::overlay-sent ::push/capture ::push/vector ::vector-sent ::vector-shown])

;; ---------------------------------------------------------------------------
;; Addresses

(defn viewport-position
  "Push pad [x y] (row 0 at the bottom) <-> viewport [column row] (row 0 at
  the top). The same flip both ways."
  [[x y]]
  [x (- (dec (second geometry)) y)])

(defn palette-index
  "The lit palette velocity nearest an [r g b] colour. Black entries are
  never chosen: an occupied pad stays visible however dim its launch's
  colour, and so do its pulse and blink."
  [palette color]
  (let [lit (or (seq (remove (fn [[_ rgb]] (every? zero? rgb)) palette)) (seq palette))]
    (key (apply min-key (fn [[_ rgb]] (reduce + (map #(let [d (- (double %1) (double %2))] (* d d)) rgb color)))
                (sort-by key lit)))))

(defn pad-message
  "LED bytes for one pad note and intent (nil: dark, note-off). An intent's
  :animation, when given, overrides the one :selected?/:source? choose."
  [{:keys [palette animations]} note {:keys [color selected? source? animation] :as intent}]
  (if (nil? intent)
    [0x80 note 0]
    [(bit-or 0x90 (get animations (or animation (cond source? :blink selected? :pulse :else :static)) 0))
     note (palette-index palette color)]))

(defn button-message [{:keys [button-on button-off]} controller on?]
  [0xb0 controller (if on? button-on button-off)])

;; ---------------------------------------------------------------------------
;; Options

(def ^:private factory-buttons
  "Shift and Accent, which the Afterglow profile adds at these addresses."
  #{{:channel 0 :controller 49} {:channel 0 :controller 57}})

(defn- free-button?
  "A captured User Mode button the profile does not already use."
  [{:keys [layout button-bindings temporary-encoder-modifier]} id]
  (let [captured (:buttons layout)]
    (and (contains? captured id)
         (not (contains? button-bindings id))
         (not (contains? #{:accent :shift temporary-encoder-modifier} id))
         (not (contains? factory-buttons (get captured id))))))

(defn validate-options!
  "Check :cue-grid before any port opens: buttons are captured, unbound
  User Mode buttons mapped to cue-grid/actions, including a mode; the
  Afterglow profile runs in an aftertouch mode with captured pads and
  without the cue variation pad overlay, which also owns the pads."
  [{:keys [layout expression-mode cue-grid cue-variation-pads?] :as options}]
  (let [{:keys [buttons columns leds]} cue-grid]
    (when-not (and (map? cue-grid) (every? #{:buttons :columns :leds} (keys cue-grid))
                   (map? buttons) (seq buttons)
                   (every? cue-grid/actions (vals buttons))
                   (some #{:cue-grid/select :cue-grid/reorder} (vals buttons))
                   (every? (partial free-button? options) (keys buttons))
                   (seq (:pads layout))
                   (not= :mpe expression-mode)
                   (not (true? cue-variation-pads?))
                   (or (nil? columns) (and (pos-int? columns) (<= columns 64)))
                   (or (nil? leds) (and (map? leds) (every? #{:palette :animations :button-on :button-off} (keys leds)))))
      (throw (ex-info (str "A Push 3 cue grid needs :buttons mapping captured, unbound User Mode buttons to "
                           "cue-grid actions (including :cue-grid/select or :cue-grid/reorder), captured pads "
                           "and an aftertouch expression mode, without :cue-variation-pads?")
                      {:push-3/error :invalid-cue-grid :cue-grid cue-grid})))
    cue-grid))

(defn validate-capture!
  "Check :capture-button before any port opens: a captured, unbound User
  Mode button that no cue grid action uses."
  [{:keys [capture-button cue-grid] :as options}]
  (when-not (and (free-button? options capture-button)
                 (not (contains? (:buttons cue-grid) capture-button)))
    (throw (ex-info "The Push 3 Capture button must be a captured, unbound User Mode button"
                    {:push-3/error :invalid-capture-button :capture-button capture-button})))
  capture-button)

(defn validate-vector-encoders!
  "Check :vector-encoders before any port opens and normalize it to
  {:encoders :buttons :linger-ms :leds}: two to eight distinct captured
  encoders, left to right; optional buttons, one per encoder, captured,
  unbound and used by no other part; and an aftertouch expression mode."
  [{:keys [layout vector-encoders expression-mode cue-grid capture-button] :as options}]
  (let [{:keys [encoders buttons linger-ms leds]} vector-encoders]
    (when-not (and (map? vector-encoders) (every? #{:encoders :buttons :linger-ms :leds} (keys vector-encoders))
                   (vector? encoders) (<= 2 (count encoders) vectors/max-components) (apply distinct? encoders)
                   (every? #(contains? (:encoders layout) %) encoders)
                   (or (nil? buttons)
                       (and (vector? buttons) (= (count encoders) (count buttons)) (apply distinct? buttons)
                            (every? (partial free-button? options) buttons)
                            (not-any? #(contains? (:buttons cue-grid) %) buttons)
                            (not-any? #{capture-button} buttons)))
                   (or (nil? linger-ms) (and (number? linger-ms) (pos? linger-ms) (Double/isFinite (double linger-ms))))
                   (vectors/valid-leds? leds)
                   (not= :mpe expression-mode))
      (throw (ex-info (str "Push 3 :vector-encoders needs :encoders, two to eight captured encoder IDs left to "
                           "right, optional :buttons (one captured, unbound button each), :linger-ms and :leds, "
                           "in an aftertouch expression mode")
                      {:push-3/error :invalid-vector-encoders :vector-encoders vector-encoders})))
    {:encoders encoders :buttons buttons
     :linger-ms (or linger-ms vectors/default-linger-ms)
     :leds (merge vectors/default-leds leds)}))

(defn validate-pad-leds! [{:keys [pad-leds]}]
  (when-not (or (nil? pad-leds)
                (and (map? pad-leds) (every? #{:palette :animations :button-on :button-off} (keys pad-leds))))
    (throw (ex-info "Push 3 :pad-leds takes :palette, :animations, :button-on and :button-off"
                    {:push-3/error :invalid-pad-leds :pad-leds pad-leds}))))

(defn content
  "The surface content a Push 3 grid attaches with."
  [{:keys [cue-grid]}]
  (cond-> {:type :active-cues}
    (:columns cue-grid) (assoc :columns (:columns cue-grid))))

;; ---------------------------------------------------------------------------
;; Cue grid

(defn- view [{:keys [surface-id content]} model]
  (cue-grid/member-view model surface-id viewport geometry content))

(defn- grid-input [{:keys [surface-id buttons] :as options} underlying state event model envelope]
  (let [{:keys [type control-id pressed?] :as decoded} event
        v (view options model)
        state (push/adopt-selection state model v)
        grid (::push/grid state)
        normalized (when (#{:pad :poly-pressure} type)
                     (some-> (descriptor/push-3-event surface-id decoded) (update :at viewport-position)))
        identity (:identity normalized)
        apply-grid (fn [{g :state commands :commands selection :selection}]
                     ;; Select re-applies even the committed launch: the encoders
                     ;; return to it after any local drift.
                     {:state (push/select-pressed (assoc state ::push/grid g) model selection)
                      :commands (push/fence model commands)})]
    (cond
      (and (= :button type) (contains? buttons control-id))
      (if pressed?
        (apply-grid (cue-grid/action grid surface-id viewport (get buttons control-id)))
        {:state state})

      (and (= :pad type) (not pressed?) (cue-grid/owns? grid identity))
      (apply-grid (cue-grid/release grid identity))

      (and (= :pad type) pressed? (or (cue-grid/open? grid) (cue-grid/owns? grid identity)))
      (apply-grid (cue-grid/press grid model v surface-id viewport normalized))

      (and (= :poly-pressure type) (cue-grid/owns? grid identity))
      {:state state}

      :else (or (underlying state event model envelope) {:state state}))))

(defn- grid-feedback [{:keys [layout buttons leds] :as options} underlying state model envelope]
  (let [v (view options model)
        state (push/follow-committed state model v)
        base (if underlying (or (underlying state model envelope) {:state state}) {:state state})
        state (push/track-selection (:state base state) model v envelope)
        grid (::push/grid state)
        {projected :state pads :pads} (cue-grid/project grid model v)
        open? (cue-grid/open? grid)
        sent (::sent state {})
        {:keys [up? down?]} (cue-grid/paging v)
        desired (into {}
                      (concat
                       (keep (fn [[xy {:keys [note]}]]
                               (let [k [:pad xy]]
                                 ;; A closed grid turns off only what it lit.
                                 (when (or open? (contains? sent k))
                                   [k (pad-message leds note (when open? (get pads (viewport-position xy))))])))
                             (:pads layout))
                       (map (fn [[id action]]
                              [[:button id]
                               (button-message leds (get-in layout [:buttons id :controller])
                                               (case action
                                                 :cue-grid/select (= :select (:mode grid))
                                                 :cue-grid/reorder (= :reorder (:mode grid))
                                                 :cue-grid/page-up (and open? up?)
                                                 :cue-grid/page-down (and open? down?)))])
                            buttons)))
        messages (mapv val (filter (fn [[k message]] (not= message (get sent k))) (sort-by (comp pr-str key) desired)))]
    (-> base
        (assoc :state (assoc state ::push/grid projected ::sent (merge sent desired)))
        (update :midi (fnil into []) messages))))

(defn- reset-grid [state] (assoc state ::push/grid cue-grid/initial-state ::sent {}))

(defn- grid-stop [{:keys [leds]} state result disconnected?]
  (let [lit (keep (fn [[k message]]
                    (when-not (or (= 0x80 (first message)) (= (:button-off leds) (peek message)))
                      (case (first k)
                        :pad [0x80 (second message) 0]
                        :button [0xb0 (second message) (:button-off leds)])))
                  (sort-by (comp pr-str key) (::sent state)))]
    (cond-> (update result :state reset-grid)
      (not disconnected?) (update :midi (fnil into []) lit))))

(defn- grid-stale-input [underlying state {:keys [type pressed? gesture-key] :as event} envelope]
  (let [identity (when gesture-key (into [:pad] gesture-key))
        grid (::push/grid state)]
    (cond
      (and (= :pad type) (not pressed?) (cue-grid/owns? grid identity))
      (update state ::push/grid #(:state (cue-grid/release % identity)))
      (and (= :pad type) pressed? (cue-grid/open? grid))
      (update-in state [::push/grid :pads] conj identity)
      underlying (underlying state event envelope)
      :else state)))

(defn with-cue-grid
  "Wrap an Afterglow Push 3 profile with the cue grid. options: :surface-id,
  :content (see content), :layout (captured), :buttons {id action} and
  :leds (merged over default-leds)."
  [profile options]
  (let [{:keys [input feedback stop reset-model stale-input ack initial-state]} profile]
    (assoc profile
           :capabilities (update-in (:capabilities profile) [:controls] (fnil conj #{}) :cue-grid)
           :fence-captured-commands? true
           :unique-command-prefix? true
           :initial-state (assoc (or initial-state {}) ::push/grid cue-grid/initial-state ::sent {})
           :input (partial grid-input options input)
           :feedback (partial grid-feedback options feedback)
           :stop (push/guarded-stop stop (partial grid-stop options) reset-grid)
           :reset-model (fn [state]
                          (assoc (if reset-model (reset-model state) state)
                                 ::push/grid (cue-grid/reset-model (::push/grid state cue-grid/initial-state))
                                 ::sent (::sent state {})))
           :stale-input (partial grid-stale-input stale-input)
           :ack (fn [state command receipt]
                  (if (contains? push/own-ops (:op command))
                    {:state state :commands []}
                    (ack state command receipt))))))

;; ---------------------------------------------------------------------------
;; Pad overlay LEDs (#547)

(defn overlay-intents
  "Pad intents by captured pad ID for an Afterglow pad overlay projection.
  Knob looks light each slot by its state (knob-look-leds); a layout with
  :colors (0-255) lights those; one with only display cells (beat ratios)
  lights its choices grey and the current one white."
  [{:keys [view colors display]}]
  (cond
    (= :knob-presets (:layout view))
    (into {} (map (fn [cell] [(:position cell) (knob-look-leds (push-variations/knob-preset-state cell))]))
          (:cells view))
    (seq colors)
    (into {} (map (fn [[position rgb]] [position {:color (mapv #(/ (double %) 255.0) rgb)}])) colors)
    :else
    (into {} (map (fn [{:keys [position selected?]}] [position {:color (if selected? [1 1 1] [0.5 0.5 0.5])}]))
          (:cells display))))

(defn- overlay-feedback [{:keys [layout leds]} underlying state model envelope]
  (let [base (or (underlying state model envelope) {:state state})
        current (:state base state)
        open? (some? (:pad-overlay current))
        intents (when open? (overlay-intents (:pad-overlay-projection current)))
        sent (::overlay-sent current {})
        desired (into {} (keep (fn [[xy {:keys [note]}]]
                                 ;; A closed overlay turns off only what it lit.
                                 (when (or open? (contains? sent xy))
                                   [xy (pad-message leds note (when open? (get intents xy)))])))
                      (:pads layout))
        messages (mapv val (filter (fn [[xy message]] (not= message (get sent xy))) (sort-by key desired)))]
    (-> base
        (assoc :state (assoc current ::overlay-sent (merge sent desired)))
        (update :midi (fnil into []) messages))))

(defn- reset-overlay [state] (assoc state ::overlay-sent {}))

(defn- overlay-stop [state result disconnected?]
  (let [lit (keep (fn [[_ [status note]]] (when-not (= 0x80 status) [0x80 note 0]))
                  (sort-by key (::overlay-sent state)))]
    (cond-> (update result :state reset-overlay)
      (not disconnected?) (update :midi (fnil into []) lit))))

(defn with-overlay-leds
  "Light the pads the Afterglow pad overlay shows. options: :layout, :leds."
  [profile options]
  (let [{:keys [feedback stop initial-state]} profile]
    (assoc profile
           :initial-state (assoc (or initial-state {}) ::overlay-sent {})
           :feedback (partial overlay-feedback options feedback)
           :stop (push/guarded-stop stop overlay-stop reset-overlay))))

;; ---------------------------------------------------------------------------
;; Capture button (#671)

(defn with-capture-button
  "Claim one captured button: a press queues a host capture."
  [profile button]
  (let [{:keys [input]} profile]
    (assoc profile
           :unique-command-prefix? true
           :input (fn [state {:keys [type control-id pressed?] :as event} model envelope]
                    (if (and (= :button type) (= button control-id))
                      {:state (cond-> state pressed? (push/capture-press envelope))}
                      (input state event model envelope))))))

;; ---------------------------------------------------------------------------
;; Vector takeover (#813)

(defn- edited-instance
  "The launch the encoders edit, as the profile's display picks it: its
  :selected-instance, else the latest launch unless a cue gesture is
  choosing one."
  [state model]
  (or (get-in model [:instances (:selected-instance state)])
      (when-not (get-in state [:cue-gesture :selection]) (last (views/active-instances model)))))

(defn- bound-vector
  "The spread of the vector bound to an encoder on the edited launch, or nil."
  [{:keys [encoder-bindings]} state model id]
  (let [instance (edited-instance state model)
        param (get-in encoder-bindings [id :param])]
    (when (and param (= :running (:phase instance)))
      (vectors/spread param (get-in instance [:definition :params param])))))

(defn- vector-valid? [options state model {:keys [instance-id param basis]}]
  (let [instance (edited-instance state model)
        spread (bound-vector options state model (:encoder basis))]
    (and (= instance-id (:id instance)) (= param (:param spread))
         (= (:size basis) (count (:components spread))))))

(defn- fine-held?
  "Shift held: a button bound to :shift, or the factory Shift the profile adds."
  [{:keys [button-bindings layout]} state]
  (some (fn [id] (or (= :shift (get button-bindings id)) (= :shift id)
                     (= {:channel 0 :controller 49} (get-in layout [:buttons id]))))
        (:buttons state)))

(defn- vector-input [{:keys [encoders linger-ms] :as options} underlying state event model envelope]
  (let [{:keys [type control-id pressed? delta]} event
        slot (when (#{:encoder :encoder-touch} type) (first (keep-indexed #(when (= control-id %2) %1) encoders)))
        v (::push/vector state vectors/initial-state)
        time-ms (:time-ms envelope)
        result (when slot
                 (case type
                   :encoder-touch
                   (if pressed?
                     (let [busy (into #{} (keep-indexed (fn [i id] (when (or (contains? (:touches state) id)
                                                                              (contains? (:encoder-touch-modes state) id))
                                                                      i)))
                                      encoders)
                           candidate (when-not (:dedicated-modifier-down? state)
                                       (when-let [spread (bound-vector options state model control-id)]
                                         (assoc spread :instance-id (:id (edited-instance state model))
                                                       :basis {:encoder control-id :size (count (:components spread))})))]
                       (some->> (vectors/touch v slot candidate busy (count encoders)) (hash-map :state)))
                     (some->> (vectors/lift v slot time-ms linger-ms) (hash-map :state)))
                   :encoder (vectors/turn v slot delta (fine-held? options state) model time-ms linger-ms)))]
    (if result
      {:state (assoc state ::push/vector (:state result)) :commands (vec (:commands result))}
      (or (underlying state event model envelope) {:state state}))))

(defn- vector-feedback [{:keys [layout buttons leds] :as options} underlying state model envelope]
  (let [base (if underlying (or (underlying state model envelope) {:state state}) {:state state})
        current (:state base state)
        v (::push/vector current vectors/initial-state)
        v (cond-> v
            (and (vectors/open? v) (not (vector-valid? options current model (:open v)))) vectors/close
            true (vectors/expire (:time-ms envelope)))
        current (assoc current ::push/vector v)
        view (or (:display base) (:last-display current))
        shown (::vector-shown current)
        decorated (when (and view (vectors/open? v)) (vectors/decorate view v model))
        display (cond decorated (when (or (:display base) (not= decorated shown)) decorated)
                      shown view)
        intents (vectors/led-intents v)
        sent (::vector-sent current {})
        desired (into {} (keep (fn [[slot id]]
                                 (let [intent (get intents slot)]
                                   (when (or intent (contains? sent id))
                                     [id [0xb0 (get-in layout [:buttons id :controller]) (vectors/led-value leds intent)]]))))
                      (map-indexed vector buttons))
        messages (mapv val (filter (fn [[id message]] (not= message (get sent id))) (sort-by (comp str key) desired)))]
    (cond-> (assoc base :state (assoc current ::vector-shown decorated ::vector-sent (merge sent desired)))
      display (assoc :display display)
      (seq messages) (update :midi (fnil into []) messages))))

(defn- reset-vectors [state]
  (assoc state ::push/vector (vectors/forget-touches (::push/vector state)) ::vector-sent {} ::vector-shown nil))

(defn- vector-stop [{:keys [leds]} state result disconnected?]
  (let [lit (keep (fn [[_ [status controller value]]] (when-not (= (:off leds) value) [status controller (:off leds)]))
                  (sort-by (comp str key) (::vector-sent state)))]
    (cond-> (update result :state reset-vectors)
      (not disconnected?) (update :midi (fnil into []) lit))))

(defn- vector-stale-input [{:keys [encoders]} underlying state {:keys [type control-id pressed?] :as event} envelope]
  (let [slot (first (keep-indexed #(when (= control-id %2) %1) encoders))
        state (if (and (= :encoder-touch type) (not pressed?) slot
                       (contains? (get-in state [::push/vector :held]) slot))
                (update-in state [::push/vector :held] disj slot)
                state)]
    (if underlying (underlying state event envelope) state)))

(defn with-vector-encoders
  "Wrap an isolated Afterglow Push 3 profile with the vector takeover.
  options: validate-vector-encoders! plus :layout, :encoder-bindings and
  :button-bindings."
  [profile options]
  (let [{:keys [input feedback stop reset-model stale-input initial-state]} profile]
    (assoc profile
           :capabilities (update-in (:capabilities profile) [:controls] (fnil conj #{}) :vector-encoders)
           :initial-state (assoc (or initial-state {}) ::push/vector vectors/initial-state ::vector-sent {} ::vector-shown nil)
           :input (partial vector-input options input)
           :feedback (partial vector-feedback options feedback)
           :stop (push/guarded-stop stop (partial vector-stop options) reset-vectors)
           :reset-model (fn [state]
                          (assoc (if reset-model (reset-model state) state)
                                 ::push/vector (vectors/forget-touches (::push/vector state))
                                 ::vector-sent (::vector-sent state {})
                                 ::vector-shown (::vector-shown state)))
           :stale-input (partial vector-stale-input options stale-input))))

;; ---------------------------------------------------------------------------
;; Profile and session

(defn profile
  "The Afterglow Push 3 profile for options with this namespace's parts:
  the vector takeover of :vector-encoders, the cue grid of :cue-grid, LEDs
  for a :pad-overlay, and :capture-button."
  [options]
  (let [{:keys [buttons leds]} (when (:cue-grid options) (validate-options! options))
        capture (when (some? (:capture-button options)) (validate-capture! options))
        _ (validate-pad-leds! options)
        vector-encoders (when (some? (:vector-encoders options)) (validate-vector-encoders! options))]
    (cond-> (push/isolate own-keys (push-3/profile (dissoc options :vector-encoders)))
      vector-encoders
      (with-vector-encoders (merge (select-keys options [:layout :encoder-bindings :button-bindings]) vector-encoders))
      (:cue-grid options)
      (with-cue-grid {:surface-id (:surface-id options (:session-id options)) :content (content options)
                      :layout (:layout options) :buttons buttons :leds (merge default-leds leds)})
      (:pad-overlay options)
      (with-overlay-leds {:layout (:layout options) :leds (merge default-leds (:pad-leds options))})
      capture
      (with-capture-button capture))))

(def grid-state
  "This session's cue grid state: :mode, :source, :shown and held pads."
  push/grid-state)

(def publish-pending! push/publish-pending!)

(def refresh! push/refresh!)

(defn connect!
  "Open a Push 3 User Mode session with this namespace's parts: the options
  of afterglow.control.devices.push-3/connect! (and devices.repl/open-push-3!)
  plus :cue-grid, :capture-button, :pad-leds, :vector-encoders, an
  optional host :publish!
  (command -> nil, must not wait on the owner) for selections the Push picks
  itself (:dispatch! is used without it) and :capture! for the Capture
  button. Without :schedule! it refreshes every 10 ms (gestures and the
  grid's feedback), and without :midi or :transport it opens the selected
  MIDI ports. A scheduled refresh and each MIDI input publish the outbox
  after they release the session lock; with :schedule! nil call refresh!
  here instead of afterglow session/refresh!."
  [options]
  (when-let [display (:display options)]
    (when-not (and (= :push-3 (:device display)) (= :view (:input display))
                   (every? #(fn? (get display %)) [:open! :render! :close!]))
      (throw (ex-info "Choose a tagged Push 3 display transport" {}))))
  (push/connect-session! options (profile options) viewport 10))
