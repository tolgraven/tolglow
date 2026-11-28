(ns tolglow.surface.push-2
  "Push 2 on the Tolglow surface layer (#706): the device mapping around the
  Afterglow Push 2 profile. It knows the Push 2 button CCs, pad notes and
  LED encoding; the cue grid, its content and commands and the shared
  selection are device-independent (tolglow.surface.cue-grid,
  tolglow.surface.push), the same code the Push 3 mapping runs.

  Each part is opt-in and every control it does not claim behaves as before.
  - :cue-grid {:buttons {cc action} :columns n} (#545), or the older
    :active-cue-grid? true with default-buttons: Select and Reorder pad modes
    over the running launches, paged by the committed surface window, with
    one selection shared with Studio, the REPL, linked members and the Push
    encoders. It replaces the Afterglow profile's own active-cue-grid, which
    is never enabled from Tolglow. The cue variation overlay (Note) keeps
    working beside it: while that overlay is open the mode buttons do
    nothing, and opening the grid closes it.
  - :knob-page {:button cc ...} (#666): the page button is lit while the
    linked-knob page is on the track encoders.
  - :capture-button cc (#671): a press runs the host's :capture! once,
    outside the session lock.
  - :vector-encoders true or {:linger-ms n :buttons [cc x8] :leds {...}}
    (#813): touching the track encoder of a ring, XY or XY+Z vector spreads
    its components over consecutive track encoders, even over a full bank
    (tolglow.surface.vector-encoders). The bank is never written, so it
    returns exactly when the takeover closes. The upper-row button under
    each taken encoder (CC 102-109, the Push 2 MIDI interface's display
    buttons) is lit in its component's colour, white while touched;
    :buttons false leaves them dark.

  Addresses and LEDs, all from Afterglow's source-derived Push 2 profile
  and codec (afterglow.control.devices.push-2, codecs.push-2), unverified
  on hardware on this Mac:
  - Pads are notes 36-99, [x y] from the bottom-left pad
    (codecs.push-2/pad-coordinates). Viewport rows count from the top.
  - Pad colours go through the profile's own LED path: its acknowledged,
    batched palette writes (codecs.push-2/pad-led-messages), fed by the
    profile's :pad-overlay hook. A press is admitted only against the pad
    mapping the Push acknowledged (:pad-overlay-presented), so a pad acts on
    the launch the performer saw lit; a change of colour alone (a Reorder
    choice turning white) keeps that mapping admissible.
  - Buttons: Select CC 48 and Duplicate CC 88 open the modes and Page
    Left/Right CC 62/63 page, as push-2/active-grid-buttons and its paging
    did. Button LEDs are CC values: brightness on the white buttons, as the
    legacy ableton-push-2/set-button-color sends them; Duplicate is half
    lit (64) in Reorder until a launch is chosen, as push-2/mode-lights.
  The grid has no pad animations on Push 2 (the profile writes static
  palette colours): a selected launch is drawn at full brightness, the
  others dimmed, and a launch chosen for Reorder is white."
  (:require [afterglow.control.codecs.push-2 :as codec]
            [afterglow.control.devices.push-2 :as push-2]
            [afterglow.control.devices.push-view :as push-view]
            [afterglow.control.views :as views]
            [tolglow.surface.cue-grid :as cue-grid]
            [tolglow.surface.descriptor :as descriptor]
            [tolglow.surface.push :as push]
            [tolglow.surface.vector-encoders :as vectors]))

(def viewport :grid)

(def geometry (get-in descriptor/push-2-grid [:viewports :grid]))

(def default-buttons
  "Afterglow's Push 2 active cue grid buttons (push-2/active-grid-buttons
  and its Page Left/Right paging)."
  {48 :cue-grid/select 88 :cue-grid/reorder 62 :cue-grid/page-up 63 :cue-grid/page-down})

(def default-leds
  "Button CC values: lit, dark, and :button-half for Reorder before a launch
  is chosen (Afterglow's push-2/mode-lights sends 64 there)."
  {:button-on 127 :button-off 0 :button-half 64})

(def profile-ccs
  "Button and encoder CCs the Afterglow Push 2 profile interprets itself
  (push-2/input and display-action). Select (48) and Duplicate (88) are
  free once its active cue grid is off."
  (into #{15 31 49 51 57 59 62 63 79 85 86 110 111 112 113}
        (concat (range 20 28) (range 44 48) (range 71 79))))

(def ^:private own-keys
  "Each add-on keeps its own state key: the grid ::push/grid and its button
  LEDs ::grid-sent, the knob page LED ::knob-sent, Capture ::push/capture,
  the vector takeover ::push/vector, its button LEDs ::vector-sent and the
  display view it last decorated ::vector-shown."
  [::push/grid ::grid-sent ::knob-sent ::push/capture ::push/vector ::vector-sent ::vector-shown])

(def default-vector-buttons
  "The upper row of display buttons, one under each track encoder (Push 2
  MIDI interface: CC 102-109). Neither the Afterglow profile nor the other
  Tolglow parts use them."
  (vec (range 102 110)))

;; ---------------------------------------------------------------------------
;; Addresses and colours

(defn viewport-position
  "Push pad [x y] (row 0 at the bottom) <-> viewport [column row] (row 0 at
  the top). The same flip both ways."
  [[x y]]
  [x (- (dec (second geometry)) y)])

(def ^:private dim 0.3)

(defn pad-rgb
  "The 8-bit [r g b] a grid pad is drawn in: a launch's colour, dimmed
  unless it is the selected launch; white for the launch chosen in Reorder.
  A missing or near-black colour is drawn grey so an occupied pad is never
  dark."
  [{:keys [color selected? source?]}]
  (if source?
    [255 255 255]
    (let [valid? (and (sequential? color) (= 3 (count color))
                      (every? #(and (number? %) (Double/isFinite (double %))) color))
          color (if (and valid? (some #(> (double %) 0.05) color)) color [0.35 0.35 0.35])
          scale (if selected? 1.0 dim)]
      (mapv #(int (Math/round (* 255.0 scale (max 0.0 (min 1.0 (double %)))))) color))))

(defn button-message
  "CC bytes for a button: on? true lit, false dark, :half half lit."
  [{:keys [button-on button-off button-half]} controller on?]
  [0xb0 controller (case on? :half (or button-half button-on) true button-on button-off)])

;; ---------------------------------------------------------------------------
;; Options

(defn grid-options
  "The cue grid options of a connection, or nil: :cue-grid as given, or the
  default buttons for :active-cue-grid? true."
  [{:keys [cue-grid active-cue-grid?]}]
  (cond (some? cue-grid) cue-grid
        (true? active-cue-grid?) {:buttons default-buttons}))

(defn- modifier-cc [{:keys [temporary-encoder-modifier]}]
  (if (integer? temporary-encoder-modifier) temporary-encoder-modifier 57))

(defn vector-options
  "The vector takeover options, normalized to {:linger-ms :buttons :leds},
  or nil when off. :buttons are eight CCs, one under each track encoder,
  none of them in taken (CCs other controls use); false leaves no LEDs."
  [{:keys [vector-encoders]} taken]
  (when (and (some? vector-encoders) (not (false? vector-encoders)))
    (let [config (if (true? vector-encoders) {} vector-encoders)
          {:keys [linger-ms leds]} config
          buttons (get config :buttons default-vector-buttons)
          cc? #(and (integer? %) (<= 0 % 127))]
      (when-not (and (map? config) (every? #{:linger-ms :buttons :leds} (keys config))
                     (or (nil? linger-ms) (and (number? linger-ms) (pos? linger-ms) (Double/isFinite (double linger-ms))))
                     (or (false? buttons)
                         (and (vector? buttons) (= 8 (count buttons)) (every? cc? buttons) (apply distinct? buttons)
                              (not-any? (into profile-ccs taken) buttons)))
                     (vectors/valid-leds? leds))
        (throw (ex-info (str "Push 2 :vector-encoders takes true or {:linger-ms ms :buttons [eight free button CCs] "
                             ":leds {:components [palette indices] :touched i :off i}}")
                        {:push-2/error :invalid-vector-encoders :vector-encoders vector-encoders})))
      {:linger-ms (or linger-ms vectors/default-linger-ms)
       :buttons (when-not (false? buttons) buttons)
       :leds (merge vectors/default-leds leds)})))

(defn validate-options!
  "Check the Push 2 parts before any port opens: grid buttons are CCs
  mapped to cue-grid actions, including a mode, and the mode buttons are
  ones the profile does not interpret (paging may share Page Left/Right,
  which keep their profile use while the grid is closed); the knob page
  and Capture buttons are free and distinct from the grid's, and so are
  the vector takeover's LED buttons. Returns {:grid :capture :knob-button
  :vectors}."
  [{:keys [knob-page capture-button cue-variations?] :as options}]
  (let [grid (grid-options options)
        {:keys [buttons columns]} grid
        knob-button (:button knob-page)
        taken (hash-set 49 57 59 (modifier-cc options))
        cc? #(and (integer? %) (<= 0 % 127))]
    (when grid
      (when-not (and (map? grid) (every? #{:buttons :columns} (keys grid))
                     (map? buttons) (seq buttons)
                     (every? cc? (keys buttons))
                     (every? cue-grid/actions (vals buttons))
                     (some #{:cue-grid/select :cue-grid/reorder} (vals buttons))
                     (every? (fn [[cc action]]
                               (and (not (taken cc)) (not= cc knob-button)
                                    (or (#{:cue-grid/page-up :cue-grid/page-down} action)
                                        (not (profile-ccs cc)))))
                             buttons)
                     (or (nil? columns) (and (pos-int? columns) (<= columns 64))))
        (throw (ex-info (str "A Push 2 cue grid needs :buttons mapping button CCs to cue-grid actions, including "
                             ":cue-grid/select or :cue-grid/reorder on buttons the profile does not use")
                        {:push-2/error :invalid-cue-grid :cue-grid grid}))))
    (when (some? capture-button)
      (when-not (and (cc? capture-button) (not (profile-ccs capture-button)) (not (taken capture-button))
                     (not (contains? buttons capture-button)) (not= capture-button knob-button)
                     (not (and (true? cue-variations?) (#{50 58} capture-button))))
        (throw (ex-info "The Push 2 Capture button must be a button CC no other Push 2 control uses"
                        {:push-2/error :invalid-capture-button :capture-button capture-button}))))
    {:grid grid :capture capture-button :knob-button knob-button
     :vectors (vector-options options (cond-> (into taken (keys buttons))
                                        knob-button (conj knob-button)
                                        capture-button (conj capture-button)
                                        (true? cue-variations?) (into #{50 58})))}))

(defn surface?
  "Whether these Push 2 options need this namespace's profile."
  [options]
  (boolean (or (grid-options options) (some? (:knob-page options)) (some? (:capture-button options))
               (and (some? (:vector-encoders options)) (not (false? (:vector-encoders options)))))))

(defn content
  "The surface content a Push 2 grid attaches with."
  [options]
  (cond-> {:type :active-cues}
    (:columns (grid-options options)) (assoc :columns (:columns (grid-options options)))))

(def descriptor-id
  "The surface a Push 2 grid attaches as: its pad grid alone
  (descriptor/push-2-grid). The encoder bank is still the Afterglow
  profile's."
  :ableton/push-2-grid)

;; ---------------------------------------------------------------------------
;; Cue grid

(defn- view [{:keys [surface-id content]} model]
  (cue-grid/member-view model surface-id viewport geometry content))

(defn- grid-projection
  "The profile's pad overlay for an open grid: :colors by Push pad [x y],
  :view (what a press is admitted against once the LEDs are acknowledged)
  and :display."
  [options overlay model]
  (let [mode (::mode overlay)
        v (view options model)
        {pads :pads} (cue-grid/project {:mode mode :source (::source overlay)} model v)
        page (inc (quot (get-in v [:window :y] 0) (second geometry)))]
    {:colors (into {} (map (fn [[at intent]] [(viewport-position at) (pad-rgb intent)])) pads)
     :view {::mode mode :window (:window v)
            :shown (into {} (map (fn [[at {:keys [instance-id]}]] [at instance-id])) pads)
            :shown-identity [(:show-id model) (:epoch model)]}
     :display {:layout :cue-grid :mode mode :page page :count (:count v)
               :title (str (if (= :select mode) "Select" "Reorder") " running cues · page " page)}}))

(defn- pad-overlay
  "The profile's :pad-overlay: the grid while it is open, otherwise the
  host's own overlay (the cue variations) when there is one. Opening the
  grid replaces that overlay; Note does not reopen it while the grid is open."
  [options host]
  {:input (fn [overlay event model context]
            (cond (contains? overlay ::mode) nil
                  host ((:input host) overlay event model context)))
   :project (fn [overlay model prior]
              (cond (contains? overlay ::mode) (when (::mode overlay) (grid-projection options overlay model))
                    host ((:project host) overlay model prior)))})

(defn- sync-overlay
  "Put the grid's mode and chosen launch into the profile's overlay state.
  A closed grid projects nothing, which the profile takes as dismissal."
  [state model]
  (let [{:keys [mode source]} (::push/grid state)
        source (when (push/running? model source) source)
        overlay (:pad-overlay state)]
    (cond
      mode (cond-> (assoc state :pad-overlay {::mode mode ::source source})
             (not (contains? overlay ::mode)) (dissoc :target-picker))
      (contains? overlay ::mode) (assoc state :pad-overlay {::mode nil})
      :else state)))

(defn- pad-identity [{:keys [channel note]}] [:pad (or channel 0) note])

(defn- grid-input [{:keys [surface-id buttons] :as options} underlying state event model envelope]
  (if-not (:ready? state)
    (underlying state event model envelope)
    (let [{:keys [type controller raw-value note]} event
          v (view options model)
          state (push/adopt-selection state model v)
          grid (::push/grid state)
          action (when (= :control-change type) (get buttons controller))
          xy (when (and note (#{:note-on :note-off :poly-pressure} type)) (codec/pad-coordinates note))
          identity (when xy (pad-identity event))
          apply-grid (fn [{g :state commands :commands selection :selection}]
                       {:state (-> (push/select-pressed (assoc state ::push/grid g) model selection)
                                   (sync-overlay model))
                        :commands (push/fence model commands)})]
      (cond
        ;; While the host's own overlay (cue variations) holds the pads, the
        ;; mode buttons are consumed, as the profile did for its own grid.
        (and action (not (cue-grid/open? grid)) (#{:cue-grid/select :cue-grid/reorder} action)
             (or (and (some? (:pad-overlay state)) (not (contains? (:pad-overlay state) ::mode)))
                 (:pad-overlay-clearing? state)))
        {:state state}

        ;; Page buttons keep their profile use while the grid is closed.
        (and action (or (cue-grid/open? grid) (#{:cue-grid/select :cue-grid/reorder} action)))
        (if (pos? raw-value)
          (apply-grid (cue-grid/action grid surface-id viewport action))
          {:state state})

        (and xy (= :note-off type) (cue-grid/owns? grid identity))
        (apply-grid (cue-grid/release grid identity))

        (and xy (= :note-on type) (or (cue-grid/open? grid) (cue-grid/owns? grid identity)))
        (let [presented (or (:pad-overlay-presented state) (:acked grid))
              shown? (and (= (::mode presented) (:mode grid)) (= (:window presented) (:window v)))]
          (apply-grid (cue-grid/press (assoc grid
                                             :shown (if shown? (:shown presented) {})
                                             :shown-identity (when shown? (:shown-identity presented)))
                                      model v surface-id viewport
                                      {:at (viewport-position xy) :identity identity})))

        (and xy (= :poly-pressure type) (cue-grid/owns? grid identity))
        {:state state}

        :else (underlying state event model envelope)))))

(defn- grid-feedback [{:keys [buttons leds] :as options} underlying state model envelope]
  (let [v (view options model)
        state (-> (push/follow-committed state model v) (sync-overlay model))
        base (or (underlying state model envelope) {:state state})
        state (push/track-selection (:state base state) model v envelope)
        ;; The last acknowledged pad mapping stays admissible while only a
        ;; colour changes (a Reorder choice turning white is still in flight).
        presented (:pad-overlay-presented state)
        projected (get-in state [:pad-overlay-projection :view])
        grid (assoc (::push/grid state) :acked (cond presented presented
                                                     (= projected (get-in state [::push/grid :acked])) projected))
        open? (cue-grid/open? grid)
        {:keys [up? down?]} (cue-grid/paging v)
        sent (::grid-sent state {})
        desired (when (and (:ready? state) (nil? (:fault state)))
                  (into {} (map (fn [[cc action]]
                                  [cc (button-message leds cc (case action
                                                                :cue-grid/select (= :select (:mode grid))
                                                                ;; Half lit until a launch is chosen, as Afterglow's grid.
                                                                :cue-grid/reorder (when (= :reorder (:mode grid))
                                                                                    (if (push/running? model (:source grid)) true :half))
                                                                :cue-grid/page-up (and open? up?)
                                                                :cue-grid/page-down (and open? down?)))]))
                        buttons))
        messages (mapv val (filter (fn [[cc message]] (not= message (get sent cc))) (sort-by key desired)))]
    (-> base
        (assoc :state (cond-> (assoc state ::push/grid (assoc grid :source (when (push/running? model (:source grid))
                                                                             (:source grid))))
                        (seq desired) (assoc ::grid-sent (merge sent desired))))
        ;; Before the profile's own messages, which end with its LED
        ;; acknowledgement query.
        (update :midi #(into messages %)))))

(defn- lit-buttons-off [{:keys [button-off]} sent]
  (keep (fn [[cc [_ _ value]]] (when-not (= button-off value) [0xb0 cc button-off])) (sort-by key sent)))

(defn- reset-grid [state] (assoc state ::push/grid cue-grid/initial-state ::grid-sent {}))

(defn- grid-stop [{:keys [leds]} state result disconnected?]
  (cond-> (update result :state reset-grid)
    (not disconnected?) (update :midi (fnil into []) (lit-buttons-off leds (::grid-sent state)))))

(defn- grid-stale-input [underlying state {:keys [type note] :as event} envelope]
  (let [xy (when (and note (#{:note-on :note-off} type)) (codec/pad-coordinates note))
        identity (when xy (pad-identity event))
        grid (::push/grid state)]
    (cond
      (and xy (= :note-off type) (cue-grid/owns? grid identity))
      (update state ::push/grid #(:state (cue-grid/release % identity)))
      (and xy (= :note-on type) (cue-grid/open? grid))
      (update-in state [::push/grid :pads] conj identity)
      underlying (underlying state event envelope)
      :else state)))

(defn with-cue-grid
  "Wrap an isolated Afterglow Push 2 profile (built with this namespace's
  :pad-overlay) with the cue grid. options: :surface-id, :content, :buttons
  {cc action} and :leds."
  [profile options]
  (let [{:keys [input feedback stop reset-model stale-input ack initial-state]} profile]
    (assoc profile
           :capabilities (update (:capabilities profile) :implemented (fnil conj #{}) :tolglow-cue-grid)
           :fence-captured-commands? true
           :unique-command-prefix? true
           :initial-state (assoc (or initial-state {}) ::push/grid cue-grid/initial-state ::grid-sent {})
           :input (partial grid-input options input)
           :feedback (partial grid-feedback options feedback)
           :stop (push/guarded-stop stop (partial grid-stop options) reset-grid)
           :reset-model (fn [state]
                          (assoc (if reset-model (reset-model state) state)
                                 ::push/grid (cue-grid/reset-model (::push/grid state cue-grid/initial-state))
                                 ::grid-sent (::grid-sent state {})))
           :stale-input (partial grid-stale-input stale-input)
           :ack (fn [state command receipt]
                  (if (contains? push/own-ops (:op command))
                    {:state state :commands []}
                    (ack state command receipt))))))

;; ---------------------------------------------------------------------------
;; Knob page LED (#666)

(defn with-knob-page-led
  "Light the knob page button while the linked-knob page is shown on the
  track encoders (tolglow.controllers.push-knobs, the profile's encoder
  overlay). Its LED state is ::knob-sent {cc message}."
  [profile button leds]
  (let [{:keys [feedback stop initial-state]} profile]
    (assoc profile
           :initial-state (assoc (or initial-state {}) ::knob-sent {})
           :feedback (fn [state model envelope]
                       (let [base (or (feedback state model envelope) {:state state})
                             current (:state base state)
                             shown? (some? (get-in current [:encoder-overlay :knob-ids]))
                             message (button-message leds button shown?)]
                         (if (and (:ready? current) (nil? (:fault current))
                                  (not= message (get-in current [::knob-sent button])))
                           (-> base (assoc-in [:state ::knob-sent button] message) (update :midi #(into [message] %)))
                           base)))
           :stop (push/guarded-stop
                  stop
                  (fn [state result disconnected?]
                    (let [lit? (= (button-message leds button true) (get-in state [::knob-sent button]))]
                      (cond-> (assoc-in result [:state ::knob-sent] {})
                        (and lit? (not disconnected?)) (update :midi (fnil conj []) (button-message leds button false)))))
                  #(assoc % ::knob-sent {})))))

;; ---------------------------------------------------------------------------
;; Capture button (#671)

(defn with-capture-button
  "Claim one button CC: a press queues a host capture."
  [profile button]
  (let [{:keys [input]} profile]
    (assoc profile
           :unique-command-prefix? true
           :input (fn [state {:keys [type controller raw-value] :as event} model envelope]
                    (if (and (:ready? state) (= :control-change type) (= button controller))
                      {:state (cond-> state (pos? raw-value) (push/capture-press envelope))}
                      (input state event model envelope))))))

;; ---------------------------------------------------------------------------
;; Vector takeover (#813)

(defn- selected-instance
  "The launch the track encoders edit, as the profile picks it: its
  :selected-instance, else the latest launch."
  [state model]
  (or (get-in model [:instances (:selected-instance state)])
      (last (views/active-instances model))))

(defn- knob-page? [state] (some? (get-in state [:encoder-overlay :knob-ids])))

(defn- vector-candidate
  "What a touch on track encoder slot would spread: the bank's parameter
  in that slot, when it is a vector the takeover spreads. :basis is what
  must stay the same for the takeover to stay valid."
  [state model slot]
  (let [instance (selected-instance state model)
        bank (push-view/parameter-bank state instance)
        param (get (:ids bank) slot)
        schema (get-in instance [:definition :params param])]
    (when (and param (= :running (:phase instance)))
      (when-let [spread (vectors/spread param schema)]
        (assoc spread :instance-id (:id instance)
                      :basis {:bank (:index bank) :slot slot :size (count (:components spread))})))))

(defn- vector-valid?
  "A running takeover still edits the selected launch, on the same bank,
  with the same vector in its slot, and nothing else holds the encoders."
  [state model {:keys [instance-id param basis]}]
  (let [instance (get-in model [:instances instance-id])
        bank (push-view/parameter-bank state instance)]
    (and (:ready? state) (nil? (:fault state)) (not (knob-page? state))
         (= instance-id (:id (selected-instance state model)))
         (= :running (:phase instance))
         (= (:bank basis) (:index bank))
         (= param (get (:ids bank) (:slot basis)))
         (= (:size basis) (count (:components (vectors/spread param (get-in instance [:definition :params param]))))))))

(defn- busy-slots
  "Track encoders the profile holds a touch or touch mode for."
  [state]
  (set (filter #(and (integer? %) (<= 0 % 7)) (concat (keys (:touches state)) (keys (:encoder-touch-modes state))))))

(defn- vector-input [{:keys [linger-ms]} underlying state event model envelope]
  (let [{:keys [type note controller raw-value]} event
        v (::push/vector state vectors/initial-state)
        time-ms (:time-ms envelope)
        slot (cond (and (#{:note-on :note-off} type) (integer? note) (<= 0 note 7)) note
                   (and (= :control-change type) (integer? controller) (<= 71 controller 78)) (- controller 71))
        result (when (and slot (:ready? state))
                 (case type
                   :note-on (when-let [next-v (vectors/touch v slot
                                                             (when (and (not (knob-page? state))
                                                                        (not (:temporary-encoder-modifier-down? state)))
                                                               (vector-candidate state model slot))
                                                             (busy-slots state) 8)]
                              {:state next-v})
                   :note-off (when-let [next-v (vectors/lift v slot time-ms linger-ms)] {:state next-v})
                   :control-change (vectors/turn v slot (codec/signed-encoder raw-value) (:shift? state)
                                                 model time-ms linger-ms)
                   nil))]
    (if result
      {:state (assoc state ::push/vector (:state result)) :commands (vec (:commands result))}
      (underlying state event model envelope))))

(defn- vector-button-messages [{:keys [buttons leds]} state]
  (when (seq buttons)
    (let [intents (vectors/led-intents (::push/vector state))
          sent (::vector-sent state {})]
      (into {} (keep (fn [[slot cc]]
                       (let [intent (get intents slot)]
                         (when (or intent (contains? sent cc))
                           [cc [0xb0 cc (vectors/led-value leds intent)]]))))
            (map-indexed vector buttons)))))

(defn- vector-feedback [options underlying state model envelope]
  (let [base (or (underlying state model envelope) {:state state})
        current (:state base state)
        v (::push/vector current vectors/initial-state)
        v (cond-> v
            (and (vectors/open? v) (not (vector-valid? current model (:open v)))) vectors/close
            true (vectors/expire (:time-ms envelope)))
        current (assoc current ::push/vector v)
        ;; The profile's own view: what it just sent, else what it last sent.
        view (or (:display base) (:last-display current))
        shown (::vector-shown current)
        decorated (when (and view (vectors/open? v)) (vectors/decorate view v model))
        ;; A view the profile sends while the takeover is open always goes
        ;; out decorated; closing sends the profile's view back once.
        display (cond decorated (when (or (:display base) (not= decorated shown)) decorated)
                      shown view)
        live? (and (:ready? current) (nil? (:fault current)))
        desired (when live? (vector-button-messages options current))
        sent (::vector-sent current {})
        messages (vectors/led-messages desired sent)]
    (cond-> (assoc base :state (assoc current
                                      ::vector-shown decorated
                                      ::vector-sent (merge sent desired)))
      display (assoc :display display)
      (seq messages) (update :midi #(into messages %)))))

(defn- reset-vectors [state]
  (assoc state ::push/vector (vectors/forget-touches (::push/vector state)) ::vector-sent {} ::vector-shown nil))

(defn- vector-stop [{:keys [leds]} state result disconnected?]
  (let [lit (keep (fn [[cc [_ _ value]]] (when-not (= (:off leds) value) [0xb0 cc (:off leds)]))
                  (sort-by key (::vector-sent state)))]
    (cond-> (update result :state reset-vectors)
      (not disconnected?) (update :midi (fnil into []) lit))))

(defn- vector-stale-input [underlying state {:keys [type note] :as event} envelope]
  (let [v (::push/vector state vectors/initial-state)
        state (if (and (= :note-off type) (integer? note) (contains? (:held v) note))
                (update-in state [::push/vector :held] disj note)
                state)]
    (if underlying (underlying state event envelope) state)))

(defn with-vector-encoders
  "Wrap an isolated Afterglow Push 2 profile with the vector takeover:
  touch, lift and turn on the track encoders (notes 0-7, CC 71-78) go to
  it while it holds them, everything else to the profile. options from
  vector-options. Its state is ::push/vector, its LEDs ::vector-sent and
  the view it last decorated ::vector-shown."
  [profile options]
  (let [{:keys [input feedback stop reset-model stale-input initial-state]} profile]
    (assoc profile
           :capabilities (update (:capabilities profile) :implemented (fnil conj #{}) :vector-encoders)
           :initial-state (assoc (or initial-state {}) ::push/vector vectors/initial-state ::vector-sent {} ::vector-shown nil)
           :input (partial vector-input options input)
           :feedback (partial vector-feedback options feedback)
           :stop (push/guarded-stop stop (partial vector-stop options) reset-vectors)
           :reset-model (fn [state]
                          (assoc (if reset-model (reset-model state) state)
                                 ::push/vector (vectors/forget-touches (::push/vector state))
                                 ::vector-sent (::vector-sent state {})
                                 ::vector-shown (::vector-shown state)))
           :stale-input (partial vector-stale-input stale-input))))

;; ---------------------------------------------------------------------------
;; Profile and session

(defn profile
  "The Afterglow Push 2 profile for options with this namespace's parts. The
  Afterglow active cue grid is never enabled; :cue-grid or :active-cue-grid?
  runs the Tolglow grid instead."
  [options]
  (let [{:keys [grid capture knob-button vectors]} (validate-options! options)
        surface-id (:surface-id options (:session-id options))
        grid-config {:surface-id surface-id :content (content options)}
        afterglow-options (cond-> (dissoc options :active-cue-grid? :cue-grid :vector-encoders)
                            grid (assoc :pad-overlay (pad-overlay grid-config (:pad-overlay options))))]
    (cond-> (push/isolate own-keys (push-2/profile afterglow-options))
      vectors (with-vector-encoders vectors)
      grid (with-cue-grid (assoc grid-config :buttons (:buttons grid) :leds default-leds))
      knob-button (with-knob-page-led knob-button default-leds)
      capture (with-capture-button capture))))

(def grid-state push/grid-state)

(def publish-pending! push/publish-pending!)

(def refresh! push/refresh!)

(defn connect!
  "Open a Push 2 session with this namespace's parts: the options of
  afterglow.control.devices.push-2/connect! (and devices.repl/open-push-2!)
  plus :cue-grid or :active-cue-grid?, :knob-page, :capture-button,
  :vector-encoders, an
  optional host :publish! (command -> nil, must not wait on the owner) for
  selections the Push picks itself (:dispatch! without it) and :capture!.
  Without :schedule! it refreshes every :refresh-ms (40, as open-push-2!),
  and without :midi or :transport it opens the selected MIDI ports. With
  :schedule! nil call refresh! here instead of afterglow session/refresh!."
  [options]
  (when-let [device (get-in options [:display :device])]
    (when-not (= :push-2 device)
      (throw (ex-info "Select a Push 2 display transport" {:device device}))))
  (push/connect-session! options (profile options) viewport 40))
