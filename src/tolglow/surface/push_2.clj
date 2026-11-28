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
    legacy ableton-push-2/set-button-color sends them.
  The grid has no pad animations on Push 2 (the profile writes static
  palette colours): a selected launch is drawn at full brightness, the
  others dimmed, and a launch chosen for Reorder is white."
  (:require [afterglow.control.codecs.push-2 :as codec]
            [afterglow.control.devices.push-2 :as push-2]
            [tolglow.surface.cue-grid :as cue-grid]
            [tolglow.surface.descriptor :as descriptor]
            [tolglow.surface.push :as push]))

(def viewport :grid)

(def geometry (get-in descriptor/push-2-grid [:viewports :grid]))

(def default-buttons
  "Afterglow's Push 2 active cue grid buttons (push-2/active-grid-buttons
  and its Page Left/Right paging)."
  {48 :cue-grid/select 88 :cue-grid/reorder 62 :cue-grid/page-up 63 :cue-grid/page-down})

(def default-leds
  "Button CC values: lit and dark."
  {:button-on 127 :button-off 0})

(def profile-ccs
  "Button and encoder CCs the Afterglow Push 2 profile interprets itself
  (push-2/input and display-action). Select (48) and Duplicate (88) are
  free once its active cue grid is off."
  (into #{15 31 49 51 57 59 62 63 79 85 86 110 111 112 113}
        (concat (range 20 28) (range 44 48) (range 71 79))))

(def ^:private own-keys [::push/grid ::sent ::push/capture])

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

(defn button-message [{:keys [button-on button-off]} controller on?]
  [0xb0 controller (if on? button-on button-off)])

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

(defn validate-options!
  "Check the Push 2 parts before any port opens: grid buttons are CCs
  mapped to cue-grid actions, including a mode, and the mode buttons are
  ones the profile does not interpret (paging may share Page Left/Right,
  which keep their profile use while the grid is closed); the knob page
  and Capture buttons are free and distinct from the grid's. Returns
  {:grid :capture :knob-button}."
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
    {:grid grid :capture capture-button :knob-button knob-button}))

(defn surface?
  "Whether these Push 2 options need this namespace's profile."
  [options]
  (boolean (or (grid-options options) (some? (:knob-page options)) (some? (:capture-button options)))))

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
        sent (::sent state {})
        desired (when (and (:ready? state) (nil? (:fault state)))
                  (into {} (map (fn [[cc action]]
                                  [cc (button-message leds cc (case action
                                                                :cue-grid/select (= :select (:mode grid))
                                                                :cue-grid/reorder (= :reorder (:mode grid))
                                                                :cue-grid/page-up (and open? up?)
                                                                :cue-grid/page-down (and open? down?)))]))
                        buttons))
        messages (mapv val (filter (fn [[cc message]] (not= message (get sent cc))) (sort-by key desired)))]
    (-> base
        (assoc :state (cond-> (assoc state ::push/grid (assoc grid :source (when (push/running? model (:source grid))
                                                                             (:source grid))))
                        (seq desired) (assoc ::sent (merge sent desired))))
        ;; Before the profile's own messages, which end with its LED
        ;; acknowledgement query.
        (update :midi #(into messages %)))))

(defn- lit-buttons-off [{:keys [button-off]} sent]
  (keep (fn [[cc [_ _ value]]] (when-not (= button-off value) [0xb0 cc button-off])) (sort-by key sent)))

(defn- grid-stop [{:keys [leds]} underlying state model envelope disconnected?]
  (let [result (or (underlying state model envelope disconnected?) {:state state})]
    (cond-> (assoc result :state (assoc (:state result) ::push/grid cue-grid/initial-state ::sent {}))
      (not disconnected?) (update :midi (fnil into []) (lit-buttons-off leds (::sent state))))))

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
           :initial-state (assoc (or initial-state {}) ::push/grid cue-grid/initial-state ::sent {})
           :input (partial grid-input options input)
           :feedback (partial grid-feedback options feedback)
           :stop (partial grid-stop options (or stop (fn [state & _] {:state state})))
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
;; Knob page LED (#666)

(defn with-knob-page-led
  "Light the knob page button while the linked-knob page is shown on the
  track encoders (tolglow.controllers.push-knobs, the profile's encoder
  overlay)."
  [profile button leds]
  (let [{:keys [feedback stop]} profile
        k [::knob-page button]]
    (assoc profile
           :feedback (fn [state model envelope]
                       (let [base (or (feedback state model envelope) {:state state})
                             current (:state base state)
                             shown? (some? (get-in current [:encoder-overlay :knob-ids]))
                             message (button-message leds button shown?)]
                         (if (and (:ready? current) (nil? (:fault current))
                                  (not= message (get-in current [::sent k])))
                           (-> base (assoc-in [:state ::sent k] message) (update :midi #(into [message] %)))
                           base)))
           :stop (fn [state model envelope disconnected?]
                   (let [result (stop state model envelope disconnected?)
                         lit? (= (button-message leds button true) (get-in state [::sent k]))]
                     (cond-> (update result :state #(update % ::sent dissoc k))
                       (and lit? (not disconnected?)) (update :midi (fnil conj []) (button-message leds button false))))))))

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
;; Profile and session

(defn profile
  "The Afterglow Push 2 profile for options with this namespace's parts. The
  Afterglow active cue grid is never enabled; :cue-grid or :active-cue-grid?
  runs the Tolglow grid instead."
  [options]
  (let [{:keys [grid capture knob-button]} (validate-options! options)
        surface-id (:surface-id options (:session-id options))
        grid-config {:surface-id surface-id :content (content options)}
        afterglow-options (cond-> (dissoc options :active-cue-grid? :cue-grid)
                            grid (assoc :pad-overlay (pad-overlay grid-config (:pad-overlay options))))]
    (cond-> (push/isolate own-keys (push-2/profile afterglow-options))
      grid (with-cue-grid (assoc grid-config :buttons (:buttons grid) :leds default-leds))
      knob-button (with-knob-page-led knob-button default-leds)
      capture (with-capture-button capture))))

(def grid-state push/grid-state)

(def publish-pending! push/publish-pending!)

(def refresh! push/refresh!)

(defn connect!
  "Open a Push 2 session with this namespace's parts: the options of
  afterglow.control.devices.push-2/connect! (and devices.repl/open-push-2!)
  plus :cue-grid or :active-cue-grid?, :knob-page, :capture-button, an
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
