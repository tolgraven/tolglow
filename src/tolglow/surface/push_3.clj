(ns tolglow.surface.push-3
  "Push 3 pad grid on the Tolglow surface layer (#545): the Select and
  Reorder modes of tolglow.surface.cue-grid on a Push 3 in User Mode.

  This namespace is the device mapping. It knows the captured pad and button
  addresses, the pad orientation (Push pad IDs count rows from the bottom;
  viewport rows count from the top) and the LED bytes. The grid behaviour,
  the content and the commands are device-independent.

  It wraps the Afterglow Push 3 User Mode profile instead of replacing it:
  with the grid closed every control behaves as before (pads launch,
  encoders edit). Buttons bound under :cue-grid {:buttons {id action}} open
  Select or Reorder and page the grid; while a mode is open the grid owns
  new pad presses, and each pad's release goes to whoever owned its press.
  There is one selection: the surface's committed :selected ([:instance id])
  and the launch the Push encoders and display edit are kept equal. A
  committed change (a pad, Studio, the REPL, a linked member) moves the
  encoders; a change the Afterglow profile makes itself (a launch pad, its
  ack, the display falling back to the latest launch) is committed as the
  shared selection, judged against the model after its commands landed:
  stopping a cue from a pad keeps the committed launch and points the
  encoders back at it. That publish goes through the host's :publish!
  outside the session lock, so feedback never waits on the show owner.
  Select on the selected pad points the encoders back at it. :cue-variation-pads? is refused alongside :cue-grid: both own the pads.

  LEDs: Afterglow sends no Push 3 pad LEDs, so the grid lights only the pads
  it shows and turns them off when it closes. The default bytes follow the
  Push 2 MIDI interface (note-on velocity is a palette index, the note-on
  channel picks the animation, note-off is black, button CC value is the
  brightness). That is unverified on Push 3 (#545 hardware checklist);
  :cue-grid {:leds {...}} replaces any part of default-leds."
  (:require [afterglow.control.devices.push-3 :as push-3]
            [afterglow.control.devices.session :as session]
            [afterglow.control.devices.transports :as transports]
            [tolglow.surface.cue-grid :as cue-grid]
            [tolglow.surface.descriptor :as descriptor]
            [tolglow.surface.link :as link]))

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

(def ^:private own-ops #{:control-surface/layout :control-surface/bank :instance/move-after})

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
  "LED bytes for one pad note and intent (nil: dark, note-off)."
  [{:keys [palette animations]} note {:keys [color selected? source?] :as intent}]
  (if (nil? intent)
    [0x80 note 0]
    [(bit-or 0x90 (get animations (cond source? :blink selected? :pulse :else :static) 0))
     note (palette-index palette color)]))

(defn button-message [{:keys [button-on button-off]} controller on?]
  [0xb0 controller (if on? button-on button-off)])

;; ---------------------------------------------------------------------------
;; Options

(def ^:private factory-buttons
  "Shift and Accent, which the Afterglow profile adds at these addresses."
  #{{:channel 0 :controller 49} {:channel 0 :controller 57}})

(defn validate-options!
  "Check :cue-grid before any port opens: buttons are captured, unbound
  User Mode buttons mapped to cue-grid/actions, including a mode; the
  Afterglow profile runs in an aftertouch mode with captured pads and
  without the cue variation pad overlay, which also owns the pads."
  [{:keys [layout button-bindings temporary-encoder-modifier expression-mode cue-grid cue-variation-pads?]}]
  (let [{:keys [buttons columns leds]} cue-grid
        captured (:buttons layout)]
    (when-not (and (map? cue-grid) (every? #{:buttons :columns :leds} (keys cue-grid))
                   (map? buttons) (seq buttons)
                   (every? cue-grid/actions (vals buttons))
                   (some #{:cue-grid/select :cue-grid/reorder} (vals buttons))
                   (every? (fn [id] (and (contains? captured id)
                                         (not (contains? button-bindings id))
                                         (not (contains? #{:accent :shift temporary-encoder-modifier} id))
                                         (not (contains? factory-buttons (get captured id)))))
                           (keys buttons))
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

(defn content
  "The surface content a Push 3 grid attaches with."
  [{:keys [cue-grid]}]
  (cond-> {:type :active-cues}
    (:columns cue-grid) (assoc :columns (:columns cue-grid))))

;; ---------------------------------------------------------------------------
;; Profile decorator

(defn- lift
  "Run an Afterglow profile function on the state without the grid's keys and
  put them back on its result."
  [state f & args]
  (let [own (select-keys state [::grid ::sent])
        result (apply f (dissoc state ::grid ::sent) args)]
    (if (and result (contains? result :state)) (update result :state merge own) result)))

(defn- view [{:keys [surface-id content]} model]
  (cue-grid/member-view model surface-id viewport geometry content))

(defn- fence [model commands]
  (mapv #(merge % (select-keys model [:show-id :epoch])) commands))

(defn- running? [model id] (= :running (get-in model [:instances id :phase])))

(defn- point-at
  "Make a running launch what the Push encoders and display edit."
  [state model id]
  (-> state
      (assoc :selected-instance id :selected-cue (get-in model [:instances id :cue-id]))
      (assoc-in [::grid :edited] id)))

;; One selection. The surface's committed :selected and the launch the
;; Afterglow profile edits (:selected-instance) are kept equal both ways.
;; ::grid keys: :adopted, the committed value last applied; :edited, the
;; profile target last seen; :publishing, a profile target waiting to be
;; committed; :outbox, the publish request for the host to send.

(defn- adopt-selection
  "A committed selection change (a pad, Studio, the REPL or a linked member)
  points the encoders at that launch and drops a pending publish, including
  a request already in the outbox."
  [state model v]
  (let [[kind id :as selected] (:selected v)]
    (if (= selected (get-in state [::grid :adopted]))
      state
      (cond-> (update state ::grid assoc :adopted selected :publishing nil :outbox nil)
        (and (= :instance kind) (running? model id)) (point-at model id)))))

(defn- follow-committed
  "Before the profile's feedback: adopt a committed change, and when the
  profile's own target is no longer running (a Push pad just stopped it),
  point back at the committed launch instead of letting the display fall
  back to another one."
  [state model v]
  (let [state (adopt-selection state model v)
        [kind id] (:selected v)
        current (:selected-instance state)]
    (if (and (some? current) (not= current id) (not (running? model current))
             (= :instance kind) (running? model id))
      (point-at state model id)
      state)))

(def ^:private retry-refreshes
  "Refreshes to wait for a published selection to commit before sending it
  again (about 200 ms at the 10 ms cadence)."
  20)

(def ^:private max-retries 3)

(defn- track-selection
  "After the profile's feedback, against the committed model it saw: a
  running target the profile picked itself (a launch pad and its ack, a cue
  gesture, the display's fallback to the latest launch) becomes a pending
  publish of the shared selection. It is put in the outbox once the surface
  is attached, so a target picked before attach commits is published too.
  If it has not committed after retry-refreshes refreshes (a refusal), it
  is sent again, at most max-retries times. Feedback never dispatches: the
  host sends the outbox outside the session lock (publish!)."
  [state model v envelope]
  (let [grid (::grid state) current (:selected-instance state) committed (:selected v)
        state (if (= current (:edited grid))
                state
                (update state ::grid assoc :edited current :emitted? false :waited 0 :retries 0 :outbox nil
                        :publishing (when (and (some? current) (running? model current)
                                               (not= [:instance current] committed))
                                      [:instance current])))
        {:keys [publishing emitted? waited retries]} (::grid state)
        emit (fn [state] (update state ::grid assoc :emitted? true :waited 0
                                 :outbox {:id (:command-id envelope) :selected publishing :over committed}))]
    (cond
      (nil? publishing) state
      (= publishing committed) (update state ::grid assoc :publishing nil :outbox nil)
      (not (running? model (second publishing))) (update state ::grid assoc :publishing nil :outbox nil)
      (not (:attached? v)) state
      (not emitted?) (emit state)
      (and (>= (inc (or waited 0)) retry-refreshes) (< (or retries 0) max-retries))
      (-> state emit (update-in [::grid :retries] (fnil inc 0)))
      :else (update-in state [::grid :waited] (fnil inc 0)))))

(defn- grid-input [{:keys [surface-id buttons] :as options} underlying state event model envelope]
  (let [{:keys [type control-id pressed?] :as decoded} event
        v (view options model)
        state (adopt-selection state model v)
        grid (::grid state)
        normalized (when (#{:pad :poly-pressure} type)
                     (some-> (descriptor/push-3-event surface-id decoded) (update :at viewport-position)))
        identity (:identity normalized)
        apply-grid (fn [{g :state commands :commands selection :selection}]
                     (let [[kind id] selection]
                       ;; Select re-applies even the committed launch: the encoders
                       ;; return to it after any local drift.
                       {:state (cond-> (assoc state ::grid g)
                                 (and (= :instance kind) (running? model id))
                                 (-> (point-at model id) (assoc-in [::grid :adopted] selection)))
                        :commands (fence model commands)}))]
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

      :else (or (lift state underlying event model envelope) {:state state}))))

(defn- grid-feedback [{:keys [layout buttons leds] :as options} underlying state model envelope]
  (let [v (view options model)
        state (follow-committed state model v)
        base (if underlying (or (lift state underlying model envelope) {:state state}) {:state state})
        state (track-selection (:state base state) model v envelope)
        grid (::grid state)
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
        (assoc :state (assoc state ::grid projected ::sent (merge sent desired)))
        (update :midi (fnil into []) messages))))

(defn- grid-stop [{:keys [leds]} underlying state model envelope disconnected?]
  (let [result (or (lift state underlying model envelope disconnected?) {:state state})
        lit (keep (fn [[k message]]
                    (when-not (or (= 0x80 (first message)) (= (:button-off leds) (peek message)))
                      (case (first k)
                        :pad [0x80 (second message) 0]
                        :button [0xb0 (second message) (:button-off leds)])))
                  (sort-by (comp pr-str key) (::sent state)))]
    (cond-> (assoc result :state (assoc (:state result) ::grid cue-grid/initial-state ::sent {}))
      (not disconnected?) (update :midi (fnil into []) lit))))

(defn- grid-stale-input [underlying state {:keys [type pressed? gesture-key] :as event} envelope]
  (let [identity (when gesture-key (into [:pad] gesture-key))
        grid (::grid state)]
    (cond
      (and (= :pad type) (not pressed?) (cue-grid/owns? grid identity))
      (update state ::grid #(:state (cue-grid/release % identity)))
      (and (= :pad type) pressed? (cue-grid/open? grid))
      (update-in state [::grid :pads] conj identity)
      underlying (merge (underlying (dissoc state ::grid ::sent) event envelope) (select-keys state [::grid ::sent]))
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
           :initial-state (assoc (or initial-state {}) ::grid cue-grid/initial-state ::sent {})
           :input (partial grid-input options input)
           :feedback (partial grid-feedback options feedback)
           :stop (partial grid-stop options (or stop (fn [state & _] {:state state})))
           :reset-model (fn [state]
                          (assoc (if reset-model (reset-model (dissoc state ::grid ::sent)) (dissoc state ::grid ::sent))
                                 ::grid (cue-grid/reset-model (::grid state cue-grid/initial-state))
                                 ::sent (::sent state {})))
           :stale-input (partial grid-stale-input stale-input)
           :ack (fn [state command receipt]
                  (if (contains? own-ops (:op command))
                    {:state state :commands []}
                    (lift state ack command receipt))))))

(defn profile
  "The Afterglow Push 3 profile for options, with the cue grid of
  (:cue-grid options) wrapped around it."
  [options]
  (let [{:keys [buttons leds]} (validate-options! options)]
    (with-cue-grid (push-3/profile options)
      {:surface-id (:surface-id options (:session-id options)) :content (content options)
       :layout (:layout options) :buttons buttons :leds (merge default-leds leds)})))

(defn grid-state
  "This session's cue grid state: :mode, :source, :shown and held pads."
  [device]
  (get-in (session/snapshot device) [:local ::grid]))

(defn publish-pending!
  "Send this session's selection outbox, if it holds a request not yet sent,
  through the host's :publish! (or :dispatch!). Runs outside the session
  lock, after a refresh or an input, so feedback never waits on the show
  owner. Returns the device."
  [device]
  (when-let [publish (::publish device)] (publish))
  device)

(defn refresh!
  "afterglow session/refresh!, then publish-pending!."
  [device]
  (session/refresh! device)
  (publish-pending! device))

(defn- committed-selection [model! surface-id]
  (try (get-in (link/window (model!) surface-id viewport) [:layout :selected])
       (catch Exception _ ::unavailable)))

(defn- publisher
  "A function that sends a session's newest outbox request once, even when
  the scheduler and the MIDI input thread both call it. A request is sent
  only while it is still the pending target and the committed selection is
  still the one it was queued over: a newer Studio, REPL or pad choice that
  landed in between wins, and the next refresh adopts it."
  [session-ref {:keys [publish! dispatch! model!] :as options}]
  (let [sent-id (atom nil)
        surface-id (:surface-id options (:session-id options))]
    (fn []
      (when (realized? session-ref)
        (let [{:keys [publishing] {:keys [id selected over]} :outbox}
              (get-in (session/snapshot @session-ref) [:local ::grid])
              seen @sent-id]
          (when (and id (not= id seen) (= publishing selected)
                     (= over (committed-selection model! surface-id))
                     (compare-and-set! sent-id seen id))
            ((or publish! dispatch!)
             {:op :control-surface/layout :target surface-id :viewport viewport :set {:selected selected}})))))))

(defn connect!
  "Open a Push 3 User Mode session with the cue grid: the options of
  afterglow.control.devices.push-3/connect! (and devices.repl/open-push-3!)
  plus :cue-grid and an optional host :publish! (command -> nil, must not
  wait on the owner) for selections the Push picks itself; :dispatch! is
  used without it. Without :schedule! it refreshes every 10 ms (gestures
  and the grid's feedback), and without :midi or :transport it opens the
  selected MIDI ports. A scheduled refresh and each MIDI input publish the
  outbox after they release the session lock; with :schedule! nil call
  refresh! here instead of afterglow session/refresh!."
  [options]
  (when-let [display (:display options)]
    (when-not (and (= :push-3 (:device display)) (= :view (:input display))
                   (every? #(fn? (get display %)) [:open! :render! :close!]))
      (throw (ex-info "Choose a tagged Push 3 display transport" {}))))
  (let [session-ref (promise)
        publish (publisher session-ref options)
        schedule (if (contains? options :schedule!) (:schedule! options) (transports/scheduler (get options :refresh-ms 10)))
        midi (cond (:midi options) (:midi options)
                   (:transport options) (:transport options)
                   :else (transports/midi-transport options))
        options (assoc options
                       :profile (profile options)
                       :schedule! (when schedule (fn [refresh] (schedule (fn [] (refresh) (publish)))))
                       :midi (update midi :open! (fn [open!] (fn [receive!] (open! (fn [input] (receive! input) (publish)))))))
        device (assoc (session/create options) ::publish publish)]
    (deliver session-ref device)
    (session/open! device)))
