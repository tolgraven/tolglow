(ns tolglow.surface.x-touch
  "X-Touch and X-Touch-Ext in MCU mode as a mixer surface (#705, #706 slice 2).

  This replaces the Afterglow X-Touch profile on Tolglow's connect path. The
  mixer behaviour lives in tolglow.surface.mixer; this namespace maps MCU
  addresses to it and its desired state back to MCU bytes. It composes the
  Afterglow session shell (lifecycle, generations, acknowledgements), the
  Afterglow MCU codec and the shared hold bindings, and keeps per-member
  state local: touches, held cues, modifiers, the stored transport-SOLO set
  and what was last sent. Strips, the window and the layout state are read
  from the committed model through the surface link, so extend, mirror and
  follow links need nothing device-specific.

  Buttons (main unit; the extender has strip controls only):
    SELECT      select the strip; again: toggle a cue's launch, or clear a group
    SOLO/MUTE   :effect-selection/toggle on the strip's instances; SHIFT or
                CONTROL keeps the others (additive)
    REC         hold a cue while pressed (exact instance release)
    PAN/SEND/PLUG-IN/USER   encoder mode; the lit mode again returns to PAN
    EQ/INST     previous/next encoder page in PLUG-IN and USER
    FLIP        swap fader and encoder on cue strips in PAN
    BANK/CHANNEL  move the committed window; SHIFT: first/last page
    transport SOLO  clear and store all solos, restore, or solo the selection
    PLAY/STOP   local transport only; ignored while an external source
                (Link with start/stop sync) owns play and stop
  Faders need a finger: a touch captures the strip's target until release,
  across bank and mode changes. Untouched fader messages (motor echoes) are
  ignored, and a touched fader gets no motor feedback."
  (:require [afterglow.control.bindings :as bindings]
            [afterglow.control.codecs.x-touch :as codec]
            [afterglow.control.devices.session :as session]
            [afterglow.control.devices.transports :as transports]
            [tolglow.surface.descriptor :as descriptor]
            [tolglow.surface.mixer :as mixer]))

(def strip-count 8)

(def ^:private master-strip 8)

(def ^:private assign-leds
  {:pan 42 :send 41 :plug-in 43 :user 69})

(def ^:private led-notes
  "Main-unit button LEDs the layout drives."
  {:track 40 :send 41 :pan 42 :plug-in 43 :eq 44 :inst 45 :user 69 :flip 50
   :global-solo 90 :stop 93 :play 94})

(defn- initial []
  {:touches {} :holds {} :hold-keys {} :sent {} :meters {} :modifiers #{} :stored-solo [] :shown []})

;; ---------------------------------------------------------------------------
;; MCU output the Afterglow codec does not cover

(def ^:private ring-modes {:single 0 :boost-cut 1 :wrap 2 :spread 3})

(defn ring-bytes
  "Encoder ring CC: bit 6 centre LED, bits 4-5 mode, bits 0-3 position 0..11."
  [strip {:keys [mode position center?]}]
  [0xb0 (+ 0x30 strip) (if position
                          (+ (if center? 0x40 0) (* 16 (ring-modes mode)) (min 11 (max 0 position)))
                          0)])

(defn meter-bytes
  "MCU channel meter (channel pressure): strip in the high nibble, level 0..12."
  [strip level]
  [0xd0 (+ (* 16 strip) (min 12 (max 0 level)))])

(defn meter-level [n] (long (Math/round (* 12.0 (min 1.0 (max 0.0 (double n)))))))

;; ---------------------------------------------------------------------------
;; Resolving what each strip shows

(defn- modifier? [state k] (contains? (:modifiers state) k))

(defn- view
  "Everything one refresh or input needs: the context, the committed layout,
  this unit's strips and, in item modes, the encoder items it shows."
  [options model]
  (let [ctx (mixer/context model)
        rows (vec ((:rows options) model))
        {:keys [first index span layout]} ((:placement options) model)
        resolved (mapv #(mixer/strip ctx layout (get rows (+ first %))) (range strip-count))
        items (when (contains? mixer/item-modes (:encoder-mode layout))
                (mixer/items ctx layout (some #(when (= (:selected layout) (mixer/row-key %)) %) rows)))
        item-at (fn [x] (get items (+ index x (* span (:page layout 0)))))]
    {:ctx ctx :layout layout :span span :strips resolved :items items :item-at item-at}))

(defn- encoder-target [{:keys [items item-at strips]} x]
  (if items (:target (item-at x)) (:encoder (get strips x))))

(defn- fenced [envelope command]
  (merge command (select-keys envelope [:show-id :epoch])))

(defn- changed [state & commands] {:state state :commands (vec (remove nil? commands))})

(defn- with-envelope [envelope {:keys [state commands]}]
  {:state state :commands (mapv #(fenced envelope %) commands)})

;; ---------------------------------------------------------------------------
;; Input

(defn- strip-button [options state v envelope x row pressed?]
  (let [s (get (:strips v) x)
        layout (:layout v)
        additive? (or (modifier? state :shift) (modifier? state :control))]
    (case row
      0 (if pressed?
          (if (or (get-in state [:hold-keys x]) (not= :cue (:kind s)))
            (changed state)
            (let [result (bindings/begin-hold (:holds state) (:cue-id s) envelope)]
              {:state (-> state (assoc :holds (:state result)) (assoc-in [:hold-keys x] (:gesture-id result)))
               :commands (:commands result)}))
          (if-let [gesture (get-in state [:hold-keys x])]
            (let [result (bindings/end-hold (:holds state) gesture envelope)]
              {:state (-> state (assoc :holds (:state result)) (update :hold-keys dissoc x)) :commands (:commands result)})
            (changed state)))
      1 (if pressed? (with-envelope envelope (changed state (mixer/selection-toggle :solo s additive?))) (changed state))
      2 (if pressed? (with-envelope envelope (changed state (mixer/selection-toggle :mute s additive?))) (changed state))
      3 (if pressed?
          (with-envelope envelope (apply changed state (mixer/select (:surface-id options) layout s)))
          (changed state)))))

(def ^:private bank-moves {:bank-left :previous :bank-right :next :channel-left :channel-previous :channel-right :channel-next})

(def ^:private shift-bank-moves {:bank-left :first :bank-right :last :channel-left :first :channel-right :last})

(defn- named-button [options state v envelope button pressed? model]
  (let [{:keys [layout ctx items span]} v
        surface-id (:surface-id options)
        layout! #(with-envelope envelope (changed state (mixer/layout-command surface-id %)))]
    (cond
      (contains? #{:shift :option :control :alt} button)
      (changed (update state :modifiers (if pressed? conj disj) button))

      (not pressed?) (changed state)

      (bank-moves button)
      (with-envelope envelope
        (changed state {:op :control-surface/bank :target surface-id :viewport :strips
                        :move ((if (modifier? state :shift) shift-bank-moves bank-moves) button)}))

      (contains? assign-leds button)
      (layout! (if (= button (:encoder-mode layout)) {:encoder-mode :pan :page 0} {:encoder-mode button :page 0}))

      (contains? #{:eq :inst} button)
      (if items
        (let [last-page (max 0 (dec (long (Math/ceil (/ (count items) (double (max 1 span)))))))
              page (min last-page (max 0 ((if (= :eq button) dec inc) (:page layout 0))))]
          (if (= page (:page layout 0)) (changed state) (layout! {:page page})))
        (changed state))

      (= :flip button) (layout! {:flip? (not (:flip? layout))})

      (contains? #{:play :stop} button)
      (if ((:external-transport? options) model)
        (changed state)
        (with-envelope envelope (changed state {:op :transport/set :transport {:playing? (= :play button)}})))

      (= :global-solo button)
      (if (modifier? state :shift)
        (changed (assoc state :stored-solo []))
        (let [selected (some #(when (= (:selected layout) (:key %)) %) (:strips v))
              {:keys [commands stored]} (mixer/global-solo ctx (:stored-solo state) selected)]
          (with-envelope envelope (apply changed (assoc state :stored-solo stored) commands))))

      :else (changed state))))

(defn input
  "Pure MCU input -> local state and owner command templates."
  [options state raw model envelope]
  (let [{:keys [group at type value delta] :as event} (descriptor/x-touch-event (:surface-id options) raw)
        [x y] at
        extender? (:extender? options)]
    (if (nil? event)
      (changed state)
      (case group
        :faders
        (case type
          :touch (if (contains? (:touches state) x)
                   (changed state)
                   (let [v (view options model) s (get (:strips v) x)]
                     ;; A strip that changed since the last refresh is not captured.
                     (changed (assoc-in state [:touches x]
                                        (when (and s (= (:key s) (get (:shown state) x))) (:fader s))))))
          :release-touch (changed (-> state (update :touches dissoc x) (update :sent dissoc [:fader x])))
          :move (if-let [captured (get-in state [:touches x])]
                  (if-let [target (mixer/hydrate model captured)]
                    (with-envelope envelope (changed state (mixer/write target (mixer/value-at target value))))
                    (changed state))
                  (changed state))
          (changed state))

        :master-fader
        (if extender?
          (changed state)
          (case type
            :touch (changed (assoc-in state [:touches master-strip] (mixer/master-target model)))
            :release-touch (changed (-> state (update :touches dissoc master-strip) (update :sent dissoc [:fader master-strip])))
            :move (if (contains? (:touches state) master-strip)
                    (let [target (mixer/master-target model)]
                      (with-envelope envelope (changed state (mixer/write target (mixer/value-at target value)))))
                    (changed state))
            (changed state)))

        :encoders
        (let [v (view options model) target (encoder-target v x)]
          (case type
            :turn (with-envelope envelope (changed state (mixer/turn target delta (modifier? state :shift))))
            :press (with-envelope envelope (changed state (some-> target mixer/reset)))
            (changed state)))

        :strip-buttons
        (strip-button options state (view options model) envelope x y (= :press type))

        :buttons
        (if extender?
          (changed state)
          (named-button options state (view options model) envelope x (= :press type) model))

        (changed state)))))

;; ---------------------------------------------------------------------------
;; Feedback

(defn- led [state] (if (true? state) :on (or state :off)))

(defn- project-view [options state {:keys [strips items item-at]}]
  (let [match #(mixer/palette-index % (:color-match options :hue) (:color-cutoff options 0.19))]
    (mapv (fn [x]
            (let [s (get strips x)
                  item (when items (item-at x))
                  encoder (if items (:target item) (:encoder s))
                  fader (:fader s)]
              {:key (:key s)
               :label (if items (:label item "") (:label s ""))
               :value (if items
                        (mixer/value-text (:target item))
                        (mixer/value-text (or encoder fader)))
               :color (cond items (if item 6 0) s (match (:color s)) :else 0)
               :fader (when s (or (some-> fader mixer/normalized) 0.0))
               :ring (mixer/ring encoder)
               :rec (if (contains? (:hold-keys state) x) :on :off)
               :solo (if (contains? #{:all :some} (:solo s)) :on :off)
               :mute (cond (contains? #{:all :some} (:mute s)) :on (:suppressed? s) :blink :else :off)
               :select (if (:selected? s) :on :off)
               :meter (meter-level (:meter s 0.0))}))
          (range strip-count))))

(defn project
  "Pure per-strip display: label, value text, palette colour, fader, ring and
  LED states, and meter level, for options as given to profile (merged with
  its defaults)."
  [options state model]
  (project-view options state (view options model)))

(defn- main-leds [options state model layout ctx]
  (let [playing? (boolean (get-in model [:transport :playing?]))
        soloed? (seq (mixer/soloed-instance-ids ctx))]
    {:pan (= :pan (:encoder-mode layout)) :send (= :send (:encoder-mode layout))
     :plug-in (= :plug-in (:encoder-mode layout)) :user (= :user (:encoder-mode layout))
     :track false :eq false :inst false
     :flip (boolean (:flip? layout))
     :global-solo (cond soloed? :on (seq (:stored-solo state)) :blink :else :off)
     :play playing? :stop (not playing?)}))

(defn feedback
  "Desired MCU state diffed against what was sent. Meters are resent while
  above zero every :meter-refresh-ms, since the unit lets them fall."
  [options state model envelope]
  (let [extender? (:extender? options)
        {:keys [layout ctx] :as v} (view options model)
        projected (project-view options state v)
        desired (into {[:text 0] (codec/text extender? 0 0 (apply str (map #(mixer/label-text (:label %)) projected)))
                       [:text 1] (codec/text extender? 1 0 (apply str (map #(mixer/label-text (:value %)) projected)))
                       :colors (codec/colors extender? (mapv :color projected))}
                      (mapcat (fn [[x {:keys [fader ring rec solo mute select]}]]
                                (concat
                                 (when (and (some? fader) (not (contains? (:touches state) x)))
                                   [[[:fader x] (codec/fader x fader)]])
                                 [[[:ring x] (ring-bytes x ring)]
                                  [[:led x] (codec/button-led x rec)]
                                  [[:led (+ 8 x)] (codec/button-led (+ 8 x) solo)]
                                  [[:led (+ 16 x)] (codec/button-led (+ 16 x) mute)]
                                  [[:led (+ 24 x)] (codec/button-led (+ 24 x) select)]]))
                              (map-indexed vector projected)))
        desired (if extender?
                  desired
                  (cond-> (into desired (map (fn [[k v]] (let [note (led-notes k)] [[:led note] (codec/button-led note (led v))])))
                                (main-leds options state model layout ctx))
                    (not (contains? (:touches state) master-strip))
                    (assoc [:fader master-strip] (codec/fader master-strip (min 1.0 (max 0.0 (double (:master model 1.0))))))))
        messages (mapv val (filter (fn [[k message]] (not= message (get-in state [:sent k])))
                                   (sort-by (comp pr-str key) desired)))
        now (:time-ms envelope 0)
        refresh (:meter-refresh-ms options 200)
        meters (keep (fn [[x {:keys [meter]}]]
                       (let [[level at] (get-in state [:meters x])]
                         (when (or (not= level meter) (and (pos? meter) (>= (- now (or at 0)) refresh)))
                           [x meter])))
                     (map-indexed vector projected))]
    {:state (-> state
                (update :sent merge desired)
                (update :meters into (map (fn [[x level]] [x [level now]])) meters)
                (assoc :shown (mapv :key projected)))
     :commands []
     :midi (into messages (map (fn [[x level]] (meter-bytes x level))) meters)}))

(defn stop
  "Release held cues and, unless the cable is gone, blank the unit: LCD,
  colours, rings, meters and every LED the layout lit. Faders stay put."
  [options state _model _envelope disconnected?]
  (let [extender? (:extender? options)
        lit (keep (fn [[k message]] (when (and (vector? k) (= :led (first k)) (not= 0 (peek message))) (second k)))
                  (sort-by (comp pr-str key) (:sent state)))]
    {:state (initial)
     :commands (mapv #(hash-map :op :instance/end :instance-id %)
                     (keep :instance-id (vals (get-in state [:holds :gestures]))))
     :midi (if disconnected?
             []
             (-> [(codec/colors extender? (vec (repeat strip-count 0)))
                  (codec/text extender? 0 0 (apply str (repeat 56 " ")))
                  (codec/text extender? 1 0 (apply str (repeat 56 " ")))]
                 (into (map #(ring-bytes % nil)) (range strip-count))
                 (into (map #(meter-bytes % 0)) (range strip-count))
                 (into (map #(codec/button-led % :off)) lit)))}))

(defn validate-channels!
  "Check configured cue channels before any port opens:
  [{:cue-id :label? :color [r g b]? :fader binding? :encoder binding?}], where
  a binding is a parameter keyword or {:param k :component n}."
  [channels]
  (let [binding? #(or (nil? %) (keyword? %)
                      (and (map? %) (keyword? (:param %))
                           (or (nil? (:component %)) (nat-int? (:component %)))))]
    (when-not (and (vector? channels)
                   (every? (fn [{:keys [cue-id color fader encoder] :as channel}]
                             (and (map? channel)
                                  (or (keyword? cue-id) (and (string? cue-id) (seq cue-id)))
                                  (or (nil? color) (and (vector? color) (= 3 (count color))
                                                        (every? #(and (number? %) (<= 0 % 1)) color)))
                                  (binding? fader) (binding? encoder)))
                           channels))
      (throw (ex-info "X-Touch channels need a stable :cue-id, an optional [r g b] :color and parameter bindings"
                      {:x-touch/error :invalid-profile})))
    channels))

(defn profile
  "Afterglow session profile for one unit. Options:
    :surface-id            the attached surface (the session ID)
    :extender?             X-Touch-Ext (device ID 15; strip controls only)
    :rows                  model -> the logical surface's strip rows
                           (tolglow.surface.link/strip-channels)
    :placement             model -> tolglow.surface.link/strip-placement
    :external-transport?   model -> true while an external source owns play
                           and stop (default never)
    :color-match           :hue (default), :rgb or :off; :color-cutoff 0.19
    :meter-refresh-ms      200"
  [options]
  (let [options (merge {:extender? false :external-transport? (constantly false) :color-match :hue
                        :color-cutoff 0.19 :meter-refresh-ms 200} options)]
    (when-not (and (some? (:surface-id options)) (boolean? (:extender? options))
                   (every? ifn? ((juxt :rows :placement :external-transport?) options))
                   (contains? #{:hue :rgb :off} (:color-match options))
                   (number? (:color-cutoff options)) (<= 0 (:color-cutoff options) 1)
                   (pos-int? (:meter-refresh-ms options)))
      (throw (ex-info "Invalid X-Touch surface options" {:surface-id (:surface-id options)})))
    {:id :x-touch
     :capabilities {:protocol :mcu :layout :tolglow/mixer :extender? (:extender? options)}
     :unique-command-prefix? true
     :fence-captured-commands? true
     :initial-state (initial)
     :decode #(if (map? %) % (codec/decode %))
     :start (partial feedback options)
     :input (partial input options)
     :feedback (partial feedback options)
     :ack (fn [state command ack]
            (if-let [gesture (:gesture-id command)]
              (let [result (bindings/acknowledge-hold (:holds state) gesture ack)]
                {:state (assoc state :holds (:state result)) :commands (:commands result)})
              {:state state :commands []}))
     :stop (partial stop options)}))

(defn connect!
  "Open one unit: host :model!/:dispatch!/:now-ms!, :session-id, the profile
  options above, and selected ports or an injected :transport. Without
  :schedule! it refreshes every :refresh-ms (40). Close with
  afterglow.control.devices.session/close!."
  [options]
  (let [options (if (contains? options :schedule!) options
                    (assoc options :schedule! (transports/scheduler (get options :refresh-ms 40))))]
    (session/open!
     (session/create (assoc options :profile (profile options)
                            :midi (or (:transport options) (transports/midi-transport options)))))))
