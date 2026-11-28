(ns tolglow.surface.descriptor
  "Surface descriptors and normalized surface events (#706 slice 0).

  A descriptor is plain data: named control groups, each with a :kind, a
  :geometry [columns rows] and :capabilities, plus :viewports, the groups that
  move together through a link window (each with its geometry). Nothing is
  assumed beyond what a descriptor declares; a Push is not forced to be an
  X-Touch or the reverse.

  A normalized input event is
    {:surface id :group g :at [x y] :type t ...}
  with :pressed?, :value, :delta or :velocity as the type needs, and an
  :identity that stays member-local (a touch, a held pad, an MPE voice).
  Linking remaps :at afterwards (tolglow.surface.link/logical-position) and
  never merges two members' identities.

  Codecs stay in Afterglow for now. The normalizers here turn their decoded
  maps into normalized events; the address tables are data."
  (:require [afterglow.control.codecs.push-2 :as push-2]
            [afterglow.control.codecs.push-3 :as push-3]
            [afterglow.control.codecs.x-touch :as mcu]))

(def kinds #{:pads :buttons :encoders :faders :touch-strip :display :leds :rings :scribble})

(def event-types #{:press :release :touch :release-touch :move :turn :pressure})

(def ^:private mcu-strip-button-rows
  "MCU strip button notes by row: REC, SOLO, MUTE, SELECT (strip = note - base)."
  [[:rec 0] [:solo 8] [:mute 16] [:select 24]])

(def mcu-named-buttons
  "Named main-unit MCU buttons: the Afterglow codec's table plus the encoder
  assign, modifier and transport SOLO buttons the mixer layout uses, at their
  standard MCU note numbers (unverified on hardware until the #706 slice 2
  checklist runs)."
  (merge mcu/buttons
         {:track 40 :send 41 :pan 42 :plug-in 43 :eq 44 :inst 45 :user 69
          :shift 70 :option 71 :control 72 :alt 73 :global-solo 90}))

(defn- mcu-groups [extender?]
  (cond-> {:faders {:kind :faders :geometry [8 1] :capabilities {:motor true :touch true :resolution 14 :readback false}}
           :encoders {:kind :encoders :geometry [8 1] :capabilities {:relative true :touch false :push true}}
           :rings {:kind :rings :geometry [8 1] :capabilities {:positions 12 :modes #{:single :boost-cut :wrap :spread}}}
           :scribble {:kind :scribble :geometry [8 1] :capabilities {:text [2 7] :colors (count mcu/palette)}}
           :strip-buttons {:kind :buttons :geometry [8 4] :capabilities {:leds #{:off :on :blink}
                                                                            :rows (mapv first mcu-strip-button-rows)}}}
    (not extender?)
    (assoc :master-fader {:kind :faders :geometry [1 1] :capabilities {:motor true :touch true :resolution 14}}
           :buttons {:kind :buttons :geometry [(count mcu-named-buttons) 1]
                     :capabilities {:names (vec (sort (keys mcu-named-buttons))) :leds #{:off :on :blink}}})))

(def x-touch
  {:id :behringer/x-touch :name "X-Touch (MCU mode)" :protocol :mcu :device-id (mcu/device-id false)
   :groups (mcu-groups false)
   :viewports {:strips [8 1]}
   :viewport-axes {:strips :channels}
   :viewport-groups {:strips #{:faders :encoders :rings :scribble :strip-buttons}}
   :verification {:protocol :source-derived :hardware "#693 identity, firmware 1.22 and feedback rate"}})

(def x-touch-ext
  (assoc x-touch :id :behringer/x-touch-ext :name "X-Touch Extender (MCU mode)" :device-id (mcu/device-id true)
         :groups (mcu-groups true)))

(def push-2
  {:id :ableton/push-2 :name "Push 2 (source-derived)"
   :groups {:pads {:kind :pads :geometry [8 8] :capabilities {:velocity true :pressure :poly :origin :bottom-left
                                                              :leds {:palette 128 :components [:red :green :blue :white]}}}
            :encoders {:kind :encoders :geometry [8 1] :capabilities {:relative true :touch true}}
            :touch-strip {:kind :touch-strip :geometry [1 1] :capabilities {:leds true}}
            :display {:kind :display :geometry [1 1] :capabilities {:image [960 160]}}}
   :viewports {:grid [8 8] :encoders [8 1]}
   :viewport-axes {:grid :cue-grid :encoders :channels}
   :viewport-groups {:grid #{:pads} :encoders #{:encoders}}
   :verification {:protocol :source-derived :hardware :not-on-this-mac}})

(def studio-grid
  {:id :tolglow/studio-grid :name "Studio cue grid"
   :groups {:pads {:kind :pads :geometry [8 8] :capabilities {:velocity false :pressure :none}}}
   :viewports {:grid [8 8]}
   :viewport-axes {:grid :cue-grid}
   :viewport-groups {:grid #{:pads}}
   :verification {:protocol :studio-state-stream}})

(defn push-3
  "Push 3 User Mode descriptor from a captured layout (afterglow
  codecs.push-3/compile-layout input). Only captured controls appear."
  [layout]
  (let [{:keys [pads encoders encoder-touches buttons touch-strip mono-pressure]} (:layout (push-3/compile-layout layout))
        columns (if (seq pads) (inc (apply max (map first (keys pads)))) 0)
        rows (if (seq pads) (inc (apply max (map second (keys pads)))) 0)]
    {:id :ableton/push-3 :name "Push 3 (User Mode, captured layout)" :layout layout
     :groups (cond-> {}
               (seq pads) (assoc :pads {:kind :pads :geometry [columns rows]
                                        :capabilities {:velocity true :pressure (if mono-pressure :mono :poly)}})
               (seq encoders) (assoc :encoders {:kind :encoders :geometry [(count encoders) 1]
                                                :capabilities {:relative true :touch (boolean (seq encoder-touches))
                                                               :ids (vec (sort (keys encoders)))}})
               (seq buttons) (assoc :buttons {:kind :buttons :geometry [(count buttons) 1]
                                              :capabilities {:names (vec (sort (keys buttons)))}})
               touch-strip (assoc :touch-strip {:kind :touch-strip :geometry [1 1] :capabilities {}}))
     :viewports (cond-> {} (seq pads) (assoc :grid [columns rows]) (seq encoders) (assoc :encoders [(count encoders) 1]))
     :viewport-axes (cond-> {} (seq pads) (assoc :grid :cue-grid) (seq encoders) (assoc :encoders :channels))
     :viewport-groups (cond-> {} (seq pads) (assoc :grid #{:pads}) (seq encoders) (assoc :encoders #{:encoders}))
     :verification {:protocol :official-manual :addresses :caller-capture}}))

(def push-3-grid
  "The registered Push 3 surface: its physical 8x8 pad grid as one :grid
  viewport on the :cue-grid axis (#545). Pad addresses still come from a
  caller capture (push-3 describes one); pads a capture lacks stay dark.
  Pad [x y] IDs count from the bottom-left pad, as in Afterglow's Push cue
  grids; tolglow.surface.push-3 turns them into top-down viewport rows."
  {:id :ableton/push-3 :name "Push 3 (User Mode pad grid)"
   :groups {:pads {:kind :pads :geometry [8 8]
                   :capabilities {:velocity true :pressure :poly :origin :bottom-left
                                  :addresses :caller-capture :leds :unverified}}}
   :viewports {:grid [8 8]}
   :viewport-axes {:grid :cue-grid}
   :viewport-groups {:grid #{:pads}}
   :verification {:protocol :official-manual :addresses :caller-capture
                  :leds "Push 2 interface palette and animation channels; unverified on Push 3 (#545)"}})

(def push-2-grid
  "The registered Push 2 cue-grid surface: its 8x8 pad grid as one :grid
  viewport on the :cue-grid axis (#545, #706), the counterpart of
  push-3-grid. Notes and LEDs come from the source-derived Afterglow
  codec; tolglow.surface.push-2 maps them. push-2 describes the whole unit."
  {:id :ableton/push-2-grid :name "Push 2 (pad grid, source-derived)"
   :groups {:pads (get-in push-2 [:groups :pads])}
   :viewports {:grid [8 8]}
   :viewport-axes {:grid :cue-grid}
   :viewport-groups {:grid #{:pads}}
   :verification {:protocol :source-derived :hardware :not-on-this-mac}})

(defmulti descriptor
  "A registered descriptor by ID; nil when unknown. Add a method to register
  another device. :ableton/push-3 registers the physical pad grid
  (push-3-grid); push-3 describes a particular captured layout."
  identity)

(defmethod descriptor :default [_] nil)

(defmethod descriptor :behringer/x-touch [_] x-touch)

(defmethod descriptor :behringer/x-touch-ext [_] x-touch-ext)

(defmethod descriptor :ableton/push-2 [_] push-2)

(defmethod descriptor :ableton/push-2-grid [_] push-2-grid)

(defmethod descriptor :ableton/push-3 [_] push-3-grid)

(defmethod descriptor :tolglow/studio-grid [_] studio-grid)

(defn valid?
  "Every group has a known kind and positive geometry; every viewport has a
  content axis and lists existing groups of the viewport's geometry width."
  [{:keys [id groups viewports viewport-groups viewport-axes]}]
  (boolean
   (and (keyword? id) (map? groups) (seq groups)
        (every? (fn [[_ {:keys [kind geometry capabilities]}]]
                  (and (contains? kinds kind) (vector? geometry) (= 2 (count geometry))
                       (every? pos-int? geometry) (map? capabilities)))
                groups)
        (= (set (keys viewports)) (set (keys viewport-groups)) (set (keys viewport-axes)))
        (every? keyword? (vals viewport-axes))
        (every? (fn [[viewport members]]
                  (every? #(and (contains? groups %)
                                (= (first (get viewports viewport)) (first (get-in groups [% :geometry]))))
                          members))
                viewport-groups))))

(defn valid-event?
  "A normalized input event for this descriptor."
  [{:keys [groups]} {:keys [surface group at type]}]
  (boolean (and (some? surface) (contains? groups group) (contains? event-types type)
                (vector? at) (= 2 (count at))
                (let [[columns rows] (get-in groups [group :geometry]) [x y] at]
                  (if (keyword? x)
                    (contains? (set (concat (get-in groups [group :capabilities :names])
                                            (get-in groups [group :capabilities :ids]))) x)
                    (and (< -1 x columns) (< -1 y rows)))))))

;; Normalizers: decoded codec maps -> normalized events

(defn x-touch-event
  "Normalize an Afterglow MCU decode result. Unknown controls return nil.
  Touch identity is the strip; button rows follow mcu-strip-button-rows."
  [surface-id {:keys [type strip note pressed? value delta]}]
  (let [base {:surface surface-id}
        named (some (fn [[name n]] (when (= n note) name)) mcu-named-buttons)]
    (case type
      :fader (if (= 8 strip)
               (merge base {:group :master-fader :at [0 0] :type :move :value value})
               (merge base {:group :faders :at [strip 0] :type :move :value value}))
      :touch (merge base {:group (if (= 8 strip) :master-fader :faders) :at [(if (= 8 strip) 0 strip) 0]
                          :type (if pressed? :touch :release-touch) :identity [:touch strip]})
      :encoder (merge base {:group :encoders :at [strip 0] :type :turn :delta delta})
      :button (let [event-type (if pressed? :press :release)]
                (cond
                  (<= 0 note 31) (merge base {:group :strip-buttons :at [(mod note 8) (quot note 8)] :type event-type
                                              :identity [:button note]})
                  (<= 32 note 39) (merge base {:group :encoders :at [(- note 32) 0] :type event-type
                                               :identity [:encoder-push note]})
                  named (merge base {:group :buttons :at [named 0] :type event-type :identity [:button note]})
                  :else nil))
      nil)))

(defn x-touch-input-bytes
  "MCU bytes a unit sends for a normalized control address (for replays and
  round-trip tests). Covers faders, touch, encoders and buttons."
  [{:keys [group at type value delta]}]
  (let [[x y] at]
    (case [group type]
      [:faders :move] (vec (mcu/fader x value))
      [:master-fader :move] (vec (mcu/fader 8 value))
      [:faders :touch] [0x90 (+ 104 x) 127]
      [:faders :release-touch] [0x90 (+ 104 x) 0]
      [:master-fader :touch] [0x90 112 127]
      [:encoders :turn] [0xb0 (+ 16 x) (if (neg? delta) (+ 64 (- delta)) delta)]
      [:encoders :press] [0x90 (+ 32 x) 127]
      [:strip-buttons :press] [0x90 (+ (* 8 y) x) 127]
      [:strip-buttons :release] [0x80 (+ (* 8 y) x) 0]
      [:buttons :press] [0x90 (get mcu-named-buttons x) 127]
      [:buttons :release] [0x80 (get mcu-named-buttons x) 0])))

(defn push-3-event
  "Normalize an Afterglow Push 3 decode result. Pad identity keeps the MIDI
  channel and note, so mirrored members never share a gesture."
  [surface-id {:keys [type control-id pressed? value delta gesture-key]}]
  (let [base {:surface surface-id}]
    (case type
      :pad (merge base {:group :pads :at control-id :type (if pressed? :press :release) :velocity value
                        :identity (into [:pad] gesture-key)})
      :poly-pressure (merge base {:group :pads :at control-id :type :pressure :value value
                                  :identity (into [:pad] gesture-key)})
      :encoder (merge base {:group :encoders :at [control-id 0] :type :turn :delta delta})
      :encoder-touch (merge base {:group :encoders :at [control-id 0] :type (if pressed? :touch :release-touch)
                                  :identity [:encoder-touch control-id]})
      :button (merge base {:group :buttons :at [control-id 0] :type (if pressed? :press :release)})
      :touch-strip (merge base {:group :touch-strip :at [0 0] :type :move :value value})
      nil)))

(defn push-2-pad-event
  "Normalize a Push 2 pad note (bottom-left origin) from the source-derived codec."
  [surface-id [status note velocity]]
  (when-let [at (push-2/pad-coordinates note)]
    (let [channel (bit-and status 0x0f) pressed? (and (= 0x90 (bit-and status 0xf0)) (pos? velocity))]
      {:surface surface-id :group :pads :at at :type (if pressed? :press :release) :velocity velocity
       :identity [:pad channel note]})))
