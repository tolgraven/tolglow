(ns tolglow.integrations.ableton
  "Pure Live bridge protocol and MIDI mapping. Clip writes and pad color changes
  are bridge intents: a Live-side Max/Remote Script adapter must execute them.
  MIDI alone does not expose Live recording state or editable clips."
  (:require [clojure.string :as str]))

(defn- fail! [message data] (throw (ex-info message data)))
(defn- midi-number? [x] (and (integer? x) (<= 0 x 127)))
(defn- clamp [x low high] (max low (min high x)))

(defn pad-page
  "Create an 8x8 Drum Rack view. Cues are row-major top-to-bottom; MIDI notes
  rise left-to-right, bottom-to-top from base-note (default 36). Logical RGB is
  preserved and :live-color is packed 0xRRGGBB, for a Live-side API bridge."
  ([cues] (pad-page cues {}))
  ([cues {:keys [base-note channel] :or {base-note 36 channel 1}}]
   (when-not (and (integer? base-note) (<= 0 base-note 64) (<= 1 channel 16)
                  (<= (count cues) 64))
     (fail! "A Drum Rack page requires at most 64 pads in MIDI range" {}))
   (mapv (fn [index]
           (let [cue (nth (vec cues) index nil)
                 color (or (:color cue) [0 0 0])
                 _ (when-not (and (vector? color) (= 3 (count color))
                                   (every? #(and (number? %) (Double/isFinite (double %)) (<= 0 % 1)) color))
                     (fail! "Pad color must be normalized RGB" {:cue-id (:id cue)}))
                 bytes (mapv #(long (Math/round (* 255.0 %))) color)]
             {:x (mod index 8) :y (quot index 8)
              :note (+ base-note (* 8 (- 7 (quot index 8))) (mod index 8))
              :channel channel :cue-id (:id cue) :label (:name cue "")
              :color color :live-color (reduce (fn [result b] (+ (* result 256) b)) 0 bytes)}))
         (range 64))))

(defn new-bridge
  "mappings: note -> {:cue-id id :mode :held|:persistent|:timed|:one-shot
  :duration-ms n :velocity-param keyword}; parameters: cc -> {:instance-id id
  :cue-id id :param keyword :bounds [min max]}. MIDI channels are 1-based."
  ([] (new-bridge {}))
  ([{:keys [id mappings parameters channel mode history-limit]
     :or {id :live mappings {} parameters {} channel 1 mode :lighting history-limit 4096}}]
   (when-not (and (#{:lighting :live} mode) (integer? channel) (<= 1 channel 16)
                  (map? mappings) (map? parameters) (every? midi-number? (keys mappings))
                  (every? midi-number? (keys parameters)) (integer? history-limit) (pos? history-limit))
     (fail! "Invalid Live bridge configuration" {}))
   {:id id :mode mode :channel channel :mappings mappings :parameters parameters
    :held {} :instances {} :recorded #{} :record-order [] :history-limit history-limit}))

(defn- command [state envelope op data]
  (merge envelope {:op op :source {:type :ableton :bridge-id (:id state) :origin :live-playback}} data))
(defn- child-envelope [envelope suffix]
  (assoc envelope :command-id (str (:command-id envelope) "/" suffix)))

(defn midi->commands
  "Translate a MIDI event without applying commands. Supply the shared control
  envelope with command/show/epoch/time. Call acknowledge-launch after applying
  a held launch so note-off targets its exact activation ID. Echoed feedback
  (:origin :tolglow-feedback) is suppressed. Unmapped and other-channel input
  yields no commands. Live mode transfers ownership away from lighting."
  [state {:keys [type note velocity controller value channel origin] :as event} envelope]
  (when-not (and (integer? channel) (<= 1 channel 16)
                 (case type
                   (:note-on :note-off) (and (midi-number? note) (midi-number? (or velocity 0)))
                   :cc (and (midi-number? controller) (midi-number? value))
                   false))
    (fail! "Invalid MIDI event" {:event event}))
  (let [off? (or (= type :note-off) (and (= type :note-on) (zero? (or velocity 0))))
        mapping (get-in state [:mappings note])
        held (get-in state [:held note])]
    (cond
      (or (not= :lighting (:mode state)) (not= channel (:channel state))
          (= origin :tolglow-feedback)) {:state state :commands []}
      (and off? held)
      (if-let [instance-id (:instance-id held)]
        {:state (update state :held dissoc note)
         :commands [(command state envelope :instance/end {:instance-id instance-id})]}
        {:state (assoc-in state [:held note :released?] true) :commands []})
      off? {:state state :commands []}
      (and (= type :note-on) mapping (nil? held))
      (let [mode (get mapping :mode :persistent)
            launch (cond-> {:mode mode :params {}}
                     (:duration mapping) (assoc :duration (:duration mapping))
                     (:duration-ms mapping) (assoc :duration-ms (:duration-ms mapping))
                     (:velocity-param mapping) (assoc-in [:params (:velocity-param mapping)] (/ velocity 127.0)))
            cmd (command state envelope :cue/launch {:cue-id (:cue-id mapping) :launch launch})]
        {:state (cond-> state (= mode :held) (assoc-in [:held note] {:command-id (:command-id cmd)}))
         :commands [cmd]})
      (= type :cc)
      (if-let [{:keys [instance-id param bounds]} (get-in state [:parameters controller])]
        (let [[low high] (or bounds [0.0 1.0])]
          (when-not (and (number? low) (number? high) (Double/isFinite (double low))
                         (Double/isFinite (double high)) (< low high))
            (fail! "Parameter mapping requires finite increasing bounds" {:controller controller}))
          {:state state :commands [(command state envelope :param/set
                                             {:instance-id instance-id :param param
                                              :value (+ low (* (/ value 127.0) (- high low)))})]})
        {:state state :commands []})
      :else {:state state :commands []})))

(defn acknowledge-launch
  "Resolve a pending held launch with a control ack. A release or handoff before
  the ack generates an immediate end for that exact instance. Stale acks cannot
  steal a newer note binding. envelope identifies any generated end command."
  [state note ack envelope]
  (let [pending (get-in state [:held note])]
    (if (or (nil? pending) (not= (:command-id pending) (:command-id ack)))
      {:state state :commands []}
      (if (and (= :accepted (:status ack)) (:instance-id ack))
        (if (:released? pending)
          {:state (update state :held dissoc note)
           :commands [(command state envelope :instance/end {:instance-id (:instance-id ack)})]}
          {:state (assoc-in state [:held note :instance-id] (:instance-id ack)) :commands []})
        {:state (update state :held dissoc note) :commands []}))))

(defn handoff
  "Transfer surface ownership, ending held lighting instances. Pending launches
  retain a released marker until their ack arrives. Playback itself continues."
  [state mode envelope]
  (when-not (#{:lighting :live} mode) (fail! "Unknown surface mode" {:mode mode}))
  (if (= mode (:mode state))
    {:state state :commands [] :events []}
    (let [held (sort-by key (:held state))]
      {:state (assoc state :mode mode :held
                     (into {} (for [[note entry] held :when (nil? (:instance-id entry))]
                                [note (assoc entry :released? true)])))
       :commands (mapv (fn [[note entry]]
                         (command state (child-envelope envelope note) :instance/end
                                  {:instance-id (:instance-id entry)}))
                       (filter (comp :instance-id val) held))
       :events [{:type :surface/clear-feedback :previous-mode (:mode state)}
                {:type :surface/mode :mode mode :redraw? (= mode :lighting)}]})))

(defn- remember [state id]
  (let [order (conj (:record-order state) id)
        evicted (take (max 0 (- (count order) (:history-limit state))) order)]
    (-> state (assoc :record-order (vec (drop (count evicted) order)))
        (update :recorded #(apply disj (conj % id) evicted)))))

(defn record-command
  "Observe an accepted control command. Return MIDI + bridge clip intents only
  when track is armed, recording, writable and has :destination-id. Param writes
  additionally need :automation-armed?. Live playback and duplicate command IDs
  are never recorded. The adapter must acknowledge execution before UI claims
  that a clip was written. Musical :beat is supplied by the Live bridge."
  [state cmd ack recording]
  (let [state (if (and (= :accepted (:status ack)) (= (:command-id cmd) (:command-id ack))
                       (= :cue/launch (:op cmd)) (:instance-id ack))
                (assoc-in state [:instances (:instance-id ack)] (:cue-id cmd)) state)
        cue-id (or (:cue-id cmd) (get-in state [:instances (:instance-id cmd)]))
        note (first (for [[note mapping] (sort-by key (:mappings state)) :when (= cue-id (:cue-id mapping))] note))
        cc (first (for [[cc mapping] (sort-by key (:parameters state))
                       :when (and (= (:param cmd) (:param mapping))
                                  (or (= (:instance-id cmd) (:instance-id mapping))
                                      (= cue-id (:cue-id mapping))))] [cc mapping]))
        recording-key [(:show-id cmd) (:epoch cmd) (:command-id cmd)]
        suppressed? (or (not= :accepted (:status ack)) (not= (:command-id cmd) (:command-id ack))
                        (= :live-playback (get-in cmd [:source :origin]))
                        (= :tolglow-feedback (get-in cmd [:source :origin]))
                        (contains? (:recorded state) recording-key))
        armed? (and (every? true? ((juxt :track-armed? :recording? :writable?) recording))
                    (:destination-id recording))
        message (case (:op cmd)
                  :cue/launch (when note {:type :note-on :note note :velocity 127})
                  :instance/end (when note {:type :note-off :note note :velocity 0})
                  :param/set (when (and cc (true? (:automation-armed? recording)))
                               (let [[controller mapping] cc [low high] (get mapping :bounds [0 1])]
                                 (when-not (and (number? low) (number? high) (< low high)
                                                (number? (:value cmd)) (Double/isFinite (double (:value cmd))))
                                   (fail! "Invalid automation parameter scale" {:controller controller}))
                                 {:type :cc :controller controller
                                  :value (long (Math/round (* 127.0 (clamp (/ (- (:value cmd) low) (- high low)) 0 1))))}))
                  nil)]
    (if (and (not suppressed?) armed? message)
      (let [message (assoc message :channel (:channel state) :origin :tolglow-feedback
                           :command-id (:command-id cmd))]
        {:state (remember state recording-key) :midi [message]
         :intents [{:op :live/record-event :protocol-version 1
                    :destination-id (:destination-id recording) :beat (:beat recording)
                    :time-ms (:time-ms cmd) :epoch (:epoch cmd) :command-id (:command-id cmd)
                    :cue-id cue-id :event message}]})
      {:state state :midi [] :intents []})))
