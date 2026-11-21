(ns tolglow.integrations.ableton
  "Pure Live bridge protocol and MIDI mapping. Clip writes and pad color changes
  are bridge intents: a Live-side Max/Remote Script adapter must execute them.
  MIDI alone does not expose Live recording state or editable clips."
  (:require [afterglow.time :as musical-time]
            [tolglow.registry :as registry]))

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

(defn- scalar-schema [schema]
  (case (:type schema)
    (:number :integer) (when-let [[low high] (or (:bounds schema) (:recommended schema))]
                         (when (< low high) {:type (:type schema) :bounds [low high]}))
    :boolean {:type :boolean}
    :enum (when (<= 1 (count (:values schema)) 127) (select-keys schema [:type :values]))
    nil))

(defn map-page
  "Allocate stable note and CC assignments for a page. Existing assignments,
  including disabled entries and manual overrides, reserve their addresses.
  Parameter velocity lanes occupy pitches outside the 64-pad Drum Rack range."
  [live registry page]
  (let [cells (sort-by (fn [[[x y]]] [y x]) (:cells page))
        pitch (fn [[x y]] (+ 36 x (* 8 (- 7 y))))
        reserved-lanes (set (keep :lane-note (vals (:parameters live))))
        cue-map (reduce (fn [m [xy cue-id]]
                          (let [preferred (pitch xy)
                                note (first (remove #(or (contains? m %) (reserved-lanes %))
                                                    (cons preferred (range 36 100))))]
                            (if (or (not (every? #(<= 0 % 7) xy)) (nil? note)
                                    (some #(= cue-id (:cue-id %)) (vals m)))
                              m (assoc m note {:cue-id cue-id :mode :held}))))
                        (:mappings live {}) cells)
        targets (for [cue-id (distinct (map val cells))
                      :let [cue (get-in registry [:cues cue-id])]
                      :when (some #(and (= cue-id (:cue-id %)) (not (:disabled? %))) (vals cue-map))
                      [param schema] (sort-by (comp str key) (:params (registry/cue-effect registry cue)))]
                  [cue-id param schema])
        parameters (reduce
                    (fn [m [cue-id param schema]]
                      (let [existing (first (sort (for [[cc target] m
                                                        :when (= [cue-id param] ((juxt :cue-id :param) target))] cc)))
                            cc (or existing (first (remove #(contains? m %) (range 128))))
                            used (into (set (keys cue-map)) (keep :lane-note (vals m)))
                            mapping (get m cc)
                            lane (or (:lane-note mapping) (first (remove used (concat (range 100 128) (range 36)))))
                            scalar (scalar-schema schema)]
                        (if (or (nil? cc) (nil? lane) (:disabled? mapping) (nil? scalar)) m
                            (assoc m cc (merge scalar {:cue-id cue-id :param param :lane-note lane} mapping (select-keys scalar [:type :values]))))))
                    (:parameters live {}) targets)]
    (assoc live :mappings cue-map :parameters parameters)))

(defn- decode-value [{:keys [type bounds values]} fraction]
  (let [[low high] (or bounds [0 1])]
    (case type
      :boolean (>= fraction 0.5)
      :enum (nth values (Math/round (* (double fraction) (dec (count values)))))
      :integer (Math/round (double (+ low (* fraction (- high low)))))
      (+ low (* fraction (- high low))))))

(defn- encode-value [{:keys [type bounds values]} value]
  (case type
    :boolean (when (boolean? value) (if value 1.0 0.0))
    :enum (let [i (.indexOf ^java.util.List values value)]
            (when-not (neg? i) (/ i (double (max 1 (dec (count values)))))))
    (let [[low high] (or bounds [0 1])]
      (when (and (number? value) (Double/isFinite (double value)) (< low high))
        (clamp (/ (- value low) (- high low)) 0 1)))))

(defn unmapped-fields [live registry page]
  (let [cues (set (map :cue-id (vals (:mappings live))))
        targets (set (map (juxt :cue-id :param) (vals (:parameters live))))]
    (vec (mapcat (fn [id]
                   (if (contains? cues id)
                     (for [[param schema] (sort-by (comp str key) (:params (registry/cue-effect registry (get-in registry [:cues id]))))
                           :when (not (targets [id param]))]
                       {:cue-id id :param param :reason (if (scalar-schema schema) :no-free-lane :needs-custom-mapping)})
                     [{:cue-id id :reason :no-free-pad}]))
                 (distinct (map val (sort-by key (:cells page))))))))

(defn mapped-pads [registry mappings]
  (vec (for [[note {:keys [cue-id disabled?]}] (sort-by key mappings)
             :let [cue (get-in registry [:cues cue-id])
                   swatch (or (:swatch cue) (:swatch (registry/cue-effect registry cue)))]
             :when (and cue (not disabled?))]
         {:note note :cue-id cue-id :label (or (:name cue) (str cue-id))
          :live-color (if (and (string? swatch) (re-matches #"(?i)#[0-9a-f]{6}" swatch))
                        (Long/parseLong (subs swatch 1) 16)
                        (:live-color (first (pad-page [cue]))))})))

(defn new-bridge
  "mappings: note -> {:cue-id id :mode :held|:persistent|:timed|:one-shot
  :duration-ms n :velocity-param keyword}; parameters: cc -> {:instance-id id
  :cue-id id :param keyword :bounds [min max] :lane-note pitch}. MIDI channels are 1-based."
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
        target (case type
                 :cc (get-in state [:parameters controller])
                 :note-on (some #(when (= note (:lane-note %)) %) (vals (:parameters state)))
                 nil)
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
      (and target (not (:disabled? target)))
      (let [cmd (command state envelope :param/set
                         {:param (:param target)
                          :value (decode-value target (if (= :cc type) (/ value 127.0) (/ (dec velocity) 126.0)))})
            pending-note (first (for [[n h] (:held state)
                                      :when (and (not (:released? h)) (nil? (:instance-id h))
                                                 (= (:cue-id target) (get-in state [:mappings n :cue-id])))] n))]
        (cond
          pending-note {:state (assoc-in state [:held pending-note :updates (:param target)] cmd) :commands []}
          (:instance-id target) {:state state :commands [(assoc cmd :instance-id (:instance-id target))]}
          :else {:state (assoc-in state [:next-params (:cue-id target) (:param target)] (:value cmd)) :commands []}))
      (and (= type :note-on) mapping (not (:disabled? mapping)) (nil? held))
      (let [mode (get mapping :mode :persistent)
            launch (cond-> {:mode mode :params (get-in state [:next-params (:cue-id mapping)] {})}
                     (:duration mapping) (assoc :duration (:duration mapping))
                     (:duration-ms mapping) (assoc :duration-ms (:duration-ms mapping))
                     (:velocity-param mapping) (assoc-in [:params (:velocity-param mapping)] (/ velocity 127.0)))
            cmd (command state envelope :cue/launch {:cue-id (:cue-id mapping) :launch launch})]
        {:state (cond-> (update state :next-params dissoc (:cue-id mapping)) (= mode :held) (assoc-in [:held note] {:command-id (:command-id cmd)}))
         :commands [cmd]})
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
        (let [instance (:instance-id ack)
              updates (mapv #(assoc % :instance-id instance) (vals (:updates pending)))]
          (if (:released? pending)
            {:state (update state :held dissoc note)
             :commands (conj updates (command state envelope :instance/end {:instance-id instance}))}
            {:state (assoc-in state [:held note] (assoc (dissoc pending :updates) :instance-id instance))
             :commands updates}))
        {:state (update state :held dissoc note) :commands []}))))

(defn handoff
  "Transfer surface ownership, ending held lighting instances. Pending launches
  retain a released marker until their ack arrives. Playback itself continues."
  [state mode envelope]
  (when-not (#{:lighting :live} mode) (fail! "Unknown surface mode" {:mode mode}))
  (if (= mode (:mode state))
    {:state state :commands [] :events []}
    (let [held (sort-by key (:held state))]
      {:state (assoc state :mode mode :next-params {} :held
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

(defn- record-one
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
        note (first (for [[note mapping] (sort-by key (:mappings state)) :when (and (not (:disabled? mapping)) (= cue-id (:cue-id mapping)))] note))
        cc (first (for [[cc mapping] (sort-by key (:parameters state))
                       :when (and (not (:disabled? mapping)) (= (:param cmd) (:param mapping))
                                  (or (and (:instance-id mapping) (= (:instance-id cmd) (:instance-id mapping)))
                                      (= cue-id (:cue-id mapping))))] [cc mapping]))
        recording-key [(:show-id cmd) (:epoch cmd) (:command-id cmd)]
        suppressed? (or (not= :accepted (:status ack)) (not= (:command-id cmd) (:command-id ack))
                        (= :live-playback (get-in cmd [:source :origin]))
                        (= :tolglow-feedback (get-in cmd [:source :origin]))
                        (contains? (:recorded state) recording-key))
        armed? (and (every? true? ((juxt :track-armed? :recording? :writable?) recording))
                    (:destination-id recording))
        fraction (when (and cc (= :param/set (:op cmd))) (encode-value (second cc) (:value cmd)))
        message (case (:op cmd)
                  :cue/launch (when note {:type :note-on :note note :velocity 127})
                  (:instance/end :instance/remove) (when note {:type :note-off :note note :velocity 0})
                  :param/set (when (and (some? fraction) (or (:lane-note (second cc)) (true? (:automation-armed? recording))))
                               {:type :cc :controller (first cc) :value (Math/round (* 127.0 (double fraction)))})
                  nil)]
    (if (and (not suppressed?) armed? message)
      (let [message (assoc message :channel (:channel state) :origin :tolglow-feedback
                           :command-id (:command-id cmd))]
        {:state (remember state recording-key) :midi (if (and (= :cc (:type message)) (not (:automation-armed? recording))) [] [message])
         :intents [{:op :live/record-event :protocol-version 1
                    :destination-id (:destination-id recording) :beat (:beat recording)
                    :time-ms (:time-ms cmd) :epoch (:epoch cmd) :command-id (:command-id cmd)
                    :cue-id cue-id :event (if-let [lane (and (= :cc (:type message)) (:lane-note (second cc)))]
                                            (-> message (dissoc :controller :value)
                                                (assoc :type :note-on :note lane :parameter-sample? true
                                                       :velocity (inc (Math/round (* 126.0 (double fraction))))))
                                            message)}]})
      {:state state :midi [] :intents []})))

(defn record-command
  "Project an accepted performance gesture into the selected recording routes.
  Launch overrides get their own editable parameter notes at the launch beat."
  [state cmd ack recording]
  (let [plan (record-one state cmd ack recording)]
    (if (or (not= :cue/launch (:op cmd)) (empty? (:intents plan))) plan
        (let [duration (or (:recording-duration cmd) (get-in cmd [:launch :duration]))
              duration-beats (when duration
                               (if-let [ms (:ms duration)]
                                 (* (/ ms 60000.0) (get-in recording [:clock :bpm] 120))
                                 (musical-time/duration->beats duration (get-in recording [:clock :meter]))))
              plan (cond-> plan duration-beats (assoc-in [:intents 0 :duration-beats] duration-beats))]
          (reduce (fn [out [param value]]
                    (let [id (pr-str [(:command-id cmd) :launch-param param])
                          child (record-one (:state out)
                                            (assoc cmd :op :param/set :param param :value value
                                                   :command-id id :instance-id (:instance-id ack))
                                            (assoc ack :command-id id) recording)]
                      (-> out (update :midi into (:midi child)) (update :intents into (:intents child)))))
                  plan (sort-by (comp str key) (get-in cmd [:launch :params])))))))
