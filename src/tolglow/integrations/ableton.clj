(ns tolglow.integrations.ableton
  "Pure Live bridge protocol and MIDI mapping. Clip writes and pad color changes
  are bridge intents: a Live-side Max/Remote Script adapter must execute them.
  MIDI alone does not expose Live recording state or editable clips."
  (:require [afterglow.time :as musical-time]
            [thi.ng.color.core :as color]
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
                 source (:color cue)
                 color (cond (satisfies? color/IRGBConvert source) ((juxt :r :g :b) (color/as-rgba source))
                             (nil? source) [0 0 0]
                             :else source)
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

(defn bind-registry-ids
  "Resolve string or keyword wire identities to the registry's original data.
  Preserve the saved velocity order of enum values across registry revisions."
  [live registry]
  (letfn [(resolve-id [entries id]
            (let [other (cond (keyword? id) (subs (str id) 1) (string? id) (keyword id))]
              (if (or (contains? entries id) (not (contains? entries other))) id other)))
          (bind-target [target]
            (let [cue-id (resolve-id (:cues registry) (:cue-id target))
                  target (assoc target :cue-id cue-id)]
              (if-let [param (:param target)]
                (let [schemas (:params (registry/cue-effect registry (get-in registry [:cues cue-id])))
                      param (resolve-id schemas param)
                      choices (set (get-in schemas [param :values]))]
                  (cond-> (assoc target :param param)
                    (:values target) (update :values #(mapv (partial resolve-id choices) %))))
                target)))
          (bind-table [table] (into {} (map (fn [[control target]] [control (bind-target target)]) table)))]
    (-> live (update :mappings bind-table) (update :parameters bind-table))))

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
                            (assoc m cc (merge scalar {:cue-id cue-id :param param :lane-note lane} mapping)))))
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

(defn- index-mappings [mappings parameters]
  {:mappings mappings :parameters parameters
   :lane-controllers (into {} (keep (fn [[cc mapping]] (when-let [note (:lane-note mapping)] [note cc]))) parameters)
   :cue-notes (into {} (for [[note mapping] (reverse (sort-by key mappings)) :when (not (:disabled? mapping))]
                         [(:cue-id mapping) note]))
   :parameter-controls (group-by (comp :param val) (remove (comp :disabled? val) (sort-by key parameters)))})

(defn- indexes [state]
  (let [cached (::indexes (meta state))]
    (if (and (identical? (:mappings state) (:mappings cached))
             (identical? (:parameters state) (:parameters cached))) cached
        (index-mappings (:mappings state) (:parameters state)))))

(defn new-bridge
  "mappings: note -> {:cue-id id :mode :held|:persistent|:timed|:one-shot
  :duration-ms n :velocity-param keyword :grid optional}. With :launch-grid, cue
  launches and releases use the show owner's timing lifecycle. MIDI channels
  are 1-based. Parameters map CC to a cue/parameter and its value encoding."
  ([] (new-bridge {}))
  ([{:keys [id mappings parameters channel mode history-limit launch-grid]
     :or {id :live mappings {} parameters {} channel 1 mode :lighting history-limit 4096}}]
   (when-not (and (#{:lighting :live} mode) (integer? channel) (<= 1 channel 16)
                  (map? mappings) (map? parameters) (every? midi-number? (keys mappings))
                  (every? midi-number? (keys parameters)) (integer? history-limit) (pos? history-limit))
     (fail! "Invalid Live bridge configuration" {}))
   (with-meta
     (cond-> {:id id :mode mode :channel channel :mappings mappings :parameters parameters
              :held {} :instances {} :instance-order [] :instance-slot 0
              :recorded #{} :record-order [] :history-limit history-limit}
       (some? launch-grid) (assoc :launch-grid launch-grid))
     {::indexes (index-mappings mappings parameters)})))

(defn- command [state envelope op data]
  (merge envelope {:op op :source (merge {:type :ableton :bridge-id (:id state) :origin :live-playback}
                                         (:source envelope))} data))
(defn- child-envelope [envelope suffix]
  (assoc envelope :command-id (str (:command-id envelope) "/" suffix)))

(defn- cancel-launch [state envelope entry reason]
  (command state envelope :cue/cancel-launch
           {:launch-command-id (:command-id entry) :source (:source entry) :reason reason}))

(defn- release-hold [state envelope entry]
  (cond
    (:owner-launch? entry) (cancel-launch state envelope entry :held-release)
    (:instance-id entry) (command state envelope :instance/end
                                  {:instance-id (:instance-id entry) :source (:source entry)})))

(defn parameter-controller [state {:keys [type controller note]}]
  (case type :cc controller :note-on (get-in (indexes state) [:lane-controllers note]) nil))

(defn midi->commands
  "Translate a MIDI event without applying commands. Supply the shared control
  envelope with command/show/epoch/time and any ingress source identity.
  Return owner receipts through acknowledge-launch. Held grid launches release
  through the show owner by launch command ID. Echoed feedback and unmapped or
  other-channel input yield no commands. Live mode transfers surface ownership."
  ([state event envelope] (midi->commands state event envelope nil))
  ([state {:keys [type note velocity controller value channel origin time-ms] :as event} envelope resolved-instance]
   (when-not (and (integer? channel) (<= 1 channel 16)
                  (case type
                    (:note-on :note-off) (and (midi-number? note) (midi-number? (or velocity 0)))
                    :cc (and (midi-number? controller) (midi-number? value))
                    false))
     (fail! "Invalid MIDI event" {:event event}))
   (let [off? (or (= type :note-off) (and (= type :note-on) (zero? (or velocity 0))))
         mapping (get-in state [:mappings note])
         grid (get mapping :grid (:launch-grid state))
         held-key (if grid [(get-in envelope [:source :source-id]) note] note)
         target (get-in state [:parameters (parameter-controller state event)])
         target (cond-> target (and target resolved-instance) (assoc :instance-id resolved-instance))
         held (get-in state [:held held-key])]
     (cond
       (or (not= :lighting (:mode state)) (not= channel (:channel state))
           (= origin :tolglow-feedback)) {:state state :commands []}
       (and off? held)
       (if-let [release (release-hold state envelope held)]
         {:state (update state :held dissoc held-key) :commands [release]}
         {:state (assoc-in state [:held held-key :released?] true) :commands []})
       off? {:state state :commands []}
       (and target (not (:disabled? target)))
       (let [cmd (command state envelope :param/set
                          {:param (:param target)
                           :value (decode-value target (if (= :cc type) (/ value 127.0) (/ (dec velocity) 126.0)))})
             pending (get-in state [:pending-cues (:cue-id target)])
             pending-note (first (for [[key h] (:held state)
                                       :when (and (not (:released? h)) (nil? (:instance-id h))
                                                  (= (:cue-id target) (:cue-id h))
                                                  (= (get-in cmd [:source :source-id]) (get-in h [:source :source-id])))] key))]
         (cond
           pending {:state (assoc-in state [:pending-launches pending :updates (:param target)] cmd) :commands []}
           pending-note {:state (assoc-in state [:held pending-note :updates (:param target)] cmd) :commands []}
           (:instance-id target) {:state state :commands [(assoc cmd :instance-id (:instance-id target))]}
           :else {:state (assoc-in state [:next-params (:cue-id target) (:param target)] (:value cmd)) :commands []}))
       (and (= type :note-on) mapping (not (:disabled? mapping)) (nil? held))
       (let [mode (get mapping :mode :persistent)
             launch (cond-> {:mode mode :params (get-in state [:next-params (:cue-id mapping)] {})}
                      (:duration mapping) (assoc :duration (:duration mapping))
                      (:duration-ms mapping) (assoc :duration-ms (:duration-ms mapping))
                      (:velocity-param mapping) (assoc-in [:params (:velocity-param mapping)] (/ velocity 127.0)))
             cmd (command state envelope :cue/launch
                          (cond-> {:cue-id (:cue-id mapping) :launch launch}
                            grid (assoc :grid grid)
                            (and grid (= mode :held)) (assoc :gesture-id (:command-id envelope))
                            (and grid (musical-time/finite-number? time-ms)) (assoc :raw {:time-ms time-ms})))
             entry {:command-id (:command-id cmd) :cue-id (:cue-id mapping) :note note
                    :source (:source cmd) :owner-launch? (some? grid)}]
         (if (and (not= mode :held) (>= (count (:pending-launches state)) (:history-limit state)))
           {:state state :commands [] :events [{:type :live/launch-queue-full :cue-id (:cue-id mapping)}]}
           {:state (cond-> (update state :next-params dissoc (:cue-id mapping))
                     (= mode :held) (assoc-in [:held held-key] entry)
                     (not= mode :held) (assoc-in [:pending-launches (:command-id cmd)] entry)
                     (not= mode :held) (assoc-in [:pending-cues (:cue-id mapping)] (:command-id cmd)))
            :commands [cmd]}))
       :else {:state state :commands []}))))

(defn- held-launch-key [state note id]
  (some (fn [[key entry]] (when (= [note id] ((juxt :note :command-id) entry)) key))
        (:held state)))

(defn awaiting-launch?
  "Does this bridge own the launch identity named by an acknowledgement?"
  [state note id]
  (and (some? id)
       (or (= note (get-in state [:pending-launches id :note] ::missing))
           (some? (held-launch-key state note id)))))

(defn acknowledge-launch
  "Keep pending launches bound until a terminal receipt. Apply deferred
  parameter updates to the accepted instance. Ignore unrelated receipts."
  [state note {:keys [command-id status instance-id]} envelope]
  (if (= :pending status)
    {:state state :commands []}
    (let [instance (when (= :accepted status) instance-id)
          entry (get-in state [:pending-launches command-id])
          held-key (held-launch-key state note command-id)
          held (get-in state [:held held-key])
          updates (fn [launch] (if instance (mapv #(assoc % :instance-id instance) (vals (:updates launch))) []))]
      (cond
        (and entry (= note (:note entry)))
        {:state (cond-> (update state :pending-launches dissoc command-id)
                  (= command-id (get-in state [:pending-cues (:cue-id entry)]))
                  (update :pending-cues dissoc (:cue-id entry)))
         :commands (updates entry)}
        (and held instance)
        (let [bound (assoc (dissoc held :updates) :instance-id instance)]
          (if (:released? held)
            {:state (update state :held dissoc held-key)
             :commands (conj (updates held) (release-hold state envelope bound))}
            {:state (assoc-in state [:held held-key] bound) :commands (updates held)}))
        held {:state (update state :held dissoc held-key) :commands []}
        :else {:state state :commands []}))))

(defn handoff
  "Transfer surface ownership, releasing held cues and cancelling pending grid
  launches. Legacy holds awaiting an acknowledgement retain a released marker."
  [state mode envelope]
  (when-not (#{:lighting :live} mode) (fail! "Unknown surface mode" {:mode mode}))
  (if (= mode (:mode state))
    {:state state :commands [] :events []}
    (let [held (sort-by (juxt :note (comp str :source-id :source)) (vals (:held state)))
          pending (sort-by :command-id (filter :owner-launch? (vals (:pending-launches state))))
          commands (concat (keep (partial release-hold state envelope) held)
                    (map #(cancel-launch state envelope % :surface-handoff) pending))]
      {:state (assoc state :mode mode :next-params {} :pending-launches {} :pending-cues {} :held
                     (into {} (for [[key entry] (:held state) :when (not (or (:owner-launch? entry) (:instance-id entry)))]
                                [key (assoc entry :released? true)])))
       :commands (mapv (fn [index cmd] (assoc cmd :command-id (:command-id (child-envelope envelope index))))
                       (range) commands)
       :events [{:type :surface/clear-feedback :previous-mode (:mode state)}
                {:type :surface/mode :mode mode :redraw? (= mode :lighting)}]})))

(defn- remember [state id]
  (let [order (conj (:record-order state) id)
        evicted (take (max 0 (- (count order) (:history-limit state))) order)]
    (-> state (assoc :record-order (vec (drop (count evicted) order)))
        (update :recorded #(apply disj (conj % id) evicted)))))

(defn- remember-instance [state id cue-id]
  (if (contains? (:instances state) id) state
      (let [slot (:instance-slot state)
            evicted (get (:instance-order state) slot)]
        (-> state
            (update :instances dissoc evicted)
            (assoc-in [:instances id] cue-id)
            (assoc-in [:instance-order slot] id)
            (assoc :instance-slot (mod (inc slot) (:history-limit state)))))))

(defn restore-bridge [bridge instances]
  (let [initialized (new-bridge bridge)
        state (with-meta (merge (dissoc bridge :next-params) initialized) (meta initialized))]
    (reduce (fn [state [id cue-id]] (remember-instance state id cue-id))
            state (sort-by (comp str key) instances))))

(defn- record-one
  "Observe an accepted control command. Return MIDI + bridge clip intents only
  when track is armed, recording, writable and has :destination-id. Param writes
  on the native CC route also need :automation-armed?. Live playback and duplicate command IDs
  are never recorded. The adapter must acknowledge execution before UI claims
  that a clip was written. Musical :beat is supplied by the Live bridge."
  [state cmd ack recording]
  (let [state (if (and (= :accepted (:status ack)) (= (:command-id cmd) (:command-id ack))
                       (= :cue/launch (:op cmd)) (:instance-id ack))
                (remember-instance state (:instance-id ack) (:cue-id cmd)) state)
        cue-id (or (:cue-id cmd) (get-in state [:instances (:instance-id cmd)]))
        compiled (indexes state)
        note (get-in compiled [:cue-notes cue-id])
        cc (first (for [[controller mapping] (get-in compiled [:parameter-controls (:param cmd)])
                        :when (or (and (:instance-id mapping) (= (:instance-id cmd) (:instance-id mapping)))
                                  (= cue-id (:cue-id mapping)))]
                    [controller mapping]))
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
         :intents [(cond-> {:op :live/record-event :protocol-version 1
                    :destination-id (:destination-id recording) :beat (:beat recording)
                    :time-ms (:time-ms cmd) :epoch (:epoch cmd) :show-id (:show-id cmd) :command-id (:command-id cmd)
                    :cue-id cue-id :event (if-let [lane (and (= :cc (:type message)) (:lane-note (second cc)))]
                                            (-> message (dissoc :controller :value)
                                                (assoc :type :note-on :note lane :parameter-sample? true
                                                       :velocity (inc (Math/round (* 126.0 (double fraction))))))
                                            message)}
                     (:cue-timing cmd) (assoc :cue-timing (:cue-timing cmd)))]})
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
