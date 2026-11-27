(ns tolglow.integrations.live-devices
  "One Max for Live parameter device per Tolglow effect on its run channel
  (#625). The device's automatable parameters carry the effect's parameters
  with the labels, units, ranges, decimals and control shapes Studio shows,
  because both read `tolglow.authoring.parameter-controls`.

  Four things are kept apart:
  - a *layout* gives each parameter component of a cue channel a stable
    *slot*. Slots are never reused: a parameter added by a later effect
    revision gets the next free slot, and one that is removed keeps its slot
    (retired), so automation recorded in Live stays on the parameter it was
    recorded for. Device `d` of a channel holds slots `32d` to `32d+31`;
  - a *device spec* is what one channel's devices should show now (names,
    ranges, units, choices and Push-style banks of eight);
  - *values* cross between the show and Live in each slot's native units:
    the parameter's own range for numbers and vector or range components, a
    choice index for choices, and hue, saturation and brightness in [0, 1]
    for a colour, edited through `afterglow.control.parameter-edit` as the
    Push encoders edit it;
  - *ownership*: who may move a slot. Live automation playback arrives with
    origin `:live-playback` and steers the instance its run clip launched; a
    performer turning the dial in Live arrives as `:live-input`, a
    performance the run record keeps as take moves. A value Tolglow wrote into
    Live comes back from Live's observer once and is dropped as an echo. A
    Tolglow write from Studio, Push, the REPL or an agent holds the slot
    against Live automation until Live reports its automation playing again
    or a run clip starts on that track. Nothing that came from Live is ever
    written back to Live.

  Nothing here reads a clock or performs IO; the host passes the committed
  model, the run record, its last mirror and the run-clip player."
  (:require [clojure.string :as str]
            [afterglow.control.parameter-edit :as parameter-edit]
            [afterglow.control.params :as params]
            [tolglow.authoring.parameter-controls :as controls]
            [tolglow.authoring.parameter-layout :as parameter-layout]
            [tolglow.registry :as registry]
            [tolglow.studio.parameter-precision :as precision]
            [tolglow.visualization.parameter-views :as views]))

(def slots-per-device
  "Automatable parameters one Tolglow parameter device declares. Keep in step
  with resources/devices/ableton-live/parameter_device.js SLOTS."
  32)

(def max-devices
  "Parameter devices one run channel may need: 256 slots."
  8)

(def bank-size
  "Parameters per bank, as on Push's eight encoders."
  8)

(def device-name
  "The name a parameter device carries in Live. Device d > 0 of a channel is
  named with its number."
  "Tolglow parameters")

(def ^:private live-origins #{:live-playback :live-input})

(defn- fail! [message data] (throw (ex-info message data)))

(defn- finite? [x] (and (number? x) (Double/isFinite (double x))))

(defn- clamp [x lo hi] (max lo (min hi x)))

(defn- tag [x] (cond (keyword? x) x (string? x) (keyword x)))

;; ---------------------------------------------------------------------------
;; Slot keys and native ranges

(defn- live-bounds
  "The range Live shows: the hard bounds, else the recommended range."
  [schema]
  (or (controls/bounds schema)
      (let [[lo hi] (:recommended schema)]
        (when (and (finite? lo) (finite? hi) (< lo hi)) [lo hi]))))

(defn- component-names [schema]
  (case (tag (:type schema))
    :color ["hue" "saturation" "brightness"]
    :range ["low" "high"]
    :vector (mapv str (views/component-labels schema))
    [nil]))

(defn- slot-kind
  "What one parameter becomes in Live, or {:unsupported reason}."
  [schema catalog]
  (let [type (controls/control-type schema catalog)
        listed (controls/choices schema catalog)]
    (cond
      (#{:waveform :shape :choice :enum} type)
      (if (and (seq listed) (<= (count listed) 128))
        {:kind :enum :components 1}
        {:unsupported :choices})
      (= :boolean type) {:kind :toggle :components 1}
      (= :gradient type) {:unsupported :gradient}
      (= :color type) {:kind :color :components 3}
      (#{:vector :range} type)
      (let [n (if (= :range type) 2 (:size schema))]
        (cond (not (and (pos-int? n) (<= n 4))) {:unsupported :vector-size}
              (not (live-bounds schema)) {:unsupported :unbounded}
              :else {:kind type :components n}))
      (#{:number :integer} type)
      (if (live-bounds schema) {:kind type :components 1} {:unsupported :unbounded})
      :else {:unsupported :type})))

(defn- component-key [param kind i] [param (when (#{:color :vector :range} kind) i)])

(defn ordered-parameters
  "The effect's parameters in Studio's order (parameter-layout)."
  [definition cue]
  (mapv first (:parameters (parameter-layout/resolve-layout definition cue))))

(defn slot-keys
  "[param component] keys of the supported components in Studio's order,
  and the unsupported parameters with their reasons."
  [definition cue catalog]
  (reduce (fn [acc param]
            (let [schema (get-in definition [:params param])
                  {:keys [kind components unsupported]} (slot-kind schema catalog)]
              (if unsupported
                (update acc :unsupported conj {:param param :reason unsupported})
                (update acc :keys into (map #(component-key param kind %) (range components))))))
          {:keys [] :unsupported []}
          (ordered-parameters definition cue)))

;; ---------------------------------------------------------------------------
;; Layouts: stable slots

(defn new-layout [] {:layout/version 1 :slots {} :order [] :next 0})

(defn allocate
  "Give every key without a slot the next free one; earlier keys keep theirs.
  Refuses a channel that would need more than max-devices devices."
  [layout keys]
  (let [layout (reduce (fn [l k]
                         (if (contains? (:slots l) k)
                           l
                           (-> l (assoc-in [:slots k] (:next l)) (update :order conj k) (update :next inc))))
                       (or layout (new-layout)) keys)]
    (when (> (:next layout) (* slots-per-device max-devices))
      (fail! "An effect channel holds at most 256 Live parameter slots" {:slots (:next layout)}))
    layout))

(defn- assign [layout k slot]
  (-> layout (assoc-in [:slots k] slot) (update :order conj k) (update :next max (inc slot))))

(defn seed-layout
  "A layout rebuilt from what a Set's parameter devices show, so a restarted
  show keeps every parameter on the slot its automation was recorded for.
  names->key maps a slot name the current spec would show to its
  [param component]; read-back is the inspected devices
  [{:ordinal o :parameters [{:index i :name n}]}]. A slot whose name the
  current revision no longer produces (an Unused slot, a removed parameter)
  stays reserved; a slot that was never configured (\"slot n\") is free.
  Keys without a slot are then allocated after the highest one."
  [names->key read-back keys]
  (allocate
   (reduce (fn [layout {:keys [ordinal parameters]}]
             (reduce (fn [layout {:keys [index name]}]
                       (let [slot (+ (* slots-per-device (or ordinal 0)) index)
                             k (get names->key name)]
                         (cond (some #{slot} (vals (:slots layout))) layout
                               (and k (not (contains? (:slots layout) k))) (assign layout k slot)
                               (or (not (string? name)) (re-matches #"slot \d+" name)) layout
                               :else (assign layout [::reserved slot] slot))))
                     layout (sort-by :index parameters)))
           (new-layout) (sort-by :ordinal read-back))
   keys))

(defn devices-needed [layout] (max 1 (long (Math/ceil (/ (:next layout 0) (double slots-per-device))))))

;; ---------------------------------------------------------------------------
;; Device spec

(defn- definition-of
  "The cue and the effect revision it launches, honouring an :effect-version pin."
  [model cue-id]
  (let [reg (:registry model)
        cue (get-in reg [:cues cue-id])]
    (when cue (when-let [effect (registry/cue-effect reg cue)] [cue effect]))))

(defn- annotate
  "The definition with :dmx-bits on parameters that reach a patched channel,
  so decimals match Studio's readouts on this rig."
  [model definition]
  (get (precision/definitions (atom nil) (get-in model [:rig :fixtures]) [[definition nil]]) [definition nil] definition))

(defn- short-name [text]
  (let [t (str/trim (str text))] (if (> (count t) 12) (subs t 0 12) t)))

(defn- slot-entry [param schema kind component catalog]
  (let [control (controls/control param schema catalog)
        part (when (some? component) (nth (component-names schema) component nil))
        name (str (:label control) (when part (str " " part)))
        [lo hi] (live-bounds schema)
        listed (:choices control)]
    (merge {:param param :component component :name name
            :short-name (short-name (if part (str (subs (:label control) 0 (min 7 (count (:label control)))) " " (subs part 0 1)) (:label control)))
            :places (:places control)}
           (case kind
             :enum {:type :enum :min 0 :max (dec (count listed)) :steps (count listed)
                    :choices (mapv :label listed) :widget :menu}
             :toggle {:type :enum :min 0 :max 1 :steps 2 :choices ["Off" "On"] :widget :toggle}
             :color {:type :float :min 0.0 :max 1.0 :places 3 :widget :rotary}
             :integer {:type :int :min (long (Math/ceil (double lo))) :max (long (Math/floor (double hi)))
                       :steps (inc (- (long (Math/floor (double hi))) (long (Math/ceil (double lo)))))
                       :widget (controls/widget (assoc schema :bounds [lo hi]))}
             {:type :float :min (double lo) :max (double hi)
              :widget (if (#{:vector :range} kind) :rotary (controls/widget (assoc schema :bounds [lo hi])))})
           (when-let [unit (:unit control)] {:unit unit})
           (when (and (#{:number :integer} kind) (not (controls/bounds schema))) {:range-source :recommended}))))

(defn device-spec
  "What one channel's parameter devices should show now:
    {:cue-id :effect-id :effect-version :devices n
     :slots [{:slot :device :index :param :component :name :short-name :type
              :min :max :steps :choices :places :unit :widget} ..]
     :banks [[slot ..] ..]       ; per device, eight per bank, Studio's order
     :unsupported [{:param :reason} ..]}
  :type is :float, :int or :enum (choices and toggles); :retired? marks a slot
  whose parameter the current revision no longer has."
  [model cue-id layout catalog]
  (when-let [[cue definition] (definition-of model cue-id)]
    (let [definition (annotate model definition)
          {:keys [keys unsupported]} (slot-keys definition cue catalog)
          current (set keys)
          entry (fn [[param component :as k] slot]
                  (let [schema (get-in definition [:params param])
                        base {:slot slot :device (quot slot slots-per-device) :index (mod slot slots-per-device)}]
                    (if (contains? current k)
                      (merge base (slot-entry param schema (:kind (slot-kind schema catalog)) component catalog))
                      (assoc base :param param :component component :retired? true
                             :name (str "Unused " (inc slot)) :short-name (str "— " (inc slot))
                             :type :float :min 0.0 :max 1.0 :places 3 :widget :rotary))))
          slots (mapv (fn [k] (entry k (get-in layout [:slots k]))) (:order layout))
          ;; Live wants distinct long names within a device.
          slots (second (reduce (fn [[seen out] s]
                                  (let [n (get seen [(:device s) (:name s)] 0)]
                                    [(update seen [(:device s) (:name s)] (fnil inc 0))
                                     (conj out (cond-> s (pos? n) (update :name str " " (inc n))))]))
                                [{} []] slots))
          devices (devices-needed layout)
          by-key (into {} (map (juxt (juxt :param :component) :slot)) slots)]
      {:cue-id cue-id :effect-id (:effect-id cue) :effect-version (:version definition)
       :devices devices
       :slots slots
       :banks (vec (for [d (range devices)]
                     (vec (map vec (partition-all bank-size
                                                  (filter #(= d (quot % slots-per-device)) (map by-key keys)))))))
       :unsupported unsupported})))

(defn device-slots
  "The slots of device d of a spec, as the device configures them."
  [spec d]
  (filterv #(= d (:device %)) (:slots spec)))

;; ---------------------------------------------------------------------------
;; Values

(defn- schema-of [model cue-id param]
  (when-let [[_ definition] (definition-of model cue-id)] (get-in definition [:params param])))

(defn- instance-schema
  "The schema a running instance actually uses: its own (pinned) definition,
  else the cue's current one."
  [model cue-id instance-id param]
  (or (get-in model [:instances instance-id :definition :params param])
      (schema-of model cue-id param)))

(defn- choice-index
  "The index of a choice holding value; for numeric choices the nearest one.
  nil for a value no choice holds, so Live never shows a wrong choice."
  [listed value]
  (let [numeric (every? (comp number? :value) listed)]
    (or (first (keep-indexed (fn [i c] (when (= value (:value c)) i)) listed))
        (when (and numeric (finite? value))
          (first (apply min-key (fn [[_ c]] (Math/abs (- (double value) (double (:value c))))) (map-indexed vector listed)))))))

(defn native-value
  "A typed parameter value as the slot's native Live value, or nil when the
  value does not fit its schema."
  [schema value component catalog]
  (let [{:keys [kind]} (slot-kind schema catalog)
        [lo hi] (live-bounds schema)]
    (try
      (case kind
        :enum (choice-index (controls/choices schema catalog) value)
        :toggle (if (true? value) 1 0)
        :color (parameter-edit/position schema value {:component component})
        (:vector :range) (let [v (nth value component)] (when (finite? v) (double (clamp v lo hi))))
        :integer (when (finite? value) (long (clamp (Math/round (double value)) (Math/ceil (double lo)) (Math/floor (double hi)))))
        :number (when (finite? value) (double (clamp value lo hi)))
        nil)
      (catch Exception _ nil))))

(defn- default-value [schema]
  (let [{:keys [type default size]} schema]
    (cond (some? default) default
          (= :color (tag type)) [0.0 0.0 0.0]
          (= :range (tag type)) (let [[lo hi] (live-bounds schema)] [lo hi])
          (= :vector (tag type)) (vec (repeat size (first (live-bounds schema))))
          :else nil)))

(defn color-value
  "The colour for hue, saturation and brightness in [0, 1], in the schema's
  colour bounds, as a Push encoder edit produces it."
  [schema [h s v]]
  (let [[low high] (params/color-bounds schema)]
    (mapv #(+ low (* % (- high low))) (parameter-edit/hsv->rgb [(mod h 1.0) (clamp s 0.0 1.0) (clamp v 0.0 1.0)]))))

(defn color-components
  "Hue, saturation and brightness of a colour value, as its slots show them."
  [schema value]
  (mapv #(parameter-edit/position schema value {:component %}) [0 1 2]))

(defn typed-value
  "A slot's native Live value as the parameter value to set, given the
  instance's current value for components. nil when it cannot be applied.
  A colour component edits the colour's hue, saturation and brightness as
  read from the current value; ingest keeps each colour's last components so
  a hue survives passing through grey."
  [schema current component native catalog]
  (let [{:keys [kind]} (slot-kind schema catalog)
        [lo hi] (live-bounds schema)
        current (if (some? current) current (default-value schema))]
    (when (finite? native)
      (try
        (case kind
          :enum (let [listed (controls/choices schema catalog)]
                  (:value (nth listed (long (clamp (Math/round (double native)) 0 (dec (count listed)))))))
          :toggle (>= native 0.5)
          :color (parameter-edit/edit schema current (clamp native 0.0 1.0) {:mode :absolute :component component})
          :vector (assoc (vec current) component (double (clamp native lo hi)))
          :range (let [[a b] current
                       v (double (clamp native lo hi))]
                   (if (zero? component) [(min v b) b] [a (max v a)]))
          :integer (long (clamp (Math/round (double native)) (Math/ceil (double lo)) (Math/floor (double hi))))
          :number (double (clamp native lo hi))
          nil)
        (catch Exception _ nil)))))

;; ---------------------------------------------------------------------------
;; Channels, tracks and instances

(defn- track-lanes
  "{track-id [cue-id lane]} from a run mirror."
  [mirror]
  (into {} (keep (fn [[[kind cue-id lane] track-id]] (when (= :lane kind) [track-id [cue-id lane]]))) (:tracks mirror)))

(defn- target-instance
  "The instance a slot on this track steers: the run its playing clip
  launched, else the open run on the track's lane."
  [{:keys [record player]} track-id cue-id lane]
  (or (get-in player [:playing track-id :instance-id])
      (let [take-id (get-in record [:lanes [cue-id lane]])
            instance-id (get-in record [:takes take-id :instance-id])]
        (when (and take-id (= take-id (get-in record [:open instance-id]))) instance-id))))

(defn new-state
  "Owner state: layouts per cue channel, outstanding writes (expected echoes),
  holds, values waiting for a replay's launch and counters."
  []
  {:state/version 1 :layouts {} :echo {} :write-seq 0 :holds #{} :automation {} :serials #{} :serial-order []
   :seen #{} :seen-order [] :counts {} :hsv {} :waiting {}})

(defn current-key?
  "Is [param component] a slot of this schema now? A slot whose parameter was
  removed or reshaped (a number become a colour, a vector shortened) is not."
  [schema component catalog]
  (when schema
    (let [{:keys [kind components]} (slot-kind schema catalog)]
      (cond (nil? kind) false
            (#{:color :vector :range} kind) (and (integer? component) (< -1 component components))
            :else (nil? component)))))

(defn ensure-layouts
  "Allocate slots for every channel of the run record whose cue has an
  effect, in Studio's order. A refused channel (too many slots) is reported
  under :problems and keeps its earlier layout."
  [state model record catalog]
  (reduce (fn [s cue-id]
            (if-let [[cue definition] (definition-of model cue-id)]
              (try (update-in s [:layouts cue-id] allocate (:keys (slot-keys definition cue catalog)))
                   (catch clojure.lang.ExceptionInfo e
                     (update s :problems (fnil conj []) {:code :too-many-slots :cue-id cue-id :message (.getMessage e)})))
              s))
          (dissoc state :problems) (:channel-order record)))

(defn seed-from-set
  "Rebuild the layout of every channel not yet seeded from the parameter
  devices its run tracks show in an inspected Set (tracks named by
  track-name-fn [cue-id lane]), then mark every layout seeded. Before its
  first sync a layout has written nothing to Live, so replacing it is safe."
  [state model set-snapshot track-name-fn lanes-by-cue catalog]
  (reduce-kv
   (fn [s cue-id layout]
     (if (:seeded? layout)
       s
       (let [devices (vec (for [lane (range (get lanes-by-cue cue-id 1))
                                track (:tracks set-snapshot)
                                :when (= (track-name-fn cue-id lane) (:name track))
                                d (:parameter-devices track)]
                            d))
             [cue definition] (definition-of model cue-id)
             keys (when definition (:keys (slot-keys definition cue catalog)))
             names->key (when definition
                          (into {} (map (juxt :name (juxt :param :component)))
                                (remove :retired? (:slots (device-spec model cue-id (allocate nil keys) catalog)))))
             layout (if (and (seq devices) definition)
                      (try (seed-layout names->key (vals (into {} (map (juxt :ordinal identity)) devices)) keys)
                           (catch clojure.lang.ExceptionInfo _ layout))
                      layout)]
         (assoc-in s [:layouts cue-id] (assoc layout :seeded? true)))))
   state (:layouts state)))

(defn specs
  "{cue-id spec} for every channel with a layout."
  [state model catalog]
  (into {} (keep (fn [[cue-id layout]] (when-let [spec (device-spec model cue-id layout catalog)] [cue-id spec])))
        (:layouts state)))

(defn- count! [state k] (update-in state [:counts k] (fnil inc 0)))

(defn- seen-serial [state k]
  (let [order (conj (:serial-order state) k)]
    (if (> (count order) 1024)
      (assoc state :serials (disj (conj (:serials state) k) (first order)) :serial-order (subvec order 1))
      (assoc state :serials (conj (:serials state) k) :serial-order order))))

(def echo-tolerance
  "Fraction of a continuous slot's range within which a value Live reports
  back equals the one Tolglow wrote (Live keeps 32-bit floats). Integer,
  choice and toggle slots match within half a step."
  1e-5)

(def echo-timeout-ms
  "An outstanding write whose echo has not come back within this long is
  forgotten."
  3000)

(def max-outstanding
  "Writes per slot that may wait for their echo at once."
  16)

(defn- echo-match
  "The index of the outstanding write a reported value echoes, or nil."
  [entries value schema catalog]
  (let [{:keys [kind]} (slot-kind schema catalog)
        [lo hi] (live-bounds schema)
        tolerance (if (#{:enum :toggle :integer} kind)
                    0.5
                    (* echo-tolerance (max 1e-9 (if (and lo hi (not= :color kind)) (- hi lo) 1.0))))]
    (first (keep-indexed (fn [i {v :value}] (when (<= (Math/abs (- (double value) (double v))) tolerance) i)) entries))))

(defn forget-writes
  "Forget the outstanding writes with these sequence numbers (a send that did
  not execute), leaving newer writes' echoes in place."
  [state track-id seqs-by-slot]
  (reduce-kv (fn [s slot seqs]
               (let [k [track-id slot] gone (set seqs)
                     left (filterv #(not (gone (:seq %))) (get-in s [:echo k]))]
                 (if (seq left) (assoc-in s [:echo k] left) (update s :echo dissoc k))))
             state seqs-by-slot))

(defn- slot-of [layout param component] (get-in layout [:slots [param component]]))

(defn- key-of [layout slot] (some (fn [[k s]] (when (= s slot) k)) (:slots layout)))

(defn clip-started
  "A run clip started on a track: Live playback owns its slots again."
  [state track-id]
  (update state :holds #(into #{} (remove (fn [[t]] (= t track-id))) %)))

(defn- playing-entry [player track-id] (get-in player [:playing track-id]))

(defn- valid-for? [schema value]
  (try (or (not (#{:number :integer :boolean :enum :color :gradient :vector :range} (tag (:type schema))))
           (params/valid-value? (update schema :type tag) value))
       (catch Exception _ false)))

(defn- colour-base
  "The hue, saturation and brightness a colour slot edits: the cached
  components while they still describe the instance's value (so a hue
  survives passing through grey), else read from the value itself."
  [state instance-id param schema current]
  (let [cached (get-in state [:hsv [instance-id param]])
        current (if (some? current) current (default-value schema))]
    (or (when (and cached (vector? current)
                   (every? true? (map #(< (Math/abs (- (double %1) (double %2))) 1e-6) (color-value schema cached) current)))
          cached)
        (try (color-components schema current) (catch Exception _ nil)))))

(defn ingest
  "Decode parameter-device values reported by the companion into commands.
  ctx: {:model :record :mirror :player :envelope-fn :catalog :now-ms}. An event:
    {:type :parameter-device-value :track-id t :slot n :value v
     :automation-state 0|1|2 :value-changed? bool :watch-session w :serial k}
  automation-state is Live's DeviceParameter.automation_state: 1 means the
  value is automation playing back. :value-changed? false reports a change of
  automation state alone (an override, a re-enable): it updates ownership and
  moves nothing. {:type :clip-started :track-id t} (queued by the host when a
  run clip starts) hands that track's slots back to Live playback.
  - A value that matches any outstanding Tolglow write of its slot is that
    write's echo: it and every older outstanding write are dropped.
  - A slot whose parameter the instance's definition no longer has in that
    shape (retired) is dropped before decoding.
  - Values for a track whose replayed clip has not had its launch
    acknowledged wait (up to 64 per track) and apply to that replay once it
    exists; they are dropped if the clip stops first.
  Returns {:state :commands :dropped}, at most one :param/set per instance
  parameter (the last value wins). A value that cannot be applied is counted
  and dropped; nothing here throws on Live's data."
  [state {:keys [model mirror player envelope-fn catalog now-ms] :as ctx} events]
  (let [lanes (track-lanes mirror)
        now (or now-ms 0)
        state (-> state
                  (update :hsv #(into {} (filter (fn [[[id] _]] (contains? (:instances model) id))) %))
                  ;; Outstanding writes whose echo never came back expire.
                  (update :echo #(into {} (keep (fn [[k es]] (let [es (filterv (fn [e] (< (- now (:at e 0)) echo-timeout-ms)) es)]
                                                               (when (seq es) [k es])))) %)))
        ;; Values that waited for a replay's launch, now bound or abandoned.
        [state released]
        (reduce-kv (fn [[s out] track-id waiting]
                     (let [entry (playing-entry player track-id)]
                       (cond (:instance-id entry) [(update s :waiting dissoc track-id) (into out waiting)]
                             entry [s out]
                             :else [(-> s (update :waiting dissoc track-id) (update-in [:counts :waiting-dropped] (fnil + 0) (count waiting))) out])))
                   [state []] (:waiting state))
        step
        (fn [{:keys [state] :as acc} {:keys [track-id slot value watch-session serial] :as event}]
          (let [automation (get event :automation-state 0)
                k [track-id slot]
                was (get-in state [:automation k])
                state (cond-> (assoc-in state [:automation k] automation)
                        serial (seen-serial [watch-session serial])
                        ;; Live re-enabled automation: playback owns the slot again.
                        (and (= 1 automation) (some? was) (not= 1 was)) (update :holds disj k))
                drop! (fn [state reason]
                        (-> acc (assoc :state (count! state reason))
                            (update :dropped conj (assoc (select-keys event [:track-id :slot :serial]) :reason reason))))
                [cue-id lane] (get lanes track-id)
                layout (get-in state [:layouts cue-id])
                [param component] (when layout (key-of layout slot))
                entry (playing-entry player track-id)
                instance-id (when cue-id (target-instance ctx track-id cue-id lane))
                schema (when param (if instance-id (instance-schema model cue-id instance-id param) (schema-of model cue-id param)))
                outstanding (get-in state [:echo k])
                echo (when (and schema (seq outstanding) (finite? value)) (echo-match outstanding value schema catalog))]
            (cond
              (not (finite? value)) (drop! state :invalid)
              (nil? cue-id) (drop! state :unknown-track)
              (nil? param) (drop! state :unknown-slot)
              (not (current-key? schema component catalog)) (drop! state :retired)
              echo (drop! (let [left (subvec outstanding (inc echo))]
                            (if (seq left) (assoc-in state [:echo k] left) (update state :echo dissoc k)))
                          :echo)
              (false? (:value-changed? event)) (-> acc (assoc :state (count! state :state-only)))
              (and (= 1 automation) (contains? (:holds state) k)) (drop! state :held)
              (and entry (nil? (:instance-id entry)))
              (if (< (count (get-in state [:waiting track-id])) 64)
                (assoc acc :state (-> state (update-in [:waiting track-id] (fnil conj []) event) (count! :waiting)))
                (drop! state :waiting-full))
              (not (and instance-id (get-in model [:instances instance-id]))) (drop! state :no-instance)
              :else
              (let [current (get-in acc [:pending [instance-id param]] (get-in model [:instances instance-id :params param]))
                    color? (= :color (:kind (slot-kind schema catalog)))
                    hsv (when color?
                          (when-let [base (colour-base state instance-id param schema current)]
                            (assoc base component (clamp (double value) 0.0 1.0))))
                    typed (try (if color?
                                 (when hsv (color-value schema hsv))
                                 (typed-value schema current component value catalog))
                               (catch Exception _ nil))]
                (if (or (nil? typed) (not (valid-for? schema typed)))
                  (drop! state :invalid)
                  (-> acc
                      (assoc :state (cond-> (count! state :applied)
                                      hsv (assoc-in [:hsv [instance-id param]] hsv)))
                      (assoc-in [:pending [instance-id param]] typed)
                      (assoc-in [:meta [instance-id param]]
                                {:origin (if (= 1 automation) :live-playback :live-input)
                                 :track-id track-id :slot slot})))))))
        safe-step (fn [acc event]
                    (try (step acc event)
                         (catch Exception _
                           (-> acc (update :state count! :invalid)
                               (update :dropped conj (assoc (select-keys event [:track-id :slot :serial]) :reason :invalid))))))
        result (reduce (fn [acc {:keys [type watch-session serial] :as event}]
                         (cond
                           (= :clip-started type) (update acc :state clip-started (:track-id event))
                           (not= :parameter-device-value type) (update acc :state count! :unknown-event)
                           (and serial (not (::released event)) (contains? (:serials (:state acc)) [watch-session serial]))
                           (update acc :state count! :repeat)
                           :else (safe-step acc (dissoc event ::released))))
                       {:state state :pending {} :meta {} :dropped []}
                       (concat (map #(assoc % ::released true) released) events))]
    {:state (:state result)
     :dropped (:dropped result)
     :commands (vec (for [[[instance-id param] value] (sort-by (comp str key) (:pending result))
                          :let [{:keys [origin track-id slot]} (get-in result [:meta [instance-id param]])]]
                      (merge (envelope-fn)
                             {:op :param/set :instance-id instance-id :param param :value value
                              :source {:type :ableton :adapter :live-device :origin origin
                                       :live/track-id track-id :live/slot slot}})))}))

(defn waiting?
  "Do values wait for a replay's launch? The host then runs ingest even in a
  cycle without new events."
  [state]
  (boolean (seq (:waiting state))))

;; ---------------------------------------------------------------------------
;; Tolglow writes mirrored into Live

(defn- fresh-ids [state model]
  (let [order (get-in model [:control :history-order])]
    (reverse (take-while #(not (contains? (:seen state) %)) (if (vector? order) (rseq order) (reverse order))))))

(defn- remember [state id]
  (let [order (conj (:seen-order state) id)]
    (if (> (count order) 4096)
      (assoc state :seen (disj (conj (:seen state) id) (first order)) :seen-order (subvec order 1))
      (assoc state :seen (conj (:seen state) id) :seen-order order))))

(defn- lane-track [mirror cue-id lane] (get-in mirror [:tracks [:lane cue-id lane]]))

(defn- instance-lane
  "[cue-id lane] whose Live track shows an instance: its open take's lane, or
  the lane whose clip is playing it back."
  [{:keys [record player mirror]} instance-id]
  (or (when-let [take-id (get-in record [:open instance-id])]
        ((juxt :cue-id :lane) (get-in record [:takes take-id])))
      (when-let [[track-id] (some (fn [[t e]] (when (= instance-id (:instance-id e)) [t])) (:playing player))]
        (get (track-lanes mirror) track-id))))

(defn mirror-writes
  "Values to write into Live for parameter changes the show accepted since the
  last call: every :param/set whose origin is not Live itself, on an instance
  shown on a bound run track whose device is present (ctx :present?, a
  (fn [cue-id lane device]); every bound track when absent). Each write joins
  its slot's outstanding writes (bounded, expiring), whose echoes ingest
  drops, and holds the slot against Live automation. Returns {:state :writes}
  with writes
    [{:track-id t :cue-id :lane :values [{:slot :device :index :value :seq} ..]} ..]
  coalesced per track (the last value of a slot wins). A write that is not
  sent should be forgotten with forget-writes."
  [state {:keys [model mirror catalog now-ms present?] :as ctx}]
  (let [history (get-in model [:control :history])
        ids (fresh-ids state model)
        [state values]
        (reduce (fn [[state values] id]
                  (let [{:keys [command ack]} (get history id)
                        state (remember state id)
                        origin (get-in command [:source :origin])
                        instance-id (:instance-id command)
                        param (:param command)]
                    (if (or (not= :param/set (:op command)) (not= :accepted (:status ack))
                            (contains? live-origins origin) (= :tolglow-feedback origin) (not (keyword? param)))
                      [state values]
                      (let [[cue-id lane] (instance-lane ctx instance-id)
                            track-id (when cue-id (lane-track mirror cue-id lane))
                            layout (get-in state [:layouts cue-id])
                            schema (when cue-id (instance-schema model cue-id instance-id param))
                            {:keys [kind components]} (when schema (slot-kind schema catalog))
                            ;; Tolglow changed this colour: Live's cached components no longer describe it.
                            state (update state :hsv dissoc [instance-id param])]
                        (if (or (nil? track-id) (nil? layout) (nil? kind))
                          [state values]
                          (reduce (fn [[state values] i]
                                    (let [component (second (component-key param kind i))
                                          slot (slot-of layout param component)
                                          device (when slot (quot slot slots-per-device))
                                          native (when (and slot (or (nil? present?) (present? cue-id lane device)))
                                                   (native-value schema (:value command) component catalog))]
                                      (if (nil? native)
                                        [state values]
                                        (let [seq-no (inc (:write-seq state))
                                              ;; A value replaced in this same batch is never sent.
                                              replaced (get-in values [[track-id slot] :seq])
                                              state (cond-> state replaced (forget-writes track-id {slot [replaced]}))]
                                          [(-> state
                                               (assoc :write-seq seq-no)
                                               (update-in [:echo [track-id slot]]
                                                          (fn [es] (let [es (conj (vec es) {:value native :seq seq-no :at (or now-ms 0)})]
                                                                     (if (> (count es) max-outstanding) (subvec es (- (count es) max-outstanding)) es))))
                                               (update :holds conj [track-id slot])
                                               (count! :mirrored))
                                           (assoc values [track-id slot] {:cue-id cue-id :lane lane :value native :seq seq-no})]))))
                                  [state values] (range components)))))))
                [state {}] ids)]
    {:state state
     :writes (vec (for [[track-id entries] (group-by (comp first key) (sort-by (comp str key) values))
                        :let [{:keys [cue-id lane]} (val (first entries))]]
                    {:track-id track-id :cue-id cue-id :lane lane
                     :values (mapv (fn [[[_ slot] {:keys [value seq]}]]
                                     {:slot slot :device (quot slot slots-per-device) :index (mod slot slots-per-device)
                                      :value value :seq seq})
                                   entries)}))}))

;; ---------------------------------------------------------------------------
;; Take moves as clip envelopes

(defn take-envelopes
  "A take's launch values and parameter moves as envelope points of its
  channel's slots: [{:slot :device :index :points [[beat native] ..]} ..].
  Beats are relative to the run's start (held time, so a move after a seek
  keeps its offset). Each slot starts at beat 0 with the launch value, or the
  first move's value when the launch had none, and holds each value until
  the next point. Moves past the take's held length are left out."
  [model spec layout take held-beats catalog]
  (let [cue-id (:cue-id take)
        params (:params take)
        moves (:moves take)
        touched (distinct (concat (keys params) (map :param moves)))]
    (vec
     (for [param touched
           :let [schema (schema-of model cue-id param)
                 {:keys [kind components]} (when schema (slot-kind schema catalog))]
           :when kind
           i (range components)
           :let [component (second (component-key param kind i))
                 slot (slot-of layout param component)
                 native #(native-value schema % component catalog)
                 points (cond->> (for [{:keys [at value] :as m} moves
                                       :when (= param (:param m))
                                       :let [rel (or at 0)]
                                       :when (and held-beats (< rel held-beats))
                                       :let [v (native value)]
                                       :when (some? v)]
                                   [rel v])
                          (contains? params param) (cons [0 (native (get params param))]))
                 points (->> points
                             (remove (comp nil? second))
                             ;; One value per beat (the last), then drop a point that
                             ;; repeats the value before it.
                             (reduce (fn [out [b v]] (if (and (seq out) (= b (first (peek out)))) (conj (pop out) [b v]) (conj out [b v]))) [])
                             (#(if (and (seq %) (pos? (first (first %)))) (into [[0 (second (first %))]] %) %))
                             (reduce (fn [out [b v]] (if (and (seq out) (= v (second (peek out)))) out (conj out [b v]))) []))]
           :when (and slot (seq points) (some #(= slot (:slot %)) (:slots spec)))]
       {:slot slot :device (quot slot slots-per-device) :index (mod slot slots-per-device) :points (vec points)}))))

;; ---------------------------------------------------------------------------
;; What the Set shows

(defn device-ordinal
  "The ordinal of a parameter device from its name: 0 for \"Tolglow
  parameters\", n-1 for \"Tolglow parameters n\", else nil."
  [name]
  (when (string? name)
    (when-let [[_ n] (re-matches #"Tolglow parameters(?: (\d+))?" name)]
      (if n (dec (parse-long n)) 0))))

(defn device-label [d] (if (zero? d) device-name (str device-name " " (inc d))))

(defn- param-matches? [{:keys [name min max]} slot]
  (and (= name (:name slot))
       (finite? min) (finite? max)
       (< (Math/abs (- (double min) (double (:min slot)))) 1e-6)
       (< (Math/abs (- (double max) (double (:max slot)))) 1e-6)))

(defn device-status
  "Compare the parameter devices an inspected run track holds with a spec:
    {:present [ordinal ..] :missing [ordinal ..] :stale [ordinal ..]
     :devices {ordinal {:device-index i :device-id id}}}
  A device is stale when any slot's read-back name or range differs."
  [track spec]
  (let [found (into {} (keep (fn [d] (when-let [o (device-ordinal (:name d))] [o d]))) (:parameter-devices track))
        wanted (range (:devices spec))]
    {:present (vec (filter found wanted))
     :missing (vec (remove found wanted))
     :stale (vec (for [o wanted
                       :let [d (get found o)]
                       :when d
                       :when (not (every? (fn [slot] (param-matches? (get (:parameters d) (:index slot)) slot))
                                          (device-slots spec o)))]
                   o))
     :devices (into {} (map (fn [[o d]] [o (select-keys d [:device-index :device-id])])) found)}))

(defn projection
  "Plain data for Studio and the REPL: each channel's device spec, where its
  devices are and the ownership counters."
  [state specs mirror-status]
  {:channels (vec (for [[cue-id spec] (sort-by (comp str key) specs)]
                    (assoc (select-keys spec [:cue-id :effect-id :effect-version :devices :banks :unsupported])
                           :slots (mapv #(select-keys % [:slot :device :index :param :component :name :short-name :type
                                                         :min :max :steps :choices :places :unit :widget :retired?])
                                        (:slots spec))
                           :tracks (get mirror-status cue-id []))))
   :holds (count (:holds state))
   :pending-echoes (count (:echo state))
   :counts (:counts state {})
   :problems (:problems state [])})
