(ns tolglow.integrations.phrase-clip
  "Render a captured phrase as editable Live MIDI clips and import them again
  (#671, #680).

  A layout allocates MIDI addresses once and is saved with the show:
  - every cue gets one pitch: one note per activation, its length the run;
  - every parameter component gets a paired 14-bit controller: the coarse
    value on CC n (1-31, skipping bank select 0 and data entry 6) and the fine
    value on CC n+32, the standard MIDI pairing.
  Compound values (colour, aim vectors, ranges) use one pair per component.
  Gradients have no reversible scalar form and are reported as unassigned.

  A clip description mirrors what the Live Object Model can hold: notes with
  pitch, start, duration and velocity, plus timed controller values. Beats are
  exact rationals. `clips->phrase` decodes an edited clip, so the edit replays
  exactly; the first encode quantises values to 14 bits (1/16383 of range).
  `clip->smf` and `smf->clip` carry the same description in a Standard MIDI
  File for export, inspection and import into Live.

  `player` decodes played-back MIDI (from Live through the bridge) into shared
  control commands with origin `:live-playback`, binding launch receipts to the
  notes that started them."
  (:require [clojure.string :as str]
            [tolglow.integrations.phrase-capture :as capture]
            [tolglow.registry :as registry])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream]
           [javax.sound.midi MetaMessage MidiEvent MidiSystem Sequence ShortMessage Track]))

(def layout-version 1)

(def clip-version 1)

(def coarse-controllers
  "Coarse controller numbers available for parameter lanes; fine is n+32."
  (vec (remove #{0 6} (range 1 32))))

(def max-14-bit 16383)

(def default-ppq 960)

(defn- fail! [message data] (throw (ex-info message data)))

(defn- finite? [x] (and (number? x) (Double/isFinite (double x))))

;; ---------------------------------------------------------------------------
;; Parameter components

(defn components
  "Scalar components of a parameter schema, or nil when it has no reversible
  controller representation."
  [{:keys [type bounds recommended values options size] :as schema}]
  (let [range (or bounds recommended)
        bounded (when (and (vector? range) (= 2 (count range)) (every? finite? range) (< (first range) (second range)))
                  range)]
    (case type
      (:number :integer) (when bounded [{:type type :bounds bounded}])
      :boolean [{:type :boolean}]
      :enum (let [choices (or values options)]
              (when (and (vector? choices) (<= 1 (count choices) (inc max-14-bit)))
                [{:type :enum :values choices}]))
      :color (vec (repeat 3 {:type :number :bounds (or bounded [0 1])}))
      :vector (when (and bounded (pos-int? size) (<= size 8)) (vec (repeat size {:type :number :bounds bounded})))
      :range (when bounded (vec (repeat 2 {:type :number :bounds bounded})))
      nil)))

(defn- component-fraction [{:keys [type bounds values]} value]
  (case type
    :boolean (when (boolean? value) (if value 1 0))
    :enum (let [i (.indexOf ^java.util.List values value)]
            (when-not (neg? i) (if (= 1 (count values)) 0 (/ i (dec (count values))))))
    ;; A value outside the encoded range (possible when only :recommended
    ;; bounds exist) does not fit the lane; clamping it would replay another value.
    (let [[low high] bounds]
      (when (and (finite? value) (<= low value high)) (/ (- (double value) low) (- high low))))))

(defn- fraction->component [{:keys [type bounds values]} v14]
  (let [f (/ (double v14) max-14-bit)]
    (case type
      :boolean (>= v14 8192)
      :enum (nth values (Math/round (* f (dec (count values)))))
      :integer (let [[low high] bounds] (long (Math/round (+ low (* f (- high low))))))
      ;; low + f(high - low) can round one ulp past high, which the value's
      ;; own bounds would then refuse on replay.
      (let [[low high] bounds] (min high (max low (+ low (* f (- high low)))))))))

(defn encode-value
  "Encode a parameter value for a lane as a vector of 14-bit integers, one per
  component, or nil when the value does not fit the lane."
  [lane value]
  (let [comps (:components lane)
        parts (if (= 1 (count comps)) [value] value)]
    (when (and (sequential? parts) (= (count parts) (count comps)))
      (let [fractions (map component-fraction comps parts)]
        (when (every? some? fractions)
          (mapv #(long (Math/round (* (double %) max-14-bit))) fractions))))))

(defn decode-value
  "Inverse of encode-value."
  [lane v14s]
  (let [comps (:components lane)
        parts (mapv fraction->component comps v14s)]
    (if (= 1 (count comps)) (first parts) parts)))

;; ---------------------------------------------------------------------------
;; Layout

(defn- param-schema [registry cue-id param]
  (get-in (registry/cue-effect registry (get-in registry [:cues cue-id])) [:params param]))

(defn allocate-layout
  "Extend layout with addresses for the given cues and [cue-id param] pairs.
  Existing allocations are kept, so clips recorded earlier still address the
  same targets. :pad-notes {cue-id note} prefers the Drum Rack pitch a cue
  already plays on. Unrepresentable or overflowing parameters are listed
  under :unassigned with a reason.
  Options: :channel (cue notes, default 1), :lane-channel (controllers,
  default the cue channel)."
  [layout registry {:keys [cues params pad-notes channel lane-channel]}]
  (let [layout (merge {:layout/version layout-version :channel (or channel 1) :cues {} :lanes []} layout)
        layout (cond-> layout lane-channel (assoc :lane-channel lane-channel))
        _ (when-not (and (<= 1 (:channel layout) 16) (<= 1 (get layout :lane-channel (:channel layout)) 16))
            (fail! "MIDI channels are 1-16" (select-keys layout [:channel :lane-channel])))
        used-pitches (set (map :pitch (vals (:cues layout))))
        candidates (concat (range 36 100) (range 100 128) (range 0 36))
        cue-layout (reduce (fn [{:keys [cues used] :as acc} cue-id]
                             (if (contains? cues cue-id) acc
                                 (let [preferred (get pad-notes cue-id)
                                       pitch (first (remove used (cond->> candidates preferred (cons preferred))))]
                                   (if pitch
                                     {:cues (assoc cues cue-id {:pitch pitch}) :used (conj used pitch)}
                                     (update acc :unassigned conj {:cue-id cue-id :reason :no-free-pitch})))))
                           {:cues (:cues layout) :used used-pitches :unassigned []}
                           (distinct (concat cues (map first params))))
        used-cc (set (mapcat #(map :cc (:components %)) (:lanes layout)))
        existing (set (map (juxt :cue-id :param) (:lanes layout)))
        lane-layout (reduce (fn [{:keys [lanes used unassigned] :as acc} [cue-id param]]
                              (if (contains? existing [cue-id param]) acc
                                  (let [comps (components (param-schema registry cue-id param))
                                        free (remove used coarse-controllers)]
                                    (cond
                                      (nil? comps) (update acc :unassigned conj {:cue-id cue-id :param param :reason :no-reversible-encoding})
                                      (< (count free) (count comps)) (update acc :unassigned conj {:cue-id cue-id :param param :reason :no-free-controller})
                                      :else (let [ccs (take (count comps) free)]
                                              {:lanes (conj lanes {:cue-id cue-id :param param
                                                                   :components (mapv #(assoc %1 :cc %2) comps ccs)})
                                               :used (into used ccs) :unassigned unassigned})))))
                            {:lanes (:lanes layout) :used used-cc :unassigned (:unassigned cue-layout)}
                            (distinct params))]
    (assoc layout :cues (:cues cue-layout) :lanes (:lanes lane-layout)
                  :unassigned (vec (distinct (:unassigned lane-layout))))))

(defn phrase-targets
  "Cue IDs and [cue-id param] pairs a phrase touches, for allocate-layout."
  [phrase]
  {:cues (vec (distinct (map :cue-id (:notes phrase))))
   :params (vec (distinct (concat (for [{:keys [cue-id params]} (:notes phrase), param (sort-by str (keys params))]
                                    [cue-id param])
                                  (map (juxt :cue-id :param) (:lanes phrase)))))})

(defn lane-channel [layout] (get layout :lane-channel (:channel layout)))

(defn channels [layout] (vec (distinct [(:channel layout) (lane-channel layout)])))

(defn- lane-for [layout cue-id param]
  (first (filter #(= [cue-id param] ((juxt :cue-id :param) %)) (:lanes layout))))

(defn- controller-messages [lane v14s]
  (mapcat (fn [{:keys [cc]} v]
            [{:controller cc :value (bit-shift-right v 7)}
             {:controller (+ cc 32) :value (bit-and v 127)}])
          (:components lane) v14s))

;; ---------------------------------------------------------------------------
;; Phrase <-> clip

(defn phrase->clips
  "Render a phrase as one clip description per MIDI channel in the layout:
    {channel {:clip/version 1 :length-beats L :meter m
              :notes [{:pitch p :start b :duration d :velocity 127}]
              :controllers [{:beat b :controller cc :value 0-127}]}}
  Launch values are written at their note's start; lane points at their
  beats. Unassigned targets are omitted and returned under :skipped in meta."
  [phrase layout]
  (capture/validate-phrase phrase)
  (let [skipped (volatile! [])
        notes (vec (for [{:keys [cue-id start length]} (:notes phrase)
                         :let [pitch (get-in layout [:cues cue-id :pitch])]
                         :when (or pitch (do (vswap! skipped conj {:cue-id cue-id}) false))]
                     {:pitch pitch :start start :duration length :velocity 127}))
        values (concat
                (for [{:keys [cue-id start params]} (:notes phrase)
                      [param value] (sort-by (comp str key) params)]
                  {:beat start :cue-id cue-id :param param :value value})
                (for [{:keys [cue-id param points]} (:lanes phrase), {:keys [beat value]} points]
                  {:beat beat :cue-id cue-id :param param :value value}))
        controllers (vec (for [[order {:keys [beat cue-id param value]}] (map-indexed vector values)
                               :let [lane (lane-for layout cue-id param)
                                     v14s (when lane (encode-value lane value))]
                               :when (or v14s (do (vswap! skipped conj {:cue-id cue-id :param param :beat beat}) false))
                               [i message] (map-indexed vector (controller-messages lane v14s))]
                           (assoc message :beat beat ::order [order i])))
        ;; One value per controller and beat: the later write wins.
        controllers (->> controllers
                         (reduce (fn [m c] (assoc m [(:beat c) (:controller c)] c)) {})
                         vals (sort-by (juxt :beat ::order)) (mapv #(dissoc % ::order)))
        base {:clip/version clip-version :length-beats (:length-beats phrase) :meter (:meter phrase)}
        note-ch (:channel layout) cc-ch (lane-channel layout)]
    (with-meta
      (if (= note-ch cc-ch)
        {note-ch (assoc base :notes notes :controllers controllers)}
        {note-ch (assoc base :notes notes :controllers [])
         cc-ch (assoc base :notes [] :controllers controllers)})
      {:skipped (vec (distinct @skipped))})))

(defn- by-cc [layout]
  (into {} (for [lane (:lanes layout), [i {:keys [cc]}] (map-indexed vector (:components lane))]
             [cc [lane i]])))

(defn clips->phrase
  "Decode clip descriptions (as from phrase->clips, Live or an SMF) back into a
  phrase. Controller values at a note's start become that activation's launch
  values; later values become lane points on the latest activation of the cue
  that is running at that beat. Values with no running activation are returned
  under :orphans in meta. Coarse-only edits keep the last fine value."
  [clips layout {:keys [meter grid-beats source]}]
  (let [clips (vals clips)
        length (apply max (map :length-beats clips))
        meter (or meter (some :meter clips) {:beats-per-bar 4 :bars-per-phrase 4})
        pitch->cue (into {} (map (fn [[cue {:keys [pitch]}]] [pitch cue])) (:cues layout))
        notes (->> (mapcat :notes clips)
                   (keep (fn [{:keys [pitch start duration]}]
                           (when-let [cue-id (pitch->cue pitch)]
                             {:cue-id cue-id :start (rationalize start) :length (rationalize duration)})))
                   (filter #(and (< (:start %) length) (pos? (:length %))))
                   (map #(update % :length min (- length (:start %))))
                   (sort-by (juxt :start (comp str :cue-id) :length))
                   (map-indexed (fn [i n] (assoc n :id (str "n" i) :params {})))
                   vec)
        index (by-cc layout)
        ;; Fold controller writes beat by beat into 14-bit registers.
        groups (->> (mapcat :controllers clips)
                    (filter #(< (rationalize (:beat %)) length))
                    (group-by #(rationalize (:beat %))) (sort-by key))
        decoded (reduce
                 (fn [{:keys [registers out]} [beat events]]
                   (let [registers (reduce (fn [r {:keys [controller value]}]
                                             (cond
                                               (contains? index controller) (assoc-in r [controller :msb] value)
                                               (contains? index (- controller 32)) (assoc-in r [(- controller 32) :lsb] value)
                                               :else r))
                                           registers events)
                         touched (distinct (keep (fn [{:keys [controller]}]
                                                   (first (or (index controller) (index (- controller 32)))))
                                                 events))]
                     {:registers registers
                      :out (into out (keep (fn [lane]
                                             (let [regs (map #(get registers (:cc %)) (:components lane))]
                                               (when (every? :msb regs)
                                                 {:beat beat :lane lane
                                                  :value (decode-value lane (mapv #(+ (* 128 (:msb %)) (or (:lsb %) 0)) regs))})))
                                           touched))}))
                 {:registers {} :out []} groups)
        attach (fn [{:keys [beat lane]}]
                 (let [candidates (filter #(and (= (:cue-id lane) (:cue-id %)) (<= (:start %) beat)
                                                (< beat (+ (:start %) (:length %))))
                                          notes)]
                   (last (sort-by (juxt :start :id) candidates))))
        result (reduce (fn [{:keys [notes points orphans]} {:keys [beat lane value] :as v}]
                         (if-let [note (attach v)]
                           (if (= beat (:start note))
                             {:notes (assoc-in notes [(Long/parseLong (subs (:id note) 1)) :params (:param lane)] value)
                              :points points :orphans orphans}
                             {:notes notes :orphans orphans
                              :points (conj points {:cue-id (:cue-id lane) :param (:param lane)
                                                    :beat beat :value value :note (:id note)})})
                           {:notes notes :points points
                            :orphans (conj orphans {:cue-id (:cue-id lane) :param (:param lane) :beat beat :value value})}))
                       {:notes notes :points [] :orphans []}
                       (:out decoded))
        lanes (->> (:points result)
                   (group-by (juxt :cue-id :param))
                   (sort-by (comp (partial mapv str) key))
                   (mapv (fn [[[cue-id param] pts]]
                           {:cue-id cue-id :param param
                            :points (mapv #(select-keys % [:beat :value :note]) (sort-by (juxt :beat :note) pts))})))]
    (with-meta
      (capture/validate-phrase
       (cond-> {:phrase/version capture/phrase-version :length-beats length :meter meter
                :notes (mapv #(assoc % :carried-in? false :held-at-end? false) (:notes result))
                :lanes lanes}
         grid-beats (assoc :grid-beats grid-beats)
         source (assoc :source source)))
      {:orphans (:orphans result)})))

(defn comparable
  "The parts of a phrase that playback reproduces: note timing and values.
  Capture provenance (:source, carried/held flags) is dropped."
  [phrase]
  {:length-beats (:length-beats phrase)
   :notes (mapv #(select-keys % [:id :cue-id :start :length :params]) (:notes phrase))
   :lanes (:lanes phrase)})

;; ---------------------------------------------------------------------------
;; Standard MIDI File

(defn- time-signature [beats-per-bar]
  (let [bpb (rationalize beats-per-bar)
        denominator (first (filter #(integer? (* bpb (/ % 4))) [4 8 16 32]))]
    (when-not denominator (fail! "Meter has no MIDI time signature" {:beats-per-bar beats-per-bar}))
    [(long (* bpb (/ denominator 4))) denominator]))

(defn- tick [ppq beat]
  (let [t (* (rationalize beat) ppq)]
    (when-not (integer? t) (fail! "Beat does not fall on a MIDI tick" {:beat beat :ppq ppq}))
    (long t)))

(defn clip->smf
  "Write one clip description as a type-1 Standard MIDI File on channel.
  Ordering within a tick is note-offs, controllers, then note-ons."
  ([clip channel] (clip->smf clip channel {}))
  ([clip channel {:keys [ppq bpm name] :or {ppq default-ppq bpm 120}}]
   (let [sequence (Sequence. Sequence/PPQ (int ppq))
         track (.createTrack sequence)
         ch (dec channel)
         [num den] (time-signature (get-in clip [:meter :beats-per-bar] 4))
         mpq (long (Math/round (/ 6.0e7 (double bpm))))
         meta! (fn [type ^bytes data t] (.add track (MidiEvent. (MetaMessage. (int type) data (alength data)) (long t))))
         short! (fn [command d1 d2 t] (.add track (MidiEvent. (ShortMessage. (int command) (int ch) (int d1) (int d2)) (long t))))
         events (concat
                 (for [{:keys [pitch start duration]} (:notes clip)]
                   [(tick ppq (+ start duration)) 0 ShortMessage/NOTE_OFF pitch 0])
                 (for [{:keys [beat controller value]} (:controllers clip)]
                   [(tick ppq beat) 1 ShortMessage/CONTROL_CHANGE controller value])
                 (for [{:keys [pitch start velocity]} (:notes clip)]
                   [(tick ppq start) 2 ShortMessage/NOTE_ON pitch (or velocity 127)]))]
     (meta! 0x03 (.getBytes (str (or name "Tolglow phrase")) "UTF-8") 0)
     (meta! 0x51 (byte-array (map unchecked-byte [(bit-shift-right mpq 16) (bit-shift-right mpq 8) mpq])) 0)
     (meta! 0x58 (byte-array (map unchecked-byte [num (Long/numberOfTrailingZeros den) 24 8])) 0)
     (doseq [[t _ command d1 d2] (sort-by (juxt first second) events)]
       (short! command d1 d2 t))
     (meta! 0x2F (byte-array 0) (tick ppq (:length-beats clip)))
     (let [out (ByteArrayOutputStream.)]
       (MidiSystem/write sequence 1 out)
       (.toByteArray out)))))

(defn smf->clip
  "Read a Standard MIDI File into a clip description. Channel selects events
  (nil reads every channel). Notes pair with the earliest open note of the same
  pitch. Length is :length-beats when supplied, else the end of track rounded
  up to a whole bar."
  ([source] (smf->clip source {}))
  ([source {:keys [channel length-beats meter]}]
   (let [bytes (if (bytes? source) source (fail! "Expected SMF bytes" {}))
         sequence (MidiSystem/getSequence (ByteArrayInputStream. bytes))
         _ (when-not (== Sequence/PPQ (.getDivisionType sequence)) (fail! "Clips use musical ticks" {}))
         ppq (.getResolution sequence)
         beat #(/ % ppq)
         events (sort-by first (for [^Track track (.getTracks sequence), i (range (.size track))
                                     :let [e (.get track i)]]
                                 [(.getTick e) i (.getMessage e)]))
         sig (some (fn [[_ _ m]] (when (and (instance? MetaMessage m) (= 0x58 (.getType ^MetaMessage m)))
                                   (let [d (.getData ^MetaMessage m)]
                                     (* (aget d 0) (/ 4 (bit-shift-left 1 (aget d 1)))))))
                   events)
         meter (or meter {:beats-per-bar (or sig 4) :bars-per-phrase 4})
         result (reduce (fn [{:keys [open notes controllers] :as acc} [t _ m]]
                          (if-not (instance? ShortMessage m) acc
                            (let [m ^ShortMessage m
                                  command (.getCommand m) d1 (.getData1 m) d2 (.getData2 m)]
                              (cond
                                (and channel (not= (dec channel) (.getChannel m))) acc
                                (and (= command ShortMessage/NOTE_ON) (pos? d2))
                                (update-in acc [:open d1] (fnil conj []) [t d2])
                                (#{ShortMessage/NOTE_ON ShortMessage/NOTE_OFF} command)
                                (if-let [[start velocity] (first (get open d1))]
                                  {:open (update open d1 subvec 1) :controllers controllers
                                   :notes (conj notes {:pitch d1 :start (beat start) :duration (beat (- t start)) :velocity velocity})}
                                  acc)
                                (= command ShortMessage/CONTROL_CHANGE)
                                (update acc :controllers conj {:beat (beat t) :controller d1 :value d2})
                                :else acc))))
                        {:open {} :notes [] :controllers []} events)
         end (beat (.getTickLength sequence))
         bpb (rationalize (:beats-per-bar meter))
         length (or length-beats (* bpb (max 1 (long (Math/ceil (/ (double end) (double bpb)))))))]
     {:clip/version clip-version :length-beats length :meter meter
      :notes (vec (sort-by (juxt :start :pitch) (filter #(pos? (:duration %)) (:notes result))))
      :controllers (:controllers result)})))

;; ---------------------------------------------------------------------------
;; Continuous feed: what Tolglow sends a monitored Live track for Capture MIDI

(defn feed-messages
  "MIDI messages for one captured observation, in the layout's encoding, for
  the continuous feed to a monitored Live track. kind is :launch, :end or
  :param (see phrase-capture events). Unassigned targets return []."
  [layout {:keys [kind cue-id param value params]}]
  (let [pitch (get-in layout [:cues cue-id :pitch])
        cc (fn [param value]
             (when-let [lane (lane-for layout cue-id param)]
               (when-let [v14s (encode-value lane value)]
                 (map #(assoc % :type :cc :channel (lane-channel layout)) (controller-messages lane v14s)))))]
    (vec (case kind
           :launch (when pitch
                     (concat (mapcat (fn [[p v]] (cc p v)) (sort-by (comp str key) params))
                             [{:type :note-on :note pitch :velocity 127 :channel (:channel layout)}]))
           :end (when pitch [{:type :note-off :note pitch :velocity 0 :channel (:channel layout)}])
           :param (cc param value)
           nil))))

;; ---------------------------------------------------------------------------
;; Playback decoder

(defn player
  "A decoder for MIDI played back from Live clips in this layout. source is
  merged into each command's :source; origin is always :live-playback."
  ([layout] (player layout {}))
  ([layout source]
   {:layout layout :by-cc (by-cc layout)
    :pitch->cue (into {} (map (fn [[cue {:keys [pitch]}]] [pitch cue])) (:cues layout))
    :source (merge {:type :ableton :adapter :live-clip} source {:origin :live-playback})
    :registers {} :lane-values {} :runs [] :serial 0}))

(defn- command [player envelope op data]
  (merge envelope {:op op :source (:source player)} data))

(defn- latest-run [player cue-id]
  (last (filter #(and (= cue-id (:cue-id %)) (not (:ended? %))) (:runs player))))

(defn- update-run [player id f]
  (update player :runs (fn [runs] (mapv #(if (= id (:id %)) (f %) %) runs))))

(defn play
  "Decode the MIDI events Live played at one beat. envelope-fn returns the
  shared command envelope (:command-id :show-id :epoch :time-ms) for the n-th
  command of this batch. Within one beat the order is note-offs, controllers,
  then note-ons, matching the clip renderer. Returns {:player :commands}.
  Launches stay pending until `acknowledge`; values and releases addressed to
  a pending run are applied when its receipt arrives."
  [player events envelope-fn]
  (let [chs (set (channels (:layout player)))
        events (filter #(contains? chs (:channel %)) events)
        rank #(case (:type %) :note-off 0 :cc 1 :note-on (if (pos? (or (:velocity %) 0)) 2 0) 3)
        n (volatile! 0)
        env #(envelope-fn (dec (vswap! n inc)))]
    (->
     (reduce
     (fn [{:keys [player commands] :as acc} {:keys [type note controller value] :as event}]
       (case (long (rank event))
         0 (if-let [run (some #(when (and (= note (:pitch %)) (not (:ended? %))) %) (:runs player))]
             (if-let [id (:instance-id run)]
               {:player (update player :runs (fn [runs] (vec (remove #(= (:id run) (:id %)) runs))))
                :commands (conj commands (command player (env) :instance/end {:instance-id id}))}
               {:player (update-run player (:id run) #(assoc % :ended? true)) :commands commands})
             acc)
         1 (let [index (:by-cc player)
                 [cc part] (cond (contains? index controller) [controller :msb]
                                 (contains? index (- controller 32)) [(- controller 32) :lsb])]
             (if-not cc acc
               (let [[lane] (index cc)
                     player (assoc-in player [:registers cc part] value)
                     regs (map #(get-in player [:registers (:cc %)]) (:components lane))]
                 (if-not (every? :msb regs) {:player player :commands commands}
                   (let [v (decode-value lane (mapv #(+ (* 128 (:msb %)) (or (:lsb %) 0)) regs))
                         player (-> player (assoc-in [:lane-values [(:cue-id lane) (:param lane)]] v)
                                    (assoc-in [:fresh [(:cue-id lane) (:param lane)]] v))
                         run (latest-run player (:cue-id lane))]
                     (cond
                       (nil? run) {:player player :commands commands}
                       (:instance-id run)
                       (let [cmd (command player (env) :param/set {:instance-id (:instance-id run) :param (:param lane) :value v})]
                         ;; Collapse coarse+fine writes of one value at one beat into the last.
                         {:player player
                          :commands (if (and (seq commands) (= ((juxt :op :instance-id :param) (peek commands))
                                                               ((juxt :op :instance-id :param) cmd)))
                                      (conj (pop commands) (assoc cmd :command-id (:command-id (peek commands))))
                                      (conj commands cmd))})
                       :else {:player (update-run player (:id run) #(assoc-in % [:updates (:param lane)] v))
                              :commands commands}))))))
         2 (if-let [cue-id (get-in player [:pitch->cue note])]
             ;; Only values written at this beat belong to the launch; older
             ;; lane values addressed earlier activations.
             (let [params (into {} (for [[[c p] v] (:fresh player) :when (= c cue-id)] [p v]))
                   cmd (command player (env) :cue/launch {:cue-id cue-id :launch {:mode :persistent :params params}})
                   id (:serial player)]
               {:player (-> player (update :serial inc)
                            (update :runs conj {:id id :pitch note :cue-id cue-id :command-id (:command-id cmd)}))
                :commands (conj commands cmd)})
             acc)
         acc))
     {:player (dissoc player :fresh) :commands []}
     (sort-by rank events))
    (update :player dissoc :fresh))))

(defn acknowledge
  "Bind a launch receipt to its run. Returns {:player :commands}, the commands
  being deferred values and a deferred release for that run."
  [player {:keys [command-id status instance-id]} envelope-fn]
  (if-let [run (some #(when (= command-id (:command-id %)) %) (:runs player))]
    (if (and (= :accepted status) instance-id)
      (let [n (volatile! 0)
            env #(envelope-fn (dec (vswap! n inc)))
            updates (for [[param v] (sort-by (comp str key) (:updates run))]
                      (command player (env) :param/set {:instance-id instance-id :param param :value v}))
            end (when (:ended? run) [(command player (env) :instance/end {:instance-id instance-id})])]
        {:player (if (:ended? run)
                   (update player :runs (fn [runs] (vec (remove #(= (:id run) (:id %)) runs))))
                   (update-run player (:id run) #(-> % (assoc :instance-id instance-id) (dissoc :updates))))
         :commands (vec (concat updates end))})
      {:player (update player :runs (fn [runs] (vec (remove #(= (:id run) (:id %)) runs)))) :commands []})
    {:player player :commands []}))

(defn release-all
  "End every bound run, for clip stop, transport stop or handoff to Live."
  [player envelope-fn]
  (let [n (volatile! 0)
        env #(envelope-fn (dec (vswap! n inc)))]
    {:player (assoc player :runs [] :registers {} :lane-values {})
     :commands (vec (for [{:keys [instance-id]} (:runs player) :when instance-id]
                      (command player (env) :instance/end {:instance-id instance-id})))}))

(defn describe-layout
  "One line per address, for the REPL and setup diagnostics."
  [layout]
  (str/join "\n" (concat (for [[cue {:keys [pitch]}] (sort-by (comp :pitch val) (:cues layout))]
                           (format "ch %d note %3d  %s" (:channel layout) pitch cue))
                         (for [{:keys [cue-id param components]} (:lanes layout)
                               [i {:keys [cc]}] (map-indexed vector components)]
                           (format "ch %d cc %2d/%2d %s %s%s" (lane-channel layout) cc (+ cc 32) cue-id param
                                   (if (> (count components) 1) (str "[" i "]") ""))))))
