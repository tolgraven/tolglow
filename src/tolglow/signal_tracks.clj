(ns tolglow.signal-tracks
  "Signal tracks (#885): the values audio features, stems, sensors and
  controllers delivered during a performance, recorded beside it so the
  performance can be replayed on another rig or at another tempo.

  A recording folds committed show models (`observe`), never a source:
  - routing ports (`[:routing :ports]`): audio features, stem levels and
    onsets, MIDI, automation and control observations, one row whenever a
    port's capture time changed;
  - audio events the frame consumed (`[:variables :audio :events]`), one
    row per event, keeping the event's own musical beat;
  - parameter writes (`:param/set`) from human controller surfaces
    (`tolglow.event-subscribers/human-source?`), one row per accepted write,
    keeping the instance it addressed.

  Every row is `[beat time-ms value confidence & extra]`: the transport beat
  of the frame that resolved the value, the capture (show) time in
  milliseconds, the value and its confidence. The tempo segments the
  recording was made under turn beats back into time (`beat->time-ms`).
  A track is bounded: rows past `:max-rows` are counted under `:dropped`
  and the track is marked `:truncated? true`.

  `finish` gives a bundle: the recording names each track by its SHA-256
  content hash and the tracks are kept beside it under that hash. Bundles
  are stored in the show model (`apply-operation`, message type
  `:signal-tracks`), so a full backup keeps them; `export-package` is the
  explicit sharing selection. Everything here is pure; the owned writer
  thread is `tolglow.signal-tracks.recorder`. See docs/signal-tracks.md."
  (:require [afterglow.time :as time]
            [tolglow.event-subscribers :as subscribers])
  (:import (java.nio.charset StandardCharsets)
           (java.security MessageDigest)
           (java.util HexFormat)))

(def version 1)

(def default-max-rows 4096)

(def max-max-rows 65536)

(def max-tracks
  "Tracks one recording may hold; sources past it are counted, not kept."
  256)

(def max-tempo-segments 1024)

(def max-stored-recordings 64)

(def max-stored-tracks 1024)

(def state-key
  "Saved under the show's :extensions, so full backups and show saves keep it."
  ::store)

(def base-columns [:beat :time-ms :value :confidence])

(def columns
  "Row columns by track source."
  {:port base-columns
   :event (conj base-columns :event-beat)
   :control (conj base-columns :instance-id)})

(def ^:private epsilon 1e-6)

(defn- fail! [message data] (throw (ex-info message (assoc data :signal-tracks/error :invalid))))

(defn- finite? [x] (time/finite-number? x))

(defn- id? [x] (or (keyword? x) (and (string? x) (<= 1 (count x) 256))))

(defn- id-name
  "The readable name of an ID: a keyword's namespace and name, or a string."
  [x]
  (cond (keyword? x) (subs (str x) 1)
        (some? x) (str x)
        :else "unknown"))

;; ---------------------------------------------------------------------------
;; Content hashes

(defn- canonical
  "Maps become key-ordered maps so equal data always prints the same."
  [x]
  (cond (map? x) (into (sorted-map-by #(compare (pr-str %1) (pr-str %2)))
                       (map (fn [[k v]] [k (canonical v)])) x)
        (vector? x) (mapv canonical x)
        (set? x) (into (sorted-set-by #(compare (pr-str %1) (pr-str %2))) (map canonical) x)
        (sequential? x) (mapv canonical x)
        :else x))

(defn content-hash
  "The SHA-256 of a track's canonical EDN, as \"sha256:<hex>\"."
  [track]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes ^String (pr-str (canonical track)) StandardCharsets/UTF_8))]
    (str "sha256:" (.formatHex (HexFormat/of) digest))))

;; ---------------------------------------------------------------------------
;; Recording

(defn new-recording
  "An empty recording. Options:
    :id           recording ID (string, required)
    :attached-to  what the tracks belong to: {:kind :rehearsal-take ...},
                  {:kind :run-take :take-id ...} or {:kind :command-recording ...}
    :max-rows     bound on rows kept per track (default 4096)."
  [{:keys [id attached-to max-rows] :or {max-rows default-max-rows}}]
  (when-not (and (string? id) (re-matches #"[A-Za-z0-9:._-]{1,128}" id))
    (fail! "A recording ID is 1-128 letters, digits or :._-" {:id id}))
  (when-not (and (map? attached-to) (#{:rehearsal-take :run-take :command-recording} (:kind attached-to)))
    (fail! "A recording is attached to a :rehearsal-take, :run-take or :command-recording" {:attached-to attached-to}))
  (when-not (and (pos-int? max-rows) (<= max-rows max-max-rows))
    (fail! "max-rows must be a positive integer up to 65536" {:max-rows max-rows}))
  {:signal-tracks/version version :id id :attached-to attached-to :max-rows max-rows
   :show nil :tempo [] :tempo-dropped 0 :frames 0 :frames-dropped 0
   :tracks {} :track-order [] :sources-dropped 0
   ::last nil})

(defn- history-tail
  "Accepted command entries after last-id in the bounded history order."
  [model last-id]
  (let [order (get-in model [:control :history-order] [])
        from (if last-id (inc (.lastIndexOf ^java.util.List order last-id)) 0)]
    (subvec order (max 0 (min from (count order))))))

(defn- baseline
  "What the next fold compares against: each port's capture time, the
  audio event count and the newest command, without making rows."
  [model]
  {:epoch (:epoch model)
   :ports (into {} (map (fn [[id port]] [id (:observed-at-ms port)])) (get-in model [:routing :ports]))
   :event-count (get-in model [:variables :audio :event-count] 0)
   :history-id (peek (get-in model [:control :history-order] []))})

(defn start
  "Begin recording from model, the committed model before the first frame
  to record. Inputs it already holds make no rows."
  [recording model]
  (assoc recording ::last (baseline model)
         :show (select-keys model [:show-id :epoch])))

(defn- append-row [track max-rows row]
  (if (< (count (:rows track)) max-rows)
    (update track :rows conj row)
    (-> track (update :dropped inc) (assoc :truncated? true))))

(defn- add-row
  "Append row to track id, creating it from meta. A source past max-tracks
  keeps no track; its rows are counted under :sources-dropped."
  [recording id meta row]
  (cond
    (contains? (:tracks recording) id)
    (update-in recording [:tracks id] append-row (:max-rows recording) row)
    (>= (count (:tracks recording)) max-tracks)
    (update recording :sources-dropped inc)
    :else
    (-> recording
        (update :track-order conj id)
        (assoc-in [:tracks id] (append-row (assoc meta :id id :rows [] :dropped 0 :truncated? false)
                                           (:max-rows recording) row)))))

(defn- port-unit [port]
  (or (get-in port [:port :unit]) (if (= :midi (:kind port)) :midi-value :normalized)))

(defn- port-rows [recording model beat last-ports]
  (reduce (fn [recording [id {:keys [observed-at-ms value confidence kind range] :as port}]]
            (if (or (= observed-at-ms (get last-ports id)) (not (finite? value)))
              recording
              (add-row recording (id-name id)
                       {:source :port :source-id id :kind kind :unit (port-unit port) :range range
                        :address (dissoc (:port port) :onset-count :run-id)}
                       [beat observed-at-ms (double value) (double (or confidence 1.0))])))
          recording
          (sort-by (comp str key) (get-in model [:routing :ports]))))

(defn- event-template
  "What stays the same across one source's events. :signal-tracks/valued?
  says the event carried a :value, which the row keeps."
  [event]
  (cond-> (dissoc event :time-ms :beat :confidence :value)
    (contains? event :value) (assoc :signal-tracks/valued? true)))

(defn- event-rows [recording model beat last-count]
  (let [{:keys [events event-count]} (get-in model [:variables :audio])
        fresh (- (or event-count 0) (or last-count 0))
        events (vec events)
        fresh-events (if (pos? fresh) (subvec events (max 0 (- (count events) fresh))) [])]
    (reduce (fn [recording event]
              (let [template (event-template event)
                    base (str "event/" (id-name (get-in event [:source :id] :analysis)) "/" (name (:type event :event)))
                    existing (get-in recording [:tracks base :template])
                    id (if (or (nil? existing) (= existing template)) base (str base "#" (subs (content-hash template) 7 15)))]
                (add-row recording id
                         {:source :event :kind :audio-event :unit :event :range [0.0 1.0] :template template}
                         [beat (double (:time-ms event (:time-ms model 0.0)))
                          (double (if (finite? (:value event)) (:value event) 1.0))
                          (double (if (finite? (:confidence event)) (:confidence event) 1.0))
                          (double (if (finite? (:beat event)) (:beat event) beat))])))
            recording fresh-events)))

(defn- control-rows [recording model beat last-id]
  (reduce (fn [recording id]
            (let [{:keys [command ack]} (get-in model [:control :history id])
                  {:keys [op source param value instance-id]} command
                  instance-id (or instance-id (:instance-id ack))
                  instance (get-in model [:instances instance-id])
                  cue-id (or (:cue-id instance) (:cue-id command))]
              (if-not (and (= :param/set op) (= :accepted (:status ack)) (subscribers/human-source? source)
                           (finite? value) (some? instance-id) (id? param))
                recording
                (let [schema (get-in instance [:definition :params param])
                      source (select-keys source [:adapter :session-id])]
                  (add-row recording (str "control/" (id-name (:adapter source)) "/" (id-name cue-id) "/" (id-name param)
                                          (when-let [session (:session-id source)] (str "@" session)))
                           {:source :control :kind :control :unit (or (:unit schema) :parameter)
                            :range (or (:bounds schema) [0.0 1.0])
                            :address {:adapter (:adapter source) :session-id (:session-id source) :cue-id cue-id :param param}}
                           [beat (double (:time-ms model 0.0)) (double value) 1.0 instance-id])))))
          recording (history-tail model last-id)))

(defn- tempo-segment [model]
  (let [{:keys [beat bpm playing?]} (:transport model)]
    {:beat (double (or beat 0.0)) :time-ms (double (:time-ms model 0.0)) :bpm (double (or bpm 120.0))
     :playing? (not (false? playing?))}))

(defn- continues? [segment next]
  (and (= (:bpm segment) (:bpm next)) (= (:playing? segment) (:playing? next))
       (let [expected (if (:playing? segment)
                        (+ (:beat segment) (/ (* (- (:time-ms next) (:time-ms segment)) (:bpm segment)) 60000.0))
                        (:beat segment))]
         (< (Math/abs (double (- expected (:beat next)))) epsilon))))

(defn- note-tempo [recording model]
  (let [segment (tempo-segment model)
        last-segment (peek (:tempo recording))]
    (cond (and last-segment (continues? last-segment segment)) recording
          (>= (count (:tempo recording)) max-tempo-segments) (update recording :tempo-dropped inc)
          :else (update recording :tempo conj segment))))

(defn observe
  "Fold one committed model into the recording. Models must arrive in commit
  order; a model from another show epoch starts a new baseline. Reads only
  what the frame already resolved."
  [recording model]
  (let [last (::last recording)]
    (cond
      (nil? last) (start recording model)
      (not= (:epoch last) (:epoch model)) (assoc (note-tempo recording model) ::last (baseline model))
      :else
      (let [beat (double (get-in model [:transport :beat] 0.0))]
        (-> recording
            (note-tempo model)
            (update :frames inc)
            (event-rows model beat (:event-count last))
            (port-rows model beat (:ports last))
            (control-rows model beat (:history-id last))
            (assoc ::last (baseline model)))))))

(defn finish
  "The finished bundle: {:recording r :tracks {hash track}}. The recording
  names each track by content hash under :tracks. frames-dropped counts
  committed frames the writer never saw; any dropped frame, source or row
  marks the recording :truncated? true."
  ([recording] (finish recording 0))
  ([recording frames-dropped]
   (let [tracks (mapv #(get-in recording [:tracks %]) (:track-order recording))
         hashed (mapv (juxt content-hash identity) tracks)
         frames-dropped (+ (:frames-dropped recording 0) frames-dropped)]
     {:recording (-> recording
                     (dissoc ::last :track-order :max-rows)
                     (assoc :frames-dropped frames-dropped
                            :truncated? (boolean (or (pos? frames-dropped) (pos? (:sources-dropped recording))
                                                     (pos? (:tempo-dropped recording)) (some :truncated? tracks)))
                            :tracks (into (sorted-map) (map (fn [[hash track]] [(:id track) hash])) hashed)))
      :tracks (into (sorted-map) hashed)})))

(defn track
  "The track of a bundle with ID id."
  [{:keys [recording tracks]} id]
  (get tracks (get-in recording [:tracks id])))

(defn row-map
  "A row as a map keyed by its track's columns."
  [track row]
  (zipmap (get columns (:source track) base-columns) row))

;; ---------------------------------------------------------------------------
;; Time and summaries

(defn beat->time-ms
  "The show time at beat under the recording's tempo segments: the last
  playing segment at or before beat, extended at its tempo. A beat inside a
  stop maps to the time the stop began."
  [{:keys [tempo]} beat]
  (when-let [segment (last (filter #(<= (:beat %) (+ beat epsilon)) tempo))]
    (if (:playing? segment)
      (+ (:time-ms segment) (/ (* (- beat (:beat segment)) 60000.0) (:bpm segment)))
      (:time-ms segment))))

(defn summary
  "Derive the per-grid summary of a track on demand: one cell per grid step
  from :from to :to (beats, default the track's rows), each with :level (the
  mean value of its rows, or the held value of the last row before it),
  :peak and :onset?. An event track's cell is an onset when it holds an
  event; another track's when a value rises by :onset-rise (default 0.2 of
  its range) over the row before. :grid is a musical duration, default
  {:beats 1/4}."
  ([track] (summary track {}))
  ([{:keys [rows range source] :as track} {:keys [grid from to onset-rise] :or {grid {:beats 1/4} onset-rise 0.2}}]
   (let [step (double (or (:beats grid) (some-> (:bars grid) (* 4)) (fail! "A grid is {:beats n} or {:bars n}" {:grid grid})))
         _ (when-not (pos? step) (fail! "A grid step must be positive" {:grid grid}))
         beats (map first rows)
         from (double (or from (if (seq beats) (* step (Math/floor (/ (apply min beats) step))) 0.0)))
         to (double (or to (if (seq beats) (+ step (apply max beats)) from)))
         [low high] (or range [0.0 1.0])
         rise (* onset-rise (- (double high) (double low)))
         cells (long (Math/ceil (/ (- to from) step)))]
     (loop [i 0 remaining rows held nil out []]
       (if (>= i cells) out
         (let [start (+ from (* i step)) end (+ start step)
               [before remaining] (split-with #(< (first %) start) remaining)
               held (if (seq before) (nth (last before) 2) held)
               [inside remaining] (split-with #(< (first %) end) remaining)
               values (mapv #(nth % 2) inside)
               onset? (if (= :event source)
                        (boolean (seq inside))
                        (boolean (some (fn [[prior value]] (and (some? prior) (>= (- value prior) rise)))
                                       (map vector (cons held values) values))))]
           (recur (inc i) remaining (if (seq values) (peek values) held)
                  (conj out {:beat start
                             :level (if (seq values) (/ (reduce + values) (count values)) held)
                             :peak (if (seq values) (apply max values) held)
                             :onset? onset?}))))))))

;; ---------------------------------------------------------------------------
;; Replay

(defn replay-cursor
  "A cursor over a bundle's rows for replay: every row, in beat order, then
  events, ports and controls, then track ID and row order."
  [{:keys [recording] :as bundle}]
  (let [order {:event 0 :port 1 :control 2}
        rows (for [[id _] (:tracks recording)
                   :let [track (track bundle id)]
                   [index row] (map-indexed vector (:rows track))]
               {:track id :source (:source track) :index index :row row})]
    {:pending (vec (sort-by (fn [{:keys [track source index row]}] [(first row) (order source) track index]) rows))
     :delivered 0}))

(defn due
  "Split the cursor at beat: [rows due at or before beat, the advanced
  cursor]. Replay asks with the beat of the frame about to be resolved, so
  each row is resolved by the frame whose beat it was recorded at."
  [{:keys [pending] :as cursor} beat]
  (let [[now later] (split-with #(<= (first (:row %)) (+ beat epsilon)) pending)
        now (vec now)]
    [now (-> cursor (assoc :pending (vec later)) (update :delivered + (count now)))]))

;; ---------------------------------------------------------------------------
;; Validation

(defn- valid-row? [track row]
  (let [n (count (get columns (:source track) base-columns))]
    (and (vector? row) (= n (count row))
         (every? finite? (take 4 row))
         (case (:source track)
           :event (finite? (nth row 4))
           :control (id? (nth row 4))
           true))))

(defn valid-track? [track]
  (boolean
   (and (map? track) (id? (:id track)) (#{:port :event :control} (:source track))
        (vector? (:rows track)) (<= (count (:rows track)) max-max-rows)
        (every? #(valid-row? track %) (:rows track))
        (nat-int? (:dropped track)) (boolean? (:truncated? track))
        (let [[low high] (:range track)] (and (finite? low) (finite? high))))))

(defn validate-bundle
  "Check an EDN-restored bundle and return it: every track matches its
  content hash and every hash the recording names is present."
  [{:keys [recording tracks] :as bundle}]
  (when-not (and (map? bundle) (map? recording) (map? tracks)
                 (= version (:signal-tracks/version recording))
                 (string? (:id recording)) (map? (:attached-to recording))
                 (vector? (:tempo recording)) (map? (:tracks recording)))
    (fail! "Expected a version 1 signal track bundle" {}))
  (when (> (count (:tracks recording)) max-tracks)
    (fail! "A recording holds at most 256 tracks" {:id (:id recording)}))
  (doseq [[id hash] (:tracks recording)
          :let [track (get tracks hash)]]
    (when-not (and track (= id (:id track)))
      (fail! "A recording names a track the bundle does not hold" {:track id :hash hash})))
  (doseq [[hash track] tracks]
    (when-not (valid-track? track) (fail! "Invalid signal track" {:hash hash :track (:id track)}))
    (when-not (= hash (content-hash track)) (fail! "A signal track does not match its content hash" {:hash hash})))
  bundle)

;; ---------------------------------------------------------------------------
;; The show store (full backups) and sharing export

(defn stored
  "The show's stored signal tracks: {:recordings {id recording} :tracks {hash track}}."
  [model]
  (get-in model [:extensions state-key] {:recordings {} :tracks {}}))

(defn stored-bundle
  "The bundle of a stored recording, or nil."
  [model id]
  (let [{:keys [recordings tracks]} (stored model)]
    (when-let [recording (get recordings id)]
      {:recording recording :tracks (select-keys tracks (vals (:tracks recording)))})))

(defn valid-saved-state? [state]
  (boolean
   (and (map? state) (every? #{:recordings :tracks} (keys state))
        (map? (:recordings state)) (map? (:tracks state))
        (<= (count (:recordings state)) max-stored-recordings)
        (<= (count (:tracks state)) max-stored-tracks)
        (every? (fn [[id recording]]
                  (and (= id (:id recording))
                       (try (validate-bundle {:recording recording :tracks (select-keys (:tracks state) (vals (:tracks recording)))})
                            true (catch clojure.lang.ExceptionInfo _ false))))
                (:recordings state))
        (= (set (keys (:tracks state))) (set (mapcat (comp vals :tracks) (vals (:recordings state))))))))

(defn- collect [{:keys [recordings] :as state}]
  (update state :tracks select-keys (set (mapcat (comp vals :tracks) (vals recordings)))))

(defn- tidy [model state]
  (if (empty? (:recordings state))
    (update model :extensions dissoc state-key)
    (assoc-in model [:extensions state-key] state)))

(defn apply-operation
  "Apply one owner operation and return {:model :ack}:
  {:op :put :bundle b} stores a finished bundle (replacing a recording with
  the same ID), {:op :remove :id id} removes one and the tracks no other
  recording names. Tracks shared by content hash are kept once."
  [model {:keys [op bundle id] :as operation}]
  (when-not (map? operation) (fail! "A signal track operation is a map with an :op" {}))
  (case (if (string? op) (keyword op) op)
    :put (let [{:keys [recording tracks]} (validate-bundle bundle)
               state (-> (stored model)
                         (assoc-in [:recordings (:id recording)] recording)
                         (update :tracks merge tracks)
                         collect)]
           (when (> (count (:recordings state)) max-stored-recordings)
             (fail! "A show keeps at most 64 signal recordings" {}))
           (when (> (count (:tracks state)) max-stored-tracks)
             (fail! "A show keeps at most 1024 signal tracks" {}))
           {:model (tidy model state)
            :ack {:status :accepted :id (:id recording) :tracks (count (:tracks recording))}})
    :remove (let [state (stored model)]
              (when-not (contains? (:recordings state) id) (fail! "Unknown signal recording" {:id id}))
              {:model (tidy model (collect (update state :recordings dissoc id)))
               :ack {:status :accepted :id id}})
    (fail! "Unknown signal track operation" {:op op})))

(defn audio-derived?
  "True for a track whose values come from audio analysis: it can identify
  the piece of music it was recorded from."
  [track]
  (or (#{:audio :audio-event} (:kind track))
      (= :audio (get-in track [:address :transport]))))

(defn export-package
  "Build a sharing package for recording id from an explicit selection.
  Without :tracks it carries the recording's description (attachment, tempo
  segments, counts) and no track. :tracks is a vector of track IDs to
  include. Each included audio-derived track is listed under :privacy
  because it can identify the music it was recorded from."
  ([model id] (export-package model id {}))
  ([model id {:keys [tracks]}]
   (let [{:keys [recording] :as bundle} (or (stored-bundle model id) (fail! "Unknown signal recording" {:id id}))
         selected (vec (distinct tracks))]
     (when-not (every? #(contains? (:tracks recording) %) selected)
       (fail! "Select tracks the recording holds" {:tracks selected}))
     (let [chosen (select-keys (:tracks recording) selected)
           data (into (sorted-map) (map (fn [[_ hash]] [hash (get (:tracks bundle) hash)])) chosen)]
       {:type :tolglow/signal-tracks :version version
        :recording (assoc recording :tracks (into (sorted-map) chosen)
                          :omitted (vec (sort (remove (set selected) (keys (:tracks recording))))))
        :tracks data
        :privacy (vec (for [[track-id hash] (sort chosen) :when (audio-derived? (get data hash))]
                        {:track track-id :identifies-music? true}))}))))
