(ns tolglow.integrations.phrase-capture
  "Retrospective capture of the last few bars of performance (#671).

  A capture buffer observes accepted show commands with the musical beat at
  which each was accepted: cue launches, instance releases and parameter
  writes. It retains a bounded window of raw observations. `capture` turns the
  most recent bars into a phrase: plain data on the beat grid, with one note per
  cue activation and one lane of points per cue parameter. Beats are exact
  rationals relative to the phrase start, so a phrase can be rendered to a Live
  clip, edited and imported again without drifting.

  Commands whose source origin is `:live-playback` or `:tolglow-feedback` are
  not captured, and neither is anything addressed to an instance such a command
  launched. Playing a captured clip back therefore cannot record itself again.

  Nothing here reads a clock or performs IO. Hosts supply observations, or call
  `observe-model` after each owner cycle with the committed model."
  (:require [afterglow.time :as musical-time]))

(def phrase-version 1)

(def default-options
  "Eight bars of history on a sixteenth-note grid."
  {:bars 8 :grid {:beats 1/4} :max-events 4096})

(def ^:private echo-origins #{:live-playback :tolglow-feedback})

(def ^:private end-ops #{:instance/end :instance/kill :instance/remove})

(def ^:private gesture-ops #{:gesture/sample :gesture/end})

(defn- fail! [message data] (throw (ex-info message data)))

(defn- finite? [x] (musical-time/finite-number? x))

(defn- grid-beats [grid meter]
  (let [beats (musical-time/duration->beats grid meter)]
    (when-not (pos? beats) (fail! "A capture grid needs a positive musical duration" {:grid grid}))
    (rationalize beats)))

(defn new-buffer
  "Create an empty capture buffer. Options: :bars retained (default 8), :grid as
  a musical duration (default a sixteenth note, {:beats 1/4}), :meter,
  :max-events (a hard bound on retained observations) and :ignore-origins
  (default #{:live-playback :tolglow-feedback}; an empty set observes
  playback, which tests use to verify what a clip replayed)."
  ([] (new-buffer {}))
  ([options]
   (let [{:keys [bars grid max-events meter]} (merge default-options options)
         meter (musical-time/normalize-meter meter)]
     (when-not (and (pos-int? bars) (<= bars 64) (pos-int? max-events))
       (fail! "A capture buffer needs 1-64 bars and a positive event bound" {:bars bars :max-events max-events}))
     {:bars bars :grid grid :grid-beats (grid-beats grid meter) :meter meter :max-events max-events
      :epoch nil :last-beat nil :bpm nil
      :events [] :baseline {} :open {} :suppressed #{} :seen #{} :seen-order []
      :serial 0 :dropped 0 :discontinuities 0
      :ignore-origins (get options :ignore-origins echo-origins)})))

(defn- retention-beats [buffer]
  (* (:bars buffer) (get-in buffer [:meter :beats-per-bar])))

(defn- fold-baseline
  "Fold events leaving retention into per-activation state, so an activation
  that began before the window is still captured with its values."
  [baseline {:keys [kind instance-id] :as event}]
  (case kind
    :launch (assoc baseline instance-id {:cue-id (:cue-id event) :launch (:beat event) :params (:params event)})
    :param (if (contains? baseline instance-id)
             (assoc-in baseline [instance-id :params (:param event)] (:value event)) baseline)
    :end (dissoc baseline instance-id)))

(defn- prune [buffer]
  (let [horizon (when-let [beat (:last-beat buffer)] (- beat (retention-beats buffer)))
        [old kept] (if horizon
                     [(filterv #(< (:beat %) horizon) (:events buffer)) (filterv #(>= (:beat %) horizon) (:events buffer))]
                     [[] (:events buffer)])
        excess (max 0 (- (count kept) (:max-events buffer)))
        old (into old (subvec kept 0 excess))]
    (assoc buffer :events (subvec kept excess) :dropped (+ (:dropped buffer) excess)
                  :baseline (reduce fold-baseline (:baseline buffer) old))))

(defn- remember-id [buffer id]
  (let [order (conj (:seen-order buffer) id)
        evict (max 0 (- (count order) (* 2 (:max-events buffer))))]
    (assoc buffer :seen (apply disj (conj (:seen buffer) id) (take evict order))
                  :seen-order (subvec order evict))))

(defn- reset-for-epoch [buffer epoch]
  (assoc buffer :epoch epoch :last-beat nil :events [] :baseline {} :open {} :suppressed #{}
                :seen #{} :seen-order []))

(defn- advance-clock
  "Track the transport. A backwards jump is a seek or loop: the history before
  it does not form a phrase with what follows, so retained events are dropped.
  Running instances stay open and are carried into the next phrase."
  [buffer {:keys [beat bpm meter]}]
  (when-not (finite? beat) (fail! "A capture observation needs a finite beat" {:beat beat}))
  (let [buffer (cond-> buffer
                 (finite? bpm) (assoc :bpm (double bpm))
                 (and meter (not= (musical-time/normalize-meter meter) (:meter buffer)))
                 (as-> b (let [m (musical-time/normalize-meter meter)]
                           (assoc b :meter m :grid-beats (grid-beats (:grid b) m)))))
        last-beat (:last-beat buffer)]
    (if (and last-beat (< beat (- last-beat 1e-9)))
      (let [baseline (reduce fold-baseline (:baseline buffer) (:events buffer))]
        (assoc buffer :last-beat beat :events [] :discontinuities (inc (:discontinuities buffer))
                      ;; Activations still running continue from the seek target.
                      :baseline (into {} (for [[id entry] baseline] [id (assoc entry :launch beat)]))))
      (assoc buffer :last-beat (max beat (or last-beat beat))))))

(defn- echo? [buffer command]
  (contains? (:ignore-origins buffer) (get-in command [:source :origin])))

(defn- append [buffer event]
  (let [serial (inc (:serial buffer))]
    (prune (-> buffer (assoc :serial serial) (update :events conj (assoc event :serial serial))))))

(defn events-after
  "Retained events appended after serial, oldest first. A host feeding Live's
  Capture MIDI keeps the last serial it sent."
  [buffer serial]
  (filterv #(> (:serial %) serial) (:events buffer)))

(defn observe
  "Fold one accepted command into the buffer. observation:
    {:command accepted-command :ack its-receipt
     :beat beat-at-acceptance :bpm tempo :meter meter
     :value committed-value-for-gesture-ops (optional)}
  Rejected commands, duplicates, echoes and unrelated operations are ignored.
  An epoch change resets the buffer."
  [buffer {:keys [command ack beat value] :as observation}]
  (let [epoch (or (:epoch ack) (:epoch command))
        buffer (if (and (some? epoch) (not= epoch (:epoch buffer))) (reset-for-epoch buffer epoch) buffer)
        buffer (advance-clock buffer observation)
        id (:command-id command)
        op (:op command)
        instance-id (or (:instance-id command) (:instance-id ack))]
    (cond
      (or (not= :accepted (:status ack)) (nil? id) (contains? (:seen buffer) id)) buffer
      (= :cue/launch op)
      (let [buffer (remember-id buffer id)
            instance-id (:instance-id ack)]
        (cond
          (nil? instance-id) buffer
          (echo? buffer command) (update buffer :suppressed conj instance-id)
          :else
          (let [params (or (get-in command [:launch :params]) {})]
            (-> buffer
                (assoc-in [:open instance-id] {:cue-id (:cue-id command) :launch-beat beat :params params})
                (append {:kind :launch :beat beat :instance-id instance-id :cue-id (:cue-id command)
                         :params params :command-id id})))))
      (contains? end-ops op)
      (let [buffer (remember-id buffer id)]
        (cond
          (contains? (:suppressed buffer) instance-id) (update buffer :suppressed disj instance-id)
          (or (echo? buffer command) (not (contains? (:open buffer) instance-id))) buffer
          :else (-> buffer (update :open dissoc instance-id)
                    (append {:kind :end :beat beat :instance-id instance-id :command-id id
                             :cue-id (get-in buffer [:open instance-id :cue-id])}))))
      (or (= :param/set op) (and (contains? gesture-ops op) (contains? observation :value)))
      (let [buffer (remember-id buffer id)
            param (:param command)
            value (if (= :param/set op) (:value command) value)
            open (get-in buffer [:open instance-id])]
        (if (or (echo? buffer command) (nil? open) (not (keyword? param))) buffer
            (-> buffer
                (assoc-in [:open instance-id :params param] value)
                (append {:kind :param :beat beat :instance-id instance-id :cue-id (:cue-id open)
                         :param param :value value :command-id id}))))
      :else (remember-id buffer id))))

(defn observe-ended
  "Close capture for open instances that are no longer running (timed and
  one-shot cues end without a command). live-ids is the set of running ids."
  [buffer live-ids beat]
  (reduce (fn [buffer id]
            (-> buffer (update :open dissoc id)
                (append {:kind :end :beat beat :instance-id id :cue-id (get-in buffer [:open id :cue-id])})))
          (update buffer :suppressed #(into #{} (filter live-ids) %))
          (sort-by str (remove live-ids (keys (:open buffer))))))

(defn observe-model
  "Read newly accepted commands from a committed show model and fold them in.
  Each command's beat is derived from its :time-ms against the model transport,
  so capture precision does not depend on owner cadence within one tempo."
  [buffer model]
  (let [{:keys [beat bpm meter time-ms]} (:transport model)
        bpm (or bpm 120.0)
        beat (or beat 0.0)
        now (or time-ms (:time-ms model) 0)
        beat-at (fn [command]
                  (if-let [t (:time-ms command)]
                    (min beat (+ beat (/ (* (- t now) bpm) 60000.0)))
                    beat))
        history (get-in model [:control :history])
        buffer (if (not= (:epoch model) (:epoch buffer)) (reset-for-epoch buffer (:epoch model)) buffer)
        buffer (reduce (fn [buffer id]
                         (if (contains? (:seen buffer) id) buffer
                             (let [{:keys [command ack]} (get history id)
                                   value (when (contains? gesture-ops (:op command))
                                           (get-in model [:instances (:instance-id command) :params (:param command)]))]
                               (observe buffer (cond-> {:command command :ack ack :beat (max (or (:last-beat buffer) 0.0) (beat-at command))
                                                        :bpm bpm :meter meter}
                                                 (some? value) (assoc :value value))))))
                       buffer (get-in model [:control :history-order]))
        buffer (advance-clock buffer {:beat beat :bpm bpm :meter meter})
        running (set (keep (fn [[id instance]] (when (not= :ended (:phase instance)) id)) (:instances model)))]
    (observe-ended buffer running beat)))

(defn- quantize-exact [grid beat]
  (let [steps (/ (rationalize beat) grid)
        floor (long (Math/floor (double steps)))
        frac (- steps floor)]
    (* grid (if (>= frac 1/2) (inc floor) floor))))

(defn phrase-window
  "The bar-aligned [start end) beats a capture at end-beat covers. A capture
  pressed near a bar line ends on the nearest bar line."
  [buffer {:keys [bars end-beat]}]
  (let [bpb (rationalize (get-in buffer [:meter :beats-per-bar]))
        bars (or bars (:bars buffer))
        end-beat (or end-beat (:last-beat buffer) 0)
        end (* bpb (max bars (Math/round (/ (double end-beat) (double bpb)))))]
    [(- end (* bars bpb)) end]))

(defn capture
  "Build a phrase from the buffer. Options: :bars (default the buffer's),
  :end-beat (default the latest observed beat). Returns
    {:phrase/version 1 :length-beats L :meter m :grid-beats g
     :source {:start-beat s :end-beat e :bpm tempo}
     :notes [{:id \"n0\" :cue-id c :start b :length l :params {...}
              :carried-in? bool :held-at-end? bool}]
     :lanes [{:cue-id c :param p :points [{:beat b :value v :note \"n0\"}]}]}
  All beats are exact rationals relative to the phrase start. An activation
  shorter than one grid step keeps one grid step. Instances already running at
  the phrase start begin at beat 0 with their values at that moment."
  ([buffer] (capture buffer {}))
  ([buffer options]
   (let [grid (:grid-beats buffer)
         [start end] (phrase-window buffer options)
         length (- end start)
         q #(quantize-exact grid %)
         rel #(min length (max 0 (- (q %) start)))
         events (:events buffer)
         ;; Reconstruct each activation touching the window, starting from those
         ;; whose launch has already left retention.
         activations (reduce (fn [acc {:keys [kind instance-id beat] :as event}]
                               (case kind
                                 :launch (assoc acc instance-id {:cue-id (:cue-id event) :launch beat
                                                                 :params (:params event) :order (count acc)})
                                 :end (if (contains? acc instance-id) (assoc-in acc [instance-id :end] beat) acc)
                                 :param (if (contains? acc instance-id)
                                          (update-in acc [instance-id :moves] (fnil conj []) event) acc)))
                             (into {} (for [[id entry] (:baseline buffer)] [id (assoc entry :order -1)]))
                             events)
         notes (->> activations
                    (keep (fn [[id {:keys [cue-id launch end params moves order]}]]
                            (let [s (if (< (q launch) start) 0 (rel launch))
                                  e (if end (rel end) length)
                                  carried-in? (< (q launch) start)
                                  ;; Values in effect at the phrase start for carried activations.
                                  before (filter #(< (q (:beat %)) start) moves)
                                  params (reduce (fn [p m] (assoc p (:param m) (:value m))) params before)]
                              (when (and (< s length)
                                         (or (nil? end) (> (q end) start) (and (not carried-in?) (>= (q end) start))))
                                {:instance-id id :cue-id cue-id :start s
                                 :length (if (<= e s) (min grid (- length s)) (- e s))
                                 :params params :carried-in? carried-in? :held-at-end? (nil? end)
                                 :moves (filterv #(<= start (q (:beat %))) moves) :order order}))))
                    (sort-by (juxt :start (comp str :cue-id) :order (comp str :instance-id)))
                    (map-indexed (fn [i note] (assoc note :id (str "n" i))))
                    vec)
         ;; A move in the cell its activation is released has nothing to steer.
         points (for [{:keys [id cue-id moves start] note-length :length} notes
                      {:keys [param value beat]} moves
                      :let [b (rel beat)]
                      :when (< b (min length (+ start note-length)))]
                  {:cue-id cue-id :param param :beat (max b start) :value value :note id})
         ;; A value written in the grid cell an activation starts is part of
         ;; its launch, which is how a clip carries it.
         starts (into {} (map (juxt :id :start)) notes)
         launch-values (reduce (fn [m {:keys [note beat param value]}]
                                 (if (= beat (starts note)) (assoc-in m [note param] value) m))
                               {} points)
         notes (mapv #(update % :params merge (get launch-values (:id %))) notes)
         points (remove #(= (:beat %) (starts (:note %))) points)
         lanes (->> points
                    (group-by (juxt :cue-id :param))
                    (sort-by (comp (partial mapv str) key))
                    (mapv (fn [[[cue-id param] pts]]
                            {:cue-id cue-id :param param
                             ;; Later writes in one grid cell win, as they did on stage.
                             :points (->> pts
                                          (reduce (fn [m p] (assoc m [(:beat p) (:note p)] p)) (array-map))
                                          vals
                                          (sort-by (juxt :beat :note))
                                          (mapv #(select-keys % [:beat :value :note])))})))]
     {:phrase/version phrase-version
      :length-beats length :meter (:meter buffer) :grid-beats grid
      :source {:start-beat start :end-beat end :bpm (:bpm buffer)}
      :notes (mapv #(select-keys % [:id :cue-id :start :length :params :carried-in? :held-at-end?]) notes)
      :lanes lanes})))

(defn- beat? [x] (and (rational? x) (not (neg? x))))

(defn validate-phrase
  "Return the phrase or throw. Checks structure, exact beats inside the phrase
  and that every lane point names an existing note of the same cue."
  [{:keys [length-beats notes lanes] :as phrase}]
  (when-not (and (= phrase-version (:phrase/version phrase)) (beat? length-beats) (pos? length-beats)
                 (vector? notes) (vector? lanes) (<= (count notes) 4096))
    (fail! "Invalid phrase" {:keys (keys phrase)}))
  (let [ids (into {} (map (juxt :id :cue-id)) notes)]
    (when-not (= (count ids) (count notes)) (fail! "Phrase note IDs must be unique" {}))
    (doseq [{:keys [id start length cue-id params]} notes]
      (when-not (and (string? id) (some? cue-id) (beat? start) (beat? length) (pos? length)
                     (<= (+ start length) length-beats) (map? params))
        (fail! "Invalid phrase note" {:note id})))
    (doseq [{:keys [cue-id param points]} lanes
            {:keys [beat note]} points]
      (when-not (and (keyword? param) (beat? beat) (< beat length-beats)
                     (or (nil? note) (= cue-id (get ids note))))
        (fail! "Invalid phrase lane point" {:cue-id cue-id :param param :beat beat :note note}))))
  phrase)
