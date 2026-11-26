(ns tolglow.integrations.live-runs
  "A record of what actually ran, mirrored into Live as run tracks, run clips
  and Group Tracks (#683).

  Three things stay distinct:
  - a cue definition lives in the registry and may never run;
  - a running instance is the show's activation, addressed by instance ID;
  - a take is the recorded run of one instance. It outlives the instance and
    is what Live shows as one clip.

  The record folds committed show models (`observe-model`), as the capture
  buffer does, but keeps every take instead of the last bars:
  - every launched cue gets one channel on its first launch, reused for each
    later run;
  - a channel has lanes. A run takes the lowest lane no other unreleased run
    of the same cue holds, so overlapping instances of one cue get separate,
    independently playable lanes, and successive runs reuse lane 0;
  - a take keeps its start, release and end beats on the show transport
    (exact rationals on the 1/960 beat grid), its launch values and its
    parameter moves;
  - a channel sits in the manual effect group of its cue (#553), and the
    record keeps that group tree for Group Tracks.

  Commands whose source origin is `:live-playback` or `:tolglow-feedback` do
  not make takes, and neither does anything addressed to an instance they
  launched. Playing a run clip back therefore steers its cue without recording
  another clip. A playback launch that names its take (`:live/take-id` in its
  source) is shown as playing that take.

  `mirror-plan` and `sync!` bring a Live Set to the record over the companion
  connection (`live-connection`): one MIDI track per channel lane, named from
  the cue ID, one Session clip per released take whose name carries the take
  ID, and the group tree as Group Tracks around the lane tracks. Tracks and
  clips are found again by name, so a reopened Set reconnects by identity and a
  rerun writes only what is missing. `player` turns run-clip playback reported
  by the companion back into shared commands.

  Nothing here reads a clock or performs IO apart from `sync!`, which sends
  operations on the connection it is given."
  (:require [clojure.string :as str]
            [tolglow.integrations.live-connection :as live]))

(def record-version 1)

(def ^:private echo-origins #{:live-playback :tolglow-feedback})

(def ^:private end-ops #{:instance/end :instance/kill :instance/remove})

(def ^:private gesture-ops #{:gesture/sample :gesture/end})

(def ^:private manual-group-key
  "The instance key effect-group-channels uses for a per-instance placement."
  :tolglow.effects.group-channels/manual-group)

(def run-pitch
  "The pitch of the one note a run clip holds. Run tracks never feed the cue
  launcher, so this pitch does not address a Drum Rack pad."
  60)

(def min-clip-beats
  "A run released in the beat it started still gets a playable clip."
  1/16)

(defn- fail! [message data] (throw (ex-info message data)))

(defn- finite? [x] (and (number? x) (Double/isFinite (double x))))

(defn snap
  "An exact beat on the 1/960 grid (Standard MIDI File ticks), the resolution
  the companion and Live clips use."
  [x]
  (live/wire-beat x))

(defn new-record
  "An empty run record. Options:
    :record-id   prefix of every take ID (default \"run\"). A host gives each
                 record its own, so takes from different sessions never share
                 a clip name in one Set.
    :max-takes   bound on kept takes (default 4096); the oldest finished takes
                 are dropped first and counted under :dropped.
    :ignore-origins  origins that never make takes (default playback and
                 feedback)."
  ([] (new-record {}))
  ([{:keys [record-id max-takes ignore-origins] :or {record-id "run" max-takes 4096}}]
   (when-not (and (string? record-id) (re-matches #"[A-Za-z0-9-]{1,32}" record-id))
     (fail! "A record ID is 1-32 letters, digits or dashes" {:record-id record-id}))
   (when-not (pos-int? max-takes) (fail! "max-takes must be a positive integer" {:max-takes max-takes}))
   {:record/version record-version :record-id record-id :max-takes max-takes
    :ignore-origins (or ignore-origins echo-origins)
    :epoch nil :last-beat nil :serial 0 :dropped 0
    :channels {} :channel-order [] :takes {} :take-order []
    :open {} :lanes {} :suppressed #{} :playback {}
    :seen #{} :seen-order []
    :groups [] :groups-source nil}))

;; ---------------------------------------------------------------------------
;; Groups (#553)

(defn- group-tree
  "The manual group tree without cue membership: [{:id :name :groups [...]}]."
  [manual]
  (mapv (fn [g] {:id (:id g) :name (or (:name g) (str (:id g))) :groups (group-tree (:groups g []))}) manual))

(defn- cue-lineages
  "{cue-id [outermost .. innermost group id]} for the manual tree."
  [manual]
  (letfn [(walk [groups ancestors]
            (mapcat (fn [g]
                      (let [lineage (conj ancestors (:id g))]
                        (concat (map #(vector % lineage) (:cue-ids g []))
                                (walk (:groups g []) lineage))))
                    groups))]
    (into {} (walk manual []))))

(defn- group-lineages [manual]
  (letfn [(walk [groups ancestors]
            (mapcat (fn [g] (let [lineage (conj ancestors (:id g))]
                              (cons [(:id g) lineage] (walk (:groups g []) lineage))))
                    groups))]
    (into {} (walk manual []))))

(defn- manual-config [model] (get-in model [:settings :effect-group-channels :manual] []))

(defn- refresh-groups
  "Follow the show's manual group tree: a regrouped cue moves its channel to
  the new group path."
  [record model]
  (let [manual (manual-config model)]
    (if (identical? manual (:groups-source record))
      record
      (let [lineages (cue-lineages manual)]
        (-> record
            (assoc :groups (group-tree manual) :groups-source manual)
            (update :channels (fn [channels]
                                (into {} (for [[cue-id ch] channels]
                                           [cue-id (assoc ch :group-path (get lineages cue-id []))])))))))))

;; ---------------------------------------------------------------------------
;; Folding commands

(defn- remember-id [record id]
  (let [order (conj (:seen-order record) id)
        seen (conj (:seen record) id)
        bound (* 2 (:max-takes record))]
    (if (> (count order) (* 2 bound))
      (let [evict (- (count order) bound)]
        (assoc record :seen (apply disj seen (subvec order 0 evict)) :seen-order (into [] (subvec order evict))))
      (assoc record :seen seen :seen-order order))))

(defn- echo? [record command]
  (contains? (:ignore-origins record) (get-in command [:source :origin])))

(defn- free-lane [record cue-id]
  (first (remove #(contains? (:lanes record) [cue-id %]) (range))))

(defn- source-summary [source]
  (select-keys source [:type :adapter :origin :session-id :device-id]))

(defn- open-take
  [record {:keys [command beat model]}]
  (let [cue-id (:cue-id command)
        instance-id (get-in command [::ack :instance-id])
        registry (:registry model)
        lineages (cue-lineages (manual-config model))
        record (if (contains? (:channels record) cue-id)
                 record
                 (-> record
                     (assoc-in [:channels cue-id]
                               {:cue-id cue-id :name (or (get-in registry [:cues cue-id :name]) (subs (str cue-id) 1))
                                :first-beat beat :lanes 0 :runs 0 :group-path (get lineages cue-id [])})
                     (update :channel-order conj cue-id)))
        lane (free-lane record cue-id)
        serial (inc (:serial record))
        take-id (str (:record-id record) "." serial)
        run (inc (get-in record [:channels cue-id :runs]))
        assigned (get-in model [:instances instance-id manual-group-key])
        group-lineage (or (get (group-lineages (manual-config model)) assigned) (get lineages cue-id []))]
    (-> record
        (assoc :serial serial)
        (update-in [:channels cue-id] #(-> % (assoc :runs run) (update :lanes max (inc lane))))
        (assoc-in [:lanes [cue-id lane]] take-id)
        (assoc-in [:open instance-id] take-id)
        (assoc-in [:takes take-id]
                  {:take-id take-id :cue-id cue-id :instance-id instance-id :lane lane :run run
                   :epoch (:epoch record) :command-id (:command-id command)
                   :source (source-summary (:source command))
                   :start-beat beat :release-beat nil :end-beat nil :phase :running
                   :params (or (get-in command [:launch :params]) {}) :moves []
                   :group-path group-lineage})
        (update :take-order conj take-id))))

(defn- release-take [record take-id beat]
  (let [{:keys [cue-id lane release-beat]} (get-in record [:takes take-id])]
    (if release-beat
      record
      (-> record
          (assoc-in [:takes take-id :release-beat] beat)
          (assoc-in [:takes take-id :phase] :releasing)
          (update :lanes #(if (= take-id (get % [cue-id lane])) (dissoc % [cue-id lane]) %))))))

(defn- end-take [record instance-id beat]
  (let [take-id (get-in record [:open instance-id])]
    (-> (release-take record take-id beat)
        (assoc-in [:takes take-id :end-beat] beat)
        (assoc-in [:takes take-id :phase] :ended)
        (update :open dissoc instance-id))))

(defn- bound-takes
  "Drop the oldest finished takes beyond :max-takes."
  [record]
  (let [excess (- (count (:takes record)) (:max-takes record))]
    (if-not (pos? excess)
      record
      (let [drop-ids (set (take excess (filter #(= :ended (get-in record [:takes % :phase])) (:take-order record))))]
        (-> record
            (update :takes #(apply dissoc % drop-ids))
            (update :take-order #(into [] (remove drop-ids) %))
            (update :dropped + (count drop-ids)))))))

(defn- observe-command
  "Fold one accepted command at its beat."
  [record {:keys [command ack beat model] :as observation}]
  (let [id (:command-id command)
        op (:op command)
        instance-id (or (:instance-id command) (:instance-id ack))]
    (cond
      (or (not= :accepted (:status ack)) (nil? id) (contains? (:seen record) id)) record

      (= :cue/launch op)
      (let [record (remember-id record id)
            launched (:instance-id ack)]
        (cond
          (nil? launched) record
          (echo? record command)
          (cond-> (update record :suppressed conj launched)
            (get-in command [:source :live/take-id])
            (assoc-in [:playback launched] {:take-id (get-in command [:source :live/take-id]) :start-beat beat
                                            :cue-id (:cue-id command)}))
          :else (bound-takes (open-take record (assoc observation :command (assoc command ::ack ack))))))

      (contains? end-ops op)
      (let [record (remember-id record id)
            take-id (get-in record [:open instance-id])]
        (cond
          (contains? (:suppressed record) instance-id) record
          (nil? take-id) record
          ;; A forced end, a kill or a removal has no release tail.
          (or (not= :instance/end op) (:force? command)) (end-take record instance-id beat)
          :else (release-take record take-id beat)))

      (or (= :param/set op) (and (contains? gesture-ops op) (contains? observation :value)))
      (let [record (remember-id record id)
            take-id (get-in record [:open instance-id])
            param (:param command)]
        (if (or (nil? take-id) (echo? record command) (not (keyword? param)))
          record
          (update-in record [:takes take-id :moves] conj
                     {:beat beat :param param :value (if (= :param/set op) (:value command) (:value observation))})))

      :else (remember-id record id))))

(defn- close-for-epoch
  "A new show epoch ends every open take where the old epoch left off. Its
  channels, lanes and finished takes stay for editing."
  [record epoch]
  (let [beat (or (:last-beat record) 0)]
    (-> (reduce (fn [r [instance-id take-id]] (-> (end-take r instance-id beat)
                                                  (assoc-in [:takes take-id :interrupted] :epoch)))
                record (sort-by key (:open record)))
        (assoc :epoch epoch :open {} :lanes {} :suppressed #{} :playback {} :seen #{} :seen-order []))))

(defn- transport-beat [model]
  (let [{:keys [beat bpm time-ms playing?]} (:transport model)
        beat (or beat 0.0)
        bpm (or bpm 120.0)
        now (or time-ms (:time-ms model) 0)]
    {:beat beat
     ;; A stopped transport holds its beat: commands accepted while stopped
     ;; land on that beat.
     :beat-at (fn [command]
                (if-let [t (and (not (false? playing?)) (:time-ms command))]
                  (min beat (+ beat (/ (* (- t now) bpm) 60000.0)))
                  beat))}))

(defn observe-model
  "Fold newly accepted commands from a committed show model into the record,
  then end takes whose instances finished without a command (timed and
  one-shot cues) and mark releases begun by the runtime. Each command's beat
  comes from its :time-ms against the model transport."
  [record model]
  (let [{:keys [beat beat-at]} (transport-beat model)
        record (if (not= (:epoch model) (:epoch record)) (close-for-epoch record (:epoch model)) record)
        record (refresh-groups record model)
        ;; A backwards seek or loop: later takes start on the beats the
        ;; transport now shows. Takes keep the beats they were recorded on.
        record (if (and (:last-beat record) (< beat (- (:last-beat record) 1e-9)))
                 (-> record (assoc :last-beat (snap beat)) (update :seeks (fnil inc 0)))
                 record)
        history (get-in model [:control :history])
        order (get-in model [:control :history-order])
        fresh (reverse (take-while #(not (contains? (:seen record) %))
                                   (if (vector? order) (rseq order) (reverse order))))
        record (reduce (fn [record id]
                         (let [{:keys [command ack]} (get history id)
                               at (snap (max (or (:last-beat record) 0) (beat-at command)))
                               value (when (contains? gesture-ops (:op command))
                                       (get-in model [:instances (:instance-id command) :params (:param command)]))]
                           (-> (observe-command record (cond-> {:command command :ack ack :beat at :model model}
                                                         (some? value) (assoc :value value)))
                               (assoc :last-beat at))))
                       record fresh)
        now (snap (max (or (:last-beat record) 0) beat))
        instances (:instances model)
        record (reduce (fn [record [instance-id take-id]]
                         (let [phase (get-in instances [instance-id :phase])]
                           (cond
                             (or (nil? phase) (= :ended phase)) (end-take record instance-id now)
                             (= :releasing phase) (release-take record take-id now)
                             :else record)))
                       record (sort-by key (:open record)))]
    (-> record
        (assoc :last-beat now)
        (update :suppressed #(into #{} (filter (fn [id] (contains? instances id))) %))
        (update :playback #(into {} (filter (fn [[id _]] (contains? instances id))) %)))))

;; ---------------------------------------------------------------------------
;; Reading the record

(defn takes
  "Takes in launch order, optionally only one cue's."
  ([record] (mapv (:takes record) (:take-order record)))
  ([record cue-id] (filterv #(= cue-id (:cue-id %)) (takes record))))

(defn held-beats
  "How long a take was held, from launch to release, or nil while it runs."
  [{:keys [start-beat release-beat]}]
  (when release-beat (max 0 (- release-beat start-beat))))

(defn clip-beats
  "The length of a take's clip: its held length, at least min-clip-beats."
  [take]
  (when-let [held (held-beats take)] (max min-clip-beats held)))

(defn- display-id [x] (if (keyword? x) (subs (str x) 1) (str x)))

(defn lane-track-name
  "The Live track for one lane of a cue's channel. Names come from the cue ID,
  which stays fixed when the cue is renamed, so a reopened Set reconnects."
  [cue-id lane]
  (str "Tolglow run " (display-id cue-id) (when (pos? lane) (str " · " (inc lane)))))

(defn group-track-name [group-id] (str "Tolglow group " (display-id group-id)))

(defn clip-name
  "The run clip's name: the cue, its run number and the take ID in brackets."
  [record {:keys [cue-id run take-id]}]
  (str (get-in record [:channels cue-id :name] (display-id cue-id)) " run " run " [" take-id "]"))

(defn take-id-of
  "The take ID in a run clip's name, or nil."
  [clip-name]
  (when (string? clip-name) (second (re-find #"\[([A-Za-z0-9-]+\.\d+)\]\s*$" clip-name))))

(defn take-clip
  "The clip description for a released take: one note held for the run, no
  controllers. Launch values and parameter moves stay in the record until the
  run track carries a parameter rack (#625) whose envelopes can hold them."
  [take]
  (when-let [length (clip-beats take)]
    {:clip/version 1 :length-beats length
     :notes [{:pitch run-pitch :start 0 :duration length :velocity 127}]
     :controllers []}))

;; ---------------------------------------------------------------------------
;; Mirroring into Live

(defn- ref? [x] (and (vector? x) (= ::ref (first x))))

(defn- group-order
  "Group IDs needed by the channels, outermost first, in tree order."
  [record]
  (let [needed (set (mapcat (comp :group-path val) (:channels record)))]
    (letfn [(walk [groups parent]
              (mapcat (fn [{:keys [id groups]}]
                        (when (contains? needed id) (cons [id parent] (walk groups id))))
                      groups))]
      (vec (walk (:groups record) nil)))))

(defn- clip-index
  "{take-id {:clip-id :track-id :slot-index :name}} for every run clip in the
  inspected Set, wherever the performer moved it."
  [set-snapshot]
  (into {} (for [track (:tracks set-snapshot), c (:clips track)
                 :let [id (take-id-of (:name c))] :when id]
             [id (assoc (select-keys c [:clip-id :slot-index :name]) :track-id (:track-id track))])))

(defn mirror-plan
  "Compare an inspected Set with the record. Returns
    {:steps [[op payload {:binds key :take-id id}] ..]
     :bindings {[:group id] track-id, [:lane cue-id lane] track-id}
     :clips {take-id {:clip-id :track-id :slot-index :name}}
     :problems [..]}
  Payload values `[::ref key]` name a track a previous step creates.
  Options: :groups? (default true) mirrors the group tree;
  :arrangement-offset-beats places each new clip in the Arrangement at its
  start beat plus the offset as well (omitted: Session only)."
  ([set-snapshot record] (mirror-plan set-snapshot record {}))
  ([set-snapshot record {:keys [groups? arrangement-offset-beats] :or {groups? true}}]
   (let [tracks (:tracks set-snapshot)
         named (fn [name kind-ok?] (filter #(and (= name (:name %)) (kind-ok? (:kind %))) tracks))
         group? #(= :group %)
         midi? #(= :midi %)
         acc {:steps [] :bindings {} :problems [] :clips (clip-index set-snapshot)}
         ;; Group Tracks, outermost first.
         acc (if-not groups? acc
                 (reduce (fn [acc [gid parent]]
                           (let [name (group-track-name gid)
                                 found (named name group?)
                                 parent-id (when parent (get-in acc [:bindings [:group parent]]))
                                 key [:group gid]]
                             (cond
                               (> (count found) 1)
                               (update acc :problems conj {:code :duplicate-tracks :group-id gid :name name})
                               (= 1 (count found))
                               (let [t (first found)]
                                 (cond-> (assoc-in acc [:bindings key] (:track-id t))
                                   (and parent (not (ref? parent-id)) (some? parent-id) (not= parent-id (:group-track-id t)))
                                   (update :problems conj {:code :group-misplaced :group-id gid :name name
                                                           :expected-group-track-id parent-id
                                                           :actual-group-track-id (:group-track-id t)})))
                               (seq (named name (complement group?)))
                               (update acc :problems conj {:code :name-not-a-group :group-id gid :name name})
                               :else
                               (-> acc
                                   (update :steps conj [:ensure-group (cond-> {:name name}
                                                                        parent (assoc :group-track-id (or parent-id [::ref [:group parent]])))
                                                        {:binds key}])
                                   (assoc-in [:bindings key] [::ref key])))))
                         acc (group-order record)))
         ;; One MIDI track per channel lane, inside its innermost group.
         acc (reduce (fn [acc [cue-id lane]]
                       (let [name (lane-track-name cue-id lane)
                             key [:lane cue-id lane]
                             found (named name midi?)
                             gid (when groups? (peek (get-in record [:channels cue-id :group-path])))
                             group-id (when gid (get-in acc [:bindings [:group gid]]))]
                         (cond
                           (> (count found) 1)
                           (update acc :problems conj {:code :duplicate-tracks :cue-id cue-id :lane lane :name name})
                           (= 1 (count found))
                           (let [t (first found)]
                             (cond-> (assoc-in acc [:bindings key] (:track-id t))
                               (and (not (ref? group-id)) (not= group-id (:group-track-id t))
                                    (or gid (:group-track-id t)))
                               (update :problems conj {:code (if gid :not-in-group :unexpected-group)
                                                       :cue-id cue-id :lane lane :name name :group-id gid
                                                       :expected-group-track-id group-id
                                                       :actual-group-track-id (:group-track-id t)})))
                           :else
                           (-> acc
                               (update :steps conj [:ensure-track (cond-> {:name name :kind :midi}
                                                                    group-id (assoc :group-track-id group-id))
                                                    {:binds key :cue-id cue-id :lane lane}])
                               (assoc-in [:bindings key] [::ref key])))))
                     acc (for [cue-id (:channel-order record)
                               :let [ch (get-in record [:channels cue-id])]
                               lane (range (:lanes ch))]
                           [cue-id lane]))
         ;; One Session clip per released take that the Set does not hold yet.
         acc (reduce (fn [acc {:keys [take-id cue-id lane start-beat] :as take}]
                       (let [track (get-in acc [:bindings [:lane cue-id lane]])
                             clip (take-clip take)]
                         (if (or (nil? clip) (nil? track) (contains? (:clips acc) take-id))
                           acc
                           (update acc :steps conj
                                   [:write-clip (cond-> {:track-id track :expected-name (lane-track-name cue-id lane)
                                                         :slot-index nil :create-scene? true
                                                         :name (clip-name record take) :clip clip}
                                                  arrangement-offset-beats
                                                  (assoc :arrangement-beat (snap (+ start-beat arrangement-offset-beats))))
                                    {:take-id take-id}]))))
                     acc (takes record))]
     acc)))

(defn- inspect! [conn]
  (let [r (live/request! conn :inspect-set {})]
    (when-not (= :executed (:status r)) (fail! "Live Set could not be inspected" {:receipt r}))
    (:result r)))

(defn- resolve-refs
  "Replace [::ref key] values with bound track IDs. A group that could not be
  bound leaves the track ungrouped (reported); a missing lane track drops the
  step."
  [payload bindings]
  (reduce-kv (fn [p k v]
               (if (ref? v)
                 (let [bound (get bindings (second v))]
                   (cond
                     (and bound (not (ref? bound))) (assoc p k bound)
                     (= :group-track-id k) (dissoc p k)
                     :else (reduced nil)))
                 p))
             payload payload))

(defn sync!
  "Bring the Live Set to the record: inspect, apply the plan in order, then
  inspect again and report what the read-back shows. A rejected step changed
  nothing and only its dependents are skipped (a track whose group could not
  be bound is created ungrouped and reported). An indeterminate receipt stops
  the run and nothing is retried: the next sync inspects the Set and finds a
  clip or track that was written after all by its name.
  Returns {:complete? :applied [{:op :status ..}] :tracks :groups :clips
           :problems :set}."
  ([conn record] (sync! conn record {}))
  ([conn record options]
   (let [before (inspect! conn)
         plan (mirror-plan before record options)
         run (reduce (fn [{:keys [bindings] :as acc} [op payload {:keys [binds] :as meta}]]
                       (let [resolved (resolve-refs payload bindings)
                             ungrouped? (and (contains? payload :group-track-id) (not (contains? resolved :group-track-id)))]
                         (if (nil? resolved)
                           (update acc :applied conj (merge meta {:op op :status :skipped}))
                           (let [receipt (live/request! conn op resolved)
                                 status (:status receipt)
                                 entry (cond-> (merge meta {:op op :status status})
                                         (not= :executed status)
                                         (assoc :error (cond-> (select-keys (:error receipt) [:code :message])
                                                         (:code receipt) (assoc :code (:code receipt))))
                                         ungrouped? (assoc :ungrouped? true))
                                 acc (update acc :applied conj entry)]
                             (case status
                               :executed (cond-> acc binds (assoc-in [:bindings binds] (get-in receipt [:result :track-id])))
                               :rejected (cond-> acc binds (update :bindings dissoc binds))
                               :indeterminate (reduced (assoc acc :stopped? true)))))))
                     {:bindings (:bindings plan) :applied [] :stopped? false}
                     (:steps plan))
         after (inspect! conn)
         verdict (mirror-plan after record options)
         failures (vec (for [{:keys [status error] :as a} (:applied run) :when (#{:rejected :indeterminate} status)]
                         (merge (select-keys a [:op :take-id :cue-id :lane :binds]) {:code (or (:code error) status) :status status})))]
     {:complete? (and (empty? (:steps verdict)) (not (:stopped? run)))
      :applied (:applied run)
      :pending (mapv (fn [[op _ meta]] (assoc meta :op op)) (:steps verdict))
      :tracks (into {} (filter (comp #{:lane} first key)) (:bindings verdict))
      :groups (into {} (filter (comp #{:group} first key)) (:bindings verdict))
      :clips (select-keys (:clips verdict) (:take-order record))
      :problems (into (:problems verdict) failures)
      :set after})))

;; ---------------------------------------------------------------------------
;; Projection for the REPL and Studio

(defn- beat-number [b] (when b (double b)))

(defn projection
  "What ran, as plain data for Studio and the REPL. mirror is the last sync!
  result (optional): it binds takes to clips and reports problems.
  Beats are doubles on the show transport; :length-beats is the held length."
  ([record] (projection record nil))
  ([record mirror]
   (let [clips (:clips mirror)
         failed (into {} (keep (fn [{:keys [take-id status]}] (when take-id [take-id status]))) (:problems mirror))
         take-row (fn [{:keys [take-id instance-id run start-beat release-beat end-beat phase params moves interrupted] :as take}]
                    (let [clip (get clips take-id)]
                      (cond-> {:take-id take-id :instance-id instance-id :run run :phase phase
                               :start-beat (beat-number start-beat) :release-beat (beat-number release-beat)
                               :end-beat (beat-number end-beat) :length-beats (beat-number (held-beats take))
                               :params params :moves (count moves)
                               :clip (cond
                                       clip (assoc (select-keys clip [:clip-id :track-id :slot-index]) :status :in-live)
                                       (nil? release-beat) {:status :recording}
                                       (get failed take-id) {:status (get failed take-id)}
                                       :else {:status :not-in-live})}
                        interrupted (assoc :interrupted interrupted))))
         by-lane (group-by (juxt :cue-id :lane) (takes record))
         channels (vec (for [cue-id (:channel-order record)
                             :let [{:keys [name lanes group-path first-beat runs]} (get-in record [:channels cue-id])]]
                         {:cue-id cue-id :name name :group-path group-path :first-beat (beat-number first-beat) :runs runs
                          :lanes (vec (for [lane (range lanes)]
                                        {:lane lane :track-name (lane-track-name cue-id lane)
                                         :track-id (get-in mirror [:tracks [:lane cue-id lane]])
                                         :takes (mapv take-row (get by-lane [cue-id lane]))}))}))
         group-row (fn group-row [{:keys [id name groups]}]
                     {:id id :name name :track-name (group-track-name id)
                      :track-id (get-in mirror [:groups [:group id]])
                      :channels (vec (for [cue-id (:channel-order record)
                                           :when (= id (peek (get-in record [:channels cue-id :group-path])))]
                                       cue-id))
                      :groups (mapv group-row groups)})]
     {:record-id (:record-id record) :latest-beat (beat-number (:last-beat record))
      :takes (count (:takes record)) :running (count (:open record)) :dropped (:dropped record)
      :channels channels
      :groups (mapv group-row (:groups record))
      :playback (vec (for [[instance-id {:keys [take-id cue-id start-beat]}] (sort-by key (:playback record))]
                       {:instance-id instance-id :take-id take-id :cue-id cue-id :start-beat (beat-number start-beat)}))
      :mirror (when mirror
                {:complete? (:complete? mirror) :pending (count (:pending mirror))
                 :problems (mapv #(select-keys % [:code :name :cue-id :lane :group-id :take-id :status]) (:problems mirror))})})))

(defn describe
  "One line per lane and take, for the REPL."
  [record]
  (str/join "\n"
            (for [{:keys [cue-id lanes group-path]} (:channels (projection record))
                  {:keys [lane track-name takes]} lanes
                  line (cons (str track-name (when (seq group-path) (str "  in " (str/join " / " (map display-id group-path)))))
                             (for [{:keys [take-id start-beat length-beats phase moves]} takes]
                               (format "  %-12s beat %8.3f  %s  %d moves" take-id start-beat
                                       (if length-beats (format "held %.3f" length-beats) (name phase)) moves)))]
              line)))

;; ---------------------------------------------------------------------------
;; Playback of run clips

(defn player
  "A decoder for run-clip playback reported by the companion. mirror is a
  sync! result: it maps clip IDs to takes. source is merged into each
  command's :source; origin is always :live-playback, so the record shows the
  playback and makes no take of it.
  Events (from the companion's run-track watch), beats on the show transport:
    {:type :clip-started :track-id t :clip-id c :serial n :beat b}
    {:type :clip-stopped :track-id t :serial n}"
  ([mirror] (player mirror {}))
  ([mirror source]
   {:clips (into {} (map (fn [[take-id {:keys [clip-id]}]] [clip-id take-id])) (:clips mirror))
    :source (merge {:type :ableton :adapter :live-run-clip} source {:origin :live-playback})
    :playing {} :stopping #{} :serials #{} :serial-order [] :ignored []}))

(defn- seen-serial [player serial]
  (if (nil? serial)
    player
    (let [order (conj (:serial-order player) serial)]
      (if (> (count order) 1024)
        (assoc player :serials (disj (conj (:serials player) serial) (first order)) :serial-order (subvec order 1))
        (assoc player :serials (conj (:serials player) serial) :serial-order order)))))

(defn- command [player envelope-fn op data]
  (merge (envelope-fn) {:op op :source (:source player)} data))

(defn- end-playing
  "End whatever the track plays. A launch not yet acknowledged is ended when
  its receipt arrives."
  [player track-id envelope-fn]
  (let [{:keys [instance-id command-id] :as entry} (get-in player [:playing track-id])]
    (cond
      (nil? entry) {:player player :commands []}
      instance-id {:player (update player :playing dissoc track-id)
                   :commands [(command player envelope-fn :instance/end {:instance-id instance-id})]}
      :else {:player (-> player (update :playing dissoc track-id) (update :stopping conj command-id)) :commands []})))

(defn play
  "Decode one playback event. envelope-fn returns a fresh command envelope
  (:command-id :show-id :epoch :time-ms) each call. A clip start launches the
  take's cue once with its launch values; a repeated report of the same start
  (same :serial) launches nothing. Starting another clip on a track that is
  playing one ends the first, as Live does. Returns {:player :commands}."
  [player record {:keys [type track-id clip-id serial beat] :as event} envelope-fn]
  (if (and serial (contains? (:serials player) serial))
    {:player player :commands []}
    (let [player (seen-serial player serial)]
      (case type
        :clip-started
        (let [take-id (get-in player [:clips clip-id])
              take (get-in record [:takes take-id])]
          (if-not take
            {:player (update player :ignored conj (select-keys event [:type :track-id :clip-id :serial])) :commands []}
            (let [{ended :player ends :commands} (end-playing player track-id envelope-fn)
                  launch (-> (command ended envelope-fn :cue/launch
                                      {:cue-id (:cue-id take) :launch {:mode :persistent :params (:params take)}})
                             (update :source assoc :live/take-id take-id :live/clip-id clip-id))
                  held (or (clip-beats take) 0)
                  due (vec (concat (for [{move-beat :beat :keys [param value]} (:moves take)
                                         :let [rel (- move-beat (:start-beat take))]
                                         :when (< rel held)]
                                     [(max 0 rel) {:op :param/set :param param :value value}])
                                   [[held {:op :instance/end}]]))]
              {:player (assoc-in ended [:playing track-id]
                                 {:take-id take-id :clip-id clip-id :command-id (:command-id launch)
                                  :start-beat beat :due due})
               :commands (conj ends launch)})))
        :clip-stopped (end-playing player track-id envelope-fn)
        {:player (update player :ignored conj (select-keys event [:type :track-id :serial])) :commands []}))))

(defn acknowledge
  "Bind a launch receipt to the playing clip it started. A clip stopped
  before its receipt arrived is ended now. Returns {:player :commands}."
  [player {:keys [command-id status instance-id]} envelope-fn]
  (let [accepted? (and (= :accepted status) instance-id)]
    (if (contains? (:stopping player) command-id)
      {:player (update player :stopping disj command-id)
       :commands (if accepted? [(command player envelope-fn :instance/end {:instance-id instance-id})] [])}
      (if-let [track-id (some (fn [[t e]] (when (= command-id (:command-id e)) t)) (:playing player))]
        (if accepted?
          {:player (assoc-in player [:playing track-id :instance-id] instance-id) :commands []}
          {:player (update player :playing dissoc track-id) :commands []})
        {:player player :commands []}))))

(defn advance
  "Emit the parameter moves and the release a playing take reaches by beat
  (the transport of the event beats). Returns {:player :commands}."
  [player beat envelope-fn]
  (reduce (fn [{:keys [player commands]} [track-id {:keys [instance-id start-beat due]}]]
            (if (or (nil? instance-id) (nil? start-beat))
              {:player player :commands commands}
              (let [[now later] (split-with (fn [[rel]] (<= (+ start-beat rel) beat)) due)
                    out (mapv (fn [[_ c]] (command player envelope-fn (:op c) (assoc (dissoc c :op) :instance-id instance-id))) now)
                    ended? (some #(= :instance/end (:op %)) out)]
                {:player (if ended?
                           (update player :playing dissoc track-id)
                           (assoc-in player [:playing track-id :due] (vec later)))
                 :commands (into commands out)})))
          {:player player :commands []}
          (sort-by key (:playing player))))
