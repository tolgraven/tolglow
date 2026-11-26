(ns tolglow.integrations.live-connection
  "The boundary between Tolglow and a Live Set for automatic setup, Capture,
  clip writes and playback (#680, #671).

  A connection is the existing companion transport shape
  `{:open! :request! :close!}` (see `live-transport/transport`): `request!`
  takes a handle, a request ID, an operation keyword and a payload map, and
  returns a receipt `{:id :status :result}` where status is `:executed`,
  `:rejected` or `:indeterminate`. The Max for Live companion carries these
  operations over its loopback HTTP endpoint; tests use a simulated Set.

  Operations (payload -> result):
    :inspect-set     {} -> {:tracks [track] :tempo bpm :signature {:beats-per-bar n}
                            :can-capture-midi? bool :live-version text}
                     track = {:track-id :index :name :kind :arm? :monitor :input
                              :group-track-id (nil when ungrouped)
                              :clips [{:clip-id :slot-index :name :length-beats}]
                              :arrangement-clips [{:name :start-beat :end-beat}]}
                     :kind is :midi, :audio or :group (a Group Track, whose arm,
                     monitor and input are not read).
    :ensure-track    {:name :kind :midi :group-track-id g} -> {:track-id :created? bool
                                                               :group-track-id read-back}
                     Finds a MIDI track by exact name or creates one at the end,
                     or after the last member of Group Track g (#683).
    :ensure-group    {:name :group-track-id parent} -> {:track-id :created? bool}
                     Finds a Group Track by exact name. The documented LOM has no
                     call that creates one, so a missing group is refused with
                     :group-creation-unavailable until a host path is verified.
    :configure-track {:track-id :expected-name :arm? :monitor :in|:auto|:off
                      :input {:port :channel}} -> {:track track}
    :write-clip      {:track-id :expected-name :slot-index n|nil :name :clip clip
                      :lanes [{:controller coarse :points [[beat v14] ..]}]}
                     -> {:clip-id :slot-index}; nil slot means the first empty one.
                     The clip's notes travel in :clip; controllers as :lanes.
                     :create-scene? true adds a scene when every slot is full;
                     :looping? false creates the clip without looping.
    :place-clip      {:track-id :expected-name :clip-id :arrangement-beat b}
                     -> {:arrangement-beat b}: copies a Session clip into the
                     track's Arrangement at song beat b. An overlap with a clip
                     already there is refused before any write.
    :extend-clip     {:track-id :expected-name :clip-id :notes [..] :lanes [..]}
                     adds to a clip written by :write-clip when one body is too small.
    :capture-midi    {:destination :auto} -> {:clips [{:clip-id :track-id :slot-index}]}
    :read-clip       {:clip-id} -> {:clip clip :track-id}
    :show-clip       {:clip-id} -> {:clip-id :view \"Detail/Clip\"}
    :fire-clip       {:clip-id} -> {:clip-id}
    :stop-clips      {:track-ids [..]} -> {}
    :watch-run-tracks {:track-ids [..]} -> {:watching n}
                     Report Session clip starts and stops on these tracks as
                     {:type :clip-started|:clip-stopped :track-id :clip-id :serial}
                     events (live-runs/player decodes them).

  Clip descriptions are those of `phrase-clip`. Live IDs are session
  identities: every write names the track it expects, and the Live side
  rejects a stale ID or a renamed track. A non-executed receipt after a
  possibly applied write is never retried automatically; rerunning setup
  inspects the Set again and repairs only what is missing."
  (:require [clojure.data.json :as json]
            [tolglow.integrations.phrase-clip :as clip]))

(def operations
  #{:inspect-set :ensure-track :ensure-group :configure-track :write-clip :extend-clip :capture-midi
    :read-clip :show-clip :fire-clip :stop-clips :watch-run-tracks :place-clip})

(def request-budget-chars
  "JSON characters of notes and lanes per request. The companion's loopback
  endpoint refuses bodies over 64 KiB; this leaves room for the envelope."
  48000)

(def capture-routes
  "Exactly one route records a phrase into Live:
  :tolglow-buffer writes the phrase Tolglow captured as a new clip;
  :live-capture-midi feeds a monitored track continuously and invokes Live's
  own Capture MIDI. Using both would record the passage twice."
  #{:tolglow-buffer :live-capture-midi})

(defn- fail! [message data] (throw (ex-info message data)))

(defn open
  "Open a connection on a transport. session-id prefixes request IDs."
  [transport {:keys [session-id]}]
  (when-not (and (fn? (:request! transport)) (fn? (:open! transport)))
    (fail! "A Live connection needs :open! and :request!" {}))
  {:transport transport :handle ((:open! transport) {}) :session-id (or session-id "tolglow")
   :counter (atom 0) :log (atom [])})

(defn close! [{:keys [transport handle]}]
  (when-let [close (:close! transport)] (close handle)))

(def ^:private ticks-per-beat
  "Beats from the wire snap to Standard MIDI File ticks, so a JSON double such
  as 0.25 or 0.3333333333333333 returns to the exact 1/4 or 1/3 beat."
  960)

(defn wire-beat
  "An exact beat from a JSON number: the nearest 1/960 beat."
  [x]
  (when (number? x) (/ (Math/round (* (double x) ticks-per-beat)) ticks-per-beat)))

(defn- keywordize [x] (if (string? x) (keyword x) x))

(defn- normalize-clip [clip]
  (when clip
    (-> (if (contains? clip :clip/version) clip (-> clip (assoc :clip/version (:version clip 1)) (dissoc :version)))
        (update :length-beats wire-beat)
        (update :meter (fn [m] (when m (update m :beats-per-bar #(if (integer? %) % (rationalize (wire-beat %)))))))
        (update :notes (fn [ns] (mapv #(cond-> (-> % (update :start wire-beat) (update :duration wire-beat))
                                         ;; Live reports note velocities as floats.
                                         (number? (:velocity %)) (update :velocity (fn [v] (long (Math/round (double v))))))
                                      ns)))
        (update :controllers (fn [cs] (mapv #(update % :beat wire-beat) cs))))))

(defn- normalize-track [track]
  (cond-> (-> track (update :kind keywordize) (update :monitor keywordize))
    (:clips track) (update :clips (partial mapv #(update % :length-beats wire-beat)))
    (:arrangement-clips track) (update :arrangement-clips (partial mapv #(-> % (update :start-beat wire-beat) (update :end-beat wire-beat))))))

(defn normalize-result
  "Bring a result decoded from JSON back to Tolglow data: enumerations become
  keywords and clip beats become exact rationals. Idempotent on Tolglow data."
  [op result]
  (case op
    :inspect-set (update result :tracks #(mapv normalize-track %))
    :configure-track (update result :track normalize-track)
    :read-clip (update result :clip normalize-clip)
    result))

(def ^:private refusals
  "HTTP answers from the companion's loopback endpoint that mean the request
  never reached Max, so nothing was written."
  {400 :malformed-request 403 :browser-origin-refused 404 :unknown-endpoint 409 :bridge-session-mismatch
   413 :payload-too-large 415 :unsupported-media-type 503 :bridge-busy})

(defn- transport-receipt
  "A receipt for a transport failure: rejected when the request provably never
  reached Max, indeterminate otherwise (it may have been written)."
  [id ^Exception e]
  (let [{:keys [http-status] :as data} (ex-data e)
        code (or (get refusals http-status) (when (:live/not-sent? data) :not-connected))]
    (if code
      {:id id :status :rejected :error {:code code :message (.getMessage e) :http-status http-status}}
      {:id id :status :indeterminate :error {:code :transport-failure :message (.getMessage e) :http-status http-status}})))

(defn request!
  "Send one operation and return its validated receipt. Never retries."
  [{:keys [transport handle session-id counter log]} op payload]
  (when-not (contains? operations op) (fail! "Unknown Live connection operation" {:op op}))
  (let [id (str session-id "/" (swap! counter inc))
        receipt (try (update ((:request! transport) handle id op payload) :status keywordize)
                     (catch Exception e (transport-receipt id e)))]
    (when-not (and (map? receipt) (= id (:id receipt)) (#{:executed :rejected :indeterminate} (:status receipt)))
      (fail! "Live connection returned an invalid receipt" {:op op :id id}))
    (swap! log conj {:id id :op op :status (:status receipt)})
    (cond-> receipt
      (map? (:result receipt)) (update :result #(normalize-result op %))
      (map? (:error receipt)) (update-in [:error :code] keywordize)
      (string? (:code receipt)) (update :code keyword))))

(defn operations-sent
  "Operations this connection sent, in order, for diagnostics and tests."
  [conn]
  (mapv :op @(:log conn)))

;; ---------------------------------------------------------------------------
;; Setup (#680)

(def default-feed-channels
  "Channels Tolglow feeds capture tracks on, one per track. Live clips do not
  keep a channel, so clip playback reaches the companion on channel 1; the
  companion drops input on the feed channel (Tolglow's own monitored feed) and
  tags everything else as playback from its track (see track-channels)."
  [16 15 14 13])

(defn desired-setup
  "The tracks a layout needs: one armed MIDI capture track per layout channel,
  fed by the named input port on its own feed channel. Monitoring is Auto so
  recorded clips play back while the track stays armed for the next capture.
  The cue track is the track that hosts the companion device (:host? true):
  setup adopts and renames the track the companion was placed on."
  [layout {:keys [port route names feed-channels] :or {port "Tolglow" route :tolglow-buffer
                                                       feed-channels default-feed-channels}}]
  (when-not (contains? capture-routes route) (fail! "Unknown capture route" {:route route}))
  (let [chs (clip/channels layout)
        default-name (fn [ch] (cond (= 1 (count chs)) "Tolglow Capture"
                                    (= ch (:channel layout)) "Tolglow Cues"
                                    :else "Tolglow Lanes"))]
    (when (some (set chs) (take (count chs) feed-channels))
      (fail! "Feed channels must differ from the layout's playback channels" {:channels chs}))
    {:route route
     :tracks (vec (for [[ch feed] (map vector chs feed-channels)]
                    (cond-> {:channel ch :name (get names ch (default-name ch)) :kind :midi
                             :arm? true :monitor :auto :input {:port port :channel feed}
                             :feed-channel feed}
                      (= ch (:channel layout)) (assoc :host? true))))}))

(defn track-channels
  "{track-id layout-channel} for a ready setup. Live clips carry no MIDI
  channel: the companion tags clip playback with its source track, and the
  playback decoder (phrase-clip/player) maps it back to the layout channel."
  [setup]
  (into {} (map (fn [[ch id]] [id ch])) (:tracks setup)))

(defn feed-routing
  "Rewrite layout-channel feed messages (clip/feed-messages) onto each capture
  track's feed channel."
  [desired messages]
  (let [by-channel (into {} (map (juxt :channel :feed-channel)) (:tracks desired))]
    (mapv #(update % :channel (fn [ch] (get by-channel ch ch))) messages)))

(defn- track-named [set-snapshot name]
  (let [matches (filter #(and (= name (:name %)) (= :midi (:kind %))) (:tracks set-snapshot))]
    (when (= 1 (count matches)) (first matches))))

(defn- spec-track
  "The Set's track for a desired capture track: for the host, the track the
  companion is on now (renamed to the spec name); otherwise the one MIDI track
  with that name. Returns {:track t} or {:duplicates? true}."
  [set-snapshot spec]
  (if (:host? spec)
    {:track (first (filter #(and (:companion? %) (= (:name spec) (:name %)) (= :midi (:kind %))) (:tracks set-snapshot)))}
    (let [named (filter #(and (= (:name spec) (:name %)) (not (:companion? %))) (:tracks set-snapshot))]
      (if (> (count named) 1) {:duplicates? true} {:track (track-named {:tracks named} (:name spec))}))))

(defn- track-state [track] (select-keys track [:arm? :monitor :input]))

(defn- wanted-state [spec] (select-keys spec [:arm? :monitor :input]))

(defn verify-setup
  "Compare an inspected Set with the desired setup. Returns
  {:ready? bool :tracks {channel track-id} :problems [..]}."
  [set-snapshot desired]
  (let [checks (for [spec (:tracks desired)
                     :let [{:keys [track duplicates?]} (spec-track set-snapshot spec)]]
                 (cond
                   duplicates? {:problem {:channel (:channel spec) :code :duplicate-tracks :name (:name spec)}}
                   (nil? track) {:problem {:channel (:channel spec) :code :missing-track :name (:name spec)}}
                   (not= (wanted-state spec) (track-state track))
                   {:track [(:channel spec) (:track-id track)]
                    :problem {:channel (:channel spec) :code :not-configured :track-id (:track-id track)
                              :expected (wanted-state spec) :actual (track-state track)}}
                   :else {:track [(:channel spec) (:track-id track)]}))
        problems (vec (keep :problem checks))
        problems (cond-> problems
                   (and (= :live-capture-midi (:route desired)) (false? (:can-capture-midi? set-snapshot)))
                   (conj {:code :capture-midi-unavailable}))]
    {:ready? (empty? problems) :tracks (into {} (keep :track checks)) :problems problems}))

(defn setup-plan
  "Operations that bring an inspected Set to the desired setup. Existing tracks
  are reused by name, so rerunning setup creates nothing twice. Duplicate
  names are reported and left for the performer: picking one could take over
  a musician's own track."
  [set-snapshot desired]
  (vec (mapcat (fn [spec]
                 (let [{:keys [track duplicates?]} (spec-track set-snapshot spec)]
                   (cond
                     duplicates? []
                     (nil? track) [[:ensure-track (cond-> {:name (:name spec) :kind :midi} (:host? spec) (assoc :host? true))]
                                   [:configure-track (assoc (wanted-state spec) :expected-name (:name spec))]]
                     (not= (wanted-state spec) (track-state track))
                     [[:configure-track (assoc (wanted-state spec) :track-id (:track-id track) :expected-name (:name spec))]]
                     :else [])))
               (:tracks desired))))

(defn setup!
  "Inspect, apply the plan, then inspect again and verify every arm, monitor
  and input routing from the read-back. Stops at the first receipt that is not
  executed. Returns {:ready? :tracks :problems :applied [[op status]] :set}."
  [conn desired]
  (let [inspect (fn [] (let [r (request! conn :inspect-set {})]
                         (when-not (= :executed (:status r)) (fail! "Live Set could not be inspected" {:receipt r}))
                         (:result r)))
        before (inspect)
        applied (loop [[[op payload] & more] (setup-plan before desired) created nil applied []]
                  (if-not op applied
                    (let [payload (cond-> payload (and (= op :configure-track) (nil? (:track-id payload))) (assoc :track-id created))
                          receipt (request! conn op payload)
                          applied (conj applied [op (:status receipt)])]
                      (if (= :executed (:status receipt))
                        (recur more (if (= op :ensure-track) (get-in receipt [:result :track-id]) created) applied)
                        applied))))
        after (inspect)
        verdict (verify-setup after desired)]
    (assoc verdict :applied applied :set after :desired desired)))

;; ---------------------------------------------------------------------------
;; Capture (#671)

(defn- track-name [setup track-id]
  (some #(when (= track-id (:track-id %)) (:name %)) (get-in setup [:set :tracks])))

(defn- json-size [x] (count (json/write-str x)))

(defn- chunks
  "Split notes and lane points into request bodies under the budget. A lane
  split across requests keeps its points in time order, so each later step
  overwrites the earlier one from where it starts."
  [notes lanes budget]
  (let [items (concat (map (fn [n] [:note n]) notes)
                      (for [{:keys [controller points]} lanes, p points] [:point controller p]))]
    (loop [[item & more :as all] items, current [], size 0, out []]
      (cond
        (nil? item) (if (seq current) (conj out current) (if (seq out) out [[]]))
        :else (let [n (json-size (peek item))]
                (if (and (seq current) (> (+ size n 8) budget))
                  (recur all [] 0 (conj out current))
                  (recur more (conj current item) (+ size n 8) out)))))))

(defn- chunk-payload [items]
  (let [notes (vec (keep (fn [[kind n]] (when (= kind :note) n)) items))
        lanes (->> items (filter #(= :point (first %))) (group-by second)
                   (sort-by key) (mapv (fn [[controller ps]] {:controller controller :points (mapv #(nth % 2) ps)})))]
    {:notes notes :lanes lanes}))

(defn- write-clip!
  "Write one clip description, in several requests when it does not fit one.
  Complete only when every request executed; otherwise the clip written so far
  is reported and nothing is retried (atomic or indeterminate)."
  [conn track-id expected-name slot-index name description budget]
  (let [lanes (clip/controller-lanes (:controllers description))
        [first-chunk & rest-chunks] (map chunk-payload (chunks (:notes description) lanes budget))
        receipt (request! conn :write-clip {:track-id track-id :expected-name expected-name :slot-index slot-index :name name
                                            :clip (-> description (dissoc :controllers) (assoc :notes (:notes first-chunk)))
                                            :lanes (:lanes first-chunk)})]
    (if-not (= :executed (:status receipt))
      {:receipts [receipt] :complete? false}
      (let [clip-id (get-in receipt [:result :clip-id])
            more (reduce (fn [acc payload]
                           (if-not (:complete? acc) acc
                             (let [r (request! conn :extend-clip (assoc payload :track-id track-id :expected-name expected-name :clip-id clip-id))]
                               (cond-> (update acc :receipts conj r)
                                 (not= :executed (:status r)) (assoc :complete? false :partial? true)))))
                         {:receipts [] :complete? true} rest-chunks)]
        (assoc more :receipts (into [receipt] (:receipts more)) :clip (assoc (:result receipt) :clip-id clip-id))))))

(defn write-phrase!
  "Route :tolglow-buffer: write a captured phrase as one new clip per capture
  track (first empty slot unless :slot-index). Controller lanes go as compact
  [beat 14-bit-value] points; a clip too large for one request is continued
  with :extend-clip. Returns {:clips [{:channel :track-id :clip-id :slot-index}]
  :receipts [..] :complete? bool :partial-clips [..]}: a clip whose later
  request failed is listed in :partial-clips, never retried."
  [conn setup phrase layout {:keys [slot-index name budget] :or {name "Tolglow capture" budget request-budget-chars}}]
  (when-not (:ready? setup) (fail! "Live setup is not ready" {:problems (:problems setup)}))
  (let [clips (clip/phrase->clips phrase layout)
        results (reduce (fn [acc [ch description]]
                          (if-not (:complete? acc) acc
                            (let [track-id (get-in setup [:tracks ch])
                                  r (write-clip! conn track-id (track-name setup track-id) slot-index name description budget)]
                              (cond-> (update acc :receipts into (:receipts r))
                                (:clip r) (update (if (:complete? r) :clips :partial-clips) conj
                                                  (merge {:channel ch :track-id track-id} (:clip r)))
                                (not (:complete? r)) (assoc :complete? false)))))
                        {:clips [] :partial-clips [] :receipts [] :complete? true}
                        (sort-by key clips))]
    (assoc results :skipped (:skipped (meta clips)))))

(defn capture-midi!
  "Route :live-capture-midi: ask Live to capture what the monitored tracks
  received. Returns the receipt's clips tagged with their layout channel."
  [conn setup]
  (when-not (:ready? setup) (fail! "Live setup is not ready" {:problems (:problems setup)}))
  (let [receipt (request! conn :capture-midi {:destination :auto})
        by-track (into {} (map (fn [[ch id]] [id ch])) (:tracks setup))]
    {:receipt receipt :complete? (= :executed (:status receipt))
     :clips (vec (for [c (get-in receipt [:result :clips]) :when (by-track (:track-id c))]
                   (assoc c :channel (by-track (:track-id c)))))}))

(defn capture!
  "Record a phrase into Live through the setup's single capture route, then
  focus the first resulting clip for editing. Returns {:route :clips :shown}."
  [conn setup {:keys [phrase layout] :as options}]
  (let [route (get-in setup [:desired :route])
        result (case route
                 :tolglow-buffer (write-phrase! conn setup phrase layout options)
                 :live-capture-midi (capture-midi! conn setup))
        shown (when-let [c (first (:clips result))]
                (request! conn :show-clip {:clip-id (:clip-id c)}))]
    (assoc result :route route :shown (some-> shown :status))))

(defn import-clips!
  "Read clips back from Live (after the performer edited them) and decode the
  phrase they now hold. clips: [{:channel :clip-id}]."
  [conn clips layout options]
  (let [read (for [{:keys [channel clip-id]} clips]
               (let [r (request! conn :read-clip {:clip-id clip-id})]
                 (when-not (= :executed (:status r)) (fail! "Clip could not be read" {:clip-id clip-id :receipt r}))
                 [channel (get-in r [:result :clip])]))]
    (clip/clips->phrase (into {} read) layout options)))
