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
                              :clips [{:clip-id :slot-index :name :length-beats}]}
    :ensure-track    {:name :kind :midi} -> {:track-id :created? bool}
                     Finds a MIDI track by exact name or creates one at the end.
    :configure-track {:track-id :expected-name :arm? :monitor :in|:auto|:off
                      :input {:port :channel}} -> {:track track}
    :write-clip      {:track-id :expected-name :slot-index n|nil :name :clip clip}
                     -> {:clip-id :slot-index}; nil slot means the first empty one.
    :capture-midi    {:destination :auto} -> {:clips [{:clip-id :track-id :slot-index}]}
    :read-clip       {:clip-id} -> {:clip clip :track-id}
    :show-clip       {:clip-id} -> {:clip-id :view \"Detail/Clip\"}
    :fire-clip       {:clip-id} -> {:clip-id}
    :stop-clips      {:track-ids [..]} -> {}

  Clip descriptions are those of `phrase-clip`. Live IDs are session
  identities: every write names the track it expects, and the Live side
  rejects a stale ID or a renamed track. A non-executed receipt after a
  possibly applied write is never retried automatically; rerunning setup
  inspects the Set again and repairs only what is missing."
  (:require [tolglow.integrations.phrase-clip :as clip]))

(def operations
  #{:inspect-set :ensure-track :configure-track :write-clip :capture-midi
    :read-clip :show-clip :fire-clip :stop-clips})

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

(defn- wire-beat [x] (if (and (number? x) (not (rational? x))) (rationalize x) x))

(defn- wire-keyword [x] (if (string? x) (keyword x) x))

(defn- wire-track [track]
  (cond-> (-> track (update :kind wire-keyword) (update :monitor wire-keyword))
    (:clips track) (update :clips (partial mapv #(update % :length-beats wire-beat)))))

(defn- wire-clip [clip]
  (-> clip
      (update :length-beats wire-beat)
      (update :notes (partial mapv #(-> % (update :start wire-beat) (update :duration wire-beat))))
      (update :controllers (partial mapv #(update % :beat wire-beat)))))

(defn- wire-receipt
  "A receipt that crossed the companion's JSON wire (live-transport) has
  strings where this namespace compares keywords (:kind, :monitor, :code) and
  doubles where it compares exact beats. Restore those shapes; receipts that
  are already Clojure data pass through unchanged."
  [op receipt]
  (cond-> (update receipt :code wire-keyword)
    (nil? (:code receipt)) (dissoc :code)
    (and (= op :inspect-set) (get-in receipt [:result :tracks]))
    (update-in [:result :tracks] (partial mapv wire-track))
    (and (= op :configure-track) (get-in receipt [:result :track])) (update-in [:result :track] wire-track)
    (and (= op :read-clip) (get-in receipt [:result :clip])) (update-in [:result :clip] wire-clip)))

(defn request!
  "Send one operation and return its validated receipt. Never retries."
  [{:keys [transport handle session-id counter log]} op payload]
  (when-not (contains? operations op) (fail! "Unknown Live connection operation" {:op op}))
  (let [id (str session-id "/" (swap! counter inc))
        receipt ((:request! transport) handle id op payload)]
    (when-not (and (map? receipt) (= id (:id receipt)) (#{:executed :rejected :indeterminate} (:status receipt)))
      (fail! "Live connection returned an invalid receipt" {:op op :id id}))
    (swap! log conj {:id id :op op :status (:status receipt)})
    (wire-receipt op receipt)))

(defn operations-sent
  "Operations this connection sent, in order, for diagnostics and tests."
  [conn]
  (mapv :op @(:log conn)))

;; ---------------------------------------------------------------------------
;; Setup (#680)

(defn desired-setup
  "The tracks a layout needs: one armed MIDI capture track per channel, fed by
  the named input port on that channel. Monitoring is Auto so recorded clips
  play back while the track stays armed for the next capture."
  [layout {:keys [port route names] :or {port "Tolglow" route :tolglow-buffer}}]
  (when-not (contains? capture-routes route) (fail! "Unknown capture route" {:route route}))
  (let [chs (clip/channels layout)
        default-name (fn [ch] (cond (= 1 (count chs)) "Tolglow Capture"
                                    (= ch (:channel layout)) "Tolglow Cues"
                                    :else "Tolglow Lanes"))]
    {:route route
     :tracks (vec (for [ch chs]
                    {:channel ch :name (get names ch (default-name ch)) :kind :midi
                     :arm? true :monitor :auto :input {:port port :channel ch}}))}))

(defn- track-named [set-snapshot name]
  (let [matches (filter #(and (= name (:name %)) (= :midi (:kind %))) (:tracks set-snapshot))]
    (when (= 1 (count matches)) (first matches))))

(defn- track-state [track] (select-keys track [:arm? :monitor :input]))

(defn- wanted-state [spec] (select-keys spec [:arm? :monitor :input]))

(defn verify-setup
  "Compare an inspected Set with the desired setup. Returns
  {:ready? bool :tracks {channel track-id} :problems [..]}."
  [set-snapshot desired]
  (let [checks (for [spec (:tracks desired)
                     :let [named (filter #(= (:name spec) (:name %)) (:tracks set-snapshot))
                           track (track-named set-snapshot (:name spec))]]
                 (cond
                   (> (count named) 1) {:problem {:channel (:channel spec) :code :duplicate-tracks :name (:name spec)}}
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
                 (let [named (filter #(= (:name spec) (:name %)) (:tracks set-snapshot))
                       track (track-named set-snapshot (:name spec))]
                   (cond
                     (> (count named) 1) []
                     (nil? track) [[:ensure-track {:name (:name spec) :kind :midi}]
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

(defn write-phrase!
  "Route :tolglow-buffer: write a captured phrase as one new clip per capture
  track (first empty slot unless :slot-index). Returns {:clips [{:channel
  :track-id :clip-id :slot-index}] :receipts [..] :complete? bool}."
  [conn setup phrase layout {:keys [slot-index name] :or {name "Tolglow capture"}}]
  (when-not (:ready? setup) (fail! "Live setup is not ready" {:problems (:problems setup)}))
  (let [clips (clip/phrase->clips phrase layout)
        results (reduce (fn [acc [ch description]]
                          (if-not (:complete? acc) acc
                            (let [track-id (get-in setup [:tracks ch])
                                  receipt (request! conn :write-clip {:track-id track-id :expected-name (track-name setup track-id)
                                                                      :slot-index slot-index :name name :clip description})]
                              (cond-> (update acc :receipts conj receipt)
                                (= :executed (:status receipt))
                                (update :clips conj (merge {:channel ch :track-id track-id} (:result receipt)))
                                (not= :executed (:status receipt)) (assoc :complete? false)))))
                        {:clips [] :receipts [] :complete? true}
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
