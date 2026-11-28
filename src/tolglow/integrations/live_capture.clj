(ns tolglow.integrations.live-capture
  "Capture to Live from the runtime (#671, #680): the Capture command's
  validated shape, the MIDI layout and capture tracks saved with the show,
  and sending a captured phrase to Live over a live-connection.

  The runtime (`show/capture-action!`) owns the state; this namespace holds
  the data rules and the connection sequence, so Studio, the REPL and the
  Push Capture button share one validated command.

  Commands (operation, payload):
    :status  {}
    :capture {:bars n :end-beat b}
    :smf     {}
    :setup   {}
    :send    {:bars n :end-beat b :capture? bool :name text :slot-index n
              :focus? bool :handoff? bool}
    :focus   {:clip-id id :handoff? bool}
  Unknown operations, unknown keys and values of the wrong type are refused
  before anything runs.

  Saved show data, under [:integrations :live-capture]:
    {:version 1
     :layout  phrase-clip layout: one pitch per cue, paired controllers per
              parameter component, the cue and lane channels
     :desired live-connection/desired-setup: one capture track per channel,
              its name, arm and monitor state, input port and feed channel,
              and the route. A track's name and channel are its playback
              return: clip MIDI from that track decodes on that channel.}
  Live track IDs are session identities and are never saved; a rerun of
  setup binds the saved tracks again by name."
  (:require [tolglow.integrations.live-connection :as live]
            [tolglow.integrations.phrase-clip :as clip]))

(def saved-version 1)

(def saved-path [:integrations :live-capture])

(defn- fail! [message data] (throw (ex-info message (merge {:http/status 400} data))))

;; ---------------------------------------------------------------------------
;; The command

(def command-keys
  "Payload keys each Capture operation accepts."
  {:status #{}
   :capture #{:bars :end-beat}
   :smf #{}
   :setup #{}
   :send #{:bars :end-beat :capture? :name :slot-index :focus? :handoff?}
   :focus #{:clip-id :handoff?}})

(defn- check-type [key value]
  (let [ok? (case key
              :bars (pos-int? value)
              :end-beat (and (number? value) (Double/isFinite (double value)) (not (neg? value)))
              (:capture? :focus? :handoff?) (boolean? value)
              :name (and (string? value) (<= 1 (count value) 128))
              :slot-index (and (int? value) (<= 0 value 999))
              :clip-id (and (or (string? value) (int? value)) (<= 1 (count (str value)) 128)))]
    (when-not ok? (fail! (str "Capture option " (name key) " has the wrong type or range") {:key key}))
    value))

(defn validate-command
  "The canonical payload for one Capture operation, or an ex-info (HTTP 400).
  Payload keys may be keywords or strings; nil values are dropped."
  [operation payload]
  (let [operation (if (string? operation) (keyword operation) operation)
        allowed (get command-keys operation)]
    (when-not allowed
      (throw (ex-info "Unknown capture operation" {:operation operation :http/status 404})))
    (when-not (or (nil? payload) (map? payload)) (fail! "Capture options must be a map" {}))
    (let [payload (into {} (keep (fn [[k v]] (when (some? v) [(if (string? k) (keyword k) k) v]))) payload)
          unknown (remove allowed (keys payload))]
      (when (seq unknown)
        (fail! (str "Capture " (name operation) " does not take " (pr-str (vec unknown))) {:keys (vec unknown)}))
      [operation (into {} (map (fn [[k v]] [k (check-type k v)])) payload)])))

;; ---------------------------------------------------------------------------
;; Saved layout and capture tracks

(def ^:private monitors #{:in :auto :off})

(defn- validate-desired [desired]
  (let [bad! #(fail! (str "Invalid saved capture tracks: " %) {:path (conj saved-path :desired)})
        channel? #(and (int? %) (<= 1 % 16))
        tracks (:tracks desired)]
    (when-not (map? desired) (bad! "not a map"))
    (when-not (contains? live/capture-routes (:route desired)) (bad! "unknown route"))
    (when-not (and (vector? tracks) (seq tracks)) (bad! "no tracks"))
    (doseq [{:keys [channel name kind arm? monitor input feed-channel host?] :as track} tracks]
      (when-not (and (map? track) (channel? channel) (string? name) (<= 1 (count name) 128) (= :midi kind)
                     (boolean? arm?) (contains? monitors monitor) (channel? feed-channel)
                     (map? input) (string? (:port input)) (channel? (:channel input))
                     (or (nil? host?) (boolean? host?)))
        (bad! (str "track " (pr-str name)))))
    (when-not (apply distinct? nil (map :channel tracks)) (bad! "two tracks on one channel"))
    (when-not (apply distinct? nil (map :name tracks)) (bad! "two tracks with one name"))
    (when-not (= 1 (count (filter :host? tracks))) (bad! "exactly one track hosts the companion"))
    (when (some (set (map :channel tracks)) (map :feed-channel tracks)) (bad! "a feed channel is also a playback channel"))
    desired))

(defn validate-saved
  "Check the saved capture data of a show file and return it."
  [saved]
  (when-not (and (map? saved) (= saved-version (:version saved)))
    (fail! "Invalid saved capture data: unknown version" {:path saved-path}))
  (when-let [unknown (seq (remove #{:version :layout :desired} (keys saved)))]
    (fail! "Invalid saved capture data: unknown keys" {:path saved-path :keys (vec unknown)}))
  (let [layout (try (clip/validate-layout (:layout saved))
                    (catch clojure.lang.ExceptionInfo e (fail! (ex-message e) {:path (conj saved-path :layout)})))]
    (when-let [desired (:desired saved)]
      (validate-desired desired)
      (when-not (= (set (clip/channels layout)) (set (map :channel (:tracks desired))))
        (fail! "Invalid saved capture data: tracks do not match the layout's channels" {:path saved-path})))
    saved))

(defn saved-state
  "What the show saves for the runtime's capture state, or nil before any
  layout exists."
  [{:keys [layout desired]}]
  (when (:layout/version layout)
    (cond-> {:version saved-version :layout (dissoc layout :unassigned)}
      desired (assoc :desired desired))))

(defn desired
  "The capture tracks for a layout. A saved desired setup is kept when it
  still covers the layout's channels (its names, port and feed channels are
  the performer's); otherwise the tracks are derived again, keeping the names
  saved for the channels that remain. port overrides the input port (the Live
  bridge's :feed-port)."
  [layout saved {:keys [port]}]
  (let [names (into {} (map (juxt :channel :name)) (:tracks saved))
        port (or port (get-in saved [:tracks 0 :input :port]) live/default-feed-port)
        feed-channels (when (seq (:tracks saved)) (map :feed-channel (:tracks saved)))
        fresh #(live/desired-setup layout (cond-> {:port port :names names :route (or (:route saved) :tolglow-buffer)}
                                            % (assoc :feed-channels %)))]
    (if (and saved (= (set (clip/channels layout)) (set (map :channel (:tracks saved)))))
      (update saved :tracks (partial mapv #(assoc-in % [:input :port] port)))
      (try (fresh (when (= (count feed-channels) (count (clip/channels layout))) feed-channels))
           (catch clojure.lang.ExceptionInfo _ (fresh nil))))))

;; ---------------------------------------------------------------------------
;; Live operations

(defn- receipt-status [receipt]
  (cond-> {:status (:status receipt)}
    (or (:code receipt) (get-in receipt [:error :code])) (assoc :code (or (:code receipt) (get-in receipt [:error :code])))))

(defn setup!
  "live-connection/setup! that reports a Set it cannot inspect (no bridge, a
  refused or lost inspection) as a problem instead of throwing."
  [conn desired]
  (try (live/setup! conn desired)
       (catch clojure.lang.ExceptionInfo e
         (if-let [receipt (:receipt (ex-data e))]
           {:ready? false :tracks {} :applied [] :desired desired
            :problems [(merge {:code :inspect-failed} (receipt-status receipt) {:op :inspect-set})]}
           (throw e)))))

(defn setup-summary
  "A setup verdict without the inspected Set, as Studio and the status show it."
  [verdict]
  (let [names (into {} (map (juxt :track-id :name)) (get-in verdict [:set :tracks]))]
    {:ready? (:ready? verdict)
     :route (get-in verdict [:desired :route])
     :tracks (vec (for [[ch id] (sort-by key (:tracks verdict))]
                    {:channel ch :track-id id :name (get names id)}))
     :problems (:problems verdict)
     :applied (mapv (fn [[op status]] {:op op :status status}) (:applied verdict))}))

(defn write!
  "Write a phrase into Live through the setup's route. Only the buffer route
  runs here: Live's own Capture MIDI needs the continuous feed to a monitored
  track, which the runtime does not send. Returns
  {:status :sent|:partial|:rejected|:indeterminate|:unsupported :clips [..]
   :partial-clips [..] :skipped [..] :code}."
  [conn verdict phrase layout options]
  (if (not= :tolglow-buffer (get-in verdict [:desired :route]))
    {:status :unsupported :code :route-not-wired :clips [] :partial-clips []}
    (let [result (live/write-phrase! conn verdict phrase layout options)
          failed (first (remove #(= :executed (:status %)) (:receipts result)))]
      (cond-> {:status (cond (:complete? result) :sent
                             (seq (:partial-clips result)) :partial
                             :else (:status failed))
               :clips (:clips result) :partial-clips (:partial-clips result)}
        (seq (:skipped result)) (assoc :skipped (:skipped result))
        failed (merge (select-keys (receipt-status failed) [:code]))))))

(defn show-clip!
  "Select a clip and open Live's Clip View. Returns {:status :clip-id :view :code}."
  [conn clip-id]
  (let [receipt (live/request! conn :show-clip {:clip-id clip-id})]
    (cond-> (assoc (receipt-status receipt) :clip-id clip-id)
      (get-in receipt [:result :view]) (assoc :view (get-in receipt [:result :view])))))
