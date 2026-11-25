(ns tolglow.integrations.mpv-session
  "Own one configured, read-only mpv player connection for a show runtime."
  (:require [tolglow.integrations.mpv-player :as player]))

(def ^:private close-timeout-ms 1000)

(defn create
  "Validate trusted host config against the show's configured reference."
  [config profile {:keys [provenance options]}]
  (when-not (and (map? config)
                 (every? #{:socket-path :catalog :max-rtt-ms} (keys config))
                 (map? (:catalog config)) (<= 1 (count (:catalog config)) 64)
                 (every? #(= (select-keys profile [:reference :duration-ms])
                             (select-keys % [:reference :duration-ms]))
                         (vals (:catalog config))))
    (throw (ex-info "Configure a local mpv catalog for this show's reference" {})))
  (let [adapter-options (cond-> {:alignment-options options}
                          (contains? config :max-rtt-ms) (assoc :max-rtt-ms (:max-rtt-ms config)))
        options (player/validate-options! {:socket-path (:socket-path config)
                                           :catalog (:catalog config) :provenance provenance
                                           :adapter-options adapter-options})]
    {:lock (Object.) :state (atom {}) :options options}))

(defn- close-player! [handle]
  (or (nil? handle) (not= ::timeout (deref ((:close! handle)) close-timeout-ms ::timeout))))

(defn- refusal [receipt reason message]
  (assoc receipt :status :rejected :reason reason :message message))

(defn reconnect!
  "Use the caller's show admission function to fence a new observation session.
  Player close and open execute after admission returns."
  [owner admit!]
  (locking (:lock owner)
    (if (:closed? @(:state owner))
      (refusal {} :show-stopped "The show has stopped")
      (let [{:keys [callbacks] :as admitted} (admit!)
            receipt (dissoc admitted :callbacks)]
        (if-not (= :accepted (:status receipt))
          receipt
          (cond
            (not (close-player! (get-in @(:state owner) [:attachment :handle])))
            (refusal receipt :player-still-closing "The previous mpv connection is still closing")

            (nil? ((:state! callbacks)))
            (refusal receipt :stale-show-session "The show or reference session changed during reconnect")

            :else
            (try
              (let [options (-> (:options owner) (assoc :provenance (:provenance callbacks))
                                (assoc-in [:adapter-options :alignment-options] (:alignment-options callbacks)))
                    handle (player/open! options callbacks)]
                (reset! (:state owner) {:attachment (assoc receipt :handle handle :state! (:state! callbacks))})
                receipt)
              (catch Exception _
                (refusal receipt :player-open-failed "The configured mpv connection could not start")))))))))

(defn status
  "Project the current alignment and transport result without exposing paths."
  [owner context]
  (let [{:keys [attachment closed?]} (some-> owner :state deref)
        {:keys [handle session-id state!]} attachment
        current? (= (select-keys context [:runtime-id :show-id :epoch])
                    (select-keys attachment [:runtime-id :show-id :epoch]))
        alignment (when (and attachment current? (not closed?)) (state!))
        done (when handle (deref (:done handle) 0 nil))]
    (merge context
           {:configured? (some? owner) :session-id session-id}
           (cond
             (nil? owner) {:status :unavailable :reason :not-configured}
             closed? {:status :unavailable :reason :show-stopped}
             (nil? attachment) {:status :unavailable :reason :not-attached}
             (not current?) {:status :stale :reason :show-changed}
             (nil? alignment) {:status :stale :reason :session-replaced}
             done {:status :unavailable :reason (:reason done)}
             :else (-> (select-keys alignment [:status :reason :confirmations :reference])
                       (assoc :status
                              (case (:status alignment)
                                :locked :aligned
                                :unavailable (if (= :expired (:reason alignment)) :stale :unavailable)
                                (:status alignment))))))))

(defn close! [owner]
  (when owner
    (locking (:lock owner)
      (let [[before _] (swap-vals! (:state owner) assoc :closed? true)]
        (when-not (:closed? before)
          (close-player! (get-in before [:attachment :handle]))))))
  nil)
