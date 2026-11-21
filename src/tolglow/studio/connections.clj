(ns tolglow.studio.connections
  "Owned studio resources. The host owns model transactions and logical time;
  this boundary owns managers, credentials, reactive caches and capture workers.
  Constructing it opens no devices and sends no provider requests."
  (:refer-clojure :exclude [sync])
  (:require [afterglow.reactive :as graph]
            [tolglow.studio.agents :as agent-manager]
            [tolglow.studio.audio :as capture]
            [tolglow.studio.credentials :as credentials]
            [tolglow.studio.fixture-service :as fixture-manager]
            [tolglow.studio.repl-manager :as repl-manager]
            [tolglow.studio.sensors :as sensor-manager]
            [tolglow.studio.sync :as sync-manager]))

(defn- fail! [code message]
  (throw (ex-info message {:connections/error code})))

(defn- portable! [value]
  (letfn [(valid? [x depth]
            (and (< depth 96) (not (record? x))
                 (or (nil? x) (boolean? x) (string? x) (keyword? x) (char? x)
                     (and (number? x) (Double/isFinite (double x)))
                     (and (map? x) (every? (fn [[k v]] (and (valid? k (inc depth)) (valid? v (inc depth)))) x))
                     (and (or (vector? x) (list? x) (set? x)) (every? #(valid? % (inc depth)) x)))))]
    (when-not (valid? value 0) (fail! :nonportable "Connection state must contain portable data only")))
  value)

(defn validate-audio-state
  "Validate the portable capture request without opening a line. Pairings and
  analyzer configuration live in :config; old run IDs/sample counters do not."
  [state]
  (when-not (and (map? state) (= 1 (:version state))
                 (every? #{:version :config :capture-wanted?} (keys state))
                 (boolean? (:capture-wanted? state))
                 (or (nil? (:config state)) (map? (:config state)))
                 (or (:config state) (false? (:capture-wanted? state))))
    (fail! :invalid-audio-state "Invalid saved capture configuration"))
  (portable! state)
  (assoc state :config (when-let [config (:config state)] (capture/validate-config config))))

(defn- preflight [model]
  (when-not (and (map? model) (or (nil? (:integrations model)) (map? (:integrations model))))
    (fail! :invalid-model "Connections require a model and optional integration map"))
  (let [integrations (portable! (or (:integrations model) {}))
        integrations (cond-> integrations
                       (contains? integrations :agents) (update :agents agent-manager/validate-state)
                       (contains? integrations :fixtures) (update :fixtures fixture-manager/validate-state)
                       (contains? integrations :sync) (update :sync sync-manager/validate-state)
                       (contains? integrations :repl) (update :repl repl-manager/validate-state)
                       (contains? integrations :audio-capture) (update :audio-capture validate-audio-state))
        reactive (graph/validate-registry (or (:reactive model) {:registrations []}))]
    (when (contains? model :sensors) (sensor-manager/validate-state (:sensors model)))
    {:integrations integrations :reactive reactive}))

(defn- live-state [connections]
  (let [state @(:state connections)]
    (when (:closed? state) (fail! :closed "Connections are closed"))
    state))

(defn agents [connections] (get-in (live-state connections) [:bundle :agents]))
(defn audio [connections] (get-in (live-state connections) [:bundle :audio]))
(defn fixtures [connections] (get-in (live-state connections) [:bundle :fixtures]))
(defn reactive [connections] (get-in (live-state connections) [:bundle :reactive]))
(defn sync [connections] (get-in (live-state connections) [:bundle :sync]))
(defn repls [connections] (get-in (live-state connections) [:bundle :repls]))
(defn sensors [connections] (get-in (live-state connections) [:bundle :sensors]))

(defn- close-bundle! [bundle]
  ;; All resources are attempted even when an injected transport fails to close.
  (reduce (fn [errors [kind close resource]]
            (if-not resource errors
              (try
                (let [result (close resource)]
                  (cond-> errors
                    (or (false? (:terminated? result)) (false? (:stopped? result)))
                    (conj {:resource kind :code :cleanup-timeout})))
                (catch Exception _ (conj errors {:resource kind :code :cleanup-failed})))))
          [] [[:sensors sensor-manager/close! (:sensors bundle)]
              [:repls repl-manager/close! (:repls bundle)]
              [:sync sync-manager/close! (:sync bundle)]
              [:audio capture/close! (:audio bundle)]
              [:agents agent-manager/close! (:agents bundle)]
              [:fixtures fixture-manager/close! (:fixtures bundle)]
              [:reactive graph/close! (:reactive bundle)]]))

(defn- prepare [model data options]
  (let [resources (volatile! {})]
    (try
      (let [agents (agent-manager/create-manager (:agent-options options))
            _ (vswap! resources assoc :agents agents)
            audio (capture/create-manager (:audio-options options))
            _ (vswap! resources assoc :audio audio)
            fixture-options (cond-> (:fixture-options options)
                              (get-in data [:integrations :fixtures])
                              (assoc :library (get-in data [:integrations :fixtures])))
            fixtures (fixture-manager/create-manager fixture-options)
            _ (vswap! resources assoc :fixtures fixtures)
            sync-options (cond-> (:sync-options options)
                           (get-in data [:integrations :sync])
                           (assoc :config (get-in data [:integrations :sync :config])))
            sync (sync-manager/create-manager sync-options)
            _ (vswap! resources assoc :sync sync)
            repl-options (cond-> (:repl-options options)
                           (get-in data [:integrations :repl])
                           (assoc :state (get-in data [:integrations :repl])))
            repls (repl-manager/create-manager (or repl-options {}))
            _ (vswap! resources assoc :repls repls)
            sensors (sensor-manager/create-manager (assoc (:sensor-options options) :model model))
            _ (vswap! resources assoc :sensors sensors)
            reactive (graph/create-system model)
            _ (vswap! resources assoc :reactive reactive)]
        ;; Resolve handlers before looking up credentials. Neither operation
        ;; publishes effects or starts remote chats/capture.
        (graph/restore! reactive (:reactive data) (:reactive-options options))
        (when-let [state (get-in data [:integrations :agents])]
          (agent-manager/restore! agents state))
        @resources)
      (catch Throwable error
        (close-bundle! @resources)
        (throw error)))))

(defn create
  "Create an inert resource owner for model. Options: :agent-options,
  :audio-options, :sync-options, :fixture-options, :repl-options, :sensor-options, :reactive-options,
  :credential-store or :credential-path, :cancel-timeout-ms (1000),
  :audio-stop-timeout-ms (1000), :sync-stop-timeout-ms (1500).
  The default lazy local credential store is under user.home/.config/tolglow.
  Saved credential references are looked up locally; no provider is contacted.
  Source registrations require explicit :reactive-options {:trusted-source? true}."
  ([model] (create model {}))
  ([model options]
   (let [data (preflight model)
         options (merge {:cancel-timeout-ms 1000 :audio-stop-timeout-ms 1000 :sync-stop-timeout-ms 1500} options)
         _ (doseq [key [:cancel-timeout-ms :audio-stop-timeout-ms :sync-stop-timeout-ms]]
             (when-not (and (integer? (get options key)) (<= 0 (get options key) 10000))
               (fail! :invalid-options "Connection timeouts must be integers in 0..10000 ms")))
         store (or (:credential-store options) (get-in options [:agent-options :credential-store])
                   (credentials/file-store
                    (or (:credential-path options)
                        (str (System/getProperty "user.home") "/.config/tolglow/credentials.edn"))))
         options (-> options (assoc-in [:agent-options :credential-store] store)
                     (assoc-in [:sensor-options :credential-store] store)
                     (update :fixture-options #(merge {:limits {:max-cache-bytes 4194304}} %)))
         bundle (prepare model data options)]
     {:lock (Object.) :options options
      :state (atom {:closed? false :restoring? false :generation 0 :model model
                    :integrations (:integrations data) :bundle bundle
                    :sync-wanted? (boolean (get-in data [:integrations :sync :wanted?]))
                    :audio-state (or (get-in data [:integrations :audio-capture])
                                     {:version 1 :config nil :capture-wanted? false})
                    :cleanup-errors []})})))

(defn status [connections]
  (select-keys @(:state connections) [:closed? :restoring? :generation :cleanup-errors]))

(defn commit-restart!
  "Invalidate callback ownership atomically while retaining every resource
  manager, REPL session and configuration. No callbacks, transport IO or waits.
  Call under the host publication lock, then rebind-restart! outside it."
  [connections model]
  (locking (:lock connections)
    (let [before (live-state connections)
          generation (inc (:generation before))]
      (when (:restoring? before) (fail! :restoring "Cannot restart during restoration"))
      (let [sync (get-in before [:bundle :sync])
            sync-run-id (sync-manager/active-run-id sync)]
        (swap! (:state connections) assoc :generation generation :model model
               :restart-inputs {:audio-run-id (capture/active-run-id (get-in before [:bundle :audio]))
                                :sync-run-id sync-run-id})
        (sync-manager/invalidate-inputs! sync sync-run-id))
      generation)))

(defn export-state
  "Export portable settings/history/cache/reactive state. Unowned integration
  settings (Ableton/Beat Link/sensors) are retained. Host serializes this with
  model publication; managers each capture their own consistent snapshot."
  ([connections] (export-state connections (:model (live-state connections))))
  ([connections model]
   (let [{:keys [bundle audio-state sync-wanted?]} (live-state connections)
         integrations (assoc (or (:integrations model) {})
                             :agents (agent-manager/export-state (:agents bundle))
                             :fixtures (fixture-manager/export-state (:fixtures bundle))
                             :sync (assoc (sync-manager/export-state (:sync bundle)) :wanted? sync-wanted?)
                             :repl (repl-manager/export-state (:repls bundle))
                             :audio-capture audio-state)]
     (portable! (cond-> {:integrations integrations :reactive (graph/export-registry (:reactive bundle))}
                  (contains? model :sensors) (assoc :sensors (sensor-manager/export-state model)))))))

(defn publish!
  "Publish one committed model to the reactive graph. Watcher actions remain
  queued until drain-actions! on the host's next cycle. Call between restores."
  [connections model]
  (let [{:keys [bundle generation restoring?]} (live-state connections)]
    (when restoring? (fail! :restoring "Cannot publish during connection restoration"))
    (let [result (graph/publish! (:reactive bundle) model)]
      (locking (:lock connections)
        (when (= generation (:generation (live-state connections)))
          (swap! (:state connections) assoc :model model :integrations (or (:integrations model) {}))))
      result)))

(defn drain-actions! [connections]
  (let [{:keys [bundle restoring?]} (live-state connections)]
    (when restoring? (fail! :restoring "Cannot drain actions during connection restoration"))
    (graph/drain-actions! (:reactive bundle))))

(defn- guard-callbacks [connections generation callbacks]
  (when-not (and (map? callbacks) (ifn? (:events! callbacks))
                 (every? #(or (nil? %) (ifn? %)) ((juxt :features! :done! :clock!) callbacks)))
    (fail! :invalid-callbacks "Capture requires callable events! and optional feature/done/clock callbacks"))
  (let [current? #(let [state @(:state connections)]
                   (and (not (:closed? state)) (not (:restoring? state))
                        (= generation (:generation state))))]
    (when (:subscriptions callbacks)
      (fail! :unsafe-subscriptions "Use guarded events! callbacks for connection-owned capture subscriptions"))
    (reduce (fn [result key]
              (if-let [callback (get callbacks key)]
                (assoc result key (fn [& args] (if (current?) (apply callback args) false))) result))
            callbacks [:events! :features! :done! :clock!])))

(defn start-audio!
  "Explicitly start capture after the host has rebased logical time. Callback
  gates reject old generations after restore/close. :audio-options :now-ms must
  use the host's current logical clock. Use events! for subscription dispatch."
  [connections config callbacks]
  (let [config (capture/validate-config config)]
    (locking (:lock connections)
      (let [{:keys [bundle generation restoring?]} (live-state connections)]
        (when restoring? (fail! :restoring "Cannot start capture during restoration"))
        (let [handle (capture/start! (:audio bundle) config (guard-callbacks connections generation callbacks))]
          (swap! (:state connections) assoc :audio-state {:version 1 :config config :capture-wanted? true})
          handle)))))

(defn stop-audio!
  "Stop capture and clear the persisted desire to capture. Closing a source
  must return promptly; the worker join uses the configured bounded timeout."
  ([connections] (stop-audio! connections nil))
  ([connections expected-generation]
  (let [[manager run-id] (locking (:lock connections)
                  (let [state (live-state connections)
                        _ (when (and expected-generation (not= expected-generation (:generation state)))
                            (fail! :stale-generation "Audio operation belongs to retired resources"))
                        manager (audio connections)]
                    (swap! (:state connections) assoc-in [:audio-state :capture-wanted?] false)
                    [manager (capture/active-run-id manager)]))]
    (capture/stop! manager (get-in connections [:options :audio-stop-timeout-ms]) run-id))))

(defn resume-audio!
  "Explicit post-clock-rebase hook. A restored capture request stays stopped
  until this call. No saved worker/analyzer sample timestamp is reused."
  [connections callbacks]
  (locking (:lock connections)
    (let [{:keys [audio-state]} (live-state connections)]
      (if (:capture-wanted? audio-state)
        (start-audio! connections (:config audio-state) callbacks)
        {:started? false :reason :not-requested}))))

(defn- sync-callbacks [connections generation callbacks]
  (when-not (and (map? callbacks) (ifn? (:events! callbacks))
                 (every? #(or (nil? %) (ifn? %)) ((juxt :commands! :envelope! :resolve-instance!) callbacks)))
    (fail! :invalid-callbacks "Synchronization requires events! and optional callable control callbacks"))
  (reduce (fn [result key]
            (if-let [callback (get callbacks key)]
              (assoc result key
                     (fn [& args]
                       (let [state @(:state connections)]
                         (if (and (not (:closed? state)) (= generation (:generation state)))
                           (apply callback args)
                           (when (#{:events! :commands!} key) false))))) result))
          callbacks [:events! :commands! :envelope! :resolve-instance!]))

(defn start-sync!
  "Start selected synchronization resources explicitly. Optional config is
  validated before replacing stopped settings. Factories run on the manager's
  worker; host callbacks are invalidated immediately by restore/close. The host
  must additionally retain the captured show epoch in command envelopes."
  ([connections callbacks] (start-sync! connections nil callbacks))
  ([connections config callbacks]
   (let [config (when config (sync-manager/validate-config config))]
     (locking (:lock connections)
       (let [{:keys [bundle generation]} (live-state connections)
             callbacks (sync-callbacks connections generation callbacks)]
         (when config (sync-manager/configure! (:sync bundle) config))
         (let [handle (sync-manager/start! (:sync bundle) callbacks)]
           (swap! (:state connections) assoc :sync-wanted? true)
           handle))))))

(defn stop-sync!
  "Stop synchronization resources and clear the saved restart request. Worker
  shutdown occurs outside the connection owner lock."
  ([connections] (stop-sync! connections nil))
  ([connections expected-generation]
  (let [[manager run-id] (locking (:lock connections)
                  (let [state (live-state connections)
                        _ (when (and expected-generation (not= expected-generation (:generation state)))
                            (fail! :stale-generation "Synchronization operation belongs to retired resources"))
                        manager (sync connections)]
                    (swap! (:state connections) assoc :sync-wanted? false)
                    [manager (sync-manager/active-run-id manager)]))]
    (sync-manager/stop! manager (get-in connections [:options :sync-stop-timeout-ms]) run-id))))

(defn resume-sync!
  "Explicit host hook after model/clock restore. A saved wanted? flag alone
  never starts discovery, MIDI or the Live bridge during resource preparation."
  [connections callbacks]
  (locking (:lock connections)
    (if (:sync-wanted? (live-state connections))
      (start-sync! connections callbacks)
      {:started? false :reason :not-requested})))

(defn rebind-restart!
  "Rebind active wanted inputs after restart without opening/closing transports.
  Generation checks serialize against restore/close and later restarts. Wanted
  flags remain authoritative; a concurrent stop cannot be undone by this call.
  Synchronization acknowledges its boundary asynchronously on its event owner."
  [connections generation audio-callbacks sync-handlers]
  (locking (:lock connections)
    (let [state @(:state connections)]
      (if (or (:closed? state) (not= generation (:generation state)))
        {:rebound? false :reason :retired-generation}
        {:rebound? true
         :audio (when (get-in state [:audio-state :capture-wanted?])
                  (let [manager (get-in state [:bundle :audio])]
                    (if (= (get-in state [:restart-inputs :audio-run-id]) (capture/active-run-id manager))
                      (capture/rebind! manager (guard-callbacks connections generation audio-callbacks))
                      {:rebound? false :reason :replaced-run})))
         :sync (when (:sync-wanted? state)
                 (let [manager (get-in state [:bundle :sync])]
                   (if (= (get-in state [:restart-inputs :sync-run-id]) (sync-manager/active-run-id manager))
                     (sync-manager/rebind! manager (sync-callbacks connections generation sync-handlers))
                     {:rebound? false :reason :replaced-run})))}))))

(defn prepare-restore
  "Validate and prepare resources without touching the current bundle. Local
  credential lookup and trusted handler compilation happen here, outside the
  host model lock. The candidate is bound to this owner and its generation.
  The caller must commit it or abort-prepared! to release staged resources."
  [connections model]
  (let [before (live-state connections)
        data (preflight model)
        bundle (prepare model data (:options connections))]
    {:owner connections :expected-generation (:generation before)
     :model-epoch (:epoch model) :model model :data data :bundle bundle
     :lifecycle (atom :prepared)}))

(defn commit-restore!
  "Atomically replace resource ownership. No IO, callbacks, cancellation or
  waits occur here. The host calls this in the same transaction/lock as model
  publication. Returns an opaque retired-resource handle for retire! OUTSIDE
  that lock. Stale/foreign/already-consumed candidates are rejected unchanged."
  [connections candidate]
  (when-not (identical? connections (:owner candidate))
    (fail! :foreign-candidate "Prepared resources belong to another owner"))
  (locking (:lock connections)
    (let [before (live-state connections)]
      (when-not (= :prepared @(:lifecycle candidate))
        (fail! :consumed-candidate "Prepared resources have already been consumed"))
      (when-not (= (:generation before) (:expected-generation candidate))
        (fail! :stale-candidate "Resources changed after preparation"))
      (let [retired {:owner connections :bundle (:bundle before)
                     :lifecycle (atom :retired) :generation (:generation before)}]
        (reset! (:state connections)
                {:closed? false :restoring? false :generation (inc (:generation before))
                 :model (:model candidate) :integrations (get-in candidate [:data :integrations])
                 :bundle (:bundle candidate)
                 :sync-wanted? (boolean (get-in candidate [:data :integrations :sync :wanted?]))
                 :audio-state (or (get-in candidate [:data :integrations :audio-capture])
                                  {:version 1 :config nil :capture-wanted? false})
                 :cleanup-errors (:cleanup-errors before)})
        (reset! (:lifecycle candidate) :committed)
        retired))))

(defn abort-prepared!
  "Release an unused candidate. Committed resources cannot be aborted; repeat
  calls are harmless. Cleanup occurs outside the connection lock."
  [candidate]
  (let [connections (:owner candidate)
        bundle (locking (:lock connections)
                 (when (= :prepared @(:lifecycle candidate))
                   (reset! (:lifecycle candidate) :aborted)
                   (:bundle candidate)))]
    {:aborted? (boolean bundle) :cleanup-errors (if bundle (close-bundle! bundle) [])}))

(defn retire!
  "Cancel and close a retired bundle once, outside the host model lock. Old
  chat callbacks must retain their original epoch; the host rejects them after
  restore. Generation gates already suppress retired audio callbacks."
  [retired]
  (let [connections (:owner retired)
        bundle (locking (:lock connections)
                 (when (= :retired @(:lifecycle retired))
                   (reset! (:lifecycle retired) :closed)
                   (:bundle retired)))
        errors (when bundle
                 (let [cancellation-error (try
                                            (when-not (:completed? (agent-manager/cancel-all!
                                                                   (:agents bundle)
                                                                   (get-in connections [:options :cancel-timeout-ms])))
                                              {:resource :agents :code :cancellation-timeout})
                                            (catch Exception _ {:resource :agents :code :cancellation-failed}))]
                   (cond-> (close-bundle! bundle) cancellation-error (conj cancellation-error))))]
    (when (seq errors)
      (locking (:lock connections)
        (swap! (:state connections) update :cleanup-errors into errors)))
    {:retired? (boolean bundle) :cleanup-errors (vec errors)}))

(defn restore!
  "Standalone composition of prepare/commit/retire. The studio instead commits
  resources and its model under one host lock, then retires outside that lock.
  Invalid data/compilation leaves current resources intact. Capture remains
  stopped until explicit resume-audio! after the clock rebase; chats never replay."
  [connections model]
  (let [candidate (prepare-restore connections model)]
    (try
      (retire! (commit-restore! connections candidate))
      (status connections)
      (finally (abort-prepared! candidate)))))

(defn close!
  "Invalidate the owner once, then attempt every cleanup outside owner locks.
  Returns portable cleanup failures/timeouts without leaking connection data."
  [connections]
  (let [bundle (locking (:lock connections)
                 (let [state @(:state connections)]
                   (when-not (:closed? state)
                     (reset! (:state connections) (-> state (assoc :closed? true :restoring? false :bundle nil)
                                                       (dissoc :transition)))
                     (:bundle state))))]
    (when bundle
      (let [errors (close-bundle! bundle)]
        (locking (:lock connections)
          (swap! (:state connections) update :cleanup-errors into errors))))
    (status connections)))
