(ns tolglow.development
  "Application development resources, separate from the saved lighting show.
  Branch creation, source editing and checks operate only in owned workspaces."
  (:require [clojure.java.io :as io]
            [tolglow.development.terminal :as terminal]
            [tolglow.development.agents :as app-agents]
            [tolglow.development.workspace :as workspace]))

(def default-profiles
  {:tolglow-pure {:repo :tolglow :argv ["./bin/test" "pure"] :timeout-ms 300000}
   :afterglow-pure {:repo :afterglow :argv ["./bin/test" "pure"] :timeout-ms 300000}
   :tolglow-parity {:repo :tolglow :argv ["./bin/test" "parity"] :timeout-ms 300000}
   :afterglow-parity {:repo :afterglow :argv ["./bin/test" "parity"] :timeout-ms 300000}
   :studio-dependencies {:repo :tolglow :argv ["npm" "ci"] :timeout-ms 300000}
   :studio-tests {:repo :tolglow :argv ["npm" "run" "studio:test"] :timeout-ms 300000}
   :studio-build {:repo :tolglow :argv ["npm" "run" "studio:build"] :timeout-ms 300000}})

(defn default-options
  "Offer the workspace only when launched inside the paired source checkout.
  Installed packages can supply explicit repository paths instead. No Git or
  Docker process starts during discovery."
  []
  (let [root (.getCanonicalFile (io/file (System/getProperty "user.dir")))
        afterglow (.getCanonicalFile (io/file root "../afterglow"))]
    (when (and (.exists (io/file root ".git")) (.exists (io/file afterglow ".git"))
               (.isFile (io/file root "src/tolglow/studio.clj")))
      {:repositories {:tolglow (str root) :afterglow (str afterglow)}
       :workspace-parent (str (.getParentFile root) "/tolglow-workspaces")})))

(defn create
  "Create an inert workspace owner. Runner availability is explicit; a missing
  Docker runner must never fall back to executing project code on the host."
  [options]
  (when options
    (let [profiles (:check-profiles options default-profiles)
          required (:required-checks options (vec (remove #{:studio-dependencies} (keys profiles))))]
      (when-not (and (vector? required) (seq required) (every? #(contains? profiles %) required))
        (throw (ex-info "Development checks must name configured profiles" {})))
      (let [workspace-ref (atom nil)
            terminal (when (and (not (:runner options))
                                (not (and (contains? options :terminal-options) (nil? (:terminal-options options)))))
                       (terminal/create-manager
                        (assoc (merge {:network :bridge} (:terminal-options options))
                               :resolve-workspace #(workspace/runtime-paths @workspace-ref %))))
            runner (or (:runner options) (when terminal (terminal/runner terminal)))
            manager (try (workspace/create-manager (assoc options :check-profiles profiles :runner runner))
                         (catch Exception error (when terminal (terminal/close! terminal)) (throw error)))]
        (reset! workspace-ref manager)
        (try
          (workspace/recover! manager)
          (let [agent-options (:app-agent-options options {})
                agents (app-agents/create-manager
                        (merge {:config-path (str (.resolve ^java.nio.file.Path (:parent manager) ".tolglow-application-agents.edn"))
                                :check-profiles required
                                :execute! (when terminal (terminal/streaming-runner terminal))}
                               agent-options {:workspaces manager}))]
            (try
              (app-agents/restore-configuration! agents)
              {:workspaces manager :terminal terminal :profiles profiles :app-agents agents
               :required-checks required :runner? (boolean runner)}
              (catch Throwable error (app-agents/close! agents) (throw error))))
          (catch Throwable error
            (workspace/close! manager)
            (when terminal (terminal/close! terminal))
            (throw error)))))))

(defn- identity-string [id] (if (keyword? id) (subs (str id) 1) id))

(defn- keyword-id [id] (if (string? id) (keyword id) id))

(defn- integer-query [value default]
  (if (nil? value) default
      (if (integer? value) value (Long/parseLong value))))

(defn operation!
  "Ring/REPL facade. Repository and workspace roots are manager configuration;
  HTTP payloads only select stable identities and relative paths."
  [development operation payload]
  (if-not development
    (if (= operation :list) {:available? false :workspaces [] :profiles [] :runner-ready? false}
        (throw (ex-info "Application development requires configured source repositories" {:http/status 503})))
    (let [manager (:workspaces development) id (identity-string (:id payload))
          repo (keyword-id (:repo payload))]
      (case operation
        :list (let [availability (when-let [terminal (:terminal development)] (terminal/availability! terminal))]
                (assoc (workspace/status manager) :available? true
                     :runner-ready? (if availability (:available? availability) (:runner? development))
                     :terminal availability
                     :profiles (mapv (fn [[id profile]] (assoc (select-keys profile [:repo :argv :timeout-ms])
                                                              :id id :required? (boolean (some #{id} (:required-checks development)))))
                                     (sort-by key (:profiles development)))))
        :status (workspace/status manager id)
        :create (workspace/create! manager (cond-> (select-keys payload [:base-refs]) id (assoc :id id)))
        :tree (workspace/tree manager id repo {:offset (integer-query (:offset payload) 0)
                                               :limit (integer-query (:limit payload) 500)})
        :file (workspace/read-file manager id repo (:path payload))
        :diff (workspace/diff manager id)
        :write (workspace/write-file! manager id repo (:path payload)
                                       (cond-> (select-keys payload [:content :expected-hash])
                                         (= "missing" (:expected-hash payload)) (assoc :expected-hash :missing)))
        :check (let [run (workspace/check! manager id (keyword-id (:profile payload)))]
                 {:status :queued :run-id (:run-id run)})
        :stage (workspace/stage-preview! manager id {:required-checks (:required-checks development)})
        (throw (ex-info "Unknown development operation" {:operation operation}))))))

(defn close! [development]
  (when development
    (try (when-let [agents (:app-agents development)] (app-agents/close! agents))
         (finally
           (try (workspace/close! (:workspaces development))
                (finally (when-let [manager (:terminal development)] (terminal/close! manager))))))))

(defn app-agent-operation!
  "Application editing has its own provider configuration and task lifecycle.
  The public payload selects identities/policies, never executables or roots."
  [development operation payload]
  (let [manager (:app-agents development)]
    (if-not manager
      (if (= :list operation) {:available? false :tasks [] :providers [] :supported-providers [:codex]
                               :execution-configured? false :preview-available? false :activation-available? false}
          (throw (ex-info "Application agents require configured development repositories" {:http/status 503})))
      (let [id (identity-string (:id payload))
            payload (dissoc payload :runtime-id :expected-epoch)]
        (case operation
          :list (assoc (app-agents/status manager) :available? true
                       :runner-ready? (if-let [terminal (:terminal development)]
                                        (:available? (terminal/availability! terminal))
                                        (boolean (:execute! manager))))
          :status (app-agents/status manager id)
          :events (app-agents/read-events manager id (integer-query (:cursor payload) 0))
          :configure (app-agents/configure-provider! manager (cond-> payload
                                                             (:provider payload) (update :provider keyword-id)))
          :start (app-agents/start! manager (cond-> payload
                                             (:provider payload) (update :provider keyword-id)
                                             (:repo payload) (update :repo keyword-id)
                                             (:policy payload) (update :policy keyword-id)))
          :cancel (app-agents/cancel! manager id)
          (throw (ex-info "Unknown application-agent operation" {:operation operation})))))))

(defn terminal-operation!
  "A browser can select owned workspace/session IDs, never a Docker mount,
  executable, image or daemon option. Shell input is carried as bounded bytes."
  [development operation payload]
  (let [manager (:terminal development)]
    (when-not manager (throw (ex-info "Docker terminal is not configured" {:http/status 503})))
    (let [id (:session-id payload)]
      (case operation
        :status (terminal/status manager)
        :read (terminal/read-output manager id (integer-query (:cursor payload) 0))
        :open (terminal/open! manager (-> (select-keys payload [:workspace-id :repo :rows :cols])
                                           (update :workspace-id identity-string)
                                           (update :repo keyword-id)))
        :input (terminal/input! manager id (:data payload))
        :resize (terminal/resize! manager id (:rows payload) (:cols payload))
        :close (terminal/close-session! manager id)
        (throw (ex-info "Unknown terminal operation" {:operation operation}))))))
