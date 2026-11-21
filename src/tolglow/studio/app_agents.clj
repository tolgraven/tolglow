(ns tolglow.studio.app-agents
  "Application editing tasks, isolated from the lighting model and live checkout.
  Production execute! is terminal/streaming-runner; this service has no host CLI
  fallback. Constructing a manager starts no process, task, or credential read."
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [tolglow.studio.credentials :as credentials]
            [tolglow.studio.workspace :as workspace])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file Files Path Paths LinkOption StandardOpenOption StandardCopyOption]
           [java.nio.file.attribute FileAttribute PosixFilePermissions]
           [java.io PushbackReader StringReader]
           [java.util UUID]
           [java.util.concurrent ArrayBlockingQueue Executors ScheduledExecutorService
            ScheduledFuture ThreadFactory ThreadPoolExecutor ThreadPoolExecutor$AbortPolicy
            RejectedExecutionException TimeUnit]))

(defn- fail! [code message] (throw (ex-info message {:app-agent/error code})))
(defn- bounded-int? [x lo hi] (and (integer? x) (<= lo x hi)))
(defn- bounded-text? [s limit] (and (string? s) (<= 1 (count s) limit) (not (str/includes? s "\u0000"))))
(defn- clip [s limit] (let [s (str s)] (if (> (count s) limit) (str (subs s 0 limit) "…") s)))
(defn- redact [value secret]
  (cond (string? value) (if (seq secret) (str/replace value secret "[REDACTED]") value)
        (map? value) (into {} (map (fn [[k v]] [k (redact v secret)])) value)
        (vector? value) (mapv #(redact % secret) value)
        :else value))
(defn- task [manager id]
  (or (get-in @(:state manager) [:runs id]) (fail! :unknown-task "Unknown application-agent task")))

(defn- validate-providers! [providers]
  (when-not (and (map? providers) (every? #{:codex} (keys providers)))
    (fail! :invalid-provider "Only the verified Codex CLI adapter is configured"))
  (doseq [[_ provider] providers]
    (when-not (and (map? provider) (every? #{:credential-ref :model} (keys provider))
                   (credentials/valid-reference? (:credential-ref provider))
                   (or (nil? (:model provider))
                       (and (bounded-text? (:model provider) 128)
                            (boolean (re-matches #"[a-zA-Z0-9][a-zA-Z0-9._:/-]*" (:model provider))))))
      (fail! :invalid-provider "Codex configuration requires a credential reference and optional model identifier")))
  providers)

(defn create-manager
  [{:keys [workspaces execute! credential-store providers check-profiles workers max-runs
           timeout-ms max-output-bytes event-limit event-byte-limit now-ms config-path]
    :or {providers {} check-profiles [] workers 1 max-runs 32 timeout-ms 300000
         max-output-bytes 2097152 event-limit 256 event-byte-limit 524288 now-ms #(System/currentTimeMillis)}}]
  (when-not (and workspaces (or (nil? execute!) (ifn? execute!))
                 (map? providers) (every? #{:codex} (keys providers))
                 (vector? check-profiles) (every? #(contains? (:profiles workspaces) %) check-profiles)
                 (bounded-int? workers 1 2) (bounded-int? max-runs 1 128)
                 (bounded-int? timeout-ms 1 300000) (bounded-int? max-output-bytes 1024 8388608)
                 (bounded-int? event-limit 8 2048) (bounded-int? event-byte-limit 16384 8388608)
                 (ifn? now-ms) (or (nil? config-path) (string? config-path)))
    (fail! :invalid-options "Application-agent manager requires owned workspaces and bounded execution settings"))
  (validate-providers! providers)
  {:workspaces workspaces :execute! execute! :credentials credential-store :providers (atom providers)
   :config-path (when config-path (.normalize (.toAbsolutePath (Paths/get config-path (make-array String 0)))))
   :configuration-persisted? (atom false)
   :check-profiles check-profiles :timeout-ms timeout-ms :max-output-bytes max-output-bytes
   :max-runs max-runs :event-limit event-limit :event-byte-limit event-byte-limit :now-ms now-ms
   :lock (Object.) :state (atom {:closed? false :runs {} :order []})
   :executor (ThreadPoolExecutor. workers workers 30 TimeUnit/SECONDS (ArrayBlockingQueue. 4)
               (reify ThreadFactory (newThread [_ f] (doto (Thread. f "tolglow-application-agent") (.setDaemon true))))
               (ThreadPoolExecutor$AbortPolicy.))
   :observer (Executors/newSingleThreadScheduledExecutor
              (reify ThreadFactory (newThread [_ f] (doto (Thread. f "tolglow-application-diff-observer") (.setDaemon true)))) )})

(defn- emit! [manager run event]
  (let [event (assoc (redact event @(:secret run)) :time-ms ((:now-ms manager)))]
    (locking (:events run)
      (swap! (:events run)
             (fn [{:keys [cursor entries bytes]}]
               (let [event (assoc event :cursor (inc cursor))
                     size (alength (.getBytes (json/write-str event) StandardCharsets/UTF_8))]
                 (loop [entries (conj entries [event size]) bytes (+ bytes size)]
                   (if (or (> (count entries) (:event-limit manager)) (> bytes (:event-byte-limit manager)))
                     (recur (subvec entries 1) (- bytes (second (first entries))))
                     {:cursor (inc cursor) :entries entries :bytes bytes}))))))))

(defn- publish! [manager run values]
  (swap! (:public run) merge (redact values @(:secret run)))
  (emit! manager run (merge {:type :task/status} (select-keys values [:status :error :checks :candidate]))))

(defn status
  ([manager]
   {:closed? (:closed? @(:state manager)) :execution-configured? (boolean (:execute! manager))
    :providers (mapv (fn [[id config]] {:id id :adapter :codex-exec :configured? true :model (:model config)})
                     (sort-by key @(:providers manager)))
    :supported-providers [:codex] :configuration-persisted? @(:configuration-persisted? manager)
    :preview-available? false :activation-available? false
    :tasks (mapv #(deref (:public (task manager %))) (:order @(:state manager)))})
  ([manager id] @(:public (task manager id))))

(def ^:private no-links (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))
(def ^:private private-file (into-array FileAttribute [(PosixFilePermissions/asFileAttribute (PosixFilePermissions/fromString "rw-------"))]))
(defn- check-config-path! [^Path path]
  (loop [p path]
    (when p
      (when (Files/isSymbolicLink p) (fail! :unsafe-config "Application settings cannot traverse symbolic links"))
      (recur (.getParent p))))
  path)

(defn restore-configuration!
  "Explicitly restore references/settings only. No credential read or task
  execution. A missing file leaves configured defaults unchanged."
  [manager]
  (when-let [^Path path (:config-path manager)]
    (locking (:lock manager)
      (check-config-path! path)
      (when (Files/exists path no-links)
        (when-not (and (Files/isRegularFile path no-links) (<= (Files/size path) 65536))
          (fail! :invalid-config "Application settings must be a bounded regular file"))
        (let [bytes (with-open [input (Files/newInputStream path (into-array java.nio.file.OpenOption [LinkOption/NOFOLLOW_LINKS]))]
                      (.readNBytes input 65537))]
          (when (> (alength bytes) 65536) (fail! :invalid-config "Application settings grew beyond their bound"))
          (with-open [reader (PushbackReader. (StringReader. (String. bytes StandardCharsets/UTF_8)))]
            (let [eof (Object.) document (edn/read {:eof eof :readers {}} reader)]
              (when-not (and (map? document) (= #{:version :providers} (set (keys document)))
                             (= 1 (:version document))
                             (identical? eof (edn/read {:eof eof :readers {}} reader)))
                (fail! :invalid-config "Expected one versioned application settings document"))
              (reset! (:providers manager) (merge @(:providers manager) (validate-providers! (:providers document))))
              (reset! (:configuration-persisted? manager) true)))))))
  (select-keys (status manager) [:providers :supported-providers :configuration-persisted?]))

(defn- save-configuration! [manager providers]
  (when-let [^Path path (:config-path manager)]
    (check-config-path! path)
    (let [parent (.getParent path)
          _ (Files/createDirectories parent (make-array FileAttribute 0))
          tmp (Files/createTempFile parent ".application-settings-" ".tmp" private-file)]
      (try
        (Files/write tmp (.getBytes (pr-str {:version 1 :providers providers}) StandardCharsets/UTF_8)
                     (into-array java.nio.file.OpenOption [StandardOpenOption/WRITE StandardOpenOption/TRUNCATE_EXISTING]))
        (with-open [channel (java.nio.channels.FileChannel/open tmp (into-array java.nio.file.OpenOption [StandardOpenOption/WRITE]))]
          (.force channel true))
        (Files/move tmp path (into-array java.nio.file.CopyOption [StandardCopyOption/ATOMIC_MOVE StandardCopyOption/REPLACE_EXISTING]))
        (finally (Files/deleteIfExists tmp))))))

(defn configure-provider!
  "Store a new key in the separate credential vault, persist only its reference,
  and return a key-free configuration projection. Running tasks keep their
  captured provider configuration. Existing references are retained deliberately."
  [manager {:keys [provider api-key credential-ref model] :or {provider :codex} :as request}]
  (when-not (and (= :codex provider) (every? #{:provider :api-key :credential-ref :model} (keys request))
                 (not (and api-key credential-ref))
                 (or (nil? api-key) (and (bounded-text? api-key 8192) (not (re-find #"[\r\n]" api-key)))))
    (fail! :invalid-provider "Configure Codex with one credential source and optional model"))
  (locking (:lock manager)
    (when (:closed? @(:state manager)) (fail! :closed "Application-agent manager is closed"))
    (let [old (get @(:providers manager) provider)
          provisional (cond-> (assoc old :credential-ref (or credential-ref (:credential-ref old) "credential-00000000-0000-0000-0000-000000000000"))
                        (contains? request :model) (assoc :model model))
          _ (validate-providers! {provider provisional})
          new-ref (when api-key
                    (if-let [put! (get-in manager [:credentials :put!])] (put! api-key)
                            (fail! :credential-store-unavailable "A credential vault is required to save an API key")))
          config (assoc provisional :credential-ref (or new-ref credential-ref (:credential-ref old)))
          providers (assoc @(:providers manager) provider config)]
      (try
        (validate-providers! providers)
        (save-configuration! manager providers)
        (reset! (:providers manager) providers)
        (reset! (:configuration-persisted? manager) (some? (:config-path manager)))
        (select-keys (status manager) [:providers :supported-providers :configuration-persisted?])
        (catch Throwable error
          (when-let [delete! (and new-ref (get-in manager [:credentials :delete!]))] (delete! new-ref))
          (throw error))))))

(defn read-events
  "Read retained, ordered progress after cursor. A dropped flag means the client
  must refresh status/diff; reconnect never starts or repeats a task."
  [manager id cursor]
  (when-not (bounded-int? cursor 0 Long/MAX_VALUE) (fail! :invalid-cursor "Cursor must be a nonnegative integer"))
  (let [run (task manager id) {:keys [entries] latest :cursor} @(:events run)
        first-cursor (or (get-in entries [0 0 :cursor]) (inc latest))]
    (when (> cursor latest) (fail! :invalid-cursor "Cursor is ahead of this task's event stream"))
    {:task-id id :cursor latest :first-cursor first-cursor :dropped? (< cursor (dec first-cursor))
     :events (mapv first (filter #(> (get-in % [0 :cursor]) cursor) entries))
     :status (:status @(:public run))}))

(defn result "Return the task's completion promise; callers choose their own bounded wait." [manager id]
  (:result (task manager id)))

(defn- cancelled! [run]
  (when @(:cancelled? run) (throw (InterruptedException. "Application task cancelled"))))

(defn- item-event [event]
  (let [item (get event "item") type (get item "type")
        base {:type :agent/item :phase (get event "type") :item-type type
              :item-id (clip (get item "id" "") 128)}]
    (when-not (and (map? item) (string? type)) (fail! :protocol "Malformed Codex item event"))
    (case type
      ("agent_message" "reasoning") (assoc base :text (clip (get item "text" "") 8192))
      "command_execution" (assoc base :command (clip (get item "command" "") 2048)
                                 :output (clip (get item "aggregated_output" "") 8192)
                                 :status (get item "status") :exit-code (get item "exit_code"))
      "file_change" (assoc base :status (get item "status")
                           :changes (mapv (fn [change] {:path (clip (get change "path" "") 1024)
                                                       :kind (get change "kind")}) (take 128 (get item "changes"))))
      "mcp_tool_call" (assoc base :server (clip (get item "server" "") 256)
                             :tool (clip (get item "tool" "") 256) :status (get item "status"))
      "web_search" (assoc base :query (clip (get item "query" "") 2048))
      "todo_list" (assoc base :items (mapv (fn [item] {:text (clip (get item "text" "") 512)
                                                     :completed? (true? (get item "completed"))})
                                          (take 64 (get item "items"))))
      "error" (assoc base :message (clip (get item "message" "") 2048))
      (assoc base :unsupported? true))))

(defn- receive-line! [manager run stream line]
  ;; The runner owns its reader threads; this callback only parses bounded data
  ;; and updates bounded memory. Git inspection belongs to the observer worker.
  (when-not @(:finished? run)
    (if (= stream :stderr)
      (emit! manager run {:type :agent/stderr :text (clip line 4096)})
      (try
        (when (> (count line) 65536) (fail! :protocol "Codex event exceeded line limit"))
        (let [event (json/read-str line) type (get event "type")
              projected
              (case type
                "thread.started" {:type :agent/thread :thread-id (clip (get event "thread_id" "") 128)}
                "turn.started" {:type :agent/turn :phase :started}
                "turn.completed" (do (swap! (:protocol run) assoc :completed? true)
                                     {:type :agent/turn :phase :completed
                                      :usage (into {} (for [key ["input_tokens" "cached_input_tokens" "output_tokens" "reasoning_output_tokens"]
                                                           :let [n (get-in event ["usage" key])] :when (and (integer? n) (<= 0 n))]
                                                       [(keyword key) n]))})
                "turn.failed" (do (swap! (:protocol run) assoc :failed? true)
                                  {:type :agent/turn :phase :failed :message (clip (get-in event ["error" "message"] "Turn failed") 2048)})
                "error" (do (swap! (:protocol run) assoc :failed? true)
                            {:type :agent/error :message (clip (get event "message" "Agent error") 2048)})
                ("item.started" "item.updated" "item.completed") (item-event event)
                (if (string? type) {:type :agent/unknown-event :event-type (clip type 128)}
                    (fail! :protocol "Codex event has no type")))]
          (emit! manager run projected))
        (catch Exception _
          (swap! (:protocol run) update :errors (fnil inc 0))
          (emit! manager run {:type :agent/protocol-error :message "Malformed or oversized Codex event"}))))))

(defn- observe! [manager run]
  (when-let [id (:workspace-id @(:public run))]
    (let [diff (workspace/diff (:workspaces manager) id)
          changes (into (sorted-map)
                        (for [[repo data] (:repositories diff)]
                          [repo (mapv (fn [line] {:status (subs line 0 (min 2 (count line)))
                                                 :path (clip (subs line (min 3 (count line))) 1024)})
                                      (take 256 (remove str/blank? (str/split-lines (:status data)))))]))
          summary {:repositories changes :file-count (reduce + (map count (vals changes)))
                   :observed-at-ms ((:now-ms manager)) :source :git-worktree}]
      (when (and (not @(:finished? run)) (not= changes @(:observed run)))
        (reset! (:observed run) changes)
        (swap! (:public run) assoc :summary summary)
        (emit! manager run {:type :workspace/changes :summary summary}))
      summary)))

(defn- finish! [manager run values]
  (when (compare-and-set! (:finished? run) false true)
    (publish! manager run (assoc values :finished-at-ms ((:now-ms manager))))
    (deliver (:result run) @(:public run))
    ;; Retain only redacted public state/events, never the credential value.
    (reset! (:secret run) nil)))

(defn- codex-request [manager run config paths prompt repo]
  (let [cwd (get-in paths [:repositories repo :path])
        others (sort (remove #{cwd} (map :path (vals (:repositories paths)))))
        argv (vec (concat ["codex" "--no-daemon" "-a" "never" "exec" "--json" "--ephemeral"
                           "--ignore-user-config" "--sandbox" "workspace-write" "-C" cwd]
                          (mapcat #(vector "--add-dir" %) others)
                          (when-let [model (:model config)] ["--model" model]) ["-"]))]
    {:workspace-id (:workspace-id paths) :workspace-root (:workspace-root paths) :cwd cwd
     :argv argv :stdin prompt :env {"CODEX_API_KEY" @(:secret run)}
     :timeout-ms (:timeout-ms manager) :max-output-bytes (:max-output-bytes manager) :max-line-bytes 65536
     :on-line! #(receive-line! manager run %1 %2)}))

(defn- checks! [manager run]
  (publish! manager run {:status :checking})
  (let [id (:workspace-id @(:public run))
        checks (into (sorted-map)
                     (for [profile (:check-profiles manager)]
                       (do
                         (cancelled! run)
                         (let [check (workspace/check! (:workspaces manager) id profile)]
                           (reset! (:check-cancel run) (:cancel! check))
                           (try
                             (let [value (deref (:result check) 310000 {:status :timed-out})]
                               (when (= :timed-out (:status value)) ((:cancel! check)))
                               (emit! manager run {:type :workspace/check :profile profile
                                                  :result (select-keys value [:status :exit-code :run-id :error])})
                               [profile (select-keys value [:status :exit-code :run-id :error])])
                             (catch InterruptedException error
                               ((:cancel! check))
                               (throw error))
                             (finally (reset! (:check-cancel run) nil)))))))]
    (cancelled! run)
    {:checks checks :candidate (when (every? #(= :passed (:status %)) (vals checks))
                                (workspace/stage-preview! (:workspaces manager) id
                                                          {:required-checks (:check-profiles manager)}))}))

(defn- run-task! [manager run request]
  (let [observer (atom nil)]
    (try
      (locking (:lock manager)
        (cancelled! run)
        (reset! (:thread run) (Thread/currentThread)))
      (let [{:keys [provider prompt repo policy base-refs]} request
            config (:provider-config run)
            secret (when-let [get! (get-in manager [:credentials :get!])] (get! (:credential-ref config)))]
        (when-not (and (bounded-text? secret 8192) (not (re-find #"[\r\n]" secret)))
          (fail! :credential-unavailable "Configured Codex credential is unavailable"))
        (reset! (:secret run) secret)
        (cancelled! run)
        (publish! manager run {:status :preparing})
        (let [id (:id @(:public run))
              created (workspace/create! (:workspaces manager) {:id id :base-refs (or base-refs {})})
              paths (workspace/runtime-paths (:workspaces manager) id)]
          (publish! manager run {:status :running :workspace-id id :workspace created})
          (cancelled! run)
          (reset! observer
                  (.scheduleWithFixedDelay ^ScheduledExecutorService (:observer manager)
                                           ^Runnable #(try (observe! manager run) (catch Exception _))
                                           0 1000 TimeUnit/MILLISECONDS))
          (let [response ((:execute! manager) (codex-request manager run config paths prompt repo))]
            (cancelled! run)
            (when-let [^ScheduledFuture observer @observer] (.cancel observer false))
            (observe! manager run)
            (cond
              (:timed-out? response) (finish! manager run {:status :failed :error {:code :timed-out :message "Agent execution reached its time limit"}})
              (:truncated? response) (finish! manager run {:status :failed :error {:code :output-limit :message "Agent output exceeded its bound"}})
              (or (not= 0 (:exit-code response)) (:failed? @(:protocol run))
                  (pos? (:errors @(:protocol run) 0)) (not (:completed? @(:protocol run))))
              (finish! manager run {:status :failed :error {:code :execution-failed
                                                          :message "Agent did not complete a valid successful turn"
                                                          :exit-code (:exit-code response)}})
              :else (let [checks (when (= policy :after-turn) (checks! manager run))]
                      (finish! manager run (merge {:status :completed :application-state :unchanged}
                                                 checks)))))))
      (catch InterruptedException _
        (when-let [cancel! @(:check-cancel run)] (cancel!))
        (finish! manager run {:status :cancelled}))
      (catch Throwable error
        (finish! manager run {:status (if @(:cancelled? run) :cancelled :failed)
                             :error {:code (or (:app-agent/error (ex-data error)) (:workspace/error (ex-data error)) :task-failed)
                                     :message (clip (.getMessage error) 2048)}}))
      (finally
        (when-let [^ScheduledFuture observer @observer] (.cancel observer false))
        (reset! (:thread run) nil)
        (reset! (:secret run) nil)))))

(defn start!
  "Queue a new isolated task; never reuse a live checkout or existing CLI thread.
  Preview is explicitly unavailable until a separate preview lifecycle exists."
  [manager {:keys [provider prompt repo policy base-refs] :or {provider :codex repo :tolglow policy :manual} :as request}]
  (when-not (and (every? #{:provider :prompt :repo :policy :base-refs} (keys request))
                 (contains? @(:providers manager) provider) (contains? (:repositories (:workspaces manager)) repo)
                 (bounded-text? prompt 131072) (#{:manual :after-turn :preview} policy)
                 (or (nil? base-refs) (map? base-refs)))
    (fail! :invalid-request "Select a configured provider, owned repository, bounded prompt and supported policy"))
  (when-not (:execute! manager) (fail! :execution-unavailable "An isolated CLI execution adapter is not configured"))
  (when (= :preview policy) (fail! :preview-unavailable "Preview process activation is not implemented"))
  (when (and (= :after-turn policy) (empty? (:check-profiles manager)))
    (fail! :checks-unavailable "After-turn policy requires configured fixed checks"))
  (let [id (str "agent-" (subs (str (UUID/randomUUID)) 0 12))
        run {:public (atom {:id id :status :queued :provider provider :repo repo :policy policy
                           :prompt prompt :created-at-ms ((:now-ms manager)) :application-state :unchanged})
             :events (atom {:cursor 0 :entries [] :bytes 0}) :protocol (atom {}) :observed (atom nil)
             :result (promise) :cancelled? (atom false) :finished? (atom false) :thread (atom nil)
             :secret (atom nil) :check-cancel (atom nil) :provider-config (get @(:providers manager) provider)}]
    (locking (:lock manager)
      (when (:closed? @(:state manager)) (fail! :closed "Application-agent manager is closed"))
      (when (>= (count (:runs @(:state manager))) (:max-runs manager)) (fail! :capacity "Application task history is full"))
      (swap! (:state manager) #(-> % (assoc-in [:runs id] run) (update :order conj id)))
      (emit! manager run {:type :task/status :status :queued})
      (try
        (.execute ^ThreadPoolExecutor (:executor manager) ^Runnable #(run-task! manager run (assoc request :provider provider :repo repo :policy policy)))
        (catch RejectedExecutionException _
          (finish! manager run {:status :failed :error {:code :capacity :message "Application worker queue is full"}}))))
    @(:public run)))

(defn cancel! [manager id]
  (let [run (task manager id)]
    (locking (:lock manager)
      (when-not @(:finished? run)
        (reset! (:cancelled? run) true)
        (if-let [^Thread worker @(:thread run)]
          (do (publish! manager run {:status :cancelling}) (.interrupt worker))
          (finish! manager run {:status :cancelled}))))
    @(:public run)))

(defn close!
  "Cancel only owned agent tasks; retain worktrees/diffs and leave workspace,
  terminal and lighting owners to their own lifecycle."
  [manager]
  (locking (:lock manager)
    (swap! (:state manager) assoc :closed? true)
    (doseq [id (:order @(:state manager))] (cancel! manager id)))
  (.shutdownNow ^ThreadPoolExecutor (:executor manager))
  (.shutdownNow ^ScheduledExecutorService (:observer manager))
  nil)
