(ns tolglow.development.terminal
  "Explicit Docker development boundary. Construction is inert. Only a trusted
  workspace resolver supplies host mounts; browser input never chooses mounts,
  image, Docker flags or a host executable. PTYs, output and cleanup are owned."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [tolglow.development.workspace :as workspace])
  (:import [java.nio.file Files Path Paths LinkOption]
           [java.nio.charset StandardCharsets]
           [java.io BufferedReader InputStreamReader OutputStreamWriter]
           [java.util Base64 UUID]
           [java.util.concurrent TimeUnit ArrayBlockingQueue]
           [com.sun.security.auth.module UnixSystem]))

(defn- fail! [code message] (throw (ex-info message {:terminal/error code})))
(defn- clean-string? [s] (and (string? s) (<= 1 (count s) 8192) (not (re-find #"[\x00-\x1f]" s))))
(defn- bounded-int? [x lo hi] (and (integer? x) (<= lo x hi)))
(defn- encode [bytes] (.encodeToString (Base64/getEncoder) ^bytes bytes))
(defn- decode [value]
  (when-not (and (string? value) (<= (count value) 87384)) (fail! :invalid-input "Input must be bounded base64 bytes"))
  (try (let [bytes (.decode (Base64/getDecoder) ^String value)]
         (when (> (alength bytes) 65536) (fail! :input-limit "Terminal input exceeds 64 KiB")) bytes)
       (catch IllegalArgumentException _ (fail! :invalid-input "Invalid base64 terminal bytes"))))
(defn- checked-path [s]
  (when-not (and (clean-string? s) (not (str/includes? s ","))) (fail! :invalid-directory "Mount paths must be absolute, without controls or commas"))
  (let [p (Paths/get s (make-array String 0))]
    (when-not (and (.isAbsolute p) (= p (.normalize p)) (= p (.toRealPath p (make-array LinkOption 0)))
                   (Files/isDirectory p (make-array LinkOption 0)) (> (.getNameCount p) 1))
      (fail! :invalid-directory "Workspace mounts must be existing canonical directories")) p))

(defn create-manager
  "Construct an inert service. resolve-workspace takes a workspace ID and returns
  workspace/runtime-paths data. image/docker/python/bridge-path are trusted host
  options, never HTTP fields. Network defaults to :none; :bridge is an explicit
  administrator choice for dependency downloads. No ports are published."
  [{:keys [resolve-workspace image docker python bridge-path max-sessions output-bytes network uid gid create-timeout-ms]
    :or {image "tolglow-dev:local" docker "docker" python "/usr/bin/python3"
         max-sessions 4 output-bytes 1048576 network :none create-timeout-ms 120000}}]
  (when-not (and (ifn? resolve-workspace) (clean-string? image) (not (str/starts-with? image "-"))
                 (clean-string? docker) (clean-string? python) (#{:none :bridge} network)
                 (bounded-int? max-sessions 1 8) (bounded-int? output-bytes 4096 8388608)
                 (bounded-int? create-timeout-ms 1 300000)
                 (or (nil? uid) (bounded-int? uid 1 2147483647)) (or (nil? gid) (bounded-int? gid 1 2147483647)))
    (fail! :invalid-options "Terminal service needs a trusted resolver and bounded configuration"))
  (let [unix (UnixSystem.)]
    {:resolve-workspace resolve-workspace :image image :docker docker :python python
     :bridge-path bridge-path
     :uid (or uid (.getUid unix)) :gid (or gid (.getGid unix)) :network network
     :max-sessions max-sessions :output-bytes output-bytes :id (str (UUID/randomUUID)) :lock (Object.)
     :create-timeout-ms create-timeout-ms
     :state (atom {:closed? false :sessions {} :jobs {} :containers {}})}))

(defn- bridge-file! [manager]
  (locking (:lock manager)
    (when (:closed? @(:state manager)) (fail! :closed "Terminal manager is closed"))
    (let [path (or (:bridge-path manager) (:bridge-temp @(:state manager)))]
      (if path
        (if (.isFile (io/file path)) (str path)
            (fail! :bridge-unavailable "Configured PTY transport script is unavailable"))
        (if-let [resource (io/resource "tolglow/terminal/pty_bridge.py")]
          (if (= "file" (.getProtocol resource))
            (str (io/file resource))
            (let [path (Files/createTempFile "tolglow-pty-" ".py" (make-array java.nio.file.attribute.FileAttribute 0))]
              (try
                (with-open [input (io/input-stream resource) output (io/output-stream (.toFile path))]
                  (io/copy input output))
                (swap! (:state manager) assoc :bridge-temp (str path))
                (str path)
                (catch Throwable error (Files/deleteIfExists path) (throw error)))))
          (fail! :bridge-unavailable "Packaged PTY transport script is unavailable"))))))

(defn- resolved [manager id]
  (when (:closed? @(:state manager)) (fail! :closed "Terminal manager is closed"))
  (when-not (and (string? id) (re-matches #"[a-zA-Z0-9][a-zA-Z0-9_-]{0,63}" id)) (fail! :invalid-workspace "Invalid workspace identity"))
  (let [data ((:resolve-workspace manager) id) root (checked-path (:workspace-root data))
        repos (:repositories data)]
    (when-not (and (= id (:workspace-id data)) (map? repos) (<= 1 (count repos) 8))
      (fail! :invalid-workspace "Resolver did not return the requested owned workspace"))
    (doseq [[_ repo] repos]
      (let [p (checked-path (:path repo)) common (checked-path (:common-dir repo)) admin (checked-path (:git-dir repo))]
        (when-not (and (.startsWith p root) (not= p root) (.startsWith admin (.resolve common "worktrees"))
                       (not (.startsWith common root)) (not (.startsWith root common)))
          (fail! :invalid-directory "Repository and Git administration mounts do not match the owned workspace"))))
    data))

(defn- run-host! [manager args timeout]
  (workspace/run-trusted-process! {:argv (into [(:docker manager)] args) :cwd "/tmp" :timeout-ms timeout :max-output-bytes 65536}))
(defn- ensure-image! [manager]
  (let [result (run-host! manager ["image" "inspect" "--format" "{{.Id}}" (:image manager)] 10000)]
    (when-not (= 0 (:exit-code result))
      (throw (ex-info "Development image is unavailable; build tolglow-dev:local explicitly. No host-shell fallback is used."
                      {:terminal/error :image-unavailable :output (:output result)})))))
(defn availability!
  "Explicit bounded Docker/image probe. Does not pull/build an image or start a
  container. Hosts call off their model owner; status remains entirely inert."
  [manager]
  (try (ensure-image! manager) {:available? true :image (:image manager) :network (:network manager)}
       (catch Exception error
         {:available? false :image (:image manager) :code (or (:terminal/error (ex-data error)) :docker-unavailable)
          :message "Docker and the configured development image must be available. Build the image explicitly; no host-shell fallback is used."
          :build-command "docker build -t tolglow-dev:local -f dev/container/Dockerfile dev/container"})))
(defn- container-name [manager] (str "tolglow-dev-" (subs (:id manager) 0 8) "-" (subs (str (UUID/randomUUID)) 0 12)))
(defn- cleanup-container! [manager name]
  ;; A cancelled `docker create` can finish on the daemon long after its caller
  ;; gives up. Never remove before creation settles, and never abandon ownership.
  (when-let [entry (get-in @(:state manager) [:containers name])]
    (let [claimed? (locking entry
                     (swap! entry assoc :cancelled? true)
                     (when (and (not= :creating (:phase @entry)) (not (:cleanup-running? @entry)))
                       (swap! entry assoc :cleanup-running? true :phase :removing)
                       true))]
      (when claimed?
        (let [interrupted? (Thread/interrupted)]
          (try
            (let [result (run-host! manager ["rm" "--force" name] 10000)
                  removed? (and (not (:timed-out? result))
                                (or (= 0 (:exit-code result))
                                    (str/includes? (:output result "") "No such container")))]
              (if removed?
                (do (swap! entry assoc :phase :removed)
                    (swap! (:state manager) update :containers dissoc name)
                    (deliver (:removed @entry) true))
                (swap! entry assoc :phase :cleanup-failed :cleanup-running? false
                       :error "Docker could not remove the owned container; cleanup remains pending"))
              removed?)
            (catch Exception _
              (swap! entry assoc :phase :cleanup-failed :cleanup-running? false
                     :error "Docker cleanup failed; ownership is retained")
              false)
            (finally (when interrupted? (.interrupt (Thread/currentThread))))))))))
(defn- mount-arguments [data]
  (vec (concat ["--mount" (str "type=bind,src=" (:workspace-root data) ",dst=" (:workspace-root data))]
               (mapcat (fn [p] ["--mount" (str "type=bind,src=" p ",dst=" p ",readonly")])
                       (distinct (map :common-dir (vals (:repositories data)))))
               (mapcat (fn [repo] ["--mount" (str "type=bind,src=" (:git-dir repo) ",dst=" (:git-dir repo))])
                       (vals (:repositories data))))))
(defn- docker-argv [manager data name cwd interactive? command & [environment-names]]
  (vec (concat [(:docker manager) "create" "--init" "--name" name "--label" (str "tolglow.owner=" (:id manager))
                "--network" (clojure.core/name (:network manager)) "--read-only" "--cap-drop" "ALL"
                "--security-opt" "no-new-privileges" "--pids-limit" "256" "--memory" "2g" "--cpus" "2"
                "--user" (str (:uid manager) ":" (:gid manager))
                "--tmpfs" "/tmp:rw,nosuid,nodev,size=536870912,mode=1777"
                "--tmpfs" (str "/home/tolglow:rw,nosuid,nodev,size=268435456,mode=700,uid=" (:uid manager) ",gid=" (:gid manager))
                "--env" "HOME=/home/tolglow" "--env" "TERM=xterm-256color" "--env" "GIT_OPTIONAL_LOCKS=0"
                "--workdir" cwd "-i"]
               (when interactive? ["-t"])
               (mount-arguments data) (mapcat (fn [k] ["--env" k]) environment-names) [(:image manager)] command)))

(def ^:private provider-environment
  #{"CODEX_API_KEY" "OPENAI_API_KEY" "ANTHROPIC_API_KEY" "OPENROUTER_API_KEY" "OPENAI_BASE_URL" "ANTHROPIC_BASE_URL"})

(defn- create-process! [argv environment]
  ;; Runs on a separately owned thread. Its lifetime is intentionally not
  ;; tied to an interrupted HTTP/check worker: killing the create CLI loses the
  ;; only reliable completion boundary for a slow Docker/VFS daemon operation.
  (let [builder (doto (ProcessBuilder. ^java.util.List argv)
                  (.directory (io/file "/tmp")) (.redirectErrorStream true))
        _ (doseq [[k v] environment] (.put (.environment builder) k v))
        process (.start builder) output (java.io.ByteArrayOutputStream.)
        reader (doto (Thread. (fn []
                               (try (with-open [input (.getInputStream process)]
                                      (let [buffer (byte-array 4096)]
                                        (loop []
                                          (let [n (.read input buffer)]
                                            (when (pos? n)
                                              (locking output
                                                (let [remaining (- 65536 (.size output))]
                                                  (when (pos? remaining) (.write output buffer 0 (min remaining n)))))
                                              (recur))))))
                                    (catch Exception _))) "tolglow-container-create-output")
                 (.setDaemon true) (.start))]
    (.close (.getOutputStream process))
    (let [exit-code (.waitFor process)]
      (.join reader 1000)
      {:exit-code exit-code :output (locking output (.toString output "UTF-8"))})))

(defn- create-container! [manager name argv environment]
  (let [result (promise) entry (atom {:phase :creating :result result :removed (promise) :cancelled? false})
        creator (Thread.
                 (fn []
                   (let [outcome (try (create-process! argv environment)
                                      (catch Throwable error {:error error}))]
                     (locking entry (swap! entry assoc :phase :created))
                     (deliver result outcome)
                     (when (:cancelled? @entry) (cleanup-container! manager name))))
                 (str "tolglow-container-create-" name))]
    (locking (:lock manager)
      (when (:closed? @(:state manager)) (fail! :closed "Terminal manager is closed"))
      ;; Include pending creates in capacity even after their callers cancel.
      (when (>= (count (:containers @(:state manager))) (+ 4 (:max-sessions manager)))
        (fail! :capacity "Container ownership capacity is full; pending cleanup must finish"))
      (swap! (:state manager) assoc-in [:containers name] entry)
      ;; Keep the cleanup owner alive through ordinary JVM shutdown as well.
      ;; An unresolved Docker create is observable in status, not silently lost
      ;; when the last request thread returns.
      (.setDaemon creator false) (.start creator))
    (try
      (let [outcome (deref result (:create-timeout-ms manager) ::timeout)]
        (cond
          (= ::timeout outcome) (fail! :create-timeout "Docker creation exceeded its wait budget; cleanup remains owned until creation settles")
          (:error outcome) (throw (:error outcome))
          (not= 0 (:exit-code outcome)) (throw (ex-info "Docker container creation failed" {:terminal/error :create-failed :output (:output outcome)}))
          (or (:closed? @(:state manager)) (:cancelled? @entry) (.isInterrupted (Thread/currentThread)))
          (throw (InterruptedException. "Container creation was cancelled"))
          :else true))
      (catch Throwable error
        (cleanup-container! manager name)
        (throw error)))))

(defn- start-argv [manager name] [(:docker manager) "start" "--attach" "--interactive" name])

(defn- stream-process!
  [argv {:keys [env stdin timeout-ms max-output-bytes on-line! max-line-bytes]
         :or {max-line-bytes 262144}}]
  (let [builder (doto (ProcessBuilder. ^java.util.List argv) (.directory (io/file "/tmp")))
        _ (doseq [[k v] env] (.put (.environment builder) k v))
        process (.start builder) output (java.io.ByteArrayOutputStream.) count-bytes (atom 0)
        truncated? (atom false) callback-error (atom nil)
        writer (Thread. (fn [] (try (with-open [out (.getOutputStream process)]
                                     (when stdin (.write out (.getBytes ^String stdin StandardCharsets/UTF_8))))
                                  (catch Exception _))) "tolglow-agent-input")
        read-stream (fn [stream input]
                      (doto
                        (Thread.
                          (fn []
                            (try
                              (with-open [input input]
                                (let [buffer (byte-array 8192) line (java.io.ByteArrayOutputStream.) discard? (atom false)]
                                  (loop []
                                    (let [n (.read input buffer)]
                                      (if (neg? n)
                                        (when (and (pos? (.size line)) (not @discard?)) (on-line! stream (.toString line "UTF-8")))
                                        (do
                                          (let [total (swap! count-bytes + n) allowed (max 0 (min n (- max-output-bytes (- total n))))]
                                            (when (< allowed n) (reset! truncated? true))
                                            (locking output (.write output buffer 0 allowed))
                                            (dotimes [i n]
                                              (let [b (bit-and 255 (aget buffer i))]
                                                (when (>= i allowed) (reset! discard? true))
                                                (if (= 10 b)
                                                  (do (when-not @discard? (on-line! stream (.toString line "UTF-8")))
                                                      (.reset line) (reset! discard? (>= (+ (- total n) i) max-output-bytes)))
                                                  (if (and (not @discard?) (< (.size line) max-line-bytes))
                                                    (.write line b)
                                                    (do (reset! truncated? true) (reset! discard? true)))))))
                                          (recur)))))))
                              (catch Exception error
                                (reset! callback-error error) (.destroyForcibly process))))
                          (str "tolglow-agent-" (name stream)))
                        (.setDaemon true) (.start)))
        stdout (read-stream :stdout (.getInputStream process)) stderr (read-stream :stderr (.getErrorStream process))]
    (.setDaemon writer true) (.start writer)
    (try
      (let [finished? (.waitFor process timeout-ms TimeUnit/MILLISECONDS)]
        (when-not finished? (.destroyForcibly process) (.waitFor process 1000 TimeUnit/MILLISECONDS))
        (.join stdout 1000) (.join stderr 1000)
        (when @callback-error (throw (ex-info "Stream callback or pipe failed" {:terminal/error :stream-failed} @callback-error)))
        {:exit-code (when-not (.isAlive process) (.exitValue process))
         :output (locking output (.toString output "UTF-8")) :timed-out? (not finished?)
         :truncated? (or @truncated? (.isAlive stdout) (.isAlive stderr))})
      (catch InterruptedException error (.destroyForcibly process) (.interrupt (Thread/currentThread)) (throw error))
      (finally
        (when (.isAlive process) (.destroyForcibly process))
        (.interrupt writer) (.interrupt stdout) (.interrupt stderr)
        (try (.close (.getInputStream process)) (catch Exception _))
        (try (.close (.getErrorStream process)) (catch Exception _))))))

(defn streaming-runner
  "Blocking isolated CLI runner with separate bounded stdout/stderr lines.
  on-line! receives [stream-keyword line] from owned reader threads and must return
  promptly. Secrets travel via the child environment and Docker --env NAME only;
  values never enter argv. Only explicit provider environment names are accepted.
  No CLI is run by constructing this function."
  [manager]
  (fn [{:keys [workspace-id workspace-root cwd argv timeout-ms max-output-bytes stdin env on-line! max-line-bytes]
        :or {env {} max-line-bytes 262144} :as request}]
    (when-not (and (vector? argv) (seq argv) (every? clean-string? argv) (ifn? on-line!)
                   (bounded-int? timeout-ms 1 300000) (bounded-int? max-output-bytes 1 8388608)
                   (bounded-int? max-line-bytes 1024 1048576)
                   (or (nil? stdin) (and (string? stdin) (<= (count stdin) 1048576)))
                   (map? env) (every? #(and (provider-environment (key %)) (string? (val %))
                                          (<= (count (val %)) 8192) (not (str/includes? (val %) "\u0000"))) env))
      (fail! :invalid-stream "Invalid bounded CLI stream request or provider environment"))
    (let [data (resolved manager workspace-id) name (container-name manager)]
      (when-not (and (= workspace-root (:workspace-root data)) (some #(= cwd (:path %)) (vals (:repositories data))))
        (fail! :invalid-directory "CLI cwd must be one of the owned paired repository roots"))
      (ensure-image! manager)
      (locking (:lock manager)
        (when (:closed? @(:state manager)) (fail! :closed "Terminal manager is closed"))
        (when (>= (count (:jobs @(:state manager))) 4) (fail! :capacity "Container worker capacity is full"))
        (swap! (:state manager) assoc-in [:jobs name] {:thread (Thread/currentThread) :workspace-id workspace-id}))
      (try (create-container! manager name (docker-argv manager data name cwd false argv (sort (keys env))) env)
           (stream-process! (start-argv manager name) (dissoc request :env))
           (finally (cleanup-container! manager name) (swap! (:state manager) update :jobs dissoc name))))))

(defn runner
  "Return the blocking fixed-argv check runner. Calls run on workspace's bounded
  worker, never a show owner. Timeout, interrupt and manager close remove only the
  uniquely named owned container. There is no network or host process fallback."
  [manager]
  (fn [{:keys [workspace-id workspace-root cwd argv timeout-ms max-output-bytes stdin] :as request}]
    (when-not (and (vector? argv) (seq argv) (every? clean-string? argv)
                   (bounded-int? timeout-ms 1 300000) (bounded-int? max-output-bytes 1 8388608)
                   (or (nil? stdin) (and (string? stdin) (<= (count stdin) 1048576))))
      (fail! :invalid-check "Invalid fixed check request"))
    (let [data (resolved manager workspace-id) name (container-name manager)
          _ (when-not (and (= workspace-root (:workspace-root data))
                           (some #(= cwd (:path %)) (vals (:repositories data))))
              (fail! :invalid-directory "Check cwd must be one of the owned paired repository roots"))]
      (ensure-image! manager)
      (locking (:lock manager)
        (when (:closed? @(:state manager)) (fail! :closed "Terminal manager is closed"))
        (when (>= (count (:jobs @(:state manager))) 4) (fail! :capacity "Container check capacity is full"))
        (swap! (:state manager) assoc-in [:jobs name] {:thread (Thread/currentThread) :workspace-id workspace-id}))
      (try (create-container! manager name (docker-argv manager data name cwd false argv) {})
           (workspace/run-trusted-process! (assoc (select-keys request [:timeout-ms :max-output-bytes :stdin])
                                                 :argv (start-argv manager name) :cwd "/tmp"))
           (finally (cleanup-container! manager name) (swap! (:state manager) update :jobs dissoc name))))))

(defn- public-session [session]
  (select-keys session [:session-id :workspace-id :repo :cwd :state :rows :cols :exit-code :error :started-at-ms :closed-at-ms :cursor :first-cursor]))
(defn status [manager]
  {:closed? (:closed? @(:state manager)) :image (:image manager) :network (:network manager)
   :sessions (mapv public-session (sort-by :started-at-ms (vals (:sessions @(:state manager)))))
   :running-checks (count (:jobs @(:state manager)))
   :pending-containers (mapv (fn [[name entry]] (assoc (select-keys @entry [:phase :cancelled? :error]) :name name))
                             (:containers @(:state manager)))})
(defn- session [manager id]
  (or (get-in @(:state manager) [:sessions id]) (fail! :unknown-session "Unknown owned terminal session")))

(defn- append-output [session bytes capacity]
  (let [n (alength ^bytes bytes) end (+ (:cursor session) n)
        chunks (conj (:chunks session) {:start (:cursor session) :bytes bytes})
        cutoff (max 0 (- end capacity))
        chunks (vec (drop-while #(<= (+ (:start %) (alength ^bytes (:bytes %))) cutoff) chunks))
        first-chunk (first chunks)
        chunks (if (and first-chunk (< (:start first-chunk) cutoff))
                 (assoc chunks 0 {:start cutoff :bytes (java.util.Arrays/copyOfRange ^bytes (:bytes first-chunk)
                                                                                    (int (- cutoff (:start first-chunk))) (alength ^bytes (:bytes first-chunk)))}) chunks)]
    (assoc session :chunks chunks :cursor end :first-cursor (or (:start (first chunks)) end))))

(defn- send! [session message]
  (let [queue ^ArrayBlockingQueue (:input-queue session)]
    (when-not queue (fail! :not-running "Terminal is not ready for input"))
    (when-not (.offer queue message) (fail! :input-capacity "Terminal input queue is full; wait for the reader"))))

(defn open!
  "Open a real Docker/zsh PTY for an owned workspace. No input is evaluated on
  the host. Explicit operation; ordinary status/snapshot calls never open it."
  [manager {:keys [workspace-id repo rows cols] :or {rows 24 cols 80}}]
  (when-not (and (bounded-int? rows 2 200) (bounded-int? cols 2 500)) (fail! :invalid-size "Invalid terminal dimensions"))
  (let [data (resolved manager workspace-id) repo (or repo (first (sort (keys (:repositories data)))))
        cwd (or (get-in data [:repositories repo :path]) (fail! :unknown-repository "Unknown paired repository"))
        name (container-name manager) id (str (UUID/randomUUID))
        bridge-path (bridge-file! manager)
        initial {:session-id id :container-name name :workspace-id workspace-id :repo repo :cwd cwd :rows rows :cols cols
                 :state :starting :started-at-ms (System/currentTimeMillis) :chunks [] :cursor 0 :first-cursor 0}]
    (ensure-image! manager)
    (locking (:lock manager)
      (when (:closed? @(:state manager)) (fail! :closed "Terminal manager is closed"))
      (when (>= (count (filter #(#{:starting :running} (:state %)) (vals (:sessions @(:state manager))))) (:max-sessions manager))
        (fail! :capacity "Terminal session capacity is full"))
      (swap! (:state manager) update :sessions
             (fn [sessions] (let [active (filter #(#{:starting :running} (:state (val %))) sessions)
                                  retired (take-last (- 31 (count active)) (sort-by (comp :started-at-ms val)
                                                                                 (remove #(#{:starting :running} (:state (val %))) sessions)))]
                              (assoc (into {} (concat active retired)) id initial)))))
    (try
      (create-container! manager name (docker-argv manager data name cwd true ["/usr/bin/zsh" "-i"]) {})
      (when (= :closed (:state (session manager id))) (throw (InterruptedException. "Terminal opening was cancelled")))
      (let [argv (start-argv manager name)
            process (.start (ProcessBuilder. ^java.util.List [(:python manager) bridge-path (json/write-str {:argv argv :rows rows :cols cols})]))
            writer (OutputStreamWriter. (.getOutputStream process) StandardCharsets/UTF_8)
            input-queue (ArrayBlockingQueue. 8)
            input-thread (Thread. (fn []
                                    (try (loop [] (let [message (.take input-queue)]
                                                   (.write writer (str (json/write-str message) "\n")) (.flush writer)
                                                   (when-not (= "close" (:op message)) (recur))))
                                         (catch Exception _))) (str "tolglow-terminal-input-" id))
            reader (Thread.
                     (fn []
                       (try
                         (with-open [lines (BufferedReader. (InputStreamReader. (.getInputStream process) StandardCharsets/UTF_8))]
                           (loop []
                             (when-let [line (.readLine lines)]
                               (when (> (count line) 131072) (fail! :bridge-output "PTY transport exceeded its line limit"))
                               (let [event (json/read-str line :key-fn keyword)]
                                 (case (:kind event)
                                   "output" (let [bytes (decode (:data event))]
                                              (swap! (:state manager) update-in [:sessions id] append-output bytes (:output-bytes manager)))
                                   "exit" (swap! (:state manager) update-in [:sessions id]
                                                  #(cond-> (assoc % :exit-code (:exit-code event)) (not= :closed (:state %)) (assoc :state :exited)))
                                   nil))
                               (recur))))
                         (catch Exception error
                           (swap! (:state manager) update-in [:sessions id]
                                  #(cond-> (assoc % :error (.getMessage error)) (not= :closed (:state %)) (assoc :state :exited))))
                         (finally
                           (cleanup-container! manager name)
                           (when (.isAlive process) (.destroyForcibly process))
                           (.interrupt input-thread)
                           (swap! (:state manager) update-in [:sessions id]
                                  #(cond-> % (#{:starting :running} (:state %)) (assoc :state :exited))))))
                     (str "tolglow-terminal-" id))]
        (locking (:lock manager)
          (when (or (:closed? @(:state manager)) (= :closed (:state (session manager id))))
            (.destroyForcibly process)
            (throw (InterruptedException. "Terminal opening was cancelled")))
          (swap! (:state manager) update-in [:sessions id] assoc :state :running :process process :writer writer :reader reader
                 :input-queue input-queue :input-thread input-thread))
        (.setDaemon input-thread true) (.start input-thread)
        (.setDaemon reader true) (.start reader)
        (when (:closed? @(:state manager)) (send! (session manager id) {:op "close"}))
        (public-session (session manager id)))
      (catch Throwable error
        (swap! (:state manager) update-in [:sessions id]
               #(cond-> (assoc % :error (.getMessage error)) (not= :closed (:state %)) (assoc :state :exited)))
        (cleanup-container! manager name) (throw error)))))

(defn input! [manager id data]
  (let [s (session manager id) bytes (decode data)]
    (when-not (= :running (:state s)) (fail! :not-running "Terminal is not running"))
    (send! s {:op "input" :data (encode bytes)}) {:accepted-bytes (alength bytes)}))

(defn resize! [manager id rows cols]
  (when-not (and (bounded-int? rows 2 200) (bounded-int? cols 2 500)) (fail! :invalid-size "Invalid terminal dimensions"))
  (let [s (session manager id)]
    (when-not (= :running (:state s)) (fail! :not-running "Terminal is not running"))
    (send! s {:op "resize" :rows rows :cols cols})
    ;; Docker's SIGWINCH forwarding is asynchronous. Resize the owned container
    ;; init process's actual input PTY as well, so later accepted input observes
    ;; the new size after this function returns. No shell or guessed pts number.
    (let [result (run-host! manager ["exec" (:container-name s) "/usr/bin/stty" "-F" "/proc/1/fd/0"
                                     "rows" (str rows) "cols" (str cols)] 5000)]
      (when-not (= 0 (:exit-code result)) (fail! :resize-failed "The owned container PTY could not be resized")))
    (swap! (:state manager) update-in [:sessions id] assoc :rows rows :cols cols)
    (public-session (session manager id))))

(defn read-output
  "Read at most 64 KiB from an absolute byte cursor. dropped? reports overwritten
  history; cursor is the next read position. Bytes may split UTF-8/ANSI sequences,
  which xterm's streaming byte input handles. Calls never consume another reader."
  [manager id cursor]
  (let [s (session manager id)]
    (when-not (and (integer? cursor) (<= 0 cursor (:cursor s))) (fail! :invalid-cursor "Cursor is outside this terminal's byte history"))
    (let [start (max cursor (:first-cursor s)) end (min (:cursor s) (+ start 65536))
          out (java.io.ByteArrayOutputStream.)]
      (doseq [{:keys [start bytes] :as chunk} (:chunks s)
              :let [left (max 0 (- (max cursor (:first-cursor s)) start)) right (min (alength ^bytes bytes) (- end start))]
              :when (> right left)]
        (.write out ^bytes bytes (int left) (int (- right left))))
      (assoc (public-session s) :data (encode (.toByteArray out)) :cursor end :available-cursor (:cursor s)
             :dropped? (< cursor (:first-cursor s))))))

(defn close-session! [manager id]
  (let [s (session manager id)]
    (swap! (:state manager) update-in [:sessions id] assoc :state :closed :closed-at-ms (System/currentTimeMillis))
    (when (:writer s) (try (send! s {:op "close"}) (catch Exception _)))
    (cleanup-container! manager (:container-name s))
    (when-let [^Process p (:process s)] (when-not (.waitFor p 1000 TimeUnit/MILLISECONDS) (.destroyForcibly p)))
    (when-let [^Thread t (:input-thread s)] (.interrupt t))
    (public-session (session manager id))))

(defn close! [manager]
  (locking (:lock manager) (swap! (:state manager) assoc :closed? true))
  (doseq [[id s] (:sessions @(:state manager)) :when (#{:starting :running} (:state s))] (close-session! manager id))
  (doseq [[name job] (:jobs @(:state manager))]
    (.interrupt ^Thread (:thread job)) (cleanup-container! manager name))
  (doseq [name (keys (:containers @(:state manager)))] (cleanup-container! manager name))
  (when-let [path (:bridge-temp @(:state manager))]
    (Files/deleteIfExists (.toPath (io/file path)))
    (swap! (:state manager) dissoc :bridge-temp))
  (cond-> {:closed? true}
    (seq (:containers @(:state manager)))
    (assoc :pending-cleanup (count (:containers @(:state manager))))))
