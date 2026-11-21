(ns tolglow.development.workspace
  "Owned paired Git worktrees for application development. File access is
  confined through secure directory handles; code checks require an explicit
  runner (normally the host's Docker boundary). No automatic merge or deploy."
  (:require [clojure.string :as str]
            [clojure.edn :as edn])
  (:import [java.nio.file Files Path Paths LinkOption StandardOpenOption SecureDirectoryStream]
           [java.nio.file.attribute BasicFileAttributeView PosixFileAttributeView PosixFilePermissions FileAttribute]
           [java.nio.channels FileChannel OverlappingFileLockException]
           [java.nio ByteBuffer]
           [java.nio.charset StandardCharsets CodingErrorAction]
           [java.io ByteArrayOutputStream PushbackReader StringReader]
           [java.security MessageDigest]
           [java.util UUID]
           [java.util.concurrent ArrayBlockingQueue ThreadPoolExecutor ThreadPoolExecutor$AbortPolicy ThreadFactory TimeUnit RejectedExecutionException]))

(defn- fail! [code message] (throw (ex-info message {:workspace/error code})))
(def ^:private no-links (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))
(def ^:private attrs (make-array FileAttribute 0))
(defn- path [value] (.normalize (.toAbsolutePath (Paths/get (str value) (make-array String 0)))))
(defn- id? [value] (and (string? value) (boolean (re-matches #"[a-zA-Z0-9][a-zA-Z0-9_-]{0,63}" value))))
(defn- now-ms [] (System/currentTimeMillis))
(defn- hex [bytes] (apply str (map #(format "%02x" (bit-and 255 %)) bytes)))
(defn- sha [bytes] (hex (.digest (MessageDigest/getInstance "SHA-256") ^bytes bytes)))
(defn- kill-process! [^Process process]
  (doseq [handle (reverse (vec (iterator-seq (.iterator (.descendants (.toHandle process))))))]
    (try (.destroyForcibly ^java.lang.ProcessHandle handle) (catch Exception _)))
  (.destroyForcibly process))

(defn run-trusted-process!
  "Bounded argv-only host process runner for trusted Git operations and explicit
  test/local profiles. This is NOT a sandbox for arbitrary project code. Normal
  application checks must supply the Docker runner to create-manager instead."
  [{:keys [argv cwd timeout-ms max-output-bytes stdin environment]
    :or {timeout-ms 30000 max-output-bytes 1048576}}]
  (when-not (and (vector? argv) (seq argv) (every? #(and (string? %) (not (str/includes? % "\u0000"))) argv)
                 (string? cwd) (integer? timeout-ms) (<= 1 timeout-ms 300000)
                 (integer? max-output-bytes) (<= 1 max-output-bytes 8388608)
                 (or (nil? stdin) (and (string? stdin) (<= (count stdin) 1048576))))
    (fail! :invalid-process "Invalid bounded process request"))
  (let [builder (doto (ProcessBuilder. ^java.util.List argv) (.directory (.toFile (path cwd))) (.redirectErrorStream true))
        _ (doseq [[k v] environment] (.put (.environment builder) (str k) (str v)))
        process (.start builder)
        output (ByteArrayOutputStream.) truncated? (atom false) read-error (atom nil)
        writer (doto (Thread. (fn [] (try (with-open [input (.getOutputStream process)]
                                                (when stdin (.write input (.getBytes ^String stdin StandardCharsets/UTF_8))))
                                             (catch Exception _))) "tolglow-workspace-process-input")
                 (.setDaemon true) (.start))
        reader (doto (Thread.
                      (fn []
                        (try
                          (with-open [input (.getInputStream process)]
                            (let [buffer (byte-array 8192)]
                              (loop []
                                (let [n (.read input buffer)]
                                  (when (pos? n)
                                    (let [remaining (- max-output-bytes (.size output))]
                                      (when (pos? remaining) (.write output buffer 0 (min remaining n)))
                                      (when (> n remaining) (reset! truncated? true)))
                                    (recur))))))
                          (catch Exception error (reset! read-error error)))) "tolglow-workspace-process-output")
                 (.setDaemon true) (.start))]
    (try
      (let [finished? (.waitFor process timeout-ms TimeUnit/MILLISECONDS)]
        (when-not finished? (kill-process! process) (.waitFor process 1000 TimeUnit/MILLISECONDS))
        (.join reader 1000)
        {:exit-code (when-not (.isAlive process) (.exitValue process))
         :output (.toString output "UTF-8") :timed-out? (not finished?) :truncated? @truncated?})
      (catch InterruptedException error (kill-process! process) (.interrupt (Thread/currentThread)) (throw error))
      (finally
        (when (.isAlive process) (kill-process! process))
        (.interrupt writer)
        (try (.close (.getInputStream process)) (catch Exception _))))))

(defn- with-git-read-view
  "Git's ref backend may read commondir even with GIT_COMMON_DIR set. Give
  each owned read a private admin directory instead of consulting that mutable
  pointer. Copy the current owned HEAD, not its creation-time base; the explicit
  index still belongs to the real worktree. Never repair user-edited metadata."
  [repo f]
  (if-not repo (f {})
    (let [head-path (.resolve ^Path (:git-dir repo) "HEAD")
          _ (when-not (Files/isRegularFile head-path no-links)
              (fail! :invalid-git-head "Owned Git HEAD must be a regular reference file"))
          head (with-open [input (Files/newInputStream head-path
                                                     (into-array java.nio.file.OpenOption
                                                                 [StandardOpenOption/READ LinkOption/NOFOLLOW_LINKS]))]
                 (.readNBytes input 4097))
          _ (when (or (zero? (alength head)) (> (alength head) 4096))
              (fail! :invalid-git-head "Owned Git HEAD must be a bounded regular reference"))
          view (Files/createTempDirectory "tolglow-git-read-" attrs)]
      (try
        (Files/write (.resolve view "HEAD") head (make-array java.nio.file.OpenOption 0))
        (Files/write (.resolve view "commondir")
                     (.getBytes (str (:common-dir repo) "\n") StandardCharsets/UTF_8)
                     (make-array java.nio.file.OpenOption 0))
        (f {"GIT_DIR" (str view) "GIT_COMMON_DIR" (str (:common-dir repo))
            "GIT_WORK_TREE" (str (:path repo))
            "GIT_INDEX_FILE" (str (.resolve ^Path (:git-dir repo) "index"))})
        (finally
          (Files/deleteIfExists (.resolve view "HEAD"))
          (Files/deleteIfExists (.resolve view "commondir"))
          (Files/deleteIfExists view))))))

(defn- git! [directory arguments]
  (let [repo (when (map? directory) directory)
        directory (if repo (:path repo) directory)
        environment {"GIT_CONFIG_NOSYSTEM" "1" "GIT_CONFIG_GLOBAL" "/dev/null" "GIT_OPTIONAL_LOCKS" "0"}
        result (with-git-read-view repo
                 (fn [identity]
                   (run-trusted-process!
                     {:argv (into (into ["git" "--no-optional-locks" "-c" "core.hooksPath=/dev/null"
                                         "-c" "core.fsmonitor=false" "-c" "extensions.worktreeConfig=false"]
                                        (mapcat (fn [key] ["-c" (str key "=")]) (:disabled-filters repo))) arguments)
                      :environment (merge environment identity) :cwd (str directory)
                      :timeout-ms 30000 :max-output-bytes 4194304})))]
    (when (or (not= 0 (:exit-code result)) (:timed-out? result) (:truncated? result))
      (throw (ex-info "Git workspace operation failed" {:workspace/error :git-failed :result result})))
    (:output result)))

(defn create-manager
  "Construct explicit workspace ownership. repositories maps stable repo keys to
  trusted Git roots; workspace-parent is separate from those checkouts. A check
  runner is optional at construction but required before check!. It receives
  {:argv :cwd :workspace-root :timeout-ms :max-output-bytes :stdin?} and returns
  {:exit-code :output :timed-out? :truncated?}. Profiles are trusted fixed argv,
  never command strings supplied by an agent. Construction creates no worktree."
  [{:keys [repositories workspace-parent runner check-profiles max-workspaces now-ms
           fingerprint-max-file-bytes fingerprint-max-bytes]
    :or {check-profiles {} max-workspaces 4 now-ms now-ms
         fingerprint-max-file-bytes 67108864 fingerprint-max-bytes 1073741824}}]
  (when-not (and (map? repositories) (<= 1 (count repositories) 8)
                 (every? #(and (keyword? (key %)) (nil? (namespace (key %))) (id? (name (key %))) (string? (val %))) repositories)
                 (string? workspace-parent) (or (nil? runner) (ifn? runner)) (ifn? now-ms)
                 (integer? max-workspaces) (<= 1 max-workspaces 16) (map? check-profiles) (<= (count check-profiles) 32)
                 (integer? fingerprint-max-file-bytes) (<= 1 fingerprint-max-file-bytes 1073741824)
                 (integer? fingerprint-max-bytes) (<= fingerprint-max-file-bytes fingerprint-max-bytes 8589934592))
    (fail! :invalid-options "Workspace manager needs explicit repository roots, parent and bounded configuration"))
  (doseq [[id profile] check-profiles]
    (when-not (and (keyword? id) (contains? repositories (:repo profile)) (vector? (:argv profile))
                   (seq (:argv profile)) (every? #(and (string? %) (not (str/includes? % "\u0000"))) (:argv profile))
                   (integer? (:timeout-ms profile 60000)) (<= 1 (:timeout-ms profile 60000) 300000)
                   (integer? (:max-output-bytes profile 1048576)) (<= 1 (:max-output-bytes profile 1048576) 8388608))
      (fail! :invalid-options "Check profile needs a known repo, fixed argv and bounded timeout/output")))
  (let [parent (path workspace-parent) repositories (into {} (map (fn [[id root]] [id (path root)]) repositories))]
    (when-not (= (count repositories) (count (set (vals repositories))))
      (fail! :invalid-options "Each paired repository must have a distinct root"))
    (when (some (fn [[_ root]] (or (.startsWith parent ^Path root) (.startsWith ^Path root parent))) repositories)
      (fail! :invalid-parent "Workspace parent must be separate from configured repository roots"))
    {:lock (Object.) :parent parent :repositories repositories :runner runner :profiles check-profiles :now-ms now-ms :max-workspaces max-workspaces
     :fingerprint-max-file-bytes fingerprint-max-file-bytes :fingerprint-max-bytes fingerprint-max-bytes
     :state (atom {:closed? false :workspaces {}})
     :executor (ThreadPoolExecutor. 2 2 30 TimeUnit/SECONDS (ArrayBlockingQueue. 8)
                 (reify ThreadFactory (newThread [_ task] (doto (Thread. task "tolglow-workspace-check") (.setDaemon true))))
                 (ThreadPoolExecutor$AbortPolicy.))}))

(defn- owned [manager id]
  (or (get-in @(:state manager) [:workspaces id]) (fail! :unknown-workspace "Unknown owned workspace")))
(defn- public-workspace [workspace]
  (-> (select-keys workspace [:id :state :created-at-ms :revision :root :repositories :checks :preview :error])
      (assoc :running-checks (into {} (map (fn [[profile run]] [profile {:run-id (:id run) :status :running}]) (:active-checks workspace))))
      (update :root str)
      (update :repositories #(into {} (map (fn [[id repo]] [id (update (select-keys repo [:path :base :branch]) :path str)]) %)))))
(defn status
  ([manager] {:closed? (:closed? @(:state manager)) :workspaces (mapv public-workspace (sort-by :id (vals (:workspaces @(:state manager)))))})
  ([manager id] (public-workspace (owned manager id))))

(defn- ensure-parent! [manager]
  (let [parent ^Path (:parent manager)]
    (loop [current parent]
      (when current
        (when (Files/isSymbolicLink current) (fail! :symlink "Workspace parent cannot contain symbolic links"))
        (recur (.getParent current))))
    (Files/createDirectories parent attrs)
    parent))

(declare persist-catalog!)

(defn create!
  "Create paired fresh branches/worktrees at explicit committed base refs
  (default HEAD). Main checkout modifications are not copied or changed. A failed
  pair removes only newly created clean worktrees; branches remain inspectable."
  [manager {:keys [id base-refs] :or {base-refs {}}}]
  (let [id (or id (str "agent-" (subs (str (UUID/randomUUID)) 0 12)))
        _ (when-not (and (id? id) (map? base-refs) (every? #(contains? (:repositories manager) %) (keys base-refs)))
            (fail! :invalid-create "Invalid workspace identity or base-ref repository"))
        root (.resolve ^Path (:parent manager) ^String id)
        entry {:id id :root root :lock (Object.) :state :creating :revision 0 :repositories {} :checks {}
               :created-at-ms ((:now-ms manager)) :active-checks {} :preview nil}]
    (locking (:lock manager)
      (when (:closed? @(:state manager)) (fail! :closed "Workspace manager is closed"))
      (when (or (get-in @(:state manager) [:workspaces id]) (>= (count (:workspaces @(:state manager))) (:max-workspaces manager)))
        (fail! :workspace-limit "Workspace identity is in use or the workspace limit was reached"))
      (swap! (:state manager) assoc-in [:workspaces id] entry))
    (try
      (ensure-parent! manager)
      (when (Files/exists root no-links) (fail! :path-exists "Workspace destination already exists"))
      (Files/createDirectory root attrs)
      (doseq [[repo-id repo-root] (sort-by (comp name key) (:repositories manager))]
        (let [repo-root (.toRealPath ^Path repo-root (make-array LinkOption 0))
              actual (path (str/trim (git! repo-root ["rev-parse" "--show-toplevel"])))
              _ (when-not (= repo-root actual) (fail! :invalid-repository "Configured repository must be its Git worktree root"))
              ref (get base-refs repo-id "HEAD")
              _ (when-not (and (string? ref) (<= 1 (count ref) 256) (not (re-find #"[\x00-\x20]" ref)))
                  (fail! :invalid-ref "Base ref must be one bounded Git revision"))
              commit (str/trim (git! repo-root ["rev-parse" "--verify" "--end-of-options" (str ref "^{commit}")]))
              branch (str "tolglow-agent/" id) destination (.resolve root (name repo-id))]
          (git! repo-root ["worktree" "add" "-b" branch (str destination) commit])
          ;; Register ownership immediately after creation. Metadata discovery can
          ;; fail independently, and the catch path must still remove this clean
          ;; worktree rather than orphan its Git registration.
          (swap! (:state manager) assoc-in [:workspaces id :repositories repo-id]
                 {:path destination :base commit :branch branch})
          (let [git-dir (path (str/trim (git! destination ["rev-parse" "--path-format=absolute" "--git-dir"])))
                common-dir (path (str/trim (git! destination ["rev-parse" "--path-format=absolute" "--git-common-dir"])))
                disabled-filters (->> (str/split-lines (git! destination ["config" "--local" "--name-only" "--list"]))
                                      (filter #(re-matches #"(?i)filter\..*\.(clean|process|smudge)" %)) vec)]
            (when-not (.startsWith git-dir (.resolve common-dir "worktrees"))
              (fail! :invalid-repository "Git did not create an owned worktree administration directory"))
            (swap! (:state manager) assoc-in [:workspaces id :repositories repo-id]
                   {:path destination :base commit :branch branch :git-dir git-dir :common-dir common-dir :disabled-filters disabled-filters}))))
      (swap! (:state manager) assoc-in [:workspaces id :state] :ready)
      (persist-catalog! manager)
      (status manager id)
      (catch Throwable error
        (doseq [[repo-id repo] (:repositories (owned manager id))]
          (try (git! (get (:repositories manager) repo-id) ["worktree" "remove" (str (:path repo))]) (catch Exception _)))
        (swap! (:state manager) update-in [:workspaces id] assoc :state :error
               :error {:code (or (:workspace/error (ex-data error)) :create-failed) :message (.getMessage error)})
        (throw error)))))

(defn- components [relative]
  (when-not (and (string? relative) (<= 1 (count relative) 4096)
                 (not (re-find #"[\\\x00-\x1f]" relative)) (not (str/starts-with? relative "/")))
    (fail! :invalid-path "File path must be a bounded relative POSIX path"))
  (let [parts (str/split relative #"/" -1)]
    (when (some #(or (#{"" "." ".."} %) (= ".git" (str/lower-case %))) parts)
      (fail! :invalid-path "Traversal and Git metadata paths are not editable"))
    parts))
(defn- relative-path [name] (Paths/get ^String name (make-array String 0)))
(defn- secure-parent
  "Resolve through NOFOLLOW directory handles, never via a path-string parent.
  New files require existing parent directories; no unsafe recursive mkdir."
  [manager workspace repo relative f]
  (let [parts (components relative)
        repo-data (or (get-in workspace [:repositories repo]) (fail! :unknown-repository "Unknown paired repository"))
        _ (when-not (= :ready (:state workspace)) (fail! :not-ready "Workspace is not ready"))
        streams (atom [])]
    (try
      (let [root (Files/newDirectoryStream ^Path (:parent manager))]
        (when-not (instance? SecureDirectoryStream root) (.close root) (fail! :unsupported-filesystem "Secure directory traversal is unavailable"))
        (swap! streams conj root)
        (let [parent (reduce (fn [^SecureDirectoryStream parent name]
                               (let [next (.newDirectoryStream parent (relative-path name) no-links)]
                                 (swap! streams conj next) next)) root (concat [(:id workspace) (name repo)] (butlast parts)))]
          (f parent (relative-path (last parts)))))
      (catch java.nio.file.FileSystemException error
        (throw (ex-info "File path is missing, unsafe, or unavailable" {:workspace/error :unsafe-path :path relative} error)))
      (finally (doseq [stream (reverse @streams)] (.close ^java.nio.file.DirectoryStream stream))))))

(defn- file-attributes [^SecureDirectoryStream parent ^Path leaf]
  (try (.readAttributes (.getFileAttributeView parent leaf BasicFileAttributeView no-links))
       (catch java.nio.file.NoSuchFileException _ nil)))
(defn- read-bytes [^SecureDirectoryStream parent ^Path leaf limit]
  (let [info (file-attributes parent leaf)]
    (when-not info (fail! :missing-file "File does not exist"))
    (when-not (.isRegularFile info) (fail! :unsafe-path "Only regular files can be read or edited"))
    (when (> (.size info) limit) (fail! :file-limit "File exceeds the configured read limit"))
    (with-open [channel (.newByteChannel parent leaf #{StandardOpenOption/READ LinkOption/NOFOLLOW_LINKS} attrs)]
      (let [out (ByteArrayOutputStream.) buffer (ByteBuffer/allocate 8192)]
        (loop []
          (.clear buffer)
          (let [n (.read channel buffer)]
            (when (pos? n)
              (when (> (+ (.size out) n) limit) (fail! :file-limit "File grew beyond the configured read limit"))
              (.write out (.array buffer) 0 n) (recur))))
        (.toByteArray out)))))
(defn- decode-text [bytes]
  (try (str (.decode (doto (.newDecoder StandardCharsets/UTF_8) (.onMalformedInput CodingErrorAction/REPORT)
                        (.onUnmappableCharacter CodingErrorAction/REPORT)) (ByteBuffer/wrap bytes)))
       (catch java.nio.charset.CharacterCodingException _ nil)))

(def ^:private catalog-name ".tolglow-workspaces.edn")
(defn- catalog-data [manager]
  {:version 1 :repositories (into {} (map (fn [[id root]] [id (str root)]) (:repositories manager)))
   :workspaces (mapv (fn [workspace]
                       (-> (select-keys workspace [:id :created-at-ms :revision])
                           (assoc :repositories
                                  (into {} (for [[id repo] (:repositories workspace)]
                                             [id (-> (select-keys repo [:base :branch :path :git-dir :common-dir])
                                                     (update :path str) (update :git-dir str) (update :common-dir str))])))))
                     (sort-by :id (filter #(= :ready (:state %)) (vals (:workspaces @(:state manager))))))})

(defn- persist-catalog! [manager]
  ;; Outside every container mount: only previously issued ownership is saved.
  (locking (:lock manager)
    (with-open [parent (Files/newDirectoryStream ^Path (:parent manager))]
      (when-not (instance? SecureDirectoryStream parent) (fail! :unsupported-filesystem "Secure catalog access is unavailable"))
      (let [^SecureDirectoryStream parent parent leaf (relative-path catalog-name)
            deadline (+ (System/nanoTime) 10000000000)]
        (with-open [channel (.newByteChannel parent (relative-path ".tolglow-workspaces.lock")
                                            #{StandardOpenOption/CREATE StandardOpenOption/WRITE LinkOption/NOFOLLOW_LINKS} attrs)
                    lock (loop []
                           (if-let [lock (try (.tryLock ^FileChannel channel) (catch OverlappingFileLockException _ nil))]
                             lock
                             (if (> (System/nanoTime) deadline) (fail! :catalog-busy "Another workspace owner is writing the catalog")
                                 (do (Thread/sleep 5) (recur)))))]
          (let [current (catalog-data manager)
                previous (when (file-attributes parent leaf)
                           (with-open [reader (PushbackReader. (StringReader. (decode-text (read-bytes parent leaf 1048576))))]
                             (let [options {:eof ::eof :readers {} :default (fn [& _] (fail! :invalid-catalog "Tagged values are not allowed"))}
                                   value (edn/read options reader)]
                               (when-not (= ::eof (edn/read options reader)) (fail! :invalid-catalog "Catalog must contain one EDN document"))
                               value)))
                _ (when (and previous (not (and (= 1 (:version previous))
                                                (= (:repositories current) (:repositories previous))
                                                (vector? (:workspaces previous))
                                                (every? #(id? (:id %)) (:workspaces previous))
                                                (= (count (:workspaces previous)) (count (set (map :id (:workspaces previous))))))))
                    (fail! :invalid-catalog "Existing workspace catalog has incompatible ownership"))
                entries (reduce (fn [entries entry]
                                  (when-let [old (get entries (:id entry))]
                                    (when-not (= (:repositories old) (:repositories entry))
                                      (fail! :invalid-catalog "Workspace identity already belongs to different Git metadata")))
                                  (assoc entries (:id entry)
                                         (update entry :revision max (:revision (get entries (:id entry)) 0))))
                                (into {} (map (juxt :id identity) (:workspaces previous))) (:workspaces current))
                _ (when (> (count entries) (:max-workspaces manager)) (fail! :workspace-limit "Persisted workspace capacity is full"))
                bytes (.getBytes (pr-str (assoc current :workspaces (vec (sort-by :id (vals entries))))) StandardCharsets/UTF_8)
                temp (relative-path (str ".tolglow-catalog-" (UUID/randomUUID)))]
            (when (> (alength bytes) 1048576) (fail! :catalog-limit "Workspace ownership catalog exceeds one MiB"))
            (try
              (with-open [output (.newByteChannel parent temp #{StandardOpenOption/CREATE_NEW StandardOpenOption/WRITE LinkOption/NOFOLLOW_LINKS} attrs)]
                (let [buffer (ByteBuffer/wrap bytes)] (while (.hasRemaining buffer) (.write output buffer)))
                (when (instance? FileChannel output) (.force ^FileChannel output true)))
              (.setPermissions (.getFileAttributeView parent temp PosixFileAttributeView no-links) (PosixFilePermissions/fromString "rw-------"))
              (.move parent temp parent leaf)
              (finally (try (.deleteFile parent temp) (catch java.nio.file.NoSuchFileException _))))))))))

(defn- canonical-directory [value]
  (when-not (string? value) (fail! :invalid-catalog "Catalog directory must be a string"))
  (let [p (path value)]
    (when-not (and (= value (str p)) (= p (.toRealPath p (make-array LinkOption 0)))
                   (Files/isDirectory p no-links))
      (fail! :invalid-catalog "Catalog directory must be an existing canonical directory"))
    p))

(defn- recovered-workspace [manager saved]
  (let [{:keys [id created-at-ms revision repositories]} saved
        _ (when-not (and (id? id) (integer? created-at-ms) (<= 0 created-at-ms)
                         (integer? revision) (<= 0 revision)
                         (= (set (keys repositories)) (set (keys (:repositories manager)))))
            (fail! :invalid-catalog "Invalid saved workspace identity or repositories"))
        root (canonical-directory (str (.resolve ^Path (:parent manager) ^String id)))
        repos (into {}
                    (for [[repo-id repo] repositories
                          :let [configured (get (:repositories manager) repo-id)
                                destination (canonical-directory (:path repo))
                                common (canonical-directory (:common-dir repo))
                                admin (canonical-directory (:git-dir repo))
                                actual-common (path (str/trim (git! configured ["rev-parse" "--path-format=absolute" "--git-common-dir"])))
                                branch (str "tolglow-agent/" id)]]
                      (do
                        (when-not (and (= destination (.resolve root (name repo-id)))
                                       (= common actual-common) (.startsWith admin (.resolve common "worktrees"))
                                       (= branch (:branch repo)) (string? (:base repo))
                                       (re-matches #"[0-9a-f]{40,64}" (:base repo)))
                          (fail! :invalid-catalog "Saved workspace does not belong to the configured Git repositories"))
                        (let [registered (str/split (git! configured ["worktree" "list" "--porcelain" "-z"]) #"\u0000")
                              descriptor (assoc repo :path destination :git-dir admin :common-dir common)
                              actual-branch (str/trim (git! descriptor ["branch" "--show-current"]))]
                          (when-not (and (some #{(str "worktree " destination)} registered) (= branch actual-branch))
                            (fail! :invalid-catalog "Saved worktree registration or branch changed"))
                          [repo-id (assoc descriptor :disabled-filters
                                          (->> (str/split-lines (git! configured ["config" "--local" "--name-only" "--list"]))
                                               (filter #(re-matches #"(?i)filter\..*\.(clean|process|smudge)" %)) vec))]))))]
    {:id id :root root :lock (Object.) :state :ready :created-at-ms created-at-ms :revision (inc revision)
     :repositories repos :checks {} :active-checks {} :preview nil :recovered? true}))

(defn recover!
  "Explicit startup recovery from the issued-workspace catalog. Does not create
  worktrees, execute project code, start containers or restore old check approval.
  Dirty source files remain untouched. Invalid catalog entries reject the whole
  recovery before changing manager state; no directory discovery/adoption."
  [manager]
  (locking (:lock manager)
    (when (:closed? @(:state manager)) (fail! :closed "Workspace manager is closed"))
    (let [parent ^Path (:parent manager) catalog (.resolve parent catalog-name)]
      (if-not (Files/exists catalog no-links)
        {:recovered 0}
        (do
          (canonical-directory (str parent))
          (let [data (with-open [directory (Files/newDirectoryStream parent)]
                       (when-not (instance? SecureDirectoryStream directory)
                         (fail! :unsupported-filesystem "Secure catalog access is unavailable"))
                       (let [source (decode-text (read-bytes directory (relative-path catalog-name) 1048576))]
                         (when-not source (fail! :invalid-catalog "Catalog must contain UTF-8 EDN"))
                         (with-open [reader (PushbackReader. (StringReader. source))]
                           (let [options {:eof ::eof :readers {} :default (fn [& _] (fail! :invalid-catalog "Tagged values are not allowed"))}
                                 value (edn/read options reader)]
                             (when-not (= ::eof (edn/read options reader)) (fail! :invalid-catalog "Catalog must contain one EDN document"))
                             value))))]
            (when-not (and (= 1 (:version data))
                           (= (:repositories data) (into {} (map (fn [[id root]] [id (str root)]) (:repositories manager))))
                           (vector? (:workspaces data)) (<= (count (:workspaces data)) (:max-workspaces manager)))
              (fail! :invalid-catalog "Catalog version, source repositories or workspace bound differ"))
            (let [entries (mapv #(recovered-workspace manager %) (:workspaces data))
                  ids (mapv :id entries)]
              (when-not (and (= (count ids) (count (set ids)))
                             (not-any? #(get-in @(:state manager) [:workspaces %]) ids))
                (fail! :invalid-catalog "Workspace recovery would replace existing ownership"))
              (swap! (:state manager) update :workspaces into (map (juxt :id identity) entries))
              {:recovered (count entries)})))))))

(defn read-file [manager id repo relative]
  (let [workspace (owned manager id)]
    (secure-parent manager workspace repo relative
                   (fn [parent leaf]
                     (let [bytes (read-bytes parent leaf 2097152) text (decode-text bytes)]
                       {:path relative :repo repo :hash (sha bytes) :size (alength bytes)
                        :binary? (or (nil? text) (str/includes? text "\u0000"))
                        :content (when (and text (not (str/includes? text "\u0000"))) text)})))))

(defn write-file!
  "Atomically replace an owned regular file, checking expected-hash; use
  :missing for a new file in an existing directory. Existing executable bits are
  preserved. A successful edit invalidates previous preview readiness."
  [manager id repo relative {:keys [content expected-hash]}]
  (when (:closed? @(:state manager)) (fail! :closed "Workspace manager is closed"))
  (when-not (and (string? content) (or (= :missing expected-hash)
                                      (and (string? expected-hash) (re-matches #"[0-9a-f]{64}" expected-hash))))
    (fail! :invalid-write "Text writes require an expected SHA256 hash or :missing"))
  (let [bytes (.getBytes ^String content StandardCharsets/UTF_8) workspace (owned manager id)]
    (when (> (alength bytes) 2097152) (fail! :file-limit "Text write exceeds two MiB"))
    (locking (:lock workspace)
      (secure-parent manager workspace repo relative
        (fn [^SecureDirectoryStream parent leaf]
          (let [info (file-attributes parent leaf)
                actual (if info (sha (read-bytes parent leaf 2097152)) :missing)]
            (when-not (= expected-hash actual) (fail! :hash-conflict "File changed since it was read; reload before writing"))
            (let [temp (relative-path (str ".tolglow-write-" (UUID/randomUUID)))
                  permissions (if info (.permissions (.readAttributes (.getFileAttributeView parent leaf PosixFileAttributeView no-links)))
                                  (PosixFilePermissions/fromString "rw-------"))]
              (try
                (with-open [channel (.newByteChannel parent temp #{StandardOpenOption/CREATE_NEW StandardOpenOption/WRITE LinkOption/NOFOLLOW_LINKS} attrs)]
                  (let [buffer (ByteBuffer/wrap bytes)] (while (.hasRemaining buffer) (.write channel buffer)))
                  (when (instance? FileChannel channel) (.force ^FileChannel channel true)))
                (.setPermissions (.getFileAttributeView parent temp PosixFileAttributeView no-links) permissions)
                (.move parent temp parent leaf)
                (finally (try (.deleteFile parent temp) (catch java.nio.file.NoSuchFileException _))))))))
      (swap! (:state manager) update-in [:workspaces id]
             #(-> % (update :revision inc) (assoc :preview nil)))
      (persist-catalog! manager)
      (assoc (read-file manager id repo relative) :revision (:revision (owned manager id))))))

(defn- repo-files [workspace repo]
  (let [root (get-in workspace [:repositories repo])]
    (when-not root (fail! :unknown-repository "Unknown paired repository"))
    (->> (str/split (git! root ["ls-files" "-z" "--cached" "--others" "--exclude-standard"]) #"\u0000")
         (remove empty?) distinct sort vec)))

(defn tree
  "Bounded page of tracked and unignored files. Symlinks are listed as
  non-editable; neither listing nor reading follows their targets."
  ([manager id repo] (tree manager id repo {}))
  ([manager id repo {:keys [offset limit] :or {offset 0 limit 500}}]
   (when-not (and (integer? offset) (<= 0 offset 1000000) (integer? limit) (<= 1 limit 2000))
     (fail! :invalid-page "Tree page needs a bounded offset and limit"))
   (let [workspace (owned manager id) paths (repo-files workspace repo)
         entries (mapv (fn [relative]
                         (try (secure-parent manager workspace repo relative
                                             (fn [parent leaf]
                                               (let [info (file-attributes parent leaf)]
                                                 {:path relative :size (when info (.size info))
                                                  :type (cond (nil? info) :deleted (.isSymbolicLink info) :symlink (.isRegularFile info) :file :else :other)
                                                  :editable? (boolean (and info (.isRegularFile info) (<= (.size info) 2097152)))})))
                              (catch clojure.lang.ExceptionInfo _ {:path relative :type :unavailable :editable? false})))
                       (take limit (drop offset paths)))]
     {:entries entries :total (count paths) :next-offset (when (< (+ offset limit) (count paths)) (+ offset limit))})))

(defn diff
  "Read bounded tracked diff plus untracked file names. Does not stage Git's
  index, execute external diff drivers, or merge into the original checkout."
  [manager id]
  (let [workspace (owned manager id)]
    {:workspace-id id
     :repositories (into {} (for [[repo data] (:repositories workspace)]
                               [repo {:diff (git! data ["diff" "--no-ext-diff" "--no-textconv" "HEAD" "--"])
                                      :status (git! data ["status" "--porcelain=v1" "--untracked-files=normal"])}]))}))

(defn- hash-file
  "Hash through one small owned buffer. Never allocate a whole binary asset just
  to fingerprint it; enforce limits against bytes actually read, including growth."
  [^SecureDirectoryStream parent ^Path leaf file-limit total workspace-limit]
  (let [info (file-attributes parent leaf)]
    (when-not info (fail! :missing-file "File does not exist"))
    (when-not (.isRegularFile info) (fail! :unsafe-path "Only regular files can be fingerprinted"))
    (when (> (.size info) file-limit) (fail! :file-limit "File exceeds the configured fingerprint limit"))
    (with-open [channel (.newByteChannel parent leaf #{StandardOpenOption/READ LinkOption/NOFOLLOW_LINKS} attrs)]
      (let [digest (MessageDigest/getInstance "SHA-256") buffer (ByteBuffer/allocate 8192)]
        (loop [read 0]
          (.clear buffer)
          (let [n (.read channel buffer)]
            (if (neg? n)
              (hex (.digest digest))
              (do
                (when (> (+ read n) file-limit) (fail! :file-limit "File grew beyond the configured fingerprint limit"))
                (when (> (swap! total + n) workspace-limit) (fail! :fingerprint-limit "Workspace exceeds the configured fingerprint byte limit"))
                (.flip buffer) (.update digest buffer)
                (recur (+ read n))))))))))

(defn fingerprint
  "Hash every tracked/unignored regular file and Git HEAD in each paired
  worktree. Hash through 8 KiB buffers, default bounds 64 MiB/file and 1 GiB/pair
  (the existing Afterglow documentation alone exceeds 256 MiB). Limits are explicit
  manager options. Symlinks are rejected for staging, never dereferenced."
  [manager id]
  (let [workspace (owned manager id) total (atom 0)]
    (into (sorted-map)
          (for [[repo data] (:repositories workspace)]
            [repo (sha (.getBytes
                         (pr-str [(str/trim (git! data ["rev-parse" "HEAD"]))
                                  (mapv (fn [relative]
                                          [relative (secure-parent manager workspace repo relative
                                                                   (fn [parent leaf]
                                                                     (if (file-attributes parent leaf)
                                                                       (hash-file parent leaf (:fingerprint-max-file-bytes manager 67108864)
                                                                                  total (:fingerprint-max-bytes manager 1073741824))
                                                                       :deleted)))]) (repo-files workspace repo))]) StandardCharsets/UTF_8))]))))

(defn check!
  "Run one configured check on a bounded worker outside state locks. Returns
  run-id/result/cancel!. Results bind to before/after paired fingerprints;
  an edit during a run makes that result stale. No runner means no code execution."
  [manager id profile-id]
  (when-not (:runner manager) (fail! :runner-required "Configure an isolated check runner before executing project code"))
  (let [profile (or (get (:profiles manager) profile-id) (fail! :unknown-check "Unknown configured check profile"))
        workspace (owned manager id) run-id (str (UUID/randomUUID)) result (promise) cancelled? (atom false) thread (atom nil)
        run {:id run-id :result result :cancelled? cancelled? :thread thread}
        _ (locking (:lock workspace)
            (when (:closed? @(:state manager)) (fail! :closed "Workspace manager is closed"))
            (when (get-in (owned manager id) [:active-checks profile-id]) (fail! :check-running "This check is already running"))
            (swap! (:state manager) assoc-in [:workspaces id :active-checks profile-id] run)
            (swap! (:state manager) assoc-in [:workspaces id :preview] nil))
        task (fn []
               (reset! thread (Thread/currentThread))
               (let [outcome
                     (try
                       (when @cancelled? (throw (InterruptedException. "Cancelled before check")))
                       (let [before (fingerprint manager id)
                             request (merge (select-keys profile [:argv :stdin])
                                            {:cwd (str (get-in workspace [:repositories (:repo profile) :path]))
                                             :workspace-root (str (:root workspace))
                                             :workspace-id id
                                             :timeout-ms (:timeout-ms profile 60000) :max-output-bytes (:max-output-bytes profile 1048576)})
                             response ((:runner manager) request)
                             after (fingerprint manager id)
                             status (cond @cancelled? :cancelled (not= before after) :stale
                                          (:timed-out? response) :timed-out (or (:truncated? response) (not= 0 (:exit-code response))) :failed :else :passed)]
                         (merge (select-keys response [:exit-code :output :timed-out? :truncated?])
                                {:status status :fingerprint after :checked-fingerprint before :run-id run-id :finished-at-ms ((:now-ms manager))}))
                       (catch InterruptedException _ {:status :cancelled :run-id run-id})
                       (catch Throwable error {:status :failed :run-id run-id :error {:code (or (:workspace/error (ex-data error)) :check-failed)
                                                                                  :message (.getMessage error)}}))]
                 (swap! (:state manager) update-in [:workspaces id]
                        #(-> % (assoc-in [:checks profile-id] outcome) (update :active-checks dissoc profile-id)))
                 (deliver result outcome)))]
    (try (.execute ^ThreadPoolExecutor (:executor manager) ^Runnable task)
         (catch RejectedExecutionException _
           (swap! (:state manager) update-in [:workspaces id :active-checks] dissoc profile-id)
           (fail! :check-capacity "Check worker capacity is full")))
    {:run-id run-id :result result
     :cancel! (fn [] (reset! cancelled? true) (when-let [^Thread worker @thread] (.interrupt worker)) true)}))

(defn stage-preview!
  "Declare a reviewable preview candidate only when all required fixed checks
  passed on exactly the current paired files. This starts no server/container,
  changes no running show and does not commit, merge, deploy or publish."
  [manager id {:keys [required-checks]}]
  (when (:closed? @(:state manager)) (fail! :closed "Workspace manager is closed"))
  (let [required (or required-checks (vec (keys (:profiles manager)))) workspace (owned manager id)]
    (when-not (and (vector? required) (seq required) (every? #(contains? (:profiles manager) %) required))
      (fail! :checks-required "Preview staging requires at least one configured check"))
    (locking (:lock workspace)
    (let [fingerprint (fingerprint manager id) checks (:checks (owned manager id))]
      (when-not (every? #(and (= :passed (get-in checks [% :status])) (= fingerprint (get-in checks [% :fingerprint]))) required)
        (fail! :checks-not-current "Required checks must pass on the current paired workspace files"))
      (let [preview {:status :ready :fingerprint fingerprint :required-checks required :staged-at-ms ((:now-ms manager))}]
        (swap! (:state manager) assoc-in [:workspaces id :preview] preview)
        preview)))))

(defn runtime-paths
  "Trusted service-only resolver for a container boundary. Git directories were
  captured at worktree creation, never read from an agent-editable .git pointer.
  The container may mount root read/write, common-dir read-only, and each owned
  git-dir read/write. This function opens no container or process."
  [manager id]
  (when (:closed? @(:state manager)) (fail! :closed "Workspace manager is closed"))
  (let [workspace (owned manager id)]
    (when-not (= :ready (:state workspace)) (fail! :not-ready "Workspace is not ready"))
    {:workspace-id id :workspace-root (str (:root workspace))
     :repositories (into {} (for [[repo data] (:repositories workspace)]
                              [repo (into {} (for [key [:path :git-dir :common-dir]] [key (str (get data key))]))]))}))

(defn close!
  "Cancel owned checks and shut down workers. Preserve worktrees/branches so
  unfinished agent edits remain reviewable; no implicit deletion or merge."
  [manager]
  (swap! (:state manager) assoc :closed? true)
  (doseq [workspace (vals (:workspaces @(:state manager))) run (vals (:active-checks workspace))]
    (reset! (:cancelled? run) true)
    (when-let [^Thread worker @(:thread run)] (.interrupt worker))
    (deliver (:result run) {:status :cancelled :run-id (:id run)}))
  (.shutdownNow ^ThreadPoolExecutor (:executor manager))
  (swap! (:state manager) update :workspaces
         (fn [workspaces] (into {} (for [[id workspace] workspaces]
                                    [id (-> workspace
                                            (update :checks merge (into {} (map (fn [[profile run]] [profile {:status :cancelled :run-id (:id run)}]) (:active-checks workspace))))
                                            (assoc :active-checks {}))]))))
  {:closed? true :terminated? (try (.awaitTermination ^ThreadPoolExecutor (:executor manager) 1000 TimeUnit/MILLISECONDS)
                                  (catch InterruptedException _ false))})
