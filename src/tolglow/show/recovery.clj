(ns tolglow.show.recovery
  "An independently scheduled working file and one prior-session slot per show."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [tolglow.persistence :as persistence])
  (:import [java.io FileOutputStream OutputStream Reader]
           [java.nio.channels FileChannel FileLock OverlappingFileLockException]
           [java.nio.charset StandardCharsets]
           [java.nio.file CopyOption Files LinkOption OpenOption Path StandardCopyOption StandardOpenOption]
           [java.nio.file.attribute BasicFileAttributes FileAttribute]
           [java.security DigestInputStream MessageDigest SecureRandom]
           [java.util HexFormat UUID]
           [java.util.concurrent ScheduledThreadPoolExecutor ThreadFactory TimeUnit]
           [javax.crypto Mac]
           [javax.crypto.spec SecretKeySpec]))

(defn- running-tests?
  "True inside a clojure.test run (run-tests binds the report counters and
  each test var binds the testing vars)."
  []
  (boolean
    (when-let [test-ns (find-ns 'clojure.test)]
      (or (some-> (ns-resolve test-ns '*report-counters*) deref)
          (seq (some-> (ns-resolve test-ns '*testing-vars*) deref))))))

(defonce ^:private test-directory
  (delay (.toFile (Files/createTempDirectory "tolglow-test-recovery" (make-array FileAttribute 0)))))

(defn default-directory
  "The per-user recovery directory when a show does not choose one. Inside a
  clojure.test run it is a temporary directory of this JVM instead, so no
  test reads or rewrites the operator's own recovery files (#742)."
  []
  (let [home (System/getProperty "user.home")]
    (cond
      (running-tests?) @test-directory
      (str/includes? (str/lower-case (System/getProperty "os.name")) "mac")
      (io/file home "Library" "Application Support" "Tolglow" "Recovery")
      :else (io/file (or (not-empty (System/getenv "XDG_STATE_HOME")) (io/file home ".local" "state"))
                     "tolglow" "recovery"))))

(defn options [value]
  (when-not (or (nil? value) (boolean? value) (map? value))
    (throw (ex-info "Invalid show recovery options" {})))
  (let [{:keys [enabled? interval-ms directory legacy-directory] :as options}
        (merge {:enabled? true :interval-ms 10000}
               (cond (false? value) {:enabled? false} (map? value) value))]
    (when-not (and (every? #{:enabled? :interval-ms :directory :legacy-directory} (keys options))
                   (boolean? enabled?) (pos-int? interval-ms))
      (throw (ex-info "Invalid show recovery options" {})))
    (cond-> (assoc options :directory (.getAbsoluteFile (io/file (or directory (default-directory)))))
      legacy-directory (assoc :legacy-directory (.getAbsoluteFile (io/file legacy-directory))))))

(defn show-directory
  "The directory under a recovery directory that holds one show's files."
  [directory show-id]
  (let [identity (.getBytes (pr-str show-id) StandardCharsets/UTF_8)]
    (io/file directory (.formatHex (HexFormat/of) (.digest (MessageDigest/getInstance "SHA-256") identity)))))

(def checkpoint-names
  "The fixed files a show keeps in its recovery directory."
  ["recovery.transit" "prior.checkpoint" "manual.edn"])

(defn- checkpoint-file ^java.io.File [directory show-id name]
  (io/file (show-directory directory show-id) name))

(defn- prepare-file! [path write! publish!]
  (let [file (.getAbsoluteFile (io/file path))]
    (io/make-parents file)
    (let [temp (Files/createTempFile (.toPath (.getParentFile file)) ".tolglow-show-" ".tmp"
                                    (make-array FileAttribute 0))]
      (try
        (write! temp)
        (publish! (fn []
                    (Files/move temp (.toPath file) (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                                                           StandardCopyOption/REPLACE_EXISTING]))
                    (.getAbsolutePath file)))
        (finally (Files/deleteIfExists temp))))))

(defn write-source!
  "Force a complete temporary file and publish it with one local atomic move.
  The optional publication callback owns whether and when to execute the move."
  ([path source] (write-source! path source (fn [replace!] (replace!))))
  ([path ^String source publish!]
   (prepare-file! path
     (fn [temp]
       (with-open [stream (FileOutputStream. (.toFile ^Path temp))]
         (.write stream (.getBytes source StandardCharsets/UTF_8))
         (.force (.getChannel stream) true)))
     publish!)))

(defn- read-document [^Reader reader]
  (let [buffer (char-array 8192) source (StringBuilder.)]
    (loop []
      (let [n (.read reader buffer)]
        (when (pos? n)
          (when (> (+ (.length source) n) persistence/max-document-characters)
            (throw (ex-info "Show file exceeds the document character limit" {})))
          (.append source buffer 0 n)
          (recur))))
    (str source)))

(defn read-source [path]
  (with-open [reader (io/reader (io/file path) :encoding "UTF-8")]
    (read-document reader)))

(def ^:private ^"[Ljava.nio.file.LinkOption;" link-options (make-array LinkOption 0))

(def ^:private source-open-options (into-array OpenOption [LinkOption/NOFOLLOW_LINKS]))

(def ^:private confirmation-key
  (delay (let [bytes (byte-array 32)]
           (.nextBytes (SecureRandom.) bytes)
           (SecretKeySpec. bytes "HmacSHA256"))))

(defn- source-identity [file]
  (when file
    (let [path (.toRealPath (.toPath (io/file file)) link-options)
          ^BasicFileAttributes attrs (Files/readAttributes path BasicFileAttributes link-options)]
      (when (and (.isRegularFile attrs) (.fileKey attrs))
        {:path path :identity [(str path) (str (.fileKey attrs)) (.size attrs)
                               (str (.creationTime attrs)) (str (.lastModifiedTime attrs))]}))))

(defn- source-token [identity]
  (let [mac (doto (Mac/getInstance "HmacSHA256") (.init @confirmation-key))]
    (.formatHex (HexFormat/of) (.doFinal mac (.getBytes (pr-str identity) StandardCharsets/UTF_8)))))

(defn read-selected-source
  "Return a bounded in-memory show source and opaque proof for a trusted file
  resolver and selection scope. Return nil when a stable, identifiable local
  file cannot be captured. The caller compares the token before admitting the
  captured bytes to the show owner. Resolution and reads run off the frame lock."
  [resolve-file scope]
  (try
    (when-let [{:keys [^Path path identity]} (source-identity (resolve-file))]
      (let [digest (MessageDigest/getInstance "SHA-256")
            source (with-open [input (DigestInputStream. (Files/newInputStream path source-open-options) digest)
                               reader (io/reader input :encoding "UTF-8")]
                     (read-document reader))]
        (when (= identity (:identity (source-identity (resolve-file))))
          {:source source
           :token (source-token [scope identity (.formatHex (HexFormat/of) (.digest digest))])})))
    (catch java.io.IOException _ nil)
    (catch clojure.lang.ExceptionInfo _ nil)))

(def ^:private prior-name "prior.checkpoint")

(def ^:private discard-prefix "tolglow-discarded-sha256 ")

(defn- checkpoint-hash [file]
  (let [digest (MessageDigest/getInstance "SHA-256")]
    (with-open [input (DigestInputStream. (io/input-stream file) digest)]
      (.transferTo input (OutputStream/nullOutputStream)))
    (.formatHex (HexFormat/of) (.digest digest))))

(defn- discarded-hash [^java.io.File prior]
  (let [length (+ (count discard-prefix) 64)]
    (when (= length (.length prior))
      (with-open [input (io/input-stream prior)]
        (second (re-matches #"tolglow-discarded-sha256 ([0-9a-f]{64})"
                            (String. (.readNBytes input (inc length)) StandardCharsets/UTF_8)))))))

(defn- prior-file [{:keys [directory settled-shows]} show-id]
  (let [prior (checkpoint-file directory show-id prior-name)
        dismissed (discarded-hash prior)]
    (cond
      (and (.isFile prior) (nil? dismissed)) prior
      (contains? @settled-shows show-id)
      ;; The first publication may have pinned the prior after the initial read.
      (when (and (.isFile prior) (nil? (discarded-hash prior))) prior)
      :else (let [working (checkpoint-file directory show-id "recovery.transit")]
              (when (and (.isFile working)
                         (or (nil? dismissed) (not= dismissed (checkpoint-hash working))))
                working)))))

(defn read-prior
  "Read the prior candidate. If the first autosave moves it from the working
  path to the prior slot during the read, read again from that slot."
  [owner show-id read!]
  (when-let [before (prior-file owner show-id)]
    (let [value (read! before)
          after (prior-file owner show-id)]
      (cond
        (= before after) value
        after (let [pinned (read! after)]
                (when (= after (prior-file owner show-id)) pinned))))))

(defn- replace-settled! [settled-shows show-id replace!]
  (let [settled? (contains? @settled-shows show-id)]
    ;; Hide the fallback before swapping its bytes or publishing a dismissal.
    ;; A pinned prior remains visible until its own file changes.
    (swap! settled-shows conj show-id)
    (try (replace!)
         (catch Exception error
           (when-not settled? (swap! settled-shows disj show-id))
           (throw error)))))

(defn discard-prior!
  "Replace just the prior slot with a durable dismissal for its exact bytes."
  [{:keys [directory open? settled-shows] :as owner} show-id publish!]
  (boolean
    (when-let [hash (read-prior owner show-id checkpoint-hash)]
      (write-source! (checkpoint-file directory show-id prior-name) (str discard-prefix hash)
        (fn [replace!]
          (publish!
            (fn []
              (when @open?
                (replace-settled! settled-shows show-id replace!)))))))))

(defn- preserve-prior! [^java.io.File working prior publish!]
  (prepare-file! prior
    (fn [temp]
      (Files/copy (.toPath working) ^Path temp
                  ^"[Ljava.nio.file.CopyOption;" (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING
                                                                       StandardCopyOption/COPY_ATTRIBUTES]))
      (with-open [channel (FileChannel/open temp (into-array OpenOption [StandardOpenOption/WRITE]))]
        (.force channel true)))
    publish!))

(declare refresh-legacy!)

(defn- autosave! [{:keys [directory status open? active? settled-shows] :as owner} capture! publish!]
  (try
    (when-let [{:keys [show-id source] :as captured} (when (and @open? @active?) (capture!))]
      (let [working (checkpoint-file directory show-id "recovery.transit")
            save! (fn [preserve!]
                    (write-source! working source
                      (fn [replace!]
                        (publish! captured
                          (fn []
                            (when @open?
                              (when (and preserve! (not (contains? @settled-shows show-id))) (preserve!))
                              (replace-settled! settled-shows show-id replace!)))))))
            path (if (and (not (contains? @settled-shows show-id)) (= working (prior-file owner show-id)))
                   (preserve-prior! working (checkpoint-file directory show-id prior-name) save!)
                   (save! nil))]
        (when path
          (swap! status assoc :last-saved (assoc (dissoc captured :source) :path path) :last-error nil))
        ;; Another window may have taken or dismissed the checkpoint from
        ;; before the update, or the show ID may have changed with an import.
        (when-let [cache (:legacy owner)]
          (let [{cached-id :show-id :keys [checkpoints]} @cache]
            (when (or (not= cached-id show-id) (seq checkpoints))
              (refresh-legacy! owner show-id))))))
    (catch Exception error
      (when @open? (swap! status assoc :last-error (ex-message error))))))

(defn start! [{:keys [enabled? interval-ms] :as config} active? capture! publish!]
  (let [owner (assoc config :open? (atom true) :active? (atom active?)
                            :settled-shows (atom #{}) :status (atom {}) :legacy (atom nil))]
    (if enabled?
      (let [worker (volatile! nil)
            factory (reify ThreadFactory
                      (newThread [_ task]
                        (vreset! worker (doto (Thread. task "tolglow-show-recovery") (.setDaemon true)))))
            executor (ScheduledThreadPoolExecutor. 1 factory)]
        (.scheduleWithFixedDelay executor ^Runnable #(autosave! owner capture! publish!)
                                 interval-ms interval-ms TimeUnit/MILLISECONDS)
        (assoc owner :executor executor :worker worker))
      owner)))

(defn activate! [owner]
  (reset! (:active? owner) true))

(defn close!
  "Retire publication and give any running autosave up to one second to clean
  up its staged files. A callback on the recovery worker may close its owner."
  [{:keys [open? ^ScheduledThreadPoolExecutor executor worker]}]
  (let [retired? (and open? (compare-and-set! open? true false))]
    (when executor
      (when retired? (.shutdownNow executor))
      (when-not (identical? (Thread/currentThread) (some-> worker deref))
        (try (.awaitTermination executor 1000 TimeUnit/MILLISECONDS)
             (catch InterruptedException _ (.interrupt (Thread/currentThread)))))))
  nil)

(defn status [owner]
  (merge (select-keys owner [:enabled? :interval-ms]) (some-> owner :status deref)))

(defn- fixed-checkpoint-path? [directory ^java.io.File file]
  (when-let [^java.io.File parent (some-> file .getParentFile)]
    (and (contains? #{"recovery.transit" "manual.edn" prior-name} (.getName file))
         (re-matches #"[0-9a-f]{64}" (.getName parent))
         (= directory (.getParentFile parent)))))

(defn manual-path! [{:keys [directory]} path]
  (let [file (.getAbsoluteFile (io/file path))
        directory (.getCanonicalFile ^java.io.File directory)
        ;; An atomic move replaces the final directory entry, even if it is a symlink.
        destination (when-let [parent (.getParentFile file)]
                      (io/file (.getCanonicalFile parent) (.getName file)))]
    (when (or (fixed-checkpoint-path? directory destination)
              (fixed-checkpoint-path? directory (.getCanonicalFile file)))
      (throw (ex-info "This path is reserved for show recovery" {:code :reserved-recovery-path}))))
  path)

(defn remember-manual! [{:keys [directory status]} show-id path]
  (try
    (write-source! (checkpoint-file directory show-id "manual.edn") (pr-str {:show-id show-id :path path}))
    (swap! status dissoc :manual-error)
    (catch Exception error (swap! status assoc :manual-error (ex-message error))))
  path)

(defn- named-file [directory show-id]
  (let [bookmarked (checkpoint-file directory show-id "manual.edn")]
    (when (and (.isFile bookmarked) (<= (.length bookmarked) 16384))
      (try
        (let [{:keys [path] :as entry} (edn/read-string (slurp bookmarked :encoding "UTF-8"))]
          (when (and (= show-id (:show-id entry)) (string? path) (.isAbsolute (io/file path)))
            (io/file path)))
        (catch Exception _ nil)))))

;; A Mac show folder autosaves to its own directory (#669). Before that, every
;; desktop show autosaved to one shared directory, which `lein run` and other
;; hosts still use. The first time a Mac folder claims its own recovery, the
;; shared directory's working and prior checkpoints are frozen once into
;; before-folders/, which nothing autosaves to. Every folder's owner offers
;; those frozen files as :legacy and :legacy-prior until an operator applies
;; one to a folder or dismisses them (#742). Nothing restores them
;; automatically and no file is moved or deleted. Applying or dismissing holds
;; a lock in the frozen directory and writes the resolution marker there, so
;; exactly one window resolves them and none offers them again.

(def before-folders-name
  "The directory under the shared recovery directory holding its frozen
  checkpoints from before per-folder recovery."
  "before-folders")

(def legacy-resolution-name
  "The file beside the frozen checkpoints that records where they went."
  "legacy-resolution.edn")

(def legacy-kinds
  "The frozen checkpoints a Mac show folder can offer, by checkpoint kind."
  (array-map :legacy "recovery.transit" :legacy-prior "prior.checkpoint"))

(def ^:private legacy-lock-name ".resolution.lock")

(def ^:private one-time-copy-marker
  "#739 copied the shared checkpoint into one folder and wrote this marker,
  holding that folder's identity."
  "offered-to-folder")

(def ^:private freeze-lock (Object.))

(defonce ^:private held-locks
  ;; Canonical paths of the file locks this process holds through
  ;; `try-file-lock`. An OS file lock belongs to the whole process, and on
  ;; macOS closing any channel on the file drops it, so a second caller in
  ;; this JVM must be refused before it opens a channel of its own.
  (atom #{}))

(defn try-file-lock
  "Take an exclusive lock on `file` for this process: {:release! :valid?},
  or nil while this process or another holds it. A refused caller in this
  JVM never opens the file, so it cannot drop the holder's lock. The lock is
  released by `:release!` or when the process ends."
  [file]
  (let [file (io/file file)
        _ (io/make-parents file)
        path (str (.getCanonicalFile file))
        [before _] (swap-vals! held-locks conj path)]
    (when-not (contains? before path)
      (let [forget! #(swap! held-locks disj path)
            ^FileChannel channel (try (FileChannel/open (.toPath (io/file path))
                                           (into-array OpenOption [StandardOpenOption/CREATE StandardOpenOption/WRITE]))
                         (catch Throwable error (forget!) (throw error)))
            ^FileLock lock (try (.tryLock channel)
                                (catch OverlappingFileLockException _ nil)
                                (catch Throwable error (.close channel) (forget!) (throw error)))]
        (if lock
          (let [released (atom false)]
            {:valid? #(.isValid lock)
             :release! (fn [] (when (compare-and-set! released false true)
                                (try (.release lock) (finally (.close channel) (forget!)))))})
          ;; Another process holds it; this process held no lock on the file.
          (do (.close channel) (forget!) nil))))))

(defn- delete-tree! [^java.io.File file]
  (when (.isDirectory file) (run! delete-tree! (.listFiles file)))
  (.delete file))

(defn- freeze-show-directory! [^java.io.File shared ^java.io.File frozen]
  (let [working (io/file shared "recovery.transit")
        prior (io/file shared prior-name)
        files (cond-> []
                (.isFile working) (conj working)
                (and (.isFile prior) (nil? (discarded-hash prior))) (conj prior))
        copied-to (let [marker (io/file shared one-time-copy-marker)]
                    (when (.isFile marker) (not-empty (str/trim (slurp marker :encoding "UTF-8")))))]
    (when (seq files)
      (.mkdirs frozen)
      (doseq [^java.io.File file files]
        (let [copy (.toPath (io/file frozen (.getName file)))]
          (Files/copy (.toPath file) copy
                      ^"[Ljava.nio.file.CopyOption;" (into-array CopyOption [StandardCopyOption/COPY_ATTRIBUTES]))
          ;; Durable before the directory move publishes it.
          (with-open [channel (FileChannel/open copy (into-array OpenOption [StandardOpenOption/WRITE]))]
            (.force channel true))))
      (when copied-to
        ;; That folder already holds this session as its own checkpoint.
        (spit (io/file frozen legacy-resolution-name)
              (pr-str {:resolution :applied :folder-id copied-to :by :one-time-copy}) :encoding "UTF-8")))))

(defn freeze-shared!
  "Freeze the shared directory `base`'s checkpoints once: copy each show's
  working and undiscarded prior files, with their times, into
  before-folders/<show>/. Later calls, from any process, change nothing, so a
  later `lein run` session is never offered as one from before the update.
  Returns true when this call froze them."
  [base]
  (let [base (io/file base)
        target (io/file base before-folders-name)]
    (when-not (.exists target)
      (.mkdirs base)
      (locking freeze-lock
        (with-open [channel (FileChannel/open (.toPath (io/file base ".before-folders.lock"))
                                              (into-array OpenOption [StandardOpenOption/CREATE StandardOpenOption/WRITE]))]
          (let [lock (.lock channel)]
            (try
              (when-not (.exists target)
                (let [staging (io/file base (str ".before-folders-" (UUID/randomUUID) ".tmp"))]
                  (try
                    (.mkdirs staging)
                    (doseq [^java.io.File directory (.listFiles base)
                            :when (and (.isDirectory directory) (re-matches #"[0-9a-f]{64}" (.getName directory)))]
                      (freeze-show-directory! directory (io/file staging (.getName directory))))
                    (Files/move (.toPath staging) (.toPath target) (make-array CopyOption 0))
                    true
                    (finally (when (.exists staging) (delete-tree! staging))))))
              (finally (.release lock)))))))))

(defn- legacy-show-directory [{:keys [directory legacy-directory]} show-id]
  (when (and legacy-directory
             (not= (.getCanonicalFile (io/file directory)) (.getCanonicalFile (io/file legacy-directory))))
    (show-directory (io/file legacy-directory before-folders-name) show-id)))

(defn- read-resolution [^java.io.File marker]
  (when (.isFile marker)
    (try (let [value (edn/read-string (slurp marker :encoding "UTF-8"))]
           (if (map? value) value {:resolution :unknown}))
         (catch Exception _ {:resolution :unknown}))))

(defn legacy-resolution
  "Where the checkpoints from before the update for `show-id` went, as
  recorded when an operator applied or dismissed them, or nil."
  [owner show-id]
  (some-> (legacy-show-directory owner show-id) (io/file legacy-resolution-name) read-resolution))

(defn- legacy-file ^java.io.File [owner show-id kind]
  (when-let [directory (legacy-show-directory owner show-id)]
    (when-not (.exists (io/file directory legacy-resolution-name))
      (let [file (io/file directory (get legacy-kinds kind))]
        (when (.isFile file) file)))))

(defn refresh-legacy!
  "Read which checkpoints from before the update this folder offers for
  `show-id`, cache them for `legacy-offer` and return them."
  [owner show-id]
  (let [checkpoints (into {} (keep (fn [kind]
                                     (when-let [^java.io.File file (legacy-file owner show-id kind)]
                                       [kind {:modified-at-ms (.lastModified file)}])))
                          (keys legacy-kinds))]
    (some-> (:legacy owner) (reset! {:show-id show-id :checkpoints checkpoints}))
    checkpoints))

(defn legacy-offer
  "The checkpoints from before the update this folder last found for
  `show-id`, as {kind {:modified-at-ms …}}, without touching the disk; nil
  when none is offered."
  [owner show-id]
  (let [{cached-id :show-id :keys [checkpoints]} (some-> (:legacy owner) deref)]
    (when (= show-id cached-id) (not-empty checkpoints))))

(defn source-hash
  "The SHA-256 of a checkpoint source's UTF-8 bytes."
  [^String source]
  (.formatHex (HexFormat/of) (.digest (MessageDigest/getInstance "SHA-256") (.getBytes source StandardCharsets/UTF_8))))

(defn- resolved-message [{:keys [resolution folder-id]}]
  (case resolution
    :applied (str "The checkpoint from before the update was already restored into "
                  (if folder-id (str "show folder " folder-id) "another show folder") ".")
    :dismissed "The checkpoint from before the update was dismissed."
    "The checkpoint from before the update is no longer offered."))

(defn reserve-legacy!
  "Hold the checkpoints from before the update for `show-id` while this show
  applies or dismisses them: {:directory :release!}. Refuses with
  :code :legacy-busy while another window holds them, :legacy-resolved once
  they were applied or dismissed and :legacy-unavailable when there are none.
  The lock is released by `finish-legacy!`, by `:release!` or when the
  process ends."
  [owner show-id]
  (let [^java.io.File directory (legacy-show-directory owner show-id)]
    (when-not (and directory (.isDirectory directory))
      (throw (ex-info "No checkpoint from before the update is offered" {:code :legacy-unavailable})))
    (let [{:keys [release! valid?]}
          (or (try-file-lock (io/file directory legacy-lock-name))
              (throw (ex-info "Another show window is restoring or dismissing the checkpoint from before the update"
                              {:code :legacy-busy})))]
      (if-let [resolution (read-resolution (io/file directory legacy-resolution-name))]
        (do (release!)
            (throw (ex-info (resolved-message resolution)
                            {:code :legacy-resolved :resolution (select-keys resolution [:resolution :folder-id])})))
        {:directory directory :show-id show-id :release! release! :valid? valid?}))))

(defn finish-legacy!
  "Record a held reservation as :applied or :dismissed with `details`, stop
  offering it everywhere and release it. Returns the record."
  [owner {:keys [directory show-id release!]} resolution details]
  (when-not (#{:applied :dismissed} resolution)
    (throw (ex-info "Unknown legacy checkpoint resolution" {:resolution resolution})))
  (try
    (let [record (merge details {:resolution resolution :show-id show-id :at-ms (System/currentTimeMillis)})]
      (write-source! (io/file directory legacy-resolution-name) (pr-str record))
      (refresh-legacy! owner show-id)
      record)
    (finally (release!))))

(defn dismiss-legacy!
  "Dismiss the checkpoints from before the update for `show-id`, recording
  the SHA-256 of each dismissed file. Returns the record, or nil when none is
  offered or they were already resolved; refuses while another window holds
  them."
  [owner show-id details]
  (when-let [reservation (try (reserve-legacy! owner show-id)
                              (catch clojure.lang.ExceptionInfo error
                                (when (= :legacy-busy (:code (ex-data error))) (throw error))))]
    (let [files (into {} (keep (fn [[_ name]]
                                 (let [file (io/file (:directory reservation) name)]
                                   (when (.isFile file) [name (checkpoint-hash file)]))))
                      legacy-kinds)]
      (if (seq files)
        (finish-legacy! owner reservation :dismissed (assoc details :sha256 files))
        (do ((:release! reservation)) nil)))))

(defn latest-file [{:keys [directory] :as owner} show-id kind]
  (let [^java.io.File file (case kind
               :recovery (checkpoint-file directory show-id "recovery.transit")
               :named (named-file directory show-id)
               :prior (prior-file owner show-id)
               (:legacy :legacy-prior) (do (refresh-legacy! owner show-id) (legacy-file owner show-id kind))
               (throw (ex-info "Unknown show checkpoint kind" {:kind kind})))]
    (when (and file (.isFile file)) file)))
