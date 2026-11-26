(ns tolglow.show.recovery
  "An independently scheduled working file and one prior-session slot per show."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [tolglow.persistence :as persistence])
  (:import [java.io FileOutputStream OutputStream Reader]
           [java.nio.channels FileChannel]
           [java.nio.charset StandardCharsets]
           [java.nio.file CopyOption Files LinkOption OpenOption Path StandardCopyOption StandardOpenOption]
           [java.nio.file.attribute BasicFileAttributes FileAttribute]
           [java.security DigestInputStream MessageDigest SecureRandom]
           [java.util HexFormat]
           [java.util.concurrent ScheduledThreadPoolExecutor ThreadFactory TimeUnit]
           [javax.crypto Mac]
           [javax.crypto.spec SecretKeySpec]))

(defn default-directory
  "The per-user recovery directory when a show does not choose one."
  []
  (let [home (System/getProperty "user.home")]
    (if (str/includes? (str/lower-case (System/getProperty "os.name")) "mac")
      (io/file home "Library" "Application Support" "Tolglow" "Recovery")
      (io/file (or (not-empty (System/getenv "XDG_STATE_HOME")) (io/file home ".local" "state"))
               "tolglow" "recovery"))))

(defn options [value]
  (when-not (or (nil? value) (boolean? value) (map? value))
    (throw (ex-info "Invalid show recovery options" {})))
  (let [{:keys [enabled? interval-ms directory] :as options}
        (merge {:enabled? true :interval-ms 10000}
               (cond (false? value) {:enabled? false} (map? value) value))]
    (when-not (and (every? #{:enabled? :interval-ms :directory} (keys options))
                   (boolean? enabled?) (pos-int? interval-ms))
      (throw (ex-info "Invalid show recovery options" {})))
    (assoc options :directory (.getAbsoluteFile (io/file (or directory (default-directory)))))))

(defn show-directory
  "The directory under a recovery directory that holds one show's files."
  [directory show-id]
  (let [identity (.getBytes (pr-str show-id) StandardCharsets/UTF_8)]
    (io/file directory (.formatHex (HexFormat/of) (.digest (MessageDigest/getInstance "SHA-256") identity)))))

(def checkpoint-names
  "The fixed files a show keeps in its recovery directory."
  ["recovery.transit" "prior.checkpoint" "manual.edn"])

(defn- checkpoint-file [directory show-id name]
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
       (with-open [stream (FileOutputStream. (.toFile temp))]
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

(def ^:private link-options (make-array LinkOption 0))

(def ^:private source-open-options (into-array OpenOption [LinkOption/NOFOLLOW_LINKS]))

(def ^:private confirmation-key
  (delay (let [bytes (byte-array 32)]
           (.nextBytes (SecureRandom.) bytes)
           (SecretKeySpec. bytes "HmacSHA256"))))

(defn- source-identity [file]
  (when file
    (let [path (.toRealPath (.toPath (io/file file)) link-options)
          attrs (Files/readAttributes path BasicFileAttributes link-options)]
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

(defn- discarded-hash [prior]
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

(defn- preserve-prior! [working prior publish!]
  (prepare-file! prior
    (fn [temp]
      (Files/copy (.toPath working) temp (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING
                                                               StandardCopyOption/COPY_ATTRIBUTES]))
      (with-open [channel (FileChannel/open temp (into-array OpenOption [StandardOpenOption/WRITE]))]
        (.force channel true)))
    publish!))

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
          (swap! status assoc :last-saved (assoc (dissoc captured :source) :path path) :last-error nil))))
    (catch Exception error
      (when @open? (swap! status assoc :last-error (ex-message error))))))

(defn start! [{:keys [enabled? interval-ms] :as config} active? capture! publish!]
  (let [owner (assoc config :open? (atom true) :active? (atom active?)
                            :settled-shows (atom #{}) :status (atom {}))]
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

(defn- fixed-checkpoint-path? [directory file]
  (when-let [parent (some-> file .getParentFile)]
    (and (contains? #{"recovery.transit" "manual.edn" prior-name} (.getName file))
         (re-matches #"[0-9a-f]{64}" (.getName parent))
         (= directory (.getParentFile parent)))))

(defn manual-path! [{:keys [directory]} path]
  (let [file (.getAbsoluteFile (io/file path))
        directory (.getCanonicalFile directory)
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

(defn latest-file [{:keys [directory] :as owner} show-id kind]
  (let [file (case kind
               :recovery (checkpoint-file directory show-id "recovery.transit")
               :named (named-file directory show-id)
               :prior (prior-file owner show-id)
               (throw (ex-info "Unknown show checkpoint kind" {:kind kind})))]
    (when (and file (.isFile file)) file)))
