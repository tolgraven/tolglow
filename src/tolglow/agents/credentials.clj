(ns tolglow.agents.credentials
  "Explicit local credential-store adapter. Secrets are kept outside portable
  show documents and code. Files use owner-only POSIX permissions and atomic
  replacement. Constructing an adapter performs no read/write or network IO."
  (:require [clojure.edn :as edn]
            [clojure.string :as str])
  (:import [java.nio.file Files Path Paths LinkOption StandardCopyOption StandardOpenOption]
           [java.nio.file.attribute PosixFilePermissions FileAttribute]
           [java.nio.channels FileChannel]
           [java.nio ByteBuffer]
           [java.nio.charset StandardCharsets]
           [java.io PushbackReader StringReader]
           [java.util UUID]))

(defn- failure [code message] (ex-info message {:credentials/error code}))
(defn valid-reference? [reference]
  (and (string? reference) (boolean (re-matches #"credential-[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}" reference))))
(defn- valid-secret? [secret]
  (and (string? secret) (not (str/blank? secret)) (<= (count secret) 16384) (not (re-find #"[\r\n]" secret))))
(defn- valid-audience? [value]
  (and (string? value) (not (str/blank? value)) (<= (count value) 8192)))

(defn- valid-entry? [entry]
  (or (valid-secret? entry)
      (and (map? entry) (= #{:audience :secret} (set (keys entry)))
           (valid-audience? (:audience entry)) (valid-secret? (:secret entry)))))

(def ^:private no-links (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))
(def ^:private owner-rw (PosixFilePermissions/fromString "rw-------"))
(def ^:private owner-rwx (PosixFilePermissions/fromString "rwx------"))
(defn- attrs [permissions] (into-array FileAttribute [(PosixFilePermissions/asFileAttribute permissions)]))

(defn- read-store [^Path path]
  (if-not (Files/exists path no-links)
    {:version 1 :credentials {}}
    (do
      (when (or (Files/isSymbolicLink path) (not (Files/isRegularFile path no-links)))
        (throw (failure :invalid-file "Credential path must be a regular file, not a symbolic link")))
      (when-not (= owner-rw (Files/getPosixFilePermissions path no-links))
        (throw (failure :unsafe-permissions "Credential file must have owner read/write permissions only (0600)")))
      (when (> (Files/size path) 1048576)
        (throw (failure :too-large "Credential store exceeds its size limit")))
      (let [data (try (with-open [reader (PushbackReader. (StringReader. (String. (Files/readAllBytes path) StandardCharsets/UTF_8)))]
                       (let [options {:readers {} :eof ::end :default (fn [& _] (throw (Exception. "tagged value")))}
                             data (edn/read options reader)]
                         (when-not (= ::end (edn/read options reader)) (throw (Exception. "trailing value"))) data))
                      (catch Exception _ (throw (failure :invalid-file "Credential store contains invalid EDN"))))]
        (when-not (and (map? data) (= #{:version :credentials} (set (keys data))) (#{1 2} (:version data)) (map? (:credentials data))
                       (<= (count (:credentials data)) 1024)
                       (every? (fn [[reference secret]]
                                 (and (valid-reference? reference)
                                      ((if (= 1 (:version data)) valid-secret? valid-entry?) secret)))
                               (:credentials data)))
          (throw (failure :invalid-file "Credential store has an invalid schema")))
        data))))

(defn- write-store! [^Path path data]
  (let [parent (.getParent path)
        bytes (.getBytes (pr-str data) StandardCharsets/UTF_8)]
    (when (> (alength bytes) 1048576) (throw (failure :too-large "Credential store exceeds its size limit")))
    ;; Existing parent permissions are not changed; newly created directories
    ;; are private. An existing shared parent such as /tmp is never chmod'ed.
    (Files/createDirectories parent (attrs owner-rwx))
    (let [temp (Files/createTempFile parent ".tolglow-credentials-" ".tmp" (attrs owner-rw))]
      (try
        (with-open [channel (FileChannel/open temp (into-array StandardOpenOption [StandardOpenOption/WRITE]))]
          (let [buffer (ByteBuffer/wrap bytes)] (while (.hasRemaining buffer) (.write channel buffer)))
          (.force channel true))
        ;; Fail on filesystems without atomic replacement; never fall back to
        ;; exposing a partially rewritten credential document.
        (Files/move temp path (into-array StandardCopyOption [StandardCopyOption/ATOMIC_MOVE StandardCopyOption/REPLACE_EXISTING]))
        (finally (Files/deleteIfExists temp))))))

(defn- access [path lock audience]
  (let [lookup (fn [data reference]
                 (when-not (valid-reference? reference)
                   (throw (failure :invalid-reference "Invalid credential reference")))
                 (let [entry (get-in data [:credentials reference])]
                   (if audience
                     (when (and (map? entry) (= audience (:audience entry))) (:secret entry))
                     (when (string? entry) entry))))]
    {:put! (fn [secret]
             (when-not (valid-secret? secret) (throw (failure :invalid-secret "Credential must be a bounded single-line string")))
             (locking lock
               (let [data (read-store path) reference (str "credential-" (UUID/randomUUID))]
                 (when (>= (count (:credentials data)) 1024) (throw (failure :full "Credential store is full")))
                 (write-store! path (assoc-in (cond-> data audience (assoc :version 2)) [:credentials reference]
                                             (if audience {:audience audience :secret secret} secret)))
                 reference)))
     :get! (fn [reference] (locking lock (lookup (read-store path) reference)))
     :delete! (fn [reference]
                (locking lock
                  (let [data (read-store path)
                        present? (some? (lookup data reference))]
                    (when present? (write-store! path (update data :credentials dissoc reference)))
                    present?)))}))

(defn file-store
  "An owner-only file store with :put!, :get! and :delete!. Call (:scope store)
  with an audience to obtain an adapter that can only access that audience's
  entries. All scopes share the same lock and atomic file replacement. Use one
  adapter per path per process. Construction performs no IO."
  [path]
  (when-not (or (string? path) (instance? Path path))
    (throw (failure :invalid-path "Credential store requires an explicit filesystem path")))
  (let [^Path source (if (instance? Path path) path (Paths/get ^String path (make-array String 0)))
        path (.normalize (.toAbsolutePath source))
        lock (Object.)]
    (assoc (access path lock nil) :kind :local-file
           :scope (fn [audience]
                    (when-not (valid-audience? audience) (throw (failure :invalid-audience "Invalid credential audience")))
                    (access path lock audience)))))
