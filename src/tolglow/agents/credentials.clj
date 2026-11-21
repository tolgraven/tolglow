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
        (when-not (and (map? data) (= #{:version :credentials} (set (keys data))) (= 1 (:version data)) (map? (:credentials data))
                       (<= (count (:credentials data)) 1024)
                       (every? (fn [[reference secret]] (and (valid-reference? reference) (valid-secret? secret))) (:credentials data)))
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

(defn file-store
  "Return {:put! [secret]->opaque-ref :get! [ref]->secret-or-nil :delete! [ref]}.
  Explicit path must be outside show exports/Git; the host chooses its local app
  config location. One adapter serializes its own operations. Use one adapter per
  file; concurrent independent processes are not a supported multiwriter store.
  POSIX private-file support is required. No real keys are generated here."
  [path]
  (when-not (or (string? path) (instance? Path path))
    (throw (failure :invalid-path "Credential store requires an explicit filesystem path")))
  (let [path (.normalize (.toAbsolutePath (if (instance? Path path) path (Paths/get ^String path (make-array String 0)))))
        lock (Object.)]
    {:kind :local-file
     :put! (fn [secret]
             (when-not (valid-secret? secret) (throw (failure :invalid-secret "Credential must be a bounded single-line string")))
             (locking lock
               (let [data (read-store path) reference (str "credential-" (UUID/randomUUID))]
                 (when (>= (count (:credentials data)) 1024) (throw (failure :full "Credential store is full")))
                 (write-store! path (assoc-in data [:credentials reference] secret))
                 reference)))
     :get! (fn [reference]
             (when-not (valid-reference? reference) (throw (failure :invalid-reference "Invalid credential reference")))
             (locking lock (get-in (read-store path) [:credentials reference])))
     :delete! (fn [reference]
                (when-not (valid-reference? reference) (throw (failure :invalid-reference "Invalid credential reference")))
                (locking lock
                  (let [data (read-store path) present? (contains? (:credentials data) reference)]
                    (when present? (write-store! path (update data :credentials dissoc reference)))
                    present?)))}))
