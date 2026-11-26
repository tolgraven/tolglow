(ns tolglow.desktop.launch
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [tolglow.modules :as modules]
            [tolglow.show.recovery :as recovery])
  (:import [java.io File IOException]
           [java.nio.file CopyOption Files Path StandardCopyOption]
           [java.util UUID]))

(defn publish-port!
  "Atomically publish the port of this window’s show to its private readiness path."
  [^Path ready port]
  (let [temporary (Files/createTempFile (.getParent ready) "listener" ".tmp"
                                        (make-array java.nio.file.attribute.FileAttribute 0))]
    (try
      (spit (.toFile temporary) (str port))
      (Files/move temporary ready (into-array StandardCopyOption [StandardCopyOption/ATOMIC_MOVE]))
      (finally (Files/deleteIfExists temporary)))))

(defn with-application-sources
  "Offer the app’s editable checkouts to this show unless it supplied its own workspace."
  [cfg]
  (if-let [sources (not-empty (System/getenv "TOLGLOW_APPLICATION_SOURCES"))]
    (let [settings (modules/settings cfg :show)]
      (if (contains? settings :workspace-options)
        cfg
        (let [options {:repositories {:tolglow (str (io/file sources "tolglow"))
                                      :afterglow (str (io/file sources "afterglow"))}
                       :workspace-parent (str (io/file sources "workspaces"))}]
          (assoc cfg :show (assoc settings :workspace-options options)))))
    cfg))

(def device-modules
  "Built-in modules that open a device, a MIDI or audio port or a fixed network
  listener. Two shows on one Mac would contend for each of them."
  [:push-3-display :rdm :osc :sensor-osc :clock-sync :audio])

(defn preview-only-message [owner]
  (str (if (str/blank? owner) "Another window's show" owner)
       " owns this Mac's devices and output, so this show is preview-only."))

(defn device-free
  "This configuration for a show opened while another window's show owns the
  Mac's devices (#669). Physical output and the built-in device modules are off
  at startup, and the show runs preview-only: its owner refuses every
  device-opening or output-arming action (`show/require-devices!`), from Studio,
  its REPL, configured modules and startup actions alike."
  [cfg owner]
  (reduce (fn [cfg id] (assoc cfg id (assoc (modules/settings cfg id) :enabled false)))
          (assoc cfg :show (-> (modules/settings cfg :show)
                               (dissoc :ola :artnet)
                               (assoc :preview-only (cond-> {:message (preview-only-message owner)}
                                                      (not (str/blank? owner)) (assoc :owner owner)))))
          device-modules))

(defn scopes-recovery?
  "True unless the configuration disabled recovery or chose its directory."
  [cfg]
  (let [value (:recovery (modules/settings cfg :show))]
    (not (or (false? value) (and (map? value) (contains? value :directory))))))

(defn scope-recovery
  "Point this show's recovery at `directory` unless the configuration disabled
  recovery or chose a directory. `legacy-directory` is the shared directory
  whose checkpoint from before per-folder recovery the show may offer (#742)."
  ([cfg directory] (scope-recovery cfg directory nil))
  ([cfg directory legacy-directory]
   (if (scopes-recovery? cfg)
     (let [settings (modules/settings cfg :show)
           value (:recovery settings)]
       (assoc cfg :show (assoc settings :recovery (cond-> (assoc (if (map? value) value {}) :directory (str directory))
                                                    legacy-directory (assoc :legacy-directory (str legacy-directory))))))
     cfg)))

(def folder-id-name
  "The file in a show folder that holds its stable identity."
  ".tolglow-folder-id")

(defn- read-folder-id [^File file]
  (when (and (.isFile file) (<= (.length file) 128))
    (let [text (str/trim (slurp file :encoding "UTF-8"))]
      (try (str (UUID/fromString text)) (catch IllegalArgumentException _ nil)))))

(defn- write-folder-id! [folder]
  (let [file (io/file folder folder-id-name)
        id (str (UUID/randomUUID))
        temporary (Files/createTempFile (.toPath (io/file folder)) ".tolglow-folder-id" ".tmp"
                                        (make-array java.nio.file.attribute.FileAttribute 0))]
    (try
      (spit (.toFile temporary) (str id "\n") :encoding "UTF-8")
      (Files/move temporary (.toPath file) (into-array CopyOption [StandardCopyOption/ATOMIC_MOVE
                                                                   StandardCopyOption/REPLACE_EXISTING]))
      id
      (finally (Files/deleteIfExists temporary)))))

(defn folder-id!
  "This show folder's stable identity: a UUID kept in `.tolglow-folder-id` and
  written on first open, so it survives renaming or moving the folder. nil
  when the folder cannot be written."
  [folder]
  (or (read-folder-id (io/file folder folder-id-name))
      (try (write-folder-id! folder) (catch IOException _ nil))))

(defn folder-recovery-directory
  "Recovery files for the show folder with identity `id` under `base`."
  [base id]
  (io/file base "folders" id))

(defn- try-lock
  "This process's lock on a folder's recovery directory, or nil while any
  show holds it (see `recovery/try-file-lock`)."
  [^File directory]
  (.mkdirs directory)
  (:release! (recovery/try-file-lock (io/file directory ".lock"))))

(def ^:private folder-record-name
  "The file in a folder's recovery directory naming the folder that last claimed it."
  "folder-path")

(defn- recorded-folder
  "The folder that last claimed `directory`, as recorded, or nil."
  [directory]
  (let [record (io/file directory folder-record-name)]
    (when (.isFile record)
      (not-empty (str/trim (slurp record :encoding "UTF-8"))))))

(defn- copied-from
  "The original folder when `directory` was last claimed from another folder
  that still exists and still carries identity `id`: `folder` is then a copy
  of it (a Finder duplicate, a backup or a clone of a repository that tracks
  the identity file), not the same folder moved or renamed, and must not share
  its recovery. nil otherwise."
  [directory folder id]
  (let [recorded (recorded-folder directory)]
    (when (and recorded
               (= id (read-folder-id (io/file recorded folder-id-name)))
               (not (try (Files/isSameFile (.toPath (io/file recorded)) (.toPath (io/file folder)))
                         (catch IOException _ false))))
      recorded)))

(defn- record-folder! [directory folder]
  (try (spit (io/file directory folder-record-name) (str (.getCanonicalFile (io/file folder)) "\n") :encoding "UTF-8")
       (catch IOException _ nil)))

(defn claim-folder-recovery!
  "Claim this show folder's own recovery directory under `base` for this
  process: {:id :directory :release! :base :folder :new? :copy-of}, or nil to
  keep the shared directory when the folder cannot hold an identity. A copied
  folder carries its original's identity, so the copy gets a new identity when
  that identity's directory is claimed by a running show, or was last claimed
  from the original folder, which still exists with the same identity;
  :copy-of then names the original. A moved or renamed folder keeps its
  identity. :new? is true when this claim wrote the identity file: the
  folder's first open, or a detected copy.

  The first claim under `base` freezes the shared directory's checkpoints
  once (`recovery/freeze-shared!`). The show offers them, never applies them,
  until an operator applies one to a folder or dismisses them (see
  `claim-options`)."
  [folder base]
  (let [renew #(try (write-folder-id! folder) (catch IOException _ nil))
        existing (read-folder-id (io/file folder folder-id-name))]
    (loop [id (or existing (renew)) renewed? false copy-of nil]
      (when id
        (let [directory (folder-recovery-directory base id)]
          (if-let [release! (try-lock directory)]
            (if-let [original (when-not renewed? (copied-from directory folder id))]
              (do (release!) (recur (renew) true original))
              (do (record-folder! directory folder)
                  (try (recovery/freeze-shared! base) (catch IOException _ nil))
                  (cond-> {:id id :directory directory :release! release! :base base
                           :folder (str (.getCanonicalFile (io/file folder)))
                           :new? (or renewed? (nil? existing))}
                    copy-of (assoc :copy-of copy-of))))
            (when-not renewed? (recur (renew) true (recorded-folder directory)))))))))

(defn claim-options
  "What a claim adds to `desktop-config`'s options: the folder's own recovery
  directory, the shared directory whose earlier checkpoint it offers, and the
  folder identity Studio and the REPL show (#742)."
  [{:keys [id directory base folder new? copy-of] :as claim}]
  (when claim
    {:recovery-directory directory
     :legacy-directory base
     :show-folder (cond-> {:id id :file folder-id-name :path folder :new? (boolean new?)}
                    copy-of (assoc :copy-of copy-of))}))

(defn desktop-config
  "The configuration a Mac window starts for its folder: recovery in the
  folder's claimed `:recovery-directory` when given, offering the
  `:legacy-directory` checkpoint, the folder identity as `:show-folder` and,
  unless this window owns the devices, preview-only (see `device-free`)."
  [cfg {:keys [devices? owner recovery-directory legacy-directory show-folder] :or {devices? true}}]
  (cond-> cfg
    recovery-directory (scope-recovery recovery-directory legacy-directory)
    show-folder (as-> cfg (assoc cfg :show (assoc (modules/settings cfg :show) :show-folder show-folder)))
    (not devices?) (device-free owner)))

(defn desktop-options
  "What the Mac app told this show process: whether it runs in a Mac window
  (TOLGLOW_DESKTOP_WINDOW), owns the devices (TOLGLOW_DESKTOP_DEVICES) and,
  if not, which show does (TOLGLOW_DESKTOP_DEVICE_OWNER)."
  ([] (desktop-options #(System/getenv %)))
  ([env]
   {:window? (= "1" (env "TOLGLOW_DESKTOP_WINDOW"))
    :devices? (not= "none" (env "TOLGLOW_DESKTOP_DEVICES"))
    :owner (env "TOLGLOW_DESKTOP_DEVICE_OWNER")}))
