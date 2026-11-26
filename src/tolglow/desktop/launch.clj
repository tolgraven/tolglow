(ns tolglow.desktop.launch
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [tolglow.modules :as modules]
            [tolglow.show.recovery :as recovery])
  (:import [java.io File IOException]
           [java.nio.channels FileChannel OverlappingFileLockException]
           [java.nio.file CopyOption Files OpenOption Path StandardCopyOption StandardOpenOption]
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
  recovery or chose a directory."
  [cfg directory]
  (if (scopes-recovery? cfg)
    (let [settings (modules/settings cfg :show)
          value (:recovery settings)]
      (assoc cfg :show (assoc settings :recovery (assoc (if (map? value) value {}) :directory (str directory)))))
    cfg))

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

(defn- try-lock [^File directory]
  (.mkdirs directory)
  (let [channel (FileChannel/open (.toPath (io/file directory ".lock"))
                                  (into-array OpenOption [StandardOpenOption/CREATE StandardOpenOption/WRITE]))
        lock (try (.tryLock channel) (catch OverlappingFileLockException _ nil))]
    (if lock
      (fn release! [] (try (.release lock) (finally (.close channel))))
      (do (.close channel) nil))))

(def ^:private legacy-marker "offered-to-folder")

(defn offer-legacy-recovery!
  "Before per-folder recovery, every desktop show autosaved to the shared
  directory under the same show ID. Copy that show's files into a folder's new
  recovery directory once, so the first folder opened after the upgrade offers
  its previous session; a marker keeps any later folder from offering it again.
  Returns true when files were copied."
  [shared directory show-id]
  (let [legacy (recovery/show-directory shared show-id)
        target (recovery/show-directory directory show-id)
        files (filterv #(.isFile ^File %) (map #(io/file legacy %) recovery/checkpoint-names))
        marker (io/file legacy legacy-marker)]
    (if (and (seq files) (not (.exists marker))
             (not-any? #(.exists (io/file target %)) recovery/checkpoint-names))
      (do (.mkdirs target)
          (doseq [^File file files]
            (Files/copy (.toPath file) (.toPath (io/file target (.getName file)))
                        (into-array CopyOption [StandardCopyOption/COPY_ATTRIBUTES])))
          (spit marker (str (.getName (io/file directory)) "\n"))
          true)
      false)))

(defn claim-folder-recovery!
  "Claim this show folder's own recovery directory under `base` for this
  process: {:id :directory :release!}, or nil to keep the shared directory when
  the folder cannot hold an identity. A copied folder carries its original's
  identity, so when that identity's directory is already claimed by a running
  show the copy gets a new identity. The first claim of a new directory offers
  the shared directory's previous session for `show-id` once."
  ([folder base] (claim-folder-recovery! folder base :main))
  ([folder base show-id]
   (loop [id (folder-id! folder) renewed? false]
     (when id
       (let [directory (folder-recovery-directory base id)]
         (if-let [release! (try-lock directory)]
           (do (offer-legacy-recovery! base directory show-id)
               {:id id :directory directory :release! release!})
           (when-not renewed?
             (recur (try (write-folder-id! folder) (catch IOException _ nil)) true))))))))

(defn desktop-config
  "The configuration a Mac window starts for its folder: recovery in the
  folder's claimed `:recovery-directory` when given and, unless this window owns
  the devices, preview-only (see `device-free`)."
  [cfg {:keys [devices? owner recovery-directory] :or {devices? true}}]
  (cond-> cfg
    recovery-directory (scope-recovery recovery-directory)
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
