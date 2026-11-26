(ns tolglow.desktop.launch
  (:require [clojure.java.io :as io]
            [tolglow.modules :as modules]
            [tolglow.show.recovery :as recovery])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file Files Path StandardCopyOption]
           [java.security MessageDigest]
           [java.util HexFormat]))

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

(defn device-free
  "This configuration without physical output or device modules. The Mac app
  opens a show this way while another window's show owns the devices, so a
  second show can be edited and previewed but cannot take over the first
  show's interfaces or arm output. A show's own custom modules are outside this."
  [cfg]
  (reduce (fn [cfg id] (assoc cfg id (assoc (modules/settings cfg id) :enabled false)))
          (assoc cfg :show (dissoc (modules/settings cfg :show) :ola :artnet))
          device-modules))

(defn- digest [^String text]
  (.formatHex (HexFormat/of) (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text StandardCharsets/UTF_8))))

(defn folder-recovery-directory
  "Recovery files for the show in this folder. Every new show model has the same
  show ID, and recovery files are keyed by show ID, so two folders open at once
  would otherwise write and restore each other's working checkpoint."
  [folder]
  (io/file (recovery/default-directory) "folders" (digest (str (.getCanonicalFile (io/file folder))))))

(defn scope-recovery
  "Give this folder's show its own recovery directory unless the configuration
  chose a directory or disabled recovery."
  [cfg folder]
  (let [settings (modules/settings cfg :show)
        value (:recovery settings)]
    (if (or (false? value) (and (map? value) (contains? value :directory)))
      cfg
      (assoc cfg :show (assoc settings :recovery (assoc (if (map? value) value {})
                                                        :directory (str (folder-recovery-directory folder))))))))

(defn desktop-config
  "The configuration a Mac window starts for its folder: recovery scoped to the
  folder and, unless this window owns the devices, no device modules or output."
  [cfg folder {:keys [devices?] :or {devices? true}}]
  (cond-> cfg
    (seq (str folder)) (scope-recovery folder)
    (not devices?) device-free))

(defn device-options
  "Whether the Mac app gave this process the devices (TOLGLOW_DESKTOP_DEVICES)."
  ([] (device-options (System/getenv "TOLGLOW_DESKTOP_DEVICES")))
  ([value] {:devices? (not= "none" value)}))
