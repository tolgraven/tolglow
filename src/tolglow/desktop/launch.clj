(ns tolglow.desktop.launch
  (:require [clojure.java.io :as io]
            [tolglow.modules :as modules])
  (:import [java.nio.file Files Path StandardCopyOption]))

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
