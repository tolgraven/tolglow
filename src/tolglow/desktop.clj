(ns tolglow.desktop
  "Own the show process started by the macOS Studio window."
  (:require [clojure.java.io :as io]
            [tolglow.application :as app]
            [tolglow.data :as data]
            [tolglow.desktop.launch :as launch]
            [tolglow.show.recovery :as recovery])
  (:import [clojure.lang Compiler DynamicClassLoader RT]
           [java.io File]
           [java.nio.file Path]))

(defn folder-config [folder]
  (let [config (io/file folder "tolglow.edn")
        source (io/file folder "show.clj")]
    (cond
      (.isFile config) (data/read-edn (slurp config))
      (.isFile source) (do (load-file (str source))
                           (if-let [v (resolve 'show/config)] @v
                             (throw (ex-info "show.clj must define show/config" {}))))
      :else (throw (ex-info "Choose a folder containing tolglow.edn or show.clj" {})))))

(defn run-show!
  "Run the show in this folder until `input` ends. One process owns one show:
  user namespaces such as `show` are global to a Clojure runtime, so the Mac
  app starts a process per window. Options: :devices? false starts it
  preview-only for :owner (see `launch/device-free`); :recovery-base gives the
  folder its own recovery directory there (see `launch/claim-folder-recovery!`)
  and offers, without applying it, the checkpoint the shared directory kept
  from before per-folder recovery (see `launch/claim-options`)."
  ([folder ready input] (run-show! folder ready input {}))
  ([^File folder ^Path ready input {:keys [recovery-base] :as options}]
   ;; The user's namespace root is scoped to this show process. A URL keeps paths
   ;; containing a colon or spaces intact and leaves bundled resources first.
   (with-open [loader (doto (DynamicClassLoader. (RT/baseLoader)) (.addURL (.toURL (.toURI folder))))]
     (with-bindings {Compiler/LOADER loader}
       (let [cfg (launch/with-application-sources (folder-config folder))
             claim (when (and recovery-base (launch/scopes-recovery? cfg))
                     (launch/claim-folder-recovery! folder recovery-base))]
         (try
           (let [cfg (launch/desktop-config cfg (merge options (launch/claim-options claim)))
                 ;; The window owns a private ephemeral listener even if this folder
                 ;; normally starts Studio through TLS or a LAN reverse proxy.
                 local-web {:enabled true :host "127.0.0.1" :port 0}
                 runtime (app/start! (assoc cfg :web-server local-web))]
             (try
               (let [port (get-in runtime [:application :modules :web-server :port])]
                 (when-not (pos-int? port)
                   (throw (ex-info "Include :web-server in the config's :init modules" {})))
                 (launch/publish-port! ready port)
                 ;; Parent exit, including a crash, closes the only writer.
                 (.read ^java.io.InputStream input))
               (finally (app/stop! runtime))))
           (finally (some-> claim :release! (apply [])))))))))

(defn -main [folder ready]
  (try (run-show! (.getCanonicalFile (io/file folder)) (.toPath (io/file ready)) System/in
                  (assoc (launch/desktop-options) :recovery-base (recovery/default-directory)))
       (finally (shutdown-agents))))
