(ns tolglow.native.desktop
  "Own a compiled show process for the macOS Studio window."
  (:gen-class)
  (:require [clojure.java.io :as io]
            [tolglow.desktop.launch :as launch]
            [tolglow.fixed-host :as host]))

(defn -main [folder ready]
  (let [home (io/file (System/getProperty "user.home") "Library" "Application Support" "Tolglow" "Local Studio")]
    (when-not (or (.isDirectory home) (.mkdirs home))
      (throw (ex-info "Cannot create local Studio's settings folder" {:path (str home)})))
    (let [port (host/-open (str home) folder)]
      (try
        (launch/publish-port! (.toPath (io/file ready)) port)
        (println "Local compiled Studio listening on" (str "http://127.0.0.1:" port))
        (flush)
        (.read System/in)
        (finally (host/-close) (shutdown-agents))))))
