(ns tolglow.qa.audience-zones
  "Run operator UI checks against a disposable show with no physical outputs."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [tolglow.application :as app]
            [tolglow.demo :as demo])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn -main [& [output]]
  (let [directory (io/file (or output "target/audience-zones"))
        _ (.mkdirs directory)
        store (Files/createTempDirectory "tolglow-audience-qa" (make-array FileAttribute 0))
        host (app/start! {:init {:components [:show] :modules [:web-server]}
                          :show {:model (demo/model) :workspace-options nil
                                 :credential-path (.resolve store "credentials.edn")}
                          :web-server {:host "127.0.0.1" :port 0}})]
    (try
      (let [origin (str "http://127.0.0.1:" (get-in host [:application :modules :web-server :port]))
            {:keys [exit out err]} (shell/sh "node" "dev/studio/audience-zones-qa.mjs" origin (str directory))]
        (print out) (flush)
        (when-not (zero? exit) (throw (ex-info "Audience operator browser check failed" {:error err}))))
      (finally
        (app/stop! host)
        (doseq [file (reverse (file-seq (.toFile store)))] (io/delete-file file))
        (shutdown-agents)))))
