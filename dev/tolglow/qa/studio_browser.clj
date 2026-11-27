(ns tolglow.qa.studio-browser
  "Exercise Studio against a disposable Ring host with its own credential store."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [tolglow.application :as app]
            [tolglow.demo :as demo])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def scenarios {"assistant-layout" "dev/studio/assistant-layout-qa.mjs"
                "audience-zones" "dev/studio/audience-zones-qa.mjs"
                "event-feed-layout" "dev/studio/event-feed-layout-qa.mjs"
                "keyframe-ceilings" "dev/studio/keyframe-browser-qa.mjs"
                "reactive-effect-authoring" "dev/studio/reactive-effect-authoring-qa.mjs"
                "registry-editor-layout" "dev/studio/registry-editor-layout-qa.mjs"
                "room-readouts" "dev/studio/room-readout-qa.mjs"})

(defn run-browser! [script output show-options]
  (let [directory (io/file output)
        store (Files/createTempDirectory "tolglow-browser-qa" (make-array FileAttribute 0))]
    (try
      (let [host (app/start! {:init {:components [:show] :modules [:web-server]}
                              :show (merge {:model (demo/model) :workspace-options nil :recovery false} show-options
                                           {:credential-path (str (.resolve store "credentials.edn"))})
                              :web-server {:host "127.0.0.1" :port 0}})]
        (try
          (.mkdirs directory)
          (let [origin (str "http://127.0.0.1:" (get-in host [:application :modules :web-server :port]))
                {:keys [exit out err]} (shell/sh "node" script origin (str directory))]
            (print out) (flush)
            (when-not (zero? exit) (throw (ex-info "Studio browser check failed" {:script script :error err}))))
          (finally (app/stop! host))))
      (finally
        (doseq [file (reverse (file-seq (.toFile store)))] (io/delete-file file))
        (shutdown-agents)))))

(defn -main [scenario & [output]]
  (if-let [script (scenarios scenario)]
    (run-browser! script (or output (str "target/" scenario)) {})
    (throw (ex-info "Unknown browser scenario" {:scenario scenario}))))
