(ns tolglow.qa.scene-audition
  "Run the optimized Studio against a preview-only show and audition a Room
  draft in a real browser. Pass a directory for screenshots and optionally a
  port; the server binds to 127.0.0.1 and sends no lighting output."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.show :as show]))

(defn -main [& [directory port]]
  (let [host (app/start! {:init {:components [:show] :modules [:web-server]}
                          :show {:model (demo/model) :workspace-options nil :recovery false}
                          :web-server {:host "127.0.0.1" :port (if port (parse-long port) 0)}})
        origin (str "http://127.0.0.1:" (get-in host [:application :modules :web-server :port]))
        directory (or directory "target/scene-audition")]
    (try
      (.mkdirs (io/file directory))
      (let [ack @(show/submit! host :command {:op :cue/launch :cue-id :ember :launch {:mode :persistent}})]
        (when-not (= :accepted (:status ack)) (throw (ex-info "Could not launch the live cue" ack)))
        (let [{:keys [exit out err]} (shell/sh "node" "dev/studio/scene-audition-qa.mjs" origin directory (:instance-id ack))]
          (print out) (flush)
          (when-not (zero? exit)
            (binding [*out* *err*] (print err) (flush))
            (throw (ex-info "Room audition browser check failed" {:error err})))))
      (finally (app/stop! host) (shutdown-agents)))))
