(ns tolglow.qa.performance-layout
  "Run the optimized Studio against a preview-only scene with four differently
  configured concurrent effects. Pass a directory for screenshots."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.qa.push-parameters :as parameters]
            [tolglow.show :as show]))

(defn -main [& [directory]]
  (let [model (-> (demo/model)
                  (assoc-in [:registry :effects :orbital] parameters/orbital-effect)
                  (assoc-in [:registry :cues :orbital] {:id :orbital :version 1 :name "Orbital wash"
                                                        :effect-id :orbital :swatch "#b6df91"})
                  (assoc-in [:pages :main :cells [0 2]] :orbital))
        host (app/start! {:init {:components [:show] :modules [:web-server]}
                          :show {:model model :workspace-options nil}
                          :web-server {:host "127.0.0.1" :port 0}})
        origin (str "http://127.0.0.1:" (get-in host [:application :modules :web-server :port]))
        directory (or directory "target/performance-layout")]
    (try
      (.mkdirs (io/file directory))
      (doseq [cue [:ember :breathe :sweep :orbital]]
        (let [ack @(show/submit! host :command {:op :cue/launch :cue-id cue :launch {:mode :persistent}})]
          (when-not (= :accepted (:status ack)) (throw (ex-info "Could not launch performance fixture" ack)))))
      (let [{:keys [exit out err]} (shell/sh "node" "dev/studio/performance-layout-qa.mjs" origin directory)]
        (print out) (flush)
        (when-not (zero? exit) (throw (ex-info "Performance layout browser check failed" {:error err}))))
      (finally (app/stop! host) (shutdown-agents)))))
