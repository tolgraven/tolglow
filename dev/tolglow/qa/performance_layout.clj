(ns tolglow.qa.performance-layout
  "Run the optimized Studio against a preview-only scene with four differently
  configured concurrent effects. Pass a directory for screenshots and optionally
  authoring for the Compose and Rig checks."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.qa.push-parameters :as parameters]
            [tolglow.show :as show]
            [tolglow.signals :as signals]))

(defn -main [& [directory scenario]]
  (let [script (case scenario
                 nil "dev/studio/performance-layout-qa.mjs"
                 "authoring" "dev/studio/compose-rig-layout-qa.mjs"
                 (throw (ex-info "Unknown Studio browser scenario" {:scenario scenario})))
        waves (let [registry (signals/new-registry)
                    sine (get-in registry [:waveforms :sine])]
                (signals/publish registry (assoc sine :id "custom/sine" :name "Studio test sine")))
        model (-> (demo/model)
                  (assoc :waveforms waves)
                  (assoc-in [:registry :effects :selector] {:id :selector :name "Wave selector" :version 1
                                                            :params {:wave (signals/selector-schema waves [:sine :triangle])}
                                                            :outputs {:dimmer {:op :param :key :wave}}})
                  (assoc-in [:registry :cues :selector] {:id :selector :name "Wave selector" :effect-id :selector :version 1})
                  (assoc-in [:pages :main :cells [1 2]] :selector)
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
      (let [{:keys [exit out err]} (shell/sh "node" script origin directory)]
        (print out) (flush)
        (when-not (zero? exit) (throw (ex-info "Studio layout browser check failed" {:scenario script :error err}))))
      (finally (app/stop! host) (shutdown-agents)))))
