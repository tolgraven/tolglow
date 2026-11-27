(ns tolglow.qa.browser-observations
  "A preview-only Studio. The Node rehearsal controls a real host consumer
  through this process's stdin; the Studio's HTTP API stays unchanged."
  (:require [afterglow.reactive :as graph]
            [clojure.data.json :as json]
            [tolglow.qa.push-scene :as scene]
            [tolglow.show :as show]
            [tolglow.studio :as studio]))

(defn- emit! [value]
  (println (str "OBSERVATION-QA " (json/write-str value)))
  (flush))

(defn- light-scene! [runtime]
  (doseq [cue scene/cue-ids]
    (let [receipt @(show/submit! runtime :command {:op :cue/launch :cue-id cue :launch {:mode :persistent}})]
      (when-not (= :accepted (:status receipt))
        (throw (ex-info "Could not light the rehearsal rig" {:receipt receipt}))))))

(defn -main [& _]
  (let [runtime (show/start! {:model (scene/scenic-show) :workspace-options nil :recovery false})
        studio (studio/listen! runtime {:host "127.0.0.1" :port 0})
        reader (atom nil)]
    (try
      (light-scene! runtime)
      (emit! {:port (:port studio)})
      (doseq [command (take-while #(not= "quit" %) (line-seq (java.io.BufferedReader. *in*)))]
        (case command
          "acquire" (do
                      (when-not @reader
                        (reset! reader (graph/acquire! (show/reactions runtime)
                                         [:browser/observation :preview.perform/center-rgb] identity)))
                      (emit! {:acquired true}))
          "read" (emit! {:sample (some-> @reader :signal deref)})
          "release" (do (when-let [lease @reader] ((:close! lease)) (reset! reader nil))
                        (emit! {:released true}))
          (throw (ex-info "Unknown rehearsal command" {:command command}))))
      (finally (when-let [lease @reader] ((:close! lease))) ((:stop! studio))
               (show/stop! runtime) (shutdown-agents)))))
