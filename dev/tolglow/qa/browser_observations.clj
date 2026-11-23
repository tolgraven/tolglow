(ns tolglow.qa.browser-observations
  "A preview-only Studio. The Node rehearsal controls a real host consumer
  through this process's stdin; the Studio's HTTP API stays unchanged."
  (:require [afterglow.reactive :as graph]
            [clojure.data.json :as json]
            [tolglow.demo :as demo]
            [tolglow.show :as show]
            [tolglow.studio :as studio]))

(defn- emit! [value]
  (println (str "OBSERVATION-QA " (json/write-str value)))
  (flush))

(defn -main [& _]
  (let [runtime (show/start! {:model (demo/model) :workspace-options nil})
        studio (studio/listen! runtime {:host "127.0.0.1" :port 0})
        reader (atom nil)]
    (try
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
