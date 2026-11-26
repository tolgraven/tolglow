(ns tolglow.qa.personality-browser
  "Exercise the optimized Studio's track-feel picker on a loopback preview show.
  The show has no output sink and writes no recovery checkpoints."
  (:require [tolglow.application :as app]
            [tolglow.demo :as demo])
  (:import [java.lang ProcessHandle]
           [java.util.concurrent TimeUnit]))

(defn- preview-show [port]
  (app/start! {:init {:components [:show] :modules [:web-server]}
               :show {:model (demo/model) :workspace-options nil :recovery false}
               :web-server {:host "127.0.0.1" :port port}}))

(defn- check-browser! [port output]
  (let [url (str "http://127.0.0.1:" port)
        process (-> (ProcessBuilder. ["node" "dev/studio/personality-picker-qa.mjs" url output])
                    (.inheritIO) (.start))]
    (try
      (when-not (.waitFor process 180 TimeUnit/SECONDS)
        (throw (ex-info "Personality picker checks timed out" {:url url})))
      (when-not (zero? (.exitValue process))
        (throw (ex-info "Personality picker checks failed" {:exit (.exitValue process)})))
      (finally
        (when (.isAlive process)
          (with-open [children (.descendants process)]
            (doseq [child (.toArray children)] (.destroyForcibly ^ProcessHandle child)))
          (.destroyForcibly process))))))

(defn -main
  "Arguments: an optional loopback port (default: any free port) and an output
  directory for screenshots (default target/personality-picker)."
  [& [port output]]
  (let [show (preview-show (if port (parse-long port) 0))]
    (try (check-browser! (get-in show [:application :modules :web-server :port]) (or output "target/personality-picker"))
         (finally (app/stop! show) (shutdown-agents)))))
