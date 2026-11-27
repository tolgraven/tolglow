(ns tolglow.qa.autopilot-browser
  "Exercise the optimized Studio on an ephemeral loopback preview show."
  (:require [tolglow.application :as app]
            [tolglow.demo :as demo])
  (:import [java.lang ProcessHandle]
           [java.util.concurrent TimeUnit]))

(defn- fixture-group [index fixture]
  (assoc fixture :groups [(if (< index 4) :front :rear)]
         :max-watts (if (even? index) 180.0 45.0)
         :measured-relative-brightness (if (even? index) 4.0 1.0)))

(defn- preview-show []
  (let [model (update-in (demo/model) [:rig :fixtures]
                         #(mapv fixture-group (range) %))]
    (app/start! {:init {:components [:show] :modules [:web-server]}
                 :show {:model model :workspace-options nil :recovery false}
                 :web-server {:host "127.0.0.1" :port 0}})))

(defn- check-browser! [port]
  (let [url (str "http://127.0.0.1:" port)
        process (-> (ProcessBuilder. ["node" "dev/studio/autopilot-browser-qa.mjs" url])
                    (.inheritIO) (.start))]
    (try
      (when-not (.waitFor process 180 TimeUnit/SECONDS)
        (throw (ex-info "Autopilot browser checks timed out" {:url url})))
      (when-not (zero? (.exitValue process))
        (throw (ex-info "Autopilot browser checks failed" {:exit (.exitValue process)})))
      (finally
        (when (.isAlive process)
          (with-open [children (.descendants process)]
            (doseq [child (.toArray children)] (.destroyForcibly ^ProcessHandle child)))
          (.destroyForcibly process))))))

(defn -main [& _]
  (let [show (preview-show)]
    (try (check-browser! (get-in show [:application :modules :web-server :port]))
         (finally (app/stop! show) (shutdown-agents)))))
