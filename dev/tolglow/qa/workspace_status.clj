(ns tolglow.qa.workspace-status
  "Exercise Studio workspace subscriptions before the first show frame."
  (:require [clojure.java.shell :as shell]
            [tolglow.demo :as demo]
            [tolglow.development.workspace :as workspace]
            [tolglow.development.workspace-test :as fixture]
            [tolglow.show :as show]
            [tolglow.studio :as studio])
  (:import [java.net InetAddress ServerSocket]
           [java.nio.file Path]))

(defn- run-browser! [output {:keys [^Path root first second]}]
  (with-open [gate (doto (ServerSocket. 0 1 (InetAddress/getByName "127.0.0.1"))
                     (.setSoTimeout 60000))]
    (let [runner (fn [request]
                   (with-open [_ (.accept gate)]
                     (workspace/run-trusted-process! request)))
          runtime (show/create-runtime
                   {:model (demo/model)
                    :workspace-options
                    {:repositories {:tolglow (str first) :afterglow (str second)}
                     :workspace-parent (str (.resolve root "workspace-status"))
                     :check-profiles {:unit {:label "Repository check" :repo :tolglow
                                             :argv ["git" "status" "--porcelain"]}}
                     :runner runner}})
          host (studio/listen! runtime {:host "127.0.0.1" :port 0})]
      (try
        (workspace/create! (get-in runtime [:development :workspaces]) {:id "lighting"})
        (let [origin (str "http://127.0.0.1:" (:port host))
              {:keys [exit out err]} (shell/sh "node" "dev/studio/workspace-status-qa.mjs"
                                               origin output (str (.getLocalPort gate)))]
          (print out)
          (when-not (zero? exit) (throw (ex-info "Workspace browser check failed" {:stderr err}))))
        (finally ((:stop! host)) (show/stop! runtime))))))

(defn -main [& [output]]
  (try (fixture/with-workspace {} (partial run-browser! (or output "target/workspace-status")))
       (finally (shutdown-agents))))
