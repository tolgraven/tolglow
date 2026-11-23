(ns tolglow.qa.developer-output
  "Exercise Studio's cursor streams with a local provider fixture and an actual Docker PTY."
  (:require [clojure.data.json :as json]
            [clojure.java.shell :as shell]
            [tolglow.demo :as demo]
            [tolglow.agents.credentials :as credentials]
            [tolglow.development.agents-test :as agent-fixture]
            [tolglow.development.workspace :as workspace]
            [tolglow.development.workspace-test :as fixture]
            [tolglow.show :as show]
            [tolglow.studio :as studio])
  (:import [java.net InetAddress ServerSocket]
           [java.nio.file Files Path Paths]))

(defn- provider [^ServerSocket release]
  (fn [{:keys [on-line!] :as request}]
    (let [publish (fn [id text]
                    (on-line! :stdout (json/write-str {:type "item.completed"
                                                      :item {:id id :type "agent_message" :text text}})))]
      (publish "first" "Checking the fixture configuration.")
      (with-open [_ (.accept release)] (publish "second" "The fixture configuration is ready."))
      (agent-fixture/completed! request))))

(defn- exercise! [^Path root output ^ServerSocket release]
  (let [repos (into {} (for [id [:tolglow :afterglow]] [id (str (fixture/repo! root (name id)))]))
        store (credentials/file-store (.resolve root "credentials.edn"))
        reference ((:put! store) agent-fixture/secret)
        runtime (show/create-runtime {:model (demo/model) :credential-store store
                                      :workspace-options
                                      {:workspace-parent (str (.resolve root "workspaces")) :repositories repos
                                       :terminal-options {:network :none}
                                       :app-agent-options {:providers {:codex {:credential-ref reference}}
                                                           :execute! (provider release)}}})
        host (studio/listen! runtime {:host "127.0.0.1" :port 0})]
    (try
      (workspace/create! (get-in runtime [:development :workspaces]) {:id "lighting"})
      (let [origin (str "http://127.0.0.1:" (:port host))
            {:keys [exit out err]} (shell/sh "node" "dev/studio/developer-output-qa.mjs" origin output
                                             (str (.getLocalPort release)))]
        (print out)
        (when-not (zero? exit)
          (binding [*out* *err*] (print err) (flush))
          (throw (ex-info "Developer output browser check failed" {:stderr err}))))
      (finally ((:stop! host)) (show/stop! runtime)))))

(defn -main [& [output]]
  ;; The Docker daemon needs the same absolute path to the owned worktree as the JVM.
  (let [target (.toAbsolutePath (Paths/get "target" (make-array String 0)))
        _ (Files/createDirectories target fixture/attrs)
        root (Files/createTempDirectory target "development-output-" fixture/attrs)]
    (try
      (with-open [release (doto (ServerSocket. 0 1 (InetAddress/getByName "127.0.0.1")) (.setSoTimeout 90000))]
        (exercise! root (or output "target/developer-output") release))
      (finally (fixture/delete-tree! root) (shutdown-agents)))))
