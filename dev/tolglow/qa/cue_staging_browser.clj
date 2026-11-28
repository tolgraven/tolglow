(ns tolglow.qa.cue-staging-browser
  "Run the optimized Studio against the preview-only demo show and exercise
  staged cue launches (#477) in headless Chrome. Phase studio stages Ember from
  its cue actions, edits the staged level twice, undoes and redoes one edit and
  launches it. Phase repl checks that a stage opened and edited from the REPL
  appears on the open page, then cancels it there. Usage:
  lein with-profile +dev run -m tolglow.qa.cue-staging-browser [directory] [port]"
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.show :as show]))

(defn- accepted! [ack what]
  (when-not (= :accepted (:status ack)) (throw (ex-info (str "Could not " what) {:ack ack})))
  ack)

(defn- node! [& args]
  (let [{:keys [exit out err]} (apply shell/sh "node" "dev/studio/cue-staging-browser-qa.mjs" args)]
    (print out) (flush)
    (when-not (zero? exit)
      (binding [*out* *err*] (print err) (flush))
      (throw (ex-info "Staged cue browser check failed" {:phase (last args) :error err})))))

(defn -main [& [directory port]]
  (let [port (Long/parseLong (or port "19920"))
        host (app/start! {:init {:components [:show] :modules [:web-server]}
                          :show {:model (demo/model) :workspace-options nil :recovery false}
                          :web-server {:host "127.0.0.1" :port port}})
        origin (str "http://127.0.0.1:" port)
        directory (or directory "target/cue-staging-browser")
        operator {:operator-id "repl/cue-staging-qa"}
        repl! (fn [command & [authority]]
                (accepted! @(show/submit! host :command (merge {:source {:adapter :repl :session-id "qa"}} command)
                                          authority)
                           (pr-str command)))]
    (try
      (.mkdirs (io/file directory))
      (node! origin directory "studio")
      (let [instances (vals (:instances (:model @(:state host))))]
        (println "After the Studio launch:" (mapv (juxt :cue-id (comp :level :params)) instances)
                 "history" (mapv (juxt :id :parameter :after :stage-id :cue-id) (:nodes (show/operator-history host)))))
      ;; REPL -> Studio: open and edit a stage, then let the page cancel it.
      (repl! {:op :cue-stage/open :cue-id :ice :stage-id "from-repl"})
      (repl! {:op :cue-stage/edit :stage-id "from-repl" :param :level :value 0.42} operator)
      (println "REPL staged:" (mapv (juxt :stage-id :status :params) (show/staged-cues host)))
      (node! origin directory "repl")
      (let [model (:model @(:state host))]
        (println "After the Studio cancel: stages" (count (show/staged-cues host))
                 "instances" (mapv :cue-id (vals (:instances model)))
                 "history" (mapv (juxt :id :stage-id) (:nodes (show/operator-history host))))
        (when (or (seq (show/staged-cues host)) (not= [:ember] (mapv :cue-id (vals (:instances model)))))
          (throw (ex-info "The Studio cancel left a stage or launched a cue" {}))))
      (finally (app/stop! host) (shutdown-agents)))))
