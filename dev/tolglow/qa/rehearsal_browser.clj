(ns tolglow.qa.rehearsal-browser
  "Run the optimized Studio against the R1o red show and check, in headless
  Chrome, the auto plan editor (#217) and the agent rehearsal panel (#670)
  from both sides: a REPL plan reaches Plan the wandering, a Studio edit
  reaches the REPL, both see a refusal; Studio arms (after confirming the
  show replacement), reads the review, starts and stops a club rehearsal
  that the REPL reads back; a REPL arm reaches the panel; Studio restores the
  show arming replaced and the REPL reads it. Usage:
  lein with-profile +dev run -m tolglow.qa.rehearsal-browser [directory] [port]"
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [tolglow.application :as app]
            [tolglow.automation.red-show :as red-show]
            [tolglow.rehearsal.session :as session]
            [tolglow.show :as show]))

(defn- browser! [origin directory phase]
  (let [{:keys [exit out err]} (shell/sh "node" "dev/studio/rehearsal-browser-qa.mjs" origin directory phase)]
    (print out) (flush)
    (when-not (zero? exit)
      (binding [*out* *err*] (print err) (flush))
      (throw (ex-info (str "Rehearsal browser check failed (" phase ")") {:error err})))))

(defn- check! [ok? message data] (when-not ok? (throw (ex-info message data))))

(defn -main [& [directory port]]
  (let [port (Long/parseLong (or port "19720"))
        host (app/start! {:init {:components [:show] :modules [:web-server]}
                          :show {:model (red-show/model) :workspace-options nil :recovery false}
                          :web-server {:host "127.0.0.1" :port port}})
        origin (str "http://127.0.0.1:" port)
        directory (or directory "target/rehearsal-browser")]
    (try
      (.mkdirs (io/file directory))
      ;; REPL -> Studio: a plan saved while auto is off.
      (let [ack @(show/submit! host :auto-plan {:plan {:palette red-show/red-palette :categories {:exclude [:strobe]}
                                                       :sections [{:id :lift :weight 3 :length {:bars 2} :transition :release
                                                                   :config {:scope {:cue-ids [:wash-color :accent]}}}
                                                                  {:id :hush :weight 1 :length {:bars 1} :transition :cut
                                                                   :config {:scope {:cue-ids [:spot :sweep]} :intent :darkness :max-lit 1}}]}})]
        (println "REPL plan:" (pr-str (select-keys ack [:status :enabled?])))
        (check! (= :accepted (:status ack)) "REPL plan refused" ack))
      (browser! origin directory "plan")
      (let [config (get-in (show/snapshot host) [:auto :config])]
        (println "After Studio save:" (pr-str {:sections (mapv :id (:sections config)) :max-lit (:max-lit config)
                                               :glow (get-in config [:sections 2])}))
        (check! (and (= [:lift :hush :glow] (mapv :id (:sections config))) (= 5 (:max-lit config))
                     (= {:cue-ids [:head-glow :sweep]} (get-in config [:sections 2 :config :scope]))
                     (= {:bars 1} (get-in config [:sections 2 :length])))
                "Studio's plan did not reach the show" {:config config}))
      ;; Studio arms, reviews, starts and stops a rehearsal; the REPL reads it.
      (browser! origin directory "rehearsal")
      (let [status (session/status host)]
        (println "REPL reads the rehearsal:" (pr-str (-> (select-keys status [:status :choice :beat :playing?])
                                                         (assoc :replies (mapv #(select-keys % [:section :accepted-beat])
                                                                               (get-in status [:run :agent]))))))
        (check! (and (= :stopped (:status status)) (false? (:playing? status))) "The REPL does not see the stopped run" status))
      ;; REPL arm -> Studio.
      (let [status (session/arm! host {:mode :concert :take :b :seed 7 :wait-ms 30000})]
        (check! (= :armed (:status status)) "REPL arm failed" status))
      (browser! origin directory "repl-armed")
      ;; Studio's Restore -> REPL: the red show with its plan is back.
      (let [model (:model @(:state host))]
        (println "After Studio restore:" (pr-str {:show-id (:show-id model) :cues (count (get-in model [:registry :cues]))
                                                  :sections (mapv :id (get-in model [:auto :config :sections]))}))
        (check! (and (= :gig-2023 (:show-id model)) (= [:lift :hush :glow] (mapv :id (get-in model [:auto :config :sections]))))
                "Studio's restore did not bring the show back" {:show-id (:show-id model)}))
      (finally
        (when-let [path (get-in (session/status host) [:before-rehearsal :path])] (.delete (io/file path)))
        (app/stop! host) (shutdown-agents)))))
