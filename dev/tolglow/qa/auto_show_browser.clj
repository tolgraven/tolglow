(ns tolglow.qa.auto-show-browser
  "Run the optimized Studio against the R1o red-show example on the 2023 gig
  rig and check auto mode (#217) from both sides in headless Chrome: a REPL
  section plan reaches the open auto panel, Studio starts it again with varied
  parameters while keeping the plan, Studio and the REPL each see a refusal,
  and a REPL stop reaches the page. Usage:
  lein with-profile +dev run -m tolglow.qa.auto-show-browser [directory] [port]"
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [tolglow.application :as app]
            [tolglow.automation.red-show :as red-show]
            [tolglow.show :as show]))

(defn- browser! [origin directory phase]
  (let [{:keys [exit out err]} (shell/sh "node" "dev/studio/auto-show-browser-qa.mjs" origin directory phase)]
    (print out) (flush)
    (when-not (zero? exit)
      (binding [*out* *err*] (print err) (flush))
      (throw (ex-info (str "Auto show browser check failed (" phase ")") {:error err})))))

(defn- auto [host] (get-in (show/snapshot host) [:auto]))

(defn -main [& [directory port]]
  (let [port (Long/parseLong (or port "18620"))
        host (app/start! {:init {:components [:show] :modules [:web-server]}
                          :show {:model (red-show/model) :workspace-options nil}
                          :web-server {:host "127.0.0.1" :port port}})
        origin (str "http://127.0.0.1:" port)
        directory (or directory "target/auto-show-browser")]
    (try
      (.mkdirs (io/file directory))
      ;; REPL -> Studio: the plan and its live section reach the open panel.
      (let [ack @(show/submit! host :auto (red-show/auto-config 217))]
        (when-not (= :accepted (:status ack)) (throw (ex-info "REPL auto start refused" ack))))
      (Thread/sleep 1500)
      (println "REPL auto:" (pr-str (select-keys (auto host) [:enabled? :section])))
      (browser! origin directory "repl")
      ;; Studio -> REPL: Studio restarted it with varied parameters and the same plan.
      (browser! origin directory "studio")
      (let [{:keys [config enabled?]} (auto host)]
        (println "After Studio start:" (pr-str {:enabled? enabled? :sections (mapv :id (:sections config))
                                                :randomize (:randomize config)}))
        (when-not (and enabled? (= [:red :purple :quiet :strobe] (mapv :id (:sections config)))
                       (= :marked (get-in config [:randomize :params]))
                       (= red-show/red-palette (get-in config [:randomize :palette])))
          (throw (ex-info "Studio start did not keep the plan or vary parameters" {:config config}))))
      ;; A REPL refusal leaves the committed config unchanged.
      (let [before (auto host)
            ack @(show/submit! host :auto {:enabled? true :count 0})]
        (println "REPL refusal:" (pr-str (select-keys ack [:status :message])))
        (when-not (and (= :rejected (:status ack)) (= (:config before) (:config (auto host))))
          (throw (ex-info "Invalid REPL config was not refused cleanly" {:ack ack}))))
      ;; REPL stop -> Studio.
      @(show/submit! host :auto {:enabled? false :keep-plan? true})
      (browser! origin directory "stopped")
      (finally (app/stop! host) (shutdown-agents)))))
