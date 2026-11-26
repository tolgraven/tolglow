(ns tolglow.qa.auto-show-browser
  "Run the optimized Studio against the R1o red-show example on the 2023 gig
  rig and check auto mode (#217) from both sides in headless Chrome: a REPL
  section plan reaches the open auto panel, Studio starts it again with varied
  parameters while keeping the plan, Studio and the REPL each see a refusal,
  and a REPL stop reaches the page. Then a change on a musical line (#767): a
  REPL change waiting on a bar is listed in the panel and Studio's Cancel
  withdraws it for the REPL, and Studio's Start on the next bar is posted
  with :at, waits in the REPL's projection and fires on its line. Usage:
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
                          :show {:model (red-show/model) :workspace-options nil :recovery false}
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
      ;; REPL change on a line -> Studio lists it; Studio's Cancel -> REPL (#767).
      (let [beat (get-in (show/snapshot host) [:transport :beat])
            bar (+ 64 (long (Math/floor (/ beat 4))))
            ack @(show/submit! host :auto (assoc (red-show/auto-config 218) :at {:bar bar} :label "Drop"))]
        (println "REPL change on a line:" (pr-str (select-keys ack [:status :event-ids :due-beats])))
        (when-not (= :accepted (:status ack)) (throw (ex-info "REPL change on a line refused" ack)))
        (spit (io/file directory "pending-bar.txt") (str bar))
        (browser! origin directory "pending")
        (when (seq (:pending (auto host)))
          (throw (ex-info "Studio's cancel did not withdraw the pending change" (auto host))))
        (println "After Studio cancel:" (pr-str (last (:recent (show/schedule host))))))
      ;; Studio start on the next bar -> REPL sees it wait, then apply on its line.
      (browser! origin directory "line")
      (let [fired (loop [n 0]
                    (let [hit (some #(when (and (= :auto/configure (:op %)) (= :fired (:status %))
                                                (= "web" (name (get-in % [:source :adapter] ""))))
                                       %)
                                    (:recent (show/schedule host)))]
                      (cond hit hit
                            (> n 60) (throw (ex-info "Studio's change on the next bar did not fire" (show/schedule host)))
                            :else (do (Thread/sleep 100) (recur (inc n))))))]
        (println "Studio change fired:" (pr-str (select-keys fired [:event-id :due-beat :at-beat :status])))
        (when-not (and (zero? (mod (:due-beat fired) 4)) (<= (:due-beat fired) (:at-beat fired) (+ (:due-beat fired) 0.5))
                       (:enabled? (auto host)))
          (throw (ex-info "Studio's change did not apply on its bar" {:fired fired :auto (auto host)}))))
      (finally (app/stop! host) (shutdown-agents)))))
