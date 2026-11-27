(ns tolglow.qa.knob-candidates-browser
  "Run the optimized Studio against the showcase rig and exercise generated
  knob candidates (#667) in headless Chrome: generate, sweep in the Room,
  reroll twice, keep, a selection with too few parameters, then a REPL
  generation and audition the open page follows. Usage:
  lein with-profile +dev run -m tolglow.qa.knob-candidates-browser [directory] [port]"
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [tolglow.application :as app]
            [tolglow.show :as show]
            [tolglow.showcase :as showcase]
            [tolglow.showcase.looks :as looks]))

(defn- accepted! [ack what]
  (when-not (= :accepted (:status ack)) (throw (ex-info (str "Could not " what) ack)))
  ack)

(defn- browser! [origin directory phase]
  (let [{:keys [exit out err]} (shell/sh "node" "dev/studio/knob-candidates-browser-qa.mjs" origin directory phase)]
    (print out) (flush)
    (when-not (zero? exit)
      (binding [*out* *err*] (print err) (flush))
      (throw (ex-info (str "Knob candidate browser check failed (" phase ")") {:error err})))))

(defn -main [& [directory port]]
  (let [port (Long/parseLong (or port "4520"))
        host (app/start! {:init {:components [:show] :modules [:web-server]}
                          :show {:model (showcase/model) :workspace-options nil :recovery false}
                          :web-server {:host "127.0.0.1" :port port}})
        origin (str "http://127.0.0.1:" port)
        directory (or directory "target/knob-candidates-browser")
        repl! #(accepted! @(show/submit! host :command (merge {:source {:adapter :repl :session-id "qa"}} %)) (pr-str %))]
    (try
      (.mkdirs (io/file directory))
      ;; No colour strobe here: it gates the whole coloured rig, so a Room
      ;; screenshot would land on a dark phase two times in three.
      (doseq [cue [:beam-shafts :aurora :can-can :mover-orbit :wash-tide :gather-path :colour-drift]
              :let [id (:instance-id (repl! {:op :cue/launch :cue-id cue :launch {:mode :persistent}}))]
              [param binding] (get looks/sources cue)]
        (accepted! @(show/submit! host :modulation-bind {:instance-id id :param param :binding binding})
                   (str "bind " cue " " param)))
      (browser! origin directory "browser")
      ;; REPL -> Studio: the open page follows a REPL generation and audition.
      (repl! {:op :knob-candidate/generate :seed 30 :character :colour})
      (repl! {:op :knob-candidate/audition :value 0.2})
      (println "REPL candidate:" (select-keys (get-in (show/snapshot host) [:knob-candidates :candidate])
                                              [:id :seed :character]))
      (browser! origin directory "repl")
      (finally (app/stop! host) (shutdown-agents)))))
