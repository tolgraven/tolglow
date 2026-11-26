(ns tolglow.qa.knobs-browser
  "Run the optimized Studio against the preview-only demo show and exercise the
  linked knobs panel (#666) in headless Chrome. The REPL side submits the same
  owner commands through this process. Usage:
  lein with-profile +dev run -m tolglow.qa.knobs-browser [directory] [port]"
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.knobs :as knobs]
            [tolglow.show :as show]))

(defn- accepted! [ack what]
  (when-not (= :accepted (:status ack)) (throw (ex-info (str "Could not " what) ack)))
  ack)

(defn -main [& [directory port]]
  (let [port (Long/parseLong (or port "4300"))
        host (app/start! {:init {:components [:show] :modules [:web-server]}
                          :show {:model (demo/model) :workspace-options nil}
                          :web-server {:host "127.0.0.1" :port port}})
        origin (str "http://127.0.0.1:" port)
        directory (or directory "target/knobs-browser")
        repl! #(accepted! @(show/submit! host :command (merge {:source {:adapter :repl :session-id "qa"}} %)) (pr-str %))]
    (try
      (.mkdirs (io/file directory))
      (let [ids (into {} (for [cue [:ember :breathe :sweep]]
                           [cue (:instance-id (repl! {:op :cue/launch :cue-id cue :launch {:mode :persistent}}))]))]
        (accepted! @(show/submit! host :modulation-bind
                                  {:instance-id (:ember ids) :param :level
                                   :binding {:type :lfo :waveform :pulse :period {:beats 1} :phase-reference :transport
                                             :inputs {:duty 0.5} :from 0.0 :to 0.5}})
                   "bind the pulse LFO")
        (let [{:keys [exit out err]} (shell/sh "node" "dev/studio/knobs-browser-qa.mjs" origin directory "browser")]
          (print out) (flush)
          (when-not (zero? exit)
            (binding [*out* *err*] (print err) (flush))
            (throw (ex-info "Knob browser check failed (Studio side)" {:error err}))))
        ;; REPL -> Studio: turn the knob Studio created and check the open page follows.
        (let [knob (first (:knobs (knobs/projection (:model @(:state host)))))]
          (repl! {:op :knob/set :knob-id (:id knob) :value 1.0})
          (println "REPL set" (:id knob) "to 1.0:"
                   (mapv (juxt :label (comp :text :resolved)) (:links (knobs/knob-view (:model @(:state host)) (:id knob))))))
        (let [{:keys [exit out err]} (shell/sh "node" "dev/studio/knobs-browser-qa.mjs" origin directory "repl")]
          (print out) (flush)
          (when-not (zero? exit)
            (binding [*out* *err*] (print err) (flush))
            (throw (ex-info "Knob browser check failed (REPL side)" {:error err})))))
      (finally (app/stop! host) (shutdown-agents)))))
