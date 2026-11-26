(ns tolglow.qa.stems-studio
  "Preview-only show for the instrument stem browser check (#675). It writes
  the synthesized four-stem recording to a WAV file and replays it, paced in
  real time, through the capture worker's :file input with the stems named,
  the kick routed to the Flash cue and the guitar level to Breathe's level.
  Only the loopback web server is opened; no audio device is touched.

  lein run -m tolglow.qa.stems-studio <port> <work-dir>

  A `stop` file in the work directory shuts the show down."
  (:require [tolglow.application :as app]
            [tolglow.audio.stem-fixtures :as fixtures]
            [tolglow.demo :as demo]
            [tolglow.show :as show]
            [tolglow.stems :as stems])
  (:import [java.io File]))

(defn- accepted! [receipt]
  (let [ack (deref receipt 5000 {:status :pending})]
    (when-not (= :accepted (:status ack))
      (throw (ex-info "Stem QA setup was not accepted" {:ack ack})))
    ack))

(defn -main [& [port work-dir]]
  (let [directory (doto (File. (str work-dir)) (.mkdirs))
        wav (File. directory "stems.wav")
        model (demo/model)
        rendered (fixtures/render {:bpm (double (get-in model [:transport :bpm] 120.0)) :bars 32})
        _ (fixtures/write-wav! rendered wav)
        runtime (app/start! {:init {:components [:show] :modules [:web-server]}
                             :show {:model model :workspace-options nil}
                             :web-server {:host "127.0.0.1" :port (Long/parseLong (str port))}})]
    (try
      (accepted! (:receipt (stems/route-onset! runtime :kick :flash)))
      (accepted! (:enabled (stems/route-level! runtime :guitar :breathe :level)))
      (accepted! (show/submit! runtime {:op :cue/launch :cue-id :breathe :launch {:mode :persistent}
                                        :source {:adapter :repl :session-id "qa-operator"}}))
      (stems/replay-file! runtime (str wav) fixtures/stem-layout)
      (println "stems-qa ready" (get-in runtime [:application :modules :web-server :port]))
      (flush)
      (loop []
        (Thread/sleep 200)
        (when-not (.exists (File. directory "stop")) (recur)))
      (finally
        (app/stop! runtime)
        (.delete wav)
        (shutdown-agents)))))
