(ns tolglow.qa.live-launcher-studio
  "Preview-only show for the cue launcher browser check (#228). It saves the
  Live bridge settings for the demo page (Push's 64 Pads layout), then sets
  the launcher up in a simulated Live Set from the test tree
  (tolglow.integrations.fake-live) whose Live cannot create chains, so the
  panel shows both named pads and setup problems. Only the loopback web
  server is opened; no Live, Max, MIDI or DMX device is touched. Run it where
  the test tree is on the classpath, for example the pooled REPL:

  $W/bin/repl run tolglow.qa.live-launcher-studio <port> <work-dir>

  A `stop` file in the work directory shuts the show down."
  (:require [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.integrations.live-connection :as live]
            [tolglow.show :as show])
  (:import [java.io File]))

(defn -main [& [port work-dir]]
  (let [directory (doto (File. (str work-dir)) (.mkdirs))
        runtime (app/start! {:init {:components [:show] :modules [:web-server]}
                             :show {:model (demo/model) :workspace-options nil}
                             :web-server {:host "127.0.0.1" :port (Long/parseLong (str port))}})]
    (try
      (let [configured (show/sync-action! runtime :configure {:config {:live {:enabled? true}} :page-id :main})
            fake-create (requiring-resolve 'tolglow.integrations.fake-live/create)
            transport (requiring-resolve 'tolglow.integrations.fake-live/transport)
            ;; A performer's Drum Rack with two filled pads, on a Live without
            ;; insert_chain: the other mapped pads are reported empty.
            pads (keys (get-in configured [:config :live :mappings]))
            live-set (fake-create {:device-insert? false :companion? false
                                   :tracks [{:name "Bass" :arm? true}
                                            {:name "Drums" :companion? true
                                             :drum-rack (zipmap (take 2 (sort pads)) ["Kick" "Snare"])}]})
            result (show/setup-live-launcher! runtime (live/open (transport live-set {:json? true}) {:session-id "qa"}) {:page-id :main})]
        (println "live-launcher-qa mapped" (count pads) "pads; setup ready?" (:ready? result)
                 "problems" (count (:problems result))))
      (println "live-launcher-qa ready" (get-in runtime [:application :modules :web-server :port]))
      (flush)
      (loop []
        (Thread/sleep 200)
        (when-not (.exists (File. directory "stop")) (recur)))
      (finally
        (app/stop! runtime)))))
