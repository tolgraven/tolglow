(ns tolglow.qa.live-launcher-studio
  "Preview-only show for the cue launcher browser check (#228). It saves the
  Live bridge settings for the demo page (Push's 64 Pads layout) and connects
  the bridge to a simulated Live Set from the test tree
  (tolglow.integrations.fake-live): a Live without insert_chain whose Drum
  Rack has three filled pads, so setup names two of them, reports the empty
  ones and warns about one pad that launches nothing. The browser runs the
  setup itself (Set up in Live). Only the loopback web server is opened; no
  Live, Max, MIDI or DMX device is touched. Run it where the test tree is on
  the classpath, for example the pooled REPL:

  $W/bin/repl run tolglow.qa.live-launcher-studio <port> <work-dir>

  A `stop` file in the work directory shuts the show down."
  (:require [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.integrations.ableton :as ableton]
            [tolglow.integrations.live-connection :as live]
            [tolglow.show :as show])
  (:import [java.io File]))

(defn- bridge-factory
  "The Live bridge's companion: status polls answer executed, Set operations
  reach the simulated Live."
  [live-set]
  (let [transport (requiring-resolve 'tolglow.integrations.fake-live/transport)]
    (fn [_]
      (let [inner (transport live-set {:json? true})]
        {:open! (fn [_] :bridge) :close! (fn [_])
         :request! (fn [handle id op payload]
                     (if (contains? live/operations op)
                       ((:request! inner) handle id op payload)
                       {:id id :status :executed :result {}}))}))))

(defn -main [& [port work-dir]]
  (let [directory (doto (File. (str work-dir)) (.mkdirs))
        fake-create (requiring-resolve 'tolglow.integrations.fake-live/create)
        ;; The demo page's first two cells are mapped; 40 is a pad no cue uses.
        filled (into {40 "Spare"} (for [[xy name] [[[0 0] "Kick"] [[1 0] "Snare"]]]
                                   [(ableton/pad-note :push-64-pads xy) name]))
        live-set (fake-create {:device-insert? false :companion? false
                               :tracks [{:name "Bass" :arm? true}
                                        {:name "Drums" :companion? true :input {:port "All Ins" :channel nil}
                                         :drum-rack filled}]})
        runtime (app/start! {:init {:components [:show] :modules [:web-server]}
                             :show {:model (demo/model) :workspace-options nil
                                    :sync-options {:live-factory (bridge-factory live-set)}}
                             :web-server {:host "127.0.0.1" :port (Long/parseLong (str port))}})]
    (try
      (let [configured (show/sync-action! runtime :configure {:config {:live {:enabled? true}} :page-id :main})]
        (show/sync-action! runtime :start {})
        (println "live-launcher-qa mapped" (count (get-in configured [:config :live :mappings])) "pads; bridge connected"))
      (println "live-launcher-qa ready" (get-in runtime [:application :modules :web-server :port]))
      (flush)
      (loop []
        (Thread/sleep 200)
        (when-not (.exists (File. directory "stop")) (recur)))
      (finally
        (app/stop! runtime)))))
