(ns tolglow.qa.live-capture-studio
  "Preview-only show for the capture-to-Live browser check (#671, #680). It
  connects the Live bridge to a simulated Live Set from the test tree
  (tolglow.integrations.fake-live), plays two demo cues and moves a
  parameter, then waits for the browser to press Send to Live and Open clip
  in Live. Only the loopback web server is opened; no Live, Max, MIDI or DMX
  device is touched. Run it where the test tree is on the classpath, for
  example the pooled REPL:

  $W/bin/repl run tolglow.qa.live-capture-studio <port> <work-dir>

  A `stop` file in the work directory shuts the show down; the simulated
  Set's selected clip is written to `selected.edn` there on every poll."
  (:require [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.integrations.live-connection :as live]
            [tolglow.show :as show])
  (:import [java.io File]))

(defn- bridge-factory
  "The Live bridge's companion: status polls and the handoff answer
  executed, Set operations reach the simulated Live."
  [live-set]
  (let [transport (requiring-resolve 'tolglow.integrations.fake-live/transport)]
    (fn [_]
      (let [inner (transport live-set {:json? true})]
        {:open! (fn [_] :bridge) :close! (fn [_])
         :request! (fn [handle id op payload]
                     (if (contains? live/operations op)
                       ((:request! inner) handle id op payload)
                       {:id id :status :executed :result {}}))}))))

(defn- perform!
  "Launch the demo's first two cues a beat apart and end them, as a
  performer would, so the buffer holds a short phrase."
  [runtime]
  (let [cues (take 2 (sort (keys (get-in @(:state runtime) [:model :registry :cues]))))
        source {:adapter :qa :session-id "performer"}
        launch (fn [cue] (:instance-id (deref (show/submit! runtime {:op :cue/launch :cue-id cue :source source
                                                                      :launch {:mode :persistent}})
                                               2000 nil)))
        ids (doall (for [cue cues] (let [id (launch cue)] (Thread/sleep 600) id)))]
    (Thread/sleep 600)
    (doseq [id ids :when id]
      (deref (show/submit! runtime {:op :instance/end :instance-id id :source source}) 2000 nil)
      (Thread/sleep 300))
    cues))

(defn -main [& [port work-dir]]
  (let [directory (doto (File. (str work-dir)) (.mkdirs))
        fake-create (requiring-resolve 'tolglow.integrations.fake-live/create)
        live-set (fake-create {:tracks [{:name "Bass" :arm? true :input {:port "Keyboard" :channel 1}}]
                               :envelope-lanes? true})
        model (assoc-in (demo/model) [:transport :playing?] true)
        runtime (app/start! {:init {:components [:show] :modules [:web-server]}
                             :show {:model model :workspace-options nil
                                    :sync-options {:live-factory (bridge-factory live-set)}}
                             :web-server {:host "127.0.0.1" :port (Long/parseLong (str port))}})]
    (try
      (show/sync-action! runtime :configure {:config {:live {:enabled? true}}})
      (show/sync-action! runtime :start {})
      (println "live-capture-qa performed" (vec (perform! runtime)))
      (println "live-capture-qa ready" (get-in runtime [:application :modules :web-server :port]))
      (flush)
      (loop []
        (spit (File. directory "selected.edn") (pr-str {:selected (:selected @live-set)
                                                         :tracks (mapv #(select-keys % [:name :arm? :monitor :input])
                                                                       (:tracks @live-set))}))
        (Thread/sleep 200)
        (when-not (.exists (File. directory "stop")) (recur)))
      (finally
        (app/stop! runtime)))))
