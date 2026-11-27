(ns tolglow.qa.live-parameter-devices-studio
  "Preview-only show for the parameter-device and shared-frame browser check
  (#625). It runs the demo ember cue with a Level move, releases it, mirrors
  the run record into the simulated Live Set from the test tree
  (tolglow.integrations.fake-live) with a Tolglow parameters device dropped on
  the run track, and keeps a second ember run going so Studio can mount its
  shared frame. Only the loopback web server is opened; no Live, Max, MIDI or
  DMX device is touched. Run it where the test tree is on the classpath:

  $W/bin/repl run tolglow.qa.live-parameter-devices-studio <port> <work-dir>

  A `stop` file in the work directory shuts the show down."
  (:require [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.integrations.live-connection :as live]
            [tolglow.show :as show])
  (:import [java.io File]))

(def ^:private operator {:adapter :repl :session-id "qa-operator"})

(defn- accepted! [receipt]
  (let [ack (deref receipt 5000 {:status :pending})]
    (when-not (= :accepted (:status ack))
      (throw (ex-info "Parameter device QA step was not accepted" {:ack ack})))
    ack))

(defn -main [& [port work-dir]]
  (let [directory (doto (File. (str work-dir)) (.mkdirs))
        runtime (app/start! {:init {:components [:show] :modules [:web-server]}
                             :show {:model (demo/model) :workspace-options nil}
                             :web-server {:host "127.0.0.1" :port (Long/parseLong (str port))}})
        pause #(Thread/sleep 400)]
    (try
      (let [a (:instance-id (accepted! (show/submit! runtime {:op :cue/launch :cue-id :ember :source operator
                                                             :launch {:mode :persistent :params {:level 0.6}}})))]
        (pause)
        (accepted! (show/submit! runtime {:op :param/set :instance-id a :param :level :value 0.25 :source operator}))
        (pause)
        (accepted! (show/submit! runtime {:op :instance/end :instance-id a :source operator :force? true}))
        (pause))
      (let [create (requiring-resolve 'tolglow.integrations.fake-live/create)
            add-device! (requiring-resolve 'tolglow.integrations.fake-live/add-parameter-device!)
            transport (requiring-resolve 'tolglow.integrations.fake-live/transport)
            live-set (create {:tracks [{:name "Bass" :arm? true} {:name "Tolglow run ember"}] :companion? false})]
        (add-device! live-set "Tolglow run ember")
        (let [result (show/sync-live-runs! runtime (live/open (transport live-set {:json? true}) {:session-id "qa"}))]
          (println "parameter-devices-qa synced" (:complete? result) (:parameter-devices result))))
      ;; A second run stays up so Studio mounts its shared frame.
      (let [b (:instance-id (accepted! (show/submit! runtime {:op :cue/launch :cue-id :ember :source operator
                                                             :launch {:mode :persistent}})))]
        (future
          (loop [i 0]
            (when-not (.exists (File. directory "stop"))
              (show/submit! runtime {:op :param/set :instance-id b :param :level :value (+ 0.5 (* 0.45 (Math/sin (* i 0.4))))
                                     :source operator})
              (Thread/sleep 150)
              (recur (inc i))))))
      (println "parameter-devices-qa ready" (get-in runtime [:application :modules :web-server :port]))
      (flush)
      (loop []
        (Thread/sleep 200)
        (when-not (.exists (File. directory "stop")) (recur)))
      (finally
        (app/stop! runtime)))))
