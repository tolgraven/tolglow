(ns tolglow.qa.live-runs-studio
  "Preview-only show for the Live runs browser check (#683). It groups two
  demo cues, performs A (ember) held, B (sage) while A is held, releases A,
  launches A again and overlaps a second A, then mirrors the run record into
  the simulated Live Set from the test tree (tolglow.integrations.fake-live)
  so the panel shows clips. Only the loopback web server is opened; no Live,
  Max, MIDI or DMX device is touched. Run it where the test tree is on the
  classpath, for example the pooled REPL:

  $W/bin/repl run tolglow.qa.live-runs-studio <port> <work-dir>

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
      (throw (ex-info "Live runs QA step was not accepted" {:ack ack})))
    ack))

(defn- launch! [runtime cue]
  (:instance-id (accepted! (show/submit! runtime {:op :cue/launch :cue-id cue :launch {:mode :persistent} :source operator}))))

(defn- release! [runtime id]
  (accepted! (show/submit! runtime {:op :instance/end :instance-id id :source operator})))

(defn -main [& [port work-dir]]
  (let [directory (doto (File. (str work-dir)) (.mkdirs))
        runtime (app/start! {:init {:components [:show] :modules [:web-server]}
                             :show {:model (demo/model) :workspace-options nil}
                             :web-server {:host "127.0.0.1" :port (Long/parseLong (str port))}})
        pause #(Thread/sleep 400)]
    (try
      (accepted! (show/submit! runtime {:op :effect-groups/configure :source operator
                                        :config {:manual [{:id :stage :name "Stage" :cue-ids [:sage]
                                                           :groups [{:id :warm :name "Warm" :cue-ids [:ember]}]}]}}))
      (let [a1 (launch! runtime :ember)]
        (pause)
        (let [b (launch! runtime :sage)]
          (pause) (release! runtime a1) (pause)
          (let [a2 (launch! runtime :ember)]
            (pause) (release! runtime b) (pause)
            (let [a3 (launch! runtime :ember)]
              (pause) (release! runtime a2) (pause) (release! runtime a3) (pause)))))
      (let [fake-create (requiring-resolve 'tolglow.integrations.fake-live/create)
            add-group! (requiring-resolve 'tolglow.integrations.fake-live/add-group-track!)
            transport (requiring-resolve 'tolglow.integrations.fake-live/transport)
            live-set (fake-create {:tracks [{:name "Bass" :arm? true}] :companion? false})
            stage (add-group! live-set "Tolglow group stage")]
        (add-group! live-set "Tolglow group warm" stage)
        (let [result (show/sync-live-runs! runtime (live/open (transport live-set {:json? true}) {:session-id "qa"}))]
          (println "live-runs-qa synced" (:complete? result) (count (:clips result)) "clips")))
      ;; One run left playing, so the panel shows a take still recording.
      (launch! runtime :sage)
      (println "live-runs-qa ready" (get-in runtime [:application :modules :web-server :port]))
      (flush)
      (loop []
        (Thread/sleep 200)
        (when-not (.exists (File. directory "stop")) (recur)))
      (finally
        (app/stop! runtime)))))
