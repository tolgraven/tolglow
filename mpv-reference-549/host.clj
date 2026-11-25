(require '[clojure.java.io :as io]
         '[tolglow.integrations.track-profile :as track-profile]
         '[tolglow.show :as show]
         '[tolglow.studio :as studio])

(let [directory (io/file (System/getenv "TOLGLOW_REFERENCE_QA"))
      track (.getCanonicalPath (io/file directory "track.wav"))
      profile (track-profile/analyze-file track {:beat-grid {:bpm 120}})
      runtime (show/start!
               {:workspace-options nil :frame-rate 15 :workers 1 :recovery false
                :section-patterns {:enabled? true :on :bar :patterns [:warm]}
                :section-reference
                {:provenance {:source-id :mpv :session-id "qa-initial" :clock-id :show-monotonic}
                 :profile profile
                 :mpv {:socket-path (str (io/file directory "player.sock"))
                       :catalog {track (select-keys profile [:reference :duration-ms])}}}}
               (constantly nil))]
  (try
    (let [listener (studio/listen! runtime {:host "127.0.0.1" :port 0
                                           :auth {:token-env "TOLGLOW_REFERENCE_TEST_AUTH"}})]
      (try
        (spit (io/file directory "port") (:port listener))
        (let [deadline (+ (System/nanoTime) 360000000000)]
          (loop []
            (when (and (< (System/nanoTime) deadline) (not (.isFile (io/file directory "stop"))))
              (Thread/sleep 100)
              (recur))))
        (finally ((:stop! listener)))))
    (finally (show/stop! runtime) (shutdown-agents))))
