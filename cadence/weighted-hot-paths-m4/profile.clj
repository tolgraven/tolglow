(require '[tolglow.perf.weighted-live :as venue])
(import '(java.nio.file Paths)
        '(java.time Duration)
        '(jdk.jfr Configuration Recording))

(let [directory "target/weighted-dmx-profile-ec569ed0"
      destination (Paths/get (str directory "/recording.jfr") (into-array String []))]
  (with-open [recording (Recording. (Configuration/getConfiguration "profile"))]
    (.setName recording "Tolglow weighted runtime in-memory DMX")
    (.withPeriod (.enable recording "jdk.ExecutionSample") (Duration/ofMillis 5))
    (.start recording)
    (let [report (venue/capture {:pixels 20000 :warmup 400 :slots 840})]
      (.stop recording)
      (.dump recording destination)
      (spit (str directory "/weighted-report.edn") (pr-str report))
      (assert (get-in report [:summary :show-output-exercised?]))
      (assert (zero? (get-in report [:summary :incomplete-packets])))
      (prn (select-keys (:summary report)
                        [:sent :completed-renders :source-counts :completed-render-ms :worker-calls])))))
(shutdown-agents)
