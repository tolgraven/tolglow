(require '[tolglow.perf.output-deadlines :as deadlines]
         '[tolglow.perf.admission-cost :as admission]
         '[clojure.data.json :as json])
(let [result (#'deadlines/run-case :pixel-sculpture (:model (admission/workload 1024 128)) 120 240 0)]
  (spit "/tmp/tolglow-sampling-capture.edn" (pr-str result))
  (spit "/tmp/tolglow-sampling-capture.json" (json/write-str result))
  (prn (:summary result)))
(shutdown-agents)
