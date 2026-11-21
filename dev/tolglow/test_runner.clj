(ns tolglow.test-runner
  (:require [clojure.test :as test]))

(def suites
  {"pure" '[tolglow.utils.iters-test]
   "legacy" '[tolglow.utils.iters-test tolglow.fixtures-test]})

(defn -main [& [suite & extra]]
  (let [namespaces (get suites (or suite "pure"))]
    (when (or (seq extra) (nil? namespaces))
      (binding [*out* *err*] (println "Usage: bin/test [pure|legacy]"))
      (System/exit 2))
    (let [status (try
                   (doseq [namespace namespaces] (require namespace))
                   (let [{:keys [fail error]} (apply test/run-tests namespaces)]
                     (if (zero? (+ fail error)) 0 1))
                   (catch Throwable error
                     (.printStackTrace error)
                     1)
                   (finally (shutdown-agents)))]
      (System/exit status))))
