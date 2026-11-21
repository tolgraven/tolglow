(ns tolglow.rdm-qa
  "Explicit browser QA host with preview output and an owned loopback OLA peer."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [tolglow.studio :as studio]
            [tolglow.rig.rdm-test :as peer]))

(defn -main [& [port connection-file]]
  (let [port (Long/parseLong (or port "8535"))
        connection-file (or connection-file "target/rdm-qa.json")
        physical (atom {})
        ola (peer/loopback-peer (partial peer/fixture-peer physical))]
    (try
      (let [runtime (studio/start! {:port port :workspace-options nil})
            close! #(do (studio/stop! runtime) ((:close! ola)))]
        (.addShutdownHook (Runtime/getRuntime) (Thread. close!))
        (io/make-parents connection-file)
        (spit connection-file
              (json/write-str {:loopback-peer true :studio-url (str "http://127.0.0.1:" port)
                               :ola-port (:port ola)}))
        (println "Owned RDM QA host ready:" connection-file)
        @(promise))
      (catch Throwable error ((:close! ola)) (throw error)))))
