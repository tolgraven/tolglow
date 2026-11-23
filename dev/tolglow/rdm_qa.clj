(ns tolglow.rdm-qa
  "Explicit browser QA host with preview output and owned loopback OLA and Art-Net gateways."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [tolglow.application :as app]
            [tolglow.rig.rdm-test :as peer]
            [tolglow.rig.rdm-gateway :as gateway]))

(defn -main [& [port connection-file]]
  (let [port (Long/parseLong (or port "8535"))
        connection-file (or connection-file "target/rdm-qa.json")
        physical (atom {})
        ola (peer/loopback-peer (partial peer/fixture-peer physical))
        direct (gateway/start!)
        close-peers! #(do ((:close! ola)) ((:close! direct)))]
    (try
      (let [runtime (app/start! (assoc (app/default-config) :web-server {:port port} :show {:workspace-options nil}))
            close! #(do (app/stop! runtime) (close-peers!))]
        (.addShutdownHook (Runtime/getRuntime) (Thread. close!))
        (io/make-parents connection-file)
        (spit connection-file
              (json/write-str {:loopback-peer true :studio-url (str "http://127.0.0.1:" port)
                               :ola-port (:port ola) :art-net (:config direct)}))
        (println "Owned RDM QA host ready:" connection-file)
        @(promise))
      (catch Throwable error (close-peers!) (throw error)))))
