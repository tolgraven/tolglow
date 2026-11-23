(ns tolglow.render.workers-test
  (:require [afterglow.render.distributed :as pool]
            [clojure.test :refer [deftest is]]
            [ring.adapter.jetty :as jetty]
            [tolglow.render.workers :as workers]
            [tolglow.studio.access :as access]
            [tolglow.transit :as wire])
  (:import [java.net URI]
           [java.net.http HttpClient WebSocket WebSocket$Listener]
           [java.util.concurrent LinkedBlockingQueue TimeUnit]))

(def token "test-only-compute-token-12345678")

(defn- connect [server]
  (let [port (.getLocalPort (first (.getConnectors server)))
        addr (str "127.0.0.1:" port)
        inbox (LinkedBlockingQueue.)
        builder (doto (.newWebSocketBuilder (HttpClient/newHttpClient))
                  (.header "Origin" (str "http://" addr)))
        listener (reify WebSocket$Listener
                   (onOpen [_ s] (.request s 1))
                   (onText [_ s text _] (.offer inbox (wire/decode (str text))) (.request s 1) nil)
                   (onClose [_ _ code _] (.offer inbox code) nil))
        socket (.get (.buildAsync builder (URI/create (str "ws://" addr "/api/compute/stream")) listener)
                     5 TimeUnit/SECONDS)]
    {:send! #(.get (.sendText ^WebSocket socket (wire/encode %) true) 3 TimeUnit/SECONDS)
     :take! #(.poll inbox 3 TimeUnit/SECONDS)
     :socket socket}))

(deftest browser-origin-and-operator-admission
  (let [hub (workers/create (pool/create) token)
        request {:remote-addr "192.0.2.1" :scheme :https
                 :headers {"host" "studio.example" "origin" "https://studio.example"}}]
    (try
      (is (nil? (workers/create (pool/create) "")))
      (is (= 503 (:status (workers/response nil request))))
      (is (= 403 (:status (workers/response hub request))))
      (is (= 403 (:status (workers/response hub (assoc request :remote-addr "127.0.0.1")))))
      (is (= 426 (:status (workers/response hub (assoc request ::access/operator? true)))))
      (is (= 403 (:status (workers/response hub (-> request (assoc ::access/operator? true)
                                                    (assoc-in [:headers "origin"] "https://other.example"))))))
      (is (= 403 (:status (workers/response hub (-> request (assoc ::access/operator? true)
                                                    (update :headers dissoc "origin"))))))
      (finally (workers/close! hub)))))

(deftest real-transit-upgrade-pairs-only-after-authentication-and-releases-a-closed-client
  (let [coordinator (pool/create)
        hub (workers/create coordinator token)
        server (jetty/run-jetty (partial workers/response hub) {:host "127.0.0.1" :port 0 :join? false})
        hello {:op :hello :caps {:max-result-floats 100000}}]
    (try
      (let [{:keys [take! send!]} (connect server)]
        (is (zero? (:workers (pool/status coordinator))))
        (send! (assoc hello :token "another-valid-length-but-wrong-token"))
        (is (= 1008 (take!)))
        (is (zero? (:workers (pool/status coordinator)))))
      (let [{:keys [take! send!]} (connect server)]
        (send! (assoc hello :token token))
        (is (= {:op :welcome} (take!)))
        (is (= 1 (:workers (pool/status coordinator))))
        (is (= workers/max-result-floats (:capacity (first (vals @(:peers coordinator))))))
        (send! {:op :result :job 2 :values "untyped"})
        (is (= 1008 (take!)))
        (is (zero? (:workers (pool/status coordinator)))))
      (finally (workers/close! hub) (.stop server)))))
