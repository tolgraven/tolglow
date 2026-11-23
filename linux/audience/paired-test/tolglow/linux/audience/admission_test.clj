(ns tolglow.linux.audience.admission-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.linux.audience.bluez-test :as radio]
            [tolglow.linux.audience.main :as gateway]
            [tolglow.sensors :as sensors])
  (:import [java.net DatagramSocket InetSocketAddress]
           [java.util UUID]))

(defn- until [f]
  (let [limit (+ (System/nanoTime) 4000000000)]
    (loop []
      (or (f) (if (< (System/nanoTime) limit) (do (Thread/sleep 5) (recur))
                    (throw (ex-info "Sensor observation did not arrive" {})))))))

(defn- exchange! [stage submit!]
  (let [secrets (atom {}) id "audience"
        store {:put! (fn [token] (let [ref (str "credential-" (UUID/randomUUID))]
                                    (swap! secrets assoc ref token) ref))
               :get! #(@secrets %) :delete! #(swap! secrets dissoc %)}
        manager (sensors/create-manager {:credential-store store})
        enrolled (sensors/pair! manager id "Linux floor")
        model (:model (sensors/configure {:time-ms 0 :transport {:beat 0}} id
                        {:version 1 :channels [] :gestures [{:id :accent :type :dance-step
                         :command {:op :cue/launch :cue-id :accent :launch {:mode :one-shot :duration {:beats 1/8}}}}]}
                        (:pairing enrolled)))
        stop (promise)]
    (try
      (let [port (:port (sensors/start-osc! manager {:host "127.0.0.1" :port 0}))
            target (InetSocketAddress. "127.0.0.1" (int port))]
        (with-open [socket (DatagramSocket.)]
          (let [publisher (future (gateway/publish-loop! stage stop {:source id :token (:pairing-token enrolled)} socket target))]
            (try
              (let [initial (until #(sensors/latest manager id))
                    session (:session-id initial)
                    armed (:model (sensors/arm! manager model id session true))
                    started (System/nanoTime)]
                (is (nil? (:pairing-token initial)))
                (submit! 3 8)
                (until #(when (seq (:gestures (sensors/latest manager id))) true))
                (let [elapsed (/ (- (System/nanoTime) started) 1.0e6)
                      commands (:commands (sensors/advance! manager armed {:time-ms 1000 :beat 2 :meter {:beats-per-bar 4 :bars-per-phrase 4}}))]
                  (is (= [:accent] (mapv :cue-id commands)))
                  (println (format "Paired UDP trace: fresh session armed, %s launched, one GATT-write to receipt observation %.2f ms"
                                   (:cue-id (first commands)) elapsed))))
              (finally (deliver stop :finished) @publisher)))))
      (finally (sensors/close! manager)))))

(deftest bluetooth-packets-reach-the-enrolled-and-armed-osc-gesture-mapping
  (radio/exercise-stage! exchange!))
