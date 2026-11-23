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

(defn- frame [manager model ms]
  (sensors/advance! manager model {:time-ms ms :beat (/ ms 500.0)
                                   :meter {:beats-per-bar 4 :bars-per-phrase 4}}))

(defn- empty-session [manager id]
  (until #(let [sample (sensors/latest manager id)]
            (when (and (seq (:values sample)) (zero? (get-in sample [:values :audience-count])))
              sample))))

(defn- await-accent [manager model ms]
  (let [state (volatile! model)
        result (until #(let [result (frame manager @state ms)]
                         (vreset! state (:model result))
                         (when (seq (:commands result)) result)))]
    (is (= [:accent] (mapv :cue-id (:commands result))))
    (:model result)))

(defn- verify-returning-dancer! [manager id model submit!]
  (let [empty (empty-session manager id)
        ;; Four ordinary level ticks span more than the default 500 ms sensor timeout.
        steady (until #(let [current (sensors/latest manager id)]
                         (when (>= (:sample-id current) (+ 4 (:sample-id empty))) current)))
        idle (frame manager model 5000)]
    (is (= (:session-id empty) (:session-id steady)))
    (is (= [0 0 0] (mapv #(long (get-in steady [:values %])) [:audience-count :pulse-count :motion-count])))
    (is (nil? (get-in steady [:values :pulse-bpm])))
    (is (empty? (:gestures steady)))
    (is (empty? (:commands idle)))
    (is (true? (get-in idle [:model :sensors :sources id :state :armed?])))
    (submit! 4 8) ; the first sample after guest expiry establishes its step baseline
    (submit! 5 9)
    (println "Empty floor: fresh zero readings, gateway still armed; dancer returning")
    (await-accent manager (:model idle) 5100)))

(defn- exercise-armed-stage! [manager model id submit! stop!]
  (let [initial (until #(sensors/latest manager id))
        armed (:model (sensors/arm! manager model id (:session-id initial) true))
        started (System/nanoTime)]
    (is (nil? (:pairing-token initial)))
    (submit! 3 8)
    (let [active (await-accent manager armed 100)
          elapsed (/ (- (System/nanoTime) started) 1.0e6)]
      (println (format "Paired UDP trace: first GATT step to mapped accent in %.2f ms" elapsed))
      (let [running (verify-returning-dancer! manager id active submit!)]
        (stop!)
        (let [settled (:model (frame manager running 5200))
              lost (frame manager settled 6500)]
          (is (false? (get-in lost [:model :sensors :sources id :state :armed?])))
          (is (some #(= :sensor/dropout (:type %)) (:events lost))))))))

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
          (let [publisher (future (gateway/publish-loop! stage stop {:source id :token (:pairing-token enrolled)} socket target))
                stop! #(do (deliver stop :finished) @publisher)]
            (try (exercise-armed-stage! manager model id submit! stop!)
                 (finally (stop!))))))
      (finally (sensors/close! manager)))))

(deftest bluetooth-packets-reach-the-enrolled-and-armed-osc-gesture-mapping
  (radio/exercise-stage! exchange!))
