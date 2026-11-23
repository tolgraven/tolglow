(require '[tolglow.sensors :as s] '[tolglow.sensors.osc :as osc]
         '[tolglow.sensors-test :as fixture] '[tolglow.sensors.audience-test :as a]
         '[clojure.data.json :as json])
(import '(java.net DatagramSocket DatagramPacket InetAddress))
(defn now [] (/ (System/nanoTime) 1e6))
(defn until! [ready? timeout]
  (let [end (+ (now) timeout)]
    (loop []
      (cond (ready?) true (< (now) end) (do (Thread/sleep 2) (recur)) :else false))))
(defn main []
  (let [manager (s/create-manager {:credential-store (fixture/vault) :now-ms now})
        zones ["front" "back" "left" "right"]
        pairs (into {} (map (fn [id] [id (s/pair! manager id)])) zones)
        tick-limit 150
        start-model (reduce (fn [m id] (:model (s/configure m id a/step-mapping (:pairing (pairs id)))))
                            (assoc-in (fixture/show) [:variables :audience :gateways] zones) zones)
        serials (atom {}) sent (atom 0)
        {port :port} (s/start-osc! manager {:host "127.0.0.1"})
        address (InetAddress/getByName "127.0.0.1")]
    (try
      (with-open [sock (DatagramSocket.)]
        (letfn [(send! [id people gestures]
                  (let [serial (get (swap! serials update id (fnil inc -1)) id)
                        data (osc/encode {:version 1 :device-id id :session-id "live" :sample-id serial
                                          :pairing-token (:pairing-token (pairs id)) :time-ms (now)
                                          :values {:audience-count people :dance-effort 0.5 :pulse-count 0}
                                          :gestures gestures})]
                    (.send sock (DatagramPacket. data (alength data) address (int port)))
                    (swap! sent inc)))
                (gestures [beat n]
                  (mapv (fn [i] {:id (str beat "-" i) :type :dance-step :phase :pulse}) (range n)))]
          (doseq [z zones] (send! z 0 []))
          (assert (until! #(= 4 (get-in (s/status manager) [:metrics :accepted])) 3000))
          (let [armed (reduce (fn [m z] (:model (s/arm! manager m z "live" true))) start-model zones)
                start (now)]
            (loop [frame 0 model armed expected 0 commands 0 peak 0 late 0]
              (let [deadline (+ start (* 25 frame))
                    pause (- deadline (now))]
                (when (pos? pause) (Thread/sleep (long pause)))
                (let [tick (inc (quot frame 8))
                      sending? (and (< tick (inc tick-limit)) (zero? (mod frame 8)))
                      onset? (not= (quot (* 200 tick) 500) (quot (* 200 (dec tick)) 500))
                      guests (cond (<= tick 10) [1 0 0 0] (<= tick 140) [24 24 24 24] :else [0 0 0 0])
                      expected (+ expected (if (and sending? onset?) (reduce + guests) 0))]
                  (when sending?
                    (doseq [[z n] (map vector zones guests)]
                      (let [events (if onset? (gestures tick n) [])]
                        (doseq [batch (if (seq events) (partition-all 8 events) [[]])]
                          (send! z n (vec batch))))))
                  (let [t (now)
                        state (s/status manager)
                        frame-result (fixture/advance manager model t (/ (- t start) 500.0))
                        output (+ commands (count (:commands frame-result)))
                        peak (max peak (:queued state)) late (+ late (if (> (- t deadline) 25) 1 0))]
                    (if (< frame (+ 20 (* 8 tick-limit)))
                      (recur (inc frame) (:model frame-result) expected output peak late)
                      (let [status (s/status manager)
                            result {:frames (inc frame) :duration-ms (- t start)
                                    :sent @sent :udp (get-in status [:udp :status])
                                    :admission (:metrics status) :peak-queue peak :remaining (:queued status)
                                    :late-by-one-period late :commands output :expected expected
                                    :last-levels (get-in frame-result [:model :variables :audience :levels])}]
                        (println (json/write-str result))
                        (assert (= expected output))
                        (assert (= @sent (get-in status [:metrics :accepted])))
                        (assert (zero? (get-in status [:metrics :overflow]))))))))))))
      (finally (s/close! manager)))))
(main)
