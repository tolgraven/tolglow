(ns tolglow.sensors.audience-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.sensors :as sensors]
            [tolglow.sensors.audience :as audience]
            [tolglow.sensors.osc :as osc]
            [tolglow.sensors-test :as fixture]))

(deftest one-dancer-one-vote-across-enrolled-gateways
  (let [now (atom 0)
        manager (sensors/create-manager {:credential-store (fixture/vault) :now-ms #(deref now)})
        zones ["front" "balcony"]
        pairs (into {} (map (fn [id] [id (sensors/pair! manager id)])) zones)
        mapping (assoc audience/gateway-mapping :gestures
                       [{:id :accent :type :dance-step :cooldown {:beats 0}
                         :command {:op :cue/launch :cue-id :accent
                                   :launch {:mode :one-shot :duration {:beats 1/4}}}}])
        model (reduce (fn [m id] (:model (sensors/configure m id mapping (:pairing (pairs id)))))
                      (assoc-in (fixture/show) [:variables :audience :gateways] zones) zones)
        receive (fn [id serial n effort wearers pulse gestures]
                  (sensors/ingest! manager
                    (osc/decode (osc/encode
                                  {:version 1 :device-id id :session-id "floor" :sample-id serial
                                   :pairing-token (:pairing-token (pairs id)) :time-ms @now
                                   :values (cond-> {:audience-count n :dance-effort effort :pulse-count wearers}
                                             pulse (assoc :pulse-bpm pulse))
                                   :gestures gestures}))))
        gesture [{:id "local-step" :type :dance-step :phase :pulse}]
        levels #(get-in % [:variables :audience :levels])]
    (try
      (doseq [id zones] (is (:accepted? (receive id 0 1 0 0 nil []))))
      (let [armed (reduce (fn [m id] (:model (sensors/arm! manager m id "floor" true))) model zones)]
        (reset! now 200)
        (is (:accepted? (receive "front" 1 3 1.0 1 120 gesture)))
        (is (:accepted? (receive "balcony" 1 9 0.0 3 80 gesture)))
        (let [result (fixture/advance manager armed @now 1)
              m (:model result)]
          (is (= {:audience-count 12 :dance-effort 0.25 :pulse-count 4 :pulse-bpm 90.0} (levels m)))
          (is (= 2 (count (:commands result))) "Each zone keeps its own gesture identity")
          (is (= (levels m) (levels (audience/project (assoc-in m [:variables :audience :gateways] ["front" "balcony" "front"])))))
          (reset! now 1300)
          (is (:accepted? (receive "front" 2 1 0.5 0 nil [])))
          (let [refreshed (:model (fixture/advance manager m @now 2))]
            (reset! now 1500)
            (let [only-front (:model (fixture/advance manager refreshed @now 3))]
              (is (= {:audience-count 1 :dance-effort 0.5 :pulse-count 0} (levels only-front)))
              (reset! now 3000)
              (is (= {:audience-count 0 :dance-effort 0.0 :pulse-count 0}
                     (levels (:model (fixture/advance manager only-front @now 4)))))))))
      (finally (sensors/close! manager)))))
