(ns tolglow.sensors.audience-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.sensors :as sensors]
            [tolglow.sensors.audience :as audience]
            [tolglow.sensors.osc :as osc]
            [tolglow.sensors-test :as fixture]))

(def step-mapping
  (assoc audience/gateway-mapping :gestures
         [{:id :accent :type :dance-step :cooldown {:beats 0}
           :command {:op :cue/launch :cue-id :accent
                     :launch {:mode :one-shot :duration {:beats 1/4}}}}]))

(defn- with-gateways [zones rehearse]
  (let [now (atom 0)
        manager (sensors/create-manager {:credential-store (fixture/vault) :now-ms #(deref now)})
        pairs (into {} (map (fn [id] [id (sensors/pair! manager id)])) zones)
        receive (fn [id serial n effort wearers pulse gestures & [additional-values]]
                  (let [packet {:version 1 :device-id id :session-id "floor" :sample-id serial
                                :pairing-token (:pairing-token (pairs id)) :time-ms @now
                                :values (merge {:audience-count n :dance-effort effort :pulse-count wearers}
                                               (when pulse {:pulse-bpm pulse}) additional-values)
                                :gestures gestures}]
                    (sensors/ingest! manager (osc/decode (osc/encode packet)))))
        enroll (fn [model id]
                 (let [configured (:model (sensors/configure model id step-mapping (:pairing (pairs id))))]
                   (receive id 0 0 0 0 nil [])
                   (:model (sensors/arm! manager configured id "floor" true))))]
    (try
      (rehearse {:manager manager :now now :receive receive
                 :model (reduce enroll (assoc-in (fixture/show) [:variables :audience :gateways] zones) zones)})
      (finally (sensors/close! manager)))))

(defn- levels [model] (get-in model [:variables :audience :levels]))

(deftest one-dancer-one-vote-across-enrolled-gateways
  (with-gateways ["front" "balcony"]
    (fn [{:keys [manager now receive model]}]
      (let [gesture [{:id "local-step" :type :dance-step :phase :pulse}]]
        (reset! now 200)
        (is (:accepted? (receive "front" 1 3 1.0 1 120 gesture)))
        (is (:accepted? (receive "balcony" 1 9 0.0 3 80 gesture)))
        (let [result (fixture/advance manager model @now 1)
              m (:model result)]
          (is (= {:audience-count 12 :dance-effort 0.25 :motion-count 0 :pulse-count 4 :pulse-bpm 90.0} (levels m)))
          (is (= 2 (count (:commands result))) "Each zone keeps its own gesture identity")
          (is (= (levels m) (levels (audience/project (assoc-in m [:variables :audience :gateways] ["front" "balcony" "front"])))))
          (reset! now 1300)
          (is (:accepted? (receive "front" 2 1 0.5 0 nil [])))
          (let [refreshed (:model (fixture/advance manager m @now 2))]
            (reset! now 1500)
            (let [only-front (:model (fixture/advance manager refreshed @now 3))]
              (is (= {:audience-count 1 :dance-effort 0.5 :motion-count 0 :pulse-count 0} (levels only-front)))
              (reset! now 3000)
              (is (= {:audience-count 0 :dance-effort 0.0 :motion-count 0 :pulse-count 0}
                     (levels (:model (fixture/advance manager only-front @now 4))))))))))))

(defn- step-batches [tick]
  (let [previous (quot (* 200 (dec tick)) 500)
        beat (quot (* 200 tick) 500)]
    (if (= previous beat)
      [[]]
      (->> (range 24)
           (map (fn [dancer] {:id (str beat "-" dancer) :type :dance-step :phase :pulse}))
           (partition-all 8)
           (mapv vec)))))

(defn crowd-rehearsal
  "Replay a 120 BPM crowd in four zones. Each tick spans 200 ms of show time."
  [ticks]
  (let [zones ["front" "back" "left" "right"]]
    (with-gateways zones
      (fn [{:keys [manager now receive model]}]
        (loop [tick 1 model model commands 0]
          (if (> tick ticks)
            {:commands commands :levels (levels model)
             :admission (select-keys (sensors/status manager) [:queued :metrics])}
            (let [packets (step-batches tick)
                  ms (* 200 tick)]
              (reset! now ms)
              (doseq [zone zones [i gestures] (map-indexed vector packets)]
                (receive zone (+ (* 3 tick) i) 24 0.5 6 110 gestures))
              (let [frame (fixture/advance manager model ms (/ ms 500))]
                (recur (inc tick) (:model frame) (+ commands (count (:commands frame))))))))))))

(deftest synchronized-steps-arrive-once-for-every-dancer
  (let [{:keys [commands levels admission]} (crowd-rehearsal 15)]
    (is (= (* 6 96) commands))
    (is (= {:audience-count 96 :dance-effort 0.5 :motion-count 0 :pulse-count 24 :pulse-bpm 110.0} levels))
    (is (zero? (:queued admission)))
    (is (zero? (get-in admission [:metrics :overflow])))
    (is (zero? (get-in admission [:metrics :rejected])))))

(deftest signed-axis-means-use-motion-contributors-and-clear-on-withdrawal
  (let [stage (fn [people movers axes]
                {:state {:armed? true
                         :channels (merge {:audience-count people :motion-count movers :dance-effort 0.5}
                                          (zipmap audience/motion-axes axes))}})
        front (stage 10 1 [9 -1 0 2 -2 0])
        rear (stage 50 3 [-3 3 0 -2 2 0])
        mix #(audience/levels {"front" front "rear" %} ["front" "rear"])]
    (is (= [0.0 2.0 0.0 -1.0 1.0 0.0] (mapv (mix rear) audience/motion-axes)))
    (is (= 4 (:motion-count (mix rear))))
    (is (= 1 (:motion-count (mix (stage 50 6 [2 3])))) "A partial reading has no weight")
    (let [quiet (audience/levels {"front" (stage 10 0 [])} ["front"])]
      (is (zero? (:motion-count quiet)))
      (is (empty? (select-keys quiet audience/motion-axes))))))

(defn motion-rehearsal [axis-readings]
  (with-gateways ["front"]
    (fn [{:keys [manager now receive model]}]
      (loop [readings (seq axis-readings) model model tick 1 captured []]
        (if-not readings
          captured
          (let [ms (* 200 tick)
                axes (first readings)
                values (assoc (zipmap audience/motion-axes axes) :motion-count (if (seq axes) 1 0))]
            (reset! now ms)
            (receive "front" tick 3 0.5 0 nil [] values)
            (let [next-model (:model (fixture/advance manager model ms (/ ms 500)))]
              (recur (next readings) next-model (inc tick) (conj captured (levels next-model))))))))))

(deftest gateway-osc-preserves-signed-units-and-withdrawal
  (let [axes [-9.5 0.5 15.0 -0.25 2.0 -6.0]
        [live quiet] (motion-rehearsal [axes []])]
    (is (= axes (mapv live audience/motion-axes)))
    (is (= 3 (:audience-count quiet)))
    (is (zero? (:motion-count quiet)))
    (is (empty? (select-keys quiet audience/motion-axes)))))
