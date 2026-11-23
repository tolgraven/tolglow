(require '[tolglow.show :as show]
         '[tolglow.connections :as connections]
         '[tolglow.sensors :as sensors]
         '[tolglow.sensors.audience :as audience]
         '[tolglow.sensors.osc :as osc]
         '[tolglow.sensors-test :as fixture]
         '[tolglow.lighting-scenes-test :as rig]
         '[clojure.data.json :as json])

(defn sampled! [rt clock pair sequence people effort]
  (let [observation {:version 1 :device-id (:device-id pair) :session-id "stage-floor"
                     :sample-id sequence :time-ms @clock :pairing-token (:pairing-token pair)
                     :values {:audience-count people :dance-effort effort :pulse-count 0}
                     :gestures []}]
    (assert (:accepted? (show/sensor-action! rt :sample (osc/decode (osc/encode observation)))))))

(defn enroll! [rt clock id]
  (let [manager (connections/sensors (:connections rt))
        pair (sensors/pair! manager id id)
        issue (fn [op data] (assert (= :accepted (:status (rig/issue! rt clock op data)))))]
    (issue :sensor-configure {:device-id id :config audience/gateway-mapping :pairing (:pairing pair)})
    (sampled! rt clock pair 0 0 0.0)
    (rig/ticks! rt clock 1)
    (issue :sensor-arm {:device-id id :session-id "stage-floor" :armed? true})
    (issue :sensor-audience-zone {:device-id id :included? true})
    pair))

(let [clock (atom 0)
      rt (rig/local false {:credential-store (fixture/vault) :sensor-options {:now-ms #(deref clock)}})]
  (try
    (let [solo (enroll! rt clock "front") balcony (enroll! rt clock "balcony")
          command {:op :apply :group-id :wash :effect-ids [] :target 0
                   :target-var [:audience :levels :dance-effort]}
          _ (assert (= :accepted (:status (rig/issue! rt clock :lighting-scene command))))
          stages [[1 1 1.0 0 0.0] [2 1 1.0 3 0.0] [3 1 1.0 3 1.0] [4 0 0.0 0 0.0]]
          trace (mapv (fn [[seq-id n a m b]]
                        (sampled! rt clock solo seq-id n a)
                        (sampled! rt clock balcony seq-id m b)
                        (rig/ticks! rt clock 12)
                        (let [group (rig/group! rt :wash)]
                          {:people (+ n m) :front a :balcony b
                           :target (:target group) :status (:status group)
                           :dmx (mapv #(rig/byte-at rt %) (range 5))}))
                      stages)]
      (println (json/write-str trace))
      (assert (= [1.0 0.25 1.0 0.0] (mapv :target trace)))
      (assert (= (get-in trace [0 :dmx]) (get-in trace [2 :dmx])))
      (assert (> (apply + (get-in trace [0 :dmx])) (apply + (get-in trace [1 :dmx])) 0))
      (assert (every? zero? (get-in trace [3 :dmx]))))
    (finally (show/stop! rt) (shutdown-agents))))
