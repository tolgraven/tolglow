(require '[tolglow.show :as show] '[tolglow.studio.sensor-host-test :as web]
         '[tolglow.sensors-test :as sensors] '[tolglow.sensors.osc :as osc]
         '[tolglow.sensors.audience :as audience] '[clojure.data.json :as json])
(let [clock (atom 0)
      rt (show/create-runtime {:credential-store (sensors/vault) :sensor-options {:now-ms #(deref clock)}})
      frame! (fn [t] (reset! clock t) (show/tick! rt t))
      transact (fn [op payload] (:data (#'web/queued-http rt op payload @clock)))
      zones [["front" :ember] ["balcony" :flash]]
      receipt (fn [id serial pair events]
                (show/sensor-action! rt :sample
                    (osc/decode (osc/encode {:version 1 :device-id id :session-id "nearby-dance"
                      :sample-id serial :time-ms @clock :pairing-token (:pairing-token pair)
                      :values {:audience-count 1 :dance-effort 0.5 :pulse-count 0}
                      :gestures events}))))
      capture (fn [stage]
                (let [s (show/snapshot rt)]
                  {:stage stage :cues (mapv :cue-id (:instances s))
                   :emitted (get-in s [:output :values :head-1 :emitted-color])
                   :dmx (into {} (map (fn [[u values]] [u (mapv #(bit-and 255 %) (take 8 values))]))
                              (get-in @(:state rt) [:output :universes]))}))]
  (try
    (let [pairs (into {} (for [[id cue] zones]
                   (let [pair (transact "pair" {:device-id id :label id})
                         mapping (assoc audience/gateway-mapping :gestures
                           [{:id :step :type :dance-step :cooldown {:beats 0}
                             :command {:op :cue/launch :cue-id cue
                                       :launch {:mode :one-shot :duration {:beats 1}}}}])]
                     (assert (= "accepted" (:status pair)))
                     (assert (= "accepted" (:status (transact "configure" {:device-id id :source (pr-str mapping)}))))
                     (assert (:accepted? (receipt id 0 pair [])))
                     (assert (= "accepted" (:status (transact "arm" {:device-id id :session-id "nearby-dance" :armed? true}))))
                     [id pair])))
          initial (capture "Armed, before motion")
          step [{:id "dance-1" :type :dance-step :phase :pulse}]
          readings (reduce (fn [rows [id t]]
                             (reset! clock t)
                             (assert (:accepted? (receipt id 1 (pairs id) step)))
                             (frame! t)
                             (conj rows (capture id))) [initial] [["front" 250] ["balcony" 275]])]
      (doseq [t (range 300 3300 25)] (frame! t))
      (let [trace (conj readings (capture "After the one-shots"))]
        (println (json/write-str trace))
        (assert (some #{:ember} (:cues (second trace))))
        (assert (some #{:flash} (:cues (nth trace 2))))
        (assert (empty? (:cues (last trace))))
        (assert (not= (:dmx initial) (:dmx (second trace))))))
    (finally (show/stop! rt))))
