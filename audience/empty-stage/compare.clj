(ns compare-floor
  (:require [clojure.data.json :as json]
            [tolglow.linux.audience.floor :as floor]
            [tolglow.linux.audience.main :as gateway]
            [tolglow.linux.audience.stage :as stage]
            [tolglow.sensors :as sensors]
            [tolglow.sensors.osc :as osc]))

(def id "floor")
(def mapping {:version 1 :timeout-ms 1200 :channels []
              :gestures [{:id :accent :type :dance-step
                          :command {:op :cue/launch :cue-id :accent :launch {:mode :one-shot :duration {:beats 1/8}}}}]})
(defn guests! [host ms]
  (let [sample! (fn [person serial steps]
                  (swap! (:state host) update :floor floor/accept person
                         {:serial serial :steps steps :effort 1.0} (/ ms 1000.0)))]
    (case ms
      250 (sample! :first 1 0)
      550 (sample! :first 2 1)
      700 (stage/leave! host :first)
      3500 (sample! :next 1 0)
      3550 (sample! :next 2 1)
      nil)))

(defn timeline [manager now]
  (let [host (stage/create) enrolled (sensors/pair! manager id "Floor")
        config {:source id :token (:pairing-token enrolled)}
        model (:model (sensors/configure {:time-ms 0 :transport {:beat 0}} id mapping (:pairing enrolled)))]
    (:rows
      (reduce
        (fn [{:keys [prior model packet-id rows]} ms]
          (reset! now ms)
          (guests! host ms)
          (let [tick (gateway/publication! host prior (/ ms 1000.0))
                packets (when (and (:send? tick) (< ms 4100))
                          (gateway/packets config "session" packet-id (/ ms 1000.0) (:levels tick) (:steps tick)))]
            (doseq [packet packets] (sensors/ingest! manager (osc/decode packet)))
            (let [model (if (= ms 500) (:model (sensors/arm! manager model id "session" true)) model)
                  result (sensors/advance! manager model {:time-ms ms :beat (/ ms 500.0)
                                                       :meter {:beats-per-bar 4 :bars-per-phrase 4}})
                  next (:model result)]
              {:prior tick :model next :packet-id (+ packet-id (count packets))
               :rows (conj rows {:ms ms :armed (get-in next [:sensors :sources id :state :armed?])
                                 :guests (get-in tick [:levels :audience-count])
                                 :observations (count packets) :cues (mapv :cue-id (:commands result))
                                 :dropout (boolean (some #(= :sensor/dropout (:type %)) (:events result)))})})))
        {:prior {} :model model :packet-id 0 :rows []} (range 0 5601 25)))))

(let [now (atom 0) secrets (atom {}) store {:put! #(do (swap! secrets assoc "credential-00000000-0000-4000-8000-000000000001" %) "credential-00000000-0000-4000-8000-000000000001")
                                           :get! #(get @secrets %) :delete! #(swap! secrets dissoc %)}
      manager (sensors/create-manager {:now-ms #(deref now) :credential-store store})]
  (try (spit (System/getenv "AUDIENCE_REPORT") (json/write-str (timeline manager now)))
       (finally (sensors/close! manager))))
