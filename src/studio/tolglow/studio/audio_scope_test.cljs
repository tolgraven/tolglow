(ns tolglow.studio.audio-scope-test
  (:require [cljs.test :refer-macros [deftest is]]
            [re-frame.db :as db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [tolglow.studio.connections :as connections]))

(def base {:show {:runtime-id "one" :epoch 2}})

(def waveform {:bins [{:min -0.3 :max 0.4 :rms 0.2 :bands {:low 0.8 :mid 0.1 :high 0.1}}]
               :source-id "software-test-source" :sample-end 480})

(defn transition [state event]
  (reset! db/app-db state)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %))
                                               (registrar/get-handler :event (first event))))))

(deftest waveform-follows-the-acknowledged-status-and-clears-on-disconnect
  (let [db (assoc base :session "tablet" :stream-id "socket" :connections {:visible? true})
        pushed (transition (assoc-in db [:show :connection-status :audio] {:running? true :waveform waveform})
                           [:connections/pushed])
        closed (transition (:db pushed) [:studio/stream-closed "tablet" "socket" "disconnected"])]
    (is (= waveform (get-in pushed [:db :connections :audio :waveform])))
    (is (= #{:db} (set (keys pushed))))
    (is (nil? (get-in closed [:db :connections :audio])))))

(deftest output-policy-settings-preserve-physical-units-and-validate-before-send
  (let [state (assoc base :connections {:output-form {:mode "buffered-interpolation" :latency-ms "25" :fade-ms "120"}})
        result (transition state [:connections/save-output])
        request (get-in result [:fx 0 1])]
    (is (= "/api/settings" (:path request)))
    (is (= {:mode "buffered-interpolation" :latency-ms 25 :fade-ms 120} (get-in request [:body :values :presentation])))
    (is (= "output" (get-in request [:body :scope])))
    (is (contains? (get-in result [:db :connections :pending]) :output)))
  (doseq [options [{:mode "unknown"} {:mode "hold-fade" :fade-ms 0} {:mode "buffered-interpolation" :latency-ms 51}]]
    (is (thrown? js/Error (connections/output-payload (assoc base :connections {:output-form options}))))))
