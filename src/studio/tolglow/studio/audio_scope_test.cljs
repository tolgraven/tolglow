(ns tolglow.studio.audio-scope-test
  (:require [cljs.test :refer-macros [deftest is]]
            [re-frame.db :as db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [tolglow.studio.audio-scope :as scope]
            [tolglow.studio.connections :as connections]))

(def base {:show {:runtime-id "one" :epoch 2}})
(def waveform {:bins [{:min -0.3 :max 0.4 :rms 0.2 :bands {:low 0.8 :mid 0.1 :high 0.1}}]
               :source-id "software-test-source" :sample-end 480})
(defn transition [state event]
  (reset! db/app-db state)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %))
                                               (registrar/get-handler :event (first event))))))

(deftest visible-capture-scope-coalesces-requests-and-never-opens-audio
  (let [opened (transition base [:audio-scope/open])
        sent (transition (:db opened) [:audio-scope/poll])
        token (:audio-scope/request sent)
        duplicate (transition (:db sent) [:audio-scope/poll])
        received (transition (:db sent) [:audio-scope/received token {:running? true :state "running" :waveform waveform} false])]
    (is (= ["one" 2 1] token))
    (is (nil? (:audio-scope/request duplicate)))
    (is (= waveform (get-in received [:db :audio-scope :waveform])))
    (is (= 100 (get-in received [:dispatch-later 0 :ms])))
    (is (= #{:db :audio-scope/request} (set (keys sent))))
    (is (= 1000 (get-in (transition (:db sent) [:audio-scope/received token {:running? true :waveform waveform} true]) [:dispatch-later 0 :ms])))))

(deftest scope-unmount-and-runtime-replacement-reject-late-data
  (let [opened (:db (transition base [:audio-scope/open]))
        sent (transition opened [:audio-scope/poll]) token (:audio-scope/request sent)
        closed (transition (:db sent) [:audio-scope/close])
        stale (transition (:db closed) [:audio-scope/received token {:waveform waveform} false])
        replaced (assoc-in (:db sent) [:show :runtime-id] "two")]
    (is (:audio-scope/cancel closed))
    (is (nil? (:db stale)))
    (is (nil? (:audio-scope/request (transition (:db closed) [:audio-scope/poll token]))))
    (is (nil? (:db (transition replaced [:audio-scope/received token {:waveform waveform} false]))))
    (is (:audio-scope/cancel (transition replaced [:audio-scope/runtime-change])))))

(deftest failed-status-clears-the-old-trace-instead-of-pretending-it-is-live
  (let [opened (:db (transition base [:audio-scope/open]))
        result (transition (assoc-in opened [:audio-scope :waveform] waveform)
                           [:audio-scope/failed (scope/token opened) "Disconnected"])]
    (is (nil? (get-in result [:db :audio-scope :waveform])))
    (is (= "Disconnected" (get-in result [:db :audio-scope :error])))
    (is (= 1000 (get-in result [:dispatch-later 0 :ms])))))

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
