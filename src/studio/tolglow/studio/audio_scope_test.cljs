(ns tolglow.studio.audio-scope-test
  (:require [cljs.test :refer-macros [deftest is]]
            [re-frame.core :as rf]
            [re-frame.db :as db]
            [reagent.core :as r]
            ["react-dom/server" :refer [renderToStaticMarkup]]
            [tolglow.studio.audio-scope :as audio-scope]
            [tolglow.studio.frame-stream :as frame-stream]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [tolglow.studio.connections :as connections]
            [tolglow.studio.parameter-widgets :as widgets]))

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

(deftest flat-capture-windows-draw-at-their-measured-amplitude
  (let [view (widgets/audio-waveform
               {:bins [{:min 0.5 :max 0.5 :rms 0.5}
                       {:min -0.25 :max -0.25 :rms 0.25}
                       {:min 0 :max 0 :rms 0}
                       {:min -0.25 :max 0.5 :rms 0.3}]})
        svg (nth view 2)
        windows (nth svg 3)]
    (is (= :line.audio-waveform-zero (first (nth svg 2))))
    (is (= [:line :line :rect] (mapv first windows)))
    (is (= [{:x1 0 :y1 26 :y2 26} {:x1 128 :y1 62 :y2 62}]
           (mapv #(select-keys (second %) [:x1 :y1 :y2]) (take 2 windows))))
    (is (= {:x 384 :y 26 :height 36}
           (select-keys (second (last windows)) [:x :y :height])))
    (is (= [:div.audio-waveform-empty "No captured audio samples"]
           (widgets/audio-waveform {:bins []})))))

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

(deftest live-capture-fields-come-from-the-connections-lane
  (frame-stream/reset-all!)
  (try
    (reset! db/app-db (assoc base :connections {:visible? true :audio {:running? true :state "running"}
                                                :sync {:state "idle" :config {:selected-source "midi-clock"}}}))
    (frame-stream/install! :connections {:token "t" :value {:audio-waveform waveform :audio-features {:rms 0.25}
                                                            :sync-clock {:status :locked :clock {:bpm 128}}}})
    (frame-stream/notify! :connections)
    (let [live @(rf/subscribe [:connections/live])
          merged (connections/with-live (connections/state @db/app-db) live)]
      (is (= {:rms 0.25} (get-in merged [:audio :features])))
      (is (= {:status "locked" :clock {:bpm 128}} (get-in merged [:sync :clock])) "presented as the projection presents")
      (is (= {:running? true :state "running"} (:audio (connections/state @db/app-db))) "app-db keeps only the committed status")
      (is (= {:audio nil} (connections/with-live {:audio nil} live)) "an absent status stays absent")
      (is (.includes (renderToStaticMarkup (r/as-element [audio-scope/panel])) "Live input"))
      (is (.includes (renderToStaticMarkup (r/as-element [audio-scope/panel])) "<svg")
          "the scope draws the lane's waveform"))
    (finally (frame-stream/reset-all!))))
