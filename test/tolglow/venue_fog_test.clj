(ns tolglow.venue-fog-test
  (:require [afterglow.control.model :as control]
            [afterglow.output.scheduler :as scheduler]
            [clojure.test :refer [deftest is]]
            [tolglow.persistence :as files]
            [tolglow.show :as show]
            [tolglow.venue-fog :as venue]))

(def fixture {:id :jet :universe 1 :address 12 :position [1 0 2] :channels {:fog 1}
              :fogger {:flow-m3-min 60 :mixing-seconds 2}})
(def model (assoc (control/new-show {:registry {:effects {:jet {:id :jet :name "Fog" :version 1
                                                              :targets [:jet] :params {} :outputs {:fog 1}}}
                                              :cues {:go {:id :go :name "Activate" :version 1 :effect-id :jet}}}})
                  :rig {:fixtures [fixture] :room {:width 10 :height 5 :depth 4}}))
(defn- command! [rt cmd t]
  (let [reply (show/submit! rt cmd)] (show/tick! rt t) @reply))
(defn- state [rt] (get-in (show/snapshot rt) [:output :atmosphere]))
(defn- amount [rt k] (get-in (state rt) [:sources 0 k]))

(deftest simulated-dmx-controls-emission-and-show-restore-rebases-the-clock
  (let [rt (show/create-runtime {:model model})]
    (try
      (let [ack (command! rt {:op :cue/launch :cue-id :go} 0)
            id (:instance-id ack)]
        (is (= :accepted (:status ack)))
        (is (= 1.0 (amount rt :flow-m3-s)))
        (show/tick! rt 1000)
        (is (= :rendered-dmx (:basis (state rt))))
        (is (= 1.0 (amount rt :emitted-m3)))
        (is (pos? (:mixed-density (state rt))))
        (is (= :accepted (:status (command! rt {:op :instance/remove :instance-id id} 2000))))
        (is (= 0.0 (amount rt :flow-m3-s)))
        (is (= 2.0 (amount rt :emitted-m3)))
        (let [doc (show/export-show-source rt)
              saved (get-in (files/read-string doc) (into [:model] venue/saved-path))
              p (show/submit! rt :import {:source doc})]
          (is (= 2.0 (get-in saved [:sources 0 :emitted-m3])))
          (show/tick! rt 90000)
          (is (= :accepted (:status @p)) (str @p))
          (is (= 0.0 (amount rt :flow-m3-s)))
          (is (= (:mixed-m3 saved) (:mixed-m3 (state rt))))))
      (finally (show/stop! rt)))))

(deftest an-independent-publisher-and-save-use-the-physical-clock
  (let [wall (atom 0)
        rt (show/create-runtime {:model model :independent-output? true
                                 :sink {:send! (fn [_ _])} :physical-now-ms #(deref wall)})
        saved #(get-in (files/read-string (show/export-show-source rt)) (into [:model] venue/saved-path))]
    (try
      (let [launch (command! rt {:op :cue/launch :cue-id :go} 0)]
        (is (= :accepted (:status launch)))
        (reset! wall 1000)
        (is (zero? (get-in (saved) [:sources 0 :emitted-m3])))
        (scheduler/publish! (:publisher rt))
        (reset! wall 4000)
        (let [fog (saved)]
          (is (= :sent-dmx (:basis fog)))
          (is (= 3.0 (get-in fog [:sources 0 :emitted-m3])))
          (is (pos? (:mixed-density fog))))
        (is (= :accepted (:status (command! rt {:op :instance/remove :instance-id (:instance-id launch)} 25))))
        (scheduler/publish! (:publisher rt))
        (reset! wall 10000)
        (is (= 3.0 (get-in (saved) [:sources 0 :emitted-m3]))))
      (finally (show/stop! rt)))))

(deftest a-failed-dmx-send-keeps-the-last-successful-pump-setting
  (let [now (atom 0) fail? (atom false)
        sink {:send! (fn [_ _] (when @fail? (throw (ex-info "Disconnected test sink" {}))))}
        rt (show/create-runtime {:model model :sink sink :physical-now-ms #(deref now)})]
    (try
      (let [id (:instance-id (command! rt {:op :cue/launch :cue-id :go} 0))]
        (reset! now 1000) (reset! fail? true)
        (command! rt {:op :instance/remove :instance-id id} 10)
        (is (= :sent-dmx (:basis (state rt))))
        (is (= 1.0 (amount rt :emitted-m3)))
        (is (some? (get-in @(:state rt) [:status :output-error])))
        (reset! now 2000) (reset! fail? false)
        (show/tick! rt 20)
        (reset! now 3000) (show/tick! rt 30)
        (is (= 2.0 (amount rt :emitted-m3)))
        (is (= 0.0 (amount rt :flow-m3-s))))
      (finally (show/stop! rt)))))
