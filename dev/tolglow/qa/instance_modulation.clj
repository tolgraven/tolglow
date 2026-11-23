(ns tolglow.qa.instance-modulation
  "Capture the actual DMX output from two launches of the same dimmer cue."
  (:require [afterglow.control.model :as control]
            [clojure.data.json :as json]
            [tolglow.show :as show]))

(defn- rig []
  (let [dimmer {:id :wash :version 1 :params {:level {:type :number :bounds [0 1] :default 0.25}}
                :outputs {:dimmer {:op :param :key :level}}}]
    (control/new-show {:show-id :binding-proof :transport {:playing? true :beat 0 :bpm 120}
                       :registry {:effects {:wash dimmer} :cues {:wash {:id :wash :version 1 :effect-id :wash}}}
                       :rig {:fixtures (mapv (fn [id address] {:id id :universe 1 :address address :channels {:dimmer 1}})
                                            [:left :right] [1 2])}})))

(defn capture []
  (let [packet (atom nil)
        runtime (show/create-runtime {:model (rig) :workers 2
                                      :sink {:send! (fn [frame _] (reset! packet (mapv #(bit-and 255 %) (take 2 (get frame 1)))))}})
        submit (fn [t op payload]
                 (let [receipt (show/submit! runtime op payload)]
                   (show/tick! runtime t)
                   (when-not (= :accepted (:status @receipt)) (throw (ex-info "Operation failed" @receipt)))
                   @receipt))
        launch (fn [t target] (:instance-id (submit t :command {:op :cue/launch :cue-id :wash :launch {:targets [target]}})))]
    (try
      (let [left (launch 0 :left) right (launch 1 :right)
            address {:instance-id left :param :level}
            hold {:instance-id left :params [:level]}
            bind (fn [waveform] (assoc address :binding {:type :lfo :waveform waveform :period {:beats 2}
                                                         :from 0.1 :to 0.9}))]
        {:left left :right right :universe 1 :addresses [1 2]
         :events {100 :sine 1500 :hold 2000 :resume 3000 :saw 4000 :unbind}
         :frames (mapv (fn [t]
                         (case t
                           100 (submit t :modulation-bind (bind :sine))
                           1500 (submit t :modulation-hold hold)
                           2000 (submit t :modulation-resume hold)
                           3000 (submit t :modulation-bind (bind :saw))
                           4000 (submit t :modulation-unbind address)
                           (show/tick! runtime t))
                         {:ms t :dmx @packet})
                       (range 25 4501 25))})
      (finally (show/stop! runtime)))))

(defn -main [& [filename]]
  (let [result (json/write-str (capture))]
    (if filename (spit filename result) (println result))))
