(ns tolglow.venue-performance-test
  (:require [afterglow.control.model :as control]
            [clojure.test :refer [deftest is]]
            [tolglow.show :as show]
            [tolglow.application :as app]))

(deftest configure-room-feedback-and-groups-from-the-ordered-startup-map
  (let [rig {:fixtures [{:id :spot :universe 1 :address 1 :channels {:dimmer 1} :groups [:front]}]}
        config (assoc (app/default-config) :web-server false
                      :show {:model (assoc (control/new-show {}) :rig rig)}
                      :lighting-capacity {:enabled true}
                      :fog-control {:enabled true :target 0.25 :fog-fixture-ids []}
                      :lighting-scenes {:enabled true :groups [{:group-id :front :target 0.72 :effect-ids []}]})
        rt (app/start! config)]
    (try
      (let [state (show/snapshot rt)]
        (is (true? (get-in state [:settings :lighting-capacity :enabled?])))
        (is (= 0.25 (get-in state [:settings :fog-control :target])))
        (is (= 0.72 (get-in state [:lighting-scenes :groups 0 :target]))))
      (finally (app/stop! rt)))))

(deftest calibrated-fog-and-independent-lighting-targets-share-a-running-show
  (let [rig {:room {:width 1 :height 1 :depth 1
                    :measured-fog {:clearance-half-life-s 3 :extinction-per-m 1.4 :reference-path-m 1}}
             :fixtures [{:id :jet :universe 1 :address 1 :channels {:fog 1}
                         :fogger {:flow-m3-min 60 :mixing-seconds 0.15
                                  :duty {:max-on-s 0.2 :min-off-s 0.25 :max-level 0.5}}}
                        {:id :wash :universe 1 :address 2 :channels {:dimmer 1} :groups [:front]}
                        {:id :spot :universe 1 :address 3 :channels {:dimmer 1} :groups [:back]}]}
        now (atom 0)
        packets (atom [])
        rt (show/create-runtime {:model (assoc (control/new-show {}) :rig rig)
                                 :physical-now-ms #(deref now)
                                 :sink {:send! (fn [universes _]
                                                 (swap! packets conj (mapv #(bit-and 255 %) (take 3 (get universes 1)))))}})
        tick! #(show/tick! rt (swap! now + 25))
        send! (fn [type value]
                (let [ack (show/submit! rt type value)] (tick!) @ack))
        group (fn [id] (some #(when (= id (:group-id %)) %) (get-in (show/snapshot rt) [:lighting-scenes :groups])))]
    (try
      (let [control {:enabled? true :target 0.3 :hysteresis 0.035 :fog-fixture-ids [:jet]}]
        (is (= :accepted (:status (send! :settings {:scope :fog-control :values control})))))
      (is (= :accepted (:status (send! :lighting-scene {:op :vary :group-id :front :target 0.72 :effect-ids []}))))
      (is (= :accepted (:status (send! :lighting-scene {:op :apply :group-id :back :target 0.0
                                                         :target-var [:fog/level] :effect-ids []}))))
      (dotimes [_ 400] (tick!))
      (let [level (get-in (show/snapshot rt) [:output :atmosphere :fog-level])
            [fog front back] (peek @packets)]
        (is (some #(pos? (first %)) @packets))
        (is (some #(zero? (first %)) (drop 20 @packets)))
        (is (<= (abs (- level 0.30)) 0.035) "The room settles within its requested fog band")
        (is (< (abs (- 0.72 (:actual (group :front)))) 0.02))
        (is (< (abs (- (/ front 255.0) 0.72)) 0.02))
        (is (< (abs (- level (:target (group :back)))) 0.025))
        (is (< (abs (- (/ back 255.0) level)) 0.06))
        (is (<= fog 128)))
      (finally (show/stop! rt)))))
