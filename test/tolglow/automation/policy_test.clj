(ns tolglow.automation.policy-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.automation.policy :as policy]
            [tolglow.registry :as registry]))

(def fixture {:id :head :position [0 0 0] :universe 1 :address 1
              :channels {:red 1 :green 2 :blue 3 :dimmer 4 :pan 5 :tilt 6}})
(def movement {:id :movement :version 1 :outputs {:pan 0.3 :tilt 0.6}})
(def emission {:id :emission :version 1 :outputs {:color [1 0 0] :dimmer 0.5}})

(defn model []
  {:show-id "auto" :epoch 1 :master 1 :blackout? false :fixtures [fixture]
   :transport {:beat 0 :bpm 120 :playing? true}
   :pages {:motion-only {:cells {[0 0] :motion}} :all {:cells {[0 0] :motion [1 0] :red}}}
   :registry (-> (registry/new-registry)
                 (registry/publish :effects movement)
                 (registry/publish :effects emission)
                 (registry/publish :cues {:id :motion :effect-id :movement})
                 (registry/publish :cues {:id :red :effect-id :emission}))})

(deftest movement-adds-real-emission-and-preserves-intentional-darkness
  ;; With two entries seed 1000 keeps motion first in Fisher-Yates ordering.
  (let [state (policy/initial-state 1000 0 "operator-auto")
        result (policy/advance (model) {} state 0)
        dark (policy/advance (model) {:intent :darkness} state 0)]
    (is (= #{:motion :red} (set (map :cue-id (:commands result)))))
    (is (= :added-emission (get-in result [:reasons 0 :reason])))
    (is (= [:motion] (mapv :cue-id (:commands dark))))
    (is (= :intentional-darkness (get-in dark [:reasons 0 :reason])))))

(deftest scope-and-black-output-do-not-create-false-prerequisites
  (let [state (policy/initial-state 1000 0 :auto)
        scoped (policy/advance (model) {:scope {:page-id :motion-only}} state 0)
        black (policy/advance (assoc (model) :master 0) {} state 0)
        existing (assoc (model) :assignments [{:target-id :head :attribute :color :value [1 1 1] :blend :replace}])
        visible (policy/advance existing {:scope {:cue-ids [:motion]}} state 0)]
    (is (empty? (:commands scoped)))
    (is (= :unsatisfied-emission (get-in scoped [:reasons 0 :reason])))
    (is (empty? (:commands black)))
    (is (= [:motion] (mapv :cue-id (:commands visible))))))

(deftest scheduling-replay-restart-and-cleanup-use-owned-instance-ids
  (let [state (policy/initial-state 0 0 :auto)
        result (policy/advance (model) {:interval-ms 100} state 0)
        same (policy/advance (model) {:interval-ms 100} (:state result) 0)
        with-id (policy/record-launch (:state result) "auto-instance")
        stopped (policy/stop-plan (model) with-id 10)
        restarted (policy/advance (assoc (model) :epoch 2) {} with-id 100)]
    (is (= result (policy/advance (model) {:interval-ms 100} state 0)))
    (is (empty? (:commands same)))
    (is (= ["auto-instance"] (mapv :instance-id (:commands stopped))))
    (is (= :show-restarted (get-in restarted [:reasons 0 :reason])))
    (is (empty? (:commands restarted)))
    (is (empty? (get-in restarted [:state :owned-instance-ids])))))

(deftest current-instance-priority-and-expiry-participate-in-emission-prediction
  (let [black {:id :black :version 1 :outputs {:color [0 0 0]}}
        instance {:id "human-blackout" :cue-id :human :effect-id :black :effect-version 1
                  :definition black :started-at-ms 0 :params {} :state {}
                  :priority 100 :order 0 :phase :running}
        show (assoc (model) :instances {"human-blackout" instance})
        state (policy/initial-state 1000 10 :auto)
        blocked (policy/advance show {} state 10)
        expired (policy/advance (assoc-in show [:instances "human-blackout" :duration-ms] 5) {} state 10)]
    (is (empty? (:commands blocked)))
    (is (= :unsatisfied-emission (get-in blocked [:reasons 0 :reason])))
    (is (= #{:motion :red} (set (map :cue-id (:commands expired)))))))

(deftest musical-intervals-use-beats-pause-and-captured-meter
  (let [show (assoc-in (model) [:transport :meter] {:beats-per-bar 3 :bars-per-phrase 2})
        config {:interval {:bars 1} :intent :darkness}
        state (policy/initial-state 1000 0 :musical)
        initial (policy/advance show config state 0)
        paused (policy/advance (-> show (assoc-in [:transport :beat] 1.5)
                                   (assoc-in [:transport :playing?] false)) config (:state initial) 90000)
        not-yet (policy/advance (-> show (assoc-in [:transport :beat] 2.9)
                                    (assoc-in [:transport :bpm] 240)
                                    (assoc-in [:transport :meter :beats-per-bar] 7)) config (:state paused) 91000)
        next (policy/advance (assoc-in show [:transport :beat] 3) config (:state not-yet) 92000)]
    (is (= {:bars 1} (get-in initial [:commands 0 :launch :duration])))
    (is (= 3 (get-in initial [:state :next-at-beat])))
    (is (empty? (:commands paused)))
    (is (empty? (:commands not-yet)))
    (is (= 1 (count (:commands next))))
    (is (= 6 (get-in next [:state :next-at-beat])))
    (is (empty? (:commands (policy/advance (assoc-in show [:transport :playing?] false) config state 0))))))

(deftest musical-emission-attacks-participate-in-real-prediction
  (let [show (update (model) :registry registry/publish :effects
                     (assoc emission :version 2 :envelope {:attack {:beats 1/2} :release {:beats 1/4}}))
        result (policy/advance show {:interval {:beats 2}} (policy/initial-state 1000 0 :fades) 0)]
    (is (= #{:motion :red} (set (map :cue-id (:commands result)))))
    (is (= :added-emission (get-in result [:reasons 0 :reason])))))
