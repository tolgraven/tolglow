(ns tolglow.param-nudge-test
  "The owner-resolved relative parameter edit, :param/nudge (#813), through
  a real show owner: every detent counts however the owner drains its queue,
  a whole-vector set and nudges to other components keep each other, and a
  nudge is validated, replayed and recorded like the param/set it commits."
  (:require [afterglow.control.model :as control]
            [clojure.test :refer [deftest is testing]]
            [tolglow.param-nudge :as param-nudge]
            [tolglow.show :as show]))

(defn- model []
  (control/new-show
   {:show-id :nudges :transport {:playing? true :beat 0.0 :bpm 120.0}
    :registry {:effects {:rig {:id :rig :version 1
                               :params {:level {:type :number :default 0.5 :bounds [0 1]}
                                        :count {:type :integer :default 2 :bounds [0 4]}
                                        :mode {:type :enum :values [:a :b] :default :a}
                                        :offset {:type :vector :size 4 :default [0.0 0.0 0.0 0.0]
                                                 :bounds [0 1] :step 0.05}
                                        :pair {:type :vector :size 2 :default [0 0] :bounds [0 10]
                                               :component-schemas [{:type :integer} {:bounds [0 20]}]}}
                               :outputs {:dimmer {:op :param :key :level}}}}
               :cues {:c0 {:id :c0 :version 1 :effect-id :rig}}}
    ;; A 16-bit dimmer: Shift on :level moves one DMX code.
    :rig {:fixtures [{:id :head :universe 1 :address 1 :channels {:dimmer [1 2]}}]}}))

(def ^:private push {:adapter :push-2 :session-id "push2"})

(def ^:private studio {:adapter :web :session-id "studio"})

(defn- submit [rt source payload] (show/submit! rt :command (assoc payload :source source)))

(defn- value [rt id k] (get-in @(:state rt) [:model :instances id :params k]))

(defn- round3 [xs] (mapv #(/ (Math/round (* 1000 (double %))) 1000.0) xs))

(defn- launch! [rt]
  (let [reply (submit rt studio {:op :cue/launch :cue-id :c0 :launch {:mode :persistent}})]
    (show/tick! rt 1)
    (:instance-id (deref reply 1000 ::missing))))

(defn- nudge [id k & {:as more}] (merge {:op :param/nudge :instance-id id :param k :detents 1} more))

(deftest three-detents-move-by-three-however-the-owner-drains-them
  (let [rt (show/create-runtime {:model (model) :workspace-options nil})]
    (try
      (let [id (launch! rt)
            ;; Two queue up before a tick and the third after it: a partial drain.
            a (submit rt push (nudge id :offset :component 0))
            b (submit rt push (nudge id :offset :component 0))
            _ (show/tick! rt 2)
            c (submit rt push (nudge id :offset :component 0))
            _ (show/tick! rt 3)]
        (is (= [:accepted :accepted :accepted] (mapv #(:status (deref % 1000 ::missing)) [a b c])))
        (is (= [0.15 0.0 0.0 0.0] (round3 (value rt id :offset)))))
      (finally (show/stop! rt)))))

(deftest a-whole-vector-set-and-nudges-to-other-components-keep-each-other
  (let [rt (show/create-runtime {:model (model) :workspace-options nil})]
    (try
      (let [id (launch! rt)]
        (testing "a Push nudge, then a Studio set of Y and a Push nudge of X in one drain"
          (deref (submit rt push (nudge id :offset :component 0 :detents 2)) 0 nil)
          (show/tick! rt 2)
          ;; Studio built its vector from the committed [0.1 0 0 0] and changes Y.
          (let [s (submit rt studio {:op :param/set :instance-id id :param :offset :value [0.1 0.5 0.0 0.0]})
                n (submit rt push (nudge id :offset :component 0))]
            (show/tick! rt 3)
            (is (= [:accepted :accepted] (mapv #(:status (deref % 1000 ::missing)) [s n])))
            (is (= [0.15 0.5 0.0 0.0] (round3 (value rt id :offset))) "both edits are kept")))
        (testing "nudges to two components queued together both land"
          (let [x (submit rt push (nudge id :offset :component 3 :detents 4))
                y (submit rt studio (nudge id :offset :component 1 :detents -2))]
            (show/tick! rt 4)
            (is (= [:accepted :accepted] (mapv #(:status (deref % 1000 ::missing)) [x y])))
            (is (= [0.15 0.4 0.0 0.2] (round3 (value rt id :offset)))))))
      (finally (show/stop! rt)))))

(deftest a-nudge-steps-clamps-rounds-and-uses-the-patch-for-fine
  (let [rt (show/create-runtime {:model (model) :workspace-options nil})
        run! (fn [time payload] (let [r (submit rt push payload)] (show/tick! rt time) (deref r 1000 ::missing)))]
    (try
      (let [id (launch! rt)]
        (testing "a number: a hundredth of its range per detent, one 16-bit DMX code with fine"
          (run! 2 (nudge id :level :detents 10))
          (is (< (Math/abs (- 0.6 (value rt id :level))) 1e-9))
          (run! 3 (nudge id :level :detents 1 :fine? true))
          (is (< (Math/abs (- (+ 0.6 (/ 1.0 65535)) (value rt id :level))) 1e-9)))
        (testing "a vector component without a patched channel: a tenth of the step with fine"
          (run! 4 (nudge id :offset :component 2 :fine? true))
          (is (= [0.0 0.0 0.005 0.0] (mapv #(/ (Math/round (* 100000 (double %))) 100000.0) (value rt id :offset)))))
        (testing "bounds clamp; integers step by one; a component's bounds stay within the vector's"
          (run! 5 (nudge id :offset :component 1 :detents -3))
          (is (= 0.0 (second (value rt id :offset))))
          (run! 6 (nudge id :count :detents 5))
          (is (= 4 (value rt id :count)))
          (run! 7 (nudge id :pair :component 0 :detents 3))
          (run! 8 (nudge id :pair :component 1 :detents 500))
          (is (= [3 10.0] (value rt id :pair)) "component 1 declares [0 20] but the vector allows [0 10]")))
      (finally (show/stop! rt)))))

(deftest invalid-nudges-are-refused-and-a-replay-commits-once
  (let [rt (show/create-runtime {:model (model) :workspace-options nil})
        run! (fn [time payload] (let [r (submit rt push payload)] (show/tick! rt time) (deref r 1000 ::missing)))]
    (try
      (let [id (launch! rt)]
        (doseq [[bad why] [[(nudge "nope" :level) "an unknown instance"]
                           [(nudge id :nope) "an unknown parameter"]
                           [(nudge id :offset) "a vector without a component"]
                           [(nudge id :offset :component 4) "a component out of range"]
                           [(nudge id :level :component 0) "a number with a component"]
                           [(nudge id :level :detents 0) "zero detents"]
                           [(nudge id :level :detents 1.5) "fractional detents"]
                           [(nudge id :level :detents 5000) "too many detents"]
                           [(nudge id :mode) "an enum"]]]
          (let [ack (run! 2 bad)]
            (is (= [:rejected :invalid-nudge] [(:status ack) (:code ack)]) why)))
        (is (= 0.5 (value rt id :level)) "nothing changed")
        (testing "a replayed nudge returns its receipt and moves nothing again"
          (let [first-ack (run! 3 (nudge id :level :command-id "turn-1" :detents 5))
                again (run! 4 (nudge id :level :command-id "turn-1" :detents 5))]
            (is (= :accepted (:status first-ack)))
            (is (= (:revision first-ack) (:revision again)))
            (is (< (Math/abs (- 0.55 (value rt id :level))) 1e-9))))
        (testing "a nudge commits as the param/set of its result, with its source"
          (is (= {:op :param/set :instance-id id :param :level :value 0.55}
                 (select-keys (get-in @(:state rt) [:model :control :history "turn-1" :command])
                              [:op :instance-id :param :value])))
          (is (= push (get-in @(:state rt) [:model :control :history "turn-1" :command :source])))))
      (finally (show/stop! rt)))))

(deftest component-schemas-intersect-with-the-vector
  (is (= [{:type :number :bounds [0 1] :step 0.1} {:type :integer :bounds [0.5 1] :step 0.1}]
         (param-nudge/component-schemas {:type :vector :size 2 :bounds [0 1] :step 0.1
                                         :component-schemas [nil {:type :integer :bounds [0.5 3]}]})))
  (is (nil? (param-nudge/component-schemas {:type :vector :size 2 :bounds [0 1]
                                            :component-schemas [nil {:bounds [2 3]}]}))))
