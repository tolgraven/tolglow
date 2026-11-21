(ns tolglow.composition.macro-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.composition.macro :as macro]))

(deftest macro-slots-have-no-timing-and-own-only-captured-instances
  (let [plan (macro/launch-plan "macro-1" [{:id :slot-a :cue-id :red :params {:level 0.5}}
                                         {:id :slot-b :cue-id :motion}])
        instances {["macro-1" :slot-a] "instance-a"
                   ["other" :slot-b] "instance-b"}]
    (is (= [:red :motion] (mapv :cue-id (:intents plan))))
    (is (= {:level 0.5} (get-in plan [:intents 0 :launch :params])))
    (is (= [{:op :instance/end :instance-id "instance-a" :owner-id "macro-1"}]
           (macro/end-plan plan instances)))
    (is (not-any? :duration-ms (:intents plan)))))

(deftest duplicate-slots-are-invalid
  (is (thrown? clojure.lang.ExceptionInfo
               (macro/launch-plan "m" [{:id :a :cue-id :red} {:id :a :cue-id :blue}]))))
