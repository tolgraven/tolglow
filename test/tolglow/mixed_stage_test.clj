(ns tolglow.mixed-stage-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.qa.mixed-stage :as stage]
            [tolglow.show :as show]))

(defn- emitted [snapshot id]
  (get-in snapshot [:output :values id :emitted-color]))

(deftest mixed-stage-uses-shared-cues-and-changing-output
  (let [runtime (show/create-runtime {:model (stage/model) :workspace-options nil})
        now (atom -25)
        tick #(show/tick! runtime (swap! now + 25))
        command (fn [action]
                  (let [receipt (show/submit! runtime action)]
                    (tick)
                    (is (= :accepted (:status @receipt)))))]
    (try
      (let [ids (stage/start-look! runtime tick)
            before (show/snapshot runtime)
            energy (:stage-energy ids)
            rated (get-in before [:output :lighting-budget :global :capacity-watts])]
        (is (= 55 (count (get-in before [:rig :fixtures]))))
        (is (= 40 (count (filter :diffuser-radius-m (get-in before [:rig :fixtures])))))
        (is (= 7 (count ids)))
        (is (pos? rated))
        (is (= (* 0.46 rated) (get-in before [:output :lighting-budget :global :limit-watts])))
        (dotimes [_ 7] (tick))
        (let [after (show/snapshot runtime)]
          (doseq [id [:head-1 :wash-1 :tube-1-head-1]]
            (is (some pos? (emitted after id)))
            (is (not= (emitted before id) (emitted after id)))))
        (command {:op :param/set :instance-id energy :param :cap :value 0.22})
        (command {:op :param/set :instance-id energy :param :brightness :value 0.27})
        (let [after (show/snapshot runtime)]
          (is (= (* 0.22 rated) (get-in after [:output :lighting-budget :global :limit-watts])))
          (is (= 0.27 (get-in after [:autopilot :requested-targets :brightness]))))
        (command {:op :instance/end :instance-id energy})
        (let [after (show/snapshot runtime)]
          (is (= rated (get-in after [:output :lighting-budget :global :limit-watts])))
          (is (not (get-in after [:autopilot :enabled?])))))
      (finally (show/stop! runtime)))))
