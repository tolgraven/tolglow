(ns tolglow.studio.audience-invitation-test
  (:require [cljs.test :refer [deftest is]]
            [tolglow.studio.audience-invitation :as invitation]))

(deftest the-published-stage-identifies-the-actual-gateway
  (let [remote {:sources ["balcony" "audience"] :receiver? true}
        form {:stage-name "My draft" :device-id "balcony" :origin "https://lights.example.org"}]
    (is (= "balcony" (:device-id (invitation/view-model remote form))))
    (is (= "audience" (:device-id (invitation/view-model remote (assoc form :device-id "missing")))))
    (let [running (assoc remote :id "current" :stage-name "Main room" :device-id "audience")]
      (is (= (assoc running :pending? true)
             (invitation/view-model running (assoc form :pending? true)))))))
