(ns tolglow.studio.navigation-test
  (:require [cljs.test :refer-macros [deftest is]]
            [tolglow.studio.navigation :as nav]
            [tolglow.studio.page-routes :as pages]
            [tolglow.studio.events-test :refer [base transition]]))

(deftest studio-links-preserve-view-and-popup-identity
  (doseq [view pages/views]
    (is (= {:name view :surface :studio :view view}
           (select-keys (:data (nav/match-path (nav/href view))) [:name :surface :view]))))
  (is (= :home (get-in (nav/match-path "/") [:data :name])))
  (let [url (nav/href :repl {:session-id "live + scratch/λ"})
        match (nav/match-path url)]
    (is (= :repl (get-in match [:data :surface])))
    (is (= "live + scratch/λ" (get-in match [:query-params :session-id]))))
  (is (= :push-display (get-in (nav/match-path (nav/href :push-display)) [:data :surface])))
  (is (nil? (nav/match-path "/api/command"))))

(deftest explicit-route-wins-over-delayed-show-preferences
  (let [rig (:db (transition base [:studio/route-matched :rig]))
        settings [:studio/settings-received {:ui {:view "assistant" :launch-mode "held"}}]
        restored (transition rig settings)
        home (:db (transition base [:studio/route-matched :home]))
        resume (transition home settings)]
    (is (= :rig (get-in restored [:db :ui :view])))
    (is (= "held" (get-in restored [:db :ui :launch-mode])))
    (is (nil? (:studio/navigate restored)))
    (is (= [:replace :assistant] (:studio/navigate resume)))
    (is (= [:push :library] (:studio/navigate (transition rig [:studio/set-view :library]))))))
