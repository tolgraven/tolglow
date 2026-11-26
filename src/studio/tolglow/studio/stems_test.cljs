(ns tolglow.studio.stems-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [tolglow.studio.connections :as connections]
            [tolglow.studio.stems :as stems]))

(deftest stem-rows-round-trip-through-the-capture-configuration
  (let [config {:stems [{:id "kick" :channel 1 :preset "kick"} {:id "bass-di" :channel 3}]}]
    (is (= [{:id "kick" :channel "1" :preset "kick"} {:id "bass-di" :channel "3" :preset "full"}]
           (stems/form-rows config)))
    (is (= [{:id :kick :channel 1 :preset :kick} {:id :bass-di :channel 3 :preset :full}]
           (stems/payload (stems/form-rows config) 4)))
    (is (nil? (stems/payload [] 4)))
    (doseq [[rows message] [[[{:id "Kick" :channel "1"}] #"lowercase"]
                            [[{:id "kick" :channel "5"}] #"channel from 1 to 4"]
                            [[{:id "kick" :channel "1"} {:id "kick" :channel "2"}] #"its own name"]]]
      (is (re-find message (try (stems/payload rows 4) "" (catch :default e (.-message e))))))))

(deftest the-audio-payload-carries-stems-for-a-multichannel-input
  (let [db {:connections {:inputs [] :audio-form (assoc connections/audio-defaults :input "system" :channels "4"
                                                        :input-channel "1"
                                                        :stems [{:id "kick" :channel "1" :preset "kick"}
                                                                {:id "guitar" :channel "4" :preset "guitar"}])}}]
    (is (= [{:id :kick :channel 1 :preset :kick} {:id :guitar :channel 4 :preset :guitar}]
           (:stems (connections/audio-payload db))))
    (testing "no stems leaves the ordinary configuration unchanged"
      (is (not (contains? (connections/audio-payload (assoc-in db [:connections :audio-form :stems] [])) :stems))))))

(deftest the-running-patch-is-read-from-its-edn-source
  (is (= {:version 1 :id :stems :sources {"stem/bass/level" {:kind :audio}}}
         (stems/current-spec {:show {:patch {:source "{:version 1 :id :stems :sources {\"stem/bass/level\" {:kind :audio}}}"}}})))
  (is (nil? (stems/current-spec {:show {:patch {}}}))))
