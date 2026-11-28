(ns tolglow.studio.stems-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [tolglow.audio.stem-routes :as stem-routes]
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

(deftest detector-overrides-set-from-the-repl-survive-a-studio-restart
  (let [config {:stems [{:id "guitar" :channel 4 :preset "guitar" :rise-db 4.0 :min-interval {:beats 0.25}}]}
        rows (stems/form-rows config)]
    (is (= [{:id "guitar" :channel "4" :preset "guitar" :overrides {:rise-db 4.0 :min-interval {:beats 0.25}}}] rows))
    (is (= [{:id :guitar :channel 4 :preset :guitar :rise-db 4.0 :min-interval {:beats 0.25}}]
           (stems/payload rows 4)))
    (testing "changing the instrument keeps the explicit settings"
      (is (= :keys (:preset (first (stems/payload (assoc-in rows [0 :preset] "keys") 4)))))
      (is (= 4.0 (:rise-db (first (stems/payload (assoc-in rows [0 :preset] "keys") 4))))))))

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

(deftest a-stem-can-turn-a-linked-knob
  (let [spec (stem-routes/with-knob-route nil :guitar "swell" {})]
    (is (= {:knob-id "swell" :range [0.0 1.0] :base 0.0 :combine :max} (get-in spec [:destinations :knob-swell])))
    (is (= {:guitar {:ports [{:route-id :stem-guitar-level-knob-swell :feature :level :knob-id "swell"}]}}
           (stem-routes/routes [] spec)))))
