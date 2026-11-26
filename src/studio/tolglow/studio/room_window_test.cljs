(ns tolglow.studio.room-window-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [tolglow.studio.room-window :as room]))

(defn- run [events] (reduce room/step room/docked events))

(deftest pop-out-and-redock-follow-one-window-at-a-time
  (let [popping (run [[:pop-out false]])]
    (is (= {:phase :popping :generation 1 :place? false} popping))
    (testing "a second pop-out while one is opening changes nothing"
      (is (= popping (room/step popping [:pop-out true]))))
    (is (= :popped (:phase (run [[:pop-out false] [:opened 1]]))))
    (is (= :redocking (:phase (run [[:pop-out false] [:opened 1] [:redock]]))))
    (is (= {:phase :docked :generation 1} (run [[:pop-out false] [:opened 1] [:redock] [:closed 1]])))
    (testing "the next window has a new generation"
      (is (= 2 (:generation (run [[:pop-out false] [:opened 1] [:redock] [:closed 1] [:pop-out false]])))))))

(deftest closing-the-window-redocks-and-stale-events-are-ignored
  (is (= {:phase :docked :generation 1} (run [[:pop-out false] [:opened 1] [:closed 1]])))
  (testing "closing while still opening"
    (is (= :docked (:phase (run [[:pop-out false] [:closed 1]])))))
  (testing "events of an earlier window"
    (let [second-window (run [[:pop-out false] [:opened 1] [:closed 1] [:pop-out false]])]
      (is (= second-window (room/step second-window [:opened 1])))
      (is (= second-window (room/step second-window [:closed 1])))
      (is (= :popped (:phase (room/step second-window [:opened 2])))))))

(deftest blocked-and-unsupported-windows-return-to-docked-with-an-explanation
  (let [blocked (run [[:pop-out false] [:failed 1 :blocked]])]
    (is (= {:phase :docked :generation 1 :failure :blocked} blocked))
    (is (re-find #"Allow pop-ups" (room/notice blocked)))
    (testing "the explanation clears on the next attempt"
      (is (nil? (:failure (room/step blocked [:pop-out false]))))))
  (is (re-find #"Fullscreen or Picture in Picture"
               (room/notice (run [[:pop-out false] [:failed 1 :unsupported]]))))
  (testing "a failure after the window opened is not a pop-out failure"
    (is (= :popped (:phase (run [[:pop-out false] [:opened 1] [:failed 1 :blocked]]))))))

(deftest move-to-display-remembers-its-request-until-the-window-opens
  (is (true? (:place? (run [[:pop-out true]]))))
  (let [asking (run [[:pop-out true] [:opened 1] [:placing 1]])]
    (is (= {:status :asking} (:display asking)))
    (is (false? (:place? asking)))
    (is (= "The Room is on DELL U2720Q. Press Fullscreen there to fill it."
           (room/notice (room/step asking [:placed 1 "DELL U2720Q"]))))))

(deftest denied-single-and-manual-placement-keep-the-window-usable
  (doseq [[kind pattern] [[:denied #"denied.*Drag the Room window"]
                          [:single #"Only one display"]
                          [:manual #"cannot place windows.*Drag"]]]
    (let [result (run [[:pop-out true] [:opened 1] [:placing 1] [:place-failed 1 kind]])]
      (is (= :popped (:phase result)))
      (is (= kind (get-in result [:display :status])))
      (is (re-find pattern (room/notice result))))))

(deftest display-removal-and-reconnection-are-announced
  (let [placed (run [[:pop-out true] [:opened 1] [:placing 1] [:placed 1 "Projector"]])
        lost (room/step placed [:screens 1 1])]
    (is (= placed (room/step placed [:screens 1 2])))
    (is (= :lost (get-in lost [:display :status])))
    (is (re-find #"disconnected" (room/notice lost)))
    (is (= :available (get-in (room/step lost [:screens 1 2]) [:display :status])))
    (is (= :single (get-in (room/step (room/step lost [:screens 1 2]) [:screens 1 1]) [:display :status]))))
  (let [single (run [[:pop-out true] [:opened 1] [:place-failed 1 :single]])]
    (is (= :available (get-in (room/step single [:screens 1 2]) [:display :status]))))
  (testing "screen changes are ignored while docked"
    (is (= room/docked (room/step room/docked [:screens 0 1])))))

(deftest the-room-goes-to-a-display-other-than-studios
  (let [screens [{:left 0 :top 0 :width 1512 :height 945}
                 {:left 1512 :top 0 :width 1920 :height 1080}
                 {:left -1920 :top 0 :width 1920 :height 1080}]]
    (is (= 0 (room/screen-at screens 700 400)))
    (is (= 1 (room/screen-at screens 2000 500)))
    (is (= 2 (room/screen-at screens -10 10)))
    (is (nil? (room/screen-at screens 9000 10)))
    (is (= 1 (room/target-screen screens 0 0)) "from Studio's display to the first free one")
    (is (= 2 (room/target-screen screens 0 1)) "pressing again cycles to the next free display")
    (is (= 1 (room/target-screen screens 0 2)))
    (is (= 0 (room/target-screen screens 1 nil)))
    (is (= 1 (room/target-screen (subvec screens 0 2) 0 1)) "one free display: stay and refill it")
    (is (nil? (room/target-screen (subvec screens 0 1) 0 0)) "no free display")))

(deftest losing-the-rooms-own-display-is-noticed-while-others-remain
  (let [placed (run [[:pop-out true] [:opened 1] [:placing 1] [:placed 1 "Projector A"]])]
    (testing "another display going away leaves the Room placed"
      (is (= placed (room/step placed [:screens 1 {:count 2 :present? true}]))))
    (testing "its own display going away is a loss even with two displays left"
      (let [lost (room/step placed [:screens 1 {:count 2 :present? false}])]
        (is (= :lost (get-in lost [:display :status])))
        (is (= lost (room/step lost [:screens 1 {:count 2 :present? false}]))
            "an unrelated change does not claim the display came back")
        (is (= :available (get-in (room/step lost [:screens 1 {:count 3 :present? true}]) [:display :status])))))
    (testing "without presence information the display count decides, as before"
      (is (= placed (room/step placed [:screens 1 {:count 2 :present? nil}])))
      (is (= :lost (get-in (room/step placed [:screens 1 {:count 1 :present? nil}]) [:display :status]))))))
