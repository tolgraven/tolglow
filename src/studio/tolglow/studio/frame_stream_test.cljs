(ns tolglow.studio.frame-stream-test
  (:require [cljs.test :refer-macros [deftest is testing use-fixtures]]
            [tolglow.studio.frame-stream :as frame-stream]
            [tolglow.studio.projection :as projection]))

(use-fixtures :each {:before frame-stream/reset-all! :after frame-stream/reset-all!})

(defn- frame [token values]
  {:token token :identity {:runtime-id "r" :show-id :main :epoch 1} :revision 2 :frame-number 1
   :time-ms 0 :value {:values values}})

(defn- publish! [token]
  (frame-stream/install! :room (frame token {}))
  (frame-stream/notify! :room))

(defn- recorder []
  (let [seen (atom [])]
    [seen (fn [f] (swap! seen conj (:token f)))]))

(deftest installing-calls-no-consumer-until-notified
  (let [[seen on-frame] (recorder)]
    (frame-stream/attach! :room :perform on-frame)
    (frame-stream/install! :room (frame "a" {}))
    (is (empty? @seen) "installation alone, before the ACK, runs no consumer work")
    (frame-stream/notify! :room)
    (is (= ["a"] @seen))))

(deftest an-owner-receives-whole-frames-until-it-releases
  (let [[seen on-frame] (recorder)]
    (publish! "a")
    (let [release! (frame-stream/attach! :room :perform on-frame)]
      (is (= ["a"] @seen) "attaching delivers the installed frame")
      (publish! "b")
      (is (= ["a" "b"] @seen))
      (release!)
      (is (empty? (frame-stream/attached :room)))
      (publish! "c")
      (is (= ["a" "b"] @seen) "a released owner receives nothing")
      (release!)
      (is (empty? (frame-stream/attached :room)) "release is idempotent")))
  (testing "a replaced attachment's late release keeps its successor"
    (let [[old-seen old] (recorder) [new-seen new] (recorder)
          release-old! (frame-stream/attach! :room :perform old)
          _ (frame-stream/attach! :room :perform new)]
      (release-old!)
      (publish! "d")
      (is (= ["c"] @old-seen))
      (is (= ["c" "d"] @new-seen))
      (is (= {:perform {:active? true :delivered "d" :failures 0}} (frame-stream/attached :room))))))

(deftest a-scheduled-consumer-receives-only-the-newest-frame-per-callback
  (let [[seen on-frame] (recorder)
        queue (atom [])
        run! #(let [callbacks @queue] (reset! queue []) (doseq [f callbacks] (f)))
        release! (frame-stream/attach! :room :perform on-frame :schedule #(swap! queue conj %))]
    (doseq [token ["a" "b" "c"]] (publish! token))
    (is (empty? @seen) "nothing runs before the consumer's frame")
    (is (= 1 (count @queue)) "one callback is scheduled for a burst")
    (run!)
    (is (= ["c"] @seen) "the newest frame wins")
    (run!)
    (is (= ["c"] @seen))
    (publish! "d")
    (release!)
    (run!)
    (is (= ["c"] @seen) "a callback after release delivers nothing")
    (testing "a re-attachment's callbacks are its own"
      (let [[again on-again] (recorder)]
        (frame-stream/attach! :room :perform on-again :schedule #(swap! queue conj %))
        (publish! "e")
        (run!)
        (is (= ["e"] @again))))))

(deftest a-paused-consumer-resumes-with-the-latest-frame-only
  (let [[seen on-frame] (recorder)]
    (frame-stream/attach! :room :rig on-frame :active? false)
    (publish! "a")
    (is (empty? @seen) "hidden Rooms receive nothing")
    (frame-stream/set-active! :room :rig true)
    (is (= ["a"] @seen))
    (frame-stream/set-active! :room :rig true)
    (is (= ["a"] @seen) "resuming twice does not repeat a delivered token")
    (frame-stream/set-active! :room :rig false)
    (doseq [token ["b" "c" "d"]] (publish! token))
    (is (= ["a"] @seen))
    (frame-stream/set-active! :room :rig true)
    (is (= ["a" "d"] @seen) "skipped frames are replaced by the newest whole frame")
    (frame-stream/set-active! :room :missing true)
    (is (= [:rig] (keys (frame-stream/attached :room))) "pausing an unknown owner creates nothing")))

(deftest a-failing-consumer-is-logged-once-and-does-not-stop-others
  (let [[seen on-frame] (recorder)
        logged (atom 0)
        original (.-error js/console)]
    (set! (.-error js/console) (fn [& _] (swap! logged inc)))
    (try
      (frame-stream/attach! :room :broken (fn [_] (throw (js/Error. "consumer failed"))))
      (frame-stream/attach! :room :perform on-frame)
      (doseq [token ["a" "b" "c"]] (publish! token))
      (is (= ["a" "b" "c"] @seen))
      (is (= "c" (:token (frame-stream/baseline :room))))
      (is (= 1 @logged) "repeats of one failure are counted, not logged")
      (is (= 3 (:failures (get (frame-stream/attached :room) :broken))))
      (finally (set! (.-error js/console) original))))
  (testing "a consumer may release itself from inside its callback"
    (let [release (atom nil) [seen on-frame] (recorder)]
      (reset! release (frame-stream/attach! :room :self (fn [f] (on-frame f) (@release)) :active? false))
      (publish! "d")
      (frame-stream/set-active! :room :self true)
      (publish! "e")
      (is (= ["d"] @seen))
      (is (not (contains? (frame-stream/attached :room) :self))))))

(deftest lanes-split-from-and-rejoin-only-their-exact-baseline
  (let [view {:runtime-id "r" :show-id :main :epoch 1 :revision 2 :frame-number 5 :time-ms 10 :master 0.5
              :status {:last-intents [{:type :instance/finished}]}
              :output {:values {:head-1 {:intensity 0.4}} :universes {1 [255]} :atmosphere {:sources []}}
              :draft-output {:draft {:id "scene"} :values {:head-1 {:intensity 1}}}}
        [app-view lanes] (frame-stream/split-view view)
        lane-frame (frame-stream/frame "t" view (:room lanes))
        installed {:room lane-frame}
        projection {:token "t" :view app-view}]
    (is (= {:universes {1 [255]} :atmosphere {:sources []}} (:output app-view)))
    (is (= {:draft {:id "scene"}} (:draft-output app-view)))
    (is (= {:values {:head-1 {:intensity 0.4}} :audition {:head-1 {:intensity 1}}} (:room lanes)))
    (is (= [5 10 0.5 [{:type :instance/finished}] {:sources []}]
           ((juxt :frame-number :time-ms :master :last-intents :atmosphere) lane-frame)))
    (is (= {:token "t" :view view} (frame-stream/join-view projection installed)))
    (testing "a patch to joined values applies at the exact token"
      (let [patch {:protocol projection/protocol :kind :patch :base-token "t" :token "u"
                   :identity (projection/identity-of view)
                   :changes [[:set [:output :values :head-1 :intensity] 0.9]]}]
        (is (= 0.9 (get-in (projection/receive (frame-stream/join-view projection installed) patch)
                           [:state :view :output :values :head-1 :intensity])))
        (testing "and requests a full view when the lane belongs to another token or show"
          (doseq [stale [(assoc-in installed [:room :token] "old")
                         (assoc-in installed [:room :identity :epoch] 0)
                         {}]]
            (let [joined (frame-stream/join-view projection stale)]
              (is (nil? (:token joined)))
              (is (= :resync (:status (projection/receive joined patch))))
              (is (= :accepted (:status (projection/receive joined {:protocol projection/protocol :kind :full
                                                                    :token "t" :identity (projection/identity-of view)
                                                                    :view view})))))))))
    (testing "absent parts join nothing and still keep the token"
      (let [bare (-> view (update :output dissoc :values) (dissoc :draft-output))
            [bare-view bare-lanes] (frame-stream/split-view bare)
            joined (frame-stream/join-view {:token "t" :view bare-view}
                                           {:room (frame-stream/frame "t" bare (:room bare-lanes))})]
        (is (= {} (:room bare-lanes)))
        (is (= {:token "t" :view bare} joined))))))
