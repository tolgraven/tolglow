(ns tolglow.studio.frame-stream-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [tolglow.studio.frame-stream :as frame-stream]
            [tolglow.studio.projection :as projection]))

(defn- frame [token values]
  {:token token :identity {:runtime-id "r" :show-id :main :epoch 1} :revision 2 :frame-number 1
   :time-ms 0 :present? true :value values})

(defn- recorder []
  (let [seen (atom [])]
    [seen (fn [f] (swap! seen conj (:token f)))]))

(deftest an-owner-receives-whole-frames-until-it-releases
  (frame-stream/reset-all!)
  (let [[seen on-frame] (recorder)]
    (frame-stream/install! :room (frame "a" {:head-1 {:intensity 0.1}}))
    (let [release! (frame-stream/attach! :room :perform on-frame)]
      (is (= ["a"] @seen) "attaching delivers the installed frame")
      (frame-stream/install! :room (frame "b" {:head-1 {:intensity 0.2}}))
      (is (= ["a" "b"] @seen))
      (release!)
      (is (empty? (frame-stream/attached :room)))
      (frame-stream/install! :room (frame "c" {}))
      (is (= ["a" "b"] @seen) "a released owner receives nothing")
      (release!)
      (is (empty? (frame-stream/attached :room)) "release is idempotent")))
  (testing "a replaced attachment's late release keeps its successor"
    (let [[old-seen old] (recorder) [new-seen new] (recorder)
          release-old! (frame-stream/attach! :room :perform old)
          _ (frame-stream/attach! :room :perform new)]
      (release-old!)
      (frame-stream/install! :room (frame "d" {}))
      (is (= ["c"] @old-seen))
      (is (= ["c" "d"] @new-seen))
      (is (= {:perform {:active? true :delivered "d"}} (frame-stream/attached :room)))))
  (frame-stream/reset-all!))

(deftest a-paused-consumer-resumes-with-the-latest-frame-only
  (frame-stream/reset-all!)
  (let [[seen on-frame] (recorder)]
    (frame-stream/attach! :room :rig on-frame :active? false)
    (frame-stream/install! :room (frame "a" {}))
    (is (empty? @seen) "hidden Rooms receive nothing")
    (frame-stream/set-active! :room :rig true)
    (is (= ["a"] @seen))
    (frame-stream/set-active! :room :rig true)
    (is (= ["a"] @seen) "resuming twice does not repeat a delivered token")
    (frame-stream/set-active! :room :rig false)
    (doseq [token ["b" "c" "d"]] (frame-stream/install! :room (frame token {})))
    (is (= ["a"] @seen))
    (frame-stream/set-active! :room :rig true)
    (is (= ["a" "d"] @seen) "skipped frames are replaced by the newest whole frame")
    (frame-stream/set-active! :room :missing true)
    (is (= [:rig] (keys (frame-stream/attached :room))) "pausing an unknown owner creates nothing"))
  (frame-stream/reset-all!))

(deftest one-failing-consumer-does-not-stop-installation-or-others
  (frame-stream/reset-all!)
  (let [[seen on-frame] (recorder)
        errors (atom 0)
        original (.-error js/console)]
    (set! (.-error js/console) (fn [& _] (swap! errors inc)))
    (try
      (let [release-broken! (frame-stream/attach! :room :broken (fn [_] (throw (js/Error. "consumer failed"))))]
        (frame-stream/attach! :room :perform on-frame)
        (frame-stream/install! :room (frame "a" {}))
        (is (= ["a"] @seen))
        (is (= "a" (:token (frame-stream/baseline :room))))
        (is (= 1 @errors))
        (release-broken!))
      (finally (set! (.-error js/console) original))))
  (testing "a consumer may release itself from inside its callback"
    (let [release (atom nil) [seen on-frame] (recorder)]
      (reset! release (frame-stream/attach! :room :self (fn [f] (on-frame f) (@release)) :active? false))
      (frame-stream/install! :room (frame "b" {}))
      (frame-stream/set-active! :room :self true)
      (frame-stream/install! :room (frame "c" {}))
      (is (= ["b"] @seen))
      (is (not (contains? (frame-stream/attached :room) :self)))))
  (frame-stream/reset-all!))

(deftest lanes-split-from-and-rejoin-only-their-exact-baseline
  (let [view {:runtime-id "r" :show-id :main :epoch 1 :revision 2 :frame-number 5 :time-ms 10
              :output {:values {:head-1 {:intensity 0.4}} :universes {1 [255]}}}
        [app-view lanes] (frame-stream/split-view view)
        installed {:room (frame-stream/frame "t" view (:room lanes))}
        projection {:token "t" :view app-view}]
    (is (= {:universes {1 [255]}} (:output app-view)))
    (is (= {:present? true :value {:head-1 {:intensity 0.4}}} (:room lanes)))
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
    (testing "an absent lane joins nothing and still keeps the token"
      (let [bare (update view :output dissoc :values)
            [bare-view bare-lanes] (frame-stream/split-view bare)
            joined (frame-stream/join-view {:token "t" :view bare-view}
                                           {:room (frame-stream/frame "t" bare (:room bare-lanes))})]
        (is (= {:token "t" :view bare} joined))))))
