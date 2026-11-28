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

(defn- lanes-at [token view-token view lanes]
  (into {} (map (fn [[lane parts]] [lane (frame-stream/frame token view-token view parts)])) lanes))

(deftest lanes-split-from-and-rejoin-only-their-exact-baseline
  (let [view {:runtime-id "r" :show-id :main :epoch 1 :revision 2 :frame-number 5 :time-ms 10 :master 0.5
              :transport {:beat 3.5 :time-ms 9 :bpm 120} :schedule {:beat 3.5 :upcoming []}
              :status {:last-intents [{:type :instance/finished}] :frame-ms 1.2 :output-scheduler {:published 4}}
              :output {:values {:head-1 {:intensity 0.4}} :universes {1 [255]} :atmosphere {:sources []}}
              :draft-output {:draft {:id "scene"} :values {:head-1 {:intensity 1}}}}
        [app-view lanes] (frame-stream/split-view view)
        installed (lanes-at "t" "t" view lanes)
        lane-frame (:room installed)
        projection {:token "t" :view app-view}]
    (is (= {:universes {1 [255]} :atmosphere {:sources []}} (:output app-view)))
    (is (= {:draft {:id "scene"}} (:draft-output app-view)))
    (testing "every per-frame field leaves the app-db view"
      (is (not-any? #(contains? app-view %) [:frame-number :time-ms]))
      (is (= {:bpm 120} (:transport app-view)))
      (is (= {:upcoming []} (:schedule app-view)))
      (is (= {:last-intents [{:type :instance/finished}]} (:status app-view)))
      (is (= {:frame-number 5 :time-ms 10 :beat 3.5 :transport-time-ms 9 :schedule-beat 3.5 :frame-ms 1.2
              :output-scheduler {:published 4}}
             (:clock lanes))))
    (is (= {:values {:head-1 {:intensity 0.4}} :audition {:head-1 {:intensity 1}}} (:room lanes)))
    (is (= [5 10 0.5 [{:type :instance/finished}] {:sources []}]
           ((juxt :frame-number :time-ms :master :last-intents :atmosphere) lane-frame)))
    (is (= {:token "t" :view view} (frame-stream/join-view projection installed)))
    (testing "a projection whose view was last written earlier joins lanes of the current token"
      (is (= {:token "u" :view view} (frame-stream/join-view projection (lanes-at "u" "t" view lanes))))
      (is (nil? (:token (frame-stream/join-view projection (lanes-at "u" "u" view lanes))))))
    (testing "a patch to joined values applies at the exact token"
      (let [patch {:protocol projection/protocol :kind :patch :base-token "t" :token "u"
                   :identity (projection/identity-of view)
                   :changes [[:set [:output :values :head-1 :intensity] 0.9]]}]
        (is (= 0.9 (get-in (projection/receive (frame-stream/join-view projection installed) patch)
                           [:state :view :output :values :head-1 :intensity])))
        (testing "and requests a full view when a lane belongs to another token or show"
          (doseq [stale [(assoc-in installed [:room :token] "old")
                         (assoc-in installed [:clock :view-token] "old")
                         (assoc-in installed [:room :identity :epoch] 0)
                         (dissoc installed :clock)
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
            joined (frame-stream/join-view {:token "t" :view bare-view} (lanes-at "t" "t" bare bare-lanes))]
        (is (= {} (:room bare-lanes)))
        (is (= {:token "t" :view bare} joined))))))

(deftest only-patches-inside-lane-subtrees-leave-app-db-alone
  (is (frame-stream/lane-only? [[:set [:frame-number] 3] [:set [:transport :beat] 1.5]
                                [:set [:status :output-scheduler :published] 9] [:remove [:output :values :head-1]]]))
  (is (frame-stream/lane-only? []))
  (is (not (frame-stream/lane-only? [[:set [:frame-number] 3] [:set [:transport :bpm] 128]])))
  (is (not (frame-stream/lane-only? [[:set [:transport] {:beat 1}]])) "replacing a parent is not lane-only")
  (is (not (frame-stream/lane-only? [[:set [:output] {}]])))
  (is (not (frame-stream/lane-only? nil))))

(deftest connection-status-and-sampled-signals-leave-app-db-on-their-own-lanes
  ;; Measured on a capture-only host with synthetic audio (#790): these are the
  ;; Connections and signal fields that change on every frame.
  (let [view {:runtime-id "r" :show-id :main :epoch 1 :revision 2
              :connection-status {:audio {:state :running :config {:sample-rate 48000}
                                          :features {:rms 0.2} :waveform {:bins [0.1]} :metrics {:blocks 9}
                                          :sample-counter 4800 :received-event-count 3}
                                  :sync {:state :idle :config {:selected-source :manual} :clock {:status :manual}}
                                  :audience {:sessions 0}}
              :patch {:enabled? true :source "{}" :sources [{:source-id "audio/rms" :value 0.2}]}
              :audio/energy 0.4
              :output {:lighting-activity {:scene {:score 0.3}} :lighting-budget {:enabled? true}}}
        [app-view lanes] (frame-stream/split-view view)]
    (is (= {:audio {:state :running :config {:sample-rate 48000}}
            :sync {:state :idle :config {:selected-source :manual}}
            :audience {:sessions 0}}
           (:connection-status app-view)))
    (is (= {:enabled? true :source "{}"} (:patch app-view)))
    (is (not (contains? app-view :audio/energy)))
    (is (= {:lighting-budget {:enabled? true}} (:output app-view)))
    (is (= {:audio-features {:rms 0.2} :audio-waveform {:bins [0.1]} :audio-metrics {:blocks 9}
            :audio-samples 4800 :audio-events 3 :sync-clock {:status :manual}}
           (:connections lanes)))
    (is (= {:patch-sources [{:source-id "audio/rms" :value 0.2}] :audio-energy 0.4
            :lighting-activity {:scene {:score 0.3}}}
           (:signals lanes)))
    (is (= {:token "t" :view view}
           (frame-stream/join-view {:token "t" :view app-view} (lanes-at "t" "t" view lanes))))
    (is (frame-stream/lane-only? [[:set [:connection-status :audio :features :rms] 0.3]
                                  [:set [:connection-status :audio :waveform :bins 0] 0.2]
                                  [:set [:connection-status :sync :clock :beat] 4]
                                  [:set [:patch :sources 0 :value] 0.5] [:set [:audio/energy] 0.5]
                                  [:set [:output :lighting-activity :scene :score] 0.4]]))
    (is (not (frame-stream/lane-only? [[:set [:connection-status :audio :state] :stopped]])))
    (is (not (frame-stream/lane-only? [[:remove [:connection-status]]])) "closing Connections changes the view")))
