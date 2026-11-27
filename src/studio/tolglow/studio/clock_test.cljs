(ns tolglow.studio.clock-test
  (:require [cljs.test :refer-macros [deftest is testing use-fixtures]]
            [re-frame.core :as rf]
            [re-frame.db :as rf-db]
            [reagent.ratom :as ratom]
            [tolglow.studio.clock :as clock]
            [tolglow.studio.cue-pad-preview :as pad-preview]
            [tolglow.studio.frame-stream :as frame-stream]
            [tolglow.studio.reactions :as reactions]
            [tolglow.studio.state-socket]))

(use-fixtures :each {:before frame-stream/reset-all! :after frame-stream/reset-all!})

(defn- publish! [lane token parts]
  (frame-stream/install! lane {:token token :value parts})
  (frame-stream/notify! lane))

(defn- watch
  "A live reader of query, as a mounted view holds one."
  [query]
  (doto (ratom/make-reaction #(deref (rf/subscribe query)) :auto-run true) deref))

(deftest views-read-per-frame-fields-from-the-clock-lane
  (let [before @rf-db/app-db]
    (try
      (reset! rf-db/app-db {:show {:transport {:bpm 120 :playing? true :meter {:beats-per-bar 4}}}})
      (let [beat (watch [:studio/clock-part :beat])
            transport (watch [:studio/transport])
            app-db-writes (atom 0)]
        (add-watch rf-db/app-db ::writes (fn [_ _ _ _] (swap! app-db-writes inc)))
        (is (nil? @beat))
        (publish! :clock "a" {:beat 3.5 :transport-time-ms 1750 :frame-number 70 :output-scheduler {:mode :direct}})
        (ratom/flush!)
        (is (= 3.5 @beat))
        (is (= {:bpm 120 :playing? true :meter {:beats-per-bar 4} :beat 3.5 :time-ms 1750} @transport))
        (is (= "direct" (:mode (:output-scheduler (clock/current)))) "event handlers read presented parts too")
        (is (zero? @app-db-writes) "a show frame never writes app-db")
        (testing "only the parts installed before notification are visible to readers"
          (frame-stream/install! :clock {:token "b" :value {:beat 4.0}})
          (ratom/flush!)
          (is (= 3.5 @beat) "installation alone, before the ACK, publishes nothing")
          (frame-stream/notify! :clock)
          (ratom/flush!)
          (is (= 4.0 @beat)))
        (remove-watch rf-db/app-db ::writes)
        (ratom/dispose! beat)
        (ratom/dispose! transport))
      (finally (reset! rf-db/app-db before)))))

(deftest a-show-keeps-its-paths-when-views-restore-the-clock
  (is (= {:frame-number 7 :time-ms 350 :transport {:bpm 120 :beat 1.5 :time-ms 340} :schedule {:beat 1.5}
          :status {:error nil :frame-ms 2.5 :output-scheduler {:published 3}}}
         (clock/with-clock {:transport {:bpm 120} :status {:error nil}}
                           {:frame-number 7 :time-ms 350 :beat 1.5 :transport-time-ms 340 :schedule-beat 1.5
                            :frame-ms 2.5 :output-scheduler {:published 3}})))
  (is (= {:transport {:bpm 120}} (clock/with-clock {:transport {:bpm 120}} nil))))

(deftest beat-paced-subscriptions-tick-on-the-clock-lane
  (let [before @rf-db/app-db ticks (atom 0)]
    (try
      (reset! rf-db/app-db {:server-projection {:view {:runtime-id "r" :epoch 1 :transport {:meter {:beats-per-bar 4}}}}})
      (reactions/reg-sub! ::per-beat {:period {:beats 1}} (fn [_ _] (swap! ticks inc)))
      (reactions/reg-sub! ::per-bar {:period {:bars 1}} (fn [_ _] :bar))
      (publish! :clock "a" {:beat 0.2 :frame-number 1})
      (let [per-beat (watch [::per-beat]) per-bar (watch [::per-bar])
            start @ticks]
        (doseq [[token beat] [["b" 0.6] ["c" 1.1] ["d" 1.4] ["e" 2.05]]]
          (publish! :clock token {:beat beat :frame-number 1})
          (ratom/flush!))
        (is (= 2 (- @ticks start)) "one recompute per beat boundary, none per frame")
        (ratom/dispose! per-beat)
        (ratom/dispose! per-bar)
        (let [n @ticks]
          (publish! :clock "f" {:beat 3.2})
          (ratom/flush!)
          (is (= n @ticks) "a released subscription stops following the clock")))
      (finally (reset! rf-db/app-db before)))))

(deftest remote-results-reach-only-their-own-readers-from-the-lane
  (let [before @rf-db/app-db]
    (try
      (reset! rf-db/app-db {})
      (let [timeline (watch [:studio/remote-result [:timeline/view {:bars 8}]])
            rdm (watch [:studio/remote-result [:rdm/status]])
            rdm-changes (atom 0)]
        (add-watch rdm ::count (fn [_ _ _ _] (swap! rdm-changes inc)))
        (publish! :remote "a" {:results {[:timeline/view {:bars 8}] {:value {:beat 1.0}}
                                         [:rdm/status] {:value {:connected? false}}}})
        (ratom/flush!)
        (is (= {:value {:beat 1.0}} @timeline))
        (let [status (get-in (frame-stream/baseline :remote) [:value :results [:rdm/status]])]
          (publish! :remote "b" {:results {[:timeline/view {:bars 8}] {:value {:beat 1.1}}
                                           [:rdm/status] status}}))
        (ratom/flush!)
        (is (= {:value {:beat 1.1}} @timeline))
        (is (= 1 @rdm-changes) "an unchanged result does not notify its reader")
        (ratom/dispose! timeline)
        (ratom/dispose! rdm))
      (finally (reset! rf-db/app-db before)))))

(deftest a-cue-cell-receives-only-its-own-pad-preview
  (let [page {:cells [{:x 0 :y 0 :cue-id :breathe} {:x 1 :y 0 :cue-id "flash"}]}
        colors {:breathe [1 0 0] "flash" [0 0 1] :other [0 1 0]}]
    (is (= {:breathe [1 0 0]} (pad-preview/cell-colors colors page 0 0)))
    (is (= {"flash" [0 0 1]} (pad-preview/cell-colors colors page 1 0)))
    (is (= {"breathe" [1 0 0]} (pad-preview/cell-colors {"breathe" [1 0 0]} page 0 0)) "text keys match typed cells")
    (is (nil? (pad-preview/cell-colors colors page 2 0)))
    (is (= (pad-preview/cell-colors colors page 1 0)
           (pad-preview/cell-colors (assoc colors :breathe [0.5 0 0]) page 1 0))
        "another pad's animation leaves this cell's props equal")))
