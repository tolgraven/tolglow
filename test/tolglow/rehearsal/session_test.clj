(ns tolglow.rehearsal.session-test
  "Arm, start, stop and review a Room rehearsal on a running show (#670),
  through the functions Studio's /api/rehearsal routes call. A real started
  show with a discarding sink; nothing is armed for physical output."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.automation.red-show :as red-show]
            [tolglow.rehearsal.session :as session]
            [tolglow.show :as show]))

(defn- wait-for [pred ms]
  (loop [n 0] (cond (pred) true (> n ms) false :else (do (Thread/sleep 1) (recur (inc n))))))

(defn- started []
  (show/start! {:model (red-show/model) :sink {:send! (fn [_ _])} :workspace-options nil :recovery false}))

(deftest arming-replaces-the-show-only-when-confirmed-and-a-run-can-be-stopped-and-reviewed
  (let [rt (started)
        checkpoint (str (java.io.File/createTempFile "before-rehearsal" ".transit"))]
    (try
      (testing "arming would replace the open show: it needs confirming"
        (let [ack (session/action! rt :arm {:mode :club :seed 670})]
          (is (= {:status :rejected :code :replace-show-required} (select-keys ack [:status :code])))
          (is (= 6 (count (get-in @(:state rt) [:model :registry :cues]))) "the open show is untouched")))
      (testing "a page bound to an earlier show session is refused"
        (is (= :stale-show-session (:code (session/action! rt :arm {:mode :club :replace-show? true :expected-epoch 99})))))
      (let [armed (session/action! rt :arm {:mode :club :seed 670 :replace-show? true :wait-ms 20000
                                            :checkpoint-path checkpoint
                                            :runtime-id (:id rt) :expected-epoch (get-in @(:state rt) [:model :epoch])})]
        (is (= :armed (:status armed)))
        (is (:loaded? armed))
        (is (= {:mode :club :take nil :seed 670} (:choice armed)))
        (is (= [:groove :breakdown :return :quiet] (mapv :section (:moods armed))))
        (is (= [:spot :sweep] (:scope (last (:moods armed)))) "the quiet mood the agent will set")
        (is (false? (:playing? armed)) "armed, not playing")
        (is (== 0.0 (:beat armed)))
        (is (= [:groove] (mapv :section (get-in armed [:progress :agent]))) "the agent's first round is taken")
        (is (= 10 (count (get-in @(:state rt) [:model :registry :cues]))) "the rehearsal show is loaded")
        (is (= checkpoint (get-in armed [:before-rehearsal :path])) "the replaced show was saved first")
        (is (= :gig-2023 (get-in armed [:before-rehearsal :show-id]))))
      (testing "re-arming this session's own show needs no confirmation"
        (is (= :armed (:status (session/action! rt :arm {:mode :concert :take :b :seed 670 :wait-ms 20000}))))
        (is (= {:mode :concert :take :b :seed 670} (:choice (session/status rt)))))
      (is (= :rejected (:status (session/action! rt :stop {}))) "nothing is playing yet")
      (let [playing (session/action! rt :start {})]
        (is (= :playing (:status playing)))
        (is (wait-for #(> (:beat (session/status rt)) 2.0) 5000) "the transport runs")
        (let [stopped (session/action! rt :stop {})
              beat (:beat stopped)]
          (is (= :stopped (:status stopped)))
          (is (true? (get-in stopped [:run :stopped?])))
          (is (false? (:playing? stopped)) "stopping the rehearsal stops the transport")
          (Thread/sleep 200)
          (is (== beat (:beat (session/status rt))) "and the beat holds")
          (is (= {"schedule_events" :accepted "configure_auto" :accepted}
                 (update-vals (get-in stopped [:run :agent 0 :receipts]) :status)))
          (is (= 0 (get-in stopped [:run :held :agent-runs-open-at-end])))))
      (testing "the review is the seeded replay of the armed concert, both takes"
        (is (wait-for #(= :ready (:review-status (session/status rt))) 60000))
        (let [{:keys [review]} (session/status rt)]
          (is (= :concert (:id review)))
          (is (= [:a :b] (mapv :take (:takes review))))
          (is (every? #(= 4 (count (:responses %))) (:takes review)))
          (is (every? :kept? (get-in review [:comparison :anchors])))
          (is (re-find #"# Concert rehearsal" (:report review)))))
      (testing "a review can be read without playing, for another choice"
        (session/action! rt :review {:mode :club :seed 671 :agent {:outage {:from-section 2 :mode :refused}}})
        (is (wait-for #(and (= :ready (:review-status (session/status rt)))
                            (= :club (get-in (session/status rt) [:review :id]))) 60000))
        (let [{:keys [review review-choice]} (session/status rt)]
          (is (= {:mode :club :take nil :seed 671} review-choice))
          (is (= [:applied :applied :agent-failed :agent-failed] (mapv :outcome (:responses review))))
          (is (empty? (get-in review [:continuity :stalls])))
          (is (= 12 (count (:phrases review))))))
      (is (= :rejected (:status (session/action! rt :arm {:mode :opera}))) "refusals read the same from Studio")
      (testing "the replaced show comes back from its checkpoint"
        (let [restored (session/action! rt :restore {})]
          (is (= :idle (:status restored)))
          (is (false? (:loaded? restored)))
          (is (= 6 (count (get-in @(:state rt) [:model :registry :cues]))) "the red show is back")))
      (finally (show/stop! rt) (.delete (java.io.File. checkpoint))))))

(defn- silent-threads []
  (count (filter (fn [^Thread t] (some #(re-find #"silent_transport" (str %)) (.getStackTrace t)))
                 (keys (Thread/getAllStackTraces)))))

(deftest a-silent-agent-at-arm-times-out-without-holding-the-request-or-outliving-the-show
  ;; Review of #823: the first round ignored the timeout, held the lock and
  ;; the HTTP thread for 15 s and left a spinning thread after the show stopped.
  (let [rt (started)
        checkpoint (str (java.io.File/createTempFile "before-rehearsal" ".transit"))]
    (try
      (let [started-at (System/nanoTime)
            ack (session/action! rt :arm {:mode :club :seed 670 :replace-show? true :checkpoint-path checkpoint
                                          :agent {:outage {:from-section 0 :mode :silent} :timeout-ms 300}})
            returned-ms (/ (- (System/nanoTime) started-at) 1e6)]
        (is (= :arming (:status ack)))
        (is (< returned-ms 2000) "the request returns while the agent is asked")
        (is (wait-for #(= :armed (:status (session/status rt))) 20000))
        (let [first-round (first (get-in (session/status rt) [:progress :agent]))]
          (is (true? (:timed-out? first-round)) "logged as timed out")
          (is (empty? (:receipts first-round))))
        (is (wait-for #(zero? (silent-threads)) 2000) "the timed-out run's thread ended"))
      (testing "a silent run still hanging ends when the show stops"
        (session/action! rt :arm {:mode :club :seed 670
                                  :agent {:outage {:from-section 0 :mode :silent} :timeout-ms 600000}})
        (is (wait-for #(pos? (silent-threads)) 5000) "the first round hangs")
        (is (= :arming (:status (session/status rt))))
        (show/stop! rt)
        (is (wait-for #(zero? (silent-threads)) 3000) "closing the session cancelled it"))
      (finally (show/stop! rt) (.delete (java.io.File. checkpoint))))))

(deftest stop-after-the-passage-ended-returns-its-status
  ;; Review of #823: a Stop racing the end of the passage left the session in :stopping.
  (let [rt (started)]
    (try
      (let [state (:state (#'session/session rt))]
        (swap! state assoc :status :finished)
        (is (= :finished (:status (session/action! rt :stop {}))) "not refused, not stuck")
        (is (= :finished (:status @state)))
        (swap! state assoc :status :idle)
        (is (= :rejected (:status (session/action! rt :stop {})))))
      (finally (show/stop! rt)))))

(deftest a-new-review-supersedes-the-one-still-running
  (let [rt (started)]
    (try
      (session/action! rt :review {:mode :concert :seed 670})
      (let [second-call (session/action! rt :review {:mode :club :seed 671})]
        (is (= 2 (:review-id second-call))))
      (is (wait-for #(= :ready (:review-status (session/status rt))) 60000))
      (let [{:keys [review review-id review-choice]} (session/status rt)]
        (is (= 2 review-id (:review-id review)))
        (is (= {:mode :club :take nil :seed 671} review-choice) "the superseded concert replay never shows"))
      (finally (show/stop! rt)))))
