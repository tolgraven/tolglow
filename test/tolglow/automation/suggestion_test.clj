(ns tolglow.automation.suggestion-test
  "Stale or late agent suggestions are refused (#670). The real show owner with
  captured output, ticked on an injected clock; agent calls go through
  show/dispatch-tool! and, for the provider path, agent/run! with a recorded
  transport that never opens a connection. Checks read committed receipts,
  auto state and the schedule."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.automation.red-show :as red-show]
            [tolglow.automation.suggestion :as suggestion]
            [tolglow.integrations.agent :as agent]
            [tolglow.rehearsal :as rehearsal]
            [tolglow.show :as show]))

(def frame-ms 125.0)

(defn- mood [cue-id]
  {:enabled? true :seed 7 :interval {:beats 1} :count 1 :scope {:cue-ids [cue-id]}
   :launch {:mode :one-shot :duration {:beats 1}}})

(defn- runtime []
  (doto (show/create-runtime {:model (red-show/model) :sink {:send! (fn [_ _])} :workspace-options nil})
    (show/tick! 0.0)))

(defn- submit! [rt t type payload]
  (let [reply (show/submit! rt type payload)]
    (show/tick! rt t)
    (deref reply 2000 ::missing)))

(defn- play!
  "Tick every frame from t to end; returns the next host time."
  [rt t end]
  (loop [t t] (if (> t end) t (do (show/tick! rt t) (recur (+ t frame-ms))))))

(defn- beat [rt] (get-in @(:state rt) [:model :transport :beat]))

(defn- session [rt] (suggestion/auto-session (:model @(:state rt))))

(defn- scope [rt] (get-in @(:state rt) [:model :auto :config :scope]))

(defn- transport! [rt t playing?]
  (submit! rt t :command {:op :transport/set :source {:adapter :repl :session-id "test"} :transport {:playing? playing?}}))

(defn- dispatch!
  "Call an agent tool as a provider run would, ticking the owner until the
  receipt arrives."
  [rt t tool arguments run-id]
  (let [identity (select-keys (:model @(:state rt)) [:epoch :show-id])
        result (future (show/dispatch-tool! rt tool arguments
                                            (merge identity {:run-id run-id :command-id (str run-id "/" tool)
                                                             :expected-epoch (:epoch identity)})))]
    (loop [n 0]
      (show/tick! rt t)
      (if (or (realized? result) (> n 5000)) (deref result 2000 ::missing)
          (do (Thread/sleep 1) (recur (inc n)))))))

(deftest a-suggestion-is-well-formed-or-refused
  (is (= [] (suggestion/validate {:basis {:auto-session "off" :revision 3} :until {:bar 4 :beat 2}})))
  (is (seq (suggestion/validate {})) "it needs a basis or a window")
  (is (seq (suggestion/validate {:basis {:session "x"}})))
  (is (seq (suggestion/validate {:until {:bar -1}})))
  (is (seq (suggestion/validate {:basis {:revision 1.5}})))
  (is (nil? (suggestion/refusal {:transport {:beat 3.0}} nil)) "no suggestion, nothing to refuse")
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid suggestion"
                        (suggestion/refusal {:transport {:beat 0.0}} {:until {:bars 2}}))))

(deftest a-change-based-on-a-session-that-has-ended-is-refused
  (let [rt (runtime)]
    (try
      (is (= :accepted (:status (submit! rt 0.0 :auto (mood :wash-color)))))
      (let [seen (get-in (show/snapshot rt) [:auto :session])
            t (play! rt frame-ms 1000.0)]
        (is (= seen (session rt)) "the projection names the playing session")
        (testing "the session it read is still playing: accepted"
          (is (= :accepted (:status (submit! rt t :auto (assoc (mood :accent) :suggestion {:basis {:auto-session seen}}))))))
        (testing "the operator restarted auto meanwhile: refused and nothing changes"
          (let [before (session rt)
                ack (submit! rt t :auto (assoc (mood :sweep) :suggestion {:basis {:auto-session seen}}))]
            (is (= {:status :rejected :code :stale-suggestion :reason :basis-changed}
                   (select-keys ack [:status :code :reason])))
            (is (re-find #"Stale suggestion: it was based on auto session" (:message ack)))
            (is (= before (session rt)))
            (is (= {:cue-ids [:accent]} (scope rt)))))
        (testing "a suggestion made while auto was off is stale once auto runs"
          (is (= :stale-suggestion (:code (submit! rt t :auto (assoc (mood :sweep) :suggestion {:basis {:auto-session "off"}})))))))
      (finally (show/stop! rt)))))

(deftest a-reply-arriving-after-its-window-is-refused-and-a-stop-holds-the-window
  (let [rt (runtime)]
    (try
      (submit! rt 0.0 :auto (mood :wash-color))
      (let [t (play! rt frame-ms 1400.0)
            _ (is (< 2.0 (beat rt) 4.0))
            _ (transport! rt t false)
            held (beat rt)
            ;; Ten seconds stopped: the beat, and so the window, holds.
            t (play! rt (+ t frame-ms) (+ t 10000.0))]
        (is (== held (beat rt)))
        (is (= :accepted (:status (submit! rt t :auto (assoc (mood :accent) :suggestion {:until {:bar 1}})))))
        (transport! rt t true)
        (let [t (play! rt (+ t frame-ms) (+ t 1500.0))
              _ (is (> (beat rt) 4.0))
              ack (submit! rt t :auto (assoc (mood :sweep) :suggestion {:until {:bar 1}}))]
          (is (= {:status :rejected :code :stale-suggestion :reason :window-passed :until-beat 4.0}
                 (select-keys ack [:status :code :reason :until-beat])))
          (is (= {:cue-ids [:accent]} (scope rt)) "the stale reply changed nothing")))
      (finally (show/stop! rt)))))

(deftest a-change-on-a-line-must-fall-in-its-window-and-is-checked-again-when-due
  (let [rt (runtime)]
    (try
      (submit! rt 0.0 :auto (mood :wash-color))
      (let [t (play! rt frame-ms 600.0)
            seen (session rt)]
        (testing "a line at or after the window's end is refused when posted"
          (let [ack (submit! rt t :auto (assoc (mood :accent) :at {:bar 2} :suggestion {:until {:bar 2}}))]
            (is (= [:rejected :stale-suggestion :outside-window] ((juxt :status :code :reason) ack)))
            (is (empty? (get-in (show/snapshot rt) [:auto :pending])))))
        (testing "inside its window it waits on the line"
          (let [ack (submit! rt t :auto (assoc (mood :accent) :at {:bar 2} :suggestion {:basis {:auto-session seen} :until {:bar 3}}))]
            (is (= {:status :accepted :scheduled? true :due-beats [8.0]} (select-keys ack [:status :scheduled? :due-beats])))
            (testing "an operator restarts auto before the line: the change is refused when due"
              (let [t (play! rt (+ t frame-ms) 2500.0)
                    _ (submit! rt t :auto (mood :sweep))
                    operator (session rt)
                    _ (play! rt (+ t frame-ms) 5000.0)
                    outcome (some #(when (= (first (:event-ids ack)) (:event-id %)) %) (:recent (show/schedule rt)))]
                (is (> (beat rt) 9.0))
                (is (= :rejected (:status outcome)))
                (is (re-find #"Stale suggestion: it was based on auto session" (:message outcome)))
                (is (= operator (session rt)) "the operator's session keeps playing")
                (is (= {:cue-ids [:sweep]} (scope rt)))
                (is (empty? (get-in (show/snapshot rt) [:auto :pending])) "nothing is left waiting")))))
        (testing "without a change in between the same suggestion applies on its line"
          (let [now (session rt)
                t (play! rt 5000.0 5000.0)
                line (* 4 (Math/ceil (/ (+ (beat rt) 1.0) 4)))
                ack (submit! rt t :auto (assoc (mood :accent) :at {:beat line} :suggestion {:basis {:auto-session now} :until {:beat (+ line 4)}}))
                _ (play! rt (+ t frame-ms) (+ t 3000.0))
                outcome (some #(when (= (first (:event-ids ack)) (:event-id %)) %) (:recent (show/schedule rt)))]
            (is (= :fired (:status outcome)) (pr-str outcome))
            (is (= {:cue-ids [:accent]} (scope rt))))))
      (finally (show/stop! rt)))))

(deftest a-strict-revision-basis-refuses-after-any-accepted-change
  (let [rt (runtime)]
    (try
      (let [revision (:revision (show/snapshot rt))]
        (is (= :accepted (:status (submit! rt 0.0 :auto (assoc (mood :accent) :suggestion {:basis {:revision revision}}))))))
      (let [revision (:revision (show/snapshot rt))]
        (submit! rt 0.0 :command {:op :cue/launch :cue-id :spot :source {:adapter :repl :session-id "test"}})
        (let [ack (submit! rt 0.0 :auto (assoc (mood :sweep) :suggestion {:basis {:revision revision}}))]
          (is (= [:rejected :stale-suggestion] ((juxt :status :code) ack)))
          (is (re-find #"show revision" (:message ack)))))
      (finally (show/stop! rt)))))

(deftest agent-tools-carry-a-suggestion-and-read-the-session-back
  (let [rt (runtime)]
    (try
      (submit! rt 0.0 :auto (mood :wash-color))
      (let [t (play! rt frame-ms 600.0)
            seen (get-in (dispatch! rt t "inspect_show" {} "run-1") [:auto :session])]
        (is (= (session rt) seen) "inspect_show exposes the session an agent names")
        (testing "schedule_events with a stale window posts nothing"
          (let [ack (dispatch! rt t "schedule_events"
                               {:events [{:op "cue/launch" :cue-id "spot" :at {:bar 3}}] :suggestion {:until {:beat 0.5}}} "run-1")]
            (is (= [:rejected :stale-suggestion :window-passed] ((juxt :status :code :reason) ack)))
            (is (empty? (:upcoming (show/schedule rt))))))
        (testing "with a current basis it posts as before"
          (is (= :accepted (:status (dispatch! rt t "schedule_events"
                                               {:events [{:op "cue/launch" :cue-id "spot" :at {:bar 3}}]
                                                :suggestion {:basis {:auto-session seen}}} "run-2")))))
        (testing "configure_auto from a provider round: the stale call is refused, the run completes"
          (submit! rt t :auto (mood :sweep))
          (let [events (atom [])
                round (rehearsal/recorded-round "Accent next." [{:id "auto" :name "configure_auto"
                                                                  :arguments {:configuration (mood "accent")
                                                                              :suggestion {:basis {:auto-session seen}}}}])
                result (future (agent/run! rehearsal/provider-config nil [{:role :user :content "Go"}] agent/default-tools
                                           (fn [tool arguments metadata]
                                             (show/dispatch-tool! rt tool arguments
                                                                  (merge metadata (select-keys (:model @(:state rt)) [:epoch :show-id]))))
                                           #(swap! events conj %)
                                           {:transport (rehearsal/recorded-transport [round (rehearsal/recorded-round "Done." [])])
                                            :run-id "run-3"}))]
            (loop [n 0] (show/tick! rt t) (when (and (not (realized? result)) (< n 5000)) (Thread/sleep 1) (recur (inc n))))
            (is (= :completed (:status @result)))
            (is (= {:status :rejected :code :stale-suggestion}
                   (select-keys (some #(when (= :tool/result (:type %)) (:result %)) @events) [:status :code])))
            (is (= {:cue-ids [:sweep]} (scope rt)))))
        (testing "a malformed suggestion is refused before it reaches the show"
          (let [errors (agent/validate-arguments (:parameters (some #(when (= "configure_auto" (:name %)) %) agent/default-tools))
                                                 {:configuration {} :suggestion {:basis {:session "x"}}})]
            (is (= [:suggestion :basis :session] (:path (first errors)))))))
      (finally (show/stop! rt)))))
