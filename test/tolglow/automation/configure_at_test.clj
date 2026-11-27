(ns tolglow.automation.configure-at-test
  "Auto configure on a musical line (#767): a request with :at waits on the
  beat scheduler and the show owner applies it on the first frame at or past
  its line, before the planner decides on that frame. Real show owner,
  captured output only; the checks read committed instances, receipts and the
  schedule."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.automation.red-show :as red-show]
            [tolglow.clock :as clock]
            [tolglow.event-subscribers :as subscribers]
            [tolglow.show :as show]))

(def frame-ms 130.0)

(def ^:private frame-beats (/ (* frame-ms 120.0) 60000.0))

(defn- mood [cue-id]
  {:enabled? true :seed 7 :interval {:beats 1} :count 1 :scope {:cue-ids [cue-id]}
   :launch {:mode :one-shot :duration {:beats 1}}})

(defn- runtime []
  (doto (show/create-runtime {:model (red-show/model) :sink {:send! (fn [_ _])} :workspace-options nil})
    (show/tick! 0.0)))

(defn- submit!
  "Submit at host time t and let the owner apply it on a tick at t."
  [rt t type payload]
  (let [reply (show/submit! rt type payload)]
    (show/tick! rt t)
    (deref reply 2000 ::missing)))

(defn- beat [rt] (get-in @(:state rt) [:model :transport :beat]))

(defn- play!
  "Tick from t to end every frame; return [t launches], launches being
  {:cue-id :started-at-beat :seen-beat} for every auto instance first seen."
  [rt t end seen]
  (loop [t t launches seen]
    (if (> t end) [t launches]
      (do (show/tick! rt t)
          (let [model (:model @(:state rt))
                fresh (for [[id i] (:instances model)
                            :when (and (= :auto (get-in i [:source :adapter])) (not (contains? launches id)))]
                        [id {:cue-id (:cue-id i) :started-at-beat (:started-at-beat i) :seen-beat (beat rt)}])]
            (recur (+ t frame-ms) (into launches fresh)))))))

(defn- recent [rt] (:recent (show/schedule rt)))

(deftest a-change-with-at-lands-on-its-line-with-neither-carry-over-nor-pre-launch
  (let [rt (runtime)]
    (try
      (is (= :accepted (:status (submit! rt 0.0 :auto (mood :wash-color)))))
      (let [[t launches] (play! rt frame-ms 1000.0 {})
            ack (submit! rt t :auto (assoc (mood :accent) :at {:bar 2}))
            _ (is (= {:status :accepted :scheduled? true :due-beats [8.0]} (select-keys ack [:status :scheduled? :due-beats])))
            _ (is (= [{:event-id (first (:event-ids ack)) :due-beat 8.0 :enabled? true :label "Auto configure"}]
                     (mapv #(dissoc % :source) (get-in (show/snapshot rt) [:auto :pending])))
                  "Studio's projection lists the pending change")
            _ (is (= {:cue-ids [:wash-color]} (get-in @(:state rt) [:model :auto :config :scope])) "nothing changes before the line")
            [_ launches] (play! rt (+ t frame-ms) 6000.0 launches)
            fired (some #(when (= (first (:event-ids ack)) (:event-id %)) %) (recent rt))
            line-frame (:at-beat fired)
            by-cue (group-by :cue-id (vals launches))]
        (is (= :fired (:status fired)))
        (is (<= 8.0 line-frame (+ 8.0 frame-beats)) "the first frame at or past the line")
        (is (seq (by-cue :wash-color)))
        (is (every? #(< (:started-at-beat %) 8.0) (by-cue :wash-color)) "no carry-over: the previous mood never decides on the line")
        (is (every? #(>= (:started-at-beat %) 8.0) (by-cue :accent)) "no pre-launch")
        (is (== line-frame (apply min (map :started-at-beat (by-cue :accent))))
            "the new mood decides on the line's own frame")
        (is (= 1 (count (filter #(== line-frame (:started-at-beat %)) (by-cue :accent))))
            "once: the change's decision is the frame's decision, not planned again")
        (is (re-matches #"sched/0/\d+/0" (get-in @(:state rt) [:model :auto :state :owner-id]))
            "the step's command ID names the new session, so a replay is deterministic"))
      (finally (show/stop! rt)))))

(deftest a-pending-change-holds-across-a-stop-and-fires-on-its-beat-after-resume
  (let [rt (runtime)
        transport (fn [t playing?] (submit! rt t :command {:op :transport/set :source {:adapter :repl :session-id "test"}
                                                          :transport {:playing? playing?}}))]
    (try
      (submit! rt 0.0 :auto (mood :wash-color))
      (let [[t launches] (play! rt frame-ms 1000.0 {})
            ack (submit! rt t :auto (assoc (mood :accent) :at {:bar 2}))
            [t launches] (play! rt (+ t frame-ms) 3000.0 launches)
            _ (transport t false)
            stopped-at (beat rt)
            tick-before (get-in @(:state rt) [:model :auto :state :tick])
            [t launches] (play! rt (+ t frame-ms) 9000.0 launches)]
        (is (< stopped-at 8.0))
        (is (== stopped-at (beat rt)) "the beat held")
        (is (= tick-before (get-in @(:state rt) [:model :auto :state :tick])) "no decision while stopped")
        (is (= {:cue-ids [:wash-color]} (get-in @(:state rt) [:model :auto :config :scope])) "six seconds stopped did not fire it")
        (is (= [8.0] (mapv :due-beat (get-in (show/snapshot rt) [:auto :pending]))))
        (transport t true)
        (let [[_ launches] (play! rt (+ t frame-ms) 13000.0 launches)
              fired (filter #(= (first (:event-ids ack)) (:event-id %)) (recent rt))
              accents (filter #(= :accent (:cue-id %)) (vals launches))]
          (is (= [:fired] (mapv :status fired)) "fired once")
          (is (<= 8.0 (:at-beat (first fired)) (+ 8.0 frame-beats)) "on its beat, not caught up when play resumed")
          (is (every? #(>= (:started-at-beat %) 8.0) accents))
          (is (seq accents))))
      (finally (show/stop! rt)))))

(deftest at-takes-a-position-or-the-next-bar-phrase-or-section-line
  (let [rt (runtime)]
    (try
      (submit! rt 0.0 :auto (mood :wash-color))
      (let [[t] (play! rt frame-ms 1200.0 {})]
        (is (< 2.0 (beat rt) 4.0))
        (is (= [4.0] (:due-beats (submit! rt t :auto (assoc (mood :accent) :at :next-bar)))))
        (is (= [16.0] (:due-beats (submit! rt t :auto (assoc (mood :accent) :at "next-phrase")))) "JSON sends the words as text")
        (is (= [6.0] (:due-beats (submit! rt t :auto (assoc (mood :accent) :at {:bar 1 :beat 2})))))
        (testing "refused when posted, leaving nothing scheduled"
          (let [pending (count (get-in (show/snapshot rt) [:auto :pending]))]
            (is (re-find #"No auto section is playing" (str (:message (submit! rt t :auto (assoc (mood :accent) :at :next-section))))))
            (is (re-find #"Count must be an integer" (str (:message (submit! rt t :auto (assoc (mood :accent) :count 99 :at {:bar 4}))))))
            (is (re-find #"No saved cue" (str (:message (submit! rt t :auto (assoc (mood :missing) :at {:bar 4}))))))
            (is (re-find #"Auto :at is" (str (:message (submit! rt t :auto (assoc (mood :accent) :at :soon))))))
            (is (= pending (count (get-in (show/snapshot rt) [:auto :pending])))))))
      (testing "the next section line is where the playing plan section ends"
        (submit! rt 1300.0 :auto (red-show/auto-config 217))
        (show/tick! rt 1430.0)
        (let [end (get-in (show/snapshot rt) [:auto :section-end-beat])]
          (is (number? end))
          (is (= [end] (:due-beats (submit! rt 1430.0 :auto (assoc (mood :accent) :at :next-section)))))))
      (finally (show/stop! rt)))))

(deftest stopping-auto-now-withdraws-a-change-waiting-on-its-line
  (let [rt (runtime)]
    (try
      (submit! rt 0.0 :auto (mood :wash-color))
      (let [ack (submit! rt 0.0 :auto (assoc (mood :accent) :at {:bar 2}))
            stop (submit! rt 0.0 :auto {:enabled? false})]
        (is (= (:event-ids ack) (:withdrawn-event-ids stop)))
        (is (empty? (get-in (show/snapshot rt) [:auto :pending])))
        (is (= {:status :cancelled :reason :auto-stopped}
               (select-keys (some #(when (= (first (:event-ids ack)) (:event-id %)) %) (recent rt)) [:status :reason])))
        (play! rt frame-ms 6000.0 {})
        (is (false? (get-in @(:state rt) [:model :auto :enabled?])) "the line passes and auto stays off"))
      (testing "a stop scheduled on a line is itself a pending change"
        (let [ack (submit! rt 6100.0 :auto {:enabled? true :seed 1 :interval {:beats 1} :scope {:cue-ids [:accent]}})
              stop (submit! rt 6100.0 :auto {:enabled? false :at :next-bar})]
          (is (= :accepted (:status ack)))
          (is (= "Stop auto" (:label (first (get-in (show/snapshot rt) [:auto :pending])))))
          (play! rt 6230.0 9000.0 {})
          (is (false? (get-in @(:state rt) [:model :auto :enabled?])))
          (is (= :fired (:status (some #(when (= (first (:event-ids stop)) (:event-id %)) %) (recent rt)))))))
      (finally (show/stop! rt)))))

(defn- dispatch!
  "Call an agent tool as a provider run would, ticking the headless owner
  until the receipt arrives."
  [rt t tool arguments run-id call-id & [conversation-id]]
  (let [identity (select-keys (:model @(:state rt)) [:epoch :show-id])
        result (future (show/dispatch-tool! rt tool arguments
                                            (cond-> (merge identity {:run-id run-id :command-id (str run-id "/" call-id)
                                                                     :expected-epoch (:epoch identity)})
                                              conversation-id (assoc :conversation-id conversation-id))))]
    (loop [n 0]
      (show/tick! rt t)
      (if (or (realized? result) (> n 5000)) (deref result 2000 ::missing)
          (do (Thread/sleep 1) (recur (inc n)))))))

(deftest the-agent-tool-puts-a-change-on-a-line-and-withdraws-its-own
  (let [rt (runtime)]
    (try
      ;; As a provider's JSON arrives: keyword keys, textual values.
      (let [configuration {:enabled? true :seed 3 :interval {:beats 1} :count 1 :scope {:cue-ids ["accent"]}
                           :launch {:mode "one-shot" :duration {:beats 1}}}
            ack (dispatch! rt 0.0 "configure_auto" {:configuration configuration :at {:bar 4}} "run-1" "auto")]
        (is (= {:status :accepted :event-ids ["run-1/auto/0"] :due-beats [16.0]} (select-keys ack [:status :event-ids :due-beats])))
        (is (= {:adapter :agent :session-id "run-1"} (get-in (show/snapshot rt) [:auto :pending 0 :source])))
        (is (= [[16.0 "run-1/auto/0" :auto/configure "Auto configure"]]
               (:upcoming (dispatch! rt 0.0 "inspect_schedule" {} "run-1" "look")))
            "inspect_schedule lists it for the agent")
        (is (= ["run-1/auto/0"] (:event-ids (dispatch! rt 0.0 "cancel_scheduled" {:mine? true} "run-1" "undo"))))
        (is (empty? (get-in (show/snapshot rt) [:auto :pending]))))
      (testing "an agent's immediate stop withdraws only its own pending changes"
        (let [human (submit! rt 0.0 :auto (assoc (mood :accent) :at {:bar 8}))
              own (dispatch! rt 0.0 "configure_auto" {:configuration {:enabled? true :scope {:cue-ids ["accent"]}} :at {:bar 6}}
                             "run-5" "auto")
              stop (dispatch! rt 0.0 "configure_auto" {:configuration {:enabled? false}} "run-5" "stop")]
          (is (= (:event-ids own) (:withdrawn-event-ids stop)))
          (is (= (:event-ids human) (:pending-event-ids stop)) "the receipt says what still waits")
          (is (= (:event-ids human) (mapv :event-id (get-in (show/snapshot rt) [:auto :pending]))) "the REPL's change still waits")
          (submit! rt 0.0 :command {:op :schedule/cancel :event-ids (:event-ids human) :source {:adapter :repl :session-id "t"}})))
      (testing "one conversation owns what it posted in earlier runs (turns)"
        (let [posted (dispatch! rt 0.0 "configure_auto" {:configuration {:enabled? true :scope {:cue-ids ["accent"]}} :at {:bar 6}}
                                "turn-1" "auto" "conv-1")
              _ (is (= {:adapter :agent :session-id "conv-1" :run-id "turn-1"}
                       (get-in (show/snapshot rt) [:auto :pending 0 :source])))
              cancelled (dispatch! rt 0.0 "cancel_scheduled" {:mine? true} "turn-2" "undo" "conv-1")
              again (dispatch! rt 0.0 "configure_auto" {:configuration {:enabled? true :scope {:cue-ids ["accent"]}} :at {:bar 6}}
                               "turn-3" "auto" "conv-1")
              other (dispatch! rt 0.0 "configure_auto" {:configuration {:enabled? false}} "turn-9" "stop" "conv-2")
              stop (dispatch! rt 0.0 "configure_auto" {:configuration {:enabled? false}} "turn-4" "stop" "conv-1")]
          (is (= (:event-ids posted) (:event-ids cancelled)) "cancel_scheduled mine? in a later turn")
          (is (= (:event-ids again) (:pending-event-ids other)) "another conversation cannot withdraw it, and is told it waits")
          (is (= (:event-ids again) (:withdrawn-event-ids stop)) "a stop in a later turn withdraws it")
          (is (empty? (get-in (show/snapshot rt) [:auto :pending])))))
      (testing "a change that arrives after its line: within a bar it applies now, marked late; later it is refused"
        (is (re-find #"nonnegative"
                     (str (:message (dispatch! rt 0.0 "configure_auto" {:configuration {:enabled? true} :at {:beat -1}} "run-2" "auto")))))
        (play! rt frame-ms 600.0 {})
        (let [late (dispatch! rt 600.0 "configure_auto" {:configuration {:enabled? true :scope {:cue-ids ["accent"]}} :at {:beat 0}}
                              "run-3" "auto")
              outcome (some #(when (= (first (:event-ids late)) (:event-id %)) %) (recent rt))]
          (is (= {:status :accepted :late? true} (select-keys late [:status :late?])))
          (is (< 1.0 (:late-beats late) 1.5))
          (is (= {:status :fired :late? true :late-beats (:late-beats late)} (select-keys outcome [:status :late? :late-beats])))
          (is (== (beat rt) (:at-beat outcome)) "applied on arrival, before auto decided on that frame")
          (is (= {:cue-ids [:accent]} (get-in @(:state rt) [:model :auto :config :scope]))))
        (play! rt 730.0 2600.0 {})
        (is (re-find #"already passed by \d"
                     (str (:message (dispatch! rt 2600.0 "configure_auto" {:configuration {:enabled? true} :at {:beat 0}} "run-6" "auto"))))
            "a bar or more late"))
      (testing "schedule_events meets the same late policy, and only the show marks a change late"
        (let [posted (dispatch! rt 2600.0 "schedule_events"
                                {:events [{:op "auto/configure" :request {:enabled? true :scope {:cue-ids ["accent"]}}
                                           :at {:beat 4}}]} "run-7" "post")]
          (is (< 1.0 (beat rt) 6.0))
          (is (= :accepted (:status posted)))
          (is (= {:status :fired :late? true} (select-keys (some #(when (= (first (:event-ids posted)) (:event-id %)) %) (recent rt))
                                                           [:status :late?]))))
        (is (re-find #"already passed by \d"
                     (str (:message (dispatch! rt 2600.0 "schedule_events"
                                               {:events [{:op "auto/configure" :request {:enabled? false} :at {:beat 0}}]}
                                               "run-8" "post")))))
        (is (re-find #"Only the show's late policy"
                     (str (:message (dispatch! rt 2600.0 "schedule_events"
                                               {:events [{:op "auto/configure" :request {:enabled? false} :at {:bar 9} :late-beats 3}]}
                                               "run-9" "post"))))))
      (testing "an auto/configure posted through schedule_events is checked like configure_auto"
        (is (re-find #"No saved cue"
                     (str (:message (dispatch! rt 600.0 "schedule_events"
                                               {:events [{:op "auto/configure" :request {:enabled? true :scope {:cue-ids ["nope"]}}
                                                          :grid {:bars 1}}]} "run-4" "post"))))))
      (finally (show/stop! rt)))))

(deftest a-subscriber-cannot-restart-auto
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"scheduled with :at, not run by a subscriber"
                        (subscribers/validate-definition {:id :bars :match {:type :clock/bar}
                                                          :actions [{:op :auto/configure :request {:enabled? true}}]}))))

(deftest a-next-section-change-moves-with-the-section-on-a-seek
  ;; Review of #768: the section timer moves with a seek or loop, so the change
  ;; waiting for its line must move by the same offset.
  (let [rt (runtime)]
    (try
      (submit! rt 0.0 :auto (red-show/auto-config 217))
      (show/tick! rt 130.0)
      (let [ack (submit! rt 130.0 :auto (assoc (mood :accent) :at :next-section))
            model (:model @(:state rt))
            end #(get-in (show/snapshot rt) [:auto :section-end-beat])
            section-end (fn [m] (let [{:keys [started-at duration]} (get-in m [:auto :state :section :timer])]
                                  (+ started-at (:value duration))))
            due (fn [m] (:due-beat (first (subscribers/upcoming m :auto/configure))))]
        (is (= [(end)] (:due-beats ack)))
        (doseq [offset [2.0 -2.0]]
          (let [moved (clock/rebase model {:beat (+ (beat rt) offset)})]
            (is (== (+ (section-end model) offset) (section-end moved)))
            (is (== (section-end moved) (due moved)) (str "a seek of " offset " beats keeps the change on the section line")))))
      (finally (show/stop! rt)))))

(deftest the-scheduler-alone-cannot-apply-an-auto-change
  ;; A host operation needs the show owner's handler; persisted and rebased
  ;; like any scheduled event.
  (let [model (assoc (red-show/model) :show-id :s :epoch 0 :revision 0 :time-ms 0)
        posted (:model (subscribers/post model {:events [{:op :auto/configure :request {:enabled? false} :at {:beat 1}}]
                                                :source {:adapter :repl :session-id "t"} :command-id "c"}))]
    (is (= posted (update posted :event-subscribers subscribers/validate-state)) "a pending change is valid saved state")
    (let [due (subscribers/advance (assoc-in posted [:transport :beat] 1.0) 500.0 {})]
      (is (= {:status :rejected :message "Only the show owner can apply this action"}
             (select-keys (last (get-in due [:event-subscribers :recent])) [:status :message]))))
    (let [seeked (subscribers/rebase-clock (assoc-in posted [:transport :beat] 4.0) 4.0 4.0)]
      (is (= :missed (:status (last (get-in seeked [:event-subscribers :recent])))) "a seek past its line misses it, as for any :at event"))))
