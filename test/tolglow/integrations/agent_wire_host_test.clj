(ns tolglow.integrations.agent-wire-host-test
  "Recorded transport (#863 P1): a proposer request goes to a capture at the
  HTTP boundary, the recorded provider reply comes back, and it passes enum
  lowercasing, validate-arguments, the owner's bounds and lowering before the
  real show owner admits or refuses it. A basis taken while the source has a
  plan pending stays current while that plan proceeds. No provider is
  contacted."
  (:require [afterglow.control.model :as control]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [tolglow.clock :as clock]
            [tolglow.event-subscribers :as subscribers]
            [tolglow.integrations.agent-wire :as wire]
            [tolglow.integrations.providers :as providers]
            [tolglow.show :as show]))

(def ^:private effect
  {:id :wash :version 1 :category :light
   :params {:level {:type :number :default 0.0 :bounds [0.0 1.0]}}
   :outputs {:dimmer {:op :param :key :level}}})

(defn- runtime []
  (show/create-runtime
   {:model (control/new-show
            {:show-id :wire-test :transport {:playing? true :beat 0.0 :bpm 120.0}
             :registry {:effects {:wash effect} :cues {:wash {:id :wash :effect-id :wash :version 1}}}
             :rig {:fixtures [{:id :left :universe 1 :address 1 :channels {:dimmer 1}}]}})
    ;; Capture sink: frames are dropped, nothing leaves the process.
    :sink {:send! (fn [_ _])} :workspace-options nil}))

(defn- dispatcher
  "show/dispatch-tool! on rt, driving owner ticks until it answers, counting calls."
  [rt calls]
  (fn [tool arguments metadata]
    (swap! calls inc)
    (let [result (future (show/dispatch-tool! rt tool arguments metadata))]
      (loop [n 0]
        (when (and (not (realized? result)) (< n 400))
          (show/tick! rt (inc (get-in @(:state rt) [:model :time-ms])))
          (Thread/sleep 5) (recur (inc n))))
      (deref result 1000 ::timeout))))

(defn- fixture [file] (slurp (io/resource (str "tolglow/integrations/agent_wire/" file))))

(def ^:private messages
  [{:role :system :content "Propose up to three alternative lighting decisions as tolglow-agent-proposals/1."}
   {:role :user :content "{\"obs\":\"o-0001\",\"rev\":0}"}])

(defn- recorded-exchange
  "Send a proposer request for provider through a capture transport that
  answers with the recorded reply file; return the request and the reply text."
  [config reply-file]
  (let [sent (atom [])
        transport (fn [request] (swap! sent conj request) {:status 200 :body (fixture reply-file)})
        request (wire/reply-request config :proposal messages "recorded-transport-placeholder")
        result (providers/invoke! transport request)]
    {:sent @sent :request request :result result}))

(def ^:private metadata {:command-id "wire/1" :run-id "wire-run"})

(def ^:private agent-source {:adapter :agent :session-id "wire-run"})

(defn- model [rt] (:model @(:state rt)))

(defn- human-launch!
  "An unrelated command from the REPL: it bumps the show revision only."
  [rt]
  (let [reply (show/submit! rt :command {:op :cue/launch :cue-id :wash :launch {:mode :persistent}
                                          :source {:adapter :repl :session-id "human"}})]
    (show/tick! rt (inc (get-in @(:state rt) [:model :time-ms])))
    (deref reply 1000 ::missing)))

(defn- arbitrate-and-post!
  "The orchestrator's part, until #826 builds it: freeze a fresh snapshot, check
  the request's basis against it, then post with that snapshot's revision."
  [rt dispatch! record i proposal]
  (let [snapshot (model rt)
        stale (wire/check-basis record snapshot)]
    (if (seq stale)
      {:status :dropped :code :stale-basis :errors stale}
      (wire/post-proposal! dispatch! i proposal (:revision snapshot) metadata))))

(deftest a-recorded-claude-reply-is-lowered-and-admitted-by-the-show-owner
  ;; Provisional until the proposal contract (#826) settles the basis and the
  ;; arbitration window: this exercises the research basis (epoch, auto session,
  ;; own pending events) and the arbitrated snapshot's revision.
  (let [rt (runtime)
        record (wire/request-record "w-0001" (wire/request-basis (model rt) agent-source "o-0001"))
        {:keys [sent request result]} (recorded-exchange {:provider :anthropic :model "claude-haiku-5-5" :max-tokens 4096}
                                                         "anthropic-proposer.reply.json")
        calls (atom 0)
        dispatch! (dispatcher rt calls)]
    (try
      (is (= [request] sent) "exactly one request reached the HTTP boundary")
      (is (:ok? result))
      (let [parsed (wire/read-reply result)
            {:keys [ok? proposals]} (wire/admit-proposals record (:value parsed))
            [p1 p2] proposals]
        (is (:ok? parsed))
        (is ok? "the recorded reply's mixed-case enums are lowercased and accepted")
        (is (= {:op "cue/launch" :id "launch-wash" :label "g-lift" :grid {:bars 1}
                :select {:ids ["wash"] :pick "first"} :launch {:mode "persistent" :params {:level 0.4}}}
               (first (:events p1))))
        (is (= {:target "param" :param "level" :to 0.9 :over {:bars 2} :curve "ease-out"}
               (select-keys (second (:events p1)) [:target :param :to :over :curve])))
        (is (= (:basis record) (:basis p1) (:basis p2)) "both alternatives carry the host's basis")
        (testing "an unrelated command between the observation and arbitration does not stale the reply"
          (is (= :accepted (:status (human-launch! rt))))
          (is (< (:revision (:basis record)) (:revision (model rt))))
          (is (= [] (wire/check-basis record (model rt)))))
        (testing "the best alternative posts through the agent tool boundary"
          (let [ack (arbitrate-and-post! rt dispatch! record 0 p1)
                upcoming (:upcoming (show/schedule rt))]
            (is (= :accepted (:status ack)))
            (is (= ["launch-wash" "swell" "mark"] (:event-ids ack)))
            (is (= ["launch-wash" "swell" "mark"] (mapv :event-id upcoming)))
            (is (every? #(= agent-source (:source %)) upcoming))
            (is (= ["g-lift" "g-lift" nil] (mapv :label upcoming)))
            (is (= [4.0 8.0] (mapv :due-beat (take 2 upcoming))) "the next bar line and absolute bar 2 (4/4)")
            (is (= 16.0 (- (:due-beat (last upcoming)) (:posted-beat (last upcoming)))) "four bars after posting")))
        (testing "after one alternative posts, this source's pending events changed, so the shared basis is stale"
          ;; The arbiter posts at most one alternative per source per window; this
          ;; is what the basis says if a second is offered anyway.
          (let [before (:upcoming (show/schedule rt))
                ack (arbitrate-and-post! rt dispatch! record 1 p2)]
            (is (= {:status :dropped :code :stale-basis} (select-keys ack [:status :code])))
            (is (= (for [id ["launch-wash" "mark" "swell"]] {:path [:basis :pending] :event-id id :change :added})
                   (mapv #(select-keys % [:path :event-id :change]) (:errors ack))))
            (is (= before (:upcoming (show/schedule rt))) "nothing was posted")))
        (testing "a command committed after arbitration is refused by the owner"
          (let [snapshot-revision (:revision (model rt))
                _ (human-launch! rt)
                proposal (assoc p2 :id "p2-late")
                ack (wire/post-proposal! dispatch! 1 proposal snapshot-revision metadata)]
            (is (= :rejected (:status ack)))
            (is (= :stale-revision (:code ack)))
            (is (= [{:path [:proposals 1] :message "Show revision changed"}] (:errors ack)))))
        (is (thrown? clojure.lang.ExceptionInfo (wire/post-proposal! dispatch! 0 p1 nil metadata))
            "the expected revision is explicit, never taken from the reply"))
      (finally (show/stop! rt)))))

(deftest owner-refusals-carry-the-wire-path-of-the-event-they-name
  (let [rt (runtime)
        record (wire/request-record "w-0002" (wire/request-basis (model rt) agent-source "o-0001"))
        {:keys [result]} (recorded-exchange {:provider :openrouter :model "anthropic/claude-haiku-5-5" :max-tokens 4096}
                                            "openrouter-proposer.reply.json")
        recorded (:value (wire/read-reply result))
        calls (atom 0)
        dispatch! (dispatcher rt calls)]
    (try
      (is (:ok? (wire/admit-proposals record recorded)) "the OpenRouter recording lowers like the Claude one")
      (testing "an unknown cue ID is valid on the wire and refused by the owner"
        (let [reply (assoc-in recorded [:proposals 0 :events 0 :select :ids] ["ghost"])
              {:keys [ok? proposals]} (wire/admit-proposals record reply)
              ack (arbitrate-and-post! rt dispatch! record 0 (first proposals))]
          (is ok?)
          (is (= :rejected (:status ack)))
          (is (= :invalid-schedule (:code ack)))
          (is (= [{:path [:proposals 0 :events 0] :message "Event 0: Unknown cue ghost"}] (:errors ack)))
          (is (empty? (:upcoming (show/schedule rt))) "one bad event rejects the whole proposal")))
      (testing "replies refused before the owner never reach it"
        (reset! calls 0)
        (let [unknown-enum (assoc-in recorded [:proposals 0 :events 1 :curve] "Wobble")
              out-of-bounds (assoc-in recorded [:proposals 0 :confidence] 1.5)
              ms-timing (assoc-in recorded [:proposals 0 :events 0 :timing :ms] 250)
              forged (assoc-in recorded [:proposals 0 :basis_rev] (:revision (model rt)))
              forged-later (do (human-launch! rt) (assoc-in recorded [:proposals 0 :basis_rev] (:revision (model rt))))]
          (is (= {:ok? false :stage :wire} (select-keys (wire/admit-proposals record unknown-enum) [:ok? :stage])))
          (is (= [:proposals 0 :events 1 :curve] (:path (first (:errors (wire/admit-proposals record unknown-enum))))))
          (is (= {:ok? false :stage :bounds
                  :errors [{:path [:proposals 0 :confidence] :message "Number is above maximum"}]}
                 (wire/admit-proposals record out-of-bounds)))
          (is (= [{:path [:proposals 0 :events 0 :timing :ms] :message "Unknown tool argument"}]
                 (:errors (wire/admit-proposals record ms-timing))))
          (is (:ok? (wire/admit-proposals record forged)) "an echo equal to the recorded revision is the honest one")
          (is (= {:ok? false :stage :basis
                  :errors [{:path [:proposals 0 :basis_rev] :code :basis-mismatch :message "Request w-0002 sent revision 0"}]}
                 (wire/admit-proposals record forged-later))
              "a forged echo of the show's current revision is refused")
          (is (= 0 @calls))))
      (finally (show/stop! rt)))))

(defn- advance-to-beat!
  "Run owner cycles in 20 ms steps until the transport reaches beat."
  [rt beat]
  (loop [n 0]
    (when (and (< (get-in (model rt) [:transport :beat]) beat) (< n 2000))
      (show/tick! rt (+ 20 (get-in (model rt) [:time-ms])))
      (recur (inc n))))
  (get-in (model rt) [:transport :beat]))

(defn- changes [record snapshot]
  (mapv (juxt :code :event-id :change) (wire/check-basis record snapshot)))

(deftest a-basis-taken-with-a-plan-pending-stays-current-while-the-plan-proceeds
  ;; R1 of #924: the source posted a plan earlier; the next request's basis
  ;; must survive that plan's own events firing, a ramp starting and a Link
  ;; phase nudge, and go stale only when the plan is changed.
  (let [rt (runtime)
        first-record (wire/request-record "w-0010" (wire/request-basis (model rt) agent-source "o-0001"))
        {:keys [result]} (recorded-exchange {:provider :anthropic :model "claude-haiku-5-5" :max-tokens 4096}
                                            "anthropic-proposer.reply.json")
        ;; The recorded swell selects the cue ID as a string, which the owner
        ;; does not match against a running instance's cue; select the launched
        ;; wash on the wire by the intensity category it writes instead, with
        ;; the empty ids list every wire selector carries. A two-beat swell
        ;; completes before the mark is due.
        reply (update-in (:value (wire/read-reply result)) [:proposals 0 :events 1] assoc
                         :select {:ids [] :categories ["intensity"] :pick "all"} :over {:unit "beats" :n 2})
        [plan] (:proposals (wire/admit-proposals first-record reply))
        dispatch! (dispatcher rt (atom 0))]
    (try
      (is (= :accepted (:status (arbitrate-and-post! rt dispatch! first-record 0 plan))))
      (let [record (wire/request-record "w-0011" (wire/request-basis (model rt) (assoc agent-source :run-id "turn-2") "o-0002")
                                        {:deadline-beat 12})
            pending (get-in record [:basis :pending])]
        (is (= agent-source (get-in record [:basis :source])))
        (is (= ["launch-wash" "mark" "swell"] (mapv :event-id (:upcoming pending))) "the plan is recorded, not hashed")
        (is (= [[:absolute 4.0] [:relative nil] [:absolute 8.0]] (mapv (juxt :anchor :due-beat) (:upcoming pending)))
            "a relative event's due beat is not part of the plan")
        (testing "a Link phase nudge shifts relative waits without changing the plan"
          (let [snapshot (model rt)
                nudged (clock/rebase snapshot {:beat (+ 0.125 (get-in snapshot [:transport :beat]))})]
            (is (= [0.0 0.0 0.125] (mapv - (map :due-beat (:upcoming (subscribers/schedule-view nudged)))
                                         (map :due-beat (:upcoming (subscribers/schedule-view snapshot)))))
                "only the relative mark's due beat moved")
            (is (= [] (changes record nudged)))))
        (testing "a seek past a bar line requantizes the grid launch: an absolute event moved"
          (is (= [[:stale-basis "launch-wash" :changed]] (changes record (clock/rebase (model rt) {:beat 5.0})))))
        (testing "a seek past bar 2 misses the absolute swell: removed without firing"
          (is (= [[:stale-basis "launch-wash" :changed] [:stale-basis "swell" :removed]]
                 (changes record (clock/rebase (model rt) {:beat 9.0})))))
        (testing "an earlier event of the plan fires"
          (advance-to-beat! rt 4.5)
          (is (= ["swell" "mark"] (mapv :event-id (:upcoming (show/schedule rt)))))
          (is (= [] (wire/check-basis record (model rt)))))
        (testing "the ramp starts and leaves the upcoming list"
          (advance-to-beat! rt 8.5)
          (is (= ["mark"] (mapv :event-id (:upcoming (show/schedule rt)))))
          (is (= ["swell"] (mapv :event-id (:ramps (show/schedule rt)))))
          (is (= [] (wire/check-basis record (model rt)))))
        (testing "the ramp completes"
          (advance-to-beat! rt 10.75)
          (is (empty? (:ramps (show/schedule rt))))
          (is (= [] (wire/check-basis record (model rt)))))
        (testing "an operator cancels the rest of the plan: removed without firing"
          (let [reply (show/cancel-events! rt {:event-ids ["mark"]})]
            (show/tick! rt (+ 20 (:time-ms (model rt))))
            (is (= :accepted (:status (deref reply 1000 ::missing)))))
          (is (= [[:stale-basis "mark" :removed]] (changes record (model rt)))))
        (testing "past the deadline beat the answer is late as well"
          (advance-to-beat! rt 12.0)
          (is (= [[:late nil nil] [:stale-basis "mark" :removed]] (changes record (model rt))))))
      (finally (show/stop! rt)))))
