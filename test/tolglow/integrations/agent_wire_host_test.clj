(ns tolglow.integrations.agent-wire-host-test
  "Recorded transport (#863 P1): a proposer request goes to a capture at the
  HTTP boundary, the recorded provider reply comes back, and it passes enum
  lowercasing, validate-arguments, the owner's bounds and lowering before the
  real show owner admits or refuses it. No provider is contacted."
  (:require [afterglow.control.model :as control]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
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
        stale (wire/check-basis record snapshot agent-source)]
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
          (is (= [] (wire/check-basis record (model rt) agent-source))))
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
            (is (= [[:basis :pending-digest]] (mapv :path (:errors ack))))
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
