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

(defn- revision [rt] (get-in @(:state rt) [:model :revision]))

(deftest a-recorded-claude-reply-is-lowered-and-admitted-by-the-show-owner
  (let [{:keys [sent request result]} (recorded-exchange {:provider :anthropic :model "claude-haiku-5-5" :max-tokens 4096}
                                                         "anthropic-proposer.reply.json")
        rt (runtime)
        calls (atom 0)
        dispatch! (dispatcher rt calls)]
    (try
      (is (= [request] sent) "exactly one request reached the HTTP boundary")
      (is (:ok? result))
      (let [parsed (wire/parse-reply (:text result))
            {:keys [ok? proposals]} (wire/admit-proposals (:value parsed))
            [p1 p2] proposals]
        (is (:ok? parsed))
        (is ok? "the recorded reply's mixed-case enums are lowercased and accepted")
        (is (= {:op "cue/launch" :id "launch-wash" :label "g-lift" :grid {:bars 1}
                :select {:ids ["wash"] :pick "first"} :launch {:mode "persistent" :params {:level 0.4}}}
               (first (:events p1))))
        (is (= {:target "param" :param "level" :to 0.9 :over {:bars 2} :curve "ease-out"}
               (select-keys (second (:events p1)) [:target :param :to :over :curve])))
        (is (= (revision rt) (get-in p1 [:basis :rev])) "the recording was made against this show revision")
        (testing "the best alternative posts through the agent tool boundary"
          (let [ack (wire/post-proposal! dispatch! 0 p1 metadata)
                upcoming (:upcoming (show/schedule rt))]
            (is (= :accepted (:status ack)))
            (is (= ["launch-wash" "swell" "mark"] (:event-ids ack)))
            (is (= ["launch-wash" "swell" "mark"] (mapv :event-id upcoming)))
            (is (every? #(= {:adapter :agent :session-id "wire-run"} (:source %)) upcoming))
            (is (= ["g-lift" "g-lift" nil] (mapv :label upcoming)))
            (is (= [4.0 8.0] (mapv :due-beat (take 2 upcoming))) "the next bar line and absolute bar 2 (4/4)")
            (is (= 16.0 (- (:due-beat (last upcoming)) (:posted-beat (last upcoming)))) "four bars after posting")))
        (testing "the other alternative was written against the same basis, which the owner now refuses"
          (let [before (:upcoming (show/schedule rt))
                ack (wire/post-proposal! dispatch! 1 p2 metadata)]
            (is (= :rejected (:status ack)))
            (is (= :stale-revision (:code ack)))
            (is (= [{:path [:proposals 1 :basis_rev] :message "Show revision changed"}] (:errors ack)))
            (is (= before (:upcoming (show/schedule rt))) "nothing was posted"))))
      (finally (show/stop! rt)))))

(deftest owner-refusals-carry-the-wire-path-of-the-event-they-name
  (let [{:keys [result]} (recorded-exchange {:provider :openrouter :model "anthropic/claude-haiku-5-5" :max-tokens 4096}
                                            "openrouter-proposer.reply.json")
        recorded (:value (wire/parse-reply (:text result)))
        rt (runtime)
        calls (atom 0)
        dispatch! (dispatcher rt calls)]
    (try
      (is (:ok? (wire/admit-proposals recorded)) "the OpenRouter recording lowers like the Claude one")
      (testing "an unknown cue ID is valid on the wire and refused by the owner"
        (let [reply (assoc-in recorded [:proposals 0 :events 0 :select :ids] ["ghost"])
              {:keys [ok? proposals]} (wire/admit-proposals reply)
              ack (wire/post-proposal! dispatch! 0 (first proposals) metadata)]
          (is ok?)
          (is (= :rejected (:status ack)))
          (is (= :invalid-schedule (:code ack)))
          (is (= [{:path [:proposals 0 :events 0] :message "Event 0: Unknown cue ghost"}] (:errors ack)))
          (is (empty? (:upcoming (show/schedule rt))) "one bad event rejects the whole proposal")))
      (testing "replies refused before the owner never reach it"
        (reset! calls 0)
        (let [unknown-enum (assoc-in recorded [:proposals 0 :events 1 :curve] "Wobble")
              out-of-bounds (assoc-in recorded [:proposals 0 :confidence] 1.5)
              ms-timing (assoc-in recorded [:proposals 0 :events 0 :timing :ms] 250)]
          (is (= {:ok? false :stage :wire} (select-keys (wire/admit-proposals unknown-enum) [:ok? :stage])))
          (is (= [:proposals 0 :events 1 :curve] (:path (first (:errors (wire/admit-proposals unknown-enum))))))
          (is (= {:ok? false :stage :bounds
                  :errors [{:path [:proposals 0 :confidence] :message "Number is above maximum"}]}
                 (wire/admit-proposals out-of-bounds)))
          (is (= [{:path [:proposals 0 :events 0 :timing :ms] :message "Unknown tool argument"}]
                 (:errors (wire/admit-proposals ms-timing))))
          (is (= 0 @calls))))
      (finally (show/stop! rt)))))
