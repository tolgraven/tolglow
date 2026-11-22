(ns tolglow.autopilot.actions-test
  (:require [afterglow.control.model :as control]
            [afterglow.render.frame :as frame]
            [afterglow.runtime.engine :as engine]
            [clojure.data.json :as json]
            [clojure.test :refer [deftest is]]
            [tolglow.agents :as agents]
            [tolglow.compiler :as compiler]
            [tolglow.autopilot.actions :as actions]
            [tolglow.effects.authoring :as authoring]))

(def fixtures [{:id :a :universe 1 :address 1 :channels {:dimmer 1}}
               {:id :b :universe 1 :address 2 :channels {:dimmer 1}}])
(def effect {:id :wash :version 1 :params {:level {:type :number :bounds [0 1] :default 0.5}}
             :outputs {:dimmer {:op :param :key :level}}})
(def cue {:id :wash-cue :effect-id :wash :version 1})
(defn model []
  (assoc (control/new-show {:registry {:effects {:wash effect} :cues {:wash-cue cue}}})
         :rig {:fixtures fixtures}))
(defn command [model cmd time-ms]
  (control/apply-command model (merge {:command-id (str (random-uuid)) :source :manual
                                       :show-id (:show-id model) :epoch (:epoch model) :time-ms time-ms} cmd)))
(defn initial []
  (let [m (model)] {:model m :state (actions/initial-state m :floor)}))
(defn advance [{:keys [model state]} op & [edits]]
  (actions/apply-decision model state (merge {:op op :domain :brightness :direction :up :amount 0.2
                                               :fixture-ids [:a :b] :duration {:beats 1}} edits) 0 command))
(defn stop [{:keys [model state]}] (actions/stop model state 0 command))

(deftest only-admitted-register-entries-and-manual-takeover
  (let [start (-> (initial) (assoc-in [:model :registry :cues :rule] (assoc cue :id :rule :code-cue? true)))
        launched (advance start :launch)
        id (get-in launched [:result :instance-id])
        changed (advance launched :adjust)]
    (is (= :wash-cue (get-in launched [:model :instances id :cue-id])))
    (is (== 0.7 (get-in changed [:model :instances id :params :level])))
    (is (= :no-eligible-look (:result (advance start :launch {:cue-ids [:rule]}))))
    (let [manual (:model (command (:model changed) {:op :param/set :instance-id id :param :level :value 0.34} 0))
          stopped (stop (assoc changed :model manual))]
      (is (= 0.34 (get-in stopped [:model :instances id :params :level])))
      (is (empty? (get-in stopped [:state :owned]))))
    (is (empty? (get-in (stop changed) [:model :instances])))))

(deftest scope-reroutes-a-broad-cue-to-an-ephemeral-instance
  (let [scoped (advance (initial) :launch {:fixture-ids [:a] :cue-ids [:wash-cue]})
        id (get-in scoped [:result :instance-id])
        tmp (get-in scoped [:model :instances id :effect-id])
        cleaned (stop scoped)]
    (is (string? id) (pr-str (:result scoped)))
    (is (= [:a] (get-in scoped [:model :instances id :definition :targets])))
    (is (= {:beats 1} (get-in scoped [:model :instances id :duration])))
    (is (nil? (get-in cleaned [:model :registry :effects tmp])))
    (is (nil? (get-in cleaned [:model :registry :versions [:effects tmp 1]])))
    (is (= {:wash effect} (get-in cleaned [:model :registry :effects])))
    (is (= {:wash-cue cue} (get-in cleaned [:model :registry :cues])))))

(deftest overdrive-is-a-reversible-overlay-on-human-look
  (let [start (initial)
        {:keys [model ack]} (command (:model start) {:op :cue/launch :cue-id :wash-cue} 0)
        id (:instance-id ack)
        adjusted (advance (assoc start :model model) :adjust)
        stopped (stop adjusted)]
    (is (= {id 1.2} (get-in adjusted [:state :instance-gains])))
    (is (= 0.5 (get-in adjusted [:model :instances id :params :level])))
    (is (= :no-owned-look (:result (advance adjusted :retire))))
    (is (empty? (get-in stopped [:state :instance-gains])))
    (is (= (:instances model) (get-in stopped [:model :instances])))))

(deftest local-generated-looks-and-late-provider-replies
  (let [generated (advance (initial) :generate {:domain :activity :fixture-ids [:b]})
        instance (first (vals (get-in generated [:model :instances])))
        request (advance (initial) :author)
        req (:author-request request)
        accept (fn [{:keys [model state]} definition] (actions/accept-authored model state req definition 0 command))]
    (is (= :one-shot (:launch-mode instance)) (pr-str (:result generated)))
    (is (= [:b] (get-in instance [:definition :targets])))
    (is (empty? (get-in (stop generated) [:state :temporary])))
    (is (= :author-pending (:result (advance request :author))))
    (is (= :stale-proposal (:result (accept (stop request) {:outputs {:dimmer 1}}))))
    (is (= :invalid-proposal (get-in (accept request {:outputs {:fog 1}}) [:result :error])))
    (is (= :launch (get-in (accept request {:outputs {:dimmer 1}}) [:result :op])))
    (let [state (actions/restore (:state request) {} 2)]
      (is (nil? (:pending state)))
      (is (= :stale-proposal (:result (actions/accept-authored (assoc (:model request) :epoch 2)
                                                               state req {:outputs {:dimmer 1}} 0 command)))))))

(deftest provider-composes-off-owner-and-cannot-execute-live-commands
  (let [events (atom []) threads (atom []) complete (promise)
        transport (fn [r consume! _]
                    (let [body (json/read-str (:body r) :key-fn keyword)
                          calls (swap! events conj body)
                          _ (swap! threads conj (.getName (Thread/currentThread)))
                          delta (if (= 1 (count calls))
                                  {:tool_calls [{:index 0 :id "write"
                                                 :function {:name "propose_effect"
                                                            :arguments (json/write-str {:definition {:outputs {:dimmer 0.5}}})}}]}
                                  {:content "done"})]
                      (consume! (str "data: " (json/write-str {:choices [{:delta delta}]}) "\n\ndata: [DONE]\n\n"))
                      {:status 200}))
        mgr (agents/create-manager {:transport transport})
        req (:author-request (advance (initial) :author))]
    (try
      (agents/configure! mgr {:provider :self-hosted :model "test" :endpoint "http://unused.test/v1/chat/completions"})
      (authoring/request! mgr req #(deliver complete %))
      (let [done (deref complete 5000 :timeout)]
        (is (= req (:request done)) (pr-str done))
        (is (= :dimmer (get-in done [:definition :outputs 0 :attribute])))
        (is (= [["propose_effect"] ["propose_effect"]]
               (mapv #(mapv (comp :name :function) (:tools %)) @events)))
        (is (= {:selected-conversation-id nil :conversations []} (agents/history mgr)))
        (is (= #{"tolglow-agent-worker"} (set @threads))))
      (finally (agents/close! mgr)))))

(deftest a-shutter-only-cue-needs-a-lit-patched-companion
  (let [start (-> (initial)
                  (assoc-in [:model :rig :fixtures 0 :channels :strobe] 2)
                  (assoc-in [:model :registry :effects :flash]
                            {:id :flash :version 1 :params {} :targets [:a] :outputs {:strobe 1.0}})
                  (assoc-in [:model :registry :cues :flash] {:id :flash :version 1 :effect-id :flash}))
        decision {:domain :activity :cue-ids [:flash] :fixture-ids [:a]}]
    (is (= :no-eligible-look (:result (advance start :launch decision))))
    (is (= :launch (get-in (advance start :launch (assoc decision :lit-fixture-ids [:a])) [:result :op])))
    (is (= :no-eligible-look (:result (advance start :launch (assoc decision :lit-fixture-ids [:b])))))))

(deftest revised-scope-and-reversed-target-discard-old-proposal
  (let [pending (advance (initial) :author)
        req (:author-request pending)
        after (advance pending :adjust {:direction :down})]
    (is (= :stale-proposal (:result (actions/accept-authored (:model after) (:state after)
                                                             req {:outputs {:dimmer 1}} 0 command))))
    (is (= :invalid-proposal
           (get-in (actions/accept-authored (assoc-in (:model pending) [:rig :fixtures] [])
                                            (:state pending) req {:outputs {:dimmer 1}} 0 command)
                   [:result :error])))))

(deftest admission-ranks-visible-demand-and-scales-a-brighter-launch-to-the-deficit
  (let [start (-> (initial)
                  (assoc-in [:model :registry :effects :dim]
                            {:id :dim :version 1 :params {} :outputs {:dimmer 0.15}})
                  (assoc-in [:model :registry :effects :black]
                            {:id :black :version 1 :params {} :outputs {:dimmer 0}}))
        small (advance start :launch)
        full (advance start :launch {:cue-ids [:wash-cue]})
        id (get-in full [:result :instance-id])]
    (is (= :dim (get-in small [:result :origin])))
    (is (== 0.4 (get-in full [:model :instances id :system :opacity])))
    (is (nil? (get-in full [:model :registry :cues :wash-cue :system])))))

(deftest activity-alone-cannot-launch-another-steady-wash-or-boost-its-level
  (let [start (initial)
        already-lit (advance start :launch)
        cmd {:domain :activity :lit-fixture-ids [:a :b]}
        attempts (take 5 (iterate #(advance % :adjust cmd) already-lit))]
    (is (= :no-eligible-look (:result (advance start :launch cmd))))
    (doseq [result attempts]
      (is (= 1 (count (get-in result [:model :instances]))))
      (is (= (get-in already-lit [:model :instances]) (get-in result [:model :instances])))
      (is (empty? (get-in result [:state :instance-gains]))))))

(deftest manual-opacity-change-relinquishes-running-instance
  (let [started (advance (initial) :launch)
        id (get-in started [:result :instance-id])
        adjusted (:model (command (:model started) {:op :instance/opacity-set :instance-id id :value 0.33} 0))
        stopped (stop (assoc started :model adjusted))]
    (is (== 0.33 (get-in stopped [:model :instances id :system :opacity])))
    (is (empty? (get-in stopped [:state :owned])))))

(defn packed-output [model time-ms]
  (let [fixture-list (get-in model [:rig :fixtures])
        patch (frame/compile-patch fixture-list)
        clock (assoc (:transport model) :beat (/ time-ms 500.0))
        tick (engine/advance (assoc model :transport clock) {:time-ms time-ms :fixtures (:fixtures patch) :transport clock}
                             compiler/compile-effect)
        output (frame/resolve-contributions patch (:contributions tick) {})]
    (mapv #(bit-and 255 %) (take 2 (get-in output [:universes 1])))))

(deftest admission-and-generator-reach-real-rendered-bytes-in-scope
  (let [admitted (advance (initial) :launch {:fixture-ids [:b]})
        pulsing (advance (initial) :generate {:domain :activity :fixture-ids [:b]})
        quiet (packed-output (:model pulsing) 0)
        peak (packed-output (:model pulsing) 60)]
    (is (= [0 51] (packed-output (:model admitted) 0)))
    (is (= 0 (first quiet) (first peak)))
    (is (< 0 (second quiet) (second peak)))))
