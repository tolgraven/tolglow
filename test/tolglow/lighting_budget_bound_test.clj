(ns tolglow.lighting-budget-bound-test
  "An agent may not raise a lighting budget limit the operator bound to a cue
  parameter (#901): not by writing the parameter, launching or ending the cue,
  revising its definition, running a macro or applying a staged draft, directly
  or from a scheduled step. Each path runs through the real show owner with a
  captured sink; the effective limit is read where Studio and the REPL read it,
  [:variables :lighting/budget :global]."
  (:require [afterglow.control.model :as control]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [tolglow.composition :as composition]
            [tolglow.event-subscribers :as subscribers]
            [tolglow.knob-candidates :as knob-candidates]
            [tolglow.knob-presets :as knob-presets]
            [tolglow.knobs :as knobs]
            [tolglow.lighting-budget.agent-policy :as policy]
            [tolglow.show :as show]
            [tolglow.show.scene-draft :as scene-draft]))

(def ^:private wash
  {:id :wash :version 1 :category :light
   :params {:level {:type :number :default 0.0 :bounds [0.0 1.0]}}
   :outputs {:dimmer {:op :param :key :level}}})

(def ^:private bound
  "The operator's limit reads the limit cue's level; with no instance running
  it falls back to 0.9, above what the operator's running instance sets."
  {:cue-id :limit-cue :param :level :default 0.9})

(def ^:private agent-meta {:conversation-id "conversation-1"})

(def ^:private agent-source {:adapter :agent :session-id "conversation-1"})

(def ^:private studio {:adapter :web :session-id "tab"})

(def ^:private repl {:adapter :repl :session-id "repl"})

(defn- live-model []
  (-> (control/new-show
       {:show-id :bound-budget :transport {:playing? true :beat 0.0 :bpm 120.0}
        :registry {:effects {:wash wash}
                   :cues {:limit-cue {:id :limit-cue :effect-id :wash :version 1}
                          :other {:id :other :effect-id :wash :version 1}}}
        :rig {:fixtures [{:id :left :universe 1 :address 1 :channels {:dimmer 1}}
                         {:id :right :universe 1 :address 2 :channels {:dimmer 1}}]}})
      (assoc-in [:settings :lighting-budget] {:enabled? true :global {:limit bound}})
      (composition/save {:id :with-limit :mode :macro :slots [{:cue-id :limit-cue} {:cue-id :other}]})
      (composition/save {:id :without-limit :mode :macro :slots [{:cue-id :other}]})))

(defn- harness []
  (let [rt (show/create-runtime {:model (live-model) :sink {:send! (fn [_ _])} :workspace-options nil})
        clock (atom 0)]
    {:rt rt
     :tick! (fn [] (show/tick! rt (swap! clock + 25)))
     :clock clock}))

(defn- driven
  "Run f, which waits on the owner, while ticking the owner until it answers."
  [{:keys [tick!]} f]
  (let [result (future (f))]
    (loop [n 0]
      (when (and (not (realized? result)) (< n 400))
        (tick!) (Thread/sleep 2) (recur (inc n))))
    (deref result 1000 ::timeout)))

(defn- send!
  "Submit a command as `source` and tick once."
  [{:keys [rt tick!]} source payload]
  (let [reply (show/submit! rt :command (assoc payload :source source))]
    (tick!)
    (deref reply 1000 ::missing)))

(defn- tool! [h tool arguments]
  (driven h #(show/dispatch-tool! (:rt h) tool arguments (assoc agent-meta :run-id (str (random-uuid))
                                                                :command-id (str (random-uuid))))))

(defn- model [{:keys [rt]}] (:model @(:state rt)))

(defn- effective
  "The global limit the allocator applied on the last frame, as a fraction."
  [h]
  ((:tick! h))
  (let [{:keys [limit-watts capacity-watts]} (get-in (model h) [:variables :lighting/budget :global])]
    (/ (Math/round (* 1000.0 (/ limit-watts capacity-watts))) 1000.0)))

(defn- level [h id] (get-in (model h) [:instances id :params :level]))

(defn- refused? [ack]
  (and (= :rejected (:status ack))
       (= :agent-budget-loosening (or (get-in ack [:details :code]) (:code ack)))))

(defn- operator-limit!
  "Launch the operator's limit cue at 0.5 from Studio; return its instance ID."
  [h]
  (let [ack (send! h studio {:op :cue/launch :cue-id :limit-cue :launch {:params {:level 0.5}}})]
    (is (= :accepted (:status ack)))
    (is (= 0.5 (effective h)))
    (:instance-id ack)))

(defn- recent-status [h event-id]
  (some #(when (= event-id (:event-id %)) %) (get-in (show/snapshot (:rt h)) [:schedule :recent])))

(defn- schedule! [h event]
  (let [ack (tool! h "schedule_events" {:events [(assoc event :in {:beats 1})]})]
    (is (= :accepted (:status ack)) (pr-str ack))
    ;; One beat at 120 BPM is 500 ms; let it fire.
    (dotimes [_ 30] ((:tick! h)))
    (recent-status h (:id event))))

(deftest parameter-writes-cannot-raise-a-bound-limit
  (let [h (harness)]
    (try
      (let [id (operator-limit! h)]
        (testing "set_parameter above the operator's value is refused and changes nothing"
          (is (refused? (tool! h "set_parameter" {:instance-id id :param "level" :value 0.8})))
          (is (= 0.5 (level h id)))
          (is (= 0.5 (effective h))))
        (testing "the agent may tighten it and relax its own tightening, never past the operator's value"
          (is (= :accepted (:status (tool! h "set_parameter" {:instance-id id :param "level" :value 0.3}))))
          (is (= 0.3 (effective h)))
          (is (= :accepted (:status (tool! h "set_parameter" {:instance-id id :param "level" :value 0.5}))))
          (is (refused? (tool! h "set_parameter" {:instance-id id :param "level" :value 0.51})))
          (is (= 0.5 (effective h))))
        (testing "another conversation cannot relax the first one's tightening"
          (is (= :accepted (:status (tool! h "set_parameter" {:instance-id id :param "level" :value 0.2}))))
          (is (refused? (driven h #(show/dispatch-tool! (:rt h) "set_parameter" {:instance-id id :param "level" :value 0.4}
                                                        {:conversation-id "conversation-2" :command-id (str (random-uuid))}))))
          (is (= 0.2 (effective h))))
        (testing "a scheduled param/set fires and is refused there"
          (let [outcome (schedule! h {:id "raise" :op "param/set" :instance-id id :param "level" :value 0.9})]
            (is (= :rejected (:status outcome)) (pr-str outcome))
            (is (re-find #"bound to a cue parameter" (str (:message outcome)))))
          (is (= 0.2 (effective h))))
        (testing "a scheduled ramp to a higher level is refused when it would start"
          (let [outcome (schedule! h {:id "swell" :op "param/ramp" :instance-id id :param "level" :to 0.9
                                      :over {:beats 2}})]
            (is (= :rejected (:status outcome)) (pr-str outcome)))
          (is (empty? (get-in (model h) [:event-subscribers :ramps])))
          (dotimes [_ 60] ((:tick! h)))
          (is (= 0.2 (effective h))))
        (testing "a ramp down whose curve overshoots past the operator's value is refused"
          (let [outcome (schedule! h {:id "dip" :op "param/ramp" :instance-id id :param "level" :to 0.0
                                      :over {:beats 2} :curve {:type :cubic-bezier :points [0.1 -6.0 0.5 1.0]}})]
            (is (= :rejected (:status outcome)) (pr-str outcome))))
        (testing "a ramp down is admitted and lands"
          (let [outcome (schedule! h {:id "fade" :op "param/ramp" :instance-id id :param "level" :to 0.1
                                      :over {:beats 1}})]
            (is (not= :rejected (:status outcome)) (pr-str outcome)))
          (dotimes [_ 60] ((:tick! h)))
          (is (= 0.1 (level h id)))
          (is (= 0.1 (effective h))))
        (testing "Studio and the REPL stay unrestricted"
          (is (= :accepted (:status (send! h studio {:op :param/set :instance-id id :param :level :value 0.8}))))
          (is (= 0.8 (effective h)))
          (is (= :accepted (:status (send! h repl {:op :param/set :instance-id id :param :level :value 0.95}))))
          (is (= 0.95 (effective h))))
        (testing "the operator's write is the new ceiling"
          (is (refused? (tool! h "set_parameter" {:instance-id id :param "level" :value 1.0})))
          (is (= :accepted (:status (tool! h "set_parameter" {:instance-id id :param "level" :value 0.9}))))))
      (finally (show/stop! (:rt h))))))

(deftest launches-and-stops-cannot-raise-a-bound-limit
  (let [h (harness)]
    (try
      (let [id (operator-limit! h)]
        (testing "launch_cue with a higher level is refused"
          (is (refused? (tool! h "launch_cue" {:cue-id "limit-cue" :launch {:params {:level 0.8}}})))
          (is (= 1 (count (get-in (model h) [:instances]))))
          (is (= 0.5 (effective h))))
        (testing "stop_instance on the operator's instance would fall back to the binding's default, so it is refused"
          (is (refused? (tool! h "stop_instance" {:instance-id id})))
          (is (= :running (get-in (model h) [:instances id :phase])))
          (is (= 0.5 (effective h))))
        (let [own (tool! h "launch_cue" {:cue-id "limit-cue" :launch {:params {:level 0.3}}})]
          (testing "a lower launch tightens it"
            (is (= :accepted (:status own)))
            (is (= 0.3 (effective h))))
          (testing "with its own launch on top, the agent still cannot end the operator's"
            (is (refused? (tool! h "stop_instance" {:instance-id id})))
            (is (= :running (get-in (model h) [:instances id :phase]))))
          (testing "it may end its own launch, which returns the operator's value"
            (is (= :accepted (:status (tool! h "stop_instance" {:instance-id (:instance-id own)}))))
            (dotimes [_ 10] ((:tick! h)))
            (is (= 0.5 (effective h)))))
        (testing "a scheduled launch with a higher level is refused when it fires"
          (let [outcome (schedule! h {:id "relaunch" :op "cue/launch" :cue-id "limit-cue"
                                      :launch {:params {:level 1.0}}})]
            (is (= :rejected (:status outcome)) (pr-str outcome)))
          (is (= 0.5 (effective h))))
        (testing "a scheduled stop of the operator's instance is refused when it fires"
          (let [outcome (schedule! h {:id "end" :op "instance/end" :instance-id id})]
            (is (= :rejected (:status outcome)) (pr-str outcome)))
          (is (= :running (get-in (model h) [:instances id :phase]))))
        (testing "the operator may end it from Studio, and the REPL may launch higher"
          (is (= :accepted (:status (send! h studio {:op :instance/end :instance-id id}))))
          (dotimes [_ 10] ((:tick! h)))
          (is (= 0.9 (effective h)))
          (is (= :accepted (:status (send! h repl {:op :cue/launch :cue-id :limit-cue :launch {:params {:level 1.0}}}))))
          (is (= 1.0 (effective h)))))
      (finally (show/stop! (:rt h))))))

(deftest definitions-cannot-raise-a-bound-limit
  (let [h (harness)]
    (try
      (operator-limit! h)
      (testing "put_cue giving the limit cue a higher level is refused"
        (is (refused? (tool! h "put_cue" {:cue {:id "limit-cue" :effect-id "wash" :params {:level 1.0}}})))
        (is (nil? (get-in (model h) [:registry :cues :limit-cue :params]))))
      (testing "publish_effect raising the default the limit cue launches with is refused"
        (let [ack (tool! h "publish_effect" {:definition (-> wash (assoc :version 2) (assoc-in [:params :level :default] 0.95))})]
          (is (refused? ack) (pr-str ack)))
        (is (= 0.0 (get-in (model h) [:registry :effects :wash :params :level :default]))))
      (testing "the operator may revise both"
        (is (= :accepted (:status (send! h studio {:op :cue/put :cue {:id :limit-cue :effect-id :wash :params {:level 0.6}}}))))
        (is (= :accepted (:status (send! h repl {:op :registry/publish :kind :effects
                                                 :definition (assoc-in (assoc wash :version 2) [:params :level :default] 0.95)})))))
      (testing "then the agent may lower the cue's level, not raise it past the operator's"
        (let [ack (tool! h "put_cue" {:cue {:id "limit-cue" :effect-id "wash" :params {:level 0.2}}})]
          (is (= :accepted (:status ack)) (pr-str ack)))
        (is (refused? (tool! h "put_cue" {:cue {:id "limit-cue" :effect-id "wash" :params {:level 0.7}}})))
        (is (= :accepted (:status (tool! h "put_cue" {:cue {:id "limit-cue" :effect-id "wash" :params {:level 0.6}}})))
            "Its own tightening may be relaxed back to the operator's revision"))
      (testing "a definition refusal leaves the running limit alone"
        (is (= 0.5 (effective h))))
      (finally (show/stop! (:rt h))))))

(deftest macros-and-staged-drafts-cannot-raise-a-bound-limit
  (let [h (harness)]
    (try
      (operator-limit! h)
      (testing "an agent may not run a macro that launches the limit cue, directly or scheduled"
        (is (refused? (send! h agent-source {:op :macro/run :macro-id :with-limit})))
        (is (empty? (:composition-runs (model h))))
        (let [outcome (schedule! h {:id "macro" :op "macro/run" :macro-id "with-limit"})]
          (is (= :rejected (:status outcome)) (pr-str outcome)))
        (is (empty? (:composition-runs (model h))))
        (is (= :accepted (:status (send! h agent-source {:op :macro/run :macro-id :without-limit}))) "A macro without it is fine"))
      (testing "a staged draft that launches the limit cue higher is refused when the agent applies it"
        (let [ops [{:op :cue/launch :cue-id :limit-cue :launch {:params {:level 1.0}}}]
              staged (send! h agent-source {:op :scene-draft/stage :draft-id "room" :operations ops})
              applied (send! h agent-source {:op :scene-draft/apply :draft-id "room" :expected-draft-revision (:draft-revision staged)})]
          (is (= :accepted (:status staged)))
          (is (refused? applied) (pr-str applied))
          (is (= 0.5 (effective h)))
          (testing "and accepted from Studio"
            (is (= :accepted (:status (send! h studio {:op :scene-draft/apply :draft-id "room"
                                                       :expected-draft-revision (:draft-revision staged)}))))
            (is (= 1.0 (effective h))))))
      (testing "the operator may run the macro"
        (is (= :accepted (:status (send! h studio {:op :macro/run :macro-id :with-limit})))))
      (finally (show/stop! (:rt h))))))

(deftest linked-knob-writes-cannot-raise-a-bound-limit
  (let [h (harness)]
    (try
      (operator-limit! h)
      (is (= :accepted (:status (send! h studio {:op :knob/create :knob-id "k" :name "Limit"
                                                 :links [{:id "l" :destination {:kind :param :cue-id :limit-cue :param :level}
                                                          :range [0 1]}]}))))
      (testing "an agent's knob turn commits the knob but refuses the write that would raise the limit"
        (let [ack (send! h agent-source {:op :knob/set :knob-id "k" :value 0.9})
              link (first (:links ack))]
          (is (= :rejected (:status link)) (pr-str ack))
          (is (= :agent-budget-loosening (:code link))))
        (is (= 0.5 (effective h))))
      (testing "a lower turn is a tightening"
        (is (= :applied (:status (first (:links (send! h agent-source {:op :knob/set :knob-id "k" :value 0.2}))))))
        (is (= 0.2 (effective h))))
      (testing "Studio's turn raises it"
        (is (= :applied (:status (first (:links (send! h studio {:op :knob/set :knob-id "k" :value 0.9}))))))
        (is (= 0.9 (effective h))))
      (finally (show/stop! (:rt h))))))

(deftest the-policy-is-one-setting
  (let [h (harness)]
    (try
      (let [id (operator-limit! h)]
        (is (= :tighten-only policy/policy) "Tighten-only is the default")
        (with-redefs [policy/policy :unrestricted]
          (is (= :accepted (:status (tool! h "set_parameter" {:instance-id id :param "level" :value 0.8}))))
          (is (= 0.8 (effective h)))))
      (finally (show/stop! (:rt h))))))

;; Classification ------------------------------------------------------------

(defn- source-forms [resource]
  (with-open [reader (java.io.PushbackReader. (io/reader (io/resource resource)))]
    (binding [*read-eval* false *ns* (the-ns 'tolglow.show)]
      (doall (take-while #(not= ::eof %) (repeatedly #(read {:eof ::eof :read-cond :allow} reader)))))))

(defn- defn-body [resource name]
  (some #(when (and (seq? %) ('#{defn defn-} (first %)) (= name (second %))) %) (source-forms resource)))

(defn- case-keys
  "The dispatch values of the first (case <sym> ...) inside form."
  [form sym]
  (let [found (first (filter #(and (seq? %) (= 'case (first %)) (= sym (second %))) (tree-seq coll? seq form)))
        clauses (drop 2 found)]
    (into #{} (mapcat (fn [k] (if (seq? k) k [k]))) (take-nth 2 (if (odd? (count clauses)) (butlast clauses) clauses)))))

(def ^:private tool-coverage
  "How each agent tool meets the bound-limit check: the regression that
  drives it, or why it cannot move a bound limit. :policy-gap names a tool
  whose later launches run as the policy, not the agent; they are listed as
  remaining work on #901."
  {"inspect_show" :reads "inspect_lighting" :reads "inspect_schedule" :reads
   "set_parameter" `parameter-writes-cannot-raise-a-bound-limit
   "schedule_events" `parameter-writes-cannot-raise-a-bound-limit
   "launch_cue" `launches-and-stops-cannot-raise-a-bound-limit
   "stop_instance" `launches-and-stops-cannot-raise-a-bound-limit
   "put_cue" `definitions-cannot-raise-a-bound-limit
   "publish_effect" `definitions-cannot-raise-a-bound-limit
   ;; Candidate edits and keep create a knob without writing; an audition
   ;; plays on the private Room model. The knob's writes are checked.
   "knob_candidate" `linked-knob-writes-cannot-raise-a-bound-limit
   "cancel_scheduled" :removes-own-events
   "set_lighting_budget" :settings-admit
   "set_lighting_scene" :admit-scene
   "configure_energy_scene" :policy-gap
   "configure_energy_autopilot" :policy-gap
   "configure_auto" :policy-gap})

(def ^:private action-coverage
  "How each scheduled action meets the check when it fires."
  {:param/set `parameter-writes-cannot-raise-a-bound-limit
   :param/ramp `parameter-writes-cannot-raise-a-bound-limit
   :cue/launch `launches-and-stops-cannot-raise-a-bound-limit
   :instance/end `launches-and-stops-cannot-raise-a-bound-limit
   :macro/run `macros-and-staged-drafts-cannot-raise-a-bound-limit
   ;; Value-checked in submit-command like the steps above.
   :effect/launch :submit-command :instance/kill :submit-command :macro/stop :submit-command
   :instance/opacity :submit-command
   :variable/set :submit-command
   :master/set :agent-refused :blackout/set :agent-refused
   :callback :variables-only
   :lighting/scene :admit-scene
   :auto/configure :policy-gap})

(def ^:private reducer-call-sites
  "Each Tolglow call into the shared control reducer outside `show/command`,
  with how it meets the check. A new call site fails this test until it is
  classified here."
  {"tolglow/spatial.clj" [4 "the reducer front itself; its callers are classified"]
   "tolglow/event_subscribers.clj" [3 "submit-command: guard-result over every scheduled step and ramp commit"]
   "tolglow/composition.clj" [1 "macro launches as :composition; an agent may not start a run with a bound member"]
   "tolglow/show/scene_draft.clj" [2 "staged steps on a private copy; apply-draft runs guard-result for its applier"]
   "tolglow/knob_candidates.clj" [1 "audition writes on the private Room model only"]
   "tolglow/lighting_scenes.clj" [1 "scene commands; admit-scene runs admit-bound for the scene's source"]
   "tolglow/routing.clj" [1 "frame-time routing configured by the operator, not an agent"]
   "tolglow/integrations/audio_return.clj" [1 "audio-return voices; source :audio-return"]
   "tolglow/showcase.clj" [1 "showcase launches from the REPL"]
   "tolglow/rig/calibration.clj" [1 "calibration fixture publishes"]
   "tolglow/rig/photometry.clj" [1 "photometry fixture publishes"]
   "tolglow/automation/prediction.clj" [1 "simulated launch on a throwaway copy"]
   "tolglow/authoring/variation_control.clj" [1 "a domain op inside `show/command`"]})

(deftest every-path-to-a-bound-limit-is-classified
  (testing "every agent tool has a regression or a reason"
    (let [tools (into #{} (filter string?) (case-keys (defn-body "tolglow/show.clj" 'dispatch-tool!) 'tool))]
      (is (= (set (keys tool-coverage)) tools)
          "Classify a new agent tool in tool-coverage, with a regression showing it cannot raise a bound limit")
      (doseq [[tool covered] tool-coverage :when (symbol? covered)]
        (is (resolve covered) (str tool " names a missing regression " covered)))))
  (testing "every scheduled action has a regression or a reason"
    (is (= (set (keys action-coverage)) (set subscribers/actions))
        "Classify a new scheduled action in action-coverage")
    (doseq [[action covered] action-coverage :when (symbol? covered)]
      (is (resolve covered) (str action " names a missing regression " covered))))
  (testing "every message the owner applies passes the owner check"
    (let [cycle (defn-body "tolglow/show.clj" 'owner-cycle!)
          calls (filter #(and (seq? %) (= 'apply-message (first %))) (tree-seq coll? seq cycle))
          checked (filter #(and (seq? %) (= '->> (first %)) (some #{'(admit-bound-message model message)} %))
                          (tree-seq coll? seq cycle))]
      (is (= 1 (count calls)) "The owner applies messages in one place")
      (is (= 1 (count checked)) "and checks each one there")))
  (testing "every direct call into the shared reducer is classified"
    (let [root (io/file "src")
          pattern #"\((?:spatial|control)/apply-(?:domain-)?command\b"
          found (into {} (keep (fn [^java.io.File f]
                                 (let [path (str/replace (str (.relativize (.toPath root) (.toPath f))) "\\" "/")
                                       n (count (re-seq pattern (slurp f)))]
                                   (when (and (pos? n) (not= path "tolglow/show.clj")) [path n]))))
                      (filter #(str/ends-with? (str %) ".clj") (file-seq root)))]
      (is (= (update-vals reducer-call-sites first) found)
          "A new or removed reducer call site: classify how it meets the bound-limit check"))))
