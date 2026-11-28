(ns tolglow.integrations.agent-wire-test
  "Wire-schema conformance (#863 P1): the static structured-output count, the
  keyword subset, every example through the real validate-arguments, refusals
  with their reasons, and the request bodies an API adapter sends."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [tolglow.integrations.agent :as agent]
            [tolglow.integrations.agent-wire :as wire]
            [tolglow.integrations.providers :as providers]))

(defn- nodes
  "Every schema node with its path."
  ([schema] (nodes schema []))
  ([schema path]
   (cons [path schema]
         (concat (mapcat (fn [[k child]] (nodes child (conj path "properties" k))) (get schema "properties"))
                 (when-let [items (get schema "items")] (nodes items (conj path "items")))))))

(deftest every-shipped-request-shape-is-within-claude-limits-and-the-wire-subset
  (let [report (wire/report)]
    (doseq [[shape result] report]
      (testing (str shape)
        (is (:ok? result))
        (is (= [] (:exceeded result)))
        (is (= [] (:violations result)))
        (is (= 0 (:optional-parameters result)) "every property is required")
        (is (= 0 (:union-parameters result)) "one type per value")))
    (testing "the counts the research recorded: depth 8 for the proposer, at most 6 strict tools"
      (is (= {:strict-tools 0 :max-depth 8 :mode :output-format}
             (select-keys (:proposer report) [:strict-tools :max-depth :mode])))
      (is (= 4 (:strict-tools (:off-clock report))))
      (is (= 6 (:strict-tools (:every-strict-tool report))))
      (is (<= (:strict-tools (:every-strict-tool report)) (:strict-tools wire/claude-limits)))
      (is (every? pos? (map (:proposer report) [:objects :properties :enum-values])) "grammar-size proxies are reported"))
    (testing "the limits are Claude's documented ones"
      (is (= {:strict-tools 20 :optional-parameters 24 :union-parameters 16} wire/claude-limits))
      (is (re-find #"platform\.claude\.com/docs/en/build-with-claude/structured-outputs" (:doc (meta #'wire/claude-limits))))
      (is (re-find #"2026-11-28" (:doc (meta #'wire/claude-limits)))))))

(deftest every-wire-schema-node-uses-only-keywords-validate-arguments-accepts
  (is (set/subset? wire/wire-keywords agent/schema-keywords))
  (is (empty? (set/intersection wire/wire-keywords wire/owner-bound-keywords)) "the wire carries no bounds")
  (doseq [id wire/schema-ids
          [path node] (nodes (wire/schema id))]
    (testing (str id " " path)
      (is (set/subset? (set (keys node)) wire/wire-keywords))
      (is (string? (get node "type")) "one type per node")
      (when (= "object" (get node "type"))
        (is (false? (get node "additionalProperties")) "every object is closed")
        (is (= (set (keys (get node "properties"))) (set (get node "required"))) "every property is required"))
      (doseq [v (get node "enum")]
        (is (re-matches #"[a-z0-9:/_-]+" v) "enum values are lowercase without spaces")))))

(defn- over-limit-schemas
  "A deliberately over-limit request: 21 strict tools, 25 optional and 17
  union-typed parameters, and one node with every kind of keyword violation."
  []
  (let [optional {"type" "object" "additionalProperties" false "required" []
                  "properties" (into (sorted-map) (for [i (range 25)] [(str "o" i) {"type" "string"}]))}
        unions {"type" "object" "additionalProperties" false
                "required" (mapv #(str "u" %) (range 17))
                "properties" (into (sorted-map) (for [i (range 17)] [(str "u" i) {"type" ["string" "null"]}]))}
        broken {"type" "object" "required" ["level" "mode" "ghost"]
                "properties" {"level" {"type" "number" "minimum" 0 "maximum" 1}
                              "mode" {"type" "string" "enum" ["Persistent" "one shot"]}
                              "ref" {"$ref" "#/$defs/x"}
                              "tag" {"type" "string" "pattern" "^[a-z]+$"}}}
        filler {"type" "object" "additionalProperties" false "required" [] "properties" {}}]
    (into (array-map :optional optional :unions unions :broken broken)
          (for [i (range 18)] [(keyword (str "filler-" i)) filler]))))

(deftest a-deliberately-over-limit-request-fails-the-check
  (let [result (wire/check-schemas :strict-tools (over-limit-schemas))
        problems (set (map (juxt :problem #(or (:keyword %) (:value %) (:property %))) (:violations result)))]
    (is (false? (:ok? result)))
    (is (= {:strict-tools 21 :optional-parameters 27 :union-parameters 17}
           (select-keys result [:strict-tools :optional-parameters :union-parameters]))
        "broken's undeclared ref and tag count as optional too")
    (is (= #{:strict-tools :optional-parameters :union-parameters} (set (map :limit (:exceeded result)))))
    (is (set/subset? #{[:owner-bound "minimum"] [:owner-bound "maximum"] [:unsupported-keyword "$ref"]
                       [:unsupported-keyword "pattern"] [:open-object nil] [:required-unknown "ghost"]
                       [:enum-casing "Persistent"] [:enum-casing "one shot"] [:type-not-single nil]}
                     problems))
    (testing "one schema alone over a single limit fails in reply-format mode too"
      (let [single (wire/check-schemas :output-format {:optional (:optional (over-limit-schemas))})]
        (is (false? (:ok? single)))
        (is (= [{:limit :optional-parameters :count 25 :max 24}] (:exceeded single)))
        (is (= [] (:violations single)))))))

(def ^:private example-record
  "The host record the proposer example was written against."
  (wire/request-record "w-example" {:obs "o-0413" :revision 1844 :epoch 3 :auto-session nil
                                    :source {:adapter :agent :session-id "w"} :pending {:upcoming [] :ramps [] :settled []}}))

(deftest every-wire-example-validates-with-the-real-validate-arguments
  (doseq [id wire/schema-ids]
    (testing (str id)
      (let [example (wire/example id)]
        (is (= [] (agent/validate-arguments (wire/schema id) example)))
        (is (= [] (wire/check-bounds id example)) "and sits within the owner's bounds")
        (is (:ok? (wire/accept id example))))))
  (testing "the proposer example lowers to schedule_events with its musical timings"
    (let [{:keys [ok? proposals]} (wire/admit-proposals example-record (wire/example :proposal))
          [p1 p2] proposals]
      (is ok?)
      (is (= [6 1] (mapv (comp count :events) proposals)))
      (is (= (:basis example-record) (:basis p1) (:basis p2)) "alternatives share the host's basis")
      (is (= "w-example" (:request-id p1)))
      (is (= {:op "param/ramp" :id "br-dim" :label "g-break" :grid {:bars 1} :target "opacity" :to 0.35
              :over {:bars 2} :curve "ease-out" :select {:categories ["intensity"] :pick "all"}}
             (first (:events p1))))
      (is (= {:mode "persistent" :params {:level 0.3 :period 4}} (:launch (nth (:events p1) 2))))
      (is (= {:bar 36} (:at (nth (:events p1) 4))))
      (is (= {:phrases 1} (:grid (first (:events p2)))))
      (is (= {:group "g-break" :intent ["breakdown" "section:break"] :rank 1 :optional? false
              :on-failure :cancel-rest :check :continuous :until {:phrase 0 :bar 40 :beat 0}}
             (dissoc (first (:groups p1)) :guard))))))

(defn- event-path [& more] (into [:proposals 0 :events 0] more))

(defn- errors-for [reply]
  (:errors (wire/accept :proposal reply)))

(defn- example-event-edit [f]
  (update-in (wire/example :proposal) [:proposals 0 :events 0] f))

(deftest replies-outside-the-wire-are-refused-with-their-reasons
  (testing "a dropped required field"
    (is (= [{:path (event-path "timing") :message "Required tool argument is missing"}]
           (errors-for (example-event-edit #(dissoc % :timing))))))
  (testing "a millisecond timing: neither an ms key nor an ms unit exists on the wire"
    (is (= [{:path (event-path :timing :ms) :message "Unknown tool argument"}]
           (errors-for (example-event-edit #(assoc-in % [:timing :ms] 500)))))
    (let [[error] (errors-for (example-event-edit #(assoc-in % [:over :unit] "ms")))]
      (is (= (event-path :over :unit) (:path error)))
      (is (re-find #"outside enum; expected one of \"none\", \"beats\", \"bars\", \"phrases\"" (:message error)))))
  (testing "an extra key"
    (is (= [{:path (event-path :priority) :message "Unknown tool argument"}]
           (errors-for (example-event-edit #(assoc % :priority 10))))))
  (testing "a wrong enum value names the choices"
    (let [[error] (errors-for (example-event-edit #(assoc % :curve "wobble")))]
      (is (= (event-path :curve) (:path error)))
      (is (re-find #"\"linear\", \"ease-in\"" (:message error)))))
  (testing "a goal's order on a group header and a rank where none is declared"
    (let [reply (update-in (wire/example :proposal) [:proposals 0 :groups 0] #(-> % (dissoc :rank) (assoc :order 1)))]
      (is (= #{{:path [:proposals 0 :groups 0 "rank"] :message "Required tool argument is missing"}
               {:path [:proposals 0 :groups 0 :order] :message "Unknown tool argument"}}
             (set (errors-for reply)))))
    (is (= [{:path [:proposals 0 :cancel 0 :rank] :message "Unknown tool argument"}]
           (errors-for (assoc-in (wire/example :proposal) [:proposals 0 :cancel 0 :rank] 1)))))
  (testing "a revision is an integer"
    (is (= [{:path [:proposals 0 :basis_rev] :message "Expected JSON type integer"}]
           (errors-for (assoc-in (wire/example :proposal) [:proposals 0 :basis_rev] 1844.5)))))
  (testing "a strict tool's arguments follow the same rule"
    (is (= [{:path [:move 0 :timing :kind] :message (str "Value is outside enum; expected one of \"at\", \"in\", \"grid\"")}]
           (:errors (wire/accept :move (assoc-in (wire/example :move) [:move 0 :timing :kind] "now")))))))

(deftest mixed-case-enum-values-are-lowercased-before-validation
  (let [reply (example-event-edit #(assoc % :op "Param/Ramp" :curve "EASE-OUT" :target "Opacity"
                                          :over {:unit "Bars" :n 2}))
        {:keys [ok? value]} (wire/accept :proposal reply)]
    (is ok?)
    (is (= {:op "param/ramp" :curve "ease-out" :target "opacity" :over {:unit "bars" :n 2}}
           (select-keys (get-in value [:proposals 0 :events 0]) [:op :curve :target :over])))
    (is (= "Breakdown a phrase early: drop the lift, go dark and slow, keep the human's wash."
           (get-in value [:proposals 0 :note])) "free text keeps its casing"))
  (is (= (event-path :curve) (:path (first (errors-for (example-event-edit #(assoc % :curve "WOBBLE"))))))
      "an unknown value stays unknown after lowercasing"))

(deftest the-owner-applies-the-bounds-the-wire-cannot-carry
  (let [p (wire/example :proposal)
        refused (fn [reply] (let [r (wire/admit-proposals example-record reply)] [(:stage r) (:errors r)]))]
    (is (= [:bounds [{:path [:proposals] :message "Too many items"}]]
           (refused (update p :proposals #(vec (take 4 (cycle %)))))))
    (is (= [:bounds [{:path [:proposals 0 :confidence] :message "Number is above maximum"}]]
           (refused (assoc-in p [:proposals 0 :confidence] 1.4))))
    (is (= [:bounds [{:path [:proposals 0 :groups 0 :intent] :message "Too many items"}]]
           (refused (assoc-in p [:proposals 0 :groups 0 :intent] ["a" "b" "c" "d" "e"]))))
    (is (= [:bounds [{:path (event-path :timing :bar) :message "Number is below minimum"}]]
           (refused (assoc-in p [:proposals 0 :events 0 :timing :bar] -1))))
    (testing "per-op rules refuse at lowering, with the wire path"
      (is (= [:lowering [{:path (event-path :timing) :message "in takes one nonzero unit of phrase, bar or beat"}]]
             (refused (assoc-in p [:proposals 0 :events 0 :timing] {:kind "in" :phrase 0 :bar 1 :beat 2}))))
      (is (= [:lowering [{:path (event-path :group) :message "Names no group header in this proposal"}]]
             (refused (assoc-in p [:proposals 0 :events 0 :group] "g-elsewhere"))))
      (is (= [:lowering [{:path [:proposals 0 :events 2 :params 0 :rgb] :message "An rgb value has three numbers"}]]
             (refused (assoc-in p [:proposals 0 :events 2 :params 0] {:name "tint" :kind "rgb" :num 0 :rgb [1 0] :flag false :choice ""}))))
      (is (= [:lowering [{:path [:proposals 0 :events 2 :select] :message "This op needs select ids or categories"}]]
             (refused (assoc-in p [:proposals 0 :events 2 :select :ids] [])))))
    (testing "proposal IDs are unique and short, since they name the posted command"
      (is (= [:lowering [{:path [:proposals 0 :id] :message "Proposal ID \"p1\" is used twice in this reply"}
                         {:path [:proposals 1 :id] :message "Proposal ID \"p1\" is used twice in this reply"}]]
             (refused (assoc-in p [:proposals 1 :id] "p1"))))
      (is (= [:lowering [{:path [:proposals 0 :id] :message "A proposal ID is 1 to 64 letters, digits, '.', ':', '_' or '-'"}]]
             (refused (assoc-in p [:proposals 0 :id] (apply str (repeat 65 "x")))))))))

(deftest the-basis-comes-from-the-host-record-not-the-reply
  (let [p (wire/example :proposal)
        refused (fn [record reply] (let [r (wire/admit-proposals record reply)] [(:stage r) (:errors r)]))]
    (testing "a forged revision or observation echo is refused"
      (is (= [:basis [{:path [:proposals 0 :basis_rev] :code :basis-mismatch :message "Request w-example sent revision 1844"}]]
             (refused example-record (assoc-in p [:proposals 0 :basis_rev] 1845))))
      (is (= [:basis [{:path [:proposals 1 :basis_obs] :code :basis-mismatch :message "Request w-example sent observation \"o-0413\""}]]
             (refused example-record (assoc-in p [:proposals 1 :basis_obs] "o-9999")))))
    (testing "an echo that matches the show's current revision but not the request is still refused"
      (let [record (wire/request-record "w-2" (assoc (:basis example-record) :revision 1700))]
        (is (= #{[:proposals 0 :basis_rev] [:proposals 1 :basis_rev]}
               (set (map :path (second (refused record p))))))))))

(deftest a-basis-goes-stale-on-epoch-or-auto-session-not-on-revision
  (let [agent {:adapter :agent :session-id "run-1" :run-id "turn-4"}
        model {:epoch 3 :revision 40 :auto {:state {:owner-id "auto/3/17"}} :transport {:beat 12.0}}
        record (wire/request-record "w-1" (wire/request-basis model agent "o-1"))]
    (is (= {:obs "o-1" :revision 40 :epoch 3 :auto-session "auto/3/17" :beat 12.0
            :source {:adapter :agent :session-id "run-1"} :pending {:upcoming [] :ramps [] :settled []}}
           (:basis record))
        "the record keeps the source it was taken for, so request and check cannot disagree")
    (is (= [] (wire/check-basis record (assoc model :revision 97))) "unrelated commands only bump the revision")
    (testing "the transport back behind the request's beat (a backward seek or loop wrap) is stale; a phase nudge is not"
      (is (= [] (wire/check-basis record (assoc-in model [:transport :beat] 11.8))))
      (is (= [{:path [:basis :beat] :code :stale-basis
               :message "The transport moved back from beat 12.0 to beat 0.0 since request w-1"}]
             (wire/check-basis record (assoc-in model [:transport :beat] 0.0)))))
    (is (= [{:path [:basis :epoch] :code :stale-basis :message "The epoch changed since request w-1"}]
           (wire/check-basis record (assoc model :epoch 4))))
    (is (= [:basis :auto-session] (:path (first (wire/check-basis record (assoc-in model [:auto :state :owner-id] "auto/3/18"))))))
    (is (thrown? clojure.lang.ExceptionInfo (wire/request-record "" (:basis record))))
    (is (thrown? clojure.lang.ExceptionInfo (wire/request-record "w-1" (dissoc (:basis record) :source))))
    (is (thrown? clojure.lang.ExceptionInfo (wire/request-basis model {:session-id "run-1"} "o-1")))))

(deftest an-answer-after-its-deadline-beat-is-late
  (let [model {:epoch 3 :revision 40 :transport {:beat 12.0}}
        basis (wire/request-basis model {:adapter :agent :session-id "run-1"} "o-1")
        record (wire/request-record "w-1" basis {:deadline-beat 16})]
    (is (= 16.0 (:deadline-beat record)))
    (is (= [] (wire/check-basis record (assoc-in model [:transport :beat] 15.75))))
    (is (= [{:path [:deadline-beat] :code :late
             :message "Request w-1 was due by beat 16.0; the transport is at beat 16.0"}]
           (wire/check-basis record (assoc-in model [:transport :beat] 16.0))))
    (is (= [:late :stale-basis] (mapv :code (wire/check-basis record (-> model (assoc :epoch 4) (assoc-in [:transport :beat] 17.0)))))
        "lateness comes first and does not hide a stale basis")
    (is (= [] (wire/check-basis (wire/request-record "w-2" basis) (assoc-in model [:transport :beat] 1e6)))
        "a record without a deadline is never late")
    (is (= [:stale-basis] (mapv :code (wire/check-basis record (assoc-in model [:transport :beat] 2.0))))
        "a loop wrap from 12 back to 2 cannot push the deadline out: the answer is refused")
    (doseq [bad [-1 "16" ##Inf ##NaN]]
      (is (thrown? clojure.lang.ExceptionInfo (wire/request-record "w-1" basis {:deadline-beat bad})) (pr-str bad)))))

;; Requests ------------------------------------------------------------------------

(defn- fixture [file] (slurp (io/resource (str "tolglow/integrations/agent_wire/" file))))

(defn- with-schema
  "The recorded body with its \"$wire:<id>\" placeholder replaced by that schema."
  [body]
  (walk/postwalk #(if (and (string? %) (.startsWith ^String % "$wire:")) (wire/schema (keyword (subs % 6))) %) body))

(def ^:private messages
  [{:role :system :content "Propose up to three alternative lighting decisions as tolglow-agent-proposals/1."}
   {:role :user :content "{\"obs\":\"o-0001\",\"rev\":0}"}])

(def ^:private placeholder-credential "recorded-transport-placeholder")

(defn- body [request] (wire/read-ordered-json (:body request)))

(deftest a-claude-proposer-request-carries-the-wire-schema-as-its-output-format
  (let [request (wire/reply-request {:provider :anthropic :model "claude-haiku-5-5" :max-tokens 4096}
                                    :proposal messages placeholder-credential)
        sent (body request)]
    (is (= (with-schema (json/read-str (fixture "anthropic-proposer.request.json"))) sent)
        "the recorded request body, schema included")
    (is (= (wire/schema :proposal) (get-in sent ["output_config" "format" "schema"])))
    (testing "the schema keeps its declared key order on the wire"
      (is (= ["id" "group" "op" "timing" "select"]
             (take 5 (keys (get-in sent ["output_config" "format" "schema" "properties" "proposals" "items"
                                         "properties" "events" "items" "properties"]))))))
    (is (empty? (select-keys sent ["temperature" "top_p" "top_k" "tool_choice" "tools"]))
        "no sampling parameters and no forced tool choice")
    (is (= "[REDACTED]" (get-in (providers/redact-request request) [:headers "x-api-key"])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no sampling parameters"
                          (wire/reply-request {:provider :anthropic :model "claude-haiku-5-5" :max-tokens 4096 :temperature 0.7}
                                              :proposal messages placeholder-credential)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no forced tool choice"
                          (wire/reply-request {:provider :anthropic :model "claude-haiku-5-5" :max-tokens 4096
                                               :tool-choice {:type "any"}}
                                              :proposal messages placeholder-credential)))
    (testing "max_tokens is explicit and sized for three alternatives"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"explicit :max-tokens of at least 3400"
                            (wire/reply-request {:provider :anthropic :model "claude-haiku-5-5"}
                                                :proposal messages placeholder-credential)))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"at least 3400"
                            (wire/reply-request {:provider :anthropic :model "claude-haiku-5-5" :max-tokens 2048}
                                                :proposal messages placeholder-credential)))
      (is (= 8192 (get (body (wire/reply-request {:provider :anthropic :model "claude-opus-5-5"
                                                  :max-tokens wire/proposer-max-tokens}
                                                 :proposal messages placeholder-credential)) "max_tokens"))))
    (testing "a reply request declares no tools, so the static count is the whole request"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"only its reply schema"
                            (wire/reply-request {:provider :anthropic :model "claude-haiku-5-5" :max-tokens 4096
                                                 :tools [(wire/strict-tool :move)]}
                                                :proposal messages placeholder-credential))))
    (testing "effort sits beside the format in output_config"
      (is (= {"effort" "low" "format" {"type" "json_schema" "schema" (wire/schema :proposal)}}
             (get (body (wire/reply-request {:provider :anthropic :model "claude-haiku-5-5" :max-tokens 4096 :effort "low"}
                                            :proposal messages placeholder-credential))
                  "output_config"))))))

(deftest an-openrouter-proposer-request-asks-only-for-providers-that-honour-the-schema
  (let [request (wire/reply-request {:provider :openrouter :model "anthropic/claude-haiku-5-5" :max-tokens 4096}
                                    :proposal messages placeholder-credential)
        sent (body request)]
    (is (= (with-schema (json/read-str (fixture "openrouter-proposer.request.json"))) sent))
    (is (= {"require_parameters" true} (get sent "provider")))
    (is (true? (get-in sent ["response_format" "json_schema" "strict"])))
    (is (empty? (select-keys sent ["temperature" "top_p" "top_k" "tool_choice"])))))

(deftest off-clock-schemas-go-out-as-strict-tools-with-the-default-tool-choice
  (doseq [[provider tools-at schema-key] [[:anthropic identity "input_schema"]
                                          [:openrouter #(get % "function") "parameters"]]]
    (testing (str provider)
      (let [sent (body (wire/tools-request {:provider provider :model "m"} messages placeholder-credential))
            tools (mapv tools-at (get sent "tools"))]
        (is (= ["author" "configure" "move" "inspect"] (mapv #(get % "name") tools)))
        (is (every? #(true? (get % "strict")) tools))
        (is (= (mapv wire/schema [:author :configure :move :inspect]) (mapv #(get % schema-key) tools)))
        (is (not (contains? sent "tool_choice")) "auto, the default")
        (is (= (when (= provider :openrouter) false) (get sent "parallel_tool_calls"))
            "OpenAI-compatible strict tools are asked for one call at a time")
        (is (empty? (select-keys sent ["temperature" "top_p" "top_k"])))))))

(defn- recorded-reply
  "invoke! a reply request through a capture transport answering with file."
  [provider file]
  (let [config (case provider
                 :anthropic {:provider :anthropic :model "claude-haiku-5-5" :max-tokens 4096}
                 :openrouter {:provider :openrouter :model "anthropic/claude-haiku-5-5" :max-tokens 4096})]
    (wire/read-reply (providers/invoke! (fn [_] {:status 200 :body (fixture file)})
                                        (wire/reply-request config :proposal messages placeholder-credential)))))

(deftest stop-reasons-are-refused-before-parsing
  (is (:ok? (recorded-reply :anthropic "anthropic-proposer.reply.json")))
  (is (:ok? (recorded-reply :openrouter "openrouter-proposer.reply.json")))
  (testing "a thinking block before the text is not part of the reply"
    (let [{:keys [ok? value]} (recorded-reply :anthropic "anthropic-thinking.reply.json")]
      (is ok?)
      (is (= ["p1" "p2"] (mapv :id (:proposals value))))))
  (is (= {:ok? false :code :truncated
          :errors [{:path [] :message "The reply stopped at max_tokens before it was complete"}]}
         (recorded-reply :anthropic "anthropic-truncated.reply.json")))
  (is (= :truncated (:code (recorded-reply :openrouter "openrouter-length.reply.json"))) "OpenAI finish_reason length")
  (is (= {:ok? false :code :refused :errors [{:path [] :message "The provider refused the request: cyber"}]}
         (recorded-reply :anthropic "anthropic-refusal.reply.json")))
  (is (= :refused (:code (wire/read-reply {:ok? true :text "" :response {:choices [{:finish_reason "stop"
                                                                                     :message {:refusal "No."}}]}}))))
  (is (= :unexpected-stop (:code (wire/read-reply {:ok? true :text "" :response {:stop_reason "tool_use" :content []}}))))
  (is (= :provider-error (:code (wire/read-reply {:ok? false :error {:code :http-error :status 529}})))))

;; Streamed replies ------------------------------------------------------------

(defn- sse [& payloads] (apply str (map #(str "data: " (json/write-str %) "\n\n") payloads)))

(def ^:private recorded-text
  "The proposer reply text of the recorded Claude reply, sent here in pieces."
  (get-in (json/read-str (fixture "anthropic-proposer.reply.json") :key-fn keyword) [:content 0 :text]))

(defn- pieces [text n] (map #(apply str %) (partition-all n text)))

(defn- anthropic-stream [stop & {:keys [stop-details terminal?] :or {terminal? true}}]
  (apply sse (concat
              [{:type "message_start" :message {:id "msg_stream_1" :usage {:input_tokens 900}}}
               {:type "content_block_start" :index 0 :content_block {:type "thinking" :thinking ""}}
               {:type "content_block_delta" :index 0 :delta {:type "thinking_delta" :thinking "Weigh the lift."}}
               {:type "content_block_stop" :index 0}
               {:type "content_block_start" :index 1 :content_block {:type "text" :text ""}}]
              (for [piece (pieces recorded-text 97)]
                {:type "content_block_delta" :index 1 :delta {:type "text_delta" :text piece}})
              [{:type "content_block_stop" :index 1}
               {:type "message_delta" :delta (cond-> {:stop_reason stop} stop-details (assoc :stop_details stop-details))
                :usage {:output_tokens 700}}]
              (when terminal? [{:type "message_stop"}]))))

(defn- openai-stream [finish & {:keys [refusal]}]
  (str (apply sse (concat
                   (if refusal
                     [{:choices [{:index 0 :delta {:role "assistant" :refusal refusal}}]}]
                     (for [piece (pieces recorded-text 97)] {:choices [{:index 0 :delta {:content piece}}]}))
                   [{:choices [{:index 0 :delta {} :finish_reason finish}]}
                    {:choices [] :usage {:prompt_tokens 900 :completion_tokens 700}}]))
       "data: [DONE]\n\n"))

(defn- streamed-reply
  "invoke! a streamed reply request through a capture transport answering with
  body; return the request sent and read-reply's result."
  [provider body]
  (let [sent (atom [])
        config (case provider
                 :anthropic {:provider :anthropic :model "claude-haiku-5-5" :max-tokens 4096 :stream? true}
                 :openrouter {:provider :openrouter :model "anthropic/claude-haiku-5-5" :max-tokens 4096 :stream? true})
        result (providers/invoke! (fn [request] (swap! sent conj request) {:status 200 :body body})
                                  (wire/reply-request config :proposal messages placeholder-credential))]
    {:sent @sent :reply (wire/read-reply result)}))

(deftest streamed-replies-read-their-stop-reason-from-the-stream
  (let [expected (:value (recorded-reply :anthropic "anthropic-proposer.reply.json"))]
    (testing "a complete Claude stream reads like the recorded non-streamed reply"
      (let [{:keys [sent reply]} (streamed-reply :anthropic (anthropic-stream "end_turn"))
            body (wire/read-ordered-json (:body (first sent)))]
        (is (= 1 (count sent)))
        (is (true? (get body "stream")))
        (is (= "text/event-stream" (get-in (first sent) [:headers "accept"])))
        (is (= {:ok? true :value expected} reply) "thinking deltas are not reply text")))
    (testing "a complete OpenRouter stream"
      (is (= {:ok? true :value expected} (:reply (streamed-reply :openrouter (openai-stream "stop")))))))
  (testing "stop reasons are refused before parsing, as for non-streamed replies"
    (is (= {:ok? false :code :truncated
            :errors [{:path [] :message "The reply stopped at max_tokens before it was complete"}]}
           (:reply (streamed-reply :anthropic (anthropic-stream "max_tokens")))))
    (is (= :truncated (:code (:reply (streamed-reply :openrouter (openai-stream "length"))))))
    (is (= {:ok? false :code :refused :errors [{:path [] :message "The provider refused the request: cyber"}]}
           (:reply (streamed-reply :anthropic (anthropic-stream "refusal" :stop-details {:type "refusal" :category "cyber"})))))
    (is (= {:ok? false :code :refused
            :errors [{:path [] :message "The provider refused the request: I can't help with that."}]}
           (:reply (streamed-reply :openrouter (openai-stream "stop" :refusal "I can't help with that."))))
        "an OpenAI refusal arrives as refusal deltas with finish_reason stop")
    (is (= :refused (:code (:reply (streamed-reply :openrouter (openai-stream "content_filter"))))))
    (is (= {:ok? false :code :unexpected-stop
            :errors [{:path [] :message "The reply ended with stop reason \"tool_use\""}]}
           (:reply (streamed-reply :anthropic (anthropic-stream "tool_use"))))))
  (testing "a stream that ends before its completion marker is a provider error that keeps the stream's reason"
    (let [{:keys [reply]} (streamed-reply :anthropic (anthropic-stream "end_turn" :terminal? false))]
      (is (= :provider-error (:code reply)))
      (is (= :truncated-stream (get-in reply [:error :code])))))
  (testing "the stream path checks the stream itself, not only the caller's :ok?"
    (let [events (:events (providers/parse-sse :anthropic (anthropic-stream "end_turn")))
          text (apply str (map :text (filter #(= :text/delta (:type %)) events)))]
      (is (:ok? (wire/read-reply {:ok? true :text text :events events})))
      (is (= {:code :truncated-stream :message "The stream ended before its completion marker"}
             (:error (wire/read-reply {:ok? true :text text :events (vec (remove #(= :done (:type %)) events))}))))
      (is (= {:code :malformed-stream-event}
             (select-keys (:error (wire/read-reply {:ok? true :text text
                                                    :events (conj (vec (butlast events))
                                                                  {:type :error :error {:code :malformed-stream-event}})}))
                          [:code])))))
  (testing "a reply request is still not streamed unless asked"
    (is (false? (get (body (wire/reply-request {:provider :anthropic :model "claude-haiku-5-5" :max-tokens 4096}
                                               :proposal messages placeholder-credential))
                     "stream")))))

(deftest the-research-set-links-to-the-shipped-schemas-instead-of-copying-them
  ;; One source of truth: docs/research/native-agent points at resources/agent/wire/.
  (is (not (.exists (io/file "docs/research/native-agent/schemas/wire"))))
  (doseq [file ["README.md" "model.md" "interface.md" "orchestration.md" "cross-rig.md"]
          :let [text (slurp (io/file "docs/research/native-agent" file))]
          [_ target] (re-seq #"\]\((\.\./\.\./\.\./resources/agent/wire/[^)#]+)\)" text)]
    (is (.exists (io/file "docs/research/native-agent" target)) (str file " links " target))))
