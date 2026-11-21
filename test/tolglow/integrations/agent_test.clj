(ns tolglow.integrations.agent-test
  (:require [clojure.test :refer :all]
            [clojure.data.json :as json]
            [afterglow.control.model :as model]
            [tolglow.integrations.agent :as agent]))

(def config {:provider :self-hosted :endpoint "http://unused.test/v1/chat/completions" :model "fixture"})
(defn event [data] (str "data: " (json/write-str data) "\n\n"))
(defn response
  ([text] (response text []))
  ([text calls]
   (str (event {:choices [{:index 0 :delta {:content text}}]})
        (apply str (map-indexed (fn [index {:keys [id name arguments]}]
                                 (event {:choices [{:index 0 :delta
                                                    {:tool_calls [{:index index :id id :function {:name name :arguments (json/write-str arguments)}}]}}]})) calls))
        "data: [DONE]\n\n")))

(defn scripted-transport [replies requests]
  (let [remaining (atom replies)]
    (fn [request on-chunk! _]
      (swap! requests conj (json/read-str (:body request) :key-fn keyword))
      (let [reply (first @remaining)]
        (swap! remaining next)
        (when-not reply (throw (ex-info "Unexpected provider round" {})))
        (on-chunk! reply)
        {:status 200}))))

(deftest streamed-agent-can-inspect-publish-place-and-launch-through-shared-control
  (let [show (atom (model/new-show)) requests (atom []) events (atom [])
        replies [(response "Inspecting." [{:id "inspect" :name "inspect_show" :arguments {}}])
                 (response "Building the cue."
                           [{:id "effect" :name "publish_effect" :arguments {:definition {:id "red" :version 1 :params {}}}}
                            {:id "cue" :name "put_cue" :arguments {:cue {:id "red-cue" :version 1 :effect-id "red"}}}
                            {:id "launch" :name "launch_cue" :arguments {:cue-id "red-cue" :launch {:mode "persistent"}}}])
                 (response "The red cue is running.")]
        dispatch (fn [name arguments metadata]
                   (if (= name "inspect_show")
                     {:instances (count (:instances @show))}
                     (let [body (case name
                                  "publish_effect" {:op :registry/publish :kind :effects :definition (:definition arguments)}
                                  "put_cue" {:op :cue/put :cue (:cue arguments)}
                                  "launch_cue" {:op :cue/launch :cue-id (:cue-id arguments) :launch {:mode :persistent}})
                           result (model/apply-command @show
                                                       (merge {:show-id :main :epoch 0 :time-ms 0 :source {:type :agent}}
                                                              body (select-keys metadata [:command-id])))]
                       (reset! show (:model result))
                       (:ack result))))
        result (agent/run! config nil [{:role :user :content "Build and run a red show."}]
                           agent/default-tools dispatch #(swap! events conj %)
                           {:transport (scripted-transport replies requests) :run-id "show-plan"})]
    (is (= :completed (:status result)))
    (is (= 3 (:rounds result)))
    (is (= 4 (:tool-calls result)))
    (is (= 1 (count (:instances @show))))
    (is (= "red-cue" (-> @show :instances vals first :cue-id)))
    (is (= ["inspect_show" "publish_effect" "put_cue" "launch_cue"]
           (mapv :name (filter #(= :tool/start (:type %)) @events))))
    (is (= :agent/done (:type (last @events))))
    (is (= "tool" (get-in @requests [1 :messages 2 :role])))))

(deftest unknown-or-invalid-tool-arguments-never-reach-dispatch
  (let [calls (atom []) events (atom [])
        replies [(response "" [{:id "unknown" :name "arbitrary_eval" :arguments {:code "danger"}}
                                {:id "bad" :name "launch_cue" :arguments {:cue-id 42}}])
                 (response "I need a valid cue id.")]
        result (agent/run! config nil [{:role :user :content "Go"}] agent/default-tools
                           #(swap! calls conj [%1 %2 %3]) #(swap! events conj %)
                           {:transport (scripted-transport replies (atom []))})]
    (is (= :completed (:status result)))
    (is (empty? @calls))
    (is (= [:invalid-tool-call :invalid-tool-call]
           (mapv #(get-in % [:result :error]) (filter #(= :tool/result (:type %)) @events))))))

(deftest repeated-tool-identities-reuse-results-but-changed-arguments-are-rejected
  (let [calls (atom 0) events (atom [])
        tool {:id "one" :name "inspect_show" :arguments {}}
        replies [(response "" [tool]) (response "" [tool])
                 (response "" [(assoc tool :name "configure_auto" :arguments {:configuration {}})])
                 (response "Done")]
        result (agent/run! config nil [{:role :user :content "Go"}] agent/default-tools
                           (fn [_ _ _] (swap! calls inc) {:ok? true}) #(swap! events conj %)
                           {:transport (scripted-transport replies (atom []))})]
    (is (= :completed (:status result)))
    (is (= 1 @calls))
    (is (= [false true false] (mapv :replayed? (filter #(= :tool/result (:type %)) @events))))
    (is (= :tool-id-conflict (->> @events (filter #(= :tool/result (:type %))) last :result :error)))))

(deftest tool-and-round-limits-and-cancellation-are-enforced
  (let [call {:id "one" :name "inspect_show" :arguments {}}
        requests (atom []) cancelled? (atom true)]
    (is (= :cancelled (:status (agent/run! config nil [{:role :user :content "Go"}] agent/default-tools
                                          (fn [& _] {}) (fn [_]) {:cancelled? cancelled?
                                                                   :transport (scripted-transport [] requests)}))))
    (is (empty? @requests))
    (is (= :tool-limit (:error (agent/run! config nil [{:role :user :content "Go"}] agent/default-tools
                                         (fn [& _] (throw (Exception. "Must not run"))) (fn [_])
                                         {:max-tool-calls 1 :transport (scripted-transport [(response "" [call (assoc call :id "two")])] (atom []))}))))
    (is (= :round-limit (:error (agent/run! config nil [{:role :user :content "Go"}] agent/default-tools
                                          (fn [& _] {}) (fn [_])
                                          {:max-rounds 1 :transport (scripted-transport [(response "" [call])] (atom []))}))))))

(deftest schema-validation-covers-nested-shape-and-rejects-unsupported-constructs
  (let [schema {:type "object" :required ["values"] :additionalProperties false
                :properties {:values {:type "array" :minItems 1 :maxItems 2
                                      :items {:type "number" :minimum 0 :maximum 1}}}}]
    (is (empty? (agent/validate-arguments schema {:values [0 0.5]})))
    (is (seq (agent/validate-arguments schema {:values [2]})))
    (is (seq (agent/validate-arguments schema {:values [] :extra true})))
    (is (seq (agent/validate-arguments {:oneOf []} {})))
    (is (seq (agent/validate-arguments {:type "object" :additionalProperties {:type "string"}} {})))))
