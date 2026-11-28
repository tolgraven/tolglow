(ns tolglow.agent-log-test
  "The per-decision agent record and decision replay (#856). Decisions come
  from the real tolglow.integrations.agent/run! with a recorded provider
  transport; the show cases dispatch through show/dispatch-tool! on a
  headless runtime with captured output. No provider, device or output is
  opened."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [tolglow.agent-log :as log]
            [tolglow.agent-log.writer :as writer]
            [tolglow.integrations.agent :as agent]
            [tolglow.rehearsal :as rehearsal]
            [tolglow.rehearsal.agent-script :as script]
            [tolglow.rehearsal.passage :as passage]
            [tolglow.show :as show])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-log []
  (str (.toFile (Files/createTempDirectory "agent-log" (make-array FileAttribute 0))) "/session.jsonl"))

(defn- sse-event [data] (str "data: " (json/write-str data) "\n\n"))

(defn- sse-round [text calls]
  (str (sse-event {:choices [{:index 0 :delta {:content text}}]})
       (apply str (map-indexed (fn [i {:keys [id name arguments]}]
                                 (sse-event {:choices [{:index 0 :delta {:tool_calls [{:index i :id id :function {:name name :arguments (json/write-str arguments)}}]}}]}))
                               calls))
       "data: [DONE]\n\n"))

(def ^:private config {:provider :self-hosted :endpoint "http://recorded.invalid/v1/chat/completions" :model "recorded"})

(def ^:private calls
  [{:id "c1" :name "set_parameter" :arguments {:instance-id "i-7" :param "level" :value 0.5}}
   {:id "c2" :name "launch_cue" :arguments {:cue-id "wash" :launch {:mode "one-shot"}}}
   {:id "c3" :name "launch_cue" :arguments {:cue-id 4}}])

(defn- capture-dispatch [sent]
  (fn [tool arguments metadata]
    (swap! sent conj [tool arguments (:command-id metadata)])
    (if (= "set_parameter" tool)
      {:status :accepted :revision 12}
      {:status :rejected :code :unknown-cue :message "No such cue"})))

(defn- run-recorded!
  "Run two recorded rounds through agent/run!; return the decisions handed
  to :on-decision and the commands dispatched."
  [& [{:keys [credential messages rounds on-decision max-tool-calls]}]]
  (let [decisions (atom []) sent (atom [])
        outcome (agent/run! config credential (or messages [{:role :user :content "Lift the wash."}]) agent/default-tools
                            (capture-dispatch sent) (fn [_])
                            {:transport (rehearsal/recorded-transport (or rounds [(sse-round "Lifting." calls) (sse-round "Done." [])]))
                             :run-id "night-1:r1" :conversation-id "night-1" :max-tool-calls (or max-tool-calls 32)
                             :on-decision (fn [d] (swap! decisions conj d) (when on-decision (on-decision d)))})]
    {:outcome outcome :decisions @decisions :sent @sent}))

(def ^:private context
  {:session {:show "main" :epoch 2 :runtime "studio-1"}
   :actor {:role "scripted" :profile "rehearsal"}
   :clock {:observed_beat 132.0 :frame 51290 :rev 1844}})

(def ^:private basis {:epoch 2 :rev 1844})

(defn- write-log [decisions]
  (let [path (temp-log) w (writer/open! path)
        built (mapv #(log/decision-record context %) decisions)]
    (doseq [entry built] (is (writer/log! w entry)))
    (is (:drained? (writer/close! w)))
    {:path path :built built}))

(deftest a-round-trip-writes-and-reads-back-each-decision
  (let [{:keys [outcome decisions sent]} (run-recorded!)
        {:keys [path built]} (write-log decisions)
        records (writer/read-records path)
        blob (writer/blob-loader path)]
    (is (= :completed (:status outcome)))
    (is (= 2 (count decisions)) "one decision per provider round")
    (is (= (mapv :record built) records) "a record reads back equal to what was built")
    (is (every? empty? (map log/validate-record records)))
    (let [[first-round final] records]
      (is (= "tolglow-agent-log/1" (:schema first-round)))
      (is (= {:id "night-1" :show "main" :epoch 2 :runtime "studio-1" :run "night-1:r1" :conversation "night-1"}
             (:session first-round)))
      (is (= ["night-1:r1/0" "night-1:r1/1"] (mapv :decision records)))
      (is (= {:role "scripted" :profile "rehearsal" :backend "self-hosted" :model "recorded"} (:actor first-round)))
      (testing "the raw reply and the request are content-addressed blobs"
        (is (= (sse-round "Lifting." calls) (blob (get-in first-round [:output :raw]))))
        (is (= "Lift the wash." (get-in (json/read-str (blob (get-in first-round [:request :ref])) :key-fn keyword)
                                        [:messages 0 :content])))
        (is (= (get-in first-round [:request :ref]) (get-in first-round [:observation :snapshot_ref]))
            "without a resolver the request messages are the observation"))
      (testing "validated commands carry their command IDs, validity and bounded receipts"
        (is (= [["night-1:r1/c1" "set_parameter" true] ["night-1:r1/c2" "launch_cue" true] ["night-1:r1/c3" "launch_cue" false]]
               (mapv (juxt :command-id :op :valid) (:commands first-round))))
        (is (= {:status "accepted" :revision 12} (dissoc (get-in first-round [:commands 0 :receipt]) :digest)))
        (is (= [["set_parameter" {:instance-id "i-7" :param "level" :value 0.5} "night-1:r1/c1"]
                ["launch_cue" {:cue-id "wash" :launch {:mode "one-shot"}} "night-1:r1/c2"]] sent)
            "the invalid call never reached the dispatcher")
        (is (= "rejected" (get-in first-round [:validation :status])))
        (is (= [{:call "c3" :path ["cue-id"] :message "Expected JSON type string"}] (get-in first-round [:validation :errors]))))
      (testing "immediate outcomes and timing"
        (is (= [{:event "night-1:r1/c2" :status "rejected" :reason "unknown-cue"}
                {:event "night-1:r1/c3" :status "rejected" :reason "invalid-tool-call"}] (:outcomes first-round)))
        (is (every? #(and (number? %) (<= 0 %)) (vals (get-in first-round [:clock :latency_ms]))))
        (is (= #{:policy :admit} (set (keys (get-in first-round [:clock :latency_ms]))))))
      (is (= [] (:commands final)))
      (is (= "accepted" (get-in final [:validation :status]))))))

(deftest decision-replay-reproduces-the-logged-commands-without-a-model
  (let [{:keys [decisions sent]} (run-recorded!)
        {:keys [path]} (write-log decisions)
        [record] (writer/read-records path)
        options {:blob (writer/blob-loader path) :basis basis}
        replayed (log/replay-decision record options)]
    (is (:ok? replayed) (pr-str replayed))
    (is (= (mapv #(select-keys % [:command-id :op :valid :arguments_digest]) (:commands record))
           (mapv #(select-keys % [:command-id :op :valid :arguments_digest]) (:commands replayed))))
    (is (= "Lifting." (:text replayed)))
    (is (= (log/replay-decision record options) replayed) "replay is a pure function of the record and its blobs")
    (testing "replay! re-applies the valid commands with their logged IDs, epoch and conversation"
      (let [again (atom []) metadata (atom [])
            result (log/replay! record options
                                (fn [tool arguments meta]
                                  (swap! metadata conj (select-keys meta [:expected-epoch :conversation-id :run-id :tool-call-id]))
                                  ((capture-dispatch again) tool arguments meta)))]
        (is (:ok? result))
        (is (= sent @again) "the same tools, arguments and command IDs as the live round")
        (is (= [{:expected-epoch 2 :conversation-id "night-1" :run-id "night-1:r1" :tool-call-id "c1"}
                {:expected-epoch 2 :conversation-id "night-1" :run-id "night-1:r1" :tool-call-id "c2"}] @metadata))
        (is (true? (:receipts-match? result)))))))

(deftest replay-refuses-another-epoch-or-basis-and-never-asks-a-model
  (let [{:keys [decisions]} (run-recorded!)
        {:keys [path]} (write-log decisions)
        [record] (writer/read-records path)
        blob (writer/blob-loader path)
        dispatched (atom 0)
        replay! #(log/replay! record % (fn [& _] (swap! dispatched inc) {:status :accepted}))]
    (is (= :epoch-mismatch (:code (replay! {:blob blob :basis (assoc basis :epoch 3)}))))
    (let [refusal (replay! {:blob blob :basis (assoc basis :rev 1845)})]
      (is (= :basis-mismatch (:code refusal)))
      (is (= {:expected {:rev 1844} :actual {:rev 1845}} (select-keys refusal [:expected :actual]))))
    (is (= :basis-mismatch (:code (replay! {:blob blob :basis (assoc basis :digest "sha256:other")}))))
    (is (= :basis-missing (:code (replay! {:blob blob :basis {:epoch 2}}))))
    (is (= :tools-mismatch (:code (replay! {:blob blob :basis basis :tools (pop agent/default-tools)}))))
    (testing "a missing or altered reply is refused; there is no model to fall back on"
      (is (= :missing-reply (:code (replay! {:blob (constantly nil) :basis basis}))))
      (is (= :missing-reply (:code (replay! {:blob (constantly (sse-round "Other." [])) :basis basis}))))
      (is (= :missing-reply (:code (replay! {:basis basis})))))
    (testing "a record whose commands were edited no longer matches its reply"
      (is (= :commands-differ
             (:code (log/replay! (assoc-in record [:commands 0 :arguments_digest] "sha256:edited")
                                 {:blob blob :basis basis} (fn [& _] (swap! dispatched inc)))))))
    (is (= :invalid-record (:code (log/replay! (dissoc record :observation) {:blob blob :basis basis} (fn [& _] (swap! dispatched inc))))))
    (is (zero? @dispatched) "nothing is dispatched on a refusal")))

(deftest a-record-is-bounded-and-large-values-go-to-blobs
  (let [big (apply str (repeat 20000 "x"))
        {:keys [decisions]} (run-recorded! {:rounds [(sse-round "Big." [{:id "b1" :name "put_cue" :arguments {:cue {:id "big" :note big}}}])
                                                    (sse-round "Done." [])]})
        {:keys [record blobs]} (log/decision-record context (first decisions))
        line (json/write-str record)
        command (first (:commands record))]
    (is (<= (count line) (:record-chars log/limits)))
    (is (not (contains? command :arguments)) "large arguments are not inline")
    (is (= {:cue {:id "big" :note big}} (json/read-str (get blobs (:arguments_ref command)) :key-fn keyword)))
    (is (not (str/includes? line big)))
    (testing "a decision that cannot be bounded is refused, counted and never thrown into the run"
      (let [many (vec (for [i (range 100)] {:id (str "m" i) :name "set_parameter"
                                            :arguments {:instance-id (apply str (repeat 900 "i")) :param "p" :value i}}))
            path (temp-log) w (writer/open! path)
            {:keys [outcome decisions]} (run-recorded! {:rounds [(sse-round "Many." many) (sse-round "Done." [])] :max-tool-calls 128
                                                        :on-decision (log/decision-hook w context)})]
        (is (= :completed (:status outcome)))
        (is (= :invalid-record (:agent-log/error (ex-data (try (log/decision-record context (first decisions)) (catch Exception e e))))))
        (let [stats (writer/close! w)]
          (is (= 1 (:refused stats)) (pr-str stats))
          (is (= 1 (:written stats)) "the final round is still logged"))))
    (testing "an oversized reply is left out, named, and cannot be replayed"
      (with-redefs [log/limits (assoc log/limits :blob-chars 32)]
        (let [{:keys [record blobs]} (log/decision-record context (first decisions))]
          (is (= ["request" "raw" "parsed"] (:omitted record)))
          (is (not (contains? blobs (get-in record [:output :raw]))))
          (is (= :missing-reply (:code (log/replay-decision record {:blob blobs :basis basis})))))))))

(deftest secrets-never-reach-the-log
  (let [credential "sk-ant-api03-live-credential-0123456789"
        path (temp-log) w (writer/open! path)
        {:keys [decisions]} (run-recorded!
                             {:credential credential
                              :messages [{:role :user :content (str "My key is " credential " and openai sk-proj-abcdefghijklmnopqrstuv")}]
                              :rounds [(sse-round (str "Echo " credential) [{:id "s1" :name "put_cue"
                                                                              :arguments {:cue {:id "c" :api_key "plain-secret-value"
                                                                                                :headers {:Authorization "Bearer abcdefghijkl"}}}}])
                                       (sse-round "Done." [])]
                              :on-decision (log/decision-hook w context)})]
    (is (= 2 (count decisions)))
    (is (= 2 (:written (writer/close! w))))
    (let [dir (.getParentFile (io/file path))
          everything (apply str (map slurp (filter #(.isFile %) (file-seq dir))))]
      (is (not (str/includes? everything credential)))
      (is (not (str/includes? everything "sk-proj-abcdefghijklmnopqrstuv")))
      (is (not (str/includes? everything "plain-secret-value")))
      (is (not (str/includes? everything "abcdefghijkl")))
      (is (str/includes? everything "[REDACTED]"))
      (is (true? (:redacted (first (writer/read-records path))))))))

(deftest a-blocked-writer-never-blocks-the-caller
  (let [release (promise)
        path (temp-log)
        w (writer/open! path {:capacity 2 :append! (fn [_ _] @release) :put-blob! (fn [& _])})
        {:keys [decisions]} (run-recorded!)
        entry (log/decision-record context (first decisions))
        started (System/nanoTime)
        accepted (count (filter true? (repeatedly 200 #(writer/log! w entry))))
        elapsed-ms (/ (- (System/nanoTime) started) 1e6)]
    (is (< elapsed-ms 250) (str "200 log! calls against a stuck disk took " elapsed-ms " ms"))
    (is (<= 2 accepted 3) "the queue holds its capacity, plus the record the stuck thread holds")
    (is (= (- 200 accepted) (:dropped (writer/stats w))))
    (deliver release nil)
    (let [stats (writer/close! w)]
      (is (:drained? stats))
      (is (= accepted (:written stats))))
    (is (false? (writer/log! w entry)) "a closed writer accepts nothing")))

(deftest the-research-examples-are-valid-records
  (doseq [example ["decision-log.record.json" "decision-log.translator.json"]]
    (let [record (json/read-str (slurp (io/file "docs/research/native-agent/examples" example)) :key-fn keyword)]
      (is (empty? (log/validate-record record)) example))))

;; Through the real show owner ---------------------------------------------------

(defn- capture-sink [captured] {:send! (fn [universes _] (reset! captured universes))})

(defn- runtime [model]
  (doto (show/create-runtime {:model model :sink (capture-sink (atom nil)) :workspace-options nil})
    (show/tick! 0.0)))

(defn- on-owner!
  "Run f on another thread while ticking the headless owner at time 0, as a
  rehearsal does, so dispatch-tool!'s queued commands commit."
  [rt f]
  (let [result (future (f))]
    (loop [n 0]
      (show/tick! rt 0.0)
      (when-not (realized? result)
        (when (> n 20000) (throw (ex-info "The owner did not finish" {})))
        (Thread/sleep 0 50000)
        (recur (inc n))))
    @result))

(defn- show-dispatch [rt]
  (fn [tool arguments metadata]
    (show/dispatch-tool! rt tool arguments (merge metadata (select-keys (:model @(:state rt)) [:epoch :show-id])))))

(deftest a-show-decision-replays-onto-the-same-epoch-and-is-refused-on-another
  (let [p (passage/concert 670 :a {})
        {:keys [run-id text] tool-calls :calls prompt :prompt} (script/suggestion p 0 0.0)
        model (passage/model (:bpm p))
        live (runtime model)
        path (temp-log) w (writer/open! path)]
    (try
      (let [context (log/model-context (:model @(:state live)) {:runtime "rehearsal" :actor {:role "scripted" :profile "rehearsal"}})
            observed (log/model-basis (:model @(:state live)))
            outcome (on-owner! live #(agent/run! rehearsal/provider-config nil [{:role :user :content prompt}] agent/default-tools
                                                 (show-dispatch live) (fn [_])
                                                 {:transport (rehearsal/recorded-transport [(sse-round text tool-calls) (sse-round "Done." [])])
                                                  :run-id run-id :conversation-id "concert-a"
                                                  :on-decision (log/decision-hook w context)}))]
        (is (= :completed (:status outcome)))
        (is (= 2 (:written (writer/close! w))))
        (let [[record] (writer/read-records path)
              receipts (mapv :receipt (:commands record))]
          (is (= ["schedule_events" "configure_auto"] (mapv :op (:commands record))))
          (is (every? #(= "accepted" (:status %)) receipts) (pr-str receipts))
          (is (seq (:event-ids (first receipts))))
          (testing "onto a fresh owner in the same epoch and on the same basis, the commands and receipts repeat"
            (let [fresh (runtime model)]
              (try
                (is (= observed (log/model-basis (:model @(:state fresh)))))
                (let [result (on-owner! fresh #(log/replay! record {:blob (writer/blob-loader path)
                                                                     :basis (log/model-basis (:model @(:state fresh)))}
                                                            (show-dispatch fresh)))]
                  (is (:ok? result) (pr-str (dissoc result :commands)))
                  (is (true? (:receipts-match? result)) (pr-str (:receipts result)))
                  (is (= (:upcoming (show/schedule live)) (:upcoming (show/schedule fresh)))
                      "the replayed decision posts the same scheduled events"))
                (finally (show/stop! fresh)))))
          (testing "another epoch is refused before anything reaches the owner"
            (let [other (runtime (update model :epoch (fnil inc 0)))
                  dispatched (atom 0)]
              (try
                (is (= :epoch-mismatch (:code (log/replay! record {:blob (writer/blob-loader path)
                                                                   :basis (log/model-basis (:model @(:state other)))}
                                                           (fn [& _] (swap! dispatched inc))))))
                (is (zero? @dispatched))
                (finally (show/stop! other)))))
          (testing "the same owner after its revision moved on is off the basis"
            (is (= :basis-mismatch (:code (log/replay-decision record {:blob (writer/blob-loader path)
                                                                       :basis (log/model-basis (:model @(:state live)))})))))))
      (finally (show/stop! live)))))
