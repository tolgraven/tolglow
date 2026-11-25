(ns tolglow.development.agents-test
  (:require [clojure.test :refer :all]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [tolglow.development.agents :as agents]
            [tolglow.development.terminal :as terminal]
            [tolglow.development.workspace :as workspace]
            [tolglow.development.workspace-test :as fixtures])
  (:import [java.util.concurrent CountDownLatch TimeUnit]
           [java.nio.file Path]))

(def reference "credential-00000000-0000-0000-0000-000000000001")

(def secret "fixture-key-not-a-real-credential")

(defn options [manager execute!]
  {:workspaces manager :execute! execute! :providers {:codex {:credential-ref reference}}
   :credential-store {:get! (fn [ref] (when (= ref reference) secret))}})

(defn finish [manager id] (deref (agents/result manager id) 15000 {:status :test-timeout}))

(defn code [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:app-agent/error (ex-data e)))))

(defn completed! [request]
  ((:on-line! request) :stdout (json/write-str {:type "turn.completed" :usage {:input_tokens 7 :output_tokens 3}}))
  {:exit-code 0})

;; Source-derived CLI protocol fixture, not a recorded provider run. The injected
;; boundary launches a real local Python process strictly for these tests.
(def fixture-program
  (str "import json, os, pathlib, sys\n"
       "prompt=sys.stdin.read()\n"
       "print(json.dumps({'type':'thread.started','thread_id':'fixture-thread'}),flush=True)\n"
       "print(json.dumps({'type':'turn.started'}),flush=True)\n"
       "pathlib.Path('src/core.clj').write_text('(ns fixture.core)\\n(def value 2)\\n')\n"
       "print(json.dumps({'type':'item.completed','item':{'id':'patch1','type':'file_change','status':'completed','changes':[{'path':'src/core.clj','kind':'update'}]}}),flush=True)\n"
       "print(json.dumps({'type':'item.completed','item':{'id':'message1','type':'agent_message','text':'Edited the fixture. '+os.environ['CODEX_API_KEY']}}),flush=True)\n"
       "print(os.environ['CODEX_API_KEY'],file=sys.stderr,flush=True)\n"
       "print(json.dumps({'type':'turn.completed','usage':{'input_tokens':7,'output_tokens':3}}),flush=True)\n"))

(defn fixture-process [record]
  (fn [{:keys [cwd stdin env on-line!] :as request}]
    (reset! record (dissoc request :on-line! :env))
    (let [builder (doto (ProcessBuilder. ["python3" "-u" "-c" fixture-program]) (.directory (io/file cwd)))
          _ (doseq [[k v] env] (.put (.environment builder) k v))
          process (.start builder)
          stderr (future (with-open [reader (io/reader (.getErrorStream process))]
                           (doseq [line (line-seq reader)] (on-line! :stderr line))))]
      (try
        (with-open [writer (io/writer (.getOutputStream process))] (.write writer stdin))
        (with-open [reader (io/reader (.getInputStream process))]
          (doseq [line (line-seq reader)] (on-line! :stdout line)))
        (when-not (.waitFor process 5000 TimeUnit/MILLISECONDS) (throw (ex-info "Fixture process timed out" {})))
        @stderr
        {:exit-code (.exitValue process)}
        (finally (.destroyForcibly process) (future-cancel stderr))))))

(deftest construction-and-unavailable-capabilities-do-not-start-processes-or-create-worktrees
  (fixtures/with-workspace {}
    (fn [{:keys [manager]}]
      (let [calls (atom 0) owner (agents/create-manager (assoc (options manager #(swap! calls inc))
                                                            :credential-store {:get! #(swap! calls inc)}))]
        (try
          (is (zero? @calls))
          (is (= [] (:tasks (agents/status owner))))
          (is (= :preview-unavailable (code #(agents/start! owner {:prompt "Edit" :policy :preview}))))
          (is (= :invalid-request (code #(agents/start! owner {:provider :claude :prompt "Edit"}))))
          (is (= :checks-unavailable (code #(agents/start! owner {:prompt "Edit" :policy :after-turn}))))
          (is (empty? (:workspaces (workspace/status manager))))
          (is (zero? @calls))
          (finally (agents/close! owner)))))))

(deftest actual-process-protocol-edits-fresh-paired-worktrees-and-redacts-credentials
  (fixtures/with-workspace {}
    (fn [{:keys [manager first second]}]
      (spit (str (.resolve ^Path first "uncommitted.clj")) "user work")
      (let [record (atom nil) owner (agents/create-manager (options manager (fixture-process record)))]
        (try
          (let [started (agents/start! owner {:prompt "Modify fixture safely"}) id (:id started)
                final (finish owner id) stream (agents/read-events owner id 0)
                ws (:workspace final)]
            (is (= :completed (:status final)))
            (is (= :unchanged (:application-state final)))
            (is (= #{:tolglow :afterglow} (set (keys (:repositories ws)))))
            (is (= (str "tolglow-agent/" id) (get-in ws [:repositories :tolglow :branch])))
            (is (= "(ns fixture.core)\n(def value 1)\n" (slurp (str (.resolve ^Path first "src/core.clj")))))
            (is (= "user work" (slurp (str (.resolve ^Path first "uncommitted.clj")))))
            (is (not (.exists (io/file (get-in ws [:repositories :tolglow :path]) "uncommitted.clj"))))
            (is (= "main" (str/trim (:output (fixtures/command! second ["git" "branch" "--show-current"])))))
            (is (= "Modify fixture safely" (:stdin @record)))
            (is (= ["codex" "--no-daemon" "-a" "never" "exec" "--json" "--ephemeral"] (subvec (:argv @record) 0 7)))
            (is (= "-" (peek (:argv @record))))
            (is (some #{"--add-dir"} (:argv @record)))
            (is (not (str/includes? (pr-str (:argv @record)) secret)))
            (is (= 1 (get-in final [:summary :file-count])))
            (is (= "src/core.clj" (get-in final [:summary :repositories :tolglow 0 :path])))
            (is (some #(= :workspace/changes (:type %)) (:events stream)))
            (is (some #(and (= :agent/item (:type %)) (str/includes? (:text % "") "[REDACTED]")) (:events stream)))
            (is (not (str/includes? (pr-str [final stream (agents/status owner)]) secret)))
            (is (apply < (map :cursor (:events stream))))
            (is (empty? (:events (agents/read-events owner id (:cursor stream)))))
            (is (str/includes? (get-in (workspace/diff manager id) [:repositories :tolglow :diff]) "+(def value 2)")))
          (finally (agents/close! owner)))))))

(deftest progress-is-visible-before-execution-completes-and-history-is-bounded
  (fixtures/with-workspace {}
    (fn [{:keys [manager]}]
      (let [ready (promise) release (promise)
            execute! (fn [request]
                       (dotimes [i 100]
                         ((:on-line! request) :stdout (json/write-str {:type "item.updated" :item {:id "x" :type "agent_message" :text (str "Progress " i)}})))
                       (deliver ready true) @release (completed! request))
            owner (agents/create-manager (assoc (options manager execute!) :event-limit 8))]
        (try
          (let [id (:id (agents/start! owner {:prompt "Work"}))]
            (is (true? (deref ready 5000 false)))
            (let [progress (agents/read-events owner id 0)]
              (is (= :running (:status progress)))
              (is (:dropped? progress))
              (is (<= (count (:events progress)) 8))
              (is (str/includes? (pr-str (:events progress)) "Progress 99")))
            (is (not (realized? (agents/result owner id))))
            (deliver release true)
            (is (= :completed (:status (finish owner id)))))
          (finally (deliver release true) (agents/close! owner)))))))

(deftest malformed-or-failed-protocol-does-not-report-a-successful-turn
  (fixtures/with-workspace {:max-workspaces 8}
    (fn [{:keys [manager]}]
      (doseq [[line response] [["not-json" {:exit-code 0}]
                               [(json/write-str {:type "turn.failed" :error {:message "Provider failed"}}) {:exit-code 0}]
                               [(json/write-str {:type "turn.started"}) {:exit-code 0}]
                               [(json/write-str {:type "turn.completed"}) {:exit-code 3}]]]
        (let [owner (agents/create-manager (options manager (fn [request] ((:on-line! request) :stdout line) response)))]
          (try
            (let [id (:id (agents/start! owner {:prompt "Work"}))]
              (is (= :failed (:status (finish owner id))))
              (is (= :execution-failed (get-in (agents/status owner id) [:error :code]))))
            (finally (agents/close! owner))))))))

(deftest unknown-protocol-events-are-visible-but-do-not-discard-a-valid-completion
  (fixtures/with-workspace {}
    (fn [{:keys [manager]}]
      (let [owner (agents/create-manager (options manager (fn [request]
                                                           ((:on-line! request) :stdout "{\"type\":\"future.progress\"}")
                                                           (completed! request))))]
        (try
          (let [id (:id (agents/start! owner {:prompt "Work"}))]
            (is (= :completed (:status (finish owner id))))
            (is (some #(= :agent/unknown-event (:type %)) (:events (agents/read-events owner id 0)))))
          (finally (agents/close! owner)))))))

(deftest cancellation-fences-running-and-queued-tasks-and-closes-owned-workers
  (fixtures/with-workspace {}
    (fn [{:keys [manager]}]
      (let [entered (promise) exited (promise)
            execute! (fn [_] (deliver entered true)
                       (try (.await (CountDownLatch. 1))
                            (finally (deliver exited true))))
            owner (agents/create-manager (options manager execute!))]
        (try
          (let [running (:id (agents/start! owner {:prompt "Wait"}))]
            (is (true? (deref entered 5000 false)))
            (let [queued (:id (agents/start! owner {:prompt "Never execute"}))]
              (is (= :cancelled (:status (agents/cancel! owner queued))))
              (is (= :cancelled (:status (finish owner queued))))
              (agents/cancel! owner running)
              (is (true? (deref exited 5000 false)))
              (is (= :cancelled (:status (finish owner running))))
              (is (= 1 (count (:workspaces (workspace/status manager)))))
              (agents/close! owner)
              (is (:closed? (agents/status owner)))
              (is (= :closed (code #(agents/start! owner {:prompt "After close"}))))))
          (finally (agents/close! owner)))))))

(deftest after-turn-checks-use-real-current-files-and-only-stage-reviewable-candidates
  (fixtures/with-workspace {:runner workspace/run-trusted-process!}
    (fn [{:keys [manager]}]
      (let [owner (agents/create-manager (assoc (options manager completed!) :check-profiles [:unit]))]
        (try
          (let [id (:id (agents/start! owner {:prompt "Work" :policy :after-turn})) final (finish owner id)]
            (is (= :completed (:status final)))
            (is (= :passed (get-in final [:checks :unit :status])))
            (is (= :ready (get-in final [:candidate :status])))
            (is (= (workspace/fingerprint manager id) (get-in final [:candidate :fingerprint])))
            (is (= :unchanged (:application-state final)))
            (let [file (workspace/read-file manager id :tolglow "src/core.clj")]
              (workspace/write-file! manager id :tolglow "src/core.clj" {:expected-hash (:hash file) :content "changed after check"})
              (is (thrown? clojure.lang.ExceptionInfo (workspace/stage-preview! manager id {:required-checks [:unit]})))))
          (finally (agents/close! owner)))))))

(deftest credential-failure-does-not-create-a-worktree-and-runtime-errors-redact-keys
  (fixtures/with-workspace {}
    (fn [{:keys [manager]}]
      (let [owner (agents/create-manager (assoc (options manager completed!) :credential-store {:get! (constantly nil)}))]
        (try
          (let [id (:id (agents/start! owner {:prompt "Work"}))]
            (is (= :credential-unavailable (get-in (finish owner id) [:error :code])))
            (is (empty? (:workspaces (workspace/status manager)))))
          (finally (agents/close! owner))))
      (let [owner (agents/create-manager (options manager #(throw (ex-info (str "Error " (get-in % [:env "CODEX_API_KEY"])) {}))))]
        (try
          (let [id (:id (agents/start! owner {:prompt "Work"})) final (finish owner id)]
            (is (= :failed (:status final)))
            (is (str/includes? (get-in final [:error :message]) "[REDACTED]"))
            (is (not (str/includes? (pr-str (agents/read-events owner id 0)) secret))))
          (finally (agents/close! owner)))))))

(deftest cancelling-during-check-interrupts-the-owned-check-worker
  (let [entered (promise) exited (promise)
        runner (fn [_] (deliver entered true)
                 (try (.await (CountDownLatch. 1))
                      (finally (deliver exited true))))]
    (fixtures/with-workspace {:runner runner}
      (fn [{:keys [manager]}]
        (let [owner (agents/create-manager (assoc (options manager completed!) :check-profiles [:unit]))]
          (try
            (let [id (:id (agents/start! owner {:prompt "Work" :policy :after-turn}))]
              (is (true? (deref entered 5000 false)))
              (is (= :checking (:status (agents/status owner id))))
              (agents/cancel! owner id)
              (is (= :cancelled (:status (finish owner id))))
              (is (true? (deref exited 5000 false)))
              (is (nil? (:candidate (agents/status owner id)))))
            (finally (agents/close! owner))))))))

(deftest failed-check-never-stages-a-preview-or-activates-code
  (fixtures/with-workspace {:runner (fn [_] {:exit-code 1 :output "real check failed"})}
    (fn [{:keys [manager]}]
      (let [owner (agents/create-manager (assoc (options manager completed!) :check-profiles [:unit]))]
        (try
          (let [id (:id (agents/start! owner {:prompt "Work" :policy :after-turn})) final (finish owner id)]
            (is (= :completed (:status final)))
            (is (= :failed (get-in final [:checks :unit :status])))
            (is (nil? (:candidate final)))
            (is (= :unchanged (:application-state final)))
            (is (nil? (:preview (workspace/status manager id)))))
          (finally (agents/close! owner)))))))

(deftest bounded-runner-failures-retain-reviewable-edits-but-cannot-claim-success
  (fixtures/with-workspace {}
    (fn [{:keys [manager]}]
      (doseq [[failure expected] [[{:timed-out? true} :timed-out] [{:truncated? true} :output-limit]]]
        (let [owner (agents/create-manager (options manager (fn [request]
                                                             (completed! request)
                                                             (merge {:exit-code 0} failure))))]
          (try
            (let [id (:id (agents/start! owner {:prompt "Work"})) final (finish owner id)]
              (is (= :failed (:status final)))
              (is (= expected (get-in final [:error :code])))
              (is (= :ready (:state (workspace/status manager id)))))
            (finally (agents/close! owner))))))))

(defn docker-smoke!
  "Explicit opt-in integration gate, excluded from ordinary run-tests. Runs the
  source-derived fixture protocol through the real Docker streaming boundary,
  with network disabled. It never invokes an authenticated provider."
  []
  (binding [*report-counters* (ref *initial-report-counters*)]
    (fixtures/with-workspace {}
      (fn [{:keys [manager]}]
        (let [boundary (terminal/create-manager {:resolve-workspace #(workspace/runtime-paths manager %)
                                                 :network :none :image "tolglow-dev:local"})
              runner (terminal/streaming-runner boundary)
              adapter (fn [request]
                        (spit (io/file (:cwd request) "agent_fixture.py") fixture-program)
                        (runner (assoc request :argv ["python3" "-u" "agent_fixture.py"])))
              owner (agents/create-manager (options manager adapter))]
          (try
            (let [id (:id (agents/start! owner {:prompt "Actual Docker fixture stream"}))
                  final (deref (agents/result owner id) 60000 {:status :test-timeout})
                  events (agents/read-events owner id 0)]
              (is (= :completed (:status final)))
              (is (= 2 (get-in final [:summary :file-count])))
              (is (str/includes? (pr-str events) "[REDACTED]"))
              (is (not (str/includes? (pr-str [final events]) secret)))
              (is (str/includes? (get-in (workspace/diff manager id) [:repositories :tolglow :diff]) "+(def value 2)"))
              (is (empty? (:jobs (terminal/status boundary)))))
            (finally (agents/close! owner) (terminal/close! boundary))))))
    @*report-counters*))

(deftest task-output-leases-replay-the-retained-window-and-release-their-watch
  (fixtures/with-workspace {}
    (fn [{:keys [manager]}]
      (let [send-lines (promise) finish-turn (promise) wake (promise)
            execute! (fn [{:keys [on-line!] :as request}]
                       @send-lines
                       (doseq [n (range 48)]
                         (on-line! :stdout (json/write-str {:type "item.completed"
                                                           :item {:id (str n) :type "agent_message" :text (str n)}})))
                       @finish-turn
                       (completed! request))
            owner (agents/create-manager (assoc (options manager execute!) :event-limit 32))]
        (try
          (let [id (:id (agents/start! owner {:provider :codex :repo :tolglow :policy :manual :prompt "Read a fixture"}))
                source (fn [n] (agents/event-source owner [:development/task-events id n]))
                lease (source 0)
                events (get-in @(:state owner) [:runs id :events])
                watches #(count (.getWatches ^clojure.lang.Atom events))
                before (watches)
                detach ((:observe! lease) (fn [] (deliver wake true)))
                read! (fn [lease] ((:sample! lease) ((:source! lease))))]
            (is (= (inc before) (watches)))
            (deliver send-lines true)
            (is (deref wake 10000 false))
            (deliver finish-turn true)
            (is (= :completed (:status (finish owner id))))
            (let [first (read! lease)
                  second (read! (source (:cursor first)))]
              (is (:dropped? first))
              (is (= 16 (count (:events first))))
              (is (= (:available-cursor first) (:cursor second)))
              (is (= :completed (:status second)))
              (is (apply < (map :cursor (concat (:events first) (:events second))))))
            (detach)
            (is (= before (watches)))
            (is (= {:error :unavailable} @(get (agents/event-source owner [:development/task-events "unknown" 0]) :result))))
          (finally (deliver send-lines true) (deliver finish-turn true) (agents/close! owner)))))))
