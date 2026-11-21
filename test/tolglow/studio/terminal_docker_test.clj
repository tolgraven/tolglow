(ns tolglow.studio.terminal-docker-test
  "Explicit local Docker gate. Run this namespace after building tolglow-dev:local;
  missing Docker/image is a failure, never silently skipped or a host-shell fallback."
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [tolglow.development.workspace :as workspace]
            [tolglow.development.workspace-test :as fixtures]
            [tolglow.development.terminal :as terminal])
  (:import [java.util Base64]
           [java.nio.charset StandardCharsets]
           [java.util.concurrent CountDownLatch TimeUnit]))

(defn encode [s] (.encodeToString (Base64/getEncoder) (.getBytes ^String s StandardCharsets/UTF_8)))
(defn decoded [response] (String. (.decode (Base64/getDecoder) ^String (:data response)) StandardCharsets/UTF_8))
(declare poll)
(defn with-container [options f]
  (fixtures/with-workspace {}
    (fn [{:keys [manager] :as fixture}]
      (workspace/create! manager {:id "container"})
      (let [term (terminal/create-manager (merge {:resolve-workspace #(workspace/runtime-paths manager %)} options))]
        (try (f (assoc fixture :terminal term :paths (workspace/runtime-paths manager "container")))
             (finally (terminal/close! term)
                      (is (empty? (:pending-containers
                                   (poll #(terminal/status term) #(empty? (:pending-containers %)) 30000)))
                          "Pending create/cleanup must settle before fixture roots disappear")))))))
(defn request [paths argv]
  {:workspace-id "container" :workspace-root (:workspace-root paths) :cwd (get-in paths [:repositories :tolglow :path])
   :argv argv :timeout-ms 20000 :max-output-bytes 65536})
(defn poll [f pred timeout]
  (let [deadline (+ (System/nanoTime) (* timeout 1000000))]
    (loop [] (let [value (f)] (if (or (pred value) (> (System/nanoTime) deadline)) value
                                (do (Thread/sleep 25) (recur)))))))

(deftest actual-container-runs-tools-on-owned-worktrees-and-protects-original-references
  (with-container {}
    (fn [{:keys [terminal paths first]}]
      (let [repo (get-in paths [:repositories :tolglow])
            code "import os,subprocess,json; print(json.dumps({'uid':os.getuid(),'cwd':os.getcwd(),'git':subprocess.check_output(['git','status','--porcelain']).decode(),'socket':os.path.exists('/var/run/docker.sock')})); open('src/from-container.txt','w').write('owned'); print(subprocess.check_output(['java','-version'],stderr=subprocess.STDOUT).decode()); print(subprocess.check_output(['node','--version']).decode()); print(subprocess.check_output(['clojure','-Sdescribe']).decode())"
            result ((terminal/runner terminal) (request paths ["python3" "-c" code]))]
        (is (= 0 (:exit-code result)) (:output result))
        (is (str/includes? (:output result) "\"socket\": false"))
        (is (str/includes? (:output result) "25.0"))
        (is (str/includes? (:output result) "v26."))
        (is (str/includes? (:output result) "1.12.7.1681"))
        (is (= "owned" (slurp (str (:path repo) "/src/from-container.txt"))))
        (is (not (.exists (java.io.File. (str first "/src/from-container.txt")))))
        (let [blocked ((terminal/runner terminal) (request paths ["python3" "-c" (str "open(" (pr-str (str (:common-dir repo) "/refs/heads/main")) ",'w').write('no')")]))]
          (is (not= 0 (:exit-code blocked))))
        (let [unowned (assoc (request paths ["true"]) :cwd "/tmp")]
          (is (thrown? clojure.lang.ExceptionInfo ((terminal/runner terminal) unowned))))))))

(deftest checks-bound-output-time-and-clean-up-owned-containers
  (with-container {}
    (fn [{:keys [terminal paths]}]
      (let [result ((terminal/runner terminal) (assoc (request paths ["python3" "-c" "print('x'*100000)"]) :max-output-bytes 1024))]
        (is (= 0 (:exit-code result))) (is (:truncated? result)) (is (= 1024 (count (:output result)))))
      (let [result ((terminal/runner terminal) (assoc (request paths ["python3" "-c" "import time; time.sleep(30)"]) :timeout-ms 500))]
        (is (:timed-out? result)))
      (let [containers (workspace/run-trusted-process! {:cwd "/tmp" :argv ["docker" "ps" "-aq" "--filter" (str "label=tolglow.owner=" (:id terminal))]
                                                       :timeout-ms 5000 :max-output-bytes 4096})]
        (is (= "" (str/trim (:output containers))))))))

(deftest real-zsh-pty-supports-bytes-resize-reconnect-and-close
  (with-container {:max-sessions 1 :output-bytes 8192}
    (fn [{:keys [terminal]}]
      (let [opened (terminal/open! terminal {:workspace-id "container" :repo :tolglow :rows 25 :cols 90}) id (:session-id opened)]
        (is (= :running (:state opened)))
        (is (thrown? clojure.lang.ExceptionInfo (terminal/open! terminal {:workspace-id "container"})))
        (terminal/input! terminal id (encode "printf '\\nPTY_%s\\n' READY\r"))
        (let [output (poll #(terminal/read-output terminal id 0) #(str/includes? (decoded %) "PTY_READY") 10000)]
          (is (str/includes? (decoded output) "PTY_READY") (decoded output))
          (is (= (:data output) (:data (terminal/read-output terminal id 0))))
          (is (= "" (:data (terminal/read-output terminal id (:cursor output))))))
        (terminal/resize! terminal id 31 101)
        (terminal/input! terminal id (encode "stty size\r"))
        (let [output (poll #(terminal/read-output terminal id 0) #(str/includes? (decoded %) "31 101") 5000)]
          (is (str/includes? (decoded output) "31 101") (decoded output)))
        (terminal/input! terminal id (encode "python3 -c \"print('x'*30000)\"\r"))
        (let [output (poll #(terminal/read-output terminal id 0) :dropped? 5000)]
          (is (:dropped? output)) (is (<= (alength (.decode (Base64/getDecoder) ^String (:data output))) 8192)))
        (is (= :closed (:state (terminal/close-session! terminal id))))
        (is (thrown? clojure.lang.ExceptionInfo (terminal/input! terminal id (encode "must not execute"))))
        (is (= :closed (:state (terminal/read-output terminal id 0))))))))

(deftest missing-image-fails-explicitly-without-running-the-request-on-host
  (with-container {:image "tolglow-deliberately-missing-image:test"}
    (fn [{:keys [terminal paths]}]
      (let [error (try ((terminal/runner terminal) (request paths ["true"])) (catch clojure.lang.ExceptionInfo error error))]
        (is (= :image-unavailable (:terminal/error (ex-data error))))
        (is (false? (:available? (terminal/availability! terminal))))
        (is (empty? (:sessions (terminal/status terminal))))))))

(deftest manager-close-cancels-a-real-check-and-reclaims-its-container
  (with-container {}
    (fn [{:keys [terminal paths]}]
      (let [result (promise)
            worker (Thread. (fn [] (try (deliver result ((terminal/runner terminal)
                                                       (request paths ["python3" "-c" "import time; time.sleep(60)"])))
                                       (catch InterruptedException _ (deliver result :interrupted)))))]
        (.start worker)
        (is (= 1 (:running-checks (poll #(terminal/status terminal) #(= 1 (:running-checks %)) 5000))))
        (is (:closed? (terminal/close! terminal)))
        (is (= :interrupted (deref result 15000 :timeout)))
        (.join worker 1000)
        (is (not (.isAlive worker)))
        (is (empty? (:pending-containers (poll #(terminal/status terminal) #(empty? (:pending-containers %)) 30000))))
        (let [containers (workspace/run-trusted-process! {:cwd "/tmp" :argv ["docker" "ps" "-aq" "--filter" (str "label=tolglow.owner=" (:id terminal))]
                                                         :timeout-ms 5000 :max-output-bytes 4096})]
          (is (= "" (str/trim (:output containers)))))))))

(deftest streamed-process-separates-json-output-and-diagnostics-with-bounded-provider-env
  (with-container {}
    (fn [{:keys [terminal paths]}]
      (let [events (atom [])
            code "import sys,os,json; print(json.dumps({'prompt':sys.stdin.read(),'credential-present':bool(os.environ.get('OPENAI_API_KEY')),'unicode':'☃'},ensure_ascii=False),flush=True); print('diagnostic only',file=sys.stderr,flush=True)"
            result ((terminal/streaming-runner terminal)
                    (assoc (request paths ["python3" "-c" code]) :stdin "explicit prompt" :env {"OPENAI_API_KEY" "dummy-test-key"}
                           :on-line! #(swap! events conj [%1 %2])))]
        (is (= 0 (:exit-code result)) (:output result))
        (is (= 2 (count @events)))
        (is (some #(and (= :stdout (first %)) (str/includes? (second %) "explicit prompt")) @events))
        (is (some #(and (= :stdout (first %)) (str/includes? (second %) "☃")) @events))
        (is (some #(= [:stderr "diagnostic only"] %) @events))
        (is (not (str/includes? (:output result) "dummy-test-key")))
        (is (thrown? clojure.lang.ExceptionInfo
                     ((terminal/streaming-runner terminal) (assoc (request paths ["true"]) :env {"DOCKER_HOST" "untrusted"} :on-line! (fn [& _]))))))
      (let [events (atom [])
            result ((terminal/streaming-runner terminal)
                    (assoc (request paths ["python3" "-c" "print('x'*10000); print('done')"])
                           :max-output-bytes 2048 :max-line-bytes 1024 :on-line! #(swap! events conj [%1 %2])))]
        (is (:truncated? result))
        (is (= 2048 (count (:output result))))
        (is (empty? @events)))
      (let [failure (try ((terminal/streaming-runner terminal)
                         (assoc (request paths ["python3" "-c" "import time; print('ready',flush=True); time.sleep(30)"])
                                :on-line! (fn [& _] (throw (ex-info "Consumer stopped" {})))))
                        (catch clojure.lang.ExceptionInfo error error))]
        (is (= :stream-failed (:terminal/error (ex-data failure)))))
      (is (zero? (:running-checks (terminal/status terminal)))))))

(deftest pre-start-executable-failure-and-codex-environment-clean-up
  (with-container {}
    (fn [{:keys [terminal paths]}]
      (let [result ((terminal/runner terminal) (request paths ["/no/such/container-command"]))]
        (is (not= 0 (:exit-code result)))
        (is (empty? (:pending-containers (terminal/status terminal)))))
      (let [events (atom [])
            result ((terminal/streaming-runner terminal)
                    (assoc (request paths ["python3" "-c" "import os; print('key-present' if os.environ.get('CODEX_API_KEY') else 'missing')"])
                           :env {"CODEX_API_KEY" "dummy-container-only-key"}
                           :on-line! #(swap! events conj [%1 %2])))]
        (is (= 0 (:exit-code result)))
        (is (= [[:stdout "key-present"]] @events))
        (is (not (str/includes? (:output result) "dummy-container-only-key")))
        (is (empty? (:pending-containers (terminal/status terminal))))))))
