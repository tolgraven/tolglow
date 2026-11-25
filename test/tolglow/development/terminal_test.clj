(ns tolglow.development.terminal-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [tolglow.development.terminal :as terminal]
            [tolglow.development.workspace :as workspace]
            [tolglow.development.workspace-test :as fixtures])
  (:import [java.util Base64]
           [java.nio.file Path]))

(deftest packaged-pty-resource-is-extracted-lazily-and-cleaned-up
  (let [source (slurp (io/resource "tolglow/terminal/pty_bridge.py"))
        archive (java.io.File/createTempFile "tolglow-pty-fixture-" ".jar")
        manager (terminal/create-manager {:resolve-workspace identity})]
    (try
      (with-open [jar (java.util.jar.JarOutputStream. (io/output-stream archive))]
        (.putNextEntry jar (java.util.jar.JarEntry. "tolglow/terminal/pty_bridge.py"))
        (.write jar (.getBytes source "UTF-8"))
        (.closeEntry jar))
      (is (nil? (:bridge-temp @(:state manager))))
      (let [resource (java.net.URL. (str "jar:" (.toURL (.toURI archive)) "!/tolglow/terminal/pty_bridge.py"))
            path (with-redefs [io/resource (constantly resource)] (#'terminal/bridge-file! manager))]
        (is (= source (slurp path)))
        (is (= path (#'terminal/bridge-file! manager)))
        (terminal/close! manager)
        (is (not (.exists (io/file path)))))
      (finally (terminal/close! manager) (.delete archive)))))

(deftest construction-and-status-do-not-start-a-process-or-resolve-workspaces
  (let [calls (atom 0) manager (terminal/create-manager {:resolve-workspace #(do (swap! calls inc) %)
                                                       :docker "/definitely/not/a/docker/executable"})]
    (is (= "tolglow-dev:local" (:image (terminal/status manager))))
    (is (= :none (:network (terminal/status manager))))
    (is (empty? (:sessions (terminal/status manager))))
    (is (zero? @calls))
    (is (= {:closed? true} (terminal/close! manager)))))

(deftest invalid-input-and-unowned-directories-fail-before-process-execution
  (let [manager (terminal/create-manager {:resolve-workspace (fn [id] {:workspace-id id :workspace-root "/tmp" :repositories {}})
                                          :docker "/definitely/not/a/docker/executable"})]
    (try
      (is (thrown? clojure.lang.ExceptionInfo (terminal/open! manager {:workspace-id "../escape"})))
      (is (thrown? clojure.lang.ExceptionInfo (terminal/open! manager {:workspace-id "valid" :rows 0})))
      (is (thrown? clojure.lang.ExceptionInfo ((terminal/runner manager) {:argv ["echo" "x"] :workspace-id "valid"
                                                                        :workspace-root "/etc" :cwd "/etc" :timeout-ms 1000 :max-output-bytes 100})))
      (is (thrown? clojure.lang.ExceptionInfo (terminal/input! manager "missing" "aGVsbG8=")))
      (finally (terminal/close! manager)))))

(deftest output-ring-keeps-byte-cursors-with-bounded-overwrite
  (let [append-output (deref #'terminal/append-output)
        bytes (byte-array (range 20))
        s (-> {:session-id "one" :state :running :chunks [] :cursor 0 :first-cursor 0}
              (append-output bytes 16) (append-output bytes 16))
        manager {:state (atom {:sessions {"one" s}})}
        result (terminal/read-output manager "one" 0)
        decoder (Base64/getDecoder)]
    (is (= 40 (:cursor result)))
    (is (= 24 (:first-cursor result)))
    (is (:dropped? result))
    (is (= (vec (range 4 20)) (vec (.decode decoder ^String (:data result)))))
    (is (= "" (:data (terminal/read-output manager "one" 40))))
    (is (false? (:dropped? (terminal/read-output manager "one" 24))))
    (is (thrown? clojure.lang.ExceptionInfo (terminal/read-output manager "one" 41)))
    (is (= result (terminal/read-output manager "one" 0)))))

(defn- eventually [predicate]
  (let [deadline (+ (System/nanoTime) 5000000000)]
    (loop []
      (if (predicate) true
          (if (> (System/nanoTime) deadline) false
              (do (Thread/sleep 10) (recur)))))))

(defn- with-delayed-docker [options f]
  ;; A real executable is the process-boundary fixture. It deliberately keeps
  ;; create alive after caller cancellation, like the VFS daemon does; production
  ;; lifecycle/thread/argv/resource code remains unchanged.
  (fixtures/with-workspace {}
    (fn [{:keys [manager root] :as fixture}]
      (workspace/create! manager {:id "delayed"})
      (let [script (.resolve ^Path root "docker-fixture")
            started (.toFile (.resolve ^Path root "create-started"))
            allow (.toFile (.resolve ^Path root "allow-create"))
            operations (.toFile (.resolve ^Path root "operations"))
            _ (spit (str script)
                    (str "#!/usr/bin/python3\nimport pathlib,sys,time,os\np=pathlib.Path(__file__).parent\na=sys.argv[1:]\n"
                         "def record(s):\n with (p/'operations').open('a') as f: f.write(s+'\\n')\n"
                         "if a[:2]==['image','inspect']:\n print('fixture-image');sys.exit(0)\n"
                         "if a[0]=='create':\n n=a[a.index('--name')+1];(p/'create-started').write_text(n)\n"
                         " while not (p/'allow-create').exists(): time.sleep(.01)\n"
                         " (p/n).write_text('owned');record('created');print(n)\n"
                         "elif a[0]=='rm':\n n=a[-1];record('removed' if (p/n).exists() else 'premature-remove');(p/n).unlink(missing_ok=True)\n"
                         "elif a[0]=='start':\n record('started');print('executed')\n"
                         "else: sys.exit(2)\n"))
            _ (.setExecutable (.toFile script) true)
            term (terminal/create-manager (merge {:resolve-workspace #(workspace/runtime-paths manager %)
                                                   :docker (str script)} options))
            paths (workspace/runtime-paths manager "delayed")
            request {:workspace-id "delayed" :workspace-root (:workspace-root paths)
                     :cwd (get-in paths [:repositories :tolglow :path]) :argv ["true"]
                     :timeout-ms 500 :max-output-bytes 1024}]
        (try (f (assoc fixture :terminal term :request request :started started :allow allow :operations operations))
             (finally (spit allow "finish") (terminal/close! term)
                      (is (eventually #(empty? (:pending-containers (terminal/status term)))))))))))

(deftest cancellation-during-create-keeps-ownership-until-late-container-is-removed
  (with-delayed-docker {}
    (fn [{:keys [terminal request started allow operations]}]
      (let [result (promise)
            worker (Thread. (fn [] (try (deliver result ((terminal/runner terminal) request))
                                       (catch InterruptedException _ (deliver result :interrupted)))))]
        (.start worker)
        (is (eventually #(.exists started)))
        (.interrupt worker)
        (is (= :interrupted (deref result 1000 :timeout)))
        (is (= :creating (:phase (first (:pending-containers (terminal/status terminal))))))
        (is (not (.exists operations)))
        (spit allow "finish")
        (is (eventually #(empty? (:pending-containers (terminal/status terminal)))))
        (is (= ["created" "removed"] (clojure.string/split-lines (slurp operations))))
        (.join worker 1000)
        (is (not (.isAlive worker)))))))

(deftest creation-timeout-does-not-discard-its-outstanding-cleanup
  (with-delayed-docker {:create-timeout-ms 50}
    (fn [{:keys [terminal request started allow operations]}]
      (let [error (try ((terminal/runner terminal) request) (catch clojure.lang.ExceptionInfo error error))]
        (is (= :create-timeout (:terminal/error (ex-data error))))
        (is (eventually #(.exists started)))
        (is (= 1 (count (:pending-containers (terminal/status terminal)))))
        (is (not (.exists operations)))
        (spit allow "finish")
        (is (eventually #(empty? (:pending-containers (terminal/status terminal)))))
        (is (= ["created" "removed"] (clojure.string/split-lines (slurp operations))))))))

(deftest closing-a-terminal-during-create-never-starts-the-late-container
  (with-delayed-docker {}
    (fn [{:keys [terminal started allow operations]}]
      (let [result (promise)
            worker (Thread. (fn [] (try (deliver result (terminal/open! terminal {:workspace-id "delayed"}))
                                       (catch InterruptedException _ (deliver result :interrupted)))))]
        (.start worker)
        (is (eventually #(.exists started)))
        (terminal/close! terminal)
        (is (= :closed (:state (first (:sessions (terminal/status terminal))))))
        (is (not (.exists operations)))
        (spit allow "finish")
        (is (= :interrupted (deref result 2000 :timeout)))
        (is (eventually #(empty? (:pending-containers (terminal/status terminal)))))
        (is (= ["created" "removed"] (clojure.string/split-lines (slurp operations))))
        (.join worker 1000)))))

(defn- with-delayed-terminal-start [f]
  (fixtures/with-workspace {}
    (fn [{:keys [manager root]}]
      (workspace/create! manager {:id "delayed-start"})
      (let [script (.toFile (.resolve ^Path root "docker-start-fixture"))
            pending (.toFile (.resolve ^Path root "start-probed"))
            allow (.toFile (.resolve ^Path root "allow-start"))
            started (.toFile (.resolve ^Path root "started"))
            removed (.toFile (.resolve ^Path root "removed"))
            _ (spit script (str "#!/usr/bin/python3\nimport pathlib,sys,time\np=pathlib.Path(__file__).parent\na=sys.argv[1:]\n"
                                "if a[:2]==['image','inspect'] or a[0]=='create': print('fixture')\n"
                                "elif a[0]=='start':\n"
                                " while not (p/'allow-start').exists() and not (p/'removed').exists(): time.sleep(.01)\n"
                                " if not (p/'removed').exists(): (p/'started').touch()\n"
                                " while not (p/'removed').exists(): time.sleep(.01)\n"
                                "elif a[0]=='inspect':\n"
                                " if not (p/'started').exists(): (p/'start-probed').touch()\n"
                                " print(str((p/'started').exists()).lower())\n"
                                "elif a[0]=='exec': sys.exit(0 if (p/'started').exists() else 1)\n"
                                "elif a[0]=='rm': (p/'removed').touch()\n"
                                "else: sys.exit(2)\n"))
            _ (.setExecutable script true)
            terminal (terminal/create-manager {:resolve-workspace #(workspace/runtime-paths manager %)
                                               :docker (str script)})
            result (promise)
            worker (Thread. (fn [] (deliver result (try (terminal/open! terminal {:workspace-id "delayed-start"})
                                                        (catch Throwable error error)))))]
        (try
          (.start worker)
          (is (eventually #(or (.exists pending) (realized? result))))
          (f {:terminal terminal :result result :allow allow :started started})
          (finally
            (terminal/close! terminal)
            (.join worker 5000)
            (is (not (.isAlive worker)))
            (is (.exists removed))
            (is (empty? (:pending-containers (terminal/status terminal))))))))))

(deftest terminal-open-waits-for-the-container-to-accept-resize
  (with-delayed-terminal-start
    (fn [{:keys [terminal result allow started]}]
      (is (not (realized? result)) "Opening must wait while Docker is still starting")
      (is (= :starting (:state (first (:sessions (terminal/status terminal))))))
      (spit allow "ready")
      (let [{:keys [session-id state] :as opened} (deref result 5000 :timeout)]
        (is (= :running state) (str opened))
        (is (.exists started) "The open acknowledgement follows container startup")
        (is (= [31 101] ((juxt :rows :cols) (terminal/resize! terminal session-id 31 101))))
        (is (= :closed (:state (terminal/close-session! terminal session-id))))))))

(deftest closing-a-terminal-during-start-cancels-the-open-acknowledgement
  (with-delayed-terminal-start
    (fn [{:keys [terminal result started]}]
      (let [id (:session-id (first (:sessions (terminal/status terminal))))]
        (is (= :closed (:state (terminal/close-session! terminal id))))
        (is (instance? InterruptedException (deref result 5000 :timeout)))
        (is (= :closed (:state (terminal/read-output terminal id 0))))
        (is (not (.exists started)))))))

(deftest terminal-output-leases-isolate-the-owned-repository-and-publish-bounded-byte-pages
  (let [bytes (.getBytes (apply str (repeat 70000 "x")) java.nio.charset.StandardCharsets/UTF_8)
        base {:chunks [] :state :running :first-cursor 0 :cursor 0 :workspace-id "lighting" :repo :tolglow}
        state (atom {:sessions {"selected" (assoc base :session-id "selected") "other" (assoc base :session-id "other")}})
        manager {:state state}
        query (fn [w repo cursor] (terminal/output-source manager [:development/terminal-output w repo "selected" cursor]))
        before (count (.getWatches ^clojure.lang.Atom state))
        lease (query "lighting" :tolglow 0)
        wake (atom 0) detach ((:observe! lease) #(swap! wake inc))
        read! (fn [lease] ((:sample! lease) ((:source! lease))))]
    (try
      (doseq [bad [(query "another" :tolglow 0) (query "lighting" :afterglow 0) (query "lighting" :tolglow -1)]]
        (is (= {:error :unavailable} @(:result bad))))
      (swap! state assoc-in [:sessions "other" :state] :exited)
      (is (zero? @wake))
      (swap! state update-in [:sessions "selected"] #'terminal/append-output bytes 68000)
      (is (= 1 @wake))
      (let [first (read! lease) next (read! (query "lighting" :tolglow (:cursor first)))]
        (is (:dropped? first))
        (is (= 65536 (count (.decode (Base64/getDecoder) ^String (:data first)))))
        (is (false? (:dropped? next)))
        (is (= 70000 (:cursor next) (:available-cursor first))))
      (finally (detach)))
    (is (= before (count (.getWatches ^clojure.lang.Atom state))))))
