(ns tolglow.development.workspace-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [tolglow.development.workspace :as workspace])
  (:import [java.nio.file Files Path LinkOption]
           [java.nio.file.attribute FileAttribute PosixFilePermissions]
           [java.util.concurrent CountDownLatch TimeUnit]))

(def attrs (make-array FileAttribute 0))

(defn command! [cwd argv]
  (let [result (workspace/run-trusted-process! {:cwd (str cwd) :argv argv :timeout-ms 5000 :max-output-bytes 32768})]
    (when-not (= 0 (:exit-code result)) (throw (ex-info "Temporary repository fixture failed" result))) result))

(defn repo! [^Path parent name]
  (let [root (.resolve parent name)]
    (Files/createDirectory root attrs)
    (Files/createDirectory (.resolve root "src") attrs)
    (spit (str (.resolve root "src/core.clj")) "(ns fixture.core)\n(def value 1)\n")
    (spit (str (.resolve root ".gitignore")) "ignored/\n")
    ;; Otherwise repositories created within one second can have identical
    ;; initial commits and accidentally hide cross-repository Git redirection.
    (command! root ["git" "init" "--initial-branch=main"])
    (command! root ["git" "add" "."])
    (command! root ["git" "-c" "user.name=Workspace test" "-c" "user.email=workspace@example.invalid" "commit" "-m" (str "Initial test fixture " name)])
    root))

(defn delete-tree! [^Path root]
  (when (Files/exists root (make-array LinkOption 0))
    (with-open [paths (Files/walk root (make-array java.nio.file.FileVisitOption 0))]
      (doseq [^Path p (reverse (sort-by #(.getNameCount ^Path %) (iterator-seq (.iterator paths))))]
        (Files/deleteIfExists p)))))

(defn with-workspace [options f]
  (let [root (Files/createTempDirectory "tolglow-workspace-test-" attrs)
        first (repo! root "original-tolglow") second (repo! root "original-afterglow")
        parent (.resolve root "agent-workspaces")
        manager (workspace/create-manager
                 (merge {:repositories {:tolglow (str first) :afterglow (str second)} :workspace-parent (str parent)
                         :check-profiles {:unit {:repo :tolglow :argv ["git" "status" "--porcelain"] :timeout-ms 5000}}} options))]
    (try (f {:manager manager :root root :first first :second second :parent parent})
         (finally (workspace/close! manager) (delete-tree! root)))))

(defn- replacement-manager [{:keys [first second parent]}]
  (workspace/create-manager {:repositories {:tolglow (str first) :afterglow (str second)}
                             :workspace-parent (str parent)
                             :runner workspace/run-trusted-process!
                             :check-profiles {:unit {:repo :tolglow :argv ["git" "status" "--porcelain"] :timeout-ms 5000}}}))

(deftest issued-workspace-catalog-restores-dirty-worktrees-without-reviving-check-approval
  (with-workspace {:runner workspace/run-trusted-process!}
    (fn [{:keys [manager parent] :as fixture}]
      (workspace/create! manager {:id "durable"})
      (let [original (workspace/read-file manager "durable" :tolglow "src/core.clj")
            content "(ns fixture.core)\n(def value :keep-my-uncommitted-edit)\n"
            saved (workspace/write-file! manager "durable" :tolglow "src/core.clj"
                                         {:expected-hash (:hash original) :content content})
            paths (workspace/runtime-paths manager "durable")
            check (workspace/check! manager "durable" :unit)]
        (is (= :passed (:status (deref (:result check) 5000 {:status :timeout}))))
        (workspace/stage-preview! manager "durable" {:required-checks [:unit]})
        (workspace/close! manager)
        (let [restored (replacement-manager fixture)]
          (try
            (is (empty? (:workspaces (workspace/status restored))))
            (is (= {:recovered 1} (workspace/recover! restored)))
            (is (= content (:content (workspace/read-file restored "durable" :tolglow "src/core.clj"))))
            (is (= (:hash saved) (:hash (workspace/read-file restored "durable" :tolglow "src/core.clj"))))
            (is (= paths (workspace/runtime-paths restored "durable")))
            (is (empty? (:checks (workspace/status restored "durable"))))
            (is (nil? (:preview (workspace/status restored "durable"))))
            (is (thrown? clojure.lang.ExceptionInfo (workspace/stage-preview! restored "durable" {:required-checks [:unit]})))
            (is (thrown? clojure.lang.ExceptionInfo (workspace/recover! restored)))
            (finally (workspace/close! restored))))))))

(deftest catalog-recovery-rejects-tampered-mounts-without-adopting-any-workspace
  (with-workspace {}
    (fn [{:keys [manager parent first] :as fixture}]
      (workspace/create! manager {:id "issued"})
      (let [catalog (.resolve ^Path parent ".tolglow-workspaces.edn")
            saved (edn/read-string (slurp (str catalog)))]
        (workspace/close! manager)
        (spit (str catalog) (pr-str (assoc-in saved [:workspaces 0 :repositories :tolglow :path] (str first))))
        (let [restored (replacement-manager fixture)]
          (try
            (is (thrown? clojure.lang.ExceptionInfo (workspace/recover! restored)))
            (is (empty? (:workspaces (workspace/status restored))))
            (finally (workspace/close! restored))))))))

(deftest catalog-reader-does-not-follow-links-or-accept-trailing-documents
  (with-workspace {}
    (fn [{:keys [manager parent root] :as fixture}]
      (workspace/create! manager {:id "issued"})
      (let [catalog (.resolve ^Path parent ".tolglow-workspaces.edn")
            saved (slurp (str catalog)) target (.resolve ^Path root "outside-catalog")]
        (workspace/close! manager)
        (spit (str target) saved)
        (Files/delete catalog)
        (Files/createSymbolicLink catalog target attrs)
        (let [restored (replacement-manager fixture)]
          (try
            (is (thrown? clojure.lang.ExceptionInfo (workspace/recover! restored)))
            (is (empty? (:workspaces (workspace/status restored))))
            (Files/delete catalog)
            (spit (str catalog) (str saved "\n{:extra :document}"))
            (is (thrown? clojure.lang.ExceptionInfo (workspace/recover! restored)))
            (is (empty? (:workspaces (workspace/status restored))))
            (finally (workspace/close! restored))))))))

(deftest two-studio-owners-do-not-overwrite-each-others-issued-catalog
  (with-workspace {}
    (fn [{:keys [manager] :as fixture}]
      (let [second-owner (replacement-manager fixture)]
        (try
          (workspace/create! manager {:id "first-owner"})
          ;; The second owner predates the first addition and has no in-memory
          ;; knowledge of it. Its atomic save must preserve that issued entry.
          (workspace/create! second-owner {:id "second-owner"})
          (let [original (workspace/read-file manager "first-owner" :tolglow "src/core.clj")]
            (workspace/write-file! manager "first-owner" :tolglow "src/core.clj"
                                   {:expected-hash (:hash original) :content "still-owned"}))
          (let [restored (replacement-manager fixture)]
            (try
              (is (= {:recovered 2} (workspace/recover! restored)))
              (is (= #{"first-owner" "second-owner"} (set (map :id (:workspaces (workspace/status restored))))))
              (is (= "still-owned" (:content (workspace/read-file restored "first-owner" :tolglow "src/core.clj"))))
              (finally (workspace/close! restored))))
          (finally (workspace/close! second-owner)))))))

(deftest construction-is-inert-and-create-uses-paired-isolated-branches
  (with-workspace {}
    (fn [{:keys [manager first second parent]}]
      (is (false? (Files/exists parent (make-array LinkOption 0))))
      (spit (str (.resolve ^Path first "uncommitted.clj")) "user's ongoing edit")
      (let [created (workspace/create! manager {:id "session-one"})]
        (is (= :ready (:state created)))
        (is (= #{:tolglow :afterglow} (set (keys (:repositories created)))))
        (is (= "tolglow-agent/session-one" (get-in created [:repositories :tolglow :branch])))
        (is (= "main" (str/trim (:output (command! first ["git" "branch" "--show-current"])))))
        (is (= "main" (str/trim (:output (command! second ["git" "branch" "--show-current"])))))
        (is (not (some #(= "uncommitted.clj" (:path %)) (:entries (workspace/tree manager "session-one" :tolglow)))))
        (is (= "user's ongoing edit" (slurp (str (.resolve ^Path first "uncommitted.clj")))))
        (is (thrown? clojure.lang.ExceptionInfo (workspace/create! manager {:id "session-one"})))))))

(deftest writes-check-hash-and-change-only-the-owned-worktree
  (with-workspace {}
    (fn [{:keys [manager first]}]
      (workspace/create! manager {:id "edit"})
      (let [original (workspace/read-file manager "edit" :tolglow "src/core.clj")
            changed (workspace/write-file! manager "edit" :tolglow "src/core.clj"
                                           {:expected-hash (:hash original) :content "(ns fixture.core)\n(def value 2)\n"})]
        (is (= 64 (count (:hash changed))))
        (is (not= (:hash original) (:hash changed)))
        (is (= 1 (:revision changed)))
        (is (= "(ns fixture.core)\n(def value 1)\n" (slurp (str (.resolve ^Path first "src/core.clj")))))
        (is (thrown? clojure.lang.ExceptionInfo
                     (workspace/write-file! manager "edit" :tolglow "src/core.clj" {:expected-hash (:hash original) :content "stale"})))
        (is (str/includes? (get-in (workspace/diff manager "edit") [:repositories :tolglow :diff]) "+(def value 2)")))
      (let [new (workspace/write-file! manager "edit" :afterglow "src/added.clj" {:expected-hash :missing :content "(ns fixture.added)"})]
        (is (= "(ns fixture.added)" (:content new)))
        (is (str/includes? (get-in (workspace/diff manager "edit") [:repositories :afterglow :status]) "src/added.clj"))))))

(deftest atomic-replacement-preserves-executable-permissions
  (with-workspace {}
    (fn [{:keys [manager]}]
      (let [created (workspace/create! manager {:id "permissions"})
            file (.resolve (java.nio.file.Paths/get (get-in created [:repositories :tolglow :path]) (make-array String 0)) "src/core.clj")
            permissions (PosixFilePermissions/fromString "rwxr-x---")]
        (Files/setPosixFilePermissions file permissions)
        (let [read (workspace/read-file manager "permissions" :tolglow "src/core.clj")]
          (workspace/write-file! manager "permissions" :tolglow "src/core.clj" {:expected-hash (:hash read) :content "changed"})
          (is (= permissions (Files/getPosixFilePermissions file (make-array LinkOption 0)))))))))

(deftest traversal-metadata-and-symbolic-links-cannot-escape-the-owned-tree
  (with-workspace {}
    (fn [{:keys [manager root]}]
      (let [created (workspace/create! manager {:id "confined"})
            worktree (java.nio.file.Paths/get (get-in created [:repositories :tolglow :path]) (make-array String 0))
            outside (.resolve ^Path root "outside-secret.txt")]
        (spit (str outside) "do-not-read-or-change")
        (Files/createSymbolicLink (.resolve worktree "escape") root attrs)
        (Files/createSymbolicLink (.resolve worktree "linked.txt") outside attrs)
        (doseq [relative ["../outside-secret.txt" "/etc/passwd" ".git" ".git/config" "src/../core.clj" "escape/outside-secret.txt" "linked.txt"]]
          (is (thrown? clojure.lang.ExceptionInfo (workspace/read-file manager "confined" :tolglow relative)))
          (is (thrown? clojure.lang.ExceptionInfo (workspace/write-file! manager "confined" :tolglow relative {:expected-hash :missing :content "no"}))))
        (is (= "do-not-read-or-change" (slurp (str outside))))
        (is (false? (:editable? (some #(when (= "linked.txt" (:path %)) %) (:entries (workspace/tree manager "confined" :tolglow))))))
        (is (thrown? clojure.lang.ExceptionInfo (workspace/fingerprint manager "confined")))
        (is (thrown? clojure.lang.ExceptionInfo
                     (workspace/write-file! manager "confined" :tolglow "missing-parent/file.clj" {:expected-hash :missing :content "new"})))))))

(deftest bounded-tree-and-binary-read-report-real-files
  (with-workspace {}
    (fn [{:keys [manager]}]
      (let [created (workspace/create! manager {:id "listing"})
            worktree (java.nio.file.Paths/get (get-in created [:repositories :tolglow :path]) (make-array String 0))]
        (Files/write (.resolve worktree "binary.bin") (byte-array [0 1 -1 2]) (make-array java.nio.file.OpenOption 0))
        (let [first (workspace/tree manager "listing" :tolglow {:limit 1})
              next (workspace/tree manager "listing" :tolglow {:offset (:next-offset first) :limit 1})]
          (is (= 1 (count (:entries first))))
          (is (not= (:entries first) (:entries next)))
          (is (= 3 (:total first))))
        (let [binary (workspace/read-file manager "listing" :tolglow "binary.bin")]
          (is (:binary? binary)) (is (nil? (:content binary))) (is (= 4 (:size binary))))))))

(deftest checks-require-runner-and-preview-requires-current-passed-fingerprint
  (with-workspace {}
    (fn [{:keys [manager]}]
      (workspace/create! manager {:id "inert"})
      (is (= :runner-required (:workspace/error (ex-data (try (workspace/check! manager "inert" :unit)
                                                             (catch clojure.lang.ExceptionInfo error error))))))
      (is (thrown? clojure.lang.ExceptionInfo (workspace/stage-preview! manager "inert" {})))))
  (with-workspace {:runner workspace/run-trusted-process!}
    (fn [{:keys [manager]}]
      (workspace/create! manager {:id "checked"})
      (let [check (workspace/check! manager "checked" :unit) result (deref (:result check) 5000 nil)]
        (is (= :passed (:status result)))
        (is (= :ready (:status (workspace/stage-preview! manager "checked" {}))))
        (let [file (workspace/read-file manager "checked" :tolglow "src/core.clj")]
          (workspace/write-file! manager "checked" :tolglow "src/core.clj" {:expected-hash (:hash file) :content "new version"})
          (is (nil? (:preview (workspace/status manager "checked"))))
          (is (thrown? clojure.lang.ExceptionInfo (workspace/stage-preview! manager "checked" {}))))))))

(deftest external-edits-during-check-make-the-result-stale
  (let [entered (promise) release (CountDownLatch. 1)]
    (with-workspace {:runner (fn [request] (deliver entered request) (.await release 3 TimeUnit/SECONDS)
                               {:exit-code 0 :output "passed old files" :timed-out? false :truncated? false})}
      (fn [{:keys [manager]}]
        (workspace/create! manager {:id "racing"})
        (let [run (workspace/check! manager "racing" :unit) request (deref entered 2000 nil)]
          (is (= (:run-id run) (get-in (workspace/status manager "racing") [:running-checks :unit :run-id])))
          (is (= "racing" (:workspace-id request)))
          (is (str/ends-with? (:workspace-root request) "/racing"))
          (is (str/ends-with? (:cwd request) "/racing/tolglow"))
          (spit (str (:cwd request) "/src/core.clj") "external editor changed it")
          (.countDown release)
          (is (= :stale (:status (deref (:result run) 5000 nil))))
          (is (thrown? clojure.lang.ExceptionInfo (workspace/stage-preview! manager "racing" {}))))))))

(deftest trusted-process-runner-bounds-output-timeout-and-cancellation
  (let [result (workspace/run-trusted-process! {:cwd "/tmp" :argv ["python3" "-c" "print('x'*10000)"] :timeout-ms 2000 :max-output-bytes 32})]
    (is (= 0 (:exit-code result))) (is (:truncated? result)) (is (= 32 (count (:output result)))))
  (let [result (workspace/run-trusted-process! {:cwd "/tmp" :argv ["python3" "-c" "import time; time.sleep(5)"]
                                              :stdin (apply str (repeat 200000 "x")) :timeout-ms 100 :max-output-bytes 32})]
    (is (:timed-out? result)))
  (let [entered (promise) gate (CountDownLatch. 1)]
    (with-workspace {:runner (fn [_] (deliver entered true) (.await gate 5 TimeUnit/SECONDS) {:exit-code 0 :output ""})}
      (fn [{:keys [manager]}]
        (workspace/create! manager {:id "cancel"})
        (let [run (workspace/check! manager "cancel" :unit)]
          (is (true? (deref entered 2000 false)))
          (is ((:cancel! run)))
          (is (= :cancelled (:status (deref (:result run) 2000 nil))))
          (is (:closed? (workspace/close! manager)))
          (is (empty? (:running-checks (workspace/status manager "cancel")))))
        (is (Files/exists (.resolve ^Path (:parent manager) "cancel/tolglow") (make-array LinkOption 0)))))))

(deftest captured-git-metadata-is-not-redirected-by-terminal-edits
  (with-workspace {:runner (fn [_] {:exit-code 0 :output "Checked owned files"})}
    (fn [{:keys [manager first second] :as fixture}]
      (workspace/create! manager {:id "metadata"})
      (let [paths (workspace/runtime-paths manager "metadata")
            repo (get-in paths [:repositories :tolglow])
            before (workspace/fingerprint manager "metadata")]
        (is (not= (:output (command! first ["git" "rev-parse" "HEAD"]))
                  (:output (command! second ["git" "rev-parse" "HEAD"]))))
        (is (= "metadata" (:workspace-id paths)))
        (is (str/includes? (:git-dir repo) "/.git/worktrees/"))
        (is (nil? (get-in (workspace/status manager "metadata") [:repositories :tolglow :git-dir])))
        (spit (str (:path repo) "/.git") (str "gitdir: " second "/.git\n"))
        (spit (str (:git-dir repo) "/commondir") (str second "/.git\n"))
        (is (= before (workspace/fingerprint manager "metadata")))
        (let [file (workspace/read-file manager "metadata" :tolglow "src/core.clj")]
          (workspace/write-file! manager "metadata" :tolglow "src/core.clj" {:expected-hash (:hash file) :content "owned version"}))
        (is (str/includes? (get-in (workspace/diff manager "metadata") [:repositories :tolglow :diff]) "+owned version"))
        (is (= repo (get-in (workspace/runtime-paths manager "metadata") [:repositories :tolglow])))
        (is (not= before (workspace/fingerprint manager "metadata")))
        (is (= :passed (:status (deref (:result (workspace/check! manager "metadata" :unit)) 5000 nil)))))
      (workspace/close! manager)
      (let [restored (replacement-manager fixture)]
        (try
          (is (= {:recovered 1} (workspace/recover! restored)))
          (is (str/includes? (get-in (workspace/diff restored "metadata") [:repositories :tolglow :diff]) "+owned version"))
          (finally (workspace/close! restored)))))))

(deftest captured-git-reads-follow-current-owned-head-and-index
  (with-workspace {}
    (fn [{:keys [manager second]}]
      (workspace/create! manager {:id "current"})
      (let [repo (get-in (workspace/runtime-paths manager "current") [:repositories :tolglow])
            root (:path repo)]
        (spit (str root "/committed.txt") "committed in owned branch")
        (command! root ["git" "add" "committed.txt"])
        (command! root ["git" "-c" "user.name=Test" "-c" "user.email=test@example.invalid" "commit" "-m" "Owned commit"])
        (spit (str root "/staged.txt") "owned staged change")
        (command! root ["git" "add" "staged.txt"])
        (command! root ["git" "update-index" "--split-index"])
        (let [before (workspace/fingerprint manager "current")]
          (spit (str root "/.git") (str "gitdir: " second "/.git\n"))
          (spit (str (:git-dir repo) "/commondir") (str second "/.git\n"))
          (let [{:keys [diff status]} (get-in (workspace/diff manager "current") [:repositories :tolglow])]
            (is (str/includes? diff "+owned staged change"))
            (is (not (str/includes? diff "committed in owned branch")))
            (is (str/includes? status "A  staged.txt")))
          (is (= before (workspace/fingerprint manager "current"))))))))

(deftest private-git-read-views-are-cleaned-after-success-failure-and-cancellation
  (with-workspace {}
    (fn [{:keys [manager]}]
      (workspace/create! manager {:id "cleanup"})
      (let [original workspace/run-trusted-process! views (atom [])]
        (doseq [failure [nil (ex-info "Injected process failure" {}) (InterruptedException. "Injected cancellation")]]
          (with-redefs [workspace/run-trusted-process!
                        (fn [request]
                          (when-let [view (get-in request [:environment "GIT_DIR"])]
                            (swap! views conj (java.nio.file.Paths/get view (make-array String 0)))
                            (is (Files/isDirectory (last @views) (make-array LinkOption 0))))
                          (if failure (throw failure) (original request)))]
            (if failure
              (is (identical? failure (try (workspace/diff manager "cleanup") (catch Throwable error error))))
              (is (= "cleanup" (:workspace-id (workspace/diff manager "cleanup")))))))
        (is (seq @views))
        (is (every? #(not (Files/exists ^Path % (make-array LinkOption 0))) @views))))))

(deftest metadata-discovery-failure-removes-every-newly-owned-worktree
  (with-workspace {}
    (fn [{:keys [manager first second parent]}]
      (let [git-var #'workspace/git! original @git-var calls (atom 0)]
        (with-redefs-fn
          {git-var (fn [directory arguments]
                     (if (and (= "config" (clojure.core/first arguments))
                              (= 2 (swap! calls inc)))
                       (throw (ex-info "Injected metadata discovery failure"
                                       {:workspace/error :fixture-failure}))
                       (original directory arguments)))}
          #(is (thrown? clojure.lang.ExceptionInfo
                        (workspace/create! manager {:id "failed-pair"}))))
        (is (= :error (:state (workspace/status manager "failed-pair"))))
        (is (= 2 @calls)))
      (doseq [repo [first second]]
        (let [listing (:output (command! repo ["git" "worktree" "list" "--porcelain"]))]
          (is (not (str/includes? listing "failed-pair")))))
      (doseq [repo-name ["tolglow" "afterglow"]]
        (is (not (Files/exists (.resolve ^Path parent (str "failed-pair/" repo-name))
                               (make-array LinkOption 0))))))))

(deftest streamed-fingerprints-cover-real-repository-sized-assets
  (with-workspace {}
    (fn [{:keys [manager]}]
      (let [created (workspace/create! manager {:id "large-assets"})
            root (get-in created [:repositories :afterglow :path])]
        ;; Sparse regular files exercise >256 MiB hashing without consuming that
        ;; much test disk. The real Afterglow repo has ~288 MiB of tracked assets.
        (doseq [n (range 6)]
          (with-open [file (java.io.RandomAccessFile. (str root "/asset-" n ".bin") "rw")]
            (.setLength file (* 48 1024 1024))))
        (let [fingerprint (workspace/fingerprint manager "large-assets")]
          (is (= #{:tolglow :afterglow} (set (keys fingerprint))))
          (is (every? #(re-matches #"[0-9a-f]{64}" %) (vals fingerprint))))))))

(deftest fingerprint-limits-remain-explicit-and-enforced
  (doseq [[options code] [[{:fingerprint-max-file-bytes 16 :fingerprint-max-bytes 1024} :file-limit]
                         [{:fingerprint-max-file-bytes 64 :fingerprint-max-bytes 64} :fingerprint-limit]]]
    (with-workspace options
      (fn [{:keys [manager]}]
        (workspace/create! manager {:id "bounded"})
        (let [error (try (workspace/fingerprint manager "bounded") (catch clojure.lang.ExceptionInfo error error))]
          (is (= code (:workspace/error (ex-data error)))))))))

(deftest workspace-status-wakes-only-for-its-selected-workspace
  (with-workspace {}
    (fn [{:keys [manager]}]
      (workspace/create! manager {:id "one"})
      (workspace/create! manager {:id "two"})
      (let [{:keys [observe! source! sample!]} (workspace/status-source manager "one")
            updates (atom 0)
            detach! (observe! #(swap! updates inc))
            write! (fn [id text] (workspace/write-file! manager id :tolglow "edit.clj"
                                                         {:expected-hash :missing :content text}))]
        (try
          (write! "two" "2")
          (is (zero? @updates))
          (write! "one" "1")
          (is (pos? @updates))
          (is (= (workspace/status manager "one") (sample! (source!))))
          (finally (detach!)))
        (let [before @updates]
          (workspace/write-file! manager "one" :tolglow "more.clj" {:expected-hash :missing :content "3"})
          (is (= before @updates)))))))
