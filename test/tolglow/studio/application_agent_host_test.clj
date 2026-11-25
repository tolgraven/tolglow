(ns tolglow.studio.application-agent-host-test
  (:require [clojure.test :refer :all]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.development.agents :as agents]
            [tolglow.development.agents-test :as fixture-agent]
            [tolglow.agents.credentials :as credentials]
            [tolglow.development :as development]
            [tolglow.development.workspace :as workspace]
            [tolglow.development.workspace-test :as fixtures]
            [tolglow.studio.web :as web])
  (:import [java.nio.file Files Path LinkOption]
           [java.io ByteArrayInputStream]))

(defn request
  ([app method uri payload query] (request app method uri payload query {}))
  ([app method uri payload query headers]
   (let [response (app {:request-method method :uri uri :query-string query
                        :headers (merge {"host" "localhost"} headers)
                        :body (ByteArrayInputStream. (.getBytes (json/write-str payload) "UTF-8"))})]
     {:status (:status response) :data (json/read-str (:body response) :key-fn keyword)})))

(defn with-host [execute! f]
  (let [root (Files/createTempDirectory "tolglow-application-agent-host-" fixtures/attrs)
        tolglow (fixtures/repo! root "tolglow") afterglow (fixtures/repo! root "afterglow")
        options {:repositories {:tolglow (str tolglow) :afterglow (str afterglow)}
                 :workspace-parent (str (.resolve root "branches"))
                 :runner workspace/run-trusted-process!
                 :check-profiles {:unit {:repo :tolglow :argv ["git" "status" "--porcelain"]}}
                 :app-agent-options {:execute! execute! :credential-store (credentials/file-store (.resolve root "credentials.edn"))}}
        owner (development/create options)
        app (web/app {:app-agent! #(development/app-agent-operation! owner %1 %2)})]
    (try (f {:root root :owner owner :app app :options options})
         (finally (development/close! owner) (fixtures/delete-tree! root)))))

(deftest ring-provider-configuration-persists-references-and-restores-without-execution
  (let [executions (atom 0)]
    (with-host #(do (swap! executions inc) (fixture-agent/completed! %))
      (fn [{:keys [root owner app options]}]
        (let [configured (request app :post "/api/application-agents/configure"
                                  {:provider "codex" :api-key fixture-agent/secret :model "gpt-5.4"} nil)
              settings (.resolve ^Path root "branches/.tolglow-application-agents.edn")]
          (is (= 200 (:status configured)))
          (is (true? (get-in configured [:data :configuration-persisted?])))
          (is (= "gpt-5.4" (get-in configured [:data :providers 0 :model])))
          (is (not (str/includes? (pr-str configured) fixture-agent/secret)))
          (is (not (str/includes? (slurp (str settings)) fixture-agent/secret)))
          (is (str/includes? (slurp (str settings)) "credential-"))
          (is (zero? @executions))
          (development/close! owner)
          (let [restored (development/create options)
                app2 (web/app {:app-agent! #(development/app-agent-operation! restored %1 %2)})]
            (try
              (is (= "gpt-5.4" (get-in (request app2 :get "/api/application-agents" nil nil) [:data :providers 0 :model])))
              (is (zero? @executions))
              (let [started (request app2 :post "/api/application-agents/start"
                                     {:provider "codex" :repo "tolglow" :policy "manual" :prompt "Inspect"} nil)
                    id (get-in started [:data :id])]
                (is (= 200 (:status started)))
                (is (= :completed (:status (fixture-agent/finish (:app-agents restored) id))))
                (is (= 1 @executions)))
              (finally (development/close! restored)))))))))

(deftest ring-task-workflow-exposes-real-diff-progress-checks-and-cancellation
  (let [record (atom nil)]
    (with-host (fixture-agent/fixture-process record)
      (fn [{:keys [owner app]}]
        (request app :post "/api/application-agents/configure" {:api-key fixture-agent/secret} nil)
        (let [started (request app :post "/api/application-agents/start"
                               {:prompt "Edit fixture" :repo "tolglow" :policy "after-turn"} nil)
              id (get-in started [:data :id])]
          (is (= 200 (:status started)))
          (is (= :completed (:status (fixture-agent/finish (:app-agents owner) id))))
          (let [status (request app :get "/api/application-agents/status" nil (str "id=" id))
                events (request app :get "/api/application-agents/events" nil (str "id=" id "&cursor=0"))]
            (is (= "completed" (get-in status [:data :status])))
            (is (= "passed" (get-in status [:data :checks :unit :status])))
            (is (= "ready" (get-in status [:data :candidate :status])))
            (is (= 1 (get-in status [:data :summary :file-count])))
            (is (seq (get-in events [:data :events])))
            (is (not (str/includes? (pr-str [status events]) fixture-agent/secret)))
            (is (= "completed" (get-in (request app :post "/api/application-agents/cancel" {:id id} nil) [:data :status]))))
          (is (= 400 (:status (request app :post "/api/application-agents/start" {:prompt "Invalid" :policy "preview"} nil))))
          (is (= 400 (:status (request app :post "/api/application-agents/start" {:prompt "Invalid" :argv ["sh"]} nil))))
          (is (= 400 (:status (request app :get "/api/application-agents/events" nil (str "id=" id "&cursor=-1")))))
          (is (= 1 (count (:workspaces (workspace/status (:workspaces owner)))))))))))

(deftest configuration-rejects-cross-origin-and-invalid-model-before-vault-writes
  (with-host fixture-agent/completed!
    (fn [{:keys [root app]}]
      (is (= 403 (:status (request app :post "/api/application-agents/configure"
                                  {:api-key fixture-agent/secret} nil {"origin" "https://unrelated.example"}))))
      (is (= 400 (:status (request app :post "/api/application-agents/configure"
                                  {:api-key fixture-agent/secret :model "bad\nmodel"} nil))))
      (is (false? (Files/exists (.resolve ^Path root "credentials.edn") (make-array LinkOption 0))))
      (is (empty? (get-in (request app :get "/api/application-agents" nil nil) [:data :providers]))))))

(deftest unsafe-application-settings-cannot-be-restored-or-replaced
  (fixtures/with-workspace {}
    (fn [{:keys [manager root]}]
      (let [target (.resolve ^Path root "settings.edn")
            outside (.resolve ^Path root "outside.edn")
            owner (agents/create-manager (assoc (fixture-agent/options manager fixture-agent/completed!) :config-path (str target)))]
        (try
          (spit (str outside) "private unrelated file")
          (Files/createSymbolicLink target outside fixtures/attrs)
          (is (thrown? clojure.lang.ExceptionInfo (agents/restore-configuration! owner)))
          (is (thrown? clojure.lang.ExceptionInfo (agents/configure-provider! owner {:credential-ref fixture-agent/reference})))
          (is (= "private unrelated file" (slurp (str outside))))
          (Files/delete target)
          (spit (str target) "{:version 1 :providers {}} {:trailing true}")
          (is (thrown? clojure.lang.ExceptionInfo (agents/restore-configuration! owner)))
          (finally (agents/close! owner)))))))

(deftest application-settings-resolve-configured-parents-but-not-planted-links
  ;; Issue #638: macOS temp/home paths pass through system symbolic links.
  (fixtures/with-workspace {}
    (fn [{:keys [manager root]}]
      (let [real (.toRealPath ^Path root (make-array LinkOption 0))
            linked (.resolve ^Path root "linked-settings")
            options (fixture-agent/options manager fixture-agent/completed!)]
        (Files/createDirectory (.resolve ^Path root "settings-home") fixtures/attrs)
        (Files/createSymbolicLink linked (.resolve real "settings-home") fixtures/attrs)
        (let [owner (agents/create-manager (assoc options :config-path (str (.resolve linked "settings.edn"))))]
          (try
            (is (:configuration-persisted? (agents/configure-provider! owner {:credential-ref fixture-agent/reference})))
            (is (Files/isRegularFile (.resolve real "settings-home/settings.edn") (make-array LinkOption 0)))
            (finally (agents/close! owner))))
        (let [outside (.resolve ^Path root "outside-settings.edn")
              target (.resolve ^Path root "preexisting-link.edn")]
          (spit (str outside) "private unrelated file")
          (Files/createSymbolicLink target outside fixtures/attrs)
          (let [owner (agents/create-manager (assoc options :config-path (str target)))]
            (try
              (is (thrown? clojure.lang.ExceptionInfo (agents/configure-provider! owner {:credential-ref fixture-agent/reference})))
              (is (= "private unrelated file" (slurp (str outside))))
              (finally (agents/close! owner)))))
        (Files/createDirectory (.resolve ^Path root "planted-home") fixtures/attrs)
        (let [owner (agents/create-manager (assoc options :config-path (str (.resolve ^Path root "planted-home/settings.edn"))))]
          (try
            (Files/delete (.resolve ^Path root "planted-home"))
            (Files/createSymbolicLink (.resolve ^Path root "planted-home") (.resolve real "settings-home") fixtures/attrs)
            (is (thrown? clojure.lang.ExceptionInfo (agents/configure-provider! owner {:credential-ref fixture-agent/reference})))
            (finally (agents/close! owner))))))))

(deftest unavailable-application-agent-service-does-not-fall-back-to-show-chat
  (let [app (web/app {})]
    (is (= 503 (:status (request app :get "/api/application-agents" nil nil))))
    (is (= 503 (:status (request app :post "/api/application-agents/start" {:prompt "Edit"} nil))))
    (is (= 404 (:status (request app :post "/api/application-agents/arbitrary" {} nil))))))

(deftest top-level-runtime-wires-the-vault-and-keeps-application-tasks-across-show-restart
  (fixtures/with-workspace {}
    (fn [{:keys [manager root]}]
      (let [entered (promise) release (promise)
            execute! (fn [request] (deliver entered true) @release (fixture-agent/completed! request))
            runtime (show/create-runtime
                     {:credential-store (credentials/file-store (.resolve ^Path root "runtime-credentials.edn"))
                      :workspace-options {:repositories (into {} (map (fn [[k v]] [k (str v)])) (:repositories manager))
                                          :workspace-parent (str (:parent manager))
                                          :runner workspace/run-trusted-process!
                                          :check-profiles {:unit {:repo :tolglow :argv ["git" "status" "--porcelain"]}}
                                          :app-agent-options {:execute! execute!}}})
            app (studio/handler runtime)]
        (try
          (is (= 400 (:status (request app :post "/api/application-agents/configure"
                                      {:runtime-id "previous-server" :api-key fixture-agent/secret} nil))))
          (is (= 200 (:status (request app :post "/api/application-agents/configure"
                                      {:runtime-id (:id runtime) :api-key fixture-agent/secret} nil))))
          (let [started (request app :post "/api/application-agents/start"
                                 {:runtime-id (:id runtime) :prompt "Wait during show restart"} nil)
                id (get-in started [:data :id])
                owner (get-in runtime [:development :app-agents])
                epoch (get-in @(:state runtime) [:model :epoch])]
            (is (= 200 (:status started)))
            (is (true? (deref entered 5000 false)))
            (let [receipt (show/submit! runtime {:op :show/restart})]
              (show/tick! runtime 0)
              (is (= :accepted (:status (deref receipt 1000 {})))))
            (is (> (get-in @(:state runtime) [:model :epoch]) epoch))
            (is (= :running (:status (agents/status owner id))))
            (deliver release true)
            (is (= :completed (:status (fixture-agent/finish owner id))))
            (is (= "completed" (get-in (request app :get "/api/application-agents/status" nil (str "id=" id)) [:data :status]))))
          (finally (deliver release true) (show/stop! runtime)))))))
