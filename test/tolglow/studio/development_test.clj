(ns tolglow.studio.development-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data.json :as json]
            [tolglow.studio.development :as development]
            [tolglow.studio.workspace :as workspace]
            [tolglow.studio.workspace-test :as fixtures]
            [tolglow.studio.web :as web])
  (:import [java.nio.file Files]
           [java.io ByteArrayInputStream]))

(defn- request [app method uri data query]
  (let [response (app {:request-method method :uri uri :query-string query
                       :headers {"host" "localhost"}
                       :body (ByteArrayInputStream. (.getBytes (json/write-str data) "UTF-8"))})]
    {:status (:status response) :data (json/read-str (:body response) :key-fn keyword)}))

(deftest ring-workspace-editing-keeps-originals-and-check-gates
  (let [root (Files/createTempDirectory "tolglow-development-host-" fixtures/attrs)
        tolglow (fixtures/repo! root "tolglow") afterglow (fixtures/repo! root "afterglow")
        manager (development/create {:repositories {:tolglow (str tolglow) :afterglow (str afterglow)}
                                     :workspace-parent (str (.resolve root "branches"))
                                     :runner workspace/run-trusted-process!
                                     :check-profiles {:unit {:repo :tolglow :argv ["git" "status" "--porcelain"]}}})
        app (web/app {:workspace! #(development/operation! manager %1 %2)})]
    (try
      (is (true? (get-in (request app :get "/api/workspaces" nil nil) [:data :runner-ready?])))
      (is (= "ready" (get-in (request app :post "/api/workspaces/create" {:id "web-edit"} nil) [:data :state])))
      (let [read (:data (request app :get "/api/workspaces/file" nil "id=web-edit&repo=tolglow&path=src%2Fcore.clj"))
            payload {:id "web-edit" :repo "tolglow" :path "src/core.clj" :content "(ns fixture.core)\n(def value 7)\n"
                     :expected-hash (:hash read)}]
        (is (= 200 (:status (request app :post "/api/workspaces/write" payload nil))))
        (is (= 400 (:status (request app :post "/api/workspaces/write" payload nil))))
        (is (= "(ns fixture.core)\n(def value 1)\n" (slurp (str (.resolve tolglow "src/core.clj")))))
        (is (.contains (get-in (request app :get "/api/workspaces/diff" nil "id=web-edit")
                              [:data :repositories :tolglow :diff]) "+(def value 7)")))
      (is (= 400 (:status (request app :get "/api/workspaces/file" nil "id=web-edit&repo=tolglow&path=..%2F..%2Ftolglow%2Fsrc%2Fcore.clj"))))
      (is (= 400 (:status (request app :post "/api/workspaces/stage" {:id "web-edit" :required-checks []} nil)))
          "A caller cannot bypass configured validation")
      (is (= 400 (:status (request app :post "/api/workspaces/check" {:id "web-edit" :profile "arbitrary" :argv ["false"]} nil))))
      (let [run (workspace/check! (:workspaces manager) "web-edit" :unit)]
        (is (= :passed (:status (deref (:result run) 10000 {})))))
      (is (= "ready" (get-in (request app :post "/api/workspaces/stage" {:id "web-edit"} nil) [:data :status])))
      (finally (development/close! manager) (fixtures/delete-tree! root)))))

(deftest absent-workspace-runner-cannot-execute-host-project-code
  (is (= {:available? false :workspaces [] :profiles [] :runner-ready? false}
         (development/operation! nil :list {})))
  (is (thrown? clojure.lang.ExceptionInfo (development/operation! nil :create {}))))
