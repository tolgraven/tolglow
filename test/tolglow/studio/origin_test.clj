(ns tolglow.studio.origin-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.studio.origin :as origin]))

(deftest browser-origin-includes-scheme-host-and-effective-port
  (let [request {:scheme :https :headers {"host" "Stage.test:443"}}]
    (is (origin/same-origin? request))
    (is (origin/same-origin? (assoc-in request [:headers "origin"] "https://stage.test")))
    (doseq [origin-value ["http://stage.test:443" "https://stage.test:8443"
                          "https://another.test" "null" "not an origin"
                          "https://stage.test/different" "https://stage.test?query" "https://name@stage.test"]]
      (is (not (origin/same-origin? (assoc-in request [:headers "origin"] origin-value)))))
    (is (not (origin/same-origin? (assoc-in request [:headers "sec-fetch-site"] "cross-site"))))))

(deftest listener-hosts-are-checked-without-resolving-user-input
  (let [invoke (fn [bind-host aliases header]
                 ((origin/wrap-listener-host (constantly {:status 200}) bind-host aliases)
                  {:headers {"host" header}}))]
    (doseq [host ["localhost:8500" "127.0.0.1:8500" "[::1]:8500"]]
      (is (= 200 (:status (invoke "127.0.0.1" nil host)))))
    (is (= 403 (:status (invoke "127.0.0.1" nil "unknown.test:8500"))))
    (is (= 200 (:status (invoke "127.0.0.1" ["desk.test"] "DESK.test:8500"))))
    (is (= 200 (:status (invoke "0.0.0.0" nil "controller.local:8500"))))
    (is (= 403 (:status (invoke "::" ["desk.test"] "unknown.test:8500"))))))
