(ns tolglow.guest-admission-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.render.workers :as workers]
            [tolglow.studio.origin :as origin]))

(deftest role-and-physical-verification-follow-the-authenticated-credential
  (let [op (apply str (repeat 32 "o")) guest (apply str (repeat 32 "g"))
        hub (workers/create {} op {:guest-bridge guest})
        hello (fn [token caps] {:op :hello :token token :caps (merge {:max-result-floats 8192} caps)})]
    (try
      (is (= {:max-result-floats 8192} (workers/admission hub true (hello op {}))))
      (is (nil? (workers/admission hub true (hello guest {}))))
      (is (nil? (workers/admission hub true (hello op {:guest? true :subject "x"}))))
      (is (= :full (:verification (workers/admission hub true (hello guest {:guest? true :subject "x" :verification :redundant})))))
      (is (nil? (workers/admission hub true (hello guest {:guest? true}))))
      (is (nil? (workers/admission hub false (hello guest {:guest? true :subject "x"}))))
      (is (= {:max-result-floats 8192} (workers/admission hub false (hello op {}))))
      (is (thrown? Exception (workers/create {} guest {:guest-bridge guest})))
      (finally (workers/close! hub)))))

(deftest stage-admission-is-bound-to-the-checked-direct-transport
  (let [local {:remote-addr "127.0.0.1" :scheme :http
               :headers {"origin" "http://127.0.0.1:8500" "host" "127.0.0.1:8500"}}
        proxied (assoc local ::origin/proxied? true)]
    (is (#'workers/stage-transport? local))
    (is (false? (#'workers/stage-transport? proxied)))
    (is (false? (#'workers/stage-transport? (assoc local :remote-addr "192.0.2.4"))))
    (is (false? (#'workers/stage-transport? (assoc-in local [:headers "origin"] "https://show.example"))))
    (doseq [header ["via" "forwarded" "x-forwarded-host" "x-forwarded-for" "x-forwarded-proto" "x-real-ip"]]
      (is (false? (#'workers/stage-transport? (assoc-in local [:headers header] "proxy")))))))
