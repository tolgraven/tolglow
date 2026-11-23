(ns tolglow.guest-admission-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.render.workers :as workers]))

(deftest role-and-physical-verification-follow-the-authenticated-credential
  (let [op (apply str (repeat 32 "o")) guest (apply str (repeat 32 "g"))
        hub (workers/create {} op {:guest-bridge guest})
        hello (fn [token caps] {:op :hello :token token :caps (merge {:max-result-floats 8192} caps)})]
    (try
      (is (= {:max-result-floats 8192} (workers/admission hub (hello op {}))))
      (is (nil? (workers/admission hub (hello guest {}))))
      (is (nil? (workers/admission hub (hello op {:guest? true :subject "x"}))))
      (is (= :full (:verification (workers/admission hub (hello guest {:guest? true :subject "x" :verification :redundant})))))
      (is (nil? (workers/admission hub (hello guest {:guest? true}))))
      (is (thrown? Exception (workers/create {} guest {:guest-bridge guest})))
      (finally (workers/close! hub)))))
