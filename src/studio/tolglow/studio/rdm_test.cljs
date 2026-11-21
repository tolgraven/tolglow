(ns tolglow.studio.rdm-test
  (:require [cljs.test :refer-macros [deftest is]]
            [re-frame.db :as db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [tolglow.studio.rdm :as rdm]))
(defn effects [event]
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %)) (registrar/get-handler :event (first event))))))
(def show {:runtime-id "runtime" :epoch 2})

(deftest passive-load-has-no-device-operation-and-actions-carry-session-identity
  (let [base {:show show}
        load (rdm/request-fx base :load nil)
        action (rdm/request-fx (assoc-in (:db load) [:ui :rdm :snapshot :session-id] "armed") :discover {:universe 1})]
    (is (= "/api/rdm" (get-in load [:studio/http :path])))
    (is (nil? (get-in load [:studio/http :body])))
    (is (= {:runtime-id "runtime" :expected-epoch 2 :session-id "armed" :universe 1}
           (get-in action [:studio/http :body])))))

(deftest old-replies-cannot-rearm-or-replace-new-show-projections
  (let [request (rdm/request-fx {:show show} :load nil)
        token (second (get-in request [:studio/http :on-success]))
        current (:db request)]
    (reset! db/app-db (assoc current :show (assoc show :epoch 3)))
    (is (empty? (effects [:rdm/reply token :load {:connected? true :session-id "old"}])))
    (is (false? (:busy? (rdm/state @db/app-db))))
    (is (nil? (get-in (rdm/state @db/app-db) [:snapshot :session-id])))
    (reset! db/app-db current)
    (let [reply (effects [:rdm/reply token :load {:connected? false :config {:host "ola.local" :port 9010}}])]
      (is (= "ola.local" (get-in reply [:db :ui :rdm :host])))
      (is (nil? (:studio/http reply))))))

(deftest selection-is-local-and-raw-bytes-validate-before-sending
  (reset! db/app-db {:show show})
  (let [result (effects [:rdm/select-fixture {:id "head-2" :universe 2}])]
    (is (= "head-2" (get-in result [:db :ui :rdm :fixture-id])))
    (is (nil? (:studio/http result))))
  (is (= [0 255 16] (rdm/hex-bytes "00 ff 10")))
  (is (= [] (rdm/hex-bytes "")))
  (is (thrown? js/Error (rdm/hex-bytes "100"))))

(deftest initial-status-cannot-overwrite-connection-fields-edited-while-loading
  (let [request (rdm/request-fx {:show show} :load nil)
        token (second (get-in request [:studio/http :on-success]))]
    (reset! db/app-db (:db request))
    (reset! db/app-db (:db (effects [:rdm/set :port "19210"])))
    (let [reply (effects [:rdm/reply token :load {:config {:host "127.0.0.1" :port 9010}}])]
      (is (= "19210" (get-in reply [:db :ui :rdm :port]))))))
