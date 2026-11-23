(ns tolglow.studio.rdm-test
  (:require [cljs.test :refer-macros [deftest is]]
            [re-frame.db :as db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [tolglow.studio.rdm :as rdm]))

(defn effects [event]
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %)) (registrar/get-handler :event (first event))))))
(def show {:runtime-id "runtime" :epoch 2})
(def snapshot-path [:server-projection :view :subscriptions [:rdm/status] :value])
(defn- base [snapshot] (assoc-in {:show show} snapshot-path snapshot))
(defn- dispatch! [e] (reset! db/app-db (:db (effects e))))

(deftest device-actions-carry-pushed-session-identity-and-transit-values
  (let [db (base {:connected? true :session-id "armed"})
        action (rdm/request-fx db :discover {:universe 1 :full? false})]
    (is (= "/api/rdm/discover" (get-in action [:studio/http :path])))
    (is (true? (get-in action [:studio/http :typed?])))
    (is (= {:runtime-id "runtime" :expected-epoch 2 :session-id "armed" :universe 1 :full? false}
           (get-in action [:studio/http :body])))))

(deftest old-action-reply-cannot-rearm-or-change-a-new-show
  (let [request (rdm/request-fx (base {}) :connect {})
        token (second (get-in request [:studio/http :on-success]))]
    (reset! db/app-db (assoc (:db request) :show (assoc show :epoch 3)))
    (dispatch! [:rdm/reply token :connect {:session-id "old"}])
    (is (false? (:busy? (rdm/state @db/app-db))))
    (is (nil? (:notice (rdm/state @db/app-db))))
    (is (nil? (get-in (rdm/state @db/app-db) [:snapshot :session-id])))))

(deftest transport-switches-load-appropriate-fields-and-preserve-edits-on-push
  (reset! db/app-db (base {:config {:transport :ola :host "ola.local" :port 9010}}))
  (is (= "ola.local" (:host (rdm/state @db/app-db))))
  (dispatch! [:rdm/set :transport "art-net"])
  (is (= "6454" (:port (rdm/state @db/app-db))))
  (dispatch! [:rdm/set :host "127.0.0.1"])
  (dispatch! [:rdm/set :controller-uid "7a70:01020304"])
  (swap! db/app-db assoc-in (conj snapshot-path :jobs) [{:status :completed}])
  (is (= {:transport :art-net :host "127.0.0.1" :port 6454 :local-port 6454 :bind-index 1
          :physical-port 1 :controller-uid "7a70:01020304" :timeout-ms 2000 :job-timeout-ms 30000}
         (rdm/config (rdm/state @db/app-db))))
  (dispatch! [:rdm/set :transport "ola"])
  (is (= "9010" (:port (rdm/state @db/app-db)))))

(deftest selection-is-local-and-raw-bytes-validate-before-sending
  (reset! db/app-db (base {:bindings {:front/head {:uid "1234:01020304"}}}))
  (let [result (effects [:rdm/select-fixture {:id :front/head :universe 2}])]
    (is (= :front/head (get-in result [:db :ui :rdm :fixture-id])))
    (is (= "1234:01020304" (get-in result [:db :ui :rdm :uid])))
    (is (nil? (:studio/http result))))
  (is (= [0 255 16] (rdm/hex-bytes "00 ff 10")))
  (is (= [] (rdm/hex-bytes "")))
  (is (thrown? js/Error (rdm/hex-bytes "100"))))
