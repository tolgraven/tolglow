(ns tolglow.studio.rdm-host-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data.json :as json]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.application :as app]
            [tolglow.rig.rdm :as rdm]
            [tolglow.transit :as transit]
            [tolglow.rig.rdm-test :as peer])
  (:import [java.io ByteArrayInputStream]))

(defn request [runtime operation payload & [typed?]]
  (let [snapshot (show/snapshot runtime)
        payload (merge {:runtime-id (:runtime-id snapshot) :expected-epoch (:epoch snapshot)} payload)
        result ((studio/handler runtime)
                {:request-method (if operation :post :get) :uri (str "/api/rdm" (when operation (str "/" (name operation))))
                 :headers {"host" "localhost" "accept" (if typed? "application/transit+json" "application/json")
                           "content-type" (if typed? "application/transit+json" "application/json")}
                 :body (ByteArrayInputStream. (.getBytes (if typed? (transit/encode payload) (json/write-str payload)) "UTF-8"))})]
    (assoc result :data (if typed? (transit/decode (:body result)) (json/read-str (:body result) :key-fn keyword)))))

(defn wait-job [runtime id]
  (peer/wait-until #(let [job (some (fn [job] (when (= id (:id job)) job)) (:jobs (rdm/status (:rdm runtime))))]
                     (when (#{:failed :completed :cancelled} (:status job)) job))))

(deftest actual-ring-device-lifecycle-readback-and-show-restore
  (let [physical (atom {}) transport (peer/loopback-peer (partial peer/fixture-peer physical))
        runtime (app/start! (assoc (app/default-config) :web-server {:port 0} :show {:workspace-options nil}))]
    (try
      (is (= 200 (:status (request runtime nil {}))))
      (is (empty? @(:requests transport)))
      (let [session (get-in (request runtime :connect {:config {:host "127.0.0.1" :port (:port transport)}} true) [:data :session-id])
            action (fn [operation payload] (request runtime operation (assoc payload :session-id session) true))]
        (is (string? session))
        (is (empty? @(:requests transport)))
        (let [id (get-in (action :discover {:universe 1 :full? true}) [:data :job-id])]
          (is (= :completed (:status (wait-job runtime id)))))
        (let [id (get-in (action :read peer/device) [:data :job-id])]
          (is (= :completed (:status (wait-job runtime id)))))
        (peer/wait-until #(get-in (rdm/saved (:model @(:state runtime))) [:observations (rdm/device-key peer/device)]))
        (is (= :accepted (get-in (action :associate {:fixture-id :head-1 :device peer/device}) [:data :status])))
        (is (= "Test Lighting" (get-in (rdm/saved (:model @(:state runtime)))
                                      [:observations (rdm/device-key peer/device) :fields :manufacturer-label :value])))
        (is (= 400 (:status (action :command (merge peer/device {:command :set :parameter :address :value 13})))))
        (is (not-any? #(= :set (:command %)) @(:requests transport)))
        (let [job (wait-job runtime (get-in (action :command (merge peer/device {:command :set :parameter :address :value 200})) [:data :job-id]))]
          (is (= :completed (:status job)))
          (is (= 200 (:address @physical)))
          (is (= :verified (get-in job [:result :verification :status])))
          (is (= 200 (get-in job [:result :readback :fields :device-info :value :address])))
          (is (= 1 (get-in @(:state runtime) [:model :rig :fixtures 0 :address]))))
        (peer/wait-until #(= 200 (get-in (rdm/saved (:model @(:state runtime)))
                                        [:observations (rdm/device-key peer/device) :fields :device-info :value :address])))
        (is (= 422 (:status (action :adopt {:fixture-id :head-1 :device (assoc peer/device :uid "1234:ffffffff")}))))
        (is (= 422 (:status (action :adopt {:fixture-id :head-1 :device peer/device :observed-at-ms 0}))))
        (is (= 1 (get-in @(:state runtime) [:model :rig :fixtures 0 :address])))
        (is (= :accepted (get-in (action :adopt {:fixture-id :head-1 :device peer/device
                                               :observed-at-ms (get-in (rdm/saved (:model @(:state runtime)))
                                                                      [:observations (rdm/device-key peer/device) :observed-at-ms])}) [:data :status])))
        (is (= 200 (get-in @(:state runtime) [:model :rig :fixtures 0 :address])))
        (let [saved (show/export-show runtime) calls (count @(:requests transport))
              before (rdm/saved (:model @(:state runtime)))
              reply (show/submit! runtime :import {:source (pr-str saved)})]
          (is (= :accepted (:status (deref reply 3000 {}))))
          (peer/wait-until #(not (:connected? (rdm/status (:rdm runtime)))))
          (is (= before (rdm/saved (:model @(:state runtime)))))
          (is (= calls (count @(:requests transport))))
          (is (= 400 (:status (request runtime :read (assoc peer/device :session-id session)))))))
      (finally (app/stop! runtime) ((:close! transport))))))

(deftest stale-request-identity-rejects-even-before-opening-a-socket
  (let [calls (atom 0) runtime (show/create-runtime {:workspace-options nil
                                                     :rdm-options {:open! (fn [& _] (swap! calls inc) (throw (Exception. "Unexpected IO")))}})]
    (try
      (is (= 400 (:status (request runtime :connect {:runtime-id "old"}))))
      (is (= 400 (:status (request runtime :connect {:expected-epoch -1}))))
      (is (zero? @calls))
      (is (false? (:connected? (rdm/status (:rdm runtime)))))
      (finally (app/stop! runtime)))))

(deftest a-blocked-rdm-request-does-not-block-show-commands-or-frame-publication
  (let [entered (promise) release (promise)
        runtime (app/start! (assoc (app/default-config) :web-server {:port 0} :show {:workspace-options nil
                                :rdm-options {:open! (fn [_ _ register!]
                                                       (register! #(deliver release true))
                                                       {:close! #(deliver release true)
                                                        :request! (fn [_ _] (deliver entered true) @release {:universe 1 :uids []})})}}))]
    (try
      (let [session (get-in (request runtime :connect {} true) [:data :session-id])]
        (request runtime :discover {:universe 1 :session-id session} true)
        (is (= true (deref entered 1000 false)))
        (let [before (:frame-number (show/snapshot runtime))
              reply (show/submit! runtime {:op :cue/launch :cue-id :ember})]
          (is (= :accepted (:status (deref reply 2000 {}))))
          (peer/wait-until #(> (:frame-number (show/snapshot runtime)) before))
          (is (seq (:instances (show/snapshot runtime))))
          (is (not (realized? release)))))
      (finally (deliver release true) (app/stop! runtime)))))

(deftest queued-observation-from-disconnected-session-is-rejected-at-commit
  (let [runtime (show/create-runtime {:workspace-options nil})
        identity {:runtime-id (:id runtime) :expected-epoch 0}]
    (try
      (let [session (rdm/connect! (:rdm runtime) {} identity)
            reply (show/submit! runtime :rdm-update
                                  (merge identity {:operation :discovered :universe 1 :uids [(:uid peer/device)]
                                                   :rdm-session-id (:session-id session)}))]
        (rdm/disconnect! (:rdm runtime))
        (show/tick! runtime 0)
        (is (= :rejected (:status (deref reply 1000 {}))))
        (is (nil? (:discovered (rdm/saved (:model @(:state runtime)))))))
      (finally (app/stop! runtime)))))
