(ns tolglow.sensors-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [afterglow.control.model :as control]
            [tolglow.sensors :as service]))
(defn vault []
  (let [secrets (atom {})]
    {:put! (fn [secret] (let [id (str "credential-" (java.util.UUID/randomUUID))] (swap! secrets assoc id secret) id))
     :get! #(get @secrets %) :delete! #(swap! secrets dissoc %) :saved secrets}))
(def config {:version 1 :timeout-ms 500 :channels [{:id :pitch :input :pitch}]
             :bindings [{:id :level :channel :pitch :command {:op :param/set :instance-id "0:1" :param :level}
                         :schema {:type :number :default 0 :bounds [0 1]}}]})
(defn show [] (control/new-show))
(defn fixture [& [options]]
  (let [clock (atom 0) store (vault) manager (service/create-manager (merge {:credential-store store :now-ms #(deref clock)} options))
        pair (service/pair! manager "phone" "Conducting phone")
        model (:model (service/configure (show) "phone" config (:pairing pair)))]
    {:clock clock :store store :manager manager :pair pair :model model}))
(defn packet [pair sequence time pitch & [session]]
  {:version 1 :device-id "phone" :session-id (or session "session1") :sample-id sequence :time-ms time
   :pairing-token (:pairing-token pair) :values {:pitch pitch} :gestures []})
(defn advance [manager model time beat]
  (service/advance! manager model {:time-ms time :beat beat :meter {:beats-per-bar 4 :bars-per-phrase 4}}))

(defn commands [result] (mapv #(dissoc % :source) (:commands result)))

(deftest pairing-exports-only-reference-and-arming-needs-fresh-observed-session
  (let [{:keys [manager pair model clock]} (fixture)]
    (try
      (is (= 1 (count (get-in model [:sensors :sources]))))
      (is (not (.contains (pr-str (service/export-state model)) (:pairing-token pair))))
      (is (not (.contains (pr-str (service/status manager)) (:pairing-token pair))))
      (is (thrown? clojure.lang.ExceptionInfo (service/arm! manager model "phone" "session1" true)))
      (is (:accepted? (service/ingest! manager (packet pair 0 0 0.4))))
      (let [armed (service/arm! manager model "phone" "session1" true)]
        (is (get-in armed [:model :sensors :sources "phone" :state :armed?]))
        (is (zero? (:queued (service/status manager))) "Arming drops observations gathered before explicit arm")
        (is (= [] (:commands (advance manager (:model armed) 100 1))))
        (service/ingest! manager (packet pair 1 100 0.6))
        (let [result (advance manager (:model armed) 1000 2)]
          (is (= [{:op :param/set :instance-id "0:1" :param :level :value 0.6}] (commands result)))
          (is (= 1000 (get-in result [:model :sensors :sources "phone" :state :last-time-ms])))
          (is (= 100 (get-in result [:model :sensors :sources "phone" :state :last-device-time-ms])))))
      (reset! clock 1000)
      (is (thrown? clojure.lang.ExceptionInfo (service/arm! manager model "phone" "session1" true)))
      (finally (service/close! manager)))))

(deftest unauthorized-stale-and-retired-sessions-cannot-enqueue-control
  (let [{:keys [manager pair model]} (fixture)]
    (try
      (is (= :unauthorized (:reason (service/ingest! manager (assoc (packet pair 0 0 0) :pairing-token "wrong-long-token")))))
      (is (:accepted? (service/ingest! manager (packet pair 1 10 0))))
      (is (= :stale-sample (:reason (service/ingest! manager (packet pair 1 10 1)))))
      (is (= :stale-device-time (:reason (service/ingest! manager (packet pair 2 9 1)))))
      (let [armed (:model (service/arm! manager model "phone" "session1" true))]
        (is (= :owned-session (:reason (service/ingest! manager (packet pair 0 20 1 "session2")))))
        (service/arm! manager armed "phone" nil false)
        (is (:accepted? (service/ingest! manager (packet pair 0 20 1 "session2")))))
      (finally (service/close! manager)))))

(deftest disarmed-calibration-and-full-state-roundtrip-never-reopen-or-rearm
  (let [{:keys [manager pair model store]} (fixture)]
    (try
      (service/ingest! manager (packet pair 0 0 0.3))
      (let [observed (advance manager model 100 1)
            calibrated (:model (service/calibrate manager (:model observed) "phone"))
            armed (:model (service/arm! manager calibrated "phone" "session1" true))
            saved (edn/read-string (pr-str (:sensors armed)))
            replacement (service/create-manager {:credential-store store :model (assoc armed :sensors saved) :now-ms (constantly 0)})]
        (try
          (is (= {:pitch 0.3} (get-in saved [:sources "phone" :state :baseline])))
          (is (get-in saved [:sources "phone" :armed-intent?]))
          (is (nil? (:udp (service/status replacement))))
          (is (false? (get-in (service/status replacement) [:devices "phone" :armed?])))
          (let [restored (advance replacement armed 100 1)]
            (is (false? (get-in restored [:model :sensors :sources "phone" :state :armed?])))
            (is (get-in restored [:model :sensors :sources "phone" :armed-intent?]))
            (is (empty? (:commands restored))))
          (finally (service/close! replacement))))
      (finally (service/close! manager)))))

(deftest bounded-queue-overflow-disarms-and-expiry-releases-only-known-instance
  (let [{:keys [manager pair model clock]} (fixture {:queue-limit 1})
        hold-config (-> config (dissoc :bindings)
                        (assoc :triggers [{:id :hold :channel :pitch :on 1 :off 0
                                           :command {:op :cue/launch :cue-id :wash :launch {:mode :held}}}]))
        model (:model (service/configure model "phone" hold-config))]
    (try
      (service/ingest! manager (packet pair 0 0 0))
      (let [armed (:model (service/arm! manager model "phone" "session1" true))]
        (service/ingest! manager (packet pair 1 10 2))
        (let [pressed (advance manager armed 10 0)
              gesture (-> pressed :commands first :gesture-id)
              acked (:model (service/acknowledge (:model pressed) "phone" gesture {:status :accepted :instance-id "0:9"}))]
          (reset! clock 1000)
          (let [expired (advance manager acked 1000 1)]
            (is (= [{:op :instance/end :instance-id "0:9"}] (commands expired)))
            (is (false? (get-in (service/status manager) [:devices "phone" :armed?])))))
        (service/ingest! manager (packet pair 2 20 2))
        (is (:overflow? (service/ingest! manager (packet pair 3 30 2))))
        (is (= 1 (:queued (service/status manager))))
        (is (false? (get-in (service/status manager) [:devices "phone" :armed?]))))
      (finally (service/close! manager)))))

(deftest expired-queued-observations-do-not-become-fresh-at-frame-admission
  (let [{:keys [manager pair model clock]} (fixture)]
    (try
      (service/ingest! manager (packet pair 0 0 0))
      (let [armed (:model (service/arm! manager model "phone" "session1" true))]
        (service/ingest! manager (packet pair 1 10 0.8))
        (reset! clock 1000)
        (is (= [] (:commands (advance manager armed 1000 10)))))
      (finally (service/close! manager)))))

(deftest restore-failure-preserves-current-admission-and-close-invalidates-receivers
  (let [{:keys [manager pair model]} (fixture)]
    (try
      (is (thrown? clojure.lang.ExceptionInfo (service/restore! manager (assoc-in model [:sensors :sources "phone" :pairing :credential-ref] "bad"))))
      (is (:accepted? (service/ingest! manager (packet pair 0 0 0))))
      (let [receiver (service/start-osc! manager {:host "127.0.0.1" :port 0})]
        (is (pos? (:port receiver)))
        (service/close! manager)
        (is (false? (:running? ((:status receiver)))))
        (is (thrown? clojure.lang.ExceptionInfo (service/ingest! manager (packet pair 1 1 1))))
        (is (:closed? (service/close! manager))))
      (finally (service/close! manager)))))

(deftest invalid-conditioned-input-is-isolated-and-disarms-with-visible-event
  (let [{:keys [manager pair model]} (fixture)]
    (try
      (service/ingest! manager (packet pair 0 0 0))
      (let [armed (:model (service/arm! manager model "phone" "session1" true))]
        (service/ingest! manager (packet pair 1 10 9))
        (let [result (advance manager armed 10 0)]
          (is (empty? (commands result)))
          (is (= :sensor/input-error (-> result :events last :type)))
          (is (false? (get-in result [:model :sensors :sources "phone" :state :armed?])))
          (is (false? (get-in (service/status manager) [:devices "phone" :armed?])))))
      (finally (service/close! manager)))))

(deftest late-held-receipt-after-new-observed-session-still-releases-exact-instance
  (let [{:keys [manager pair model]} (fixture)
        hold-config (-> config (dissoc :bindings)
                        (assoc :triggers [{:id :hold :channel :pitch :on 1 :off 0
                                           :command {:op :cue/launch :cue-id :wash :launch {:mode :held}}}]))
        model (:model (service/configure model "phone" hold-config))]
    (try
      (service/ingest! manager (packet pair 0 0 0))
      (let [armed (:model (service/arm! manager model "phone" "session1" true))]
        (service/ingest! manager (packet pair 1 10 2))
        (let [pressed (advance manager armed 10 0)
              gesture (-> pressed :commands first :gesture-id)
              disarmed (:model (service/arm! manager (:model pressed) "phone" "session1" false))]
          (service/ingest! manager (packet pair 0 0 0 "session2"))
          (let [observed (advance manager disarmed 20 0)
                acknowledged (service/acknowledge (:model observed) "phone" gesture {:status :accepted :instance-id "0:late"})]
            (is (= "session2" (get-in observed [:model :sensors :sources "phone" :state :session-id])))
            (is (= [{:op :instance/end :instance-id "0:late"}] (commands acknowledged)))
            (is (empty? (:commands (service/acknowledge (:model acknowledged) "phone" gesture {:status :accepted :instance-id "0:late"})))))))
      (finally (service/close! manager)))))

(deftest service-commands-carry-owned-source-and-restore-retains-known-held-releases
  (let [{:keys [manager pair model]} (fixture)]
    (try
      (service/ingest! manager (packet pair 0 0 0))
      (let [armed (:model (service/arm! manager model "phone" "session1" true))]
        (service/ingest! manager (packet pair 1 10 0.7))
        (let [result (advance manager armed 10 0)]
          (is (= {:adapter :sensor :device-id "phone" :session-id "session1"} (-> result :commands first :source)))))
      (let [owned {:adapter :sensor :device-id "phone" :session-id "old-session"}
            saved (-> model (assoc-in [:instances "4:9"] {:id "4:9" :source owned})
                      (assoc-in [:sensors :sources "phone" :state :armed?] true)
                      (assoc-in [:sensors :sources "phone" :state :holds :gestures]
                                {"known" {:pressed? true :instance-id "4:9"}
                                 "pending" {:pressed? true}}))
            prepared (service/prepare-restored-model saved)
            result (advance manager prepared 10 0)]
        (is (= #{"known"} (set (keys (get-in prepared [:sensors :sources "phone" :state :holds :gestures])))))
        (is (= [{:op :instance/end :instance-id "4:9" :source owned}] (:commands result)))
        (is (empty? (get-in result [:model :sensors :sources "phone" :state :holds :gestures]))))
      (finally (service/close! manager)))))

(deftest pairing-close-race-cleans-the-unpublished-credential-outside-manager-lock
  (let [entered (promise) proceed (promise) deleted (atom [])
        reference "credential-00000000-0000-0000-0000-000000000099"
        store {:put! (fn [_] (deliver entered true) @proceed reference)
               :get! (constantly nil) :delete! #(swap! deleted conj %)}
        manager (service/create-manager {:credential-store store})
        pending (future (try (service/pair! manager "phone")
                             (catch clojure.lang.ExceptionInfo error (:sensors/error (ex-data error)))))]
    (is (= true (deref entered 1000 ::timeout)))
    (service/close! manager)
    (deliver proceed true)
    (is (= :closed (deref pending 1000 ::timeout)))
    (is (= [reference] @deleted))))

(deftest a-continuous-sensor-backlog-releases-drained-packets
  (let [{:keys [manager pair model]} (fixture {:drain-limit 1 :queue-limit 3})
        backing (fn [queue]
                  (if (instance? clojure.lang.APersistentVector$SubVector queue)
                    (.-v ^clojure.lang.APersistentVector$SubVector queue)
                    queue))]
    (try
      (service/ingest! manager (packet pair 0 0 0.0))
      (loop [model model i 1]
        (when (<= i 200)
          (service/ingest! manager (packet pair i i 0.5))
          (let [result (advance manager model i i)]
            (recur (:model result) (inc i)))))
      (let [queue (:queue @(:state manager))]
        (is (= [200] (mapv #(get-in % [:observation :sample-id]) queue)))
        (is (<= (count (backing queue)) (:queue-limit manager))))
      (finally (service/close! manager)))))
