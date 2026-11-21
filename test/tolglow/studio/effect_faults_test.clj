(ns tolglow.studio.effect-faults-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data.json :as json]
            [afterglow.reactive :as reactive]
            [tolglow.studio :as studio]
            [tolglow.studio.demo :as demo]
            [tolglow.studio.effect-faults :as faults])
  (:import [java.io ByteArrayInputStream]
           [java.util.concurrent ThreadPoolExecutor]))

(def unstable
  {:id :unstable :version 1 :name "Live edit" :targets [:head-1]
   :params {:divisor {:type :number :default 0.0 :bounds [0.0 1.0]}}
   :outputs {:dimmer {:op :divide :args [{:op :const :value 0.25} {:op :param :key :divisor}]}}})

(defn show []
  (-> (demo/model)
      (assoc-in [:registry :effects :unstable] unstable)
      (assoc-in [:registry :cues :unstable]
                {:id :unstable :version 1 :effect-id :unstable :priority 20 :reload-policy :preserve-state})))

(defn- apply! [runtime payload t]
  (let [reply (studio/submit! runtime payload)]
    (studio/tick! runtime t)
    (deref reply 2000 ::timeout)))

(defn- post! [runtime payload t]
  (let [result (future ((studio/handler runtime)
                       {:request-method :post :uri "/api/command" :headers {"host" "localhost"}
                        :body (ByteArrayInputStream. (.getBytes (json/write-str payload) "UTF-8"))}))
        deadline (+ (System/nanoTime) 3000000000)]
    (try
      (loop []
        (cond (realized? result) (json/read-str (:body @result) :key-fn keyword)
              (> (System/nanoTime) deadline) (throw (ex-info "Ring operation timed out" {}))
              :else (do (studio/tick! runtime t) (Thread/sleep 5) (recur))))
      (finally (future-cancel result)))))

(defn fault-sequence [context _] (get-in @(:db context) [:variables :runtime :event-sequence] 0))
(defn fault-action [_ value _] (when (pos? value) {:op :master/set :value 0.8}))

(defn- register-watcher! [runtime]
  (let [graph (studio/reactions runtime)]
    (reactive/reg-sub! graph ::fault-sequence fault-sequence
                       {:id ::fault-sequence :kind :sub
                        :implementation {:type :var :symbol "tolglow.studio.effect-faults-test/fault-sequence"}})
    (reactive/reg-fx! graph ::fault-action fault-action
                      {:id ::fault-action :kind :fx
                       :implementation {:type :var :symbol "tolglow.studio.effect-faults-test/fault-action"}})
    (reactive/watch! graph ::fault-watcher [::fault-sequence] ::fault-action)))

(deftest actual-compiler-workers-ring-and-output-continue-around-a-broken-effect
  (let [logged (promise) output (atom []) owner-thread (.getName (Thread/currentThread))
        runtime (studio/create-runtime {:model (show) :workspace-options nil
                                        :log! #(deliver logged {:event % :thread (.getName (Thread/currentThread))})
                                        :sink {:send! (fn [universes frame] (swap! output conj [frame (vec (get universes 1))]))}})]
    (try
      (register-watcher! runtime)
      (apply! runtime {:op :cue/launch :cue-id :breathe} 0)
      (let [bad (:instance-id (apply! runtime {:op :cue/launch :cue-id :unstable} 100))
            failed (studio/snapshot runtime)
            previous (second (last @output))]
        (is (= :evaluate (get-in @(:state runtime) [:model :instances bad :fault :stage])))
        (is (= :unstable (get-in failed [:effect-events :events 0 :effect-id])))
        (is (= :effect/failed (get-in (deref logged 3000 ::timeout) [:event :type])))
        (is (not= owner-thread (:thread @logged)))
        (studio/tick! runtime 500)
        (is (not= previous (second (last @output))) "Healthy musical effect keeps changing DMX capture")
        (is (= 0.8 (:master (studio/snapshot runtime))) "Fault event drives an ordinary durable reactive handler")
        (is (nil? (get-in (studio/snapshot runtime) [:status :error])))
        (is (= 1 (get-in (studio/snapshot runtime) [:effect-events :event-sequence])))
        (is (= "accepted" (:status (post! runtime {:op "param/set" :instance-id bad :param "divisor" :value 0.5} 500))))
        (is (= "accepted" (:status (post! runtime {:op "instance/retry" :instance-id bad} 500))))
        (is (nil? (get-in @(:state runtime) [:model :instances bad :fault])))
        (is (= :effect/recovered (-> (studio/snapshot runtime) :effect-events :events last :type)))
        (is (= 0.4 (get-in (studio/snapshot runtime) [:output :values :head-1 :dimmer]))))
      (finally (studio/stop! runtime)))))

(deftest full-show-restore-preserves-quarantine-and-remaps-active-diagnostics
  (let [runtime (studio/create-runtime {:model (show) :workspace-options nil :log! (fn [_])})]
    (try
      (let [bad (:instance-id (apply! runtime {:op :cue/launch :cue-id :unstable} 0))
            saved (studio/export-show runtime)
            reply (studio/submit! runtime :import {:source (pr-str saved)})]
        (studio/tick! runtime 20)
        (is (= :accepted (:status @reply)))
        (let [new (first (:instances (studio/snapshot runtime)))]
          (is (not= bad (:id new)))
          (is (= (:id new) (get-in new [:fault :instance-id])))
          (is (= 1 (get-in new [:fault :epoch])))
          (studio/tick! runtime 30)
          (is (= 1 (get-in (studio/snapshot runtime) [:effect-events :event-sequence])))
          (is (nil? (get-in (studio/snapshot runtime) [:status :error])))
          (apply! runtime {:op :show/restart} 40)
          (is (empty? (:instances (studio/snapshot runtime))))))
      (finally (studio/stop! runtime)))))

(deftest asynchronous-logging-is-bounded-and-cannot-block-the-frame-owner
  (let [entered (promise) release (promise)
        logger (faults/create {:queue-size 1 :log! (fn [_] (deliver entered true) @release)})]
    (try
      (faults/offer! logger [{:type :effect/failed}])
      (is (= true (deref entered 2000 ::timeout)))
      (faults/offer! logger (repeat 10 {:type :effect/failed}))
      (is (= {:queued 1 :dropped 9 :failed 0} (faults/status logger)))
      (finally
        (deliver release true)
        (faults/close! logger)
        (is (.isShutdown ^ThreadPoolExecutor (:pool logger)))))))
