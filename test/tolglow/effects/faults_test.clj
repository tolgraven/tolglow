(ns tolglow.effects.faults-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data.json :as json]
            [afterglow.reactive :as reactive]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.demo :as demo]
            [tolglow.effects.faults :as faults])
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
  (let [reply (show/submit! runtime payload)]
    (show/tick! runtime t)
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
              :else (do (show/tick! runtime t) (Thread/sleep 5) (recur))))
      (finally (future-cancel result)))))

(defn fault-sequence [context _] (get-in @(:db context) [:variables :runtime :event-sequence] 0))
(defn fault-action [_ value _] (when (pos? value) {:op :master/set :value 0.8}))

(defn- register-watcher! [runtime]
  (let [graph (show/reactions runtime)]
    (reactive/reg-sub! graph ::fault-sequence fault-sequence
                       {:id ::fault-sequence :kind :sub
                        :implementation {:type :var :symbol "tolglow.effects.faults-test/fault-sequence"}})
    (reactive/reg-fx! graph ::fault-action fault-action
                      {:id ::fault-action :kind :fx
                       :implementation {:type :var :symbol "tolglow.effects.faults-test/fault-action"}})
    (reactive/watch! graph ::fault-watcher [::fault-sequence] ::fault-action)))

(deftest actual-compiler-workers-ring-and-output-continue-around-a-broken-effect
  (let [logged (promise) output (atom []) owner-thread (.getName (Thread/currentThread))
        runtime (show/create-runtime {:model (show) :workspace-options nil
                                        :effect-log! #(deliver logged {:event % :thread (.getName (Thread/currentThread))})
                                        :sink {:send! (fn [universes frame] (swap! output conj [frame (vec (get universes 1))]))}})]
    (try
      (register-watcher! runtime)
      (apply! runtime {:op :cue/launch :cue-id :breathe} 0)
      (let [bad (:instance-id (apply! runtime {:op :cue/launch :cue-id :unstable} 100))
            failed (show/snapshot runtime)
            previous (second (last @output))]
        (is (= :evaluate (get-in @(:state runtime) [:model :instances bad :fault :stage])))
        (is (= :unstable (get-in failed [:effect-events :events 0 :effect-id])))
        (is (= :unstable (:effect-id (first @(reactive/subscribe (show/reactions runtime) [:runtime/effect-events])))))
        (is (= :effect/failed (get-in (deref logged 3000 ::timeout) [:event :type])))
        (is (not= owner-thread (:thread @logged)))
        (show/tick! runtime 500)
        (is (not= previous (second (last @output))) "Healthy musical effect keeps changing DMX capture")
        (is (= 0.8 (:master (show/snapshot runtime))) "Fault event drives an ordinary durable reactive handler")
        (is (nil? (get-in (show/snapshot runtime) [:status :error])))
        (is (= 1 (get-in (show/snapshot runtime) [:effect-events :event-sequence])))
        (is (= "accepted" (:status (post! runtime {:op "param/set" :instance-id bad :param "divisor" :value 0.5} 500))))
        (is (= "accepted" (:status (post! runtime {:op "instance/retry" :instance-id bad} 500))))
        (is (nil? (get-in @(:state runtime) [:model :instances bad :fault])))
        (is (= :effect/recovered (-> (show/snapshot runtime) :effect-events :events last :type)))
        (is (= 0.4 (get-in (show/snapshot runtime) [:output :values :head-1 :intensity]))))
      (finally (show/stop! runtime)))))

(deftest full-show-restore-preserves-quarantine-and-remaps-active-diagnostics
  (let [runtime (show/create-runtime {:model (show) :workspace-options nil :effect-log! (fn [_])})]
    (try
      (let [bad (:instance-id (apply! runtime {:op :cue/launch :cue-id :unstable} 0))
            saved (show/export-show runtime)
            reply (show/submit! runtime :import {:source (pr-str saved)})]
        (show/tick! runtime 20)
        (is (= :accepted (:status @reply)))
        (let [new (first (:instances (show/snapshot runtime)))]
          (is (not= bad (:id new)))
          (is (= (:id new) (get-in new [:fault :instance-id])))
          (is (= 1 (get-in new [:fault :epoch])))
          (is (contains? @(reactive/subscribe (show/reactions runtime) [:runtime/effect-faults]) (:id new)))
          (show/tick! runtime 30)
          (is (= 1 (get-in (show/snapshot runtime) [:effect-events :event-sequence])))
          (is (nil? (get-in (show/snapshot runtime) [:status :error])))
          (apply! runtime {:op :show/restart} 40)
          (is (empty? (:instances (show/snapshot runtime))))))
      (finally (show/stop! runtime)))))

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

(defn- with-effect [model effect]
  (-> model
      (assoc-in [:registry :effects (:id effect)] effect)
      (assoc-in [:registry :cues (:id effect)] {:id (:id effect) :version 1 :effect-id (:id effect)
                                               :priority 20 :reload-policy :preserve-state})))

(defn- submit-operation! [runtime type payload t]
  (let [reply (show/submit! runtime type payload)]
    (show/tick! runtime t)
    (deref reply 2000 ::timeout)))

(deftest renderer-rejects-one-bad-aim-and-hot-reload-recovers-the-last-good-revision
  (let [position (:position (first (demo/fixtures)))
        effect {:id :aim-code :version 1 :params {} :targets [:head-1]
                :outputs {:aim {:op :const :value [0.0 0.0 0.0]}}}
        runtime (show/create-runtime {:model (with-effect (demo/model) effect)
                                        :workspace-options nil :effect-log! (fn [_])})]
    (try
      (apply! runtime {:op :cue/launch :cue-id :breathe} 0)
      (let [id (:instance-id (apply! runtime {:op :cue/launch :cue-id :aim-code} 0))
            bad (assoc-in (assoc effect :version 2) [:outputs :aim :value] position)]
        (is (= :accepted (:status (submit-operation! runtime :definition {:kind :effects :definition bad} 100))))
        (is (= 1 (get-in @(:state runtime) [:model :instances id :effect-version])))
        (is (= :output (get-in @(:state runtime) [:model :instances id :reload-fault :stage])))
        (is (nil? (get-in (show/snapshot runtime) [:status :error])))
        (is (pos? (get-in (show/snapshot runtime) [:output :values :head-1 :intensity])))
        (show/tick! runtime 200)
        (is (= [0.0 0.0 0.0] (get-in (show/snapshot runtime) [:output :values :head-1 :aim])))
        (is (= 1 (get-in (show/snapshot runtime) [:effect-events :event-sequence])))
        (is (= :accepted (:status (submit-operation! runtime :definition
                                                     {:kind :effects :definition (assoc effect :version 3)} 300))))
        (is (= 3 (get-in @(:state runtime) [:model :instances id :effect-version])))
        (is (nil? (get-in @(:state runtime) [:model :instances id :reload-fault])))
        (is (= :effect/recovered (-> (show/snapshot runtime) :effect-events :events last :type))))
      (finally (show/stop! runtime)))))

(deftest actual-typed-output-failures-do-not-hide-the-healthy-lighting
  (doseq [[model effect]
          [[(update-in (demo/model) [:rig :fixtures 0] assoc :channels {:dimmer 1} :rgbw-policy :none)
            {:id :unsupported-color :version 1 :params {} :targets [:head-1]
             :outputs {:color {:op :const :value [1.0 0.2 0.1]}}}]
           [(demo/model)
            {:id :overflow :version 1 :params {} :targets [:head-1]
             :outputs [{:attribute :dimmer :blend :add :value {:op :const :value 0.8}}]}]]]
    (let [runtime (show/create-runtime {:model (with-effect model effect)
                                          :workspace-options nil :effect-log! (fn [_])})]
      (try
        (apply! runtime {:op :cue/launch :cue-id :breathe} 0.0)
        (let [id (:instance-id (apply! runtime {:op :cue/launch :cue-id (:id effect)} 500.0))]
          (is (= :output (get-in @(:state runtime) [:model :instances id :fault :stage])))
          (is (nil? (get-in (show/snapshot runtime) [:status :error])))
          (is (pos? (get-in (show/snapshot runtime) [:output :values :head-1 :intensity])))
          (show/tick! runtime 700.0)
          (is (= 1 (get-in (show/snapshot runtime) [:effect-events :event-sequence])))
          (is (= 2 (count (:instances (show/snapshot runtime))))))
        (finally (show/stop! runtime))))))

(deftest diagnostics-reach-the-local-rotating-file
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory "tolglow-log-test" (make-array java.nio.file.attribute.FileAttribute 0)))
        owner (faults/create {:log-file (java.io.File. dir "faults-%g.log")})]
    (try
      (faults/offer! owner [{:type :reactive/fault :id :bad-reaction :message "invalid subscription result"}])
      (faults/close! owner)
      (is (re-find #"invalid subscription result" (slurp (java.io.File. dir "faults-0.log"))))
      (finally
        (doseq [file (.listFiles dir)] (.delete file))
        (.delete dir)))))
