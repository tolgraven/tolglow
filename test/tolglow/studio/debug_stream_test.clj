(ns tolglow.studio.debug-stream-test
  (:require [clojure.test :refer [deftest is]]
            [ring.adapter.jetty :as jetty]
            [afterglow.reactive :as reactive]
            [tolglow.studio :as studio]
            [tolglow.studio.effect-faults-test :as fault-fixtures]
            [tolglow.studio.show-events :as show-events]
            [tolglow.studio.debug-stream :as debug]
            [tolglow.studio.transit :as wire])
  (:import [java.net URI]
           [java.net.http HttpClient WebSocket WebSocket$Listener]
           [java.util Date]
           [java.util.concurrent LinkedBlockingQueue TimeUnit]))

(defn- connect [server query]
  (let [port (.getLocalPort (first (.getConnectors server)))
        inbox (LinkedBlockingQueue.)
        text (StringBuilder.)
        socket (.get (.buildAsync (.newWebSocketBuilder (HttpClient/newHttpClient))
                                  (URI. (str "ws://127.0.0.1:" port "/api/debug/events" query))
                                  (reify WebSocket$Listener
                                    (onOpen [_ s] (.request s 1))
                                    (onText [_ s part last?]
                                      (.append text ^CharSequence part)
                                      (when last? (.offer inbox (wire/decode (.toString text))) (.setLength text 0))
                                      (.request s 1) nil))) 5 TimeUnit/SECONDS)]
    {:socket socket :inbox inbox}))

(defn- send! [client value]
  (.get (.sendText ^WebSocket (:socket client) (wire/encode value) true) 3 TimeUnit/SECONDS))

(defn- receive [client]
  (or (.poll ^LinkedBlockingQueue (:inbox client) 3 TimeUnit/SECONDS)
      (throw (ex-info "Expected debug message" {}))))

(deftest real-commit-feed-and-selected-universe
  (let [runtime (studio/create-runtime {:workspace-options nil})
        server (jetty/run-jetty (studio/handler runtime) {:host "127.0.0.1" :port 0 :join? false})
        graph (studio/reactions runtime)]
    (try
      (is (false? (debug/observing? (:debug-stream runtime))))
      (reactive/reg-sub! graph ::count (fn [{:keys [db]} _] (count (:instances @db))))
      (reactive/reg-fx! graph ::read (fn [_ _ _] nil))
      (reactive/watch! graph ::w [::count] ::read)
      (let [client (connect server "") first-batch (receive client)]
        (try
          (is (= [:events 0 []] ((juxt :kind :cursor :events) first-batch)))
          (send! client {:op :ack :cursor 0})
          (let [submitted (studio/submit! runtime :command {:op :cue/launch :cue-id :ember})]
            (studio/tick! runtime 500)
            (is (= :accepted (:status @submitted))))
          (let [batch (receive client) rows (:events batch) launch (first (filter #(= :instance/launched (:type %)) rows))]
            (is (= (:id runtime) (:runtime-id batch)))
            (is (= :ember (get-in launch [:data :cue-id])))
            (is (instance? Date (:timestamp launch)))
            (is (= 500 (get-in launch [:time :time-ms])))
            (is (some #(and (= :reactive/dispatch (:type %)) (= [::w] (:subscribers %))) rows))
            (is (some #(= :clock/beat (:type %)) rows))
            ;; Withhold the event ACK; DMX has its own ACK and does not block.
            (let [universe (first (keys (get-in @(:state runtime) [:output :universes])))]
              (send! client {:op :dmx/subscribe :universe universe})
              (let [packet (receive client)]
                (is (= :dmx (:kind packet)))
                (is (bytes? (:bytes packet)))
                (is (= 512 (count (:bytes packet))))
                (is (= (seq (get-in @(:state runtime) [:output :universes universe])) (seq (:bytes packet))))
                (send! client {:op :dmx/unsubscribe})))
            (send! client {:op :ack :cursor (:cursor batch)})
            (let [resume (connect server (str "?after=" (:cursor batch) "&runtime=" (:id runtime)))]
              (try (is (empty? (:events (receive resume)))) (finally (.abort ^WebSocket (:socket resume))))))
          (finally (.abort ^WebSocket (:socket client)))))
      (finally (studio/stop! runtime) (.stop server)))))

(deftest stale-receiver-sees-bounded-history-with-an-exact-gap
  (let [runtime (studio/create-runtime {:workspace-options nil})
        server (jetty/run-jetty (studio/handler runtime) {:host "127.0.0.1" :port 0 :join? false})]
    (try
      (let [client (connect server "") _ (receive client)
            events (mapv (fn [i] {:type :example/pulse :emitter {:kind :effect :id :pulse} :time {}
                                  :category :effect :data {:index i :secret "must stay local"}}) (range 600))]
        (try
          (debug/commit! (:debug-stream runtime) {} events {} nil)
          (send! client {:op :ack :cursor 0})
          (let [reply (receive client)]
            (is (= [88 152 64] [(:skipped reply) (:cursor reply) (count (:events reply))]))
            (is (= 89 (:id (first (:events reply)))))
            (is (not (.contains (pr-str reply) "must stay local"))))
          (finally (.abort ^WebSocket (:socket client)))))
      (finally (studio/stop! runtime) (.stop server)))))

(deftest headless-dispatch-sees-real-inputs-and-semantic-events-without-a-debug-window
  (let [received (atom [])
        runtime (assoc (studio/create-runtime {:workspace-options nil}) :event-admit
                       (fn [model batch] (swap! received into batch)
                         {:model (assoc-in model [:variables :observed?] true) :deliveries {}}))
        audio (:events! (studio/audio-callbacks runtime))
        sync (:events! (studio/sync-callbacks runtime))]
    (try
      (audio [{:type :audio/kick :source {:id :capture} :time-ms 8}])
      (sync {:type :beat-link/beat :source-id :deck :time-ms 9})
      (studio/submit! runtime :command {:op :cue/launch :cue-id :ember})
      (studio/tick! runtime 100)
      (is (false? (debug/observing? (:debug-stream runtime))))
      (is (= true (get-in @(:state runtime) [:model :variables :observed?])))
      (is (some #(and (= :audio/kick (:type %)) (= :capture (get-in % [:emitter :id]))) @received))
      (is (some #(and (= :beat-link/beat (:type %)) (= :deck (get-in % [:emitter :id]))) @received))
      (is (some #(and (= :instance/launched (:type %)) (= :ember (get-in % [:data :cue-id]))) @received))
      (reset! received [])
      (studio/tick! runtime 110)
      (is (not-any? #(#{:audio/kick :beat-link/beat :instance/launched} (:type %)) @received))
      (finally (studio/stop! runtime)))))

(deftest quarantined-effect-emits-one-committed-fault
  (let [captured (atom [])
        runtime (assoc (studio/create-runtime {:model (fault-fixtures/show) :workspace-options nil
                                              :effect-log! (fn [_])})
                       :event-admit (fn [model events] (swap! captured into events) {:model model}))]
    (try
      (let [launch (studio/submit! runtime :command {:op :cue/launch :cue-id :unstable})]
        (studio/tick! runtime 25)
        (studio/tick! runtime 50)
        (let [faults (filter #(= :effect/failed (:type %)) @captured)]
          (is (= 1 (count faults)))
          (is (= (:instance-id @launch) (get-in (first faults) [:data :instance-id])))
          (is (= :unstable (get-in (first faults) [:emitter :id])))))
      (finally (studio/stop! runtime)))))

(deftest opaque-effect-intents-and-failed-handler-admission-preserve-committed-state
  (let [model {:show-id :main :epoch 1 :frame-number 2 :time-ms 18 :transport {:beat 0}}
        state {:model model :output {:universes {}} :status {:last-intents [:spark 1 {:op :echo}]}}
        result (show-events/commit (fn [_ _] (throw (ex-info "Bad handler" {}))) model state [])]
    (is (= [:spark :effect/intent :echo] (mapv :type (:events result))))
    (is (= model (get-in result [:state :model])))
    (is (= "Bad handler" (get-in result [:state :status :event-dispatch-error :message])))))

(deftest a-blocked-encoder-cannot-stall-the-frame-owner
  (let [runtime (studio/create-runtime {:workspace-options nil})
        server (jetty/run-jetty (studio/handler runtime) {:host "127.0.0.1" :port 0 :join? false})
        entered (promise) resume (promise) encode wire/encode
        row {:type :test/frame :category :effect :emitter {:kind :effect :id :test} :data {}}]
    (try
      (let [client (connect server "") _ (receive client)]
        (try
          (send! client {:op :ack :cursor 0})
          (with-redefs [wire/encode (fn [value]
                                      (when (and (= :events (:kind value)) (seq (:events value)))
                                        (deliver entered true) (deref resume 5000 nil))
                                      (encode value))]
            (debug/commit! (:debug-stream runtime) {} [row] {} nil)
            (is (true? (deref entered 3000 false)))
            (let [producer (future
                             (dotimes [_ 20]
                               (debug/commit! (:debug-stream runtime) {} (vec (repeat 50 row)) {} nil))
                             :complete)]
              (is (= :complete (deref producer 2000 :timeout))))
            (deliver resume true)
            (let [first-batch (receive client)]
              (send! client {:op :ack :cursor (:cursor first-batch)}))
            (let [latest (receive client)]
              (is (= 488 (:skipped latest)))
              (is (= 490 (:id (first (:events latest)))))))
          (finally (deliver resume true) (.abort ^WebSocket (:socket client)))))
      (finally (studio/stop! runtime) (.stop server)))))
