(ns tolglow.studio.debug-stream-test
  (:require [clojure.test :refer [deftest is]]
            [ring.adapter.jetty :as jetty]
            [afterglow.reactive :as reactive]
            [tolglow.demo :as demo]
            [tolglow.event-subscribers :as subscribers]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.effects.faults-test :as fault-fixtures]
            [tolglow.events :as show-events]
            [tolglow.studio.debug-stream :as debug]
            [tolglow.transit :as wire])
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
  (let [runtime (show/create-runtime {:workspace-options nil})
        server (jetty/run-jetty (studio/handler runtime) {:host "127.0.0.1" :port 0 :join? false})
        graph (show/reactions runtime)]
    (try
      (is (false? (debug/observing? (get-in @(:observers runtime) [:studio :debug-stream]))))
      (reactive/reg-sub! graph ::count (fn [{:keys [db]} _] (count (:instances @db))))
      (reactive/reg-fx! graph ::read (fn [_ _ _] nil))
      (reactive/watch! graph ::w [::count] ::read)
      (let [client (connect server "") first-batch (receive client)]
        (try
          (is (= [:events 0 []] ((juxt :kind :cursor :events) first-batch)))
          (send! client {:op :ack :cursor 0})
          (let [submitted (show/submit! runtime :command {:op :cue/launch :cue-id :ember})]
            (show/tick! runtime 500)
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
      (finally (show/stop! runtime) (.stop server)))))

(deftest identical-custom-intents-keep-their-origin-in-scheduled-actions-and-debug
  (let [intents [:test/pulse {:type :test/accent}]
        model (-> (demo/model)
                  (update-in [:registry :effects] #(assoc % :event-subscriber (assoc (:ember %) :id :event-subscriber)))
                  (assoc-in [:registry :cues :ember :effect-id] :event-subscriber))
        runtime (update (show/create-runtime {:model model :workspace-options nil}) :compile
                        (fn [compile-effect]
                          (fn [{:keys [id] :as definition}]
                            (if (#{:event-subscriber :ice} id)
                              (fn [context]
                                (assoc context :intents intents :done? (= :event-subscriber id)))
                              (compile-effect definition)))))
        server (jetty/run-jetty (studio/handler runtime) {:host "127.0.0.1" :port 0 :join? false})
        types [:test/pulse :test/accent]
        subscriber-id (fn [effect type] (keyword (name effect) (name type)))
        row (juxt :type :emitter #(select-keys (:data %) [:effect-id :instance-id :effect-version :cue-id]))
        debug-rows (fn [batch] (filterv #(some #{(:type %)} types) (:events batch)))
        runs #(get-in @(:state runtime) [:model :event-subscribers :runs])]
    (try
      (let [client (connect server "") _ (receive client)]
        (try
          (send! client {:op :ack :cursor 0})
          (let [registrations (vec (for [effect [:event-subscriber :ice] type types
                                         :let [follow-up? (= [:event-subscriber :test/pulse] [effect type])]]
                                     (show/register-event-subscriber! runtime
                                       {:id (subscriber-id effect type)
                                        :match {:category :effect :emitter-id effect :type type}
                                        :actions [(cond-> {:op :cue/launch :select {:ids [:ember] :pick :first}
                                                           :launch {:mode :one-shot}}
                                                    (not follow-up?) (assoc :delay {:beats 8}))]})))
                ember (show/submit! runtime {:op :cue/launch :cue-id :ember :launch {:mode :one-shot}
                                             :source {:adapter :repl :session-id "intent-test"}})
                ice (show/submit! runtime {:op :cue/launch :cue-id :ice})]
            (is (empty? (get-in @(:state runtime) [:model :instances])))
            (show/tick! runtime 50)
            (let [ember-id (:instance-id @ember) ice-id (:instance-id @ice)
                  expected (vec (for [[effect cue id] [[:event-subscriber :ember ember-id] [:ice :ice ice-id]] type types]
                                  [type {:kind :effect :id effect}
                                   {:effect-id effect :instance-id id :effect-version 1 :cue-id cue}]))
                  subscribers [:event-subscriber/pulse :event-subscriber/accent :ice/pulse :ice/accent]
                  batch (receive client) rows (debug-rows batch)]
              (is (every? #(= :accepted (:status @%)) (into [ember ice] registrations)))
              (is (= #{ice-id} (set (keys (get-in @(:state runtime) [:model :instances])))))
              (is (= (into (conj intents {:type :instance/finished :instance-id ember-id :reason :effect-complete}) intents)
                     (get-in @(:state runtime) [:status :last-intents])))
              (is (not (contains? @(:state runtime) ::show-events/intent-emissions)))
              (is (= expected (mapv (comp row :event) (runs))))
              (is (= [:repl :repl :studio :studio] (mapv #(get-in % [:event :data :cause-adapter]) (runs))))
              (is (= subscribers (mapv :subscriber (runs))))
              (is (= expected (mapv row rows)))
              (is (= (mapv vector subscribers) (mapv :subscribers rows)))
              (is (every? #(= :matched (:delivery %)) rows))
              (send! client {:op :ack :cursor (:cursor batch)})
              (let [pause (show/submit! runtime {:op :instance/pause :instance-id ice-id})]
                (show/tick! runtime 75)
                (is (= :accepted (:status @pause)))
                (let [batch (receive client) rows (debug-rows batch)
                      launch (first (filter #(= :instance/launched (:type %)) (:events batch)))
                      follow-up-id (get-in launch [:data :instance-id])]
                  (is (and follow-up-id (not= ember-id follow-up-id)))
                  (is (= (mapv #(assoc-in % [2 :instance-id] follow-up-id) (take 2 expected)) (mapv row rows)))
                  (is (every? #(and (= :unknown (:delivery %)) (nil? (:subscribers %))) rows))
                  (is (= (subvec subscribers 1) (mapv :subscriber (runs))))
                  (send! client {:op :ack :cursor (:cursor batch)})))
              (show/tick! runtime 100)
              (is (empty? (get-in @(:state runtime) [:status :last-intents])))
              (is (= (subvec subscribers 1) (mapv :subscriber (runs))))))
          (finally (.abort ^WebSocket (:socket client)))))
      (finally (show/stop! runtime) (.stop server)))))

(deftest stale-receiver-sees-bounded-history-with-an-exact-gap
  (let [runtime (show/create-runtime {:workspace-options nil})
        server (jetty/run-jetty (studio/handler runtime) {:host "127.0.0.1" :port 0 :join? false})]
    (try
      (let [client (connect server "") _ (receive client)
            events (mapv (fn [i] {:type :example/pulse :emitter {:kind :effect :id :pulse} :time {}
                                  :category :effect :data {:index i :secret "must stay local"}}) (range 600))]
        (try
          (debug/commit! (get-in @(:observers runtime) [:studio :debug-stream]) {} events {} nil)
          (send! client {:op :ack :cursor 0})
          (let [reply (receive client)]
            (is (= [88 152 64] [(:skipped reply) (:cursor reply) (count (:events reply))]))
            (is (= 89 (:id (first (:events reply)))))
            (is (not (.contains (pr-str reply) "must stay local"))))
          (finally (.abort ^WebSocket (:socket client)))))
      (finally (show/stop! runtime) (.stop server)))))

(deftest headless-dispatch-sees-real-inputs-and-semantic-events-without-a-debug-window
  (let [received (atom [])
        runtime (assoc (show/create-runtime {:workspace-options nil}) :event-admit
                       (fn [model batch] (swap! received into batch)
                         {:model (assoc-in model [:variables :observed?] true) :deliveries {}}))
        audio (:events! (show/audio-callbacks runtime))
        sync (:events! (show/sync-callbacks runtime))]
    (try
      (audio [{:type :audio/kick :source {:id :capture} :time-ms 8}])
      (sync {:type :beat-link/beat :source-id :deck :time-ms 9})
      (show/submit! runtime :command {:op :cue/launch :cue-id :ember})
      (show/tick! runtime 100)
      (is (empty? @(:observers runtime)))
      (is (= true (get-in @(:state runtime) [:model :variables :observed?])))
      (is (some #(and (= :audio/kick (:type %)) (= :capture (get-in % [:emitter :id]))) @received))
      (is (some #(and (= :beat-link/beat (:type %)) (= :deck (get-in % [:emitter :id]))) @received))
      (is (some #(and (= :instance/launched (:type %)) (= :ember (get-in % [:data :cue-id]))) @received))
      (reset! received [])
      (show/tick! runtime 110)
      (is (not-any? #(#{:audio/kick :beat-link/beat :instance/launched} (:type %)) @received))
      (finally (show/stop! runtime)))))

(deftest quarantined-effect-emits-one-committed-fault
  (let [captured (atom [])
        runtime (assoc (show/create-runtime {:model (fault-fixtures/show) :workspace-options nil
                                              :effect-log! (fn [_])})
                       :event-admit (fn [model events] (swap! captured into events) {:model model}))]
    (try
      (let [launch (show/submit! runtime :command {:op :cue/launch :cue-id :unstable})]
        (show/tick! runtime 25)
        (show/tick! runtime 50)
        (let [faults (filter #(= :effect/failed (:type %)) @captured)]
          (is (= 1 (count faults)))
          (is (= (:instance-id @launch) (get-in (first faults) [:data :instance-id])))
          (is (= :unstable (get-in (first faults) [:emitter :id])))))
      (finally (show/stop! runtime)))))

(deftest opaque-effect-intents-and-failed-handler-admission-preserve-committed-state
  (let [model {:show-id :main :epoch 1 :frame-number 2 :time-ms 18 :transport {:beat 0}}
        before (assoc-in model [:instances "1:1"] {:effect-id :old-effect :source {:adapter :midi}})
        state {:model model :output {:universes {}} :status {:last-intents [:spark 1 {:op :echo :instance-id "1:1"}]}}
        result (show-events/commit (fn [_ _] (throw (ex-info "Bad handler" {}))) before state [])]
    (is (= [:spark :effect/intent :echo] (mapv :type (:events result))))
    (is (= [:engine :engine :old-effect] (mapv #(get-in % [:emitter :id]) (:events result))))
    (is (= :midi (get-in result [:events 2 :data :cause-adapter])))
    (is (= model (get-in result [:state :model])))
    (is (= "Bad handler" (get-in result [:state :status :event-dispatch-error :message])))))

(deftest intent-payload-ids-and-originating-effect-attribution
  (let [source {:instance-id "origin" :effect-id :actual :effect-version 2
                :cue-id :actual-cue :adapter :event-subscriber}
        nested {:id "foreign" :effect-id :foreign :effect-version 12 :cue-id :foreign-cue
                :launch-mode :held :source {:adapter :studio}}
        explicit {:instance-id "other" :effect-id :target :effect-version 7 :cue-id :target-cue}
        intents [{:type :test/inspect :instance nested :cause-adapter :studio}
                 (merge explicit {:type :test/inspect :instance nested :cause-adapter :studio})
                 {:type :test/inspect :instance {:cue-id :partial-cue}}
                 {:type :test/inspect :instance {}}]
        model (subscribers/configure (demo/model)
                                    {:op :save :subscriber {:id :inspect
                                                            :match {:category :effect :type :test/inspect :emitter-id :actual}
                                                            :actions [{:op :cue/launch :select {:ids [:ember] :pick :first}}]}})
        state {:model model :status {:last-intents intents}
               ::show-events/intent-emissions (mapv #(hash-map :intent % :source source) intents)}
        result (show-events/commit subscribers/admit model state [])
        manual (show-events/commit subscribers/admit model
                                    (update state ::show-events/intent-emissions
                                            #(mapv (fn [emission] (assoc-in emission [:source :adapter] :repl)) %))
                                    [])
        origin (-> source (dissoc :adapter) (assoc :cause-adapter :event-subscriber))]
    (is (= (repeat 4 {:kind :effect :id :actual}) (map :emitter (:events result))))
    (is (= [{:instance-id "foreign" :effect-id :foreign :effect-version 12 :cue-id :foreign-cue
             :launch-mode :held :cause-adapter :event-subscriber}
            (merge origin explicit {:launch-mode :held})
            (assoc origin :cue-id :partial-cue)
            origin]
           (mapv :data (:events result))))
    (is (empty? (:deliveries result)))
    (is (= {0 [:inspect] 1 [:inspect] 2 [:inspect] 3 [:inspect]} (:deliveries manual)))
    (is (every? #(= :repl (get-in % [:data :cause-adapter])) (:events manual)))
    (is (= (dissoc state ::show-events/intent-emissions) (:state result)))))

(deftest a-blocked-encoder-cannot-stall-the-frame-owner
  (let [runtime (show/create-runtime {:workspace-options nil})
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
            (debug/commit! (get-in @(:observers runtime) [:studio :debug-stream]) {} [row] {} nil)
            (is (true? (deref entered 3000 false)))
            (let [producer (future
                             (dotimes [_ 20]
                               (debug/commit! (get-in @(:observers runtime) [:studio :debug-stream]) {} (vec (repeat 50 row)) {} nil))
                             :complete)]
              (is (= :complete (deref producer 2000 :timeout))))
            (deliver resume true)
            (let [first-batch (receive client)]
              (send! client {:op :ack :cursor (:cursor first-batch)}))
            (let [latest (receive client)]
              (is (= 488 (:skipped latest)))
              (is (= 490 (:id (first (:events latest)))))))
          (finally (deliver resume true) (.abort ^WebSocket (:socket client)))))
      (finally (show/stop! runtime) (.stop server)))))
