(ns tolglow.studio.state-stream-test
  (:require [afterglow.sync :as sync]
            [tolglow.connections :as connections]
            [clojure.test :refer [deftest is testing]]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.params :refer [wrap-params]]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.studio.projection :as projection]
            [tolglow.studio.state-stream :as stream]
            [tolglow.transit :as transit])
  (:import [java.net URI]
           [java.net.http HttpClient WebSocket WebSocket$Listener]
           [java.util.concurrent LinkedBlockingQueue TimeUnit]))

(defrecord TestColour [red green blue])

(deftest typed-edits-preserve-map-keys-and-record-boundaries
  (let [a (->TestColour 1 0 0) b (->TestColour 0 1 0)
        old {:value a :s {:red 1 "red" 2} :ids [{:id :a} {:id :b}]}
        new {:value b :s {:red 0 "green" 4} :ids [{:id :b} {:id :a}]}]
    (is (= #{[:set [:value] b] [:remove [:s "red"]] [:set [:s :red] 0] [:set [:s "green"] 4]
             [:set [:ids] [{:id :b} {:id :a}]]}
           (set (projection/typed-changes old new))))))

(defn- connect [server session]
  (let [port (.getLocalPort (first (.getConnectors server)))
        messages (LinkedBlockingQueue.)
        buffer (StringBuilder.)
        client (HttpClient/newHttpClient)
        uri (URI. (str "ws://127.0.0.1:" port "/api/state/stream?session=" session))
        socket (.get (.buildAsync (.newWebSocketBuilder client) uri
                                  (reify WebSocket$Listener
                                    (onOpen [_ ws] (.request ws 1))
                                    (onText [_ ws chunk last?]
                                      (.append buffer ^CharSequence chunk)
                                      (when last?
                                        (.offer messages (transit/decode (.toString buffer)))
                                        (.setLength buffer 0))
                                      (.request ws 1)
                                      nil))) 5 TimeUnit/SECONDS)]
    {:socket socket :messages messages}))

(defn- receive [client]
  (or (.poll ^LinkedBlockingQueue (:messages client) 3 TimeUnit/SECONDS)
      (throw (ex-info "Expected a pushed state message" {}))))

(defn- send-control [client data]
  (.get (.sendText ^WebSocket (:socket client) (transit/encode data) true) 3 TimeUnit/SECONDS))

(defn- acknowledge [client message] (send-control client {:op :ack :token (:token message)}))

(defn- apply-patch [base {:keys [kind view changes]}]
  (if (= kind :full) view
      (reduce (fn [data [op path value]]
                (case op
                  :set (assoc-in data path value)
                  :remove (update-in data (vec (butlast path)) dissoc (peek path))))
              base changes)))

(deftest clients-get-the-latest-committed-show-at-their-own-pace
  (let [runtime (show/create-runtime {:workspace-options nil})
        server (jetty/run-jetty (studio/handler runtime) {:host "127.0.0.1" :port 0 :join? false})]
    (try
      (let [slow (connect server "slow") fast (connect server "fast")
            slow-base (receive slow) fast-base (receive fast)]
        (try
          (is (= [:full :full] (mapv :kind [slow-base fast-base])))
          (is (= (:id runtime) (get-in slow-base [:identity :runtime-id])))
          (is (keyword? (get-in fast-base [:view :rig :fixtures 0 :id])))
          (acknowledge fast fast-base)
          (let [reply (show/submit! runtime :command {:op :cue/launch :cue-id :ember})]
            (show/tick! runtime 100)
            (is (= :accepted (:status @reply))))
          (let [next (receive fast)
                fast-view (apply-patch (:view fast-base) next)]
            (is (= :patch (:kind next)))
            (is (= (:token fast-base) (:base-token next)))
            (is (= :ember (get-in fast-view [:instances 0 :cue-id])))
            (is (= 100 (:time-ms fast-view))))
          (doseq [t (range 101 121)] (show/tick! runtime t))
          (is (nil? (.poll ^LinkedBlockingQueue (:messages slow) 90 TimeUnit/MILLISECONDS))
              "One unacknowledged snapshot bounds a slow client")
          (acknowledge slow slow-base)
          (let [next (receive slow) view (apply-patch (:view slow-base) next)]
            (is (= (:token slow-base) (:base-token next)))
            (is (= 120 (:time-ms view)))
            (is (= (get-in @(:state runtime) [:model :frame-number]) (:frame-number view)))
            (is (number? (get @(:leases runtime) "slow")))
            (send-control slow {:op :resync})
            (let [full (receive slow)]
              (is (= :full (:kind full)))
              (is (= view (:view full)))
              (acknowledge slow full)
              (is (nil? (.poll ^LinkedBlockingQueue (:messages slow) 90 TimeUnit/MILLISECONDS))
                  "Acknowledging a publication never generates another snapshot of it")))
          (finally (.abort ^WebSocket (:socket slow)) (.abort ^WebSocket (:socket fast)))))
      (finally (show/stop! runtime) (.stop server)))))

(deftest rendering-does-not-wait-for-projection
  (let [entered (promise) release (promise)
        hub (stream/create (fn [source] (deliver entered true) @release source) identity)
        server (jetty/run-jetty (wrap-params #(stream/response hub %)) {:host "127.0.0.1" :port 0 :join? false})]
    (try
      (let [v {:show-id :show :runtime-id "a" :epoch 0 :frame-number 1}
            _ (stream/publish! hub v) client (connect server "gate")]
        (try
          (is (= true (deref entered 2000 ::timeout)))
          (let [done (future (doseq [n (range 2 1000)]
                               (stream/publish! hub (assoc v :frame-number n))) :published)]
            (is (= :published (deref done 2000 ::timeout))))
          (deliver release true)
          (let [first-message (receive client)]
            (is (= 1 (get-in first-message [:view :frame-number])))
            (acknowledge client first-message)
            (is (= [[:set [:frame-number] 999]] (:changes (receive client)))))
          (finally (.abort ^WebSocket (:socket client)))))
      (finally (deliver release true) (stream/close! hub) (.stop server)))))

(deftest upgrade-is-same-origin-and-service-lifecycle-is-owned
  (let [hub (stream/create identity identity)
        http {:scheme :http :query-params {"session" "a"} :headers {"host" "127.0.0.1:8080"}}]
    (try
      (is (= 426 (:status (stream/response hub http))))
      (is (= 403 (:status (stream/response hub (assoc-in http [:headers "origin"] "https://another.show")))))
      (is (not (realized? (:worker hub))))
      (finally (stream/close! hub)))
    (is (= 503 (:status (stream/response hub http))))))

(deftest repeated-publications-obey-the-configured-cadence-and-identity-changes-reset-the-base
  (let [sample-times (atom [])
        hub (stream/create (fn [v] (swap! sample-times conj (System/nanoTime)) v) identity {:frame-rate 20})
        server (jetty/run-jetty (wrap-params #(stream/response hub %)) {:host "127.0.0.1" :port 0 :join? false})
        initial {:show-id :main :runtime-id "studio" :epoch 0 :frame-number 0}]
    (try
      (stream/publish! hub initial)
      (let [client (connect server "cadence")]
        (try
          (acknowledge client (receive client))
          (doseq [i (range 1 6)]
            (stream/publish! hub (assoc initial :frame-number i))
            (let [frame (receive client)]
              (is (= [[:set [:frame-number] i]] (:changes frame)))
              (acknowledge client frame)))
          ;; Measure projection on its owning worker; OS packet arrival times
          ;; can bunch together after a suspended client resumes.
          (is (every? #(>= % 50000000) (map - (rest @sample-times) @sample-times)))
          (stream/publish! hub (assoc initial :epoch 1))
          (let [frame (receive client)]
            (is (= :full (:kind frame)))
            (is (nil? (:base-token frame)))
            (is (= 1 (get-in frame [:identity :epoch]))))
          (finally (.abort ^WebSocket (:socket client)))))
      (let [reconnected (connect server "cadence")]
        (try (is (= (assoc initial :epoch 1) (:view (receive reconnected))))
             (finally (.abort ^WebSocket (:socket reconnected)))))
      (finally (stream/close! hub) (.stop server)))))

(defn- silence? [client]
  (nil? (.poll ^LinkedBlockingQueue (:messages client) 90 TimeUnit/MILLISECONDS)))

(deftest connections-status-is-opt-in-and-follows-owner-changes-with-a-stationary-show
  (let [runtime (show/create-runtime {:workspace-options nil})
        manager (connections/sync (:connections runtime))
        server (jetty/run-jetty (studio/handler runtime) {:host "127.0.0.1" :port 0 :join? false})
        observer (connect server "observer") tablet (connect server "tablet") phone (connect server "phone")]
    (try
      (doseq [client [observer tablet phone]]
        (let [base (receive client)]
          (is (not (contains? (:view base) :connection-status)))
          (acknowledge client base)))
      (doseq [client [tablet phone]] (send-control client {:op :connections :enabled? true}))
      (let [left (receive tablet) right (receive phone)
            base (:view left)]
        (is (= :full (:kind left) (:kind right)))
        (is (= (:token left) (:token right)))
        (is (= :idle (get-in base [:connection-status :audio :state])))
        (is (= :manual (get-in base [:connection-status :sync :config :selected-source])))
        (acknowledge tablet left)
        (sync/select-source! manager :ableton-link)
        (let [change (receive tablet)]
          (is (= :patch (:kind change)))
          (is (= (:token left) (:base-token change)))
          (is (= :ableton-link (get-in (apply-patch base change) [:connection-status :sync :clock :selected-source])))
          (is (silence? observer))
          (is (silence? phone))
          ;; Dropping a scope while its patch is outstanding must discard that
          ;; baseline. Even an eventual matching ACK cannot re-enable it.
          (send-control tablet {:op :connections :enabled? false})
          (acknowledge tablet change)
          (let [plain (receive tablet)]
            (is (= :full (:kind plain)))
            (is (not (contains? (:view plain) :connection-status)))
            (acknowledge tablet plain)))
        (acknowledge phone right)
        (let [latest (receive phone)]
          (is (= (:token right) (:base-token latest)))
          (send-control phone {:op :resync})
          (let [full (receive phone)]
            (is (= :full (:kind full)))
            (is (= (apply-patch base latest) (:view full)))
            (acknowledge phone full))))
      (is (silence? tablet))
      (send-control phone {:op :connections :enabled? false})
      (is (not (contains? (:view (receive phone)) :connection-status)))
      (is (empty? (.getWatches ^clojure.lang.IRef (:state manager))))
      (finally
        (doseq [client [observer tablet phone]] (.abort ^WebSocket (:socket client)))
        (show/stop! runtime)
        (.stop server)))))

(deftest status-watch-rebinds-to-the-restored-owners
  (let [runtime (show/create-runtime {:workspace-options nil})
        owner (:connections runtime) model (:model @(:state runtime))
        retired (connections/sync owner) signals (atom 0)
        detach! (connections/observe-status! owner #(swap! signals inc))]
    (try
      (connections/restore! owner (update model :epoch inc))
      (let [count-at-restore @signals current (connections/sync owner)
            snapshot (connections/status-at-commit owner)]
        (is (pos? count-at-restore))
        (is (empty? (.getWatches ^clojure.lang.IRef (:state retired))))
        (is (nil? (snapshot {:model model})))
        (is (= :idle (get-in (snapshot {:model (update model :epoch inc)}) [:audio :state])))
        (sync/select-source! current :beat-link)
        (is (> @signals count-at-restore))
        (detach!)
        (is (empty? (.getWatches ^clojure.lang.IRef (:state current)))))
      (finally (detach!) (show/stop! runtime)))))

(deftest capture-pushes-measured-waveform-without-a-render-tick
  (let [samples (byte-array (take 960 (cycle [(byte 0) (byte 32)])))
        input (java.io.ByteArrayInputStream. samples)
        runtime (show/create-runtime {:workspace-options nil
                                      :audio-options {:open-source! (fn [_] {:started-at-ms 0.0
                                                                            :read! #(.read input %1 %2 %3)
                                                                            :close! #(.close input)})}})
        server (jetty/run-jetty (studio/handler runtime) {:host "127.0.0.1" :port 0 :join? false})
        client (connect server "waveform")]
    (try
      (acknowledge client (receive client))
      (send-control client {:op :connections :enabled? true})
      (let [base (receive client)]
        (acknowledge client base)
        (let [handle (connections/start-audio! (:connections runtime) {:default? true} {:events! identity})
              result (deref (:result handle) 3000 ::timeout)]
          (is (= :completed (:status result)) (str (:error result))))
        (loop [old (:view base) remaining 4]
          (let [message (receive client)
                view (apply-patch old message)
                audio (get-in view [:connection-status :audio])]
            (acknowledge client message)
            (if (and (pos? remaining) (not= :completed (:state audio)))
              (recur view (dec remaining))
              (do (is (= :completed (:state audio)))
                  (is (= 480 (get-in audio [:waveform :sample-end])))
                  (is (seq (get-in audio [:waveform :bins])))
                  (is (= (:frame-number (:view base)) (:frame-number view))))))))
      (finally (.abort ^WebSocket (:socket client)) (show/stop! runtime) (.stop server)))))
