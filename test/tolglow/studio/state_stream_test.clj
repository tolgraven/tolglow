(ns tolglow.studio.state-stream-test
  (:require [afterglow.reactive :as rx]
            [afterglow.sync :as sync]
            [tolglow.connections :as connections]
            [tolglow.development.observation :as observation]
            [tolglow.development.workspace :as workspace]
            [tolglow.development.workspace-test :as fixture]
            [tolglow.application :as app]
            [tolglow.rig.rdm-gateway :as gateway]
            [tolglow.rig.rdm-test :as rdm-peer]
            [tolglow.studio.rdm-host-test :as rdm-host]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.params :refer [wrap-params]]
            [tolglow.show :as show]
            [tolglow.show.recovery :as recovery]
            [tolglow.studio :as studio]
            [tolglow.studio.access :as access]
            [tolglow.studio.projection :as projection]
            [tolglow.studio.query-stream :as query-stream]
            [tolglow.studio.state-stream :as stream]
            [tolglow.studio.web :as web]
            [tolglow.transit :as transit])
  (:import [java.io StringReader]
           [java.net URI]
           [java.net.http HttpClient WebSocket WebSocket$Listener]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util.concurrent CountDownLatch LinkedBlockingQueue TimeUnit]
           [java.util.concurrent.atomic AtomicInteger]))

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
        closed (promise)
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
                                      nil)
                                    (onClose [_ _ code reason]
                                      (deliver closed [code reason])
                                      nil))) 5 TimeUnit/SECONDS)]
    {:socket socket :messages messages :closed closed}))

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

(deftest queued-checkpoint-results-follow-the-actual-owner-commit
  (let [directory (.toFile (Files/createTempDirectory "tolglow-checkpoint-stream-" (make-array FileAttribute 0)))
        runtime (assoc (show/create-runtime {:workspace-options nil :workers 1})
                       :recovery (atom (recovery/start! (recovery/options {:enabled? false :directory directory})
                                                        false (constantly nil) (constantly nil))))]
    (try
      (show/tick! runtime 0)
      (show/save-show! runtime (io/file directory "named.transit"))
      (let [master (show/submit! runtime {:op :master/set :value 0.3})]
        (show/tick! runtime 1)
        (is (= :accepted (:status @master))))
      (let [handler (studio/handler runtime)
            server (jetty/run-jetty handler {:host "127.0.0.1" :port 0 :join? false})]
        (try
          (let [socket (connect server "checkpoint-stream")]
            (try
              (let [initial (receive socket)
                    owner (:identity initial)
                    restore #(let [response (handler {:request-method :post :uri "/api/show/checkpoints/restore"
                                                     :headers {"accept" "application/transit+json" "content-type" "application/transit+json"}
                                                     :body (java.io.StringReader. (transit/encode (assoc owner :kind :named)))})]
                               (is (= 200 (:status response)))
                               (transit/decode (:body response)))
                    [accepted rejected] (with-redefs [studio/checkpoint-wait-ms 0] [(restore) (restore)])
                    accepted-id (:receipt-id accepted)
                    rejected-id (:receipt-id rejected)]
                (acknowledge socket initial)
                (is (= [:pending :pending] (mapv :status [accepted rejected])))
                (is (every? string? [accepted-id rejected-id]))
                (is (not= accepted-id rejected-id))
                (is (= 0.3 (:master (show/snapshot runtime))))
                (is (nil? (get-in initial [:view :checkpoint-results])))
                (show/tick! runtime 2)
                (let [restored (receive socket)
                      epoch (inc (:epoch owner))
                      expected [{:id accepted-id :identity owner :status :accepted :epoch epoch}
                                {:id rejected-id :identity owner :status :rejected :reason :stale-show-session}]]
                  (is (= :full (:kind restored)))
                  (is (= epoch (get-in restored [:identity :epoch])))
                  (is (= 1.0 (get-in restored [:view :master])))
                  (is (= expected (get-in restored [:view :checkpoint-results])))
                  (acknowledge socket restored)
                  (show/tick! runtime 3)
                  (let [reconnected (connect server "checkpoint-reconnect")]
                    (try
                      (is (= expected (get-in (receive reconnected) [:view :checkpoint-results])))
                      (finally (.abort ^WebSocket (:socket reconnected)))))))
              (finally (.abort ^WebSocket (:socket socket)))))
          (finally (.stop server))))
      (finally
        (show/stop! runtime)
        (doseq [file (reverse (file-seq directory))] (io/delete-file file))))))

(deftest timed-out-history-requests-push-exact-terminal-results-through-the-show-stream
  (let [runtime (show/create-runtime {:workers 1 :workspace-options nil})
        app (access/listener-handler (studio/handler runtime) {})
        server (jetty/run-jetty app {:host "127.0.0.1" :port 0 :join? false})
        submit (fn [payload time & [authority]]
                 (let [reply (show/submit! runtime :command payload authority)]
                   (show/tick! runtime time)
                   (deref reply 1000 ::missing)))
        post (fn [command]
               (with-redefs-fn {#'web/history-ack-timeout-ms 0}
                 #(let [response (app {:request-method :post :uri "/api/command"
                                       :headers {"host" "localhost" "accept" "application/transit+json"
                                                 "content-type" "application/transit+json" "x-tolglow-form" "studio-v1"}
                                       :body (StringReader. (transit/encode command))})]
                    [(:status response) (transit/decode (:body response))])))]
    (try
      (let [id (:instance-id (submit {:op :cue/launch :cue-id :ember :launch {:params {:level 0.2}}} 0))
            gesture {:instance-id id :param :level :control-id :test :gesture-id :change}
            operator {:operator-id :test-operator}
            scope {:runtime-id (:id runtime) :expected-epoch 0}
            undo (merge scope {:op :history/undo :command-id "delayed-undo"
                               :expected-cursor 1 :expected-history-position 1})
            reject (merge scope {:op :history/redo :command-id "delayed-rejection" :node-id 99
                                 :expected-cursor 0 :expected-history-position 2})]
        (is (= :accepted (:status (submit (assoc gesture :op :gesture/begin) 1 operator))))
        (is (= :accepted (:status (submit (assoc gesture :op :gesture/sample :sequence 1 :value 0.7) 2 operator))))
        (is (= :accepted (:status (submit (assoc gesture :op :gesture/end :last-sequence 1) 3 operator))))
        (let [client (connect server "delayed-history")]
          (try
            (let [baseline (receive client)]
              (doseq [command [undo reject]]
                (let [[status receipt] (post command)]
                  (is (= 202 status))
                  (is (= {:status :pending :command-id (:command-id command) :epoch 0}
                         (select-keys receipt [:status :command-id :epoch])))
                  (is (nil? (:revision receipt)))))
              (is (= 1 (get-in baseline [:view :operator-history :current-id])))
              (is (empty? (get-in baseline [:view :history-command-outcomes])))
              ;; Hold the baseline ACK through a subsequent commit to exercise
              ;; coalescing, including the rejection that made no model edit.
              (show/tick! runtime 4)
              (show/tick! runtime 5)
              (acknowledge client baseline)
              (let [message (receive client)
                    view (apply-patch (:view baseline) message)
                    [accepted rejected] (:history-command-outcomes view)]
                (is (= ["delayed-undo" 0 :accepted 0 2]
                       (mapv accepted [:command-id :epoch :status :history-cursor :history-position])))
                (is (= ["delayed-rejection" 0 :rejected :unknown-history-node]
                       (mapv rejected [:command-id :epoch :status :code])))
                (is (every? #(not-any? % [:authority :operator-id :source :details]) [accepted rejected]))
                (is (= [0 2] ((juxt :current-id :position) (:operator-history view))))
                (is (= 0.2 (get-in @(:state runtime) [:model :instances id :params :level])))
                (is (= :accepted (:status (submit {:op :show/restart} 6))))
                (acknowledge client message)
                (let [restart (receive client)]
                  (is (= 1 (get-in restart [:identity :epoch])))
                  (is (= [] (get-in restart [:view :history-command-outcomes]))))))
            (finally (.abort ^WebSocket (:socket client))))))
      (finally (show/stop! runtime) (.stop server)))))

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

(deftest common-view-reconstructs-show-fields-and-spatial-members-are-leased
  (let [runtime (show/create-runtime {:workspace-options nil})
        server (jetty/run-jetty (studio/handler runtime) {:host "127.0.0.1" :port 0 :join? false})]
    (try
      (let [client (connect server "spatial-lease") initial (receive client)
            query [:spatial/members :room]]
        (try
          (acknowledge client initial)
          (let [receipt (show/submit! runtime :settings {:scope :spatial :values
                         {:enabled? true :run-when :enabled
                          :volumes {:room {:shape :sphere :position [0 0 0] :radius 100}}}})]
            (show/tick! runtime 25)
            (is (= :accepted (:status @receipt))))
          (let [common-message (receive client)
                common (apply-patch (:view initial) common-message)
                domain (show/snapshot runtime)
                members (get-in domain [:spatial :members :room])]
            (is (seq members))
            (is (= (dissoc domain :spatial :status) (dissoc common :status)))
            (is (not (contains? common :spatial)))
            (acknowledge client common-message)
            (send-control client {:op :queries :queries [query]})
            (let [leased (:view (receive client))]
              (is (= members (get-in leased [:subscriptions query :value])))
              (is (= (dissoc common :status) (dissoc leased :status :subscriptions)))))
          (finally (.abort ^WebSocket (:socket client)))))
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

(deftest leased-query-readers-share-one-graph-and-recover-with-typed-keyframes
  (let [query [:field/body :ball]
        model {:show-id :fixture-show :epoch 0 :revision 0 :frame-number 0
               :settings {:studio {:public-queries [:field/body]}} :ball {:position [0 1 0]}}
        graph (rx/create-system model)
        calls (atom 0) owner (atom graph)
        hub (stream/create (fn [_] (throw (ex-info "Query-only socket projected the whole show" {})))
                           (constantly nil)
                           {:runtime-id "server" :get-graph #(deref owner) :checkpoint-every 1})
        server (jetty/run-jetty (wrap-params #(stream/response hub %)) {:host "127.0.0.1" :port 0 :join? false})
        change! (fn [i ball] (let [m (assoc model :frame-number i :ball ball)]
                               (rx/publish! @owner m) (stream/publish! hub {:model m})))
        take! (fn [client] (let [m (receive client)] (acknowledge client m) m))
        register! (fn [system] (rx/reg-sub! system :field/body
                                  (fn [{:keys [db]} _] (swap! calls inc) (:ball @db))))
        open! #(connect server (str % "&view=queries"))]
    (try
      (register! graph)
      (stream/publish! hub {:model model})
      (let [a (open! "a") b (open! "b")]
        (try
          (doseq [client [a b]]
            (is (= {} (get-in (take! client) [:view :subscriptions])))
            (send-control client {:op :queries :queries [query]})
            (is (= (:ball model) (get-in (take! client) [:view :subscriptions query :value]))))
          (is (= 1 @calls))
          (change! 1 (:ball model))
          (is (nil? (.poll ^LinkedBlockingQueue (:messages a) 120 TimeUnit/MILLISECONDS)))
          (change! 2 {:position [0 2 0]})
          (doseq [client [a b]]
            (let [patch (take! client)]
              (is (= :patch (:kind patch)))
              (is (some #{[:set [:subscriptions query :value :position 1] 2]} (:changes patch)))))
          (change! 3 {:position [0 3 0]})
          (doseq [client [a b]]
            (is (= :full (:kind (take! client))) "A fresh baseline follows one changed delta"))
          (send-control a {:op :resync})
          (is (= 3 (get-in (take! a) [:view :subscriptions query :value :position 1])))
          (send-control b {:op :queries :queries [[:field/private]]})
          (is (= :not-exposed (get-in (take! b) [:view :subscriptions [:field/private] :error])))
          (is (rx/demanded? graph #{:field/body}))
          (send-control a {:op :queries :queries []})
          (is (empty? (get-in (take! a) [:view :subscriptions])))
          (is (false? (rx/demanded? graph #{:field/body})))
          (let [n @calls] (change! 4 {:position [0 4 0]}) (is (= n @calls)))
          (send-control a {:op :queries :queries [query]})
          (is (= 4 (get-in (take! a) [:view :subscriptions query :value :position 1])))
          (let [next-model (assoc model :epoch 1 :ball {:position [9 9 9]})
                next-graph (rx/create-system next-model)]
            (register! next-graph)
            (reset! owner next-graph)
            (stream/publish! hub {:model next-model})
            (let [frame (take! a)]
              (is (= [:full 1 [9 9 9]]
                     [(:kind frame) (get-in frame [:identity :epoch])
                      (get-in frame [:view :subscriptions query :value :position])])))
            (is (false? (rx/demanded? graph #{:field/body}))))
          (finally (.abort ^WebSocket (:socket a)) (.abort ^WebSocket (:socket b)))))
      (finally (stream/close! hub) (.stop server) (rx/close! graph)
               (when-not (identical? @owner graph) (rx/close! @owner))))))

(deftest last-query-release-does-not-wait-for-another-clients-projection
  (let [query [::sampled-query]
        model {:show-id :fixture-show :epoch 0 :revision 0 :frame-number 0
               :settings {:studio {:public-queries [(first query)]}}}
        graph (rx/create-system model)
        graph-state (:state graph)
        lease-count #(count (get-in % [:options [:node [:subscription query]] :watches]))
        sample-starts (LinkedBlockingQueue.)
        sample-interrupted (CountDownLatch. 1)
        finish-sample (CountDownLatch. 1)
        projection-started (CountDownLatch. 1)
        finish-projection (CountDownLatch. 1)
        unsubscribe-received (CountDownLatch. 1)
        resubscribe-received (CountDownLatch. 1)
        lease-released (CountDownLatch. 1)
        hub (stream/create (fn [{:keys [model block-projection?]}]
                             (when block-projection?
                               (.countDown projection-started)
                               (.await finish-projection))
                             (assoc model :runtime-id "server"))
                           (constantly nil) {:runtime-id "server" :get-graph (constantly graph)})
        server (jetty/run-jetty (wrap-params #(stream/response hub %))
                               {:host "127.0.0.1" :port 0 :join? false})]
    (try
      (rx/reg-cofx! graph ::sample {:mode :background :interval {:ms 20} :initial :waiting
                                    :sample (fn [_]
                                              (.offer sample-starts :started)
                                              (try (.await finish-sample)
                                                   (catch InterruptedException e
                                                     (.countDown sample-interrupted)
                                                     (throw e)))
                                              :complete)})
      (rx/reg-sub! graph (first query) (fn [{:keys [cofx]} _] (cofx ::sample)))
      (stream/publish! hub {:model model})
      (let [reader (connect server "reader&view=queries") spectator (connect server "spectator")]
        (try
          (acknowledge reader (receive reader))
          (acknowledge spectator (receive spectator))
          (send-control reader {:op :queries :queries [query]})
          (let [initial (receive reader)]
            (is (= :waiting (get-in initial [:view :subscriptions query :value])))
            (acknowledge reader initial))
          (is (= :started (.poll sample-starts 3 TimeUnit/SECONDS)))
          (is (= 1 (lease-count @graph-state)))
          (add-watch graph-state ::lease-release
                     (fn [_ _ before after]
                       (when (and (= 1 (lease-count before)) (zero? (lease-count after)))
                         (.countDown lease-released))))
          (let [reader-state (first (filter #(:query-only? @%) (vals @(:clients hub))))]
            (add-watch reader-state ::unsubscribe
                       (fn [_ _ before after]
                         (cond
                           (and (seq (:queries before)) (empty? (:queries after)))
                           (.countDown unsubscribe-received)

                           (and (empty? (:queries before)) (seq (:queries after)))
                           (.countDown resubscribe-received))))
            (try
              (stream/publish! hub {:model (assoc model :frame-number 1) :block-projection? true})
              (is (.await projection-started 3 TimeUnit/SECONDS))
              (send-control reader {:op :queries :queries []})
              (is (.await unsubscribe-received 3 TimeUnit/SECONDS))
              (is (.await lease-released 3 TimeUnit/SECONDS)
                  "Removing the last query releases its graph lease while projection is blocked")
              (is (zero? (lease-count @graph-state)))
              (is (.await sample-interrupted 3 TimeUnit/SECONDS)
                  "The final release interrupts the active background sample")
              (send-control reader {:op :queries :queries [query]})
              (is (.await resubscribe-received 3 TimeUnit/SECONDS))
              (.countDown finish-projection)
              (let [remounted (receive reader)]
                (is (= :full (:kind remounted)))
                (is (= :waiting (get-in remounted [:view :subscriptions query :value]))))
              (is (= :started (.poll sample-starts 3 TimeUnit/SECONDS)))
              (is (= 1 (lease-count @graph-state)))
              (finally (remove-watch reader-state ::unsubscribe))))
          (finally
            (.countDown finish-projection)
            (.abort ^WebSocket (:socket reader))
            (.abort ^WebSocket (:socket spectator)))))
      (finally
        (remove-watch graph-state ::lease-release)
        (.countDown finish-projection)
        (.countDown finish-sample)
        (stream/close! hub)
        (.stop server)
        (rx/close! graph)))))

(deftest unobserved-surface-retires-while-another-client-is-projecting
  (let [query [::surface :fixture/one]
        model {:show-id :fixture-show :epoch 0 :revision 0 :frame-number 0}
        observed (atom {:color :blue})
        mounts (atom 0)
        samples (atom [])
        closing (LinkedBlockingQueue.)
        finish-close (CountDownLatch. 1)
        project! (fn [{:keys [model projection-started finish-projection]}]
                   (when projection-started
                     (.countDown projection-started)
                     (.await finish-projection 10 TimeUnit/SECONDS))
                   (assoc model :runtime-id "server"))
        hub (stream/create project! (constantly nil)
                           {:runtime-id "server" :get-graph (constantly nil)
                            :surfaces {::surface
                                       (fn [[_ fixture-id]]
                                         (let [generation (swap! mounts inc)]
                                           (assoc (observation/selected observed identity
                                                    (fn [value]
                                                      (swap! samples conj [generation (:color value)])
                                                      (assoc value :fixture fixture-id :generation generation)))
                                                  :close! (fn []
                                                            (.offer closing generation)
                                                            (when (= 1 generation)
                                                              (.await finish-close 10 TimeUnit/SECONDS))))))}})
        server (jetty/run-jetty (wrap-params #(stream/response hub %))
                               {:host "127.0.0.1" :port 0 :join? false})
        projection-started (CountDownLatch. 1)
        finish-projection (CountDownLatch. 1)
        replacement-demand (CountDownLatch. 1)
        next-projection-started (CountDownLatch. 1)
        finish-next-projection (CountDownLatch. 1)
        take! (fn [client] (let [frame (receive client)] (acknowledge client frame) frame))
        observed-value #(get-in % [:view :subscriptions query :value])
        watch-count #(count (.getWatches observed))]
    (try
      (stream/publish! hub {:model model})
      (let [spectator (connect server "spectator")
            a (connect server "surface-a&view=queries")
            _ (take! a)
            a-state (first (filter #(:query-only? @%) (vals @(:clients hub))))
            b (connect server "surface-b&view=queries")]
        (try
          (take! b)
          (doseq [client [a b]]
            (send-control client {:op :queries :queries [query]})
            (is (= {:color :blue :fixture :fixture/one :generation 1} (observed-value (take! client)))))
          (let [spectator-base (take! spectator)]
            (send-control a {:op :queries :queries []})
            (is (empty? (get-in (take! a) [:view :subscriptions])))
            (is (= 1 @mounts (watch-count)))
            (stream/publish! hub {:model (assoc model :frame-number 1)
                                  :projection-started projection-started :finish-projection finish-projection})
            (is (.await projection-started 3 TimeUnit/SECONDS))
            (send-control b {:op :queries :queries []})
            (is (= 1 (.poll closing 3 TimeUnit/SECONDS)) "The final unsubscribe starts cleanup while projection is blocked")
            (is (zero? (watch-count)))
            (reset! observed {:color :green})
            (add-watch a-state ::surface-demand
                       (fn [_ _ _ next] (when (seq (:queries next)) (.countDown replacement-demand))))
            (send-control a {:op :queries :queries [query]})
            (is (.await replacement-demand 3 TimeUnit/SECONDS))
            (.countDown finish-projection)
            (let [first-pass (apply-patch (:view spectator-base) (take! spectator))]
              (is (= 1 (:frame-number first-pass)))
              (stream/publish! hub {:model (assoc model :frame-number 2)})
              (is (= 2 (:frame-number (apply-patch first-pass (take! spectator)))))
              (is (= 1 @mounts) "A fresh delivery pass sees the new demand and waits for the retiring producer"))
            (.countDown finish-close)
            (is (empty? (get-in (take! b) [:view :subscriptions])))
            (let [remounted (take! a)]
              (is (= :full (:kind remounted)))
              (is (= {:color :green :fixture :fixture/one :generation 2} (observed-value remounted))))
            (is (= [[1 :blue] [2 :green]] @samples) "The pre-removal delivery pass cannot sample the retired lease")
            (is (= 1 (watch-count)))
            (stream/publish! hub {:model (assoc model :frame-number 3)
                                  :projection-started next-projection-started :finish-projection finish-next-projection})
            (is (.await next-projection-started 3 TimeUnit/SECONDS))
            (.abort ^WebSocket (:socket a))
            (is (= 2 (.poll closing 3 TimeUnit/SECONDS)) "Disconnecting the last reader releases the replacement")
            (is (zero? (watch-count))))
          (finally
            (remove-watch a-state ::surface-demand)
            (.countDown finish-projection)
            (.countDown finish-next-projection)
            (.countDown finish-close)
            (doseq [client [a b spectator]] (.abort ^WebSocket (:socket client))))))
      (finally (stream/close! hub) (.stop server)))))

(deftest failed-detach-fences-the-surface-until-shutdown-retries-it
  (let [query [::faulty-surface]
        observed (atom :blue)
        mounts (atom 0)
        closes (atom 0)
        detach-attempts (atom 0)
        test-detach! (promise)
        hub (stream/create identity (constantly nil)
                           {:get-graph (constantly nil)
                            :surfaces {::faulty-surface
                                       (fn [_]
                                         (swap! mounts inc)
                                         (let [lease (observation/selected observed identity identity)]
                                           (assoc lease :observe! (fn [wake!]
                                                                    (let [detach! ((:observe! lease) wake!)]
                                                                      (deliver test-detach! detach!)
                                                                      #(if (= 1 (swap! detach-attempts inc))
                                                                         (throw (ex-info "Surface detach failed" {}))
                                                                         (detach!))))
                                                        :close! #(swap! closes inc))))}})
        server (jetty/run-jetty (wrap-params #(stream/response hub %))
                               {:host "127.0.0.1" :port 0 :join? false})
        take! (fn [client] (let [frame (receive client)] (acknowledge client frame) frame))]
    (try
      (stream/publish! hub {:model {:show-id :fixture-show :epoch 0}})
      (let [first-reader (connect server "original&view=queries")]
        (try
          (take! first-reader)
          (send-control first-reader {:op :queries :queries [query]})
          (is (= {:value :blue} (get-in (take! first-reader) [:view :subscriptions query])))
          (send-control first-reader {:op :queries :queries []})
          (is (= {} (get-in (take! first-reader) [:view :subscriptions])))
          (let [replacement (connect server "replacement&view=queries")]
            (try
              (take! replacement)
              (send-control replacement {:op :queries :queries [query]})
              (is (= {:error :unavailable} (get-in (take! replacement) [:view :subscriptions query])))
              (is (= 1 @mounts @closes (count (.getWatches observed))))
              (finally (.abort ^WebSocket (:socket replacement)))))
          (finally (.abort ^WebSocket (:socket first-reader)))))
      (stream/close! hub)
      (is (= 2 @detach-attempts))
      (is (= 1 @closes))
      (is (empty? (.getWatches observed)))
      (finally
        (stream/close! hub)
        (.stop server)
        (when (realized? test-detach!) (@test-detach!))))))

(deftest shutdown-closes-all-sockets-and-leases-after-a-surface-close-fails
  (let [observed (atom :blue)
        closed-queries (atom [])
        surface (fn [query]
                  (assoc (observation/selected observed identity identity)
                         :close! (fn []
                                   (swap! closed-queries conj query)
                                   (when (= query [::broken])
                                     (throw (ex-info "Surface close failed" {}))))))
        hub (stream/create identity (constantly nil)
                           {:get-graph (constantly nil) :surfaces {::broken surface ::healthy surface}})
        server (jetty/run-jetty (wrap-params #(stream/response hub %))
                               {:host "127.0.0.1" :port 0 :join? false})]
    (try
      (stream/publish! hub {:model {:show-id :fixture-show :epoch 0}})
      (let [broken (connect server "broken&view=queries")
            healthy (connect server "healthy&view=queries")]
        (try
          (doseq [[client query] [[broken [::broken]] [healthy [::healthy]]]]
            (acknowledge client (receive client))
            (send-control client {:op :queries :queries [query]})
            (is (= {:value :blue} (get-in (receive client) [:view :subscriptions query]))))
          (is (= 2 (count (.getWatches observed))))
          (stream/close! hub)
          (doseq [client [broken healthy]]
            (is (= [1001 "Studio stopped"] (deref (:closed client) 3000 nil))))
          (stream/close! hub)
          (is (= {[::broken] 1 [::healthy] 1} (frequencies @closed-queries)))
          (is (empty? (.getWatches observed)))
          (finally
            (doseq [client [broken healthy]] (.abort ^WebSocket (:socket client))))))
      (finally (stream/close! hub) (.stop server)))))

(deftest shutdown-waits-for-a-retirement-already-claimed-by-a-socket
  (let [query [::surface]
        closing (CountDownLatch. 1)
        finish-close (CountDownLatch. 1)
        closes (atom 0)
        hub (stream/create identity (constantly nil)
                           {:get-graph (constantly nil)
                            :surfaces {::surface (fn [_] {:sample! (constantly :blue)
                                                         :close! (fn []
                                                                   (.countDown closing)
                                                                   (.await finish-close 10 TimeUnit/SECONDS)
                                                                   (swap! closes inc))})}})
        server (jetty/run-jetty (wrap-params #(stream/response hub %))
                               {:host "127.0.0.1" :port 0 :join? false})]
    (try
      (stream/publish! hub {:model {:show-id :fixture-show :epoch 0}})
      (let [reader (connect server "reader&view=queries")]
        (try
          (acknowledge reader (receive reader))
          (send-control reader {:op :queries :queries [query]})
          (is (= {:value :blue} (get-in (receive reader) [:view :subscriptions query])))
          (send-control reader {:op :queries :queries []})
          (is (.await closing 3 TimeUnit/SECONDS))
          (let [shutdown (future (stream/close! hub) :stopped)]
            (try
              (is (= [1001 "Studio stopped"] (deref (:closed reader) 3000 nil)))
              (is (= :waiting (deref shutdown 100 :waiting)))
              (finally (.countDown finish-close)))
            (is (= :stopped (deref shutdown 3000 nil)))
            (is (= 1 @closes)))
          (finally (.abort ^WebSocket (:socket reader)))))
      (finally
        (.countDown finish-close)
        (stream/close! hub)
        (.stop server)))))

(deftest terminal-query-cleanup-keeps-ownership-after-the-shutdown-wait
  (let [blocked (mapv #(vector ::a-blocked %) (range 4))
        healthy [::z-healthy 0]
        requested (into (sorted-set healthy) blocked)
        source {:model {:show-id :fixture-show :epoch 0}}
        closing (CountDownLatch. 4)
        finish-close (CountDownLatch. 1)
        closed (CountDownLatch. 4)
        healthy-detached (promise)
        healthy-closed (promise)
        observed (atom :blue)
        attempts (atom {})
        store (query-stream/create (constantly nil) (constantly nil)
                                   {::a-blocked (fn [query]
                                                  {:sample! (constantly :blue)
                                                   :close! (fn []
                                                             (swap! attempts update query (fnil inc 0))
                                                             (.countDown closing)
                                                             (.await finish-close 10 TimeUnit/SECONDS)
                                                             (.countDown closed))})
                                    ::z-healthy (fn [_]
                                                  (let [lease (observation/selected observed identity identity)]
                                                    (assoc lease
                                                           :observe! (fn [wake!]
                                                                       (let [detach! ((:observe! lease) wake!)]
                                                                         (fn []
                                                                           (.await closing 3 TimeUnit/SECONDS)
                                                                           (detach!)
                                                                           (deliver healthy-detached true))))
                                                           :close! #(deliver healthy-closed true))))})]
    (try
      (is (= (conj blocked healthy) (vec (keys (query-stream/reconcile! store source requested)))))
      (is (= 1 (count (.getWatches observed))))
      (let [shutdown (future (query-stream/close! store) :stopped)]
        (is (.await closing 3 TimeUnit/SECONDS))
        (is (true? (deref healthy-detached 3000 nil)))
        (is (empty? (.getWatches observed)))
        (is (= :stopped (deref shutdown 3000 nil)))
        (is (= 4 (.getCount closed)))
        (is (nil? (query-stream/reconcile! store source requested)))
        (query-stream/close! store)
        (.countDown finish-close)
        (is (.await closed 3 TimeUnit/SECONDS))
        (is (true? (deref healthy-closed 3000 nil)))
        (is (= (zipmap blocked (repeat 1)) @attempts)))
      (finally (.countDown finish-close) (query-stream/close! store)))))

(deftest invalid-control-closes-the-socket-before-its-lease-finishes
  (let [query [::surface]
        closing (CountDownLatch. 1)
        finish-close (CountDownLatch. 1)
        released (promise)
        hub (stream/create identity (constantly nil)
                           {:get-graph (constantly nil)
                            :surfaces {::surface (fn [_] {:sample! (constantly :blue)
                                                         :close! (fn []
                                                                   (.countDown closing)
                                                                   (deliver released (.await finish-close 10 TimeUnit/SECONDS)))})}})
        server (jetty/run-jetty (wrap-params #(stream/response hub %))
                               {:host "127.0.0.1" :port 0 :join? false})]
    (try
      (stream/publish! hub {:model {:show-id :fixture-show :epoch 0}})
      (let [reader (connect server "invalid&view=queries")]
        (try
          (acknowledge reader (receive reader))
          (send-control reader {:op :queries :queries [query]})
          (is (= {:value :blue} (get-in (receive reader) [:view :subscriptions query])))
          (send-control reader {:op :invalid})
          (is (.await closing 3 TimeUnit/SECONDS))
          (is (= [1008 "Invalid stream control"] (deref (:closed reader) 3000 nil)))
          (let [shutdown (future (stream/close! hub) :stopped)]
            (is (= :stopped (deref shutdown 3000 nil)))
            (is (not (realized? released))))
          (.countDown finish-close)
          (is (true? (deref released 3000 nil)))
          (finally (.abort ^WebSocket (:socket reader)))))
      (finally (.countDown finish-close) (stream/close! hub) (.stop server)))))

(deftest shutdown-cancels-query-admission-before-a-blocked-open-or-observe-finishes
  (doseq [stage [:graph :open :observe]]
    (testing (name stage)
      (let [query [::surface]
            opening (CountDownLatch. 1)
            finish-opening (CountDownLatch. 1)
            observed (atom :blue)
            observe-count (atom 0)
            released (promise)
            block! (fn [step] (when (= stage step)
                               (.countDown opening)
                               (.await finish-opening 10 TimeUnit/SECONDS)))
            hub (stream/create identity (constantly nil)
                               {:get-graph (fn [] (block! :graph) nil)
                                :surfaces {::surface
                                           (fn [_]
                                             (block! :open)
                                             (let [lease (observation/selected observed identity identity)]
                                               (assoc lease :observe! (fn [wake!]
                                                                        (swap! observe-count inc)
                                                                        (let [detach! ((:observe! lease) wake!)]
                                                                          (block! :observe)
                                                                          detach!))
                                                            :close! #(deliver released true))))}})
            server (jetty/run-jetty (wrap-params #(stream/response hub %))
                                   {:host "127.0.0.1" :port 0 :join? false})]
        (try
          (stream/publish! hub {:model {:show-id :fixture-show :epoch 0}})
          (let [reader (connect server "blocked&view=queries")]
            (try
              (acknowledge reader (receive reader))
              (send-control reader {:op :queries :queries [query]})
              (is (.await opening 3 TimeUnit/SECONDS))
              (let [shutdown (future (stream/close! hub) :stopped)]
                (is (= [1001 "Studio stopped"] (deref (:closed reader) 3000 nil)))
                (is (= :stopped (deref shutdown 3000 nil)))
                (is (:closed? @(:state (:query-store hub))))
                (is (empty? (:leases @(:state (:query-store hub)))))
                (is (not (realized? released))))
              (.countDown finish-opening)
              (.join ^Thread @(:worker hub) 3000)
              (is (not (.isAlive ^Thread @(:worker hub))))
              (is (= (not= stage :graph) (realized? released)))
              (is (= (if (= stage :observe) 1 0) @observe-count))
              (is (empty? (.getWatches observed)))
              (finally (.abort ^WebSocket (:socket reader)))))
          (finally (.countDown finish-opening) (stream/close! hub) (.stop server)))))))

(deftest shutdown-closes-all-sockets-and-releases-other-scopes-after-a-scoped-release-fails
  (let [broken [::scoped :broken]
        healthy [::scoped :healthy]
        releases (atom [])
        hub (stream/create identity (constantly nil)
                           {:get-graph (constantly nil) :scoped-queries {::scoped identity}
                            :release-query! (fn [query]
                                              (swap! releases conj query)
                                              (when (= broken query) (throw (ex-info "Scoped release failed" {}))))
                            :surfaces {::scoped (fn [query] {:sample! (constantly (second query))})}})
        server (jetty/run-jetty (wrap-params #(stream/response hub %))
                               {:host "127.0.0.1" :port 0 :join? false})]
    (try
      (stream/publish! hub {:model {:show-id :fixture-show :epoch 0}})
      (let [first-reader (connect server "scoped-broken&view=queries")
            other-reader (connect server "scoped-healthy&view=queries")]
        (try
          (doseq [[client query] [[first-reader broken] [other-reader healthy]]]
            (acknowledge client (receive client))
            (send-control client {:op :queries :queries [query]})
            (is (= {:value (second query)} (get-in (receive client) [:view :subscriptions query]))))
          (stream/close! hub)
          (doseq [client [first-reader other-reader]]
            (is (= [1001 "Studio stopped"] (deref (:closed client) 3000 nil))))
          (is (empty? @(:clients hub)))
          (is (= {broken 1 healthy 1} (frequencies @releases)))
          (finally (doseq [client [first-reader other-reader]] (.abort ^WebSocket (:socket client))))))
      (finally (stream/close! hub) (.stop server)))))

(deftest subscription-baselines-wait-for-a-retiring-query
  (let [query [::mount]
        model {:show-id :fixture-show :epoch 0 :revision 0 :frame-number 0
               :settings {:studio {:public-queries [(first query)]}}}
        graph (rx/create-system model)
        mounts (atom 0)
        acquire! rx/acquire!
        release! query-stream/release-unused-leases!
        first-lease? (atom true)
        removals (atom 0)
        first-removal (CountDownLatch. 1)
        second-removal (CountDownLatch. 1)
        second-returned (CountDownLatch. 1)
        closing (CountDownLatch. 1)
        finish-close (CountDownLatch. 1)
        projecting (CountDownLatch. 1)
        finish-projection (CountDownLatch. 1)
        replacement-demand (CountDownLatch. 1)
        hub (stream/create (fn [{:keys [model block-projection?]}]
                             (when block-projection?
                               (.countDown projecting)
                               (.await finish-projection 10 TimeUnit/SECONDS))
                             (assoc model :runtime-id "server"))
                           (constantly nil) {:runtime-id "server" :get-graph (constantly graph)})
        server (jetty/run-jetty (wrap-params #(stream/response hub %))
                               {:host "127.0.0.1" :port 0 :join? false})
        open-query! (fn [name]
                      (let [client (connect server (str name "&view=queries"))]
                        (acknowledge client (receive client))
                        client))]
    (try
      (rx/reg-sub! graph (first query)
                   (fn [_ _] (let [generation (swap! mounts inc)] (constantly generation))))
      (stream/publish! hub {:model model})
      (with-redefs [rx/acquire! (fn [system acquired-query on-change]
                                 (let [{:keys [close!] :as lease} (acquire! system acquired-query on-change)]
                                   (if (compare-and-set! first-lease? true false)
                                     (assoc lease :close! (fn []
                                                           (.countDown closing)
                                                           (.await finish-close 10 TimeUnit/SECONDS)
                                                           (close!)))
                                     lease)))
                    query-stream/release-unused-leases!
                    (fn [& args]
                      (case (swap! removals inc)
                        1 (do (.countDown first-removal)
                              (.await second-removal 10 TimeUnit/SECONDS)
                              (apply release! args))
                        2 (do (.countDown second-removal)
                              (.await closing 10 TimeUnit/SECONDS)
                              (try (apply release! args)
                                   (finally (.countDown second-returned))))
                        (apply release! args)))]
        (let [original (open-query! "original")
              other-holder (open-query! "other-holder")
              spectator (connect server "spectator")
              spectator-base (receive spectator)
              existing (set (vals @(:clients hub)))
              replacement (open-query! "replacement")
              replacement-state (first (remove existing (vals @(:clients hub))))]
          (try
            (acknowledge spectator spectator-base)
            (doseq [holder [original other-holder]]
              (send-control holder {:op :queries :queries [query]})
              (let [mounted (receive holder)]
                (is (= 1 (get-in mounted [:view :subscriptions query :value])))
                (acknowledge holder mounted)))
            ;; Make the removing sockets, rather than the delivery worker,
            ;; race to retire the last shared watcher.
            (stream/publish! hub {:model (assoc model :frame-number 1) :block-projection? true})
            (is (.await projecting 3 TimeUnit/SECONDS))
            (send-control original {:op :queries :queries []})
            (is (.await first-removal 3 TimeUnit/SECONDS))
            (send-control other-holder {:op :queries :queries []})
            (is (.await closing 3 TimeUnit/SECONDS))
            (is (.await second-returned 3 TimeUnit/SECONDS))
            (add-watch replacement-state ::mount
                       (fn [_ _ _ next] (when (seq (:queries next)) (.countDown replacement-demand))))
            (send-control replacement {:op :queries :queries [query]})
            (is (.await replacement-demand 3 TimeUnit/SECONDS))
            (.countDown finish-projection)
            (is (= 1 (:frame-number (apply-patch (:view spectator-base) (receive spectator))))
                "A separate socket still receives updates during cleanup")
            (let [early-unsubscribe (.poll ^LinkedBlockingQueue (:messages original) 150 TimeUnit/MILLISECONDS)
                  early-shared-unsubscribe (.poll ^LinkedBlockingQueue (:messages other-holder) 150 TimeUnit/MILLISECONDS)
                  early-remount (.poll ^LinkedBlockingQueue (:messages replacement) 150 TimeUnit/MILLISECONDS)]
              (is (nil? early-unsubscribe) "An empty baseline acknowledges completed retirement")
              (is (nil? early-shared-unsubscribe) "Both removers await their shared query's retirement")
              (is (nil? early-remount) "A remount waits for a fresh graph lease")
              (.countDown finish-close)
              (is (= {} (get-in (or early-unsubscribe (receive original)) [:view :subscriptions])))
              (is (= {} (get-in (or early-shared-unsubscribe (receive other-holder)) [:view :subscriptions])))
              (let [remounted (or early-remount (receive replacement))]
                (is (= :full (:kind remounted)))
                (is (= 2 (get-in remounted [:view :subscriptions query :value])))))
            (finally
              (remove-watch replacement-state ::mount)
              (.countDown finish-projection)
              (.countDown second-removal)
              (.countDown finish-close)
              (doseq [client [original other-holder spectator replacement]] (.abort ^WebSocket (:socket client)))))))
      (finally (stream/close! hub) (.stop server) (rx/close! graph)))))

(deftest live-registration-repairs-a-missing-query-without-reconnecting
  (let [query [:custom/late]
        model {:epoch 1 :show-id :test :settings {:studio {:public-queries [:custom/late]}}}
        graph (rx/create-system model)
        hub (stream/create identity (constantly nil) {:runtime-id "test" :get-graph (constantly graph)})
        server (jetty/run-jetty (wrap-params #(stream/response hub %)) {:host "127.0.0.1" :port 0 :join? false})]
    (try
      (stream/publish! hub {:model model})
      (let [socket (connect server "live&view=queries")]
        (try
          (acknowledge socket (receive socket))
          (send-control socket {:op :queries :queries [query]})
          (let [absent (receive socket)]
            (is (= :unavailable (get-in absent [:view :subscriptions query :error])))
            (acknowledge socket absent)
            (rx/reg-sub! graph (first query) (constantly nil) {:validate number?})
            (stream/publish! hub {:model model})
            ;; A registered source whose initial value fails validation also remains unavailable.
            (let [invalid (receive socket) unavailable (apply-patch (:view absent) invalid)]
              (is (= {:error :unavailable} (get-in unavailable [:subscriptions query])))
              (acknowledge socket invalid)
              (rx/reg-sub! graph (first query) (constantly 42))
              (rx/publish! graph model)
              (stream/publish! hub {:model model})
              (let [updated (receive socket) fixed (apply-patch unavailable updated)]
              (is (= {:value 42} (get-in fixed [:subscriptions query])))
              (acknowledge socket updated)
              ;; Custom hosts can provide projections without the show's configuration admission.
              (stream/publish! hub {:model (assoc-in model [:settings :studio :public-queries] :custom/late)})
              (let [bad (receive socket) denied (apply-patch fixed bad)]
                (is (= {:error :not-exposed} (get-in denied [:subscriptions query])))
                (acknowledge socket bad))
              (send-control socket {:op :client-fault :fault {:kind :event :id "custom-handler" :message "changed form"}})
              (stream/publish! hub {:model model})
              (is (some? (receive socket))))))
          (finally (.abort ^WebSocket (:socket socket)))))
      (finally (stream/close! hub) (.stop server) (rx/close! graph)))))

(defn- next-rdm [client base matches?]
  (loop [previous base attempts 0]
    (when (> attempts 64) (throw (ex-info "RDM feedback did not converge" {})))
    (let [message (receive client)
          view (apply-patch previous message)
          status (get-in view [:subscriptions [:rdm/status] :value])]
      (acknowledge client message)
      (if (and status (matches? status)) [view message] (recur view (inc attempts))))))

(deftest configured-headless-rdm-pushes-direct-gateway-readbacks-and-restores-offline
  (let [{:keys [config requests close!]} (gateway/start!)
        runtime (app/start! (assoc (app/default-config) :web-server false :show {:workspace-options nil}
                                   :rdm (assoc config :enabled true)))
        server (jetty/run-jetty (studio/handler runtime) {:host "127.0.0.1" :port 0 :join? false})
        client (connect server "rig-fixtures&view=queries")
        call! (fn [op body] (:data (rdm-host/request runtime op body true)))
        qpath [:subscriptions [:rdm/status] :value]]
    (try
      (let [initial (receive client)]
        (acknowledge client initial)
        (is (= {} (:subscriptions (:view initial)))))
      (send-control client {:op :queries :queries [[:rdm/status]]})
      (let [[connected _] (next-rdm client {} :connected?)
            session (get-in connected (conj qpath :session-id))
            run! (fn [op body] (call! op (assoc body :session-id session)))
            _ (is (empty? @requests))
            _ (run! :discover {:universe 1 :full? false})
            [inventory diff] (next-rdm client connected #(seq (get-in % [:discovered 1])))
            _ (is (= :patch (:kind diff)))
            _ (is (= [:cached] @requests))
            _ (is (= [(:uid rdm-peer/device)] (get-in inventory (into qpath [:discovered 1]))))
            _ (run! :read rdm-peer/device)
            observed? #(= "Test Lighting" (get-in % [:observations "1/1234:01020304/0" :fields :manufacturer-label :value]))
            [readback _] (next-rdm client inventory observed?)
            source (show/export-show-source runtime)
            prev @requests]
        (is (= :accepted (:status (deref (show/submit! runtime :import {:source source}) 5000 {}))))
        (let [[restored _] (next-rdm client readback #(and (observed? %) (not (:connected? %))))]
          (is (= :art-net (get-in restored (into qpath [:config :transport]))))
          (is (= [] (get-in restored (conj qpath :jobs))))
          (is (nil? (get-in restored (conj qpath :session-id))))
          (is (= prev @requests))))
      (finally (.abort ^WebSocket (:socket client)) (.stop server) (app/stop! runtime) (close!)))))

(deftest completed-browser-readback-is-private-to-its-socket-and-stops-on-last-reader
  (let [runtime (show/create-runtime {:workspace-options nil})
        system (show/reactions runtime)
        server (jetty/run-jetty (studio/handler runtime) {:host "127.0.0.1" :port 0 :join? false})
        source :preview.rig/center-rgb
        query [:studio/observation-demand :rig]
        client (connect server "ipad")
        other (connect server "ipad")
        acquire #(rx/acquire! system [:browser/observation source] identity)
        claim! (fn [client]
                 (acknowledge client (receive client))
                 (send-control client {:op :queries :queries [query]})
                 (let [frame (receive client)]
                   (acknowledge client frame)
                   (:view frame)))
        grants #(get-in % [:subscriptions query :value])
        next-view (fn [client before]
                    (let [frame (receive client)] (acknowledge client frame) (apply-patch before frame)))
        packet (fn [grant rgb]
                 (merge (select-keys grant [:show :token])
                        {:op :observation :query query :source source :sequence 1
                         :frame-number 0 :sampled-ms 500 :completed-ms 501 :value rgb}))]
    (try
      (let [base (claim! client)
            backup (claim! other)
            reader (acquire)]
        (is (= {} (grants base) (grants backup)))
        (let [started (next-view client base) grant (get (grants started) source)]
          (is (= :center-rgb (:sampler grant)))
          (send-control other (packet grant [99 99 99]))
          (send-control other {:op :resync})
          (next-view other backup)
          (rx/publish! system)
          (is (nil? @(:signal reader)) "A second socket cannot borrow the first socket's grant")
          (send-control client (packet grant [12 34 56]))
          ;; A :resync is ordered behind its preceding observation on this same
          ;; socket. The show can stay paused while producers join and leave.
          (send-control client {:op :resync})
          (let [synced (next-view client started)]
            (rx/publish! system)
            (is (= [12 34 56] (:value @(:signal reader))))
            ((:close! reader))
            (let [stopped (next-view client synced) reader (acquire)]
              (is (= {} (grants stopped)))
              (let [restarted (next-view client stopped)]
                (is (not= (:token grant) (get-in (grants restarted) [source :token])))
                (.abort ^WebSocket (:socket client))
                (let [promoted (next-view other backup)
                      next-grant (get (grants promoted) source)]
                  (is (not= (:token grant) (:token next-grant)))
                  (send-control other (packet grant [1 2 3]))
                  (send-control other {:op :resync})
                  (let [fenced (next-view other promoted)]
                    (rx/publish! system)
                    (is (nil? @(:signal reader)))
                    (send-control other (packet next-grant [2 4 8]))
                    (send-control other {:op :resync})
                    (next-view other fenced)
                    (rx/publish! system)
                    (is (= [2 4 8] (:value @(:signal reader))))))
                ((:close! reader)))))))
      (finally (.abort ^WebSocket (:socket client)) (.abort ^WebSocket (:socket other))
               (show/stop! runtime) (.stop server)))))

(deftest workspace-check-status-is-pushed-without-advancing-the-show
  (fixture/with-workspace {}
    (fn [{:keys [root first second]}]
      (let [finish (promise)
            runtime (show/create-runtime
                      {:workspace-options
                       {:repositories {:tolglow (str first) :afterglow (str second)}
                        :workspace-parent (str (.resolve ^java.nio.file.Path root "studio-workspaces"))
                        :check-profiles {:unit {:repo :tolglow :argv ["git" "status" "--porcelain"]}}
                        :runner (fn [request] @finish (workspace/run-trusted-process! request))}})
            manager (get-in runtime [:development :workspaces])
            _ (workspace/create! manager {:id "lighting"})
            server (jetty/run-jetty (studio/handler runtime) {:host "127.0.0.1" :port 0 :join? false})
            client (connect server "workspace&view=queries")
            query [:development/workspace "lighting"]
            cursor [:subscriptions query :value]
            initial-watches (count (.getWatches ^clojure.lang.Atom (:state manager)))]
        (try
          (let [initial (receive client)]
            (is (= 0 (:frame-number (show/snapshot runtime))))
            (is (= 0 (get-in initial [:view :frame-number])))
            (acknowledge client initial))
          (send-control client {:op :queries :queries [query]})
          (let [initial (receive client)
                initial-status (get-in (:view initial) cursor)]
            (is (= "lighting" (:id initial-status)))
            (is (empty? (:running-checks initial-status)))
            (is (= (inc initial-watches) (count (.getWatches ^clojure.lang.Atom (:state manager)))))
            (acknowledge client initial)
            (let [run (workspace/check! manager "lighting" :unit)
                  running (receive client)
                  running-view (apply-patch (:view initial) running)]
              (is (= :running (get-in running-view (into cursor [:running-checks :unit :status]))))
              (acknowledge client running)
              (deliver finish true)
              ;; The check hashes two real Git worktrees before and after its process.
              ;; Start the transport deadline once the status actually exists.
              (is (= :passed (:status (deref (:result run) 30000 nil))))
              (let [done (receive client)
                    done-view (apply-patch running-view done)]
                (is (= :passed (get-in done-view (into cursor [:checks :unit :status]))))
                (is (= (:frame-number (:view initial)) (:frame-number done-view)))
                (is (= (:run-id run) (get-in done-view (into cursor [:checks :unit :run-id]))))
                (acknowledge client done))))
          (send-control client {:op :queries :queries []})
          (let [released (receive client)]
            (is (empty? (get-in released [:view :subscriptions])))
            (is (= initial-watches (count (.getWatches ^clojure.lang.Atom (:state manager))))))
          (finally (deliver finish true) (.abort ^WebSocket (:socket client))
                   (.stop server) (show/stop! runtime)))))))

(deftest output-only-reaches-the-operator-who-requested-that-workspace-and-repository
  (let [owner (show/create-runtime {:workspace-options nil})
        session {:workspace-id "lighting" :repo :tolglow :session-id "one" :state :running
                 :first-cursor 0 :cursor 0 :chunks []}
        sessions (atom {:sessions {"one" session}})
        runtime (assoc-in owner [:development :terminal] {:state sessions})
        server (jetty/run-jetty (studio/handler runtime) {:host "127.0.0.1" :port 0 :join? false})
        operator (connect server "terminal&view=queries")
        spectator (connect server "other-operator&view=queries")
        own [:development/terminal-output "lighting" :tolglow "one" 0]
        other [:development/terminal-output "lighting" :afterglow "one" 0]
        watches #(count (.getWatches ^clojure.lang.Atom sessions))]
    (try
      (doseq [client [operator spectator]] (acknowledge client (receive client)))
      (send-control operator {:op :queries :queries [own]})
      (let [initial (receive operator)]
        (is (= "" (get-in initial [:view :subscriptions own :value :data])))
        (acknowledge operator initial)
        (send-control spectator {:op :queries :queries [other]})
        (let [denied (receive spectator)]
          (is (= {:error :unavailable} (get-in denied [:view :subscriptions other])))
          (acknowledge spectator denied))
        (is (= 1 (watches)))
        (swap! sessions update-in [:sessions "one"] assoc :cursor 1 :chunks [{:start 0 :bytes (byte-array [65])}])
        (let [changed (receive operator)
              view (apply-patch (:view initial) changed)]
          (is (= "QQ==" (get-in view [:subscriptions own :value :data])))
          (is (= 0 (:frame-number view)))
          (acknowledge operator changed))
        (is (silence? spectator)))
      (send-control operator {:op :queries :queries []})
      (acknowledge operator (receive operator))
      (is (zero? (watches)))
      (finally (.abort ^WebSocket (:socket operator)) (.abort ^WebSocket (:socket spectator))
               (.stop server) (show/stop! owner)))))

(defn- primitive-array-paths
  "Paths to non-byte primitive arrays. The codec gives those typed browser
  arrays, while Studio consumers of projected numbers expect vectors."
  [value path]
  (cond (and (some? value) (.isArray (class value)) (not (bytes? value))
             (.isPrimitive (.getComponentType (class value))))
        [path]
        (map? value) (mapcat (fn [[k v]] (primitive-array-paths v (conj path k))) value)
        (coll? value) (mapcat (fn [[i v]] (primitive-array-paths v (conj path i))) (map-indexed vector value))
        :else nil))

(deftest studio-projections-carry-no-primitive-numeric-arrays
  (let [probe (show/create-runtime {:workspace-options nil :workers 1})
        model (try (:model @(:state probe)) (finally (show/stop! probe)))
        runtime (show/create-runtime
                 {:workspace-options nil :workers 1
                  :model (update model :settings merge
                                 {:lighting-capacity {:enabled? true} :lighting-budget {:enabled? true}
                                  :lighting-activity {:enabled? true} :output-response {:enabled? true}})})]
    (try
      (dotimes [i 6] (show/tick! runtime (* 25.0 (inc i))))
      (let [state @(:state runtime)
            stream-view (studio/stream-view runtime state)
            snapshot (show/snapshot runtime)]
        (is (seq (get-in stream-view [:output :values])) "the projection includes rendered fixtures")
        (is (map? (get-in stream-view [:output :lighting-capacity])) "capacity metering ran")
        (is (empty? (primitive-array-paths stream-view [])))
        (is (empty? (primitive-array-paths snapshot [])))
        (is (empty? (primitive-array-paths (studio/browser-snapshot runtime {} (constantly nil)) [])))
        (is (= stream-view (transit/decode (transit/encode stream-view)))))
      (finally (show/stop! runtime)))))

;; Query sampling isolation (#906) ---------------------------------------------

(def ^:private bound-ms
  "Upper bound for any single wait in the sampling tests. Never a threshold."
  10000)

(defn- eventually [description predicate]
  (let [deadline (+ (System/nanoTime) (* 1000000 bound-ms))]
    (loop []
      (or (predicate)
          (if (< (System/nanoTime) deadline)
            (do (Thread/sleep 5) (recur))
            (throw (ex-info (str "Timed out waiting for " description) {})))))))

(defn- live-threads [hub] (get-in (stream/sampling-status hub) [:threads :live]))

(defn- follower
  "Receive and acknowledge a client's messages, as Studio does, until done?
  holds for its applied view. Returns that view."
  [client]
  (let [view (volatile! nil)]
    (fn [done?]
      (loop [n 0]
        (let [message (receive client)]
          (acknowledge client message)
          (let [applied (vswap! view apply-patch message)]
            (cond (done? applied) applied
                  (< n 400) (recur (inc n))
                  :else (throw (ex-info "The expected view never arrived" {:view applied})))))))))

(defn- frame-of [committed] (get-in committed [:model :frame-number]))

(deftest a-blocked-sampler-delays-only-the-clients-that-lease-its-query
  ;; reader leases a slow and a fast surface, other only the fast one and the
  ;; spectator none. The slow sampler blocks from frame 1 until released.
  (let [block-from (atom Long/MAX_VALUE)
        entered (promise) release (promise)
        running (AtomicInteger.) overlaps (atom 0) slow-frames (atom []) closes (atom 0)
        hub (stream/create (fn [{:keys [model]}] (assoc model :runtime-id "server")) (constantly nil)
                           {:runtime-id "server" :get-graph (constantly nil)
                            :surfaces {::slow (fn [_]
                                                {:sample! (fn [committed]
                                                            (when (> (.incrementAndGet running) 1) (swap! overlaps inc))
                                                            (try
                                                              (swap! slow-frames conj (frame-of committed))
                                                              (when (>= (frame-of committed) @block-from)
                                                                (deliver entered true)
                                                                (deref release bound-ms nil))
                                                              {:frame (frame-of committed)}
                                                              (finally (.decrementAndGet running))))
                                                 :close! #(swap! closes inc)})
                                       ::fast (fn [_] {:sample! (fn [committed] {:frame (frame-of committed)})})}})
        server (jetty/run-jetty (wrap-params #(stream/response hub %)) {:host "127.0.0.1" :port 0 :join? false})
        model {:show-id :fixture-show :epoch 0 :revision 0 :frame-number 0}
        publish! #(stream/publish! hub {:model (assoc model :frame-number %)})
        slow #(get-in % [:subscriptions [::slow]])
        fast #(get-in % [:subscriptions [::fast]])]
    (try
      (publish! 0)
      (let [reader (connect server "reader") other (connect server "other") spectator (connect server "spectator")
            [follow-reader follow-other follow-spectator] (map follower [reader other spectator])]
        (try
          (doseq [follow [follow-reader follow-other follow-spectator]] (follow some?))
          (send-control reader {:op :queries :queries [[::slow] [::fast]]})
          (send-control other {:op :queries :queries [[::fast]]})
          (follow-reader #(and (= {:value {:frame 0}} (slow %)) (= {:value {:frame 0}} (fast %))))
          (follow-other #(= {:value {:frame 0}} (fast %)))
          (reset! block-from 1)
          (publish! 1)
          (is (deref entered bound-ms false) "The slow sampler blocks on frame 1")
          (doseq [frame (range 2 21)] (publish! frame) (Thread/sleep 5))
          (testing "clients without the blocked query keep receiving frames"
            (is (= 20 (:frame-number (follow-spectator #(= 20 (:frame-number %))))))
            (is (= {:value {:frame 20}} (fast (follow-other #(= {:value {:frame 20}} (fast %)))))))
          (testing "its client gets the other fields and a stale marker for the blocked query"
            (let [view (follow-reader #(and (= 20 (:frame-number %)) (= {:value {:frame 20}} (fast %))))]
              (is (= {:value {:frame 0} :stale {:reason :late :frame-number 0}} (slow view)))))
          (is (= 1 (.get running)) "The sampler is still blocked")
          (let [{:keys [totals queries threads]} (stream/sampling-status hub)]
            (is (= {:reason :late :frame-number 0} (get-in queries [[::slow] :stale])))
            (is (true? (get-in queries [[::slow] :in-flight?])))
            (is (pos? (:late totals)) "The overrun is counted late")
            (is (pos? (:skipped totals)) "Frames committed while it was blocked are counted skipped")
            (is (<= 1 (:live threads) (:max threads))))
          (deliver release true)
          (testing "the released query recovers to the newest frame"
            (is (= {:value {:frame 20}} (slow (follow-reader #(= {:value {:frame 20}} (slow %))))))
            (is (not (contains? (get-in (stream/sampling-status hub) [:queries [::slow]]) :stale))
                "The REPL drops the marker once the fresh result is delivered"))
          (is (zero? @overlaps) "A lease never samples concurrently with itself")
          (is (apply distinct? @slow-frames) "Each committed source is sampled at most once")
          (is (= [0 1 20] @slow-frames) "Frames committed while it was blocked were skipped, not queued")
          (finally
            (doseq [client [reader other spectator]] (.abort ^WebSocket (:socket client))))))
      (finally
        (deliver release true)
        (stream/close! hub)
        (.stop server)))
    (is (= 1 @closes) "The slow lease closes once")
    (eventually "this stream's sampler threads to exit" #(zero? (live-threads hub)))))

(defn- blocking-surface
  "A surface whose samples report :entered and block until release."
  [entered release & {:keys [group closes]}]
  (fn [[_ i]]
    (cond-> {:sample! (fn [committed]
                        (.incrementAndGet ^AtomicInteger entered)
                        (deref release bound-ms nil)
                        {:query i :frame (frame-of committed)})}
      group (assoc :sample-group group)
      closes (assoc :close! #(swap! closes inc)))))

(deftest blocked-samplers-hold-a-bounded-number-of-threads
  ;; Three surfaces with four blocked queries each. Each surface may hold four
  ;; threads and the pool eight, so the third surface's queries are refused and
  ;; marked :busy. No sample is queued behind them. The reader then drops one
  ;; busy and one blocked query and keeps receiving frames.
  (let [release (promise) entered (AtomicInteger.) closes (atom 0)
        surfaces [::blocking-a ::blocking-b ::blocking-c]
        queries (vec (for [surface surfaces i (range 4)] [surface i]))
        hub (stream/create (fn [{:keys [model]}] (assoc model :runtime-id "server")) (constantly nil)
                           {:runtime-id "server" :get-graph (constantly nil)
                            :surfaces (zipmap surfaces (repeat (blocking-surface entered release :closes closes)))})
        server (jetty/run-jetty (wrap-params #(stream/response hub %)) {:host "127.0.0.1" :port 0 :join? false})
        model {:show-id :fixture-show :epoch 0 :revision 0 :frame-number 0}
        publish! #(stream/publish! hub {:model (assoc model :frame-number %)})]
    (try
      (publish! 0)
      (let [reader (connect server "reader&view=queries") spectator (connect server "spectator")
            [follow-reader follow-spectator] (map follower [reader spectator])]
        (try
          (follow-reader some?)
          (follow-spectator some?)
          (send-control reader {:op :queries :queries queries})
          (let [view (follow-reader #(= 12 (count (:subscriptions %))))
                reasons (frequencies (map (comp :reason :stale) (vals (:subscriptions view))))]
            (is (= {:late 8 :busy 4} reasons))
            (is (every? #(= :unavailable (:error %)) (vals (:subscriptions view)))
                "Nothing has completed, so each query is unavailable"))
          (eventually "eight samplers to enter" #(= 8 (.get entered)))
          (doseq [frame (range 1 6)] (publish! frame) (Thread/sleep 5))
          (is (= 5 (:frame-number (follow-spectator #(= 5 (:frame-number %))))))
          (let [{:keys [threads totals]} (stream/sampling-status hub)]
            (is (= 8 (:live threads) (:max threads)))
            (is (<= 4 (:busy totals))))
          (let [status (:queries (stream/sampling-status hub))
                busy (first (filter #(= :busy (get-in status [% :stale :reason])) queries))
                blocked (first (filter #(get-in status [% :in-flight?]) queries))
                kept (vec (remove #{busy blocked} queries))]
            (send-control reader {:op :queries :queries kept})
            (doseq [frame (range 6 9)] (publish! frame) (Thread/sleep 5))
            (testing "dropping a refused and a blocked query does not hold the reader"
              (let [view (follow-reader #(and (= 10 (count (:subscriptions %))) (= 8 (:frame-number %))))]
                (is (= (set kept) (set (keys (:subscriptions view)))))))
            (is (= 1 @closes) "The refused lease retired and closed at once")
            (is (= #{blocked} (:closing (stream/sampling-status hub))) "The blocked lease closes after its sample"))
          (deliver release true)
          (let [view (follow-reader (fn [v] (every? #(= 8 (get-in % [:value :frame])) (vals (:subscriptions v)))))]
            (is (= 10 (count (:subscriptions view))))
            (is (not-any? :stale (vals (:subscriptions view)))))
          (eventually "the blocked lease to close"
                      #(and (= 2 @closes) (empty? (:closing (stream/sampling-status hub)))))
          (finally
            (doseq [client [reader spectator]] (.abort ^WebSocket (:socket client))))))
      (finally
        (deliver release true)
        (stream/close! hub)
        (.stop server)))
    (eventually "this stream's sampler threads to exit" #(zero? (live-threads hub)))))

(deftest a-client-that-drops-a-blocked-query-keeps-receiving-frames
  ;; The dropped lease is fenced and detached at once; only its close waits
  ;; for the blocked sample, on the sampling thread.
  (let [release (promise) entered (promise) closes (atom 0) detaches (atom 0)
        observed (atom 0)
        hub (stream/create (fn [{:keys [model]}] (assoc model :runtime-id "server")) (constantly nil)
                           {:runtime-id "server" :get-graph (constantly nil)
                            :surfaces {::slow (fn [_]
                                                {:sample! (fn [committed]
                                                            (when (pos? (frame-of committed))
                                                              (deliver entered true)
                                                              (deref release bound-ms nil))
                                                            {:frame (frame-of committed)})
                                                 :observe! (fn [_] (swap! observed inc) #(swap! detaches inc))
                                                 :close! #(swap! closes inc)})}})
        server (jetty/run-jetty (wrap-params #(stream/response hub %)) {:host "127.0.0.1" :port 0 :join? false})
        model {:show-id :fixture-show :epoch 0 :revision 0 :frame-number 0}
        publish! #(stream/publish! hub {:model (assoc model :frame-number %)})]
    (try
      (publish! 0)
      (let [reader (connect server "reader") follow (follower reader)]
        (try
          (follow some?)
          (send-control reader {:op :queries :queries [[::slow]]})
          (follow #(= {:value {:frame 0}} (get-in % [:subscriptions [::slow]])))
          (publish! 1)
          (is (deref entered bound-ms false))
          (follow #(= 1 (:frame-number %)))
          (send-control reader {:op :queries :queries []})
          (doseq [frame (range 2 6)] (publish! frame) (Thread/sleep 5))
          (let [view (follow #(= 5 (:frame-number %)))]
            (is (empty? (:subscriptions view))))
          (is (= [1 1 0] [@observed @detaches @closes]) "Detached at once; the close waits for the sample")
          (is (= #{[::slow]} (:closing (stream/sampling-status hub))))
          (deliver release true)
          (eventually "the dropped lease to close"
                      #(and (= 1 @closes) (empty? (:closing (stream/sampling-status hub)))))
          (finally (.abort ^WebSocket (:socket reader)))))
      (finally
        (deliver release true)
        (stream/close! hub)
        (.stop server)))
    (is (= 1 @detaches @closes))))

(defn- refusing-store
  "A store whose sampler, once asked for during a sample start, lets another
  thread retire [::s] (as a socket that drops it would) and then hands back
  pool. The retirement may wait for the start, but at most 200 ms."
  [store pool]
  (assoc store :sampler (reify clojure.lang.IDeref
                          (deref [_]
                            (deref (future (query-stream/release-unused-leases! store (constantly #{}) [[::s]]))
                                   200 nil)
                            pool))))

(deftest a-refused-sample-start-never-loses-a-retirement
  (doseq [[label pool! release!]
          [["closed pool" #(doto (java.util.concurrent.Executors/newSingleThreadExecutor) (.shutdown)) (fn [_])]
           ["full pool" #(let [blocked (promise)
                               pool (java.util.concurrent.ThreadPoolExecutor.
                                     0 1 1 TimeUnit/SECONDS (java.util.concurrent.SynchronousQueue.))]
                           (.execute pool ^Runnable (fn [] (deref blocked bound-ms nil)))
                           [pool blocked])
            (fn [[_ blocked]] (deliver blocked true))]]]
    (testing label
      (let [closes (atom 0) detaches (atom 0)
            base (query-stream/create (constantly nil) (fn [])
                                      {::s (fn [_] {:sample! (fn [source] {:n (:n source)})
                                                    :observe! (fn [_] #(swap! detaches inc))
                                                    :close! #(swap! closes inc)})})
            source {:model {:frame-number 0} :n 0}
            lease (get (query-stream/reconcile! base source #{[::s]}) [::s])
            made (pool!)
            pool (if (vector? made) (first made) made)]
        (try
          (let [entries (query-stream/sample! (refusing-store base pool) {[::s] lease} {:model {:frame-number 1} :n 1})]
            (is (nil? (:flight (get entries [::s]))) "The start was refused")
            (eventually "the retirement to finish" #(= 1 @closes))
            (is (= 1 @detaches))
            (is (empty? (query-stream/retiring-queries base)))
            (is (empty? (:closing (query-stream/sampling-status base)))))
          (finally (release! made) (.shutdown ^java.util.concurrent.ExecutorService pool)
                   (query-stream/close! base false)))))))

(deftest grouped-and-capped-surfaces-leave-the-pool-to-the-others
  ;; Pad-like leases share a :sample-group: a blocked one holds one thread and
  ;; the rest queue behind it in pass order. An ungrouped surface holds at most
  ;; four threads. A timeline-like surface still samples on time.
  (let [release (promise) entered (AtomicInteger.) order (atom [])
        pads (fn [[_ i]] {:sample-group ::pad-service
                          :sample! (fn [committed]
                                     (swap! order conj [i (frame-of committed)])
                                     (when (and (zero? i) (= 1 (frame-of committed))) (deref release bound-ms nil))
                                     {:pad i :frame (frame-of committed)})})
        store (query-stream/create (constantly nil) (fn [])
                                   {::pads pads
                                    ::stuck (blocking-surface entered release)
                                    ::timeline (fn [_] {:sample! (fn [committed] {:frame (frame-of committed)})})}
                                   {:sample-wait-ms 100})
        pad-queries (mapv #(vector ::pads %) (range 5))
        stuck-queries (mapv #(vector ::stuck %) (range 6))
        all (into (conj pad-queries [::timeline]) stuck-queries)
        sources (mapv (fn [n] {:model {:frame-number n}}) (range 2))
        source #(nth sources %)
        leases (query-stream/reconcile! store (source 0) (set all))
        pass! (fn [n] (let [entries (query-stream/sample! store leases (source n))
                            served (atom {})]
                        (query-stream/serve-when-sampled! store entries [[:all (vec (keys entries))]]
                                                          (fn [_] (reset! served (into {} (map (fn [[q e]] [q (query-stream/result e)])) entries))))
                        @served))]
    (try
      (is (= {:frame 0} (:value (get (pass! 0) [::timeline])))
          "Before anything blocks, the first pass samples pads and the timeline")
      (eventually "four stuck samplers to enter" #(= 4 (.get entered)))
      (let [results (pass! 1)
            {:keys [threads queries]} (query-stream/sampling-status store)]
        (is (= {:value {:frame 1}} (get results [::timeline])) "The timeline is never refused")
        (is (= {:late 4 :busy 2} (frequencies (map #(get-in results [% :stale :reason]) stuck-queries)))
            "Only the surface's own queries past its four threads are refused")
        (is (<= (:live threads) 6) "The stuck surface holds four threads and the pads one, plus the timeline's")
        (is (not-any? #(= :busy (get-in results [% :stale :reason])) pad-queries) "No pad is refused")
        (is (= :late (get-in results [[::pads 0] :stale :reason])) "The blocked pad is late")
        (is (every? #(or (= {:pad (second %) :frame 1} (:value (get results %)))
                         (and (get-in queries [% :in-flight?]) (= :late (get-in results [% :stale :reason]))))
                    pad-queries)
            "Every other pad either sampled before it or is queued behind it"))
      (deliver release true)
      (eventually "the queued pads to finish" #(every? (fn [q] (= {:pad (second q) :frame 1}
                                                                (:value (query-stream/read-result store (get leases q) (source 1) 0))))
                                                       pad-queries))
      (let [frames (mapv second @order)]
        (is (= frames (sort frames)) "The pad service sees frames in pass order"))
      (finally
        (deliver release true)
        (query-stream/close! store false)))))

(deftest a-waiting-client-is-served-when-its-own-samples-finish
  (let [release (promise)
        store (query-stream/create (constantly nil) (fn [])
                                   {::slow (fn [_] {:sample! (fn [c] (when (= 1 (frame-of c)) (deref release bound-ms nil))
                                                               {:frame (frame-of c)})})
                                    ::fast (fn [_] {:sample! (fn [c] {:frame (frame-of c)})})}
                                   {:sample-wait-ms 3000})
        source (fn [n] {:model {:frame-number n}})
        leases (query-stream/reconcile! store (source 0) #{[::slow] [::fast]})
        pass! (fn [n] (let [entries (query-stream/sample! store leases (source n))
                            started (System/nanoTime) served (atom [])]
                        (future (Thread/sleep 300) (deliver release true))
                        (query-stream/serve-when-sampled!
                         store entries [[:slow-reader [[::slow] [::fast]]] [:fast-reader [[::fast]]]]
                         #(swap! served conj [% (quot (- (System/nanoTime) started) 1000000)]))
                        [entries @served]))]
    (try
      (query-stream/read-result store (get leases [::slow]) (source 0))
      (let [[_ served] (pass! 1)]
        (is (= [:fast-reader :slow-reader] (map first served)))
        (is (< (second (first served)) 250) "The fast reader does not wait for the slow sample"))
      (finally (deliver release true) (query-stream/close! store false)))))

(deftest a-late-lease-is-awaited-again-after-several-on-time-samples
  (let [slow? (atom true)
        store (query-stream/create (constantly nil) (fn [])
                                   {::s (fn [_] {:sample! (fn [c] (when @slow? (Thread/sleep 80)) {:frame (frame-of c)})})}
                                   {:sample-wait-ms 20})
        source (fn [n] {:model {:frame-number n}})
        lease (get (query-stream/reconcile! store (source 0) #{[::s]}) [::s])
        pass! (fn [n] (let [src (source n)
                            entries (query-stream/sample! store {[::s] lease} src)
                            awaited? (:await? (get entries [::s]))]
                        (query-stream/serve-when-sampled! store entries [[:x [[::s]]]] (fn [_]))
                        (query-stream/read-result store lease src)
                        awaited?))]
    (try
      (is (true? (pass! 0)) "A new lease is awaited")
      (reset! slow? false)
      (is (= [false false false true] (mapv pass! [1 2 3 4]))
          "After its overrun, three on-time samples pass before it is awaited again")
      (is (= 1 (get-in (query-stream/sampling-status store) [:totals :late])))
      (finally (query-stream/close! store false)))))

(deftest read-result-waits-behind-a-running-sample
  (let [release (promise)
        store (query-stream/create (constantly nil) (fn [])
                                   {::s (fn [_] {:sample! (fn [c] (when (= 1 (frame-of c)) (deref release bound-ms nil))
                                                            {:frame (frame-of c)})})})
        one {:model {:frame-number 1}}
        lease (get (query-stream/reconcile! store {:model {:frame-number 0}} #{[::s]}) [::s])]
    (try
      (is (some? (:flight (get (query-stream/sample! store {[::s] lease} one) [::s]))))
      (let [read (future (query-stream/read-result store lease one))]
        (Thread/sleep 50)
        (is (not (realized? read)) "It waits for the running sample of that source")
        (deliver release true)
        (is (= {:value {:frame 1}} (deref read bound-ms :timed-out))))
      (finally (deliver release true) (query-stream/close! store false)))))
