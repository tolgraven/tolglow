(ns tolglow.cue-timing-host-test
  (:require [afterglow.sync :as sync]
            [afterglow.sync.session :as session]
            [clojure.data.json :as json]
            [clojure.test :refer [deftest is]]
            [tolglow.connections :as connections]
            [tolglow.demo :as demo]
            [tolglow.integrations.live-sync :as live-sync]
            [tolglow.show :as show]
            [tolglow.studio.web :as web]
            [tolglow.transit :as transit])
  (:import (java.io ByteArrayInputStream)))

(defn- apply-at! [runtime type payload time-ms]
  (let [reply (show/submit! runtime type payload)]
    (show/tick! runtime time-ms)
    (deref reply 1000 ::missing)))

(defn- launch [id cue-id grid]
  {:command {:op :cue/launch :command-id id :cue-id cue-id
             :source {:adapter :repl :session-id "grid-test"}}
   :grid grid})

(defn- model [runtime] (:model @(:state runtime)))

(deftest live-grid-keeps-raw-and-actual-timing-and-launch-order-through-a-tempo-change
  (let [runtime (show/create-runtime {:workspace-options nil})
        ember (assoc (launch "ember" :ember {:beats 1}) :raw {:time-ms 110 :beat 0.22})
        ice (launch "ice" :ice {:beats 1})
        held (assoc-in (launch "held" :ember {:beats 1}) [:command :launch] {:mode :held})]
    (try
      (let [submissions (mapv #(show/submit! runtime :cue-schedule %) [ember ice held])]
        (show/tick! runtime 125)
        (is (= [:pending :pending :pending] (mapv #(:status (deref % 1000 {})) submissions)))
        (is (= 1 (:due-beat (show/quantized-cue-status runtime "ember"))))
        (is (true? (:duplicate? (apply-at! runtime :cue-schedule ember 125))))
        (is (= :command-id-conflict
               (:reason (apply-at! runtime :cue-schedule (assoc ember :grid {:beats 1/2}) 125))))
        (is (= :command-id-conflict
               (:reason (apply-at! runtime :command (:command ember) 125))))
        (is (= :command-id-conflict
               (:reason (apply-at! runtime :command {:op :master/set :value 0.5 :command-id "ember"} 125))))
        (is (empty? (:instances (model runtime))))
        (is (= :accepted (:status (apply-at! runtime :command {:op :transport/set :transport {:bpm 60}} 250))))
        (is (= :cancelled
               (:status (apply-at! runtime :cue-cancel {:command-id "held" :reason :held-release} 750))))
        (let [[a b] (mapv #(show/quantized-cue-status runtime %) ["ember" "ice"])
              instances (sort-by :order (vals (:instances (model runtime))))]
          (is (= [:fired :fired] (mapv :status [a b])))
          (is (= [:ember :ice] (mapv :cue-id instances)))
          (is (= (mapv :id instances) (mapv :instance-id [a b])))
          (is (= [750 750] (mapv :started-at-ms instances)))
          (is (= [1.0 1.0] (mapv :started-at-beat instances)))
          (is (= {:time-ms 110 :beat 0.22} (:raw a)))
          (is (= {:time-ms 125 :beat 0.25} (:requested-at a)))
          (is (= {:time-ms 750 :beat 1.0} (:admitted-at a)))
          (is (= (select-keys a [:raw :requested-at :grid :due-beat :dispatched-at :admitted-at])
                 (get-in (model runtime) [:control :history "ember" :command :cue-timing])))
          (is (= :fired (:status (apply-at! runtime :cue-schedule ember 800))))
          (is (= 2 (count (:instances (model runtime)))))))
      (finally (show/stop! runtime)))))

(deftest idle-transport-and-session-changes-fence-pending-launches
  (let [runtime (show/create-runtime {:workspace-options nil})]
    (try
      (is (= :accepted (:status (apply-at! runtime :command {:op :transport/set :transport {:playing? false}} 250))))
      (is (= :pending (:status (apply-at! runtime :cue-schedule (launch "paused" :ember {:beats 1/2}) 250))))
      (show/tick! runtime 700)
      (is (empty? (:instances (model runtime))))
      (is (= :pending (:status (show/quantized-cue-status runtime "paused"))))
      (is (= :accepted (:status (apply-at! runtime :command {:op :transport/set :transport {:playing? true}} 700))))
      (is (= {:time-ms 700 :beat 0.5} (:admitted-at (show/quantized-cue-status runtime "paused"))))
      (is (= :pending (:status (apply-at! runtime :cue-schedule (launch "restart" :ice {:bars 1}) 750))))
      (is (= :accepted (:status (apply-at! runtime :command {:op :show/restart} 800))))
      (is (= {:status :missed :reason :clock-changed}
             (select-keys (show/quantized-cue-status runtime "restart") [:status :reason])))
      (show/tick! runtime 4000)
      (is (empty? (:instances (model runtime))))
      (finally (show/stop! runtime)))))

(deftest immediate-and-missed-receipts-use-the-usual-launch-owner
  (let [runtime (show/create-runtime {:workspace-options nil})
        bad (assoc-in (launch "bad" :missing {:beats 1}) [:command :source]
                      {:adapter "web" :session-id "studio" :internal "unprojected"})]
    (try
      (is (= :fired (:status (apply-at! runtime :cue-schedule (launch "now" :ember :off) 100))))
      (is (= :command-id-conflict
             (:reason (apply-at! runtime :command (:command (launch "now" :ember :off)) 100))))
      (is (= :pending (:status (apply-at! runtime :cue-schedule bad 110))))
      (show/tick! runtime 500)
      (is (= {:status :missed :reason :launch-rejected :attempted-at {:time-ms 500 :beat 1.0}}
             (select-keys (show/quantized-cue-status runtime "bad") [:status :reason :attempted-at])))
      (is (= [{:command-id "bad" :cue-id :missing :status :missed :reason :launch-rejected
               :source {:adapter :web :session-id "studio"}}]
             (mapv #(dissoc % :code) (:cue-launch-outcomes (show/snapshot runtime)))))
      (is (= 1 (count (:instances (model runtime)))))
      (finally (show/stop! runtime)))))

(deftest restoring-a-show-does-not-revive-the-live-launch-queue
  (let [runtime (show/create-runtime {:workspace-options nil})]
    (try
      (is (= :pending (:status (apply-at! runtime :cue-schedule (launch "restore" :ember {:bars 1}) 100))))
      (let [saved (show/export-show runtime)]
        (is (nil? (get-in saved [:model :control :tolglow.cue-timing/queue])))
        (is (= :accepted (:status (apply-at! runtime :import {:source (show/export-show-source runtime)} 120))))
        (is (nil? (show/quantized-cue-status runtime "restore")))
        (show/tick! runtime 4000)
        (is (empty? (:instances (model runtime)))))
      (finally (show/stop! runtime)))))

(defn- studio-command [app mime command]
  (let [encode (if (= mime "application/json") json/write-str transit/encode)
        response (app {:request-method :post :uri "/api/command"
                       :headers {"host" "localhost" "content-type" mime
                                 "x-tolglow-form" "studio-v1" "accept" "application/transit+json"}
                       :body (ByteArrayInputStream. (.getBytes ^String (encode command) "UTF-8"))})]
    [(:status response) (transit/decode (:body response))]))

(deftest studio-transit-admission-and-json-release-share-the-same-owner
  (let [runtime (show/create-runtime {:workspace-options nil})
        now (atom 100)
        studio (web/app {:heartbeat! #(show/heartbeat! runtime %)
                         :submit! (fn [operation payload]
                                   (let [reply (show/submit! runtime operation payload)]
                                     (show/tick! runtime @now)
                                     reply))})
        source {:adapter "web" :session-id "ipad"}
        held (fn [id] {:op "cue/launch" :cue-id "ember" :grid {:beats 1}
                       :launch {:mode "held"} :gesture-id id :command-id id :source source})
        release (fn [id] {:op "cue/cancel-launch" :launch-command-id id :reason "held-release"
                          :command-id (str id "-release") :source source})
        send-held #(studio-command studio "application/transit+json" (held %))
        send-release #(studio-command studio "application/json" (release %))]
    (try
      (is (= :cancelled (get-in (send-release "early") [1 :status])))
      (is (= :cancelled (get-in (send-held "early") [1 :status])))
      (is (empty? (:pending-cue-launches (show/snapshot runtime))))
      (is (= :pending (get-in (send-held "later") [1 :status])))
      (is (= :stale-epoch (get-in (studio-command studio "application/json"
                                               (assoc (release "later") :epoch -1)) [1 :reason])))
      (is (= [{:command-id "later" :cue-id :ember :mode :held :due-beat 1.0 :source source}]
             (mapv #(dissoc % :requested-at) (:pending-cue-launches (show/snapshot runtime)))))
      (is (= 422 (first (studio-command studio "application/json"
                                       (assoc-in (release "later") [:source :session-id] "other")))))
      (show/tick! runtime 500)
      (let [id (:instance-id (show/quantized-cue-status runtime "later"))]
        (is (= :ember (get-in (model runtime) [:instances id :cue-id])))
        (is (empty? (:pending-cue-launches (show/snapshot runtime))))
        (reset! now 501)
        (is (= :accepted (get-in (send-release "later") [1 :status])))
        (is (not= :running (get-in (model runtime) [:instances id :phase])))
        (is (= :fired (get-in (send-release "later") [1 :status]))))
      (finally (show/stop! runtime)))))

(deftest a-pending-hold-cannot-fire-after-its-web-lease-expires
  (let [runtime (show/create-runtime {:workspace-options nil})
        intent (assoc (launch "gone" :ember {:bars 16})
                      :command {:op :cue/launch :cue-id :ember :command-id "gone"
                                :source {:adapter :web :session-id "gone"} :launch {:mode :held}})]
    (try
      (is (= :pending (:status (apply-at! runtime :cue-schedule intent 100))))
      (show/tick! runtime 10101)
      (is (= {:status :cancelled :reason :lease-expired}
             (select-keys (show/quantized-cue-status runtime "gone") [:status :reason])))
      (show/tick! runtime 33000)
      (is (empty? (:instances (model runtime))))
      (finally (show/stop! runtime)))))

(deftest held-release-is-admitted-even-when-regular-owner-commands-fill-the-queue
  (let [runtime (show/create-runtime {:workspace-options nil})
        source {:adapter :push-2 :session-id "test"}
        down {:op :cue/launch :cue-id :ember :command-id "press" :gesture-id "press"
              :grid {:beats 1} :launch {:mode :held} :source source}
        up {:op :cue/cancel-launch :command-id "release" :launch-command-id "press"
            :reason :held-release :source source}]
    (try
      (is (= :pending (:status (apply-at! runtime :command down 100))))
      (dotimes [i 256]
        (show/submit! runtime {:op :master/set :value 1 :command-id (str "ordinary-" i)}))
      (is (= :rejected (:status (deref (show/submit! runtime {:op :master/set :value 1}) 1000 nil))))
      (let [reply (show/submit! runtime up)]
        (is (not (realized? reply)))
        (show/tick! runtime 500)
        (is (= :cancelled (:status (deref reply 1000 nil))))
        (is (empty? (:instances (model runtime)))))
      (finally (show/stop! runtime)))))

(deftest an-earlier-release-wins-across-the-two-ingress-queues
  (let [runtime (show/create-runtime {:workspace-options nil})
        source {:adapter :web :session-id "main"}
        up {:op :cue/cancel-launch :command-id "release-first" :launch-command-id "down"
            :reason :held-release :source source}
        down {:op :cue/launch :cue-id :ember :command-id "down" :gesture-id "down"
              :grid :off :launch {:mode :held} :source source}]
    (try
      (let [release (show/submit! runtime up)
            press (show/submit! runtime down)]
        (show/tick! runtime 500)
        (is (= :cancelled (:status (deref release 1000 nil))))
        (is (= :cancelled (:status (deref press 1000 nil))))
        (is (empty? (:instances (model runtime)))))
      (finally (show/stop! runtime)))))

(deftest clock-selection-round-trip-invalidates-the-old-queue
  (let [runtime (show/create-runtime {:workspace-options nil})
        manager (connections/sync (:connections runtime))
        intent (assoc-in (launch "old" :ember {:beats 1}) [:command :source] :repl)]
    (try
      (is (= :pending (:status (apply-at! runtime :cue-schedule intent 100))))
      (is (= :repl (get-in (show/snapshot runtime) [:pending-cue-launches 0 :source])))
      (sync/select-source! manager :midi-clock)
      (sync/select-source! manager :manual)
      (show/tick! runtime 500)
      (is (= :missed (:status (show/quantized-cue-status runtime "old"))))
      (is (empty? (:instances (model runtime))))
      (finally (show/stop! runtime)))))

(deftest an-explicit-forward-transport-jump-misses-the-old-boundary
  (let [runtime (show/create-runtime {:workspace-options nil})
        manager (connections/sync (:connections runtime))
        cue (assoc-in (launch "seek" :ember {:beats 1}) [:command :source]
                      {:adapter :web :session-id "touch"})
        clock! (fn [position at discontinuity?]
                 (session/apply-clock! manager nil {:type :clock :source-id :midi-clock :time-ms at
                                                    :beat position :bpm 120 :playing? true :confidence 1
                                                    :discontinuity? discontinuity?}))]
    (try
      (sync/select-source! manager :midi-clock)
      (clock! 0.25 125 false)
      (is (= :pending (:status (apply-at! runtime :cue-schedule cue 125))))
      (clock! 32 150 true)
      (show/tick! runtime 150)
      (is (= :missed (:status (show/quantized-cue-status runtime "seek"))))
      (is (= [{:command-id "seek" :cue-id :ember :status :missed :reason :clock-changed
               :source {:adapter :web :session-id "touch"}}]
             (:cue-launch-outcomes (show/snapshot runtime))))
      (is (empty? (:instances (model runtime))))
      (finally (show/stop! runtime)))))

(deftest one-clock-fence-projects-every-pending-miss-in-order
  (let [runtime (show/create-runtime {:workspace-options nil})
        manager (connections/sync (:connections runtime))
        ids (mapv #(str "pending-" %) (range 128))
        source {:adapter :web :session-id "queue-test"}]
    (try
      (let [replies (mapv #(show/submit! runtime {:op :cue/launch :cue-id :ember :command-id %
                                                 :grid {:bars 1} :source source}) ids)]
        (show/tick! runtime 125)
        (is (every? #(= :pending (:status (deref % 1000 nil))) replies))
        (sync/select-source! manager :midi-clock)
        (show/tick! runtime 150)
        (let [snapshot (show/snapshot runtime)
              missed (:cue-launch-outcomes snapshot)]
          (is (empty? (:pending-cue-launches snapshot)))
          (is (= ids (mapv :command-id missed)))
          (is (every? #(= :clock-changed (:reason %)) missed))))
      (finally (show/stop! runtime)))))

(deftest live-recording-observes-each-fire-even-when-retry-history-evicts-it
  (let [runtime (show/create-runtime {:workspace-options nil
                                     :model (assoc-in (demo/model) [:control :history-limit] 1)})
        recorded (atom [])
        acks (atom [])
        note (fn [intent note]
               (assoc-in intent [:command :source] {:type :ableton :note note :sync-run-id "injected"}))]
    (try
      (with-redefs [live-sync/record! (fn [_ command ack] (swap! recorded conj [command ack]))
                    live-sync/acknowledge! (fn [_ note ack _] (swap! acks conj [note ack]))]
        (doseq [[id cue value] [["a" :ember 36] ["b" :ice 37]]]
          (let [intent (note (launch id cue {:beats 1}) value)]
            (is (= :pending (:status (apply-at! runtime :command
                                                (assoc (:command intent) :grid (:grid intent)) 125))))))
        (show/tick! runtime 500)
        (is (= ["b"] (get-in (model runtime) [:control :history-order])))
        (is (= ["a" "b"] (mapv (comp :command-id first) @recorded)))
        (is (= [[36 :pending] [37 :pending] [36 :accepted] [37 :accepted]]
               (mapv (fn [[note ack]] [note (:status ack)]) @acks)))
        (is (= [1 1] (mapv #(get-in % [0 :cue-timing :due-beat]) @recorded)))
        (is (= [500 500] (mapv #(get-in % [0 :cue-timing :admitted-at :time-ms]) @recorded))))
      (finally (show/stop! runtime)))))

(deftest midi-only-push-3-reaches-its-hold-deadline-without-a-host-schedule
  ;; #694: no :display and no :schedule!, through the show host on the wall
  ;; clock. Demo cues are expressive, so the gesture modifier makes the pad wait.
  ;; The session stamps input and deadline checks with the show's device clock,
  ;; the later of the committed frame time and the live clock. A frame is
  ;; committed at its output deadline, so this clock can lead wall time by up to
  ;; one output period, and the lead differs between press and refresh. The hold
  ;; is 100 ms on that clock, which is what the bounds below compare.
  (let [runtime (show/start! {:workspace-options nil :workers 1 :frame-rate 40 :model (demo/model)}
                             (constantly nil))
        receive (atom nil) launched (promise) ended (promise)
        live-ms @#'show/now-ms
        device-ms #(max (get-in @(:state runtime) [:model :time-ms] 0) (live-ms runtime))
        submit! show/submit!]
    (with-redefs [show/submit! (fn [runtime & [command :as args]]
                                 (when (map? command)
                                   (case (:op command)
                                     :cue/launch (deliver launched command)
                                     :instance/end (deliver ended command)
                                     nil))
                                 (apply submit! runtime args))]
      (try
        (let [device (show/connect-device! runtime :push-3
                       {:session-id "midi-only-push-3" :user-mode? true :expression-mode :poly-aftertouch
                        :cue-gestures? true :hold-ms 100
                        :layout {:pads {[0 0] {:channel 3 :note 12}}
                                 :buttons {:choose {:channel 5 :controller 42}}}
                        :button-bindings {:choose :choose-cue} :gesture-modifier :choose-cue
                        :midi {:open! (fn [callback] (reset! receive callback) :capture)
                               :send! (fn [_ _]) :close! (fn [_])}})]
          (is (some? (:schedule @(:resources device))) "the session owns its refresh cadence")
          (@receive [0xb5 42 127])
          ;; Both clocks only move forward, so reading before the note-on gives
          ;; a time no later than the session's own press stamp.
          (let [pressed (device-ms)]
            (@receive [0x93 12 96])
            (let [launch (deref launched 2000 ::timeout)]
              (is (map? launch) "a held pad becomes a hold without further input")
              (when (map? launch)
                (is (= [:ember :held] [(:cue-id launch) (get-in launch [:launch :mode])]))
                (is (<= (+ pressed 100) (:time-ms launch) (+ pressed 1000))))))
          (@receive [0x83 12 0])
          (is (= :instance/end (:op (deref ended 2000 ::timeout))))
          (@receive [0xb5 42 0]))
        (finally (show/disconnect-device! runtime "midi-only-push-3") (show/stop! runtime))))))
