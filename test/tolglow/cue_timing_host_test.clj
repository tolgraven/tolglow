(ns tolglow.cue-timing-host-test
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is]]
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
  (let [runtime (show/create-runtime {:workspace-options nil})]
    (try
      (is (= :fired (:status (apply-at! runtime :cue-schedule (launch "now" :ember :off) 100))))
      (is (= :pending (:status (apply-at! runtime :cue-schedule (launch "bad" :missing {:beats 1}) 110))))
      (show/tick! runtime 500)
      (is (= {:status :missed :reason :launch-rejected :attempted-at {:time-ms 500 :beat 1.0}}
             (select-keys (show/quantized-cue-status runtime "bad") [:status :reason :attempted-at])))
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
