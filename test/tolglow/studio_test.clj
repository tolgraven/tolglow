(ns tolglow.studio-test
  (:require [clojure.test :refer :all]
            [clojure.data.json :as json]
            [ring.core.protocols :as protocols]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.application :as app]
            [tolglow.data :as data]
            [tolglow.studio.transit :as wire]
            [tolglow.studio.agents :as agents]
            [tolglow.studio.output :as output]))

(defmacro with-runtime [[name] & body]
  `(let [~name (show/create-runtime)]
     (try ~@body (finally (app/stop! ~name)))))

(defn apply-at! [runtime type payload time]
  (let [reply (show/submit! runtime type payload)]
    (show/tick! runtime time)
    (deref reply 1000 ::missing)))

(deftest real-control-compiler-workers-and-output
  (with-runtime [r]
    (let [payload {:op :cue/launch :cue-id :ember :command-id "launch-once"}
          ack (apply-at! r :command payload 0)]
      (is (= :accepted (:status ack)))
      (is (= 1 (count (:instances (show/snapshot r)))))
      (show/tick! r 100)
      (is (pos? (first (get-in (show/snapshot r) [:output :values :head-1 :emitted-color]))))
      (is (= ack (apply-at! r :command payload 200)) "Server-stamped time preserves retries")
      (is (= 1 (count (:instances (show/snapshot r)))))
      (is (= :accepted (:status (apply-at! r :command {:op :param/set :instance-id (:instance-id ack)
                                                       :param :level :value 0.25} 300))))
      (is (= 0.25 (get-in (show/snapshot r) [:instances 0 :params :level])))
      (is (nil? (get-in (show/snapshot r) [:status :error]))))))

(deftest blackout-survives-an-effect-failure
  (with-runtime [r]
    (apply-at! r :command {:op :cue/launch :cue-id :ember} 0)
    (show/tick! r 100)
    (let [failing (assoc r :compile (fn [_] (fn [_] (throw (ex-info "bad effect" {})))))
          ack (apply-at! failing :command {:op :blackout/set :value true} 200)]
      (is (= :accepted (:status ack)))
      (is (get-in (show/snapshot r) [:instances 0 :fault]))
      (is (nil? (get-in (show/snapshot r) [:status :error])))
      (is (every? zero? (mapcat seq (vals (get-in @(:state r) [:output :universes]))))))))

(deftest held-gesture-lease-and-instance-specific-release
  (with-runtime [r]
    (let [held (apply-at! r :command {:op :cue/launch :cue-id :ember :launch {:mode :held}
                                      :source {:adapter :web :session-id "gone"}} 0)]
      (apply-at! r :command {:op :cue/launch :cue-id :ice} 1)
      (show/tick! r 11000)
      (is (= :releasing (get-in @(:state r) [:model :instances (:instance-id held) :phase])))
      (show/tick! r 12000)
      (is (= [:ice] (mapv :cue-id (:instances (show/snapshot r))))))))

(deftest logical-sequencer-owns-only-its-launches
  (with-runtime [r]
    (apply-at! r :command {:op :cue/launch :cue-id :spot} 0)
    (is (= :accepted (:status (apply-at! r :composition
                                         {:id :test :mode :sequence :loop :once
                                          :slots [{:cue-id :ember :duration-ms 200}
                                                  {:cue-id :ice :duration-ms 200}]} 0))))
    (apply-at! r :composition-run {:id :test} 0)
    (is (some #{:ember} (map :cue-id (:instances (show/snapshot r)))))
    (show/tick! r 250)
    (is (some #{:ice} (map :cue-id (:instances (show/snapshot r)))))
    (apply-at! r :composition-stop {:id :test} 300)
    (show/tick! r 2000)
    (is (= [:spot] (mapv :cue-id (:instances (show/snapshot r)))))))

(deftest full-state-export-import-restores-output-and-rejects-stale-operations
  (with-runtime [r]
    (apply-at! r :command {:op :cue/launch :cue-id :ember} 0)
    (show/tick! r 150)
    (let [handler (studio/handler r)
          saved (show/snapshot r)
          old-id (get-in saved [:instances 0 :id])
          response (handler {:request-method :get :uri "/api/export"})
          exported (:body response)]
      (is (= "application/transit+json" (get-in response [:headers "Content-Type"])))
      (is (= "attachment; filename=tolglow-show.transit" (get-in response [:headers "Content-Disposition"])))
      (let [document (wire/decode exported)]
        (is (= 2 (:schema-version document)))
        (is (every? bytes? (vals (get-in document [:frame-output :universes])))))
      (is (= :accepted (:status (apply-at! r :import {:source exported} 100))))
      (is (= 1 (:epoch (show/snapshot r))))
      (is (= (:time-ms saved) (:time-ms (show/snapshot r))))
      (is (= (:frame-number saved) (:frame-number (show/snapshot r))))
      (is (= (:output saved) (:output (show/snapshot r))))
      (is (= 1 (count (:instances (show/snapshot r)))))
      (is (not= old-id (get-in (show/snapshot r) [:instances 0 :id])))
      (is (= :rejected (:status (apply-at! r :command {:op :instance/end :instance-id old-id} 150))))
      (is (= :rejected (:status (apply-at! r :command {:op :cue/launch :cue-id :ember :epoch 0} 200))))
      (let [rig (:rig (show/snapshot r))
            bad (assoc-in rig [:fixtures 1 :address] 1)]
        (is (= :rejected (:status (apply-at! r :rig bad 300))))
        (is (= rig (:rig (show/snapshot r))))))))

(deftest stale-runtime-and-queued-pre-restore-actions-cannot-mutate-restored-state
  (with-runtime [r]
    (let [exported (pr-str (show/export-show r))
          restore (show/submit! r :import {:source exported})
          stale (show/submit! r {:op :master/set :value 0.2})]
      (show/tick! r 0)
      (is (= :accepted (:status @restore)))
      (is (= :rejected (:status @stale)))
      (is (= 1.0 (:master (show/snapshot r))))
      (is (= :rejected (:status @(show/submit! r {:op :blackout/set :value true :runtime-id "old-process"})))))))

(deftest browser-api-keeps-public-snapshot-and-origin-boundary
  (with-runtime [r]
    (let [handler (studio/handler r)
          response (handler {:request-method :get :uri "/api/state"})
          state (json/read-str (:body response))]
      (is (= 200 (:status response)))
      (is (= "same-origin" (get-in response [:headers "Cross-Origin-Opener-Policy"])))
      (is (nil? (get state "control")))
      (is (= 8 (count (get-in state ["rig" "fixtures"]))))
      (is (= 403 (:status (handler {:request-method :post :uri "/api/command"
                                    :headers {"origin" "https://elsewhere.test" "host" "localhost:8500"}}))))
      (is (= 404 (:status (handler {:request-method :get :uri "/../deps.edn"})))))))

(deftest artnet-encodes-protocol-universe-order-and-unsigned-payload
  (let [bytes (output/artdmx-packet 257 7 (vec (concat [0 127 128 255] (repeat 508 0))))
        unsigned (mapv #(bit-and % 255) bytes)]
    (is (= 530 (count bytes)))
    (is (= [65 114 116 45 78 101 116 0 0 80 0 14 7 0 0 1 2 0] (subvec unsigned 0 18)))
    (is (= [0 127 128 255] (subvec unsigned 18 22)))
    (is (thrown? clojure.lang.ExceptionInfo (output/artdmx-packet 0 1 [0 0])))))

(deftest stopped-runtime-rejects-instead-of-orphaning-promises
  (let [r (show/create-runtime)
        pending (show/submit! r {:op :master/set :value 0.2})]
    (app/stop! r)
    (is (= :rejected (:status (deref pending 1000 ::missing))))
    (is (= :rejected (:status (deref (show/submit! r {:op :master/set :value 0.3}) 1000 ::missing))))))

(deftest selected-response-modulates-output-without-drifting-canonical-params
  (with-runtime [r]
    (let [ack (apply-at! r :command {:op :cue/launch :cue-id :ember} 0)
          id (:instance-id ack)]
      (show/tick! r 100)
      (let [full (get-in (show/snapshot r) [:output :values :head-1 :emitted-color])
            canonical (get-in @(:state r) [:model :instances id :params])]
        (is (= :accepted (:status (apply-at! r :response {:id :fade :select {:instance-ids [id]}
                                                         :params [:level] :envelope {:attack-ms 100 :release-ms 100}}
                                            100))))
        (is (every? zero? (get-in (show/snapshot r) [:output :values :head-1 :emitted-color])))
        (show/tick! r 150)
        (is (= (mapv #(* 0.5 %) full) (get-in (show/snapshot r) [:output :values :head-1 :emitted-color])))
        (show/tick! r 200)
        (is (= canonical (get-in @(:state r) [:model :instances id :params])))
        (apply-at! r :response-release {:id :fade} 200)
        (show/tick! r 300)
        (is (every? zero? (get-in (show/snapshot r) [:output :values :head-1 :emitted-color])))
        (apply-at! r :response-delete {:id :fade} 300)
        (is (= full (get-in (show/snapshot r) [:output :values :head-1 :emitted-color])))
        (is (= canonical (get-in @(:state r) [:model :instances id :params])))))))

(deftest clock-integrates-old-tempo-before-applying-current-commands
  (with-runtime [r]
    (show/tick! r 500)
    (is (= 1.0 (get-in (show/snapshot r) [:transport :beat])))
    (apply-at! r :command {:op :transport/set :transport {:bpm 60}} 1000)
    (is (= 2.0 (get-in (show/snapshot r) [:transport :beat])))
    (apply-at! r :command {:op :transport/set :transport {:playing? false}} 1500)
    (show/tick! r 5000)
    (is (= 2.5 (get-in (show/snapshot r) [:transport :beat])))
    (apply-at! r :command {:op :transport/set :transport {:playing? true}} 5000)
    (show/tick! r 6000)
    (is (= 3.5 (get-in (show/snapshot r) [:transport :beat])))))

(deftest registry-page-generation-is-visible-and-idempotent
  (with-runtime [r]
    (is (= :accepted (:status (apply-at! r :page-generate {:page-id :library} 0))))
    (let [first-page (get-in @(:state r) [:model :pages :library])]
      (is (seq (:cells first-page)))
      (is (empty? (:placements (apply-at! r :page-generate {:page-id :library} 0))))
      (is (= first-page (get-in @(:state r) [:model :pages :library]))))))

(deftest graphical-rig-edits-preserve-existing-string-identities
  (with-runtime [r]
    (let [initial (get-in @(:state r) [:model :rig])
          portable (assoc-in initial [:fixtures 0 :id] "imported/head")]
      (is (= :accepted (:status (apply-at! r :rig portable 0))))
      (let [from-browser (-> portable data/normalize-json
                             (assoc-in [:fixtures 0 :position] [1 2 3]))]
        (is (= :accepted (:status (apply-at! r :rig from-browser 0))))
        (is (= "imported/head" (get-in @(:state r) [:model :rig :fixtures 0 :id])))
        (is (= [1 2 3] (get-in @(:state r) [:model :rig :fixtures 0 :position]))))
      (doseq [invalid [(dissoc portable :fixtures)
                       (assoc-in portable [:room :height] 0)
                       (assoc-in portable [:trusses 0 :length] -2)]]
        (is (= :rejected (:status (apply-at! r :rig invalid 0)))))
      (is (= "imported/head" (get-in @(:state r) [:model :rig :fixtures 0 :id]))))))

(deftest streaming-agent-authors-and-runs-through-the-real-owner-and-renderer
  (let [event (fn [data] (str "data: " (json/write-str data) "\n\n"))
        tools [{:id "effect" :name "publish_effect"
                :arguments {:definition {:id "agent-red" :version 1 :params {}
                                         :outputs {:color [1 0 0] :dimmer 0.8}}}}
               {:id "cue" :name "put_cue" :arguments {:cue {:id "agent-red" :version 1 :effect-id "agent-red"}}}
               {:id "launch" :name "launch_cue" :arguments {:cue-id "agent-red" :launch {:mode "persistent"}}}]
        round (atom 0)
        transport (fn [_ consume! _]
                    (consume! (if (= 1 (swap! round inc))
                                (str (event {:choices [{:delta {:tool_calls
                                                               (mapv (fn [i tool]
                                                                       {:index i :id (:id tool)
                                                                        :function {:name (:name tool)
                                                                                   :arguments (json/write-str (:arguments tool))}})
                                                                     (range) tools)}}]}) "data: [DONE]\n\n")
                                (str (event {:choices [{:delta {:content "Red is running."}}]}) "data: [DONE]\n\n")))
                    {:status 200})
        r (app/start! (assoc (app/default-config) :web-server {:port 0} :show {:agent-options {:transport transport}}))]
    (try
      (agents/configure! (show/agent-manager r) {:provider :self-hosted :model "scripted" :endpoint "http://unused.test/v1/chat/completions"})
      (let [handler (studio/handler r)
            response (handler {:request-method :post :uri "/api/chat"
                               :body (java.io.ByteArrayInputStream. (.getBytes "{\"message\":\"Create and run a red cue\"}" "UTF-8"))})
            output (java.io.ByteArrayOutputStream.)]
        (protocols/write-body-to-stream (:body response) response output)
        (is (= 200 (:status response)))
        (is (re-find #"Red is running" (.toString output "UTF-8")))
        (is (some #(= :agent-red (:id %)) (:effects (show/snapshot r)))))
      (is (some #(= :agent-red (:cue-id %)) (:instances (show/snapshot r))))
      (is (= :agent-red (get-in @(:state r) [:model :pages :agent :cells [0 0]])))
      (is (= [0.8 0.0 0.0] (get-in (show/snapshot r) [:output :values :head-1 :emitted-color])))
      (finally (app/stop! r)))))
