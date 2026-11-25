(ns tolglow.studio-test
  (:require [clojure.test :refer :all]
            [clojure.data.json :as json]
            [afterglow.control.pages :as cue-pages]
            [ring.core.protocols :as protocols]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.application :as app]
            [tolglow.data :as data]
            [tolglow.demo :as demo]
            [tolglow.agents.venue :as venue]
            [tolglow.transit :as wire]
            [tolglow.agents :as agents]))

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
        (is (not (contains? (get-in document [:model :pages]) cue-pages/recent-page-id)))
        (is (every? bytes? (vals (get-in document [:frame-output :universes])))))
      (is (= :accepted (:status (apply-at! r :import {:source exported} 100))))
      (is (= 1 (:epoch (show/snapshot r))))
      (is (= (:time-ms saved) (:time-ms (show/snapshot r))))
      (is (= (:frame-number saved) (:frame-number (show/snapshot r))))
      (is (= (:recent-page saved) (:recent-page (show/snapshot r))))
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

(deftest authoring-can-read-the-exact-registered-reactive-definition
  (with-runtime [r]
    (let [definition {:id :reactive :version 1 :targets [:head-1]
                      :reactive {:inputs {:level [:subscription [:wash/level 2/3]]}
                                 :transition {:beats 1/3}}
                      :outputs {:dimmer {:op :reactive :key :level}}}
          ack (apply-at! r :definition {:kind :effects :definition definition} 0)
          request {:request-method :get :uri "/api/definition" :query-string "kind=effects&id=reactive"
                   :headers {"accept" "application/transit+json"}}
          handler (studio/handler r)
          source (wire/decode (:body (handler request)))
          typed (wire/decode (:body (handler (update request :query-string str "&data=true"))))]
      (is (= :accepted (:status ack)))
      (is (= #{:source} (set (keys source))))
      (is (= (:source source) (:source typed)))
      (is (= definition (:definition typed))))))

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

(deftest recent-placements-are-shared-by-repl-and-studio-and-remain-read-only
  (with-runtime [r]
    (let [authored (:pages (show/snapshot r))
          id cue-pages/recent-page-id
          cells [{:x 0 :y 0 :cue-id :ice} {:x 1 :y 0 :cue-id :ember}]]
      (is (empty? (get-in (show/snapshot r) [:recent-page :cells])))
      (is (= :accepted (:status (apply-at! r :command {:op :cue/launch :cue-id :ember} 0))))
      (is (= :accepted (:status (apply-at! r :command {:op :cue/launch :cue-id :ice} 50))))
      (is (= {:id id :name "Recent cues" :columns 8 :rows 8 :cells cells}
             (:recent-page (show/snapshot r))))
      (let [state (json/read-str (:body ((studio/handler r) {:request-method :get :uri "/api/state"})))]
        (is (= id (get-in state ["recent-page" "id"])))
        (is (= ["ice" "ember"] (mapv #(get % "cue-id") (get-in state ["recent-page" "cells"])))))
      (doseq [[type payload] [[:command {:op :page/put :page {:id id :cells {}}}]
                              [:page-generate {:page-id id}]
                              [:agent-cue {:page-id id :cue {:id :attempted :effect-id :ember}}]]]
        (is (= :rejected (:status (apply-at! r type payload 100)))))
      (is (= authored (:pages (show/snapshot r))))
      (is (nil? (get-in @(:state r) [:model :registry :cues :attempted]))))))

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

(deftest show-chat-discovers-and-varies-a-group-at-measured-capacity
  (let [requests (atom [])
        tool (fn [id name input]
               {:tool_calls [{:index 0 :id id :function {:name name :arguments (json/write-str input)}}]})
        transport (fn [request consume! _]
                    (let [body (json/read-str (:body request) :key-fn keyword)
                          n (count (swap! requests conj body))
                          messages (:messages body)
                          last-result (fn [] (json/read-str (:content (peek messages)) :key-fn keyword))
                          delta (case n
                                  1 (tool "discover" "inspect_lighting" {})
                                  2 (tool "vary" "set_lighting_scene"
                                          {:op "vary" :group-id (get-in (last-result) [:groups :items 0 :group-id])
                                           :target 0.5})
                                  3 (tool "on-rise" "configure_energy_scene" {:op "save" :group-id "stage/left-movers"
                                                                               :above 0.5 :below 0.2 :target 0.5})
                                  4 (tool "budget" "set_lighting_budget" {:group-id "stage/left-movers"
                                                                           :max-active 1 :limit 0.8
                                                                           :strategy "priority" :fixture-order ["bank/spot-1"]})
                                  5 (tool "readback" "inspect_lighting" {:group-id "stage/left-movers"})
                                  {:content "I varied the left movers and checked their measured output."})]
                      (consume! (str "data: " (json/write-str {:choices [{:delta delta}]}) "\n\ndata: [DONE]\n\n"))
                      {:status 200}))
        observation {:level 0.32 :epoch 0 :sample-time-ms 120 :path-m 7 :phase :clearing}
        model (-> (demo/model)
                  (assoc-in [:registry :effects] {})
                  (assoc-in [:registry :cues] {})
                  (assoc :pages {})
                  (assoc-in [:rig :fixtures] [{:id "bank/spot-1" :groups ["stage/left-movers"]
                                               :channels {:dimmer 1} :universe 1 :address 3}
                                              {:id :fog :channels {:fog 1} :universe 1 :address 1}])
                  (assoc-in [:variables :audio/energy] 0.5)
                  (assoc-in [:variables :audio :features] {:breakdown? true :time-ms 34})
                  (assoc-in [:variables :fog/observation] observation)
                  (assoc-in [:variables :private-value] "DO-NOT-EXPORT-HIDDEN-VARIABLE"))
        rt (app/start! (assoc (app/default-config) :web-server {:port 0}
                              :show {:model model :agent-options {:transport transport}}))]
    (try
      (agents/configure! (show/agent-manager rt) {:provider :self-hosted :model "scripted"
                                                  :endpoint "http://unused.test/v1/chat/completions"})
      (let [response ((studio/handler rt) {:request-method :post :uri "/api/chat"
                                           :body (java.io.ByteArrayInputStream.
                                                  (.getBytes "{\"message\":\"Vary the left movers at half brightness now and whenever audio energy rises above half after falling below one fifth\"}" "UTF-8"))})
            stream (java.io.ByteArrayOutputStream.)]
        (protocols/write-body-to-stream (:body response) response stream)
        (is (= 200 (:status response)))
        (is (re-find #"checked their measured output" (.toString stream "UTF-8")))
        (is (contains? (set (map #(get-in % [:function :name]) (:tools (first @requests)))) "inspect_lighting"))
        (is (= 6 (count @requests)))
        (let [read-result (fn [index] (json/read-str (:content (peek (:messages (get @requests index)))) :key-fn keyword))
              discovered (read-result 1)
              varied (read-result 2)
              budgeted (read-result 4)
              measured (read-result 5)]
          (is (= ["bank/spot-1"] (mapv :id (get-in discovered [:eligible-fixtures :items]))))
          (is (= {:local-brightness 0.0 :global-brightness 0.0 :global-maximum 1.0}
                 (select-keys (get-in discovered [:eligible-fixtures :items 0])
                              [:local-brightness :global-brightness :global-maximum])))
          (is (= "stage/left-movers" (get-in discovered [:groups :items 0 :group-id])))
          (is (= 0.5 (get-in discovered [:music :audio/energy])))
          (is (= 0.32 (get-in discovered [:fog :observation :level])))
          (is (not (clojure.string/includes? (pr-str @requests) "DO-NOT-EXPORT-HIDDEN-VARIABLE")))
          (is (= "accepted" (:status varied)))
          (is (= "accepted" (:status budgeted)))
          (is (= 0.8 (get-in measured [:lighting-budget :groups :stage/left-movers :limit-watts])))
          (is (= ["bank/spot-1"] (get-in (show/snapshot rt) [:settings :lighting-budget :groups "stage/left-movers" :fixture-order])))
          (is (= {:above 0.5 :below 0.2 :target 0.5 :enabled? true}
                 (get-in measured [:groups :items 0 :energy-binding])))
          (is (= 1 (get-in measured [:groups :items 0 :generation])))
          (is (= 0.5 (get-in measured [:groups :items 0 :target])))
          (is (< (abs (- 0.5 (get-in measured [:groups :items 0 :actual]))) 0.004))))
      (finally (app/stop! rt)))))

(deftest group-tool-pages-and-namespaced-signal-bindings
  (let [state {:model {:epoch 5 :rig {:fixtures [{:id :bank/left :groups [:area/one] :channels {:red 1}}
                                                   {:id "right" :groups ["two"] :channels {:dimmer 2}}]}
                       :variables {:signals {"user/drive" 0.48} :fog/observation {:epoch 4 :level 0.28 :level-change-per-s -0.03 :session-id "private"}}}
               :output {:atmosphere {:fog-level 0.42 :sample-time-ms 10 :last-presented-ms 8 :reference-path-m 7
                                     :fog-level-change-per-s 0.012 :clearance-half-life-s 22 :sources [{:dmx-coarse 255}]}
                        :fog-control {:enabled? true :target 0.4 :zone :in-band :pumps {:jet {:active? true}}}}}
        listing (venue/inspect state {:limit 1})
        single (venue/inspect state {:group-id "area/one"})
        next-page (venue/inspect state {:fixture-offset 1 :offset 1 :limit 1})]
    (is (= 2 (get-in listing [:groups :total])))
    (is (= 1 (get-in listing [:eligible-fixtures :next-offset])))
    (is (= "two" (get-in next-page [:groups :items 0 :group-id])))
    (is (= "right" (get-in next-page [:eligible-fixtures :items 0 :id])))
    (is (= "bank/left" (get-in single [:eligible-fixtures :items 0 :id])))
    (is (= {:fog-level 0.42 :fog-level-change-per-s 0.012 :clearance-half-life-s 22 :sample-time-ms 10 :last-presented-ms 8 :reference-path-m 7}
           (get-in single [:fog :estimate])))
    (is (= {:enabled? true :target 0.4 :zone :in-band} (get-in single [:fog :controller])))
    (is (= {:epoch 4 :level 0.28 :level-change-per-s -0.03 :current-epoch? false} (get-in single [:fog :observation])))
    (is (= {:group-id :area/one :op :apply :fixture-ids [:bank/left "right"]
            :target-var [:audio/energy]}
           (venue/scene-request (:model state)
                                {:group-id "area/one" :op "apply" :fixture-ids ["bank/left" "right"]
                                 :target-var ["audio/energy"]})))
    (is (= [:signals "user/drive"] (:target-var (venue/scene-request (:model state)
                                                                    {:group-id "two" :target-var ["signals" "user/drive"]}))))
    (is (thrown? Exception (venue/scene-request (:model state) {:group-id "two" :target-var ["config" "secret"]})))
    (is (thrown? Exception (venue/inspect state {:limit 1000})))
    (is (thrown? Exception (venue/inspect state {:group-id "stage/missing"})))))

(deftest rejected-inline-replacement-preserves-the-live-callback
  (with-runtime [r]
    (let [original identity replacement (fn [_] nil)
          descriptor {:id :test/inline :enabled? true :match {:type :audio/kick}}
          register #(show/register-event-subscriber! r (assoc descriptor :middleware %))
          accepted (register original)]
      (show/tick! r 10)
      (is (= :accepted (:status @accepted)))
      (let [queue (java.util.concurrent.LinkedBlockingQueue. 1)
            full (assoc r :queue queue)
            callback-id (get-in (show/snapshot r) [:event-subscribers :subscribers 0 :actions 0 :implementation :callback-id])]
        (.add queue :occupied)
        (is (= :rejected (:status @(show/register-event-subscriber! full (assoc descriptor :middleware replacement)))))
        (is (= {[:test/inline callback-id] original} @(get-in r [:event-callbacks :inline-handlers])))))))
