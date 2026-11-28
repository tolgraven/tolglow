(ns tolglow.studio-test
  (:require [clojure.test :refer :all]
            [clojure.data.json :as json]
            [afterglow.control.pages :as cue-pages]
            [ring.core.protocols :as protocols]
            [tolglow.show :as show]
            [tolglow.composition :as composition]
            [tolglow.studio :as studio]
            [tolglow.application :as app]
            [tolglow.data :as data]
            [tolglow.demo :as demo]
            [tolglow.agents.venue :as venue]
            [tolglow.transit :as wire]
            [tolglow.agents :as agents])
  (:import [java.util.concurrent LinkedBlockingQueue]))

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

(deftest ring-macro-append-preserves-an-earlier-queued-repl-edit
  (with-runtime [runtime]
    (let [accepted-appends (volatile! [])
          admit (:event-admit runtime)
          runtime (assoc runtime :event-admit
                         (fn [model events]
                           (vswap! accepted-appends into (keep #(when (and (= :command/accepted (:type %))
                                                                          (= :composition-append (get-in % [:data :operation])))
                                                                 (get-in % [:data :command-id])) events))
                           (admit model events)))
          macro-id :opening-look
          macro {:id macro-id :mode :macro :name "Opening look"
                 :slots [{:id :first :cue-id :ember}]}
          edited-slot {:id :second :cue-id :ice :params {:level 1/3}}]
      (is (= :accepted (:status (apply-at! runtime :composition macro 0))))
      (is (= :accepted (:status (apply-at! runtime :composition-run {:id macro-id} 0))))
      (let [before (show/snapshot runtime)
            repl-save (show/submit! runtime :composition (update macro :slots conj edited-slot))
            handler (studio/handler runtime)
            payload {:macro-id macro-id :cue-id :spot :command-id "browser-append" :base-revision (:revision before)
                     :expected-epoch (:epoch before) :runtime-id (:runtime-id before)}
            submitted (future (handler {:request-method :post :uri "/api/composition/append"
                                        :headers {"host" "localhost" "accept" "application/transit+json"
                                                  "content-type" "application/transit+json"}
                                        :body (java.io.StringReader. (wire/encode payload))}))
            ^LinkedBlockingQueue queue (:queue runtime)
            deadline (+ (System/nanoTime) 3000000000)]
        (try
          ;; Admit both requests before advancing the frame owner.
          (loop []
            (when (and (< (.size queue) 2) (not (realized? submitted)) (< (System/nanoTime) deadline))
              (Thread/sleep 5)
              (recur)))
          (is (= 2 (.size queue)))
          (show/tick! runtime 10)
          (is (= {:status :accepted} (deref repl-save 1000 ::missing)))
          (let [response (deref submitted 1000 ::missing)
                receipt (some-> response :body wire/decode)
                after (show/snapshot runtime)
                saved (some #(when (= macro-id (:id %)) %) (:compositions after))]
            (is (= 200 (:status response)))
            (is (= {:status :accepted :command-id "browser-append" :macro-id macro-id :revision (:revision after)} receipt))
            (is (= receipt (apply-at! runtime :composition-append payload 10)))
            (is (= ["browser-append"] @accepted-appends))
            (is (= [:ember :ice :spot] (mapv :cue-id (:slots saved))))
            (is (= edited-slot (get-in saved [:slots 1])))
            (is (:running? saved))
            (is (= (mapv :id (:instances before)) (mapv :id (:instances after))))
            (let [rejected (apply-at! runtime :composition-append (assoc payload :command-id "bad-append" :cue-id :missing) 10)]
              (is (= :rejected (:status rejected)))
              (is (= [:slots 3] (get-in rejected [:details :errors 0 :path])))
              (is (= (:compositions after) (:compositions (show/snapshot runtime))))))
          (finally (future-cancel submitted)))))))

(deftest macro-append-replay-and-cell-validation-use-exact-typed-identities
  (with-runtime [runtime]
    (let [macro {:id :set :mode :macro :slots [{:cue-id :ice}]}
          page {:id "main" :cells {[0 0] "ember"}}]
      (is (= :accepted (:status (apply-at! runtime :command {:op :cue/put :cue {:id "ember" :effect-id :ember}} 0))))
      (is (= :accepted (:status (apply-at! runtime :command {:op :page/put :page page} 0))))
      (is (= :accepted (:status (apply-at! runtime :composition macro 0))))
      (let [{:keys [epoch revision runtime-id]} (show/snapshot runtime)
            payload {:macro-id :set :cue-id "ember" :cell {:page-id "main" :x 0 :y 0}
                     :command-id "original" :base-revision revision :expected-epoch epoch :runtime-id runtime-id}
            accepted (apply-at! runtime :composition-append payload 1)]
        (is (= :accepted (:status accepted)))
        (is (= [:ice "ember"] (mapv :cue-id (get-in @(:state runtime) [:model :compositions :set :slots]))))
        (apply-at! runtime :command {:op :page/put :page (assoc-in page [:cells [0 0]] :ember)} 2)
        (is (= "The cue cell changed. Open its actions again."
               (:message (apply-at! runtime :composition-append (assoc payload :command-id "stale") 2))))
        (is (= :rejected (:status (apply-at! runtime :composition-append (assoc payload :cue-id :ember) 2))))
        (apply-at! runtime :composition macro 2)
        (is (= accepted (apply-at! runtime :composition-append payload 2)))
        (is (= [:ice] (mapv :cue-id (get-in @(:state runtime) [:model :compositions :set :slots]))))))))

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

(deftest studio-response-requests-preserve-repl-authored-ids
  (let [r (show/start! {:recovery false :workspace-options nil :workers 1})
        handler (studio/handler r)
        repl! (fn [type body] (deref (show/submit! r type body) 5000 ::missing))
        headers {"accept" "application/transit+json"}
        ring-view #(wire/decode (:body (handler {:request-method :get :uri "/api/state" :headers headers})))
        studio! (fn [path body]
                  (let [response (handler {:request-method :post :uri path
                                           :headers (assoc headers "content-type" "application/transit+json")
                                           :body (java.io.ByteArrayInputStream. (.getBytes (wire/encode body) "UTF-8"))})]
                    [(:status response) (:status (wire/decode (:body response)))]))
        ids [:live/fade "live/fade"]]
    (try
      (doseq [id ids]
        (is (= :accepted (:status (repl! :response {:id id :select {:effect-ids []}
                                                     :params [:level] :envelope {:release {:beats 1}}})))))
      (is (= ids (mapv :id (:responses (ring-view)))))
      (doseq [[id remaining] [["live/fade" [:live/fade]] [:live/fade []]]]
        (is (= [200 :accepted] (studio! "/api/response/release" {:id id})))
        (is (= [id] (into [] (keep #(when (get-in % [:state :released-at-beat]) (:id %)))
                          (:responses (show/snapshot r)))))
        (is (= [200 :accepted] (studio! "/api/response/delete" {:id id})))
        (is (= remaining (mapv :id (:responses (show/snapshot r))))))
      (let [definition (assoc (get-in (demo/model) [:registry :effects :ember]) :id "ember")]
        (is (= :accepted (:status (repl! :definition {:kind :effects :definition definition}))))
        (doseq [id [:ember "ember"]]
          (is (= :accepted (:status (repl! :command {:op :effect/launch :effect-id id})))))
        (is (= [200 :accepted] (studio! "/api/response" {:id :response :params [:level] :operation :multiply
                                                        :select {:effect-ids ["ember"]} :envelope {:sustain 0.5}})))
        (let [{:keys [responses instances]} (ring-view)
              levels (into {} (map (juxt :effect-id #(get-in % [:parameter-sources :level :effective-value]
                                                           (get-in % [:params :level])))) instances)]
          (is (= {:effect-ids ["ember"]} (get-in responses [0 :select])))
          (is (= {:ember 0.7 "ember" 0.35} levels))))
      (finally (show/stop! r)))))

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

(deftest studio-and-repl-share-committed-compositing-order
  (let [model (reduce #(assoc-in %1 [:registry :effects %2 :envelope :attack] {:beats 0}) (demo/model) [:ember :ice])
        r (show/start! {:model model :recovery false})
        submit! #(deref (show/submit! r %) 5000 ::missing)
        launch! (fn [cue priority level]
                  (:instance-id
                    (submit! {:op :cue/launch :cue-id cue :launch {:priority priority :params {:level level}}})))
        handler (studio/handler r)
        ring-state #(json/read-str (:body (handler {:request-method :get :uri "/api/state"})) :key-fn keyword)
        ring-command! (fn [payload]
                        (let [response (handler {:request-method :post :uri "/api/command"
                                                 :headers {"content-type" "application/transit+json"
                                                           "accept" "application/transit+json"
                                                           "x-tolglow-form" "studio-v1"}
                                                 :body (java.io.ByteArrayInputStream.
                                                         (.getBytes (wire/encode payload) "UTF-8"))})]
                          [(:status response) (wire/decode (:body response))]))
        emitted #(get-in (show/snapshot r) [:output :values :head-1 :emitted-color])]
    (try
      (let [first-ember (launch! :ember -1 0.86)
            second-ember (launch! :ember 0 0.48)
            ice (launch! :ice 4 0.74)
            view (ring-state)
            before (emitted)
            move {:op "instance/move-after" :source {:adapter "web" :session-id "studio-order"}
                  :epoch (:epoch view) :runtime-id (:runtime-id view) :expected-revision (:revision view)
                  :instance-id first-ember :after-instance-id ice}
            [status ack] (ring-command! move)
            after (emitted)
            [stale-status stale] (ring-command! (assoc move :instance-id second-ember))]
        (is (= 3 (count (set [first-ember second-ember ice]))))
        (is (= [first-ember second-ember ice] (:active-instance-order view)))
        (is (= [200 :accepted [second-ember ice first-ember]]
               [status (:status ack) (:instance-ids ack)]))
        (is (= (:instance-ids ack) (:active-instance-order (ring-state))
               (:active-instance-order (show/snapshot r))))
        (is (= [422 :stale-revision] [stale-status (:code stale)]))
        (is (= [[first-ember 0.86] [second-ember 0.48] [ice 0.74]]
               (mapv (juxt :id #(get-in % [:params :level])) (:instances (show/snapshot r)))))
        (is (> (last before) (first before) 0))
        (is (> (first after) 0.8 (last after)))
        (let [saved (show/export-show-source r)]
          (is (= :accepted (:status (submit! {:op :instance/move-after :instance-id ice
                                              :after-instance-id first-ember :expected-revision (:revision ack)}))))
          (is (= [second-ember first-ember ice] (:active-instance-order (ring-state))))
          (is (= :accepted (:status (deref (show/submit! r :import {:source saved}) 5000 ::missing))))
          (let [restored (show/snapshot r)
                by-id (into {} (map (juxt :id #(get-in % [:params :level]))) (:instances restored))
                [restored-second restored-ice restored-first] (:active-instance-order restored)]
            (is (= [0.48 0.74 0.86] (mapv by-id (:active-instance-order restored))))
            (is (= :accepted (:status (submit! {:op :instance/end :instance-id restored-first}))))
            (let [new-ember (launch! :ember 4 0.92)]
              (is (not= restored-first new-ember))
              (is (= :instance-not-running (:code (submit! {:op :instance/move-after :instance-id restored-first
                                                            :after-instance-id restored-ice}))))
              (is (some #{restored-second} (:active-instance-order (ring-state))))
              (is (some #{new-ember} (:active-instance-order (ring-state))))))))
      (finally (app/stop! r)))))

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

(deftest command-sources-come-from-the-transport-not-the-payload
  (let [r (show/start! {:model (demo/model) :recovery false})
        handler (studio/handler r)
        post! (fn [uri payload]
                (let [response (handler {:request-method :post :uri uri :headers {"host" "localhost"}
                                         :body (java.io.ByteArrayInputStream. (.getBytes (json/write-str payload) "UTF-8"))})]
                  [(:status response) (json/read-str (:body response) :key-fn keyword)]))
        agent-meta {:conversation-id "conversation-1" :run-id "run-1"}
        tool! (fn [tool arguments] (show/dispatch-tool! r tool arguments (assoc agent-meta :command-id (str (random-uuid)))))
        model #(:model @(:state r))
        history-source #(get-in (model) [:control :history % :command :source])
        budget #(get-in (model) [:settings :lighting-budget])
        operator! (fn [values] (deref (show/submit! r :settings {:scope :lighting-budget :values values}) 5000 ::missing))]
    (try
      (testing "Studio's HTTP command speaks as the web surface whatever it claims"
        (let [[status ack] (post! "/api/command" {:op "cue/launch" :cue-id "ember" :command-id "claims-agent"
                                                  :source {:adapter "agent" :session-id "conversation-1" :run-id "run-1"}})]
          (is (= [200 "accepted"] [status (:status ack)]))
          (is (= {:adapter :web :session-id "conversation-1"} (history-source "claims-agent")))
          (is (= {:adapter :web :session-id "conversation-1"}
                 (get-in (model) [:instances (:instance-id ack) :source])))))
      (testing "a scheduled auto change posted over HTTP belongs to the web surface, so the agent it names cannot withdraw it"
        (let [[status ack] (post! "/api/auto" {:enabled? false :at "next-bar" :event-id "web-auto"
                                               :source {:adapter "agent" :session-id "conversation-1"}})]
          (is (= [200 "accepted"] [status (:status ack)]))
          (is (= {:adapter :web :session-id "conversation-1"}
                 (:source (first (filter #(= "web-auto" (:event-id %)) (:upcoming (show/schedule r)))))))
          (tool! "cancel_scheduled" {:event-ids ["web-auto"]})
          (is (some #(= "web-auto" (:event-id %)) (:upcoming (show/schedule r))))))
      (testing "a tool argument cannot rename the agent"
        (let [ack (tool! "launch_cue" {:cue-id "ice" :source {:adapter "web" :session-id "tab"}})]
          (is (= :accepted (:status ack)))
          (is (= {:adapter :agent :session-id "conversation-1" :run-id "run-1"}
                 (get-in (model) [:instances (:instance-id ack) :source])))))
      (testing "the agent may tighten the operator's lighting budget and relax it back, never past it"
        (is (= :accepted (:status (operator! {:enabled? true :global {:limit 0.8 :max-active 4}}))))
        (is (= :accepted (:status (tool! "set_lighting_budget" {:limit 0.5}))))
        (is (= 0.5 (get-in (budget) [:global :limit])))
        (is (= :accepted (:status (tool! "set_lighting_budget" {:limit 0.7 :max-active 2}))))
        (doseq [loosening [{:limit 0.9} {:max-active 6} {:enabled? false} {:standby-level 0.2 :standby-mode "all"}]]
          (let [ack (tool! "set_lighting_budget" loosening)]
            (is (= :rejected (:status ack)) (pr-str loosening))
            (is (= :agent-budget-loosening (get-in ack [:details :code])) (pr-str loosening))))
        (is (= {:limit 0.7 :max-active 2} (select-keys (:global (budget)) [:limit :max-active])) "Refusals change nothing"))
      (testing "the operator stays unrestricted, from Studio even when the request claims to be the agent, and from the REPL"
        (let [[status ack] (post! "/api/settings" {:scope "lighting-budget" :values {:global {:limit 1.0 :max-active nil}}
                                                   :source {:adapter "agent" :session-id "conversation-1"}})]
          (is (= [200 "accepted"] [status (:status ack)]))
          (is (= 1.0 (get-in (budget) [:global :limit]))))
        (is (= :accepted (:status (tool! "set_lighting_budget" {:limit 0.9}))) "The ceiling follows the operator up")
        (is (= :accepted (:status (operator! {:global {:limit 0.3}}))))
        (is (= :rejected (:status (tool! "set_lighting_budget" {:limit 0.5}))) "and down again"))
      (finally (show/stop! r)))))

(deftest agent-budget-limits-hold-across-bindings-scenes-and-restores
  (let [front #{:head-1 :head-2 :head-3 :head-4}
        model (update-in (demo/model) [:rig :fixtures]
                         (partial mapv #(cond-> % (front (:id %)) (assoc :groups ["front"]))))
        r (show/start! {:model model :recovery false})
        tool! (fn tool! ([tool arguments] (tool! "conversation-1" tool arguments))
                ([conversation tool arguments]
                 (show/dispatch-tool! r tool arguments {:conversation-id conversation :run-id (str (random-uuid))
                                                        :command-id (str (random-uuid))})))
        budget #(get-in @(:state r) [:model :settings :lighting-budget])
        operator! (fn [values] (deref (show/submit! r :settings {:scope :lighting-budget :values values}) 5000 ::missing))
        import! (fn [source] (deref (show/submit! r :import {:source source}) 5000 ::missing))
        binding {:cue-id :ember :param :level :default 0.4}]
    (try
      (testing "a fixed agent limit caps an operator's bound limit and never replaces it"
        (is (= :accepted (:status (operator! {:enabled? true :global {:limit binding}}))))
        (is (= :accepted (:status (tool! "set_lighting_budget" {:limit 0.85}))))
        (is (= {:op :min :args [binding 0.85] :default 0.4} (get-in (budget) [:global :limit])))
        (is (= :accepted (:status (tool! "set_lighting_budget" {:limit 0.3}))))
        (is (= {:op :min :args [binding 0.3] :default 0.3} (get-in (budget) [:global :limit]))))
      (testing "the lighting scene and autopilot tools cannot loosen the budget"
        (is (= :accepted (:status (operator! {:global {:limit 0.8} :groups {"front" {:limit 0.5}}}))))
        (let [before (budget)
              narrowed (tool! "set_lighting_scene" {:op "apply" :group-id "front" :fixture-ids ["head-1"] :target 0.5})]
          (is (= :rejected (:status narrowed)))
          (is (= :agent-budget-loosening (get-in narrowed [:details :code])))
          (is (empty? (get-in @(:state r) [:model :extensions :tolglow.lighting-scenes/control :groups])))
          (is (= :accepted (:status (tool! "set_lighting_scene" {:op "apply" :group-id "front" :target 0.5})))
              "A look over the whole group is fine")
          (is (= :accepted (:status (tool! "configure_energy_autopilot" {:enabled? true :scope "global"
                                                                         :targets {:brightness 0.9}}))))
          (is (= before (budget)))))
      (testing "a restored show keeps who tightened the budget; another show's settings become the ceiling"
        (is (= :accepted (:status (operator! {:groups {"front" nil}}))))
        (is (= :accepted (:status (tool! "set_lighting_budget" {:limit 0.4}))))
        (let [saved (show/export-show-source r)]
          (is (= :accepted (:status (operator! {:global {:limit 0.2}}))))
          (is (= :accepted (:status (import! saved))))
          (is (= 0.4 (get-in (budget) [:global :limit])))
          (is (= :rejected (:status (tool! "conversation-2" "set_lighting_budget" {:limit 0.6})))
              "Another conversation cannot relax it")
          (is (= :accepted (:status (tool! "set_lighting_budget" {:limit 0.6})))
              "The conversation that tightened it can, up to the operator's setting")
          (is (= :rejected (:status (tool! "set_lighting_budget" {:limit 0.9}))))
          (let [other (show/start! {:model (assoc-in model [:settings :lighting-budget] {:enabled? true :global {:limit 0.25}})
                                    :recovery false})
                foreign (try (show/export-show-source other) (finally (show/stop! other)))]
            (is (= :accepted (:status (import! foreign))))
            (is (= :rejected (:status (tool! "set_lighting_budget" {:limit 0.6}))))
            (is (= :accepted (:status (tool! "set_lighting_budget" {:limit 0.25})))))))
      (finally (show/stop! r)))))

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

(deftest macro-card-sizes-share-one-validated-command-and-survive-export
  (with-runtime [r]
    (let [macro {:id :opening-look :mode :macro :name "Opening look"
                 :slots [{:id :first :cue-id :ember} {:id :second :cue-id :ice}]}
          saved #(some (fn [c] (when (= :opening-look (:id c)) c)) (:compositions (show/snapshot r)))]
    (is (= :accepted (:status (apply-at! r :composition macro 0))))
    (testing "the REPL sets a size and an order and reads them back"
      (is (= :accepted (:status (apply-at! r :composition-cards
                                           {:id :opening-look :cards [{:slot-id :second :size :large}]} 1))))
      (is (= [{:slot-id :second :size :large}] (:cards (saved)))))
    (testing "Studio sends the same command over typed transit"
      (let [{:keys [epoch runtime-id]} (show/snapshot r)
            handler (studio/handler r)
            payload {:id :opening-look :cards [{:slot-id :first :size :medium} {:slot-id :second :size :large}]
                     :expected-epoch epoch :runtime-id runtime-id}
            response (future (handler {:request-method :post :uri "/api/composition/cards"
                                       :headers {"host" "localhost" "accept" "application/transit+json"
                                                 "content-type" "application/transit+json"}
                                       :body (java.io.StringReader. (wire/encode payload))}))]
        (loop [n 0] (when (and (not (realized? response)) (< n 300)) (show/tick! r (+ 2 n)) (Thread/sleep 5) (recur (inc n))))
        (is (= 200 (:status (deref response 1000 ::missing))))
        (is (= [{:slot-id :first :size :medium} {:slot-id :second :size :large}] (:cards (saved))))))
    (testing "a resize and a move apply to the arrangement the show holds"
      (is (= :accepted (:status (apply-at! r :composition-cards {:id :opening-look :move {:slot-id :second :offset -1}} 300))))
      (is (= :accepted (:status (apply-at! r :composition-cards {:id :opening-look :resize {:slot-id "first" :size "small"}} 301))))
      (is (= [{:slot-id :second :size :large} {:slot-id :first :size :small}] (:cards (saved))))
      (is (= :accepted (:status (apply-at! r :composition-cards {:id :opening-look :move {:slot-id :second :offset 1}} 302))))
      (is (= :accepted (:status (apply-at! r :composition-cards {:id :opening-look :resize {:slot-id :first :size :medium}} 303))))
      (is (= [{:slot-id :first :size :medium} {:slot-id :second :size :large}] (:cards (saved)))))
    (testing "invalid cards are refused and leave the saved arrangement alone"
      (let [before (:cards (saved))]
        (doseq [cards [[{:slot-id :first :size :huge}] [{:slot-id :missing :size :small}]
                       [{:slot-id :first :size :small} {:slot-id :first :size :large}]]]
          (is (= :rejected (:status (apply-at! r :composition-cards {:id :opening-look :cards cards} 400)))))
        (is (= :rejected (:status (apply-at! r :composition
                                             {:id :steps :mode :sequence :slots [{:id :a :cue-id :ember}]
                                              :cards [{:slot-id :a :size :large}]} 400))))
        (is (= :rejected (:status (apply-at! r :composition-cards {:id :nothing :cards []} 400))))
        (doseq [change [{:resize {:slot-id :first :size :huge}} {:resize {:slot-id :missing :size :small}}
                        {:move {:slot-id :first :offset 0.5}} {:move {:slot-id :missing :offset 1}}
                        {} {:cards [] :resize {:slot-id :first :size :small}}]]
          (is (= :rejected (:status (apply-at! r :composition-cards (assoc change :id :opening-look) 400))) (pr-str change)))
        (is (= before (:cards (saved))))))
    (testing "a running member carries the macro's layer and slot"
      (apply-at! r :composition-run {:id :opening-look} 500)
      (let [{:keys [layer-id member-slots]} (saved)
            members (filter #(= layer-id (get-in % [:source :native-layer :id])) (:instances (show/snapshot r)))]
        (is (= [:first :second] member-slots))
        (is (= #{0 1} (set (map #(get-in % [:source :native-layer :member]) members))))))
    (testing "the arrangement survives export and import"
      (let [exported (:body ((studio/handler r) {:request-method :get :uri "/api/export"}))]
        (is (= :accepted (:status (apply-at! r :import {:source exported} 600))))
        (is (= [{:slot-id :first :size :medium} {:slot-id :second :size :large}] (:cards (saved))))))
    (testing "nil returns every card to its default"
      (is (= :accepted (:status (apply-at! r :composition-cards {:id :opening-look :cards nil} 700))))
      (is (not (contains? (saved) :cards)))))))

(deftest macro-card-pins-share-the-cards-command-and-carry-over
  (with-runtime [r]
    (let [saved #(some (fn [c] (when (= :p (:id c)) c)) (:compositions (show/snapshot r)))
          rejection #(get-in % [:details :errors 0 :path])
          slots [{:id :pulse :cue-id :breathe} {:id :glow :cue-id :ember}]]
      (apply-at! r :composition {:id :p :mode :macro :slots slots} 0)
      (testing "the REPL pins a member's parameter, appended after its current pins"
        (is (= :accepted (:status (apply-at! r :composition-cards {:id :p :pin {:slot-id :pulse :parameter :period}} 1))))
        (is (= :accepted (:status (apply-at! r :composition-cards {:id :p :pin {:slot-id "pulse" :parameter "level"}} 2))))
        (is (= [{:slot-id :pulse :size :small :pins [:period :level]} {:slot-id :glow :size :small}] (:cards (saved)))
            "a wire string names the effect's own parameter key"))
      (testing "Studio's typed transit sends the same command"
        (let [{:keys [epoch runtime-id]} (show/snapshot r)
              response (future ((studio/handler r)
                                {:request-method :post :uri "/api/composition/cards"
                                 :headers {"host" "localhost" "accept" "application/transit+json"
                                           "content-type" "application/transit+json"}
                                 :body (java.io.StringReader.
                                        (wire/encode {:id :p :pin {:slot-id :pulse :parameter :level :pinned? false}
                                                      :expected-epoch epoch :runtime-id runtime-id}))}))]
          (loop [n 0] (when (and (not (realized? response)) (< n 300)) (show/tick! r (+ 3 n)) (Thread/sleep 5) (recur (inc n))))
          (is (= 200 (:status (deref response 1000 ::missing))))
          (is (= [:period] (:pins (first (:cards (saved))))))))
      (testing "a resize and a move keep the pins"
        (apply-at! r :composition-cards {:id :p :resize {:slot-id :pulse :size :medium}} 400)
        (apply-at! r :composition-cards {:id :p :move {:slot-id :pulse :offset 1}} 401)
        (is (= [{:slot-id :glow :size :small} {:slot-id :pulse :size :medium :pins [:period]}] (:cards (saved)))))
      (testing "an explicit edit refuses a parameter its member's effect does not declare"
        (let [before (:cards (saved))]
          (is (= [:pin :parameter] (rejection (apply-at! r :composition-cards {:id :p :pin {:slot-id :pulse :parameter :tint}} 402))))
          (is (= [:pins :pins 0] (rejection (apply-at! r :composition-cards {:id :p :pins {:slot-id :glow :pins [:period]}} 403))))
          (is (= [:cards 0 :pins 0] (rejection (apply-at! r :composition-cards
                                                          {:id :p :cards [{:slot-id :glow :size :small :pins [:period]}]} 404))))
          (doseq [change [{:pin {:slot-id :missing :parameter :level}} {:pin {:slot-id :pulse}}
                          {:pin {:slot-id :pulse :parameter :level :pinned? "yes"}}
                          {:pins {:slot-id :pulse :pins :level}} {:pin :level}
                          {:pin {:slot-id :pulse :parameter :level} :resize {:slot-id :pulse :size :small}}]]
            (is (= :rejected (:status (apply-at! r :composition-cards (assoc change :id :p) 405))) (pr-str change)))
          (is (= before (:cards (saved))))))
      (testing "a save that omits :cards keeps the pins of an unchanged member, and replacing pins works"
        (is (= :accepted (:status (apply-at! r :composition {:id :p :mode :macro :name "Again" :slots slots} 500))))
        (is (= [:period] (:pins (second (:cards (saved))))))
        (is (= :accepted (:status (apply-at! r :composition-cards {:id :p :pins {:slot-id :pulse :pins ["level" :period]}} 501))))
        (is (= [:level :period] (:pins (second (:cards (saved))))))
        (is (= :accepted (:status (apply-at! r :composition-cards {:id :p :pins {:slot-id :pulse :pins nil}} 502))))
        (is (not (contains? (second (:cards (saved))) :pins)))
        (apply-at! r :composition-cards {:id :p :pins {:slot-id :pulse :pins [:period]}} 503))
      (testing "pins survive export and import"
        (let [exported (:body ((studio/handler r) {:request-method :get :uri "/api/export"}))]
          (is (= :accepted (:status (apply-at! r :import {:source exported} 600))))
          (is (= [:period] (:pins (second (:cards (saved))))))))
      (testing "a member whose slot now names another cue loses its card and pins"
        (is (= :accepted (:status (apply-at! r :composition {:id :p :mode :macro
                                                             :slots [{:id :pulse :cue-id :ice} {:id :glow :cue-id :ember}]} 700))))
        (is (not-any? #(= :pulse (:slot-id %)) (:cards (saved)))))
      (testing "a stale pin is pruned on save, not refused"
        (is (= :accepted (:status (apply-at! r :composition {:id :p :mode :macro :slots slots
                                                             :cards [{:slot-id :pulse :size :small :pins [:gone :period]}]} 800))))
        (is (= [{:slot-id :pulse :size :small :pins [:period]}] (:cards (saved))))))))

(deftest a-stale-pin-never-blocks-an-arrange-edit
  ;; Review repro (#816): pin :level on :a, drop :level from its effect, then
  ;; resize :b. The stale pin is pruned instead of refusing the edit.
  (let [base (let [r (show/create-runtime)]
               (try (get-in @(:state r) [:model]) (finally (app/stop! r))))
        model (-> (composition/save base {:id :m :mode :macro :slots [{:id :a :cue-id :breathe} {:id :b :cue-id :ember}]})
                  (composition/set-cards :m {:pin {:slot-id :a :parameter :level}})
                  (composition/set-cards :m {:pin {:slot-id :a :parameter :period}}))
        effect-id (get-in model [:registry :cues :breathe :effect-id])
        dropped (update-in model [:registry :effects effect-id :params] dissoc :level)
        cards (fn [m] (get-in m [:compositions :m :cards]))
        edit (fn [change] (cards (composition/set-cards dropped :m change)))
        refusal (fn [change]
                  (try (composition/set-cards dropped :m change) nil
                       (catch clojure.lang.ExceptionInfo e (get-in (ex-data e) [:errors 0 :path]))))]
    (is (= [:level :period] (:pins (first (cards model)))))
    (testing "a resize of another card succeeds and prunes the stale pin"
      (is (= [{:slot-id :a :size :small :pins [:period]} {:slot-id :b :size :large}]
             (edit {:resize {:slot-id :b :size :large}}))))
    (testing "a move, and a pin on the same card, succeed too"
      (is (= [:b :a] (mapv :slot-id (edit {:move {:slot-id :b :offset -1}}))))
      (is (= [:period] (:pins (first (edit {:pin {:slot-id :a :parameter :period}}))))))
    (testing "unpinning the stale parameter is accepted; pinning it again is refused"
      (is (= [:period] (:pins (first (edit {:pin {:slot-id :a :parameter :level :pinned? false}})))))
      (is (= [:pin :parameter] (refusal {:pin {:slot-id :a :parameter :level}})))
      (is (= [:pins :pins 1] (refusal {:pins {:slot-id :a :pins [:period :level]}}))))))

(deftest re-saving-a-macro-keeps-its-cards-for-the-same-members
  (with-runtime [r]
    (let [saved #(some (fn [c] (when (= :m (:id c)) c)) (:compositions (show/snapshot r)))
          rejection #(get-in % [:details :errors 0 :path])]
      (apply-at! r :composition {:id :m :mode :macro :slots [{:id :a :cue-id :ember} {:id :b :cue-id :ice}]} 0)
      (apply-at! r :composition-cards {:id :m :cards [{:slot-id :b :size :large} {:slot-id :a :size :medium}]} 1)
      (testing "a REPL save that omits :cards keeps them, as a Compose save does"
        (is (= :accepted (:status (apply-at! r :composition {:id :m :mode :macro :name "Renamed"
                                                             :slots [{:id :a :cue-id :ember} {:id :b :cue-id :ice}]} 2))))
        (is (= [{:slot-id :b :size :large} {:slot-id :a :size :medium}] (:cards (saved)))))
      (testing "removing a member drops its card instead of refusing the save"
        (is (= :accepted (:status (apply-at! r :composition {:id :m :mode :macro :slots [{:id :a :cue-id :ember}]} 3))))
        (is (= [{:slot-id :a :size :medium}] (:cards (saved)))))
      (testing "a card follows its member: a positional slot ID that now names another cue loses it"
        (apply-at! r :composition {:id :m :mode :macro :slots [{:cue-id :ember} {:cue-id :ice}]} 4)
        (apply-at! r :composition-cards {:id :m :cards [{:slot-id "slot-1" :size :large}]} 5)
        (is (= :accepted (:status (apply-at! r :composition {:id :m :mode :macro :slots [{:cue-id :ice} {:cue-id :spot}]} 6))))
        (is (nil? (:cards (saved)))))
      (testing "an explicit :cards nil clears them"
        (apply-at! r :composition-cards {:id :m :cards [{:slot-id "slot-0" :size :large}]} 7)
        (apply-at! r :composition {:id :m :mode :macro :cards nil :slots [{:cue-id :ice} {:cue-id :spot}]} 8)
        (is (not (contains? (saved) :cards))))
      (testing "projection keys read back from a snapshot are not saved"
        (apply-at! r :composition-run {:id :m} 9)
        (is (:layer-id (saved)))
        (is (= :accepted (:status (apply-at! r :composition (assoc (saved) :name "Again") 10))))
        (apply-at! r :composition-stop {:id :m} 11)
        (is (= [false nil nil] ((juxt :running? :layer-id :member-slots) (saved))))
        (is (not-any? #(contains? (get-in @(:state r) [:model :compositions :m]) %)
                      [:running? :playback :layer-id :member-slots])))
      (testing "a size that is not a keyword is refused with a path"
        (is (= [:resize :size] (rejection (apply-at! r :composition-cards {:id :m :resize {:slot-id "slot-0" :size 3}} 12))))
        (is (= :rejected (:status (apply-at! r :composition-cards {:id :m :resize {:slot-id "slot-0" :size "huge"}} 12)))))
      (testing "a sequence refuses :cards, even nil"
        (is (= [:cards] (rejection (apply-at! r :composition {:id :s :mode :sequence :cards nil
                                                              :slots [{:cue-id :ember :duration-ms 100}]} 13))))))))
