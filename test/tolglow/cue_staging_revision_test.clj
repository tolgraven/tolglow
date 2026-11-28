(ns tolglow.cue-staging-revision-test
  "Regressions from the review of #913: stage events and history targets,
  following the cue's definition, launch outcomes, retries and the Studio
  route's source."
  (:require [afterglow.sync :as sync]
            [afterglow.sync.session :as session]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [tolglow.connections :as connections]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.studio.access :as access]
            [tolglow.transit :as transit])
  (:import [java.io ByteArrayInputStream]))

(def ^:private operator {:operator-id "repl/stage-review"})

(defn- apply-at!
  ([runtime payload time] (apply-at! runtime payload time nil))
  ([runtime payload time authority]
   (let [receipt (show/submit! runtime :command payload authority)]
     (show/tick! runtime time)
     (deref receipt 1000 ::missing))))

(defn- model [runtime] (:model @(:state runtime)))

(defn- staged [runtime stage-id]
  (some #(when (= stage-id (:stage-id %)) %) (show/staged-cues runtime)))

(defn- history [runtime] (show/operator-history runtime))

(defn- undo! [runtime time]
  (let [{:keys [current-id position]} (history runtime)]
    (apply-at! runtime {:op :history/undo :expected-cursor current-id :expected-history-position position}
               time operator)))

(defn- redo! [runtime node-id time]
  (let [{:keys [current-id position]} (history runtime)]
    (apply-at! runtime {:op :history/redo :expected-cursor current-id :expected-history-position position
                        :node-id node-id} time operator)))

(defn- level [runtime instance-id] (get-in (model runtime) [:instances instance-id :params :level]))

(defn- runtime [] (show/create-runtime {:workers 1 :workspace-options nil}))

(defn- turn! [runtime instance-id value gesture-id time]
  (doseq [[op extra] [[:gesture/begin {}] [:gesture/sample {:sequence 1 :value value}] [:gesture/end {:last-sequence 1}]]]
    (is (= :accepted (:status (apply-at! runtime (merge {:op op :instance-id instance-id :param :level :control-id "c"
                                                         :gesture-id gesture-id} extra)
                                         time operator))))))

(defn- revise-ember! [runtime bounds default time]
  (let [effect (get-in (model runtime) [:registry :effects :ember])
        revised (-> effect (update :version inc)
                    (update-in [:params :level] assoc :bounds bounds :default default
                               :recommended [(first bounds) (second bounds)]))
        reply (show/submit! runtime :definition {:kind :effects :definition revised})]
    (show/tick! runtime time)
    (deref reply 2000 ::missing)))

(deftest staged-edits-emit-stage-events-and-no-instance-ever-appears
  (let [rt (runtime) seen (atom [])]
    (try
      (swap! (:observers rt) assoc ::probe {:events! (fn [_ events & _] (swap! seen into events))
                                            :frame! (fn [_]) :tracing? (constantly false)})
      (apply-at! rt {:op :cue-stage/open :cue-id :ember :stage-id "s"} 0)
      (apply-at! rt {:op :cue-stage/edit :stage-id "s" :param :level :value 0.3 :command-id "e"} 10 operator)
      (apply-at! rt {:op :param/set :stage-id "s" :param :level :value 0.4} 20)
      (is (empty? (filter #(= :param/changed (:type %)) @seen)) "no committed :param/changed for a stage")
      (is (= [{:stage-id "s" :param :level :value 0.3} {:stage-id "s" :param :level :value 0.4}]
             (mapv :data (filter #(= :cue-stage/param-changed (:type %)) @seen))))
      (is (not-any? #(contains? (:command %) :instance-id) (vals (get-in (model rt) [:control :history])))
          "control history names no instance for a staged edit")
      (is (= [:staged-param "s" :level] (get-in (model rt) [:control :history "e" :command :edit-target])))
      (is (= :cue-stage/set (get-in (model rt) [:control :history (last (get-in (model rt) [:control :history-order]))
                                               :command :op])))
      (is (= [[:staged-param "s" :level]] (keys (get-in (model rt) [:control :edit-history :generations]))))
      (is (= :unknown-instance (:code (apply-at! rt {:op :param/set :instance-id "stage:s" :param :level :value 0.9} 30)))
          "no reserved instance ID exists")
      (is (not-any? #(str/includes? (pr-str %) "stage:") (show/staged-cues rt)))
      (testing "agents cannot stage"
        (let [agent {:adapter :agent :session-id "run-1"}]
          (is (= :agent-refused (:code (apply-at! rt {:op :cue-stage/open :cue-id :ice :stage-id "a" :source agent} 40))))
          (is (= :agent-refused (:code (apply-at! rt {:op :param/set :stage-id "s" :param :level :value 0.5 :source agent} 41))))
          (let [reply (future (show/dispatch-tool! rt "set_parameter" {:stage-id "s" :param "level" :value 0.5}
                                                   {:command-id "agent-set" :epoch 0}))]
            (while (not (realized? reply)) (show/tick! rt 42) (Thread/sleep 5))
            (is (= :agent-refused (:code @reply)) "the agent tool boundary reaches the same refusal"))))
      (finally (show/stop! rt)))))

(deftest a-revision-narrowing-bounds-clamps-the-staged-value-and-launches-with-it
  (let [rt (runtime)]
    (try
      (apply-at! rt {:op :cue-stage/open :cue-id :ember :stage-id "s"} 0)
      (apply-at! rt {:op :cue-stage/edit :stage-id "s" :param :level :value 0.6 :command-id "e"} 10 operator)
      (is (= :accepted (:status (revise-ember! rt [0.0 0.5] 0.2 20))))
      (let [stage (staged rt "s")]
        (is (= [0.0 0.5] (get-in stage [:schema :level :bounds])))
        (is (= 0.5 (get-in stage [:params :level])))
        (is (= {:effect-version 2 :adjusted {:level [0.6 0.5]}} (:revised stage))))
      (testing "the clamp advanced the edit's generation, so its undo conflicts"
        (is (= :history-conflict (:code (undo! rt 25)))))
      (is (= :invalid-params (:code (apply-at! rt {:op :cue-stage/edit :stage-id "s" :param :level :value 0.9
                                                   :command-id "e2"} 30 operator))))
      (is (some? (:revised (staged rt "s"))) "a refused edit leaves the note")
      (is (= :accepted (:status (apply-at! rt {:op :cue-stage/edit :stage-id "s" :param :level :value 0.45
                                               :command-id "e3"} 35 operator))))
      (is (nil? (:revised (staged rt "s"))) "the next edit clears it")
      (let [ack (apply-at! rt {:op :cue-stage/launch :stage-id "s" :command-id "go"} 40)]
        (is (= :accepted (:status ack)))
        (is (= [2 0.45] ((juxt :effect-version (comp :level :params)) (get-in (model rt) [:instances (:instance-id ack)])))))
      (finally (show/stop! rt)))))

(deftest a-revision-that-changes-a-waiting-launch-withdraws-it
  (let [rt (runtime)]
    (try
      (apply-at! rt {:op :cue-stage/open :cue-id :ember :stage-id "s"} 0)
      (apply-at! rt {:op :cue-stage/edit :stage-id "s" :param :level :value 0.6 :command-id "e"} 10 operator)
      (is (= :pending (:status (apply-at! rt {:op :cue-stage/launch :stage-id "s" :grid {:bars 1} :command-id "go"} 15))))
      (is (= :accepted (:status (revise-ember! rt [0.0 0.5] 0.2 20))))
      (let [stage (staged rt "s")]
        (is (= [:staged 0.5] ((juxt :status (comp :level :params)) stage)) "back to editing with the clamped value")
        (is (= {:command-id "go" :status :cancelled :reason :stage-revised}
               (select-keys (:last-launch stage) [:command-id :status :reason])))
        (is (= {:level [0.6 0.5]} (get-in stage [:revised :adjusted]))))
      (is (= :cancelled (:status (show/quantized-cue-status rt "go"))))
      (show/tick! rt 2100)
      (is (empty? (:instances (model rt))) "the operator's 0.6 never fires as 0.5")
      (testing "the operator confirms by launching again, which clears the note"
        (let [ack (apply-at! rt {:op :cue-stage/launch :stage-id "s" :command-id "go-2"} 2200)]
          (is (= :accepted (:status ack)))
          (is (= [2 0.5] ((juxt :effect-version (comp :level :params)) (get-in (model rt) [:instances (:instance-id ack)]))))))
      (finally (show/stop! rt)))))

(deftest an-external-edit-target-is-refused
  (let [rt (runtime)
        direct (fn [stage-id gesture-id time]
                 (apply-at! rt {:op :gesture/begin :edit-target [:staged-param stage-id :level] :control-id "c"
                                :gesture-id gesture-id} time operator))]
    (try
      (apply-at! rt {:op :cue-stage/open :cue-id :ember :stage-id "s"} 0)
      (is (= :invalid-command (:code (direct "s" "g1" 1))))
      (is (= :pending (:status (apply-at! rt {:op :cue-stage/launch :stage-id "s" :grid {:bars 1} :command-id "go"} 2))))
      (is (= :invalid-command (:code (direct "s" "g2" 3))) "a waiting stage cannot be edited that way either")
      (is (= :invalid-command (:code (apply-at! rt {:op :param/set :edit-target [:staged-param "s" :level] :value 0.2} 4))))
      (is (= {} (get-in (model rt) [:control :edit-history :open] {})))
      (is (= 0.7 (get-in (staged rt "s") [:params :level])))
      (finally (show/stop! rt)))))

(deftest a-deleted-cue-makes-the-stage-stale-and-withdraws-its-launch
  (let [rt (runtime)
        delete-ember! (fn [time]
                        (doseq [[_ page] (:pages (model rt))]
                          (apply-at! rt {:op :page/put :page (update page :cells #(into {} (remove (fn [[_ c]] (= :ember c))) %))}
                                     time))
                        (apply-at! rt {:op :cue/delete :cue-id :ember} time))]
    (try
      (apply-at! rt {:op :cue-stage/open :cue-id :ember :stage-id "s"} 0)
      (apply-at! rt {:op :cue-stage/edit :stage-id "s" :param :level :value 0.3 :command-id "e"} 1 operator)
      (is (= :pending (:status (apply-at! rt {:op :cue-stage/launch :stage-id "s" :grid {:bars 1} :command-id "go"} 2))))
      (is (= :accepted (:status (delete-ember! 3))))
      (let [stage (staged rt "s")]
        (is (= :stale (:status stage)))
        (is (= :cue-deleted (get-in stage [:stale :code])))
        (is (= {:command-id "go" :status :cancelled :reason :stage-stale}
               (select-keys (:last-launch stage) [:command-id :status :reason]))))
      (is (= :cancelled (:status (show/quantized-cue-status rt "go"))))
      (is (= "The staged cue is stale: The cue was deleted."
             (:target-unavailable-reason (last (:nodes (history rt))))))
      (is (= :cue-deleted (:code (apply-at! rt {:op :cue-stage/launch :stage-id "s" :command-id "again"} 4))))
      (is (= :stage-unavailable (:code (apply-at! rt {:op :cue-stage/edit :stage-id "s" :param :level :value 0.4
                                                      :command-id "e2"} 5 operator))))
      (show/tick! rt 2100)
      (is (empty? (:instances (model rt))))
      (is (= :accepted (:status (apply-at! rt {:op :cue-stage/cancel :stage-id "s"} 2200))))
      (is (empty? (rest (:nodes (history rt)))))
      (finally (show/stop! rt)))))

(deftest a-missed-or-refused-grid-launch-returns-the-stage-with-its-reason
  (testing "a transport jump misses the boundary"
    (let [rt (runtime)
          manager (connections/sync (:connections rt))
          clock! (fn [position at discontinuity?]
                   (session/apply-clock! manager nil {:type :clock :source-id :midi-clock :time-ms at :beat position :bpm 120
                                                      :playing? true :confidence 1 :discontinuity? discontinuity?}))]
      (try
        (sync/select-source! manager :midi-clock)
        (clock! 0.25 125 false)
        (apply-at! rt {:op :cue-stage/open :cue-id :ember :stage-id "s"} 125)
        (is (= :pending (:status (apply-at! rt {:op :cue-stage/launch :stage-id "s" :grid {:beats 1} :command-id "go"} 125))))
        (clock! 32 150 true)
        (show/tick! rt 150)
        (let [stage (staged rt "s")]
          (is (= :staged (:status stage)))
          (is (= {:command-id "go" :status :missed :reason :clock-changed}
                 (select-keys (:last-launch stage) [:command-id :status :reason]))))
        (is (empty? (:instances (model rt))))
        (is (= :accepted (:status (apply-at! rt {:op :cue-stage/launch :stage-id "s" :command-id "go-2"} 160))))
        (finally (show/stop! rt)))))
  (testing "a launch the show refuses when it fires"
    (let [rt (runtime)]
      (try
        (apply-at! rt {:op :cue-stage/open :cue-id :ember :stage-id "s" :launch {:priority "high"}} 0)
        (is (= :pending (:status (apply-at! rt {:op :cue-stage/launch :stage-id "s" :grid {:beats 1} :command-id "go"} 10))))
        (show/tick! rt 600)
        (let [stage (staged rt "s")]
          (is (= :staged (:status stage)))
          (is (= {:command-id "go" :status :missed :reason :launch-rejected}
                 (select-keys (:last-launch stage) [:command-id :status :reason])))
          (is (keyword? (get-in stage [:last-launch :code]))))
        (is (empty? (:instances (model rt))))
        (finally (show/stop! rt))))))

(deftest retries-are-matched-by-stage-and-value
  (let [rt (runtime)]
    (try
      (apply-at! rt {:op :cue-stage/open :cue-id :ember :stage-id "a"} 0)
      (apply-at! rt {:op :cue-stage/open :cue-id :ice :stage-id "b"} 0)
      (is (= :pending (:status (apply-at! rt {:op :cue-stage/launch :stage-id "a" :grid {:bars 1} :command-id "go"} 10))))
      (is (= {:status :pending :duplicate? true :stage-id "a"}
             (select-keys (apply-at! rt {:op :cue-stage/launch :stage-id "a" :grid {:bars 1} :command-id "go"} 11)
                          [:status :duplicate? :stage-id])))
      (is (= :command-id-conflict (:code (apply-at! rt {:op :cue-stage/launch :stage-id "b" :command-id "go"} 20))))
      (is (= :staged (:status (staged rt "b"))))
      (testing "a plain launch cannot take over a stage's pending launch"
        (is (= :rejected (:status (apply-at! rt {:op :cue/launch :cue-id :ice :stage-id "a" :command-id "go"} 30))))
        (is (= :scheduled (:status (staged rt "a")))))
      (apply-at! rt {:op :cue-stage/edit :stage-id "b" :param :level :value 0.3 :command-id "e"} 40 operator)
      (is (= :accepted (:status (apply-at! rt {:op :cue-stage/edit :stage-id "b" :param :level :value 0.3 :command-id "e"}
                                           41 operator))))
      (is (= :command-id-conflict (:code (apply-at! rt {:op :cue-stage/edit :stage-id "b" :param :level :value 0.35
                                                        :command-id "e"} 42 operator))))
      (is (= 0.3 (get-in (staged rt "b") [:params :level])))
      (finally (show/stop! rt)))))

(deftest older-steps-undo-while-a-grid-launch-waits
  (let [rt (runtime)]
    (try
      (let [id (:instance-id (apply-at! rt {:op :cue/launch :cue-id :ice :launch {:params {:level 0.2}}} 0))]
        (turn! rt id 0.4 "g1" 1)                                                           ; step 1
        (apply-at! rt {:op :cue-stage/open :cue-id :ember :stage-id "s"} 2)
        (apply-at! rt {:op :cue-stage/edit :stage-id "s" :param :level :value 0.3 :command-id "e"} 3 operator) ; step 2
        (is (= :pending (:status (apply-at! rt {:op :cue-stage/launch :stage-id "s" :grid {:bars 1} :command-id "go"} 4))))
        (is (= 1 (:history-cursor (undo! rt 5))) "the staged step undoes while its launch waits")
        (is (= 0.7 (get-in (staged rt "s") [:params :level])))
        (is (= 0 (:history-cursor (undo! rt 6))) "and the step before it")
        (is (= 0.2 (level rt id)))
        (is (= :stage-unavailable (:code (apply-at! rt {:op :cue-stage/edit :stage-id "s" :param :level :value 0.5
                                                        :command-id "late"} 7 operator)))
            "direct edits stay frozen")
        (is (= 1 (:history-cursor (redo! rt 1 8))))
        (show/tick! rt 2100)
        (let [launched (some #(when (= :ember (:cue-id %)) %) (vals (:instances (model rt))))]
          (is (= 0.7 (get-in launched [:params :level])) "the launch reads the stage's value when it fires")
          (is (= 2 (:history-cursor (redo! rt 2 2200))))
          (is (= 0.3 (level rt (:id launched))))))
      (finally (show/stop! rt)))))

(deftest cancel-splices-an-interleaved-tree-and-redo-still-reaches-the-reattached-step
  (let [rt (runtime)]
    (try
      (let [id (:instance-id (apply-at! rt {:op :cue/launch :cue-id :ice :launch {:params {:level 0.2}}} 0))]
        (turn! rt id 0.4 "g1" 1)                                                           ; 1
        (apply-at! rt {:op :cue-stage/open :cue-id :ember :stage-id "s"} 2)
        (apply-at! rt {:op :cue-stage/edit :stage-id "s" :param :level :value 0.3 :command-id "e"} 3 operator) ; 2
        (turn! rt id 0.5 "g2" 4)                                                           ; 3, child of 2
        (undo! rt 5) (undo! rt 6)
        (turn! rt id 0.6 "g3" 7)                                                           ; 4, from 1
        (is (= [nil 0 1 2 1] (mapv :parent-id (:nodes (history rt)))))
        (is (= :accepted (:status (apply-at! rt {:op :cue-stage/cancel :stage-id "s"} 8))))
        (is (= {0 nil 1 0 3 1 4 1} (into {} (map (juxt :id :parent-id)) (:nodes (history rt)))))
        (is (= 4 (:current-id (history rt))))
        (is (= 1 (:history-cursor (undo! rt 9))))
        (is (= 3 (:history-cursor (redo! rt 3 10))))
        (is (= 0.5 (level rt id))))
      (finally (show/stop! rt)))))

(defn- studio-command [app command]
  (let [response (app {:request-method :post :uri "/api/command"
                       :headers {"host" "localhost" "content-type" "application/json"
                                 "x-tolglow-form" "studio-v1" "accept" "application/transit+json"}
                       :body (ByteArrayInputStream. (.getBytes ^String (json/write-str command) "UTF-8"))})]
    [(:status response) (transit/decode (:body response))]))

(defn- post! [rt app command time]
  (let [reply (future (studio-command app command))
        deadline (+ (System/nanoTime) 3000000000)]
    (try
      (loop []
        (cond
          (realized? reply) @reply
          (< (System/nanoTime) deadline) (do (show/tick! rt time) (Thread/sleep 5) (recur))
          :else (throw (ex-info "Studio command timed out" {:command command}))))
      (finally (future-cancel reply)))))

(deftest the-studio-staged-edit-route-assigns-the-source-and-streams-outcomes
  (let [rt (runtime)
        local (access/listener-handler (studio/handler rt) {})
        spoofed {:adapter "repl" :origin "live-playback" :session-id "tab-1" :device-id "push"}
        envelope {:epoch 0 :runtime-id (:id rt) :source spoofed}]
    (try
      (apply-at! rt {:op :cue-stage/open :cue-id :ember :stage-id "s"} 0)
      (let [[status body] (post! rt local (merge envelope {:op "cue-stage/edit" :stage-id "s" :param "level" :value 0.35
                                                           :command-id "edit" :instance-id "0:9" :launch {:mode "held"}})
                                 10)]
        (is (= [200 :accepted] [status (:status body)])))
      (let [entry (get-in (model rt) [:control :history "edit" :command])]
        (is (= {:adapter :web :session-id "tab-1"} (:source entry)))
        (is (not-any? #(contains? entry %) [:instance-id :launch])))
      (let [[status body] (post! rt local (merge envelope {:op "cue-stage/edit" :stage-id "s" :param "level" :value 7
                                                           :command-id "too-loud"}) 20)]
        (is (= [422 :rejected] [status (:status body)])))
      (is (some #(and (= "too-loud" (:command-id %)) (= :rejected (:status %)))
                (:history-command-outcomes (show/snapshot rt)))
          "a refused staged edit reaches the history outcome stream")
      (finally (show/stop! rt)))))
