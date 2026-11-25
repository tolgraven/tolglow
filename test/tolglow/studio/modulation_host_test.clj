(ns tolglow.studio.modulation-host-test
  (:require [afterglow.control.codecs.push-2 :as push-codec]
            [afterglow.control.devices.session :as device-session]
            [afterglow.control.model :as control]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [tolglow.operator-history :as operator-history]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.studio.access :as access]
            [tolglow.transit :as transit])
  (:import [java.io StringReader]))

(defn- apply-at!
  ([runtime type payload time] (apply-at! runtime type payload time nil))
  ([runtime type payload time authority]
   (let [receipt (show/submit! runtime type payload authority)]
     (show/tick! runtime time)
     (deref receipt 1000 ::missing))))

(defn- turn! [runtime instance-id gesture-id value time authority]
  (let [gesture {:instance-id instance-id :param :level :control-id "private-control" :gesture-id gesture-id}]
    (is (= :accepted (:status (apply-at! runtime :command (assoc gesture :op :gesture/begin) time authority))))
    (is (= :accepted (:status (apply-at! runtime :command (assoc gesture :op :gesture/sample :sequence 1 :value value)
                                        (inc time) authority))))
    (apply-at! runtime :command (assoc gesture :op :gesture/end :last-sequence 1) (+ time 2) authority)))

(defn- history-request [app method payload headers]
  (let [media "application/transit+json"
        response (app {:request-method method :uri (if (= :get method) "/api/state" "/api/command")
                       :headers (merge {"host" "localhost" "accept" media "content-type" media
                                        "x-tolglow-form" "studio-v1"} headers)
                       :body (when payload (StringReader. (transit/encode payload)))})]
    (cond-> response
      (str/starts-with? (get-in response [:headers "Content-Type"] "") media)
      (update :body transit/decode))))

(defn- post-history! [runtime app payload headers time]
  (let [reply (future (history-request app :post payload headers))
        deadline (+ (System/nanoTime) 3000000000)]
    (try
      (loop []
        (cond
          (realized? reply) @reply
          (< (System/nanoTime) deadline) (do (show/tick! runtime time) (Thread/sleep 5) (recur))
          :else (throw (ex-info "Studio history request timed out" {:op (:op payload)}))))
      (finally (future-cancel reply)))))

(deftest studio-history-commands-share-committed-travel-and-listener-authority
  (let [runtime (show/create-runtime {:workers 1 :workspace-options nil})
        app (studio/handler runtime)
        token "test-only-studio-history-credential"
        signed (access/authenticated app token)
        local (access/listener-handler app {})
        login {"authorization" (str "Bearer " token)}
        push {:operator-id "push-2/history-fixture"}
        scope {:runtime-id (:id runtime) :expected-epoch 0}]
    (try
      (let [id (:instance-id (apply-at! runtime :command
                                       {:op :cue/launch :cue-id :ember :launch {:params {:level 0.2}}} 0))
            level #(get-in @(:state runtime) [:model :instances id :params :level])
            cursor #(:current-id (show/operator-history runtime))
            undo (merge scope {:op :history/undo :expected-cursor 2 :expected-history-position 2
                               :command-id "studio-undo"})
            forged (assoc undo :operator-id "forged" :authority {:operator-id "forged"}
                          ::access/operator-authority {:operator-id "forged"}
                          :source {:adapter :forged :session-id "forged"} :epoch 99 :show-id :forged)]
        (is (= 1 (:history-cursor (turn! runtime id "first" 0.4 5 push))))
        (is (= 2 (:history-cursor (turn! runtime id "second" 0.7 10 push))))
        (testing "Preview reads committed history and supplies no authority to command payloads"
          (let [before @(:state runtime)
                view (:body (history-request app :get nil {}))]
            (is (= (show/operator-history runtime) (:operator-history view)))
            (is (= [2 0.7] [(cursor) (level)]))
            (is (= 401 (:status (history-request signed :post forged {}))))
            (is (= :operator-required (get-in (history-request app :post forged {}) [:body :code])))
            (is (= 403 (:status (history-request local :post undo {"origin" "https://elsewhere.test"}))))
            (is (identical? before @(:state runtime)))))
        (testing "Signed browser sessions retain server-assigned authority across retries"
          (let [session (history-request signed :get nil login)
                cookie (first (str/split (get-in session [:headers "Set-Cookie"]) #";"))
                result (post-history! runtime signed forged {"cookie" cookie} 20)
                entry #(get-in @(:state runtime) [:model :control :history "studio-undo"])]
            (is (= 200 (:status result)))
            (is (= [:accepted 1 3] ((juxt :status :history-cursor :history-position) (:body result))))
            (is (= [1 0.4] [(cursor) (level)]))
            (is (re-find #"^studio/" (:operator-id (entry))))
            (is (= {:adapter :studio :session-id "studio"} (get-in (entry) [:command :source])))
            (is (not (re-find #"forged" (pr-str (entry)))))
            (is (not-any? #(contains? (:command (entry)) %) [:operator-id :authority ::access/operator-authority]))
            (is (= (:body result) (:body (post-history! runtime signed undo login 25))))
            (is (= [1 0.4] [(cursor) (level)]))))
        (testing "The default listener supports undo, explicit redo and alternate branch selection"
          (is (= 3 (:history-cursor (turn! runtime id "alternate" 0.5 30 push))))
          (let [select (merge scope {:op :history/select :expected-cursor 3 :expected-history-position 4
                                     :node-id 2 :command-id "studio-select"})
                submit #(get (post-history! runtime local % {} 40) :body)]
            (doseq [field [:runtime-id :expected-epoch :expected-cursor :expected-history-position :node-id]]
              (is (= :invalid-history-command (get-in (history-request local :post (dissoc select field) {}) [:body :code]))))
            (doseq [stale [(assoc select :runtime-id "retired-runtime") (assoc select :expected-epoch 1)]]
              (is (= :rejected (:status (submit stale)))))
            (is (= :stale-history-cursor (:code (submit (assoc select :expected-cursor 1)))))
            (is (= [3 0.5] [(cursor) (level)]))
            (is (= 2 (:history-cursor (submit select))))
            (is (= [2 0.7] [(cursor) (level)]))
            (is (= 1 (:history-cursor (submit (merge scope {:op :history/undo :expected-cursor 2
                                                           :expected-history-position 5})))))
            (is (= 3 (:history-cursor (submit (merge scope {:op :history/redo :expected-cursor 1
                                                           :expected-history-position 6 :node-id 3})))))
            (is (= :stale-history-position
                   (:code (submit (assoc select :command-id "stale-position")))))
            (is (= [3 0.5] [(cursor) (level)]))
            (is (= [nil 0 1 1] (mapv :parent-id (:nodes (show/operator-history runtime)))))
            (apply-at! runtime :command {:op :param/set :instance-id id :param :level :value 0.8} 45)
            (is (= :history-conflict (:code (:body (post-history! runtime local
                                                    (merge scope {:op :history/undo :expected-cursor 3
                                                                  :expected-history-position 7}) {} 50)))))
            (is (= [3 0.8] [(cursor) (level)])))))
      (finally (show/stop! runtime)))))

(deftest snapshots-and-repl-share-committed-operator-history
  (let [runtime (show/create-runtime {:workers 1 :workspace-options nil})
        studio {:operator-id "studio/private-session"}
        push {:operator-id "push-2/private-port/private-connection"}]
    (try
      (let [initial (show/snapshot runtime)
            identity (mapv initial [:runtime-id :show-id :epoch])
            launch! (fn [cue time]
                      (:instance-id (apply-at! runtime :command
                                               {:op :cue/launch :cue-id cue :launch {:params {:level 0.2}}} time)))
            ember (launch! :ember 0)
            ice (launch! :ice 1)]
        (is (= {:identity identity :current-id nil :position 0 :nodes []
                :retention {:limit control/edit-history-limit :retained 0 :pruned? false}}
               (:operator-history initial)))
        (is (= 1 (:history-cursor (turn! runtime ember "private-ember-turn" 0.4 5 push))))
        (is (= 2 (:history-cursor (turn! runtime ice "private-ice-turn" 0.7 10 studio))))
        (is (= 1 (:history-cursor (apply-at! runtime :command {:op :history/undo :expected-cursor 2
                                                             :expected-history-position 2} 15 studio))))
        (is (= 3 (:history-cursor (turn! runtime ice "private-alternate" 0.5 20 {:operator-id :caller}))))
        (let [committed @(:state runtime)
              projection (show/operator-history runtime)
              [root first-edit second-edit alternate] (:nodes projection)]
          (is (= projection (:operator-history (show/snapshot runtime))))
          (is (identical? committed @(:state runtime)))
          (is (= identity (:identity projection)))
          (is (= 3 (:current-id projection)))
          (is (= 4 (:position projection)))
          (is (= {:limit control/edit-history-limit :retained 4 :pruned? false} (:retention projection)))
          (is (= {:id 0 :parent-id nil :sequence 0} root))
          (is (= {:id 1 :parent-id 0 :sequence 1 :actor "Push 2" :parameter :level :before 0.2 :after 0.4
                  :cue-id :ember :cue-label "Ember" :effect-id :ember :effect-label "Ember"}
                 first-edit))
          (is (= [2 1 "Studio" :ice "Ice" :ice "Ice"]
                 (mapv second-edit [:id :parent-id :actor :cue-id :cue-label :effect-id :effect-label])))
          (is (= [3 1 "Operator"] (mapv alternate [:id :parent-id :actor])))
          (is (not (re-find #"private-" (pr-str projection)))))
        (is (= :accepted (:status (apply-at! runtime :command {:op :instance/end :instance-id ember :force? true} 25))))
        (let [[root first-edit second-edit alternate] (:nodes (show/operator-history runtime))]
          (is (string? (:unavailable-reason root)))
          (is (string? (:target-unavailable-reason first-edit)))
          (is (= [:level :ember "Ember" :ember "Ember"]
                 (mapv first-edit [:parameter :cue-id :cue-label :effect-id :effect-label])))
          (is (every? #(nil? (:unavailable-reason %)) [first-edit second-edit alternate])))
        (is (= 1 (:history-cursor (apply-at! runtime :command {:op :history/undo :expected-cursor 3
                                                             :expected-history-position 4} 30 studio))))
        (is (= :target-unavailable
               (:code (apply-at! runtime :command {:op :history/select :expected-cursor 1
                                                 :expected-history-position 5 :node-id 0} 35 studio))))
        (let [before (show/operator-history runtime)
              source (show/export-show-source runtime)
              restored (apply-at! runtime :import {:source source} 40)
              after (show/operator-history runtime)]
          (is (= :accepted (:status restored)))
          (is (= (update identity 2 inc) (:identity after)))
          (is (= 1 (:current-id after)))
          (is (= (dissoc before :identity) (dissoc after :identity)))))
      (finally (show/stop! runtime)))))

(deftest operator-history-reports-retention-after-pruning
  (let [model (assoc-in (control/new-show) [:control :edit-history]
                        {:limit 2 :cursor 9 :next-id 10
                         :nodes {7 {:parent nil}
                                 9 {:parent 7 :target [:unavailable-instance-param "0:1" :level 0]
                                    :actor :caller :control-id :slider :before 0.2 :after 0.6 :generation 0}}})
        projection (operator-history/project "runtime" model)]
    (is (= {:limit 2 :retained 2 :pruned? true} (:retention projection)))
    (is (= [7 9] (mapv :id (:nodes projection))))
    (is (string? (get-in projection [:nodes 0 :unavailable-reason])))))

(deftest operator-edits-publish-only-the-committed-parameter-changes
  (let [runtime (show/create-runtime {:workspace-options nil})
        authority {:operator-id "test-operator"}
        observed (atom [])]
    (try
      (show/attach-observer! runtime :history-test
                             (fn [] {:tracing? (constantly false)
                                     :events! (fn [_ events _ _] (swap! observed into events))
                                     :frame! (fn [_]) :close! (fn [])}))
      (let [id (:instance-id (apply-at! runtime :command {:op :cue/launch :cue-id :ember} 0))
            level #(get-in @(:state runtime) [:model :instances id :params :level])
            emitted #(first (get-in (show/snapshot runtime) [:output :values :head-1 :emitted-color]))
            initial (level)
            held? #(some? (get-in @(:state runtime) [:model :modulation :holds id :level]))
            gesture {:instance-id id :param :level :control-id :encoder-0 :gesture-id "turn"}
            begin (assoc gesture :op :gesture/begin :command-id "begin")
            sample (assoc gesture :op :gesture/sample :command-id "sample" :sequence 1 :value 0.9)
            end (assoc gesture :op :gesture/end :command-id "end" :last-sequence 1)]
        (is (= :operator-required (:code (apply-at! runtime :command
                                                   (assoc begin :authority authority :operator-id "forged") 20))))
        (is (= :accepted (:status (apply-at! runtime :modulation-start
                                            {:id :rhythm :seed 42 :every {:beats 1/4} :params [:level]
                                             :select {:instance-ids [id]} :ranges {:level [0.1 0.4]}} 100))))
        (is (= :accepted (:status (apply-at! runtime :command begin 100 authority))))
        (let [ack (apply-at! runtime :command sample 120 authority)]
          (is (= :accepted (:status ack)))
          (is (= 0.9 (emitted)))
          (is (held?))
          (apply-at! runtime :modulation-resume {:instance-id id :params [:level]} 150)
          (is (not (held?)))
          (is (= ack (apply-at! runtime :command sample 160 authority)))
          (is (not (held?)) "A replay must leave the resumed modulation alone")
          (let [matching (filter #(= "sample" (get-in % [:data :command-id])) @observed)]
            (is (= [:command/accepted] (mapv :type matching)))
            (is (= "test-operator" (get-in (first matching) [:emitter :id])))))
        (is (= 1 (:history-cursor (apply-at! runtime :command end 180 authority))))
        (is (= 0 (:history-cursor (apply-at! runtime :command
                                             {:op :history/undo :expected-cursor 1
                                              :expected-history-position 1} 200 authority))))
        (is (== initial (level) (emitted)))
        (is (held?)))
      (finally (show/stop! runtime)))))

(deftest cancelled-gesture-settles-only-its-own-modulation-hold
  (doseq [[label prior-deadline [intervening-op changes] expected-hold]
          [["active modulation resumes" nil nil nil]
           ["the prior finite hold keeps its deadline" 1.5 nil {:until-beat 1.5}]
           ["the prior hold expired during the gesture" 0.5 nil nil]
           ["an explicit permanent hold supersedes the gesture" 1.5 [:modulation-hold] {:until-beat nil}]
           ["an explicit finite hold supersedes the gesture" nil
            [:modulation-hold {:until-beat 1.6}] {:until-beat 1.6}]
           ["an explicit resume supersedes the gesture" 1.5 [:modulation-resume] nil]]]
    (testing label
      (let [runtime (show/create-runtime {:workers 1 :workspace-options nil})
            authority {:operator-id "test-operator"}]
        (try
          (let [id (:instance-id (apply-at! runtime :command {:op :cue/launch :cue-id :ember} 0))
                model #(:model @(:state runtime))
                level #(get-in (model) [:instances id :params :level])
                hold #(get-in (model) [:modulation :holds id :level])
                controller #(first (get-in (model) [:modulation :controllers]))
                emitted #(first (get-in (show/snapshot runtime) [:output :values :head-1 :emitted-color]))
                target {:instance-id id :params [:level]}
                gesture {:instance-id id :param :level :control-id :encoder-0 :gesture-id "cancel-turn"}
                cancel (assoc gesture :op :gesture/cancel :command-id "cancel-turn")
                canonical (level)]
            (is (= :accepted (:status (apply-at! runtime :modulation-start
                                                {:id :rhythm :seed 42 :every {:beats 1/4} :params [:level]
                                                 :select {:instance-ids [id]} :ranges {:level [0.1 0.4]}} 100))))
            (is (<= 0.1 (emitted) 0.4))
            (when prior-deadline
              (apply-at! runtime :modulation-hold (assoc target :until-beat prior-deadline) 110))
            (is (= :accepted (:status (apply-at! runtime :command (assoc gesture :op :gesture/begin) 120 authority))))
            (is (= :accepted (:status (apply-at! runtime :command
                                                (assoc gesture :op :gesture/sample :sequence 1 :value 0.9) 125 authority))))
            (is (= [0.9 0.9] [(level) (emitted)]))
            (is (some? (hold)))
            (is (= :unknown-gesture (:code (apply-at! runtime :command (dissoc cancel :command-id) 130
                                                     {:operator-id "different-operator"}))))
            (is (= [0.9 0.9] [(level) (emitted)]))
            (is (= {:until-beat nil} (get-in (show/snapshot runtime) [:modulation :holds id :level])))
            (is (= {:until-beat nil} (get-in (show/export-show runtime) [:model :modulation :holds id :level])))
            (doseq [time [230 355 480]]
              (show/tick! runtime time))
            (is (> (:ticks (controller)) 3))
            (when intervening-op
              (is (= :accepted (:status (apply-at! runtime intervening-op (merge target changes) 490)))))
            (show/tick! runtime 610)
            (let [before-cancel (controller)
                  ack (apply-at! runtime :command cancel 620 authority)]
              (is (= [:accepted 0] ((juxt :status :history-cursor) ack)))
              (is (empty? (get-in (model) [:control :edit-history :open])))
              (is (= canonical (level)))
              (is (= expected-hold (hold)))
              (is (= before-cancel (controller)) "Cancel leaves schedule, RNG and existing cells alone")
              (show/tick! runtime 735)
              (if expected-hold
                (is (= canonical (emitted)))
                (is (<= 0.1 (emitted) 0.4)))
              (show/tick! runtime 860)
              (if (and expected-hold (nil? (:until-beat expected-hold)))
                (is (= canonical (emitted)))
                (is (<= 0.1 (emitted) 0.4)))
              (apply-at! runtime :modulation-hold (assoc target :until-beat 2.5) 880)
              (is (= ack (apply-at! runtime :command cancel 890 authority)))
              (is (= {:until-beat 2.5} (hold)))
              (is (= 1 (:history-cursor (turn! runtime id "committed-turn" 0.8 900 authority))))
              (is (= {:until-beat nil} (hold)))))
          (finally (show/stop! runtime)))))))

(deftest captured-push-encoder-updates-become-one-operator-edit
  (let [runtime (show/start! {:workspace-options nil} (constantly nil))
        input (atom nil)
        midi {:open! (fn [receive!] (reset! input receive!) :capture)
              :send! (fn [_ _]) :close! (fn [_])}]
    (try
      (let [device (show/connect-device! runtime :push-2
                                         {:session-id "history-push" :midi midi
                                          :operator-gestures? true :schedule! nil})]
        (doseq [index (range 128)]
          (@input (assoc (push-codec/palette-write 0 index [0 0 0 0]) 6 4)))
        (let [model #(:model @(:state runtime))
              history #(get-in (model) [:control :edit-history])
              [x y] (some (fn [[coordinate cue]] (when (= :ember cue) coordinate))
                          (get-in (model) [:pages :main :cells]))
              _ (@input [0x90 (push-codec/pad-note x y) 127])
              instance-id (get-in (device-session/snapshot device) [:local :selected-instance])
              level #(get-in (model) [:instances instance-id :params :level])
              initial (level)]
          (is (number? initial))
          (@input [0x90 0 127])
          (dotimes [_ 5] (@input [0xb0 71 1]))
          (is (> (level) initial))
          (is (= 0 (:cursor (history))))
          (@input [0x80 0 0])
          (let [{:keys [target actor before after]} (get-in (history) [:nodes (:cursor (history))])]
            (is (= [:instance-param instance-id :level] target))
            (is (re-find #"^push-2/history-push/" actor))
            (is (= [initial (level)] [before after]))
            (is (= 2 (count (:nodes (history)))))
            (is (empty? (:open (history)))))
          (let [app (access/listener-handler (studio/handler runtime) {})
                scope {:runtime-id (:id runtime) :expected-epoch (:epoch (model))}
                submit #(get-in (history-request app :post (merge scope %) {}) [:body :history-cursor])
                edited (level)]
            (is (= [0 initial] [(submit {:op :history/undo :expected-cursor 1 :expected-history-position 1}) (level)]))
            (is (= [1 edited] [(submit {:op :history/redo :expected-cursor 0 :expected-history-position 2
                                      :node-id 1}) (level)])))
          (is (empty? (:errors (device-session/snapshot device))))))
      (finally (show/stop! runtime)))))

(deftest captured-push-3-encoder-restores-through-the-durable-owner-queue
  (with-redefs [show/device-command-wait-ms 0]
    (let [receive (atom nil)
          dimmer (atom nil)
          runtime (show/create-runtime {:workspace-options nil
                                        :sink {:send! (fn [packet _]
                                                        (reset! dimmer (bit-and 255 (aget ^bytes (get packet 1) 0))))}})
          options {:session-id "push3-owner" :user-mode? true :expression-mode :poly-aftertouch
                   ;; Synthetic User Mode capture from the paired device regression.
                   :layout {:encoder-touches {:level {:channel 2 :note 9}}
                            :encoders {:level {:channel 2 :controller 19}}
                            :buttons {:shift-button {:channel 4 :controller 23}}}
                   :button-bindings {:shift-button :shift}
                   :encoder-bindings {:level {:param :level :step 0.02}}
                   :schedule! nil
                   :display {:device :push-3 :input :view :open! (constantly :capture-display)
                             :render! (fn [_ _]) :close! (fn [_])}
                   :midi {:open! (fn [callback] (reset! receive callback) :capture)
                          :send! (fn [_ _] (throw (ex-info "Unexpected Push 3 MIDI output" {})))
                          :close! (fn [_])}}
          connect! #(show/connect-device! runtime :push-3 options)]
      (try
        (let [id (:instance-id (apply-at! runtime :command {:op :cue/launch :cue-id :ember} 0))
              _ (show/tick! runtime 100)
              device (connect!)
              input! (fn [time bytes] (@receive bytes) (show/tick! runtime (+ 100 time)))
              model #(:model @(:state runtime))
              level #(get-in (model) [:instances id :params :level])
              history #(get-in (model) [:control :edit-history])
              open #(get-in (history) [:open [:instance-param id :level]])
              terminals #(vals @(:push-gesture-ends runtime))
              initial (level)
              initial-dimmer @dimmer
              source {:adapter :push-3 :session-id "push3-owner"}]
          (is (= id (get-in (device-session/snapshot device) [:local :selected-instance])))
          (input! 10 [0xb0 57 127])
          (input! 20 [0x92 9 127])
          (let [{:keys [actor gesture-id control-id]} (open)
                forged {:op :gesture/cancel :instance-id id :param :level :gesture-id gesture-id
                        :control-id control-id :source source :temporary? true
                        :operator-id actor ::show/push-source source}]
            (is (re-find #"^push-3/push3-owner/" actor))
            (is (= :level control-id))
            (is (= :operator-required (:code (apply-at! runtime :command forged 125))))
            (is (= actor (:actor (open)))))
          (input! 30 [0xb2 19 12])
          (let [peak (level)
                {:keys [model frame-output]} (show/export-show runtime)]
            (is (> peak initial))
            (is (> @dimmer initial-dimmer))
            (is (= initial (get-in model [:instances id :params :level])))
            (is (nil? frame-output))
            (input! 40 [0xb0 57 0])
            (is (= peak (level))))
          (@receive [0xb2 19 1])
          (dotimes [_ 255] (show/submit! runtime {:op :master/set :value 1}))
          (is (= :rejected (:status (deref (show/submit! runtime {:op :master/set :value 1}) 0 nil))))
          (@receive [0x82 9 0])
          (@receive [0x92 9 0])
          (let [{:keys [payload authority reply]} (first (terminals))]
            (is (= 1 (count (terminals))))
            (is (= :gesture/cancel (:op payload)))
            (is (= source (:source payload) (::show/push-source authority)))
            (is (= (:actor (open)) (:operator-id authority)))
            (show/tick! runtime 150)
            (is (= :accepted (:status (deref reply 0 nil))))
            (is (= [initial initial-dimmer] [(level) @dimmer]))
            (is (empty? (:open (history))))
            (is (= [0 0] ((juxt :cursor :position) (history)))))
          (input! 55 [0xb4 23 127])
          (input! 60 [0x92 9 127])
          (is (false? (boolean (:temporary? (open)))))
          (input! 70 [0xb2 19 1])
          (input! 80 [0x82 9 0])
          (let [committed (level)]
            (is (< initial committed (+ initial 0.02)))
            (is (= [1 1] ((juxt :cursor :position) (history))))
            (input! 85 [0xb4 23 0])
            (input! 90 [0xb0 57 127])
            (input! 100 [0x92 9 127])
            (@receive [0xb2 19 10])
            (let [old-receive @receive old-actor (:actor (open))]
              (show/disconnect-device! runtime "push3-owner")
              (is (= :closed (:status (device-session/snapshot device))))
              (is (= :gesture/cancel (get-in (first (terminals)) [:payload :op])))
              (let [reply (:reply (first (terminals)))]
                (show/tick! runtime 210)
                (is (= :accepted (:status (deref reply 0 nil)))))
              (is (= committed (level)))
              (let [replacement (connect!)]
                (input! 120 [0xb0 57 127])
                (input! 130 [0x92 9 127])
                (is (not= old-actor (:actor (open))))
                (old-receive [0x82 9 0])
                (is (empty? (terminals)))
                (input! 140 [0x82 9 0])
                (is (= committed (level)))
                (is (empty? (:open (history))))
                (is (empty? (:errors (device-session/snapshot replacement)))))))
          (is (empty? (:errors (device-session/snapshot device)))))
        (finally (show/stop! runtime))))))

(defn- open-history-push! [runtime & [options]]
  (let [input (atom nil)
        midi {:open! (fn [receive!] (reset! input receive!) :capture)
              :send! (fn [_ _]) :close! (fn [_])}
        device (show/connect-device! runtime :push-2
                                     (merge {:session-id "manual-push" :midi midi
                                             :schedule! nil :timeout-ms 30000}
                                            options))]
    (doseq [index (range 128)]
      (@input (assoc (push-codec/palette-write 0 index [0 0 0 0]) 6 4)))
    {:device device :receive! @input}))

(deftest temporary-push-turn-restores-owned-value-and-output-with-a-full-ingress-queue
  (with-redefs [show/device-command-wait-ms 0]
    (let [dimmer (atom nil)
          runtime (show/create-runtime {:workspace-options nil
                                        :sink {:send! (fn [packet _]
                                                        (reset! dimmer (bit-and 255 (aget ^bytes (get packet 1) 0))))}})]
      (try
        (let [id (:instance-id (apply-at! runtime :command {:op :cue/launch :cue-id :ember} 0))
              {:keys [device receive!]} (open-history-push! runtime)
              model #(:model @(:state runtime))
              level #(get-in (model) [:instances id :params :level])
              hold #(get-in (model) [:modulation :holds id :level])
              history #(get-in (model) [:control :edit-history])
              readout #(vector (level) @dimmer)
              input! (fn [time bytes] (receive! bytes) (show/tick! runtime time))
              target {:instance-id id :params [:level]}]
          (apply-at! runtime :modulation-start {:id :rhythm :seed 42 :every {:beats 1/4} :params [:level]
                                               :select {:instance-ids [id]} :ranges {:level [0.1 0.4]}} 100)
          (apply-at! runtime :modulation-hold (assoc target :until-beat 4) 110)
          (let [[original original-byte :as baseline] (readout)]
            (is (pos? original-byte))
            (input! 120 [0xb0 57 127])
            (input! 130 [0x90 0 127])
            (input! 140 [0xb0 71 30])
            (is (> (level) original))
            (is (> @dimmer original-byte))
            (let [live (readout)
                  {:keys [model frame-output]} (show/export-show runtime)]
              (is (= original (get-in model [:instances id :params :level])))
              (is (= {:until-beat 4} (get-in model [:modulation :holds id :level])))
              (is (= [0 0] (mapv #(get-in model [:control :edit-history %]) [:cursor :position])))
              (is (nil? frame-output))
              (is (= live (readout))))
            (input! 150 [0xb0 57 0])
            (is (> (level) original))
            (receive! [0xb0 71 5])
            (dotimes [_ 255] (show/submit! runtime {:op :master/set :value 1}))
            (is (= :rejected (:status (deref (show/submit! runtime {:op :master/set :value 1}) 0 nil))))
            (receive! [0x80 0 0])
            (receive! [0x80 0 0])
            (let [{:keys [payload authority reply]} (first (vals @(:push-gesture-ends runtime)))]
              (is (= :gesture/cancel (:op payload)))
              (is (= :gesture-end-queued
                     (:code (deref (show/submit! runtime :command
                                                 (assoc payload :command-id "duplicate-cancel") authority) 0 nil))))
              (show/tick! runtime 160)
              (is (= :accepted (:status (deref reply 0 nil))))
              (is (= baseline (readout)))
              (is (= {:until-beat 4} (hold)))
              (is (= [0 0] ((juxt :cursor :position) (history))))
              (is (empty? (:open (history)))))
            (input! 170 [0x90 0 127])
            (input! 180 [0xb0 71 5])
            (input! 190 [0x80 0 0])
            (is (> (level) original))
            (is (> @dimmer original-byte))
            (is (= [1 1] ((juxt :cursor :position) (history))))
            (is (some? (:frame-output (show/export-show runtime))))
            (is (empty? (:errors (device-session/snapshot device))))))
        (finally (show/stop! runtime))))))

(deftest disconnect-queues-temporary-cancel-and-only-restores-the-owning-generation
  (with-redefs [show/device-command-wait-ms 0]
    (let [runtime (show/create-runtime {:workspace-options nil})
          options {:temporary-encoder-modifier 41}]
      (try
        (let [id (:instance-id (apply-at! runtime :command {:op :cue/launch :cue-id :ember} 0))
              level #(get-in @(:state runtime) [:model :instances id :params :level])
              history #(get-in @(:state runtime) [:model :control :edit-history])
              original (level)
              open-touch! (fn [time]
                            (let [{:keys [receive!] :as connected} (open-history-push! runtime options)]
                              (receive! [0xb0 41 127])
                              (receive! [0x90 0 127])
                              (show/tick! runtime time)
                              (receive! [0xb0 71 5])
                              connected))
              old (open-touch! 20)]
          ;; The sample is pending when the callback and transports are detached.
          (show/disconnect-device! runtime "manual-push")
          (let [reply (:reply (first (vals @(:push-gesture-ends runtime))))]
            (is (= :closed (:status (device-session/snapshot (:device old)))))
            (show/tick! runtime 30)
            (is (= :accepted (:status (deref reply 0 nil))))
            (is (= original (level)))
            (is (empty? (:open (history)))))
          (let [{:keys [receive!]} (open-touch! 40)]
            (show/tick! runtime 50)
            (is (> (level) original))
            ((:receive! old) [0x80 0 0])
            (is (empty? @(:push-gesture-ends runtime)))
            (apply-at! runtime :command {:op :param/set :instance-id id :param :level :value 0.4} 60)
            (receive! [0x80 0 0])
            (show/disconnect-device! runtime "manual-push")
            (let [reply (:reply (first (vals @(:push-gesture-ends runtime))))]
              (show/tick! runtime 70)
              (is (= {:status :accepted :discarded :history-conflict}
                     (select-keys (deref reply 0 nil) [:status :discarded]))))
            (is (= 0.4 (level)))
            (is (= [0 0] ((juxt :cursor :position) (history))))
            (is (empty? (:open (history))))))
        (finally (show/stop! runtime))))))

(deftest pending-push-touch-releases-through-a-full-command-queue
  (with-redefs [show/device-command-wait-ms 0]
    (let [runtime (show/create-runtime {:workspace-options nil})]
      (try
        (apply-at! runtime :command {:op :cue/launch :cue-id :ember} 0)
        (let [{:keys [device receive!]} (open-history-push! runtime)
              history #(get-in @(:state runtime) [:model :control :edit-history])
              ends #(vals @(:push-gesture-ends runtime))]
          (receive! [0x90 0 127])
          (is (= :pending (get-in (device-session/snapshot device) [:last-ack :status])))
          (is (empty? (:open (history))))
          (dotimes [_ 255] (show/submit! runtime {:op :master/set :value 1}))
          (is (= :rejected (:status (deref (show/submit! runtime {:op :master/set :value 1}) 0 nil))))
          (receive! [0x80 0 0])
          (receive! [0x80 0 0])
          (let [{:keys [payload authority reply]} (first (ends))]
            (is (= 1 (count (ends))))
            (is (not (realized? reply)))
            (is (= :gesture-end-queued
                   (:code (deref (show/submit! runtime :command
                                              (assoc payload :command-id "duplicated-end") authority) 0 nil))))
            (is (= :unknown-gesture
                   (:code (deref (show/submit! runtime :command
                                              (assoc payload :gesture-id "unrelated-touch") authority) 0 nil))))
            (is (= 1 (count (ends))))
            (show/tick! runtime 25)
            (is (= :accepted (:status (deref reply 0 nil))))
            (is (= 0 (:cursor (history))))
            (is (empty? (:open (history))))
            (is (empty? (ends))))
          (is (empty? (:errors (device-session/snapshot device))))
          (receive! [0x90 0 127])
          (show/disconnect-device! runtime "manual-push")
          (let [reply (:reply (first (ends)))]
            (is (= :closed (:status (device-session/snapshot device))))
            (show/tick! runtime 50)
            (is (= :accepted (:status (deref reply 0 nil))))
            (is (empty? (:open (history)))))
          (let [{:keys [receive!]} (open-history-push! runtime)
                _ (receive! [0x90 0 127])
                _ (receive! [0x80 0 0])
                reply (:reply (first (ends)))]
            (show/stop! runtime)
            (is (= :rejected (:status (deref reply 0 nil))))
            (is (empty? (ends)))))
        (finally (show/stop! runtime))))))

(deftest pending-push-sample-finishes-on-handoff-with-the-owner-sequence
  (with-redefs [show/device-command-wait-ms 0]
    (let [runtime (show/create-runtime {:workspace-options nil})]
      (try
        (let [id (:instance-id (apply-at! runtime :command {:op :cue/launch :cue-id :ember} 0))
              {:keys [device receive!]} (open-history-push! runtime)
              model #(:model @(:state runtime))
              history #(get-in (model) [:control :edit-history])
              target [:instance-param id :level]
              gesture #(get-in (history) [:open target])
              level #(get-in (model) [:instances id :params :level])
              initial (level)]
          (receive! [0x90 0 127])
          (show/tick! runtime 25)
          (receive! [0xb0 71 1])
          (show/tick! runtime 50)
          (is (= 1 (:last-sequence (gesture))))
          (let [{:keys [actor control-id gesture-id]} (gesture)
                end {:op :gesture/end :instance-id id :param :level :control-id control-id :gesture-id gesture-id
                     :last-sequence 0 :source {:adapter :push-2 :session-id "manual-push"}}]
            (is (= :operator-required
                   (:code (apply-at! runtime :command (assoc end :operator-id actor ::show/push-session-id "manual-push") 60))))
            (is (= :stale-gesture-end
                   (:code (apply-at! runtime :command end 65 {:operator-id actor})))))
          (is (= 1 (:last-sequence (gesture))))
          (receive! [0xb0 71 1])
          (is (= :pending (get-in (device-session/snapshot device) [:last-ack :status])))
          (receive! [0xb0 59 127])
          (is (= :suspended (:status (device-session/snapshot device))))
          (let [{:keys [payload reply]} (first (vals @(:push-gesture-ends runtime)))]
            (is (= 1 (:last-sequence payload)))
            (show/tick! runtime 75)
            (is (= :accepted (:status (deref reply 0 nil)))))
          (is (empty? (:open (history))))
          (is (= 1 (:cursor (history))))
          (let [{:keys [before after]} (get-in (history) [:nodes 1])]
            (is (= [initial (level)] [before after]))
            (is (< 0.019 (- after before) 0.021)))
          (is (empty? (:errors (device-session/snapshot device)))))
        (finally (show/stop! runtime))))))

(deftest musical-modulation-renders-overlays-and-yields-to-human-control
  (let [runtime (show/create-runtime)]
    (try
      (let [id (:instance-id (apply-at! runtime :command {:op :cue/launch :cue-id :ember} 0))
            canonical (get-in @(:state runtime) [:model :instances id :params :level])
            config {:id :rhythm :seed 42 :every {:beats 1/4} :params [:level]
                    :select {:instance-ids [id]} :ranges {:level [0.1 0.4]}}]
        (is (= :accepted (:status (apply-at! runtime :modulation-start config 100))))
        (let [value (get-in (show/snapshot runtime) [:modulation :overrides id :level])]
          (is (<= 0.1 value 0.4))
          (is (= canonical (get-in @(:state runtime) [:model :instances id :params :level])))
          (is (= value (first (get-in (show/snapshot runtime) [:output :values :head-1 :emitted-color]))))
          (show/tick! runtime 150)
          (is (= value (get-in (show/snapshot runtime) [:modulation :overrides id :level]))))
        (apply-at! runtime :command {:op :param/set :instance-id id :param :level :value 0.9
                                    :source {:adapter :web :session-id "human"}} 150)
        (is (= 0.9 (first (get-in (show/snapshot runtime) [:output :values :head-1 :emitted-color]))))
        (is (nil? (get-in (show/snapshot runtime) [:modulation :overrides id :level])))
        (apply-at! runtime :modulation-resume {:instance-id id :params [:level]} 200)
        (apply-at! runtime :command {:op :transport/set :transport {:playing? false}} 200)
        (let [controller (first (get-in @(:state runtime) [:model :modulation :controllers]))]
          (show/tick! runtime 10000)
          (is (= controller (first (get-in @(:state runtime) [:model :modulation :controllers]))))))
      (finally (show/stop! runtime)))))

(deftest modulation-cues-use-normal-held-lifecycle-and-category-selection
  (let [runtime (show/create-runtime)]
    (try
      (let [target (:instance-id (apply-at! runtime :command {:op :cue/launch :cue-id :ember} 0))
            created (apply-at! runtime :modulation-cue
                               {:id :random-level :name "Level rhythm" :page-id :main
                                :modulation {:seed 2 :every {:bars 1} :params [:level]
                                             :select {:categories [:intensity]} :ranges {:level [0.2 0.3]}}} 0)
            launched (apply-at! runtime :command {:op :cue/launch :cue-id :random-level :launch {:mode :held}} 100)
            owner (:instance-id launched)]
        (is (= :accepted (:status created)))
        (is (= :accepted (:status launched)))
        (is (some #{:random-level} (vals (get-in @(:state runtime) [:model :pages :main :cells]))))
        (is (= owner (get-in (show/snapshot runtime) [:modulation :controllers 0 :owner-instance-id])))
        (is (<= 0.2 (get-in (show/snapshot runtime) [:modulation :overrides target :level]) 0.3))
        (apply-at! runtime :command {:op :instance/end :instance-id owner} 200)
        (is (empty? (get-in (show/snapshot runtime) [:modulation :controllers])))
        (is (contains? (get-in @(:state runtime) [:model :instances]) target)))
      (finally (show/stop! runtime)))))

(deftest full-show-restore-keeps-modulation-schedule-and-rebinds-targets
  (let [runtime (show/create-runtime)]
    (try
      (let [id (:instance-id (apply-at! runtime :command {:op :cue/launch :cue-id :ember} 0))]
        (apply-at! runtime :modulation-start {:id :saved :seed 17 :every {:beats 1/2}
                                             :select {:instance-ids [id]} :params [:level]} 100)
        (apply-at! runtime :command {:op :transport/set :transport {:playing? false}} 100)
        (let [saved (show/export-show runtime)
              controller (first (get-in saved [:model :modulation :controllers]))]
          (is (= :accepted (:status (apply-at! runtime :import {:source (pr-str saved)} 200))))
          (let [new-id (:id (first (:instances (show/snapshot runtime))))
                restored (first (get-in @(:state runtime) [:model :modulation :controllers]))]
            (is (not= id new-id))
            (is (= (select-keys controller [:seed :ticks :next-beat :anchor-beat])
                   (select-keys restored [:seed :ticks :next-beat :anchor-beat])))
            (is (= [new-id] (get-in restored [:config :select :instance-ids])))
            (show/tick! runtime 300)
            (is (= (:seed controller) (get-in @(:state runtime) [:model :modulation :controllers 0 :seed])))
            (is (number? (get-in (show/snapshot runtime) [:modulation :overrides new-id :level]))))))
      (finally (show/stop! runtime)))))

(deftest waveform-preview-and-persistence-use-the-versioned-registry
  (let [runtime (show/create-runtime)]
    (try
      (let [preview (show/waveform-action! runtime :preview
                                            {:selection {:selector 0 :choices ["sine" "triangle"]}
                                             :options {:samples 5}})]
        (is (= 5 (count (:samples preview))))
        (is (= 1.0 (get-in preview [:samples 1 :value]))))
      (let [definition {:id :custom :version 1 :name "Flat" :params {} :range [0 1]
                        :expression {:op :const :value 0.25}}
            ack (apply-at! runtime :waveform {:source (pr-str definition)} 0)]
        (is (= :accepted (:status ack)))
        (is (= definition (get-in (show/export-show runtime) [:model :waveforms :waveforms :custom])))
        (is (= :rejected (:status (apply-at! runtime :waveform
                                            {:source (pr-str (assoc-in definition [:expression :value] 0.5))} 0)))))
      (finally (show/stop! runtime)))))
