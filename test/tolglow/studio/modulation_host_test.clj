(ns tolglow.studio.modulation-host-test
  (:require [afterglow.control.codecs.push-2 :as push-codec]
            [afterglow.control.devices.session :as device-session]
            [afterglow.control.model :as control]
            [clojure.test :refer [deftest is]]
            [tolglow.operator-history :as operator-history]
            [tolglow.show :as show]))

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
        (is (= {:identity identity :current-id nil :nodes []
                :retention {:limit control/edit-history-limit :retained 0 :pruned? false}}
               (:operator-history initial)))
        (is (= 1 (:history-cursor (turn! runtime ember "private-ember-turn" 0.4 5 push))))
        (is (= 2 (:history-cursor (turn! runtime ice "private-ice-turn" 0.7 10 studio))))
        (is (= 1 (:history-cursor (apply-at! runtime :command {:op :history/undo :expected-cursor 2} 15 studio))))
        (is (= 3 (:history-cursor (turn! runtime ice "private-alternate" 0.5 20 {:operator-id :caller}))))
        (let [committed @(:state runtime)
              projection (show/operator-history runtime)
              [root first-edit second-edit alternate] (:nodes projection)]
          (is (= projection (:operator-history (show/snapshot runtime))))
          (is (identical? committed @(:state runtime)))
          (is (= identity (:identity projection)))
          (is (= 3 (:current-id projection)))
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
          (is (= :level (:parameter first-edit)))
          (is (every? #(nil? (:unavailable-reason %)) [first-edit second-edit alternate])))
        (is (= 1 (:history-cursor (apply-at! runtime :command {:op :history/undo :expected-cursor 3} 30 studio))))
        (is (= :target-unavailable
               (:code (apply-at! runtime :command {:op :history/select :expected-cursor 1 :node-id 0} 35 studio))))
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
                                             {:op :history/undo :expected-cursor 1} 200 authority))))
        (is (== initial (level) (emitted)))
        (is (held?)))
      (finally (show/stop! runtime)))))

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
          (is (empty? (:errors (device-session/snapshot device))))))
      (finally (show/stop! runtime)))))

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
