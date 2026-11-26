(ns tolglow.persistence-test
  (:require [clojure.test :refer :all]
            [afterglow.control.model :as control]
            [afterglow.control.pages :as cue-pages]
            [tolglow.transit :as wire]
            [thi.ng.color.core :as color]
            [afterglow.runtime.engine :as engine]
            [afterglow.render.frame :as render]
            [afterglow.time :as time]
            [tolglow.compiler :as compiler]
            [tolglow.automation.policy :as policy]
            [tolglow.integrations.ableton :as ableton]
            [tolglow.composition :as composition]
            [tolglow.demo :as demo]
            [tolglow.agents :as agents]
            [tolglow.fixtures.definitions :as fixture-library]
            [tolglow.persistence :as files]
            [tolglow.show :as show])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn command [model n payload]
  (let [result (control/apply-command model
                  (merge {:show-id (:show-id model) :epoch (:epoch model) :command-id (str n)
                          :time-ms (:time-ms model) :source {:adapter :studio :session-id "old-browser"}}
                         payload))]
    (is (= :accepted (get-in result [:ack :status])) (pr-str (:ack result)))
    result))

(defn rich-model []
  (let [model (-> (demo/model) (assoc :time-ms 4000)
                  (assoc-in [:transport :time-ms] 4000) (assoc-in [:transport :beat] 8))
        launched (command model 1 {:op :cue/launch :cue-id :ember
                                   :launch {:mode :timed :duration {:bars 2} :params {:level 0.4}}})
        id (get-in launched [:ack :instance-id])
        model (-> (:model launched)
                  (assoc-in [:instances id :state] {:counter 12 :path [1/3 0.25] :flags #{:a :b}})
                  (assoc-in [:instances id :source] {:adapter :auto :session-id "auto-before"}))
        published (command model 2 {:op :registry/publish :kind :effects
                                    :definition (-> (get-in model [:registry :effects :ember])
                                                    (assoc :version 2 :name "Edited ember"))})
        macro {:id :stack :mode :macro :slots [{:id :one :cue-id :orchid :params {}}]}
        sequence {:id :steps :mode :sequence :loop :loop :timebase :beats
                  :slots [{:id :a :cue-id :ice :params {} :duration {:beats 1}}
                          {:id :b :cue-id :sage :params {} :duration {:beats 2}}]}
        model (-> (:model published) (composition/save macro) (composition/start :stack 4000)
                  (composition/save sequence) (composition/start :steps 4000) (composition/advance 4000))
        held (command model 3 {:op :cue/launch :cue-id :spot :launch {:mode :held}})
        bridge (-> (ableton/new-bridge {:mappings {36 {:cue-id :ember :mode :held}}
                                       :parameters {12 {:instance-id id :cue-id :ember :param :level :bounds [0 1]}}})
                   (assoc :instances {id :ember "0:ended" :ember}
                          :held {36 {:instance-id id :command-id "pending"}}
                          :clip-links {:ember {:track-id "lighting" :clip-id "verse"}}
                          :recorded #{[:main 0 "old"]} :record-order [[:main 0 "old"]]))
        auto (assoc (policy/initial-state 12345 4000 "auto-before")
                    :tick 7 :epoch 0 :owned-instance-ids [id "0:ended"]
                    :interval-timer (time/start {:bars 2} {:beat 8 :time-ms 4000 :meter time/default-meter}))]
    (assoc (:model held)
           :auto {:enabled? true :config {:interval {:bars 2} :count 1 :scope :all} :state auto}
           :integrations {:ableton bridge :audio {:input-gain 0.8} :extensions {:vendor/mapping {:version 3}}}
           :responses [{:id :response :select {:instance-ids [id]} :params [:level]
                        :envelope {:attack {:beats 1}} :state {:started-at-ms 4000 :started-at-beat 8}}]
           :variables {:palette :winter :fraction 1/3}
           :extensions {:vendor/plugin {:opaque-id id :preferences {:theme "dim"}}})))

(defn restored [model] (files/thaw (assoc (demo/model) :epoch 9 :revision 200 :time-ms 9000) (files/freeze model)))

(defn- operator-command [model id payload]
  (control/apply-command model
                         (merge {:show-id (:show-id model) :epoch (:epoch model) :time-ms (:time-ms model)
                                 :command-id id :source :test-controller}
                                payload)
                         {:operator-id :front-of-house}))

(defn- operator-edit [model id payload]
  (let [{:keys [ack model]} (operator-command model id payload)]
    (is (= :accepted (:status ack)) (pr-str ack))
    model))

(defn- gesture [instance-id gesture-id]
  {:instance-id instance-id :param :level :control-id :encoder-1 :gesture-id gesture-id})

(defn- sample-gesture [model gesture value]
  (let [id (name (:gesture-id gesture))]
    (-> model
        (operator-edit (str id "-begin") (assoc gesture :op :gesture/begin))
        (operator-edit (str id "-sample") (assoc gesture :op :gesture/sample :sequence 1 :value value)))))

(defn- commit-gesture [model gesture value]
  (-> model
      (sample-gesture gesture value)
      (operator-edit (str (name (:gesture-id gesture)) "-end") (assoc gesture :op :gesture/end :last-sequence 1))))

(defn- history [model] (get-in model [:control :edit-history]))

(deftest open-gesture-is-sealed-in-the-saved-branch-only
  (let [{:keys [model ack]} (command (demo/model) "launch" {:op :cue/launch :cue-id :ember
                                                          :launch {:params {:level 0.2}}})
        id (:instance-id ack)
        target [:instance-param id :level]
        live (-> model
                 (commit-gesture (gesture id :first-turn) 0.3)
                 (commit-gesture (gesture id :second-turn) 0.4)
                 (operator-edit "step-back" {:op :history/undo :expected-cursor 2 :expected-history-position 2})
                 (sample-gesture (gesture id :open-turn) 0.6))
        document (files/read-string (files/write-string live))
        saved (:model document)
        reloaded (files/thaw (assoc (demo/model) :epoch 9) document)
        new-id (first (keys (:instances reloaded)))
        new-target [:instance-param new-id :level]
        undone (operator-edit reloaded "undo-restored" {:op :history/undo :expected-cursor 3 :expected-history-position 4})
        redone (operator-edit undone "alternate-restored" {:op :history/redo :expected-cursor 1
                                                           :expected-history-position 5 :node-id 2})
        selected (operator-edit redone "switch-restored" {:op :history/select :expected-cursor 2
                                                         :expected-history-position 6 :node-id 3})]
    (is (= document (files/freeze live)))
    (is (= 1 (:cursor (history live))))
    (is (= :open-turn (get-in (history live) [:open target :gesture-id])))
    (is (= 1 (get-in live [:control :history "open-turn-sample" :command :sequence])))
    (is (= ["launch" "step-back"] (get-in saved [:control :history-order])))
    (is (= (select-keys (get-in live [:control :history]) ["launch" "step-back"])
           (get-in saved [:control :history])))
    (is (nil? (:open (history saved))))
    (is (= [3 4 4 5 6 7] (mapv (comp :position history) [live saved reloaded undone redone selected])))
    (is (= {:parent 1 :target target :actor :front-of-house :control-id :encoder-1
            :before 0.3 :after 0.6 :generation 0
            :target-display {:cue-id :ember :cue-label "Ember" :effect-id :ember :effect-label "Ember"}}
           (get-in (history saved) [:nodes 3])))
    (is (= 1 (get-in (history saved) [:nodes 2 :parent])))
    (is (= 3 (:cursor (history reloaded))))
    (is (nil? (:open (history reloaded))))
    (is (= {new-target 0} (:generations (history reloaded))))
    (is (every? #(= new-target (get-in (history reloaded) [:nodes % :target])) [1 2 3]))
    (is (= [0.6 0.3 0.4 0.6]
           (mapv #(get-in % [:instances new-id :params :level]) [reloaded undone redone selected])))))

(deftest save-keeps-external-writes-and-seals-only-current-gestures
  (let [{:keys [model ack]} (command (demo/model) :launch-ember {:op :cue/launch :cue-id :ember
                                                                :launch {:params {:level 0.2}}})
        ember (:instance-id ack)
        {:keys [model ack]} (command model :launch-ice {:op :cue/launch :cue-id :ice
                                                         :launch {:params {:level 0.1}}})
        ice (:instance-id ack)
        live (-> model
                 (commit-gesture (gesture ember :earlier-turn) 0.3)
                 (sample-gesture (gesture ember :conflicted-turn) 0.5)
                 (sample-gesture (gesture ice :current-turn) 0.7)
                 (command :external-change {:op :param/set :instance-id ember :param :level :value 0.6}) :model
                 (command :external-return {:op :param/set :instance-id ember :param :level :value 0.5}) :model)
        saved (files/freeze live)
        reloaded (files/thaw (demo/model) saved)
        [new-ember new-ice] (mapv :id (sort-by :order (vals (:instances reloaded))))
        undone (operator-edit reloaded "undo-current" {:op :history/undo :expected-cursor 2 :expected-history-position 2})
        rejected (operator-command undone "undo-conflicted" {:op :history/undo :expected-cursor 1
                                                              :expected-history-position 3})]
    (is (= #{[:instance-param ember :level] [:instance-param ice :level]} (set (keys (:open (history live))))))
    (is (= #{0 1 2} (set (keys (:nodes (history reloaded))))))
    (is (= {[:instance-param new-ember :level] 2 [:instance-param new-ice :level] 0}
           (:generations (history reloaded))))
    (is (= [:instance-param new-ice :level] (get-in (history reloaded) [:nodes 2 :target])))
    (is (= 0.5 (get-in undone [:instances new-ember :params :level])))
    (is (= 0.1 (get-in undone [:instances new-ice :params :level])))
    (is (= :history-conflict (get-in rejected [:ack :code])))
    (is (identical? undone (:model rejected)))))

(deftest missing-history-target-cannot-attach-to-a-new-instance
  (let [{:keys [model ack]} (command (demo/model) :launch-old {:op :cue/launch :cue-id :ember
                                                              :launch {:params {:level 0.2}}})
        old-id (:instance-id ack)
        live (-> model
                 (commit-gesture (gesture old-id :old-turn) 0.4)
                 (command :end-old {:op :instance/end :instance-id old-id :force? true}) :model
                 (command :launch-replacement {:op :cue/launch :cue-id :ember :launch {:params {:level 0.4}}}) :model)
        future-id "10:1"
        raw-target [:instance-param future-id :level]
        document (-> (files/freeze live)
                     (assoc-in [:model :control :edit-history :nodes 1 :target] raw-target)
                     (assoc-in [:model :control :edit-history :generations] {raw-target 0}))
        reloaded (files/thaw (assoc (demo/model) :epoch 9) document)
        reloaded-again (files/thaw reloaded (files/read-string (files/write-string reloaded)))
        archived [:unavailable-instance-param future-id :level 10]
        rejected (operator-command reloaded "undo-ended" {:op :history/undo :expected-cursor 1
                                                           :expected-history-position 1})]
    (is (= #{future-id} (set (keys (:instances reloaded)))))
    (is (= 0.4 (get-in reloaded [:instances future-id :params :level])))
    (is (= #{0 1} (set (keys (:nodes (history reloaded))))))
    (is (= archived (get-in (history reloaded) [:nodes 1 :target])))
    (is (= {archived 0} (:generations (history reloaded))))
    (is (= (history reloaded) (history reloaded-again)))
    (is (= :target-unavailable (get-in rejected [:ack :code])))
    (is (identical? reloaded (:model rejected)))))

(deftest show-file-restores-an-undoable-gesture-and-its-output
  (let [file (Files/createTempFile "tolglow-edit-history" ".transit" (make-array FileAttribute 0))]
    (try
      (let [runtime (show/create-runtime {:workers 1 :workspace-options nil})
            model #(:model @(:state runtime))
            emitted #(first (get-in (show/snapshot runtime) [:output :values :head-1 :emitted-color]))
            run-command (fn [time payload]
                          (let [receipt (show/submit! runtime :command payload {:operator-id :front-of-house})]
                            (show/tick! runtime time)
                            (let [ack (deref receipt 1000 ::missing)]
                              (is (= :accepted (:status ack)) (pr-str ack))
                              ack)))]
        (try
          (let [id (:instance-id (run-command 0 {:op :cue/launch :cue-id :ember :launch {:params {:level 0.2}}}))
                turn (gesture id :held-while-saving)
                target [:instance-param id :level]]
            (run-command 20 (assoc turn :op :gesture/begin))
            (run-command 40 (assoc turn :op :gesture/sample :sequence 1 :value 0.7))
            (let [saved-emission (emitted)]
              (show/save-show! runtime (str file))
              (is (= 0 (:cursor (history (model)))))
              (is (= :held-while-saving (get-in (history (model)) [:open target :gesture-id])))
              (let [receipt (show/load-show! runtime (str file))]
                (show/tick! runtime 60)
                (is (= :accepted (:status (deref receipt 1000 ::missing)))))
              (let [new-id (first (keys (:instances (model))))
                    level #(get-in (model) [:instances new-id :params :level])]
                (is (not= id new-id))
                (is (= 0.7 (level)))
                (is (= saved-emission (emitted)))
                (is (= 0 (:history-cursor (run-command 80 {:op :history/undo :expected-cursor 1
                                                          :expected-history-position 1}))))
                (is (== 0.2 (level) (emitted))))))
          (finally (show/stop! runtime))))
      (finally (Files/deleteIfExists file)))))

(deftest malformed-edit-history-is-rejected-before-restore
  (let [{:keys [model ack]} (command (demo/model) :launch {:op :cue/launch :cue-id :ember
                                                          :launch {:params {:level 0.2}}})
        id (:instance-id ack)
        target [:instance-param id :level]
        model (-> model
                  (commit-gesture (gesture id :first-turn) 0.4)
                  (commit-gesture (gesture id :second-turn) 0.5))
        doc (files/freeze model)
        path [:model :control :edit-history]]
    (is (nil? (history (files/thaw model (update-in doc [:model :control] dissoc :edit-history)))))
    (let [legacy (-> doc
                     (update-in path dissoc :position)
                     (update-in (into path [:nodes]) update-vals #(dissoc % :target-display)))
          restored (files/thaw model legacy)
          undone (operator-edit restored "undo-legacy" {:op :history/undo :expected-cursor 2
                                                        :expected-history-position 0})]
      (is (= [1 1] ((juxt :cursor :position) (history undone)))))
    (doseq [[suffix value] [[[] '(:malformed)]
                            [[:limit] 129] [[:limit] 2] [[:nodes] {}]
                            [[:cursor] 77] [[:next-id] 2] [[:next-id] Long/MAX_VALUE]
                            [[:position] -1] [[:position] 1.5] [[:position] nil] [[:position] Long/MAX_VALUE]
                            [[:nodes 0] {:parent 2}] [[:nodes 1] {:parent nil}]
                            [[:nodes 1 :parent] 2] [[:nodes 1 :parent] 1] [[:nodes 1 :parent] 77]
                            [[:nodes 1] '(:malformed)]
                            [[:nodes 1 :actor] {:session-id "opaque"}] [[:nodes 1 :control-id] ""]
                            [[:nodes 1 :before] :invalid] [[:nodes 1 :after] 0.2]
                            [[:nodes 1 :target] [:unrecognized id :level]]
                            [[:nodes 1 :target-display] {:cue-id :ember}]
                            [[:nodes 1 :target-display] {:effect-id :ember :effect-label 42}]
                            [[:nodes 1 :target-display :private] "opaque"]
                            [[:nodes 1 :gesture-id] :unexpected]
                            [[:nodes 1 :generation] 1] [[:generations target] -1]
                            [[:generations] {}]
                            [[:generations] {target 0 [:instance-param id :unused] 0}]
                            [[:generations] {[:unavailable-instance-param id :level 99] 0}]
                            [[:open] {target {:actor :front-of-house :last-sequence 1}}]]]
      (testing (pr-str suffix value)
        (let [broken (assoc-in doc (into path suffix) value)
              failure (try (files/thaw model broken)
                           (catch clojure.lang.ExceptionInfo error (ex-data error)))]
          (is (= :invalid-show (:persistence/error failure))))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (files/read-string (wire/encode (assoc-in doc (into path [:cursor]) 99)))))))

(deftest cue-presets-survive-roundtrip-and-reject-invalid-file-values
  (let [launched (command (demo/model) :preset-launch {:op :cue/launch :cue-id :ember
                                                      :launch {:params {:level 0.37}}})
        saved (:model (command (:model launched) :preset-save
                               {:op :cue/save-params :cue-id :ember :instance-id (get-in launched [:ack :instance-id])}))
        cue (get-in saved [:registry :cues :ember])
        result (restored saved)
        relaunched (command result :preset-relaunch {:op :cue/launch :cue-id :ember})]
    (is (= cue (get-in result [:registry :cues :ember])))
    (is (= 0.37 (get-in relaunched [:model :instances (get-in relaunched [:ack :instance-id]) :params :level])))
    (doseq [bad [nil [] {:level 2} {:missing 1}]]
      (let [document (-> (files/freeze saved)
                         (assoc-in [:model :registry :cues :ember :saved-params] bad)
                         (assoc-in [:model :registry :versions [:cues :ember (:version cue)] :saved-params] bad))]
        (is (thrown? clojure.lang.ExceptionInfo (files/thaw (demo/model) document)))))))

(deftest incompatible-effect-edits-leave-a-restorable-cue
  (let [before (demo/model)
        cue (-> (get-in before [:registry :cues :ember])
                (dissoc :version)
                (assoc :saved-params {:level 0.9}))
        saved (:model (command before :preset {:op :cue/put :cue cue}))
        revision (-> (get-in saved [:registry :effects :ember])
                     (assoc :version 2)
                     (assoc-in [:params :level :bounds] [0 0.8])
                     (assoc-in [:params :level :recommended] [0.2 0.7]))
        attempt (control/apply-command saved {:show-id (:show-id saved) :epoch (:epoch saved)
                                              :source :test :command-id :edit :time-ms (:time-ms saved)
                                              :op :registry/publish :kind :effects :definition revision})
        reopened (files/read-string (files/write-string (:model attempt)))
        restored (files/thaw before reopened)
        launch (command restored :relaunch {:op :cue/launch :cue-id :ember})]
    (is (= :incompatible-cue-presets (get-in attempt [:ack :code])))
    (is (identical? saved (:model attempt)))
    (is (= 0.9 (get-in launch [:model :instances (get-in launch [:ack :instance-id]) :params :level])))))

(deftest complete-portable-roundtrip-preserves-musical-progress
  (let [model (rich-model)
        source (files/write-string model)
        document (files/read-string source)
        result (restored model)]
    (is (= model (:model document)))
    (is (= 2 (:schema-version document)))
    (is (= 10 (:epoch result)))
    (is (= 201 (:revision result)))
    (is (= 4000 (:time-ms result)))
    (is (= (:transport model) (:transport result)))
    (is (= (:registry model) (:registry result)))
    (is (= (:pages model) (:pages result)))
    (is (= (:rig model) (:rig result)))
    (is (= (:variables model) (:variables result)))
    (is (= (:extensions model) (:extensions result)))
    (is (= (:compositions model) (:compositions result)))
    (is (= (count (:instances model)) (count (:instances result))))
    (is (= (map :state (sort-by :order (vals (:instances model))))
           (map :state (sort-by :order (vals (:instances result))))))
    (is (= (map :duration-timer (sort-by :order (vals (:instances model))))
           (map :duration-timer (sort-by :order (vals (:instances result))))))
    (is (every? #(clojure.string/starts-with? % "10:") (keys (:instances result))))
    (is (empty? (get-in result [:control :history])))
    (is (empty? (get-in result [:control :history-order])))))

(deftest captured-old-version-survives-a-new-current-definition
  (let [model (rich-model) result (restored model)
        instance (first (filter #(= :ember (:effect-id %)) (vals (:instances result))))]
    (is (= 2 (get-in result [:registry :effects :ember :version])))
    (is (= 1 (:effect-version instance)))
    (is (= 1 (get-in instance [:definition :version])))
    (is (= {:counter 12 :path [1/3 0.25] :flags #{:a :b}} (:state instance)))))

(deftest all-domain-ownership-rekeys-without-external-leases
  (let [model (rich-model) result (restored model)
        ids (set (keys (:instances result)))
        owners (set (map :owner-id (vals (:composition-runs result))))
        auto-state (get-in result [:auto :state])
        held (first (filter #(= :spot (:cue-id %)) (vals (:instances result))))]
    (is (every? ids (vals (:composition-tokens result))))
    (is (every? owners (map first (keys (:composition-tokens result)))))
    (is (every? #(= 10 (:epoch %)) (vals (:composition-runs result))))
    (is (= (get-in result [:composition-runs :steps :owner-id])
           (get-in result [:composition-runs :steps :state :owner-id])))
    (is (= (get-in result [:composition-runs :steps :owner-id])
           (first (get-in result [:composition-runs :steps :state :active-token]))))
    (is (= 12345 (:seed auto-state)))
    (is (= 7 (:tick auto-state)))
    (is (= 10 (:epoch auto-state)))
    (is (= 1 (count (:owned-instance-ids auto-state))))
    (is (every? ids (:owned-instance-ids auto-state)))
    (is (= {:adapter :restore :epoch 10} (:source held)))
    (is (= :held (:launch-mode held)))
    (is (= (get-in model [:auto :state :interval-timer]) (:interval-timer auto-state)))
    (is (every? ids (map :instance-id (:commands (policy/stop-plan result auto-state 4000)))))))

(deftest ableton-links-and-configuration-roundtrip-with-fresh-live-session
  (let [model (rich-model) result (restored model)
        before (get-in model [:integrations :ableton])
        bridge (get-in result [:integrations :ableton])
        parameter-id (get-in bridge [:parameters 12 :instance-id])]
    (is (= (:mappings before) (:mappings bridge)))
    (is (= (:clip-links before) (:clip-links bridge)))
    (is (= {:input-gain 0.8} (get-in result [:integrations :audio])))
    (is (= :ember (get-in result [:instances parameter-id :cue-id])))
    (is (= #{parameter-id} (set (keys (:instances bridge)))))
    (is (= {} (:held bridge)))
    (is (= #{} (:recorded bridge)))
    (is (= [] (:record-order bridge)))
    (is (= [parameter-id] (get-in result [:responses 0 :select :instance-ids])))))

(deftest restored-sequence-continues-at-its-saved-position
  (let [model (restored (rich-model))
        old-id (get (:composition-tokens model) (get-in model [:composition-runs :steps :state :active-token]))
        next-model (composition/advance (assoc-in model [:transport :beat] 9) 4500)
        run (get-in next-model [:composition-runs :steps])
        new-id (get (:composition-tokens next-model) (get-in run [:state :active-token]))]
    (is (= 1 (get-in run [:state :step-index])))
    (is (= :sage (get-in next-model [:instances new-id :cue-id])))
    (is (not= old-id new-id))
    (is (clojure.string/starts-with? new-id "10:"))
    (is (= :releasing (get-in next-model [:instances old-id :phase])))))

(deftest restored-sequence-resumes-from-an-empty-step-without-a-launch-token
  (let [definition {:id :steps :mode :sequence
                    :slots [{:id :rest :kind :empty :duration {:beats 1/2}}
                            {:id :muted :cue-id :ice :duration {:beats 1/2} :muted? true}
                            {:id :cue :cue-id :sage}]}
        running (-> (demo/model) (composition/save definition)
                    (composition/start :steps 0) (composition/advance 0))
        model (restored running)
        muted (-> model (assoc-in [:transport :beat] 1/2) (composition/advance 250))
        audible (-> muted (assoc-in [:transport :beat] 1) (composition/advance 500))
        instance-id (first (vals (:composition-tokens audible)))]
    (is (= :rest (get-in model [:composition-runs :steps :playback :slot-id])))
    (is (= {:beats 1/2} (get-in model [:compositions :steps :slots 0 :duration])))
    (is (empty? (:composition-tokens model)))
    (is (true? (get-in muted [:composition-runs :steps :playback :muted?])))
    (is (empty? (:composition-tokens muted)))
    (is (= :sage (get-in audible [:instances instance-id :cue-id])))))

(deftest restoring-a-paused-transport-does-not-fast-forward-musical-timers
  (let [model (-> (rich-model) (assoc-in [:transport :playing?] false))
        result (restored model)
        frame (:model (engine/advance result {:time-ms 14000 :transport (:transport result)} compiler/compile-effect))]
    (is (= 8 (get-in frame [:transport :beat])))
    (is (= (count (:instances result)) (count (:instances frame))))
    (is (= :running (:phase (first (filter #(= :ember (:effect-id %)) (vals (:instances frame)))))))))

(deftest restore-epoch-exceeds-both-models-and-input-is-unchanged
  (let [saved (assoc (demo/model) :epoch 23 :revision 40)
        before (pr-str saved)
        result (files/thaw (assoc (demo/model) :epoch 8 :revision 7) (files/freeze saved))]
    (is (= 24 (:epoch result)))
    (is (= 41 (:revision result)))
    (is (= before (pr-str saved)))
    (is (= saved (files/validate-model saved)))))

(deftest invalid-files-never-return-a-partial-model
  (let [model (rich-model) document (files/freeze model) before (pr-str model)]
    (doseq [broken [(assoc document :schema-version 1)
                    (assoc document :unexpected true)
                    (assoc-in document [:model :transport :bpm] 0)
                    (assoc-in document [:model :transport :beat] Double/NaN)
                    (assoc-in document [:model :rig :fixtures 1 :address] 1)
                    (assoc-in document [:model :pages :main :cells [0 0]] :missing)
                    (assoc-in document [:model :registry :effects :ember :outputs :color] {:op :unknown})
                    (assoc-in document [:model :composition-runs :steps :epoch] 99)
                    (assoc-in document [:model :auto :state :seed] 1.2)]]
      (is (thrown? Exception (files/thaw model broken))))
    (is (= before (pr-str model)))))

(deftest saved-recent-cue-history-rejects-invalid-shapes-and-identities
  (let [model (demo/model)
        document (files/freeze model)
        path [:control :recent-cue-ids]
        rejected-path (fn [load!]
                        (try (load!)
                             (catch clojure.lang.ExceptionInfo error
                               (let [{:keys [errors] :as data} (ex-data error)]
                                 (when (= :invalid-show (:persistence/error data))
                                   (:path (first errors)))))))]
    (doseq [[ids suffix] [[nil []] ['(:ember) []] [#{:ember} []]
                          [[:ember :ember] []] [(mapv #(str "cue-" %) (range 65)) []]
                          [[:ember 42] [1]] [[" "] [0]] [[""] [0]] [[nil] [0]]]]
      (let [invalid (assoc-in document (into [:model] path) ids)
            expected (into path suffix)]
        (is (= expected (rejected-path #(files/read-string (wire/encode invalid)))))
        (is (= expected (rejected-path #(files/thaw model invalid))))))))

(deftest saved-recent-cue-history-keeps-valid-stale-ids-and-allows-older-files
  (let [model (demo/model)
        path [:control :recent-cue-ids]
        ids (into [:ember "ember"] (map #(keyword "stale" (str %))) (range 62))
        restored (files/thaw model (files/read-string (files/write-string (assoc-in model path ids))))
        legacy (files/thaw model (files/read-string (files/write-string (update model :control dissoc :recent-cue-ids))))
        launched (:model (command restored :launch-after-restore {:op :cue/launch :cue-id :ice}))]
    (is (= ids (get-in restored path)))
    (is (= {[0 7] :ember} (:cells (cue-pages/page restored cue-pages/recent-page-id))))
    (is (= [:ice :ember] (get-in launched path)))
    (is (not (contains? (:control legacy) :recent-cue-ids)))
    (is (empty? (cue-pages/recent-page-source legacy)))))

(deftest rejects-nonportable-state-and-secrets-without-silently-dropping-them
  (let [model (demo/model)]
    (doseq [value [(atom 1) (fn [] 1) (Object.) 'invoke]]
      (is (thrown? Exception (files/freeze (assoc-in model [:variables :bad] value)))))
    (doseq [change [{:provider {:api-key "do-not-persist"}}
                    {:authorization "Bearer secret"}]]
      (is (thrown? Exception (files/freeze (assoc model :integrations change)))))
    (is (thrown? Exception (files/freeze (assoc model :leases {}))))
    (is (thrown? Exception (files/freeze (assoc model :unknown-extension {}))))
    (is (= {:vendor/data {:a [1 2 3]}} (:extensions (:model (files/freeze (assoc model :extensions {:vendor/data {:a [1 2 3]}}))))))))

(deftest connection-descriptors-and-chat-history-are-portable-data
  (let [model (rich-model)
        settings {:version 1 :config {:provider :self-hosted :model "local-model"
                                      :endpoint "http://127.0.0.1:1234/v1/chat/completions"}
                  :credential-ref nil :selected-conversation-id "chat-1"
                  :conversations [{:id "chat-1" :title "Fixture test"
                                   :messages [{:role :user :content "Historical instance 0:1"}
                                              {:role :assistant :content "Ready"}]
                                   :status :completed :updated-at-ms 0.0}]}
        model (-> model
                  (assoc-in [:integrations :agents] settings)
                  (assoc :settings {:ui {:page :main :panel :rig} :beat-detection {:enabled? true}})
                  (assoc-in [:integrations :ableton :device-ports] ["Push 2 User Port"])
                  (assoc-in [:integrations :beat-link] {:peer-id "deck-2" :enabled? true :restore-policy :discover}))
        result (restored model)]
    (is (= (agents/validate-state settings) (get-in result [:integrations :agents])))
    (is (= (:settings model) (:settings result)))
    (is (= ["Push 2 User Port"] (get-in result [:integrations :ableton :device-ports])))
    (is (= (get-in model [:integrations :beat-link]) (get-in result [:integrations :beat-link])))))

(deftest reader-rejects-eval-tags-multiple-forms-and-invalid-schema
  (doseq [source ["#=(System/exit 1)" "#foreign/object {}" "#inst \"2026-01-01\""
                  "{:schema-version 1 :model {}}" "{} {}" ""]]
    (is (thrown? Exception (files/read-string source))))
  (let [source (files/write-string (demo/model))]
    (is (thrown? Exception (files/read-string (str source " {}")))))
  (is (thrown? Exception (files/read-string (String. (char-array (inc files/max-document-characters) \space))))))

(deftest corrupted-captured-definitions-and-timers-reject
  (let [model (rich-model) id (:id (first (sort-by :order (vals (:instances model)))))
        doc (files/freeze model)]
    (doseq [broken [(assoc-in doc [:model :instances id :params :level] 2)
                    (assoc-in doc [:model :instances id :definition :version] 99)
                    (assoc-in doc [:model :instances id :duration-timer :duration :value] 999)
                    (assoc-in doc [:model :instances id :id] "wrong")
                    (assoc-in doc [:model :instances id :state :invalid] (atom 1))]]
      (is (thrown? Exception (files/thaw model broken))))))

(deftest captured-instances-can-outlive-a-deleted-cue
  (let [launch (command (demo/model) 1 {:op :cue/launch :cue-id :ember})
        removed (command (:model launch) 2 {:op :cue/delete :cue-id :ember})
        model (:model removed)
        result (files/thaw model (files/freeze model))]
    (is (nil? (get-in result [:registry :cues :ember])))
    (is (= 1 (count (:instances result))))
    (is (= :ember (:cue-id (first (vals (:instances result))))))))

(deftest committed-frame-can-roundtrip-without-sampling-effects
  (let [model (rich-model)
        rendered (render/resolve-frame (get-in model [:rig :fixtures]) [] {})
        output (-> (select-keys rendered [:values :target-order :universes])
                   (update :universes #(into {} (map (fn [[u bytes]] [u (mapv int bytes)]) %))))
        document (files/freeze model output)
        parsed (files/read-string (files/write-string model output))
        result (files/thaw (demo/model) parsed)]
    (is (= output (:frame-output document)))
    (is (= document parsed))
    (is (= (map :state (sort-by :order (vals (:instances model))))
           (map :state (sort-by :order (vals (:instances result))))))
    (is (= (:time-ms model) (:time-ms result)))
    (is (not (contains? result :frame-output)))))

(deftest invalid-output-rejects-before-a-restored-model-is-returned
  (let [model (demo/model)
        rendered (render/resolve-frame (get-in model [:rig :fixtures]) [] {})
        output (-> (select-keys rendered [:values :target-order :universes])
                   (update :universes #(into {} (map (fn [[u bytes]] [u (mapv int bytes)]) %))))
        document (files/freeze model output)]
    (doseq [bad [(assoc-in document [:frame-output :universes 1 0] 256)
                 (assoc-in document [:frame-output :universes 1 0] -129)
                 (assoc-in document [:frame-output :universes 1] [0])
                 (assoc-in document [:frame-output :universes 2] (vec (repeat 512 0)))
                 (assoc-in document [:frame-output :values :unknown-fixture] {})
                 (assoc-in document [:frame-output :target-order] [])
                 (assoc-in document [:frame-output :universes 1] (byte-array 512))]]
      (is (thrown? Exception (files/thaw model bad))))
    (let [unsigned (update output :universes #(into {} (map (fn [[u bytes]] [u (mapv (fn [b] (bit-and 255 b)) bytes)]) %)))]
      (is (= unsigned (files/validate-frame-output model unsigned))))
    (let [compact-preview (assoc output :values (select-keys (:values output) [:head-1]))]
      (is (= compact-preview (files/validate-frame-output model compact-preview))))))

(deftest reactive-data-restores-without-resolving-or-evaluating-code
  (let [model (rich-model)
        old-id (:id (first (sort-by :order (vals (:instances model)))))
        registry {:registrations
                  [{:id :show/beat :kind :sub
                    :implementation {:type :source :namespace "user" :form "(fn [context query] (:beat context))" :bindings {}}}
                   {:id :show/action :kind :fx
                    :implementation {:type :source :namespace "user" :form "(fn [context value] (throw (Exception. \"must not run\")))"
                                     :bindings {"target" {:instance-id old-id}}}}]
                  :state {:cells {:offset 1/2 :enabled? true}
                          :pending-actions [{:op :instance/end :instance-id old-id}]
                          :watchers [{:id :beat-watcher :query [:show/beat] :effect-id :show/action
                                      :initialized? true :value 8}]}}
        model (assoc model :reactive registry)
        result (restored model)
        new-id (:id (first (sort-by :order (vals (:instances result)))))]
    (is (= (:registrations registry) (get-in (files/read-string (files/write-string model)) [:model :reactive :registrations])))
    (is (= {:offset 1/2 :enabled? true} (get-in result [:reactive :state :cells])))
    (is (= 8 (get-in result [:reactive :state :watchers 0 :value])))
    (is (= new-id (get-in result [:reactive :state :pending-actions 0 :instance-id])))
    (is (= new-id (get-in result [:reactive :registrations 1 :implementation :bindings "target" :instance-id])))
    (is (thrown? Exception (files/freeze (assoc-in model [:reactive :state :pending-actions 0 :epoch] 0))))
    (is (thrown? Exception (files/freeze (assoc-in model [:reactive :state :watchers 0 :effect-id] :missing/handler))))
    (is (thrown? Exception (files/freeze (assoc-in model [:reactive :registrations 0 :implementation :form] "#=(println :bad)"))))
    (is (thrown? Exception (files/freeze (assoc-in model [:reactive :state :cells :db] {}))))))

(deftest fixture-library-and-limits-are-validated-before-saving
  (let [cache (assoc (fixture-library/new-library) :manufacturers {"robe" {"name" "Robe"}})
        model (assoc-in (demo/model) [:integrations :fixtures] cache)]
    (is (= cache (get-in (restored model) [:integrations :fixtures])))
    (is (thrown? Exception (files/freeze (assoc-in model [:integrations :fixtures :revision] -1))))
    (with-redefs [files/max-document-characters 100]
      (is (thrown? Exception (files/freeze model)))
      (is (thrown? Exception (files/write-string model))))))

(deftest malformed-agent-history-is-rejected-before-restore
  (let [model (assoc-in (demo/model) [:integrations :agents]
                         {:version 1 :config nil :credential-ref nil :selected-conversation-id "missing"
                          :conversations []})]
    (is (thrown? Exception (files/freeze model))))
  (is (thrown? Exception
               (files/freeze (assoc-in (demo/model) [:integrations :agents]
                                      {:version 1 :config nil :credential-ref "raw-key"
                                       :selected-conversation-id nil :conversations []})))))

(deftest saved-callback-validation-never-invokes-custom-readers
  (let [calls (atom 0)]
    (binding [*data-readers* {'sideeffect (fn [_] (swap! calls inc))}
              *default-data-reader-fn* (fn [_ _] (swap! calls inc))]
      (doseq [source ["(fn [] #sideeffect {})" "(fn [] #unknown/tag {})"]]
        (is (thrown? Exception
                     (files/freeze
                      (assoc (demo/model) :reactive
                             {:registrations [{:id :example/source :kind :sub
                                               :implementation {:type :source :namespace "user"
                                                                :form source :bindings {}}}]}))))))
    (is (zero? @calls))))

(deftest show-files-use-transit-values-and-own-their-snapshot
  (let [stamp (java.util.Date. 2222)
        packet (byte-array [0 1 -1])
        values {:received-at stamp :dmx packet :aim [-1.25 3.5 4] :id (random-uuid)
                :ink (color/hsla 0.25 0.5 0.75 0.4) :steps (list :down :up)}
        model (assoc-in (demo/model) [:extensions :trace] values)
        document (files/freeze model)
        source (files/write-string model)
        decoded (get-in (wire/decode source) [:model :extensions :trace])]
    (is (= (dissoc values :dmx) (dissoc decoded :dmx)))
    (is (= [0 1 -1] (vec (:dmx decoded))))
    (.setTime stamp 3333)
    (aset-byte packet 0 (byte 55))
    (is (= 2222 (.getTime (get-in document [:model :extensions :trace :received-at]))))
    (is (= [0 1 -1] (vec (get-in document [:model :extensions :trace :dmx]))))
    (is (= (:ink values) (get-in (files/thaw (demo/model) (files/read-string source)) [:extensions :trace :ink])))))

(deftest previous-schema-two-files-open-and-next-save-is-transit
  (let [model (demo/model)
        previous (pr-str {:schema-version 2 :model model})]
    (is (= model (:model (files/read-string previous))))
    (is (= model (:model (wire/decode (files/write-string model)))))))

(deftest cue-and-captured-effect-layouts-survive-a-show-roundtrip
  (let [base (demo/model)
        effect (assoc (get-in base [:registry :effects :breathe])
                      :version 2 :ui/layout {:template :pulse :order [:period :level]})
        published (:model (command base :effect {:op :registry/publish :kind :effects :definition effect}))
        cue (assoc (get-in published [:registry :cues :breathe])
                   :version 2 :ui/layout {:monitor :period})
        prepared (:model (command published :cue {:op :cue/put :cue cue}))
        running (command prepared :launch {:op :cue/launch :cue-id :breathe})
        result (files/thaw base (files/read-string (files/write-string (:model running))))
        instance (first (vals (:instances result)))]
    (is (= (:ui/layout effect) (get-in instance [:definition :ui/layout])))
    (is (= (:ui/layout cue) (get-in result [:registry :cues :breathe :ui/layout])))))

(deftest gradient-definitions-survive-a-show-roundtrip
  (let [base (demo/model)
        wash [{:position 0 :color [1 0 0]} {:position 1/2 :color [0 1 0]} {:position 1.0 :color [0 0 1]}]
        effect (-> (get-in base [:registry :effects :breathe])
                   (assoc :version 2)
                   (assoc-in [:params :wash] {:type :gradient :default wash}))
        published (:model (command base :effect {:op :registry/publish :kind :effects :definition effect}))
        running (command published :launch {:op :cue/launch :cue-id :breathe
                                            :launch {:params {:wash [(first wash)]}}})
        result (files/thaw base (files/read-string (files/write-string (:model running))))
        instance (first (vals (:instances result)))]
    (is (= (:params effect) (get-in result [:registry :effects :breathe :params])))
    (is (= wash (get-in result [:registry :effects :breathe :params :wash :default])))
    (is (= [(first wash)] (get-in instance [:params :wash])))
    (let [document (-> (files/freeze published)
                       (assoc-in [:model :registry :effects :breathe :params :wash :default] [(last wash) (first wash)])
                       (assoc-in [:model :registry :versions [:effects :breathe 2] :params :wash :default]
                                 [(last wash) (first wash)]))]
      (is (thrown? clojure.lang.ExceptionInfo (files/thaw base document))
          "an out-of-order saved gradient is rejected on open"))))

(def ^:private dangling-v1
  "An effect whose dimmer reads parameter :p (#703)."
  {:id :dangling :version 1 :name "Dangling"
   :params {:p {:type :number :default 0.5 :bounds [0 1]}}
   :outputs {:color {:op :const :value [1 0.5 0.2]} :dimmer {:op :param :key :p}}})

(def ^:private dangling-v2
  "A revision that drops :p while the dimmer still reads it."
  (assoc dangling-v1 :version 2 :params {:q {:type :number :default 0.1 :bounds [0 1]}}))

(def ^:private dangling-reference
  {:path [:outputs 1 :value :key] :message "Expression references an unknown parameter"})

(deftest owner-refuses-a-revision-that-drops-a-referenced-parameter
  (let [runtime (show/create-runtime {:workers 1 :workspace-options nil})
        model #(:model @(:state runtime))
        request (fn [time type payload]
                  (let [receipt (show/submit! runtime type payload)]
                    (show/tick! runtime time)
                    (deref receipt 1000 ::missing)))]
    (try
      (is (= :accepted (:status (request 0 :command {:op :registry/publish :kind :effects
                                                     :definition dangling-v1}))))
      (testing "a raw owner command (REPL, subscribers, compositions) is refused with the dangling path"
        (let [ack (request 20 :command {:op :registry/publish :kind :effects :definition dangling-v2})]
          (is (= :rejected (:status ack)) (pr-str ack))
          (is (= :invalid-definition (:code ack)))
          (is (= "Expression references an unknown parameter" (:message ack)))
          (is (= [dangling-reference] (get-in ack [:details :errors])))))
      (testing "the Studio editor and agent publish_effect route is refused the same way"
        (let [ack (request 40 :definition {:kind :effects :definition dangling-v2})]
          (is (= :rejected (:status ack)) (pr-str ack))
          (is (= [dangling-reference] (get-in ack [:details :errors])))))
      (is (= dangling-v1 (dissoc (get-in (model) [:registry :effects :dangling]) :imported-prefabs)))
      (is (nil? (get-in (model) [:registry :versions [:effects :dangling 2]])))
      (testing "the show still backs up and restores"
        (let [restored (files/thaw (model) (files/freeze (model)))]
          (is (= 1 (get-in restored [:registry :effects :dangling :version])))))
      (finally (show/stop! runtime)))))

(deftest a-show-already-holding-a-dangling-revision-names-it-and-recovers
  ;; Older owners admitted the revision through Afterglow's publish alone. The
  ;; control model below takes that same route to recreate such a show.
  (let [base (:model (command (demo/model) :v1 {:op :registry/publish :kind :effects :definition dangling-v1}))
        broken (:model (command base :v2 {:op :registry/publish :kind :effects :definition dangling-v2}))]
    (testing "the backup names the broken definition and the dangling reference"
      (let [error (try (files/freeze broken) nil (catch clojure.lang.ExceptionInfo e e))]
        (is (some? error))
        (is (= :invalid-show (:persistence/error (ex-data error))))
        (is (= [(update dangling-reference :path (partial into [:registry :effects :dangling]))]
               (:errors (ex-data error))))))
    (testing "publishing a corrected revision makes the show back up again, history intact"
      (let [fixed (assoc dangling-v1 :version 3)
            repaired (:model (command broken :v3 {:op :registry/publish :kind :effects :definition fixed}))
            document (files/freeze repaired)
            restored (files/thaw base document)]
        (is (= dangling-v2 (get-in document [:model :registry :versions [:effects :dangling 2]]))
            "the superseded revision is kept verbatim")
        (is (= fixed (get-in restored [:registry :effects :dangling])))
        (is (= dangling-v2 (get-in restored [:registry :versions [:effects :dangling 2]])))
        (testing "a cue pinned to the broken revision could launch it, so the backup names that revision"
          (let [cue {:id :pinned :version 1 :effect-id :dangling :effect-version 2 :params {}}
                pinned (:model (command repaired :pin {:op :cue/put :cue cue}))
                error (try (files/freeze pinned) nil (catch clojure.lang.ExceptionInfo e e))]
            (is (= [(update dangling-reference :path (partial into [:registry :versions [:effects :dangling 2]]))]
                   (:errors (ex-data error))))))))))

(deftest owner-refuses-a-cue-or-revision-the-backup-would-refuse
  ;; The backup now keeps a superseded revision that does not compile (#703).
  ;; The owner must not let a later command make the show unsaveable again.
  (let [base (:model (command (demo/model) :v1 {:op :registry/publish :kind :effects :definition dangling-v1}))
        legacy (-> (:model (command base :v2 {:op :registry/publish :kind :effects :definition dangling-v2}))
                   (command :v3 {:op :registry/publish :kind :effects :definition (assoc dangling-v1 :version 3)})
                   :model)
        runtime (show/create-runtime {:workers 1 :workspace-options nil})
        model #(:model @(:state runtime))
        request (fn [time type payload]
                  (let [receipt (show/submit! runtime type payload)]
                    (show/tick! runtime time)
                    (deref receipt 1000 ::missing)))]
    (try
      (is (= :accepted (:status (request 0 :import {:source (pr-str (files/freeze legacy))}))))
      (is (= dangling-v2 (get-in (model) [:registry :versions [:effects :dangling 2]])))
      (testing "a cue may not pin the superseded revision that does not compile"
        (let [ack (request 20 :command {:op :cue/put :cue {:id :pinned :effect-id :dangling :effect-version 2 :params {}}})]
          (is (= [:rejected :invalid-definition] [(:status ack) (:code ack)]) (pr-str ack))
          (is (= [[:effect-version]] (map :path (get-in ack [:details :errors]))))
          (is (= [dangling-reference] (get-in ack [:details :errors 0 :effect-errors]))))
        (let [ack (request 40 :command {:op :registry/publish :kind :cues
                                        :definition {:id :pinned :version 1 :effect-id :dangling :effect-version 2 :params {}}})]
          (is (= :invalid-definition (:code ack)) (pr-str ack))))
      (testing "a cue/put the backup would refuse is refused with its path"
        (let [ack (request 60 :command {:op :cue/put :cue {:id :aimed :effect-id :dangling :params {} :targets "nope"}})]
          (is (= :invalid-definition (:code ack)) (pr-str ack))
          (is (= [[:targets]] (map :path (get-in ack [:details :errors]))))))
      (testing "an expression the compiler cannot read is an :invalid-definition, not a stray exception"
        (let [ack (request 80 :command {:op :registry/publish :kind :effects
                                        :definition (assoc dangling-v1 :version 4 :outputs {:dimmer {:op :add :args 0}})})]
          (is (= :invalid-definition (:code ack)) (pr-str ack))
          (is (= [[]] (map :path (get-in ack [:details :errors]))))))
      (testing "pinning a revision that compiles still works"
        (is (= :accepted (:status (request 100 :command {:op :cue/put :cue {:id :pinned :effect-id :dangling
                                                                            :effect-version 1 :params {}}})))))
      (is (nil? (get-in (model) [:registry :cues :aimed])))
      (is (= 1 (get-in (model) [:registry :cues :pinned :effect-version])))
      (is (= 3 (get-in (files/thaw base (files/freeze (model))) [:registry :effects :dangling :version]))
          "every command the owner accepted leaves a show that saves and restores")
      (finally (show/stop! runtime)))))
