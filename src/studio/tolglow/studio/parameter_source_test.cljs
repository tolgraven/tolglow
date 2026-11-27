(ns tolglow.studio.parameter-source-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [tolglow.studio.parameter-source :as source]
            [tolglow.studio.events]
            [tolglow.studio.performance :as performance]))

(deftest monitors-follow-active-drivers-and-editors-retain-held-or-faulted-bindings
  (let [manual {:parameter :level :schema {:type :number}}
        held {:parameter :depth :schema {:type :number}}
        axes {:attribute :aim :components [[:x {}] [:y {}] [:z {}]]}
        instance {:params {:level 1 :depth 0 :x 0 :y 0 :z 0}
                  :parameter-sources {:level {:effective-value 1 :sources []}
                                      :depth {:effective-value 0 :sources [{:type :lfo :held? true}]}
                                      :z {:effective-value nil :sources [{:type :patch}]}}}
        controls [manual held axes]]
    (is (empty? (performance/driven-controls (dissoc instance :parameter-sources) controls)))
    (is (= [axes] (performance/driven-controls instance controls)))
    (is (source/bound? instance :depth))
    (is (false? (source/driven? (assoc-in instance [:parameter-sources :depth :sources] [{:type :random :pending? true}]) :depth)))
    (is (false? (source/driven? (assoc-in instance [:parameter-sources :z :error] "Response resolution failed") :z)))
    (is (nil? (source/result-visual :visual instance :depth {})))
    (is (nil? (source/result-visual :visual instance :level {})))
    (let [steady (assoc-in instance [:parameter-sources :depth :sources] [{:type :random :held? false}])
          mixed (update-in instance [:parameter-sources :depth :sources] conj {:type :patch})
          faulted (assoc-in instance [:parameter-sources :depth :sources] [{:type :lfo :error "Invalid waveform"}])]
      (is (= [held axes] (performance/driven-controls steady controls)))
      (is (= [held axes] (performance/driven-controls mixed controls)))
      (is (source/bound? faulted :depth))
      (is (= [axes] (performance/driven-controls faulted controls)))
      (is (= held (:selected (performance/resolve-monitor (performance/driven-controls steady controls) (performance/control-id manual) :level)))))))

(deftest source-bindings-round-trip-typed-endpoints-and-musical-fractions
  (is (= "user/wave" (:waveform (source/editor-draft {} 0 nil [{:id "user/wave"}]))))
  (is (= :sine (:waveform (source/editor-draft {} 0 nil [{:id :triangle} {:id :sine}]))))
  (is (= :launch (:phase-reference (source/draft {} 0 nil))))
  (is (= :launch (:phase-reference (source/draft {} 0 {:waveform :saw :phase 0.25}))))
  (doseq [id [:house/saw "house/saw"] phase-reference [:launch :transport]]
    (let [binding {:type :lfo :waveform id :period {:phrases 0.5} :phase 0.25 :inputs {:slope 0.8}
                   :phase-reference phase-reference :from :lamp/red :to "lamp/red"}
          form (source/draft {:type :enum :values [:lamp/red "lamp/red"]} :lamp/red binding)]
      (is (= binding (source/binding-config (assoc form :period "1/2"))))))
  (doseq [[schema base endpoints] [[{:type :boolean} true [false true]]
                                  [{:type :integer :bounds [0 255]} 64 [0 255]]
                                  [{:type :integer :bounds [1.2 4.8]} 2 [2 4]]
                                  [{:type :integer :recommended [1.2 4.8]} 2 [2 4]]
                                  [{:type :integer :recommended [1.2 1.8]} 2 [0 1]]
                                  [{:type :color} [1 0 0] [[0 0 0] [1 1 1]]]
                                  [{:type :color :bounds [-1 0.8]} [0 0.2 0.4] [[0 0 0] [0.8 0.8 0.8]]]
                                  [{:type :color :bounds [0.2 2] :recommended [0.4 0.6]} [0.2 0.4 0.6] [[0.2 0.2 0.2] [1 1 1]]]
                                  [{:type :color :recommended [0.2 3]} [0.2 0.4 0.6] [[0.2 0.2 0.2] [1 1 1]]]
                                  [{:type :color :recommended [2 3]} [0.2 0.4 0.6] [[0 0 0] [1 1 1]]]
                                  [{:type :color :recommended [-3 -2]} [0.2 0.4 0.6] [[0 0 0] [1 1 1]]]
                                  [{:type :vector :size 3 :bounds [-1 1]} [0 0 0] [[-1 -1 -1] [1 1 1]]]]]
    (is (= endpoints ((juxt :from :to) (source/draft schema base nil)))))
  (is (thrown? js/Error (source/binding-config (assoc (source/draft {} 0 nil) :period "0")))))

(deftest a-binding-request-retains-its-show-and-locks-only-its-own-control
  (let [db {:show {:runtime-id "studio" :epoch 2}}
        result (source/request db "bind" "2:1" :level {:type :lfo} "attach")]
    (is (= {:runtime-id "studio" :expected-epoch 2 :instance-id "2:1" :param :level :binding {:type :lfo} :request-id "attach"}
           (get-in result [:studio/http :body])))
    (is (true? (get-in result [:studio/http :typed?])))
    (is (empty? (source/request (:db result) "unbind" "2:1" :level nil "detach")))
    (is (seq (source/request (:db result) "unbind" "2:2" :level nil "detach")))
    (is (seq (source/request (assoc-in (:db result) [:show :epoch] 3) "unbind" "2:1" :level nil "detach")))))

(deftest a-destination-move-sends-one-typed-binding-request
  (let [db {:show {:runtime-id "studio" :epoch 2}}
        endpoints {:from :step/one :to "step/two"}
        expected [[2 12] :wash 4 1]
        move {:from-param :cue/level :owner ["studio" 2] :expected-source expected}
        result (source/request db "bind" "2:1" :cue/step endpoints "move" move)]
    (is (= "/api/modulation/bind" (get-in result [:studio/http :path])))
    (is (true? (get-in result [:studio/http :typed?])))
    (is (= {:runtime-id "studio" :expected-epoch 2 :instance-id "2:1"
            :from-param :cue/level :param :cue/step :binding endpoints :expected-source expected :request-id "move"}
           (get-in result [:studio/http :body])))
    (is (empty? (source/request (:db result) "unbind" "2:1" :cue/level nil "other")))
    (is (empty? (source/request (:db result) "bind" "2:1" :cue/step {:type :lfo} "other")))
    (is (empty? (source/request (assoc-in db [:show :runtime-id] "replaced") "bind" "2:1" :cue/step endpoints "move" move))))
  (let [hue {:type :number :bounds [0 360]}
        step {:type :enum :values [:step/one "step/two"]}
        instance {:definition {:params {:cue/level {} :cue/hue hue :cue/step step :occupied {}}}
                  :parameter-sources {:occupied {:sources [{:type :lfo :held? true}]}
                                      :cue/step {:sources [{:type :random}]}}}]
    (is (= [[:cue/hue hue] [:cue/step step]] (source/destination-options instance :cue/level)))
    (let [schema {:type :integer :bounds [-1000 1000] :default 0 :category :priority :name "Layer priority"}
          priority {:param :layer/priority :schema schema}]
      (testing "the layer priority destination follows the effect parameters while it is free"
        (is (= [[:cue/hue hue] [:cue/step step] [:layer/priority schema]]
               (source/destination-options instance :cue/level priority)))
        (is (= [[:cue/hue hue] [:cue/step step]]
               (source/destination-options (assoc-in instance [:parameter-sources :layer/priority :sources] [{:type :lfo}])
                                           :cue/level priority)))
        (is (= [[:cue/hue hue] [:cue/level {}] [:cue/step step]]
               (source/destination-options instance :layer/priority priority))))
      (testing "the projected destination name becomes the keyword the bind request sends"
        (is (= :layer/priority (:param (source/priority-destination {:layer-priority-destination {:param "layer/priority" :schema schema}}))))
        (is (nil? (source/priority-destination {}))))
      (testing "the schema of either kind of destination comes from the projection"
        (is (= schema (source/destination-schema instance priority :layer/priority)))
        (is (= hue (source/destination-schema instance priority :cue/hue)))
        (is (nil? (source/destination-schema instance nil :layer/priority)))))))

(deftest layer-priority-bindings-carry-typed-hysteresis
  (let [schema {:type :integer :bounds [-1000 1000] :default 0 :category :priority}
        form (source/editor-draft schema 0 nil [{:id :sine}])]
    (is (= [-1 1 "0.25" "0" :beats] ((juxt :from :to :band :dwell :dwell-unit) form)))
    (testing "a dwell authored in bars or phrases keeps its unit through the editor"
      (doseq [dwell [{:bars 1} {:phrases 0.5} {:beats 3}]]
        (let [bound {:type :lfo :waveform :sine :period {:beats 1} :phase 0 :phase-reference :launch
                     :from -1 :to 1 :inputs {} :hysteresis {:band 0.25 :dwell dwell}}
              draft (source/editor-draft schema 0 bound [{:id :sine}])]
          (is (= (ffirst dwell) (:dwell-unit draft)))
          (is (= {:band 0.25 :dwell dwell} (:hysteresis (source/binding-config draft)))))))
    (is (= {:band 0.25} (:hysteresis (source/binding-config form))))
    (is (= {:band 0 :dwell {:beats 0.5}} (:hysteresis (source/binding-config (assoc form :band "0" :dwell "1/2")))))
    (is (thrown? js/Error (source/binding-config (assoc form :band "-1"))))
    (let [bound {:type :lfo :waveform :triangle :period {:beats 4} :phase 0 :phase-reference :transport
                 :from -2 :to 3 :inputs {} :hysteresis {:band 0.5 :dwell {:beats 2}}}]
      (is (= bound (source/binding-config (assoc (source/editor-draft schema 0 bound [{:id :sine}]) :period "4")))))
    (testing "effect parameters never send hysteresis"
      (is (not (contains? (source/binding-config (source/editor-draft {:type :number} 0 nil [{:id :sine}])) :hysteresis))))))

(deftest source-controls-wait-for-their-committed-revision-and-correlate-queued-results
  (let [view {:runtime-id "studio" :epoch 2 :revision 12}
        db {:show view :server-projection {:view view}}
        move {:from-param :level :owner ["studio" 2] :expected-source [[2 11] :wash 4 1]}
        pending (:db (source/request db "bind" "2:1" :step {:from 1 :to 5} "move" move))
        key [["studio" 2] "2:1" :level]
        response #(source/record-reply %1 key "move" %2)
        status #(source/request-state % "2:1" :level)
        accepted (response pending {:status :accepted :revision 14})
        waiting (response pending {:status :pending})
        failed (assoc-in waiting [:server-projection :view :parameter-source-outcomes]
                         [{:request-id "move" :epoch 2 :status :rejected :message "Source changed"}])
        succeeded (assoc-in waiting [:server-projection :view :parameter-source-outcomes]
                            [{:request-id "move" :epoch 2 :status :accepted :revision 14}
                             {:request-id "later" :epoch 2 :status :accepted :revision 15}])
        expired (response pending {:status :expired})
        retry (:db (source/request expired "bind" "2:1" :step {:from 1 :to 5} "retry" move))]
    (is (:pending? (status accepted)))
    (is (nil? (status (assoc-in accepted [:server-projection :view :revision] 14))))
    (is (= {:pending? true :waiting? true} (status waiting)))
    (is (= {:error "Source changed"} (status failed)))
    (is (nil? (status succeeded)))
    (is (not (:pending? (status expired))))
    (is (string? (:error (status expired))))
    (is (= retry (response retry {:status :accepted :revision 14})))
    (let [stale (assoc-in pending [:show :runtime-id] "replaced")]
      (is (nil? (status stale)))
      (is (= stale (response stale {:status :rejected :message "Late response"}))))))

(deftest fixture-blocks-observe-real-members-of-a-restricted-rig
  (let [rig (mapv #(hash-map :id % :position [1 2 3]) [:a :b :c])]
    (is (= [{:id :b}] (source/fixture-options rig {:targets [:a :b] :outputs [{:attribute :dimmer :value 1 :targets [:b :c]}]})))
    (is (empty? (source/fixture-options rig {:targets []})))
    (is (empty? (source/fixture-options rig {:launch-targets []})))
    (is (= [:b :a] (mapv :id (source/fixture-options rig {:launch-targets [:b :c :a] :targets [:a :b]}))))
    (is (= [:a :b :c] (mapv :id (source/fixture-options rig {:outputs {:dimmer 1}}))))))

(deftest cards-follow-the-composed-order-and-show-a-modulated-tier
  (let [low {:id "0:1" :priority 0
             :parameter-sources {:layer/priority {:base-value 0 :effective-value 1 :sources [{:type :lfo}]}}}
        high {:id "0:2" :priority 0}
        show {:instances [high low] :active-instance-order ["0:1" "0:2"] :effective-instance-order ["0:2" "0:1"]}
        destination {:param :layer/priority}]
    (is (= ["0:2" "0:1"] (mapv :id (performance/effective-instances show))))
    (is (= ["0:1" "0:2"] (mapv :id (performance/ordered-instances show))))
    (is (= ["0:1" "0:2"] (mapv :id (performance/effective-instances (dissoc show :effective-instance-order)))))
    (is (= 1 (performance/modulated-priority low destination)))
    (is (nil? (performance/modulated-priority high destination)))
    (is (nil? (performance/modulated-priority (assoc-in low [:parameter-sources :layer/priority :effective-value] 0) destination)))
    (is (nil? (performance/modulated-priority (assoc-in low [:parameter-sources :layer/priority :sources 0 :held?] true) destination)))))

(deftest an-lfo-panel-remembers-through-a-gap-and-forgets-a-confirmed-absence
  (let [lfo {:type :lfo :binding {:waveform :sine}}
        step (fn [state & {:as input}] (source/remembered-lfo state input))
        seen (step {} :current lfo :revision 10 :pending? false)]
    (is (= lfo (:known seen)))
    (testing "a brief gap during a round trip (resume) keeps the LFO"
      (is (= lfo (:known (step seen :current nil :revision 10 :pending? false))) "at the revision that showed it")
      (is (= lfo (:known (step seen :current nil :revision 15 :pending? true))) "while a request is pending"))
    (testing "an unbind from the REPL is confirmed at the next revision without it"
      (is (nil? (:known (step seen :current nil :revision 11 :pending? false)))))
    (testing "this panel's own detach is not learned again while the projection still shows it"
      (let [detaching (assoc seen :known nil :detaching (source/lfo-identity lfo))]
        (is (nil? (:known (step detaching :current lfo :revision 11 :pending? true))))
        (is (nil? (:detaching (step detaching :current nil :revision 12 :pending? false))))))
    (testing "a new binding is learned"
      (let [other {:type :lfo :binding {:waveform :saw}}]
        (is (= other (:known (step seen :current other :revision 11 :pending? false))))))))

(deftest a-re-attach-with-identical-settings-is-a-new-lfo
  (let [old {:type :lfo :binding {:waveform :sine} :identity [100 :wave 1 1]}
        again {:type :lfo :binding {:waveform :sine} :identity [140 :wave 1 1]}
        detaching {:known nil :seen 10 :detaching (source/lfo-identity old)}]
    (is (nil? (:known (source/remembered-lfo detaching {:current old :revision 11 :pending? true})))
        "the detached LFO is not learned again")
    (is (= again (:known (source/remembered-lfo detaching {:current again :revision 12 :pending? false})))
        "the projection jumping straight to an identical re-attach is learned")
    (is (= [100 :wave 1 1] (source/lfo-identity old)))
    (is (= {:waveform :sine} (source/lfo-identity {:binding {:waveform :sine}})) "older projections compare by settings")))

(deftest plot-choices-leave-with-their-launch
  (is (= {["a" "param/level"] {:window 8}}
         (tolglow.studio.events/prune-plot-views {["a" "param/level"] {:window 8} ["gone" "param/level"] {:window 16}}
                                                 [{:id "a"} {:id "b"}]))))
