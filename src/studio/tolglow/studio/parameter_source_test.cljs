(ns tolglow.studio.parameter-source-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [tolglow.studio.parameter-source :as source]
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
                                  [{:type :color} [1 0 0] [[0 0 0] [1 1 1]]]
                                  [{:type :color :bounds [-1 0.8]} [0 0.2 0.4] [[0 0 0] [0.8 0.8 0.8]]]
                                  [{:type :color :bounds [0.2 2]} [0.2 0.4 0.6] [[0.2 0.2 0.2] [1 1 1]]]
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
    (is (= [[:cue/hue hue] [:cue/step step]] (source/destination-options instance :cue/level)))))

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
