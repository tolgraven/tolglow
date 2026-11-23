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
  (doseq [id [:house/saw "house/saw"]]
    (let [binding {:type :lfo :waveform id :period {:phrases 0.5} :phase 0.25 :inputs {:slope 0.8}
                   :from :lamp/red :to "lamp/red"}
          form (source/draft {:type :enum :values [:lamp/red "lamp/red"]} :lamp/red binding)]
      (is (= binding (source/binding-config (assoc form :period "1/2"))))))
  (doseq [[schema base endpoints] [[{:type :boolean} true [false true]]
                                  [{:type :integer :bounds [0 255]} 64 [0 255]]
                                  [{:type :color} [1 0 0] [[0 0 0] [1 1 1]]]
                                  [{:type :vector :size 3 :bounds [-1 1]} [0 0 0] [[-1 -1 -1] [1 1 1]]]]]
    (is (= endpoints ((juxt :from :to) (source/draft schema base nil)))))
  (is (thrown? js/Error (source/binding-config (assoc (source/draft {} 0 nil) :period "0")))))

(deftest a-binding-request-retains-its-show-and-locks-only-its-own-control
  (let [db {:show {:runtime-id "studio" :epoch 2}}
        result (source/request db "bind" "2:1" :level {:type :lfo})]
    (is (= {:runtime-id "studio" :expected-epoch 2 :instance-id "2:1" :param :level :binding {:type :lfo}}
           (get-in result [:studio/http :body])))
    (is (true? (get-in result [:studio/http :typed?])))
    (is (empty? (source/request (:db result) "unbind" "2:1" :level nil)))
    (is (seq (source/request (:db result) "unbind" "2:2" :level nil)))
    (is (seq (source/request (assoc-in (:db result) [:show :epoch] 3) "unbind" "2:1" :level nil)))))

(deftest fixture-blocks-observe-real-members-of-a-restricted-rig
  (let [rig (mapv #(hash-map :id % :position [1 2 3]) [:a :b :c])]
    (is (= [{:id :b}] (source/fixture-options rig {:targets [:a :b] :outputs [{:attribute :dimmer :value 1 :targets [:b :c]}]})))
    (is (empty? (source/fixture-options rig {:targets []})))
    (is (empty? (source/fixture-options rig {:launch-targets []})))
    (is (= [:b :a] (mapv :id (source/fixture-options rig {:launch-targets [:b :c :a] :targets [:a :b]}))))
    (is (= [:a :b :c] (mapv :id (source/fixture-options rig {:outputs {:dimmer 1}}))))))
