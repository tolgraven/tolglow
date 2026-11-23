(ns tolglow.material-library-test
  (:require [afterglow.control.model :as control]
            [afterglow.runtime :as runtime]
            [afterglow.reactive :as rx]
            [clojure.test :refer [deftest is]]
            [tolglow.demo :as demo]
            [tolglow.effects.block-trace :as trace]
            [tolglow.effects.prefabs :as prefabs]
            [tolglow.effects.variable-blocks :as variables]
            [tolglow.persistence :as files]
            [tolglow.show :as show]))

(defn material [factor]
  {:name "Copper screen" :mode :multiply :inputs {:factor factor} :glsl "factor"})

(defn lamp [id version]
  {:id id :version version :outputs {:dimmer 1.0}
   :blocks {[:outputs :dimmer] {:stages [{:block :tour/copper}]}}})

(defn submit [rt clock kind payload]
  (let [ack (show/submit! rt kind payload)]
    (show/tick! rt (swap! clock + 25.0))
    (deref ack 1000 ::timeout)))

(deftest published-effects-capture-only-the-library-entries-they-use
  (let [a (material 0.8) b (material 0.2) scope {:definitions {:tour/copper a :tour/unused b}}
        initial (assoc-in (demo/model) [:settings :prefabs] scope)
        rt (show/create-runtime {:model initial})
        clock (atom 0.0)
        request #(submit rt clock %1 %2)
        publish (fn [definition] (request :definition {:kind :effects :definition definition}))]
    (try
      (let [effect (lamp :first 1)
            raw {:op :registry/publish :kind :effects :command-id "first-revision" :definition effect}]
        (is (= :accepted (:status (request :command raw))))
        (is (= :accepted (:status (publish (lamp :second 1)))))
        (is (= :accepted (:status (request :settings {:scope :prefabs :values {:definitions {:tour/copper b}}}))))
        (is (not= :rejected (:status (request :command raw))))
        (is (= :accepted (:status (publish (lamp :first 2)))))
        (let [m (:model @(:state rt))
              first-v1 (get-in m [:registry :versions [:effects :first 1]])
              first-v2 (get-in m [:registry :effects :first])
              second-v1 (get-in m [:registry :effects :second])
              decoded (:model (files/read-string (files/write-string m)))]
          (is (= {:tour/copper a} (:imported-prefabs first-v1) (:imported-prefabs second-v1)))
          (is (= {:tour/copper b} (:imported-prefabs first-v2)))
          (is (= (:registry m) (:registry decoded)))
          (is (= (prefabs/catalog m) (prefabs/catalog decoded)))
          (is (= first-v1 (prefabs/capture {} first-v1)))))
      (finally (show/stop! rt)))))

(deftest a-library-edit-changes-global-output-while-pinned-effects-keep-their-material
  (let [initial (-> (demo/model)
                    (assoc-in [:settings :prefabs :definitions] {:tour/copper (material 0.8)})
                    (assoc-in [:settings :variable-blocks :bindings [:variables :stage-light]]
                              {:base 1.0 :stages [{:block :tour/copper}]}))
        rt (show/create-runtime {:model initial})
        clock (atom 0.0)
        request #(submit rt clock %1 %2)
        cue {:id :pinned-copper :effect-id :copper-light :effect-version 1}]
    (try
      (request :definition {:kind :effects :definition (lamp :copper-light 1)})
      (request :command {:op :cue/put :cue cue})
      (let [instance-id (:instance-id (request :command {:op :cue/launch :cue-id :pinned-copper}))
            reading #(-> (get-in @(:state rt) [:model :instances instance-id])
                         (runtime/observe [[:outputs :dimmer] nil]) :result)]
        (is (= 0.8 (reading) (:stage-light (show/variables rt))))
        (is (= :accepted (:status (request :settings {:scope :prefabs
                                                       :values {:definitions {:tour/copper (material 0.2)}}}))))
        (is (= 0.8 (reading)))
        (is (= 0.2 (:stage-light (show/variables rt))))
        (request :settings {:scope :prefabs :values {:definitions {}}})
        (is (= 0.2 (:stage-light (show/variables rt))) "A material in a live global chain cannot be removed")
        (is (= 0.2 (get-in @(:state rt) [:model :settings :prefabs :definitions :tour/copper :inputs :factor]))))
      (finally (show/stop! rt)))))

(deftest startup-customization-and-local-precedence
  (let [stage (material 0.9) local (material 0.4)
        config {:tour/copper stage}
        effect (lamp :one 1)
        model (-> (control/new-show)
                  (assoc-in [:settings :prefabs :definitions] config)
                  (assoc-in [:registry :effects :one] effect))]
    (is (= config (get-in (prefabs/startup model) [:registry :effects :one :imported-prefabs])))
    (let [settings {:bindings {[:variables :light] {:base 1.0 :stages [{:block :tour/copper}]}}}
          sample (fn [settings] (-> (variables/compile-settings settings config)
                                    (variables/sample model 0.0) (get-in [:values [:variables :light]])))]
      (is (= 0.9 (sample settings)))
      (is (= 0.4 (sample (assoc settings :prefabs {:tour/copper local})))))
    (is (= {} (:imported-prefabs (prefabs/capture config (assoc effect :prefabs {:tour/copper local})))))
    (let [builtin (assoc (lamp :builtin 1) :blocks {[:outputs :dimmer] {:stages [{:block :lfo/sine}]}})
          configured (-> model
                         (assoc-in [:registry :effects :one] (prefabs/capture {} builtin))
                         (assoc-in [:settings :prefabs :definitions :lfo/sine] stage))]
      (is (= {} (get-in (prefabs/startup configured) [:registry :effects :one :imported-prefabs]))))
    (is (thrown? Exception (prefabs/validate! {:definitions {:tour/bad (assoc stage :glsl "missing")}})))))

(deftest the-live-picker-updates-only-when-its-material-sources-change
  (let [model (assoc-in (demo/model) [:settings :prefabs :definitions] {:tour/copper (material 0.8)})
        graph (trace/register! (rx/create-system model))]
    (try
      (let [query (rx/subscribe graph [:runtime/block-library])
            first-result @query]
        (is (= "Copper screen" (get-in first-result [:tour/copper :name])))
        (rx/publish! graph (assoc model :frame-number 12))
        (is (identical? first-result @query))
        (rx/publish! graph (assoc-in model [:settings :prefabs :definitions :tour/copper] (material 0.6)))
        (is (= 0.6 (get-in @query [:tour/copper :inputs :factor]))))
      (finally (rx/close! graph)))))
