(ns tolglow.material-inputs-test
  (:require [afterglow.render.frame :as frame]
            [afterglow.runtime :as runtime]
            [afterglow.runtime.engine :as engine]
            [afterglow.runtime.keyframes :as keyframes]
            [clojure.test :refer [deftest is]]
            [tolglow.compiler :as compiler]
            [tolglow.compiler.batch :as batch]
            [tolglow.compiler-test :refer [command]]
            [tolglow.demo :as demo]
            [tolglow.effects.prefabs :as prefabs]
            [tolglow.effects.variable-blocks :as variable-blocks]
            [tolglow.persistence :as files]))

(def audio {:op :variable :path [:audio :features :low-rms]})
(def tint {:op :variable :path [:controller "watch" :tint] :type :vec3})
(def wet {:op :variable :path [:faders 0]})
(def transmission
  {:name "Bass transmission" :mode :multiply :inputs {:gain audio} :glsl "gain"})

(def light
  {:id :reactive-light :version 1
   :keyframes {:interval {:beats 1}}
   :params {:level {:type :number :default 0.5 :bounds [0.0 1.0]}}
   :blocks {[:varyings :lens] {:base 1.0 :stages [{:block :room/bass}]}
            [:params :level] {:stages [{:block :room/bass}]}
            [:outputs :color] {:stages [{:block :optics/gel :inputs {:transmission tint} :amount wet}]}}
   :outputs {:dimmer {:op :param :key :level}
             :focus {:op :varying :key :lens}
             :color [0.5 0.5 1.0]}})

(defn- test-model []
  (-> (demo/model)
      (assoc-in [:settings :prefabs :definitions :room/bass] transmission)
      (assoc-in [:registry :effects (:id light)] light)
      (assoc-in [:variables :audio :features :low-rms] 0.5)
      (assoc-in [:variables :controller "watch" :tint] [0.5 0.75 0.0])
      (assoc-in [:variables :faders] [0.5])
      prefabs/startup))

(defn- run-frame [rig step model prior beat]
  (engine/advance (assoc-in model [:transport :beat] beat)
                  {:time-ms (* 500.0 beat) :fixtures rig :keyframes (:keyframes prior)} (constantly step)))

(deftest materials-read-live-inputs-on-each-numeric-backend
  (let [model (test-model) rig (get-in model [:rig :fixtures])
        effect (get-in model [:registry :effects (:id light)])
        launch (command model :launch :effect/launch :effect-id (:id light) :time-ms 0)
        id (get-in launch [:ack :instance-id])
        expected-paths #{[:variables :audio :features :low-rms] [:variables :controller "watch" :tint] [:variables :faders 0]}]
    (is (= transmission (get-in effect [:imported-prefabs :room/bass])))
    (is (= effect (get-in (:model (files/read-string (files/write-string model))) [:registry :effects (:id light)])))
    (doseq [step [(compiler/compile-effect effect) (batch/compile-effect effect rig)
                  (batch/compile-effect effect rig {:enabled? false})]]
      (let [tick #(run-frame rig step %1 %2 %3)
            first-frame (tick (:model launch) nil 0)
            intermediate (tick (assoc-in (:model first-frame) [:variables :unused] 0.25) first-frame 0.25)
            new-input (assoc-in (:model intermediate) [:variables :audio :features :low-rms] 0.75)
            changed (tick new-input intermediate 0.5)
            fields #(get-in (frame/resolve-contributions rig (:contributions %) {}) [:values :head-1])
            initial (get-in first-frame [:keyframes id])
            stable (get-in intermediate [:keyframes id])
            refreshed (get-in changed [:keyframes id])
            observe (fn [r path] (runtime/observe (get-in r [:model :instances id]) [path :head-1]))]
        (is (= expected-paths (set (::keyframes/variable-paths (meta step)))))
        (is (empty? (:fault-events first-frame)))
        (is (= 0.25 (:dimmer (fields first-frame)) (:result (observe first-frame [:params :level]))))
        (is (= 0.5 (:focus (fields first-frame)) (:result (observe first-frame [:varyings :lens]))))
        (is (= [0.375 0.4375 0.5] (:color (fields first-frame)) (:result (observe first-frame [:outputs :color]))))
        (is (= (:target-clock initial) (:target-clock stable)))
        (is (not= (:target-clock stable) (:target-clock refreshed)))
        (is (= 0.375 (:dimmer (fields changed))))
        (is (= 0.75 (:result (observe changed [:varyings :lens]))))
        (is (= 0.5 (get-in changed [:model :instances id :params :level])))))))

(deftest global-chain-and-ordinary-output-can-read-a-signal
  (let [model (test-model)
        config {:bindings {[:variables :room :level] {:base 0.5 :stages [{:block :room/bass}]}}}
        plan (variable-blocks/compile-settings config (prefabs/catalog model))
        sampled (variable-blocks/sample plan model 0.0)
        resolved (variable-blocks/resolved model sampled)
        direct {:id :meter :version 1 :outputs {:dimmer {:op :variable :path [:room :level]}}}
        ctx {:frame {:time-ms 0.0 :variables (:variables resolved) :transport {:beat 0.0}
                     :fixtures (get-in model [:rig :fixtures])} :instance {}}
        result ((compiler/compile-effect direct) ctx)]
    (is (= 0.25 (:result (variable-blocks/inspect sampled [:variables :room :level]))))
    (is (= 0.25 (:value (first (:assignments result)))))
    (is (= 0.5 (get-in resolved [:variables :audio :features :low-rms])))
    (is (nil? (get-in model [:variables :room :level])))))

(deftest invalid-variable-references-fail-before-use
  (doseq [bad-path [[] :audio [false] [:audio -1]]]
    (is (thrown? clojure.lang.ExceptionInfo
                 (compiler/compile-effect {:id :invalid :version 1 :outputs {:dimmer {:op :variable :path bad-path}}}))))
  (doseq [entries [[{:attribute :color :value tint} {:attribute :dimmer :value (dissoc tint :type)}]
                   [{:attribute :dimmer :value (dissoc tint :type)} {:attribute :color :value tint}]]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"agree"
                          (compiler/compile-effect {:id :invalid :version 1 :outputs entries}))))
  (is (thrown? clojure.lang.ExceptionInfo
               (prefabs/validate! {:definitions {:room/bass (assoc-in transmission [:inputs :gain :type] :boolean)}}))))
