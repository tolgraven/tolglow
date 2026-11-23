(ns tolglow.block-bindings-test
  (:require [afterglow.control.model :as control]
            [afterglow.render.frame :as frame]
            [afterglow.runtime :as runtime]
            [afterglow.runtime.engine :as engine]
            [clojure.test :refer [deftest is]]
            [tolglow.compiler :as compiler]
            [tolglow.effects.variable-blocks :as variable-blocks]
            [tolglow.persistence :as files]
            [tolglow.show :as show]
            [tolglow.signals :as signals]
            [tolglow.compiler.batch :as batch]
            [tolglow.effects.block-trace :as trace]
            [tolglow.compiler-test :refer [command]]))

(def through-water
  {:id :pool :version 1 :params {:level {:type :number :default 0.8 :bounds [0.0 1.0]}}
   :blocks {[:variables :weather :density] {:stages [{:block :lfo/sine :mode :multiply}]}
            [:params :level] {:stages [{:block :optics/through-water}]}
            [:varyings :ripple] {:base {:op :param :key :level}
                                 :stages [{:block :optics/ribbed-glass}]}
            [:outputs :color] {:stages [{:block :optics/gel}
                                        {:block :optics/fog-transmission
                                         :inputs {:density {:op :variable :path [:weather :density]}
                                                  :distance 2.0}}]}}
   :outputs {:dimmer {:op :param :key :level} :focus {:op :varying :key :ripple}
             :color [0.9 0.6 0.4]}})

(def rig
  (mapv (fn [id x address]
          {:id id :position [x 1.0 -0.5] :universe 1 :address address
           :channels {:red 1 :green 2 :blue 3 :dimmer 4 :focus 5}})
        [:near :far] [0.8 1.92] [1 6]))

(defn- launch
  ([] (launch through-water))
  ([definition]
  (let [show (control/new-show {:show-id "pool" :transport {:beat 1.0 :bpm 120.0 :playing? true}})
        published (command show :pub :registry/publish :kind :effects :definition definition)
        cued (command (:model published) :put :cue/put :cue {:id :swim :effect-id :pool})
        launched (command (:model cued) :launch :cue/launch :cue-id :swim :time-ms 0)]
    [(-> (:model launched) (assoc-in [:variables :weather :density] 0.2)
         (assoc-in [:transport :beat] 1.25))
     (get-in launched [:ack :instance-id])])))

(deftest cue-variables-private-inputs-and-light-use-one-chain
  (let [[model id] (launch)
        advance #(engine/advance model {:time-ms 125.0 :fixtures rig} %)
        scalar (advance compiler/compile-effect)
        typed (advance (batch/compiler-for-rig rig))
        values (:values (frame/resolve-frame rig (:assignments scalar)))
        instance #(get-in % [:model :instances id])]
    (is (empty? (:fault-events scalar)))
    (is (empty? (:fault-events typed)))
    (is (= :batch (:backend (meta (batch/compile-effect through-water rig)))))
    (is (not= (get-in values [:near :focus]) (get-in values [:far :focus])))
    (doseq [result [scalar typed] fid [:near :far]
            [leaf attr] [[[:params :level] :dimmer] [[:varyings :ripple] :focus] [[:outputs :color] :color]]]
      (let [observation (runtime/observe (instance result) [leaf fid])]
        (is (= 125.0 (:time-ms observation)))
        (is (= (get-in values [fid attr]) (:result observation)))))
    (let [query [:runtime/block-trace id [:outputs :color] :near]
          sample (trace/inspect {:db (atom (:model scalar))} query)
          view {:display-mode :block-trace :instance-id id :epoch (:epoch model) :definition-version 1
                :binding-path [:outputs :color] :target-id :near}]
      (is (= [:optics/gel :optics/fog-transmission] (mapv :block (:stages sample))))
      (is (= sample (get-in (trace/project-display :runtime (:model scalar) view) [:block-trace :sample])))
      (is (nil? (get-in (trace/project-display :runtime (:model scalar) (assoc view :epoch -1))
                        [:block-trace :sample]))))
    (is (= 0.8 (get-in (instance scalar) [:params :level])))
    (is (= 0.2 (get-in scalar [:model :variables :weather :density])))
    (is (nil? (runtime/observe (instance scalar) [[:outputs :color] :unknown])))))

(deftest invalid-bindings-fail-at-publication
  (doseq [bindings [{[:varyings :a] {:base {:op :varying :key :b} :stages []}
                     [:varyings :b] {:base {:op :varying :key :a} :stages []}}
                    {[:params :missing] {:stages []}}
                    {[:outputs :zoom] {:stages []}}
                    {[:params :level] {:stages [{:block :optics/gel}]}}]]
    (is (thrown? clojure.lang.ExceptionInfo
                 (compiler/compile-plan (assoc through-water :blocks bindings))))))

(deftest registered-waveform-derivative-becomes-a-prefab
  (let [library (signals/new-registry)
        prefab (signals/waveform-block library :sine {:channel :slope})
        effect {:id :derived :version 1 :prefabs {:music/slope prefab}
                :blocks {[:outputs :pan] {:stages [{:block :music/slope :amount 0.05}]}}
                :outputs {:pan 0.5}}
        context {:frame {:time-ms 100.0 :transport {:beat 2.25} :fixtures rig}
                 :instance {:started-at-ms 0 :started-at-beat 2} :assignments []}
        result (first (:assignments ((compiler/compile-effect effect) context)))]
    (is (= {:id :sine :version 1 :channel :slope} (:waveform prefab)))
    (is (< (abs (- (:value result) 0.475)) 1.0e-10))))

(deftest global-variable-resolution-and-transit-restart
  (let [[model id] (launch)
        settings {:bindings {[:variables :weather :density]
                             {:stages [{:block :optics/fog-transmission
                                        :inputs {:extinction 1.0 :density 0.2 :distance 2.0}}]}}}
        model (-> model (assoc-in [:settings :variable-blocks] settings) (assoc-in [:rig :fixtures] rig))
        rt (show/create-runtime {:model model})]
    (try
      (doseq [t [125.0 150.0]]
        (show/tick! rt t)
        (let [snapshot @(:state rt) m (:model snapshot)
              expected (* 0.2 (Math/exp -0.4))
              data (trace/inspect {:db (atom (trace/source snapshot))}
                                  [:runtime/block-trace nil [:variables :weather :density]])]
          (is (nil? (get-in snapshot [:status :error])))
          (is (= 0.2 (:base data) (get-in m [:variables :weather :density])))
          (is (= expected (:result data) (get-in (show/variables rt) [:weather :density])))
          (is (= expected (:base (runtime/observe (get-in m [:instances id]) [[:variables :weather :density] :near]))))
          (let [decoded (:model (files/read-string (files/write-string m)))]
            (is (= settings (get-in decoded [:settings :variable-blocks])))
            (is (= (:blocks through-water) (get-in decoded [:registry :effects :pool :blocks])))
            (is (nil? (runtime/observe (get-in decoded [:instances id]) [[:outputs :color] :near]))))))
      (finally (show/stop! rt)))))

(deftest global-variable-failure-is-isolated
  (let [plan (variable-blocks/compile-settings
              {:bindings {[:variables :healthy] {:base 0.7 :stages []}
                          [:variables :invalid] {:base {:op :divide :args [1 0]} :stages []}}})
        sample (variable-blocks/sample plan {:transport {:beat 1.0}} 25.0)]
    (is (= {:healthy 0.7} (:variables (variable-blocks/resolved {} sample))))
    (is (string? (:error (variable-blocks/inspect sample [:variables :invalid]))))))

(deftest scalar-packed-and-target-traces-read-the-same-fixture-chart
  (let [effect {:id :pool :version 1 :params {} :outputs {:focus 0.0}
                :prefabs {:rig/chart {:mode :replace :glsl "0.1 * world.z + 0.5 * uv.x + 0.1 * uv.y"}}
                :blocks {[:outputs :focus] {:stages [{:block :rig/chart}]}}}
        fixtures (mapv (fn [i chart]
                         (cond-> (assoc (first rig) :id (keyword (str "head-" i)) :position [0.5 0.5 1.0] :address (+ 1 (* i 5)))
                           chart (assoc :surface-uv chart)))
                       (range 3) [[0.1 0.2] [0.7 0.6] nil])
        [live id] (launch effect)
        model (-> live (assoc-in [:rig :fixtures] fixtures) files/write-string files/read-string :model)]
    (is (= fixtures (get-in model [:rig :fixtures])))
    (let [inputs (atom [])
          prepare (fn [& args] (swap! inputs conj args) :fixture-group)]
      (batch/compile-effect effect fixtures {:prepare-group prepare})
      (is (= [[0.1 0.2] [0.7 0.6] nil] (nth (first @inputs) 3)))
      (reset! inputs [])
      (batch/compile-effect effect (mapv #(dissoc % :surface-uv) fixtures) {:prepare-group prepare})
      (is (= 3 (count (first @inputs)))))
    (doseq [build [compiler/compile-effect (batch/compiler-for-rig fixtures)]]
      (let [frame (engine/advance model {:time-ms 125.0 :fixtures fixtures} build)
            values (:values (frame/resolve-contributions fixtures (:contributions frame)))
            instance (get-in frame [:model :instances id])]
        (is (empty? (:fault-events frame)))
        (doseq [[fixture uv expected] (map vector fixtures [[0.1 0.2] [0.7 0.6] [0.5 0.5]] [0.17 0.51 0.4])]
          (let [sample (runtime/observe instance [[:outputs :focus] (:id fixture)])]
            (is (= uv (:surface-uv sample)))
            (is (= [0.5 0.5 1.0] (:position sample)))
            (is (< (abs (- expected (:result sample))) 1e-10))
            (is (< (abs (- expected (get-in values [(:id fixture) :focus]))) 1e-10))))))))
