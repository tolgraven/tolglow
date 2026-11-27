(ns tolglow.block-bindings-test
  (:require [afterglow.control.model :as control]
            [afterglow.render.frame :as frame]
            [afterglow.runtime :as runtime]
            [afterglow.runtime.engine :as engine]
            [afterglow.runtime.events :as events]
            [clojure.test :refer [deftest is]]
            [tolglow.clock :as clock]
            [tolglow.compiler :as compiler]
            [tolglow.effects.parameter-sources :as sources]
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
        ;; Compilers skip heads without the written attribute (#688).
        heads (mapv #(assoc-in % [:channels :pan] 7) rig)
        context {:frame {:time-ms 100.0 :transport {:beat 2.25} :fixtures heads}
                 :instance {:started-at-ms 0 :started-at-beat 2} :assignments []}
        result (first (:assignments ((compiler/compile-effect effect) context)))]
    (is (= {:id :sine :version 1 :channel :slope} (:waveform prefab)))
    (is (< (abs (- (:value result) 0.475)) 1.0e-10))))

(deftest transport-blocks-and-parameter-lfos-share-phase-through-pause-and-seek
  (let [path [:outputs :dimmer]
        period {:bars 1}
        phase {:op :oscillator-phase :phase-reference :transport :period period :phase 0.125}
        wave (signals/waveform-block (signals/new-registry) :saw
                                    {:phase-reference :launch :period period :phase 0.125})
        effect {:id :pool :version 1 :outputs {:dimmer 0.0 :focus 0.0}
                :params {:level {:type :number :default 0.0 :bounds [0 1]}}
                :prefabs {:house/saw wave}
                :blocks {path {:stages [{:block :lfo/saw :inputs {:phase phase}}]}
                         [:outputs :focus] {:stages [{:block :house/saw}]}}}
        [model first-id] (launch effect)
        second-launch (command (assoc model :transport {:beat 1.5 :bpm 120 :playing? true :meter {:beats-per-bar 2}})
                               :later :cue/launch :cue-id :swim :time-ms 250 :launch {:targets [:far]})
        second-id (get-in second-launch [:ack :instance-id])
        binding {:type :lfo :waveform :saw :period period :phase 0.125 :phase-reference :transport :from 0.0 :to 1.0}
        fresh (reduce (fn [m id] (sources/bind m {:instance-id id :param :level :binding binding}))
                      (assoc-in (:model second-launch) [:transport :meter :beats-per-bar] 3) [first-id second-id])
        restored (:model (files/read-string (files/write-string (assoc fresh :rig {:fixtures rig}))))]
    (is (= :accepted (get-in second-launch [:ack :status])))
    (is (= (:blocks effect) (get-in restored [:registry :effects :pool :blocks])))
    (is (= (:prefabs effect) (get-in restored [:registry :effects :pool :prefabs])))
    (doseq [build [compiler/compile-effect (batch/compiler-for-rig rig)]]
      (let [tick (fn [m beat t] (engine/advance (assoc-in m [:transport :beat] beat) {:time-ms t :fixtures rig} build))
            pause (fn [m op token] (:model (command m token op :instance-id first-id :time-ms (:time-ms m))))
            before (tick restored 3.0 1000.0)
            waiting (tick (pause (:model before) :instance/pause :pause) 3.75 1375.0)
            resumed (tick (pause (:model waiting) :instance/resume :resume) 4.0 1500.0)
            sought (tick (clock/rebase (:model resumed) {:beat 1.25}) 1.25 1510.0)
            target-pairs [[first-id :near] [second-id :far]]
            observed (fn [result binding-path [id target]]
                       (:result (runtime/observe (get-in result [:model :instances id]) [binding-path target])))]
        (doseq [[result expected-launch] [[before [0.625 0.875]] [waiting [0.625 0.25]]
                                          [resumed [0.6875 0.375]] [sought [0.6875 0.375]]]]
          (let [m (:model result)
                parameter-values (:overrides (sources/sample m {:time-ms (:time-ms m) :transport (:transport m)} {}))
                packed (:values (frame/resolve-contributions rig (:contributions result)))
                dimmers (mapv #(observed result path %) target-pairs)]
            (is (empty? (:fault-events result)))
            (is (= expected-launch (mapv #(observed result [:outputs :focus] %) target-pairs)))
            (is (= dimmers (mapv #(get-in parameter-values [% :level]) [first-id second-id])))
            (is (= (first dimmers) (second dimmers)))
            (is (= dimmers (mapv #(get-in packed [% :dimmer]) [:near :far])))))))))

(deftest global-oscillator-blocks-use-transport-and-isolate-a-missing-clock
  (let [path [:variables :dimmer]
        phase {:op :oscillator-phase :phase-reference :transport :period {:bars 1} :phase 0.25}
        settings {:bindings {path {:base 0.0 :stages [{:block :lfo/saw :inputs {:phase phase}}]}
                             [:variables :healthy] {:base 0.5 :stages []}}}
        plan (variable-blocks/compile-settings settings)
        model {:transport {:beat 1.0 :meter {:beats-per-bar 2}}}
        result (variable-blocks/sample plan model 100.0)
        missing (variable-blocks/sample plan {} 110.0 result)]
    (is (= 0.75 (get-in result [:values path])))
    (is (= 0.75 (get-in missing [:values path])))
    (is (= 0.5 (get-in missing [:values [:variables :healthy]])))
    (is (:held? (variable-blocks/inspect missing path)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (variable-blocks/compile-settings (assoc-in settings [:bindings path :stages 0 :inputs :phase :phase-reference] :launch))))))

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

(deftest a-beat-tween-can-be-the-dry-signal
  (let [effect {:id :fade :version 1
                :outputs {:focus {:op :tween :spec {:keyframes [{:at {:beats 0} :value 0.0}
                                                                 {:at {:beats 1} :value 1.0}]}}}
                :blocks {[:outputs :focus] {:stages [{:block :optics/ribbed-glass
                                                      :coordinates [0.0 0.0]}]}}}
        frame {:time-ms 750.0 :transport {:beat 11.5} :fixtures rig}
        ctx {:frame frame :instance {:id :fade :started-at-beat 11 :started-at-ms 500} :assignments []}
        scalar ((compiler/compile-effect effect) ctx)
        typed ((batch/compile-effect effect rig) ctx)
        trace #(runtime/observe % [[:outputs :focus] :near])]
    (is (= 0.5 (:base (trace scalar)) (:base (trace typed))))
    (is (= (* 0.5 0.825) (:result (trace scalar)) (:result (trace typed)) (:value (first (:assignments scalar)))))))

(deftest invalid-read-holds-last-good-variable-until-recovery
  (let [path [:variables :dimmer] plan (variable-blocks/compile-settings {:bindings {path {:stages []}}})
        model {:epoch 1 :variables {:dimmer 0.75}}
        read (fn [m before] (variable-blocks/sample plan m 25.0 before))
        good (read model nil)
        bad-model (assoc-in model path :broken)
        failed (read bad-model good)
        held (read bad-model failed)
        recovered (read (assoc-in model path 0.2) held)]
    (is (= 0.75 (get-in (variable-blocks/resolved bad-model failed) path)
           (get-in (variable-blocks/resolved bad-model held) path)))
    (is (:held? (variable-blocks/inspect held path)))
    (is (= 0.2 (get-in (variable-blocks/resolved model recovered) path)))
    (is (nil? (:held? (variable-blocks/inspect (read (assoc bad-model :epoch 2) held) path))))))

(deftest a-scalar-source-parent-releases-its-failing-binding
  (let [path [:variables :weather :density]
        plan (variable-blocks/compile-settings {:bindings {path {:base 0.4 :stages []}
                                                           [:variables :healthy] {:base 0.8 :stages []}}})
        model {:epoch 1 :variables {:weather {:density 0.2}}}
        changed (assoc-in model [:variables :weather] 0.7)
        tick #(variable-blocks/sample plan %1 25.0 %2)
        good (tick model nil)
        failed (tick changed good)
        same (tick changed failed)
        recovered (tick model same)
        reported #(mapv :event (events/pending (variable-blocks/collect-events (events/begin model) %1 %2)))]
    (is (= {:weather 0.7 :healthy 0.8} (:variables (variable-blocks/resolved changed failed))))
    (is (= 0.7 (get-in (variable-blocks/resolved changed good) [:variables :weather])))
    (is (string? (:error (variable-blocks/inspect failed path))))
    (is (nil? (:held? (variable-blocks/inspect failed path))))
    (is (= {:type :variable-block/failed :held? false :path path}
           (select-keys (first (reported good failed)) [:type :held? :path])))
    (is (empty? (reported failed same)))
    (is (= [{:type :variable-block/recovered :path path}] (reported same recovered)))
    (is (= 0.4 (get-in (variable-blocks/resolved model recovered) path)))))

(deftest restored-mismatched-source-shapes-keep-rendering-the-show
  (let [model (-> (control/new-show {:registry {:effects {:lamp {:id :lamp :version 1 :outputs {:red 1.0}}}}})
                  (assoc :rig {:fixtures rig})
                  (assoc-in [:variables :weather] 0.7)
                  (assoc-in [:settings :variable-blocks :bindings]
                            {[:variables :weather :density] {:base 0.4 :stages []}
                             [:variables :healthy] {:base 0.8 :stages []}}))
        launch (command model :launch :effect/launch :effect-id :lamp :time-ms 0)
        rt (show/create-runtime {:model (:model launch)})
        inspect-frame (fn [t]
                        (show/tick! rt t)
                        (let [{:keys [status output variable-observation]} @(:state rt)]
                          (is (nil? (:error status)))
                          (is (= 255 (bit-and 255 (first (get (:universes output) 1)))))
                          (is (= {:weather 0.7 :healthy 0.8}
                                 (select-keys (show/variables rt) [:weather :healthy])))
                          (is (contains? (:errors variable-observation) [:variables :weather :density]))))]
    (try
      (is (= :accepted (get-in launch [:ack :status])))
      (inspect-frame 25.0)
      (let [restore (show/submit! rt :import {:source (show/export-show-source rt)})]
        (show/tick! rt 50.0)
        (is (= :accepted (:status (deref restore 1000 nil)))))
      (inspect-frame 75.0)
      (finally (show/stop! rt)))))

(deftest a-phase-accumulator-keeps-its-phase-when-its-period-changes
  (let [effect {:id :spin :version 1
                :params {:beats {:type :number :default 2.0 :bounds [0.5 8.0]}}
                :outputs {:dimmer {:op :fract :args [{:op :phase-accumulator :id :spin
                                                      :period {:op :param :key :beats}}]}}}
        frame (fn [beat] {:time-ms (* 500.0 beat) :transport {:beat beat :playing? true} :fixtures rig})
        instance (fn [beats] {:started-at-ms 0 :started-at-beat 1000.0 :params {:beats beats}})
        phase (fn [state] (get-in state [:tolglow.compiler.blocks/phases :spin :phase]))]
    (is (not (compiler/predictable-outputs? effect)) "it advances instance state, so it is never forecast")
    ;; The batch backend writes typed contributions; both keep the same state.
    (doseq [[build values?] [[compiler/compile-effect true] [(batch/compiler-for-rig rig) false]]]
      (let [step (build effect)
            run (fn [state beat beats]
                  (let [result (step {:frame (frame beat) :instance (instance beats) :state state :assignments []})]
                    [(:state result) (:value (first (:assignments result)))]))
            [s0 v0] (run nil 1000.0 2.0)
            [s1 v1] (run s0 1001.0 2.0)
            [s2 v2] (run s1 1001.0 4.0)
            [s3 v3] (run s2 1001.0 4.0)
            [s4 v4] (run s3 1002.0 4.0)]
        (is (= [0.0 0.5 0.5 0.5 0.75] (mapv phase [s0 s1 s2 s3 s4]))
            "it starts at the launch, keeps its phase across a new period, adds nothing on a held beat, then runs at the new rate")
        (is (= {:spin {:beat 2.0 :phase 0.75}} (:tolglow.compiler.blocks/phases s4)))
        (when values? (is (= [0.0 0.5 0.5 0.5 0.75] [v0 v1 v2 v3 v4])))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"one scalar for the whole frame"
                          (compiler/compile-plan (assoc-in effect [:outputs :dimmer :args 0 :period]
                                                           {:op :position :axis :x}))))))
