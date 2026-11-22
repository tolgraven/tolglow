(ns tolglow.compiler.batch-test
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.compiler :as compiler]
            [tolglow.compiler.batch :as batch]
            [tolglow.composition.response :as response]
            [afterglow.control.model :as control]
            [afterglow.runtime :as runtime]
            [afterglow.runtime.reuse :as reuse]
            [afterglow.runtime.engine :as engine]
            [afterglow.runtime.executor :as executor]
            [afterglow.render.frame :as render]
            [afterglow.output.artnet :as artnet]))

(def fixtures
  (mapv (fn [index id]
          {:id id :position [(* 0.2 (inc index)) 0 1] :universe 1 :address (inc (* index 10))
           :channels {:red 1 :green 2 :blue 3 :dimmer 4 :pan [5 6] :tilt 7 :focus 8}})
        (range) [:a :b :c]))

(defn context [definition]
  {:frame {:time-ms 500 :fixtures fixtures :transport {:beat 1.0 :bpm 120}}
   :instance {:id "test" :definition definition :effect-id (:id definition)
              :params {} :started-at-ms 0 :started-at-beat 0 :timing-meter {:beats-per-bar 4 :bars-per-phrase 4}
              :phase :running}
   :state {:owned true} :assignments [] :contributions [] :intents [] :done? false})

(defn blocks [result] (:contributions (runtime/append-contributions result [])))
(defn frame-bytes [frame] (into {} (map (fn [[id values]] [id (vec values)])) (:universes frame)))
(defn compiled-frame [definition input compile]
  (render/resolve-contributions fixtures (blocks ((compile definition) input))
                                {:include-values? false :compact-values? true}))

(deftest selectors-duplicate-attributes-and-blends-match-the-scalar-compiler
  (let [definition {:id :layers :version 1 :targets [:a :b]
                    :params {:level {:type :number :default 0.7 :bounds [0 1]}}
                    :outputs [{:attribute :color :value [0.1 0.2 0.3]}
                              {:attribute :red :value 0.2}
                              {:attribute :red :blend :add :targets [:b :c] :value 0.1}
                              {:attribute :red :blend :max :value 0.4}
                              {:attribute :red :blend :add :value 0.1}
                              {:attribute :focus :value {:op :position :axis :x}}
                              {:attribute :dimmer :value {:op :param :key :level}}]}
        input (context definition)
        scalar (compiled-frame definition input compiler/compile-effect)
        batched (compiled-frame definition input (batch/compiler-for-rig fixtures))]
    (is (= (frame-bytes scalar) (frame-bytes batched)))
    (is (= (render/project-compact (:compact scalar)) (render/project-compact (:compact batched))))
    (is (= :batch (:backend (meta (batch/compile-effect definition fixtures)))))
    (is (= {:owned true} (:state ((batch/compile-effect definition fixtures) input))))
    (is (= [] (:assignments input)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Rig changed"
                          ((batch/compile-effect definition fixtures) (assoc-in input [:frame :fixtures] (vec (reverse fixtures))))))))

(deftest responses-and-musical-envelope-preserve-canonical-parameters
  (let [definition {:id :musical :version 1 :envelope {:attack {:beats 2} :release {:beats 1}}
                    :params {:level {:type :number :default 0.8 :bounds [0 1]}}
                    :outputs {:color [1 0.25 0.1] :dimmer {:op :param :key :level}
                              :focus {:op :multiply :args [{:op :bar} 0.5]}}}
        middleware (response/compile-middleware [{:id :global :params [:level] :operation :multiply
                                                  :range [0.5 0.5] :envelope {} :state {:started-at-ms 0}}])
        scalar #(comp (compiler/compile-effect %) middleware)
        typed #(comp (batch/compile-effect % fixtures) middleware)
        input (assoc-in (context definition) [:instance :params] {:level 0.8})
        releasing (-> input (assoc-in [:instance :phase] :releasing)
                      (assoc-in [:instance :release-at-beat] 1.0) (assoc-in [:instance :release-at-ms] 500))]
    (doseq [ctx [input (assoc-in input [:frame :time-ms] 50000)
                 (assoc-in releasing [:frame :transport :beat] 1.5)
                 (assoc-in releasing [:frame :transport :beat] 2.0)]]
      (is (= (frame-bytes (compiled-frame definition ctx scalar)) (frame-bytes (compiled-frame definition ctx typed))))
      (is (= (:done? ((scalar definition) ctx)) (:done? ((typed definition) ctx)))))
    (is (= {:level 0.8} (get-in input [:instance :params])))))

(deftest world-space-fields-share-a-batch-with-color-and-focus
  (let [rig (mapv #(assoc % :rotation-deg [12 -41 9]
                            :calibration {:tilt {:min-deg -43 :max-deg 71}})
                  fixtures)
        target {:op :vec3 :args [{:op :position :axis :x} -2.5 5.25]}
        definition {:id :spot :version 1
                    :outputs [{:attribute :direction :value [-1.0 1.5 -2.0]}
                              {:attribute :color :value [0.25 0.5 0.75]}
                              {:attribute :focus :value 0.625}
                              {:attribute :aim :value target :targets [:b :c]}]}
        step (batch/compile-effect definition rig)
        ctx (assoc-in (context definition) [:frame :fixtures] rig)
        scalar (compiler/compile-effect definition)
        typed-blocks (blocks (step ctx))
        scalar-blocks (blocks (scalar ctx))]
    (is (= :batch (:backend (meta step))))
    (is (= [:batch :batch] (mapv :kind typed-blocks)))
    (is (= #{:direction :color :focus} (set (keys (get-in typed-blocks [0 :batch :outputs])))))
    (is (= [:b :c] (get-in typed-blocks [1 :batch :target-ids])))
    (doseq [opacity [1.0 0.5 0.0]
            :let [opts {:compact-values? true :previous-values {:b {:pan 0.96}}}
                  lower {:kind :assignments :assignments
                         (mapv #(hash-map :target-id (:id %) :attribute :aim :value [-4.0 1.0 -3.0]) rig)}
                  weight (fn [block]
                           (case (:kind block)
                             :batch (assoc-in block [:batch :opacity] opacity)
                             :assignments (update block :assignments
                                            #(mapv (fn [a] (assoc a :opacity opacity)) %))))
                  render-blocks #(render/resolve-contributions rig (into [lower] (map weight) %) opts)
                  expected (render-blocks scalar-blocks) actual (render-blocks typed-blocks)]]
      (is (= (frame-bytes expected) (frame-bytes actual)))
      (is (= (:values expected) (:values actual)))
      (is (= (render/project-compact (:compact expected) [0 1 2])
             (render/project-compact (:compact actual) [0 1 2]))))))

(deftest explicit-fallbacks-and-invalid-expressions
  (doseq [[definition reason]
          [[{:id :tween :version 1 :outputs {:dimmer {:op :tween :spec {:keyframes [{:at-ms 0 :value 0}
                                                                                       {:at-ms 1000 :value 1}]}}}} :tween]]]
    (let [compiled (batch/compile-effect definition fixtures)]
      (is (= :ordinary (:backend (meta compiled))))
      (is (= [reason] (:fallback-reasons (meta compiled))))
      (is (= ((compiler/compile-effect definition) (context definition)) (compiled (context definition))))))
  (is (= [:disabled] (:fallback-reasons (meta (batch/compile-effect {:id :x :version 1 :outputs {:dimmer 1}}
                                                                   fixtures {:enabled? false})))))
  (doseq [definition [{:id :x :version 1 :outputs {:dimmer {:op :fixture-query}}}
                      {:id :x :version 1 :outputs {:dimmer {:op :param :key :missing}}}]]
    (is (thrown? clojure.lang.ExceptionInfo (batch/compile-effect definition fixtures))))
  (let [definition {:id :invalid :version 1 :outputs {:dimmer 2}}]
    (is (thrown? clojure.lang.ExceptionInfo ((batch/compile-effect definition fixtures) (context definition))))))

(defn command [model id payload]
  (let [result (control/apply-command model (merge {:show-id (:show-id model) :epoch (:epoch model)
                                                   :command-id id :source :batch-test :time-ms (:time-ms model)} payload))]
    (is (= :accepted (get-in result [:ack :status])))
    (:model result)))

(defn launched-show []
  (let [color {:id :color :version 1 :priority -1 :params {:level {:type :number :default 0.4 :bounds [0 1]}}
               :duration {:beats 2} :envelope {:attack {:beats 1/2} :release {:beats 1}}
               :outputs {:color [1 0.25 0] :dimmer {:op :param :key :level}}}
        focus {:id :focus :version 1 :outputs {:focus {:op :tween :spec {:keyframes [{:at-ms 0 :value 0}
                                                                                  {:at-ms 2000 :value 1}]}}}}
        initial (control/new-show {:show-id "batch-show" :transport {:beat 0 :bpm 120 :playing? true}})]
    (reduce (fn [m [id payload]] (command m id payload)) initial
            [[:e1 {:op :registry/publish :kind :effects :definition color}]
             [:e2 {:op :registry/publish :kind :effects :definition focus}]
             [:c1 {:op :cue/put :cue {:id :color :effect-id :color}}]
             [:c2 {:op :cue/put :cue {:id :focus :effect-id :focus}}]
             [:l1 {:op :cue/launch :cue-id :color :launch {:mode :one-shot :reload-policy :preserve-state}}]
             [:l2 {:op :cue/launch :cue-id :focus :launch {:mode :persistent}}]])))

(deftest actual-engine-mixed-effects-parallel-replay-release-reload-and-artnet-capture
  (let [show (launched-show)
        compile (batch/compiler-for-rig fixtures)
        frame {:time-ms 500 :fixtures fixtures :beat 1 :epoch (:epoch show)}
        scalar (engine/advance show frame compiler/compile-effect)
        typed (engine/advance show frame compile)
        rendered #(render/resolve-contributions fixtures (:contributions %) {:include-values? false :compact-values? true})
        captured (mapv (fn [[universe values]] (vec (artnet/artdmx-packet universe 1 values)))
                       (:universes (rendered typed)))]
    (is (nil? (:assignments typed)))
    (is (= [:batch :assignments] (mapv :kind (:contributions typed))))
    (is (= (:model scalar) (:model typed)))
    (is (= (frame-bytes (rendered scalar)) (frame-bytes (rendered typed))))
    (is (= [65 114 116 45 78 101 116 0] (subvec (first captured) 0 8)))
    (with-open [^java.io.Closeable pool (executor/create {:workers 2 :queue-size 8 :timeout-ms 1000})]
      (let [parallel (engine/advance show frame compile (executor/mapper pool))]
        (is (= (:model typed) (:model parallel)))
        (is (= (frame-bytes (rendered typed)) (frame-bytes (rendered parallel))))))
    (let [next-definition (-> (get-in show [:registry :effects :color]) (assoc :version 2)
                              (assoc-in [:params :level :default] 0.9))
          updated (command (:model typed) :reload {:op :registry/publish :kind :effects :definition next-definition})
          tail (engine/advance updated (assoc frame :time-ms 1250 :beat 2.5) compile)
          scalar-tail (engine/advance updated (assoc frame :time-ms 1250 :beat 2.5) compiler/compile-effect)
          done (engine/advance (:model tail) (assoc frame :time-ms 1500 :beat 3) compile)]
      (is (= (frame-bytes (rendered tail)) (frame-bytes (rendered scalar-tail))))
      (is (some #(= :effect/reloaded (:type %)) (:intents tail)))
      (is (= 1 (count (get-in done [:model :instances]))))
      (is (vector? (:assignments done)) "Only the ordinary fallback remains"))))


(deftest exact-number-arithmetic-deliberately-selects-the-scalar-oracle
  (let [constant {:id :exact :version 1
                  :outputs {:dimmer {:op :subtract :args [9007199254740993 9007199254740992]}}}
        step (batch/compile-effect constant fixtures)]
    (is (= [:exact-literal] (:fallback-reasons (meta step))))
    (is (= (frame-bytes (compiled-frame constant (context constant) compiler/compile-effect))
           (frame-bytes (compiled-frame constant (context constant) (batch/compiler-for-rig fixtures))))))
  (let [definition {:id :fraction :version 1 :params {:level {:type :number :default 0.5 :bounds [0 1]}}
                    :outputs {:dimmer {:op :multiply :args [{:op :param :key :level} 1.0]}}}
        input (assoc-in (context definition) [:instance :params :level] 1/3)
        result ((batch/compile-effect definition fixtures) input)]
    (is (seq (:assignments result)))
    (is (empty? (:contributions result)))
    (is (= (frame-bytes (compiled-frame definition input compiler/compile-effect))
           (frame-bytes (compiled-frame definition input (batch/compiler-for-rig fixtures)))))))

(deftest declared-fields-reuse-only-their-actual-inputs
  (let [calls (atom 0)
        bind-pure (fn [spec calculate]
                    (reuse/evaluator spec (fn [inputs] (swap! calls inc) (calculate inputs))))
        definition {:id :hold :version 1
                    :params {:level {:type :number :default 0.25 :bounds [0 1]}}
                    :outputs {:dimmer {:op :param :key :level}}}
        step (batch/compile-effect definition fixtures {:bind-pure bind-pure})
        ctx (context definition)
        values #(get-in (first (:contributions (step %))) [:batch :outputs :dimmer :values])
        original (values ctx)]
    (aset-double ^doubles original 0 0.9)
    (is (= [0.25 0.25 0.25] (vec (values (assoc-in ctx [:frame :time-ms] 900)))))
    (is (= 1 @calls))
    (is (= [0.75 0.75 0.75] (vec (values (assoc-in ctx [:instance :params :level] 0.75)))))
    (is (= 2 @calls))
    (let [clock-def (assoc definition :outputs {:pan {:op :fract :args [{:op :phrase :clock :absolute}]}})
          step (batch/compile-effect clock-def fixtures {:bind-pure bind-pure})
          sample #(vec (get-in (first (:contributions (step %))) [:batch :outputs :pan :values]))]
      (is (= [0.0625 0.0625 0.0625] (sample ctx)))
      (is (= [0.0625 0.0625 0.0625] (sample (assoc-in ctx [:frame :time-ms] 901))))
      (is (= 3 @calls))
      (is (= [0.125 0.125 0.125] (sample (assoc-in ctx [:instance :timing-meter :beats-per-bar] 2))))
      (is (= 4 @calls))
      (is (= [0.25 0.25 0.25] (sample (assoc-in ctx [:frame :transport :beat] 4.0))))
      (is (= 5 @calls)))))

(deftest exact-coordinates-are-checked-on-the-targets-and-axes-each-output-reads
  (let [rig (mapv #(assoc %1 :position %2) fixtures [[0.25 0 0] [0 0.5 0] [0 0 0]])
        spatial (fn [axis] {:op :multiply :args [{:op :position :axis axis} 0.5]})
        selected {:id :selected :version 1 :targets [:a :b]
                  :outputs [{:attribute :focus :targets [:a :c] :value (spatial :x)}
                            {:attribute :dimmer :targets [:b :c] :value (spatial :y)}
                            {:attribute :focus :targets [:c] :value {:op :add :args [1/3 1/3]}}]}
        context-for #(assoc-in (context %) [:frame :fixtures] rig)
        step (batch/compile-effect selected rig)
        expected (blocks ((compiler/compile-effect selected) (context-for selected)))
        actual (blocks (step (context-for selected)))
        render #(frame-bytes (render/resolve-contributions rig %))]
    (is (= :batch (:backend (meta step))))
    (is (= [[:a] [:b]] (mapv #(get-in % [:batch :target-ids]) actual)))
    (is (= (render expected) (render actual)))
    (let [all (dissoc selected :targets)]
      (is (= :ordinary (:backend (meta (batch/compile-effect all rig))))))))

(deftest selected-exact-positions-preserve-integer-cancellation-and-direct-fractions
  (let [rig (assoc-in fixtures [0 :position] [9007199254740993 1/3 9007199254740992])]
    (doseq [[expr expected] [[{:op :subtract :args [{:op :position :axis :x} {:op :position :axis :z}]} 1]
                              [{:op :position :axis :y} 1/3]]]
      (let [definition {:id :exact-position :version 1 :targets [:a] :outputs {:dimmer expr}}
            input (assoc-in (context definition) [:frame :fixtures] rig)
            step (batch/compile-effect definition rig)
            result (step input)]
        (is (= [:exact-position] (:fallback-reasons (meta step))))
        (is (= expected (get-in result [:assignments 0 :value])))
        (is (= (frame-bytes (render/resolve-contributions rig (blocks ((compiler/compile-effect definition) input))))
               (frame-bytes (render/resolve-contributions rig (blocks result)))))))))
