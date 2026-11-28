(ns tolglow.compiler.batch-test
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.compiler :as compiler]
            [tolglow.compiler.batch :as batch]
            [tolglow.composition.response :as response]
            [tolglow.modulation :as modulation]
            [afterglow.control.model :as control]
            [afterglow.runtime :as runtime]
            [afterglow.runtime.cadence :as cadence]
            [afterglow.runtime.reuse :as reuse]
            [afterglow.runtime.engine :as engine]
            [afterglow.runtime.executor :as executor]
            [afterglow.runtime.keyframes :as keyframes]
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

(def ^:private literal-wash
  {:id :wash :version 1 :outputs {:color [0.23 0.18 0.37] :white 0.15}})

(deftest cpu-literal-wash-reuses-automatic-endpoints-with-identical-artdmx
  (let [rig (mapv #(assoc-in % [:channels :white] 9) fixtures)
        repatched (assoc-in rig [2 :address] 41)
        model (-> (control/new-show)
                  (command :publish {:op :registry/publish :kind :effects :definition literal-wash})
                  (command :cue {:op :cue/put :cue {:id :wash :effect-id :wash}})
                  (command :launch {:op :cue/launch :cue-id :wash :launch {:targets [:a :c]}}))
        id (first (keys (:instances model)))
        compile (into {} (map (fn [patch] [patch (batch/compiler-for-rig patch {:bind-pure reuse/evaluator})]))
                      [rig repatched])
        advance (fn [wrap previous t]
                  (let [{:keys [samples measure-effect]} (cadence/observer)
                        patch (if (< t 200) rig repatched)
                        model (cond-> (:model previous)
                                (= t 25) (command :opacity {:op :instance/opacity-set :instance-id id :value 0.5}))
                        model (case t
                                225 (command model :pause {:op :instance/pause :instance-id id})
                                250 (command model :resume {:op :instance/resume :instance-id id})
                                275 (command model :end {:op :instance/end :instance-id id})
                                model)
                        frame {:time-ms t :beat (/ t 500.0) :fixtures patch :keyframes (:keyframes previous)
                               :measure-effect measure-effect :keyframe-intervals {id {:ms 100}}}
                        result (engine/advance model frame (comp wrap (compile patch)))]
                    (assoc result :patch patch :calls (get-in @samples [id :calls] 0))))
        run #(vec (rest (reductions (partial advance %) {:model model} [0 25 175 200 225 250 275])))
        reference (run keyframes/predictable)
        actual (run identity)
        artdmx (fn [result]
                 (let [frame (render/resolve-contributions (:patch result) (:contributions result) {})]
                   (into {} (map (fn [[universe values]] [universe (vec (artnet/artdmx-packet universe 1 values))]))
                         (:universes frame))))
        white-values #(get-in % [:contributions 0 :batch :outputs :white :values])]
    (is (= [2 0 2 2 1 2 0] (mapv :calls reference)))
    (is (= [1 0 0 1 1 1 0] (mapv :calls actual)))
    (is (= [true true true true false true false] (mapv #(contains? (:keyframes %) id) actual)))
    (is (every? empty? (map :fault-events actual)))
    (is (= (map artdmx reference) (map artdmx actual)))
    (is (not= (artdmx (first actual)) (artdmx (second actual))))
    (is (not= (artdmx (actual 2)) (artdmx (actual 3))))
    (is (= (map :model reference) (map :model actual)))
    (is (= (map :intents reference) (map :intents actual)))
    (is (= [:a :c] (get-in actual [0 :contributions 0 :batch :target-ids])))
    (is (= [0.15 0.15] (vec (white-values (first actual)))))
    (is (identical? (white-values (first actual)) (white-values (actual 2))))
    (is (not (identical? (white-values (actual 2)) (white-values (actual 3)))))))

(deftest reloaded-nonliteral-wash-honors-instance-parameters-and-live-overlays
  (let [rig (mapv #(assoc-in % [:channels :white] 9) fixtures)
        model (-> (control/new-show)
                  (command :publish {:op :registry/publish :kind :effects :definition literal-wash})
                  (command :cue {:op :cue/put :cue {:id :wash :effect-id :wash}})
                  (command :launch {:op :cue/launch :cue-id :wash :launch {:reload-policy :preserve-state}}))
        id (first (keys (:instances model)))
        revised {:id :wash :version 2 :targets [:b]
                 :params {:level {:type :number :default 0.5 :bounds [0 1]}}
                 :outputs {:white {:op :param :key :level}}}
        compile (batch/compiler-for-rig rig)
        advance (fn [model prior t interval overrides]
                  (let [{:keys [samples measure-effect]} (cadence/observer)
                        frame {:time-ms t :fixtures rig :keyframes prior :measure-effect measure-effect
                               :keyframe-params overrides :keyframe-intervals (when interval {id {:ms interval}})}
                        build #(keyframes/with-inputs (compile %) (modulation/middleware overrides)
                                                     {:preserve-time-invariance? true})
                        result (engine/advance model frame build)]
                    (assoc result :calls (get-in @samples [id :calls] 0))))
        initial (advance model nil 0 100 {})
        bad-overlay (advance (:model initial) (:keyframes initial) 25 100 {id {:level 0.75}})
        rejected (control/apply-command (:model initial) {:op :param/set :instance-id id :param :level :value 0.4
                                                          :show-id (:show-id model) :epoch (:epoch model)
                                                          :command-id :unknown :source :batch-test :time-ms 0})
        revised-model (command (:model initial) :reload {:op :registry/publish :kind :effects :definition revised})
        reloaded (advance revised-model (:keyframes initial) 25 100 {})
        overridden-model (command (:model reloaded) :override {:op :param/set :instance-id id :param :level :value 0.25})
        overridden (advance overridden-model (:keyframes reloaded) 50 nil {})
        modulated (advance (:model overridden) (:keyframes overridden) 75 nil {id {:level 0.75}})
        white #(get-in % [:contributions 0 :batch :outputs :white :values])]
    (is (= :rejected (get-in rejected [:ack :status])))
    (is (= [:effect/failed] (mapv :type (:fault-events bad-overlay))))
    (is (empty? (:contributions bad-overlay)))
    (is (empty? (:keyframes bad-overlay)))
    (is (= [1 2 1 1] (mapv :calls [initial reloaded overridden modulated])))
    (is (= [0.15 0.15 0.15] (vec (white initial))))
    (is (= [0.5] (vec (white reloaded))))
    (is (= [0.25] (vec (white overridden))))
    (is (= [0.75] (vec (white modulated))))
    (is (= [:b] (get-in modulated [:contributions 0 :batch :target-ids])))
    (is (= {:level 0.25} (get-in modulated [:model :instances id :params])))
    (is (= 2 (get-in modulated [:model :instances id :effect-version])))
    (is (empty? (:keyframes modulated)))
    (is (every? empty? (map :fault-events [initial reloaded overridden modulated])))))

(deftest other-compilations-keep-ordinary-endpoint-sampling
  (doseq [definition [(assoc literal-wash :keyframes {:interval {:ms 50}})
                      (assoc literal-wash :keyframes {:max-interval {:ms 50}})
                      (assoc literal-wash :envelope {:attack {:ms 50}})
                      (assoc-in literal-wash [:outputs :white] {:op :multiply :args [0.25 {:op :beat}]})
                      (-> literal-wash
                          (assoc :params {:level {:type :number :default 0.5 :bounds [0 1]}})
                          (assoc-in [:outputs :white] {:op :param :key :level}))
                      (assoc-in literal-wash [:outputs :direction] [1.0 0.0 0.0])
                      (assoc-in literal-wash [:outputs :white] -0.0)]
          :let [step (batch/compile-effect definition fixtures)
                {:keys [samples measure-effect]} (cadence/observer)
                input (-> (context definition)
                          (assoc :effect-id "test")
                          (update :frame merge {:time-ms 0 :beat 0.0 :bpm 120.0 :measure-effect measure-effect
                                                :keyframe-intervals {"test" {:ms 100}}}))]]
    (keyframes/evaluate step input)
    (is (= 2 (get-in @samples ["test" :calls])) (pr-str definition)))
  (is (keyframes/predictable? (batch/compile-effect literal-wash fixtures {:enabled? false})))
  (is (keyframes/predictable? (batch/compile-effect literal-wash fixtures {:prepare-group (constantly {})}))))

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
    ;; Evaluated fields are read-only and shared: a hit returns the same array.
    (is (identical? original (values (assoc-in ctx [:frame :time-ms] 900))))
    (is (= [0.25 0.25 0.25] (vec original)))
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

(deftest envelope-scaling-never-writes-to-retained-fields
  ;; A stopped transport hits the pure binding every frame. Scaling the shared
  ;; array in place would compound the envelope frame after frame.
  (let [calls (atom 0)
        retained (atom [])
        bind-pure (fn [spec calculate]
                    (let [evaluate (reuse/evaluator spec (fn [inputs] (swap! calls inc) (calculate inputs)))]
                      (fn [env] (let [result (evaluate env)] (swap! retained conj result) result))))
        definition {:id :fade :version 1 :envelope {:attack {:beats 2} :release {:beats 1}}
                    :params {:level {:type :number :default 0.5 :bounds [0 1]}}
                    :outputs {:dimmer {:op :param :key :level} :focus {:op :param :key :level}}}
        step (batch/compile-effect definition fixtures {:bind-pure bind-pure})
        scalar (compiler/compile-effect definition)
        ctx (context definition)
        expected (frame-bytes (render/resolve-contributions fixtures (blocks (scalar ctx)) {:include-values? false}))
        frames (vec (repeatedly 4 #(step ctx)))
        published (fn [frame attribute] (get-in (first (:contributions frame)) [:batch :outputs attribute :values]))]
    (is (= 1 @calls) "every frame after the first hits the binding")
    (is (every? #(identical? (first @retained) %) @retained))
    (is (= [0.5 0.5 0.5] (vec (get-in (first @retained) [:outputs :dimmer :values]))) "the retained field is unscaled")
    (doseq [frame frames]
      (is (= expected (frame-bytes (render/resolve-contributions fixtures (blocks frame) {:include-values? false}))))
      (is (not= [0.5 0.5 0.5] (vec (published frame :dimmer))) "the envelope is mid-attack")
      (is (not (identical? (get-in (first @retained) [:outputs :dimmer :values]) (published frame :dimmer)))
          "a scaled attribute gets an array the frame owns")
      (is (identical? (get-in (first @retained) [:outputs :focus :values]) (published frame :focus))
          "an unscaled attribute keeps the shared array"))
    (is (apply = (map #(vec (published % :dimmer)) frames)))))

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

(deftest floating-position-arithmetic-keeps-integer-geometry-in-the-typed-kernel
  (let [x {:op :position :axis :x}
        rig (assoc-in fixtures [0 :position] [-5 0 0])
        effect (fn [expr] {:id :geometry :version 1 :targets [:a] :outputs {:dimmer expr}})
        floating {:op :add :args [0.5 {:op :multiply :args [x 0.05]}]}
        d (effect floating)
        input (assoc-in (context d) [:frame :fixtures] rig)
        fast (batch/compile-effect d rig)
        slow (compiler/compile-effect d)
        packet #(frame-bytes (render/resolve-contributions rig (blocks (% input))))]
    (is (= :batch (:backend (meta fast))))
    (is (= (packet slow) (packet fast)))
    ;; A second use of the very same axis can still calculate in integer space.
    (doseq [expr [{:op :add :args [floating {:op :subtract :args [x x]}]}
                  {:op :multiply :args [x x 0.05]}
                  {:op :sin :args [{:op :multiply :args [x x]}]}]]
      (is (= [:exact-position] (:fallback-reasons (meta (batch/compile-effect (effect expr) rig))))))
    ;; Values outside double's integer precision still use the scalar path.
    (is (= [:exact-position]
           (:fallback-reasons (meta (batch/compile-effect d (assoc-in rig [0 :position 0] 9007199254740993))))))))


(deftest targeted-tween-fallback-observes-the-current-rig-in-rig-order
  (let [definition {:id :tween-selection :version 1 :targets [:b :a :absent]
                    :outputs [{:attribute :dimmer :targets [:b :a]
                               :value {:op :tween :spec {:keyframes [{:at-ms 0 :value 0.25}
                                                                    {:at-ms 1000 :value 0.75}]}}}]}
        step (batch/compile-effect definition fixtures)
        input (context definition)
        result #(mapv (juxt :target-id :value) (:assignments (step (assoc-in input [:frame :fixtures] %))))]
    (is (= :ordinary (:backend (meta step))))
    (is (= [[:a 0.5] [:b 0.5]] (result fixtures)))
    (is (= [[:b 0.5] [:a 0.5]] (result (vec (reverse fixtures)))))
    (is (= [] (result [(last fixtures)])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"stable IDs" (result [{}])))))

(defn- rgbw-rig
  "Four-channel pixels in patch order. `uvs` holds an optional chart per pixel."
  ([positions] (rgbw-rig positions nil))
  ([positions uvs]
   (vec (map-indexed (fn [i position]
                       (cond-> {:id (str "px-" i) :position position
                                :universe (inc (quot (* 4 i) 512)) :address (inc (mod (* 4 i) 512))
                                :channels {:red 1 :green 2 :blue 3 :white 4}}
                         (nth uvs i nil) (assoc :surface-uv (nth uvs i))))
                     positions))))

(def ^:private matrix-positions
  (vec (for [row (range 6) col (range 10)] [(double col) 0.0 (double row)])))

(def ^:private strip-positions
  ;; One diagonal strip: every pixel has its own x and z.
  (vec (for [i (range 40)] [(* 0.25 i) 0.0 (* 0.5 i)])))

(defn- axis [a] {:op :position :axis a})

(defn- unit-sine [phase]
  {:op :multiply :args [0.5 {:op :add :args [1.0 {:op :sin :args [phase]}]}]})

(def ^:private x-wash
  {:op :vec3 :args [(unit-sine {:op :add :args [{:op :multiply :args [(axis :x) 0.05]} {:op :bar}]})
                    {:op :param :key :level} 0.25]})

(def ^:private x-saw
  {:op :fract :args [{:op :add :args [{:op :multiply :args [{:op :beat} 0.5]}
                                      {:op :multiply :args [(axis :x) 0.02]}]}]})

(def ^:private z-saw
  {:op :fract :args [{:op :add :args [{:op :bar} {:op :multiply :args [(axis :z) 0.07]}]}]})

(def ^:private xz-field
  {:op :multiply :args [0.5 {:op :add :args [(unit-sine {:op :multiply :args [(axis :x) 0.3]})
                                             (unit-sine {:op :add :args [{:op :beat} {:op :multiply :args [(axis :z) 0.2]}]})]}]})

(def ^:private u-saw
  {:op :fract :args [{:op :add :args [{:op :bar} {:op :multiply :args [{:op :surface-uv :axis :u} 1.7]}]}]})

(defn- pixel-effect [outputs]
  {:id :pixels :version 1 :params {:level {:type :number :default 0.4 :bounds [0 1]}} :outputs outputs})

(defn- field-bits
  "Every published field as raw double bits, with its batch's target IDs."
  [result]
  (vec (for [block (blocks result) :when (= :batch (:kind block))]
         [(get-in block [:batch :target-ids])
          (into (sorted-map) (map (fn [[attribute field]]
                                    [attribute (mapv #(Double/doubleToRawLongBits %) (:values field))]))
                (get-in block [:batch :outputs]))])))

(defn- artdmx [rig result]
  (let [frame (render/resolve-contributions rig (blocks result) {:include-values? false})]
    (into (sorted-map) (map (fn [[universe values]] [universe (vec (artnet/artdmx-packet universe 1 values))]))
          (:universes frame))))

(defn- frames-for [definition rig]
  (for [[t beat] [[500 1.0] [1375 2.75] [9125 18.25]]]
    (-> (context definition)
        (assoc-in [:frame :fixtures] rig)
        (assoc-in [:frame :time-ms] t)
        (assoc-in [:frame :transport :beat] beat))))

(defn- check-against-plain-evaluation [label rig definition expected-samples]
  (testing label
    (let [sampled (batch/compile-effect definition rig)
          plain (batch/compile-effect definition rig {:coordinate-classes? false})
          scalar (compiler/compile-effect definition)]
      (is (= :batch (:backend (meta sampled))))
      (is (= expected-samples (:coordinate-samples (meta sampled))))
      (is (every? nil? (:coordinate-samples (meta plain))))
      (doseq [ctx (frames-for definition rig)]
        (let [result (sampled ctx)]
          (is (= (field-bits (plain ctx)) (field-bits result)) "every field bit equals the plain evaluation")
          (is (= (artdmx rig (plain ctx)) (artdmx rig result)) "ArtDMX equals the plain evaluation")
          (is (= (artdmx rig (scalar ctx)) (artdmx rig result)) "ArtDMX equals the scalar compiler"))))))

(deftest coordinate-classes-reproduce-the-plain-evaluation-on-every-rig-shape
  (let [matrix (rgbw-rig matrix-positions)
        strip (rgbw-rig strip-positions)
        integer-strip (rgbw-rig (vec (for [i (range 40)] [i 0 (* 2 i)])))
        mixed (rgbw-rig (into matrix-positions strip-positions))
        charted (rgbw-rig matrix-positions
                          ;; Even rows are charted with u along the row number.
                          (vec (for [[_ _ z] matrix-positions]
                                 (when (even? (long z)) [(/ z 6.0) 0.5]))))]
    (check-against-plain-evaluation "a matrix under one-axis fields: one sample per column"
                                    matrix (pixel-effect [{:attribute :color :value x-wash}
                                                          {:attribute :white :value 0.1}
                                                          {:attribute :white :blend :max :value x-saw}])
                                    [10 10])
    (check-against-plain-evaluation "a single strip never engages"
                                    strip (pixel-effect {:color x-wash :white x-saw}) [nil])
    (check-against-plain-evaluation "integer-authored strip coordinates load as the same doubles"
                                    integer-strip (pixel-effect {:white x-saw}) [nil])
    (check-against-plain-evaluation "a two-axis field on a matrix never engages"
                                    matrix (pixel-effect {:white xz-field}) [nil])
    (check-against-plain-evaluation "a mixed rig samples per group"
                                    mixed (pixel-effect [{:attribute :color :value x-wash}
                                                         {:attribute :red :blend :max :value z-saw}
                                                         {:attribute :green :blend :add :value {:op :multiply :args [0.5 xz-field]}}
                                                         {:attribute :blue :blend :max :targets (mapv :id (take-nth 2 mixed)) :value x-saw}])
                                    [40 40 nil 20])
    (check-against-plain-evaluation "charted heads sample their UVs and uncharted heads their world x/y"
                                    charted (pixel-effect [{:attribute :white :value u-saw}
                                                           {:attribute :color :blend :max :value x-wash}])
                                    ;; Three charted u values and ten uncharted x; 0.0 is shared.
                                    [12 10])))

(deftest coordinate-classes-engage-only-at-half-the-targets-or-fewer
  (doseq [[xs expected] [[[0 0 1 1 2 2 3 3] [4]]
                         [[0 0 1 1 2 2 3 4] [nil]]
                         [[0 1 2 3 4 5 6 7] [nil]]
                         [[5 5 5 5 5 5 5 5] [1]]]
          :let [rig (rgbw-rig (mapv #(vector (double %) 0.0 0.0) xs))]]
    (check-against-plain-evaluation (str "x " xs) rig (pixel-effect {:white x-saw :color x-wash}) expected))
  (testing "a field that reads no coordinate keeps the evaluator's own uniform fill"
    (is (= [nil] (:coordinate-samples (meta (batch/compile-effect (pixel-effect {:white 0.5})
                                                                  (rgbw-rig matrix-positions)))))))
  (testing "deferred GPU groups evaluate every head"
    (is (= [nil] (:coordinate-samples (meta (batch/compile-effect (pixel-effect {:white x-saw}) (rgbw-rig matrix-positions)
                                                                  {:prepare-group (constantly {})}))))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"must be boolean"
                        (batch/compile-effect (pixel-effect {:white x-saw}) fixtures {:coordinate-classes? 1}))))

(deftest coordinate-classes-raise-the-plain-evaluation-fault
  (let [rig (rgbw-rig matrix-positions)
        definition (pixel-effect {:white {:op :divide :args [0.0 {:op :subtract :args [(axis :x) 3.0]}]}})
        fault (fn [options]
                (try ((batch/compile-effect definition rig options) (first (frames-for definition rig))) nil
                     (catch clojure.lang.ExceptionInfo error [(ex-message error) (ex-data error)])))]
    (is (= [10] (:coordinate-samples (meta (batch/compile-effect definition rig)))))
    (is (some? (fault {})))
    (is (= (fault {:coordinate-classes? false}) (fault {})))))

(deftest stopped-transport-reuses-the-sampled-field-without-evaluating
  (let [calls (atom 0)
        bind-pure (fn [spec calculate]
                    (reuse/evaluator spec (fn [inputs] (swap! calls inc) (calculate inputs))))
        rig (rgbw-rig matrix-positions)
        definition (pixel-effect {:color x-wash :white x-saw})
        step (batch/compile-effect definition rig {:bind-pure bind-pure})
        plain (batch/compile-effect definition rig {:coordinate-classes? false})
        stopped (vec (for [t [500 525 550 575]]
                       (-> (context definition) (assoc-in [:frame :fixtures] rig) (assoc-in [:frame :time-ms] t))))
        published #(get-in (first (blocks %)) [:batch :outputs :white :values])
        results (mapv step stopped)]
    (is (= [10] (:coordinate-samples (meta step))))
    (is (= 1 @calls) "a stopped transport hits the binding on every later frame")
    (is (every? #(identical? (published (first results)) (published %)) results))
    (is (= (field-bits (plain (first stopped))) (field-bits (last results))))
    (step (assoc-in (first stopped) [:frame :transport :beat] 1.25))
    (is (= 2 @calls) "a moving transport evaluates again")))
