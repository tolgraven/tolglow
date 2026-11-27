(ns tolglow.compiler.gradient-test
  "A typed :gradient parameter sampled into colour outputs through the
  declarative effect path (#689)."
  (:require [afterglow.control.model :as control]
            [afterglow.render.frame :as render]
            [afterglow.render.ir :as ir]
            [afterglow.runtime :as runtime]
            [afterglow.runtime.engine :as engine]
            [clojure.test :refer [deftest is testing]]
            [tolglow.compiler :as compiler]
            [tolglow.compiler.batch :as batch]
            [tolglow.compiler.gradient :as gradient]
            [tolglow.composition.response :as response]
            [tolglow.effects :as fx]
            [tolglow.registry :as registry]
            [tolglow.show :as show]
            [tolglow.compiler-test :refer [command]]))

(def rgb-stops
  [{:position 0.0 :color [1.0 0.0 0.0]}
   {:position 0.5 :color [0.0 1.0 0.0]}
   {:position 1.0 :color [0.0 0.0 1.0]}])

(def rig
  "Five pixels along x from -0.25 to 1.25, so two sit outside the stops."
  (mapv (fn [i x] {:id (keyword (str "p" i)) :position [x 0.5 0.0] :universe 1 :address (inc (* 3 i))
                   :channels {:red 1 :green 2 :blue 3}})
        (range) [-0.25 0.25 0.5 0.75 1.25]))

(defn- effect [value & [params]]
  {:id :ramp :version 1
   :params (merge {:palette {:type :gradient :default rgb-stops}
                   :shift {:type :number :default 0.0 :bounds [-1.0 1.0]}
                   :level {:type :number :default 1.0 :bounds [0.0 1.0]}}
                  params)
   :outputs [{:attribute :color :value value}]})

(defn- context [definition & [params]]
  {:frame {:time-ms 500 :fixtures rig :transport {:beat 1.0 :bpm 120}}
   :instance {:id "i" :definition definition :params (or params {}) :started-at-ms 0 :started-at-beat 0
              :timing-meter {:beats-per-bar 4 :bars-per-phrase 4} :phase :running}
   :state {} :assignments [] :contributions [] :intents [] :done? false})

(defn- colours
  "Captured colour assignments per pixel from the scalar compiler."
  [definition & [params]]
  (into {} (map (juxt :target-id :value))
        (:assignments ((compiler/compile-effect definition) (context definition params)))))

(defn- close? [expected actual]
  (and (= (count expected) (count actual))
       (every? true? (map #(< (Math/abs (- (double %1) (double %2))) 1.0e-12) expected actual))))

(defn- frame-bytes [definition params compile]
  (let [result ((compile definition) (context definition params))
        frame (render/resolve-contributions rig (:contributions (runtime/append-contributions result []))
                                            {:include-values? false :compact-values? true})]
    (into {} (map (fn [[id values]] [id (vec values)])) (:universes frame))))

(def along-x {:op :gradient :key :palette :args [{:op :position :axis :x}]})

(deftest a-spatial-sample-interpolates-stored-components-and-holds-the-ends
  (let [values (colours (effect along-x))]
    (is (close? [1.0 0.0 0.0] (:p0 values)) "before the first stop the first colour holds")
    (is (close? [0.5 0.5 0.0] (:p1 values)))
    (is (close? [0.0 1.0 0.0] (:p2 values)))
    (is (close? [0.0 0.5 0.5] (:p3 values)))
    (is (close? [0.0 0.0 1.0] (:p4 values)) "after the last stop the last colour holds")))

(deftest fixed-and-parameter-driven-positions
  (testing "a constant position paints every head one colour"
    (is (every? #(close? [0.0 0.5 0.5] %) (vals (colours (effect (fx/gradient :palette 0.75)))))))
  (testing "another parameter moves the sample"
    (let [definition (effect (fx/gradient :palette (fx/param :shift)))]
      (is (every? #(close? [1.0 0.0 0.0] %) (vals (colours definition))))
      (is (every? #(close? [0.5 0.5 0.0] %) (vals (colours definition {:shift 0.25}))))))
  (testing "position and parameter combine, and the colour nests in arithmetic"
    (let [definition (effect (fx/multiply (fx/gradient :palette (fx/add (fx/position :x) (fx/param :shift)))
                                          (fx/param :level)))
          values (colours definition {:shift 0.25 :level 0.5})]
      (is (close? [0.5 0.0 0.0] (:p0 values)))
      (is (close? [0.0 0.5 0.0] (:p1 values)))
      (is (close? [0.0 0.25 0.25] (:p2 values)))
      (is (close? [0.0 0.0 0.5] (:p3 values))))))

(deftest stops-inside-the-unit-range-and-extension-modes
  (let [inset [{:position 0.25 :color [0.2 0.4 0.6]} {:position 0.75 :color [0.6 0.4 0.2]}]
        params {:palette {:type :gradient :default inset}}
        sample (fn [extend x]
                 (:p0 (colours (effect (fx/gradient :palette x (if extend {:extend extend} {})) params))))]
    (is (close? [0.2 0.4 0.6] (sample nil 0.1)))
    (is (close? [0.4 0.4 0.4] (sample nil 0.5)))
    (is (close? [0.6 0.4 0.2] (sample nil 0.9)))
    (is (close? [0.6 0.4 0.2] (sample nil 3.0)))
    (testing ":repeat wraps the position into [0,1)"
      (is (close? (sample nil 0.5) (sample :repeat 1.5)))
      (is (close? (sample nil 0.5) (sample :repeat -0.5)))
      (is (close? (sample nil 0.0) (sample :repeat 1.0))))
    (testing ":mirror reflects back over [1,2]"
      (is (close? (sample nil 0.5) (sample :mirror 1.5)))
      (is (close? (sample nil 0.25) (sample :mirror 1.75)))
      (is (close? (sample nil 1.0) (sample :mirror 1.0))))))

(deftest a-single-stop-is-a-solid-colour
  (let [params {:palette {:type :gradient :default [{:position 0.3 :color [0.1 0.2 0.3]}]}}]
    (is (every? #(close? [0.1 0.2 0.3] %) (vals (colours (effect along-x params)))))))

(deftest batch-backend-compiles-the-sample-and-matches-the-scalar-bytes
  (doseq [value [along-x
                 (fx/gradient :palette (fx/add (fx/position :x) (fx/param :shift)) {:extend :mirror})
                 (fx/multiply (fx/gradient :palette (fx/param :shift) {:extend :repeat}) (fx/param :level))]
          params [{} {:shift 0.3 :level 0.6}
                  {:palette [{:position 0.0 :color [0.0 0.0 0.0]} {:position 0.2 :color [1.0 1.0 1.0]}
                             {:position 0.4 :color [0.0 0.5 1.0]} {:position 1.0 :color [1.0 0.0 0.5]}]}]]
    (let [definition (effect value)
          step (batch/compile-effect definition rig)]
      (is (= :batch (:backend (meta step))))
      (is (= [] (:fallback-reasons (meta step))))
      (is (= (frame-bytes definition params compiler/compile-effect)
             (frame-bytes definition params (batch/compiler-for-rig rig)))
          (pr-str value params)))))

(deftest the-lowered-sample-emits-glsl-with-stop-uniforms
  (let [expression (:expression (first (:outputs (compiler/compile-plan (effect along-x)))))
        shader (ir/emit-glsl-batch {:color expression})]
    (is (= :vec3 (ir/expression-type expression)))
    (is (= [{:attribute :color :type :vec3 :components 3 :varying "out_0" :offset 0}] (:outputs shader)))
    (is (contains? (:uniforms shader) (gradient/input-key :palette 0 :color)))
    (is (contains? (:uniforms shader) (gradient/input-key :palette 7 :scale)))
    (is (not (contains? (:uniforms shader) (gradient/input-key :palette 8 :scale)))
        "the default capacity is eight stops")))

(deftest live-values-change-without-recompiling
  (let [definition (effect along-x)
        step (compiler/compile-effect definition)
        sample #(into {} (map (juxt :target-id :value)) (:assignments (step (context definition {:palette %}))))]
    (testing "a value with a different stop count"
      (let [values (sample [{:position 0.0 :color [0.0 0.0 0.0]} {:position 1.0 :color [1.0 1.0 1.0]}])]
        (is (close? [0.25 0.25 0.25] (:p1 values)))
        (is (close? [0.75 0.75 0.75] (:p3 values)))))
    (testing "a value over the capacity drops the stops that change its ramp least"
      (let [definition (effect along-x {:palette {:type :gradient :default rgb-stops :max-stops 3}})
            step (compiler/compile-effect definition)
            ;; 0.25 and 0.75 lie on the straight lines between their neighbours.
            five [{:position 0.0 :color [1.0 0.0 0.0]} {:position 0.25 :color [0.5 0.5 0.0]}
                  {:position 0.5 :color [0.0 1.0 0.0]} {:position 0.75 :color [0.0 0.5 0.5]}
                  {:position 1.0 :color [0.0 0.0 1.0]}]
            values (into {} (map (juxt :target-id :value)) (:assignments (step (context definition {:palette five}))))]
        (is (= rgb-stops (gradient/simplify five 3)))
        (is (close? [0.5 0.5 0.0] (:p1 values)))
        (is (close? [0.0 0.5 0.5] (:p3 values)))))))

(deftest simplify-keeps-the-end-stops-and-the-sharpest-turns
  (let [stops [{:position 0.0 :color [0.0 0.0 0.0]} {:position 0.1 :color [0.1 0.1 0.1]}
               {:position 0.5 :color [1.0 0.0 0.0]} {:position 1.0 :color [0.0 0.0 1.0]}]]
    (is (= stops (gradient/simplify stops 4)))
    (is (= [0.0 0.5 1.0] (mapv :position (gradient/simplify stops 3))))
    (is (= [0.0 1.0] (mapv :position (gradient/simplify stops 2))))
    (is (= [0.0] (mapv :position (gradient/simplify stops 1))))))

(defn- authoring-error [definition]
  (first (compiler/definition-errors :effects definition)))

(deftest misuse-is-refused-at-authoring-time-with-a-named-alternative
  (testing "reading a gradient as a number names the gradient sample"
    (let [{:keys [path message]} (authoring-error (effect {:op :param :key :palette}))]
      (is (= [:outputs 0 :value] path))
      (is (re-find #"\{:op :gradient :key :palette :args \[position\]\}" message))))
  (testing "a gradient sample of a numeric parameter names :param"
    (is (re-find #"Read other parameters with \{:op :param\}"
                 (:message (authoring-error (effect {:op :gradient :key :shift :args [0.5]}))))))
  (doseq [[value path pattern]
          [[{:op :gradient :key :missing :args [0.5]} [:outputs 0 :value :key] #"unknown parameter"]
           [{:op :gradient :key :palette} [:outputs 0 :value :args] #"exactly one position"]
           [{:op :gradient :key :palette :args [0.1 0.2]} [:outputs 0 :value :args] #"exactly one position"]
           [{:op :gradient :key :palette :args [[0.1 0.2 0.3]]} [:outputs 0 :value :args 0] #"scalar number"]
           [{:op :gradient :key :palette :args [0.5] :extend :wrap} [:outputs 0 :value :extend] #":clamp, :repeat or :mirror"]
           [{:op :gradient :key :palette :args [0.5] :at 0.5} [:outputs 0 :value] #"takes :key, :args"]
           [{:op :gradient :key :palette :args [{:op :param :key :nope}]} [:outputs 0 :value :args 0 :key] #"unknown parameter"]]]
    (let [{actual-path :path message :message} (authoring-error (effect value))]
      (is (= path actual-path) (pr-str value))
      (is (re-find pattern message) (pr-str value))))
  (testing "a gradient colour cannot feed a scalar output"
    (is (re-find #"Output needs number values"
                 (:message (first (compiler/definition-errors
                                   :effects (assoc (effect along-x) :outputs [{:attribute :dimmer :value along-x}]))))))))

(deftest capacity-declarations-are-validated
  (is (re-find #"declare :max-stops up to 16"
               (:message (authoring-error
                          (effect along-x {:palette {:type :gradient :default (vec (for [i (range 10)] {:position (/ i 10.0) :color [0.0 0.0 0.0]}))}})))))
  (is (empty? (compiler/definition-errors
               :effects (effect along-x {:palette {:type :gradient :max-stops 10
                                                   :default (vec (for [i (range 10)] {:position (/ i 10.0) :color [0.0 0.0 0.0]}))}}))))
  (doseq [bad [0 17 2.5 "4"]]
    (is (= [[:params :palette :max-stops]]
           (mapv :path (registry/validate-parameter {:type :gradient :default rgb-stops :max-stops bad} [:params :palette])))))
  (is (= [[:params :level :max-stops]]
         (mapv :path (registry/validate-parameter {:type :number :default 0.5 :max-stops 4} [:params :level]))))
  (is (re-find #"reserved"
               (:message (authoring-error (effect along-x {:tolglow.gradient/x {:type :number :default 0.0}}))))))

(deftest reactive-outputs-refuse-a-gradient-sample
  (let [definition {:id :reactive-ramp :version 1 :targets [:p0]
                    :reactive {:inputs {:x [:source ::x]}}
                    :outputs [{:attribute :color :value {:op :gradient :key :palette :args [{:op :reactive :key :x}]}}]}]
    (is (= [{:path [:outputs 0 :value] :message "Reactive outputs read only declared signal values and constants"}]
           (compiler/definition-errors :effects definition)))))

(deftest a-block-oscillator-phase-drives-the-sample
  (let [definition (effect (fx/gradient :palette {:op :oscillator-phase :phase-reference :launch :period {:bars 1}}))
        step (compiler/compile-effect definition)
        at-beat #(:value (first (:assignments (step (-> (context definition)
                                                        (assoc-in [:frame :transport :beat] %))))))]
    (is (some? (:block-plan (compiler/compile-plan definition))))
    (is (close? [1.0 0.0 0.0] (at-beat 0.0)))
    (is (close? [0.0 1.0 0.0] (at-beat 2.0)))
    (is (close? [0.0 0.5 0.5] (at-beat 3.0)))
    (is (= :batch (:backend (meta (batch/compile-effect definition rig)))))))

(deftest responses-scale-the-position-identically-on-both-backends
  (let [definition (effect (fx/gradient :palette (fx/param :level)))
        middleware (response/compile-middleware [{:id :global :params [:level] :operation :multiply
                                                  :range [0.5 0.5] :envelope {} :state {:started-at-ms 0}}])
        scalar #(comp (compiler/compile-effect %) middleware)
        typed #(comp (batch/compile-effect % rig) middleware)
        params {:level 1.0}]
    (is (= (frame-bytes definition params scalar) (frame-bytes definition params typed)))
    (is (every? #(close? [0.0 1.0 0.0] %)
                (map :value (:assignments ((scalar definition) (context definition params))))))))

(deftest the-control-model-sets-and-revises-a-sampled-gradient
  (let [definition (effect along-x)
        published (command (control/new-show {:show-id "ramp"}) :publish :registry/publish
                           :kind :effects :definition definition)
        cued (command (:model published) :cue :cue/put :cue {:id :ramp :effect-id :ramp})
        launched (command (:model cued) :launch :cue/launch :cue-id :ramp :time-ms 0 :launch {:mode :persistent :reload-policy :preserve-state})
        id (get-in launched [:ack :instance-id])
        frame (fn [model compile]
                (let [result (engine/advance model {:time-ms 100 :fixtures rig} compile)]
                  [result (:values (render/resolve-contributions rig (:contributions result) {}))]))
        [first-result first-values] (frame (:model launched) compiler/compile-effect)
        white [{:position 0.0 :color [0.0 0.0 0.0]} {:position 1.0 :color [1.0 1.0 1.0]}]
        set (command (:model first-result) :set :param/set :instance-id id :param :palette :value white)
        [_ set-values] (frame (:model set) compiler/compile-effect)
        [_ typed-values] (frame (:model set) (batch/compiler-for-rig rig))
        revised (command (:model set) :revise :registry/publish :kind :effects
                         :definition (assoc definition :version 2
                                            :outputs [{:attribute :color
                                                       :value (fx/gradient :palette (fx/position :x) {:extend :mirror})}]))
        [revised-result revised-values] (frame (:model revised) compiler/compile-effect)]
    (doseq [result [published cued launched set revised]] (is (= :accepted (get-in result [:ack :status]))))
    (is (empty? (:fault-events first-result)))
    (is (close? [0.5 0.5 0.0] (get-in first-values [:p1 :color])))
    (is (close? [0.25 0.25 0.25] (get-in set-values [:p1 :color])))
    (is (= (get-in set-values [:p3 :color]) (get-in typed-values [:p3 :color])))
    (is (empty? (:fault-events revised-result)))
    (is (= 2 (get-in revised-result [:model :instances id :definition :version])))
    (is (= white (get-in revised-result [:model :instances id :params :palette])) "the edit survives the revision")
    (is (close? [0.75 0.75 0.75] (get-in revised-values [:p4 :color])) "mirror reflects 1.25 to 0.75")))

(deftest the-helper-infers-gradient-schemas-with-double-stops
  (let [schema (fx/parameter [{:position 0 :color [1 0 0]} {:position 1 :color [0 0 1]}])]
    (is (= {:type :gradient :default [{:position 0.0 :color [1.0 0.0 0.0]} {:position 1.0 :color [0.0 0.0 1.0]}]}
           schema))
    (is (= {:op :gradient :key :palette :args [{:op :const :value 0.5}] :extend :repeat}
           (fx/gradient :palette 0.5 {:extend :repeat})))
    (is (= :ramp (:id (fx/effect :ramp {:params {:palette schema}}
                                 (fx/color (fx/gradient :palette (fx/position :x)))))))))

(deftest a-headless-show-renders-and-retargets-a-sampled-gradient
  (let [model (-> (control/new-show {:registry {:effects {:ramp (effect along-x)}
                                                :cues {:ramp {:id :ramp :effect-id :ramp}}}})
                  (assoc :rig {:fixtures rig}))
        rt (show/create-runtime {:model model :workspace-options nil})
        tick (fn [t] (show/tick! rt t)
               (is (nil? (get-in @(:state rt) [:status :error])))
               (mapv #(bit-and 255 %) (get-in @(:state rt) [:output :universes 1])))
        submit (fn [t payload] (let [ack (show/submit! rt :command payload)] (tick t) @ack))]
    (try
      (let [launched (submit 0 {:op :cue/launch :cue-id :ramp :launch {:mode :persistent}})
            id (:instance-id launched)
            first-bytes (tick 25)
            _ (submit 50 {:op :param/set :instance-id id :param :palette
                          :value [{:position 0.0 :color [0.0 0.0 0.0]} {:position 1.0 :color [1.0 1.0 1.0]}]})
            grey-bytes (tick 75)]
        (is (= :accepted (:status launched)))
        (is (empty? (keep :fault (:instances (show/snapshot rt)))))
        ;; p0 holds red, p1 is halfway to green, p2 green, p3 halfway to blue, p4 holds blue.
        (is (= [255 0 0 128 128 0 0 255 0 0 128 128 0 0 255] (subvec first-bytes 0 15)))
        (is (= [0 0 0 64 64 64 128 128 128 191 191 191 255 255 255] (subvec grey-bytes 0 15))))
      (finally (show/stop! rt)))))
