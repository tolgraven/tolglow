(ns tolglow.effects-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [tolglow.effects :as fx]
            [tolglow.compiler :as compiler]
            [tolglow.compiler.batch :as batch]
            [tolglow.registry :as registry]
            [afterglow.runtime :as runtime]
            [afterglow.runtime.engine :as engine]
            [afterglow.control.model :as control]
            [afterglow.render.frame :as render]))

(def fixtures
  (mapv (fn [index id x]
          {:id id :position [x 3.0 0.0] :universe 1 :address (inc (* index 10))
           :channels {:red 1 :green 2 :blue 3 :dimmer 4 :pan [5 6] :tilt [7 8] :focus 9 :zoom 10}
           :calibration {:pan {:min-deg -270 :max-deg 270} :tilt {:min-deg -135 :max-deg 135}}})
        (range) [:left :center :right] [-1.0 0.0 1.0]))

(defn context [beat]
  {:frame {:time-ms (* 500.0 beat) :fixtures fixtures :transport {:beat (double beat) :bpm 120 :playing? true}}
   :instance {:id "i" :phase :running :started-at-ms 0.0 :started-at-beat 0.0 :params {}
              :timing-meter {:beats-per-bar 4 :bars-per-phrase 4}}
   :state {:counter 3} :assignments [] :contributions [] :intents [] :done? false})
(defn frame [step input]
  (render/resolve-contributions fixtures (:contributions (runtime/append-contributions (step input) []))))
(defn close? [expected actual] (< (Math/abs (- (double expected) (double actual))) 1.0e-10))
(defn frame-bytes [output] (into {} (map (fn [[universe values]] [universe (vec values)])) (:universes output)))

(deftest oscillator-quarter-cycles-use-real-scalar-and-typed-compilers
  (doseq [[shape expected] [[:sine [0.5 1.0 0.5 0.0 0.5]]
                            [:triangle [0.0 0.5 1.0 0.5 0.0]]
                            [:saw [0.0 0.25 0.5 0.75 0.0]]
                            [:pulse [1.0 1.0 0.0 0.0 1.0]]]]
    (let [definition (fx/effect shape {} (fx/intensity (fx/wave shape {:period {:bars 1}})))
          scalar (compiler/compile-effect definition) typed (batch/compile-effect definition fixtures)]
      (is (= :batch (:backend (meta typed))))
      (doseq [[beat expected] (map vector [0.0 1.0 2.0 3.0 4.0] expected)]
        (let [input (context beat) scalar-frame (frame scalar input) typed-frame (frame typed input)]
          (is (close? expected (get-in scalar-frame [:values :left :dimmer])) (str shape " at " beat))
          (is (= (frame-bytes scalar-frame) (frame-bytes typed-frame)))
          (is (= [:batch] (mapv :kind (:contributions (typed input)))))
          (is (= {:counter 3} (:state (typed input)))))))))

(deftest musical-units-captured-meter-and-paused-transport-control-phase
  (doseq [[period quarter-beat] [[{:beats 1/2} 0.125] [{:bars 2} 1.5] [{:phrases 1} 1.5]]]
    (let [definition (fx/effect :clock {} (fx/intensity (fx/saw {:period period})))
          step (batch/compile-effect definition fixtures)
          input (-> (context (+ 8.0 quarter-beat))
                    (assoc-in [:instance :started-at-beat] 8.0)
                    (assoc-in [:instance :timing-meter] {:beats-per-bar 3 :bars-per-phrase 2}))
          paused (-> input (assoc-in [:frame :time-ms] 90000.0)
                      (assoc-in [:frame :transport :bpm] 240)
                      (assoc-in [:frame :transport :playing?] false)
                      (assoc-in [:frame :transport :meter] {:beats-per-bar 7 :bars-per-phrase 8}))]
      (is (close? 0.25 (get-in (frame step input) [:values :left :dimmer])))
      (is (= (frame-bytes (frame step input)) (frame-bytes (frame step paused))))))
  (let [definition (fx/effect :absolute {} (fx/intensity (fx/saw {:period {:beats 4} :clock :absolute})))
        input (assoc-in (context 3.0) [:instance :started-at-beat] 2.0)]
    (is (close? 0.75 (get-in (frame (compiler/compile-effect definition) input) [:values :left :dimmer]))))
  (let [definition (fx/effect :physical {} (fx/intensity (fx/saw {:period {:ms 1000}})))
        step (batch/compile-effect definition fixtures)]
    (is (close? 0.25 (get-in (frame step (assoc-in (context 0.0) [:frame :time-ms] 250.0)) [:values :left :dimmer])))))

(deftest expression-waveform-helpers-select-the-cue-or-transport-phase
  (let [input (-> (context 3.0)
                  (assoc-in [:frame :transport-clock] {:beat 7.0 :meter {:beats-per-bar 3}})
                  (assoc-in [:instance :started-at-beat] 2.0))]
    (doseq [[reference expected] [[:launch 0.375] [:transport (+ 1/3 0.125)]]]
      (let [definition (fx/effect :ref {} (fx/intensity (fx/saw {:period {:bars 1} :phase-reference reference :phase 0.125})))
            scalar (frame (compiler/compile-effect definition) input)
            typed (frame (batch/compile-effect definition fixtures) input)]
        (is (close? expected (get-in scalar [:values :left :dimmer])))
        (is (= (frame-bytes scalar) (frame-bytes typed)))))
    (is (thrown? clojure.lang.ExceptionInfo (fx/saw {:period {:ms 100} :phase-reference :transport})))
    (is (thrown? clojure.lang.ExceptionInfo (fx/saw {:clock :absolute :phase-reference :launch})))))

(deftest spatial-wave-and-parameter-shorthand-render-a-colored-pixel-pattern
  (let [definition (fx/breathing-wash :spatial
                                     {:color [1 0 0] :level 0.8
                                      :wave {:shape :saw :axis :x :wavelength 4 :period {:beats 4}}})
        step (batch/compile-effect definition fixtures)
        output (frame step (context 1.0))]
    (is (= :batch (:backend (meta step))))
    (is (= [:batch] (mapv :kind (:contributions (step (context 1.0))))))
    (doseq [[id expected] [[:left 0.0] [:center 0.2] [:right 0.4]]]
      (is (close? expected (get-in output [:values id :dimmer])))
      (is (close? expected (get-in output [:values id :emitted-color 0]))))
    (is (= (frame-bytes output) (frame-bytes (frame (compiler/compile-effect definition) (context 1.0)))))
    (is (= {:bars 1} (:duration definition)))
    (is (= [1.0 0.0 0.0] (get-in definition [:params :tint :default])))))

(deftest output-composition-retains-layer-order-and-target-intersection
  (let [definition (fx/effect :layers {:params {:base [0.2 0 1]} :targets [:left :center]}
                              (fx/compose (fx/color (fx/rgb 1 0.5 0)) (fx/intensity (fx/param :base)))
                              (fx/intensity 0.3 {:blend :add :targets [:left :right]}))
        output (frame (batch/compile-effect definition fixtures) (context 0.0))]
    (is (= [:color :dimmer :dimmer] (mapv :attribute (:outputs definition))))
    (is (close? 0.5 (get-in output [:values :left :dimmer])))
    (is (close? 0.2 (get-in output [:values :center :dimmer])))
    (is (= [0.0 0.0 0.0] (get-in output [:values :right :emitted-color])))
    (is (every? map? (tree-seq #(seq (:args %)) :args (get-in definition [:outputs 0 :value]))))))

(deftest aim-zoom-and-focus-recipes-use-the-same-geometry-renderer
  (let [definition (fx/aim-sweep :sweep {:center [1 0 2] :width 4 :zoom 0.3 :wave {:period {:bars 1}}})
        step (batch/compile-effect definition fixtures)
        input (context 1.0)
        actual (frame step input)
        reference (frame (compiler/compile-effect definition) input)]
    (is (= :batch (:backend (meta step))))
    (is (empty? (:fallback-reasons (meta step))))
    (is (= (frame-bytes reference) (frame-bytes actual)))
    (is (= (:values reference) (:values actual)))
    (is (= [[5.0 0.0 2.0] [5.0 0.0 2.0] [5.0 0.0 2.0]]
           (mapv #(get-in actual [:values (:id %) :aim]) fixtures)))
    (is (= [:emission] (:requires definition)))
    (is (= [:movement :beam-shape] (:capabilities definition)))
    (is (close? 0.3 (get-in (frame step input) [:values :left :zoom])))
    (is (= [0.0 0.0 0.0] (get-in (frame step input) [:values :left :emitted-color]))))
  (let [definition (fx/effect :beam {} (fx/zoom (fx/triangle)) (fx/focus 0.7))
        output (frame (batch/compile-effect definition fixtures) (context 1.0))]
    (is (close? 0.5 (get-in output [:values :left :zoom])))
    (is (close? 0.7 (get-in output [:values :left :focus])))))

(deftest recipes-remain-portable-and-schema-errors-are-actionable
  (doseq [definition [(fx/wash :wash) (fx/breathing-wash :breath) (fx/aim-sweep :sweep)]]
    (is (registry/data? definition))
    (is (= definition (edn/read-string (pr-str definition))))
    (is (empty? (registry/validate-definition :effects definition))))
  (let [schemas (fx/params {:level [0.6 0 1] :enabled true
                            :palette (fx/parameter :red {:values [:red :purple]})
                            :count (fx/parameter 3 {:type :integer :bounds [1 8]})})]
    (is (= :number (get-in schemas [:level :type])))
    (is (= [0 1] (get-in schemas [:level :bounds])))
    (is (= :boolean (get-in schemas [:enabled :type])))
    (is (= :enum (get-in schemas [:palette :type])))
    (is (= 3 (get-in schemas [:count :default]))))
  (doseq [author [(fn [] (fx/sine {:period {:beats 0}}))
                  (fn [] (fx/triangle {:period {:bars -1}}))
                  (fn [] (fx/saw {:clock :wall}))
                  (fn [] (fx/position-wave :sine {:wavelength 0}))
                  (fn [] (fx/position-wave :sine {:axis :q}))
                  (fn [] (fx/pulse {:duration {:beats 4}}))
                  (fn [] (fx/params {:level [2 0 1]}))
                  (fn [] (fx/effect :unknown {} (fx/intensity (fx/param :missing))))]]
    (is (seq (try (author) nil (catch clojure.lang.ExceptionInfo error (:errors (ex-data error))))))))

(deftest authored-definition-hot-reloads-through-real-control-and-engine
  (let [original (fx/effect :editable {} (fx/color [1 0 0]) (fx/intensity 0.8))
        revised (fx/effect :editable {:version 2} (fx/color [0 0 1]) (fx/intensity 0.8))
        initial (control/new-show {:show-id "recipes" :registry {:effects {:editable original}
                                                                :cues {:red {:id :red :version 1 :effect-id :editable}}}
                                   :transport {:beat 0.0 :bpm 120 :playing? true}})
        launch (control/apply-command initial {:op :cue/launch :cue-id :red :command-id :launch :show-id "recipes"
                                               :epoch 0 :time-ms 0.0 :source :test
                                               :launch {:mode :persistent :reload-policy :preserve-state}})
        compile (batch/compiler-for-rig fixtures)
        first-frame (engine/advance (:model launch) {:time-ms 0.0 :fixtures fixtures :beat 0.0} compile)
        published (control/apply-command (:model first-frame)
                                         {:op :registry/publish :kind :effects :definition revised :command-id :revision
                                          :show-id "recipes" :epoch 0 :time-ms 500.0 :source :test})
        second-frame (engine/advance (:model published) {:time-ms 500.0 :fixtures fixtures :beat 1.0} compile)
        rendered (render/resolve-contributions fixtures (:contributions second-frame))]
    (is (= :accepted (get-in launch [:ack :status])))
    (is (= :accepted (get-in published [:ack :status])))
    (is (= (keys (get-in first-frame [:model :instances])) (keys (get-in second-frame [:model :instances]))))
    (is (some #(= :effect/reloaded (:type %)) (:intents second-frame)))
    (is (= [0.0 0.0 0.8] (get-in rendered [:values :left :emitted-color])))))

(deftest semantic-categories-and-control-annotations-remain-independent-from-cue-colors
  (let [definition (fx/breathing-wash :annotated {:swatch "#123456" :wave {:axis :x}})
        overrides (fx/params {:level (fx/parameter 0.5 {:control/axis nil})
                             :pan (fx/parameter 0.5 {:control/axis :lift})})]
    (is (= :pressure (get-in definition [:params :level :control/axis])))
    (is (= [:color :intensity] (:categories definition)))
    (is (= [:oscillator :sine :spatial] (:modifiers definition)))
    (is (= "#123456" (:swatch definition)))
    (is (nil? (get-in overrides [:level :control/axis])))
    (is (= :lift (get-in overrides [:pan :control/axis])))
    (is (= [:color :intensity :movement :beam-shape]
           (fx/output-categories (fx/compose (fx/color [1 0 0]) (fx/intensity 0.5)
                                              (fx/aim [0 0 0]) (fx/focus 0.5)))))))
