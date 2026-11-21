(ns tolglow.signals-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [tolglow.signals :as signals]
            [tolglow.effects :as fx]
            [tolglow.effects-test :as recipes]
            [tolglow.compiler :as compiler]
            [tolglow.compiler.batch :as batch]))

(defn near? [expected actual tolerance]
  (< (Math/abs (- (double expected) (double actual))) tolerance))

(deftest builtin-registry-and-pinned-history-survive-edn-reload
  (let [initial (signals/new-registry)
        source (pr-str initial)
        restored (signals/validate-registry (edn/read-string source))
        original (get-in initial [:waveforms :sine])
        changed (assoc original :version 2 :expression (fx/constant 0.25))
        revised (signals/publish restored changed)]
    (is (= initial restored))
    (is (= #{:sine :triangle :saw :pulse} (set (keys (:waveforms restored)))))
    (is (every? empty? (map signals/validate-waveform (vals (:waveforms restored)))))
    (is (identical? revised (signals/publish revised changed)))
    (is (= original (get-in revised [:versions [:sine 1]])))
    (is (= revised (signals/validate-registry (edn/read-string (pr-str revised)))))
    (is (near? 1.0 (get-in (signals/preview revised :sine {:version 1 :samples 5}) [:samples 1 :value]) 1.0e-12))
    (is (= 0.25 (get-in (signals/preview revised :sine {:samples 5}) [:samples 1 :value])))
    (is (thrown? clojure.lang.ExceptionInfo (signals/publish revised (assoc original :name "Changed old revision"))))))

(deftest sine-derived-channels-match-analytic-cycle-domain-values
  (let [result (signals/preview (signals/new-registry) :sine {:samples 5 :window 1.0e-4 :angle-span-deg 180})
        curvature (* 2 Math/PI Math/PI)]
    (doseq [[row value slope jagginess]
            (map vector (:samples result) [0.5 1 0.5 0 0.5]
                 [Math/PI 0 (- Math/PI) 0 Math/PI] [0 curvature 0 curvature 0])]
      (is (near? value (:value row) 1.0e-10))
      (is (near? slope (:slope row) 1.0e-5))
      (is (near? (* 180 slope) (:angular-velocity row) 1.0e-3))
      (is (near? jagginess (:jagginess row) 1.0e-4)))
    (is (= :amplitude-per-cycle (get-in result [:units :slope])))
    (is (= :degrees-per-cycle (get-in result [:units :angular-velocity])))
    (is (= :amplitude-per-cycle-squared (get-in result [:units :jagginess])))))

(deftest pulse-discontinuities-are-visible-window-dependent-values
  (let [state (signals/new-registry)
        result (signals/preview state :pulse {:samples 3 :window (/ 1.0 128.0)})
        narrower (signals/preview state :pulse {:samples 3 :window (/ 1.0 256.0)})]
    (is (= [1.0 0.0 1.0] (mapv :value (:samples result))))
    (is (= [64.0 -64.0 64.0] (mapv :slope (:samples result))))
    (is (= [16384.0 16384.0 16384.0] (mapv :jagginess (:samples result))))
    (is (= [128.0 -128.0 128.0] (mapv :slope (:samples narrower))))
    (is (= [0.0 0.0 0.0]
           (mapv :value (:samples (signals/preview state :pulse {:samples 3 :params {:duty 0.0}})))))
    (is (= [1.0 1.0 1.0]
           (mapv :value (:samples (signals/preview state :pulse {:samples 3 :params {:duty 1.0}})))))))

(deftest custom-waveforms-compose-by-lowering-existing-templates
  (let [initial (signals/new-registry)
        body (fx/multiply (signals/sample initial :sine {:phase (fx/param signals/phase-input)})
                          (signals/sample initial :triangle {:phase (fx/param signals/phase-input)}))
        definition {:id :rounded :version 1 :name "Rounded triangle" :category :composite
                    :range [0.0 1.0] :params {} :expression body}
        state (signals/publish initial definition)
        output (signals/preview state :rounded {:samples 5})]
    (is (empty? (signals/validate-waveform definition)))
    (is (= definition (get-in (signals/validate-registry (edn/read-string (pr-str state))) [:waveforms :rounded])))
    (doseq [[expected sample] (map vector [0.0 0.5 0.5 0.0 0.0] (:samples output))]
      (is (near? expected (:value sample) 1.0e-10)))))

(deftest selector-schema-and-boundaries-execute-through-the-real-typed-compiler
  (let [state (signals/new-registry) choices [:sine :triangle :saw :pulse]
        schema (signals/selector-schema state choices {:default-id :triangle})
        pinned (:choices schema)
        signal (signals/select-waveform state (fx/param :waveform) pinned {:phase (fx/cycles {:beats 4})})
        definition (fx/effect :selected {:params {:waveform schema}} (fx/intensity signal))
        scalar (compiler/compile-effect definition)
        typed (batch/compile-effect definition recipes/fixtures)]
    (is (= :waveform-selector (:semantic/type schema)))
    (is (= ["Sine" "Triangle" "Saw" "Pulse"] (mapv :label (:choices schema))))
    (is (= 1.0 (:default schema)))
    (is (= :slide (:control/axis schema)))
    (is (= :batch (:backend (meta typed))))
    (doseq [[index expected] [[0.0 (+ 0.5 (/ (Math/sqrt 2) 4))]
                              [0.49 (+ 0.5 (/ (Math/sqrt 2) 4))]
                              [0.5 0.25] [1.49 0.25] [1.5 0.125] [2.49 0.125] [2.5 1.0] [3.0 1.0]]]
      (let [input (assoc-in (recipes/context 0.5) [:instance :params :waveform] index)
            actual (recipes/frame typed input) expected-frame (recipes/frame scalar input)]
        (is (near? expected (get-in actual [:values :left :dimmer]) 1.0e-10))
        (is (= (recipes/frame-bytes expected-frame) (recipes/frame-bytes actual)))
        (is (= [:batch] (mapv :kind (:contributions (typed input)))))))
    (let [preview (signals/preview state {:selector 1.5 :choices pinned} {:samples 5})]
      (is (= [0.0 0.25 0.5 0.75 0.0] (mapv :value (:samples preview)))))))

(deftest derived-channels-compose-into-other-numeric-fixture-outputs
  (let [state (signals/new-registry)
        channels (signals/channels state :sine {:phase (fx/cycles {:bars 1}) :window 1.0e-4})
        definition (fx/effect :derived {}
                     (fx/color [0.5 0.02 0.15])
                     (fx/intensity (fx/add 0.5 (fx/multiply 0.1 (:slope channels))))
                     (fx/zoom (fx/multiply 0.02 (:jagginess channels)))
                     (fx/focus (fx/multiply 0.0001 {:op :abs :args [(:angular-velocity channels)]})))
        typed (batch/compile-effect definition recipes/fixtures)
        scalar (compiler/compile-effect definition)
        input (recipes/context 1.0)
        output (recipes/frame typed input)]
    (is (= :batch (:backend (meta typed))))
    (is (= [:batch] (mapv :kind (:contributions (typed input)))))
    (is (near? 0.5 (get-in output [:values :left :dimmer]) 1.0e-9))
    (is (near? (* 0.04 Math/PI Math/PI) (get-in output [:values :left :zoom]) 1.0e-5))
    (is (near? 0.0 (get-in output [:values :left :focus]) 1.0e-9))
    (is (= (recipes/frame-bytes (recipes/frame scalar input)) (recipes/frame-bytes output)))))

(deftest selector-metadata-preserves-per-choice-bindings-for-authoring-and-preview
  (let [state (signals/new-registry)
        schema (signals/selector-schema state [:sine {:id :pulse :params {:duty 0.25}}])
        restored (edn/read-string (pr-str schema))
        preview (signals/preview state {:selector 1 :choices (:choices restored)} {:samples 5})
        expression-schema (signals/selector-schema state [{:id :pulse :params {:duty (fx/param :width)}}])]
    (is (= schema restored))
    (is (= {:duty 0.25} (get-in restored [:choices 1 :params])))
    (is (= [1.0 0.0 0.0 0.0 1.0] (mapv :value (:samples preview))))
    (is (= {:duty (fx/param :width)} (get-in expression-schema [:choices 0 :params])))))

(deftest invalid-registry-templates-and-preview-bounds-reject-with-paths
  (let [state (signals/new-registry) sine (get-in state [:waveforms :sine])]
    (doseq [definition [(assoc sine :expression {:op :time})
                        (assoc sine :expression (fx/param :missing))
                        (assoc sine :params nil)
                        (assoc sine :params {signals/phase-input (fx/parameter 0.0)})
                        (assoc sine :expression (fx/constant [0 1 0]))
                        (assoc sine :callback (fn [] :not-data))]]
      (is (seq (signals/validate-waveform definition)))
      (is (thrown? clojure.lang.ExceptionInfo (signals/publish state definition))))
    (doseq [action [(fn [] (signals/preview state :sine {:samples 2049}))
                    (fn [] (signals/preview state :sine {:cycles 0}))
                    (fn [] (signals/preview state :sine {:window 0}))
                    (fn [] (signals/preview state :pulse {:params {:duty 2}}))
                    (fn [] (signals/select-waveform state 0 (vec (repeat 17 :sine)) {}))
                    (fn [] (signals/selector-schema state [:sine] {:default-id :pulse}))
                    (fn [] (signals/sample state :sine {:channel :unknown}))
                    (fn [] (signals/validate-registry (assoc-in state [:waveforms :sine :name] "Altered history")))]]
      (is (seq (try (action) nil (catch clojure.lang.ExceptionInfo error (:errors (ex-data error)))))))))
