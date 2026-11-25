(ns tolglow.authoring.cue-variations-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [tolglow.authoring.cue-variations :as variations]
            [tolglow.compiler :as compiler]
            [tolglow.demo :as demo]
            [tolglow.registry :as registry]))

(deftest seeded-full-states-use-the-authored-schema-and-real-beat-clock
  (let [model (demo/model)
        registry (:registry model)
        cue (get-in registry [:cues :breathe])
        effect (registry/cue-effect registry cue)
        schemas (:params effect)
        options {:keys [:period :level]}
        {:keys [seed variants] :as generated} (variations/full-variants schemas (:params cue) 47 options)
        states (mapv :params variants)
        {:keys [period level] :as state} (first states)
        step (compiler/compile-effect effect)
        context {:frame {:time-ms 9100 :fixtures [(first (demo/fixtures))]
                         :transport {:beat (+ 4 (/ period 4)) :bpm 137 :playing? true}}
                 :instance {:id "cue" :phase :running :params state :started-at-beat 4 :started-at-ms 0
                            :system {:opacity 0.2}}}
        sampled (get-in (step context) [:assignments 0 :value])]
    (is (= generated (variations/full-variants schemas (:params cue) 47 {:keys [:level :period]})))
    (is (not= variants (:variants (variations/full-variants schemas (:params cue) seed options))))
    (is (= 8 (count (distinct states))))
    (is (every? #(= :generated (:kind %)) variants))
    (is (every? #(and (= #{:level :period} (set (keys %)))
                      (<= 0.15 (:level %) 0.85)
                      (variations/valid-beat-ratio? (:period schemas) (:period %) :preferred)
                      (empty? (registry/validate-params schemas %))) states))
    (is (< (Math/abs (- level sampled)) 1.0e-12))
    (is (every? #(= 8 (get-in % [:params :period]))
                (:variants (variations/full-variants schemas {} 47 {}))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (variations/parameter-variants schemas {} :system/opacity 47 {})))))

(deftest exact-beat-ratio-preference-and-hard-limits
  (let [schema {:type :number :default 1 :bounds [1/4 4] :semantic/type :beat-ratio
                :ratio-components {:numerator [1 9] :denominator [1 11]}}
        valid? (partial variations/valid-beat-ratio? schema)
        schemas {:period schema}
        preferred (:variants (variations/parameter-variants schemas {:period 5/11} :period 17 {:count 64}))
        free (:variants (variations/parameter-variants schemas {} :period 17 {:count 64 :ratio-mode :free}))]
    (is (= [1 2 3 4 6 8] variations/preferred-components))
    (is (valid? 3/8 :preferred))
    (is (valid? 5/11 :free))
    (is (not (valid? 5/11 :preferred)))
    (is (not-any? #(valid? % :free) [0 -2 1.5 5 10/11 11/12]))
    (is (every? #(valid? (:value %) :preferred) preferred))
    (is (every? #(valid? (:value %) :free) free))
    (is (some #(= 5/11 (:value %)) free))
    (is (thrown? clojure.lang.ExceptionInfo
                 (variations/parameter-variants schemas {:period 0.5} :period 17 {:ratio-mode :free})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (variations/parameter-variants
                  {:period (assoc-in schema [:ratio-components :denominator] [0 11])} {} :period 17 {})))
    (let [decimal-bounds (assoc schema :bounds [0.25M 4M])
          choices (:variants (variations/parameter-variants {:period decimal-bounds} {} :period 17 {}))]
      (is (seq choices))
      (is (every? #(registry/valid-value? decimal-bounds (:value %)) choices)))))

(deftest parameter-rows-offer-useful-typed-choices
  (let [schemas {:tint {:type :color :default [0.7 0.2 0.1]}
                 :band {:type :range :default [0.2 0.8] :bounds [0 1] :random {:max-change 0.15}}
                 :position {:type :vector :size 3 :default [0 0 0] :bounds [-1 1]}
                 :count {:type :integer :default 2 :bounds [1 4]}
                 :mode {:type :enum :default :wash :values [:wash :beam nil false]}
                 :static {:type :number :default 1 :bounds [0 2] :random {:max-change 0}}
                 :locked {:type :boolean :default false :random {:enabled? false}}}
        choices (fn [param] (mapv :value (:variants (variations/parameter-variants schemas {} param 27 {}))))
        colors (choices :tint)
        luminance (fn [color] (reduce + (map * color [0.2126 0.7152 0.0722])))
        initial (luminance (get-in schemas [:tint :default]))]
    (is (some #(< (luminance %) (- initial 0.05)) colors))
    (is (some #(> (luminance %) (+ initial 0.05)) colors))
    (is (= #{1 3 4} (set (choices :count))))
    (is (= #{:beam nil false} (set (choices :mode))))
    (is (empty? (choices :static)))
    (is (empty? (choices :locked)))
    (is (= 8 (count (distinct (choices :position)))))
    (is (= 8 (count (distinct (choices :band)))))
    (is (every? (fn [[low high]] (and (<= 0.05 low 0.35) (<= 0.65 high 0.95) (<= low high)))
                (choices :band)))
    (is (every? (fn [param] (every? #(registry/valid-value? (get schemas param) %) (choices param)))
                (keys schemas)))))

(deftest limited-boolean-choices-yield-distinct-full-states
  (let [flag {:type :boolean :default false :random {:enabled? true}}
        options {:count 64}
        variants (:variants (variations/full-variants {:a flag :b flag} {} 47 options))]
    (is (= #{{:a true :b true} {:a true :b false} {:a false :b true}}
           (set (map :params variants))))))

(deftest saved-slots-append-evict-overwrite-and-revalidate-on-recall
  (let [schemas {:step {:type :integer :default 0 :bounds [0 100]}
                 :period {:type :number :default 1 :bounds [1/4 4] :semantic/type :beat-ratio}}
        params (fn [step] {:step step :period 5/11})
        append #(variations/append %1 schemas (params %2))
        bank (reduce append nil (range 8))
        edited (variations/overwrite bank schemas 2 (params 42))
        next-bank (append edited 8)
        restored-data (edn/read-string (pr-str next-bank))]
    (is (= (vec (range 8)) (mapv :step bank)))
    (is (= [1 42 3 4 5 6 7 8] (mapv :step restored-data)))
    (is (= {:kind :saved :slot 1 :params (params 42)} (variations/recall restored-data schemas 1)))
    (is (nil? (variations/recall [(params 0)] schemas 1)))
    (is (thrown? clojure.lang.ExceptionInfo (variations/overwrite [] schemas 0 (params 1))))
    (is (thrown? clojure.lang.ExceptionInfo (variations/recall bank schemas 8)))
    (is (thrown? clojure.lang.ExceptionInfo (variations/recall (conj bank (params 8)) schemas 0)))
    (doseq [invalid [(dissoc (params 0) :step) (assoc (params 0) :system/opacity 0.6)
                     (params 101) (assoc (params 0) :step 0.5) (assoc (params 0) :period 0.5)]]
      (is (thrown? clojure.lang.ExceptionInfo (variations/append bank schemas invalid))))
    (testing "schema changes reject only an incompatible recalled state, which can be overwritten"
      (let [narrower (assoc-in schemas [:step :bounds] [0 10])]
        (is (thrown? clojure.lang.ExceptionInfo (variations/recall restored-data narrower 1)))
        (is (= (params 1) (:params (variations/recall restored-data narrower 0))))
        (is (= (params 2) (:params (variations/recall (variations/overwrite restored-data narrower 1 (params 2)) narrower 1)))))
      (is (thrown? clojure.lang.ExceptionInfo (variations/recall restored-data (dissoc schemas :step) 0)))
      (is (thrown? clojure.lang.ExceptionInfo
                   (variations/recall restored-data (assoc schemas :new {:type :boolean :default false}) 0))))))
