(ns tolglow.visualization.waveform-shapes-test
  (:require [afterglow.render.ir :as ir]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [tolglow.effects :as fx]
            [tolglow.effects.parameter-sources :as parameter-sources]
            [tolglow.signals :as signals]
            [tolglow.visualization.parameter-views-test :as registry-walk]
            [tolglow.visualization.waveform-shapes :as shapes]
            [tolglow.waveforms :as waveforms]))

(defn- expected-glyph
  "Evaluate a registered waveform's own lowered expression at each phase."
  [registry id samples params]
  (let [definition (get-in registry [:waveforms id])
        [low high] (:range definition)
        evaluate (ir/compile-expression (signals/sample registry id {:phase (fx/param :test/phase) :params params}))]
    (mapv (fn [index]
            (let [value (evaluate {:params {:test/phase (/ (double index) (dec samples))}})]
              (/ (- value low) (- high low))))
          (range samples))))

(deftest glyphs-sample-the-registered-waveform-the-engine-runs
  (let [registry (signals/new-registry)]
    (doseq [id [:sine :triangle :saw :pulse]
            :let [glyph (signals/glyph registry id)]]
      (is (= 49 (count glyph)) (str id))
      (is (every? #(<= 0.0 % 1.0) glyph) (str id))
      (is (every? true? (map #(< (Math/abs (- %1 %2)) 5.1e-5) glyph (expected-glyph registry id 49 {})))
          (str id " equals its compiled expression at every phase")))
    (testing "waveform parameters bind like an effect binds them"
      (let [narrow (signals/glyph registry :pulse {:params {:duty 0.25}})]
        (is (every? true? (map #(< (Math/abs (- %1 %2)) 5.1e-5) narrow (expected-glyph registry :pulse 49 {:duty 0.25}))))
        (is (not= narrow (signals/glyph registry :pulse)))))
    (is (= #{0.0 1.0} (set (signals/glyph registry :pulse))) "a pulse only holds its two levels")
    (is (apply < (subvec (signals/glyph registry :saw) 1 48)) "a saw rises through its cycle")
    (is (= 9 (count (signals/glyph registry :sine {:samples 9}))))))

(deftest the-waveform-catalog-carries-each-glyph-and-keeps-it-per-registry
  (let [catalog (parameter-sources/waveform-options {})
        registry (signals/new-registry)
        model {:waveforms registry}]
    (is (= [:pulse :saw :sine :triangle] (mapv :id catalog)))
    (doseq [{:keys [id glyph]} catalog]
      (is (= (signals/glyph (signals/new-registry) id) glyph) (str id)))
    (is (identical? (parameter-sources/waveform-options model) (parameter-sources/waveform-options model))
        "an unchanged registry does not resample its glyphs on every snapshot")
    (is (= catalog (parameter-sources/waveform-options model)))))

(def catalog (delay (parameter-sources/waveform-options {})))

(deftest a-choice-between-registered-waveforms-resolves-to-their-glyphs
  (let [enum {:type :enum :values [:sine :saw :square] :default :saw}
        resolved (shapes/choices enum @catalog)
        glyph (fn [id] (:glyph (some #(when (= id (:id %)) %) @catalog)))]
    (is (= [{:value :sine :label "Sine" :waveform :sine :glyph (glyph :sine)}
            {:value :saw :label "Saw" :waveform :saw :glyph (glyph :saw)}
            {:value :square :label "Pulse" :waveform :pulse :glyph (glyph :pulse)}]
           resolved)
        "values stay the schema's own; the legacy square name draws the registered pulse")
    (testing "a Push view carries the schema and the catalog through JSON"
      (let [device-catalog (json/read-str (json/write-str (mapv #(select-keys % [:id :name :glyph]) @catalog)) :key-fn keyword)
            device-schema {:type "enum" :choices ["sine" "saw" "sawtooth"]}]
        (is (= [["sine" :sine] ["saw" :saw] ["sawtooth" :saw]]
               (mapv (juxt :value :waveform) (shapes/choices device-schema device-catalog))))))
    (testing "a registered selector resolves by each choice's waveform ID"
      (let [schema (signals/selector-schema (signals/new-registry) [:sine :triangle :saw :pulse])
            resolved (shapes/choices schema @catalog)]
        (is (= [0.0 1.0 2.0 3.0] (mapv :value resolved)))
        (is (= ["Sine" "Triangle" "Saw" "Pulse"] (mapv :label resolved)))
        (is (= 2 (shapes/selected-index resolved 1.5)) "ties go to the later choice, like the selector")
        (is (= 1 (shapes/selected-index resolved 1.2)))))
    (testing "any other enum stays text"
      (is (nil? (shapes/choices {:type :enum :values [:sphere :rounded-box]} @catalog)) "a geometry shape")
      (is (nil? (shapes/choices {:type :enum :values [:sine :random]} @catalog)) "one unregistered choice")
      (is (nil? (shapes/choices enum [])) "no catalog")
      (is (nil? (shapes/choices {:type :number :bounds [0 1]} @catalog)))
      (is (nil? (shapes/choices {:type :enum :values (vec (repeat 17 :sine))} @catalog))))
    (is (= 0 (shapes/selected-index resolved "sine")) "a JSON value finds its keyword choice")
    (is (nil? (shapes/selected-index resolved :triangle)))))

(deftest glyph-points-fit-their-box
  (let [points (shapes/points [0.0 1.0 0.5 1.4] 40 16 2)
        pairs (mapv #(mapv parse-double (str/split % #",")) (str/split points #" "))]
    (is (= [[2.0 14.0] [14.0 2.0] [26.0 8.0] [38.0 2.0]] pairs) "values above the range are clamped")
    (is (nil? (shapes/points [0.5] 40 16 2)))
    (is (nil? (shapes/points [0.5 ##NaN] 40 16 2)))))

(deftest push-views-carry-only-the-glyphs-their-parameters-choose
  (let [attach #'waveforms/attach-waveform-glyphs
        shape {:id :shape :schema {:type :enum :values [:sine :square]}}
        level {:id :level :schema {:type :number :bounds [0 1]}}
        view (attach {:parameters [level nil shape]} @catalog)]
    (is (= [:pulse :sine] (mapv :id (:waveform-glyphs view))))
    (is (every? #(= #{:id :name :glyph} (set (keys %))) (:waveform-glyphs view)))
    (is (not (contains? (attach {:parameters [level]} @catalog) :waveform-glyphs)))
    (is (< (count (json/write-str (:waveform-glyphs (attach {:parameters [{:schema {:type :enum :values [:sine :triangle :saw :pulse]}}]}
                                                            @catalog))))
           2048)
        "the whole builtin catalog stays a small part of a 64 KiB Push packet")))

(deftest no-builtin-effect-chooses-a-waveform-through-a-plain-enum
  ;; The audit for #655: a plain shape enum would be a candidate for an
  ;; attached signal source. The shipped registry has none; a new one needs a
  ;; decision, so it fails here until it is migrated or listed as follow-up.
  (let [definitions (registry-walk/builtin-definitions)
        plain (for [definition definitions [param schema] (:params definition)
                    :when (and (shapes/choices schema @catalog)
                               (not= :waveform-selector (keyword (:semantic/type schema))))]
                [(:id definition) param])]
    (is (<= 19 (count definitions)))
    (is (empty? plain) (pr-str plain))))

(deftest the-offline-fixture-glyphs-are-the-registered-glyphs
  (let [fixture (json/read-str (slurp (io/file "dev/push-display/waveform-glyphs.json")) :key-fn keyword)]
    (is (= (mapv #(-> % (select-keys [:id :name :glyph]) (update :id name)) @catalog) fixture)
        "regenerate with: lein run -m tolglow.qa.parameter-widgets/write-fixture-glyphs")))
