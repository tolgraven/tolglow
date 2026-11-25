(ns tolglow.studio.block-builder-test
  (:require [cljs.test :refer-macros [deftest is]]
            [tolglow.studio.block-builder :as builder]))

(deftest write-only-the-block-map
  (let [before "; Preserve\n{:id :fx\n :outputs {:dimmer 0.3}} ; end"
        b {[:outputs :dimmer] {:stages [{:block :lfo/sine}]}}
        after (builder/write-bindings before b)]
    (is (= (str "; Preserve\n{:id :fx\n :outputs {:dimmer 0.3}\n :blocks " (pr-str b) "} ; end") after))
    (is (= (str "; Preserve\n{:id :fx\n :outputs {:dimmer 0.3}\n :blocks {}} ; end")
           (builder/write-bindings after {})))
    (is (= before (builder/write-bindings before {})))))

(deftest moving-and-removing-stages-preserves-other-bindings
  (is (= [:b :a :c] (builder/move-stage [:a :b :c] 0 1)))
  (is (= [:a] (builder/move-stage [:a] 0 -1)))
  (let [p [:varyings :gel] b {p {:base 0.3 :stages [:a]}}]
    (is (= {p {:base 0.3 :stages []}} (builder/replace-stages b p [])))
    (is (= b (builder/replace-stages (assoc b [:outputs :color] {:stages [:x]}) [:outputs :color] [])))))

(deftest oscillator-timing-retains-period-offset-and-scope
  (is (= {:op :oscillator-phase :phase-reference :launch :period {:bars 1} :phase 0.0}
         (builder/oscillator-timing {:op :bar} false)))
  (is (= :transport (:phase-reference (builder/oscillator-timing {:op :beat} true))))
  (let [phase {:op :oscillator-phase :phase-reference :transport :period {:beats 1.5} :phase 0.25}
        binding {[:outputs :dimmer] {:stages [{:block :lfo/saw :inputs {:phase phase}}]}}]
    (is (= phase (builder/oscillator-timing phase false)))
    (is (= (str "{:id :fx :outputs {:dimmer 0.3}\n :blocks " (pr-str binding) "}")
           (builder/write-bindings "{:id :fx :outputs {:dimmer 0.3}}" binding)))))

(deftest picker-previews-the-materials-in-the-unpublished-draft
  (let [shared {:optics/gel {:source :builtin} :show/water {:source :show}}
        definition {:imported-prefabs {:optics/gel :touring-gel :show/water :previous-water}
                    :prefabs {:tour/glass :unpublished}}]
    (is (= {:optics/gel :touring-gel :show/water {:source :show} :tour/glass :unpublished}
           (builder/draft-catalog shared definition)))
    (is (= :new-water (:show/water (builder/draft-catalog shared {:prefabs {:show/water :new-water}}))))))
