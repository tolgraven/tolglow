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
