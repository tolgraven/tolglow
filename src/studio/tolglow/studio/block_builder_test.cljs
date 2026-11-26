(ns tolglow.studio.block-builder-test
  (:require [cljs.reader :as edn]
            [cljs.test :refer-macros [deftest is testing]]
            [clojure.string :as str]
            [tolglow.studio.block-builder :as builder]
            [tolglow.studio.pretty :as pretty]))

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
    (is (= (str "{:id :fx :outputs {:dimmer 0.3}\n :blocks " (pretty/edn binding {:column 9 :tail 1}) "}")
           (builder/write-bindings "{:id :fx :outputs {:dimmer 0.3}}" binding)))))

(deftest block-maps-splice-into-pretty-printed-source
  (let [definition {:id :fx :version 2 :name "Pretty splice" :swatch "#b981a3" :duration {:bars 2}
                    :envelope {:attack {:beats 0.125} :release {:beats 1}}
                    :params {:level {:type :number :default 0.75 :bounds [0 1]}}
                    :outputs {:color {:op :const :value [0.725 0.5 0.25]} :dimmer {:op :param :key :level}}
                    :blocks {[:outputs :dimmer] {:stages [{:block :lfo/sine :inputs {:phase 0.25}}]}
                             [:outputs :color] {:stages [{:block :lfo/triangle}]}}}
        source (pretty/edn definition)
        lines (str/split-lines source)
        replaced {[:outputs :dimmer] {:stages [{:block :lfo/saw :inputs {:phase 0.5}} {:block :scale :amount 0.5}]}
                  [:outputs :color] {:stages [{:block :lfo/sine}]}}
        written (builder/write-bindings source replaced)]
    (testing "the printed source spans lines and holds its block map in the middle"
      (is (< 8 (count lines)))
      (is (str/starts-with? (nth lines 3) " :blocks ")))
    (testing "replacing a multi-line block map gives the lines a fresh print would"
      (is (= (pretty/edn (assoc definition :blocks replaced)) written))
      (is (= (assoc definition :blocks replaced) (edn/read-string written)))
      (let [after (drop-while #(not (str/starts-with? % " :duration")) lines)
            written-lines (str/split-lines written)]
        (is (< 1 (- (count lines) 3 (count after))))
        (is (= (take 3 lines) (take 3 written-lines)))
        (is (= after (take-last (count after) written-lines)))))
    (testing "removing every binding and adding them back"
      (let [emptied (builder/write-bindings source {})]
        (is (= (assoc definition :blocks {}) (edn/read-string emptied)))
        (is (= (assoc definition :blocks replaced) (edn/read-string (builder/write-bindings emptied replaced))))))
    (testing "a definition without blocks gets them as its last entry, indented with the others"
      (let [bare (pretty/edn (dissoc definition :blocks))
            added (builder/write-bindings bare replaced)
            added-lines (str/split-lines added)]
        (is (= (assoc definition :blocks replaced) (edn/read-string added)))
        (is (= (butlast (str/split-lines bare)) (take (dec (count (str/split-lines bare))) added-lines)))
        (is (some #(str/starts-with? % " :blocks {[:outputs ") added-lines))
        (is (every? #(<= (count %) pretty/default-width) added-lines))))))

(deftest picker-previews-the-materials-in-the-unpublished-draft
  (let [shared {:optics/gel {:source :builtin} :show/water {:source :show}}
        definition {:imported-prefabs {:optics/gel :touring-gel :show/water :previous-water}
                    :prefabs {:tour/glass :unpublished}}]
    (is (= {:optics/gel :touring-gel :show/water {:source :show} :tour/glass :unpublished}
           (builder/draft-catalog shared definition)))
    (is (= :new-water (:show/water (builder/draft-catalog shared {:prefabs {:show/water :new-water}}))))))
