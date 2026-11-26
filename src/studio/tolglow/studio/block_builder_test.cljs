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
    (is (= (str "{:id :fx :outputs {:dimmer 0.3}\n :blocks\n   " (pretty/edn binding {:column 3 :tail 1}) "}")
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
      (is (= " :blocks" (nth lines 3))))
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
    (testing "a block map that grows or shrinks moves to the line a fresh print gives it"
      (let [small {[:outputs :dimmer] {:stages [{:block :lfo/sine}]}}
            with-small (pretty/edn (assoc definition :blocks small))]
        (is (some #{" :blocks {[:outputs :dimmer] {:stages [{:block :lfo/sine}]}}"} (str/split-lines with-small)))
        (is (= (pretty/edn (assoc definition :blocks replaced)) (builder/write-bindings with-small replaced)))
        (is (= with-small (builder/write-bindings source small)))))
    (testing "a block map on one line among other entries stays on one line"
      (let [compact (pr-str definition)
            written (builder/write-bindings compact replaced)]
        (is (= 1 (count (str/split-lines written))))
        (is (= (assoc definition :blocks replaced) (edn/read-string written)))))
    (testing "a definition without blocks gets them as its last entry, indented with the others"
      (let [bare (pretty/edn (dissoc definition :blocks))
            added (builder/write-bindings bare replaced)
            added-lines (str/split-lines added)]
        (is (= (assoc definition :blocks replaced) (edn/read-string added)))
        (is (= (butlast (str/split-lines bare)) (take (dec (count (str/split-lines bare))) added-lines)))
        (is (some #{" :blocks"} added-lines))
        (is (some #(str/starts-with? % "   {[:outputs ") added-lines))
        (is (every? #(<= (count %) pretty/default-width) added-lines))))))

(deftest picker-previews-the-materials-in-the-unpublished-draft
  (let [shared {:optics/gel {:source :builtin} :show/water {:source :show}}
        definition {:imported-prefabs {:optics/gel :touring-gel :show/water :previous-water}
                    :prefabs {:tour/glass :unpublished}}]
    (is (= {:optics/gel :touring-gel :show/water {:source :show} :tour/glass :unpublished}
           (builder/draft-catalog shared definition)))
    (is (= :new-water (:show/water (builder/draft-catalog shared {:prefabs {:show/water :new-water}}))))))

(deftest stage-edits-made-before-a-render-compose
  ;; A wet amount committed on blur and the tap on "Move later" that caused the blur both
  ;; run against the stages that were rendered; each must apply to the latest stages.
  (let [rendered [{:block :optics/through-water} {:block :optics/gel}]
        amount #(builder/update-stage % 1 assoc :amount 0.4)
        move #(builder/move-stage % 0 1)
        coordinates #(builder/update-stage % 0 assoc :coordinates :surface-uv)]
    (is (= [{:block :optics/gel :amount 0.4 :coordinates :surface-uv} {:block :optics/through-water}]
           (-> rendered amount move coordinates)))
    (is (= rendered (builder/update-stage rendered 2 assoc :amount 1)))
    (is (= [{:block :optics/gel}] (builder/remove-stage rendered 0)))
    (is (= rendered (builder/remove-stage rendered 5)))))

(deftest stage-edits-find-their-stage-by-id
  ;; Callbacks are made at render. Edits made before the next render must each reach
  ;; the stage they were made for, not whichever stage now sits at its rendered index.
  (let [rendered [{:block :optics/through-water} {:block :optics/gel} {:block :scale}]
        [water gel scale] (map-indexed builder/stage-id rendered)
        remove-water #(builder/edit-stage % water builder/remove-stage)
        amount #(builder/edit-stage % gel (fn [stages i] (builder/update-stage stages i assoc :amount 0.4)))
        later #(builder/edit-stage % water (fn [stages i] (builder/move-stage stages i (inc i))))
        coordinates #(builder/edit-stage % water (fn [stages i] (builder/update-stage stages i assoc :coordinates :surface-uv)))]
    (testing "two taps on one Remove inside a frame remove one stage"
      (is (= [{:block :optics/gel} {:block :scale}] (-> rendered remove-water remove-water))))
    (testing "a blur, the tap that caused it and a later edit compose"
      (is (= [{:block :optics/gel :amount 0.4} {:block :optics/through-water :coordinates :surface-uv} {:block :scale}]
             (-> rendered amount later coordinates))))
    (testing "an edit for a removed stage changes nothing"
      (let [removed (remove-water rendered)]
        (is (= removed (coordinates removed)))
        (is (= [{:block :optics/gel}] (builder/edit-stage removed scale builder/remove-stage)))))
    (testing "IDs survive edits and never reach the EDN or equality"
      (let [edited (-> rendered amount later)]
        (is (= [gel water scale] (map-indexed builder/stage-id edited)))
        (is (= "[{:block :optics/gel, :amount 0.4} {:block :optics/through-water} {:block :scale}]" (pr-str edited)))
        (is (not (str/includes? (pretty/edn {:stages edited}) "position")))))
    (testing "an added stage has an ID of its own"
      (let [a (builder/new-stage :scale) b (builder/new-stage :scale)]
        (is (= a b {:block :scale}))
        (is (not= (builder/stage-id 0 a) (builder/stage-id 0 b)))))))
