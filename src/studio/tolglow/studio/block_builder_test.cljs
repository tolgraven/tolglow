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

;; Studio's own definitions for the gradient-sample and typed-parameter tests (#775).

(def schemas
  {:palette {:type :gradient :default [{:position 0.0 :color [1.0 0.0 0.0]} {:position 1.0 :color [0.0 0.0 1.0]}]}
   :level {:type :number :default 0.8 :bounds [0 1]}
   :steps {:type :integer :default 4 :bounds [1 8]}
   :tint {:type :color :default [1.0 0.5 0.0]}
   :mode {:type :enum :values [:a :b] :default :a}})

(defn- stage-change!
  "The `change!` stage-list gives a stage: a function and arguments applied to
  the current stage, here kept in `stage`."
  [stage]
  (fn [f & args] (swap! stage #(apply f % args))))

(deftest a-parameter-is-offered-to-inputs-that-can-read-it
  (testing "a scalar input reads only scalar parameters, integers included"
    (is (= [:level :steps] (builder/parameter-choices schemas :number)))
    (is (= "tint (color)" ((builder/parameter-title schemas (builder/parameter-fits :number)) :tint)))
    (is (= "palette (gradient)" ((builder/parameter-title schemas (builder/parameter-fits :number)) :palette))))
  (testing "a colour input reads colours and, broadcast, numbers, but never a gradient"
    (is (= [:level :steps :tint] (builder/parameter-choices schemas :vec3)))
    (is (= "level" ((builder/parameter-title schemas (builder/parameter-fits :vec3)) :level)) "a published broadcast is not an error")
    (is (= "gone (missing)" ((builder/parameter-title schemas (builder/parameter-fits :vec3)) :gone))))
  (is (= [:palette] (builder/gradient-choices schemas)))
  (testing "a scalar input whose definition's only parameter is a gradient offers no Parameter mode"
    (let [only-gradient (select-keys schemas [:palette])]
      (is (not-any? #{:parameter :gradient} (builder/input-modes {:type :number :value 0.5 :schemas only-gradient})))
      (is (= [:constant :beat :bar :phrase :time :gradient]
             (builder/input-modes {:type :vec3 :value [1 1 1] :schemas only-gradient})))))
  (testing "a colour input in a definition with only a number parameter offers it"
    (is (some #{:parameter} (builder/input-modes {:type :vec3 :value [1 1 1] :schemas (select-keys schemas [:level])}))))
  (testing "Parameter mode defaults to the first parameter the input can read"
    (is (= {:op :param :key :level} (builder/mode-value :parameter {:type :number :schemas schemas})))
    (is (= {:op :param :key :level} (builder/mode-value :parameter {:type :vec3 :schemas schemas}))))
  (testing "block bindings list only parameters an expression can read"
    (is (= [[:outputs :color] [:params :level] [:params :steps] [:params :tint]]
           (builder/binding-paths {:params schemas :outputs {:color [1 1 1]}})))))

(deftest a-private-varying-has-the-type-of-its-base
  (let [bindings {[:varyings :sweep] {:base 0.0 :stages [{:block :lfo/sine}]}
                  [:varyings :hue] {:base {:op :param :key :tint} :stages []}
                  [:varyings :glow] {:base [0.0 0.0 0.0] :stages []}
                  [:varyings :echo] {:base {:op :varying :key :hue} :stages []}
                  [:varyings :loop] {:base {:op :varying :key :loop} :stages []}
                  [:varyings :swell] {:base {:op :param :key :level} :stages []}}
        varyings (builder/varying-types bindings schemas)]
    (is (= {:sweep :number :hue :vec3 :glow :vec3 :echo :vec3 :loop :number :swell :number} varyings))
    (is (= :vec3 (builder/input-type {:op :param :key :tint} schemas {})))
    (is (= :vec3 (builder/input-type {:op :varying :key :echo} schemas varyings)))
    (testing "a gradient position reads only scalar varyings, never the edited one"
      (is (= [:loop :sweep :swell] (builder/readable-varyings varyings :number)))
      (is (= [:loop :swell] (builder/readable-varyings varyings :number :sweep)))
      (is (= "hue (vec3)" ((builder/varying-title varyings :number) :hue))))
    (testing "a colour input reads colour and scalar varyings"
      (is (= [:echo :glow :hue :loop :sweep :swell] (builder/readable-varyings varyings :vec3))))
    (testing "a colour varying's base is a colour input: it resets to a colour and lists colour parameters"
      (is (= [0.0 0.0 0.0] (builder/zero (:hue varyings))))
      (is (= :vec3 (builder/input-type (builder/zero (:hue varyings)))))
      (is (= "tint" ((builder/parameter-title schemas (builder/parameter-fits :vec3)) :tint))))))

(deftest a-colour-input-samples-a-gradient-at-a-scalar-position
  (let [spec {:type :vec3 :default [0.65 0.86 1.0] :schemas schemas}
        sample (builder/mode-value :gradient spec)]
    (is (= {:op :gradient :key :palette :args [0.0]} sample))
    (is (= :vec3 (builder/input-type sample)))
    (is (= {:op :gradient :key :palette :args [0.0] :extend :mirror} (builder/with-extend sample :mirror)))
    (is (= sample (builder/with-extend (builder/with-extend sample :repeat) :clamp)) "clamp is the default and is left out")
    (testing "the position is a scalar input offering the fixture position, an oscillator and scalar varyings"
      (let [varyings {:sweep :number :hue :vec3}]
        (is (= [:constant :beat :bar :phrase :time :position :oscillator-phase :parameter :varying]
               (builder/input-modes {:type :number :value 0.0 :schemas schemas :varyings varyings
                                     :position? true :phase? true})))
        (is (not-any? #{:varying} (builder/input-modes {:type :number :value 0.0 :varyings {:hue :vec3}})))
        (is (= {:op :position :axis :x} (builder/mode-value :position {:type :number})))
        (is (= {:op :varying :key :sweep} (builder/mode-value :varying {:type :number :varyings varyings})))
        (is (= :oscillator-phase (:op (builder/mode-value :oscillator-phase {:type :number}))))))))

(deftest edits-made-before-a-render-compose-through-the-stage-wiring
  ;; Every callback is made at render. Fields committed before the next render (a
  ;; blur and the tap that caused it, or two quick choices) must all reach the stage.
  (let [stage (atom {:block :optics/gel})
        edit! (builder/stage-input-edit (stage-change! stage) :transmission)
        transmission! (builder/current-edit edit! [0.65 0.86 1.0])
        sample (builder/gradient-callbacks transmission!)
        position! (builder/current-edit (:position sample) 0.0)
        timing (builder/timing-callbacks position! false)]
    (transmission! (constantly {:op :gradient :key :palette :args [0.0]}))
    ((:extend sample) :mirror)
    (position! (constantly {:op :beat}))
    ((:amount timing) 2)
    ((:phase timing) 0.25)
    ((:unit timing) :bars)
    (is (= {:block :optics/gel
            :inputs {:transmission {:op :gradient :key :palette :extend :mirror
                                    :args [{:op :oscillator-phase :period {:bars 2} :phase 0.25 :phase-reference :launch}]}}}
           @stage))
    (testing "a field of a mode the input has since left changes nothing"
      (transmission! (constantly [0.1 0.2 0.3]))
      ((:extend sample) :repeat)
      ((:amount timing) 4)
      (is (= [0.1 0.2 0.3] (get-in @stage [:inputs :transmission]))))
    (testing "two vector components committed together both land"
      (let [{:keys [transmission]} (:inputs @stage)]
        (transmission! (partial builder/edit-constant 0 0.9))
        (transmission! (partial builder/edit-constant 2 0.7))
        (is (= [0.9 0.2 0.7] (get-in @stage [:inputs :transmission])))
        (is (= [0.1 0.2 0.3] transmission))))
    (testing "an unset input edits its default"
      (let [stage (atom {:block :lfo/sine})
            phase! (builder/current-edit (builder/stage-input-edit (stage-change! stage) :phase) {:op :beat})
            timing (builder/timing-callbacks phase! true)]
        ((:amount timing) 3)
        ((:phase timing) 0.5)
        (is (= {:op :oscillator-phase :period {:beats 3} :phase 0.5 :phase-reference :transport}
               (get-in @stage [:inputs :phase])))))))

(deftest a-loaded-gradient-sample-is-edited-and-written-back-with-its-comments
  (let [source (str "; Tide: a gel following the palette along x\n"
                    "{:id :tide :version 1\n"
                    " ;; the palette runs red to blue\n"
                    " :params " (pr-str (select-keys schemas [:palette :level])) "\n"
                    " :outputs {:color [1.0 1.0 1.0]}\n"
                    " :blocks {[:outputs :color] {:stages [{:block :optics/gel :inputs {:transmission "
                    "{:op :gradient :key :palette :args [{:op :position :axis :x}] :extend :mirror}}}]}}}"
                    " ; end of tide\n")
        bindings (atom (:blocks (edn/read-string source)))
        ;; The editor's write! for stage 0 of the colour binding.
        change! (fn [f & args] (swap! bindings update-in [[:outputs :color] :stages 0] #(apply f % args)))
        transmission! (builder/current-edit (builder/stage-input-edit change! :transmission) [0.65 0.86 1.0])
        sample (builder/gradient-callbacks transmission!)]
    ((:extend sample) :repeat)
    ((:position sample) (partial builder/edit-op :position #(assoc % :axis :y)))
    (let [written (builder/write-bindings source @bindings)]
      (is (str/starts-with? written "; Tide: a gel following the palette along x\n{:id :tide :version 1\n ;; the palette runs red to blue\n"))
      (is (str/ends-with? written " ; end of tide\n"))
      (is (= {:op :gradient :key :palette :args [{:op :position :axis :y}] :extend :repeat}
             (get-in (edn/read-string written) [:blocks [:outputs :color] :stages 0 :inputs :transmission]))))))
