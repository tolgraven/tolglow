(ns tolglow.studio.compound-widgets-test
  "The XY pad, Z fader, vertical fader, ring knob and Ember knob skin (#655)
  render from the shared description. Pointer and touch gestures are checked
  in Chromium by dev/studio/compound-widgets-qa.mjs."
  (:require [cljs.test :refer-macros [deftest is testing]]
            [clojure.string :as str]
            ["react-dom/server" :refer [renderToStaticMarkup]]
            [re-frame.db :as rf-db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [reagent.core :as r]
            [tolglow.studio.events :as events]
            [tolglow.studio.parameter-widgets :as widgets]
            [tolglow.studio.push-display :as display]
            [tolglow.studio.subs]
            [tolglow.visualization.control-geometry :as geometry]))

(defn- html [component] (renderToStaticMarkup (r/as-element component)))

(defn- count-of [pattern text] (count (re-seq pattern text)))

(defn- tag
  "The opening tag of the first element with this exact class, whatever the
  attribute order."
  [markup class]
  (some #(when (str/includes? % (str "class=\"" class "\"")) %) (re-seq #"<[a-z]+ [^>]*>" markup)))

(defn- styled? [markup class style]
  (some-> (tag markup class) (str/includes? (str "style=\"" style "\""))))

(def offset {:type :vector :size 2 :bounds [-1 1] :name "Offset"})

(def aim {:type :vector :size 3 :name "Aim" :component-labels ["Pan" "Tilt" "Depth"]
          :component-schemas [{:bounds [-180 180] :unit :degrees} {:bounds [-90 90] :unit :degrees} {:bounds [0 20] :unit :meters}]})

(def mix {:type :vector :size 4 :bounds [0 1] :name "Mix" :component-labels ["Low" "Mid" "High" "Air"]})

(defn- noop [& _])

(deftest an-xy-pad-draws-its-pointer-and-the-base-it-edits
  (let [markup (html [widgets/vector-compound {:title "Offset" :schema offset :values [0.5 -0.5] :base [0 0] :set! noop :set-component! noop :skin :standard}])]
    (is (str/includes? markup "data-widget=\"xy\""))
    (is (str/includes? markup "role=\"slider\""))
    (is (str/includes? markup "aria-roledescription=\"XY pad\""))
    (is (str/includes? markup "aria-label=\"Offset pad\""))
    (is (str/includes? markup "aria-valuetext=\"X 0, Y 0\"") "the pad announces the values it edits")
    (testing "the pointer marks the effective value; a ghost marks the base"
      (is (styled? markup "xy-pointer" "left:75%;bottom:25%"))
      (is (styled? markup "xy-base" "left:50%;bottom:50%")))
    (is (= 2 (count-of #"xy-zero" markup)) "both axes of a bipolar pad show zero")
    (is (str/includes? markup "Edit Offset components") "exact entries stay beside the pad")))

(deftest an-xyz-pad-puts-a-vertical-z-fader-beside-the-pad
  (let [markup (html [widgets/vector-compound {:title "Aim" :schema aim :values [90 45 5] :base [90 45 5] :set! noop :set-component! noop :skin :ember}])]
    (is (str/includes? markup "data-widget=\"xyz\""))
    (is (str/includes? markup "aria-valuetext=\"Pan 90, Tilt 45\""))
    (is (str/includes? markup "aria-orientation=\"vertical\""))
    (is (str/includes? markup "aria-label=\"Aim Depth\""))
    (is (str/includes? markup "aria-valuetext=\"5 meters\"") "the Z fader keeps its component unit")
    (is (styled? markup "fader-thumb" "bottom:25%"))
    (is (str/includes? markup "data-skin=\"ember\""))))

(deftest a-vertical-fader-fills-from-zero-and-ticks-short-integer-spans
  (let [steps (html [widgets/vertical-fader {:type :integer :bounds [0 8]} 3 noop {:name "Steps"}])
        pan (html [widgets/vertical-fader {:type :number :bounds [-1 1]} 0.5 noop {:name "Pan" :base-value 0.5
                                                                                 :effective-value -0.5}])]
    (is (= 9 (count-of #"class=\"fader-tick" steps)))
    (is (= 3 (count-of #"class=\"fader-tick major\"" steps)) "0, 4 and 8 are heavier")
    (is (styled? steps "fader-fill" "bottom:0%;height:37.5%"))
    (is (every? #(str/includes? (tag steps "parameter-fader") %)
                ["aria-valuemin=\"0\"" "aria-valuemax=\"8\"" "aria-valuenow=\"3\"" "aria-orientation=\"vertical\""]))
    (is (zero? (count-of #"fader-tick" pan)) "a continuous fader has no ticks")
    (is (styled? pan "fader-fill" "bottom:25%;height:25%") "a bipolar fader fills from zero")
    (is (str/includes? pan "fader-base") "a driven fader marks its base")
    (is (nil? (widgets/vertical-fader {:type :number} 1 noop {})) "an unbounded number draws no fader")))

(deftest a-ring-knob-nests-each-sub-parameter-and-opens-on-press
  (let [markup (html [widgets/vector-compound {:title "Mix" :schema mix :values [0.2 0.4 0.6 0.8] :base [0.2 0.4 0.6 0.8] :set! noop :set-component! noop :skin :standard}])]
    (is (str/includes? markup "data-widget=\"rings\""))
    (is (= 4 (count-of #"class=\"ring-track\"" markup)))
    (is (= 4 (count-of #"class=\"ring-value\"" markup)))
    (is (str/includes? markup "aria-expanded=\"false\""))
    (is (not (str/includes? markup "rings-components")) "sub-knobs open only when pressed")
    (is (str/includes? markup "aria-label=\"Mix: Low 0.2, Mid 0.4, High 0.6, Air 0.8\""))))

(deftest the-push-display-draws-rings-and-opens-sub-knobs-on-touch
  (let [mix-parameter {:id "mix" :param "mix" :name "Mix" :value [0.2 0.4 0.6 0.8]
                       ;; A device snapshot carries its schema through JSON.
                       :schema {:type "vector" :size 4 :bounds [0 1] :ui/widget "rings"}}
        parameters (mapv #(assoc mix-parameter :component %) (range 4))
        view {:instance-id "a" :parameters parameters}
        resting (html [display/display (display/normalize-view view)])
        touched (html [display/display (display/normalize-view
                                        (assoc view :touched-parameter {:instance-id "a" :param "mix" :component 1}))])]
    (is (pos? (count-of #"typed-rings" resting)) "untouched, each encoder shows the rings")
    (is (zero? (count-of #"ring-components" resting)))
    (is (= 1 (count-of #"class=\"ring-components\"" touched)))
    (is (= 4 (count-of #"class=\"ring-component ring-" touched)) "touching opens one knob per sub-parameter")))

(deftest the-ember-skin-draws-the-tolglow-web-knob-face
  (let [schema {:type :number :bounds [0 1]}
        ember (html [widgets/rotary schema 0.25 {:skin :ember}])
        standard (html [widgets/rotary schema 0.25 {}])]
    (is (str/includes? ember "data-skin=\"ember\""))
    (is (str/includes? ember "class=\"ember-fill\""))
    (is (str/includes? ember "class=\"ember-pointer\""))
    (is (str/includes? ember "rotate(-67.5)") "the bar pointer turns with the value")
    (is (not (str/includes? standard "ember")))
    (is (str/includes? standard "class=\"dial-value\""))))

(deftest the-wheel-turns-an-ember-knob-by-distance
  (let [steps {:type :integer :bounds [0 8]}]
    (is (= {:value 3 :remainder 20} (widgets/wheel-value steps 3 20 0)) "a small scroll carries its distance")
    (is (= {:value 4 :remainder 10} (widgets/wheel-value steps 3 50 20)) "and steps once it adds up")
    (is (= {:value 2 :remainder -40} (widgets/wheel-value steps 3 -100 0)))
    (is (= 8 (:value (widgets/wheel-value steps 8 600 0))) "the wheel stops at the bound"))
  (is (= 0.6 (:value (widgets/wheel-value {:type :number :bounds [0 1]} 0.5 100 0))))
  (is (nil? (widgets/wheel-value {:type :number} 0.5 100 0)) "an unbounded number does not turn")
  (is (= (geometry/knob-skin :ember) :ember)))

(defn- transition [db event]
  (reset! rf-db/app-db db)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %)) (registrar/get-handler :event (first event))))))

(defn- with-instance [params]
  (assoc-in (events/initial-db "compound-test") [:show :instances] [{:id "i1" :params params}]))

(deftest compound-gestures-set-components-against-the-current-value
  (testing "a pad gesture sets X and Y and keeps a Z moved meanwhile"
    (let [db (with-instance {:aim [0 0 15]})
          result (transition db [:studio/param-components "i1" :aim {0 -90 1 45} [0 0 5]])]
      (is (= [-90 45 15] (get-in result [:db :show :instances 0 :params :aim])))
      (is (= "param/set" (get-in result [:db :drafts ["i1" :aim] :op])))))
  (testing "repeated key presses accumulate on the current component"
    (let [schema {:type :number :bounds [-1 1]}
          once (transition (with-instance {:offset [0 0]}) [:studio/param-component-key "i1" :offset schema 0 "ArrowUp" [0 0]])
          twice (transition (:db once) [:studio/param-component-key "i1" :offset schema 0 "ArrowUp" [0 0]])]
      (is (= [0.02 0] (get-in once [:db :show :instances 0 :params :offset])))
      (is (= [0.04 0] (get-in twice [:db :show :instances 0 :params :offset])))))
  (testing "a gesture that changes no component sends nothing"
    (is (nil? (transition (with-instance {:offset [0.5 0]}) [:studio/param-components "i1" :offset {0 0.5} [0 0]]))))
  (testing "the composition slot editor steps a component against app-db, so presses accumulate"
    (let [schema {:type :number :bounds [-1 1]}
          db (-> (with-instance {})
                 (assoc-in [:show :effects] [{:id "fx" :params {:offset {:type :vector :size 2 :bounds [-1 1] :default [0 0]}}}])
                 (assoc-in [:show :cues] [{:id "cue" :effect-id "fx"}])
                 (assoc-in [:ui :composition :slots] [{:cue-id "cue"}]))
          once (transition db [:studio/slot-param-component-key 0 :offset schema 1 "ArrowUp"])
          twice (transition (:db once) [:studio/slot-param-component-key 0 :offset schema 1 "ArrowUp"])]
      (is (= [0 0.04] (get-in twice [:db :ui :composition :slots 0 :params :offset])))))
  (testing "an index outside the vector changes nothing"
    (is (nil? (transition (with-instance {:offset [0 0]}) [:studio/param-components "i1" :offset {3 1} [0 0]])))))

(deftest ember-rings-sit-on-a-disc-and-shade-each-arc-in-its-colour
  (let [axes (mapv #(geometry/axis % {:bounds [0 1]}) ["Low" "Mid" "High" "Air"])
        ember (html [widgets/rings-glyph "Tone" axes [0.2 0.4 0.6 0.8] :ember])
        standard (html [widgets/rings-glyph "Tone" axes [0.2 0.4 0.6 0.8]])]
    (is (str/includes? ember "rings-disc"))
    (is (= 4 (count-of #"<linearGradient" ember)) "one sweep gradient per ring")
    (is (str/includes? ember "var(--ring)") "each sweep reads its ring colour")
    (is (= 4 (count-of #"stroke=\"url\(#rings-" ember)))
    (is (not (str/includes? standard "rings-disc")) "Standard stays flat")))
