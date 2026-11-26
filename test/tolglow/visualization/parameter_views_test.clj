(ns tolglow.visualization.parameter-views-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.compiler :as compiler]
            [tolglow.demo :as demo]
            [tolglow.effects :as fx]
            [tolglow.show :as show]
            [tolglow.spatial-host-test :as host]
            [tolglow.visualization.parameter-views :as views]))

(defn- painted-volume
  "The effect a spatial paint request publishes, read back from a real show."
  []
  (let [rt (host/runtime true)]
    (try
      (host/submit rt 0 :spatial-cue
                   {:op :spawn :id :orb :page-id :main :cell [2 1]
                    :volume {:shape :ball :position [0 1 0] :velocity [0 0 0] :gravity [0 0 0]
                             :radius 0.5 :restitution 0.75 :selection #{:position}
                             :falloff {:inner-m 0.5 :outer-m 0.5 :curve :linear}}
                    :paint {:color [1.0 0.2 0.1]}})
      (get-in @(:state rt) [:model :registry :effects :orb])
      (finally (show/stop! rt)))))

(defn builtin-definitions
  "Every effect the application ships or builds from its own recipes: the
  default show, the portable examples, the authoring recipes and a painted volume."
  []
  (concat (vals (get-in (demo/model) [:registry :effects]))
          (compiler/demo-definitions)
          [(fx/wash :catalog/wash)
           (fx/breathing-wash :catalog/breathing-wash)
           (fx/aim-sweep :catalog/aim-sweep)
           (painted-volume)]))

(defn- parameters [definitions]
  (for [definition definitions [id schema] (:params definition)]
    {:effect (:id definition) :param id :schema schema}))

(defn- as-device-json
  "A Push view carries the schema through JSON, so its type is text."
  [schema]
  (cond-> (update schema :type name)
    (keyword? (:semantic/type schema)) (update :semantic/type name)))

(def preferences [nil :auto "auto" :spatial "spatial" :history "history" :controls "controls"])

(def source-states [nil :active "active" :held "held" :pending :error])

(deftest no-colour-parameter-in-the-builtin-registry-opens-the-vector-space
  (let [definitions (builtin-definitions)
        colours (filter (comp views/color-kind :schema) (parameters definitions))]
    (is (<= 19 (count definitions)) "the walk covers the shipped effects")
    (is (<= 5 (count colours)) "the shipped effects include colour parameters")
    (is (some #(= [:orb :color] ((juxt :effect :param) %)) colours))
    (doseq [{:keys [effect param schema]} colours
            schema [schema (as-device-json schema)]
            :let [where (pr-str [effect param (:type schema)])]]
      (is (nil? (views/spatial-kind schema)) where)
      (is (= :color (views/monitor-view schema)) where)
      (is (= [:color :history] (views/studio-choices schema)) where)
      (doseq [preference preferences]
        (is (not= :spatial (views/studio-view schema preference)) where)
        (doseq [state source-states
                :let [plotted? (and (#{:history "history"} preference) (#{:active "active"} state))]]
          (is (= (if plotted? :history :color)
                 (views/push-view {:schema schema :source-state state} preference))
              where))))))

(deftest spatial-vectors-keep-the-vector-space-and-declared-colours-leave-it
  (let [vectors (filter #(= :vector (:type (:schema %))) (parameters (builtin-definitions)))]
    (is (some #(= [:catalog/aim-sweep :center] ((juxt :effect :param) %)) vectors))
    (doseq [{:keys [schema]} vectors
            schema [schema (as-device-json schema)]]
      (is (= :position (views/spatial-kind schema)))
      (is (= :spatial (views/push-view {:schema schema} :auto)))
      (is (= :spatial (views/studio-view schema nil)))
      (is (= [:spatial :history] (views/studio-choices schema)))
      (doseq [layout [:rgb "rgb" :hsl :rgbw :color]
              :let [declared (assoc schema :semantic/type layout)]]
        (is (nil? (views/spatial-kind declared)))
        (is (= :color (views/push-view {:schema declared} :spatial)))
        (is (= :color (views/studio-view declared :spatial)))))
    (is (= :orientation (views/spatial-kind {:type "vector" :size 3 :semantic/type "orientation"})))
    (is (= :direction (views/spatial-kind {:type :vector :size 3 :semantic/type :direction})))
    (is (nil? (views/spatial-kind {:type :vector :size 4})))))

(deftest discrete-and-scalar-parameters-keep-their-controls
  (is (= :controls (views/selected-mode {:type :enum :values [:a :b]} :history)))
  (is (= :controls (views/selected-mode {:type :number :choices [{:value 0}]} :history)))
  (is (= :history (views/selected-mode {:type :number :visualization :history} :auto)))
  (is (= :controls (views/selected-mode {:type :number} nil)))
  (is (= :color (views/selected-mode {:type :gradient} :history)))
  (is (= [:color] (views/studio-choices {:type :gradient})))
  (is (= [:history] (views/studio-choices {:type :number})))
  (is (= :history (views/monitor-view {:type :number})))
  (let [schema {:type :number :visualization :history}
        closed (views/history-toggle schema :auto)]
    (is (= :controls (views/selected-mode schema closed)))
    (is (= :history (views/history-toggle schema closed)))))

(deftest each-colour-layout-converts-from-its-own-space
  (let [near? (fn [a b] (every? #(< (Math/abs (double %)) 1e-9) (map - a b)))]
    (doseq [[layout value rgba] [[:rgb [1 0.5 0] [1 0.5 0 1]]
                                 [:rgba [0 0 1 0.5] [0 0 1 0.5]]
                                 [:rgbw [0.5 0 0 0.5] [1 0.5 0.5 1]]
                                 [:hsl [1/3 1 0.5] [0 1 0 1]]
                                 [:hsla [2/3 1 0.25 0.5] [0 0 0.5 0.5]]
                                 [:hsv [0 1 1] [1 0 0 1]]
                                 [:hsv [1.5 1 0.5] [0 0.5 0.5 1]]]
            schema [{:type :vector :semantic/type layout} {:type "vector" :semantic/type (name layout)}]]
      (is (= layout (views/color-space schema)))
      (is (near? rgba (views/color-rgba layout value)) (pr-str layout value)))
    (is (near? [1 0.5 0] (views/hsv->rgb (views/rgb->hsv [1 0.5 0]))))
    (is (= :rgba (views/color-space {:type :color})))
    (is (near? [1 0 0 1] (views/color-rgba :rgba [1 0 0])) "an RGB value without alpha is opaque")
    (is (= ["R" "G" "B" "W"] (views/component-labels {:type "vector" :semantic/type "rgbw"})))
    (is (= ["H" "S" "V"] (views/component-labels {:type :vector :semantic/type :hsv})))
    (is (= ["X" "Y" "Z" "W"] (views/component-labels {:type :vector :size 3})))
    (is (= ["P" "T"] (views/component-labels {:type :vector :component-labels ["P" "T"]})))))
