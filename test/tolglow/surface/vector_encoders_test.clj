(ns tolglow.surface.vector-encoders-test
  "The device-independent vector takeover (#813): which parameters spread,
  where their components land, touch ownership, the linger and component
  edits against the committed value."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.surface.vector-encoders :as vectors]))

(def ^:private ring {:type :vector :size 4 :bounds [0 1] :step 0.05 :default [0 0 0 0]})

(defn- candidate [schema]
  (assoc (vectors/spread :offset schema) :instance-id "i1" :basis {}))

(defn- model [value] {:instances {"i1" {:id "i1" :params {:offset value}}}})

(deftest which-parameters-spread
  (is (= ["X" "Y" "Z" "W"] (mapv :label (:components (vectors/spread :offset ring)))))
  (is (= [0.05 0.05 0.05 0.05] (mapv (comp :step :schema) (:components (vectors/spread :offset ring)))))
  (is (= ["#e8a083" "#aed39a" "#89b9e0" "#cfb2dd"] (mapv :color (:components (vectors/spread :offset ring)))))
  (testing "XY, XY+Z and an unbounded vector (component readouts) spread; per-component bounds are kept"
    (is (= 2 (count (:components (vectors/spread :xy {:type :vector :size 2 :bounds [-1 1]})))))
    (is (= 3 (count (:components (vectors/spread :aim {:type :vector :size 3})))))
    (is (= [[0 1] [0 10]] (mapv (comp :bounds :schema)
                                (:components (vectors/spread :p {:type :vector :size 2 :bounds [0 1]
                                                                 :component-schemas [nil {:bounds [0 10]}]}))))))
  (testing "a colour vector, a scalar, one component or more than eight do not"
    (is (nil? (vectors/spread :tint {:type :vector :size 3 :bounds [0 1] :semantic/type :rgb})))
    (is (nil? (vectors/spread :level {:type :number :bounds [0 1]})))
    (is (nil? (vectors/spread :one {:type :vector :size 1 :bounds [0 1]})))
    (is (nil? (vectors/spread :nine {:type :vector :size 9 :bounds [0 1]})))))

(deftest components-land-to-the-right-of-the-touch-or-as-far-right-as-they-fit
  (is (= 2 (vectors/window 2 4 8)))
  (is (= 4 (vectors/window 6 4 8)))
  (is (= 0 (vectors/window 0 8 8)))
  (let [state (vectors/touch vectors/initial-state 6 (candidate ring) #{} 8)]
    (is (= [4 5 6 7] (vectors/slots state)))
    (is (= 2 (vectors/component-at state 6)) "the touched encoder edits the component under it")
    (is (nil? (vectors/component-at state 3)))))

(deftest touches-and-lifts
  (testing "a window holding another touched encoder does not open"
    (is (nil? (vectors/touch vectors/initial-state 2 (candidate ring) #{4} 8)))
    (is (some? (vectors/touch vectors/initial-state 2 (candidate ring) #{2 6} 8)) "touches outside the window do not matter"))
  (testing "no candidate, no takeover: the touch is left to the device"
    (is (nil? (vectors/touch vectors/initial-state 2 nil #{} 8))))
  (let [opened (vectors/touch vectors/initial-state 2 (candidate ring) #{} 8)
        two (vectors/touch opened 4 nil #{} 8)]
    (is (= #{2 4} (get-in two [:open :touched])) "a touch inside the takeover is its own, candidate or not")
    (is (nil? (vectors/touch two 7 (candidate ring) #{} 8)) "a touch outside it is left to the device")
    (let [lifted (-> two (vectors/lift 2 100 1000) (vectors/lift 4 200 1000))]
      (is (= 1200 (get-in lifted [:open :until])) "the last lift starts the linger")
      (is (vectors/open? (vectors/expire lifted 1199)))
      (is (not (vectors/open? (vectors/expire lifted 1200))))
      (let [retouched (vectors/touch lifted 3 nil #{} 8)]
        (is (nil? (get-in retouched [:open :until])) "a touch within the linger keeps it open")
        (is (= 1300 (get-in (vectors/lift retouched 3 300 1000) [:open :until])) "until its own lift and linger")))
    (testing "a forced close keeps a held encoder's lift and turns until it lifts"
      (let [closed (vectors/close two)]
        (is (vectors/owns? closed 4))
        (is (= {:state closed :commands []} (vectors/turn closed 4 3 false (model [0 0 0 0]) 0 1000)))
        (is (not (vectors/owns? (vectors/lift closed 4 0 1000) 4)))
        (is (nil? (vectors/lift closed 6 0 1000)))))))

(deftest turns-edit-one-component-of-the-committed-vector
  (let [state (vectors/touch vectors/initial-state 2 (candidate ring) #{} 8)
        {s1 :state [c1] :commands} (vectors/turn state 3 2 false (model [0.0 0.0 0.0 0.0]) 0 1000)]
    (is (= {:op :param/set :instance-id "i1" :param :offset} (dissoc c1 :value)))
    (is (= [0.0 0.1 0.0 0.0] (mapv #(/ (Math/round (* 1000 (double %))) 1000.0) (:value c1))))
    (testing "while that commit is in flight a turn on another component keeps it"
      (let [{[c2] :commands} (vectors/turn s1 5 1 false (model [0.0 0.0 0.0 0.0]) 0 1000)]
        (is (= [0.0 0.1 0.0 0.05] (mapv #(/ (Math/round (* 1000 (double %))) 1000.0) (:value c2))))))
    (testing "an edit from elsewhere that lands first wins"
      (let [{[c2] :commands} (vectors/turn s1 5 1 false (model [0.5 0.5 0.5 0.5]) 0 1000)]
        (is (= [0.5 0.5 0.5 0.55] (mapv #(/ (Math/round (* 1000 (double %))) 1000.0) (:value c2))))))
    (testing "bounds clamp and an unchanged value sends nothing"
      (is (= [] (:commands (vectors/turn state 2 -4 false (model [0.0 0.0 0.0 0.0]) 0 1000)))))
    (testing "Shift is a tenth of the step"
      (let [x (-> (vectors/turn state 2 1 true (model [0.0 0.0 0.0 0.0]) 0 1000) :commands first :value first)]
        (is (< (Math/abs (- 0.005 x)) 1e-12))))
    (testing "an untouched turn restarts the linger"
      (let [lifted (vectors/lift state 2 0 1000)]
        (is (= 5000 (get-in (:state (vectors/turn lifted 4 1 false (model [0 0 0 0]) 4000 1000)) [:open :until])))))
    (testing "integer components step by whole numbers"
      (let [ints (vectors/touch vectors/initial-state 0
                                (candidate {:type :vector :size 2 :bounds [0 10]
                                            :component-schemas [{:type :integer} {:type :integer}]})
                                #{} 8)]
        (is (= [3 0] (:value (first (:commands (vectors/turn ints 0 3 false (model [0 0]) 0 1000))))))))))

(deftest the-display-shows-components-in-the-taken-columns
  (let [state (vectors/touch vectors/initial-state 6 (candidate ring) #{} 8)
        m (assoc-in (model [0.25 0.5 0.75 1.0]) [:instances "i1" :definition :params :offset] ring)
        view {:parameters [{:param :a} {:param :b} {:param :offset} {:param :c}] :touched-parameters []}
        decorated (vectors/decorate view state m)]
    (is (= [:a :b :offset :c :offset :offset :offset :offset] (mapv :param (:parameters decorated))))
    (is (= [nil nil nil nil 0 1 2 3] (mapv :component (:parameters decorated))))
    (is (= [{:instance-id "i1" :param :offset :component 2 :encoder 6}] (:touched-parameters decorated)))
    (is (= ["0.250" "0.500" "0.750" "1.000"] (mapv :text (get-in decorated [:vector-encoders :components]))))
    (is (= {4 {:component 0 :touched? false} 5 {:component 1 :touched? false}
            6 {:component 2 :touched? true} 7 {:component 3 :touched? false}}
           (vectors/led-intents state)))
    (is (= view (vectors/decorate view (vectors/close state) m)) "a closed takeover leaves the view alone")))
