(ns tolglow.surface.vector-encoders-test
  "The device-independent vector takeover (#813): which parameters spread,
  where their components land, touch ownership, the linger, the nudges a
  turn sends and the display view Studio receives."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.test :refer [deftest is testing]]
            [tolglow.surface.vector-encoders :as vectors]))

(def ^:private ring {:type :vector :size 4 :bounds [0 1] :step 0.05 :default [0 0 0 0]})

(defn- candidate [schema]
  (assoc (vectors/spread :offset schema) :instance-id "i1" :basis {}))

(defn- model [value] {:instances {"i1" {:id "i1" :params {:offset value}}}})

(deftest which-parameters-spread
  (is (= ["X" "Y" "Z" "W"] (mapv :label (:components (vectors/spread :offset ring)))))
  (is (= [0.05 0.05 0.05 0.05] (mapv (comp :step :schema) (:components (vectors/spread :offset ring)))))
  (is (= ["#e8a083" "#aed39a" "#89b9e0" "#cfb2dd"] (mapv :color (:components (vectors/spread :offset ring)))))
  (testing "XY, XY+Z and an unbounded vector (component readouts) spread"
    (is (= 2 (count (:components (vectors/spread :xy {:type :vector :size 2 :bounds [-1 1]})))))
    (is (= 3 (count (:components (vectors/spread :aim {:type :vector :size 3}))))))
  (testing "a component's own bounds are kept within the vector's, which every element must satisfy"
    (is (= [[0 1] [0.25 1]]
           (mapv (comp :bounds :schema)
                 (:components (vectors/spread :p {:type :vector :size 2 :bounds [0 1]
                                                  :component-schemas [{:bounds [-5 5]} {:bounds [0.25 10]}]})))))
    (is (= [[0 10] [-1 1]]
           (mapv (comp :bounds :schema)
                 (:components (vectors/spread :p {:type :vector :size 2
                                                  :component-schemas [{:bounds [0 10]} {:bounds [-1 1]}]})))))
      "an unbounded vector keeps its components' own bounds")
  (testing "a colour vector, a scalar, one component, more than eight, or an empty component range do not"
    (is (nil? (vectors/spread :tint {:type :vector :size 3 :bounds [0 1] :semantic/type :rgb})))
    (is (nil? (vectors/spread :level {:type :number :bounds [0 1]})))
    (is (nil? (vectors/spread :one {:type :vector :size 1 :bounds [0 1]})))
    (is (nil? (vectors/spread :nine {:type :vector :size 9 :bounds [0 1]})))
    (is (nil? (vectors/spread :p {:type :vector :size 2 :bounds [0 1] :component-schemas [nil {:bounds [2 3]}]})))))

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
        (is (= {:state closed :commands []} (vectors/turn closed 4 3 false 0 1000)))
        (is (not (vectors/owns? (vectors/lift closed 4 0 1000) 4)))
        (is (nil? (vectors/lift closed 6 0 1000)))))
    (testing "a finger still held from a closed takeover is a touch of the next one over it"
      ;; Encoders 2 and 4 stay held across a forced close; 3 opens the next ring over 3-6.
      (let [next-open (vectors/touch (vectors/close two) 3 (candidate ring) #{} 8)]
        (is (= [3 4 5 6] (vectors/slots next-open)))
        (is (= #{3 4} (get-in next-open [:open :touched])) "encoder 4 is still held, so it counts")
        (is (nil? (get-in (vectors/lift next-open 3 10 1000) [:open :until]))
            "lifting the opening finger does not start the linger while 4 is held")
        (is (= 1010 (get-in (-> next-open (vectors/lift 3 10 1000) (vectors/lift 4 10 1000)) [:open :until])))))
    (testing "refresh closes a takeover that no longer applies, then one whose linger ran out"
      (is (not (vectors/open? (vectors/refresh two (constantly false) 0))))
      (is (vectors/open? (vectors/refresh two (constantly true) 99999)) "touched: no linger")
      (is (not (vectors/open? (vectors/refresh (vectors/lift (vectors/lift two 2 0 1000) 4 0 1000)
                                               (constantly true) 1000)))))))

(deftest turns-send-one-owner-resolved-nudge-per-event
  (let [state (vectors/touch vectors/initial-state 2 (candidate ring) #{} 8)]
    (is (= {:state state
            :commands [{:op :param/nudge :instance-id "i1" :param :offset :component 1 :detents 2 :fine? false}]}
           (vectors/turn state 3 2 false 0 1000)))
    (is (= [{:op :param/nudge :instance-id "i1" :param :offset :component 0 :detents -1 :fine? true}]
           (:commands (vectors/turn state 2 -1 true 0 1000))))
    (is (= [] (:commands (vectors/turn state 2 0 false 0 1000))) "no detents, no command")
    (testing "an untouched turn restarts the linger"
      (let [lifted (vectors/lift state 2 0 1000)]
        (is (= 5000 (get-in (:state (vectors/turn lifted 4 1 false 4000 1000)) [:open :until])))))))

(def ^:private studio-fixture
  "The decorated view Studio's push_display_test reads (after a JSON round
  trip), so a shape drift between this namespace and Studio fails a test."
  "src/studio/tolglow/studio/fixtures/push_vector_takeover.edn")

(defn- fixture-view []
  (let [state (vectors/touch vectors/initial-state 2 (candidate (assoc ring :name "Offset")) #{} 8)
        m (assoc-in (model [0.25 0.5 0.75 1.0]) [:instances "i1" :definition :params :offset] (assoc ring :name "Offset"))
        level (fn [k] {:id k :param k :name (name k) :value 0.5 :component 0 :component-label :value
                       :schema {:type :number :bounds [0 1]}})]
    (vectors/decorate {:instance-id "i1" :parameters (mapv level [:a :b :offset :c :d :e :f :g])
                       :touched-parameters []}
                      state m)))

(deftest the-display-shows-components-in-the-taken-columns
  (let [state (vectors/touch vectors/initial-state 6 (candidate ring) #{} 8)
        m (assoc-in (model [0.25 0.5 0.75 1.0]) [:instances "i1" :definition :params :offset] ring)
        view {:parameters [{:param :a} {:param :b} {:param :offset} {:param :c}] :touched-parameters []}
        decorated (vectors/decorate view state m)]
    (is (= [:a :b :offset :c :offset :offset :offset :offset] (mapv :param (:parameters decorated))))
    (is (= [nil nil nil nil 0 1 2 3] (mapv :component (:parameters decorated))))
    (is (= [{:instance-id "i1" :param :offset :component 2 :encoder 6}] (:touched-parameters decorated)))
    (is (= ["0.250" "0.500" "0.750" "1.000"] (mapv :text (get-in decorated [:vector-encoders :components]))))
    (is (= {4 {:component 0 :color "#e8a083" :touched? false} 5 {:component 1 :color "#aed39a" :touched? false}
            6 {:component 2 :color "#89b9e0" :touched? true} 7 {:component 3 :color "#cfb2dd" :touched? false}}
           (vectors/led-intents state)))
    (is (= view (vectors/decorate view (vectors/close state) m)) "a closed takeover leaves the view alone"))
  (testing "Studio's fixture is this namespace's output; regenerate it with (write-studio-fixture!)"
    (is (= (fixture-view) (edn/read-string (slurp (io/file studio-fixture)))))))

(defn write-studio-fixture!
  "Rewrite Studio's fixture from decorate's current output."
  []
  (spit studio-fixture (with-out-str (pprint/pprint (fixture-view)))))
