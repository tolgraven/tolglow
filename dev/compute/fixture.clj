(ns compute.fixture
  "Deterministic multi-universe show for checking native and distributed render output."
  (:require [afterglow.control.model :as control]
            [tolglow.demo :as demo]))

(defn rig []
  (mapv (fn [i]
          (assoc (first (demo/fixtures)) :id (keyword (str "beam-" i)) :universe (inc (quot i 32))
                 :address (inc (* 12 (mod i 32))) :position [(- (* 0.25 (mod i 16)) 2.0) 3.0 (- (quot i 16) 2.0)]))
        (range 64)))

(defn program [i]
  (let [id (keyword (str "layer-" i)), p demo/param, lit demo/constant]
    {:id id :name (name id) :version 1
     :params {:red {:type :number :default (/ (+ 1.0 (mod i 7)) 8) :bounds [0 1]}
              :aim-x {:type :number :default 0.25 :bounds [-2 2]}}
     :outputs {:dimmer (lit 0.75)
               :focus (lit 0.375)
               :color {:op :vec3 :args [(p :red) (lit 0.125) (lit (/ (+ 1.0 (mod i 5)) 8))]}
               :aim {:op :vec3 :args [(p :aim-x) (lit 0.0)
                                      {:op :add :args [(lit 2.0) {:op :position :axis :x}]}]}}}))

(defn model []
  (let [effects (mapv program (range 18))
        cues (map-indexed (fn [i {:keys [id name]}] {:id id :name name :version 1 :effect-id id :priority i}) effects)]
    (assoc (control/new-show {:registry {:effects (into {} (map (juxt :id identity)) effects)
                                         :cues (into {} (map (juxt :id identity)) cues)}})
           :rig {:fixtures (rig) :room {:width 10 :height 5 :depth 8}})))
