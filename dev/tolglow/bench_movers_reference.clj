(ns tolglow.bench-movers-reference
  "Declarative fixture/effect corpus for the complete Tolglow moving-show profiler."
  (:require [afterglow.bench-movement-reference :as movement]))

(defn fixtures [n]
  (mapv (fn [i]
          (let [slot (+ 506 (* 12 i))]
            {:id (str "moving-" i) :benchmark/index i
             :position [(- (* 2.3 (mod i 8)) 8.0) (+ 3.5 (* 0.3 (mod i 5)))
                        (- (* 2.1 (quot i 8)) 6.0)]
             :rotation-deg [(- (* 7.0 (mod i 7)) 21.0)
                            (- (* 17.0 (mod i 13)) 102.0)
                            (- (* 11.0 (mod i 9)) 44.0)]
             :calibration {:pan {:min-deg -270.0 :max-deg 270.0}
                           :tilt {:min-deg -135.0 :max-deg 135.0}}
             :universe (inc (quot slot 512)) :address (inc (mod slot 512)) :footprint 12
             :channels {:red 1 :green 2 :blue 3 :white 4 :dimmer 5
                        :pan [6 7] :tilt [8 9] :focus 10 :zoom 11}})) (range n)))

(def layer-values
  [{:color [0.9 0.1 0.15] :white 0.04 :dimmer 0.65 :focus 0.2 :zoom 0.15}
   {:color [0.1 0.8 0.25] :white 0.12 :dimmer 0.8 :focus 0.6 :zoom 0.45}
   {:color [0.15 0.2 0.95] :white 0.23 :dimmer 0.95 :focus 0.85 :zoom 0.75}])

;; Explicit endpoint, rise/fall and concurrent-layer combinations repeat during
;; warmup and measurement. The first sixteen preview targets are not special.
(def alpha-cycle
  [[1.0 0.0 0.0] [1.0 0.25 0.0] [1.0 0.5 0.0] [1.0 0.75 0.0] [1.0 1.0 0.0]
   [1.0 0.75 0.25] [1.0 0.5 0.5] [1.0 0.25 0.75] [1.0 0.0 1.0]
   [0.75 0.25 0.75] [0.5 0.5 0.5] [0.25 0.75 0.25] [0.0 1.0 0.0]
   [0.0 0.0 1.0] [0.0 0.0 0.0] [0.25 0.0 0.0] [0.5 0.0 0.0]
   [0.75 0.0 0.0] [1.0 1.0 1.0] [0.4 0.7 0.2]])

(defn alphas [i] (nth alpha-cycle (mod i (count alpha-cycle))))
(defn effect-id [layer group] (keyword "moving-profile" (str "layer-" layer "-group-" group)))
(defn- op [kind & args] {:op kind :args (vec args)})

(defn definitions [fixtures]
  (vec (for [layer (range 3) group (range 8)
             :let [phase (op :add (op :add (op :multiply {:op :position :axis :x} 0.137)
                                                       (op :multiply {:op :position :axis :z} 0.19))
                                 (+ (* group 0.2) (* layer 0.7)))
                   theta (op :add (op :multiply {:op :beat} (+ 0.35 (* group 0.02) (* layer 0.1))) phase)]]
         {:id (effect-id layer group) :version 1 :priority (* layer 10)
          :targets (mapv :id (filter #(= group (mod (:benchmark/index %) 8)) fixtures))
          :outputs (assoc (nth layer-values layer) :aim
                          (op :vec3
                              (op :add (op :multiply 5.0 (op :sin theta))
                                  (op :multiply {:op :position :axis :x} 0.1))
                              (op :add 1.2 (op :multiply 0.6 (op :cos (op :multiply theta 0.7))))
                              (op :add (+ 2.0 layer) (op :multiply 4.0 (op :cos theta)))))})))

(defn expected-aim [fixture layer beat]
  (let [[x _ z] (:position fixture) group (mod (:benchmark/index fixture) 8)
        phase (+ (+ (* x 0.137) (* z 0.19)) (+ (* group 0.2) (* layer 0.7)))
        theta (+ (* beat (+ 0.35 (* group 0.02) (* layer 0.1))) phase)]
    [(+ (* 5.0 (Math/sin theta)) (* x 0.1))
     (+ 1.2 (* 0.6 (Math/cos (* theta 0.7))))
     (+ (+ 2.0 layer) (* 4.0 (Math/cos theta)))]))

(defn expected-assigned [fixture beat alpha]
  (reduce (fn [values layer]
            (let [gain (nth alpha layer)]
              (if (zero? gain) values
                  (reduce-kv (fn [values key value]
                               (let [old (get values key (if (vector? value) [0.0 0.0 0.0] 0.0))
                                     blend (fn [a b] (if (= gain 1.0) b (+ a (* gain (- b a)))))]
                                 (assoc values key (if (vector? value) (mapv blend old value) (blend old value)))))
                             values (assoc (nth layer-values layer) :aim (expected-aim fixture layer beat))))))
          {} (range 3)))

(defn check-frame! [fixtures beat alpha previous-preview preview universes]
  (movement/check-frame! fixtures (mapv #(expected-assigned % beat alpha) fixtures)
                         previous-preview preview universes))

(defn self-check! []
  (movement/self-check!)
  (let [fixture (first (fixtures 1))]
    (doseq [[alpha layer] [[[1.0 0.0 0.0] 0] [[1.0 1.0 0.0] 1] [[1.0 1.0 1.0] 2]]]
      (when-not (= (nth layer-values layer) (dissoc (expected-assigned fixture 1.0 alpha) :aim))
        (throw (ex-info "Opaque layer must replace earlier layers" {:layer layer}))))
    (when-not (= {} (expected-assigned fixture 1.0 [0.0 0.0 0.0]))
      (throw (ex-info "Transparent layers must contribute nothing" {})))
    true))
