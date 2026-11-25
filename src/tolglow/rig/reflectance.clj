(ns tolglow.rig.reflectance
  "Estimate a receiver's diffuse return relative to a neutral card at the same point."
  (:require [tolglow.rig.surfaces :as surfaces]))

(defn- check [ok message]
  (when-not ok (throw (ex-info message {:component :surface-reflectance}))))

(defn- finite? [x] (and (number? x) (Double/isFinite (double x))))

(defn- positive? [x] (and (finite? x) (pos? x)))

(defn eligible-lamp? [{:keys [channels position measured-relative-brightness]}]
  (and (positive? measured-relative-brightness) (surfaces/xyz? position)
       (not-any? #(contains? channels %) [:pan :tilt])))

(defn receivers [rig]
  (mapv (fn [s] {:id (surfaces/wire-id (:id s)) :point (:point s)
                 :revision (pr-str (mapv s [:point :normal :u-axis :size :reflectance]))})
        (:surfaces rig)))

(defn prepare [rig fixture {:keys [receiver-id receiver-revision card-fraction card-tolerance shutter-denominator]}]
  (let [planes (:surfaces rig)
        index (first (keep-indexed #(when (= (surfaces/wire-id receiver-id) (surfaces/wire-id (:id %2))) %1) planes))
        surface (get planes index)]
    (check (and surface (= receiver-revision (:revision (get (receivers rig) index))))
           "Reconnect and select the current receiving surface")
    (check (eligible-lamp? fixture) "Choose a stationary fixture with measured white brightness and rig coordinates")
    (check (and (positive? card-fraction) (positive? card-tolerance)
                (< 0 (- card-fraction card-tolerance) (+ card-fraction card-tolerance) 1)
                (integer? shutter-denominator) (<= 1 shutter-denominator 10000))
           "Enter the neutral card's nominal fraction and its rated absolute tolerance, and a valid shutter")
    (let [ray (mapv - (:position fixture) (:point surface))
          distance (Math/sqrt (reduce + (map #(* % %) ray)))
          n (:n (surfaces/surface-axes surface))
          incidence (when (positive? distance) (/ (reduce + (map * ray n)) distance))]
      (check (and (positive? distance) (finite? incidence) (<= 0.25 incidence 1.000001))
             "Choose a fixture facing the receiver's measured front, away from grazing incidence")
      {:index index :samples [] :card-fraction card-fraction :card-tolerance card-tolerance
       :shutter-denominator shutter-denominator
       :geometry {:fixture-id (:id fixture) :fixture-position (:position fixture)
                  :fixture-relative-brightness (:measured-relative-brightness fixture)
                  :point (:point surface) :normal n :source-distance-m distance :incidence-cosine incidence}})))

(defn- sample [values]
  (check (and (vector? values) (= 15 (count values))
              (every? #(and (finite? %) (<= 0 % 1)) values))
         "Supply 15 linear, unclipped frame luminances per phase")
  (let [sorted (vec (sort values))
        [lo mid hi] (mapv sorted [1 7 13])]
    (check (<= (- hi lo) (max 0.003 (* 0.12 mid))) "Frame series varied too much")
    {:low lo :value mid :high hi}))

(defn- signal [[dark lit]]
  (let [low (- (:low lit) (:high dark))]
    (check (>= low (max 0.003 (* 0.03 (:value dark)))) "Too little light above the ambient background")
    {:low low :value (- (:value lit) (:value dark)) :high (- (:high lit) (:low dark))}))

(defn- estimate [{:keys [samples card-fraction card-tolerance geometry shutter-denominator]}]
  (let [[first-card surface last-card] (map signal (partition 2 samples))
        avg-card (/ (+ (:value first-card) (:value last-card)) 2.0)
        drift (/ (abs (- (:value first-card) (:value last-card))) avg-card)
        ratio (/ (:value surface) avg-card)
        low (/ (* (- card-fraction card-tolerance) (:low surface)) (max (:high first-card) (:high last-card)))
        high (/ (* (+ card-fraction card-tolerance) (:high surface)) (min (:low first-card) (:low last-card)))]
    (check (<= drift 0.05) "Reference changed by more than 5%. Check the mount, card and illumination")
    (check (<= low 1) "Result exceeds the diffuse range. Check for glare or inconsistent card placement")
    {:method "bracketed-neutral-card" :relative-to-card ratio :estimate (* card-fraction ratio)
     :range [low high] :reference-drift drift :geometry geometry :card-fraction card-fraction
     :card-tolerance card-tolerance :shutter-denominator shutter-denominator :samples samples}))

(defn record [meter {:keys [sample-index frame-levels]} phase confirmed?]
  (let [i (count (:samples meter)) required (if (even? i) :dark :white)]
    (check (and (= i sample-index) (< i 6) (= phase required) confirmed?)
           "Submit the current confirmed dark or white phase in reference, surface, reference order")
    (let [measured (update meter :samples conj (sample frame-levels))]
      (cond-> measured (= i 5) (assoc :fit (estimate measured))))))
