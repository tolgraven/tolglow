(ns tolglow.rig.reference
  "Canonical reference rigs and the rig descriptor rig.sig (#858).

  A reference rig is ordinary rig data with declared roles, built from the
  fixture types the showcase and 2023 gig rigs already publish. A decision
  retargeted onto one (tolglow.rig.retarget) can be compared, benchmarked and
  shared without any venue's geometry. There is one rig per class:

    :pars                 four front and two back RGBW pars
    :pars-strip           front and back pars and a pixel strip upstage
    :movers-pars          front and back pars and four moving heads upstage
    :movers-pars-pixels   pars, narrow beams upstage, pixel tubes and blinders

  rig.sig is a fixed-length, versioned descriptor: counts by kind, extent,
  spacing, height layers, symmetry and capability shares, each scaled to
  0…1 (signature-fields). classify assigns a rig to the class of the nearest
  reference rig by that descriptor.

    (require '[tolglow.rig.reference :as reference] '[tolglow.venues.gig-2023 :as gig])
    (reference/classify (gig/rig))        ; {:class :movers-pars :distance …}
    (reference/distance-table {:gig (gig/rig) :pars (reference/rig :pars)})
    (def runtime (show/start! {:model (reference/model :pars-strip)}))

  Loading this namespace builds data and opens nothing."
  (:require [afterglow.control.model :as control-model]
            [tolglow.fixtures.patch :as patch]
            [tolglow.registry :as registry]
            [tolglow.rig.pixel-layout :as pixel-layout]
            [tolglow.rig.roles :as roles]
            [tolglow.showcase.rig :as showcase-rig]))

;; rig.sig -------------------------------------------------------------------

(def signature-version 1)

(def signature-fields
  "rig.sig version 1, in order. A unit is one fixture, or one pixel layout
  with all its heads (tolglow.rig.roles/units)."
  [:units          ; log2(1 + units) / log2(1 + 64)
   :pixels         ; log2(1 + pixel heads) / log2(1 + 4096)
   :mover-share    ; share of units with pan and tilt
   :static-share   ; share of units without pan, tilt or pixels
   :pixel-share    ; share of units that are pixel layouts
   :atmosphere-share
   :width          ; log2(1 + extent m) / log2(1 + 40), over head positions
   :height         ; log2(1 + extent m) / log2(1 + 20)
   :depth          ; log2(1 + extent m) / log2(1 + 40)
   :spacing        ; d / (d + 2) for the median nearest-unit distance d in metres
   :layers         ; distinct 0.75 m height bands of units / 4, at most 1
   :symmetry       ; share of units with a mirror partner of the same kind across the rig's centre line
   :color-share    ; units with red, green and blue
   :white-share    ; units with white
   :strobe-share   ; units with a strobe or shutter channel
   :narrow-share]) ; units with a beam of at most 15 degrees

(defn- log-scale [n top] (min 1.0 (/ (Math/log (+ 1.0 (double n))) (Math/log (+ 1.0 (double top))))))

(defn- share [pred xs] (if (empty? xs) 0.0 (/ (double (count (filter pred xs))) (count xs))))

(defn- dist [a b] (Math/sqrt (reduce + (map #(let [d (- (double %1) (double %2))] (* d d)) a b))))

(defn- median [xs]
  (when (seq xs)
    (let [v (vec (sort xs)) n (count v)]
      (if (odd? n) (nth v (quot n 2)) (/ (+ (nth v (dec (quot n 2))) (nth v (quot n 2))) 2.0)))))

(defn signature
  "rig.sig for rig: {:v signature-version :x [numbers in 0…1]} in the order of
  signature-fields."
  [rig]
  (let [fixtures (:fixtures rig)
        units (filterv :position (roles/units fixtures))
        kinds (mapv roles/kind units)
        {:keys [min max]} (or (roles/bounds fixtures) {:min [0 0 0] :max [0 0 0]})
        [w h d] (map - max min)
        centre-x (/ (+ (double (first min)) (double (first max))) 2.0)
        nearest (for [u units]
                  (reduce clojure.core/min Double/MAX_VALUE
                          (for [o units :when (not= (:id o) (:id u))] (dist (:position u) (:position o)))))
        spacing (or (median (remove #(= Double/MAX_VALUE %) nearest)) 0.0)
        mirrored? (fn [u]
                    (let [[x y z] (:position u)
                          mirror [(- (* 2.0 centre-x) x) y z]]
                      (some #(and (= (roles/kind u) (roles/kind %)) (<= (dist mirror (:position %)) 0.5)) units)))
        channels (fn [k] #(contains? (:channels %) k))]
    {:v signature-version
     :x [(log-scale (count units) 64)
         (log-scale (count (filter #(or (:pixel-layout-id %) (:fixture-id %)) fixtures)) 4096)
         (share #{:mover} kinds)
         (share #{:static} kinds)
         (share #{:pixel} kinds)
         (share #{:atmosphere} kinds)
         (log-scale w 40)
         (log-scale h 20)
         (log-scale d 40)
         (/ spacing (+ spacing 2.0))
         (clojure.core/min 1.0 (/ (count (distinct (map #(Math/floor (/ (double (second (:position %))) 0.75)) units))) 4.0))
         (share mirrored? units)
         (share #(every? (:channels %) [:red :green :blue]) units)
         (share (channels :white) units)
         (share #(or (contains? (:channels %) :strobe) (contains? (:channels %) :shutter)) units)
         (share #(some-> (:beam-angle %) (<= 15)) units)]}))

(def signature-weights
  "Field weights for distance, in the order of signature-fields. What kinds
  of head a rig has (pixels and movers above all) weighs more than its
  height or spacing, so a rig's class follows its capabilities first."
  [1.0 2.0 2.0 1.0 2.0 0.5 1.0 0.5 0.5 0.5 0.5 0.5 1.0 0.5 0.5 1.0])

(defn distance
  "Distance between two rig.sig descriptors of the same version, 0…1: the
  weighted root mean square difference over the fields (signature-weights)."
  [a b]
  (when-not (= (:v a) (:v b)) (throw (ex-info "rig.sig versions differ" {:a (:v a) :b (:v b)})))
  (Math/sqrt (/ (reduce + (map (fn [w x y] (let [d (- (double x) (double y))] (* w d d)))
                               signature-weights (:x a) (:x b)))
                (reduce + signature-weights))))

;; Reference rigs --------------------------------------------------------------

(def fixture-types
  "The existing fixture types the reference rigs use, unchanged."
  (select-keys showcase-rig/fixture-types [:rgbw-18-12-par :rgbw-7-12-moving :rgbw-60-moving-beam :wa-100-cob-par]))

(def room {:width 8 :height 4 :depth 6})

(def ^:private front-z 1.5)

(def ^:private back-z -1.5)

(def ^:private bar-y 3.6)

(defn- pars [xs z tilt]
  (mapv (fn [x] [:rgbw-18-12-par x bar-y z {:rotation-deg [tilt 0 0]}]) xs))

(def ^:private front-pars {:universe 1 :offset 1 :type :wash :list (pars [-3.0 -1.0 1.0 3.0] front-z 40)})

(def ^:private back-pars {:universe 1 :offset 41 :type :wash :list (pars [-3.0 -1.0 1.0 3.0] back-z -40)})

(defn- movers [type]
  ;; Hung under the upstage bar, yokes rolled over as on the showcase truss.
  {:universe 1 :offset 101 :type :moving
   :list (mapv (fn [x] [type x 3.4 back-z {:rotation-deg [0 0 180]}]) [-2.0 -0.7 0.7 2.0])})

(def ^:private strip
  {:id :strip :shape :path :group :strip :universe 2 :address 1 :pixels 48 :color-layout :rgbw
   :diffuser-radius-m 0.02 :points [[-3.0 1.2 -2.0] [3.0 1.2 -2.0]]})

(def ^:private tubes
  (vec (map-indexed (fn [i x]
                      {:id (keyword (str "tube-" (inc i))) :shape :path :group :tubes
                       :universe 2 :address (inc (* i 40)) :pixels 10 :color-layout :rgbw
                       :diffuser-radius-m 0.03 :points [[x 0.4 -2.0] [x 2.2 -2.0]]})
                    [-2.5 -1.5 -0.5 0.5 1.5 2.5])))

(def rigs
  "Reference rig ID -> definition. :patches and :pixel-layouts are the
  ordinary patch forms; :roles is the declared assignment."
  {:pars
   {:id :pars :class :pars :name "Pars only"
    :patches {:front-par front-pars
              :back-par (update back-pars :list #(vec (take-nth 3 %)))}
    :order [:front-par :back-par]
    :roles {:front.wash [:front-par-1 :front-par-2 :front-par-3 :front-par-4]
            :back.wash [:back-par-1 :back-par-2]}}

   :pars-strip
   {:id :pars-strip :class :pars-strip :name "Pars and a strip"
    :patches {:front-par front-pars :back-par back-pars}
    :order [:front-par :back-par]
    :pixel-layouts [strip]
    :roles {:front.wash [:front-par-1 :front-par-2 :front-par-3 :front-par-4]
            :back.wash [:back-par-1 :back-par-2 :back-par-3 :back-par-4]
            :back.pixel [:strip]}}

   :movers-pars
   {:id :movers-pars :class :movers-pars :name "Movers and pars"
    :patches {:front-par front-pars :back-par back-pars :back-mover (movers :rgbw-7-12-moving)}
    :order [:front-par :back-par :back-mover]
    :roles {:front.wash [:front-par-1 :front-par-2 :front-par-3 :front-par-4]
            :back.wash [:back-par-1 :back-par-2 :back-par-3 :back-par-4]
            :back.beam [:back-mover-1 :back-mover-2 :back-mover-3 :back-mover-4]}}

   :movers-pars-pixels
   {:id :movers-pars-pixels :class :movers-pars-pixels :name "Movers, pars and pixels"
    :patches {:front-par front-pars :back-par back-pars :back-beam (movers :rgbw-60-moving-beam)
              :back-blinder {:universe 1 :offset 201 :type :blinder
                             :list [[:wa-100-cob-par -3.4 3.3 back-z {:rotation-deg [-68 0 0]}]
                                    [:wa-100-cob-par 3.4 3.3 back-z {:rotation-deg [-68 0 0]}]]}}
    :order [:front-par :back-par :back-beam :back-blinder]
    :pixel-layouts tubes
    :roles {:front.wash [:front-par-1 :front-par-2 :front-par-3 :front-par-4]
            :back.wash [:back-par-1 :back-par-2 :back-par-3 :back-par-4]
            :back.beam [:back-beam-1 :back-beam-2 :back-beam-3 :back-beam-4]
            :back.blinder [:back-blinder-1 :back-blinder-2]
            :back.pixel (mapv :id tubes)}}})

(def classes
  "Rig classes in order of capability."
  [:pars :pars-strip :movers-pars :movers-pars-pixels])

(defn rig
  "The reference rig id as a validated rig with its roles declared."
  [id]
  (let [{:keys [patches order pixel-layouts roles] :as definition} (get rigs id)]
    (when-not definition (throw (ex-info "Unknown reference rig" {:id id :known (vec (keys rigs))})))
    (-> (cond-> {:room room
                 :fixtures (into (patch/expand fixture-types {} patches order)
                                 (mapcat pixel-layout/fixtures) pixel-layouts)}
          (seq pixel-layouts) (assoc :pixel-layouts pixel-layouts))
        (roles/assign roles))))

(defn model
  "A show model on reference rig id with its fixture types published."
  [id]
  (reduce #(update %1 :registry registry/publish :fixtures %2)
          (control-model/new-show {:show-id (keyword (str "reference-" (name id))) :rig (rig id)})
          (vals fixture-types)))

(def ^:private signatures (delay (into {} (map (fn [id] [id (signature (rig id))])) classes)))

(defn classify
  "The class of rig: the class of the nearest reference rig by rig.sig, with
  its distance and the distance to every reference rig."
  [rig]
  (let [sig (signature rig)
        distances (into (sorted-map) (map (fn [[id s]] [id (distance sig s)])) @signatures)
        [nearest d] (apply min-key val distances)]
    {:class (get-in rigs [nearest :class]) :nearest nearest :distance d :distances distances}))

(defn distance-table
  "Pairwise rig.sig distances for named rigs: {[a b] distance}."
  [named-rigs]
  (let [sigs (update-vals named-rigs signature)]
    (into (sorted-map) (for [[a sa] sigs [b sb] sigs] [[a b] (distance sa sb)]))))
