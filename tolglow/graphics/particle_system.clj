(ns tolglow.graphics.particle-system
  (:require [newtonian.utils :as utils]
            [tolglow.param :as param])
  (:import [newtonian.utils Vector2D])) ;gotta be a shit for brains idea to shadow java stuff in thid way?

(defprotocol Emission
  (emit [this] "Emits a Particle with ParticleEmitter attributes"))

;maybe Time and Gravity? dunno, gravity causes acceleration which when applied with time causes movement
(defprotocol Kinematics "Movements of bodies"
  (move [b] [b p] "Moves body one time step"))

(defprotocol FieldForce "Force influence by field"
  ;; (influence [field particle] "Exact influence on a particle"))
  (influence [particle field] "Exact influence on a particle"))
;^ wait this goes field particle, opposite down in record?
;guess no impact as long as all are same...
;; (defprotocol Oscillation "Something to apply mild randomness to objects...")

(defrecord Particle
  [#^Vector2D position #^Vector2D velocity #^Vector2D accel
   ;; #^int mass
   #^int age #^int death ^clojure.lang.Keyword parent-id])

(defrecord ParticleEmitter ;oh right velocity is for spawns so gets silly when using it to move...
  [#^Vector2D position #^Vector2D velocity #^Vector2D accel ;needs to be uniform yeah...
   #^int size #^int life #^double spread #^int emission-rate ^clojure.lang.Keyword id])

(defrecord Field [#^Vector2D position #^int size #^double mass]) ;prob give vel and acc too for good measure? and could have like "type" to only affect certain particles

(extend-protocol Emission
  ParticleEmitter
  (emit [this]
    (Particle. (:position this)
               (utils/mk-vec (utils/mag (:velocity this))
                             (+ (utils/angle (:velocity this))
                                (- (:spread this)
                                   (* (rand) (:spread this) 2.0))))
               (Vector2D. 0.0 0.0)
               0 (:life this) (:id this)))
  Particle ;support particles asploding, shedding, whatever. tho hmm can only return 1 tho. so particle would emit short-lived particleemiter in turn emitting the particles?
  (emit [this]
    (ParticleEmitter. (:position this) (:velocity this) (:accel this)
                      1 10 Math/PI 10 :explode)))

(defonce scaler (atom 1))
(defn dump []
(def sine (param/quick-lfo "sine" :low 0.02 :high 2.5 :beats 27)) ;see this is where mutability could come handy
(def saw (param/quick-lfo "sawtooth" :low 0.09 :high 1.9 :beats 9)) ;see this is where mutability could come handy
;just put something somewhere then continue to modify the def. but yeah wrap and resolve still works blabla
(param/auto-resolve sine)
(param/auto-resolve saw sine)
(param/multiply-normalized (map #(param/quick-lfo %) ["sine" "saw"]))
;; (param/multiply-normalized (param/quick-lfo "sine") (param/quick-lfo "sawtooth"))
(reset! scaler (param/mix [sine saw] param/sum :mode :loop :min 0.09 :max 3.5))
(reset! scaler (param/mix [sine (param/quick-lfo "sawtooth" :beats 13)]
                          param/avg :mode :loop :min 0.12 :max 2.5))
   ;; (move-fn this (+ 0.09 (param/auto-resolve (param/rng :min 0.01 :max 5)))))
   ;; (move-fn this 0.2)) ;cool glitches 0.1-0.5
   ;; (move-fn this (:x (:accel this)))) ;nice glitchy
)

(defn move-fn ;couldve just done lookup with one arity heh
  ([this]
   ;; (move-fn this (param/auto-resolve @scaler) ))
   (move-fn this (param/auto-resolve sine)))
  ([this scale]
   (let [vel (utils/add (:velocity this) (:accel this))
         pos (utils/add (:position this) (utils/div (:velocity this) scale))]
     (assoc this :velocity vel :position pos))))

(extend-protocol Kinematics
  Particle
  (move [this] (move-fn this))
  ParticleEmitter ;hmm so particle just assocs to self but PE recreates?
  (move
    ;; ([this] (move-fn this (:size this)))
    ([this] (move-fn this 0.3))
    ([this p] (assoc this :position p)) ;ah so this just "recreate at new pos"
    ;; (ParticleEmitter. p (:velocity this) (:size this)
    ;;                   (:life this) (:spread this) (:emission-rate this)))
  ;; Field
  ;; (move [this] (Field. ))
  ))

(defn influence-fn
  [us them]
  (let [{:keys [x y]} (utils/sub (:position them) (:position us)) ;distance...
          field-mass (:mass them)
          our-mass (or (:mass us) (:size us) 1)
          old-accel (:accel us)
          force (/ field-mass
                   (Math/pow (+ (* x x) (/ field-mass 2.0)
                                (* y y) (/ field-mass 2.0))
                    1.5)
                   (* 5 our-mass))]
                   ;; (/ our-mass 1))]
      (assoc us :accel (Vector2D. (+ (:x old-accel) (* x force)) ;wait more force further aray?
                                  (+ (:y old-accel) (* y force))))))

(extend-protocol FieldForce
  ;; Particle
  ;; (influence [p f] (influence-fn p f))
  ;; ParticleEmitter
  ;; (influence [pe f] (influence-fn pe f))
  Field
  (influence [f p] (influence p f))
  Object
  (influence [this them] (influence-fn this them))) ;right can apply force from either side sorta...
; also, f f would be cool...


(defn mk-particle-emitter [#^Vector2D position #^Vector2D velocity]
  (ParticleEmitter. position velocity (Vector2D. 0.0 0.0) 8 -1 (/ Math/PI 32) 4 :generic))

(defn build-particle-emitter
  [#^Vector2D position #^Vector2D velocity &
  {:keys [size life spread emission-rate id]
  :or {size 8 life -1 spread (/ 3.1415 32) emission-rate 4 id :generic}}]
  (ParticleEmitter. position velocity (Vector2D. 0.0 0.0)
                    size life spread emission-rate id))
;; (defn build-particle-emitter [#^Vector2D position #^Vector2D velocity]
;;   (ParticleEmitter. position velocity (Vector2D. 0.0 0.0) 8 -1 (/ Math/PI 32) 4) :generic)
