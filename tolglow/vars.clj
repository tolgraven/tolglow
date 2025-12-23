(ns tolglow.vars "Show variables, cue var generation and varmap manipulation"
  (:require [afterglow
             [midi :as midi :refer [sync-to-midi-clock]]
             [show :as show :refer [add-effect! all-fixtures end-effect! fixtures-named get-variable patch-fixture! set-cue! set-variable! start! stop! sync-to-external-clock]]]
            [clojure.string :as string :refer [capitalize upper-case]]
            [com.evocomputing.colors :as colors :refer [adjust-hue color-name create-color darken desaturate hue lighten lightness saturate saturation]]
            [thi.ng.color.core :as clr]
            [tolglow
             [color :as color :refer []]
             [config :as config :refer [cfg param-data ptr-cfg var-data]]
             [util :as util :refer []]])

  (:import [clojure.lang Counted Indexed ILookup Seqable]
           [java.io Writer]))

;; uh guess some bs like this for many things (tho min-max better range...) but beat/cycles,
(deftype Pair [a b]
  Seqable (seq [_] (seq [a b]))

  Counted (count [_] 2)

  Indexed
  (nth [_ i]
    (case i
      0 a
      1 b
      (throw (IllegalArgumentException.))))
  (nth [this i _] (nth this i))

  ILookup
  (valAt [_ k _]
    (case k
      0 a
      1 b
      (throw (IllegalArgumentException.))))
  (valAt [this k] (.valAt this k nil)))

(defmethod print-method Pair
  [pair ^Writer w]
  (.write w "#Pair")
  (print-method (vec (seq pair)) w))

(defmethod print-dup Pair
  [pair w]
  (print-method pair w))



(defn type-for-vm
 [v]
 (condp = (type v)
  Boolean        :boolean
  Long           :integer
  thi.ng.color.core.HSLA :color ;btw a la aim-transformer etc, could have a "color transformer" param which only adds or subtracts its own values to some base color?
  ;; something.TwoNumbers :range ;or actually could also be p/t, x/y...
  ;; Vector2d already in use for params so why not that as backing hmm
  ;; unless moving to that clojure vec math lib prob good idea...
  ;; Vector3dorsmthn :xyz
  ;; something.TwoIntegers :fraction, ev returns :double. should be settable so only accepts eg 1-2-3-4-6-8, or 1-2-4-8, or 1-2-3 + 2nds/3rds = 1-2-4, 1-3-6...
  ;;
  nil)) ;nil infers :double


(defn key-to-name
 [k]
 (-> (name k) string/capitalize (string/replace "-" " ")))

;; (defprotocol ParsedCueVar "Doomed to try below this way since looking at other vars too... but surta what it's for"
;;  (var-start [x vkey])
;;  (var-key [x])
;;  (var-name [x vkey])
;;  (var-type [x vstart])
;;  (velocity [x]))
;; (extend-protocol ParsedCueVar
;;  String
;;  (var-start [s _] (color/like s))
;;  (var-key  [s] s)
;;  (var-name [s vkey] (or s (key-to-name vkey)))
;;  (var-type [s vstart] (or s (type-for-vm vstart)))
;;  ::color/color
;;  (var-start [c _] c)
;;  (var-key [c] "color")
;;  Boolean
;; _(var-start [b vkey] b))

;XXX introduce additional key like :unprotected, if true opens var to "non-action" modification, eg randomizer functions
; alt name... from-vec? create? make?
(defn cue-map "Create variable for cue from minimum required information"
([[var-key var-start var-min var-max] & {:keys [var-type var-name velocity vel-min vel-max centered resolution]}] ;put start before min-max i guess so can skip them for bools
 (let [var-start (cond (color/color? var-key) var-key ;why we putting color obj in key and not start anyways? just bc first not second
                       (string? var-start) (color/like var-start)
                       (instance? Boolean var-start) (if-not var-min var-start) ;bool and no var-min/max means is bool, else bool start means velocity, so nil. prob change so min/max but no start = velo...?
                       (nil? var-start) nil ;generally to be left none, due to show-var keyword var-key... ;(when (number? var-min) (+ var-min (rand (- var-max var-min))))
                       :else var-start)
       var-key (if (color/color? var-key) "color" var-key)
       var-name (or var-name (key-to-name var-key) #_(-> (name var-key) capitalize (string/replace "-" " ")))
       var-type (or var-type (type-for-vm var-start)) ;add, if type keyword try to look up and auto bind to show var?
       velocity (or velocity (when (and var-min (not var-start)) true))
       segments {:key var-key :name var-name :min var-min :max var-max :start var-start :type var-type
                 :velocity velocity :velocity-min vel-min :velocity-max vel-max
                 :centered centered :resolution resolution}]
  (into {}
        (filter
         (fn [[_ v]] some? v)
         segments))))) ;nuke any nil vals


(def template (into {} (for [[k v] var-data] {k (cue-map v)})))


(defn cue-maps [lots & settings]
 (map #(apply cue-map %1 settings) lots))


(defprotocol Color
 (createcolor [x]))

(extend-protocol Color
 String
 (createcolor [s] (color/create s)))

(deftype FunkyColor [args]
 Color
 (createcolor [_] (createcolor args)))


(defprotocol HandleCueVars
 (handle-input [x]))
(extend-protocol HandleCueVars
 clojure.lang.Keyword
 (handle-input [kw] (conj [] (kw template)))
 clojure.lang.PersistentArrayMap
 (handle-input [m] (conj [] m))
;;  :com.evocomputing.colors/color
 FunkyColor
 (handle-input [c] (conj [] (cue-map [c])))
 String
 (handle-input [s] (conj [] (cue-map [(color/like s)])))
 )
;; (def blu (FunkyColor. "blue"))
;; (util/value blu)
;; (type blu)

; per-var can merge in overloads for specific keys. so think eg grab general lfo stuff, adjust starting phase without making a var from scratch?
(defn auto "resolve loose keywords through use-vars, merge complete (vector) var-maps and single-map vars. XXX rename assemble?"
 [& more]
 (reduce into [] (map
                  (fn [element]
                   (condp = (type element)
                     clojure.lang.Keyword (conj [] (element template))
                     clojure.lang.PersistentArrayMap (conj [] element)
                     thi.ng.color.core.HSLA (conj [] (cue-map [element]))
                     String (conj [] (cue-map [(color/like element)]))
                     element))
                  more)))

(defn alt-start
 [k start]
 (assoc (k template) :start start))

(defn rescale
 [vm scale & {:keys [scale-target number-type relative-scaling? old-scale]
              :or {scale-target :max}}]
 (let [target (into {} (filter #(= (:key %) "max") vm))
       number-type (or number-type (:type target))
       other (remove #(= (:key %) "max") vm)
       fixed (assoc target scale-target scale)]
  (println target)
  (println other)
  (println fixed)
  (conj other target)))

(defn prefixed "Add prefix to existing map, so can grab eg (auto :bars) and run pan-bars + tilt-bars"
 [prefix maps]
 (mapv (fn [m]
        (let [with-k (update m :key #(str (name prefix) "-" %))]
         (assoc with-k :name (key-to-name (:key with-k)))))
       maps))

(defn prefixed-lfos "Get cue-vars for multiple params. Takes a map like {:pan [0 180 4] :tilt [-90 90 3]} where vector is starting value for min, max and interval (default :bars) respectively" ;more cfg?
 [m & {:keys [interval phase? scale] :or {interval :bars}}] ;have scale? bounds overrides? or strictly from cfg?
 (let [targets [:min :max]]
  (flatten (for [[pre defs] m]
  (let [data (ptr-cfg :vars pre)]
   (prefixed pre (auto (map #(cue-map (flatten [%1 %2 (map data targets)]))
                            (map name targets) defs)
                       (alt-start interval (last defs))
                       (when phase? :phase))))))))

(defn grouped
 [prefixes m]) ;XXX overrides for diff groups

(defn interval
 [beats-start & cycles-start] ;XXX period changable? nah
 (let [[beats cycles] (auto :beats :cycles)]
  (conj [(assoc beats :start beats-start)] cycles)))

(defn colors "Create color vars for cue var-map"
 [& names]
 (if (= (count names) 1)
  (cue-map ["color" (color/like (first names))])
  (doall (map-indexed #(cue-map [(str "color-" (+ %1 1)) (color/like %2)]) #_(fn [i s] (cue-map [(str "color-" i) s])) names))))

(defn min-max "Create pair of :min / :max cue-var maps"
 [min max & start-mods]
 (let [min-start (or (first start-mods) min)
       max-start (or (second start-mods) max)]
  (cue-maps [["min" min-start min max] ["max" max-start min max]])))
;; SHOW VARS
;; XXX regular should share min/max etc functionality with cue-vars, no?
;; so don't go craze

(defmacro init! "Ensure any show var we'll be trying to bind to actually has some value set. Check whether var is set before evaluating incoming value expression, since it might be an expensive param creation. Pass through key."
 [show-key value]
 `(if (type (show/get-variable ~show-key)) ~show-key ;if actually set dont bother. should maybe actually check type but eh
   (let [kind# (type ~show-key)
         value# (or ~value 0)]
     (set-variable! ~show-key value#)
     ;; (println "init: " ~show-key value#)
  ~show-key))) ;always return key so can chain

;; or
;; (defn init! "ensure any show var we'll be trying to bind to actually has a value set"
;;  [key default & force] ;[key & default]
;;  (let [default (or default 0)  ;default default is Number
;;        unset? (apply not= (map type [(show/get-variable key) default]))
;;        ;; value (show/get-variable key)
;;        force-reset (or (first force) false)]
;;   (when (or unset? force-reset)
;;    (set-variable! key default)
;;    (println "init: " key default))
;;   key)) ;always return key so can chain

(defn update! "Update show variable using fn"
 [key update-fn]
 (set-variable! key (update-fn (get-variable key))))

(defn get-show "Get show var, assembling its keyword for ya"
 [& key-parts]
 (show/get-variable (apply util/key-str key-parts)))
