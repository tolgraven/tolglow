(ns tolglow.vars "Show variables, cue var generation and varmap manipulation"
  (:require [afterglow
             [midi :as midi :refer [sync-to-midi-clock]]
             [show :as show :refer [add-effect! all-fixtures end-effect! fixtures-named get-variable patch-fixture! set-cue! set-variable! start! stop! sync-to-external-clock]]]
            [clojure.string :as string :refer [capitalize upper-case]]
            [com.evocomputing.colors :as colors :refer [adjust-hue color-name create-color darken desaturate hue lighten lightness saturate saturation]]
            [tolglow
             [color :as color :refer []]
             [config :as config :refer [cfg param-data ptr-cfg var-data]]
             [util :as util :refer []]]))

(defn type-for-vm
 [v]
 (condp = (type v)
  Boolean        :boolean
  Long           :integer
  ::colors/color :color
  nil)) ;nil infers :double

(defn key-to-name
 [k]
 (-> (name k) string/capitalize (string/replace "-" " ")))

;XXX introduce additional key like :unprotected, if true opens var to "non-action" modification, eg randomizer functions
; alt name... from-vec? create? make?
(defn cue-map "Create variable for cue from minimum required information"
([[var-key var-start var-min var-max] & {:keys [var-type var-name velocity vel-min vel-max centered resolution]}] ;put start before min-max i guess so can skip them for bools
 (let [var-start (cond (color/color? var-key) var-key ;just starting color obj for color var
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
  (into {} (filter (fn [[_ v]] some? v) segments))))) ;nuke any nil vals

(def template (into {} (for [[k v] var-data] {k (cue-map v)})))


(defn cue-maps [lots & settings]
 (map #(apply cue-map %1 settings) lots))

; per-var can merge in overloads for specific keys. so think eg grab general lfo stuff, adjust starting phase without making a var from scratch?
(defn auto "resolve loose keywords through use-vars, merge complete (vector) var-maps and single-map vars"
 [& more]
 (let [more (map
                 (fn [element]
                  (condp = (type element)
                    clojure.lang.Keyword (conj [] (element template))
                    clojure.lang.PersistentArrayMap (conj [] element)
                    ::colors/color (conj [] (cue-map [element]))
                    String (conj [] (cue-map [(color/like element)]))
                    element))
                 more)]
  (reduce into [] more)))

(defn alt-start
 [k start]
 (assoc (k template) :start start))

(defn prefixed "Add prefix to existing map, so can grab eg (auto :bars) and run pan-bars + tilt-bars"
 [prefix maps]
 (mapv (fn [m]
        (let [with-k (update m :key #(str (name prefix) "-" %))]
         (assoc with-k :name (key-to-name (:key with-k)))))
       maps))

(defn prefixed-lfos "Get cue-vars for multiple params. Takes a map like {:pan [64 191 4] :tilt [0 127 3]} where vector is starting value for min, max and interval (default :bars) respectively" ;more cfg?
 [m & {:keys [interval phase? scale]}] ;have scale? bounds overrides? or strictly from cfg?
 (let [interval (or interval :bars)
       targets [:min :max]]
  (flatten (for [[pre defs] m]
  (let [data (ptr-cfg :vars pre)]
   (prefixed pre (auto (map #(cue-map (flatten [%1 %2 (map data targets)]))
                            (map name targets) defs)
                       (alt-start interval (last defs))
                       (when phase? :phase))))))))

(defn grouped
 [prefixes m]) ;XXX overrides for diff groups

(defn interval
 [beats-start & cycles-start]
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


(defn init! "ensure any show var we'll be trying to bind to actually has a value set"
 [key default & force] ;[key & default]
 (let [default (or default 0)  ;default default is Number
       unset (apply not= (map type [(show/get-variable key) default]))
       force-reset (or (first force) false)]
  (when (or unset force-reset)
   (set-variable! key default)
   #_(println key default)))) ;and something something bind to it so can resolve but looks like hmm

(defn update! "Update show variable using fn"
 [key update-fn]
 (set-variable! key (update-fn (get-variable key))))

(defn get-show "Get show var, assembling its keyword for ya"
 [& key-parts]
 (show/get-variable (apply util/key-str key-parts)))
