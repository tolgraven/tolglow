(ns tolglow.param "Param builders"
  (:require [afterglow
             [rhythm :as rhythm :refer [metro-snapshot metro-start metronome]]
             [show :as show :refer [add-effect! all-fixtures end-effect! fixtures-named get-variable patch-fixture! set-cue! set-variable! start! stop! sync-to-external-clock]]
             [show-context :refer [*show* set-default-show! with-show]]
             [transform :as tf :refer [degrees]]]
            [afterglow.effects
             [oscillators :as lfo :refer [build-oscillated-param sawtooth sine square triangle]]
             [params :as params :refer [bind-keyword-param build-aim-param build-color-param build-direction-param-from-pan-tilt build-pan-tilt-param build-param-formula build-spatial-param build-step-param frame-dynamic-param? param? resolve-param validate-param-type]]]
            [clojure.math.numeric-tower :as math]
            [clojure.string :as string :refer [capitalize upper-case]]
            [com.evocomputing.colors :as colors :refer [adjust-hue color-name create-color darken desaturate hue lighten lightness saturate saturation]]
            [taoensso.timbre :as timbre]
            [tolglow
             [color :as color :refer []]
             [config :as config :refer [at cfg]]
             [util :as util :refer [clamp-number get-map-with key-str value]]
             [debug :as debug :refer [det]]
             [vars :as vars]])
  (:import afterglow.rhythm.Metronome
           [afterglow.effects.params IParam Param]))


(def types (for [m config/param-data]
                (update m :variables (partial apply vars/auto))))

(defn bind-keys "Bind keys to default values, inferring type from default, and returning default for nil keys"
 [& pairs]
 (doall
  (for [[k v] (partition 2 pairs)]
   (let [kind (cond (number? v) Number
                    (instance? Boolean v) Boolean
                    (color/color? v) ::colors/color)
         k (or k v)] ;put val as key when key nil (like when missing from vm) -> still get param (unless nil? v)
    (bind-keyword-param k kind v)))))

(defn prefix-set
 [prefix & ks]
 (let [#_colltype #_(possible look and return same if drop &?)]
  ;; (doall (map #(key-str (when prefix (str prefix "-")) %) ks))))
  (set (apply map #(key-str (when prefix (str prefix "-")) %) ks))))

(defn dig-in-map ;XXX filter inside instead so cant end up with more than anticipated args (if map somehow has :beats AND :bars, presumably for different things?)
 [m ks]
 (->> (for [k ks]
        (if (keyword? k) (m k) (dig-in-map m k)))
      (filter some?)
      flatten))

(defn extract "Get relevant params, by keywords, from map" ;return type irrel, move to vars or util?
 [kind m & {:keys [prefix]}]
 (let [ks (condp = kind
           :ratio [(config/values-data :time-unit)
                   (config/values-data :time-div)]
           :fraction [:width]
           ; fallback search below...
           (let [pattern (re-pattern (str (util/ensure-is :string kind) "-\\w+"))] ;XXX fix pattern so matches eg :pan-min-fart as well...
            (filter #(re-matches pattern (util/ensure-is :string %))
                    (keys m))))]
  (dig-in-map m ks))) ;XXX end up wrong order -max -min because alphabet. Specify fallback order stuff dunno?

(defn atoms "Make atoms. Put in vars?"
 [& types]
 (map atom types))

(defn param-specs "move to config hey"
 []
 {:metronome {:type Metronome :default (:metronome *show*)}
  :measure {:type ::tf/distance-measure, :default (tf/build-distance-measure 0 0 0)}
  :color {:type ::colors/color, :default (color/create :white)}
  :bool {:type Boolean :default false}
  :number {:type Number :default 0} })

;; :herebestep {:type ::make-a-type-yo :default (build-step-param)}
;; :herebelfo {:type :saem :default (sine)}
;; ^ i mean no point as already got other fns to handle them but unified/having types would be better no?

(defn bind-default "Bind param by default kind and value"
 [type-key symbol] ;only for quickies
 (bind-keyword-param symbol (-> param-specs type-key :type) (-> param-specs type-key :default)))
;;  (apply bind-keyword-param symbol (map #(-> param-specs type-key %) [:type :default])))

(defn bind-vars "Bind args to keyword params, ensuring nil never results"
 [m]
 {:pre [(map? m)]}
 (let [f (fn [mp [k _]]
          (assoc mp k
                 (let [default (util/default m k)
                       v (or (util/arg m k) default) ;bind-keyword-param dumb and nil as first arg resolves to nil, not default, so...
                       kind (condp = (type default)
                             Long Number, Double Number
                             ;maybe need vector -> java.util.List per pinstripes?
                             (type default))]
                  (if (params/param? v) ;; (if (instance? IParam v) ;temp prob, as step-param bugs out...
                   v
                   (bind-keyword-param v kind default (name k))))))]
  (reduce f {} m)))

(defn auto-resolve "Resolve param to *show* and now"
 [p-or-m & {:keys [target-key dynamic show snapshot head] ;target key to only resolve specific
            :or {dynamic true, show *show*, snapshot (metro-snapshot (:metronome *show*))}}]
 (let [resolve-fn (if dynamic params/resolve-param params/resolve-unless-frame-dynamic)]
  (if-not (map? p-or-m) ;also support vectors tho (then receiving vector back, not map)
   (resolve-fn p-or-m show snapshot)
   (let [f (fn [m [k v]] (assoc m k (resolve-fn v show snapshot head)))]
    (reduce f {} p-or-m)))))

(defn assemble "Assemble params from map of arguments sent to Effect. creator from those passed, and defaults, bound to keywords ready for cue-var. Also resolves non-dynamic params. Returns pm (param-map) for usage in function"
 [args arg-spec & {:keys [resolve-vars] :or {resolve-vars false}}] ;XXX make compat diff lengths args/arg-spec? like could have a fallback with lots of stuff used for multiple things
 (let [[vars opts] (map #(util/ks-show-ks-defaults args %)
                        (map (arg-spec) [:vars :opts]))] ;align fallback map
  (merge (let [vars (bind-vars vars)]
          (if resolve-vars
           (auto-resolve vars :dynamic false)
           vars)) ;theoretically should work fine as avoids preemptively resolving what shouldnt. but dunno
         (auto-resolve (bind-vars opts)))
  #_OR
  ;; (apply merge (map #(auto-resolve (bind-vars %1) :dynamic %2) [vars opts] [false true]))
  #_(apply merge (map #(auto-resolve (bind-vars %) :dynamic false) [:vars :opts])) ;regardless of final spec, maybe best just run all early? if not dynamic won't change anyways...
  ))

;; ; does below make sense? build-param-formula already resolves for us, wouldn't this lock vals?
;; (defn number-formula "Build param formula with Number return type, resolve incoming nils to 0 unless specified"
;;  [f & params]
;;  (let [resolved (map #(or (params/resolve-param % *show* (metro-snapshot (:metronome *show*)))
;;                           0)
;;                    params)]
;;   (apply build-param-formula Number f resolved)))
;;   XXX makes more sense defer res nil to 0 at calc time no?
(defn number-formula "Build param formula with Number return type, resolve incoming nils to 0 unless specified"
 [f & params]
 (let [resolved (map #(or (params/resolve-param % *show* (metro-snapshot (:metronome *show*)))
                          0)
                   params)]
  (apply build-param-formula Number f resolved)))


(defn ratio "Create dynamic param setting the beat ratio for LFO-containing cues. Expects var-map to contain keys `:beats` and `:cycles`, defaulting to 1 if missing."; {{{
  [vm & {:keys [prefix]}] ;XXX should be a display overlay (add-control-held-feedback-overlay) touching "beats" brings up cycles...
  ; AND rows of 12345678 beats, 12345678 cycles, mult, div, maybe presets? instant 8/3 etc
  ;; (util/dlet [candidates (set (prefix-ks prefix (config/values-data :time-unit)))
  (let [candidates
        #_(set (map #(key-str (when prefix (str prefix "-")) %) (config/values-data :time-unit)))
              (prefix-set prefix (config/values-data :time-unit))
        k (some candidates (flatten (seq vm)))
        [ticks cycles mul div] (bind-keys (get vm k) 4, (:cycles vm ((prefix-set prefix #{:cycles}) vm)) 1
                                          :lfo-metro-mul 1, :lfo-metro-div 1)
        f (fn [ticks cycles mul div] (* mul (/ ticks cycles div)))] ;XXX clamp all four params to 1,2,3,4,6,8 etc
    (build-param-formula Number f ticks cycles mul div)))

;; (prefix-set "pan" #{ :bar :quo })
;; (ratio (assoc (starting-vm-for "square") :pan-beats 4) :prefix "pan")


(defn fraction "Create dynamic param representing a fraction as either 0-1 or 1/divisor with optional offset"
  ;; [vm & {:keys [target offset prefix]}]
  [vm target & {:keys [offset prefix]}]
  (if-let [target-key (or target (some (prefix-set prefix (config/values-data :time-unit)) (flatten (seq vm))))] ;no idea continuing if we don't get something here...
   (let [target (target-key vm)
         offset-key (key-str target-key "-offset") #_(-> (name target-key) (str "-offset") keyword)
         offset-global (key-str offset-key "-global") #_(-> (name offset-key) (str "-global") keyword)
         offset-bind (or offset (offset-key vm) offset-global #_(get-variable offset-global)) ;manual offset can go in fn call
         [fraction-param offset-param] (bind-keys target 1, offset-bind 0)
         f (fn [fraction offset]
            (if (< 1 fraction)
             (let [offset (or offset 0 #_(/ 1 fraction 8))] ;always leave a little glitch?
              (- (/ 1 fraction) offset))
             fraction))]
    (build-param-formula Number f fraction-param offset-param))
   1 #_(bind-keyword-param 1 Number 1))) ;eh why not just return 1??

(defn sum "Get total from values" [& vs] (apply + vs))
(defn sub "Subtract values from first" [& vs] (apply - vs))
(defn avg "Get average of values" [& vs] (/ (apply + vs) (count vs)))
(defn div "Divide value by later (presumably between 0.1-10-ish) ones" [& vs] (apply / (map #(util/clamp-number % 0.1 10) vs)))
(defn mul "Divide value by later (presumably between 0.1-10-ish) ones" [& vs] (apply * vs))

(defn extremes "Get lowest / highest result possible (assuming linear) from running f on all combinations of min/max input, summing for however many invocations. So can normalize ahead of time."
 ([f pairs] (extremes f (first pairs) (rest pairs)))
 ([f coll pairs]
  (let [coll (for [x (range (count coll))
                   y [first second]]
               (f ((vec coll) x) (y (first pairs))))]
   (if (next pairs)
    (extremes f coll (next pairs))
    (map #(apply % coll) [min max])))))

(defn mix "Mix params by applying f"
 [params f & {:keys [min max normalize? reflect? clip?]}] ;or single scale number showing whether 0.0-1.0 or 0-255...
 (when *show*
  (let [wrapped-fn (cond
                    normalize?
                    (let [candidates (repeat (count params) [min max])
                          [lo hi] (extremes f candidates)]
                      (fn [& params]
                       (util/scale-number (apply f params) min max :old-low lo :old-high hi)))
                    #_reflect?

                    (and min max)
                    (fn [& params]
                     (util/clamp-number (apply f params) min max))
                   :else f)]
   (apply build-param-formula Number wrapped-fn params))))
(def mix-dmx #(mix %1 %2 :min 0 :max 255))
(def mix-dmx-funky #(mix %1 %2 :min 0 :max 255 :scale? true))

(defn average "Average value from any number of input params"
 [& params] (mix params avg))

(defn any-lfo
 [chooser & {:keys [min max beats blend-shite] :or {min 0 max 255 beats 4}}]
 ;;  (let [lfos (build-oscillated-param % :min min :max max)]))
 (let [syms (mapv #(ns-resolve @(at :ns) (symbol "lfo" %)) ["sine" "square" "sawtooth" "triangle"])
       lfos (mapv #(build-oscillated-param (% :interval-ratio beats) :min min :max max) syms)
       f (fn [chooser lfos]
          (auto-resolve (lfos (clojure.core/min 3 (int chooser)))))] ; modulo fix so loops
  (build-param-formula Number f chooser lfos)))

; to smooth without keeping track of earlier values, just use rolling average as output? would also work for others
; but we still gotta count invocations to hmm wait... create a unique var to hold vector of last x values?
(defn noise "Random noise param, with options for smoothing (rolling average) and min/max volatility"
 [vm & {:keys [smoothing min-jump-fraction max-jump-fraction]}])
(defn jitter "Hold specified value, but keep it jiggly"
 [])
(defn drift "Drift from specified value, unsteadily, towards target"
 [])
(defn color-breathing "Color param, but moves around given color, within bounds, never staying static"
 [])
(defn trigger "Trigger some shit when reaching a specific range. Differs from whatever-param driving a chase by running actual code.
                     Meaning shit can evolve more better hopefully"
 [vm])
(defn quick-lfo "quick wrapper around lfo-param, to make lots"
 [])


(defn random-wrapper "Generates new random number (within range) each time wrapped square lfo hits max. Either lfo 1 frame wide, or have some flag get reset once reaching min..."
 [vm]
 (let [[min max] (map #(bind-keyword-param (%1 vm) Number %2) [:min :max] [0 255])
       [last-val already-triggered] (repeat 2 (ref nil))
       square (build-oscillated-param (lfo/square :interval-ratio (ratio vm)
                                                  :phase (:phase vm) :width 0.01)
                                      :min min :max max)
       f ()]
  (build-param-formula Number f square :last-random-wrap-test)))

(defn quick-lfo "quick wrapper around lfo-param, to make lots"
 [kind & {:keys [opts beats low high] :or {beats 4 low 0 high 255}}]
 (when *show* ;;  {:pre [(some? *show*)]} ;pre fucks repl start if got loose invocations around code we forgot to clean up. when much better...
  (let [lfo-sym (ns-resolve @(at :ns) (symbol "lfo" kind))
        lfo (apply lfo-sym :interval-ratio beats (flatten (seq opts)))]
  (build-oscillated-param lfo :min low :max high))))
;; (def tall (map quick-lfo ["sine" "square" "sawtooth" "triangle"]))
;; (def tallvg (apply average tall))
;; (def tsub (mix-dmx (map quick-lfo ["sine" "sawtooth"]) sub))

;;XXX a workflow of independently existing/created params (just running as sep cues I guess for now)
;; that are merely selected *from* the effect makes most sense?  synth-like...
;; then can have the same param swapping between different effects, like a synth
;; (melody/rhythm stays the same, sound changes)
;; or same effect and swapping "melody"
;; basically we need a list selector for the cue vars/interface...

(defn starting-vm-for "Lookup starting/default values for param-name, for visualizing before actually running. Only supports number values"
 [param-name]
 (into {} (map (fn [v] [(keyword (:key v)) (or (:start v) 0)])
               (:variables (util/get-map-for-param param-name types)))))

(defn lfo-raw
 [vm type]
 (let [lfo (ns-resolve @(cfg :ns :active) (symbol "lfo" type))]
   (apply lfo :interval-ratio (ratio vm) :phase (:phase vm 0.0)
          (condp = type
           "sawtooth" [:down? (:down vm)]
           "square"   [:width (fraction vm :width)]))))

(declare rng)
;;XXX !!! "router"/"glue" fn sits between cue/auto and effect-fn.
;; this maps the param values to destinations (first so can do beyond fixtures level
;; for complex effects. Eventually on-push map single param to several fx args,
;; at different scales/flips)
;; maybe group "Active param with associated control vars" as Generator?
;; Need easy way to nest and (re)point these
;; ([Control] -> Generator) -> Scaler -> [Router -> Scaler -> Endpoint]
;; where Endpoint can be another Generator Control, cue/effect-var, etc
;; Think the design through...



#_(defn auto-vm "Create parameter from var-map and name (looked up against types) for use with `cue/auto` for both the actual effect and its visualizer/color-fn"
 [vm param-name & {:keys [min max phase down width gain offset level smoothing noise min-change vm-prefix param-defs] :as all}] ;so can set these without them being present in var-map
 (let [vm (or vm (starting-vm-for param-name))
       p '[min max phase down width gain offset level min-change]
       ks (map keyword p)
       ;; pre #(if vm-prefix (str vm-prefix "-" %) %)
       ;; ks (map #(keyword (pre %)) p)
       ;; syms (map symbol p)
       [min max phase down width gain offset level min-change] (map #(% all (% vm)) ks)
       params [min max phase gain offset level min-change]
       [min max phase gain offset level min-change]
       (map #(if %1 (bind-keyword-param %1 Number %2) %2) params [0 255 0 1 0 0 0.1]) ;actually: grab the standard :start val...
       ;; (map #(bind-keyword-param %1 Number %2) params [0 255 0 1 0 0 0.1]) ;interesting that this doesnt work...

       ;; m (interleave ks (map vm ks))
       ;; m (interleave ks (for [[k default] [ks #_(map vm ks) [0 255 0 true 1 1 0 255 0.1]]]
       ;;                   #_(println k default) (vm k default)))
       ;; m (interleave ks (map #(%1 vm %2) ks [0 255 0 true 1 1 0 255 0.1]) ) ;XXX get defaults from var defs...
       [noise smoothing] (map #((vars/init! % 0.0)
                               (bind-keyword-param % Number 0.0)) ;utility fn for this, have map of globals and defaults duh
                              [:param-noise-all :param-smooth-all])  ;this one for all. best opt for smoothing?-basic
       ;; binds (zipmap ks (apply bind-keys m)) ;then look up below so like (get-with-prefix binds :min vm-prefix)
       ;; binds (into (zipmap ks (apply bind-keys m)) {:noise noise #_:smoothing #_smoothing}) ;then look up below so like (get-with-prefix binds :min vm-prefix)
       ;; b #(binds (pre %))

       lfo (resolve (symbol "afterglow.effects.oscillators" param-name)) ;max binding workaround...

       ; hmm gotta change eg (ratio) (fraction) to handle prefix no?
       raw (case param-name ;XXX param handling should be specified in param/types def
            ;; ("level" "held") (b :level)  ;straight level as "lfo", later incorporate ext control etc and rename function... ;level, just held
            ;; ("level" "held") ( :level)  ;straight level as "lfo", later incorporate ext control etc and rename function... ;level, just held
            ;;  "random" (rng :min (b :min) :max (b :max) :min-change (b :min-change))
            ;; (build-oscillated-param (apply lfo :interval-ratio (ratio vm) :phase (b :phase)
            ;;                          (cond (= param-name "sawtooth") [:down? (b :down)]
            ;;                                (= param-name "square")   [:width (fraction vm :width)]))
                                    ;; :min (b :min) :max (b :max)))
            ("level" "held") level #_(bind-keyword-param (:level vm level) Number 255) ;straight level as "lfo", later incorporate ext control etc and rename function... ;level, just held
             "random" (rng :min min :max max :min-change min-change)
            (if lfo
             (build-oscillated-param (apply lfo :interval-ratio (ratio vm) :phase phase
                                     (cond (= param-name "sawtooth") [:down? down]
                                           (= param-name "square")   [:width (fraction vm :width)]))
                                    :min min :max max)
             (println "Can't resolve" param-name "to function")))
       ;; raw-future (resolve-param raw *show* (metro-snapshot (:metronome *show*)))
       ;; final {:type param-name :param raw :min min :max max :gain gain :off offset :noise noise :vm vm}
       f (fn [param min max gain offset noise] ;;calculate actual resultant value, incorporating gain, offset and noise... XXX and any additional mixed-in params...
          (let [offset (* max offset)
                noise-base (* max (rand (or noise 0))) ;XXX main thing noise has got to be its own bound param we can just resolve and add in. w/ ctrl params for smoothing etc
                noise (if (< 0 noise-base) (- noise-base (/ (* max noise) 2)) 0)
                result (+ offset noise (* gain param))]
           (clamp-number result min max)))
       #_args #_(map b [:min :max :gain :offset :noise])] ;clamp final
  ;; (println raw f (map value args))
  #_(println param-name (map value [raw min max gain offset noise]))
  (build-param-formula Number f raw min max gain offset noise)))

(defn auto-vm "Create parameter from var-map and name (looked up against types) for use with `cue/auto` for both the actual effect and its visualizer/color-fn"
 [vm param-name & {:keys [min max phase down width gain offset level smoothing noise min-change vm-prefix param-defs] :as all}] ;so can set these without them being present in var-map

 (let [vm (or vm (starting-vm-for param-name))
       p '[min max phase down width gain offset level min-change]
       ks (map keyword p)
       [min max phase down width gain offset level min-change] (map #(or (% all) (% vm)) ks)
       params [min max phase gain offset level min-change]
       [min max phase gain offset level min-change]
       (map #(if %1 (bind-keyword-param %1 Number %2) %2) params [0 255 0 1 0 0 0.1]) ;actually: grab the standard :start val...
       [noise smoothing] (map (fn [k]
                               (vars/init! k 0.0)
                               (bind-keyword-param k Number 0.0)) ;utility fn for this, have map of globals and defaults duh
                               [:param-noise-all :param-smooth-all])  ;this one for all. best opt for smoothing?-basic
       lfo (resolve (symbol "afterglow.effects.oscillators" param-name)) ;max binding workaround...

       raw (case param-name ;XXX param handling should be specified in param/types def
            ("level" "held") (bind-keyword-param (:level vm level) Number 255) ;straight level as "lfo", later incorporate ext control etc and rename function... ;level, just held
             "random" (rng :min min :max max :min-change min-change :interval-ratio (ratio vm))
            (if lfo
             (build-oscillated-param (apply lfo :interval-ratio (ratio vm) :phase phase
                                     (cond (= param-name "sawtooth") [:down? down]
                                           (= param-name "square")   [:width (fraction vm :width)]))
                                    :min min :max max)
             (do (println "Can't resolve" param-name "to function:")
              (clojure.pprint/pprint vm))))

       f (fn [param min max gain offset noise] ;;calculate actual resultant value, incorporating gain, offset and noise... XXX and any additional mixed-in params...
          (let [offset (* max offset)
                noise-base (* max (rand (or noise 0))) ;XXX main thing noise has got to be its own bound param we can just resolve and add in. w/ ctrl params for smoothing etc
                noise (if (< 0 noise-base) (- noise-base (/ (* max noise) 2)) 0)
                result (+ offset noise (* gain param))]
           (clamp-number result min max)))
       #_args #_(map b [:min :max :gain :offset :noise])] ;clamp final
  ;; (println raw f (map value args))
  #_(println param-name (map value [raw min max gain offset noise]))
  (build-param-formula Number f raw min max gain offset noise)))


(defn auto-vm-dev "Create parameter from var-map and name (looked up against types) for use with `cue/auto` for both the actual effect and its visualizer/color-fn"
 [vm param-name & {:keys [ncvm like vm-prefix param-defs scale] :or {param-defs types} :as input}] ;so can set these without them being present in var-map
 (let [defaults {:vars (starting-vm-for param-name)}
       vm (or vm (:vars defaults))
       pm (assemble (merge vm ncvm input) defaults) ;so with :input we get all other potential passed crap but whats the harm really? will only get passed once and discarded right?
       ;btw should assemble also like put an :interval-ratio (ratio pm...) when finds right pieces? probably
       ;; pre #(if vm-prefix (str vm-prefix "-" %) %)
       [noise smooth] (map #(bind-keyword-param (vars/init! % 0.0) Number 0.0) [:param-noise-all :param-smooth-all]) ;utility fn for this, have map of globals and defaults duh
       param-f (eval (:fn (util/get-map-for-param param-name param-defs))) ;then fix (how?) so can simply apply. will likely need a (build-oscillated-param) wrapper/mod that can pass through and create lfo itself
       interval-ratio (apply ratio pm (when vm-prefix [:prefix vm-prefix])) ;for now

       raw (case param-name ;XXX param handling should be specified in param/types def
            ("level" "held") (:level pm) ;but prefix, duh... also bind dunno?
             "random" (apply param-f :interval-ratio interval-ratio (flatten (seq pm)))
             (apply build-oscillated-param
                    (apply param-f :interval-ratio interval-ratio :phase (:phase pm)
                           (cond (= param-name "sawtooth") [:down? (:down pm)]
                                 (= param-name "square")   [:width (apply fraction pm :width (when vm-prefix [:prefix vm-prefix]))]))
                    (flatten (seq pm)))) ;passing all but only min/max should actually get use no?
       f (fn [param min max gain offset #_noise] ;;calculate actual resultant value, incorporating gain, offset and noise... XXX and any additional mixed-in params...
          (let [[min max gain offset #_noise] (map #(or %1 %2) [min max gain offset #_noise] [0 255 1 0 #_0])
                offset (* max offset) ;tho should scale by max-max not curr...
                #_noise-base #_(* max (rand (or noise 0))) ;XXX main thing noise has got to be its own bound param we can just resolve and add in. w/ ctrl params for smoothing etc
                #_noise #_(if (< 0 noise-base) (- noise-base (/ (* max noise) 2)) 0)
                result (+ offset #_noise (* gain param))]
           (clamp-number result min max)))]
  (apply build-param-formula Number f raw (map pm [:min :max :gain :offset #_:noise]))))


(defn lfo-color-fn
 "Stripped down version. But only reasonable way I think is for all cues
  showing same thing (inactive) to share params and either way, should ease up if detecting high load?"
 [vm param-name & {:keys [min max phase down? width] :as all}] ;so can set these without them being present in vm
 (let [[min max phase] (map #(or (% all) (% vm)) [:min :max :phase])
       [min max phase] (map #(if %1 (bind-keyword-param %1 Number %2) %2) [min max phase] [0 255 0])
       lfo (ns-resolve @(cfg :ns :active) (symbol "lfo" param-name)) ;max binding workaround...
       raw (case param-name
             ("level" "held") (bind-keyword-param (:level vm) Number 255) ;straight level as "lfo", later incorporate ext control etc and rename function... ;level, just held
             ("random" rng :min min :max max :min-change (:min-change vm 0.1)) ;#_(build-param-formula Number #(* %1 %2 1/3) phase max)) ;phase filling in, fix proper later, +interval-ratio
             (build-oscillated-param (lfo :interval-ratio (ratio vm) :phase phase
                                          :down? (:down vm) :width (fraction vm :width)) ;maybe clear out the nil ones tho. but no real harm either way
                                     :min min :max max))]
  raw))

#_(defn #_defmacro def-base-lfo-color-fns
 [& lfo-names]
 (eval (cons 'do
       (map (fn [param-name]
           (let [lfo-sym (symbol (str "base-" param-name "-color-param"))
                 vm (:variables (util/get-map-with param-name types :by :type))]
             `(def ~lfo-sym ~(lfo-color-fn vm param-name))))
          lfo-names)))) ;defonce not actually necessary I guess since got all info from start
;and won't mod... or maybe should scale these with global metro scale tho...
;surely they should just be stuck in a map btw lol

;; (macroexpand-1 `(def-base-lfo-color-fns "square"))
;; (eval (def-base-lfo-color-fns "random"))
;; (value base-sine-color-param)
;; (value base-random-color-param)


(defn lfo-viz "Create visualizer for lfo-cues"
 [vm show param-name scale]
  (let [p (auto-vm vm param-name)] ;should only create it twice right. Probably in lfo-cue, bind to show var
   ;; could have :lfo entry just as now has :effect :color-fn etc. Inactive cues can often share same instances, just for color-fn
   ;; Then once var-map created, build own to be shared by effect/viz/color, and "pointed" towards varmap targets...
   ;; To change lfo just link new show var...
  (fn [snapshot] (/ (params/evaluate p show snapshot nil) scale))))

(defn lfo-chooser "run lfo params into a picker whoosing which one to use. SHOULD also allow mixing. XY thing with them in the corners?"; {{{
 [vm lfo-names & {:keys [picker-param [lfo-params]]}] ;picker could be in var-map as well...
 (let [[min max] (map #(bind-keyword-param (%1 vm) Number %2) [:min :max] [0 255])
       lfo-syms (map #(ns-resolve @(cfg :ns :active) (symbol "lfo" %)) lfo-names)
       [lfo-sine lfo-saw lfo-tri lfo-square]
       (map (fn [param-name]
             ;; (println (name param-name))
             (build-oscillated-param
                      ;; (param-name :interval-ratio (ratio var-map)
                      ;;           :phase (:phase var-map) :down? (:down var-map)
                      ;;           :width (fraction var-map :width)) ;maybe clear out the nil ones tho. but no real harm either way
                      (param-name :interval-ratio 4 :phase 0.5
                                :down? true
                                :width 0.25) ;maybe clear out the nil ones tho. but no real harm either way
                      :min 0 :max 255))
                 lfo-syms)
       picker (bind-keyword-param (or picker-param (:lfo-picker vm)) Number 0)
       ;; held-param  (bind-keyword-param (:level vm) Number 255) ;keep held in this somehow for quick modulation between lfos and M4L incoming - just merge it on

       ;; f (fn [picker & params]
       f (fn [picker lfo-sine lfo-saw lfo-tri lfo-square]
          (let [params [lfo-sine lfo-saw lfo-tri lfo-square]]
           (params picker)))]
 (build-param-formula Number f picker lfo-sine lfo-saw lfo-tri lfo-square)))
 ;; (apply build-param-formula Number f picker params)))


(defn variable "Create variable parameter from var-map"
 [vm]
 (params/build-variable-param (show/get-variable (:variable vm))))

;; (value (step nil))
(defn step "Create step parameter from var-map"
 [vm & {:keys [interval] :or {interval :beat}}]
 (build-step-param :interval interval :interval-ratio (ratio vm)
                   :fade-fraction (:fade vm 0.25) :fade-curve (if (:sine-curve vm) :sine :linear)))

(defn lfo-step "Create step parameter, from var-map, actual speed of which, as a fraction of set :interval-ratio, is run off LFO (would need high speed/fade-fraction no?)"
 [vm & {:keys [interval param-name] :or {interval :beat, param-name "sawtooth"}}]
  (build-step-param :interval interval :interval-ratio (auto-vm vm param-name)
                   :fade-fraction (:fade vm) :fade-curve (if (:sine-curve vm) :sine :linear)))

(defn lfo-offset-step "Create step parameter, from var-map, which actual position is offset by LFO mapped from 0-1 to +-0.5 (at least by default), making it rock ahead unsteadily"
 ;; seems easiest achieved by making :starting dynamic and shifting that to achieve proper offset?
 [vm & {:keys [interval param-name] :or {interval :beat, param-name "sawtooth"}}]
 (build-step-param :interval interval :interval-ratio (ratio vm)
                   :fade-fraction (:fade vm) :fade-curve (if (:sine-curve vm) :sine :linear)
                   :starting )); }}}

(defn multi-lfo "run multiple lfos (through oscillated-params) and combine them in various ways to generate a resultant end value"
;;  [lfo-first lfo-second phase & {:keys [htp? lowtp? average add subtract multiply]}]) ;divide would get hectic...
 [lfo-1 lfo-2 & {:keys [phase htp? lowtp? math-fn]}] ;average add subtract multiply]}]
 (let [f (fn [one two] (/ (+ one two) 2))]
  (build-param-formula Number f lfo-1 lfo-2))) ;divide would get hectic...

;XXX  modulating previous values properly. So a mod-level effect of 1.0
;followed by 0.5 no-htp doesn't set from 255 to 127, but half of earlier.
;Above-1.0 also possible to gain-up


;  RANDOM NUMBER PARAM

(defn any-dynamic? "Check whether any params in assembled map are frame-dynamic"
 [pm]
 ; dynamic-inputs? (#_apply some param? (map p [:min :max :min-change :interval]))
 ; ^^ but stuff can be param (bound/unresolved) without being dynamic tho, right? so further check needed,
 ; any existing in params?
 (some param? (vals pm)))

(defn get-range "Get absolute difference between max and min"
 [min max]
 (math/abs (- max min)))

(defn pick-new-value "Helper for random-params, pick new value with min difference (as fraction) from last value."
 [current min max min-change]
 (let [range (get-range min max)
       min-change (clamp-number min-change 0 0.33)] ;prob shouldn't be hardcoded here...
  (loop [candidate (+ min (rand range))]
   (if (or (nil? current) (>= (get-range current candidate) min-change))
     candidate
     (recur (+ min (rand range)))))))

(defn or-rng "Defaults for random number generator" []
 {:vars {:min 0 :max 255 :min-change 0.1 :interval :bar, :interval-ratio 1, :fade-fraction 0.25, :fade-curve :sine}})
(defn rng "Returns a dynamic number parameter which gets a new random value each interval. XXX fade to new value"
 [& {:keys [min max min-change interval interval-ratio fade-fraction fade-curve] :as args}] ;to skip or not skip listing possible keys...
 {:pre [(some? *show*)]}
 (let [pm (assemble args or-rng)
       step (apply build-step-param (flatten (seq pm)))
       [current-step last-value coming-value] (map ref (repeat nil))
       eval-fn (fn [show snapshot _]
                (let [pm (auto-resolve pm :show show :snapshot snapshot)
                      now (params/resolve-param step show snapshot)]
                 (dosync (when (not= (int now) @current-step) ;only update target when reaches new whole number
                          (ref-set current-step (int now))
                          (ref-set last-value (or @coming-value 0))
                          (apply alter coming-value pick-new-value (map pm [:min :max :min-change])))
                         (+ @last-value (* (- now @current-step) (- @coming-value @last-value)))))) ;return faded value
       presolve-fn (fn [show snapshot head]
                    (apply rng (flatten (seq (auto-resolve pm :dynamic false :show show :snapshot snapshot)))))]
  (Param. "RNG" true Number eval-fn presolve-fn)))

; XXX how look ahead to show graphically like an lfo? means different approach since same step
; in a particular step-param run
; should always resolve to same random nr. So we use what, a rolling list being filled and purged from back
; as we go? gotta cache, but also recompute future numbers as params change... good challenge!

(defn smoother "Returns a dynamic number parameter which smoothes the response of another one"
 ;; thinking mainly for smoothed squares and stuff. how best? or put straight in an osc?
 []
 (let []))
