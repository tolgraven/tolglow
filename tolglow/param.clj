(ns tolglow.param "Param builders"
  (:require [afterglow
             [rhythm :as rhythm :refer [metro-snapshot metro-start metronome]]
             [show :as show :refer [add-effect! all-fixtures end-effect! fixtures-named get-variable patch-fixture! set-cue! set-variable! start! stop! sync-to-external-clock]]
             [show-context :refer [*show* set-default-show! with-show]]
             [transform :as tf :refer [degrees]]]
            [afterglow.effects
             [oscillators :as lfo :refer [build-oscillated-param sawtooth sine square triangle]]
             [params :as params :refer [bind-keyword-param build-aim-param build-direction-param-from-pan-tilt build-pan-tilt-param build-param-formula build-spatial-param build-step-param frame-dynamic-param? param? resolve-param resolve-unless-frame-dynamic validate-param-type]]]
            [clojure.math.numeric-tower :as math]
            [clojure.string :as string :refer [capitalize upper-case]]
            [com.evocomputing.colors :as colors :refer [adjust-hue color-name create-color darken desaturate hue lighten lightness saturate saturation]]
            [thi.ng.color.core :as clr]
            [thi.ng.math.core :as cmath]
            [taoensso.timbre :as timbre]
            [taoensso.timbre.profiling :refer [pspy]]
            [tolglow.utils.iters :refer [mapv-fast mapcatv]]
            [tolglow
             [color :as color :refer []]
             [config :as config :refer [at cfg]]
             [util :as util :refer [clamp get-map-with key-str value]]
             [graph :as graph]
             [viz :as viz]
             [vars :as vars]]
            [afterglow.effects.cues :as cues])
  (:import afterglow.rhythm.Metronome
           [afterglow.effects.params IParam Param]))


(def types (for [m config/param-data]
                (update m :variables (partial apply vars/auto))))

;; XXX deprecated? does same thing as bind-vars but on pairs instead of map, so confusing...
(defn bind-keys "Bind keys to default values, inferring type from default, and returning default for nil keys"
 [& pairs]
 (doall ;; (doseq [[k v] (partition 2 pairs)] ;uh so how does for+doall work but doseq not when we're just calling a fn for side effects, not gathering data?
  (for [[k v] (partition 2 pairs)]
   (let [kind (cond (number? v) Number
                    (boolean? v) Boolean
                    (color/color? v) params/color-type #_::colors/color)
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
           ;; (let [pattern (re-pattern (str (util/ensure-is :string kind) "-\\w+"))] ;XXX fix pattern so matches eg :pan-min-fart as well...
           (let [pattern (re-pattern (str (name kind) "-\\w+"))] ;XXX fix pattern so matches eg :pan-min-fart as well...
            (filter #(re-matches pattern (name %)) (keys m))))]
  (dig-in-map m ks))) ;XXX end up wrong order -max -min because alphabet. Specify fallback order stuff dunno?
  ;; (apply sorted-map-by #(- (compare %1 %2)) (dig-in-map m ks)))) ;XXX end up wrong order -max -min because alphabet. Specify fallback order stuff dunno?
;; (sorted-map-by compare)

;; (defrecord ReturnType [^java.lang.Class kind
;;                        ^clojure.lang.Keyword kind-key
;;                        ;; ...
;;                        ]
;;  ) ;or something dunno. record to like implement multiple param
   ;representations on top of same type. Or a like-single-number view
;  ;for complelx types...
(defn param-specs "move to config hey"
 []
 {:metronome {:type Metronome :default (:metronome *show*)}
  :measure {:type ::tf/distance-measure, :default (tf/build-distance-measure 0 0 0)}
  :color {:type (color/used-type), :default (color/create :white)}
  :bool {:type Boolean :default false}
  :number {:type Number :default 0}
  :step {:type Number :default (build-step-param :interval :bar :fade-fraction 0.25 :fade-curve :sine)}
  :range {:type 'tolglow.vars.RangedNumber } ;something like this. tho ultimately it just returns a number I suppose... then again ultimately a color just returns three.
   ;it would work like, an effect or param could take either a min and max, or a range (combined min-max) for representing it
   ;could also be used *WITH* an actual settable value (not just lfo/auto types)
   ;to either form the original range, or clamp or rescale an existing param?
   ;the main (only?) place needed to be updated for it would be varmap/controller cue interface...
   :looping-number {} ;well not a param but for controller, vals that loop around if you keep spinning
  ;; :lfo {:type afterglow.effects.oscillators.Oscillator :default ()} ;like why not really? it'd just have to be a thing that it resolve itself twice.
  })

(defn bind-default "Bind param by default kind and value"
 [type-key show-key] ;only for quickies
 (let [data (-> param-specs type-key)]
  (bind-keyword-param show-key (:type data) (:default data))))


(defn bind-vars "Bind args to keyword params, ensuring nil never results"
 [m]
 {:pre [(map? m)]}
 (let [f (fn [pm [id _]]
          (assoc pm id
                 (let [default (util/default m id)
                       param (or (util/arg m id) default) ;bind-keyword-param dumb and nil as first arg resolves to nil, not default, so...
                       kind  (cond (number? default) Number
                                   (or (seq? default) (vector? default)) java.util.List
                                   :else (type default))]
                  (if (params/param? param)
                   param ;just return if already a param. tho that's what bind-keyword-param already does right so why not just pass through?
                   (bind-keyword-param param kind default (name id))))))]
  (reduce f {} m)))


;; (type [])
;; 100 ms extra for 10000 calls ok when resolve hundreds and.graphics lib taking seconds. but no more.
;; still dont get whats so slow tho...
(defn auto-resolve "Resolve param(s) to *show* and now. IS MAJOR BOTTLENECK now that param? is unfucked"
 [p & {:keys [dynamic show snapshot head] ;XXX really need to change name of dynamic param
         :or {dynamic true, }}]
         ;; :or {show *show*, snapshot (metro-snapshot (:metronome *show*))}}] ;oook interesting. :or with such a massive var as *show* slows down fn _EVEN_ when fallback isnt run.
 (let [show (or show *show*) ;that slowdown does NOT occur when (or ) in let and passing
       snapshot (or snapshot (rhythm/metro-snapshot (:metronome show)))]
  (pspy :auto-resolve-fn
        (let [resolve-fn (if dynamic params/resolve-param params/resolve-unless-frame-dynamic)]
  ;; XXX should auto-resolve also try to auto-bind keywords? hmm
  (cond (params/param? p) (resolve-fn p show snapshot head) ;shouldnt really be used for this but latency from checks def biggest for groups of individual calls so
        (and (map? p) (seq p) (not (record? p))) ;be careful just checking for map now that Param is record
        #_(map? p) (pspy :reduce-kv-resolvers
                         (reduce-kv (fn [pm id param] ;ohh right ONCE AGAIN remember records are maps so might go reducing for no reason
                          (assoc pm id (resolve-fn param show snapshot head)))
                         {} p)) ;ouff yeah this is not quick...
        ;; (vector? p) (mapv #(resolve-fn % show snapshot head) p) ;; (mapv #(auto-resolve % :dynamic dynamic :show show :snapshot snapshot :head head) p) ;throwing in nil head doesnt matter right cause thats what internal does?
        ;; (or (vector? p) (list? p)) (mapv #(resolve-fn % show snapshot head) p) ;; (mapv #(auto-resolve % :dynamic dynamic :show show :snapshot snapshot :head head) p) ;throwing in nil head doesnt matter right cause thats what internal does?
        (or (vector? p) (seq? p)) (mapv #(resolve-fn % show snapshot head) p) ;; (mapv #(auto-resolve % :dynamic dynamic :show show :snapshot snapshot :head head) p) ;throwing in nil head doesnt matter right cause thats what internal does?
        :else p)))))

(defn assemble "Assemble params from map of arguments sent to Effect. creator from those passed, and defaults, bound to keywords ready for cue-var. Also resolves non-dynamic params. Returns pm (param-map) for usage in function"
 [args arg-spec & {:keys [resolve-vars] :or {resolve-vars false}}] ;XXX make compat diff lengths args/arg-spec? like could have a fallback with lots of stuff used for multiple things
;;  [args arg-spec & {:keys [resolve-vars] :or {resolve-vars true}}] ;yeah seems fucks step params still? blabla keyword... byt manual auto-resolve + auto-resolve works fine...
 (let [arg-spec (cond (map? arg-spec) arg-spec
                      (ifn? arg-spec) (arg-spec)) ;could get spec as either a map or getter-fn (since defaults may be dynamic / not ready at app launch)
       [vars opts] (doall (mapv #(util/ks-show-ks-defaults args %)
                         (map arg-spec [:vars :opts])))] ;align fallback map
  (merge (let [vars (bind-vars vars)]
          (if resolve-vars ;why wouldnt we want to resolve non-dynamic stuff tho?? was it step param something back in the day?
           (auto-resolve vars :dynamic false)
           vars)) ;theoretically should work fine as avoids preemptively resolving what shouldnt. but dunno
         (auto-resolve (bind-vars opts)))))

(defn ratio "Create dynamic param setting the beat ratio for LFO-containing cues. Expects var-map to contain keys `:beats` and `:cycles`, defaulting to 1 if missing."; {{{
  [vm & {:keys [prefix]}] ;XXX should be a display overlay (add-control-held-feedback-overlay) touching "beats" brings up cycles...
  ; AND rows of 12345678 beats, 12345678 cycles, mult, div, maybe presets? instant 8/3 etc
  (let [candidates (prefix-set prefix (config/values-data :time-unit))]
        (if-let [k (some candidates (flatten (seq vm)))] ;XXX clamp all four params to 1,2,3,4,6,8 etc
         (let [[ticks cycles mul div]
                 (bind-keys (k vm) 4, (:cycles vm ((prefix-set prefix #{:cycles}) vm)) 1
                            :lfo-metro-mul 1, :lfo-metro-div 1)
                 f (fn [ticks cycles mul div] (* mul (/ ticks cycles div)))]
   (build-param-formula Number f ticks cycles mul div))
   #_1))) ;more reasonable else to return 1 or nil? fallback vs. unintended values...

(defn fraction "Create dynamic param representing a fraction as either 0-1 or 1/divisor with optional offset"
  [vm target & {:keys [offset prefix]}]
  (if-let [target-key (or target (some (prefix-set prefix (config/values-data :time-unit))
                                       (flatten (seq vm))))] ;no idea continuing if we don't get something here...
   (let [target (target-key vm)
         offset-key (key-str target-key "-offset")
         offset-global (key-str offset-key "-global")
         offset-bind (or offset (offset-key vm) offset-global) ;manual offset can go in fn call
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
(defn div "Divide value by later (presumably between 0.1-10-ish) ones" [& vs] (apply / (map #(util/clamp % 0.1 10) vs)))
(defn mul "Divide value by later (presumably between 0.1-10-ish) ones" [& vs] (apply * vs))

(defn extremes "Get lowest / highest result possible (assuming linear) from running f on all combinations of min/max input, summing for however many invocations. So can normalize ahead of time."
 ([f pairs] (extremes f (first pairs) (rest pairs))) ;this is what we actually call
 ([f coll pairs]
  (let [coll (for [x (range (count coll))
                   y [first second]] ;haha i remember being proud of this but wtf is first second
               (f ((vec coll) x) (y (first pairs))))]
   (if (next pairs)
    (extremes f coll (next pairs))
    (map #(apply % coll) [min max])))))
; ^uh how can memoize?

(defn mix "Mix params by applying f"
 [params f & {:keys [min max mode]; normalize loop reflect clip
              :or {mode :normalize}}] ;or single scale number showing whether 0.0-1.0 or 0-255...
 (when *show*
  (let [wrapped-fn (condp = mode #_cond ;ok wouldnt work here but cond-> and a pipe of transformations like this, nifty stuff
                    :normalize #_(= mode :normalize)
                    (let [candidates (repeat (count params) [min max]) ;ugh dont like shadowing but what else to call them
                          [lo hi] (extremes f candidates)]
                      (fn [& ps]
                       (util/scale-number (apply f ps) min max :old-low lo :old-high hi)))
                    :loop
                    (fn [& ps]
                     (mod (apply f ps) max))

                    :bounc
                    (fn [& ps]
                     (let [res (apply f ps)]
                      (- max (mod res max))))

                    (if (and min max)
                     (fn [& ps]
                      (util/clamp (apply f ps) min max))
                     f))]
   (apply build-param-formula Number wrapped-fn params))))
(def mix-dmx #(mix %1 %2 :min 0 :max 255))
(def mix-dmx-funky #(mix %1 %2 :min 0 :max 255 :normalize? true))

(defn average "Average value from any number of input params"
 [& params] (mix params avg))
(defn multiply-normalized [& params]
 (mix params mul))

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
 [vm & {:keys [param-to-transform smoothing jump-fraction time-volatility]}]
 (let [was (atom 0) ;keep track of previous
       eval-fn (fn [show snapshot head]
                )] ;however to generate.
  (Param. "Noise" true Number eval-fn (partial identity))))

(defn jitter "Hold specified value, but keep it jiggly" [])
(defn drift "Drift from specified value, unsteadily, towards target" [])
(defn color-breathing "Color param, but moves around given color, within bounds, never staying static" [])
(defn trigger "Trigger some shit when reaching a specific range. Differs from whatever-param driving a chase by running actual code.  Meaning shit can evolve more better hopefully" [vm])

(defn quick-lfo "quick wrapper around lfo-param, to make lots"
 [kind & {:keys [opts beats low high] :or {beats 4 low 0 high 255}}]
 (when *show* ;;  {:pre [(some? *show*)]} ;pre fucks repl start if got loose invocations around code we forgot to clean up. when much better...
  (let [lfo-sym (ns-resolve @(at :ns) (symbol "lfo" kind))
        lfo (apply lfo-sym :interval-ratio beats (flatten (seq opts)))]
  (build-oscillated-param lfo :min low :max high))))
;; (def sin (build-oscillated-param (lfo/sine) :min -1 :max 1))
;; (def qsin (quick-lfo "sine" :low ))
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

(defn lfo-raw "Get raw lfo (ie not param) from vm, a la param/auto"
 [vm kind]
 (let [lfo (ns-resolve @(at :ns) (symbol "lfo" kind))]
   (apply lfo :interval-ratio (ratio vm) :phase (:phase vm 0.0)
          (condp = kind
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
;; (vars/init! :param-noise-all 0.0)

(defn auto-vm "Create parameter from var-map and name (looked up against types) for use with `cue/auto` for both the actual effect and its visualizer/color-fn"
 [vm param-name & {:keys [min max phase down width gain offset level smoothing noise min-change fade-fraction vm-prefix param-defs] :as all}] ;so can set these without them being present in var-map
 (let [vm (or vm (starting-vm-for param-name))
;;  (debug/det [vm (or vm (starting-vm-for param-name))
       p '[min max phase down width gain offset level min-change fade-fraction]
       ks (map keyword p)
       [min max phase down width gain offset level min-change fade-fraction] (map #(or (% all) (% vm)) ks)
       params [min max phase gain offset level min-change fade-fraction]
       [min max phase gain offset level min-change fade-fraction]
       (map #(if %1 (bind-keyword-param %1 Number %2) %2) params [0 255 0 1 0 0 0.1 0.25]) ;actually: grab the standard :start val...
       [noise smoothing] (map (fn [k]
                               (vars/init! k 0.0)
                               (bind-keyword-param k Number 0.0)) ;utility fn for this, have map of globals and defaults duh
                              [:param-noise-all :param-smooth-all])  ;this one for all. best opt for smoothing?-basic
       lfo (resolve (symbol "afterglow.effects.oscillators" param-name)) ;max binding workaround...

       raw (case param-name ;XXX param handling should be specified in param/types def
            ("level" "held") (bind-keyword-param (:level vm level) Number 255) ;straight level as "lfo", later incorporate ext control etc and rename function... ;level, just held
             "random" (rng :min min :max max :min-change min-change :interval-ratio (ratio vm) :fade-fraction fade-fraction)
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
           (clamp result min max)))] ;clamp final
  (build-param-formula Number f raw min max gain offset noise)))


(defn auto-vm-dev "Create parameter from var-map and name (looked up against types) for use with `cue/auto` for both the actual effect and its visualizer/color-fn"
 [vm param-name & {:keys [ncvm like vm-prefix param-defs scale] :or {param-defs types} :as input}] ;so can set these without them being present in var-map
 (let [defaults {:vars (starting-vm-for param-name)}
       vm (or vm (:vars defaults))
       pm (assemble (merge vm ncvm input) defaults) ;so with :input we get all other potential passed crap but whats the harm really? will only get passed once and discarded right?
       ;btw should assemble also like put an :interval-ratio (ratio pm...) when finds right pieces? probably
       ;; pre #(if vm-prefix (str vm-prefix "-" %) %)
       [noise smooth] (doall (map #(bind-keyword-param (vars/init! % 0.0) Number 0.0) [:param-noise-all :param-smooth-all])) ;utility fn for this, have map of globals and defaults duh
       ;^ makes most sense if both of these are "param transformers" (that can take a param and apply themselves) so at end of (formula) we pipe through first smooth then noise...
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
           (clamp result min max)))]
  (apply build-param-formula Number f raw (map pm [:min :max :gain :offset #_:noise]))))
; XXX ^ use (formula) instead?

(defn lfo-color-fn
 "Stripped down version. But only reasonable way I think is for all cues
  showing same thing (inactive) to share params and either way, should ease up if detecting high load?"
 [vm param-name & {:keys [min max phase down? width] :as all}] ;so can set these without them being present in vm
 (let [[min max phase] (map #(or (% all) (% vm)) [:min :max :phase])
       [min max phase] (map #(if %1 (bind-keyword-param %1 Number %2) %2) [min max phase] [0 255 0])
       lfo (ns-resolve @(at :ns) (symbol "lfo" param-name)) ;max binding workaround...
       raw (case param-name
             ("level" "held") (bind-keyword-param (:level vm) Number 255) ;straight level as "lfo", later incorporate ext control etc and rename function... ;level, just held
             ("random" rng :min min :max max :min-change (:min-change vm 0.1)) ;#_(build-param-formula Number #(* %1 %2 1/3) phase max)) ;phase filling in, fix proper later, +interval-ratio
             (build-oscillated-param (lfo :interval-ratio (ratio vm) :phase phase
                                          :down? (:down vm) :width (fraction vm :width)) ;maybe clear out the nil ones tho. but no real harm either way
                                     :min min :max max))]
  raw))


(defn lfo-viz "Create visualizer for lfo-cues"
 [vm show param-name scale]
  (let [p (auto-vm-dev vm param-name)] ;should only create it twice right. Probably in lfo-cue, bind to show var
   ;; could have :lfo entry just as now has :effect :color-fn etc. Inactive cues can often share same instances, just for color-fn
   ;; Then once var-map created, build own to be shared by effect/viz/color, and "pointed" towards varmap targets...
   ;; To change lfo just link new show var...
  (fn [snapshot] (/ (params/resolve-param p show snapshot nil) scale))))

(defn lfo-chooser "run lfo params into a picker whoosing which one to use. SHOULD also allow mixing. XY thing with them in the corners?"; {{{
 [vm lfo-names & {:keys [picker-param lfo-params]}] ;picker could be in var-map as well...
 (let [[min max] (map #(bind-keyword-param (%1 vm) Number %2) [:min :max] [0 255])
       lfo-syms (map #(ns-resolve @(at :ns) (symbol "lfo" %)) lfo-names)
       lfos (map (fn [param-name]
             (build-oscillated-param
              (param-name :interval-ratio (ratio vm)
                          :phase (:phase vm) :down? (:down vm)
                          :width (fraction vm :width)) ;maybe clear out the nil ones tho. but no real harm either way
              ;; (param-name :interval-ratio 4 :phase 0.5
              ;;           :down? true :width 0.25) ;maybe clear out the nil ones tho. but no real harm either way
              :min min :max max))
                 lfo-syms)
       picker (bind-keyword-param (or picker-param (:lfo-picker vm)) Number 0)

       f (fn [picker & params]
          ((auto-resolve picker) params))] ;picker should resolve to number, used to index vector. needs checks obvs...
 (apply build-param-formula Number f picker lfos)))


(defn variable "Create variable parameter from var-map"
 [vm]
 (params/build-variable-param (show/get-variable (:variable vm))))

(defn step "Create step parameter from var-map"
 [vm & {:keys [interval fade-curve] :or {interval :beat fade-curve :sine}}]
 (build-step-param :interval interval :interval-ratio (ratio vm)
                   :fade-fraction (:fade vm 0.25) :fade-curve (:fade-curve vm fade-curve)))

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


(defn formula "Improved version?  Whenever the parameter's value is needed, it will evaluate all of the parameters passed as `input-params`, and call `calc-fn` with their current values, returning its result, which must have the type specified by `param-type`.
The compound dynamic parameter will be frame dynamic if any of its input parameters are."
  [param-type calc-fn & input-params]
  (let [dyn (some frame-dynamic-param? input-params)
        eval-fn (fn [show snapshot head]
                  (apply calc-fn (map #(resolve-param % show snapshot head) input-params)))
        resolve-fn (fn [show snapshot head]
                     (apply build-param-formula param-type calc-fn
                            (map #(resolve-unless-frame-dynamic % show snapshot head) input-params)))]
    (Param. "Formula" dyn param-type eval-fn resolve-fn))) ;first, give sensible name based on input params...

;  RANDOM NUMBER PARAM

;; (defn any-dynamic? "Check whether any params in assembled map are frame-dynamic"
;;  [pm]
;;  ; dynamic-inputs? (#_apply some param? (map p [:min :max :min-change :interval]))
;;  ; ^^ but stuff can be param (bound/unresolved) without being dynamic tho, right? so further check needed,
;;  ; any existing in params?
;;  (some param? (vals pm))
;;  #_(not (not-any? params/frame-dynamic? (vals pm))))

(defn get-range "Get absolute difference between max and min"
 [min max]
 (math/abs (- max min)))

(defn pick-new-value "Helper for random-params, pick new value with min difference (as fraction) from last value."
 [current min max min-change]
 (let [range (get-range min max)
       min-change (clamp min-change 0 0.33)] ;prob shouldn't be hardcoded here...
  (loop [candidate (+ min (rand range))]
   (if (or (nil? current) (>= (get-range current candidate) min-change))
     candidate
     (recur (+ min (rand range))))))) ;why not just force a value outside no recur?

(defn or-rng "Defaults for random number generator" []
 {:vars {:min 0 :max 255 :min-change 0.1 :interval :beat, :interval-ratio 1, :fade-fraction 0.25, :fade-curve :sine}})
(defn rng "Returns a dynamic number parameter which gets a new random value each interval."
 [& {:keys [min max min-change interval interval-ratio fade-fraction fade-curve] :as args}] ;to skip or not skip listing possible keys...
 {:pre [(some? *show*)]}
 (let [pm (assemble args or-rng)
       step (apply build-step-param (flatten (seq pm)))
       [current-step last-value coming-value] (map ref (repeat nil)) ;why not atoms? also haha mapv got it stuck, i need to pay more attention...
       eval-fn (fn [show snapshot _]
                (let [pm (auto-resolve pm :show show :snapshot snapshot)
                      now (params/resolve-param step show snapshot)]
                 (dosync (when (not= (int now) @current-step) ;only update target when reaches new whole number
                          (ref-set current-step (int now))
                          (ref-set last-value (or @coming-value 0))
                          (apply alter coming-value pick-new-value
                                 (mapv pm [:min :max :min-change])))
                         (+ @last-value (* (- now @current-step)
                                           (- @coming-value @last-value)))))) ;return faded value
        resolve-fn (fn [show snapshot head] ;auto resolve has quite high overhead. more relevant now that params are ten+ times faster. Fix
                    (apply rng (flatten (seq (auto-resolve pm :dynamic false :show show :snapshot snapshot :head head)))))]
  (Param. "RNG" true Number eval-fn resolve-fn)))

; XXX how look ahead to show graphically like an lfo? means different approach since same step
; in a particular step-param run
; should always resolve to same random nr. So we use what, a rolling list being filled and purged from back
; as we go? gotta cache, but also recompute future numbers as params change... good challenge!

(defn smoother "Returns a dynamic number parameter which smoothes the response of another one"
 ;; thinking mainly for smoothed squares and stuff. how best? or put straight in an osc?
 []
 (let []))

(defn build-color-param
  "Returns a dynamic color parameter. If supplied, `:color` is passed
  to [[interpret-color]] to establish the base color to which other
  arguments are applied. The default base color is black, in the form
  of all zero values for `r`, `g`, `b`, `h`, `s`, and `l`. To this
  base it will then assign values passed in for individual color
  parameters.

  All incoming parameter values may be literal or dynamic, and may be
  keywords, which will be dynamically bound to variables
  in [[*show*]].

  Not all parameter combinations make sense, of course: you will
  probably want to stick with either some of `:h`, `:s`, and `:l`, or
  some of `:r`, `:g`, and `:b`. If values from both are supplied, the
  `:r`, `:g`, and/or `:b` assignments will occur first, then then any
  `:h`, `:s`, and `:l` assignments will be applied to the resulting
  color.

  Finally, if any adjustment values have been supplied for hue,
  saturation or lightness, they will be added to the corresponding
  values (rotating around the hue circle, clamped to the legal range
  for the others).

  If you do not specify an explicit value for `:frame-dynamic`, this
  color parameter will be frame dynamic if it has any incoming
  parameters which themselves are."
  [& {:keys [color r g b h s l adjust-hue adjust-saturation adjust-lightness frame-dynamic]
      :or {color params/default-color frame-dynamic :default}}]
  {:pre [(some? *show*)]}
  (let [c (bind-keyword-param (params/interpret-color color) params/color-type params/default-color "color")
        [r g b, h s l, ah as al :as params]
        (map #(bind-keyword-param % Number 0) [r g b, h s l, adjust-hue adjust-saturation adjust-lightness])
        clamp (fn [n] (when n (cmath/clamp n 0.0 1.0)))]
    (if (not-any? param? [c r g b h s l adjust-hue adjust-saturation adjust-lightness])
      ;; Optimize the degenerate case of all constant parameters
      (let [result-color (atom c)]
        (if (seq (filter identity [r g b]))
          (let [red (when r (clamp r))
                green (when g (clamp g))
                blue (when b (clamp b))]
           (swap! result-color #(clr/as-hsla (clr/rgba (or red (clr/red %))
                                                       (or green (clr/green %))
                                                       (or blue (clr/blue %))
                                                       (clr/alpha %))))))
        (if (seq (filter identity [h s l]))
          (let [hue (when h (clamp (double h)))
                saturation (when s (clamp (double s)))
                lightness (when l (clamp (double l)))]
           (swap! result-color #(clr/hsla (or hue (clr/hue %))
                                          (or saturation (clr/saturation %))
                                          (or lightness (clr/luminance %))))))
        (when adjust-hue
          (swap! result-color #(clr/rotate-hue % (double adjust-hue))))
        (when adjust-saturation
          (swap! result-color #(clr/adjust-saturation % (double adjust-saturation))))
        (when adjust-lightness
          (swap! result-color #(clr/adjust-luminance % (double adjust-lightness))))
        @result-color)
      ;; Handle the general case of some dynamic parameters
      (let [dyn (if (= :default frame-dynamic)
                  ;; Default means incoming args control how dynamic we should be
                  (boolean (some frame-dynamic-param?
                                 [color r g b h s l adjust-hue adjust-saturation adjust-lightness]))
                  ;; We were given an explicit value for frame-dynamic
                  (boolean frame-dynamic))
            eval-fn (fn [show snapshot head]
                      (let [result-color (atom (resolve-param c show snapshot head))]
                        (if (seq (filter identity [r g b]))
                          (let [red (when r (clamp (resolve-param r show snapshot head)))
                                green (when g (clamp (resolve-param g show snapshot head)))
                                blue (when b (clamp (resolve-param b show snapshot head)))]
                           (swap! result-color #(clr/as-hsla (clr/rgba (or red (clr/red %))
                                                                  (or green (clr/green %))
                                                                  (or blue (clr/blue %))
                                                                  (clr/alpha %))))))
                        (if (seq (filter identity [h s l]))
                          (let [hue (when h (clamp (double (resolve-param h show snapshot head))))
                                saturation (when s (clamp
                                                    (double (resolve-param s show snapshot head))))
                                lightness (when l (clamp
                                                   (double (resolve-param l show snapshot head))))]
                           (swap! result-color #(clr/hsla (or hue (clr/hue %))
                                                          (or saturation (clr/saturation %))
                                                          (or lightness (clr/luminance %))))))
                                (when adjust-hue
                                  (swap! result-color
                                         #(clr/rotate-hue % (double (resolve-param adjust-hue show snapshot head)))))
                                (when adjust-saturation
                                  (swap! result-color #(clr/adjust-saturation % (double (resolve-param adjust-saturation
                                                                                                 show snapshot head)))))
                                (when adjust-lightness
                                  (swap! result-color
                                         #(clr/adjust-luminance % (double (resolve-param
                                                                     adjust-lightness show snapshot head)))))
                                @result-color))
            resolve-fn (fn [show snapshot head]
                         (with-show show
                           (build-color-param :color (resolve-unless-frame-dynamic c show snapshot head)
                                              :r (resolve-unless-frame-dynamic r show snapshot head)
                                              :g (resolve-unless-frame-dynamic g show snapshot head)
                                              :b (resolve-unless-frame-dynamic b show snapshot head)
                                              :h (resolve-unless-frame-dynamic h show snapshot head)
                                              :s (resolve-unless-frame-dynamic s show snapshot head)
                                              :l (resolve-unless-frame-dynamic l show snapshot head)
                                              :adjust-hue (resolve-unless-frame-dynamic adjust-hue show snapshot head)
                                              :adjust-saturation (resolve-unless-frame-dynamic
                                                                  adjust-saturation show snapshot head)
                                              :adjust-lightness (resolve-unless-frame-dynamic
                                                                 adjust-lightness show snapshot head)
                                              :frame-dynamic dyn)))]
       (Param. "Color" dyn params/color-type eval-fn resolve-fn)))))


; CHANGE:
; (Param) - DONE
; refactor / make smaller - WHO A
; no atoms - DONE
; fix bugs
; alpha as a first class member influencing any assignments
; optimize if possible - likely switch color lib? this one recreates on each adjustment
; (which given spatial stuff, bloom... can be lots of) running through whole dispatch thing
(defn color
  "Send a color (name, object...) and create a base param to which other
  arguments are applied. The default base color is black, in the form
  of all zero values for `r`, `g`, `b`, `h`, `s`, and `l`. To this
  base it will then assign values passed in for individual color
  parameters.

  All incoming parameter values may be literal or dynamic, and may be
  keywords, which will be dynamically bound to variables in [[*show*]]."
  [& {:keys [base-color r g b h s l a adjust-hue adjust-saturation adjust-lightness frame-dynamic]
      :or {base-color params/default-color frame-dynamic :default}}]
  {:pre [(some? *show*)]}
   (pspy :color-param
       (let [c (bind-keyword-param (params/interpret-color base-color) params/color-type params/default-color "color")
         [r g b, h s l, a ah as al :as params]
         (mapv-fast #(bind-keyword-param % Number 0) [r g b, h s l, a adjust-hue adjust-saturation adjust-lightness])
         clamp (fn [n] (when n (cmath/clamp n 0.0 1.0)))
         update-color (fn [current [run? f]] (if run? (f current) current))
         dyn (boolean (if (= :default frame-dynamic)
                       (some frame-dynamic-param? params) ;; Default means incoming args control how dynamic we should be
                       frame-dynamic)) ;; We were given an explicit val e for frame-dynamic
         eval-fn (fn [show snapshot head]
                  (pspy :color-param-eval
                 (reduce
                   update-color (resolve-param c show snapshot head)
                   [[(seq (filter some? [r g b]))
                     (fn [c]
                      (let [[r g b] (mapv-fast #(when % (clamp (resolve-param % show snapshot head))) [r g b])] ;slightly less optimal I guess cause nil vals get an extra fn call and check but eh
                       (clr/as-hsla (clr/rgba (or r (clr/red c)) (or g (clr/green c))
                                              (or b (clr/blue c)) (or a (clr/alpha c))))))]
                    [(seq (filter some? [h s l]))
                     (fn [c]
                      (let [[h s l] (mapv-fast #(when % (clamp (resolve-param % show snapshot head))) [h s l])]
                       (clr/hsla (or h (clr/hue c)) (or s (clr/saturation c))
                                 (or l (clr/luminance c)) (or a (clr/alpha c)))))]
                    [ah #(clr/rotate-hue % (resolve-param ah show snapshot head))]
                    [as #(clr/adjust-saturation % (resolve-param as show snapshot head))]
                    [al #(clr/adjust-luminance % (resolve-param al show snapshot head))]])))

         resolve-fn (fn [show snapshot head]
                     (let [[c r g b h s l a ah as al]
                           (mapv-fast #(resolve-unless-frame-dynamic % show snapshot head) (into [c] params))]
                      (color :base-color c :r r :g g :b b :h h :s s :l l :a a
                             :adjust-hue ah :adjust-saturation as :adjust-lightness al
                             :frame-dynamic dyn)))] ;shouldn't this just be built-in on creation?
  (cond
   (not-any? param? params) ;3x slower creation - how big deal?
   (eval-fn *show* (rhythm/metro-snapshot (:metronome *show*)) nil)
   (not-any? params/frame-dynamic-param? params)
   (resolve-fn (rhythm/metro-snapshot (:metronome *show*)) nil)
   :else
   (Param. "Color" dyn params/color-type eval-fn resolve-fn)))))
  ;; (if (not-any? param? params) ;3x slower creation - how big deal?
  ;;  (eval-fn *show* (rhythm/metro-snapshot (:metronome *show*)) nil)
  ;;  (Param. "Color" dyn params/color-type eval-fn resolve-fn)))))
;however slow going from 80-90 to  <40 loc is pretty nice


; add to Param. probably:
; whether has / potentially can be further optimized? then no need for reify/return self
; also maybe remove resolve-non-frame-dynamic? just always handle that on creation?
; - whether is spatial / varies per head -> rest can ignore that completely
;
;
(defprotocol Resolver "disappointing - 5x slower than auto-resolve on vec of 1000 arams..."
 (resolv [this]))
(extend-protocol Resolver
 afterglow.effects.params.Param
 (resolv [this] (afterglow.effects.params/resolve-param this *show* (afterglow.rhythm/metro-snapshot (:metronome *show*))))
 clojure.lang.PersistentArrayMap
 (resolv [this] (reduce-kv (fn [pm id param] ;ohh right ONCE AGAIN remember records are maps so might go reducing for no reason
                          (assoc pm id (afterglow.effects.params/resolve-param param *show* (afterglow.rhythm/metro-snapshot (:metronome *show*)) nil)))
                         {} this))
 clojure.lang.PersistentVector
 (resolv [this] (map #(afterglow.effects.params/resolve-param % *show* (afterglow.rhythm/metro-snapshot (:metronome *show*))) this))
 )

(defmacro times "how long?"
 [n f]
 `(time (doseq [i# (range ~n)]
              ~f)
        #_(doall (for [i# (range ~n)] ~f))))
;; arf

(defn defs []
 (def show *show*)
 (def snapshot (metro-snapshot (:metronome *show*)))
 (def lfo-param (quick-lfo "sine"))
 (def rand-color (color/create (rand) (rand) (rand)))
 [ (auto-resolve [nil nil nil nil nil 3 (color/like "blue") nil nil (quick-lfo "sine")])]

 (def color-lfo-param (color :base-color rand-color :adjust-hue (quick-lfo "sine")))
 (def color-crazy-param (color :base-color rand-color :adjust-hue (quick-lfo "sine") :adjust-lightness (quick-lfo "sawtooth") :s :fart))
 (def color-static-param (color :base-color rand-color :adjust-hue 7))
 (def color-lfo-param-reify (build-color-param :color rand-color :adjust-hue (quick-lfo "sine")))
 (def color-static-param-reify (build-color-param :color rand-color :adjust-hue 7))
 (util/avar :fart nil #_0.3)
 (def color-crazy-param-reify (build-color-param :color rand-color :adjust-hue (quick-lfo "sine") :adjust-lightness (quick-lfo "sawtooth") :s :fart))
 (def color-param-vec (vec (repeat 10000 color-lfo-param)))
 (def color-param-vec-big (vec (repeat 1000000 color-lfo-param)))
 (def color-lfo-param-unsolved (auto-resolve color-lfo-param :dynamic false))
 (def color-not-param (auto-resolve color-lfo-param))
 (def color-evo-static-param (params/build-color-param :color (colors/create-color :h (rand 360) :s (rand 100) :l (rand 100))))
 (def color-evo-lfo-param (params/build-color-param :color (colors/create-color :h (rand 360) :s (rand 100) :l (rand 100))
          :adjust-hue (quick-lfo "sine")))
 (def rgb [nil 0.7 0.0])
 (def hurf (params/bind-keyword-param :hurf Number 0))
 (def hurf-set (params/bind-keyword-param :hurf-set Number 0))
 (def metro (:metronome *show*))
 (def measure (tf/build-distance-measure 0 0 0 :ignore-z true))
 (util/avar :hurf-set (quick-lfo "sine"))
 (params/frame-dynamic-param? hurf)

 (defn get-timer [f] #(times 10000 (f %)))
 (def r-param (get-timer #(resolve-param % show snapshot))))

(defn runs []
 (defs)
 (type color-lfo-param)
 (times 10000 (resolve-param color-lfo-param show snapshot)) ;400 -> 20
 (times 1 (auto-resolve color-param-vec)) ; - so no penalty for vecs/maps then
 (times 1 (auto-resolve color-param-vec-big)) ;1745
 (times 1000 (resolv color-lfo-param)) ;45 -> 4 - so no penalty for vecs/maps then
 (times 1000 (auto-resolve color-lfo-param)) ;
 (times 1000 (resolve-unless-frame-dynamic color-lfo-param show snapshot)) ;140 -> 50 - way slower than eval but ok since not called as often...

 (times 1000 (if (param? color-not-param) (resolve-param color-not-param show snapshot) color-not-param)) ;44->1
 (times 10000 (resolve-param color-not-param show snapshot)) ;350 -> 2.7
 (times 10000 (resolve-param color-lfo-param show snapshot)) ; 21
 (times 10000 (param? color-not-param)) ;35 - 0.7 after fixing param?
 (times 10000 (param? metro)) ;35 - 0.7 after fixing param?
 (times 10000 (param? measure)) ;35 - 0.7 after fixing param?
 (times 10000 (map? {}))
 (times 10000 (param? color-lfo-param)) ;35 - 0.7 after fixing param?
 (times 10000 (param? color-evo-lfo-param)) ;4
 (times 10000 (param? color-evo-static-param)) ;240 -> 4 without satisfies

 (times 10000 (if (param? nil) (resolve-param nil show snapshot))) ;1.6
 (times 10000 (do (param? nil) (resolve-param nil show snapshot))) ;1.6
 (times 10000 (auto-resolve nil :show show :snapshot snapshot)) ;7.5
 (times 10000 (auto-resolve nil)) ;87

 (instance? Param nil)
 (instance? Param lfo-param)
 (instance? afterglow.effects.params.IParam color-evo-lfo-param)
 (times 10000 (instance? Param nil))
 (times 10000 (instance? Param (quick-lfo "sine"))) ;slow bc of _creation_
 (times 10000 (instance? Param lfo-param)) ;1.4
 (times 10000 (instance? afterglow.effects.params.IParam color-evo-lfo-param)) ;1.4
;;  (times 10000 (satisfies? afterglow.effects.params.Param color-evo-lfo-param)) ;1.4
 (times 10000 (instance? afterglow.effects.params.Param lfo-param)) ;1.4

 (times 10000 (seq (filter identity rgb)))
 (times 10000 (seq (filter some? rgb)))

 (map r-param [lfo-param]) ;number params, dynamic. 6
 (map r-param [hurf hurf-set]) ;fallback vs show var or lfo (3 vs 4-5ms)

 (map r-param [color-evo-lfo-param color-evo-static-param]) ;88 2
 (map r-param [color-lfo-param       color-crazy-param       color-static-param #_color-lfo-param-unsolved]) ;36 33 ish -> 23 0,7, mine proper slower...
 (map r-param [color-lfo-param-reify color-crazy-param-reify color-static-param-reify]) ;37 37 -> 16 0.7
 ; ^ reify/old color-param still quicker. but close


 (def fiz (clr/hsla (rand) (rand) (rand)))
 (times 10000 (clr/rotate-hue fiz Math/PI)) ;; 2.5
 (times 10000 (color :base-color rand-color :adjust-hue 7)) ;; 150ms
 (times 10000 (params/resolve-param (color :base-color rand-color :adjust-hue 7) show snapshot)) ;; 150ms
 (times 10000 (color :base-color rand-color :adjust-hue lfo-param)) ;; 70
 (times 10000 (params/resolve-param (color :base-color rand-color :adjust-hue lfo-param) show snapshot)) ;; 100
 (times 1000000 (clr/rotate-hue rand-color 7)) ;; 45ms. hmmm wtf
 ; ^ goddamn somethings off w my iml if creating dynamic then resolving takes 2/3 time of creating static??


 (times 10000 (build-color-param :base-color rand-color :adjust-hue 7)) ;; 45
 (times 10000 (build-color-param :base-color rand-color :adjust-hue lfo-param)) ;; 80
 (times 10000 (clr/rotate-hue (clr/hsla (rand) (rand) (rand)) Math/PI)) ;; 7.5

 (times 10000 (colors/mix-hsl (colors/adjust-hue (colors/create-color :h (rand 360) :s (rand 100) :l (rand 100)) 90)
                              (colors/create-color :h (rand 360) :s (rand 100) :l (rand 100)) 50.0)) ;320
 (times 10000 (map #(int (* 255 %))
                    @(clr/as-rgba (cmath/mix
                                   (clr/rotate-hue (clr/hsla (rand) (rand) (rand)) (/ Math/PI 2))
                                   (clr/hsla (rand) (rand) (rand)) 0.5)))) ;15 - 20x+ faster

 (times 10000 (colors/create-color :h (rand 360) :s (rand 100) :l (rand 100))) ;77
 (times 10000 (clr/hsla (rand) (rand) (rand))) ;3.5 - 20x faster
)
