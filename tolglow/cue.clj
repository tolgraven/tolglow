(ns tolglow.cue "Cue builders"
  (:require
   [afterglow
     [channels :as chan :refer [expand-heads extract-channels find-rgb-heads]]
     [effects :as fx :refer [chase scene]]
     [rhythm :as rhythm :refer [metro-snapshot metro-start metronome snapshot-bar-phase snapshot-beat-phase snapshot-beat-within-bar snapshot-down-beat?]]
     [show :as show :refer [add-effect! all-fixtures end-effect! fixtures-named get-variable set-cue! set-variable!]]
     [controllers :as ct]
     [show-context :refer [*show*]]]
    [afterglow.effects
     [channel :as chan-fx]
     [cues :as cues :refer [apply-merging-var-map code-cue color-fn-from-cue-var cue function-cue]]
     [dimmer :as dimmer :refer [dimmer-effect master-set-level]]
     [fun :as fun]
     [movement :as move]
     [oscillators :as lfo :refer [build-oscillated-param sawtooth sine square triangle]]
     [params :as params :refer [bind-keyword-param build-aim-param build-color-param build-direction-param-from-pan-tilt build-pan-tilt-param build-param-formula build-spatial-param build-step-param frame-dynamic-param? param? resolve-param validate-param-type]]
     [show-variable :as var-fx :refer []]]
    [clojure.string :as string :refer [capitalize upper-case]]
    [com.evocomputing.colors :as colors :refer [color-name]]
    [tolglow
     [color :as color :refer []]
     [config :as config :refer [at cfg ptr-cfg]]
     [fx :as tolfx]
     [param :as param :refer [lfo-viz]]
     [util :as util :refer [add-midi-callback apply-vm clamp get-channels get-map-with key-str random-in-range space-phase x-phase]]
     [vars :as vars]])
  (:import javax.media.j3d.Transform3D
           [javax.vecmath Point3d Vector3d]))


(defn group-end-keys "Helper function to produce a vector of effect keywords to end all effects running on light groups with a given suffix."
  [effect-suffix]
  (mapv #(keyword (str (name %) "-" effect-suffix)) (cfg :fixtures :groups)))

(defn group-parts "Helper function for common variables when creating a cue to run on either all, or a named group of lights."
 ;XXX onlt require one suffix and use for both, sep name-suffix set by :key?
  [group effect-suffix name-suffix & {:keys [end-rest? channel] :or {end-rest? true}}]
  ;; {:pre [()]}
  (let #_debug/det
    [effect-key #_(keyword (or (some string? (map #(when % (str (name %) "-" effect-suffix))
                                                   [group channel])) effect-suffix))
                   (or (when group   (key-str group "-" effect-suffix))
                       (when channel (key-str channel "-" effect-suffix))
                       (keyword effect-suffix))
        fixtures   (cond (keyword? group) (fixtures-named group)
                         (string? group) (fixtures-named (keyword group))
                         (vector? group) (reduce into [] (apply map fixtures-named group))
                         (not group) (all-fixtures))
        end-keys   (when end-rest?
                    (or (when group [(keyword effect-suffix)])
                        (group-end-keys effect-suffix)))
        fix #(capitalize (util/ensure-is :string %))
        dominant (or (when group (fix group))
                     (when channel (fix channel))
                     "All")
        effect-name (str dominant " " (capitalize name-suffix))]
    [effect-key fixtures effect-name end-keys])) ;changed order because end-keys often ignored

(defn wrap-effect "Wrap cue effect fn in tolfx/effect, for alpha control, passing through var-map to underlying fx"
 [f]
 (fn [vm] (tolfx/effect (f vm) (param/fraction vm :alpha))))
;;XXX guess could fully work as macro? now need all that is passed through to be weapped, not depend on vm access.


(defn pos-for-page "Convert page index + offset to actual"
 [& {:keys [pos x y page page-x page-y] :or {page [0 0]}}]
 (let [page-x (if page-x page-x (page 0))
       page-y (if page-y page-y (page 1))

       x (+ (or x (pos 0)) (* page-x 8))
       y (+ (or y (pos 1)) (* page-y 8))]
  [x y]))

; this but look up by type/name/tag instead of x-y...
(defn launch! "Find cue at x/y, run it respecting key, priority, end-keys etc. Returns id of new effect, or nil if no cue found. Velocity-responsive vars will start at `:velocity`, or 127 if not specified.  A map of variable keywords to values can be supplied with `:var-overrides`, replacing the `:start` value used normally.  Example usage: compound-cues, afterglow-max. If a `:var-override` is specified for a variable which is also configured as velocity sensitive, the override will win."
  [x y & {:keys [velocity var-overrides priority-override page-x page-y page-id page-index] ;page-id should be key of page creator. if multiple, specify somehow. naming in settings?
          :or {velocity 127 page-x 0 page-y 0}}]
  {:pre [(some? *show*) (number? velocity) (<= 0 velocity 127)]}
  (let [[page-x page-y] (map #(or % 0) [page-x page-y])
        [x y] (map #(+ %1 (* %2 8)) [x y] [page-x page-y])]
   (when-let [cue (ct/cue-at (:cue-grid *show*) x y)]
    (doseq [k (:end-keys cue)] (end-effect! k))
    (let [saved-vars (ct/cue-vars-saved-at (:cue-grid *show*) x y)
          velocity-vars (ct/starting-vars-for-velocity cue velocity)
          vm (#'show/introduce-cue-variables (:variables cue) x y
                                             (merge saved-vars velocity-vars var-overrides))
          id (add-effect! (:key cue) ((:effect cue) vm)
                          :priority (or priority-override (:priority cue)) :from-cue cue :x x :y y :var-map vm)]
      (ct/activate-cue! (:cue-grid *show*) x y id)
      id)))) ;easy way to switch up running effects in repeatable ways - just retrigger from pool of randomized (or pre-set) overrides...

(defn attach-param "Attach a param to(? post by wrapping formula?) var of running cue, easiest prob relaunch effect with new cue var :key?" []
 ;main thing I guess add oscillation/variation to settable value which still acts as baseline
 ;fully overriding also option tho
 )


;; (defn group "Base helper for cue builders. Only makes sense as a macro probably"
;;  [group x y & {:keys [held? priority name end-rest? extra-params
;;                       channels channel-type heads head-type]}] ; XXX rename(?) group and make it handle anything thrown at it - group name, vector of fixture keywords, output of (fixtures-named) etc
;;  (let [[fx-key fixtures fx-name] (group-parts group "color" "color")
;;        fx-name (or name fx-name)]))


(defn color "Create a cue-grid entry which establishes a global color effect, given a named color. Also set up a cue color parameter so the color can be tweaked in the Web UI or on the Ableton Push, and changes can be saved to persist between invocations."
  [group x y color & {:keys [include-color-wheels? held? fixtures priority cue-name]
                      :or   {include-color-wheels? true}}]
  (let [[color desc] (cond (color/color? color) [color, "color"] ;need a named-colors-rgb-to-name with tolerance, find closest one...
                           (keyword? color)     [(color/like (name color)), (name color)]
                           (string? color)      [(color/like color), color]
                           (and (param? color) (= (params/result-type color) thi.ng.color.core.HSLA #_::colors/color))
                                                [color, "variable"]
                           :else                [(color/like (name color)), color])
        [fx-key fixtures fx-name] (group-parts group "color" desc)
        color-var (vars/cue-map ["color" color]) #_{:key "color", :type :color, :start color, :name "Color"}
        f (fn [vm]
              (tolfx/effect (tolfx/color (bind-keyword-param (:color vm) thi.ng.color.core.HSLA color) ;gotta bind vm like this?
                            :effect-name fx-name :fixtures fixtures :include-color-wheels? include-color-wheels?)
                            (param/fraction vm :alpha)))
        ;; cue (cue (util/key-str fx-key (rand)) f :priority priority :held held? :color color
        cue (cue fx-key f :priority priority :held held? :color color
                 :color-fn (cues/color-fn-from-cue-var color-var x y)
                 :variables (vars/auto [color-var] :alpha))]
   (set-cue! x y cue)))

(defn dimmer "Create cue adjusting dimmer level of a group of fixtures, being one of the values in [[light-groups]], or `nil` if the cue should affect all lights."
  [group x y color & {:keys [priority end-rest? htp? add-virtual-dimmers? held? velocity?] :or {priority 5, htp? true}}]
  (let [[fx-key fixtures fx-name end-keys] (group-parts group "dimmers" "dimmers")]
    (set-cue! x y
      (cue fx-key
           ;; (fn [vm] (dimmer-effect (bind-keyword-param (:level vm 255) Number 255) fixtures
           (fn [vm] (dimmer-effect (:level vm 255) fixtures
                                   :effect-name fx-name, :htp? htp?, :add-virtual-dimmers? add-virtual-dimmers?))
           :variables [{:key "level", :min 0, :max 255, :start 255, :velocity velocity? :velocity-min 10 :name "Level"}]
           :held held?, :priority priority, :color color, :end-keys (if end-rest? end-keys)))))

(defn strobe "tweaks, incl uniform [group x y] params inline with other make-cues, dynamic color-var and velocity-target, evt externalize other vars (level etc), not just color"; {{{
  [group x y & {:keys [strobe-color fixtures velocity-target]
                :or   {strobe-color :strobe-color, velocity-target :level}}]
  (vars/init! strobe-color (color/like :mediumpurple2))
  (let [[fx-key fixtures fx-name] (group-parts group "strobe" "strobe") ; XXX check group for vec of fixture names as well...
        color-var {:key strobe-color, :type :color, :name (name strobe-color)}
        ;; [level-var lightness-var] (map #(vars/cue-map [%1 (if (= velocity-target (keyword %1)) true %2) 0 100])
        ;;                           ["level" "lightness"] [80 100]) ;]
        vars (vars/auto (vars/cue-map ["level" (if (= velocity-target :level) true 80) 0 100])
                        (vars/cue-map ["lightness" (if (= velocity-target :lightness) true 0.90) 0.0 1.0])) ] ;dont include color var in actual cue vars...
    (set-cue! x y
      (cue fx-key
           (fn [vm] (apply-vm vm tolfx/color-strobe (:level vm) fixtures :fx-name fx-name))
           :held true, :priority 11000, :color (get-variable strobe-color)
           :color-fn (fn [cue active show snapshot]
                       (if (> (snapshot-bar-phase snapshot 0.5) 0.95) ;should use normalized strobe hz rather?
                         (color/create :black) ;hah but it blinks white tho?
                         (or (get-variable strobe-color) (:color cue))))
           :variables vars))))

(defn channel "control channel in group/fixture. TODO: optionally pass multiple fixtures, and control multiple channels from one cue"
 [group x y channel-key & {:keys [color held? priority fx-name]
                          :or {color :white, priority 0}}] ; fixtures (all-fixtures),
 (let [[fx-key fixtures fx-name] (group-parts group (name channel-key) (name channel-key))]
   (set-cue! x y (cue fx-key
      (fn [vm] (tolfx/channel (:level vm) (get-channels fixtures channel-key) :effect-name fx-name))
      :name fx-name, :held held?, :color color, :priority priority
      :variables (vars/cue-map ["level" 0 0 255] :var-name fx-name :velocity held?)
      #_[{:key "level" :name fx-name :min 0 :max 255 :start 0 :type :integer :velocity held?}]))))

;XXX proper auto is when this fn looks at incoming doc/:keys, and auto-maps from that
(defn auto "Create cue which applies a param of named type to target effect, for specified group/fixtures, with cue variables to adjust the oscillator parameters. Target effect must take two keys, value and fixtures. Additional keys can be passed"
;; WISH-LIST: pass which key is to be controlled so complex effects don't have to be wrapped, + keys for var-map for full control.
;; later: dont actually couple to effect-function at all, but start osc-cue, then pick another cue to be controlled, then pick which var to control.
  ;; could pass like "ext" as "lfo" type for <> ctrl straight over libmapper etc, through common interface of simply using show var
 [group x y param-name fx-fn & ;;XXX change fx-fn to fx-id, resolve from that, then get easy short name etc...
  {:keys [priority end-rest? color scale held? args vars chs ch-type heads head-type] ;channels/heads for eg channel-effect requiring that - handle automatically later...
   :or {priority 10}}]
 (let [param-data (util/get-map-for-param param-name param/types) ;(into {} (filter #(= (:type %) param-name) param/types))
       [fx-key fixtures fx-name end-ks] (group-parts group (str fx-fn) param-name :channel ch-type)
       chs (or chs (get-channels fixtures ch-type))
       heads (or heads (when head-type (chan/extract-heads-with-some-matching-channel fixtures #(= (:type %) head-type)))) ;chs and heads would be maps ready to go, ch-type head-type are keywords
       color (or color (:color param-data))
       variables (apply vars/auto (:variables param-data) (flatten [vars :alpha])) ;XXX switch to float min-max and use scale for eventual value. express width as 1/cycles (+- offset for fine adj?), same concept as reg beats/cycles
       scale (or scale (:max (or (map #(get-map-with % variables) ["level" "max"])) 255)) ;get scale from max level if not otherwise specified
       targets (or chs heads fixtures) ;decr order of prio. XXX function support as well
       held? (or held? (:velocity (get-map-with "level" variables) false)) ;velocity implies held unless specified. (if not or) so handles non-nil falsey
       visualizer (fn [vm show] (lfo-viz vm show param-name scale))
       ; visualizer (fn [vm show] (let [p (param/auto-vm-dev vm param-name)]
       ;                           (fn [snapshot] (/ (params/resolve-param p show snapshot nil) scale))))
       ;; color-fn (fn [cue active show snapshot] ;this needs to reuse, not recreate every frame...  (:variables active) is vm. but is nil until started, (:variables cue) is same as passed, vector of var-defs. could construct similar from :key and :start but that'd go in sep func and reuse those values. should try to reuse visualizer param if available, esp since that's normalized etc
       ;;           (let [l (param/auto-vm-dev (:variables active) param-name :min -30 :max 0) ;so this doesnt work for :level which doesnt handle min/max. which is weird tho cause there is a min and a max to that var lol...
       ;;                 value (resolve-param l show snapshot)
       ;;                 mul (if active 2.0 1.0)] ;lightness boosted when cue active so need larger movement then
       ;; color-param (let [])
       ;color-fn might need redesigning in afterglow - be like fx/visualizer and _return_ fn, capturing scope... tho since unlike those it's also called when not active... hmm
       color-fn (fn [cue active show snapshot]
                 (let [l (param/auto-vm-dev (:variables active) param-name :min -0.30 :max 0) ;so this doesnt work for :level which doesnt handle min/max. which is weird tho cause there is a min and a max to that var lol...
                       value (resolve-param l show snapshot)
                       mul (if active 2.0 1.0)] ;lightness boosted when cue active so need larger movement then
                  (param/color :base-color (:color cue) :adjust-saturation (* mul value)
                               :adjust-hue (* 0.7 mul value)))) ;(cues/color-fn-from-param color-param) ; NOTE: lightness has no effect, only hue/sat
       ; color-fn (let [p (param/auto-vm-dev (:variables active) param-name :min -0.30 :max 0)] ;so this doesnt work for :level which doesnt handle min/max. which is weird tho cause there is a min and a max to that var lol...
       ;           (fn [cue active show snapshot]
       ;            (let [value (resolve-param l show snapshot)
       ;                  mul (if active 2.0 1.0)] ;lightness boosted when cue active so need larger movement then
       ;             (param/color :base-color (:color cue) :adjust-saturation (* mul value)
       ;                          :adjust-hue (* 0.7 mul value))))) ;(cues/color-fn-from-param color-param) ; NOTE: lightness has no effect, only hue/sat
       ;; f (fn [vm] (tolfx/effect (apply-vm vm fx-fn (param/auto-vm-dev vm param-name) targets :effect-name fx-name) ;(tolfx/effect (fx-fn (lfo-param vm param-name) targets :effect-name fx-name)) ;(apply (make-fn apply-vm) vm fx-fn (lfo-param vm param-name) targets :effect-name fx-name, args)
       param-key (key-str fx-name "-" x "-" y "-param")
       get-param (fn [vm] (bind-keyword-param
                           (vars/init! param-key (param/auto-vm-dev vm param-name)) Number 0 fx-name))
       p (atom nil) ;most reasonable. create a common param on first invocation, then re-use for fx, viz, etc
       f (fn [vm] (let []
                    (tolfx/effect (apply-vm vm fx-fn (get-param vm) targets :effect-name fx-name)
                                ;^ so should we try to bind the auto-param to a show key for reuse across fx + color-fn + push-viz? just some different scaling needed.
                                ;wont makke much difference unless also implement caching? if a param has been resolved already this frame/snapshot, dont recalc...
                                (param/fraction vm :alpha))))]
  (set-cue! x y ;XXX put stuff that ends up here in [[ :as cue-args]] then zipmap with keywords and (apply cue fx-key f cue-args)?
            (cue fx-key f :priority priority, :color color, :variables variables, :held held? ;XXX pack resolution into all doubles for better control? shift adjust obvs necessary later
                 :visualizer visualizer, #_:color-fn #_color-fn :end-keys (if end-rest? end-ks)))))

#_(defn auto-param-for-cue "Like if want something with auto configured vars etc but to throw in somewhere random"
 [param-name & {:keys [scale options] :or {scale 255}}]
 (let [param-data (util/get-map-for-param "random" param/types)
       variables (apply vars/auto (:variables param-data) )]
  ;; how best rescale max?
  ))
;; (def prevm (apply vars/auto (:variables (util/get-map-for-param "random" param/types))))
;; (assoc prevm )
;; (vars/rescale prevm 100)
;; (-> (into {} (filter #(= (:key %) "max") prevm))
;;      (assoc , :max 100))
;; (remove #(= (:key %) "max") prevm)
;;XXX ^^ haha this is param/auto-vm. but some stuff lacking from there + that one needs redoing... ugh

(defn multi-lfo "Create cue which applies an lfo of named type to target effect, for specified group/fixtures, with cue variables to adjust the oscillator parameters. Target effect must take two keys, value and fixtures. Additional keys can be passed"
 [group x y lfo-names fx-fn &
  {:keys [priority end-rest? color scale held? extra-keys extra-vars
          channels channel-type heads head-type]
   :or {priority 10}}]
 (let [param-data (into {} (filter #(= (:type %) lfo-names) param/types))
       [fx-key fixtures fx-name end-keys] (group-parts group (str fx-fn) "multi" :channel channel-type) ;FIX might not be getting channel-type
       color :white #_(or color #_(:color param-data))
       variables (apply vars/auto (:variables param-data) (or extra-vars) :alpha) ;XXX switch to float min-max and use scale for eventual value. express width as 1/cycles (+- offset for fine adj?), same concept as reg beats/cycles
       scale (or scale (:max (get-map-with "level" variables)) (:max (get-map-with "max" variables)) 255)
       ;; channels (or channels (get-channels fixtures channel-type))
       targets (or channels heads fixtures)
       held? (or held? (:velocity (get-map-with "level" variables)) held?)] ;should scan all vars rather  ;velocity implies held unless specified. (if not or) so handles non-nil falsey
   (set-cue! x y
     (cue fx-key
          (fn [vm] (fx-fn (param/lfo-chooser vm param/types :picker-param (param/rng :interval :bar :min 1 :max 4))
                          targets :effect-name fx-name))
          :priority priority, :color color, :variables variables
          ;;            (let [l (param/lfo-color-fn (:variables active) lfo-type :min -35 :max 10)]
          ;;             (build-color-param :color (:color cue)
          ;;                                :adjust-saturation (resolve-param l show snapshot) ;(* -20))
          ;;                                :adjust-hue (resolve-param l show snapshot)))) ;(* 40) (- 20)))))
          :held held?
          :end-keys (if end-rest? end-keys)))))

(defn sparkle "build cue for sparkle effect"
 [group x y color & {:keys [param-target param-type priority custom-measure end-rest?]
                     :or {param-target :chance, param-type "sine", priority 20, end-rest? false}}]
  (let [[fx-key fixtures fx-name end-keys] (group-parts group "sparkle" "sparkle" :end-rest? end-rest?) ; = makes not end-rest by default
        color (color/create color)
        variables (vars/auto :chance :fade-time (vars/cue-map ["color" color]) :alpha) ;XXX allow put lfo if wish...
        ; ^ need major adj to do held/vel. prob fuckit leave it til got a better system than custom cue fns...
        ;; visualizer (fn [vm show] (lfo-viz vm show param-type 1.0))
        f #(tolfx/effect (apply-vm % tolfx/sparkle fixtures) (param/fraction % :alpha))
        ;; f (wrap-effect (tolfx/sparkle fixtures))
        #_f #_(fn [vm]
             (let [v (param/auto-vm vm param-type)]
              (apply-vm vm tolfx/sparkle fixtures param-target v)))]
   (set-cue! x y
      (cue fx-key f
           :variables variables, ;:visualizer visualizer
           ;:color-fn (color-fn-from-cue-var (:color variables) #_color)
           :short-name fx-name, :priority priority, :color color
           :end-keys end-keys))))


(defn confetti "cue for confetti effect"
 [group & {:keys [color priority end-rest? aim?]
           :or {color "red" priority 10, aim? true}}]
 (let [[fx-key fixtures short-name end-keys] (group-parts group "confetti" "confetti" :end-rest? end-rest?)
       f (fn [vm]
          (apply-vm vm tolfx/confetti fixtures #_:step #_(param/step vm)))
          ;; (tolfx/effect (apply-vm vm tolfx/confetti fixtures :step (param/step vm) #_:aim? #_aim?)
          ;;               (param/fraction vm :alpha)))
       variables (vars/auto :beats :cycles
                            (map #(vars/cue-map [%1 %2 %3 %4])
                                 (for [t ["add" "dur" "sat" "hue"] a ["min" "max"]]
                                   (str a "-" t)) [1 4 1 4 30 70 210 260] ;;defaults like these should go away from code
                                 [1 1 1 1 0 0 0 0] [4 8 16 16 100 100 360 360]))]
  (cue fx-key f
       :variables variables
       :short-name short-name
       :color color, :priority priority, :end-keys end-keys)))


(defn bloom "build cue for bloom effect"
 [group x y color & {:keys [mods? keyhole? halo? measure fraction-param param-type priority end-rest?]
                     :or {measure ((cfg :measure :default)), param-type "sine", priority 20, end-rest? false}}]
  (let [[fx-key fixtures fx-name end-keys] (group-parts group "bloom" (str "bloom" param-type) :end-rest? end-rest?)
        color (color/create color)
        variables (apply vars/auto :beats :cycles :min-num :max-num :width
                         (vars/cue-map ["color" (param/color :base-color color)])
                         (flatten [#_(when mods? [:hue-mod :lightness-mod :saturation-mod])
                                   (when keyhole? [:keyhole?]) ;why this buggin out? effect itself seems to deal ok...
                                   (when halo? [:halo])
                                   [:alpha]]))
        ;; visualizer (fn [vm show] (lfo-viz vm show param-type 1.0))
        ;; f (wrap-effect (tolfx/bloom fixtures :fraction fraction :measure measure)) ;nyah can do
        f (fn [vm]
             (let [fraction (or fraction-param (param/auto-vm vm param-type))]
              (tolfx/effect (apply-vm vm tolfx/bloom fixtures :fraction fraction #_:measure #_ easure)
                            (param/fraction vm :alpha))))]
   (set-cue! x y
      (cue fx-key f
           :variables variables, ;:visualizer visualizer
           :short-name fx-name, :priority priority, :color color
           :end-keys end-keys))))

(def or-various "Fallback & example of various cue settings map"
 {:bloom {:mods? false :keyhole? false :width 0.25 :measure (cfg :measure :default)}})
(defn various "build cue for various effects"
;;  [group x y color kind & {:keys [settings fraction-param param-type priority end-rest?]
 [group x y color target-fn & {:keys [id level-param auto-param-type cue-vars settings priority end-rest?]
                           :or {#_settings #_or-various, priority 20, end-rest? false}}]
  (let [[fx-key fixtures fx-name end-keys] (group-parts group "various" (name id) :end-rest? end-rest?)
        color (color/create color)
        variables (or cue-vars #_(or-target-fn sorta thing, get default setup...))
        visualizer (fn [vm show] (lfo-viz vm show (or level-param auto-param-type) 1.0))
        ;; target-fn
        f (fn [vm]
             (let [level (or level-param (param/auto-vm vm auto-param-type))]
              ;; (apply-vm vm target-fn fixtures level (flatten (seq settings)))))]
              (apply-vm (merge vm settings) target-fn fixtures level)))]
   (set-cue! x y
      (cue fx-key f
           :variables variables
           :visualizer visualizer
           :short-name fx-name, :priority priority, :color color :end-keys end-keys))))

(defn pinstripe "Pinstripe cues, try turn into more generic n-color base..."
 [group x y colors & {:keys [tolerance?] :or {tolerance? true}}]
 (let [[fx-key fixtures fx-name end-keys] (group-parts group "pinstripe" "pinstripe")
       target-fn #(tolfx/color %2 :effect-name "pin color" :fixtures %1)
       color-keys (mapv #(keyword (str "color-" %)) (range 1 (inc (count colors))))
       fx-fn (fn [vm]
              (tolfx/effect (apply-vm vm tolfx/stripes fixtures target-fn
                                      :step (param/step vm) :values (map vm color-keys))
                            (param/fraction vm :alpha)))
       variables (vars/auto :beats :cycles
                            (apply vars/colors (or colors [:coral1 :aquamarine])) ;tho check for incoming color params hmm
                            (vars/cue-map ["fade" 0.0 0 1])
                            (when tolerance? (vars/cue-map ["tolerance" 0.0 0 1]))
                            :alpha)
       color-fn (fn [cue active show snapshot]
                 (let [vm (:variables active)
                       r (params/resolve-param (param/ratio vm) show snapshot)
                       i (-> (snapshot-beat-phase snapshot (or r 4)))
                       i (int (- (* (count colors)
                                    (snapshot-beat-phase
                                     snapshot (or (params/resolve-param (param/ratio vm) show snapshot) 4)))
                                 0.01))] ;avoid oob
                  (if vm
                   (get-variable (get vm (color-keys i)))
                   (color/create (colors i)))))]
 (set-cue! x y
           (cue fx-key fx-fn :variables variables, :short-name (str fx-name " " (count colors))
                :color (colors 0), :color-fn color-fn))))

; Fix prefab getters Color, Pointing, Dimmer, strobe(?),
; XXX this should really be turned into a stripe spatial param? then connect that to alpha,
; use it to scale whatever, put as conditional-effect param etc...
(defn stripe "tripe cue, try turn into more generic to use for pins as well.."
 [group x y values target-fn & {:keys [id tolerance? color cue-vars] :or {tolerance? true color :aquamarine}}]
 (let [[fx-key fixtures fx-name end-keys] (group-parts group "stripe" (str id "-stripe"))
       target-fn #(move/pan-tilt-effect "stripe pan" %2 %1)
       fx-fn (fn [vm]
                 ;; (let [values (map #(params/build-pan-tilt-param :pan %1 :tilt %2) [-30 30] [30 45])]
                 (let [values (map #(params/build-pan-tilt-param :pan %1 :tilt %2) [-0 0] [10 45])]
                  (tolfx/effect (apply-vm vm tolfx/stripes fixtures target-fn :step (param/step vm) :values values)
                                (param/fraction vm :alpha))))
       variables (vars/auto :beats :cycles
                            ;; (get-vars-for-target-fn-values ctrl... eg pan/tilt in this case)
                            ;; (apply vars/colors (or colors [:coral1 :aquamarine])) ;tho check for incoming color params hmm
                            (vars/cue-map ["fade" 0.0 0 1])
                            (when tolerance? (vars/cue-map ["tolerance" 0.0 0 1]))
                            :alpha)]
 (set-cue! x y (cue fx-key fx-fn :variables variables, :short-name fx-name :color color))))


#_(defn or-color-cycle-chase []
 {:vars {:colors (map #(color/s -18 %) ["mediumpurple" "seagreen" "darkblue" "black"])
         }
  :opts {:chase-fn fun/iris-out-color-cycle-chase, :index-fn rhythm/snapshot-bar-within-phrase
         :transition-fn rhythm/transition-fn}})
#_(defn color-cycle-chase "Color cycle cues" ;XXX
 [group x y & {:keys [colors chase-fn index-fn transition-fn fx-name]
               :or {colors (map #(color/s -18 %) ["mediumpurple" "seagreen" "darkblue" "black"])
                    chase-fn fun/iris-out-color-cycle-chase, index-fn rhythm/snapshot-bar-within-phrase
                    transition-fn rhythm/snapshot-bar-phase}}]
 (let [[fx-key fixtures fx-name end-keys] (group-parts group "color-cycle" "color-cycle")
       variables (vars/auto (vars/colors (:colors p)))
       fx-fn (fn [vm] (chase-fn fixtures, :color-cycle colors
                                :color-index-function index-fn
                                :transition-phase-function transition-fn
                                :effect-name fx-name))]
 (set-cue! x y (cue fx-key fx-fn :variables variables, :short-name fx.name
                    #_:color-fn #_color-fn, :color (first (:colors p))))))


(defn auto-chase "Not sure if belongs here since returns fn... move to cue?"
 [chase-key x y effects & {:keys [name playback param-type pad? pad]
                            :or {name "Chase" playback :loop pad [(fx/blank) (fx/blank) (tolfx/color :black)] #_param-type #_"step"}}] ;#_eventually,fixsupport!!
 (let [id (or (key-str chase-key) :chase)
       f  (fn [vm]
           (let [effects (if pad? (apply interleave effects (map repeat pad)) effects)
                 ;amount (count effects) ;need if using non-step params so know where to cap (unless :loop/:bounce...)
                 position (param/step vm) #_(param/auto-vm vm "step" #_param-type)]
            (chase name effects position :beyond playback)))
       variables (vars/auto (:variables (util/get-map-for-param "random" param/types)))]
  (set-cue! x y (cues/cue id f :variables variables))))

(defn one-shot "Runs an effect for x beats, then finishes"
 [k x y effect & {:keys [ticks envelope] :or {ticks 4}}]
 (let [f (fn [vm]
          (let [effects [(fx/blank) effect (fx/blank)]
                steps  [(build-step-param :interval-ratio 1 :fade-fraction 1.0 :fade-curve :sine)
                        (build-step-param :interval-ratio ticks :fade-fraction 0.1)
                        (build-step-param :interval-ratio 1 :fade-fraction 1.0)]
                chooser  (fn [in on out]
                          (cond
                           (< in 2) in
                           (< on 2) (+ on 1)
                           :else out))
                pos (apply build-param-formula Number chooser steps)
                name (str "One-shot " (:name effect))
                #_variables #_(vars/auto ())]
          (chase name effects pos)))]
  (set-cue! x y (cues/cue k f #_:variables #_variables :priority 100000))))
;; or do we use a regular fade thing instead like not sure how to play it...
;; also for something like this cool with scaling pos through spatial param and slightly staggered invocation
;; so do we create three different one-step steppers?
;; fade-in
;; effect (with :starting quantized, optionally)
;; fade-out
;; and which one is in use depends on value
;; (def steps [(build-step-param :interval-ratio 1 :fade-fraction 1.0 :fade-curve :sine)
;;             (build-step-param :interval-ratio 2)
;;             (build-step-param :interval-ratio 1)])
;; (defn choose-step
;;  [in on out]
;;  (cond
;;   (< in 2) in
;;   (< on 2) (+ on 1)
;;   :else out
;;   ))
;; (apply build-param-formula Number f steps)


(defn gen "cue that runs a scene, including taking input from operator, but also running code to replace itself, with a new cue, taking more input, eventually spawning an effect cue"
 []
 (code-cue))

(defn code "code-cue wrapper." ;XXX extend code-cue to be able to take other args than [show snapshot]
 [x y f label & {:keys [color]}]
 (set-cue! x y (apply code-cue f label (when color [:color color]))))


(defn param-column "Create a column of lfo-cues, from passed param/types" ;XXX general row fn
 [group x yb effect-fn & {:keys [args vars params cue-fn colormap]
                          :or {params param/types, cue-fn auto}}]
 (doall (map-indexed
     (fn [y p]
      (let [y (+ yb y)
            opts [(when args [:args args]) (when vars [:vars vars])]
            color (:color (or colormap p))]
       ;XXX flag to put "group" into eg :channel-type needed to make truly generic wrt effect-fn...
       (apply cue-fn group x y (:type p) effect-fn #_(or args) opts))) ;, :color (:color lfo) ;not needed handled in lfo-cue?
     ; ^this is weird shit tho
     params)))

(defn param-page "Create a page of lfo-cues, from passed groups and param/types" ;XXX general page fn
 [groups xb yb effect-fn & {:keys [args vars params cue-fn colormap]
                            :or {params param/types, cue-fn auto}}]
  (doall (map-indexed
   (fn [x group]
    (let [opts [(when args [:args args]) (when vars [:vars vars])]] ;XXX look or if "group" actually something else supposed to go somewhere else than fixture group
     (apply param-column group (+ xb x) yb effect-fn opts)))
     ;; (param-column group (+ xb x) yb effect-fn )))
   groups)))


(defn rescale
 [transform & {:keys [x y z xyz scaling] :or {x false y false z false scaling 1.0}}]
 (.setScale transform (apply #(Vector3d. %1 %2 %3)
                             (mapv #(if % (- scaling) scaling) [x y z]))))


(defn- pointing-var-key "Determine cue variable key value for aim/direction cues. If `group` is nil `base-name` is returned as string, for use as cue-var :key. Else keyword, to refer to a show variable identifying which group it belongs to."
  [base-name group kind]
  (if (string/blank? (if group (name group)))
    (name base-name)
    (key-str kind "-" (if group (str (name group) "-")) (name base-name))))

(defn pointing-transform ;this should prob be a single cue with three bools xyz no?
 [group x y flip-str id color]
 (let [cue-name (pointing-var-key :transform group id)]
  (set-cue! x y
            (cue cue-name
                 (fn [_]
                  (let [transform (Transform3D.)]
                   (apply rescale transform (interleave (map #(keyword (str %)) flip-str)
                                                        (repeat true)))
                   (var-fx/variable-effect @(at :vars) cue-name transform)))
            :color color
            :short-name (str (capitalize (name group)) " flip " (upper-case (reduce str flip-str)))))))

(defn pointing "Aim / Direction / Pan-tilt AIO in <30 LoC bow BOW"
 [fixture-key group transform? color & {:keys [id] :or {id :aim}}]
 (let [group (if group (name group))
       isolated? (string/blank? group)
       get-key #(pointing-var-key % group id)
       {:keys [vars name-prefix fns] :as from-cfg} (ptr-cfg :type id) ;eh pass things tho...
       [p-fn tf-fn fx-fn] (map eval fns)
       _ (map #(show/set-variable! (get-key %) (:start (ptr-cfg :vars %))) vars) ;this should go in page tho prob cause doesnt need to run all the f time...
       variables (vars/auto (mapv #(let [{:keys [start min max]} (ptr-cfg :vars %)]
                                    (vars/cue-map [(get-key %) (if isolated? start) min max]
                                       :centered true :resolution 0.05));; pga shared vars, but eh need start just not overwrite when already exists...  they were getting reset either way when new ones come on so...  solution prob to init show vars earlier on and then omit start?
                                  vars) :alpha)
       arg-map (flatten (mapv #(do [% (get-key %)]) vars))
       fx-name (str name-prefix " " (name fixture-key)
                             (when-not isolated?
                              (str " (" (upper-case group) (when transform? " flip") ")")))
       f (fn [vm]
          (let [base (if isolated?  (apply-vm vm p-fn)  (apply p-fn arg-map)) ;use shared show variables for groups. so why do we override them each time??
                final (if (and transform? tf-fn) (tf-fn base (get-key :transform)) base)] ;aint working properly now, different keys between flip and reg?
           (fx-fn fx-name final (fixtures-named fixture-key))))]
   (cue (key-str id "-" fixture-key) (wrap-effect f) ;; (cue (key-str id "-" fixture-key) (fn [vm] (tolfx/effect (f vm) (param/fraction vm :alpha)))
        :variables variables :color color :priority 20)))

;; (ptr-cfg :vars :pan)
;; (map #(pointing-var-key % :a :aim) [:pan :tilt])

(defn pointing-lfo "General ting" ;XXX lfo params should be able to be shared, like above...
 [fixture-key id & {:keys [param-type] :or {param-type "sine"}}]
  (let [{:keys [vars name-prefix fns] :as type-cfg} (ptr-cfg :type id)
        [p-fn _ fx-fn] (map eval fns)
        fx-name (str name-prefix " " (name fixture-key))
        variables (vars/auto (vars/prefixed-lfos (select-keys (ptr-cfg :lfo) vars))
                             :alpha)
        f (fn [vm]
            (let [params (map #(build-oscillated-param ;XXX fix auto-vm for name-prefix so EZ
                            (sine :interval :bar ;actually dont hardcode, all could be interesting...
                                  :interval-ratio ((key-str % "-bars") vm)
                                  :phase ((key-str % "-phase") vm 0))
                            :min (get vm (key-str % "-min")) :max (get vm (key-str % "-max")))
                          vars)
                  param (apply p-fn (interleave vars params))]
             (fx-fn fx-name param (fixtures-named fixture-key))))]
   (cue (key-str name-prefix "-" fixture-key) (wrap-effect f)
        :variables variables, :color :dodgerblue, :priority 30)))

;XXX general to function-lfo then absorb into lfo...
(defn focus-lfo "Returns a cue which oscillates a fixture's focus between a minimum and minimum value using a sine oscillator with cue variables to adjust the range and the oscillator's parameters."
  [effect-key effect-name fixtures]
  (cue effect-key
       (fn [vm]
        (let [osc (param/auto-vm vm "sine")]
            ;; (chan-fx/function-effect effect-name :focus osc fixtures)))
            (tolfx/function osc :focus :effect-name effect-name :fixtures fixtures)))
            :color :yellow
            :variables (vars/auto (vars/min-max 0 100) :beats :cycles (vars/alt-start :phase 0.5))))

(defn function "fix"
  [function-key effect-key effect-name fixtures]
  (let [osc-type (case function-key )])
  (cue effect-key
       (fn [vm]
        (let [osc (param/auto-vm vm "sine")]
            (tolfx/function osc :focus :effect-name effect-name :fixtures fixtures)))
            :color :yellow
            :variables (vars/auto (vars/min-max 0 100) :beats :cycles (vars/alt-start :phase 0.5))))


; good test case. We want to be able to introduce an auto-param with given vars, while not hijacking or
; getting its jacked...
(defn sweep "test"
 [group x y param-name fx-fn & {:keys [sweep-fn vars] :or {sweep-fn tolfx/sweep}}]
 (let [data (util/get-map-for-param param-name param/types)
       [fx-key fixtures fx-name end-keys] (group-parts group "sweep" param-name)
       ;; vars (apply vars/auto (:variables data) (flatten [:width :level-dmx :fade (or vars) :alpha]))
       vars (apply vars/auto (flatten [:beats :cycles :min-num :max-num :width :level-dmx :fade :phase #_(or vars) :alpha]))
       ;; position #(apply param/auto-vm-dev % param-name :min 0.0 :max 1.0 :phase (:phase %))
       position #(param/auto-vm-dev % param-name)
       ;; position #(apply param/auto-vm-dev % param-name :min 0.0 :max 1.0 :phase (:phase %)
       ;;                  (when (= param-name "sawtooth") [:down? (:down %)]))
       ;; lfo (resolve (symbol "afterglow.effects.oscillators" param-name)) ;fix real params...
       ;; position #(apply lfo :interval-ratio (param/ratio %) :phase (:phase %)
       ;;                             (when (= lfo-name "sawtooth") [:down? (:down %)]))
       ]
  (set-cue! x y
            (cue fx-key
                 (fn [vm] (tolfx/effect (apply-vm vm sweep-fn fixtures (position vm) :effect-fn fx-fn)
                                        (param/fraction vm :alpha)))
                 :color :red :short-name fx-name
                 :variables vars))))

(defn sweep-column
 [group x yb fx-fn &
  {:keys [sweep-fn args extra-vars params]
   :or {sweep-fn #_tolfx/dimmer-sweep tolfx/sweep,
        params ["sawtooth" "triangle" "random"]}}] ;decouple param creation from osc thing for other support...
 (doall (map-indexed
         (fn [i param]
          (sweep group x (+ yb i) param fx-fn))
         params)))


;; AUTOMATIC CUE PLACEMENT
(defonce put-data (atom {:row 0 :column 0
                         :page {:x 0 :y 0}}))

(defn inc-cue-pos "Sets position on grid for auto-cue"
 ([]
  (let [{:keys [row column page]} @put-data
        high #(-> (% page) inc (* 8) dec)
        low #(-> (high %) (- 7))
        y (inc row)
        y (if (> y (high :y)) (low :y) y) ;reset y to bottom
        x (cond-> column (= y (low :y)) inc)] ;inc when y at minimum (= stepped round)
  (swap! put-data #(assoc % :row y :column x))
    (when (and (>= x (high :x)) #_(>= y (high :y))) ;when does it start ticking page-y instead or vice versa?
              (swap! put-data update-in [:page :x] inc)
              (swap! put-data #(assoc % :column (low :x)))) ;or throw an error saying page is full, depending on cfg...
    (map @put-data [:column :row])))

 ([step]
   (when (> step 1)
    (inc-cue-pos (dec step)))
   (inc-cue-pos)))

(defn set-column! "Set row to auto-populate with cues"
 [column]
 (swap! put-data #(assoc % :column (+ column (* 8 (:x (:page %)))))))
;;  (swap! put-data (fn [s] (update s :column (+ column (* 8 (:x (:page @put-data))))))))
;; (defn by-row []) ;should also be possible. first of all store state in more reasonable fashion
(defn set-page! "Set row to auto-populate with cues"
 [x y]
 (swap! put-data #(assoc % :page {:x x :y y} :row 0 :column 0)))

(defn put "Automatically place cue in next free pos on grid"
 [cue]
 (let [{:keys [column row page]} @put-data
       offset #(if (= % :x) column row)
       [x y] (map #(+ (offset %) (* 8 (% page))) [:x :y])]
  (show/set-cue! x y cue)
  (inc-cue-pos)))

;; (place ())
