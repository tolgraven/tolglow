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
     [debug :as debug :refer [det]]
     [fx :as tolfx]
     [param :as param :refer [lfo-viz]]
     [util :as util :refer [add-midi-callback apply-vm clamp-number get-channels get-map-with key-str random-in-range space-phase x-phase]]
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
  (let
   #_debug/det [effect-key #_(keyword (or (some string? (map #(when % (str (name %) "-" effect-suffix))
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

(defn wrap-effect "Wrap cue effect fn in tolfx/effect, for alpha control. "
 [f]
 (fn [vm] (tolfx/effect (f vm) (param/fraction vm :alpha))))

(defn pos-for-page "Convert page index + offset to actual"
 [& {:keys [pos x y page page-x page-y] :or {page [0 0]}}]
 (let [page-x (if page-x page-x (page 0))
       page-y (if page-y page-y (page 1))

       x (+ (or x (pos 0)) (* page-x 8))
       y (+ (or y (pos 1)) (* page-y 8))]
  [x y]))

; this but look up by type/name/tag instead of x-y...
(defn launch!  "Find cue at x/y, run it respecting key, priority, end-keys etc. Returns id of new effect, or nil if no cue found. Velocity-responsive vars will start at `:velocity`, or 127 if not specified.  A map of variable keywords to values can be supplied with `:var-overrides`, replacing the `:start` value used normally.  Example usage: compound-cues, afterglow-max. If a `:var-override` is specified for a variable which is also configured as velocity sensitive, the override will win."
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
  (let [[color desc] (cond (= (type color) ::colors/color) [color, (color-name color)]
                          (keyword? color)   [(color/like (name color)), (name color)]
                          (and (param? color) (= (params/result-type color) ::colors/color))
                            [color, "variable"]
                          :else              [(color/like (name color)), color])
        [fx-key fixtures fx-name] (group-parts group "color" desc)
        color-var (vars/cue-map ["color" color]) #_{:key "color", :type :color, :start color, :name "Color"}
        efn (fn [vm]
              (tolfx/effect (tolfx/color (bind-keyword-param (:color vm) ::colors/color color) ;gotta bind vm like this?
                            :effect-name fx-name :fixtures fixtures :include-color-wheels? include-color-wheels?)
                            (param/fraction vm :alpha)))
        cue (cue (util/key-str fx-key (rand)) efn :priority priority :held held? :color color
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
        [level-var lightness-var] (map #(vars/cue-map [%1 (if (= velocity-target %1) true %2) 0 100])
                                  [:level :lightness] [80 100])]
        ;; vars (vars/auto color-var level-var lightness-var)]
        ;; vars (vars/auto color-var :level-percent :lightness-percent)]
        ;; vars (vars/auto :level-percent :lightness-percent)]
    (set-cue! x y
      (cue fx-key
           (fn [vm] (fun/strobe-2 fx-name fixtures (:level vm) (:lightness vm)))
           #_(fn [vm] (fun/strobe-2 fx-name fixtures
                                  (bind-keyword-param (:level vm) Number 80)
                                  (bind-keyword-param (:lightness vm) Number 80)))
           ;; (fn [vm] (tolfx/strobe (bind-keyword-param (:level vm) Number 80) fixtures :name fx-name :lightness (:lightness vm)))
           ;; (fn [vm] (apply-vm vm fun/strobe-2 fx-name fixtures))
           ;; (fn [vm] (tolfx/strobe-effect (:level vm) fixtures :effect-name fx-name :lightness (:lightness vm)))
           :held true, :priority 11000, :color (get-variable strobe-color)
           :color-fn (fn [cue active show snapshot]
                       (if (> (snapshot-bar-phase snapshot 0.5) 0.95) ;should use normalized strobe hz rather?
                         (color/create :black)
                         (or (get-variable strobe-color) (:color cue))))
           ;; :variables vars #_[level-var lightness-var color-var]))))
           :variables [level-var lightness-var color-var]))))
;; (show/add-effect! :strobe (fun/strobe-2 "strobe" (show/all-fixtures) 80 80))

(defn channel "control channel in group/fixture. TODO: optionally pass multiple fixtures, and control multiple channels from one cue"
 [group x y channel-key & {:keys [color held? priority fx-name]
                          :or {color :white, priority 0}}] ; fixtures (all-fixtures),
 (let [[fx-key fixtures fx-name] (group-parts group (name channel-key) (name channel-key))]
   (set-cue! x y (cue fx-key
      (fn [vm] (tolfx/channel (:level vm) (get-channels fixtures channel-key) :effect-name fx-name))
      :name fx-name, :held held?, :color color, :priority priority
      :variables (vars/cue-map ["level" 0 0 255] :var-name fx-name :velocity held?)
      #_[{:key "level" :name effect-name :min 0 :max 255 :start 0 :type :integer :velocity held?}]))))

;XXX proper auto is when this fn looks at incoming doc/:keys, and auto-maps from that
(defn auto "Create cue which applies a param of named type to target effect, for specified group/fixtures, with cue variables to adjust the oscillator parameters. Target effect must take two keys, value and fixtures. Additional keys can be passed"
;; WISH-LIST: pass which key is to be controlled so complex effects don't have to be wrapped, + keys for var-map for full control.
;; later: dont actually couple to effect-function at all, but start osc-cue, then pick another cue to be controlled, then pick which var to control.
  ;; could pass like "ext" as "lfo" type for <> ctrl straight over libmapper etc, through common interface of simply using show var
 [group x y param-name fx-fn & ;;XXX change fx-fn to fx-id, resolve from that, then get easy short name etc...
  {:keys [priority end-rest? color scale held? args vars chs ch-type heads head-type] ;channels/heads for eg channel-effect requiring that - handle automatically later...
   :or {priority 10}}]
 (#_tolglow.debug/det
  let [param-data (util/get-map-for-param param-name param/types) ;(into {} (filter #(= (:type %) param-name) param/types))
       [fx-key fixtures fx-name end-ks] (group-parts group (str fx-fn) param-name :channel ch-type)
       chs (or chs (get-channels fixtures ch-type)) ;XXX afterglow, when getting chs/fixtures/heads seems lots redundant info? each channel has defs of entire fixture, itself including all the info _again_ per ch?
       heads (or heads (when head-type (chan/extract-heads-with-some-matching-channel fixtures #(= (:type %) head-type)))) ;chs and heads would be maps ready to go, ch-type head-type are keywords
       color (or color (:color param-data))
       variables (apply vars/auto (:variables param-data) (flatten [vars :alpha])) ;XXX switch to float min-max and use scale for eventual value. express width as 1/cycles (+- offset for fine adj?), same concept as reg beats/cycles
       scale (or scale (:max (or (map #(get-map-with % variables) ["level" "max"])) 255)) ;get scale from max level if not otherwise specified
       targets (or chs heads fixtures) ;decr order of prio. XXX function support as well
       held? (or held? (:velocity (get-map-with "level" variables) false)) ;velocity implies held unless specified. (if not or) so handles non-nil falsey
       visualizer (fn [vm show] (lfo-viz vm show param-name scale))
       color-fn (fn [cue active show snapshot] ;this needs to reuse, not recreate every frame...  (:variables active) is vm. but is nil until started, (:variables cue) is same as passed, vector of var-defs. could construct similar from :key and :start but that'd go in sep func and reuse those values. should try to reuse visualizer param if available, esp since that's normalized etc
                 (let [l (param/auto-vm (:variables active) param-name :min -30 :max 0)
                       value (resolve-param l show snapshot)
                       mul (if active 2.0 1.0)] ;lightness boosted when cue active so need larger movement then
                  (build-color-param :color (:color cue) :adjust-saturation (* mul value)
                                     :adjust-hue (* 0.7 mul value)))) ;(cues/color-fn-from-param color-param) ; NOTE: lightness has no effect, only hue/sat
       f (fn [vm] (tolfx/effect (apply-vm vm fx-fn (param/auto-vm #_nil vm param-name) targets :effect-name fx-name) ;(tolfx/effect (fx-fn (lfo-param vm param-name) targets :effect-name fx-name)) ;(apply (make-fn apply-vm) vm fx-fn (lfo-param vm param-name) targets :effect-name fx-name, args)
                                (param/fraction vm :alpha)))]
  (set-cue! x y ;XXX put stuff that ends up here in [[ :as cue-args]] then zipmap with keywords and (apply cue fx-key f cue-args)?
            (cue fx-key f :priority priority, :color color, :variables variables, :held held? ;XXX pack resolution into all doubles for better control? shift adjust obvs necessary later
                 #_:visualizer #_visualizer, :color-fn color-fn :end-keys (if end-rest? end-ks)))))



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
          (fn [vm] (fx-fn (param/lfo-chooser vm param/types :picker-param (param/rng 1 4))
                          targets :effect-name fx-name))
          :priority priority, :color color, :variables variables
          ;;            (let [l (param/lfo-color-fn (:variables active) lfo-type :min -35 :max 10)]
          ;;             (build-color-param :color (:color cue)
          ;;                                :adjust-saturation (resolve-param l show snapshot) ;(* -20))
          ;;                                :adjust-hue (resolve-param l show snapshot)))) ;(* 40) (- 20)))))
          :held held?
          :end-keys (if end-rest? end-keys)))))

#_(defn sparkle "build cue for sparkle effect"
 [group x y color & {:keys [param-target param-type priority end-rest?]
                     :or {param-target "chance", param-type "sine", priority 20, end-rest? false}}]
  (let [[fx-key fixtures fx-name end-keys] (group-parts group "sparkle" "sparkle" :end-rest? end-rest?) ; = makes not end-rest by default
        color (color/create color)
        variables (vars/auto :chance :fade-time (vars/cue-map ["color" color])) ;XXX allow put lfo if wish...
        ;; visualizer (fn [vm show] (lfo-viz vm show param-type 1.0))
        f #(apply-vm % tolfx/sparkle fixtures)
        #_f #_(fn [vm]
             (let [chance (or chance-param chance #_(param/auto-vm vm param-type))]
              (apply-vm vm tolfx/sparkle fixtures :fraction fraction :measure measure)))]
   (set-cue! x y
      (cue fx-key f
           :variables variables, ;:visualizer visualizer
           :short-name fx-name, :priority priority, :color color
           :end-keys end-keys))))


(defn bloom "build cue for bloom effect"
 [group x y color & {:keys [mods? keyhole? halo? measure fraction-param param-type priority end-rest?]
                     :or {measure ((cfg :measure :default)), param-type "sine", priority 20, end-rest? false}}]
  (let [[fx-key fixtures fx-name end-keys] (group-parts group "bloom" (str "bloom" param-type) :end-rest? end-rest?)
        color (color/create color)
        variables (apply vars/auto :beats :cycles :min-num :max-num :width
                         (vars/cue-map ["color" color])
                         (flatten [(when mods? [:hue-mod :lightness-mod :saturation-mod])
                                   (when keyhole? [:keyhole?]) ;why this buggin out? effect itself seems to deal ok...
                                   (when halo? [:halo])
                                   [:alpha]]))
        visualizer (fn [vm show] (lfo-viz vm show param-type 1.0))
        f (fn [vm]
             (let [fraction (or fraction-param (param/auto-vm vm param-type))]
              (tolfx/effect (apply-vm vm tolfx/bloom fixtures :fraction fraction :measure measure)
                            (param/fraction vm :alpha))))]
   (set-cue! x y
      (cue fx-key f
           :variables variables
           :visualizer visualizer
           :short-name fx-name, :priority priority, :color color
           :end-keys end-keys))))

(def or-various "Fallback & example of various cue settings map"
 {:bloom {:mods? false :keyhole? false :width 0.25 :measure (cfg :measure :default)}})
#_(defn various "build cue for various effects"
;;  [group x y color kind & {:keys [settings fraction-param param-type priority end-rest?]
 [group x y color fx-id & {:keys [settings fraction-param param-type priority end-rest?]
                           :or {settings or-various, priority 20, end-rest? false}}]
  (let [[fx-key fixtures fx-name end-keys] (group-parts group "various" (util/ensure-is :string fx-id) :end-rest? end-rest?)
        color (color/create color)
        variables ;these set by case
        (apply vars/auto :beats :cycles :min-num :max-num :width
                         (vars/cue-map ["color" color])
                         (flatten [(when (:mods? settings) [:hue-mod :lightness-mod :saturation-mod])
                                   (when (:keyhole? settings) [:keyhole?])])) ;why this buggin out? effect itself seems to deal ok...
        visualizer (fn [vm show] (lfo-viz vm show param-type 1.0))
        fx ()
        f (fn [vm]
             (let [fraction (or fraction-param (param/auto-vm vm param-type))]
              (apply-vm vm tolfx/bloom fixtures :fraction fraction :measure measure)))]
   (set-cue! x y
      (cue fx-key f
           :variables variables
           :visualizer visualizer
           :short-name fx-name, :priority priority, :color color
           :end-keys end-keys))))

(defn pinstripe "Pinstripe cues, try turn into more generic n-color base..."
 [group x y colors & {:keys [tolerance?]}]
 (let [[fx-key fixtures fx-name end-keys] (group-parts group "pinstripe" "pinstripe")]
 (set-cue! x y
           (cue :pinstripes
                (fn [vm]
                 (let [colors (map #((keyword (str "color-" %)) vm) (range (count colors)))]
                  (fun/pinstripes fixtures :step (param/step vm) :colors colors)))
                :variables (vars/auto :beats :cycles
                                      (apply vars/colors (or colors [:coral1 :aquamarine])) ;tho check for incoming color params hmm
                                      (vars/cue-map ["fade" 0.0 0 1])
                                      (when tolerance? (vars/cue-map ["tolerance" 0.0 0 1])))
                :color :orange :short-name "Pinstripes"
                :color-fn (fn [cue active show snapshot]
                           (let [vm (:variables active)]
                            (if (> (snapshot-bar-phase snapshot 0.5) 0.5)
                             (:color-1 vm (color/create (colors 0)) #_(:start (colors))) #_(get-variable (:color-1 vm))
                             (:color-2 vm (color/create (colors 1)) #_(:start color-2)) #_(get-variable (:color-2 vm)))))))))


(defn color-cycle-chase "Color cycle cues" ;XXX
 [group x y colors &
  {:keys [chase-fn index-fn transition-fn fx-name]
   :or {chase-fn fun/iris-out-color-cycle-chase, index-fn rhythm/snapshot-bar-within-phrase, rhythm/transition-fn snapshot-beat-phase}}]
 (let [[fx-key fixtures fx-name end-keys] (group-parts group "color-cycle" "color-cycle")]
 #_(set-cue! x y (cue ))))

(defn blank "cue that runs a scene, including taking input from operator, but also running code to replace itself, with a new cue, taking more input, eventually spawning an effect cue"
 []
 (code-cue))

(defn code "code-cue wrapper." ;XXX extend code-cue to be able to take other args than [show snapshot]
 [x y f label & {:keys [color]}]
 (set-cue! x y (apply code-cue f label (when color [:color color]))))
;; (defn codez "code-cue wrapper"
;;  [xb yb & maps]
;;  (doseq [[cue-name ]]))

(defn param-column "Create a row of lfo-cues, from passed param/types" ;XXX general row fn
 [group x yb effect-fn & {:keys [args vars params cue-fn colormap] :or {params param/types, cue-fn auto}}]
 (doall (map-indexed
     (fn [y p]
      (let [y (+ yb y)
            type (:type p)
            opts [(when args [:args args]) (when vars [:vars vars])]
            color (:color (or colormap p))]
       ;XXX flag to put "group" into eg :channel-type needed to make truly generic wrt effect-fn...
       ;; (apply cue-fn group x y type effect-fn #_(or args) opts))) ;, :color (:color lfo) ;not needed handled in lfo-cue?
       (cue-fn group x y type effect-fn ))) ;, :color (:color lfo) ;not needed handled in lfo-cue?
     params)))

(defn param-page "Create a page of lfo-cues, from passed groups and param/types" ;XXX general page fn
 [groups xb yb effect-fn & {:keys [args vars params cue-fn colormap] :or {params param/types, cue-fn auto}}]
  (doall (map-indexed
   (fn [x group]
    (let [;group group
          opts [(when args [:args args]) (when vars [:vars vars])]] ;XXX look for if "group" actually something else supposed to go somewhere else than fixture group
     (apply param-column group (+ xb x) yb effect-fn opts)))
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
                  (let [transform (Transform3D.)] ;exact reason this is here? need new object for each or? then just reset at top of loop instead
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
       from-cfg #(ptr-cfg :type id %)
       [var-keys name-prefix] (map from-cfg [:vars :name-prefix])
       [p-fn tf-fn fx-fn] (map eval (from-cfg :fns))
       variables (vars/auto (mapv #(let [cue-val (fn [k] (ptr-cfg :vars % k))]
                         (vars/cue-map [(get-key %) (if isolated? (cue-val :start))
                                        (cue-val :min) (cue-val :max)] ;XXX handle bounds?
                                       :centered true :resolution 0.05))
                       var-keys) :alpha)
       f (fn [vm]
          (let [arg-map (flatten (mapv #(do [% (get-key %)]) var-keys))
                base (if isolated? (apply-vm vm p-fn) (apply p-fn arg-map)) ;use shared show variables for groups
                final (if transform? (tf-fn base (get-key :transform)) base) ;aint working properly now, different keys between flip and reg?
                fx-name (str name-prefix " " (name fixture-key)
                             (when-not isolated?
                              (str " (" (upper-case group)
                                   (when transform? " flip") ")")))]
           (fx-fn fx-name final (fixtures-named fixture-key))))]
   ;; (cue (key-str id "-" fixture-key) (fn [vm] (tolfx/effect (f vm) (param/fraction vm :alpha)))
   (cue (key-str id "-" fixture-key) (wrap-effect f)
        :variables variables :color color :priority 2)))


(defn pointing-lfo "General ting" ;XXX lfo params should be able to be shared, like above...
 ; ALSO share vector between aim and dir, so can flip between two by overlaying, with same vector.
 ; + groups of vectors.
  [fixture-key id]
  (let [prefix (ptr-cfg :type id :name-prefix)
        fx-name (str prefix " " (name fixture-key))
        fixtures (fixtures-named fixture-key) ;should prob use group and group-parts instead...
        variables (vars/auto (case id ;XXX get from settings / venue
                   :pt (vars/prefixed-lfos {:pan [64 191 4] :tilt [0 127 3]})
                   (vars/prefixed-lfos {:x [-2.0 2.0 4] :y [-2.0 2.0 3] :z [2.0 3.0 8]}))
                   :alpha)
        fx (fn [vm]
            (let #_tolglow.debug/det
             [vars (config/ptr-cfg :type id :vars)
              params (map #(build-oscillated-param ;XXX fix auto-vm for prefix so EZ
                            (sine :interval :bar
                                  :interval-ratio ((key-str % "-bars") vm)
                                  :phase ((key-str % "-phase") vm 0))
                            :min ((key-str % "-min") vm) :max ((key-str % "-max") vm))
                          vars)]
              (case id :pt (let [chs (map #(get-channels fixtures %) vars)
                                effects (map #(chan-fx/channel-effect (name %1) %2 %3) vars params chs)]
                           (apply fx/scene fx-name effects))
                       (let [[p-fn _ fx-fn] (map eval (ptr-cfg :type id :fns))
                             param (apply p-fn (interleave [:x :y :z] params))]
                        (fx-fn fx-name param fixtures)))))]
   (cue (key-str prefix "-" fixture-key) (wrap-effect fx) ;(fn [vm] (tolfx/effect (fx vm) (param/fraction vm :alpha)))
        :variables variables, :color :dodgerblue, :priority 3)))

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
  [effect-key effect-name fixtures]
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
       lfo (resolve (symbol "afterglow.effects.oscillators" param-name))
       vars (apply vars/auto (:variables data) (flatten [:width :level-dmx :fade (or vars) :alpha]))
       position #(apply param/auto-vm nil param-name :min 0.0 :max 1.0 :phase (:phase %)
                        (when (= param-name "sawtooth") [:down? (:down %)]))
       ;; position #(apply lfo :interval-ratio (param/ratio %) :phase (:phase %)
       ;;                             (when (= lfo-name "sawtooth") [:down? (:down %)]))
       ]
           (set-cue! x y
             (cue fx-key
                  (fn [vm] (apply-vm vm sweep-fn fixtures (position vm) :effect-fn fx-fn))
                  :color :red :short-name fx-name
                  :variables vars))))

(defn sweep-column
 [group x yb fx-fn &
  {:keys [sweep-fn args extra-vars params]
   :or {sweep-fn tolfx/dimmer-sweep #_tolfx/sweep,
        params ["sawtooth" "triangle" "random"]}}] ;decouple param creation from osc thing for other support...
 (doall (map-indexed
         (fn [y param]
          (sweep group x yb param fx-fn))
         params)))


;; AUTOMATIC CUE PLACEMENT
(defonce auto-x (atom 0))
(defonce auto-y (atom 0))
(defonce auto-page (atom {:x 0, :y 0}))


(defn incr-cue-pos "Sets position on grid for auto-cue"
 ([]
  (let [high #(dec (* (inc (% @auto-page)) 8))
        low #(- (high %) 7)
        x (inc @auto-x)
        x (if (> x (high :x)) (low :x) x)
        y (if (= x (low :x)) (inc @auto-y) @auto-y) ;inc when x at minimum (= stepped round)
        page (if (> y (high :y)) (reset! auto-page (update @auto-page :x inc)))
        y (if (> y (high :y)) (low :y) y)
        x (if (or (> x (high :x)) (< x (low :x))) (low :x) x)]
    (reset! auto-x x)
    (reset! auto-y y)
    ;; (doseq [[atm val] [[auto-x x] [auto-y y]]] (reset! atm val))
    [x y]))
 ([step]; [& {:keys [page-x page-y direction]}]
  (let [step (dec step)]
   (when (> step 0)
    (incr-cue-pos step))
   (incr-cue-pos))))

;; (defn xb [offset & base] "quick and tiny adding for cues"
;;  (let [base (or (first base) @x-base 0)] (+ base offset)))
;; (defn yb [offset & base]
;;  (let [base (or (first base) @y-base 0)] (+ base offset)))

(defn auto-cue "Automatically place cue in next free pos on grid"
 [])

