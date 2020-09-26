(ns tolglow.core "Big dump errthing" {:author "Joen Tolgraven"}
  (:gen-class)
  (:require
   [afterglow
    [core :as core] [controllers :as ct]
    [midi :as midi :refer [sync-to-midi-clock]]
    [show :as show :refer [add-effect! all-fixtures end-effect! fixtures-named get-variable patch-fixture! set-cue! set-variable! start! stop! sync-to-external-clock]]
    [show-context :refer [*show* with-show]]]
   [afterglow.effects
    [channel :as chan-fx] [dimmer :as dimmer]
    [oscillators :as lfo :refer [build-oscillated-param sawtooth sine square triangle]]
    [cues :as cues :refer []]
    [params :as params :refer [param?]]
    [show-variable :as var-fx :refer [variable-effect]]]
   [afterglow.web.routes.show-control :as aweb]
   [clojure.string :as string :refer [capitalize upper-case]]
   [com.evocomputing.colors :as colors :refer [color-name]]
   [thi.ng.color.core :as clr]
   [thi.ng.math.core :as cmath]
   ;; [overtone.osc :as oosc :refer [osc-close osc-handle osc-listen osc-send]]
   [tolglow
    [color :as color :refer [color?]]
    [config :as config :refer [at cfg venue wall]]
    [cue :as cue :refer [group-parts]]
    [fixtures :as fixtures] [fx :as fx] [osc :as osc] [page :as page]
    [param :as param] [setup :as setup]
    [graph :as graph]
    [util :as util :refer [apply-vm avar clamp clear! get-channels get-map-with key-str random-in-range space-phase value x-phase]]
    [vars :as vars :refer [alt-start cue-map cue-maps]]
    [viz :as viz]]
   [clojure.pprint :refer [pprint pp print-table]]))
#_(when (cfg :debug :enabled)
 (map use '[clojure.tools.namespace.repl #_clojure.tools.trace #_debugger.core]))

(defn activate-show
 [& {:keys [force-new?]}] ;have a thing to reset state but not actually fully-replace show: should be default when one already exists
 (setup/init #(core/init-logging) :logging)

 (print "\nInit core components...")
 (setup/init-by-ks (cfg :init :components) :required true)
 (print "\nInit modules..." "\tForce" (if (cfg :debug :force-init) "on" "off"))
 (setup/init-by-ks (cfg :init :modules))
 (print "\nDone setting up show, id" (:id *show*) "\t" #_"\n")
 (setup/init-by-ks (cfg :init :post))) ;finish setup, presumably by loading a terminal-repl

;; (show/add-effect! :strobe (chan-fx/function-effect "strobe level" :strobe 90 (show/all-fixtures)))
;; (show/add-effect! :color (fx/color (color/like :blue)))
;; (show/add-effect! :dimmer (fx/global-dimmer-effect 255))
;; (show/add-effect! :dimmer (fx/metronome-effject (all-fixtures)))
;; (util/value (util/value rngy ))
; (def show *show*)
; (require 'quil.core)
; (viz/init)

(defn reprioritize "Restart cue with new effect-priority, preserving state otherwise"
 [cue prio]
 #_(cue/save-vars osv))

#_(map #(keys @(% *show*))
[:send-buffer-fns :frame-fns :next-id :task :statistics
 :movement :grid-controllers :metronome :dimensions
 :variables :cue-grid :active-effects :fixtures])
(defn cue-dim [] @(-> *show* :cue-grid :dimensions))
(defn cues [] @(-> *show* :cue-grid :cues))
(defn fx [] @(-> *show* :active-effects))
(defn dim [] (keys @(-> *show* :dimensions)))
(defn vars [] @(*show* :variables))
(defn move [] @(*show* :movement))
(defn vizi [] (:visualizer-visible  @(*show* :dimensions)))
(defn unis [] (*show* :universes))
(defn fixtures [] (keys @(*show* :fixtures)))
(defn ks [] (keys *show*))
(defn ks [] (keys @(:active-effects *show*)))
;; (require '[puget.printer :as puget])
;; (filter #(= (:color %) :red) chans)
;; (afterglow.fixtures/printable (fixtures-named :test))
;; ((vizi) 26)
;; (count (vizi))
;; (:color-1 (:previous (move)))

; START MODELING GRAPHQL STUFF
; chucking dat real data to frontend

(defn n-dim-ks "Build keys from n(?) vectors" ;FIXME wtf is this for?
 [& colls]
 (loop [result (map key-str (first colls)), colls (next colls)]
  ;; (println result)
  (let [result (map #(key-str %1 "-" %2) result (first colls))]
   (if (next colls) (recur (next colls) result)))))

(key-str [:what :is :this])
(n-dim-ks [:what :is :this] [:seriously :weird :shit])
#_(for [pos [:min :center :max] axis [:x :y :z]]
 (key-str pos "-" axis))
#_(map #(% @(-> *show* :dimensions))
     [:min-x :center-x :max-x
      :min-y :center-y :max-y
      :min-z :center-z :max-z]) ;; :timestamp :visualizer-visible
#_(show/patch-fixture! :strip-1 (tolglow.fixtures/pixel-strip
                               10 :mode :rgbw, :x [-3.0 -0.0] :y [2.0 2.0]
                               :channels config/pixtol-chs)
                     5 1)

(def pur (clr/hsla 0.8 0.5 0.5))
(def c (clr/hsla 0.2 0.6 0.6))
(def z (clr/hsva 0.2 0.6 0.0))
(def crgb (clr/as-rgba c))
(def chsv (clr/as-hsva c))
(def e (clr/hsla 1.3 0.6 0.5))

(def grey (clr/rgba 0.4 0.5 0.6))
(-> (clr/rgba 1.0 1.0 1.0) (clr/as-hsla))
(-> grey (clr/adjust-brightness 1.0) (clr/as-hsla))
(-> grey (clr/adjust-brightness 1.0))
(-> grey (clr/adjust-brightness -1.0))
(-> z clr/as-hsla (clr/adjust-luminance 1.0))
(-> pur (clr/adjust-luminance 0.45) clr/as-rgba)
(-> c clr/as-hsva (clr/adjust-brightness -0.9) #_clr/as-hsla)
(-> c clr/as-hsva (clr/adjust-brightness -0.1) clr/as-rgba)
(-> c clr/as-hsva (clr/adjust-brightness -0.03) clr/as-rgba clr/as-hsla)
(-> c clr/as-hsva (clr/adjust-brightness 0.03) clr/as-rgba clr/as-hsla)
(-> c clr/as-hsva (clr/adjust-brightness 0.8) clr/as-hsla clr/as-rgba)
(-> chsv (clr/adjust-brightness -0.3))


(defn -main [& args] (activate-show))
(defn shortcuts [] (keys (:cue-grid *show*)))
;XXX add to effect interface
; :inlets (how can we control it? NEEDED for proper cue-var automapping / dynparam anything)
;   vector of params + extra data "is this one reasonable to randomly fuck with?"
; :pause-fn (anything that needs to be done to temporarily halt / resume it?)
; :endpoints / :categories (or similar, "what kind(s) of stuff does the effect affect?")
;
;param interface
; :inlets (color :h :s :l, spatial: :x :y :z, etc)
;   so can auto-chain params onto sub-components of others
; alt
; :components (so we know Color is three Numbers)

; new Cue interface?

;; bind position to volume instead of straight step param = slo-mo between beats, speedup during
;; bind position-in-animation (frame) to lots of stuff - note etc, forwards-backwards...
