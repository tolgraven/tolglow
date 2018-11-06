(ns tolglow.core "Big dump errthing" {:author "Joen Tolgraven"}
  (:gen-class)
  (:require
   [afterglow
    [core :as core]
    [controllers :as ct]
    [midi :as midi :refer [sync-to-midi-clock]]
    [show :as show :refer [add-effect! all-fixtures end-effect! fixtures-named get-variable patch-fixture! set-cue! set-variable! start! stop! sync-to-external-clock]]
    [show-context :refer [*show* with-show]]]
   [afterglow.effects
    [channel :as chan-fx]
    [dimmer :as dimmer :refer [dimmer-effect]]
    [oscillators :as lfo :refer [build-oscillated-param sawtooth sine square triangle]]
    [params :as params :refer [param?]]
    [show-variable :as var-fx :refer [variable-effect]]]
   [clojure.string :as string :refer [capitalize upper-case]]
   [com.evocomputing.colors :as colors :refer [color-name]]
   [overtone.osc :as oosc :refer [osc-close osc-handle osc-listen osc-send]]
   [tolglow
    [color :as color :refer [color?]]
    [config :as config :refer [at cfg venue wall]]
    [cue :as cue :refer [group-parts]]
    [fixtures :as fixtures] [fx :as fx] [osc :as osc] [page :as page]
    [param :as param] [setup :as setup]
    [quil :as quil]
    [util :as util :refer [apply-vm avar clamp-number clear! get-channels get-map-with key-str random-in-range space-phase value x-phase]]
    [vars :as vars :refer [alt-start cue-map cue-maps]]]))
;; (when-not (cfg :debug :enabled)
;;  (map use ['clojure.pprint 'clojure.tools.namespace.repl 'clojure.tools.trace]))
  ;; 'debugger.core

(defn activate-show []
 (setup/init #(core/init-logging) :logging)

 (print "\nInit core components...")
 (setup/init-by-ks (cfg :init :components) :required true)
 (print "\nInit modules..." "\tForce" (if (cfg :debug :force-init) "on" "off"))
 (setup/init-by-ks (cfg :init :modules))
 (print "\nDone setting up show, id" (:id *show*) "\t" #_"\n")
 (setup/init-by-ks (cfg :init :post))) ;finish setup, presumably by loading a terminal-repl

;; ;; (show/patch-fixture! :wash-1 (fixtures/rgbw-7-12-moving) 10 65)
;; (show/remove-fixture! :wash-3)
;; (show/add-effect! :strobe (chan-fx/function-effect "strobe level" :strobe 90 (show/all-fixtures)))
;; (show/add-effect! :color (fx/color (color/like :blue)))
;; (show/add-effect! :dimmer (fx/global-dimmer-effect 255))

(defn reprioritize "Restart cue with new effect-priority, preserving state otherwise"
 [cue prio]
;;  (cue/save-vars osv)
 )

#_(map #(keys @(% *show*)) [; :send-buffer-fns :frame-fns :next-id :task :statistics  :id :universes :sync :pool
;:movement :grid-controllers :metronome :dimensions
 :variables :cue-grid :active-effects :fixtures])
(defn cue-dim [] @(-> *show* :cue-grid :dimensions))
(defn cues [] @(-> *show* :cue-grid :cues))
(defn fx [] @(-> *show* :active-effects))
(defn dim [] (keys @(-> *show* :dimensions)))
(defn vars [] (:variables *show*))

;; (use 'puget.printer)
;; (cprint @(-> *show* :active-effects))

(defn n-dim-ks "Build keys from n(?) vectors" ;FIXME
 [& colls]
 (loop [colls (next colls) result (map key-str (first colls))]
  (println result)
  (let [result (map #(key-str %1 "-" %2) result (first colls))]
   (if (next colls) (recur (next colls) result)))))

#_(for [pos [:min :center :max] axis [:x :y :z]]
 (key-str pos "-" axis))
#_(map #(% @(-> *show* :dimensions))
     [:min-x :center-x :max-x
      :min-y :center-y :max-y
      :min-z :center-z :max-z]) ;; :timestamp :visualizer-visible

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
(defn -main [& args] (activate-show))

(defn shortcuts []
 (page/create :pointing 1 1 "aim")
 (page/create :pointing 2 1 "direction")
 (keys (:cue-grid *show*)))


;; refactor-nrepl
;; analyzer artifacts config
;; extract-definition find-symbol find-unbound
;; middleware
;; ns.clean-ns ns.dependencies ns.helpers ns.ns-parser ns.rebuild ns.resolve-missing
;; ns.slam.hound.future ns.slam.hound.regrow ns.slam.hound.search
;; plugin
;; rename-file-or-dir
;; stubs-for-interface
;; util

;; (defn debugging []
;;  (ex-info :ok {:huh *e})
;;  (trace "tag" (* 2 3)) ;; To trace a value and assign a trace tag
;; (deftrace fubar [x v] (+ x v)) ;; To trace a function call and its return value
;; (fubar 2 3)
;;
;; (trace-forms (+ 1 3) (/ 1 0) (* 5 6)) ;; To identify which form is failing
;; (trace-vars tolglow.cue/pan-tilt-lfo) ;; To dynamically trace/untrace specific fns (untrace-vars myown.namespace/fubar)
;; (untrace-vars tolglow.cue/pan-tilt-lfo))
