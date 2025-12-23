(ns tolglow.sequencer "Some attempt at using code to run cues to develop the interface without going the proper route..."
  (:require [afterglow.web.routes.show-control :as control]
            [tolglow.fx :as tolfx]))

; so best way to get called each frame, beat etc?
; register frame rendering extension like show vars or?
; or we simply just check every supposed hz and yada?
; even check metro periodically, calc bpm/hz how long ish to sleep before
; next beat...
; or just effect with no assigners -> resolve snap
; but then not supposed to act on that on same thread, dispatch?
; need 2 thing: add/remote cue, toggle.
; then can make simple step sequencer... each step/fx coulc even(? and why lol) handle
; shutting down self
; and launching + providing next with fn to do same to one after that etc, and would know
; what to do when encounter various forms of 'active' (existing/launched) cues
(defn secue "Cue with an effect running code on cues, acting as the current position of a step parameter"
  [x y f]
  (let []))
(defn secudfx "That effect running code on cues"
  []
  (let []))
;; (control/handle-create-macro)

;; util/hook-var

; Port macro stuff to push. Will need a mechanism of tagging either effects on screen or buttons on grid
; Use same things to make creating cues easier using only cues.
; Like tagging cues to put their effects in a chase, merge them into a scene, etc.
; Some pages would have no user functionality but just act as sequencer/visual help
;
;; But feels too much is not generalized between web, different controllers etc...
;; they should just be inputs qnd outputs, no business logic :|
;;
;;
;;
;; XXX broken in clojure-lsp:
;;
;; basically, let it run on itself and observe issues:
;; unused warnings as errors (fixed)
;; unused-public - should that even be a thing?? limit to defn- and ^private by default imo
;; "unused declaration" both for defns and refers (but refers are so not the same thing - repl!!) - should refers also go under alias or as a new category? I want them as hints or info, not warnings or errors
;; "this" could be handled as _? dont complain if unused
;;
;; #_ reader-skipped comments still parsed so warns unused, errors etc
;; defmacro: let/catch sym# (unknown symbol)
;; letfn (unknown symbol)
;; runs by default on project.clj and profiles.clj, ok lein is not the language but cmon...
