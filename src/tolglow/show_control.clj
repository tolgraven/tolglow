(ns tolglow.show-control
  )

; make a general interface for communicating with and controlling show
; current afterglow is not generalized enough - lots of stuff in web/show-control and controllers/push
; etc should be common.
;
; so any interface (web/rest, controller/push, standalone midi, osc, max(?)) would tell afterglow
; what it wants to show and then parse updates
; same as eg the js side does for web now

; important thing is never do anything directly - even from within clj side afterglow
; (like a controller) should be based on subscribe and dispatch
;
; so everything is uniform, implement once, work with data
; messaging conforms to a standard so all fns working with them take same args
;
; look in controllers, they already use protocols - should just be moved one step "higher"
; so is same also for web etc

(defprotocol IShowController
  "Would be extended by IGridController to add physical controller specific stuff (height, width)?"
  (display-name [this])
  (add-listener [this kind f])
  (remove-listener [this kind f])

  ;or like
  (add-filter [this hmm] "By default subscribes to all info? If only interacting with a subset we can filter what. Tho if we filter here it'd still send everything to everyone = bad... So prob have a subscribe fn on other end and also a subscribe-all..."))

(defprotocol ICueController
  "Maybe too fine-grained. But eg filter what cue vars we actually show/use,
   in a general way? Remember these are per-implementation fns tho...
   point is to use the same fn everywhere hahah"
  (add-cue-var-filter [this]))

(defn cue-grid "Currently in controllers. But should it even be here??  More like in a cue ns. Because cues can still be saved and used in pure code so grid exists independently of any show-control. Or maybe should save cues with some other ID and *then* map those to grid for show-control? Then this here becomes very simple, just a mapping of ids to positions!" []
  {:dimensions (ref [0 0])
     :cues (ref {}) ;; For now using a sparse grid in the form of a map whose keys are the cue coordinates, and whose values are the cues. If performance dictates, can change to nested vectors or something else later.

     ; REMOVE. show-control doesn't know about midi or whatever
     :midi-feedback (ref {}) ;; Also track any non-grid controllers which have bound controls or notes to cues and want feedback as they activate and deactivate.  A nested set of maps: The first key is a tuple of the grid coordinates.  The next map is keyed by the tuple [MidiDevice channel note kind] identifying where the feedback should be sent, and the values are a tuple of [feedback-on feedback-off device disconnect-handler], the MIDI values to send as feedback when the cue turns on or off, the overtone.midi :midi-device map corresponding to the MidiDevice object (for convenience in sending messages), and the handler function that was registered to clear the feedback in case the device ever got disconnected. It is included so that it can be unregistered if the user explicitly cancels the feedback.

     :fn-feedback (ref {}) ;; maybe. more inclined to also have fns register as thing? not sure why any of that would be in the same map as cues themselves tho...

     ; should be extended so can save multiple versions of a cue var
     ; (and all cue states?)
     :saved-vars (ref {}) ;; Track saved cue variables to be reapplied the next time the cue is run.  A map indexed in the same way as :cues, whose values are the map of variable keys and values.
     })

; OK START OVER!
; would then have helps fns to both assemble and (later) change this stuff th
(def controller-data-spec
  {:subscriptions {:filter [] :filtering-in? true} ;what data from show are we interested in and might react to? empty means everything
   ; since the controller might be running outside the app,
   ; makes little sense to have any awareness of its implementation
   ; but to reduce boilerplate and if given an ns would still try to
   ; auto-resolve fns with equivalent name to event type
   :cue-middleware {} ;default would wrap cues in grid probably?

   })
(defn register "Register a new show-controller"
  [controller-data] ;probably
  )

