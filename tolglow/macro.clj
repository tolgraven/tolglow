(ns tolglow.macro "Borrows and expands on afterglows facility for macros.")
;; TODO: Take his shit and separate it from the web crao,,,k

(defn gather-cues "some mechanism to tag relevant cues easier. crap in web atm and nothing on push"
  [selector & {:keys [all? category attribute]}]) ;could filter by priority, whether it's movement, color, etc
;; want to "tag" cues without launching them. to add to a macro, or maybe link their launch''
;; in-place to another cue?
;;
;;this just brings fn?
(defn as-compound-effect "Surely should only gather the macro stuff into a cue format, which is then placed by regular means."
  [macro-effects & macro-name])
;;some kinda auto name/description as well showing contents? Could also have colorfn switching between contents colors...
(defn as-individual-effects "Should work right? Instead of compound effect, just gather same data and launch individually by code-cue"
  [macro-effects & macro-name]
    (cues/code-cue (fn [] (map show/add-effect!/from-cue-grid?))) macro-effects)) ;;and/or for compound it's possible to have like one param per fx still available hehe 

(defn as-chase "right"
  [])


(defn save-to-file "Basic file saving already but probably want to keep some order,
                    decide to actively save to a specific session, ensure any dependencies also saved"
  [macro & {:keys [filename session-key or-whatev]}]
  ())

(defn load-from-file
  [filter-or-something?]) ;; idea: macros can be put on another "layer" above regular grid. Making it mostly east to keep relevant stuff where makes sense
;; then also easier to just save and load casually since no worries new regular cues will suddenly be in the way etc
;;
;;might even just be fully integrated with cues ns, such similar stuff...
(defn save-show-to-file ;which is like same tech but again not really macro. oh well its the right ns to work on this stuff.
  [filename])
(defn load-show-from-file
  [filename])

{:auto-save-cue-vars-on-kill true} ;asap fixa
;; so on cue end -> snapshot-cue-variables -> save-cue-vars -> store ???

;;ideally evt webUI has allow compositing cues from scratch.
;;Choose/write an fx-fn, decide what to hook to its inputs...
;;sorta nodeRED/maxMSP-esque...vthat's where all my design towards anyways, something that would be easy to make quite generic
;; anyways it keeps tracks of actual calls at same time and saves the ran code cool?

;; (defonce ^:private ^{:doc "Used to assign unique keywords to cues created as macros."}
;;   macro-counter (atom 0))
;; (defonce macro-record-file ^{:doc "When not nil, any macros defined will have the code that can re-create them appended to the named file."}
;;   (atom nil))
;;
;; (defn- reformat-colors-for-saving "Rewrite the cue variables of a macro being saved for later use in a form where the color objects will get properly recreated."
;;   [cues-with-vars]
;;   (mapv (fn [[x y vars]]
;;           [x y (reduce (fn [r [k v]]
;;                          (assoc r k (if (= (type v) :com.evocomputing.colors/color)
;;                                       (list 'colors/create-color (str \" (colors/rgb-hexstr v) \"))
;;                                       v))) {} vars)])
;;         cues-with-vars))
;;
;; (defn- handle-create-macro "Process a request to create a macro from running effects."
;;   [page-info x y macro-name macro-effects]
;;   (with-show (:show page-info)
;;     (let [cues (map #(controllers/cue-at (:cue-grid (:show page-info)) (:x %) (:y %)) macro-effects)
;;           cue-errors (filter identity (map (fn [cue effect]
;;                                              (when (nil? cue) (str "No cue found at [" (:x effect) (:y effect) "].")))
;;                                            cues macro-effects))
;;           cues-with-vars (when (empty? cue-errors)
;;                            (vec (map (fn [cue effect]
;;                                        [(:x effect) (:y effect) (cues/snapshot-cue-variables cue (:id effect))])
;;                                      cues macro-effects)))
;;           errors (concat cue-errors (when (clojure.string/blank? macro-name) "No macro name provided."))]
;;       (if (seq errors)
;;         {:error (clojure.string/join " " errors)}
;;         (let [macro-key (keyword (str "macro-" (swap! macro-counter inc)))]
;;           (when-let [file @macro-record-file]
;;             (spit file (with-out-str
;;                           (println "\n (show/set-cue!" x  y)
;;                           (println "   (cues/cue" macro-key)
;;                           (println (str "    (fn [_] (cues/compound-cues-effect \"" macro-name "\" *show*"))
;;                           (print "           " (reformat-colors-for-saving cues-with-vars))
;;                          (println "))))"))
;;                   :append true))
;;           (controllers/set-cue!
;;            (:cue-grid (:show page-info)) x y
;;            (cues/cue macro-key
;;                      (fn [_]
;;                        (cues/compound-cues-effect
;;                         macro-name (:show page-info) cues-with-vars))))
;;           {:macro-created macro-key})))))
;;
;;
;; (defn- handle-cue-click-event
;;   "Process a mouse down on a cue grid cell."
;;   [page-info kind req]
;;   (let [[left bottom] (:view page-info)
;;         [_ column row] (clojure.string/split kind #"-")
;;         [x y] (map + (map #(Integer/valueOf %) [column row]) [left bottom])  ; Translate relative page coordinates
;;         [cue active] (show/find-cue-grid-active-effect (:show page-info) x y)
;;         shift (get-in req [:params :shift])]
;;     (if cue
;;       (with-show (:show page-info)
;;         (if (and active (not (:held cue)))
;;           (do (show/end-effect! (:key cue))
;;               {:ended kind})
;;           (let [id (show/add-effect-from-cue-grid! x y)]
;;             (if (and (:held cue) (not shift))
;;               (do
;;                 ;; Let the grid know a momentary cue is being held, so proper feedback can be shown
;;                 (swap! clients assoc-in [(:id page-info) :holding] [x y id])
;;                 {:holding {:x x :y y :id id}})
;;               {:started id}))))
;;       (let [macro-name (get-in req [:params :macroName])
;;             macro-effects (get-in req [:params :macroEffects])]
;;         (if (seq macro-effects)
;;           (handle-create-macro page-info x y macro-name macro-effects)
;;           {:error (str "No cue found for cell: " kind)})))))
