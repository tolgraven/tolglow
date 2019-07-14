(ns tolglow.util "Utility functions"
 (:require
  [afterglow
   [channels :as chan :refer [expand-heads extract-channels find-rgb-heads]]
   [controllers :as ct]
   [midi :as midi :refer [sync-to-midi-clock]]
   [rhythm :as rhythm :refer [metro-snapshot metro-start metronome snapshot-bar-phase snapshot-beat-phase snapshot-beat-within-bar snapshot-down-beat?]]
   [show :as show :refer [add-effect! all-fixtures end-effect! fixtures-named get-variable patch-fixture! set-cue! set-variable! start! stop! sync-to-external-clock]]
   [show-context :refer [*show* set-default-show! with-show]]]
  [afterglow.effects.params :as params :refer [bind-keyword-param build-aim-param build-color-param build-direction-param-from-pan-tilt build-pan-tilt-param build-param-formula build-spatial-param build-step-param frame-dynamic-param?  param?  resolve-param validate-param-type]]
  [clojure.string :as string :refer [capitalize upper-case]]
  [com.evocomputing.colors :as colors]
  [thi.ng.color.core :as clr]
  [tolglow
   [color :as color :refer []]
   [config :as config :refer [at cfg ptr-cfg]]]))


(defn value "shortcut resolve param to val"
 [param]
 (resolve-param param *show* (metro-snapshot (:metronome *show*))))

(defn clamp-number "Clamp a number within min-max range"
  [number min max]
  (let [number (or number 0)] ;XXX check int or float auto 0.0-1.0 / 0-255?
    (cond (>= number max) max
          (<= number min) min
          (<= min number max) number)))

#_(defn scale-number "Scale a number from 0.0-1.0 to min-max range"
  [number lo hi]
  (let [number (or number 0)
        hi (max lo hi)
        range (- max  min)
        ] ;XXX check int or float auto 0.0-1.0 / 0-255?
    #_(cond (>= number max) max
          (<= number min) min
          (<= min number max) number)))

(defn ensure-is
 [kind v]
 (case kind
 :string (try (name v) (catch Throwable t (str v)))
 :keyword (if (keyword? v) v (keyword v))))

(defn key-str ;XXX make macro so can write - without quoting and stuff...
 [& args]
 (keyword (apply str (map #(ensure-is :string %) args))))

(defn find-fn "Should auto-resolve keywords, strings, etc, to any reasonable fn, from cfg list of ns's to look in, or manually specd, or by keyword ns..."
 [id & {:keys [location]}])

(defn random-in-range "Get random value between min and max, automatically handling int, float, color, bool..."
 [low high]
 (let [f (fn [low high] (+ low (rand (- high low))))]
  (condp = (type low)
         Boolean (> 0.5 (rand))
         Long    (long (f low high)) ;:integer
         Integer (int (f low high)) ;:integer
         ;; Double   ;:float
         ;; Float    ;:float
         ::colors/color (color/random low high) ;:color
         (f low high) )))

(defn catchall "Wrap f in (try (f) (catch Exception e))"
 [f & logfn]
  (let [logfn (or logfn
                  (fn [f e]
                   #_(println "\n" (str e) #_"\n" #_e)
                   (when (cfg :debug :auto-print-trace) (printf "\n%s" e)) ;welp dang to do this, fucks pst etc...
                   (swap! (at :exceptions) #(concat % [e]))))]
   (try
    (f)
    (catch Exception e
      (logfn f (str e))))))


(defn find-nested "Find key at any level of nested maps"
  [m k]
  (->> (tree-seq map? vals m)
       (filter map?)
       (some k)))

(defn avar "get/set show variable shortcut"
 ([key]
  (show/get-variable key))
 ([key value]
  (show/set-variable! key value)))

(defn print-var-on-change
  [key value]
  (print key " " value "\t"))

(defn watch-var
 [key]
 (show/clear-variable-set-fn! key print-var-on-change)
 (show/add-variable-set-fn! key print-var-on-change))

(defn hook-var
 [show-key f]
 (show/clear-variable-set-fn! show-key f)
 (show/add-variable-set-fn! show-key f))

;; (defn check-pre-map "Run keys (fns) against vals (vectors of variables)"
;;  [m] ;nice idea, but useless since can't tell what caused assert duh. needs macro
;;  (every? true? (mapcat #(map %1 %2) (map key m) (map val m))))
(defmacro check-pre-map "Run keys (fns) against vals (vectors of variables)"
 [m] ;nice idea, but useless since can't tell what caused assert duh. Could throw own assertion tho?
 (let [fn-names (keys m)
       vars (vals m)
       results (map map ~fn-names ~vars)]
  results))

#_(let [m {some? [1 3 nil 4], integer? [1 1.0 nil]}]
 (check-pre-map m)
 (vec m)
  (zipmap (keys m) (map map (map key m) (map val m)))
 (map map m))

(defn get-fixtures "Resolve keywords to fixture maps, return maps as-is and all-fixtures for nil"
 [fixtures]
 (cond
  (keyword? (first fixtures)) (mapcat show/fixtures-named fixtures)
  (some? fixtures) fixtures
  :else (show/all-fixtures)))

(defn get-channels "wrap extract-channels"
 [fixtures channel-type]
 (let [fixtures (get-fixtures fixtures)
       heads (chan/expand-heads fixtures) ;XXX use?
       channel-type (ensure-is :keyword channel-type)]
       ;; channel-key (map #(if (keyword? %) % (keyword %)) (flatten [channel-type]) #_[channel-type])] ;support multiple
 (when channel-type (chan/extract-channels fixtures #(= (:type %) channel-type)))))
 ;; (when channel-key (mapcat (fn [k] (extract-channels fixtures #(= (:type %) k))) channel-key))))

#_(defn scaled-metronome "not needed for what I thought, since scaling time in ratio-param... but might still be good to finish."
 ([]) ; update
 ([multiplier divisor] ;setup
   (set-variable! :metro-scale (/ multiplier divisor))
   (set-variable! :scaled-master-metro (metronome (/ (:bpm (metro-snapshot (:metronome *show*)))
                                                     (get-variable :metro-scale)))))
 ([scale-param])) ;setup dynamic


(defn add-midi-callback
  [device-filter channel index f & {:keys [kind info-requested] :or {kind :cc}}] ;grab different stuff from msg depending on info-requested. maybe :info-requested on and get true/false note-on/off, etc
  {:pre [#_(check-pre-map {some? [*show* device-filter] integer? [channel index]}) (some? *show*) (some? device-filter)
         (integer? channel) (integer? index) (<= 0 channel 15) (<= 0 index 127)]}
  (let [show *show*  ; Bind so we can pass it to update function running on another thread
        update-fn (fn [msg] (with-show show (f (:velocity msg))))
        route (condp = kind :cc midi/add-control-mapping :note midi/add-note-mapping)]
    (route device-filter channel index update-fn)
    update-fn))

(defn add-midi-var-mapping "Fix general midi mapping thing"
  [device-filter channel index variable & {:keys [min max transform-fn kind] :or {min 0 max 127 kind :cc}}]
  {:pre [(some? *show*) (some? device-filter) (some? variable) (integer? channel) (<= 0 channel 15)
         (integer? index) (<= 0 index 127)
         (number? min) (number? max) (not= min max) (or (nil? transform-fn) (ifn? transform-fn))]}
  (let [show *show*  ; Bind so we can pass it to update function running on another thread
        scale-fn (cond
                   (and (zero? min) (= max 127)) (fn [v] v)
                   (< min max) (fn [v] (double (+ min (/ (* v (- max min)) 127))))
                   (> min max) (fn [v] (double (+ max (/ (* v (- min max)) 127)))))
        calc-fn (apply comp (filter identity [transform-fn scale-fn]))
        update-fn (fn [msg]
                    (with-show show
                      (set-variable! variable (calc-fn (:velocity msg)))))]
    ((condp = kind :cc midi/add-control-mapping
                   :note midi/add-note-mapping)
     device-filter channel index update-fn)
    update-fn))




(defn space-phase "Return position of head, in dimension, with show bounds as 0-1 - generally left to right for x, low to high for y, front to back for z."
  [head show & {:keys [axis] :or {axis :x}}]
  (let [dim @(:dimensions *show*)
        [min-axis max-axis] (map #(key-str % "-" axis) ["min" "max"])
        ;; min-dim (keyword (str "min-" (str axis)))
        ;; max-dim (keyword (str "max-" (str axis)))
         ]
    (/ (- (axis head) (min-axis dim)) (- (max-axis dim) (min-axis dim)))))


(defn x-phase "Return a value that ranges from zero for the leftmost fixture in a show to 1 for the rightmost, for staggering the phase of an oscillator in making a can-can chase."
  [head show]
  (let [dimensions @(:dimensions *show*)]
    (/ (- (:x head) (:min-x dimensions)) (- (:max-x dimensions) (:min-x dimensions)))))


(defn get-map-with "extract single map from vector by string matched to value of some key, default :key..."
 [string coll & {:keys [by] :or {by :key}}]
 (into {} (filter #(= (by %) string) coll)))

(defn get-map-for-param
 [param-name param-types]
 (get-map-with param-name param-types :by :type))



(defn clear-cues! []
 (let [[x y] @(:dimensions (:cue-grid *show*))]
 (doseq [x (range x), y (range y)] ;XXX should rather find placed cues...
  (show/clear-cue! x y))))

(defn clear-fixtures! []
 (doseq [k (keys @(:fixtures *show*))]
  (show/remove-fixture! k)))

(defn clear!
 [& what]
 (let [what (or what [:cues :effects])]
  (doseq [thing what]
   (condp = thing
    :cues (clear-cues!)
    :fixtures (clear-fixtures!)
    :effects (show/clear-effects!)))))


;; (def #^{:macro true} apply-vm #'cues/apply-merging-var-map) ;; give macro another name
;; ^^ XXX dunno why but above suddenly fucking me, wasted 3 hours... just after made some project.cfg additions. locals clearing off now only thing i know. anyways, DANGER
(defmacro apply-vm ;XXX include some common actions so eg beats+cycles auto-resolves through rato-param etc...
  "Call fn, merging k/v from supplied cue var-map to end of arg list, so optional effect params can be cue-controlled without manually specifying each."
  [var-map f & args]
  `(apply ~f ~@args
          (flatten (seq ~var-map))))

(defn show-keys-and-defaults "Create map where keys are values of incoming effect args (show variable keys, params, raw values or nil) and values are values for corresponding keys in fallback defaults map. This is then used with [[bind-keyword-param]] in effect fn"
 [arg-map defaults-map]
 (apply zipmap
        (map (fn [coll]
              (vals (sort-by #(name (first %))
                             coll)))
             [arg-map defaults-map])))


;; (def defaults-map {:bars 1 :cycles 1 :stagger 0 :spread 0 :pan-min 0 :pan-max 0 :tilt-min -100 :tilt-max 100})
(defn ks-show-ks-defaults "Create map with keys from possible effect args and values a vector of incoming
                           (show variable keys, params, raw values or nil) and default fallback. This is then used with [[bind-keyword-param]] in effect fn"
 [arg-map defaults-map]
 (into {}
       (for [k (keys defaults-map)]
        {k {:default (k defaults-map) :arg (k arg-map)}}))
 #_(zipmap (keys (sort-by #(name (first %)) ; zipmap keys/vals no good because limits us to size of incoming args.
                        defaults-map #_arg-map))
         (apply zipmap (map (fn [coll]
                             (vals (sort-by #(name (first %))
                                            coll)))
              [arg-map defaults-map]))))

(defn default "Get default from combined defaults-arg map"
 [m k]
 (:default (m k)))
(defn arg "Get arg from combined defaults-arg map"
 [m k]
 (:arg (m k)))

; make macro to generate shortcuts...
; [lfo-p build-oscillated param, step-p build-step-param, color-p build-color-param
; spatial-p build-spatial-param, aim-p build-aim-param, pt-p build-pan-tilt-param
; dir-from-pt-p build-direction-param-from-pan-tilt]

(defn afx [] (show/active-effect-keys *show*))

(defn get-lfo
 [lfo-name]
 (resolve (symbol "afterglow.effects.oscillators" lfo-name)))

(defn patch! ;fix/wrap this to grab defs straight from cfg...
 [k f universe offset & [[x y z] args]]
 (let [pos [:x (or x 0.0) :y (or y 0.0) :z (or z 0.0)]
       args (filter #(not= nil %) (merge pos args))]
  ;; (println pos) (println args)
  (apply show/patch-fixture! k (f) universe offset args)))

(defn patch-group! "FIXME"
 [group f universe offset & [pos args]]
  (map #(show/patch-fixture! %1 f universe %2)))

(defn patch-cfg!
 []
 (print "->  from config...")
 (doseq [[group-key group-data] (cfg :fixtures :defs)]
   (print "\n     ")
   (loop [i 0
          offset (or (:offset group-data) 1)]
     ;(tolglow.debug/det
     (let [fixture ((:list group-data) i)
           k (key-str group-key "-" (inc i))
           universe (or (:universe group-data) (+ i (:start-universe group-data)))
           [f x y z & args] fixture
           f-in-fixtures #(ns-resolve 'tolglow.fixtures (symbol (name %)))
           fix-map (or
                    (and (map? f) f)
                    (and (fn? f) (f))
                    (and (vector? f)
                         (if (> (count f) 1)
                          (apply (try (eval (first f))
                                      (catch Throwable t
                                       (f-in-fixtures (first f))))
                          ;; (apply (or (try (eval (first f))
                          ;;             (catch Throwable t)) (f-in-fixtures (first f)))
                                 (rest f)) ;now only supports quoted, if has args...
                          ((first f))))
                    (and (keyword? f) (f tolglow.config/fixture-types))
                    (and (fn? (try (eval f) (catch Throwable t))) ((eval f)))
                    ;; ((ns-resolve 'tolglow.fixtures (symbol (name f))))) ;works for both quoted syms and keywords without luck from fixture-types config
                    ((f-in-fixtures f))) ;works for both quoted syms and keywords without luck from fixture-types config
           ;; fix-map (cond
           ;;          (map? f) f
           ;;          (fn? f) (f)
           ;;          (vector? f) (if (> (count f) 1) (apply (first f) (rest f))
           ;;                       ((first f)))
           ;;          (keyword? f) (f tolglow.config/fixture-types)
           ;;          (fn? (try (eval f) (catch Throwable t))) ((eval f))
           ;;          :else ((ns-resolve 'tolglow.fixtures (symbol (name f)))))
           ;; fix-map (if (map? f) f
           ;;          (try ((eval f)) (catch Throwable t
           ;;                          ((ns-resolve 'tolglow.fixtures (symbol (name f)))))))
           jump (if (:universe group-data)
                 (apply max (map :offset (:channels fix-map))) ;XXX also count heads... also don't count for strips...
                 0)
           pos (reduce into [] (filter (fn [[_ v]] (some? v)) {:x x :y y :z z}))
           args (into pos args)]
     (print k #_f "\t")
     (apply patch-fixture! k fix-map universe offset (eval args) #_args)
     (when (< (inc i) (count (:list group-data)))
       (recur (inc i) (+ offset jump))))))
 (println))

(defn reset-fixture-binds! "Reset atoms containing maps of the different (super-fixture-group) fixture types"
 []
 (println "Resetting binds for fixture groups...")
 (doseq [k (keys (cfg :fixtures :types))]
  (let [group (cfg :fixtures :types k)
        bind (at :fixture-type k)]
         (when bind
          (reset! bind (mapcat fixtures-named (get group :groups)))
          (println k "\t" (count @bind) "fixtures")))))


;; XXX XXX yoyo XXX solution to controlling color lightness from push: touch both encoders = touchstrip becomes lightness (curr controls both hue and sat instead heh)
;; send DMX ctrl chs with flags for pixels in stream, esp for when multiple streams overdubbed. Then can switch htp/lowtp/blend modes etc per pixel instead of stream - and barely takes up data!!!
;; ^^ same flag map for the ctrl chs, for easy override dimmer, cancel out unwanted rotation or whatnot.  fuck yeah. FUCK YEAH.
(defn push-toggle "Take control of Push2 from Ableton Live, or give it back" ;; needs to create/destroy a virtual port Live is pointed to. Find shell thing to do it until I figure out CoreMidi4J?  using copperlan for now..
 []
 ;; (show/register-grid-controller) ;see if can get quicker bind if use this straight instead of autobind?0
 (let [push (at :controllers :push)]
  (if (nil? @push)
    (reset! push (ct/bind-to-show *show* "Ableton Push" #_:refresh-interval #_ms))
   (do
    (ct/deactivate push)
    (reset! push nil)))))

