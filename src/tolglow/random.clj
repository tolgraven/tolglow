(ns tolglow.random "Functions to drive show forward by doing random shite"
  (:require [afterglow
             [channels :as chan]
             [rhythm :as rhythm :refer [metro-snapshot metro-start metronome snapshot-bar-phase snapshot-beat-phase snapshot-beat-within-bar snapshot-down-beat?]]
             [show :as show :refer [add-effect! all-fixtures end-effect! fixtures-named get-variable set-cue! set-variable!]]
             [show-context :refer [*show* set-default-show! with-show]]]
            [afterglow.effects.cues :as cues]
            [clojure.string :as string :refer [capitalize upper-case]]
            [tolglow
             [config :as config :refer [cfg]]
             [cue :as cue]
             [util :as util]]))

(defn ize-colors "Set new colors for all, or specified, cues/color show vars"
;;  [& {:keys [hue-min hue-max sat-min sat-max] :or {sat-min 20, sat-max 70}}]
 [& {:keys [hue sat lightness]
     :or {hue (rand 360) sat {:min 30 :max 70} lightness {:min 35 :max 65}}}] ;or like, pass a single value and optionally range around (so instead :min 30 :max 70, :sat 50 :sat-range 40)
 ) ;might have to make compatible by setting such color vars globally through a helper

(defn ize-show-var "Set an appropriate number for cue show var, within :min :max"
 [show-key & m]
 (:meta (:variables *show*)))

#_(defonce temp-vars "Put any var/cue var with temporary duration. Key, orig value, randomized value, duration, metronome snapshot..."
 (atom []))

(defn ize-cue-vars!
;;  [& {:keys [cue-prefix var-type var-key volatility per-cue-limit duration show] :or {volatility 0.5, show *show*}}] ;add metadata to vars setting not just min-max but like "reasonable range", scale/clamp-fn,
 [& {:keys [cue-prefix var-id volatility per-cue-limit duration show] :or {volatility 0.5, show *show*}}] ;add metadata to vars setting not just min-max but like "reasonable range", scale/clamp-fn,
;;  (let [effects (:meta @(:active-effects show))
 (let [effects (:meta @(:active-effects *show*))
       get-max #(+ (:current % (:start %)) (* volatility (- (:max %) (:current %)))) ] ;volatility 1.0 = full random, 0.0 = no change. So 0.5 if [0 127 255] = 64 192...       ]
  (doseq [fx effects, m (zipmap (sort-by #(name (first %)) (fx :variables))
                                (sort-by #(:key %) (-> fx :cue :variables)))]
   ;; (let [[[_ k] v] m]
   (let [k (second (first m))
         v (second m)]
         ;; XXX var-key (ie :alpha) actually comes as str so make sure test for all kinds
          ;; (when (or (not var-type) (not var-key) (= var-type (:type v)) (= var-key (:key v))) ;(type v)))
          (when (or (not var-id) (= var-id (:type v)) (= var-id (:key v))) ;(type v)))
           (let [old-val (show/get-variable k)
                new-val (util/random-in-range (:min v (:start v)) (:max v))] ;bool and color lack min/max...
    ;; (println k " /  " v " ->" new-val)
    (set-variable! k new-val)))

    #_(swap! temp-vars conj {:key k :duration duration :was old-val :val new-val
                           :snapshot (metro-snapshot (:metronome *show*))}))))) ;short term: :avoid :min larger than :max, weird beats/cycles combos, ?

;STUFF:
;(snapshot-cue-variables cue when-id)
;(set-cue-variable! cue var-spec value)
;(get-cue-variable cue var-spec :with-id)
#_(defn ize-effect-vars
 [& {:keys [effects cues] :or {effects (:meta @(:active-effects *show*))}}]
  (def effects (:meta @(:active-effects *show*)))
  (def var-maps (map :variables effects))
 (let [cues (map :start (flatten (map :variables (map :cue effects))))
       [vms varspecs] (map :variables [effects cues])
       show-key (reduce #(into %1 (vals %2)) [] vms)]
  (println show-key)))

(defonce auto-running (atom []))

(defn run-cue! ;would this like set up a one-step chase or how else control duration of arbitrary cue?
 [& {:keys [categories ;could be like, :self-running (lfo/step/random - not good to randomize too much)
                                     ; :static, :velocity...
            from-page
            row column ;for now so eg strobe random thing...
            randomize-vars? duration ;hook into render loop and check whether time to remove? or just code-cue running a cleanup fn at intervals...
            duration-interval]
     :or {duration-interval :bar}}]
 (let [;page (or from-page [0 0])
       ;; bound-x (cond column 0, from-page 7, (@(-> *show* :cue-grid :dimensions) 0))
       ;; bound-y (cond row 0, from-page 7, (@(-> *show* :cue-grid :dimensions) 1))
       bounds (map #(cond %1 0, from-page 7, :else (@(-> *show* :cue-grid :dimensions) %2)) [column row] [0 1])
       ;; bounds (if from-page [8 8]))
      ;; xy [(rand-int 8) (rand-int 8)]
      xy (map #(+ (or %1 0) (rand-int %2)) [column row] bounds)
      _ (println bounds xy )
      ;; pos (apply cue/pos-for-page :pos xy (when from-page [:page from-page]))
      pos (apply cue/pos-for-page (into [:pos xy] (when from-page [:page from-page])))
      _ (println bounds xy pos)
      new-cue (apply cue/launch! pos)]
  ;; (println new-cue pos)
  (when new-cue (swap! auto-running conj {:pos pos :duration 4}))))

(defn crap []
 (run-cue! :from-page [0 0])
;;  (clean!)
 auto-running)

(defn get-cue [pos] (apply show/find-cue-grid-active-effect *show* pos))

(defn clean! "Run every beat checking whether any auto-launched cues have expired..."
 ;XXX should be general and restore other types of temp env changes. Vars, inactivated cues, etc
 [& {:keys [f] :or {f (fn [k id] (show/end-effect! k :id id))}}]
 (doseq [interval [:beat :bar :phrase]
         {:keys [pos duration]} @auto-running]
  (when-let [started (:started ((get-cue pos) 1))]
   (let [start (interval started)
        snap (metro-snapshot (:metronome *show*))
        curr (interval snap)]
    (if (< (+ duration start) curr)
      (let [k (:key ((get-cue pos) 0))
            id (:id ((get-cue pos) 1))]
        (println k id)
        (f k id)
        (swap! auto-running (fn [v] (remove #(not= pos %) v)))))))))

(defn restore-temp-var-changes "Restore any temporarily randomized cue vars that have expired to their inital values"
 []
 )


(defn do-random-action
 []) ;runs, kills random registered(?)/cue-grid shit, updates random show var off list, etc

(defn get-random-fixture
 [& {:keys [with-channel multiple? group? stop-after allow-latest? prefer-latest?]}])

(defn get-random-head "calls get-random-fixture, then iterates over heads? which is good since so many more ledstrip heads like..."
 [])

(defn kill-random-cue
 [save-vars? cue-filter])

