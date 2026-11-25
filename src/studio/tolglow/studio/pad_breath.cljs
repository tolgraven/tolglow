(ns tolglow.studio.pad-breath
  "Running cue pads breathe with the projected transport, like playing clips
  on a Push session view. One shared clock writes the beat phase to the cue
  grid as CSS custom properties; each pad's breath is pure CSS layered over
  its live preview colour. There are no per-pad timers.

  The only musical-time arithmetic here interpolates the latest projected
  transport sample (beat, bpm, playing?) between stream frames. A seek or
  restart arrives as a new transport generation and re-anchors immediately."
  (:require [reagent.core :as r]
            [tolglow.studio.events :as events]))

(def max-extrapolation-ms
  "Hold the last projected beat when the stream has been silent this long,
  rather than inventing transport progress the server has not confirmed."
  2000)

(def release-bright-fraction
  "Afterglow's Push grid blinks ending cues bright for the first 40% of each
  beat (`snapshot-beat-phase` <= 0.4) and dim for the rest. Keep that phase."
  0.4)

(defn sample-key
  "Identity of one projected transport sample. Unrelated re-renders reuse the
  same key and must not re-anchor the clock, which would drift it forwards."
  [transport generation]
  [(or generation 0) (:time-ms transport) (:beat transport) (:bpm transport) (boolean (:playing? transport))])

(defn next-anchor
  "Anchor the clock to a newly received transport sample at local time `now-ms`.
  Returns `previous` unchanged when the sample has not changed."
  [previous transport generation now-ms]
  (let [key (sample-key transport generation)]
    (if (and previous (= key (:key previous)))
      previous
      {:key key :generation (or generation 0)
       :beat (or (:beat transport) 0) :bpm (or (:bpm transport) 120)
       :playing? (boolean (:playing? transport)) :at now-ms})))

(defn beat-at
  "Interpolated transport beat at local time `now-ms`."
  [{:keys [beat bpm playing? at]} now-ms]
  (if (and playing? (number? bpm) (pos? bpm))
    (+ beat (* (min max-extrapolation-ms (max 0 (- now-ms at))) (/ bpm 60000)))
    beat))

(defn breath-phase
  "Fraction of the current beat in [0, 1); 0 is the downbeat, as Afterglow's
  `snapshot-beat-phase`."
  [anchor now-ms]
  (let [beat (beat-at anchor now-ms) phase (- beat (js/Math.floor beat))]
    (if (js/Number.isFinite phase) phase 0)))

(defn levels
  "Shared drive values for one phase. `breath` is a raised cosine peaking on
  the beat; `blink` is Afterglow's ending-cue blink; `flash` is the
  half-beat blink Push uses for clips waiting to launch."
  [phase]
  {:breath (* 0.5 (+ 1 (js/Math.cos (* 2 js/Math.PI phase))))
   :blink (if (<= phase release-bright-fraction) 1 0)
   :flash (if (< (- (* 2 phase) (js/Math.floor (* 2 phase))) 0.5) 1 0)})

(defn- phase-name [instance] (some-> (:phase instance) events/wire-id))

(defn pad-state
  "How a pad's cue is playing, from its projected instances and queued launches.
  Returns a set of :active (any instance), :running, :held, :releasing and
  :queued."
  [instances queued?]
  (let [phases (map phase-name instances)
        running (filter #(= "running" (phase-name %)) instances)]
    (cond-> #{}
      (seq instances) (conj :active)
      (seq running) (conj :running)
      (some #(= "held" (some-> (:launch-mode %) events/wire-id)) running) (conj :held)
      (some #{"releasing"} phases) (conj :releasing)
      queued? (conj :queued))))

(defn pad-classes
  "CSS classes for a pad. `active` keeps the existing lit styling; the others
  select the breath, held, release blink and queued flash layers."
  [state]
  (str (when (:active state) "active ")
       (when (:running state) "running ")
       (when (:held state) "held ")
       (when (and (:releasing state) (not (:running state))) "releasing ")
       (when (:queued state) "queued ")))

(defn animated? [state] (boolean (some state [:running :releasing :queued])))

;; One clock per page. Only the mounted grid drives it.
(defonce ^:private clock (atom {:anchor nil :el nil :raf nil :animate? false}))

(defn- reduced-motion? []
  (and (exists? js/matchMedia) (.-matches (js/matchMedia "(prefers-reduced-motion: reduce)"))))

(defn- write! [el {:keys [breath blink flash]} phase]
  (let [style (.-style el)]
    (.setProperty style "--beat-phase" (.toFixed phase 4))
    (.setProperty style "--breath" (.toFixed breath 4))
    (.setProperty style "--release-blink" blink)
    (.setProperty style "--queued-flash" flash)))

(defn- frame! [timestamp]
  (let [{:keys [anchor el animate?]} @clock]
    (swap! clock assoc :raf nil)
    (when (and el anchor)
      (let [phase (breath-phase anchor timestamp)]
        (write! el (levels phase) phase)))
    (when (and el animate? (:playing? anchor) (not (.-hidden js/document)))
      (swap! clock assoc :raf (js/requestAnimationFrame frame!)))))

(defn- sync! []
  (let [{:keys [raf el animate? anchor]} @clock
        reduced? (reduced-motion?)
        run? (and el animate? anchor (:playing? anchor) (not (.-hidden js/document)) (not reduced?))]
    (cond
      (and run? (nil? raf)) (swap! clock assoc :raf (js/requestAnimationFrame frame!))
      (and (not run?) raf) (do (js/cancelAnimationFrame raf) (swap! clock assoc :raf nil)))
    ;; A paused transport holds one still frame at its phase. Reduced motion
    ;; writes nothing; its CSS shows a steady running indicator instead.
    (when (and el anchor (not run?) (not reduced?)) (frame! (.now js/performance)))))

(defn- grid-element [] (.getElementById js/document "cue-grid"))

(defn- update-clock! [transport generation animate?]
  (swap! clock (fn [state]
                 (assoc state :el (grid-element) :animate? animate?
                        :anchor (next-anchor (:anchor state) transport generation (.now js/performance))))))

(defn- stop-clock! []
  (when-let [raf (:raf @clock)] (js/cancelAnimationFrame raf))
  (swap! clock assoc :el nil :raf nil :animate? false))

(defn clock-driver
  "Invisible sibling of the cue grid. `transport` and `generation` come from
  the show projection; the grid element receives the CSS properties. It is
  mounted once per grid, so the page has a single animation frame loop."
  [_ _ _]
  (let [visibility #(sync!)
        apply! (fn [this]
                 (let [[_ transport generation animate?] (r/argv this)]
                   (update-clock! transport generation animate?)
                   (sync!)))]
    (r/create-class
     {:display-name "pad-breath-clock"
      :component-did-mount (fn [this]
                             (.addEventListener js/document "visibilitychange" visibility)
                             (apply! this))
      :component-did-update (fn [this _] (apply! this))
      :component-will-unmount (fn [_]
                                (.removeEventListener js/document "visibilitychange" visibility)
                                (stop-clock!))
      :reagent-render (fn [_ _ _] nil)})))
