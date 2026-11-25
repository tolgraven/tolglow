(ns tolglow.studio.pad-breath
  "Running cue pads breathe with the projected transport, like playing clips
  on a Push session view. The breath, release blink and queued flash are CSS
  animations on each pad, layered over its live preview colour. There is no
  per-frame JavaScript and no per-pad timer.

  One clock per page aligns those animations to the transport. Each
  projected transport sample gives the local time of the last downbeat. The
  clock holds every animation's start time on the document timeline at the
  least-delayed recent sample, so that its progress equals the beat phase.
  The only musical-time arithmetic here interpolates the latest projected
  sample (beat, bpm, playing?). A seek or restart arrives as a new transport
  generation and re-anchors immediately."
  (:require [reagent.core :as r]
            [tolglow.studio.events :as events]))

(def max-extrapolation-ms
  "Show the still indicator when the stream has been silent this long, rather
  than inventing transport progress the server has not confirmed."
  2000)

(def release-bright-fraction
  "Afterglow's Push grid blinks ending cues bright for the first 40% of each
  beat (`snapshot-beat-phase` <= 0.4) and dim for the rest. The
  `pad-release-blink` keyframes in console.css keep that phase."
  0.4)

(def animation-names
  "CSS animations this clock aligns. Each repeats once per beat, or twice
  for the queued flash, so one beat-aligned start time serves them all."
  #{"pad-breath" "pad-breath-held" "pad-release-blink" "pad-queued-flash" "pad-queued-dot"})

(def sync-tolerance-ms
  "Leave the pad animations alone while the least-delayed recent sample is this
  close to the start time they hold. The document timeline advances once per
  display frame, so a tighter bound would follow frame quantisation."
  20)

(def latency-window-ms
  "How long a sample counts toward the least-delayed estimate. The stream
  delivers about thirty samples a second, so a second holds enough of them to
  include one that arrived promptly."
  1000)

(def resync-ms
  "A sample this far from the held start time is a jump in the transport, such
  as a clock source correcting its phase, rather than receive jitter. It is
  followed at once."
  100)

(defn sample-key
  "Identity of one projected transport sample. Unrelated re-renders reuse the
  same key and must not re-anchor the clock, which would drift it forwards."
  [transport generation]
  [(or generation 0) (:time-ms transport) (:beat transport) (:bpm transport) (boolean (:playing? transport))])

(defn- tempo [bpm] (when (and (number? bpm) (js/Number.isFinite bpm) (pos? bpm)) bpm))

(defn next-anchor
  "Anchor the clock to a newly received transport sample at local time `now-ms`.
  Returns `previous` unchanged when the sample has not changed. A sample
  without a usable tempo keeps `:bpm` nil; the pads then stay still."
  [previous transport generation now-ms]
  (let [key (sample-key transport generation)]
    (if (and previous (= key (:key previous)))
      previous
      {:key key :generation (or generation 0)
       :beat (or (:beat transport) 0) :bpm (tempo (:bpm transport))
       :playing? (boolean (:playing? transport)) :at now-ms})))

(defn beat-at
  "Interpolated transport beat at local time `now-ms`."
  [{:keys [beat bpm playing? at]} now-ms]
  (if (and playing? bpm)
    (+ beat (* (min max-extrapolation-ms (max 0 (- now-ms at))) (/ bpm 60000)))
    beat))

(defn breath-phase
  "Fraction of the current beat in [0, 1); 0 is the downbeat, as Afterglow's
  `snapshot-beat-phase`."
  [anchor now-ms]
  (let [beat (beat-at anchor now-ms) phase (- beat (js/Math.floor beat))]
    (if (js/Number.isFinite phase) phase 0)))

(defn beat-duration-ms
  "Beat length rounded to 0.1 ms, so tempo jitter from a Link source does not
  rewrite --beat-duration and restyle the grid."
  [{:keys [bpm]}]
  (when bpm (/ (js/Math.round (/ 600000 bpm)) 10)))

(defn start-time
  "Document-timeline start time that makes a once-per-beat animation's
  progress equal the beat phase: the local time of the anchor's last
  downbeat. Half-beat animations share it."
  [{:keys [beat at] :as anchor}]
  (when-let [duration (beat-duration-ms anchor)]
    (- at (* (- beat (js/Math.floor beat)) duration))))

(defn mode
  "`:live` animates the pads; `:still` shows the steady indicator. Pads are
  still when nothing on the page animates, the transport is paused or has no
  tempo, the stream is stale, the page is hidden or motion is reduced."
  [anchor now-ms {:keys [animate? hidden? reduced? stale?]}]
  (if (and animate? anchor (:playing? anchor) (:bpm anchor) (not hidden?) (not reduced?) (not stale?)
           (< (- now-ms (:at anchor)) max-extrapolation-ms))
    :live
    :still))

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

;; The clock owns its boundary through a host map, so tests can supply one.
;; Host functions: :now, :grid, :hidden?, :reduced?, :set-timeout,
;; :clear-timeout, :animations (grid -> breath animations), :write! (grid,
;; mode, duration-ms or nil) and :watch (callbacks -> unwatch function).

(defn controller [host]
  (atom {:host host :anchor nil :animate? false :timer nil :written nil :unwatch nil}))

(defn phase-offset-ms
  "Signed distance in ms from start time `b` to start time `a` for animations
  that repeat every `period` ms, in [-period/2, period/2). Start times a whole
  number of beats apart show the same phase, so their offset is zero."
  [a b period]
  (let [half (/ period 2)]
    (- (mod (+ (- a b) half) period) half)))

(defn next-lock
  "The start time the pad animations should hold after the sample `anchor`.
  Stream and frame latency only ever delay a sample, so among recent samples
  the one with the earliest downbeat is the closest to the transport. The
  lock moves to that least-delayed sample once it is more than the tolerance
  away and holds through samples that were delayed longer. A new tempo, a new
  transport generation or a jump beyond receive jitter starts a fresh
  estimate from the sample.

  `lock` is nil or a map of :start, :duration, :generation, the :key of its
  latest sample and :recent, a vector of [local-ms offset-from-start] for
  each sample in the window."
  [lock {:keys [at generation key] :as anchor}]
  (let [duration (beat-duration-ms anchor)
        target (start-time anchor)
        offset (when (and lock (= duration (:duration lock)) (= generation (:generation lock)))
                 (phase-offset-ms target (:start lock) duration))]
    (cond
      (and lock (= key (:key lock))) lock

      (or (nil? offset) (> (js/Math.abs offset) resync-ms))
      {:start target :duration duration :generation generation :key key :recent [[at 0]]}

      :else
      (let [recent (conj (filterv #(< (- at (first %)) latency-window-ms) (:recent lock)) [at offset])
            least (apply min (map second recent))]
        (if (> (js/Math.abs least) sync-tolerance-ms)
          (assoc lock :key key :start (+ (:start lock) least)
                 :recent (mapv (fn [[sampled-at from-start]] [sampled-at (- from-start least)]) recent))
          (assoc lock :key key :recent recent))))))

(def ^:private held-within-ms
  "An animation holds the locked start time when it reads back this close to
  it. A new animation starts wherever the browser created it and is moved."
  1)

(defn- align! [host el {:keys [start duration]}]
  (doseq [^js animation ((:animations host) el)]
    (let [current (.-startTime animation)]
      (when (or (nil? current) (> (js/Math.abs (phase-offset-ms current start duration)) held-within-ms))
        (set! (.-startTime animation) start)))))

(declare refresh!)

(defn- expire!
  "The stale-stream timer for the sample `key` ran. The timer decides, not a
  second clock comparison: the document timeline advances once per display
  frame, so it can still read just under the limit when the timeout runs."
  [ctl key]
  (swap! ctl (fn [state] (cond-> (assoc state :timer nil :timer-key nil)
                           (= key (:key (:anchor state))) (assoc :expired key))))
  (refresh! ctl))

(defn refresh!
  "Apply the current mode to the grid. Writes the grid only when its mode or
  beat duration changed. While live, each new sample updates the lock and the
  animations are checked against it; new pad animations also align on
  animationstart. Keeps a single stale-stream timer per anchor. A sample whose
  timer ran stays stale until the next sample replaces it."
  [ctl]
  (let [{:keys [host anchor animate? timer timer-key written aligned expired lock]} @ctl
        now ((:now host))
        el ((:grid host))
        stale? (and (some? expired) (= expired (:key anchor)))
        next-mode (mode anchor now {:animate? animate? :hidden? ((:hidden? host)) :reduced? ((:reduced? host))
                                    :stale? stale?})
        duration (when (= :live next-mode) (beat-duration-ms anchor))
        fresh? (not= el (:el written))
        write-duration (when (and duration (or fresh? (not= duration (:duration written)))) duration)
        wanted {:el el :mode next-mode :duration (or duration (when-not fresh? (:duration written)))}
        live? (and el (= :live next-mode))
        alignment (when live? [el (:key anchor) duration])
        lock (when live? (next-lock lock anchor))
        keep-timer? (and live? timer (= timer-key (:key anchor)))
        stale-in (when (and live? (not keep-timer?)) (max 0 (- (+ (:at anchor) max-extrapolation-ms) now)))]
    (when (and timer (not keep-timer?)) ((:clear-timeout host) timer))
    (when (and el (not= wanted written))
      ((:write! host) el next-mode write-duration))
    (when (and live? (not= alignment aligned)) (align! host el lock))
    (let [key (:key anchor)
          timer (cond keep-timer? timer
                      stale-in ((:set-timeout host) #(expire! ctl key) stale-in))]
      (swap! ctl assoc :timer timer :timer-key (when timer key) :mode next-mode
             :written (when el wanted) :aligned alignment :lock lock :expired (when stale? expired)))))

(defn sample!
  "Record the latest projection and refresh. Reads the clock before updating
  state; the state update itself is pure."
  [ctl transport generation animate?]
  (let [now ((:now (:host @ctl)))]
    (swap! ctl (fn [state] (assoc state :animate? animate?
                                  :anchor (next-anchor (:anchor state) transport generation now))))
    (refresh! ctl)))

(defn align-now!
  "A pad animation just started; bring it into phase."
  [ctl]
  (let [{:keys [host lock mode]} @ctl
        el ((:grid host))]
    (when (and el lock (= :live mode)) (align! host el lock))))

(defn attach! [ctl]
  (let [unwatch ((:watch (:host @ctl)) {:changed #(refresh! ctl) :animation-started #(align-now! ctl)})]
    (swap! ctl assoc :unwatch unwatch)))

(defn detach! [ctl]
  (let [{:keys [host timer unwatch]} @ctl]
    (when timer ((:clear-timeout host) timer))
    (when unwatch (unwatch))
    (swap! ctl assoc :timer nil :timer-key nil :unwatch nil :mode nil :written nil :aligned nil :lock nil
           :animate? false)))

(defn- motion-query []
  (when (exists? js/matchMedia) (js/matchMedia "(prefers-reduced-motion: reduce)")))

(defn- listen! [^js target event f]
  (cond (.-addEventListener target) (do (.addEventListener target event f) #(.removeEventListener target event f))
        (.-addListener target) (do (.addListener target f) #(.removeListener target f))))

(def dom-host
  {:now #(or (some-> js/document .-timeline .-currentTime) (.now js/performance))
   :grid #(.getElementById js/document "cue-grid")
   :hidden? #(.-hidden js/document)
   :reduced? #(boolean (some-> (motion-query) .-matches))
   :set-timeout (fn [f ms] (js/setTimeout f ms))
   :clear-timeout #(js/clearTimeout %)
   :animations (fn [^js el]
                 (if (.-getAnimations el)
                   (filter #(contains? animation-names (.-animationName ^js %))
                           (array-seq (.getAnimations el #js {:subtree true})))
                   []))
   :write! (fn [^js el mode duration]
             (.setAttribute el "data-breath" (name mode))
             (when duration (.setProperty (.-style el) "--beat-duration" (str duration "ms"))))
   :watch (fn [{:keys [changed animation-started]}]
            (let [started #(when (contains? animation-names (.-animationName ^js %)) (animation-started))
                  stops (cond-> [(listen! js/document "visibilitychange" changed)
                                 (listen! js/document "animationstart" started)]
                          (motion-query) (conj (listen! (motion-query) "change" changed)))]
              #(doseq [stop stops] (when stop (stop)))))})

(defn clock-driver
  "Invisible sibling of the cue grid. `transport` and `generation` come from
  the show projection; `animate?` says whether the page has a pad to animate.
  It is mounted once per grid, so the page has one clock."
  [_ _ _]
  (let [ctl (controller dom-host)
        apply! (fn [this]
                 (let [[_ transport generation animate?] (r/argv this)]
                   (sample! ctl transport generation animate?)))]
    (r/create-class
     {:display-name "pad-breath-clock"
      :component-did-mount (fn [this] (attach! ctl) (apply! this))
      :component-did-update (fn [this _] (apply! this))
      :component-will-unmount (fn [_] (detach! ctl))
      :reagent-render (fn [_ _ _] nil)})))
