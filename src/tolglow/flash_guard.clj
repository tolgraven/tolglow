(ns tolglow.flash-guard
  "Photosensitivity flash guard on delivered output (#836).

  The show owner runs the guard on each delivered frame, after output
  response and budgeting and before activity scoring. It counts large-area
  flashes of the heads it guards and, once the last second of wall-clock time
  already holds the permitted number, holds the next large-area rise below
  the flash threshold until the window has room again. It scales emission
  only, through Afterglow's emission scaling, so pan, tilt, motor speed,
  shutter and strobe channels, gobos, fog and every other non-emission channel
  pass unchanged, and coarse/fine channels keep their encoding. It reads
  committed DMX and never evaluates or advances an effect.

  Counting follows Harding: each head tracks its own trough and peak, and a
  head makes a transition when its intensity rises at least `:min-change`
  above its last trough, with no leak, so a ramp counts like a step. Heads
  whose transitions fall within half a period at the limit add up; a flash
  is counted when together they reach `:area` of the rig. A head's intensity
  is measured against its own maximum at the colour it shows. Its share of
  the rig is the larger of its luminous share and its visual share: one unit
  per fixture (split across a strip's pixels), at least two for beams,
  blinders and strobe-capable fixtures, or a declared `:flash-area`.
  Saturated red counts separately against `:red-limit-hz`. Strobe functions
  free-running in a fixture are budgeted with worst-case phase: the sum of
  share times rate over strobing heads stays at or under `:area` times the
  limit, cutting the largest first.

  By default it guards light an agent influenced (tolglow.flash-guard.influence);
  `:scope :show` guards everything. An operator `:grant` raises the agent
  limit, never the red limit, for bursts of at most `:burst-beats` beats.
  This is a mitigation, not a certification that a show is safe. Defaults
  and how to change them: docs/flash-guard.md."
  (:require [afterglow.render.emission :as emission]
            [afterglow.runtime.events :as events]
            [clojure.string :as str]
            [tolglow.flash-guard.influence :as influence]
            [tolglow.lighting-activity :as activity]
            [tolglow.lighting-capacity :as capacity]))

(def presets
  "Named limits. `:broadcast` (the default) is the three-flashes-a-second line
  of ITU-R BT.1702 and WCAG 2.3.1; `:club` the 4 Hz figure for venue strobes
  run by a person who can see the room. Both treat saturated red at 3 Hz."
  {:club {:limit-hz 4.0 :red-limit-hz 3.0}
   :broadcast {:limit-hz 3.0 :red-limit-hz 3.0}})

(def defaults
  "Settings under [:settings :flash-guard]. `:limit-hz` and `:red-limit-hz`,
  when present, override the preset."
  {:enabled? true :scope :agent :preset :broadcast :area 0.25 :min-change 0.1
   :sources influence/default-sources :grant nil})

(def grant-caps
  "The most an operator grant may allow."
  {:limit-hz 12.0 :burst-beats 8})

(def ^:private window-ms 1000.0)

(def ^:private max-hz 60.0)

(defn- rate? [x] (and (number? x) (Double/isFinite (double x)) (< 0 x) (<= x max-hz)))

(defn- grant? [grant]
  (or (nil? grant)
      (and (map? grant) (every? #{:limit-hz :burst-beats :epoch} (keys grant))
           (rate? (:limit-hz grant)) (<= (:limit-hz grant) (:limit-hz grant-caps))
           (number? (:burst-beats grant)) (< 0 (:burst-beats grant)) (<= (:burst-beats grant) (:burst-beats grant-caps))
           (or (nil? (:epoch grant)) (integer? (:epoch grant))))))

(defn settings
  "Validate a complete flash guard settings map and return it."
  [options]
  (let [{:keys [enabled? scope preset area min-change sources limit-hz red-limit-hz grant]} (merge defaults options)]
    (when-not (and (map? options)
                   (every? (set (into (keys defaults) [:limit-hz :red-limit-hz])) (keys options))
                   (boolean? enabled?) (#{:agent :show} scope) (contains? presets preset)
                   (number? area) (< 0 area) (<= area 1)
                   (number? min-change) (< 0 min-change) (<= min-change 1)
                   (vector? sources) (every? keyword? sources)
                   (or (nil? limit-hz) (rate? limit-hz)) (or (nil? red-limit-hz) (rate? red-limit-hz))
                   (grant? grant))
      (throw (ex-info (str "Flash guard settings take :enabled? (boolean), :scope (:agent or :show), "
                           ":preset (:broadcast or :club), :limit-hz and :red-limit-hz (0 to 60 Hz), "
                           ":area and :min-change (0 to 1), :sources (adapter keywords) "
                           "and :grant (nil or {:limit-hz up to 12 :burst-beats up to 8})")
                      {:settings options})))
    options))

(defn operator-source?
  "Only an operator surface may change the guard. A sensor is not an operator."
  [human-source? source]
  (and (map? source) (human-source? source) (not= :sensor (:adapter source))))

(defn edit-settings
  "Merge an operator edit. A new grant is stamped with the show epoch and
  lapses when the show restarts or another show is loaded."
  [current edit epoch]
  (settings (cond-> (merge current edit)
              (some? (:grant edit)) (assoc-in [:grant :epoch] epoch))))

(defn- resolved-limits [{:keys [preset limit-hz red-limit-hz]}]
  [(double (or limit-hz (get-in presets [preset :limit-hz])))
   (double (or red-limit-hz (get-in presets [preset :red-limit-hz])))])

(defn tighten-loaded
  "Settings from a saved show or profile may only tighten the defaults: the
  guard stays on, limits and areas take the stricter of the two, the agent
  sources keep the defaults, and no grant survives a load. An operator can
  loosen them again in the session."
  [saved]
  (if (nil? saved)
    saved
    (let [saved (try (settings (dissoc saved :grant)) (catch clojure.lang.ExceptionInfo _ {}))
          opts (merge defaults saved)
          [base red] (resolved-limits defaults)
          [limit red-limit] (resolved-limits opts)]
      (cond-> {:limit-hz (min base limit) :red-limit-hz (min red red-limit)
               :area (min (:area defaults) (:area opts)) :min-change (min (:min-change defaults) (:min-change opts))
               :sources (vec (distinct (into (vec (:sources defaults)) (:sources opts))))}
        (= :show (:scope opts)) (assoc :scope :show)))))

(defn loaded-model
  "A model from a saved show, profile or checkpoint: its flash guard settings
  may only tighten, and agent influence from another session is dropped."
  [model]
  (cond-> model
    (contains? (:settings model) :flash-guard) (update-in [:settings :flash-guard] tighten-loaded)
    (get-in model influence/path) (update :extensions dissoc :tolglow.flash-guard/influence)))

(defn config
  "The resolved guard for this model, or nil when it is off."
  [model]
  (let [{:keys [enabled? grant] :as opts} (merge defaults (get-in model [:settings :flash-guard]))]
    (when enabled?
      (let [[base red] (resolved-limits opts)]
        (assoc opts :limit-hz base :red-limit-hz red
               :sources (set (:sources opts))
               ;; A grant applies in the epoch it was given and only raises the limit.
               :grant (when (and grant (= (:epoch grant) (:epoch model)) (> (:limit-hz grant) base))
                        grant))))))

(defn- live-burst? [history]
  (let [{:keys [beats cooldown]} (:burst history)]
    (or (some? beats) (and cooldown (pos? cooldown)))))

(defn active?
  "Whether the guard has work this frame: it covers the whole show, light an
  agent influenced is running, or its window or a grant burst or cooldown is
  still live."
  [model history]
  (when-let [{:keys [scope sources]} (config model)]
    (let [inf (influence/summary model)]
      (boolean (or (= :show scope) (influence/show-wide? inf)
                   (some #(influence/attributed? sources inf %) (vals (:instances model)))
                   (seq (:rises history)) (seq (:red-rises history)) (live-burst? history))))))

;; ------------------------------------------------------------------ rig

(defn- strobe-function? [{:keys [type]}]
  (and (keyword? type) (str/includes? (name type) "strobe")))

(defn- strobe-channels
  "Every compiled channel of a head that can select a strobe: any channel
  with a strobe-typed function range, and a plain :strobe channel."
  [fixture]
  (into [] (keep (fn [[attribute channel]]
                   (when (and (map? channel) (:coarse channel)
                              (or (some strobe-function? (:functions channel))
                                  (and (= :strobe attribute) (empty? (:functions channel)))))
                     (assoc channel :attribute attribute))))
        (:compiled-channels fixture)))

(def ^:private floor-units 2.0)

(defn- visual-units
  "Declared :flash-area, else one unit per fixture, at least two for beams
  (a beam angle of 15 degrees or less), blinders and strobe-capable fixtures."
  [model fixture strobe?]
  (let [declared (or (:flash-area fixture)
                     (get-in model [:registry :fixtures (:fixture-type-id fixture) :flash-area]))
        beam? (some-> (:beam-angle fixture) (<= 15))
        blinder? (some-> (:fixture-type-id fixture) name (str/includes? "blinder"))]
    (double (or declared (if (or strobe? beam? blinder?) floor-units 1.0)))))

(defn- rig-plan [model patch meter]
  (let [heads (:writers-by-head meter)
        n (count heads)
        ids (object-array n)
        lum (double-array n)
        weights (double-array n)
        total (double (:relative-capacity meter))
        fixture-index (into {} (map-indexed (fn [i f] [(:id f) i])) (:fixtures patch))
        targets (int-array n -1)
        strobes (object-array n)
        groups (java.util.HashMap.)
        group-of (object-array n)]
    (doseq [[id [i]] heads
            :let [fixture (get-in patch [:by-id id])
                  channels (strobe-channels fixture)
                  group (or (:pixel-layout-id fixture) (:fixture-id fixture) id)]]
      (aset ids (int i) id)
      (aset lum (int i) (if (pos? total) (/ (double (get-in meter [:source-capacities id :relative-capacity] 0.0)) total) 0.0))
      (aset targets (int i) (int (get fixture-index id -1)))
      (when (seq channels) (aset strobes (int i) channels))
      (aset group-of (int i) group)
      (.put groups group (conj (or (.get groups group) []) [i fixture (seq channels)])))
    ;; One visual unit per fixture or strip, shared by its heads.
    (let [units (into {} (for [[group members] groups
                               :when (some (fn [[i]] (pos? (aget lum (int i)))) members)]
                           [group (visual-units model (second (first members)) (boolean (some #(nth % 2) members)))]))
          unit-total (reduce + 0.0 (vals units))]
      (doseq [[group members] groups
              :let [share (if (and (pos? unit-total) (units group))
                            (/ (double (units group)) unit-total (count members)) 0.0)]
              [i] members]
        (aset weights (int i) (if (pos? (aget lum (int i))) (max (aget lum (int i)) share) 0.0))))
    {:patch patch :meter meter :ids ids :weights weights :targets targets :strobes strobes :n n
     :fixtures (count (:fixtures patch))
     :index (update-vals heads first)
     :all (int-array (filter #(pos? (aget weights (int %))) (range n)))
     :strobe-heads (int-array (filter #(and (aget strobes (int %)) (pos? (aget weights (int %)))) (range n)))}))

(defn- read-byte [universes [u i]]
  (if-let [^bytes data (get universes u)] (bit-and 255 (int (aget data (int i)))) 0))

(defn- span-rate [[lo hi] position]
  (+ (double lo) (* (double position) (- (double hi) (double lo)))))

(defn strobe-rate
  "The flash rate in Hz a channel asks the fixture for, or nil when it does
  not strobe. A strobe range without a declared :hz span reads as ##Inf:
  faster than any limit."
  [{:keys [coarse functions hz attribute]} universes]
  (when coarse
    (let [value (read-byte universes coarse)]
      (if (seq functions)
        (when-let [{:keys [start end] :as f} (some #(when (<= (:start %) value (:end %)) %) functions)]
          (when (strobe-function? f)
            (if-let [span (:hz f)]
              (span-rate span (if (= start end) 1.0 (/ (- value start) (double (- end start)))))
              ##Inf)))
        (when (and (= :strobe attribute) (pos? value))
          (if hz (span-rate hz (/ value 255.0)) ##Inf))))))

(defn- head-rate [channels universes]
  (reduce (fn [best channel] (if-let [hz (strobe-rate channel universes)] (max best (double hz)) best)) 0.0 channels))

;; ------------------------------------------------------------------ state

(defn- buffers [n]
  {:trough (double-array (* 2 n)) :peak (double-array (* 2 n)) :armed (boolean-array (* 2 n))})

(defn- remap
  "Carry per-head transition state to a new rig plan by head ID, so a rig
  edit keeps the window and history of unchanged heads."
  [old-plan new-plan state]
  (let [{:keys [^doubles trough ^doubles peak ^booleans armed]} state
        fresh (buffers (:n new-plan))
        ^doubles t2 (:trough fresh) ^doubles p2 (:peak fresh) ^booleans a2 (:armed fresh)
        ^objects ids (:ids new-plan)
        old-index (:index old-plan)]
    (dotimes [i (:n new-plan)]
      (if-let [o (get old-index (aget ids i))]
        (dotimes [c 2]
          (aset t2 (+ (* 2 i) c) (aget trough (+ (* 2 (int o)) c)))
          (aset p2 (+ (* 2 i) c) (aget peak (+ (* 2 (int o)) c)))
          (aset a2 (+ (* 2 i) c) (aget armed (+ (* 2 (int o)) c))))
        (dotimes [c 2] (aset a2 (+ (* 2 i) c) true))))
    fresh))

(defn- seed
  "Start transition state from one measured frame: every head armed at its level."
  [plan ^doubles levels]
  (let [{:keys [^doubles trough ^doubles peak ^booleans armed] :as fresh} (buffers (:n plan))
        ^ints all (:all plan)]
    (dotimes [k (alength all)]
      (let [i (aget all k)]
        (dotimes [c 2]
          (let [j (+ (* 2 i) c) l (aget levels j)]
            (aset trough j l) (aset peak j l) (aset armed j true)))))
    fresh))

(defn- measure!
  "Levels by meter index, two per head; heads outside :all are not written."
  ^doubles [plan meter universes ^doubles out]
  (capacity/visible-light! meter universes (:all plan) out))

(defn- expire
  "Drop timestamps a window old. Times are a vector, oldest first."
  [times now]
  (cond
    (empty? times) []
    (<= (double (first times)) (- (double now) window-ms)) (into [] (filter #(> (double %) (- (double now) window-ms))) times)
    :else times))

(defn- allowed? [times limit] (<= (inc (count times)) limit))

(defn- expire-span [ring now span]
  (cond
    (empty? ring) []
    (<= (double (first (first ring))) (- (double now) span)) (into [] (filter #(> (double (first %)) (- (double now) span))) ring)
    :else ring))

(defn- ring-area [ring] (reduce + 0.0 (map second ring)))

;; ------------------------------------------------------------------ grant

(defn- burst-limit [{:keys [limit-hz grant]} {:keys [beats cooldown]}]
  (if (and grant (not (and cooldown (pos? cooldown)))
           (or (nil? beats) (< beats (:burst-beats grant))))
    (double (:limit-hz grant))
    limit-hz))

(defn- next-burst
  "Burst length is counted in forward beats. Once started a burst lasts its
  cap; a backward jump of the beat ends it early. Either way the cooldown is
  the full burst length, drains on forward beats only and survives relaunches."
  [{:keys [grant]} {:keys [beats cooldown last-beat] :as burst} beat over-base?]
  (let [step (if last-beat (- (double beat) (double last-beat)) 0.0)
        rewound? (neg? step)
        forward (max 0.0 step)
        cooldown (when cooldown (- cooldown forward))
        beats (when beats (+ beats forward))
        cap (:burst-beats grant (:burst-beats grant-caps))
        end (fn [] {:cooldown (double cap) :last-beat beat})]
    (cond
      (and beats (or rewound? (>= beats cap))) (end)
      beats {:beats beats :last-beat beat}
      (and cooldown (pos? cooldown)) {:cooldown cooldown :last-beat beat}
      (and grant over-base?) {:beats 0.0 :last-beat beat}
      grant {:last-beat beat}
      :else nil)))

;; ------------------------------------------------------------------ frame

(defn- split-writers
  "Head ID -> attributed instance IDs, and head ID -> the other instances."
  [targets instances sources inf]
  (reduce-kv (fn [[a o] source heads]
               (let [id (:instance-id source)
                     instance (get instances id)]
                 (cond
                   (and instance (influence/attributed? sources inf instance))
                   [(reduce #(update %1 %2 (fnil conj #{}) id) a heads) o]
                   id [a (reduce #(update %1 %2 (fnil conj #{}) id) o heads)]
                   :else [a o])))
             [{} {}] targets))

(defn- writer-ids
  "Sorted IDs of the instances writing to the held heads."
  [by-head ^objects ids ^ints held]
  (if (empty? by-head)
    []
    (let [found (java.util.HashSet.)]
      (dotimes [k (alength held)]
        (when-let [xs (get by-head (aget ids (aget held k)))] (.addAll found xs)))
      (vec (sort-by str found)))))

(defn- guarded-heads
  "Meter indices this frame guards. In the agent scope also each head's
  attributed and other writers. Rebuilt only when the writer graph, the
  instances or the influence change; the whole-show scope needs neither."
  [plan model blocks sources show-wide? previous]
  (if show-wide?
    (if (and (:show-wide? previous) (identical? plan (:plan previous)))
      previous
      (let [^booleans mask (boolean-array (:n plan))]
        (doseq [i (:all plan)] (aset mask (int i) true))
        {:mask mask :count (count (:all plan)) :show-wide? true :plan plan}))
    (let [writers (activity/emission-writer-targets (:patch plan) blocks (:writers previous))
          instances (:instances model)
          inf (influence/summary model)]
      (if (and (identical? (:targets writers) (get-in previous [:writers :targets]))
               (identical? instances (:instances previous)) (identical? inf (:influence previous))
               (not (:show-wide? previous)) (identical? plan (:plan previous)))
        (assoc previous :writers writers)
        (let [index (:index plan)
              [agent others] (split-writers (:targets writers) instances sources inf)
              ^booleans mask (boolean-array (:n plan))]
          (doseq [id (keys agent) :let [i (index id)] :when i] (aset mask (int i) true))
          {:mask mask :count (loop [i 0 c 0] (if (== i (alength mask)) c (recur (inc i) (if (aget mask i) (inc c) c))))
           :agent agent :others others :writers writers :instances instances :influence inf
           :show-wide? false :plan plan})))))

(defmacro ^:private step-channel!
  "Advance one head's transition state at slot j with level l. True when the
  head crosses into a transition. A macro so the per-head loop stays primitive."
  [trough peak armed new-trough new-peak new-armed j l delta]
  `(let [j# (int ~j) l# (double ~l) delta# (double ~delta)
         trough# (doubles ~trough) peak# (doubles ~peak) armed# (booleans ~armed)
         nt# (doubles ~new-trough) np# (doubles ~new-peak) na# (booleans ~new-armed)
         t# (aget trough# j#) p# (aget peak# j#)]
     (if (aget armed# j#)
       (if (>= (- l# t#) delta#)
         (do (aset na# j# false) (aset np# j# l#) (aset nt# j# t#) true)
         (do (aset na# j# true) (aset nt# j# (min t# l#)) (aset np# j# p#) false))
       (let [p# (max p# l#)]
         (if (>= (- p# l#) delta#)
           (do (aset na# j# true) (aset nt# j# l#) (aset np# j# p#) false)
           (do (aset na# j# false) (aset nt# j# t#) (aset np# j# p#) false))))))

(defn advance
  "Guard one delivered frame. `previous` is this guard's private state,
  `prior` the last delivered frame, `wall-ms` the host clock (the window
  drains on it, even while the transport is stopped) and `beat` the transport
  beat (grant bursts count forward beats). Returns {:frame :state :report};
  :frame is `frame` itself when nothing was held."
  [previous model patch meter prior frame blocks wall-ms beat]
  (let [{:keys [scope sources area min-change limit-hz red-limit-hz] :as cfg} (config model)
        area (double area) delta (double min-change)
        same-plan? (and (identical? patch (get-in previous [:plan :patch])) (identical? meter (get-in previous [:plan :meter])))
        plan (if same-plan? (:plan previous) (rig-plan model patch meter))
        n (long (:n plan))
        now (double wall-ms)
        ^doubles weights (:weights plan)
        ^objects ids (:ids plan)
        ^ints all (:all plan)
        inf (influence/summary model)
        show-wide? (or (= :show scope) (influence/show-wide? inf))
        guarded (guarded-heads plan model blocks sources show-wide? (:guarded previous))
        ^booleans mask (:mask guarded)
        ;; Two owned buffer sets alternate, so the committed state is never
        ;; written after it was returned. The level scratch array is private
        ;; to the guard and rewritten in full before each use.
        scratch (let [s (:scratch previous)]
                  (if (and s (= (alength ^doubles (:levels s)) (* 2 n))) s
                    {:levels (double-array (* 2 n)) :lum-x (int-array n) :red-x (int-array n) :flags (byte-array n)}))
        ^doubles levels (:levels scratch)
        _ (measure! plan meter (:universes frame) levels)
        current (cond
                  (and same-plan? (:state previous)) (:state previous)
                  (:state previous) (remap (get-in previous [:plan]) plan (:state previous))
                  (:universes prior) (seed plan (measure! plan meter (:universes prior) (double-array (* 2 n))))
                  :else (seed plan levels))
        spare (let [s (:spare previous)] (if (and s same-plan? (= (alength ^doubles (:trough s)) (* 2 n))) s (buffers n)))
        {^doubles trough :trough ^doubles peak :peak ^booleans armed :armed} current
        {^doubles new-trough :trough ^doubles new-peak :peak ^booleans new-armed :armed} spare
        rises (expire (:rises previous) now)
        red-rises (expire (:red-rises previous) now)
        burst (:burst previous)
        limit (burst-limit cfg burst)
        span (/ 500.0 limit)
        ring (expire-span (:ring previous) now span)
        red-ring (expire-span (:red-ring previous) now (/ 500.0 red-limit-hz))
        ;; Step every head; collect guarded crossings into owned scratch.
        ^ints lum-x (:lum-x scratch) ^ints red-x (:red-x scratch) ^bytes flags (:flags scratch)
        counts (loop [k 0 nl 0 nr 0 al 0.0 ar 0.0]
                 (if (== k (alength all))
                   [nl nr al ar]
                   (let [i (aget all k)
                         lum? (step-channel! trough peak armed new-trough new-peak new-armed (* 2 i) (aget levels (* 2 i)) delta)
                         red? (step-channel! trough peak armed new-trough new-peak new-armed (inc (* 2 i)) (aget levels (inc (* 2 i))) delta)
                         guarded? (aget mask i)
                         lum? (and lum? guarded?) red? (and red? guarded?)
                         w (aget weights i)]
                     (when lum? (aset lum-x nl i))
                     (when red? (aset red-x nr i))
                     (recur (inc k) (if lum? (inc nl) nl) (if red? (inc nr) nr)
                            (if lum? (+ al w) al) (if red? (+ ar w) ar)))))
        n-lum (long (nth counts 0)) n-red (long (nth counts 1))
        crossing-area (double (nth counts 2)) red-area (double (nth counts 3))
        flash? (and (pos? crossing-area) (>= (+ (ring-area ring) crossing-area) area))
        red-flash? (and (pos? red-area) (>= (+ (ring-area red-ring) red-area) area))
        hold? (and flash? (not (allowed? rises limit)))
        red-hold? (and red-flash? (not (allowed? red-rises red-limit-hz)))
        ;; Fixture strobes, worst-case phase: sum of share x rate <= area x limit.
        strobing (let [^ints sh (:strobe-heads plan) ^objects strobes (:strobes plan)]
                   (loop [k 0 acc nil]
                     (if (== k (alength sh)) acc
                       (let [i (aget sh k)]
                         (if (and (aget mask i) (>= (aget levels (* 2 i)) delta))
                           (let [hz (double (head-rate (aget strobes i) (:universes frame)))]
                             (recur (inc k) (if (pos? hz) (conj (or acc []) [i (* (aget weights i) hz)]) acc)))
                           (recur (inc k) acc))))))
        strobe-load (reduce + 0.0 (map second strobing))
        strobe-cut (when (> strobe-load (* area limit))
                     (loop [load strobe-load
                            order (sort-by (fn [[i load]] [(- (double load)) i]) strobing)
                            cut []]
                       (if (or (<= load (* area limit)) (empty? order)) cut
                         (let [[i l] (first order)]
                           (recur (- load (double l)) (rest order) (conj cut i))))))
        holding? (or hold? red-hold? (seq strobe-cut))
        ;; Flags per held head: 1 luminance hold, 2 red hold, 4 strobe cut.
        held (when holding?
               (let [mark! (fn [^long i bit] (aset flags i (unchecked-byte (bit-or (aget flags i) (long bit)))))]
                 (when hold? (dotimes [k n-lum] (mark! (aget lum-x k) 1)))
                 (when red-hold? (dotimes [k n-red] (mark! (aget red-x k) 2)))
                 (doseq [i strobe-cut] (mark! i 4))
                 (let [out (java.util.Arrays/copyOf lum-x (int n))]
                   ;; Reuse the crossing scratch length for the marked heads, ascending.
                   (java.util.Arrays/copyOf
                    out (int (loop [i 0 c 0]
                               (if (== i n) c
                                 (if (pos? (aget flags i)) (do (aset out c i) (recur (inc i) (inc c))) (recur (inc i) c)))))))))
        gains (when held
                (let [g (double-array (:fixtures plan) 1.0)
                      ^ints targets (:targets plan)
                      ^ints held held]
                  (dotimes [k (alength held)]
                    (let [i (aget held k) f (aget flags i) t (aget targets i)
                          lj (* 2 i) rj (inc (* 2 i))
                          l (aget levels lj) r (aget levels rj)
                          gain (cond-> 1.0
                                 (and (pos? (bit-and f 1)) (pos? l)) (min (/ (* 0.98 (+ (aget trough lj) delta)) l))
                                 (and (pos? (bit-and f 2)) (pos? r)) (min (/ (* 0.98 (+ (aget trough rj) delta)) r))
                                 (pos? (bit-and f 4)) (min 0.0))]
                      (when (>= t 0) (aset g t (min (aget g t) gain)))))
                  g))
        delivered (if gains (emission/scale-emission patch frame gains) frame)
        ;; Held heads were kept below a transition: re-read them and keep them armed.
        _ (when held
            (let [^ints held held]
              (capacity/visible-light! meter (:universes delivered) held levels)
              (dotimes [k (alength held)]
                (let [i (aget held k) f (aget flags i)]
                  (aset flags i (unchecked-byte 0))
                  (dotimes [c 2]
                    (let [j (+ (* 2 i) c) l (aget levels j)]
                      (cond
                        ;; Light returning after a strobe cut is a new flash.
                        (pos? (bit-and f 4))
                        (do (aset new-armed j true) (aset new-trough j l) (aset new-peak j l))

                        ;; A held rise never became a transition.
                        (pos? (bit-and f (if (zero? c) 1 2)))
                        (do (aset new-armed j true) (aset new-trough j (min (aget trough j) l))
                            (aset new-peak j (aget peak j)))

                        (not (aget new-armed j)) (aset new-peak j l))))))))
        counted? (and flash? (not hold?))
        red-counted? (and red-flash? (not red-hold?))
        rises (if counted? (conj rises now) rises)
        red-rises (if red-counted? (conj red-rises now) red-rises)
        ring (cond counted? [] (and (pos? crossing-area) (not hold?)) (conj ring [now crossing-area]) :else ring)
        red-ring (cond red-counted? [] (and (pos? red-area) (not red-hold?)) (conj red-ring [now red-area]) :else red-ring)
        rate (count rises)
        over-base? (or (> rate (:limit-hz cfg))
                       (> (reduce + 0.0 (map second strobing)) (* area (:limit-hz cfg))))
        burst (next-burst cfg burst beat over-base?)
        [agent-writers other-writers] (cond
                                        (nil? held) nil
                                        (:show-wide? guarded) (split-writers (:targets (activity/emission-writer-targets patch blocks nil))
                                                                             (:instances model) sources inf)
                                        :else [(:agent guarded) (:others guarded)])
        limiting (if held
                   {:until (+ now window-ms) :held-heads (count held)
                    :reasons (cond-> [] hold? (conj :luminance) red-hold? (conj :saturated-red)
                                     (seq strobe-cut) (conj :strobe-channel))
                    :instance-ids (writer-ids agent-writers ids held)
                    :human-instance-ids (writer-ids other-writers ids held)}
                   (let [last-hold (:limiting previous)]
                     (when (and last-hold (< now (double (:until last-hold)))) last-hold)))
        report (cond-> (merge {:scope (if show-wide? :show scope) :limit-hz limit :base-limit-hz (:limit-hz cfg)
                               :red-limit-hz red-limit-hz :area area :rate-hz rate :red-rate-hz (count red-rises)
                               :guarded-heads (:count guarded) :limiting? (some? limiting)}
                              (dissoc limiting :until))
                 (:grant cfg) (assoc :grant (select-keys (:grant cfg) [:limit-hz :burst-beats])
                                     :burst (dissoc burst :last-beat)))]
    {:frame delivered
     :report report
     :state {:plan plan :guarded guarded :time-ms now :state spare :spare current :scratch scratch
             :rises rises :red-rises red-rises :ring ring :red-ring red-ring
             :burst burst :limiting limiting}}))

(defn idle-state
  "Carry the window, a grant burst or cooldown and the last limiting report
  through a frame the guard does not inspect: blackout, master at zero or
  calibration. Transition state restarts from the next inspected frame."
  [previous wall-ms]
  (when previous
    (let [now (double wall-ms)
          rises (expire (:rises previous) now)
          red-rises (expire (:red-rises previous) now)]
      (when (or (seq rises) (seq red-rises) (live-burst? previous))
        (-> previous
            (assoc :time-ms now :rises rises :red-rises red-rises :ring [] :red-ring []
                   :limiting (when (some-> (:limiting previous) :until (> now)) (:limiting previous)))
            (dissoc :state :spare :scratch))))))

(def ^:private published-keys [:scope :limit-hz :base-limit-hz :red-limit-hz :limiting? :held-heads :reasons
                               :instance-ids :human-instance-ids :grant])

(defn publish
  "Keep the latest report under [:variables :lighting/flash-guard] and emit a
  :lighting/flash-guard event when limiting starts, stops or changes what it
  holds back."
  [model report]
  (let [old (get-in model [:variables :lighting/flash-guard])]
    (if-not report
      (cond-> model old (update :variables dissoc :lighting/flash-guard))
      (let [edge (select-keys report published-keys)
            before (select-keys old published-keys)
            changed? (or (not= (boolean (:limiting? edge)) (boolean (:limiting? before)))
                         (and (:limiting? edge) (not= (dissoc edge :held-heads) (dissoc before :held-heads))))]
        (cond-> (if (= old report) model (assoc-in model [:variables :lighting/flash-guard] report))
          changed?
          (events/collect :lighting {:kind :lighting-rig :id (:show-id model)}
                          [(cond-> (assoc (select-keys report [:scope :limit-hz :red-limit-hz :rate-hz :red-rate-hz
                                                               :held-heads :reasons :instance-ids :human-instance-ids])
                                          :type :lighting/flash-guard
                                          :status (if (:limiting? report) :limiting :clear))
                             (:limiting? report) (assoc :reason :flash-guard))]))))))
