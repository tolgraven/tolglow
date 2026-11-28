(ns tolglow.flash-guard
  "Photosensitivity flash guard on delivered output (#836).

  The guard reads each delivered frame after output response and budgeting,
  before activity scoring, and counts large-area flashes of the heads it
  guards. Once the last second of show time already holds the permitted
  number of flashes, it holds further rises of those heads at their previous
  delivered level until the window has room again. It scales emission only,
  through Afterglow's emission scaling, so pan, tilt, motor speed, gobos,
  fog and every other non-emission channel pass unchanged, and coarse/fine
  and discrete channels keep their encoding. It reads committed DMX and never
  evaluates or advances an effect.

  A flash is a rise of area-weighted luminance of at least `:area` (a share
  of the whole rig's visible output) within about half a period at the
  limit. Fixtures flashing out of step add up because rises are summed
  across heads before the threshold. Saturated red counts separately against
  `:red-limit-hz`. A head whose own strobe or shutter channel runs a strobe
  function faster than the limit (or at an undeclared rate) has its emission
  removed while the strobing heads together exceed `:area`.

  By default it guards only light from agent-attributed instances (`:sources`
  adapters, `:agent`); `:scope :show` guards everything, human launches
  included. An operator `:grant` raises the agent limit for bursts of at most
  `:burst-beats` beats. This is a mitigation, not a certification that a show
  is safe. Defaults and how to change them: docs/flash-guard.md."
  (:require [afterglow.render.emission :as emission]
            [afterglow.runtime.events :as events]
            [clojure.string :as str]
            [tolglow.lighting-activity :as activity]
            [tolglow.lighting-capacity :as capacity]))

(def presets
  "Named limits. `:club` follows the 4 Hz strobe figure for public
  performances and clubs; `:broadcast` the three-flashes-per-second line of
  ITU-R BT.1702 and WCAG 2.3.1. Both treat saturated red at 3 Hz."
  {:club {:limit-hz 4.0 :red-limit-hz 3.0}
   :broadcast {:limit-hz 3.0 :red-limit-hz 3.0}})

(def defaults
  "Settings under [:settings :flash-guard]. `:limit-hz` and `:red-limit-hz`,
  when present, override the preset."
  {:enabled? true :scope :agent :preset :club :area 0.25 :sources [:agent] :grant nil})

(def ^:private window-ms 1000.0)

(def ^:private max-hz 60.0)

(defn- rate? [x] (and (number? x) (Double/isFinite (double x)) (< 0 x) (<= x max-hz)))

(defn- grant? [grant]
  (or (nil? grant)
      (and (map? grant) (every? #{:limit-hz :burst-beats :epoch} (keys grant))
           (rate? (:limit-hz grant))
           (number? (:burst-beats grant)) (< 0 (:burst-beats grant) 1025)
           (or (nil? (:epoch grant)) (integer? (:epoch grant))))))

(defn settings
  "Validate a complete flash guard settings map and return it."
  [options]
  (let [{:keys [enabled? scope preset area sources limit-hz red-limit-hz grant] :as opts} (merge defaults options)]
    (when-not (and (map? options)
                   (every? (set (into (keys defaults) [:limit-hz :red-limit-hz])) (keys options))
                   (boolean? enabled?) (#{:agent :show} scope) (contains? presets preset)
                   (number? area) (< 0 area) (<= area 1)
                   (vector? sources) (every? keyword? sources)
                   (or (nil? limit-hz) (rate? limit-hz)) (or (nil? red-limit-hz) (rate? red-limit-hz))
                   (grant? grant))
      (throw (ex-info (str "Flash guard settings take :enabled? (boolean), :scope (:agent or :show), "
                           ":preset (:club or :broadcast), :limit-hz and :red-limit-hz (0 to 60 Hz), "
                           ":area (0 to 1 of the rig's visible output), :sources (adapter keywords) "
                           "and :grant (nil or {:limit-hz :burst-beats})")
                      {:settings options})))
    options))

(defn edit-settings
  "Merge an operator edit. A new grant is stamped with the show epoch and
  lapses when the show restarts."
  [current edit epoch]
  (settings (cond-> (merge current edit)
              (some? (:grant edit)) (assoc-in [:grant :epoch] epoch))))

(defn config
  "The resolved guard for this model, or nil when it is off."
  [model]
  (let [{:keys [enabled? preset limit-hz red-limit-hz grant] :as opts} (merge defaults (get-in model [:settings :flash-guard]))]
    (when enabled?
      (let [base (double (or limit-hz (get-in presets [preset :limit-hz])))]
        (assoc opts :limit-hz base
               :red-limit-hz (double (or red-limit-hz (get-in presets [preset :red-limit-hz])))
               :sources (set (:sources opts))
               ;; A grant applies in the epoch it was given and only raises the limit.
               :grant (when (and grant (= (:epoch grant) (:epoch model)) (> (:limit-hz grant) base))
                        grant))))))

(defn- attributed? [sources instance]
  (contains? sources (get-in instance [:source :adapter])))

(defn active?
  "Whether the guard has work this frame: it covers the whole show, an
  attributed instance is running, or its window still holds recent flashes."
  [model history]
  (when-let [{:keys [scope sources]} (config model)]
    (boolean (or (= :show scope)
                 (some #(attributed? sources %) (vals (:instances model)))
                 (seq (:rises history)) (seq (:red-rises history))))))

;; ------------------------------------------------------------------ rig

(defn- strobe-channel [fixture]
  (some (fn [attribute]
          (when-let [channel (get-in fixture [:compiled-channels attribute])]
            (when (or (seq (:functions channel)) (= :strobe attribute))
              (assoc channel :attribute attribute))))
        [:strobe :shutter]))

(defn- rig-plan [patch meter]
  (let [heads (:writers-by-head meter)
        n (count heads)
        ids (object-array n)
        weights (double-array n)
        total (double (:relative-capacity meter))
        strobes (object-array n)]
    (doseq [[id [i]] heads]
      (aset ids (int i) id)
      (aset weights (int i) (if (pos? total) (/ (double (get-in meter [:source-capacities id :relative-capacity] 0.0)) total) 0.0))
      (aset strobes (int i) (strobe-channel (get-in patch [:by-id id]))))
    {:patch patch :meter meter :ids ids :weights weights :strobes strobes
     :index (update-vals heads first)
     :all (int-array (filter #(pos? (aget weights (int %))) (range n)))}))

(defn- read-byte [universes [u i]]
  (if-let [^bytes data (get universes u)] (bit-and 255 (int (aget data (int i)))) 0))

(defn- span-rate [[lo hi] position]
  (+ (double lo) (* (double position) (- (double hi) (double lo)))))

(defn strobe-rate
  "The flash rate in Hz a strobe or shutter channel asks the fixture for, or
  nil when it does not strobe. A strobe range without a declared :hz span
  reads as ##Inf: faster than any limit."
  [{:keys [coarse functions hz attribute]} universes]
  (when coarse
    (let [value (read-byte universes coarse)]
      (if (seq functions)
        (when-let [{:keys [type start end] :as f} (some #(when (<= (:start %) value (:end %)) %) functions)]
          (when (and (keyword? type) (str/includes? (name type) "strobe"))
            (if-let [span (:hz f)]
              (span-rate span (if (= start end) 1.0 (/ (- value start) (double (- end start)))))
              ##Inf)))
        (when (and (= :strobe attribute) (pos? value))
          (if hz (span-rate hz (/ value 255.0)) ##Inf))))))

;; ------------------------------------------------------------------ frame

(defn- guarded-indices
  "Meter-head indices this frame guards, ascending, and the attributed
  instances writing emission to each head. Reuse the previous result while
  the writer graph and the attributed instances are unchanged."
  [plan model blocks sources scope previous]
  (if (= :show scope)
    {:indices (:all plan)}
    (let [writers (activity/emission-writer-targets (:patch plan) blocks (:writers previous))
          instances (:instances model)
          attributed (into #{} (keep (fn [source]
                                       (let [id (:instance-id source)]
                                         (when (attributed? sources (get instances id)) id))))
                           (keys (:targets writers)))]
      (if (and (identical? (:targets writers) (get-in previous [:writers :targets]))
               (= attributed (:attributed previous)) (identical? plan (:plan previous)))
        (assoc previous :writers writers)
        (let [by-head (reduce-kv (fn [m source heads]
                                   (let [id (:instance-id source)]
                                     (if (contains? attributed id)
                                       (reduce #(update %1 %2 (fnil conj #{}) id) m heads)
                                       m)))
                                 {} (:targets writers))
              index (:index plan)]
          {:indices (int-array (sort (keep index (keys by-head))))
           :by-head by-head :writers writers :attributed attributed :plan plan})))))

(defn- masses
  "[rise fall red-rise] of area-weighted change from `before` to `after`."
  [^doubles weights ^ints indices ^doubles before ^doubles after]
  (loop [k 0 rise 0.0 fall 0.0 red 0.0]
    (if (== k (alength indices))
      [rise fall red]
      (let [w (aget weights (aget indices k))
            j (* 2 k)
            d (- (aget after j) (aget before j))
            dr (- (aget after (inc j)) (aget before (inc j)))]
        (recur (inc k) (if (pos? d) (+ rise (* w d)) rise) (if (neg? d) (- fall (* w d)) fall)
               (if (pos? dr) (+ red (* w dr)) red))))))

(defn- prune [times now] (into [] (filter #(> (double %) (- now window-ms))) times))

(defn- allowed? [times limit] (<= (inc (count times)) limit))

(defn- run-up
  "Leak the previous rise run, restart it after a net fall, add this frame's rise."
  [acc decay rise fall]
  (if (> fall rise) 0.0 (+ (* acc decay) rise)))

(defn- burst-limit [{:keys [limit-hz grant]} burst beat]
  (if (and grant (not (some-> (:cooldown-until burst) (> beat)))
           (or (nil? (:start burst)) (< (- beat (:start burst)) (:burst-beats grant))))
    (double (:limit-hz grant))
    limit-hz))

(defn- next-burst [{:keys [limit-hz grant]} burst beat over-base?]
  (let [start (:start burst)
        cooling? (some-> (:cooldown-until burst) (> beat))]
    (cond
      (nil? grant) nil
      (and start (>= (- beat start) (:burst-beats grant))) {:cooldown-until (+ beat (:burst-beats grant))}
      (and start (not over-base?)) nil
      start burst
      (and over-base? (not cooling?)) {:start beat}
      cooling? burst
      :else nil)))

(defn advance
  "Guard one delivered frame. `previous` is this guard's private state from
  the last committed frame of the same rig, `prior` that frame's delivered
  output, `show-ms` the show clock (it stands still with the transport) and
  `beat` the transport beat. Returns {:frame :state :report}; :frame is
  `frame` itself when nothing was held."
  [previous model patch meter prior frame blocks show-ms beat]
  (let [{:keys [scope sources area limit-hz red-limit-hz] :as cfg} (config model)
        plan (if (and (identical? patch (get-in previous [:plan :patch])) (identical? meter (get-in previous [:plan :meter])))
               (:plan previous) (rig-plan patch meter))
        now (double show-ms)
        elapsed (when-let [t (:time-ms previous)] (- now (double t)))
        decay (if (and elapsed (<= 0.0 elapsed) (< elapsed window-ms))
                (Math/exp (- (/ elapsed (/ 1000.0 (* 2.0 limit-hz))))) 0.0)
        rises (prune (:rises previous) now)
        red-rises (prune (:red-rises previous) now)
        burst (:burst previous)
        limit (burst-limit cfg burst beat)
        guarded (guarded-indices plan model blocks sources scope (:guarded previous))
        {:keys [^ints indices by-head]} guarded
        n (alength indices)
        ^doubles weights (:weights plan)
        ^objects ids (:ids plan)
        ^doubles current (capacity/visible-light! meter (:universes frame) indices (double-array (* 2 n)))
        ^doubles before (if-let [universes (:universes prior)]
                 (capacity/visible-light! meter universes indices (double-array (* 2 n)))
                 current)
        [rise fall red] (masses weights indices before current)
        run (run-up (double (:run previous 0.0)) decay rise fall)
        red-run (run-up (double (:red-run previous 0.0)) decay red fall)
        hold? (and (>= run area) (not (allowed? rises limit)))
        red-hold? (and (>= red-run area) (not (allowed? red-rises red-limit-hz)))
        ;; Heads whose own strobe channel flashes faster than the limit.
        strobing (into [] (keep (fn [k]
                                  (let [i (aget indices k)]
                                    (when-let [channel (aget ^objects (:strobes plan) i)]
                                      (when-let [hz (strobe-rate channel (:universes frame))]
                                        (when (> hz limit) [k hz]))))))
                       (range n))
        strobe-mass (reduce (fn [sum [k]] (+ sum (* (aget weights (aget indices (int k))) (aget current (int (* 2 k)))))) 0.0 strobing)
        strobe-cut? (>= strobe-mass area)
        gains (cond-> {}
                (or hold? red-hold?)
                (as-> g (reduce (fn [g ^long k]
                                  (let [j (* 2 k)
                                        light (aget current j)
                                        lum (when (and hold? (> light (aget before j))) (/ (aget before j) light))
                                        r (aget current (inc j))
                                        red (when (and red-hold? (> r (aget before (inc j)))) (/ (aget before (inc j)) r))
                                        gain (min (or lum 1.0) (or red 1.0))]
                                    (if (< gain 1.0) (assoc g (aget ids (aget indices k)) gain) g)))
                                g (range n)))
                strobe-cut? (into (map (fn [[k]] [(aget ids (aget indices k)) 0.0])) strobing))
        delivered (if (seq gains) (emission/scale-emission patch frame gains) frame)
        ^doubles final (if (seq gains) (capacity/visible-light! meter (:universes delivered) indices (double-array (* 2 n))) current)
        [rise fall red] (if (seq gains) (masses weights indices before final) [rise fall red])
        run (run-up (double (:run previous 0.0)) decay rise fall)
        red-run (run-up (double (:red-run previous 0.0)) decay red fall)
        counted? (and (>= run area) (allowed? rises limit))
        red-counted? (and (>= red-run area) (allowed? red-rises red-limit-hz))
        rises (if counted? (conj rises now) rises)
        red-rises (if red-counted? (conj red-rises now) red-rises)
        rate (count rises)
        over-base? (or (> rate (:limit-hz cfg))
                       (and (seq strobing) (>= strobe-mass area)
                            (some (fn [[_ hz]] (> hz (:limit-hz cfg))) strobing)))
        burst (next-burst cfg burst beat over-base?)
        ;; A held strobe is held only on its rising frames. Report limiting
        ;; until a window has passed without a hold, so it reads steadily.
        limiting (if (seq gains)
                   (cond-> {:until (+ now window-ms) :held-heads (count gains)
                            :reasons (cond-> [] hold? (conj :luminance) red-hold? (conj :saturated-red)
                                             strobe-cut? (conj :strobe-channel))}
                     by-head (assoc :instance-ids (vec (into (sorted-set-by #(compare (str %1) (str %2)))
                                                             (mapcat #(get by-head %)) (keys gains)))))
                   (let [last-hold (:limiting previous)]
                     (when (and last-hold (< now (double (:until last-hold)))) last-hold)))
        report (cond-> (merge {:scope scope :limit-hz limit :base-limit-hz (:limit-hz cfg) :red-limit-hz red-limit-hz
                               :area area :rate-hz rate :red-rate-hz (count red-rises)
                               :guarded-heads n :limiting? (some? limiting)}
                              (dissoc limiting :until))
                 (:grant cfg) (assoc :grant (select-keys (:grant cfg) [:limit-hz :burst-beats])
                                     :burst burst))]
    {:frame delivered
     :report report
     :state {:plan plan :guarded guarded :time-ms now
             :run (if counted? 0.0 (if (allowed? rises limit) run (min run (* 0.999 area))))
             :red-run (if red-counted? 0.0 (if (allowed? red-rises red-limit-hz) red-run (min red-run (* 0.999 area))))
             :rises rises :red-rises red-rises
             :burst burst :limiting limiting}}))

(defn idle-state
  "Carry the window through a frame the guard does not inspect: blackout,
  master at zero, calibration, or no guarded light. The rise run restarts."
  [previous show-ms]
  (when previous
    (let [now (double show-ms)
          rises (prune (:rises previous) now)
          red-rises (prune (:red-rises previous) now)]
      (when (or (seq rises) (seq red-rises) (:burst previous))
        (assoc previous :time-ms now :run 0.0 :red-run 0.0 :rises rises :red-rises red-rises
               :limiting (when (some-> (:limiting previous) :until (> now)) (:limiting previous)))))))

(def ^:private published-keys [:scope :limit-hz :base-limit-hz :red-limit-hz :limiting? :held-heads :reasons :instance-ids :grant :burst])

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
                                                               :held-heads :reasons :instance-ids])
                                          :type :lighting/flash-guard
                                          :status (if (:limiting? report) :limiting :clear))
                             (:limiting? report) (assoc :reason :flash-guard))]))))))
