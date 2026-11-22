(ns tolglow.autopilot.controller
  "Pure musical feedback policy. Input and returned state contain only data.
  Fixture gains and cue requests are applied by the show owner."
  (:require [afterglow.time :as time]
            [tolglow.automation.random :as random]
            [tolglow.autopilot.scope :as scope]))

(def defaults
  {:interval {:beats 1} :strategy :gradual :preference :parameters :step 0.10
   :tolerance 0.025 :overshoot 0.0 :standby-level 0.0 :max-gain 2.0
   :rotate? true :seed 1 :duration {:bars 1}})

(def scope-ids scope/fixture-ids)
(defn- unit? [v] (and (time/finite-number? v) (<= 0 v 1)))
(defn- positive-duration? [d]
  (and (empty? (time/validate-duration d)) (pos? (val (first d)))))
(defn settings [opts]
  (let [s (merge defaults opts)]
    (when-not (and (or (nil? opts) (map? opts)) (every? (set (keys defaults)) (keys opts))
                   (every? #(unit? (s %)) [:step :tolerance :standby-level :overshoot])
                   (time/finite-number? (:max-gain s)) (<= 1 (:max-gain s))
                   (integer? (:seed s)) (boolean? (:rotate? s))
                   (#{:gradual :instant} (:strategy s))
                   (#{:parameters :cues :generate :author} (:preference s))
                   (every? positive-duration? ((juxt :interval :duration) s)))
      (throw (ex-info "Invalid resolved autopilot controller options" {:options opts})))
    s))

(defn- restarted? [state clock epoch]
  (or (nil? (:clock state)) (not= epoch (:epoch state))
      (some (fn [k] (let [now (get clock k) before (get-in state [:clock k])]
                      (and (number? now) (number? before) (< now before)))) [:beat :time-ms])))

(defn due?
  "Cheap check before metering individual raw sources. Musical intervals hold
  while paused. Missed intervals collapse into one decision on the same grid."
  [state clock epoch options]
  (let [clock (time/frame-clock clock) interval (:interval options (:interval defaults))
        timer (:timer state)]
    (and (not (false? (:playing? clock)))
         (or (restarted? state clock epoch) (not= interval (get-in timer [:duration :source]))
             (:done? (time/sample timer clock))))))

(defn- next-timer [old interval clock]
  (let [fresh (time/start interval clock)]
    (if (= interval (get-in old [:duration :source]))
      (let [span (double (get-in old [:duration :value]))
            periods (Math/floor (/ (- (:started-at fresh) (:started-at old)) span))]
        (assoc old :started-at (+ (:started-at old) (* periods span))))
      fresh)))

(defn- clamp [lo hi x] (max lo (min hi x)))

(defn- level [source gain] (min 1.0 (* (:level source) gain)))
(defn- weighted [sources gains total]
  (if (zero? total) 0.0
      (/ (reduce (fn [sum {:keys [id capacity] :as s}]
                   (+ sum (* capacity (level s (get gains id 1.0))))) 0.0 sources) total)))

(defn- shuffled [sources prev seed rotate?]
  (cond
    (not rotate?) [seed sources]
    (< (count sources) 2) [seed sources]
    :else (let [[seed order] (random/shuffle-values seed sources)]
            [seed (if (= (map :id order) prev) (conj (subvec order 1) (first order)) order)])))

(defn- allocate
  "Keep the selected lamps at their authored level where possible. Spend above
  authored white only when every source together would otherwise fall short."
  [sources order desired extra total {:keys [standby-level max-gain]}]
  (let [low (fn [s] (min standby-level (:level s)))
        floor (reduce (fn [sum s] (+ sum (* (low s) (:capacity s)))) 0.0 sources)
        desired (* total (clamp 0.0 1.0 desired))
        slack (* total extra)
        floor-scale (if (pos? floor) (min 1.0 (/ desired floor)) 1.0)
        boost? (> desired (reduce (fn [sum s] (+ sum (* (:level s) (:capacity s)))) 0.0 sources))
        ;; Scale all the way to physical full white where max-gain permits it.
        limit (if boost? max-gain 1.0)]
    (loop [remaining order left (max 0.0 (- desired floor)) gains (transient {})]
      (if-let [id (:id (first remaining))]
        (let [{:keys [capacity level] :as s} (first remaining)
              base (* floor-scale (low s))
              cost (* capacity (max 0.0 (- (min 1.0 (* level limit)) base)))
              grant (if (and (pos? left) (<= cost (+ left slack))) cost (min left cost))
              gain (if (and (pos? level) (pos? capacity)) (/ (+ base (/ grant capacity)) level)
                       ;; An unselected lamp stays within standby if its LFO rises between decisions.
                       standby-level)]
          (recur (next remaining) (max 0.0 (- left grant)) (assoc! gains id (clamp 0.0 max-gain gain))))
        (persistent! gains)))))

(defn- pending-at [pending clock coverage]
  (into [] (keep (fn [{:keys [timer] :as correction}]
                   (let [{:keys [done? progress]} (time/sample timer clock)]
                     (when-not done? (assoc correction :remaining (max 0.0 (- 1.0 (/ progress coverage)))))))) pending))

(defn- predicted [statistics pending axis]
  (clamp 0.0 1.0 (+ (double (get-in statistics [axis :mean] 0.0))
                    (reduce (fn [sum p] (+ sum (* (:remaining p) (get p axis 0.0)))) 0.0 pending))))

(defn- change [actual target {:keys [tolerance strategy step]}]
  (let [error (- target actual)]
    (if (<= (abs error) (+ tolerance 1e-9)) 0.0
        (if (= strategy :instant) error (clamp (- step) step error)))))

(defn- request [axis delta dark? options ids lit-ids cue-ids]
  (let [up? (pos? delta)
        prefer (:preference options)
        op (cond (and up? (= :generate prefer)) :generate
                 (and up? (= :author prefer)) :author
                 (and up? (or (= :cues prefer) dark?)) :launch
                 (and (not up?) (not= :parameters prefer)) :retire
                 :else :adjust)]
    (cond-> {:op op :domain axis :direction (if up? :up :down) :amount (min 1.0 (abs delta))
             :fixture-ids ids :lit-fixture-ids lit-ids :max-gain (:max-gain options) :duration (:duration options)}
      (some? cue-ids) (assoc :cue-ids cue-ids))))

(defn- choose-actions [requests strategy]
  (let [requests (sort-by (juxt #(if (and (empty? (:lit-fixture-ids %)) (= :brightness (:domain %))) 0 1)
                                 (comp - :amount)) requests)
        create? #(#{:launch :generate :author} (:op %))]
    (cond
      (= :gradual strategy) (into [] (take 1) requests)
      (and (= 2 (count requests)) (every? create? requests)) [(first requests)]
      :else (vec requests))))

(defn- active-ids [sources gains floor]
  (into #{} (comp (filter #(> (level % (get gains (:id %) 1.0)) (+ floor 1e-9))) (map :id)) sources))

(defn- allocate-order [sources state order wanted slack total opts]
  (let [gains (allocate sources order wanted slack total opts)
        active (active-ids sources gains (:standby-level opts))
        previous (:active state)]
    ;; A shuffled prefix can contain exactly the same lamps. Give one waiting,
    ;; emitting source first pick when another selection is physically possible.
    (if-let [next-source (when (and (:rotate? opts) (seq active) (= previous active))
                          (some (fn [{:keys [id level] :as source}]
                                  (when (and (not (active id)) (> level (:standby-level opts))) source))
                                order))]
      (let [order (into [next-source] (remove #(= (:id next-source) (:id %))) order)
            gains (allocate sources order wanted slack total opts)]
        [(mapv :id order) gains (active-ids sources gains (:standby-level opts))])
      [(mapv :id order) gains active])))

(defn- light-plan [state sources brightness target opts]
  (let [ids (mapv :id sources)
        total (reduce + 0.0 (map :capacity sources))
        seed (if (= (:initial-seed state) (:seed opts)) (:seed state) (:seed opts))
        [seed order] (shuffled sources (:order state) seed (:rotate? opts))
        delta (change brightness target opts)
        now (weighted sources (:gains state) total)
        ;; Preserve an authored pulse's duty cycle when translating average light
        ;; to the instantaneous level at the top of that pulse.
        duty (if (and (pos? now) (pos? brightness)) (min 1.0 (/ brightness now)) 1.0)
        wanted (if (and (zero? target) (= :instant (:strategy opts))) 0.0
                   (clamp 0.0 1.0 (+ now (/ delta duty))))
        [order gains active] (cond
                               (and (= :gradual (:strategy opts)) (zero? (:step opts)))
                               (let [gains (into {} (map (fn [id] [id (get (:gains state) id 1.0)])) ids)]
                                 [(:order state ids) gains (active-ids sources gains (:standby-level opts))])
                               (some #(pos? (:level %)) sources)
                               (allocate-order sources state order wanted (/ (:overshoot opts) duty) total opts)
                               :else
                               ;; An unlit sample can be the trough of a dimmer LFO.
                               ;; Keep selected gains ready to respond to its next rise.
                               [(mapv :id order) (into {} (map (fn [id] [id (if (zero? target) 0.0
                                                                        (get (:gains state) id 1.0))])) ids) #{}])
        delivered (weighted sources gains total)
        achieved (* duty (- delivered now))
        residual (- delta achieved)
        residual (if (= (pos? residual) (pos? delta)) residual 0.0)
        request (cond (empty? ids) 0.0
                      (> (abs residual) (:tolerance opts)) residual
                      (not= :parameters (:preference opts)) delta
                      :else 0.0)]
    {:ids ids :gains gains :active active :order order :seed seed :before now
     :lit (into [] (comp (filter #(pos? (level % (gains (:id %))))) (map :id)) sources)
     :achieved achieved :residual residual :request request
     :raw (weighted sources {} total) :wanted wanted :delivered delivered}))

(defn- actions [light activity-change opts cue-ids]
  (let [{:keys [ids lit before]} light
        make-request #(request %1 %2 (zero? before) opts ids lit cue-ids)]
    (if (empty? ids) []
        (choose-actions (cond-> []
                          (not (zero? (:request light))) (conj (make-request :brightness (:request light)))
                          (not (zero? activity-change)) (conj (make-request :activity activity-change)))
                        (:strategy opts)))))

(defn- source-membership [sources targets]
  (when-not (every? #(unit? (get targets %)) [:brightness :activity])
    (throw (ex-info "Expected resolved energy targets and calibrated sources" {:targets targets})))
  (let [membership (persistent!
                    (reduce (fn [result {:keys [id level capacity]}]
                              (when-not (and (some? id) (unit? level)
                                             (time/finite-number? capacity) (not (neg? capacity)))
                                (throw (ex-info "Expected resolved energy targets and calibrated sources" {:targets targets})))
                              (assoc! result id (double capacity)))
                            (transient {}) sources))]
    (when-not (= (count sources) (count membership))
      (throw (ex-info "Duplicate source IDs" {})))
    membership))

(defn- append-correction [pending correction]
  (let [pending (vec pending)]
    (if (< (count pending) 64) (conj pending correction)
        ;; Retain the aggregate pending response even at sub-beat decision rates
        ;; where many more than 64 moves fit inside the averaging window.
        (let [[a b] pending
              scale (if (pos? (:remaining b)) (/ (:remaining a) (:remaining b)) 0.0)
              merged (reduce (fn [m axis] (update m axis + (* scale (axis a)))) b [:brightness :activity])]
          (conj (into [merged] (subvec pending 2)) correction)))))

(defn- retain-correction [pending clock window light activity-change requested]
  ;; Already issued changes enter a trailing window progressively. Account for
  ;; the unobserved part before issuing another adjustment or launching a cue.
  (let [axes (set (map :domain requested))
        correction {:brightness (+ (:achieved light) (if (axes :brightness) (:residual light) 0))
                    :activity (if (axes :activity) activity-change 0.0)
                    :timer (time/start window clock) :remaining 1.0}]
    (if (every? zero? ((juxt :brightness :activity) correction)) pending
        (append-correction pending correction))))

(defn advance
  "Consume resolved scalar targets and the rolling statistics of the delivered
  frame. Each raw source is {:id … :capacity full-white-weight :level 0..1}.
  :actions are requests for this decision only. Keep :state for the next call."
  [previous {:keys [clock epoch sources targets statistics options cue-ids]}]
  (let [clock (time/frame-clock clock)]
    (if-not (due? previous clock epoch options)
      {:state previous :gains (:gains previous {}) :actions [] :due? false :report (:report previous)}
      (let [opts (settings options)
            membership (source-membership sources targets)
            state (when-not (restarted? previous clock epoch) previous)
            window (or (:window statistics) (:interval opts))
            pending (when (and (= window (:window state)) (= membership (:membership state)))
                      (pending-at (:pending state) clock (max 1e-9 (:coverage statistics 1.0))))
            ;; A show can be restored with live outputs before its first statistics
            ;; window exists. Start from that light and await a real activity score.
            brightness (if (some? (get-in statistics [:brightness :mean]))
                         (predicted statistics pending :brightness)
                         (weighted sources (:gains state) (reduce + 0.0 (map :capacity sources))))
            activity (if (some? (get-in statistics [:activity :mean]))
                       (predicted statistics pending :activity) (:activity targets))
            light (light-plan state sources brightness (:brightness targets) opts)
            activity-change (change activity (:activity targets) opts)
            requested (actions light activity-change opts cue-ids)
            decision (inc (:decision state 0))
            report {:targets targets :forecast {:brightness brightness :activity activity}
                    :raw-brightness (:raw light) :requested-brightness (:wanted light)
                    :estimated-brightness (:delivered light) :active-count (count (:active light))
                    :decision decision :time-ms (:time-ms clock)}
            next-state (merge (select-keys light [:order :gains :active :seed])
                              {:clock clock :epoch epoch :initial-seed (:seed opts) :membership membership
                               :window window :pending (retain-correction pending clock window light activity-change requested)
                               :timer (next-timer (:timer state) (:interval opts) clock)
                               :report report :decision decision})]
        {:state next-state :report report :actions requested :gains (:gains light) :due? true}))))
