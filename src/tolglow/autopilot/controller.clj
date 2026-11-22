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
                   (pos? (:step s)) (time/finite-number? (:max-gain s)) (<= 1 (:max-gain s))
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

(defn- shuffled [ids prev seed rotate?]
  (cond
    (not rotate?) [seed ids]
    (< (count ids) 2) [seed ids]
    :else (let [[seed order] (random/shuffle-values seed ids)]
            [seed (if (= order prev) (conj (subvec order 1) (first order)) order)])))

(defn- allocate
  "Keep the selected lamps at their authored level where possible. Spend above
  authored white only when every source together would otherwise fall short."
  [sources order desired extra total {:keys [standby-level max-gain]}]
  (let [by-id (into {} (map (juxt :id identity)) sources)
        low (fn [s] (min standby-level (:level s)))
        floor (reduce (fn [sum s] (+ sum (* (low s) (:capacity s)))) 0.0 sources)
        desired (* total (clamp 0.0 1.0 desired))
        slack (* total extra)
        floor-scale (if (pos? floor) (min 1.0 (/ desired floor)) 1.0)
        boost? (> desired (reduce (fn [sum s] (+ sum (* (:level s) (:capacity s)))) 0.0 sources))
        ; Scale all the way to physical full white where max-gain permits it.
        limit (if boost? max-gain 1.0)]
    (loop [remaining order left (max 0.0 (- desired floor)) gains (transient {})]
      (if-let [id (first remaining)]
        (let [{:keys [capacity level] :as s} (by-id id)
              base (* floor-scale (low s))
              cost (* capacity (max 0.0 (- (min 1.0 (* level limit)) base)))
              grant (if (and (pos? left) (<= cost (+ left slack))) cost (min left cost))
              gain (if (and (pos? level) (pos? capacity)) (/ (+ base (/ grant capacity)) level)
                       ;; An unselected lamp stays within standby if its LFO rises between decisions.
                       standby-level)]
          (recur (next remaining) (max 0.0 (- left grant)) (assoc! gains id (clamp 0.0 max-gain gain))))
        (persistent! gains)))))

(defn- pending-at [pending clock]
  (into [] (keep (fn [{:keys [timer] :as correction}]
                   (let [{:keys [done? progress]} (time/sample timer clock)]
                     (when-not done? (assoc correction :remaining (- 1.0 progress)))))) pending))

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
    (cond-> {:op op :domain axis :direction (if up? :up :down) :amount (abs delta)
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

(defn advance
  "Consume resolved scalar targets and the rolling statistics of the delivered
  frame. Each raw source is {:id … :capacity full-white-weight :level 0..1}.
  :actions are requests for this decision only. Keep :state for the next call."
  [previous {:keys [clock epoch sources targets statistics options cue-ids]}]
  (let [clock (time/frame-clock clock)]
    (if-not (due? previous clock epoch options)
      {:state previous :gains (:gains previous {}) :actions [] :due? false :report (:report previous)}
      (let [opts (settings options)
            state (when-not (restarted? previous clock epoch) previous)
            _ (when-not (and (every? #(unit? (get targets %)) [:brightness :activity])
                             (every? (fn [{:keys [id level capacity]}]
                                       (and (some? id) (unit? level)
                                            (time/finite-number? capacity) (not (neg? capacity)))) sources))
                (throw (ex-info "Expected resolved energy targets and calibrated sources" {:targets targets})))
            ids (mapv :id sources)
            _ (when-not (= (count ids) (count (set ids))) (throw (ex-info "Duplicate source IDs" {})))
            total (reduce + 0.0 (map :capacity sources))
            [seed order] (shuffled ids (:order state) (get state :seed (:seed opts)) (:rotate? opts))
            window (or (:window statistics) (:interval opts))
            pending (if (= window (:window state)) (pending-at (:pending state) clock) [])
            brightness (predicted statistics pending :brightness)
            activity (predicted statistics pending :activity)
            light-change (change brightness (:brightness targets) opts)
            activity-change (change activity (:activity targets) opts)
            now (weighted sources (:gains state) total)
            ; Mean/instant is the observed duty factor for a repeating look.
            ; Scaling a once-per-bar flash must preserve its authored dark time.
            duty (if (and (pos? now) (pos? brightness)) (min 1.0 (/ brightness now)) 1.0)
            wanted (clamp 0.0 1.0 (+ now (/ light-change duty)))
            gains (if (some #(pos? (:level %)) sources)
                    (allocate sources order wanted (/ (:overshoot opts) duty) total opts)
                    ;; While all shutters/LFOs are dark, keep the previous selection
                    ;; available for its next pulse, including a newly launched cue.
                    (into {} (map (fn [id] [id (get (:gains state) id (if (pos? (:brightness targets)) 1.0 0.0))])) ids))
            lit-ids (into [] (comp (filter #(pos? (level % (get gains (:id %) 1.0)))) (map :id)) sources)
            delivered (weighted sources gains total)
            change-achieved (* duty (- delivered now))
            residual (- light-change change-achieved)
            ; Keep a falling or mismatched axis from triggering a second raise.
            residual (if (= (pos? residual) (pos? light-change)) residual 0.0)
            ; Cue-driven looks can also replace lamps whose middleware gains fell.
            plan-light (cond
                         (empty? ids) 0.0
                         (> (abs residual) (:tolerance opts)) residual
                         (not= :parameters (:preference opts)) light-change
                         :else 0.0)
            actions (if (empty? ids) []
                        (choose-actions
                        (cond-> []
                          (not (zero? plan-light))
                          (conj (request :brightness plan-light (zero? now) opts ids lit-ids cue-ids))
                          (not (zero? activity-change))
                          (conj (request :activity activity-change (zero? now) opts ids lit-ids cue-ids))) (:strategy opts)))
            ; Remaining, unobserved response of each decision in a rectangular
            ; trailing window prevents repeated corrections to the same error.
            correction {:brightness change-achieved :activity (if (some #(= :activity (:domain %)) actions) activity-change 0.0)
                        :timer (time/start window clock) :remaining 1.0}
            pending (if (every? zero? ((juxt :brightness :activity) correction)) pending
                        (conj (vec (take-last 63 pending)) correction))
            report {:targets targets :forecast {:brightness brightness :activity activity}
                    :raw-brightness (weighted sources {} total) :requested-brightness wanted
                    :estimated-brightness delivered :active-count (count (filter (fn [s]
                        (> (level s (gains (:id s))) (+ (:standby-level opts) 1e-9))) sources))
                    :decision (inc (:decision state 0)) :time-ms (:time-ms clock)}
            next-state {:clock clock :epoch epoch :seed seed :order order :window window :pending pending
                        :gains gains :timer (next-timer (:timer state) (:interval opts) clock)
                        :report report :decision (:decision report)}]
        {:state next-state :report report :actions actions :gains gains :due? true}))))
