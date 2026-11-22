(ns tolglow.automation.policy
  "Deterministic auto-show selection. Proposals are commands, never direct show
  mutations. Emission dependencies use the same compiler and frame resolver as
  real output; the planner is intentionally outside the rendering deadline."
  (:require [clojure.set :as set]
            [afterglow.time :as time]
            [afterglow.render.frame :as render]
            [tolglow.automation.prediction :as prediction]
            [tolglow.lighting-budget :as budget]
            [tolglow.lighting-activity :as activity]
            [tolglow.registry :as registry]
            [tolglow.automation.random :as random]))

(defn initial-state
  "Create one auto-mode session. owner-id must be unique per start so command
  deduplication cannot confuse a restarted planner with its previous session."
  [seed time-ms owner-id]
  (random/next-random seed)
  (when-not (and (registry/finite-number? time-ms) (some? owner-id))
    (throw (ex-info "Auto mode requires explicit time and owner ID" {})))
  {:seed seed :next-at-ms time-ms :owner-id owner-id :tick 0
   :epoch nil :owned-instance-ids []})

(defn record-launch
  "The command shell records successful launch instance IDs for exact cleanup."
  [state instance-id]
  (update state :owned-instance-ids #(vec (distinct (conj % instance-id)))))

(defn- command [model state op payload index time-ms]
  (merge {:op op :command-id (str "auto:" (:owner-id state) ":" (:tick state) ":" index)
          :source {:adapter :auto :session-id (:owner-id state)}
          :show-id (:show-id model) :epoch (:epoch model) :time-ms time-ms} payload))

(defn stop-plan [model state time-ms]
  {:state (assoc state :owned-instance-ids [] :tick (inc (:tick state)))
   :commands (if (= (:epoch state) (:epoch model))
               (mapv (fn [i id] (command model state :instance/end {:instance-id id} i time-ms))
                     (range) (:owned-instance-ids state)) [])})

(defn- interval-duration [config]
  (or (:interval config) (when (contains? config :interval-ms) {:ms (:interval-ms config)}) {:beats 1}))

(defn- launch-options [config]
  (or (:launch config)
      (if (contains? config :interval-ms)
        {:mode :timed :duration-ms (:interval-ms config)}
        {:mode :timed :duration (interval-duration config)})))

(defn- candidates [model config]
  (let [scope (:scope config :all)
        ids (cond
              (= scope :all) nil
              (and (map? scope) (contains? scope :cue-ids)) (set (:cue-ids scope))
              (and (map? scope) (:page-id scope))
              (let [page (get-in model [:pages (:page-id scope)])]
                (set (if (map? page)
                       (or (:cue-ids page) (vals (:cells page))) page)))
              :else (throw (ex-info "Auto scope must be all, cue IDs, or an explicit page ID" {:scope scope})))]
    (->> (get-in model [:registry :cues]) vals
         (filter #(and (or (nil? ids) (ids (:id %)))
                       (not (false? (:auto? %)))))
         (sort-by (comp str :id))
         (mapv #(-> %
                    (update :params (fn [params] (merge params (get-in config [:launch :params]))))
                    (assoc :prediction-launch (launch-options config)))))))

(defn- definition [model cue] (get-in model [:registry :effects (:effect-id cue)]))

(defn- movement? [effect]
  (or (some #{:movement} (:capabilities effect))
      (some #(#{:pan :tilt :aim :direction} (:attribute %)) (registry/output-entries (:outputs effect)))))

(defn- target-ids [fixtures effect]
  (let [all (set (map :id fixtures))
        movement (filter #(#{:pan :tilt :aim :direction} (:attribute %))
                         (registry/output-entries (:outputs effect)))
        output-targets (if (seq movement)
                         (into #{} (mapcat #(:targets % (vec all))) movement) all)]
    (set/intersection all (set (:targets effect (vec all))) output-targets)))

(defn- emitting-targets [model cues time-ms]
  (let [output (prediction/frame model (::patch model) cues time-ms)]
    (into #{} (keep (fn [[id value]]
                     (when (some pos? (:emitted-color value)) id))) (:values output))))

(defn- complete-selection [model pool selected config time-ms]
  (if (= :darkness (:intent config)) {:cues selected :reason :intentional-darkness}
      (let [required (into #{}
                           (mapcat #(when (movement? (definition model %))
                                      (target-ids (:fixtures model) (definition model %)))) selected)]
        (loop [chosen selected remaining (remove (set selected) pool)]
          (let [missing (when (seq required) (set/difference required (emitting-targets model chosen time-ms)))]
            (cond
              (empty? missing) {:cues chosen :reason (if (= chosen selected) :eligible :added-emission)}
              (empty? remaining) {:cues [] :reason :unsatisfied-emission :missing missing}
              :else
              (let [candidate (first remaining)
                    after (emitting-targets model (conj chosen candidate) time-ms)]
                (recur (if (seq (set/intersection missing after)) (conj chosen candidate) chosen)
                       (rest remaining)))))))))

(defn- activity-order [model target candidates]
  (if (nil? target) candidates
    (let [distance (fn [cue]
                     (if-some [score (activity/predicted-score model cue)]
                       (abs (- target score)) Double/POSITIVE_INFINITY))]
      (mapv second (sort-by first (mapv (fn [cue] [(distance cue) cue]) candidates))))))

(defn- cost [model ctx choices time-ms]
  (budget/unmet-watts ctx (prediction/frame model (::patch model) choices time-ms (:standby-block ctx))))

(defn- choose-with-budget [model ctx ordered config time-ms]
  (loop [result {:cues [] :reason :empty-scope} remaining ordered]
    (if (or (empty? remaining) (>= (count (:cues result)) (:count config 1)))
      result
      (let [best
            (reduce (fn [best candidate]
                      (let [with-dependencies (complete-selection model ordered (conj (:cues result) candidate) config time-ms)
                            choices (:cues with-dependencies)]
                        (if (empty? choices) best
                          (let [loss (cost model ctx choices time-ms)
                                option (assoc with-dependencies :unmet-watts loss)]
                            (cond (nil? loss) best
                                  (<= loss 1e-9) (reduced option)
                                  (or (nil? best) (< loss (:unmet-watts best))) option
                                  :else best))))) nil remaining)]
        (if best
          (recur best (remove (set (:cues best)) remaining))
          (if (seq (:cues result)) result {:cues [] :reason :unsatisfied-emission}))))))

(defn- make-room [model ctx selected owned time-ms]
  (loop [model model loss (:unmet-watts selected 0.0) pending owned retired []]
    (if (or (<= loss 1e-9) (empty? pending))
      (assoc selected :retired retired :unmet-watts loss)
      (let [id (first pending)
            without (update model :instances dissoc id)
            next-loss (cost without ctx (:cues selected) time-ms)]
        (if (and next-loss (< next-loss (- loss 1e-9)))
          (recur without next-loss (next pending) (conj retired id))
          (recur model loss (next pending) retired))))))

(defn advance
  "Select cues when the interval expires. Model supplies :registry, canonical
  patched :fixtures. Live :instances are evaluated for exact priority/lifecycle;
  without them optional :assignments act as background contributions. Config has :interval-ms,
  :count (default1), :scope (:all / {:page-id ...} / {:cue-ids [...]}), optional
  :intent :darkness, and :launch {:mode :timed :duration-ms ...}. Missed ticks
  are skipped. Scope also constrains emission companions. Return commands plus
  reasons; pages use the canonical {:cells {coordinate cue-id}} shape, with
  :cue-ids accepted as a compact alias. The caller dispatches commands and feeds
  accepted IDs to record-launch."
  [model config state time-ms]
  (let [duration (interval-duration config)
        clock (time/frame-clock (assoc model :time-ms time-ms))
        normalized (time/normalize-duration duration (:meter clock))
        count (:count config 1)
        timer (:interval-timer state)
        unit (:unit normalized)]
    (when-not (and (registry/finite-number? time-ms) (pos? (:value normalized))
                   (integer? count) (<= 1 count 64)
                   (or (nil? (:activity-target config))
                       (and (registry/finite-number? (:activity-target config)) (<= 0 (:activity-target config) 1))))
      (throw (ex-info "Invalid auto-mode time, interval or count" {:config config :time-ms time-ms})))
    (time/coordinate clock unit)
    (cond
      (and (some? (:epoch state)) (not= (:epoch state) (:epoch model)))
      {:state (-> state (assoc :epoch (:epoch model) :owned-instance-ids [] :next-at-ms time-ms)
                  (dissoc :interval-timer :next-at-beat))
       :commands [] :reasons [{:reason :show-restarted}]}
      (and (= unit :beats) (false? (:playing? clock)))
      {:state state :commands [] :reasons []}
      (and timer (not= duration (get-in timer [:duration :source])))
      {:state (assoc state :interval-timer (time/start duration clock)) :commands [] :reasons []}
      (and timer (not (:done? (time/sample timer clock))))
      {:state state :commands [] :reasons []}
      (and (nil? timer) (= :ms unit) (< time-ms (:next-at-ms state)))
      {:state state :commands [] :reasons []}
      :else
      (let [patch (render/compile-patch (:fixtures model []))
            model (assoc model ::patch patch)
            ctx (budget/planning-context model patch)
            pool (candidates model config)
            [seed ordered] (random/shuffle-values (:seed state) pool)
            ordered (activity-order model (:activity-target config) ordered)
            owned (filterv (fn [id]
                             (let [instance (get-in model [:instances id])]
                               (and (= :running (:phase instance))
                                    (= {:adapter :auto :session-id (:owner-id state)} (:source instance)))))
                           (:owned-instance-ids state))
            result (cond
                     (empty? ordered) {:cues [] :reason :empty-scope}
                     ctx (make-room model ctx (choose-with-budget model ctx ordered config time-ms) owned time-ms)
                     :else (complete-selection model pool (vec (take count ordered)) config time-ms))
            ended (:retired result)
            releases (mapv (fn [i id] (command model state :instance/end {:instance-id id} i time-ms)) (range) ended)
            launch (launch-options config)
            commands (into releases
                           (map-indexed (fn [i cue]
                                          (command model state :cue/launch {:cue-id (:id cue) :launch launch}
                                                   (+ (clojure.core/count releases) i) time-ms)))
                           (:cues result))
            timer (time/start duration clock)
            next-at (+ (:started-at timer) (get-in timer [:duration :value]))]
        {:state (cond-> (assoc state :seed seed :epoch (:epoch model) :interval-timer timer
                               :tick (inc (:tick state)))
                  ctx (assoc :owned-instance-ids (into [] (remove (set ended)) owned))
                  (= :ms unit) (assoc :next-at-ms next-at)
                  (= :beats unit) (assoc :next-at-beat next-at))
         :commands commands :reasons [(dissoc result :cues)]}))))
