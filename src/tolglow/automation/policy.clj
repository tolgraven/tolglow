(ns tolglow.automation.policy
  "Deterministic auto-show selection. Proposals are commands, never direct show
  mutations. Emission dependencies use the same compiler and frame resolver as
  real output; the planner is intentionally outside the rendering deadline."
  (:require [clojure.set :as set]
            [afterglow.time :as time]
            [afterglow.render.frame :as render]
            [tolglow.automation.prediction :as prediction]
            [tolglow.lighting-budget :as budget]
            [tolglow.lighting-capacity :as capacity]
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

(defn- light-forecast [model cues time-ms]
  (let [ctx (::budget model)
        raw (prediction/frame model (::patch model) cues time-ms (:standby-block ctx))
        frame (if ctx (budget/apply-frame ctx raw) raw)
        meter (or (:meter ctx) (force (::meter model)))
        ^doubles levels (capacity/local-brightnesses meter (:universes frame))]
    (cond-> {:emitting (into #{} (keep (fn [[id [index _]]]
                                         (when (pos? (aget levels (int index))) id))) (:writers-by-head meter))}
      ctx (assoc :unmet-watts (budget/unmet-watts ctx raw)))))

(defn- movement-needs [model cues config]
  (when-not (= :darkness (:intent config))
    (into #{} (mapcat #(when (movement? (definition model %))
                        (target-ids (:fixtures model) (definition model %)))) cues)))

(def ^:private light-controls (into registry/emitter-attributes [:color :dimmer :shutter :strobe]))

(defn- light-control? [model missing cue]
  (let [effect (definition model cue)
        intersects? (fn [targets] (or (nil? targets) (some missing targets)))]
    (and (intersects? (:targets effect))
         (some #(and (light-controls (:attribute %))
                     (intersects? (:targets %)))
               (registry/output-entries (:outputs effect))))))

(defn- combined-light [model required missing chosen pool time-ms]
  ;; A dimmer, an emitter and a shutter can be separate cues. When none of them
  ;; alone lights the fixture, audition the group once, then prune redundant cues.
  (let [selected (set chosen)
        helpers (filterv #(and (not (selected %)) (light-control? model missing %)) pool)]
    (when (> (count helpers) 1)
      (let [all (into chosen helpers)
            trial (light-forecast model all time-ms)]
        (when (set/subset? required (:emitting trial))
          (reduce (fn [{:keys [cues] :as result} helper]
                    (let [subset (filterv #(not= helper %) cues)
                          trial (light-forecast model subset time-ms)]
                      (if (set/subset? required (:emitting trial))
                        {:cues subset :forecast trial} result)))
                  {:cues all :forecast trial} (rseq helpers)))))))

(defn- complete-selection [model pool selected config time-ms]
  (let [required (movement-needs model selected config)
        result (fn [chosen forecast]
                 (merge (select-keys forecast [:unmet-watts])
                        {::required required :cues chosen :reason (if (= chosen selected) :eligible :added-emission)}))]
    (if (empty? required)
      {:cues selected :reason (if (= :darkness (:intent config)) :intentional-darkness :eligible)}
      (loop [chosen selected remaining (remove (set selected) pool)
             forecast (light-forecast model selected time-ms)]
        (let [missing (set/difference required (:emitting forecast))]
          (cond
            (empty? missing) (result chosen forecast)
            (empty? remaining)
            (if-let [{:keys [cues forecast]} (combined-light model required missing chosen pool time-ms)]
              (result cues forecast)
              {:cues [] :reason :unsatisfied-emission :missing missing})
            :else (let [proposal (conj chosen (first remaining))
                        after (light-forecast model proposal time-ms)]
                    (if (seq (set/intersection missing (:emitting after)))
                      (recur proposal (next remaining) after)
                      (recur chosen (next remaining) forecast)))))))))

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
                          (let [loss (if (contains? with-dependencies :unmet-watts)
                                       (:unmet-watts with-dependencies) (cost model ctx choices time-ms))
                                option (assoc with-dependencies :unmet-watts loss)]
                            (cond (nil? loss) best
                                  (<= loss 1e-9) (reduced option)
                                  (or (nil? best) (< loss (:unmet-watts best))) option
                                  :else best))))) nil remaining)]
        (if best
          (recur best (remove (set (:cues best)) remaining))
          (if (seq (:cues result)) result {:cues [] :reason :unsatisfied-emission}))))))

(defn- make-room [model ctx selected owned time-ms]
  (let [initial (:unmet-watts selected 0.0)
        cost (fn [trial]
               (if-let [required (::required selected)]
                 (let [forecast (light-forecast trial (:cues selected) time-ms)]
                   (when (set/subset? required (:emitting forecast)) (:unmet-watts forecast)))
                 (cost trial ctx (:cues selected) time-ms)))
        ;; A later opaque cue can hide an earlier owner's contribution. If no
        ;; single removal helps, try releasing their combination. Restore each
        ;; unnecessary instance in reverse age order, preserving human writes.
        trial (fn [model ids] (update model :instances #(apply dissoc % ids)))]
    (loop [current model loss initial pending owned retired []]
      (cond
        (<= loss 1e-9) (assoc selected :retired retired :unmet-watts loss)
        (seq pending)
        (let [id (first pending)
              without (trial current [id])
              next-loss (cost without)]
          (if (and next-loss (< next-loss (- loss 1e-9)))
            (recur without next-loss (next pending) (conj retired id))
            (recur current loss (next pending) retired)))
        :else
        (let [remaining (vec (remove (set retired) owned))
              without (when (> (count remaining) 1) (trial current remaining))
              best (when without (cost without))]
          (if (and best (< best (- loss 1e-9)))
            (loop [kept without pending (rseq remaining) release retired score best]
              (if-let [id (first pending)]
                (let [with (assoc-in kept [:instances id] (get-in model [:instances id]))
                      restored (cost with)]
                  (if (and restored (<= restored (+ score 1e-9)))
                    (recur with (next pending) release restored)
                    (recur kept (next pending) (conj release id) score)))
                (assoc selected :retired release :unmet-watts score)))
            (assoc selected :retired retired :unmet-watts loss)))))))

(defn- budget-plan [model ctx ordered config owned time-ms]
  (let [selected (choose-with-budget model ctx ordered config time-ms)
        ;; A previously owned look can occupy the last visible slot. Auditioning
        ;; without it allows a new motion look to request that slot, with
        ;; make-room retaining any owners that can coexist with the new choice.
        retry? (and (empty? (:cues selected)) (seq owned))
        available (when retry? (update model :instances #(apply dissoc % owned)))
        next-look (when available (choose-with-budget available ctx ordered config time-ms))]
    (if (seq (:cues next-look))
      (make-room model ctx (assoc next-look :unmet-watts Double/POSITIVE_INFINITY) owned time-ms)
      (make-room model ctx selected owned time-ms))))

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
            ctx (budget/planning-context model patch)
            model (assoc model ::patch patch ::budget ctx ::meter (delay (capacity/prepare patch)))
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
                     ctx (budget-plan model ctx ordered config owned time-ms)
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
         :commands commands :reasons [(dissoc result :cues ::required)]}))))
