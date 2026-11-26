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
            [tolglow.autopilot.signal :as signal]
            [tolglow.automation.launch-params :as launch-params]
            [tolglow.automation.random :as random]
            [tolglow.automation.sections :as sections]))

(defn initial-state
  "Create one auto-mode session. owner-id must be unique per start so command
  deduplication cannot confuse a restarted planner with its previous session."
  [seed time-ms owner-id]
  (random/next-random seed)
  (when-not (and (registry/finite-number? time-ms) (some? owner-id))
    (throw (ex-info "Auto mode requires explicit time and owner ID" {})))
  {:seed seed :base-seed seed :next-at-ms time-ms :owner-id owner-id :tick 0
   :epoch nil :owned-instance-ids []})

(defn- manual-controls
  "Operator-set controls on an instance; progress fields are excluded."
  [instance]
  (cond-> (select-keys (:system instance) [:opacity :paused? :attack :release])
    (not (== 1.0 (get-in instance [:system :gain] 1.0))) (assoc :gain (get-in instance [:system :gain]))))

(defn record-launch
  "The command shell records successful launch instance IDs for exact cleanup.
  With the launched instance, also record its parameters and manual controls:
  a later change by anyone else hands that instance to the performer, and auto
  mode never ends it again."
  ([state instance-id]
   (update state :owned-instance-ids #(vec (distinct (conj % instance-id)))))
  ([state instance-id instance]
   (cond-> (record-launch state instance-id)
     instance (update :owned-launches
                      (fn [launches]
                        (conj (vec (remove #(= instance-id (:instance-id %)) launches))
                              {:instance-id instance-id :params (:params instance)
                               :controls (manual-controls instance)}))))))

(defn- taken-over?
  "True when a recorded launch no longer matches its instance: a performer (or
  another writer) changed a parameter or a manual control since auto launched it."
  [model state id]
  (when-let [launch (some #(when (= id (:instance-id %)) %) (:owned-launches state))]
    (when-let [instance (get-in model [:instances id])]
      (or (not= (:params launch) (:params instance))
          (not= (:controls launch) (manual-controls instance))))))

(defn- hand-over
  "Drop instances a performer has taken over, or that have ended, from auto
  ownership. Returns [state taken-over-ids]."
  [model state]
  (let [ids (:owned-instance-ids state)
        taken (filterv #(taken-over? model state %) ids)
        kept (filterv #(and (contains? (:instances model) %) (not (some #{%} taken))) ids)]
    [(cond-> (assoc state :owned-instance-ids kept)
       (contains? state :owned-launches)
       (update :owned-launches (fn [launches] (filterv #(some #{(:instance-id %)} kept) launches))))
     taken]))

(defn- command [model state op payload index time-ms]
  (merge {:op op :command-id (str "auto:" (:owner-id state) ":" (:tick state) ":" index)
          :source {:adapter :auto :session-id (:owner-id state)}
          :show-id (:show-id model) :epoch (:epoch model) :time-ms time-ms} payload))

(defn stop-plan
  "End the instances this session still owns. Instances a performer took over
  stay running."
  [model state time-ms]
  (let [[owned-state _] (if (= (:epoch state) (:epoch model)) (hand-over model state) [state []])]
    {:state (assoc state :owned-instance-ids [] :owned-launches [] :tick (inc (:tick state)))
     :commands (if (= (:epoch state) (:epoch model))
                 (mapv (fn [i id] (command model state :instance/end {:instance-id id} i time-ms))
                       (range) (:owned-instance-ids owned-state)) [])}))

(defn- interval-duration [config]
  (or (:interval config) (when (contains? config :interval-ms) {:ms (:interval-ms config)}) {:beats 1}))

(defn- launch-options [config]
  (or (:launch config)
      (if (contains? config :interval-ms)
        {:mode :timed :duration-ms (:interval-ms config)}
        {:mode :timed :duration (interval-duration config)})))

(defn- cue-categories [model cue]
  (launch-params/cue-categories (registry/cue-effect (:registry model) cue) cue))

(defn- category-match? [model config cue]
  (let [{:keys [include exclude]} (:categories config)
        categories (cue-categories model cue)]
    (and (or (empty? include) (some categories include))
         (not (some categories exclude)))))

(defn- performer-cue-ids
  "Cues someone other than this auto session is running. Auto mode yields
  them rather than stacking a second copy over a performer's look."
  [model state]
  (into #{} (keep (fn [instance]
                    (when (and (= :running (:phase instance))
                               (not= {:adapter :auto :session-id (:owner-id state)} (:source instance)))
                      (:cue-id instance))))
        (vals (:instances model))))

(defn- with-launch-params
  "Attach this decision's seeded parameter choices. The cue's :params become
  the full launch values so emission prediction sees exactly what will run."
  [model config state cue]
  (let [effect (registry/cue-effect (:registry model) cue)
        fixed (get-in config [:launch :params] {})
        chosen (when (and effect (:randomize config)
                          (not= :none (:params (launch-params/normalize (:randomize config)))))
                 (launch-params/choose effect cue (:randomize config)
                                       (:base-seed state (:seed state)) (:tick state) fixed))]
    (-> cue
        (assoc :params (merge (:params cue) (:saved-params cue) fixed (:params chosen)))
        (assoc ::launch-params (merge fixed (:params chosen)))
        (cond-> chosen (assoc ::randomized chosen)))))

(defn- refusal-key
  "What a remembered refusal depends on: the cue and effect versions and the
  patched rig. A change to any of them lets the cue be tried again."
  [model cue]
  {:cue-version (:version cue)
   :effect-version (:version (registry/cue-effect (:registry model) cue))
   :rig (hash (:fixtures model))})

(defn- refused-cue? [model state]
  (let [remembered (:refusals state {})]
    (if (empty? remembered)
      (constantly false)
      (fn [cue] (when-let [entry (get remembered (:id cue))]
                  (= (:key entry) (refusal-key model cue)))))))

(def remembered-refusals
  "Refusal codes that depend only on the cue, its effect and the patched rig,
  so they hold until one of those changes. Refusals that depend on the moment
  (a head's current pan wrap, a section's launch options, a stale gesture) are
  reported for that decision only and tried again next time."
  #{:incapable-target :invalid-targets :invalid-params :invalid-definition
    :stale-calibration :unknown-cue :unknown-effect})

(defn- remember-refusal [state model cue-id code]
  (let [cue (get-in model [:registry :cues cue-id])]
    (if (and cue (remembered-refusals code))
      (assoc-in state [:refusals cue-id] {:code code :key (refusal-key model cue)})
      state)))

(defn record-refusal
  "The command shell reports a launch the show rejected at dispatch. Remember
  a definition-dependent refusal like one met while planning, so later
  decisions skip that cue until its definition or the rig changes."
  [state model cue-id ack]
  (remember-refusal state model cue-id (:code ack)))

(defn- candidates [model config state]
  (let [scope (:scope config :all)
        ids (cond
              (= scope :all) nil
              (and (map? scope) (contains? scope :cue-ids)) (set (:cue-ids scope))
              (and (map? scope) (:page-id scope))
              (let [page (get-in model [:pages (:page-id scope)])]
                (set (if (map? page)
                       (or (:cue-ids page) (vals (:cells page))) page)))
              :else (throw (ex-info "Auto scope must be all, cue IDs, or an explicit page ID" {:scope scope})))
        yielded (if (false? (:yield-to-performers? config)) #{} (performer-cue-ids model state))
        refused (refused-cue? model state)]
    (->> (get-in model [:registry :cues]) vals
         (filter #(and (or (nil? ids) (ids (:id %)))
                       (not (false? (:auto? %))) (not (:show-control? %))
                       (not (:show-control? (registry/cue-effect (:registry model) %)))
                       (not (yielded (:id %)))
                       (not (refused %))
                       (category-match? model config %)))
         (sort-by (comp str :id))
         (mapv #(-> (with-launch-params model config state %)
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

(defn- lit-plan
  "Choose up to :count cues (with any emission companions) while the predicted
  number of emitting fixtures, existing light included, stays within :max-lit.
  A quiet section never extinguishes a performer's light; it adds nothing when
  that light already fills the limit."
  [model pool ordered config time-ms]
  (let [limit (:max-lit config) wanted (:count config 1)]
    (loop [chosen [] picks 0 remaining ordered best nil]
      (if (or (empty? remaining) (>= picks wanted))
        (if (seq chosen) (assoc best :cues chosen) {:cues [] :reason :lit-limit :max-lit limit})
        (let [candidate (first remaining)]
          (if (some #{candidate} chosen)
            (recur chosen picks (rest remaining) best)
            (let [trial (complete-selection model pool (conj chosen candidate) config time-ms)
                  cues (:cues trial)
                  lit (when (seq cues) (count (:emitting (light-forecast model cues time-ms))))]
              (if (and lit (<= lit limit))
                (recur cues (inc picks) (rest remaining) (assoc trial :lit lit))
                (recur chosen picks (rest remaining) best)))))))))

(defn- activity-target
  "A fixed target in [0,1] or a signal binding (an LFO, audio energy or a
  variable) sampled at this decision and clamped to [0,1]."
  [model config time-ms]
  (let [target (:activity-target config)]
    (cond
      (nil? target) nil
      (number? target) target
      :else (max 0.0 (min 1.0 (double (signal/value (assoc model :time-ms time-ms) target)))))))

(defn- identifier? [x] (or (keyword? x) (and (string? x) (seq x))))

(def ^:private config-keys
  #{:interval :interval-ms :count :scope :intent :launch :activity-target
    :randomize :categories :max-lit :yield-to-performers? :sections})

(defn validate-config
  "Path/message errors for an auto-mode config, [] when valid."
  [config]
  (if-not (and (map? config) (registry/data? config))
    [{:path [] :message "Auto config must be portable map data"}]
    (vec
     (concat
      (when-not (every? config-keys (keys config))
        [{:path [] :message "Unknown auto setting" :keys (vec (remove config-keys (keys config)))}])
      (when-let [duration (or (:interval config) (when (contains? config :interval-ms) {:ms (:interval-ms config)}))]
        (when-not (and (empty? (time/validate-duration duration)) (pos? (val (first duration))))
          [{:path [:interval] :message "Interval needs one positive beats, bars, phrases or ms value"}]))
      (when-not (and (integer? (:count config 1)) (<= 1 (:count config 1) 64))
        [{:path [:count] :message "Count must be an integer from 1 to 64"}])
      (let [scope (:scope config :all)]
        (when-not (or (= :all scope)
                      (and (map? scope) (= [:page-id] (keys scope)) (identifier? (:page-id scope)))
                      (and (map? scope) (= [:cue-ids] (keys scope)) (vector? (:cue-ids scope))
                           (every? identifier? (:cue-ids scope))))
          [{:path [:scope] :message "Scope is :all, {:page-id id} or {:cue-ids [ids]}"}]))
      (when (contains? config :launch)
        (let [{:keys [mode duration duration-ms params] :as launch} (:launch config)]
          (cond
            (not (and (map? launch) (every? #{:mode :duration :duration-ms :params :priority :release-duration} (keys launch))))
            [{:path [:launch] :message "Launch takes :mode, :duration or :duration-ms, :params, :priority and :release-duration"}]
            (not (#{:persistent :timed :one-shot} (or mode :persistent)))
            [{:path [:launch :mode] :message "Launch mode is :persistent, :timed or :one-shot"}]
            (and (some? duration) (not (and (empty? (time/validate-duration duration)) (pos? (val (first duration))))))
            [{:path [:launch :duration] :message "Launch duration needs one positive beats, bars, phrases or ms value"}]
            (and (some? duration-ms) (not (and (registry/finite-number? duration-ms) (pos? duration-ms))))
            [{:path [:launch :duration-ms] :message "Launch duration must be positive milliseconds"}]
            (not (or (nil? params) (map? params)))
            [{:path [:launch :params] :message "Launch params must be a map"}])))
      (when-not (contains? #{nil :darkness} (:intent config))
        [{:path [:intent] :message "Intent is :darkness or absent"}])
      (let [target (:activity-target config)]
        (when-not (or (nil? target)
                      (and (registry/finite-number? target) (<= 0 target 1))
                      (and (map? target) (signal/valid? target)))
          [{:path [:activity-target] :message "Activity target is a number in [0,1] or a signal binding"}]))
      (when (contains? config :randomize)
        (map #(update % :path (fn [p] (into [:randomize] p))) (launch-params/validate (:randomize config))))
      (when (contains? config :categories)
        (let [{:keys [include exclude] :as filters} (:categories config)]
          (when-not (and (map? filters) (every? #{:include :exclude} (keys filters))
                         (every? #(or (nil? %) (and (vector? %) (every? keyword? %))) [include exclude]))
            [{:path [:categories] :message "Category filters are {:include [..] :exclude [..]} keyword vectors"}])))
      (when-not (or (nil? (:max-lit config)) (nat-int? (:max-lit config)))
        [{:path [:max-lit] :message "Lit-fixture limit must be a nonnegative integer"}])
      (when-not (contains? #{nil true false} (:yield-to-performers? config))
        [{:path [:yield-to-performers?] :message "Yield to performers must be boolean"}])
      (when (contains? config :sections)
        (map #(update % :path (fn [p] (into [:sections] p)))
             (sections/validate (:sections config) validate-config)))))))

(defn validate-references
  "Errors for scopes (top level and in each section) that name a page or cue
  the show does not have. validate-config checks shape; this checks the show."
  [model config]
  (let [check (fn [path scope]
                (cond
                  (and (map? scope) (contains? scope :page-id) (not (get-in model [:pages (:page-id scope)])))
                  [{:path (conj path :scope) :message (str "No saved page " (pr-str (:page-id scope)))}]
                  (and (map? scope) (vector? (:cue-ids scope)))
                  (when-let [missing (seq (remove #(get-in model [:registry :cues %]) (:cue-ids scope)))]
                    [{:path (conj path :scope) :message (str "No saved cue " (pr-str (first missing)))}])))]
    (vec (concat (check [] (:scope config))
                 (mapcat (fn [i section] (check [:sections i :config] (get-in section [:config :scope])))
                         (range) (:sections config))))))

(defn sanitize-config
  "For a saved show: set aside settings this planner does not know (top level
  and in section configs) instead of refusing the whole show. Returns
  {:config .. :dropped [paths] :unrecognized {:config {k v} :sections {id {k v}}}};
  unrecognized values are kept verbatim so restore-unrecognized can write them
  back on the next save. New configure commands stay strict."
  [config]
  (if-not (map? config) {:config config :dropped [] :unrecognized {}}
    (let [unknown (vec (remove config-keys (keys config)))
          top (select-keys config unknown)
          config (apply dissoc config unknown)
          sections (when (vector? (:sections config))
                     (map-indexed (fn [i section]
                                    (if (map? (:config section))
                                      (let [{c :config d :dropped u :unrecognized} (sanitize-config (:config section))]
                                        {:section (assoc section :config c)
                                         :dropped (mapv #(into [:sections i :config] %) d)
                                         :unrecognized (when (seq (:config u)) [(:id section) (:config u)])})
                                      {:section section :dropped []}))
                                  (:sections config)))
          by-section (into {} (keep :unrecognized) sections)]
      {:config (cond-> config sections (assoc :sections (mapv :section sections)))
       :dropped (into (mapv vector unknown) (mapcat :dropped sections))
       :unrecognized (cond-> {} (seq top) (assoc :config top) (seq by-section) (assoc :sections by-section))})))

(defn restore-unrecognized
  "Put settings sanitize-config set aside back into config, verbatim, so a save
  keeps what an older or newer build wrote. Section settings return to the
  section with the same ID; a section that no longer exists drops them."
  [config {top :config by-section :sections}]
  (cond-> (merge top config)
    (and (seq by-section) (vector? (:sections config)))
    (update :sections (fn [sections]
                        (mapv #(if-let [extra (get by-section (:id %))]
                                 (update % :config (fn [c] (merge extra c)))
                                 %)
                              sections)))))

(defn- plan-without-refusals
  "Run plan over the pool. A cue whose launch the show would refuse (a head
  that cannot take one of its attributes, a stale aim path) is dropped with
  its refusal and the plan runs again, so one unlaunchable cue cannot stall
  auto mode. Returns [result refusals]."
  [plan pool ordered]
  (loop [pool pool ordered ordered refused []]
    (let [outcome (try {:result (plan pool ordered)}
                       (catch clojure.lang.ExceptionInfo e
                         (let [{:keys [cue-id ack]} (ex-data e)]
                           ;; The pool shrinks by one per refusal, so this ends.
                           (if (and cue-id ack (some #(= cue-id (:id %)) pool))
                             {:refused {:cue-id cue-id :code (:code ack) :message (:message ack)}}
                             (throw e)))))]
      (if-let [r (:refused outcome)]
        (let [keep? #(not= (:cue-id r) (:id %))]
          (recur (filterv keep? pool) (filterv keep? ordered) (conj refused r)))
        [(:result outcome) refused]))))

(defn normalize-config
  "Turn the textual choices a JSON client sends into the planner's keywords:
  section transitions, category filters and randomize selections. Other
  values pass through for validate-config to judge."
  [config]
  (let [words (fn [xs] (if (vector? xs) (mapv #(if (string? %) (keyword %) %) xs) xs))
        categories (fn [filters] (cond-> filters (map? filters)
                                   (-> (cond-> (contains? filters :include) (update :include words))
                                       (cond-> (contains? filters :exclude) (update :exclude words)))))
        one (fn one [config]
              (cond-> config
                (map? (:categories config)) (update :categories categories)
                (map? (:randomize config)) (update :randomize #(-> (launch-params/normalize %)
                                                                   (cond-> (vector? (:categories %)) (update :categories words))))
                (vector? (:sections config))
                (update :sections (fn [sections]
                                    (mapv #(cond-> %
                                             (string? (:transition %)) (update :transition keyword)
                                             (map? (:config %)) (update :config one))
                                          sections)))))]
    (if (map? config) (one config) config)))

(defn keep-plan
  "Carry the authored plan of a previous config into a new one: its sections,
  category filters, lit limit, performer policy, a signal-bound activity target
  and its randomize domains (palette, ranges, maximum change, categories, saved
  policy). Domains are kept even when the new config does not randomize: a
  stop keeps the previous selection; a start without randomize keeps the
  domains with :params :none, so a later start can vary them again. New
  settings win where both are given."
  ([previous config] (keep-plan previous config true))
  ([previous config enabled?]
   (let [plan (cond-> (select-keys previous [:sections :categories :max-lit :yield-to-performers?])
                ;; A signal-bound target (an LFO or audio energy) is part of the plan.
                (map? (:activity-target previous)) (assoc :activity-target (:activity-target previous)))
         previous-random (:randomize previous)
         domains (select-keys previous-random [:palette :ranges :max-change :categories :saved])]
     (cond-> (merge plan config)
       (:randomize config) (update :randomize #(merge domains %))
       (and (nil? (:randomize config)) previous-random (not enabled?)) (assoc :randomize previous-random)
       (and (nil? (:randomize config)) (seq domains) enabled?) (assoc :randomize (assoc domains :params :none))))))

(defn- end-commands [model state ids force? time-ms]
  (mapv (fn [i id] (command model state :instance/end (cond-> {:instance-id id} force? (assoc :force? true)) i time-ms))
        (range) ids))

(defn advance
  "Select cues when the interval expires. Model supplies :registry, canonical
  patched :fixtures. Live :instances are evaluated for exact priority/lifecycle;
  without them optional :assignments act as background contributions. Config has :interval-ms,
  :count (default1), :scope (:all / {:page-id ...} / {:cue-ids [...]}), optional
  :intent :darkness, and :launch {:mode :timed :duration-ms ...}. Missed ticks
  are skipped. Scope also constrains emission companions. Return commands plus
  reasons; pages use the canonical {:cells {coordinate cue-id}} shape, with
  :cue-ids accepted as a compact alias. The caller dispatches commands and feeds
  accepted IDs to record-launch. A host can supply its :patch and :meter as
  values or delays and the :previous-budget from its last committed frame.
  Delayed plans are realized only when a selection is due.

  #217 settings: :randomize chooses launch parameters within each schema's
  domain (see tolglow.automation.launch-params); :categories filters cues by
  category; :max-lit caps emitting fixtures; :yield-to-performers? (default
  true) skips cues someone else is running; :activity-target may be a signal
  binding; :sections plays a weighted seeded plan of musical sections whose
  :config overrides these settings. A stopped transport makes no decision and
  holds every timer. An instance a performer changes leaves auto ownership."
  [model config state time-ms & [plans]]
  (let [errors (validate-config config)
        _ (when (seq errors)
            (let [{:keys [path message]} (first errors)]
              (throw (ex-info (str "Invalid auto config: " message (when (seq path) (str " at " (pr-str path))))
                              {:errors errors :config config}))))
        clock (time/frame-clock (assoc model :time-ms time-ms))
        _ (when-not (registry/finite-number? time-ms)
            (throw (ex-info "Invalid auto-mode time, interval or count" {:config config :time-ms time-ms})))]
    (cond
      (and (some? (:epoch state)) (not= (:epoch state) (:epoch model)))
      {:state (-> state (assoc :epoch (:epoch model) :owned-instance-ids [] :next-at-ms time-ms)
                  (dissoc :interval-timer :next-at-beat :section :owned-launches :refusals))
       :commands [] :reasons [{:reason :show-restarted}]}

      (false? (:playing? clock))
      {:state state :commands [] :reasons []}

      :else
      (let [[state taken] (hand-over model state)
            handed (when (seq taken) [{:reason :performer-took-over :instance-ids taken}])
            plan (:sections config)
            step (when plan (sections/step plan (:section state) (:base-seed state (:seed state)) clock))
            section (when plan (sections/current plan (:state step)))
            entered (:entered step)
            transition (when (:changed? step) (:transition entered :overlap))
            state (cond-> state
                    step (assoc :section (:state step))
                    entered (-> (dissoc :interval-timer) (assoc :next-at-ms time-ms)))
            config (if section (sections/effective-config config section) config)
            duration (interval-duration config)
            normalized (time/normalize-duration duration (:meter clock))
            count (:count config 1)
            timer (:interval-timer state)
            unit (:unit normalized)
            tag (fn [reason] (cond-> reason section (assoc :section (:id section))))]
        (time/coordinate clock unit)
        (cond
          (and timer (not= duration (get-in timer [:duration :source])))
          {:state (assoc state :interval-timer (time/start duration clock)) :commands [] :reasons (vec handed)}
          (and timer (not (:done? (time/sample timer clock))))
          {:state state :commands [] :reasons (vec handed)}
          (and (nil? timer) (= :ms unit) (< time-ms (:next-at-ms state)))
          {:state state :commands [] :reasons (vec handed)}
          :else
          (let [plans (force plans)
                patch (or (force (:patch plans)) (render/compile-patch (:fixtures model [])))
                meter (or (:meter plans) (delay (capacity/prepare patch)))
                ctx (when (budget/enabled? model)
                      (budget/prepare-frame-context (:previous-budget plans) model patch (force meter)))
                ;; A cut or release transition ends this session's instances
                ;; before the new section chooses, so the prediction sees the
                ;; room the new section starts from.
                cut-ids (when (#{:cut :release} transition)
                          (filterv #(= :running (get-in model [:instances % :phase])) (:owned-instance-ids state)))
                cuts (end-commands model state cut-ids (= :cut transition) time-ms)
                model (cond-> model (seq cut-ids) (update :instances #(apply dissoc % cut-ids)))
                state (cond-> state (seq cut-ids)
                        (-> (update :owned-instance-ids #(filterv (complement (set cut-ids)) %))
                            (update :owned-launches #(filterv (fn [l] (not (some #{(:instance-id l)} cut-ids))) %))))
                model (assoc model ::patch patch ::budget ctx ::meter meter)
                pool (candidates model config state)
                [seed ordered] (random/shuffle-values (:seed state) pool)
                ordered (activity-order model (activity-target model config time-ms) ordered)
                owned (filterv (fn [id]
                                 (let [instance (get-in model [:instances id])]
                                   (and (= :running (:phase instance))
                                        (= {:adapter :auto :session-id (:owner-id state)} (:source instance)))))
                               (:owned-instance-ids state))
                [result refused] (plan-without-refusals
                                  (fn [pool ordered]
                                    (cond
                                      (empty? ordered) {:cues [] :reason :empty-scope}
                                      (:max-lit config) (lit-plan model pool ordered config time-ms)
                                      ctx (budget-plan model ctx ordered config owned time-ms)
                                      :else (complete-selection model pool (vec (take count ordered)) config time-ms)))
                                  pool ordered)
                ended (:retired result)
                releases (mapv (fn [i id] (command model state :instance/end {:instance-id id} (+ (clojure.core/count cuts) i) time-ms))
                               (range) ended)
                launch (launch-options config)
                commands (into (into cuts releases)
                               (map-indexed (fn [i cue]
                                              (command model state :cue/launch
                                                       {:cue-id (:id cue)
                                                        :launch (cond-> launch (seq (::launch-params cue))
                                                                  (assoc :params (::launch-params cue)))}
                                                       (+ (clojure.core/count cuts) (clojure.core/count releases) i) time-ms)))
                               (:cues result))
                randomized (into (sorted-map-by #(compare (str %1) (str %2)))
                                 (keep (fn [cue] (when-let [r (::randomized cue)] [(:id cue) r])))
                                 (:cues result))
                timer (time/start duration clock)
                next-at (+ (:started-at timer) (get-in timer [:duration :value]))]
            {:state (cond-> (assoc state :seed seed :epoch (:epoch model) :interval-timer timer
                                   :tick (inc (:tick state)))
                      (seq refused)
                      (as-> s (reduce (fn [s {:keys [cue-id code]}] (remember-refusal s model cue-id code)) s refused))
                      ctx (assoc :owned-instance-ids (into [] (remove (set ended)) owned))
                      (= :ms unit) (assoc :next-at-ms next-at)
                      (= :beats unit) (assoc :next-at-beat next-at))
             :commands commands
             :reasons (cond-> (vec handed)
                        (seq refused) (conj (tag {:reason :launch-refused :refused refused}))
                        ;; A cue or page deleted after configure: plan without it, and say so.
                        (seq (validate-references model config))
                        (conj (tag {:reason :scope-missing :missing (mapv :message (validate-references model config))}))
                        (seq cut-ids) (conj (tag {:reason transition :instance-ids cut-ids}))
                        true (conj (tag (cond-> (dissoc result :cues ::required)
                                          (seq randomized) (assoc :randomized randomized)))))}))))))
