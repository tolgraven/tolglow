(ns tolglow.lighting-budget.agent-policy
  "Whether an agent may loosen the lighting budget. By default it may only
  tighten it: lower a limit, an active-source count or a standby level, add a
  rule or enable budgets. An agent may relax its own tightening again, but never
  past the setting it started from, and never another agent's tightening. The
  operator, Studio and the REPL stay unrestricted.

  The same policy covers a limit bound to a cue parameter: an agent may not
  raise the value such a limit reads, whether it writes the parameter, launches
  or ends the cue, revises its definition, runs a macro or applies a staged
  draft (`admit-bound`).

  Change the policy in one place: `policy` below. See
  docs/group-lighting-scenes.md#what-chat-may-change."
  (:require [afterglow.control.params :as params]
            [tolglow.autopilot.signal :as signal]
            [tolglow.lighting-budget.allocation :as allocation]
            [tolglow.registry :as registry]))

(def policy
  "How the show admits a lighting budget change whose source is an agent:
  a change to the budget settings, or any write that moves a cue parameter
  a bound limit reads (`admit-bound`).

  :tighten-only  (default) refuse any change that would let more light
                 through than the operator's setting allows.
  :unrestricted  admit agent changes like operator changes.

  To loosen the default for a running process, from the REPL:
  (alter-var-root #'tolglow.lighting-budget.agent-policy/policy (constantly :unrestricted))"
  :tighten-only)

(def ^:private ceiling-key
  "Model extension that remembers the setting an agent started from while it
  has tightened the budget: {:applied <settings it committed> :operator
  <settings before that> :session {:adapter :agent :session-id ..}}. It applies
  only to that agent session, and only while the settings still equal
  :applied. Any other writer replaces the settings, and the record then falls
  away, so the operator's latest setting is the ceiling again."
  ::ceiling)

(defn agent-source? [source]
  (and (map? source) (contains? #{:agent "agent"} (:adapter source))))

(defn- session [source] (select-keys source [:adapter :session-id]))

(defn ceiling
  "The lighting budget settings an agent may not loosen past on this model.
  With a source, an agent's own earlier tightening does not lower its ceiling;
  without one, the record's starting point is returned whenever it applies."
  ([model] (ceiling model nil))
  ([model source]
   (let [current (get-in model [:settings :lighting-budget])
         {:keys [applied operator] owner :session} (get-in model [:extensions ceiling-key])]
     (if (and (some? applied) (= applied current) (or (nil? source) (= owner (session source))))
       operator current))))

(def ^:private unbounded (allocation/settings {}))

(defn- normalized [settings] (if (map? settings) (allocation/settings settings) unbounded))

(defn- rule
  "The effective rule at path: scope defaults where budgets are off or no rule is set."
  [settings path]
  (merge allocation/scope-defaults (when (:enabled? settings) (get-in settings path))))

(defn- min-form? [limit] (and (map? limit) (= :min (:op limit))))

(defn- no-looser?
  "Is limit `p` certainly at or below limit `c` whatever the bindings read?
  p is no looser than min(a, b) when it is no looser than both; min(a, b) is no
  looser than c when either argument is. A binding is no looser only than
  itself: its current value says nothing about where it can go."
  [c p]
  (cond (= p c) true
        ;; p <= every argument of c means p <= their minimum.
        (min-form? c) (every? #(no-looser? % p) (:args c))
        (min-form? p) (boolean (some #(no-looser? c %) (:args p)))
        (or (map? p) (map? c)) false
        :else (<= (double p) (+ (double c) 1e-9))))

(defn- standby-look [{:keys [standby-mode standby-level]}]
  (if (= :all standby-mode) (double standby-level) 0.0))

(defn- rule-loosened [path ceiling proposed]
  (let [entry (fn [field c p] {:path (conj path field) :ceiling c :proposed p})
        {c-limit :limit c-active :max-active c-standby :standby-level} ceiling
        {p-limit :limit p-active :max-active p-standby :standby-level} proposed]
    (cond-> []
      (not (no-looser? c-limit p-limit)) (conj (entry :limit c-limit p-limit))

      (and (some? p-active) (some? c-active) (> p-active c-active)) (conj (entry :max-active c-active p-active))
      (and (nil? p-active) (some? c-active)) (conj (entry :max-active c-active nil))

      (> (double p-standby) (double c-standby)) (conj (entry :standby-level c-standby p-standby))

      (> (standby-look proposed) (standby-look ceiling))
      (conj (entry :standby-mode (:standby-mode ceiling) (:standby-mode proposed))))))

(defn- rule-paths [& settings]
  (into [[:global]] (map #(vector :groups %)) (distinct (mapcat (comp keys :groups) settings))))

(defn loosened
  "What `proposed` loosens relative to `ceiling`, as [{:path :ceiling :proposed}].
  Empty when proposed lets no more light through anywhere. Strategy and fixture
  order only redistribute light and are free. Any change to :unrated-watts can
  raise a cap (more estimated watts make a larger allowance for rated fixtures)."
  [ceiling proposed]
  (let [ceiling (normalized ceiling)
        proposed (normalized proposed)]
    (if (and (:enabled? ceiling) (not (:enabled? proposed)))
      [{:path [:enabled?] :ceiling true :proposed false}]
      (cond-> (into [] (mapcat #(rule-loosened % (rule ceiling %) (rule proposed %))) (rule-paths ceiling proposed))
        (not= (double (:unrated-watts proposed)) (double (:unrated-watts ceiling)))
        (conj {:path [:unrated-watts] :ceiling (:unrated-watts ceiling) :proposed (:unrated-watts proposed)})))))

(defn- under-binding
  "Keep a bound ceiling limit and cap it with a fixed proposal:
  {:op :min :args [binding fixed]}. The binding keeps moving; the agent's value
  only ever lowers it. A ceiling that is already capped keeps one level: its
  fixed argument becomes the lower of the two, so alternating conversations do
  not nest the form."
  [ceiling-limit fixed]
  (let [default (min (double fixed) (double (:default ceiling-limit 0.0)))]
    (if (and (min-form? ceiling-limit) (some number? (:args ceiling-limit)))
      (assoc ceiling-limit :default default
             :args (mapv #(if (number? %) (min (double %) (double fixed)) %) (:args ceiling-limit)))
      {:op :min :args [ceiling-limit fixed] :default default})))

(defn compose
  "Rewrite each fixed limit an agent proposes where the ceiling's limit is a
  binding, so it caps that binding instead of replacing it."
  [ceiling proposed]
  (let [ceiling (normalized ceiling)]
    (if-not (and (map? proposed) (:enabled? ceiling))
      proposed
      (reduce (fn [settings path]
                (let [c (:limit (rule ceiling path))
                      p (get-in settings (conj path :limit))]
                  (if (and (map? c) (number? p))
                    (assoc-in settings (conj path :limit) (under-binding c p))
                    settings)))
              proposed (rule-paths ceiling proposed)))))

(defn- refuse! [problems policy]
  (throw (ex-info (str "An agent may tighten the lighting budget but not loosen it past the operator's setting: "
                       (pr-str (mapv :path problems)))
                  {:code :agent-budget-loosening :policy policy :loosened problems})))

;; Limits bound to a cue parameter ------------------------------------------

(def ^:private bound-key
  "Model extension for the bound limits an agent has tightened:
  {path {:ceiling <value before> :applied <value it left> :session ..}}. A
  record applies only to its agent session and only while the limit still
  reads :applied, so the agent may relax its own tightening back to :ceiling."
  ::bound-ceiling)

(def ^:private epsilon 1e-9)

(defn bound-limits
  "Each budget rule limit that reads a cue parameter, as {path limit}. Rules
  kept while budgets are off count too, so a raise cannot wait for the
  operator to enable them."
  [model]
  (let [settings (get-in model [:settings :lighting-budget])]
    (if-not (map? settings)
      {}
      (into {} (keep (fn [path]
                       (let [limit (get-in settings (conj path :limit))]
                         (when (map? limit) [path limit]))))
            (rule-paths settings)))))

(defn- leaves [limit]
  (filter :cue-id (tree-seq #(and (map? %) (vector? (:args %))) :args limit)))

(defn bound-cues
  "The cue IDs the model's bound budget limits read."
  [model]
  (into #{} (comp (mapcat leaves) (map :cue-id)) (vals (bound-limits model))))

(defn- limit-value
  "The effective limit as the allocator resolves it, or nil if it cannot be read."
  [model limit]
  (try (-> (double (signal/value model limit)) (min 1.0) (max 0.0))
       (catch Exception _ nil)))

(defn- without-agent-instances
  "The model with every agent-launched instance gone: what the limit reads
  once they end, by a stop, a duration or a release."
  [model]
  (update model :instances #(into {} (remove (fn [[_ instance]] (agent-source? (:source instance)))) %)))

(defn- launch-value
  "The parameter a fresh launch of the cue would start with: its effect's
  defaults under the cue's own and saved parameters. The binding's default
  when the cue or its effect is missing."
  [model {:keys [cue-id param] :as leaf}]
  (let [cue (get-in model [:registry :cues cue-id])
        effect (when cue (registry/cue-effect (:registry model) cue))
        values (when effect
                 (try (params/resolve-values (:params effect {}) (merge (:params cue {}) (:saved-params cue {})))
                      (catch Exception _ nil)))
        value (get values param)]
    (if (number? value) value (:default leaf 0))))

(defn- substitute [limit f]
  (cond (:cue-id limit) (f limit)
        (and (map? limit) (vector? (:args limit))) (update limit :args (partial mapv #(substitute % f)))
        :else limit))

(defn- probes
  "What a bound limit reads now (:current), once agent-launched instances end
  (:latent) and on a fresh launch of each cue it reads (:launch)."
  [model limit]
  {:current (limit-value model limit)
   :latent (limit-value (without-agent-instances model) limit)
   :launch (limit-value model (substitute limit #(launch-value model %)))})

(defn- started-runs
  "Composition runs the write started, as {id definition}."
  [before after]
  (into {} (keep (fn [[id run]]
                   (when-not (= (:owner-id run) (get-in before [:composition-runs id :owner-id]))
                     [id (:definition run)])))
        (:composition-runs after)))

(defn- macro-problems
  "A macro or sequence launches its members as itself, after the write that
  started it, so their parameters and their end escape this check. An agent
  may not start one with a member that a bound limit reads."
  [before after cues]
  (for [[id definition] (started-runs before after)
        slot (:slots definition)
        :when (contains? cues (:cue-id slot))]
    {:path [:compositions id :slots (:id slot)] :reads (:cue-id slot) :probe :macro}))

(defn- same-probes? [a b]
  (every? (fn [k] (let [x (get a k) y (get b k)]
                    (or (= x y) (and (number? x) (number? y) (< (Math/abs (- (double x) (double y))) epsilon)))))
          [:current :latent :launch]))

(defn- bound-problems
  "Each probe of each bound limit that the write raises past its ceiling: the
  value before the write, or for the agent session that tightened it, the
  value before that tightening. Returns {:problems [..] :starts {path [start now]}}."
  [before after source limits records]
  (let [owner (session source)]
    (reduce-kv
     (fn [result path limit]
       (let [was (probes before limit) now (probes after limit)
             {:keys [ceiling applied] record-owner :session} (get records path)
             start (if (and applied (= owner record-owner) (same-probes? applied was)) ceiling was)
             reads (mapv #(select-keys % [:cue-id :param]) (leaves limit))
             raised (fn [probe]
                      (let [ceiling (get start probe) proposed (get now probe)]
                        (when (and (some? ceiling) (some? proposed) (> proposed (+ ceiling epsilon)))
                          {:path (conj path :limit) :reads reads :probe probe :ceiling ceiling :proposed proposed})))]
         (-> result
             (update :problems into (keep raised [:current :latent :launch]))
             (assoc-in [:starts path] [start now]))))
     {:problems [] :starts {}} limits)))

(defn- refuse-bound! [problems policy]
  (throw (ex-info (str "An agent may not raise a lighting budget limit bound to a cue parameter: "
                       (pr-str (mapv (juxt :path :probe) problems)))
                  {:code :agent-budget-loosening :policy policy :loosened problems})))

(defn admit-bound
  "Commit-time check for any write that can move a cue parameter a budget
  limit reads. `before` is the model the write applies to and `after` the
  result. Under :tighten-only an agent write throws (code
  :agent-budget-loosening) when it raises such a limit now, once its own
  launches end, or on a later launch of the cue, or when it starts a macro
  with a bound member. An admitted agent write that lowers a limit records
  its session's ceiling, so the same session may relax it again. A write by
  anyone else drops the record of each limit whose value it moves."
  ([before after source] (admit-bound before after source policy))
  ([before after source policy]
   (let [limits (bound-limits before)
         records (get-in after [:extensions bound-key])]
     (cond
       (empty? limits) after

       (not (agent-source? source))
       (if-let [moved (seq (remove #(or (not (contains? limits %))
                                        (same-probes? (probes before (get limits %)) (probes after (get limits %))))
                                   (keys records)))]
         (let [left (apply dissoc records moved)]
           (if (empty? left) (update after :extensions dissoc bound-key) (assoc-in after [:extensions bound-key] left)))
         after)

       (not= :tighten-only policy) after

       :else
       (let [{:keys [problems starts]} (bound-problems before after source limits (get-in before [:extensions bound-key]))
             problems (into problems (macro-problems before after (bound-cues before)))
             _ (when (seq problems) (refuse-bound! problems policy))
             records (reduce-kv (fn [records path [start now]]
                                  (if (same-probes? start now)
                                    (dissoc records path)
                                    (assoc records path {:ceiling start :applied now :session (session source)})))
                                (or records {}) starts)]
         (if (empty? records)
           (cond-> after (contains? (:extensions after) bound-key) (update :extensions dissoc bound-key))
           (assoc-in after [:extensions bound-key] records)))))))

(defn bound-refusal
  "A rejected command result for an :agent-budget-loosening error, in the
  shape the shared reducer uses, with the problems under :details as an
  owner-level refusal carries them. `model` is the model left unchanged."
  [model command error]
  (let [details (ex-data error)]
    {:model model :events []
     :ack (merge (select-keys model [:show-id :epoch :revision])
                 {:command-id (:command-id command) :status :rejected :code (:code details)
                  :message (ex-message error) :details details})}))

(defn guard-result
  "Run `admit-bound` over an accepted command `result` from `before` for the
  command's source. A refusal returns `bound-refusal` with `before`; anything
  else the result, with its model carrying the ceiling record."
  [before command result]
  (if (and (= :accepted (get-in result [:ack :status])) (map? (:model result))
           (or (agent-source? (:source command)) (contains? (:extensions (:model result)) bound-key)))
    (try (assoc result :model (admit-bound before (:model result) (:source command)))
         (catch clojure.lang.ExceptionInfo error
           (if (= :agent-budget-loosening (:code (ex-data error)))
             (bound-refusal before command error)
             (throw error))))
    result))

(defn admit
  "Commit-time check for a lighting budget write. `before` is the model the
  write applies to and `after` the model with the new settings. An agent write
  that loosens the budget throws (code :agent-budget-loosening); an admitted
  agent write records its session's ceiling. Any other write clears the record."
  ([before after source] (admit before after source policy))
  ([before after source policy]
   (if-not (agent-source? source)
     (cond-> after (contains? (:extensions after) ceiling-key) (update :extensions dissoc ceiling-key))
     (let [start (ceiling before source)
           tighten? (= :tighten-only policy)
           proposed (cond->> (get-in after [:settings :lighting-budget]) tighten? (compose start))
           problems (when tighten? (loosened start proposed))]
       (when (seq problems) (refuse! problems policy))
       (allocation/settings proposed)
       (-> after
           (assoc-in [:settings :lighting-budget] proposed)
           (assoc-in [:extensions ceiling-key] {:applied proposed :operator start :session (session source)}))))))

(def ^:private scene-groups [:extensions :tolglow.lighting-scenes/control :groups])

(defn- members
  "The fixtures a group budget rule covers, as the allocator resolves them:
  a controlled scene group's fixtures, otherwise the patch group."
  [model group-id]
  (if-let [ids (get-in model (conj scene-groups group-id :fixture-ids))]
    (set ids)
    (into #{} (comp (filter #(some #{group-id} (:groups %))) (map :id)) (get-in model [:rig :fixtures]))))

(defn admit-scene
  "Commit-time check for a lighting scene write. A scene group with a budget
  rule's ID decides which fixtures that rule covers, and its cap scales with
  their watts, so an agent may not change which fixtures they are: fewer
  escape the rule, more (even dark ones) raise the cap for the lit ones
  (code :agent-budget-loosening). Rules kept while budgets are off count too,
  so a change cannot wait for the operator to enable them."
  ([before after source] (admit-scene before after source policy))
  ([before after source policy]
   (when (and (agent-source? source) (= :tighten-only policy))
     (let [rules (keys (get-in before [:settings :lighting-budget :groups]))
           problems (into [] (keep (fn [group-id]
                                     (let [was (members before group-id) now (members after group-id)]
                                       (when-not (= was now)
                                         {:path [:groups group-id :members]
                                          :ceiling (vec (sort-by str was)) :proposed (vec (sort-by str now))}))))
                          rules)]
       (when (seq problems) (refuse! problems policy))))
   ;; A scene's launches and releases may also move a bound limit (#901).
   (admit-bound before after source policy)))
