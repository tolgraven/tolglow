(ns tolglow.lighting-budget.agent-policy
  "Whether an agent may loosen the lighting budget. By default it may only
  tighten it: lower a limit, an active-source count or a standby level, add a
  rule or enable budgets. An agent may relax its own tightening again, but never
  past the setting it started from, and never another agent's tightening. The
  operator, Studio and the REPL stay unrestricted.

  Change the policy in one place: `policy` below. See
  docs/group-lighting-scenes.md#what-chat-may-change."
  (:require [clojure.set :as set]
            [tolglow.lighting-budget.allocation :as allocation]))

(def policy
  "How the show admits a lighting budget change whose source is an agent.

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
  min(a, b) is no looser when either argument is. A binding is no looser only
  than itself: its current value says nothing about where it can go."
  [c p]
  (cond (= p c) true
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
  only ever lowers it."
  [ceiling-limit fixed]
  {:op :min :args [ceiling-limit fixed] :default (min (double fixed) (double (:default ceiling-limit 0.0)))})

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
  rule's ID decides which fixtures that rule covers, so an agent may not drop
  fixtures from it (code :agent-budget-loosening)."
  ([before after source] (admit-scene before after source policy))
  ([before after source policy]
   (when (and (agent-source? source) (= :tighten-only policy))
     (let [settings (normalized (get-in before [:settings :lighting-budget]))
           problems (when (:enabled? settings)
                      (into [] (keep (fn [group-id]
                                       (let [was (members before group-id) now (members after group-id)]
                                         (when-not (set/subset? was now)
                                           {:path [:groups group-id :members]
                                            :ceiling (vec (sort-by str was)) :proposed (vec (sort-by str now))}))))
                            (keys (:groups settings))))]
       (when (seq problems) (refuse! problems policy))))
   after))
