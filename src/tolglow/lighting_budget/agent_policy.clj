(ns tolglow.lighting-budget.agent-policy
  "Whether an agent may loosen the lighting budget. By default it may only
  tighten it: lower a limit, an active-source count or a standby level, add a
  rule or enable budgets. It may relax its own tightening again, but never past
  the operator's setting. The operator, Studio and the REPL stay unrestricted.

  Change the policy in one place: `policy` below. See
  docs/group-lighting-scenes.md#what-chat-may-change."
  (:require [tolglow.autopilot.signal :as signal]
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
  "Model extension that remembers the operator's setting while agents have
  tightened it: {:applied <settings an agent committed> :operator <settings
  before that>}. It describes the current settings only while they still equal
  :applied. Any other writer replaces the settings, and the record then falls
  away, so the operator's latest setting is the ceiling again."
  ::ceiling)

(defn agent-source? [source]
  (and (map? source) (contains? #{:agent "agent"} (:adapter source))))

(defn ceiling
  "The lighting budget settings an agent may not loosen past on this model."
  [model]
  (let [current (get-in model [:settings :lighting-budget])
        {:keys [applied operator]} (get-in model [:extensions ceiling-key])]
    (if (and (some? applied) (= applied current)) operator current)))

(def ^:private unbounded (allocation/settings {}))

(defn- normalized [settings] (if (map? settings) (allocation/settings settings) unbounded))

(defn- rule
  "The effective rule at path: scope defaults where budgets are off or no rule is set."
  [settings path]
  (merge allocation/scope-defaults (when (:enabled? settings) (get-in settings path))))

(defn- limit-value [model limit]
  (if (map? limit) (-> (signal/value model limit) double (min 1.0) (max 0.0)) (double limit)))

(defn- standby-look [{:keys [standby-mode standby-level]}]
  (if (= :all standby-mode) (double standby-level) 0.0))

(defn- rule-loosened [model path ceiling proposed]
  (let [entry (fn [field c p] {:path (conj path field) :ceiling c :proposed p})
        {c-limit :limit c-active :max-active c-standby :standby-level} ceiling
        {p-limit :limit p-active :max-active p-standby :standby-level} proposed]
    (cond-> []
      ;; A bound limit cannot be shown to stay below another; only the same binding is no looser.
      (if (map? p-limit)
        (not= p-limit c-limit)
        (> (double p-limit) (+ (limit-value model c-limit) 1e-9)))
      (conj (entry :limit c-limit p-limit))

      (and (some? p-active) (some? c-active) (> p-active c-active)) (conj (entry :max-active c-active p-active))
      (and (nil? p-active) (some? c-active)) (conj (entry :max-active c-active nil))

      (> (double p-standby) (double c-standby)) (conj (entry :standby-level c-standby p-standby))

      (> (standby-look proposed) (standby-look ceiling))
      (conj (entry :standby-mode (:standby-mode ceiling) (:standby-mode proposed))))))

(defn loosened
  "What `proposed` loosens relative to `ceiling`, as [{:path :ceiling :proposed}].
  Empty when proposed lets no more light through anywhere. Strategy and fixture
  order only redistribute light and are free."
  [model ceiling proposed]
  (let [ceiling (normalized ceiling)
        proposed (normalized proposed)]
    (if (and (:enabled? ceiling) (not (:enabled? proposed)))
      [{:path [:enabled?] :ceiling true :proposed false}]
      (let [paths (into [[:global]] (map #(vector :groups %))
                        (distinct (concat (keys (:groups ceiling)) (keys (:groups proposed)))))]
        (cond-> (into [] (mapcat #(rule-loosened model % (rule ceiling %) (rule proposed %))) paths)
          (< (double (:unrated-watts proposed)) (double (:unrated-watts ceiling)))
          (conj {:path [:unrated-watts] :ceiling (:unrated-watts ceiling) :proposed (:unrated-watts proposed)}))))))

(defn admit
  "Commit-time check for a lighting budget write. `before` is the model the
  write applies to and `after` the model with the new settings. An agent write
  that loosens the budget throws (code :agent-budget-loosening); an admitted
  agent write records the operator's ceiling. Any other write clears the record."
  ([before after source] (admit before after source policy))
  ([before after source policy]
   (if-not (agent-source? source)
     (cond-> after (contains? (:extensions after) ceiling-key) (update :extensions dissoc ceiling-key))
     (let [operator (ceiling before)
           proposed (get-in after [:settings :lighting-budget])
           problems (when (= :tighten-only policy) (loosened before operator proposed))]
       (when (seq problems)
         (throw (ex-info (str "An agent may tighten the lighting budget but not loosen it past the operator's setting: "
                              (pr-str (mapv :path problems)))
                         {:code :agent-budget-loosening :policy policy :loosened problems})))
       (assoc-in after [:extensions ceiling-key] {:applied proposed :operator operator})))))
