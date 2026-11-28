(ns tolglow.rig.retarget
  "Retarget recorded agent decisions onto a reference rig (#858).

  A decision addresses cues, categories and groups, so it means the same on
  another rig once the cues it names do. Retargeting lifts the source show's
  cues to fixture roles (tolglow.rig.roles): a cue whose fixtures are exactly
  one role on the source rig becomes a cue on that role, with no fixture ID in
  it, and resolves through the ordinary target narrowing on the target rig.
  The decisions' tool calls are then rewritten to name only cues that were
  carried. The policy is report, do not substitute: a cue on a fixture set no
  role matches, a world-space aim or volume, a role the target lacks, or a
  call that names fixtures or instances is listed with its reason, never
  guessed. Signals, capability substitution and in-role fixture assignment
  belong to cross-rig translation (#861, #883) and are not attempted here.

    (require '[tolglow.rig.retarget :as retarget]
             '[tolglow.rehearsal.passage :as passage]
             '[tolglow.rehearsal.agent-script :as script])
    (def p (passage/club 670))
    (def decisions (mapv #(script/suggestion p %) (range (count (:sections p)))))
    (def record (retarget/retarget (passage/model (:bpm p)) :movers-pars decisions))
    (:report record)                ; every refused or unmappable item, with reasons
    (retarget/target-model (retarget/prepare (passage/model (:bpm p)) :movers-pars))

  The record holds no fixture ID, address or position of either rig. Its
  :digest is afterglow.runtime.replay/digest of everything else, so the same
  inputs give the same digest. tolglow.rehearsal.retarget replays a
  retargeted rehearsal take through the real show owner."
  (:require [afterglow.runtime.replay :as replay]
            [clojure.string :as str]
            [tolglow.compiler :as compiler]
            [tolglow.registry :as registry]
            [tolglow.rig.reference :as reference]
            [tolglow.rig.roles :as roles]
            [tolglow.spatial.targets :as targets]))

(def schema "tolglow-retarget/1")

(defn- label [x] (cond (keyword? x) (subs (str x) 1) (string? x) x :else (str x)))

(defn- same-id? [a b] (= (label a) (label b)))

(defn- source-identifiers
  "Labels of everything that names a physical thing on rig: fixture, head and
  pixel layout IDs."
  [rig]
  (into #{} (comp (mapcat (juxt :id :fixture-id :pixel-layout-id)) (remove nil?) (map label))
        (:fixtures rig)))

(defn- identifiers-in
  "Labels of the identifiers in x that are in ids."
  [ids x]
  (into (sorted-set)
        (comp (filter #(or (keyword? %) (string? %))) (map label) (filter ids))
        (tree-seq coll? seq x)))

(defn- role-of
  "The role whose members on the source are exactly heads, preferring a role
  without an ordinal, or nil."
  [role-groups heads]
  (let [wanted (set heads)]
    (->> role-groups
         (filter (fn [[_ members]] (= wanted (set members))))
         (map key)
         (sort-by (juxt #(boolean (:ordinal (roles/parse %))) label))
         first)))

(defn- overlapping [role-groups heads]
  (let [heads (set heads)]
    (vec (sort-by label (for [[r members] role-groups :when (some heads members)] r)))))

(defn- world-space? [effect]
  (or (some #(= :aim (:attribute %)) (registry/output-entries (:outputs effect)))
      (some #(and (map? %) (#{:position :surface-uv} (:op %))) (tree-seq coll? seq (:outputs effect)))))

(defn- lift-cue
  "{:cue :effect :role} for cue lifted to roles, or {:refused reason-map}."
  [source role-groups target-groups ids cue]
  (let [registry (:registry source)
        effect (registry/cue-effect registry cue)
        selector (:selector cue)
        subject {:kind :cue :id (:id cue)}
        refuse (fn [reason & {:as more}] {:refused (merge subject {:reason reason} more)})]
    (cond
      (nil? effect) (refuse :missing-effect)
      (world-space? effect) (refuse :world-space :detail "aims at a point or samples positions in source metres")
      (:volume-id selector) (refuse :world-space :detail "selects a volume in source space")
      (some #(contains? % :targets) (registry/output-entries (:outputs effect))) (refuse :named-fixtures)
      :else
      (let [heads (cond (= :all selector) :all
                        (map? selector) (try (targets/resolve-ids source selector) (catch clojure.lang.ExceptionInfo _ ::unknown))
                        (some? (:targets effect)) (vec (:targets effect))
                        :else :untargeted)
            role (when (sequential? heads) (role-of role-groups heads))
            lifted-effect (dissoc effect :targets)
            lifted-cue (cond-> (dissoc cue :selector :effect-version)
                         role (assoc :selector (roles/selector role))
                         (= :all heads) (assoc :selector :all))
            leftover (identifiers-in ids [lifted-cue lifted-effect])]
        (cond
          (= ::unknown heads) (refuse :unknown-selector)
          (and (sequential? heads) (nil? role))
          (refuse :no-matching-role :heads (count heads) :overlaps (overlapping role-groups heads))
          (seq leftover) (refuse :fixture-reference :count (count leftover))
          (and role (not (contains? target-groups (keyword (name role))))) (refuse :role-absent :role role)
          :else {:cue lifted-cue :effect lifted-effect :role role})))))

(defn- lift-definitions [source role-groups target-groups ids]
  (reduce (fn [acc cue]
            (let [{:keys [refused cue effect role]} (lift-cue source role-groups target-groups ids cue)
                  prior (get-in acc [:effects (:id effect)])]
              (cond
                refused (update acc :report conj refused)
                (and prior (not= prior effect))
                (update acc :report conj {:kind :cue :id (:id cue) :reason :effect-revision-conflict})
                :else (-> acc (assoc-in [:effects (:id effect)] effect) (assoc-in [:cues (:id cue)] cue)
                          (cond-> role (update :roles conj role))))))
          {:effects (sorted-map-by #(compare (label %1) (label %2)))
           :cues (sorted-map-by #(compare (label %1) (label %2)))
           :roles (sorted-set-by #(compare (label %1) (label %2)))
           :report []}
          (sort-by (comp label :id) (vals (get-in source [:registry :cues])))))

(defn- target-model-of [target]
  (if (keyword? target) (reference/model target) target))

(defn prepare
  "Lift source-model's cues onto target: a reference rig ID or a show model.
  Returns the context retarget-decision and target-model use."
  [source-model target]
  (let [target-model (target-model-of target)
        source-rig (:rig source-model)
        role-map (roles/role-map source-rig)
        target-groups (roles/groups (:rig target-model))
        ids (source-identifiers source-rig)
        lifted (lift-definitions source-model (:groups role-map) target-groups ids)
        checked (reduce (fn [acc [id cue]]
                          (let [effect (get-in lifted [:effects (:effect-id cue)])
                                errors (concat (compiler/definition-errors :effects effect {:publication? true})
                                               (compiler/definition-errors :cues cue {:publication? true}))]
                            (if (seq errors)
                              (-> acc (update :cues dissoc id)
                                  (update :report conj {:kind :cue :id id :reason :invalid-on-target
                                                        :messages (mapv :message errors)}))
                              acc)))
                        lifted (:cues lifted))
        used-effects (set (map :effect-id (vals (:cues checked))))]
    {:source {:roles (:source role-map) :class (:class (reference/classify source-rig))}
     :target (cond-> {:class (:class (reference/classify (:rig target-model)))}
               (keyword? target) (assoc :reference target))
     :target-model target-model
     :identifiers (into ids (source-identifiers (:rig target-model)))
     :roles {:used (vec (:roles checked))
             :absent (vec (sort-by label (distinct (keep #(when (= :role-absent (:reason %)) (:role %)) (:report checked)))))}
     :definitions {:effects (vec (keep (fn [[id e]] (when (used-effects id) e)) (:effects checked)))
                   :cues (vec (vals (:cues checked)))}
     :carried (into #{} (map (comp label :id)) (vals (:cues checked)))
     :report (:report checked)}))

(defn target-model
  "The target show model with the lifted effects and cues published."
  [{:keys [target-model definitions]}]
  (as-> target-model m
    (reduce #(update %1 :registry registry/publish :effects %2) m (:effects definitions))
    (reduce #(update %1 :registry registry/publish :cues %2) m (:cues definitions))))

;; Decisions -------------------------------------------------------------------

(defn- carried? [ctx cue-id] (contains? (:carried ctx) (label cue-id)))

(defn- filter-scope
  "[scope dropped-cue-ids] for an auto scope, or [::refused reason]."
  [ctx scope]
  (cond
    (or (nil? scope) (#{:all "all"} scope)) [scope []]
    (some? (or (:page-id scope) (get scope "page-id"))) [::refused :page-scope]
    :else (let [k (if (contains? scope :cue-ids) :cue-ids "cue-ids")
                ids (get scope k)
                kept (filterv #(carried? ctx %) ids)
                dropped (filterv #(not (carried? ctx %)) ids)]
            (if (empty? kept) [::refused :no-carried-cue] [(assoc scope k kept) dropped]))))

(defn- configure-auto [ctx where {:keys [configuration] :as arguments}]
  (let [[scope dropped] (filter-scope ctx (:scope configuration))]
    (if (= ::refused scope)
      [nil [(assoc where :reason dropped)]]
      (let [sections (:sections configuration)
            results (map-indexed
                     (fn [i section]
                       (if-let [s (get-in section [:config :scope])]
                         (let [[s d] (filter-scope ctx s)]
                           (if (= ::refused s)
                             [nil [(assoc where :section (:id section i) :reason d)]]
                             [(assoc-in section [:config :scope] s)
                              (when (seq d) [(assoc where :section (:id section i) :reason :cue-not-carried :cues d)])]))
                         [section nil]))
                     sections)
            kept (into [] (keep first) results)
            reports (cond-> (vec (mapcat second results))
                      (seq dropped) (conj (assoc where :reason :cue-not-carried :cues dropped))
                      (and (seq sections) (empty? kept)) (conj (assoc where :reason :no-section-carried)))
            configuration (cond-> (if (some? (:scope configuration)) (assoc configuration :scope scope) configuration)
                            (seq sections) (#(if (seq kept) (assoc % :sections kept) (dissoc % :sections))))]
        [(assoc arguments :configuration configuration) reports]))))

(defn- launch-event
  "[event reports] for one scheduled event or launch_cue arguments; event is
  nil when it is refused."
  [ctx where source event]
  (let [where (cond-> where (:id event) (assoc :event (:id event)))
        cue-id (:cue-id event)
        selector (get-in event [:launch :selector])]
    (cond
      (and cue-id (not (carried? ctx cue-id))) [nil [(assoc where :reason :cue-not-carried :cues [cue-id])]]
      (some? (get-in event [:launch :targets])) [nil [(assoc where :reason :named-fixtures)]]
      (map? selector)
      (let [heads (try (targets/resolve-ids source selector) (catch clojure.lang.ExceptionInfo _ nil))
            role (when (seq heads) (role-of (:groups (roles/role-map (:rig source))) heads))]
        (if (and role (contains? (roles/groups (get-in ctx [:target-model :rig])) (keyword (name role))))
          [(assoc-in event [:launch :selector] (roles/selector role)) []]
          [nil [(assoc where :reason (if role :role-absent :no-matching-role))]]))
      :else [event []])))

(def ^:private read-only-tools #{"inspect_show" "inspect_schedule" "inspect_lighting" "cancel_scheduled"})

(defn- retarget-call [ctx source decision-id {:keys [id name arguments] :as call}]
  (let [where {:kind :call :decision decision-id :call (or id name) :tool name}
        [arguments reports]
        (case name
          "configure_auto" (configure-auto ctx where arguments)
          "schedule_events" (let [results (mapv #(launch-event ctx where source %) (:events arguments))
                                  kept (into [] (keep first) results)
                                  reports (vec (mapcat second results))]
                              (if (seq kept)
                                [(assoc arguments :events kept) reports]
                                [nil (conj reports (assoc where :reason :no-event-carried))]))
          "launch_cue" (launch-event ctx where source arguments)
          ("stop_instance" "set_parameter") [nil [(assoc where :reason :instance-reference)]]
          (if (read-only-tools name)
            (if (and (= "inspect_lighting" name) (some? (:group-id arguments)))
              [(dissoc arguments :group-id) [(assoc where :reason :group-filter-dropped)]]
              [arguments []])
            [nil [(assoc where :reason :unsupported-tool)]]))
        leftover (when arguments (identifiers-in (:identifiers ctx) arguments))]
    (if (seq leftover)
      [nil (conj reports (assoc where :reason :fixture-reference :count (count leftover)))]
      [(when arguments (assoc call :arguments arguments)) reports])))

(defn retarget-decision
  "{:decision {:id :calls} :report [...]} for decision ({:id or :run-id,
  :calls [{:id :name :arguments}]}) retargeted with ctx from prepare."
  [ctx source decision]
  (let [decision-id (or (:id decision) (:run-id decision))
        results (mapv #(retarget-call ctx source decision-id %) (:calls decision))]
    {:decision {:id decision-id :calls (into [] (keep first) results)}
     :report (vec (mapcat second results))}))

(defn retarget-suggestion
  "A recorded agent suggestion (tolglow.rehearsal.agent-script/suggestion)
  with its calls retargeted, for replay through the same agent path."
  [ctx source suggestion]
  (assoc suggestion :calls (get-in (retarget-decision ctx source suggestion) [:decision :calls])))

(defn- strip-ids [ctx record]
  (let [leaked (identifiers-in (:identifiers ctx) record)]
    (when (seq leaked)
      (throw (ex-info "A retargeted record would carry rig identifiers" {:count (count leaked)})))
    record))

(defn retarget
  "The retargeted record for decisions made on source-model, onto target (a
  reference rig ID or a show model)."
  [source-model target decisions]
  (let [ctx (prepare source-model target)
        results (mapv #(retarget-decision ctx source-model %) decisions)
        record {:schema schema
                :vocabulary (:version roles/vocabulary)
                :signature-version reference/signature-version
                :source (:source ctx)
                :target (:target ctx)
                :roles (:roles ctx)
                :definitions (:definitions ctx)
                :decisions (mapv :decision results)
                :report (into (vec (:report ctx)) (mapcat :report) results)}]
    (strip-ids ctx (assoc record :digest (replay/digest record)))))

(defn summary
  "One line per report entry, for the REPL and a review."
  [record]
  (str/join "\n" (for [{:keys [kind id decision call section reason cues role]} (:report record)]
                   (str (clojure.core/name kind) " " (label (or id call)) (when decision (str " in " decision))
                        (when section (str " section " (label section)))
                        ": " (clojure.core/name reason)
                        (when role (str " " (label role)))
                        (when (seq cues) (str " (" (str/join ", " (map label cues)) ")"))))))
