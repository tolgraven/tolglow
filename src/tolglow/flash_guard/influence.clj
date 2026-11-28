(ns tolglow.flash-guard.influence
  "Which running light an agent influenced, for the flash guard (#836).

  Light counts as agent-attributed when an agent launched it, and also when
  an agent last wrote one of its parameters or its opacity, published the
  effect revision or saved the cue it runs, or configured the policy that
  launched it (auto, the energy autopilot, lighting scenes, an energy-scene
  subscriber). The owner records that influence here from each committed
  command's source, so every route an agent can reach the show by (tool
  calls, scheduled events, ramps, future routes) is covered at the owner
  rather than in one tool path.

  A claim ends only when an operator replaces what the agent wrote: a
  different parameter value, a cue with different effect or parameters, an
  effect revision with different outputs or parameters, a different auto
  scope, scene or autopilot setting. A rename or a re-save of the same
  content carries the claim to the new version. An operator can also adopt
  agent light explicitly (`adopt`).

  An agent write to a show variable cannot yet be traced to the cues that
  read it: while one is live the guard covers the whole show. It lapses when
  that agent run ends, or when an operator writes a different value.

  A restore carries claims to the restored instances under their new IDs.

  Pure functions over the model. This namespace must not require the
  scheduler, persistence or the show, which call it.")

(def path [:extensions :tolglow.flash-guard/influence])

(def default-sources [:agent])

(defn sources
  "Source adapters the guard treats as agents in this model."
  [model]
  (set (get-in model [:settings :flash-guard :sources] default-sources)))

(defn summary
  "The influence as the guard reads it."
  [model]
  (get-in model path))

(defn agent-source?
  "An agent adapter, or a subscriber an agent saved acting on its behalf."
  [model source]
  (and (map? source)
       (or (contains? (sources model) (:adapter source))
           (and (= :event-subscriber (:adapter source))
                (contains? (:subscribers (summary model)) (str (:session-id source)))))))

(defn- edit [model f]
  (let [old (get-in model path)
        new (into {} (remove (comp empty? val)) (f (or old {})))]
    (cond
      (= old new) model
      (empty? new) (if old (update model :extensions dissoc :tolglow.flash-guard/influence) model)
      :else (assoc-in model path new))))

(defn- claim
  "An agent writes `fingerprint` at `k`/`id`; an operator write keeps the
  claim only when it writes the same fingerprint."
  [influence agent? k id fingerprint]
  (cond
    agent? (assoc-in influence [k id] fingerprint)
    (= fingerprint (get-in influence [k id])) influence
    :else (update influence k dissoc id)))

(defn cue-fingerprint [cue] (hash (select-keys cue [:effect-id :params :saved-params])))

(defn effect-fingerprint [definition] (hash (select-keys definition [:outputs :params :modulation])))

(defn scene-fingerprint [request]
  (hash (select-keys request [:op :group-id :target :fixture-ids :effect-ids :params :target-var :allow-generated?])))

(defn subscriber-fingerprint [subscriber] (hash (select-keys subscriber [:match :actions])))

(def ^:private opacity-ops #{:instance/opacity-set :instance/opacity-toggle})

(defn- publish [influence agent? {:keys [id version] :as definition}]
  (let [fingerprint (effect-fingerprint definition)
        claimed (get-in influence [:effects id])]
    (cond
      agent? (assoc-in influence [:effects id] {:versions (conj (:versions claimed #{}) version) :fingerprint fingerprint})
      ;; An operator revision with the agent's outputs and parameters (a rename,
      ;; a re-save) carries the claim to its version.
      (and claimed (= fingerprint (:fingerprint claimed))) (update-in influence [:effects id :versions] conj version)
      :else influence)))

(defn prune
  "Drop claims nothing references any more: ended instances, deleted cues,
  and effect revisions neither current nor running."
  [influence model]
  (let [instances (:instances model)
        running (into #{} (map (juxt :effect-id :effect-version)) (vals instances))
        cues (get-in model [:registry :cues])
        effects (get-in model [:registry :effects])]
    (cond-> influence
      (seq (:params influence)) (update :params #(into {} (filter (fn [[[id] _]] (contains? instances id))) %))
      (seq (:instances influence)) (update :instances #(into #{} (filter (partial contains? instances)) %))
      (seq (:adopted influence)) (update :adopted #(into #{} (filter (partial contains? instances)) %))
      (seq (:cues influence)) (update :cues #(into {} (filter (fn [[id]] (contains? cues id))) %))
      (seq (:effects influence))
      (update :effects (fn [claims]
                         (into {} (keep (fn [[id {:keys [versions] :as entry}]]
                                          (let [kept (into #{} (filter #(or (= % (get-in effects [id :version]))
                                                                            (contains? running [id %])))
                                                           versions)]
                                            (when (seq kept) [id (assoc entry :versions kept)]))))
                               claims))))))

(defn note
  "Record what one accepted command did. `agent?` when its source is an
  agent, `human?` when it is an operator surface. An operator write ends the
  agent's claim only where it replaces what the agent wrote; other sources
  leave claims as they are."
  [model {:keys [op source] :as command} events agent? human?]
  (if-not (or agent? human?)
    model
    (edit model
          (fn [influence]
            (let [system-op (if (= :group/apply op) (get-in command [:action :op]) op)
                  run (select-keys source [:run-id :session-id])
                  influence (reduce (fn [inf {:keys [type] :as event}]
                                      (case type
                                        :param/changed (claim inf agent? :params [(:instance-id event) (:param event)] (:value event))
                                        :variable/changed
                                        (cond
                                          agent? (assoc-in inf [:variables (:variable event)] (assoc run :value (:value event)))
                                          (= (:value event) (get-in inf [:variables (:variable event) :value])) inf
                                          :else (update inf :variables dissoc (:variable event)))
                                        :instance/system-changed
                                        (if (opacity-ops system-op)
                                          (claim inf agent? :params [(:instance-id event) :opacity] (:value command))
                                          inf)
                                        inf))
                                    influence events)
                  influence (case op
                              :lighting/scene (claim influence agent? :policies :lighting-scene (scene-fingerprint (:scene command)))
                              :auto/configure (if (or agent? (contains? (:request command) :scope))
                                                (claim influence agent? :policies :auto (hash (get-in command [:request :scope])))
                                                influence)
                              :cue/put (if-let [id (get-in command [:cue :id])]
                                         (claim influence agent? :cues id (cue-fingerprint (:cue command)))
                                         influence)
                              :registry/publish (if (and (= :effects (:kind command)) (get-in command [:definition :id]))
                                                  (publish influence agent? (:definition command))
                                                  influence)
                              influence)]
              (prune influence model))))))

(defn note-policy
  "An agent configured, or an operator reconfigured, the policy whose
  launches carry `adapter`. `fingerprint` names what was configured; an
  operator configuration with the same fingerprint, or nil (one that does
  not touch it, such as stopping auto), keeps the claim."
  [model adapter fingerprint agent? human?]
  (cond
    agent? (edit model #(assoc-in % [:policies adapter] fingerprint))
    (and human? (some? fingerprint)) (edit model #(claim % false :policies adapter fingerprint))
    :else model))

(defn note-subscriber
  "An agent saved, or an operator saved or removed, an event subscriber.
  Runs of an agent's subscriber act with agent influence."
  [model {:keys [op subscriber id]} agent? human?]
  (let [key (str (if (= :save op) (:id subscriber) id))]
    (cond
      (and agent? (= :save op)) (edit model #(assoc-in % [:subscribers key] (subscriber-fingerprint subscriber)))
      (and human? (= :save op)) (edit model #(claim % false :subscribers key (subscriber-fingerprint subscriber)))
      (and (or agent? human?) (= :remove op)) (edit model #(update % :subscribers dissoc key))
      :else model)))

(defn adopt
  "An operator takes over agent light. `selection` names :instance-ids,
  :cue-ids, :effect-ids, :policies (adapters), :subscribers or :variables;
  :all? adopts everything. Adopted instances stop counting as agent light
  even when an agent launched them."
  [model {:keys [instance-ids cue-ids effect-ids policies subscribers variables all?]}]
  (edit model
        (fn [influence]
          (if all?
            (let [instances (:instances model)]
              (cond-> {}
                (seq instances) (assoc :adopted (set (keys instances)))))
            (-> influence
                (update :adopted (fnil into #{}) instance-ids)
                (update :instances #(reduce disj (or % #{}) instance-ids))
                (update :params #(into {} (remove (fn [[[id] _]] (some #{id} instance-ids))) %))
                (update :cues #(apply dissoc % cue-ids))
                (update :effects #(apply dissoc % effect-ids))
                (update :policies #(apply dissoc % policies))
                (update :subscribers #(apply dissoc % (map str subscribers)))
                (update :variables #(apply dissoc % variables)))))))

(defn run-ended
  "An agent run ended: its show-wide variable claims lapse."
  [model run-id]
  (if (some #(= run-id (:run-id %)) (vals (:variables (summary model))))
    (edit model (fn [inf] (update inf :variables #(into {} (remove (fn [[_ v]] (= run-id (:run-id v)))) %))))
    model))

(defn attributed?
  "Whether this instance's light counts as agent-attributed."
  [sources influence instance]
  (let [adapter (get-in instance [:source :adapter])
        id (:id instance)]
    (boolean
     (and (not (contains? (:adopted influence) id))
          (or (contains? sources adapter)
              (and (= :event-subscriber adapter)
                   (contains? (:subscribers influence) (str (get-in instance [:source :session-id]))))
              (contains? (:policies influence) adapter)
              (contains? (:instances influence) id)
              (contains? (get-in influence [:effects (:effect-id instance) :versions]) (:effect-version instance))
              (contains? (:cues influence) (:cue-id instance))
              (some (fn [[[i]]] (= i id)) (:params influence)))))))

(defn show-wide?
  "Agent influence the guard cannot yet trace to instances: it then covers
  the whole show."
  [influence]
  (boolean (seq (:variables influence))))

(defn show-wide-variables [influence] (vec (sort-by str (keys (:variables influence)))))

(defn loaded
  "The ledger of a model loaded without a restore (same IDs): variable claims
  belonged to agent runs that no longer exist."
  [model]
  (if (seq (:variables (summary model))) (edit model #(dissoc % :variables)) model))

(defn restored
  "Carry claims across a restore. `saved` is the saved model, `ids` maps its
  instance IDs to the restored ones, `model` is the restored model. Every
  instance that was agent light is claimed under its new ID, so a restore
  that re-sources it to :restore keeps it guarded; variable claims lapse
  with the runs that made them."
  [model saved ids]
  (let [old (summary saved)
        srcs (sources saved)
        claimed (into #{} (keep (fn [instance]
                                  (when (attributed? srcs old instance) (ids (:id instance)))))
                      (vals (:instances saved)))
        adopted (into #{} (keep ids) (:adopted old))
        ledger (cond-> (dissoc (or old {}) :variables :params :instances :adopted)
                 (seq claimed) (assoc :instances claimed)
                 (seq adopted) (assoc :adopted adopted))
        ledger (into {} (remove (comp empty? val)) ledger)]
    (if (empty? ledger)
      (cond-> model (get-in model path) (update :extensions dissoc :tolglow.flash-guard/influence))
      (assoc-in model path ledger))))
