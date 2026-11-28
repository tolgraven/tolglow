(ns tolglow.template-groups
  "Template groups (#812): a saved, reusable group of cue members with open
  roles that a launch binds to fixtures, and member cards with sizes and an
  order. One template can run as several placements at once, each with its
  own bindings. See docs/template-groups.md; the operator has not yet agreed
  the design.

  Definitions are portable data under `[:extensions state-key]`. A placement
  runs as an ordinary composition run whose captured definition is a macro
  derived from the template, so ownership, stop, restore and launch faults
  are the macros' own (`tolglow.composition`). Members and cards are
  validated by `composition/save` and card edits go through
  `composition/set-cards`, so macros and template groups share one path.
  Nothing here reads a clock or opens a device."
  (:require [clojure.string :as str]
            [tolglow.authoring.card-layout :as card-layout]
            [tolglow.composition :as composition]
            [tolglow.registry :as registry]
            [tolglow.spatial.targets :as targets]))

(def state-key
  "Saved under the show's :extensions, so full backups and show saves keep it."
  ::groups)

(def default-placement :main)

(def definition-keys
  "Fields a saved template group keeps. Anything else in a save, such as the
  :placements and :version a snapshot adds for views, is dropped; the owner
  sets :version."
  [:id :name :opacity :roles :members :cards])

(def terms
  "How the shared macro validators name a template group and its members, so
  their errors use :members paths and template words."
  {:noun "template group" :item "member" :items :members})

(def ^:private save-opts {:terms terms :placement? true})

(def ^:private role-keys #{:id :name :selector})

(def ^:private template-keys #{:id :version :placement :bindings :resolved :empty-roles})

(def ^:private check-id
  "The scratch composition ID used to validate a template through
  `composition/save`; never stored."
  ::check)

(defn- fail! [path message]
  (throw (ex-info message {:errors [{:path path :message message}]})))

(defn- identifier? [x] (or (keyword? x) (and (string? x) (not (str/blank? x)))))

(def ^:private key-name
  "IDs compare by name, as slot IDs do: the JSON boundary turns textual IDs
  into keywords."
  card-layout/slot-key)

(defn- run-name?
  "An identifier that can be part of a run ID: no `/`, so
  \"template-group/<template>/<placement>\" stays unambiguous."
  [x]
  (and (identifier? x) (not (str/includes? (key-name x) "/"))))

(defn groups
  "Saved template groups, {id definition}."
  [model]
  (get-in model [:extensions state-key] {}))

(defn lookup
  "The saved template group named `id` (a keyword or its name), or nil."
  [model id]
  (when (identifier? id)
    (let [saved (groups model)]
      (or (get saved id) (some (fn [[k group]] (when (= (key-name k) (key-name id)) group)) saved)))))

(defn run-id
  "The composition run ID of a template group placement. Neither name may
  contain `/`, and composition IDs may not use `composition/reserved-prefix`,
  so no other run can share it."
  [template-id placement]
  (str composition/reserved-prefix (key-name template-id) "/" (key-name placement)))

(defn- template-run? [template-id [_ run]]
  (when-let [id (get-in run [:template :id])]
    (= (key-name id) (key-name template-id))))

(defn placements
  "Running placements of a template group: [[run-id run] ...] by run ID."
  [model template-id]
  (sort-by key (filter #(template-run? template-id %) (:composition-runs model))))

(defn- derived-macro
  "The macro a placement runs: members as slots in member order, each role's
  selector copied into its members' :launch :selector. `bound` maps role
  names (`key-name`) to selectors; a role it leaves out keeps its members on
  their cues' own targets."
  [group id bound]
  (cond-> {:id id :mode :macro
           :slots (mapv (fn [{:keys [role] :as member}]
                          (let [slot (dissoc member :role)]
                            (if-let [selector (and (some? role) (get bound (key-name role)))]
                              (assoc-in slot [:launch :selector] selector)
                              slot)))
                        (:members group))
           :cards (:cards group)}
    (:name group) (assoc :name (:name group))
    (contains? group :opacity) (assoc :opacity (:opacity group))))

(defn- role-defaults [group]
  (into {} (keep (fn [{:keys [id selector]}] (when (some? selector) [(key-name id) selector]))) (:roles group)))

(defn- used-roles
  "Names (`key-name`) of the roles some member uses."
  [group]
  (set (keep (comp #(some-> % key-name) :role) (:members group))))

(defn needs-binding
  "IDs of used roles without a default selector: a launch must bind them."
  [group]
  (let [used (used-roles group) defaults (role-defaults group)]
    (vec (keep (fn [{:keys [id]}] (when (and (used (key-name id)) (not (defaults (key-name id)))) id)) (:roles group)))))

(defn- check-shape!
  "Structure that `composition/save` does not know: the ID, roles, member IDs
  and member roles."
  [definition]
  (when-not (and (map? definition) (registry/data? definition) (identifier? (:id definition)))
    (fail! [:id] "A template group needs portable data and an ID"))
  (when-not (run-name? (:id definition))
    (fail! [:id] "A template group ID may not contain /"))
  (let [{:keys [roles members]} definition]
    (when-not (or (nil? roles) (and (vector? roles) (every? map? roles)))
      (fail! [:roles] "Roles are a vector of {:id :name :selector}"))
    (doseq [[i role] (map-indexed vector roles)]
      (when-not (identifier? (:id role)) (fail! [:roles i :id] "A role needs an ID"))
      (when-not (every? role-keys (keys role)) (fail! [:roles i] "A role names its :id, and optionally its :name and :selector"))
      (when (and (contains? role :selector) (not (targets/selector? (:selector role))))
        (fail! [:roles i :selector] "A role's selector is {:group-id}, {:fixture-ids}, {:volume-id} or :all")))
    (when-let [repeated (->> roles (map (comp key-name :id)) frequencies (keep (fn [[k n]] (when (< 1 n) k))) seq)]
      (fail! [:roles] (str "A role is named once: " (str/join ", " repeated))))
    (when-not (and (vector? members) (seq members) (every? map? members))
      (fail! [:members] "A template group needs a nonempty vector of members"))
    (let [role-names (set (map (comp key-name :id) roles))]
      (doseq [[i {:keys [id role launch] :as member}] (map-indexed vector members)]
        (when-not (identifier? id) (fail! [:members i :id] "A member needs a stable ID"))
        (when (contains? member :role)
          (when-not (and (identifier? role) (contains? role-names (key-name role)))
            (fail! [:members i :role] "A member's role names one of this template group's roles"))
          (when (and (map? launch) (some #(contains? launch %) [:selector :targets]))
            (fail! [:members i :launch] "A member with a role takes its fixtures from the role, not :selector or :targets")))))
    (when-let [repeated (->> members (map (comp key-name :id)) frequencies (keep (fn [[k n]] (when (< 1 n) k))) seq)]
      (fail! [:members] (str "A member is named once: " (str/join ", " repeated))))))

(defn- checked-macro
  "Validate `group`'s members, opacity and cards as `composition/save`
  validates a macro, against the show's current cues and in template terms.
  Returns the saved macro (its cards canonical)."
  [model group]
  (get-in (composition/save model (derived-macro group check-id (role-defaults group)) save-opts) [:compositions check-id]))

(defn- launch-shape [group] (select-keys group [:roles :members :opacity]))

(defn save
  "Validate and save a template group. The owner sets :version: 1 for a new
  group, one more when its roles, members or opacity change; a card edit or
  rename keeps it. `:expected-version` guards the save: nil creates only
  (refused when the ID is taken), a number replaces only that version, and
  leaving it out is last writer wins. A save that omits :cards keeps the cards
  whose member keeps its ID and cue, as a macro save does; `:cards nil` clears
  them. Running placements keep the definition they launched with."
  [model definition]
  (check-shape! definition)
  (let [saved (lookup model (:id definition))]
    (when (contains? definition :expected-version)
      (let [expected (:expected-version definition)]
        (cond
          (and (nil? expected) saved)
          (fail! [:id] "A template group with this ID exists; choose another ID or replace its version")
          (and (some? expected) (not (pos-int? expected)))
          (fail! [:expected-version] "An expected version is a positive integer, or nil to create only")
          (and (some? expected) (not= expected (:version saved)))
          (fail! [:expected-version] (str "The template group is at version " (:version saved "none")
                                          ", not " expected "; read it again before saving")))))
    (let [definition (-> (select-keys definition definition-keys)
                         (assoc :id (if saved (:id saved) (:id definition))))
          cards (if (contains? definition :cards)
                  (:cards definition)
                  (card-layout/carry (:members saved) (:cards saved) (:members definition)))
          checked (checked-macro model (assoc definition :cards cards))
          definition (cond-> (dissoc definition :cards)
                       (seq (:cards checked)) (assoc :cards (:cards checked))
                       (empty? (:roles definition)) (dissoc :roles))
          version (cond (nil? saved) 1
                        (= (launch-shape saved) (launch-shape definition)) (:version saved)
                        :else (inc (:version saved)))]
      (-> model
          (assoc-in [:extensions state-key (:id definition)] (assoc definition :version version))
          (update :revision inc)))))

(defn remove-group
  "Remove a saved template group that has no running placement."
  [model id]
  (let [group (or (lookup model id) (fail! [:id] "Choose a saved template group"))]
    (when (seq (placements model (:id group)))
      (fail! [:id] "Stop this template group's placements before removing it"))
    (-> model (update-in [:extensions state-key] dissoc (:id group)) (update :revision inc))))

(defn- bindings!
  "Each used role's selector, the launch's binding or the role's default,
  keyed by `key-name`, and the used roles whose volume is empty now. Only
  used roles are resolved. A selector that does not resolve on the current
  rig, or a group or fixture list that selects no fixture, refuses the
  launch before anything starts; an empty volume is allowed, because volumes
  fill as bodies move."
  [model group bindings]
  (when-not (or (nil? bindings) (map? bindings))
    (fail! [:bindings] "Bindings map each role to a selector"))
  (let [roles (:roles group)
        index (into {} (map-indexed (fn [i role] [(key-name (:id role)) i])) roles)
        used (used-roles group)]
    (doseq [[role selector] bindings]
      (when-not (and (identifier? role) (contains? index (key-name role)))
        (fail! [:bindings role] "A binding names one of this template group's roles"))
      (when-not (targets/selector? selector)
        (fail! [:bindings role] "A binding is {:group-id}, {:fixture-ids}, {:volume-id} or :all")))
    (let [given (into {} (map (fn [[role selector]] [(key-name role) selector])) bindings)
          bound (select-keys (merge (role-defaults group) given) used)
          role-id #(:id (nth roles (index %)))
          path #(if (contains? given %) [:bindings (role-id %)] [:roles (index %) :selector])]
      (doseq [role (sort used) :when (not (contains? bound role))]
        (fail! [:bindings (role-id role)] (str "Bind role " role ": it has no default selector")))
      {:bound bound
       :empty-roles
       (vec (for [[role selector] (sort-by key bound)
                  :let [ids (try (targets/resolve-ids model selector)
                                 (catch clojure.lang.ExceptionInfo error
                                   (fail! (path role) (str "Role " role " does not resolve on this rig: " (ex-message error)))))]
                  :when (empty? ids)]
              (if (and (map? selector) (contains? selector :volume-id))
                (role-id role)
                (fail! (path role) (str "Role " role " selects no fixtures on this rig")))))})))

(defn launch
  "Start or replace one placement of a saved template group: its members
  launch together as one macro run, each used role bound to the launch's
  `:bindings` selector or the role's default. `:placement` defaults to
  :main; launching the same placement again replaces only that one. The run
  captures the template's :version, the explicitly given :bindings (which
  `relaunch` reuses), the :resolved selector of every used role, and the
  roles bound to a volume that was empty (:empty-roles)."
  [model {:keys [id placement bindings]} time-ms]
  (let [group (or (lookup model id) (fail! [:id] "Choose a saved template group"))
        placement (if (some? placement) placement default-placement)
        _ (when-not (run-name? placement) (fail! [:placement] "A placement is a keyword or name without /"))
        {:keys [bound empty-roles]} (bindings! model group bindings)
        id (run-id (:id group) placement)
        held (get-in model [:composition-runs id])
        _ (when (and held (not (template-run? (:id group) [id held])))
            (fail! [:placement] "Another run holds this placement's ID"))
        definition (get-in (composition/save model (derived-macro group id bound) save-opts) [:compositions id])
        role-id (into {} (map (juxt (comp key-name :id) :id)) (:roles group))
        by-role (fn [selectors] (into {} (map (fn [[role selector]] [(role-id (key-name role)) selector])) selectors))]
    (-> (composition/start-definition model definition time-ms)
        (update-in [:composition-runs id] assoc
                   :template (cond-> {:id (:id group) :version (:version group) :placement placement
                                      :bindings (by-role bindings) :resolved (by-role bound)}
                               (seq empty-roles) (assoc :empty-roles empty-roles))))))

(defn relaunch
  "Launch a running placement again with the current template: the bindings
  it was explicitly given that still name one of the template's roles, and
  the template's current defaults for every other role. So a role the
  template dropped never refuses the relaunch, and a changed default applies.
  Studio's Relaunch on a Changed placement sends this."
  [model {:keys [id placement]} time-ms]
  (let [group (or (lookup model id) (fail! [:id] "Choose a saved template group"))
        placement (if (some? placement) placement default-placement)
        [_ run] (or (first (filter (fn [[_ run]] (= (key-name placement) (key-name (get-in run [:template :placement]))))
                                   (placements model (:id group))))
                    (fail! [:placement] "This placement is not running"))
        roles (set (map (comp key-name :id) (:roles group)))
        given (into {} (filter (fn [[role _]] (contains? roles (key-name role)))) (get-in run [:template :bindings]))]
    (launch model {:id (:id group) :placement (get-in run [:template :placement]) :bindings given} time-ms)))

(defn stop
  "Stop one placement of a template group, or all of them when `:placement`
  is nil. Each releases only its own instances. An unknown template, or a
  placement that is not running, is refused. An orphan placement (restored
  without its template) stops by its template ID."
  [model {:keys [id placement]} time-ms]
  (when-not (identifier? id) (fail! [:id] "Name a template group"))
  (when-not (or (nil? placement) (identifier? placement)) (fail! [:placement] "A placement is a keyword or name"))
  (let [running (placements model id)
        chosen (filter (fn [[_ run]] (or (nil? placement) (= (key-name placement) (key-name (get-in run [:template :placement])))))
                       running)]
    (when-not (or (lookup model id) (seq running))
      (fail! [:id] "Choose a saved template group or a running placement"))
    (when (and (some? placement) (empty? chosen))
      (fail! [:placement] "This placement is not running"))
    (reduce (fn [model [run-id _]] (composition/stop model run-id time-ms)) model chosen)))

(defn set-cards
  "Change a template group's card arrangement with one `composition/set-cards`
  form. The arrangement belongs to the template and is presentation only, so
  its :version stays and every running placement shows the change at once."
  [model id change]
  (let [group (or (lookup model id) (fail! [:id] "Choose a saved template group"))
        scratch (composition/save model (derived-macro group check-id (role-defaults group)) save-opts)
        cards (get-in (composition/set-cards scratch check-id change save-opts) [:compositions check-id :cards])]
    (-> model
        (assoc-in [:extensions state-key (:id group)] (cond-> (dissoc group :cards) (seq cards) (assoc :cards cards)))
        (update :revision inc))))

(defn- placement-view [group [run-id {:keys [owner-id template definition]}]]
  (cond-> {:placement (:placement template) :run-id run-id :layer-id owner-id
           :member-slots (mapv :id (:slots definition))
           :bindings (:bindings template) :resolved (:resolved template) :version (:version template)
           :stale? (and (some? group) (not= (:version template) (:version group)))}
    (seq (:empty-roles template)) (assoc :empty-roles (:empty-roles template))))

(defn projection
  "Saved template groups for views, sorted by ID. Each has :needs-binding
  (used roles with no default) and :placements [{:placement :run-id :layer-id
  :member-slots :bindings :resolved :version :stale? :empty-roles}]: the
  explicitly given bindings and every used role's resolved selector. :layer-id is the
  run's owner ID, which its member instances carry as their native layer, and
  :stale? says the template has changed since launch. Placements whose
  template is not saved (restored without it) are listed under their
  template ID with :orphan? true and the members they launched."
  [model]
  (let [saved (sort-by (comp key-name :id) (vals (groups model)))
        orphans (->> (:composition-runs model)
                     (filter (fn [[_ run]] (and (:template run) (nil? (lookup model (get-in run [:template :id]))))))
                     (group-by (fn [[_ run]] (key-name (get-in run [:template :id])))))]
    (into (mapv (fn [group]
                  (cond-> (assoc group :placements (mapv #(placement-view group %) (placements model (:id group))))
                    (seq (needs-binding group)) (assoc :needs-binding (needs-binding group))))
                saved)
          (map (fn [[_ runs]]
                 (let [runs (sort-by key runs)
                       [_ run] (first runs)]
                   {:id (get-in run [:template :id]) :orphan? true :name (get-in run [:definition :name])
                    :members (mapv #(select-keys % [:id :cue-id]) (get-in run [:definition :slots]))
                    :placements (mapv #(placement-view nil %) runs)})))
          (sort-by key orphans))))

(defn- check-template-run!
  "A placement run's :template data, as a load checks it."
  [run-key run]
  (let [{:keys [id version placement bindings empty-roles] :as template} (:template run)]
    (when-not (and (map? template) (every? template-keys (keys template)))
      (fail! [:template] "A placement names its template :id, :version, :placement and :bindings"))
    (when-not (run-name? id) (fail! [:template :id] "A placement names its template by an ID without /"))
    (when-not (pos-int? version) (fail! [:template :version] "A placement's template version is a positive integer"))
    (when-not (run-name? placement) (fail! [:template :placement] "A placement name has no /"))
    (doseq [k [:bindings :resolved]
            :let [selectors (get template k)]]
      (when-not (and (map? selectors) (every? identifier? (keys selectors)) (every? targets/selector? (vals selectors)))
        (fail! [:template k] "A placement's bindings map roles to selectors")))
    (when-not (or (nil? empty-roles) (and (vector? empty-roles) (every? identifier? empty-roles)))
      (fail! [:template :empty-roles] "A placement's empty roles are a vector of role IDs"))
    (when-not (= (key-name run-key) (run-id id placement))
      (fail! [] "A placement's run ID names its template and placement"))))

(defn validate-state
  "Check saved template groups against `model`'s cues, and every placement
  run's :template data, as a load does. Throws ex-info with :errors paths
  relative to the show model."
  [model]
  (let [state (get-in model [:extensions state-key] {})
        within (fn [prefix f]
                 (try (f)
                      (catch clojure.lang.ExceptionInfo error
                        (throw (ex-info (ex-message error)
                                        {:errors (mapv #(update % :path (partial into prefix)) (:errors (ex-data error)))})))))]
    (when-not (map? state) (fail! [:extensions state-key] "Template groups are a map of ID to definition"))
    (doseq [[id group] state]
      (within [:extensions state-key id]
              (fn []
                (when-not (and (map? group) (= id (:id group))) (fail! [] "Template group ID mismatch"))
                (when-not (pos-int? (:version group)) (fail! [:version] "A template group's version is a positive integer"))
                (when-let [extra (seq (remove (set (conj definition-keys :version)) (keys group)))]
                  (fail! [] (str "Unknown template group fields: " (str/join ", " extra))))
                (check-shape! group)
                (checked-macro model group))))
    (doseq [[run-key run] (:composition-runs model) :when (or (contains? run :template) (composition/reserved-id? run-key))]
      (within [:composition-runs run-key] #(check-template-run! run-key run)))
    model))
