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
  [:id :name :roles :members :cards])

(def ^:private role-keys #{:id :name :selector})

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
  "The composition run ID of a template group placement."
  [template-id placement]
  (str "template-group/" (key-name template-id) "/" (key-name placement)))

(defn- template-run? [template-id [_ run]]
  (when-let [id (get-in run [:template :id])]
    (= (key-name id) (key-name template-id))))

(defn placements
  "Running placements of a template group: [[run-id run] ...] by run ID."
  [model template-id]
  (sort-by key (filter #(template-run? template-id %) (:composition-runs model))))

(defn- in-template-terms
  "Errors from `composition/save` or `set-cards`, named as the template names
  them: slot paths become member paths and \"macro\" becomes \"template group\"."
  [error]
  (let [{:keys [errors]} (ex-data error)]
    (ex-info (str/replace (ex-message error) #"(?i)\bmacro\b" "template group")
             {:errors (mapv (fn [{:keys [path] :as e}]
                              (cond-> (update e :message #(some-> % (str/replace #"\bmacro\b" "template group")))
                                (= :slots (first path)) (assoc :path (into [:members] (rest path)))))
                            (or errors [{:path [] :message (ex-message error)}]))})))

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
    (:name group) (assoc :name (:name group))))

(defn- role-defaults [group]
  (into {} (keep (fn [{:keys [id selector]}] (when (some? selector) [(key-name id) selector]))) (:roles group)))

(defn- check-shape!
  "Structure that `composition/save` does not know: roles, member IDs and
  member roles."
  [definition]
  (when-not (and (map? definition) (registry/data? definition) (identifier? (:id definition)))
    (fail! [:id] "A template group needs portable data and an ID"))
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
  "Validate `group`'s members and cards as `composition/save` validates a
  macro, against the show's current cues. Returns the saved macro (its cards
  canonical)."
  [model group]
  (try (get-in (composition/save model (derived-macro group check-id (role-defaults group))) [:compositions check-id])
       (catch clojure.lang.ExceptionInfo error (throw (in-template-terms error)))))

(defn- launch-shape [group] (select-keys group [:roles :members]))

(defn save
  "Validate and save a template group. The owner sets :version: 1 for a new
  group, one more when its roles or members change; a card edit or rename
  keeps it. A save that omits :cards keeps the cards whose member keeps its
  ID and cue, as a macro save does; `:cards nil` clears them. Running
  placements keep the definition they launched with."
  [model definition]
  (check-shape! definition)
  (let [saved (lookup model (:id definition))
        definition (-> (select-keys definition definition-keys)
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
        (update :revision inc))))

(defn remove-group
  "Remove a saved template group that has no running placement."
  [model id]
  (let [group (or (lookup model id) (fail! [:id] "Choose a saved template group"))]
    (when (seq (placements model (:id group)))
      (fail! [:id] "Stop this template group's placements before removing it"))
    (-> model (update-in [:extensions state-key] dissoc (:id group)) (update :revision inc))))

(defn- bindings!
  "Each used role's selector: the launch's binding or the role's default.
  Every selector must resolve on the current rig, so an unknown group or
  fixture refuses the launch before anything starts. Keyed by `key-name`."
  [model group bindings]
  (when-not (or (nil? bindings) (map? bindings))
    (fail! [:bindings] "Bindings map each role to a selector"))
  (let [roles (:roles group)
        index (into {} (map-indexed (fn [i role] [(key-name (:id role)) i])) roles)
        used (set (keep (comp #(some-> % key-name) :role) (:members group)))]
    (doseq [[role selector] bindings]
      (when-not (and (identifier? role) (contains? index (key-name role)))
        (fail! [:bindings role] "A binding names one of this template group's roles"))
      (when-not (targets/selector? selector)
        (fail! [:bindings role] "A binding is {:group-id}, {:fixture-ids}, {:volume-id} or :all")))
    (let [given (into {} (map (fn [[role selector]] [(key-name role) selector])) bindings)
          bound (merge (role-defaults group) given)]
      (doseq [role (sort used) :when (not (contains? bound role))]
        (fail! [:bindings (:id (nth roles (index role)))] (str "Bind role " role ": it has no default selector")))
      (doseq [[role selector] (sort-by key bound)]
        (try (targets/resolve-ids model selector)
             (catch clojure.lang.ExceptionInfo error
               (fail! (if (contains? given role) [:bindings (:id (nth roles (index role)))] [:roles (index role) :selector])
                      (str "Role " role " does not resolve on this rig: " (ex-message error))))))
      bound)))

(defn launch
  "Start or replace one placement of a saved template group: its members
  launch together as one macro run, each role bound to the launch's
  `:bindings` selector or the role's default. `:placement` defaults to
  :main; launching the same placement again replaces only that one. The run
  captures the template's :version and the bindings it used."
  [model {:keys [id placement bindings]} time-ms]
  (let [group (or (lookup model id) (fail! [:id] "Choose a saved template group"))
        placement (if (some? placement) placement default-placement)
        _ (when-not (identifier? placement) (fail! [:placement] "A placement is a keyword or name"))
        bound (bindings! model group bindings)
        id (run-id (:id group) placement)
        _ (when (some #(= (key-name id) (key-name %)) (keys (:compositions model)))
            (fail! [:placement] "A saved composition already has this placement's ID"))
        definition (try (get-in (composition/save model (derived-macro group id bound)) [:compositions id])
                        (catch clojure.lang.ExceptionInfo error (throw (in-template-terms error))))
        role-id (into {} (map (juxt (comp key-name :id) :id)) (:roles group))]
    (-> (composition/start-definition model definition time-ms)
        (update-in [:composition-runs id] assoc
                   :template {:id (:id group) :version (:version group) :placement placement
                              :bindings (into {} (map (fn [[role selector]] [(role-id role) selector])) bound)}))))

(defn stop
  "Stop one placement of a template group, or all of them when `:placement`
  is nil. Each releases only its own instances. Stopping a placement that is
  not running changes nothing."
  [model {:keys [id placement]} time-ms]
  (when-not (identifier? id) (fail! [:id] "Name a template group"))
  (when-not (or (nil? placement) (identifier? placement)) (fail! [:placement] "A placement is a keyword or name"))
  (reduce (fn [model [run-id run]]
            (if (or (nil? placement) (= (key-name placement) (key-name (get-in run [:template :placement]))))
              (composition/stop model run-id time-ms)
              model))
          model (placements model id)))

(defn set-cards
  "Change a template group's card arrangement with one `composition/set-cards`
  form (`:cards`, `:resize` or `:move`, and the forms it adds later). The
  arrangement belongs to the template and is presentation only, so its
  :version stays and every running placement shows the change at once."
  [model id change]
  (let [group (or (lookup model id) (fail! [:id] "Choose a saved template group"))
        scratch (composition/save model (derived-macro group check-id (role-defaults group)))
        cards (try (get-in (composition/set-cards scratch check-id change) [:compositions check-id :cards])
                   (catch clojure.lang.ExceptionInfo error (throw (in-template-terms error))))]
    (-> model
        (assoc-in [:extensions state-key (:id group)] (cond-> (dissoc group :cards) (seq cards) (assoc :cards cards)))
        (update :revision inc))))

(defn projection
  "Saved template groups for views, sorted by ID, each with :placements
  [{:placement :run-id :layer-id :member-slots :bindings :version :stale?}].
  :layer-id is the run's owner ID, which its member instances carry as their
  native layer, and :stale? says the template has changed since launch."
  [model]
  (mapv (fn [group]
          (assoc group :placements
                 (mapv (fn [[run-id {:keys [owner-id template definition]}]]
                         {:placement (:placement template) :run-id run-id :layer-id owner-id
                          :member-slots (mapv :id (:slots definition))
                          :bindings (:bindings template) :version (:version template)
                          :stale? (not= (:version template) (:version group))})
                       (placements model (:id group)))))
        (sort-by (comp key-name :id) (vals (groups model)))))

(defn validate-state
  "Check saved template groups against `model`'s cues, as a load does. Throws
  ex-info with :errors paths relative to the saved state."
  [model]
  (let [state (get-in model [:extensions state-key])]
    (when-not (map? state) (fail! [] "Template groups are a map of ID to definition"))
    (doseq [[id group] state]
      (try
        (when-not (and (map? group) (= id (:id group))) (fail! [] "Template group ID mismatch"))
        (when-not (pos-int? (:version group)) (fail! [:version] "A template group's version is a positive integer"))
        (when-let [extra (seq (remove (set (conj definition-keys :version)) (keys group)))]
          (fail! [] (str "Unknown template group fields: " (str/join ", " extra))))
        (check-shape! group)
        (checked-macro model group)
        (catch clojure.lang.ExceptionInfo error
          (throw (ex-info (ex-message error)
                          {:errors (mapv #(update % :path (partial into [id])) (:errors (ex-data error)))})))))
    model))
