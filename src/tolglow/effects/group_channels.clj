(ns tolglow.effects.group-channels
  "Manual effect channels, exclusive auto overlay membership and mixer projections."
  (:require [afterglow.control.categories :as categories]
            [afterglow.control.views :as views]
            [clojure.set :as set]
            [tolglow.registry :as registry]))

(def ^:private settings-path [:settings :effect-group-channels])

(def ^:private defaults {:manual [] :overlays {} :active-overlay nil})

(def ^:private selector-keys {:fixtures :fixture-ids :categories :categories :cue-pages :page-ids})

(defn operation? [op]
  (contains? #{:effect-groups/configure :effect-groups/overlay-set
               :effect-groups/level-set :effect-groups/assign-instance} op))

(defn- reject! [code message details]
  (throw (ex-info message {:control/error code :details details})))

(defn- id? [x]
  (or (keyword? x) (and (string? x) (seq x))))

(defn- ids? [xs id-test]
  (and (vector? xs) (every? id-test xs) (= (count xs) (count (set xs)))))

(defn- level? [level]
  (and (registry/finite-number? level) (<= 0 level 1)))

(defn- level [group] (double (:level group 1)))

(defn- validate-group! [group kind fields]
  (when-not (and (map? group) (every? (into #{:id :name :level} fields) (keys group))
                 (id? (:id group)) (level? (:level group 1))
                 (or (not (contains? group :name)) (string? (:name group))))
    (reject! :invalid-group "A group needs a stable ID, optional name and a unit level"
             {:kind kind :group-id (:id group)})))

(defn- manual-entries [manual]
  (letfn [(entries [groups indices ancestors]
            (mapcat (fn [index group]
                      (let [path (conj indices index)
                            lineage (conj ancestors (:id group))]
                        (cons {:group group :path path :lineage lineage}
                              (lazy-seq (entries (:groups group []) (conj path :groups) lineage)))))
                    (range) groups))]
    (entries manual [:manual] [])))

(defn- require-unique! [ids field]
  (when-not (= (count ids) (count (set ids)))
    (reject! :duplicate-membership "Group and cue IDs must be unique within their grouping"
             {:field field})))

(defn- validate-config! [config]
  (when-not (and (map? config) (every? (set (keys defaults)) (keys config)))
    (reject! :invalid-groups "Expected manual groups, overlays and an optional active overlay" {}))
  (let [{:keys [manual overlays active-overlay] :as config} (merge defaults config)]
    (when-not (and (vector? manual) (map? overlays) (every? selector-keys (keys overlays))
                   (or (nil? active-overlay) (contains? overlays active-overlay)))
      (reject! :invalid-groups "Manual groups must be a vector and an active overlay must be configured" {}))
    (let [groups (map :group (manual-entries manual))]
      (doseq [group groups]
        (validate-group! group :manual [:cue-ids :groups])
        (when-not (and (vector? (:groups group [])) (ids? (:cue-ids group []) id?))
          (reject! :invalid-group "Manual children and distinct cue IDs must be vectors" {:group-id (:id group)})))
      (require-unique! (map :id groups) :manual)
      (require-unique! (mapcat :cue-ids groups) :cue-ids))
    (doseq [[kind groups] overlays
            :let [selector-key (selector-keys kind)]]
      (when-not (vector? groups)
        (reject! :invalid-overlay "Overlay groups must be an ordered vector" {:kind kind}))
      (doseq [group groups]
        (validate-group! group kind (cond-> [selector-key] (= :categories kind) (conj :match)))
        (let [values (get group selector-key)]
          (when-not (and (ids? values (if (= :categories kind) keyword? id?)) (seq values)
                         (#{:any :all} (:match group :any)))
            (reject! :invalid-selector "Overlay groups need a nonempty vector of distinct selector IDs"
                     {:kind kind :group-id (:id group)}))))
      (require-unique! (map :id groups) kind))
    config))

(defn- configuration [model] (get-in model settings-path defaults))

(defn- orphaned-placements [model config]
  (let [group-ids (into #{} (map (comp :id :group)) (manual-entries (:manual config)))]
    (keep (fn [[id instance]]
            (when (and (contains? instance ::manual-group)
                       (not (contains? group-ids (::manual-group instance))))
              id))
          (:instances model))))

(defn validate-state!
  "Validate portable group settings and instance placements before restoring them."
  [model]
  (let [config (validate-config! (configuration model))]
    (when-let [id (first (orphaned-placements model config))]
      (throw (ex-info "Manual placement refers to a missing group"
                      {:path [:instances id ::manual-group]}))))
  model)

(defn- group-path [{:keys [manual overlays]} [kind id]]
  (if (= :manual kind)
    (some (fn [{:keys [group path]}] (when (= id (:id group)) path)) (manual-entries manual))
    (some (fn [[index group]] (when (= id (:id group)) [:overlays kind index]))
          (map-indexed vector (get overlays kind)))))

(defn resolve-text-address
  "Resolve an untyped JSON group ID using the owner model. A receipt retains
  the original typed address if the same spelling is retried after a reconfigure."
  [{:keys [op channel-id group-id] :as command} model previous]
  (let [previous (when (= op (:op previous)) previous)
        resolve-id (fn [kind text recorded]
                     (let [key-id (keyword text)]
                       (cond
                         (or (= recorded text) (= recorded key-id)) recorded
                         (group-path (configuration model) [kind text]) text
                         :else key-id)))]
    (case op
      :effect-groups/level-set
      (if (and (vector? channel-id) (= 2 (count channel-id))
               (string? (second channel-id)) (seq (second channel-id)))
        (let [[kind id] channel-id
              [previous-kind previous-id] (:channel-id previous)]
          (assoc command :channel-id [kind (resolve-id kind id (when (= kind previous-kind) previous-id))]))
        command)

      :effect-groups/assign-instance
      (cond-> command
        (and (string? group-id) (seq group-id))
        (assoc :group-id (resolve-id :manual group-id (:group-id previous))))

      command)))

(defn- transition [before after event result]
  {:model after :result result
   :events (if (= before after) [] [(assoc result :type event)])})

(defn apply-operation
  "Apply a group command to an immutable model after show-owner admission.
  A level address is [kind group-id]. A nil instance assignment restores its
  authored cue placement. Invalid input throws the shared control error shape."
  [model {:keys [op config channel-id instance-id group-id overlay value] :as command}]
  (let [current (configuration model)]
    (case op
      :effect-groups/configure
      (let [patch? (contains? command :config-patch)
            patch (:config-patch command)
            _ (when (and patch? (or (contains? command :config) (not (map? patch))))
                (reject! :invalid-groups "Supply one group configuration or settings patch" {}))
            config (validate-config! (if patch? (merge current patch) config))
            next-model (if (= current config) model
                           (reduce (fn [m id] (update-in m [:instances id] dissoc ::manual-group))
                                   (assoc-in model settings-path config) (orphaned-placements model config)))]
        (transition model next-model
                    :effect-groups/configured {:active-overlay (:active-overlay config)}))

      :effect-groups/overlay-set
      (do
        (when-not (and (contains? command :overlay) (or (nil? overlay) (contains? (:overlays current) overlay)))
          (reject! :invalid-overlay "Select a configured overlay or nil" {:overlay overlay}))
        (let [next-model (if (= overlay (:active-overlay current)) model
                             (assoc-in model (conj settings-path :active-overlay) overlay))]
          (transition model next-model :effect-groups/overlay-changed {:active-overlay overlay})))

      :effect-groups/level-set
      (do
        (when-not (and (vector? channel-id) (= 2 (count channel-id))
                       (or (= :manual (first channel-id)) (contains? selector-keys (first channel-id)))
                       (id? (second channel-id)) (level? value))
          (reject! :invalid-value "Set a unit level at a group channel ID" {:channel-id channel-id}))
        (let [path (group-path current channel-id)]
          (when-not path
            (reject! :unknown-group "Group channel no longer exists" {:channel-id channel-id}))
          (let [value (double value)
                next-model (if (== value (level (get-in current path))) model
                               (assoc-in model (into settings-path (conj path :level)) value))]
            (transition model next-model :effect-groups/level-changed {:channel-id channel-id :level value}))))

      :effect-groups/assign-instance
      (do
        (when-not (get-in model [:instances instance-id])
          (reject! :unknown-instance "Instance no longer exists" {:instance-id instance-id}))
        (when-not (and (contains? command :group-id)
                       (or (nil? group-id) (group-path current [:manual group-id])))
          (reject! :unknown-group "Instance placement requires a manual group ID or nil" {:group-id group-id}))
        (let [path [:instances instance-id]
              next-model (if (nil? group-id) (update-in model path dissoc ::manual-group)
                             (assoc-in model (conj path ::manual-group) group-id))]
          (transition model next-model :effect-groups/instance-assigned
                      {:instance-id instance-id :group-id group-id})))

      (reject! :unknown-operation "Unknown effect group operation" {:op op}))))

(defn- narrow [selected ids]
  (if (some? ids) (set/intersection selected (set ids)) selected))

(defn- fixture-footprint [instance patch supplied]
  (let [{:keys [id definition targets]} instance
        program (:layer definition)
        slot (get-in instance [:source :native-layer])
        transforms (concat (:transforms program) (:transforms slot))]
    (cond
      (contains? supplied id)
      (if-some [ids (get supplied id)]
        {:source :captured :target-ids (set/intersection patch (set ids))}
        {:source :unresolved :target-ids #{}})
      (and (nil? targets) (seq transforms)
           (some #(= :previous (:targets %)) [program slot]))
      {:source :unresolved :target-ids #{}}
      :else
      (let [selected (narrow (narrow patch (:targets definition)) targets)
            incoming (reduce (fn [ids output] (into ids (narrow selected (:targets output))))
                             #{} (registry/output-entries (:outputs definition)))
            layer-targets (or (when (some? targets) selected)
                              (when program (seq (:targets definition)))
                              (:target-ids slot))]
        {:source :declared
         :target-ids (if (seq transforms) (into incoming (narrow patch (or layer-targets []))) incoming)}))))

(defn- overlay-membership [kind group {:keys [fixture-ids categories page-ids]}]
  (let [actual (case kind :fixtures fixture-ids :categories categories :cue-pages page-ids)
        matches ((selector-keys kind) group)]
    (if (= :all (:match group)) (every? actual matches) (some actual matches))))

(defn- group-row [kind group]
  {:id [kind (:id group)] :type :group :name (:name group (str (:id group))) :level (level group)})

(defn- manual-rows [groups memberships]
  (mapv (fn [group]
          (let [members (get memberships (:id group))]
            (assoc (group-row :manual group)
                   :instance-ids (mapv :instance-id members)
                   :direct-instance-ids (into [] (comp (filter #(= (:id group) (peek (:manual-path %))))
                                                       (map :instance-id)) members)
                   :groups (manual-rows (:groups group) memberships)))) groups))

(defn projection
  "Project shared committed levels and explicit membership from ordered active instances.
  Fixture selectors use declared targets on the current patch by default.
  Optional :target-ids-by-instance maps instance IDs to pre-mixer target ID
  collections for cases requiring a captured footprint. An empty collection
  denotes a known empty footprint. :member-levels contains only non-unity
  factors to apply at the existing per-instance opacity boundary."
  ([model] (projection model {}))
  ([model {:keys [target-ids-by-instance]}]
   (let [{:keys [manual overlays active-overlay]} (configuration model)
         entries (manual-entries manual)
         manual-by-id (into {} (map (juxt (comp :id :group) identity)) entries)
         manual-by-cue (into {} (mapcat (fn [{:keys [group]}] (map #(vector % (:id group)) (:cue-ids group)))) entries)
         overlay-groups (get overlays active-overlay [])
         overlay-levels (into {} (map (juxt :id level)) overlay-groups)
         fixtures (when (= :fixtures active-overlay)
                    (into {} (map (juxt :id :fixture-id)) (get-in model [:rig :fixtures])))
         patch (set (keys fixtures))
         pages-by-cue (when (= :cue-pages active-overlay)
                        (reduce-kv (fn [index id page]
                                     (reduce #(update %1 %2 (fnil conj #{}) id) index (vals (:cells page))))
                                   {} (:pages model)))
         instances
         (mapv (fn [{:keys [id cue-id effect-id definition phase system] :as instance}]
                 (let [assigned (::manual-group instance)
                       group-id (or (when (contains? manual-by-id assigned) assigned)
                                    (get manual-by-cue cue-id))
                       path (get-in manual-by-id [group-id :lineage] [])
                       footprint (when fixtures (fixture-footprint instance patch target-ids-by-instance))
                       cats (categories/definition-categories definition)
                       facts {:fixture-ids (when footprint (into (:target-ids footprint) (keep fixtures) (:target-ids footprint)))
                              :categories (set cats)
                              :page-ids (get pages-by-cue cue-id #{})}
                       candidates (into [] (comp (filter #(overlay-membership active-overlay % facts)) (map :id))
                                        overlay-groups)
                       winner (first candidates)
                       factor (reduce (fn [factor id] (* factor (level (get-in manual-by-id [id :group]))))
                                      (get overlay-levels winner 1.0) path)]
                   (cond-> {:id [:instance id] :instance-id id :cue-id cue-id :effect-id effect-id
                            :name (or (get-in model [:registry :cues cue-id :name]) (:name definition) (str (or cue-id effect-id)))
                            :phase phase :type :instance
                            :categories cats :opacity (:opacity system 1.0) :manual-path path
                            :overlay-candidates candidates :overlay-group winner :member-level factor}
                     footprint (assoc :target-source (:source footprint) :target-ids (:target-ids footprint)))))
               (views/active-instances model))
         manual-members (reduce (fn [index row]
                                  (reduce #(update %1 %2 (fnil conj []) row) index (:manual-path row)))
                                {} instances)
         overlay-members (group-by :overlay-group instances)]
     {:active-overlay active-overlay
      :instances instances
      :member-levels (into {} (keep (fn [{:keys [instance-id member-level]}]
                                     (when-not (== 1 member-level) [instance-id member-level]))) instances)
      :manual (manual-rows manual manual-members)
      :overlay (mapv (fn [group]
                       (assoc (group-row active-overlay group)
                              :instance-ids (mapv :instance-id (get overlay-members (:id group)))
                              :matching-instance-ids (into [] (comp (filter #(some #{(:id group)} (:overlay-candidates %)))
                                                                    (map :instance-id)) instances)))
                     overlay-groups)})))

(defn- reload-candidates [model]
  (into {} (keep (fn [[id {:keys [effect-id effect-version] :as instance}]]
                   (let [{:keys [version] :as definition} (get-in model [:registry :effects effect-id])]
                     (when (and (not= :pinned (:reload-policy instance :pinned))
                                (not (get-in instance [:system :paused?]))
                                (not= version (get-in instance [:reload-fault :effect-version]))
                                (integer? version) (> version effect-version))
                       [id (assoc instance :effect-version version :definition definition)]))))
        (:instances model)))

(defn frame-levels
  "Freeze non-unity factors using the same membership as the mixer projection.
  A definition-dependent membership change carries both version levels so the
  engine can apply the factor for whichever definition it evaluates."
  [model]
  (when-let [{:keys [manual overlays active-overlay]} (get-in model settings-path)]
    (let [overlay-levels? (some #(not (== 1 (level %))) (get overlays active-overlay))]
      (when (or overlay-levels?
                (some #(not (== 1 (level (:group %)))) (manual-entries manual)))
        (let [factors (:member-levels (projection model))
              candidates (when (and overlay-levels? (#{:fixtures :categories} active-overlay))
                           (reload-candidates model))
              candidate-factors (when (seq candidates) (:member-levels (projection (assoc model :instances candidates))))]
          (not-empty
            (reduce-kv (fn [levels id candidate]
                         (let [captured (get factors id 1)
                               proposed (get candidate-factors id 1)]
                           (if (== captured proposed) levels
                             (assoc levels id {(get-in model [:instances id :effect-version]) captured
                                               (:effect-version candidate) proposed}))))
                       factors candidates)))))))

(defn channel-rows
  "Flatten one mixer view with stable channel IDs. Expansion is local to the
  caller. Keep authored group order and shared instance order within each group."
  [{:keys [instances manual overlay]} view expanded]
  (let [instances-by-id (into {} (map (juxt :instance-id identity)) instances)
        overlay? (= :overlay view)
        parents (if overlay? overlay manual)]
    (letfn [(instance-rows [depth ids]
              (map #(assoc (instances-by-id %) :depth depth) ids))
            (group-rows [depth groups]
              (mapcat (fn [{:keys [id instance-ids direct-instance-ids] children :groups :as group}]
                        (let [open? (contains? expanded id)]
                          (cons (assoc (dissoc group :groups :direct-instance-ids) :depth depth :expanded? open?)
                                (when open?
                                  (concat (group-rows (inc depth) children)
                                          (instance-rows (inc depth)
                                                         (if overlay? instance-ids direct-instance-ids)))))))
                      groups))]
      (into (vec (group-rows 0 parents))
            (comp (filter (if overlay? (comp nil? :overlay-group) (comp empty? :manual-path)))
                  (map #(assoc % :depth 0))) instances))))
