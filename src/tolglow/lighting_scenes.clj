(ns tolglow.lighting-scenes
  "Target the fraction of visible light in an operator-selected part of a rig.
  A scene owns one low-priority cue instance. Meter its actual group output after
  all writers and the operator master, then update only that instance's opacity."
  (:require [afterglow.control.model :as control]
            [clojure.string :as str]
            [tolglow.automation.random :as random]
            [tolglow.lighting-capacity :as capacity]
            [tolglow.registry :as registry]))

(def ^:private location [:extensions ::control :groups])
(def ^:private light-fields (conj registry/emitter-attributes :color :dimmer))
(def ^:private special-tags #{:code :code-cue :utility :show-control :control :system :modulation :scene})
(defn- fail [message data] (throw (ex-info message data)))
(defn- identifier? [x] (or (keyword? x) (and (string? x) (not (str/blank? x)))))
(defn- unit? [x] (and (number? x) (<= 0 x 1)))
(defn- fixtures [model] (get-in model [:rig :fixtures]))
(defn- group-path [id] (conj location id))
(defn- eligible? [{:keys [channels]}]
  (or (some #(contains? channels %) (disj light-fields :color :dimmer :uv))
      (and (:dimmer channels) (not (some #(contains? channels %) [:fog :haze :pump :relay :fan :uv])))))
(defn eligible-ids [rig] (into [] (comp (filter eligible?) (map :id)) rig))
(defn groups [model] (get-in model location {}))

(defn snapshot [model eligible]
  {:eligible-fixture-ids eligible
   :groups (mapv (fn [g]
                   (assoc (select-keys g [:group-id :fixture-ids :target :actual :status :source-effect-id
                                         :gain :instance-id :generation :target-var :effect-ids :allow-generated?])
                          :source-params (get-in model [:instances (:instance-id g) :params])))
                 (sort-by (comp str :group-id) (vals (groups model))))})

(defn validate-request [{:keys [op group-id target fixture-ids effect-ids target-var params allow-generated?]}]
  (when-not (and (#{:apply :vary :release} op) (identifier? group-id)
                 (or (nil? target) (unit? target))
                 (or (nil? fixture-ids) (and (vector? fixture-ids) (every? identifier? fixture-ids)))
                 (or (nil? effect-ids) (and (vector? effect-ids) (every? identifier? effect-ids)))
                 (or (nil? target-var) (and (vector? target-var) (seq target-var)
                                             (every? identifier? target-var)))
                 (or (nil? params) (and (map? params) (registry/data? params)))
                 (or (nil? allow-generated?) (boolean? allow-generated?)))
    (fail "A lighting scene needs a group, an apply/vary/release operation and normalized target" {:group group-id})))

(defn validate-state! [model]
  (when-let [{:keys [groups receipts] :as state} (get-in model [:extensions ::control])]
    (when-not (and (map? state) (map? (or groups {})) (or (nil? receipts) (and (vector? receipts) (<= (count receipts) 128))))
      (fail "Invalid saved lighting scene state" {}))
    (reduce-kv
     (fn [occupied id {:keys [group-id gain generation target fixture-ids instance-id] :as group}]
       (validate-request (assoc group :op :apply))
       (when-not (and (= group-id id) (unit? gain) (unit? target) (pos-int? generation)
                      (string? instance-id) (seq fixture-ids) (= (count fixture-ids) (count (distinct fixture-ids)))
                      (not-any? occupied fixture-ids))
         (fail "Invalid or overlapping saved lighting group" {:group-id id}))
       (into occupied fixture-ids)) #{} groups))
  model)

(defn- targets [model {:keys [group-id fixture-ids]} old]
  (let [rig (fixtures model)
        selected (or fixture-ids (:fixture-ids old)
                     (mapv :id (filter #(some #{group-id} (:groups %)) rig)))
        ids (set selected)
        known (set (map :id rig))
        heads (filterv #(and (ids (:id %)) (eligible? %)) rig)
        available (set (map :id heads))]
    (when-not (and (every? known selected) (seq heads))
      (fail "Choose existing light-emitting fixtures or a patched group" {:group group-id}))
    (when-let [other (some (fn [[id g]] (when (and (not= id group-id) (some available (:fixture-ids g))) id)) (groups model))]
      (fail "A fixture already belongs to another controlled lighting group" {:group other}))
    heads))

(defn- existing-id [ids value]
  (if (contains? ids value) value
    (let [alternate (cond (keyword? value) (subs (str value) 1) (string? value) (keyword value))]
      (if (contains? ids alternate) alternate value))))

(defn- normalize-ids [model request]
  (let [rig (fixtures model)
        active (set (keys (groups model)))
        group-ids (if (contains? active (existing-id active (:group-id request))) active
                    (into active (mapcat :groups) rig))]
    (cond-> (update request :group-id (partial existing-id group-ids))
      (:fixture-ids request) (update :fixture-ids #(mapv (partial existing-id (set (map :id rig))) %))
      (:effect-ids request) (update :effect-ids #(mapv (partial existing-id (set (keys (get-in model [:registry :effects])))) %)))))

(defn- supported? [fixture attr]
  (if (= attr :color) (every? (:channels fixture) [:red :green :blue])
      (or (contains? (:channels fixture) attr) (= attr :dimmer))))

(defn- scoped-outputs [definition heads]
  (let [all (when (contains? definition :targets) (set (:targets definition)))]
    (into [] (keep (fn [{:keys [attribute targets] :as output}]
                     (when (and (light-fields attribute) (not= attribute :uv))
                       (let [selected (when targets (set targets))
                             ids (into [] (comp (filter #(and (supported? % attribute)
                                                              (or (nil? all) (all (:id %)))
                                                              (or (nil? selected) (selected (:id %)))))
                                                (map :id)) heads)]
                         (when (seq ids) (if (= (count ids) (count heads)) (dissoc output :targets) (assoc output :targets ids)))))))
          (registry/output-entries (:outputs definition)))))

(defn- safe-source? [entry]
  (and entry (not= false (:auto? entry)) (not (:code-cue? entry)) (not (:show-control? entry))
       (not (special-tags (:kind entry))) (not (special-tags (:role entry)))
       (not (:modulation entry)) (not (:layer entry)) (not (:scene-owner entry))
       (not-any? special-tags (concat (:tags entry) (:categories entry)))))

(defn- choose-source [model heads {:keys [effect-ids params last-source-id]} generation]
  (let [allowed (when (some? effect-ids) (set effect-ids))
        cues (vals (get-in model [:registry :cues]))
        by-effect (group-by :effect-id cues)
        ordered (->> (vals (get-in model [:registry :effects]))
         (filter #(and (safe-source? %) (or (nil? allowed) (allowed (:id %)))
                       ;; An effect attached exclusively to show-control cues stays outside this pool.
                       (or (empty? (get by-effect (:id %))) (some safe-source? (get by-effect (:id %))))
                       (every? (set (keys (:params %))) (keys params))))
                     (sort-by (comp str :id)) vec)]
    (when (seq ordered)
      (let [offset (mod (dec generation) (count ordered))
            rotated (concat (subvec ordered offset) (subvec ordered 0 offset))]
        (some (fn [effect]
                (let [outs (scoped-outputs effect heads)]
                  (when (and (seq outs)
                             (or (some #(not= :dimmer (:attribute %)) outs)
                                 (some #(and (:dimmer (:channels %))
                                             (not (some registry/emitter-attributes (keys (:channels %))))) heads)))
                    {:effect effect :outputs outs})))
              (concat (remove #(= last-source-id (:id %)) rotated)
                      (filter #(= last-source-id (:id %)) rotated)))))))

(defn- generated [heads target generation]
  (let [fields (sort (filter (disj light-fields :color :uv) (set (mapcat (comp keys :channels) heads))))
        accent (when-let [colors (seq (remove #{:dimmer} fields))]
                 (nth colors (mod generation (count colors))))
        outputs (mapv (fn [field]
                        (let [targets (mapv :id (filter #(get-in % [:channels field]) heads))]
                          (cond-> {:attribute field
                                   :value {:op :const :value (if (= accent field) (+ 0.75 (* 0.25 target)) 1.0)}}
                            (< (count targets) (count heads)) (assoc :targets targets)))) fields)]
    {:effect {:params {}} :outputs outputs}))

(defn- issue [model group now cmd]
  (let [result (control/apply-command model
                 (merge {:show-id (:show-id model) :epoch (:epoch model) :time-ms now
                         :source {:adapter :lighting-scene :group-id group}
                         :command-id (pr-str [:lighting-scene (:epoch model) group (:revision model) (:op cmd)])} cmd))]
    (if (= :accepted (get-in result [:ack :status])) result
        (fail (get-in result [:ack :message] "Group scene command rejected") (:ack result)))))

(defn- retire [model group now]
  (let [instance (get-in model [:instances (:instance-id group)])]
    (if (= {:adapter :lighting-scene :group-id (:group-id group)} (:source instance))
      (:model (issue model (:group-id group) now {:op :instance/remove :instance-id (:id instance)})) model)))

(defn session-source [model instance]
  (let [{:keys [adapter group-id] :as source} (:source instance)]
    (when (and (= adapter :lighting-scene) (= group-id (get-in instance [:definition :scene-owner]))
               (= (:id instance) (get-in model (conj (group-path group-id) :instance-id))))
      source)))

(defn restore [model ids]
  (if (seq (groups model))
    (update-in model location (fn [gs]
                                (into {} (keep (fn [[id g]]
                                                 (when-let [new-id (get ids (:instance-id g))]
                                                   [id (assoc g :instance-id new-id :status :seeking)]))) gs)))
    model))

(defn- launch [model group choice now]
  (let [{:keys [group-id target generation params]} group
        id (str "Lighting scene " (pr-str group-id))
        existing (get-in model [:registry :effects id])
        _ (when (and existing (not= group-id (:scene-owner existing)))
            (fail "A user definition already owns the scene name" {:id id}))
        old-cue (get-in model [:registry :cues id])
        _ (when (and old-cue (not= group-id (:scene-owner old-cue)))
            (fail "A user cue already owns the scene name" {:id id}))
        effect (:effect choice)
        definition {:id id :version (inc (:version existing 0)) :name id :scene-owner group-id
                    :tags [:emission :scene] :auto? false :targets (:fixture-ids group) :params (:params effect {}) :outputs (:outputs choice)}
        cue {:id id :name id :effect-id id :scene-owner group-id :tags [:scene] :auto? false
             :priority Integer/MIN_VALUE :reload-policy :pinned}
        model (retire model group now)
        model (if (= (dissoc definition :version) (dissoc existing :version)) model
                (:model (issue model group-id now {:op :registry/publish :kind :effects :definition definition})))
        model (if (= cue (dissoc old-cue :version)) model
                (:model (issue model group-id now {:op :cue/put :cue cue})))
        ;; Only opt-in randomizable source parameters change, and explicit
        ;; caller overrides always win. Replaying the same state is deterministic.
        schemas (:params definition)
        varied (when (and (:id effect) (> generation 1) (seq schemas))
                 (:params (random/randomize-params schemas {} (bit-and 0xffffffff (hash [group-id generation])))))
        launch-params (if (:id effect) (merge {} varied params) {})
        res (issue model group-id now {:op :cue/launch :cue-id id
                                       :launch {:mode :persistent :params launch-params
                                                :system {:opacity target}}})
        instance-id (get-in res [:ack :instance-id])
        group (assoc group :generation generation :instance-id instance-id :gain target :saturated 0
                     :source-effect-id (:id effect) :last-source-id (or (:id effect) (:last-source-id group)) :status :seeking)]
    (assoc-in (:model res) (group-path group-id) group)))

(defn- apply-new-request [model {:keys [op group-id] :as request} now]
  (validate-request request)
  (let [old (get-in model (group-path group-id))]
    (if (= op :release)
      (-> (retire model old now) (update-in location dissoc group-id))
      (let [heads (targets model request old)
            group (merge {:allow-generated? true :generation 0}
                         (cond-> old (contains? request :target) (dissoc :target-var))
                         (select-keys request [:group-id :target :effect-ids :target-var :params :allow-generated?])
                         {:fixture-ids (mapv :id heads)})
            group (if (:target-var group)
                    (assoc group :target (get-in (:variables model) (:target-var group) (:target group))) group)
            _ (when-not (unit? (:target group)) (fail "Supply a normalized lighting target or a bound show variable" {:group group-id}))
            replace? (or (= op :vary) (nil? old)
                         (some #(not= (get group %) (get old %)) [:fixture-ids :effect-ids :params :allow-generated?]))]
        (if replace?
          (let [generation (inc (:generation group))
                choice (or (choose-source model heads group generation)
                           (when (:allow-generated? group) (generated heads (:target group) generation))
                           (fail "No permitted registry effect emits on these fixtures" {:group group-id}))]
            (launch model (assoc group :generation generation) choice now))
          (assoc-in model (group-path group-id) (assoc group :status :seeking :saturated 0)))))))

(defn apply-request
  "Apply one scene ownership transition, deduplicating explicitly identified
  requests in the same epoch. Event gate state remains separate from receipts."
  [model request now]
  (let [request (normalize-ids model request)
        id (:command-id request)
        body (select-keys request [:op :group-id :target :fixture-ids :effect-ids :params :target-var :allow-generated?])
        receipts (get-in model [:extensions ::control :receipts] [])
        previous (when id (some #(when (and (= (:epoch model) (:epoch %)) (= id (:id %))) %) receipts))]
    (if previous
      (if (= body (:request previous)) model (fail "Scene request ID already used with different settings" {:id id}))
      (let [updated (apply-new-request model request now)
            updated (if (and (not= model updated) (= (:revision model) (:revision updated)))
                      (update updated :revision inc) updated)]
        (if id
          (assoc-in updated [:extensions ::control :receipts]
                    (conj (vec (take-last 127 receipts)) {:id id :epoch (:epoch model) :request body}))
          updated)))))

(defn prepare [patch ids]
  (let [lookup (:by-id patch)
        fixtures (into [] (keep lookup) ids)]
    (capacity/prepare (assoc patch :fixtures fixtures))))

(defn feedback
  "Use one committed normalized measurement per group. Each update is bounded;
  operator blackout, pause and master zero suspend regulation. Generated fill
  can replace an insufficient source only when the caller permits it."
  [model measurements now suspended?]
  (reduce-kv
   (fn [m id group]
     (let [{:keys [target instance-id gain saturated target-var]} group
           value (get measurements id)
           target (if target-var (get-in (:variables m) target-var target) target)
           owned? #(= {:adapter :lighting-scene :group-id id} (:source %))
           instance (or (when-let [inst (get-in m [:instances instance-id])] (when (owned? inst) inst))
                        (some #(when (owned? %) %) (vals (:instances m))))
           instance-id (:id instance)
           group (assoc group :instance-id instance-id)]
       (cond
         (nil? instance) (update-in m location dissoc id)
         (or (not (unit? target)) (not (number? value))) m
         :else
         (let [suspended? (or suspended? (:blackout? m) (get-in instance [:system :paused?]) (= :releasing (:phase instance))
                              (zero? (:master m)) (false? (get-in m [:transport :playing?])))
               error (- target value)
               close? (<= (abs error) 0.015)
               next-gain (cond (or close? suspended?) gain
                               :else (max 0.0 (min 1.0 (+ gain (max -0.2 (min 0.2 (* error 0.7)))))))
               capped? (and (> error 0.015) (>= gain 0.999))
               saturated (if (and capped? (not suspended?)) (min 6 (inc (or saturated 0))) 0)
               status (cond suspended? :limited close? :at-target
                            (and (> saturated 2) (zero? value)) :unlit
                            (> saturated 2) :limited
                            (and (< error -0.015) (zero? gain)) :over-target :else :seeking)
               adjusted (assoc group :actual value :target target :gain next-gain :saturated saturated :status status)
               m (-> m (assoc-in (group-path id) adjusted)
                     (assoc-in [:instances instance-id :system :opacity] next-gain))]
           (if (and (= saturated 5) (:allow-generated? group) (:source-effect-id group))
             (let [wanted (set (:fixture-ids group))
                   heads (filterv #(and (wanted (:id %)) (eligible? %)) (fixtures m))]
               (if (seq heads) (launch m adjusted (generated heads target (:generation group)) now) m))
             m))))) model (groups model)))
