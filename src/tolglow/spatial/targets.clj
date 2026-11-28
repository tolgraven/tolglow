(ns tolglow.spatial.targets
  "Fixture selection, searchable registry properties and per-show picker usage."
  (:require [afterglow.control.targets :as selection]
            [clojure.set :as set]
            [clojure.string :as str]
            [tolglow.fixtures.bounds :as bounds]
            [tolglow.registry :as registry]
            [tolglow.rig.roles :as roles]))

(def usage-path [:extensions ::usage])

(defn- label [id] (if (keyword? id) (subs (str id) 1) (str id)))

(defn- id? [x] (or (keyword? x) (and (string? x) (not (str/blank? x)))))

(def selector? selection/selector?)

(defn- identify [ids id]
  ;; Transit preserves IDs verbatim. Text clients can still address an unambiguous ID.
  (or (some #(when (= id %) %) ids)
      (let [matches (filter #(= (label %) (label id)) ids)]
        (when (= 1 (count matches)) (first matches)))))

(defn volume-id [configured requested]
  (identify (keys configured) requested))

(defn strip-segment-selector
  "Select the heads of strip-id whose centres are within a reference's declared
  extent on a world axis, inclusively. The reference can be a fixture or one of
  its heads. Call with a current show snapshot before launching a cue."
  [model {:keys [strip-id reference-id axis] :or {axis :x} :as request}]
  (let [axis-index ({:x 0 :y 1 :z 2} axis)
        fixtures (get-in model [:rig :fixtures])
        by-id (into {} (map (juxt :id identity)) fixtures)
        heads (group-by :fixture-id (filter :fixture-id fixtures))
        strip (get heads (identify (keys heads) strip-id))
        reference-key (identify (into (set (keys by-id)) (keys heads)) reference-id)
        reference (get by-id reference-key)
        anchor (when-not reference
                 (some #(when (:fixture-bounds-m %) %) (get heads reference-key)))
        box (or (:bounds-m reference) (:fixture-bounds-m anchor))]
    (when-not axis-index (throw (ex-info "Strip selection axis must be :x, :y or :z" request)))
    (when-not strip (throw (ex-info "Unknown strip or strip without heads" request)))
    (when-not box (throw (ex-info "Reference fixture or head needs declared bounds" request)))
    ;; A saved anchor without a body pose uses its head-relative bounds.
    (let [pose (or reference (merge anchor (:fixture-bounds-pose anchor)))
          [lo hi] (bounds/world-span pose box axis-index)]
      {:fixture-ids (into [] (comp (filter #(<= lo (nth (or (:position %) [0 0 0]) axis-index) hi))
                                   (map :id)) strip)})))

(defn- groups
  "Patch group tags, the rig's declared fixture roles (#858) and controlled
  lighting groups, each as group ID -> fixture IDs."
  [model]
  (merge (reduce (fn [gs {:keys [id groups]}]
                   (reduce #(update %1 %2 (fnil conj []) id) gs groups))
                 {} (get-in model [:rig :fixtures]))
         (roles/groups (:rig model))
         (update-vals (get-in model [:extensions :tolglow.lighting-scenes/control :groups] {}) :fixture-ids)))

(defn resolve-ids
  "Resolve selection in patch order against the live membership snapshot. Empty
  volumes are valid selections; stale/unknown named selectors fail closed."
  ([model selector] (resolve-ids model selector (get-in model [:variables :spatial :members])))
  ([model selector members]
   (when-not (selector? selector) (throw (ex-info "Invalid fixture selection" {:selector selector})))
   (let [all (mapv :id (get-in model [:rig :fixtures]))]
     (if (= :all selector) all
         (let [selected (cond
                          (:fixture-ids selector)
                          (map #(or (identify all %) (throw (ex-info "Unknown fixture" {:id %}))) (:fixture-ids selector))
                          (:group-id selector)
                          (let [gs (groups model) k (identify (keys gs) (:group-id selector))]
                            (or (get gs k) (throw (ex-info "Unknown fixture group" {:selector selector}))))
                          :else (let [volumes (get-in model [:settings :spatial :volumes] members)
                                      k (volume-id volumes (:volume-id selector))]
                                  (if k (get members k #{}) (throw (ex-info "Unknown volume" {:selector selector})))))]
           (filterv (set selected) all))))))

(defn resolve-command
  ([model cmd] (resolve-command model cmd (get-in model [:variables :spatial :members])))
  ([model cmd members]
   (if (#{:cue/launch :effect/launch} (:op cmd))
     (let [launch (:launch cmd)
           cached (get-in model [:control :history (:command-id cmd) :command :launch])
           selector (or (:selector launch) (when-not (contains? launch :targets)
                                              (or (:selector cached) (get-in model [:registry :cues (:cue-id cmd) :selector]))))]
       (if (nil? selector) cmd
           (do
             (when (and (contains? launch :targets) (:selector launch))
               (throw (ex-info "Supply either a selector or resolved targets at launch" {:launch launch})))
             (update cmd :launch assoc :selector selector :targets
                     (if (= selector (:selector cached)) (:targets cached) (resolve-ids model selector members))))))
     cmd)))

(defn remember [result command]
  (if (and (#{:cue/launch :effect/launch} (:op command))
           (seq (:events result)) (= :accepted (get-in result [:ack :status]))
           (not (#{:spatial :auto :autopilot} (get-in command [:source :adapter]))))
    (let [selector (get-in command [:launch :selector])
          target-keys (cond (:group-id selector) [[:group (:group-id selector)]]
                            (:volume-id selector) [[:volume (:volume-id selector)]]
                            :else (map #(vector :fixture %) (get-in command [:launch :targets])))
          entry (if (:cue-id command) [:cue (:cue-id command)] [:effect (:effect-id command)])]
      (update-in result (into [:model] usage-path)
                 (fn [uses] (reduce #(update-in %1 %2 (fnil inc 0)) (or uses {}) (conj (vec target-keys) entry)))))
    result))

(defn- output-attributes [effect]
  (into #{} (map :attribute) (concat (registry/output-entries (:outputs effect)) (get-in effect [:layer :transforms]))))

(defn- parameter-properties [e cue]
  (reduce-kv (fn [m k v]
               (assoc m k (assoc (select-keys v [:type :values :default :bounds :size])
                                  :initial (get (:saved-params cue) k (get (:params cue) k (:default v))))))
             {} (:params e)))

(defn- shape [effect cue]
  {:attributes (output-attributes effect) :categories (set (registry/definition-categories effect))
   :params (parameter-properties effect cue)})

(defn- precedes [registry kind {:keys [id version]}]
  (when version
    (or (get-in registry [:versions [kind id (dec version)]])
        (reduce-kv (fn [best [k i v] entry]
                     (if (and (= k kind) (= i id) (< v version) (> v (:version best 0))) entry best))
                   nil (:versions registry)))))

(defn analyze
  "Derive searchable writes and parameter properties from the registered revision.
  Diff against the last published revision (versions need not be consecutive)."
  [registry kind definition]
  (let [cue? (= kind :cue) before (precedes registry (if cue? :cues :effects) definition)
        props (fn [d] (when d (if cue? (shape (registry/cue-effect registry d) d) (shape d nil))))
        prior (props before) now (props definition)
        writes (fn [d] (let [e (if cue? (registry/cue-effect registry d) d)]
                         (group-by :attribute (concat (registry/output-entries (:outputs e))
                                                      (get-in e [:layer :transforms])))))
        from (writes before) to (writes definition)
        keys-changed (fn [k] (set/difference (get now k) (get prior k #{})))
        params (set/union (set (keys (:params now))) (set (keys (:params prior))))]
    (assoc now :changes {:from-version (:version before)
                         :added-attributes (keys-changed :attributes)
                         :changed-outputs (into #{} (filter #(not= (get from %) (get to %)))
                                                (set/union (set (keys from)) (set (keys to))))
                         :removed-attributes (set/difference (get prior :attributes #{}) (:attributes now))
                         :changed-params (into #{} (filter #(not= (get-in now [:params %]) (get-in prior [:params %]))) params)})))

(defn- safe-look? [definition]
  (let [control? #{:code :code-cue :show-control}]
    (and definition (not (:show-control? definition)) (not (:code-cue? definition))
         (not (some control? (concat [(:kind definition) (:role definition)] (:tags definition) (:categories definition)))))))

(defn- catalog [model]
  (let [rig (get-in model [:rig :fixtures]) by-id (into {} (map (juxt :id identity)) rig)
        registry (:registry model)
        volumes (merge (get-in model [:variables :spatial :members]) (get-in model [:settings :spatial :volumes]))]
    (concat
     (for [{:keys [id name channels]} rig] {:kind :fixture :id id :label (or name (label id))
                                             :selector {:fixture-ids [id]} :attributes (set (keys channels))})
     (for [[id members] (groups model)]
       {:kind :group :id id :label (label id) :selector {:group-id id} :member-count (count members)
        :attributes (into #{} (mapcat #(keys (:channels (by-id %)))) members)})
     (for [id (keys volumes)] {:kind :volume :id id :label (label id) :selector {:volume-id id}})
     (for [[kind entries] [[:cue (:cues registry)] [:effect (:effects registry)]]
           [id d] entries
           :let [e (if (= :cue kind) (registry/cue-effect registry d) d)]]
       (merge {:kind kind :id id :label (or (:name d) (label id)) :version (:version d)
               :lighting? (boolean (and (safe-look? e) (safe-look? d) (not (:spatial-preset e))))}
              (analyze registry kind d)))
     (for [[kind entries] [[:page (:pages model)] [:macro (:macros model)] [:sequence (:sequences model)]
                           [:waveform (get-in model [:waveforms :waveforms])] [:fixture-type (:fixtures registry)]]
           [id d] entries :when (id? id)]
       {:kind kind :id id :label (or (:name d) (label id))}))))

(defn- param-match? [schema q]
  (and schema
       (if (map? q)
         (every? (fn [[k v]] (if (= k :allows) (registry/valid-value? schema v) (= v (get schema k)))) q)
         (registry/valid-value? schema q))))

(defn search
  "In-process, deterministic ranked picker. :params {:shape {:allows :triangle},
  :level {:type :number :initial 0.7}} and :attributes/:categories are conjunctive."
  ([model] (search model {}))
  ([model {:keys [text kinds attributes categories params lighting-only? limit] :or {limit 100}}]
   (let [query (str/lower-case (str/trim (or text "")))
         terms (remove str/blank? (str/split query #"\s+"))
         usage (get-in model usage-path)]
     (->> (catalog model)
          (keep (fn [{:keys [id label kind] :as row}]
                  (let [name (str/lower-case (str label)) id-text (str/lower-case (tolglow.spatial.targets/label id))
                        properties (str/lower-case (pr-str (select-keys row [:categories :attributes :params])))
                        matches? (every? #(or (str/includes? name %) (str/includes? id-text %)
                                             (str/includes? properties %)) terms)]
                    (when (and matches? (or (empty? kinds) ((set kinds) kind))
                               (or (not lighting-only?) (:lighting? row))
                               (every? (set (:attributes row)) attributes)
                               (every? (set (:categories row)) categories)
                               (every? (fn [[k q]] (param-match? (get-in row [:params k]) q)) params))
                      (let [uses (get-in usage [kind id] 0)
                            score (+ (cond (empty? query) 0 (= query id-text) 100 (= query name) 95
                                           (str/starts-with? name query) 55 (str/includes? name query) 30 :else 10)
                                     (if (= :group kind) 12 0) (min 30 (* 4 (Math/log1p (double uses)))))]
                        (assoc row :uses uses :score score))))))
          (sort-by (juxt (comp - :score) :label (comp str :kind) (comp pr-str :id)))
          (take (max 0 (min 1000 limit))) vec))))
