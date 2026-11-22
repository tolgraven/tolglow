(ns tolglow.autopilot.candidates
  "Scope and rank declarative inputs before acting on a mixed performance."
  (:require [afterglow.render.photometry :as photometry]
            [clojure.set :as set]
            [tolglow.effects.authoring :as authoring]
            [tolglow.lighting-activity :as activity]
            [tolglow.registry :as registry]))

(def ^:private control-tags #{:code :code-cue :show-control :system :control :utility :modulation :scene})

(defn- eligible? [entry]
  (and (map? entry) (not= false (:auto? entry))
       (not-any? entry [:code-cue? :show-control? :layer :modulation :scene-owner :performance-owner])
       (not-any? control-tags (concat [(:kind entry) (:role entry)] (:tags entry) (:categories entry)))))

(defn target-ids [fixture-ids effect]
  (let [scope (if (:targets effect) (set/intersection fixture-ids (set (:targets effect))) fixture-ids)
        outputs (registry/output-entries (:outputs effect))]
    (if (some #(nil? (:targets %)) outputs) scope
      (set/intersection scope (into #{} (mapcat :targets) outputs)))))

(defn- plausible-light? [{:keys [attribute value]}]
  (let [v (if (= :const (:op value)) (:value value) value)]
    (and (authoring/light-attributes attribute)
         (cond (number? v) (pos? v)
               (and (vector? v) (every? number? v)) (some pos? v)
               :else true))))

(defn- usable-output? [fixture {:keys [attribute targets]}]
  (let [channels (:channels fixture)]
    (and (or (nil? targets) (contains? targets (:id fixture)))
         (case attribute
           :dimmer (or (:dimmer channels) (some channels registry/emitter-attributes))
           :color (every? channels [:red :green :blue])
           (:aim :direction) (or (:pan channels) (:tilt channels))
           (contains? channels attribute)))))

(defn- variable-motion? [{:keys [attribute value]}]
  (or (and (#{:strobe :shutter} attribute) (if (number? value) (pos? value) true))
      (some #(and (map? %) (#{:time :beat :bar :phrase :tween} (:op %)))
            (tree-seq coll? seq value))))

(defn- estimate-value [params v]
  (cond (number? v) (max 0.0 (min 1.0 (double v)))
        (and (vector? v) (every? number? v)) v
        (= :const (:op v)) (estimate-value params (:value v))
        (= :param (:op v)) (estimate-value params (get params (:key v) 0.5))
        :else 0.5))

(defn- estimated-white [channels fields lit?]
  (let [dimmer (get fields :dimmer 1.0)
        emitters (filter registry/emitter-attributes (keys channels))
        color (:color fields)
        rgb (when color (zipmap [:red :green :blue] (if (vector? color) color (repeat 3 color))))
        has-color? (or rgb (some fields emitters))
        response (fn [attr] (first (photometry/emitter-spectrum attr)))
        weights (reduce + 0.0 (map response emitters))]
    (* dimmer (cond
                (empty? emitters) (if (:dimmer fields) 1.0 0.0)
                (zero? weights) 0.0
                (and (not has-color?) lit?) 1.0
                :else (/ (reduce + 0.0 (map #(* (response %) (get fields % (get rgb % 0.0))) emitters))
                         weights)))))

(defn- brightness-estimate
  "Estimate the share of selected sources illuminated by a launch. An unknown
  time-varying expression is estimated at half drive pending output feedback.
  A dimmer command on a black RGB source cannot turn on its emitters."
  [by-id selected overlap cue effect rows lit]
  (let [params (merge (update-vals (:params effect) :default) (:params cue) (:saved-params cue))
        rows (mapv #(update % :value (partial estimate-value params)) rows)
        fields (into {} (map (juxt :attribute :value)) rows)
        total (if (every? #(nil? (:targets %)) rows)
                ;; Thousands of strip pixels share one channel layout and the
                ;; same authored values. Only lit/unlit backdrops can differ.
                (let [counts (frequencies (map (fn [id] [(:channels (by-id id)) (contains? lit id)]) overlap))]
                  (reduce-kv (fn [sum [channels lit?] n] (+ sum (* n (estimated-white channels fields lit?))))
                             0.0 counts))
                (reduce (fn [sum id]
                          (let [per-head (into {} (comp (filter #(or (nil? (:targets %)) ((:targets %) id)))
                                                       (map (juxt :attribute :value))) rows)]
                            (+ sum (estimated-white (:channels (by-id id)) per-head (contains? lit id)))))
                        0.0 overlap))]
    (/ total (max 1.0 (count selected)))))

(defn- estimate [model effect cue companion?]
  ;; Supply visible backdrop for a shutter/movement-only cue. The measured
  ;; score, where present, already includes that revision's actual backdrop.
  (let [model (if companion?
                (assoc-in model [:registry :effects (:id effect) :outputs]
                          (conj (registry/output-entries (:outputs effect)) {:attribute :dimmer :value 1.0}))
                model)]
    (or (activity/predicted-score model (or cue {:effect-id (:id effect)})) 0.0)))

(defn choose
  "Rank scoped cues and bare effects by their recorded or estimated activity.
  Explicit cue/page scope excludes unattached effects. A launch touching other
  groups can be copied as a transient with strictly scoped output targets."
  [model state {:keys [fixture-ids cue-ids domain direction amount lit-fixture-ids]}]
  (let [selected (set fixture-ids)
        fixtures (get-in model [:rig :fixtures])
        by-id (into {} (map (juxt :id identity)) fixtures)
        rig (set (keys by-id))
        lit (set lit-fixture-ids)
        reg (:registry model)
        all-cues (vals (:cues reg))
        linked (into #{} (map :effect-id) all-cues)
        cues (cond->> all-cues cue-ids (filter (comp (set cue-ids) :id)))
        entries (concat (for [cue cues :when (eligible? cue)] [cue (registry/cue-effect reg cue)])
                        (when (nil? cue-ids)
                          (for [[id effect] (:effects reg) :when (not (linked id))] [nil effect])))
        active (into #{} (map #(or (:origin %) (:cue-id %) (:effect-id %))) (vals (:owned state)))
        candidates (into [] (keep
                             (fn [[cue effect]]
                               (when (eligible? effect)
                                 (let [rows (mapv #(cond-> % (:targets %) (update :targets set))
                                                  (registry/output-entries (:outputs effect)))
                                       ids (target-ids rig effect)
                                       overlap (into #{} (filter #(some (partial usable-output? (by-id %)) rows))
                                                     (set/intersection ids selected))
                                       emits? (some plausible-light? rows)
                                       companion? (and (= :activity domain) (seq (set/intersection lit overlap))
                                                       (some #(#{:aim :direction :pan :tilt :shutter :strobe}
                                                               (:attribute %)) rows))
                                       origin (or (:id cue) (:id effect))
                                       supported? (every? #(authoring/performance-attributes (:attribute %)) rows)]
                                   (when (and supported? (seq overlap) (or emits? companion?)
                                              (or (not= domain :activity) (some variable-motion? rows)))
                                     (let [brightness (brightness-estimate by-id selected overlap cue effect rows lit)]
                                       (when (or (pos? brightness) companion?)
                                         {:cue cue :effect effect :origin origin :ids (filterv overlap fixture-ids)
                                          :scoped? (not= ids overlap) :brightness brightness
                                          :score (if (= domain :activity)
                                                   (estimate model effect cue (and companion? (not emits?))) brightness)
                                          :running? (contains? active origin)}))))))) entries)
        fresh (sort-by (comp str :origin) (remove :running? candidates))]
    (when (seq fresh)
      (let [start (mod (dec (:serial state)) (count fresh))
            rotated (concat (drop start fresh) (take start fresh))]
        (first (sort-by #(abs (- (double amount) (double (:score %)))) rotated))))))

(def ^:private brightness-keys [:level :brightness :intensity :dimmer :gain :amount])
(def ^:private activity-keys [:speed :rate :frequency :strobe-rate :flash-rate :pulse-rate :duty-cycle])

(defn adjustment
  "Prefer explicitly annotated parameters. For ordinary cues, recognize
  common dimmer/gain and movement/rate names and honor their numeric bounds.
  Mark inverse controls (e.g. period) with :energy-control {:domain :activity
  :inverse? true} in the parameter schema."
  [instance {:keys [domain amount direction]}]
  (let [schemas (get-in instance [:definition :params])
        keys (distinct (concat (sort-by str (keep (fn [[k spec]]
                                              (when (= domain (get-in spec [:energy-control :domain])) k)) schemas))
                               (if (= domain :brightness) brightness-keys activity-keys)))
        change (if (= direction :down) (- amount) amount)]
    (some (fn [k]
            (let [{:keys [type bounds recommended energy-control]} (schemas k)
                  value (get-in instance [:params k])
                  [lo hi] (or bounds recommended)]
              (when (and (#{:number :integer} type) (number? value) (number? lo) (number? hi) (> hi lo)
                         (if energy-control (= domain (:domain energy-control))
                           (some (fn [row]
                                   (and (if (= :activity domain) (variable-motion? row)
                                           (authoring/light-attributes (:attribute row)))
                                        (some #(and (map? %) (= :param (:op %)) (= k (:key %)))
                                              (tree-seq coll? seq (:value row)))))
                                 (registry/output-entries (get-in instance [:definition :outputs])))))
                (let [slope (if (:inverse? energy-control) -1.0 1.0)
                      wanted (+ value (* slope change (- hi lo)))
                      clamped (max lo (min hi wanted))
                      target (if (= type :integer)
                               (long (if (> wanted value) (Math/ceil clamped) (Math/floor clamped))) clamped)]
                  (when-not (== value target) {:param k :value target}))))) keys)))
