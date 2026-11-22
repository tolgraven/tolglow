(ns tolglow.autopilot.candidates
  "Scope and rank declarative inputs before acting on a mixed performance."
  (:require [clojure.set :as set]
            [tolglow.effects.authoring :as authoring]
            [tolglow.lighting-activity :as activity]
            [tolglow.registry :as registry]))

(def ^:private control-tags #{:code :code-cue :show-control :system :control :utility :modulation :scene})

(defn- eligible? [entry]
  (and (map? entry) (not= false (:auto? entry))
       (not-any? entry [:code-cue? :show-control? :layer :modulation :scene-owner :performance-owner])
       (not-any? control-tags (concat [(:kind entry) (:role entry)] (:tags entry) (:categories entry)))))

(defn target-ids [fixture-ids effect]
  (let [scope (if (:targets effect) (set/intersection fixture-ids (set (:targets effect))) fixture-ids)]
    (into #{} (mapcat (fn [row]
                        (if-let [targets (:targets row)] (filter scope targets) scope)))
          (registry/output-entries (:outputs effect)))))

(defn- plausible-light? [{:keys [attribute value]}]
  (let [v (if (= :const (:op value)) (:value value) value)]
    (and (authoring/light-attributes attribute)
         (cond (number? v) (pos? v)
               (and (vector? v) (every? number? v)) (some pos? v)
               :else true))))

(defn- usable-output? [fixture {:keys [attribute targets]}]
  (let [channels (:channels fixture)]
    (and (or (nil? targets) (some #{(:id fixture)} targets))
         (case attribute
           :dimmer (or (:dimmer channels) (some channels registry/emitter-attributes))
           :color (every? channels [:red :green :blue])
           (:aim :direction) (or (:pan channels) (:tilt channels))
           (contains? channels attribute)))))

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
  [model state {:keys [fixture-ids cue-ids domain direction lit-fixture-ids]}]
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
                                 (let [rows (registry/output-entries (:outputs effect))
                                       ids (target-ids rig effect)
                                       overlap (into #{} (filter #(some (partial usable-output? (by-id %)) rows))
                                                     (set/intersection ids selected))
                                       emits? (some plausible-light? rows)
                                       companion? (and (= :activity domain) (seq (set/intersection lit overlap))
                                                       (some #(#{:aim :direction :pan :tilt :shutter :strobe}
                                                               (:attribute %)) rows))
                                       origin (or (:id cue) (:id effect))
                                       supported? (every? #(authoring/performance-attributes (:attribute %)) rows)]
                                   (when (and supported? (seq overlap) (or emits? companion?))
                                     {:cue cue :effect effect :origin origin :ids (filterv overlap fixture-ids)
                                      :scoped? (not= ids overlap)
                                      :score (estimate model effect cue (and companion? (not emits?)))
                                      :running? (contains? active origin)}))))) entries)
        fresh (sort-by (comp str :origin) (remove :running? candidates))]
    (when (seq fresh)
      (let [start (mod (dec (:serial state)) (count fresh))
            rotated (concat (drop start fresh) (take start fresh))]
        (first (sort-by #(if (and (= domain :activity) (= direction :up))
                           (- (:score %)) (:score %)) rotated))))))

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
                         (or (nil? energy-control) (= domain (:domain energy-control))))
                (let [slope (if (:inverse? energy-control) -1.0 1.0)
                      wanted (+ value (* slope change (- hi lo)))
                      clamped (max lo (min hi wanted))
                      target (if (= type :integer)
                               (long (if (> wanted value) (Math/ceil clamped) (Math/floor clamped))) clamped)]
                  (when-not (== value target) {:param k :value target}))))) keys)))
