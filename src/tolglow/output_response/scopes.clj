(ns tolglow.output-response.scopes
  "Resolve output response scopes against the actual patch and composed writers."
  (:require [afterglow.render.emission :as emission]
            [afterglow.render.frame :as frame]
            [clojure.set :as set]
            [tolglow.lighting-activity :as activity]
            [tolglow.lighting-scenes :as scenes]))

(def scopes [:groups :fixtures :cues :compositions])

(defn- writer-members [model config targets]
  (let [runs (into {} (map (fn [[id {:keys [owner-id]}]] [owner-id id])) (:composition-runs model))]
    (reduce-kv
     (fn [members {:keys [cue-id instance-id]} heads]
       (let [owner (get-in model [:instances instance-id :source])
             run (when (= :composition (:adapter owner)) (runs (:session-id owner)))]
         (cond-> members
           (contains? (:cues config) cue-id) (update [:cues cue-id] set/union heads)
           (contains? (:compositions config) run) (update [:compositions run] set/union heads))))
     {} targets)))

(defn- release-tails [patch previous config active prior-frame frame]
  (when (and prior-frame (or (seq (:tails previous)) (not= active (:active previous))))
    (let [falling (delay (emission/falling-heads patch prior-frame frame))
          retained? (fn [id]
                      (when-let [i (get-in patch [:by-id id ::frame/index])]
                        (aget ^booleans @falling (int i))))]
      (reduce-kv
       (fn [tails key before]
         (if-not (:release (get-in config key)) tails
           (let [present (get active key #{}) departed (if (= before present) #{} (set/difference before present))
                 ongoing (into #{} (filter retained?) departed)]
             (cond-> tails (seq ongoing) (assoc key ongoing)))))
       {} (merge-with set/union (:active previous) (:tails previous))))))

(defn- patch-members [model config]
  (let [scenes (scenes/groups model)]
    (merge (into {} (map (fn [id] [[:groups id] (:fixture-ids (get scenes id))])) (keys (:groups config)))
           (into {} (map (fn [id] [[:fixtures id] #{id}])) (keys (:fixtures config))))))

(defn- invert [members]
  (reduce-kv (fn [index scope heads]
               (reduce #(update %1 %2 (fnil conj #{}) scope) index heads))
             {} members))

(defn- row-layout [patch config members tails]
  (let [active (invert members) releasing (invert tails)
        by-scopes (reduce-kv
                   (fn [rows i {:keys [id groups]}]
                     (let [both (into (get active id #{})
                                      (keep #(when (contains? (:groups config) %) [:groups %])) groups)
                           ids [both (get releasing id)]]
                       (update rows ids (fnil conj []) i)))
                   {} (:fixtures patch))]
    {:indices (update-vals by-scopes int-array) :count (count (:fixtures patch))}))

(defn resolve-scopes [model patch config blocks prior-frame frame previous]
  (when (some #(seq (get config %)) scopes)
    (let [by-writer? (or (seq (:cues config)) (seq (:compositions config)))
          writers (when by-writer? (activity/emission-writer-targets patch blocks (:writers previous)))
          active (when writers (writer-members model config (:targets writers)))
          tails (when writers (release-tails patch previous config active prior-frame frame))
          members (merge (patch-members model config) active)
          layout (if (and (identical? patch (:patch previous)) (= members (:members previous))
                          (= tails (:tails previous)))
                   (:layout previous) (row-layout patch config members tails))]
      {:patch patch :writers writers :active active :tails tails :members members :layout layout})))
