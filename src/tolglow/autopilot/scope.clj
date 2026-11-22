(ns tolglow.autopilot.scope
  "Resolve energy-control scopes against the current show's actual patch."
  (:require [tolglow.lighting-scenes :as scenes]
            [tolglow.registry :as registry]))

(defn page-cue-ids [model page-id]
  (when-not (contains? (:pages model) page-id)
    (throw (ex-info "Unknown autopilot cue page" {:page-id page-id})))
  (let [page (get-in model [:pages page-id])]
    (into [] (distinct) (if (map? page) (or (:cue-ids page) (vals (:cells page))) page))))

(defn- capable? [fixture attr]
  (let [channels (:channels fixture)]
    (case attr
      :color (some #(contains? channels %) registry/emitter-attributes)
      (:aim :direction) (or (contains? channels :pan) (contains? channels :tilt))
      :dimmer true ; a virtual master is available on emitter-only fixtures
      (contains? channels attr))))

(defn- output-scopes [model ids]
  (let [registry (:registry model)]
    (for [id ids
          :let [cue (get-in registry [:cues id])
                effect (registry/cue-effect registry cue)]
          :when (and effect (not (:show-control? cue)) (not (:show-control? effect))
                     (not= :show-control (:kind cue)) (not= :show-control (:kind effect)))
          {:keys [attribute targets]} (registry/output-entries (:outputs effect))]
      {:attribute attribute :effect-targets (some-> (:targets effect) set)
       :output-targets (some-> targets set)})))

(defn fixture-ids
  "Ordered visible emitter IDs for the rig, a group, selected IDs or a cue page.
  Page cues use the effect revision the cue would launch. A target restriction
  on an output intersects the enclosing effect's targets."
  [model scope]
  (let [rig (get-in model [:rig :fixtures])
        global (scenes/eligible-ids rig)
        eligible (set global)]
    (cond
      (= :global scope) global
      (not (and (map? scope) (= 1 (count scope))))
      (throw (ex-info "Select one autopilot scope" {:scope scope}))
      (contains? scope :fixture-ids)
      (let [ids (:fixture-ids scope) known (into #{} (map :id) rig)]
        (when-not (and (sequential? ids) (every? known ids))
          (throw (ex-info "Unknown autopilot fixture" {:scope scope})))
        (filterv (set ids) global))
      (contains? scope :group-id)
      (let [id (:group-id scope)
            scene (get (scenes/groups model) id)
            patched (filter #(some #{id} (:groups %)) rig)]
        (when-not (or scene (seq patched)) (throw (ex-info "Unknown autopilot group" {:group-id id})))
        (filterv (set (or (:fixture-ids scene) (map :id patched))) global))
      (contains? scope :page-id)
      (let [outputs (output-scopes model (page-cue-ids model (:page-id scope)))]
        (into [] (comp (filter #(eligible (:id %)))
                       (filter (fn [{:keys [id] :as fixture}]
                                 (some (fn [{:keys [attribute effect-targets output-targets]}]
                                         (and (or (nil? effect-targets) (effect-targets id))
                                              (or (nil? output-targets) (output-targets id))
                                              (capable? fixture attribute))) outputs)))
                       (map :id)) rig))
      :else (throw (ex-info "Unknown autopilot scope" {:scope scope})))))
