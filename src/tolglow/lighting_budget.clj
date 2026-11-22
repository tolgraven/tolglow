(ns tolglow.lighting-budget
  "Concurrent emitter budgets for rig output, fixture groups and cue selection."
  (:require [afterglow.render.emission :as emission]
            [afterglow.render.frame :as render]
            [tolglow.automation.prediction :as prediction]
            [tolglow.lighting-budget.allocation :as allocation]
            [tolglow.lighting-capacity :as capacity]))

(def defaults allocation/defaults)
(def settings allocation/settings)

(defn enabled? [model] (true? (get-in model [:settings :lighting-budget :enabled?])))

(defn- context [model patch meter-plan]
  (let [config (get-in model [:settings :lighting-budget])
        fixtures (:fixtures patch)
        shares (into {} (map (juxt :id #(:emission-share % 1.0))) fixtures)
        capacities (update-vals (:source-capacities meter-plan) #(select-keys % [:rated-watts]))
        info (into {} (map (fn [[id {:keys [rated-watts] :as row}]]
                             [id (assoc row :watts (or rated-watts
                                                    (* (:unrated-watts config 1.0) (shares id))))])) capacities)]
    {:patch patch :meter meter-plan :source-info info
     :scopes (allocation/scopes config fixtures
                                (get-in model [:extensions :tolglow.lighting-scenes/control :groups]) info)}))

(defn- read-sources [{:keys [meter source-info]} frame]
  (let [measured (capacity/measure-sources meter (:universes frame) (keys source-info))]
    (merge-with merge source-info measured)))

(defn apply-frame
  "Apply caps after ordinary effects, response and master. Inactive sources stay
  dark. The reported watts come from the resulting packed emitter/dimmer bytes."
  [model patch meter-plan frame]
  (if-not (enabled? model) frame
    (let [ctx (context model patch meter-plan)
          rows (read-sources ctx frame)
          allocated (allocation/allocate (:scopes ctx) rows)
          gains (into {} (keep (fn [[id drive]]
                                 (let [request (get-in rows [id :emitter-drive])]
                                   (when (< drive request) [id (/ drive request)])))) allocated)
          adjusted (if (seq gains) (emission/scale-emission patch frame gains) frame)
          actual (if (seq gains) (read-sources ctx adjusted) rows)]
      (assoc adjusted :lighting-budget (allocation/report (:scopes ctx) rows actual)))))

(defn planning-context
  "Prepare the meter and fixed budget scopes once for an entire selection."
  [model patch]
  (when (enabled? model) (context model patch (capacity/prepare patch))))

(defn unmet-watts [ctx frame]
  (let [rows (read-sources ctx frame)]
    (allocation/shortfall rows (allocation/allocate (:scopes ctx) rows))))

(defn- unused-id [registry]
  (first (remove #(or (contains? (:effects registry) %) (contains? (:cues registry) %))
                 (map #(keyword "lighting-budget.prediction" (str %)) (range)))))

(defn rank-candidates
  "Prefer scoped looks which fit the current rig and group budgets. Sample each
  through the normal compiler at attack completion. Stable ties preserve the
  caller's variation order; the physical cap still applies as the effect evolves."
  [model heads choices]
  (if-not (enabled? model) choices
    (let [patch (render/compile-patch (get-in model [:rig :fixtures]))
          ctx (planning-context model patch)
          id (unused-id (:registry model))
          cue {:id id :effect-id id}
          model (assoc-in model [:registry :cues id] cue)
          targets (mapv :id heads)
          score (fn [{:keys [effect outputs params]}]
                  (let [target-ids (if-some [specified (:targets effect)] (filterv (set specified) targets) targets)
                        effect (assoc effect :id id :outputs outputs :targets target-ids)
                        shadow (assoc-in model [:registry :effects id] effect)]
                    (unmet-watts ctx (prediction/frame shadow patch [(assoc cue :params params)] (:time-ms model 0)))))]
      (mapv second (sort-by first (mapv (fn [choice] [(score choice) choice]) choices))))))
