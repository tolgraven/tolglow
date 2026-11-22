(ns tolglow.lighting-budget
  "Concurrent emitter budgets for rig output, fixture groups and cue selection."
  (:require [afterglow.render.emission :as emission]
            [afterglow.render.frame :as render]
            [afterglow.render.photometry :as photometry]
            [tolglow.automation.prediction :as prediction]
            [tolglow.lighting-budget.allocation :as allocation]
            [tolglow.lighting-capacity :as capacity]))

(def defaults allocation/defaults)
(def settings allocation/settings)

(defn enabled? [model] (true? (get-in model [:settings :lighting-budget :enabled?])))

(defn- standby-assignments [fixture level]
  (let [{:keys [id channels]} fixture
        attrs (keys channels)
        rgb? (every? #(contains? channels %) [:red :green :blue])
        visible (filter #(pos? (get-in photometry/emitter-spectrum [% 0] 0)) attrs)
        ;; Standard channel packing rounds to nearest. Keep background below
        ;; the active-source threshold even on a coarse 8-bit dimmer.
        level (max 0.0 (- (double level) (/ 0.500000001 255.0)))
        make (fn [attribute value] {:target-id id :attribute attribute :value value :blend :replace})]
    (cond rgb? [(make :color [level level level])]
          (seq visible) (mapv #(make % level) visible)
          (and (:dimmer channels) (not-any? photometry/emitter-spectrum attrs)) [(make :dimmer level)]
          :else [])))

(defn- standby [patch scopes]
  (let [levels (reduce (fn [levels {:keys [ids rule]}]
                         (if (and (= :all (:standby-mode rule)) (pos? (:standby-level rule)))
                           (reduce #(update %1 %2 (fnil max 0.0) (:standby-level rule)) levels ids)
                           levels)) {} scopes)]
    (when (seq levels)
      {:kind :assignments
       :assignments (into [] (mapcat (fn [[id level]] (standby-assignments (get-in patch [:by-id id]) level))) levels)})))

(defn- same-group-members? [previous config groups]
  ;; Scene feedback may replace a group record on every frame; membership is
  ;; small, ordered and stable for the life of a configured scene group.
  (every? (fn [id] (= (get-in previous [:members id]) (get-in groups [id :fixture-ids])))
          (keys (:groups config))))

(defn prepare-frame-context
  "Reuse the immutable patch, rule and group compilation while a show runs."
  [previous model patch meter-plan]
  (let [config (get-in model [:settings :lighting-budget])
        groups (get-in model [:extensions :tolglow.lighting-scenes/control :groups])]
    (if (and (identical? patch (:patch previous)) (identical? meter-plan (:meter previous))
             (= config (:config previous)) (same-group-members? previous config groups))
      previous
      (let [indices (update-vals (:writers-by-head meter-plan) first)
            source-capacities (:source-capacities meter-plan)
            n (count indices)
            watts (double-array n)
            target-indices (int-array n)
            rated (into #{} (keep (fn [[id row]] (when (some? (:rated-watts row)) id))) source-capacities)]
        (doseq [[j {:keys [id emission-share]}] (map-indexed vector (:fixtures patch))
                :let [i (indices id)] :when (some? i)]
          (aset watts (int i) (double (or (get-in source-capacities [id :rated-watts])
                                          (* (:unrated-watts config 1.0) (or emission-share 1.0)))))
          (aset target-indices (int i) (int j)))
        (let [scopes (allocation/scopes config (:fixtures patch) groups indices watts rated)]
          {:config config :patch patch :meter meter-plan :watts watts :target-indices target-indices
           :members (into {} (map (fn [id] [id (get-in groups [id :fixture-ids])])) (keys (:groups config)))
           :scopes scopes :standby-block (standby patch scopes)})))))

(defn- target-gains [ctx ^doubles requested ^doubles granted]
  (when-not (java.util.Arrays/equals requested granted)
    (let [order (get-in ctx [:patch :target-order])
          ^ints targets (:target-indices ctx)]
      (persistent!
       (loop [i 0 out (transient {})]
         (if (= i (alength requested)) out
           (let [req (aget requested i) grant (aget granted i)]
             (recur (inc i) (if (< grant req) (assoc! out (nth order (aget targets i)) (/ grant req)) out)))))))))

(defn apply-frame
  "Apply caps after ordinary effects, response and master. The reported watts
  come from the resulting packed emitter/dimmer bytes. For :standby-mode :all,
  prepend the prepared :standby-block before resolving authored contributions."
  ([model patch meter-plan frame]
   (if (enabled? model) (apply-frame (prepare-frame-context nil model patch meter-plan) frame) frame))
  ([{:keys [scopes meter watts patch] :as ctx} frame]
   (let [requested (capacity/emitter-drives meter (:universes frame))
         granted (allocation/allocate scopes watts requested)
         gains (target-gains ctx requested granted)
         adjusted (if gains (emission/scale-emission patch frame gains) frame)
         actual (if gains (capacity/emitter-drives meter (:universes adjusted)) requested)]
     (assoc adjusted :lighting-budget (allocation/report scopes watts requested actual)))))

(defn planning-context
  "Prepare the meter and fixed budget scopes once for an entire selection."
  [model patch]
  (when (enabled? model) (prepare-frame-context nil model patch (capacity/prepare patch))))

(defn unmet-watts [ctx frame]
  (let [requested (capacity/emitter-drives (:meter ctx) (:universes frame))
        granted (allocation/allocate (:scopes ctx) (:watts ctx) requested)]
    (allocation/shortfall (:watts ctx) requested granted)))

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
                    (unmet-watts ctx (prediction/frame shadow patch [(assoc cue :params params)] (:time-ms model 0) (:standby-block ctx)))))]
      (mapv second (sort-by first (mapv (fn [choice] [(score choice) choice]) choices))))))
