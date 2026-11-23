(ns tolglow.visualization.cue-pads
  "Sample the visible cue grid on presentation workers. All previews are
  private scalar evaluations over a single representative head."
  (:require [afterglow.control.views :as views]
            [afterglow.control.system :as system]
            [afterglow.render.frame :as frame]
            [tolglow.compiler :as compiler]
            [tolglow.effects.variable-blocks :as variable-blocks]
            [tolglow.registry :as registry]
            [tolglow.waveforms :as waveforms]))

(def query-id :visualization/cue-pads)

(defn manager []
  {:lock (Object.) :cache (volatile! {}) :leases (volatile! {})
   :governor (atom {:level 0 :clients 0})})

(defn region? [[id page origin rows :as query]]
  (and (= 4 (count query)) (= query-id id) (or (keyword? page) (string? page))
       (vector? origin) (= 2 (count origin)) (every? #(and (nat-int? %) (< % 1000000)) origin)
       (pos-int? rows) (<= rows 8)))

(defn- update-governor! [service]
  (let [eligible (count (filter :plan (vals @(:cache service))))]
    (swap! (:governor service) #(if (zero? eligible) {:level 0 :clients 0}
                                  (assoc % :clients eligible)))))

(defn- update-demand! [service]
  (let [demand (into #{} (mapcat val) @(:leases service))]
    (vswap! (:cache service) select-keys demand)
    (update-governor! service)))

(defn release! [service lease]
  (locking (:lock service)
    (when (contains? @(:leases service) lease)
      (vswap! (:leases service) dissoc lease)
      (update-demand! service))))

(defn status [service] @(:governor service))

(defn sample-period-ms [service]
  (* 100 (bit-shift-left 1 (:level (status service)))))

(defn- elapsed-policy [{:keys [level clients hot cool] :as state} elapsed period]
  (let [hot (if (> elapsed (* 0.85 period)) (inc (or hot 0)) 0)
        cool (if (< elapsed (* 0.65 period)) (inc (or cool 0)) 0)
        next-level (cond (and (>= hot 2) (< level 4)) (inc level)
                         (and (>= cool 32) (pos? level)) (dec level)
                         :else level)]
    (if (zero? clients) state
      (assoc state :level next-level :defer-live? (and (pos? hot) (< level 4))
             :hot (if (= level next-level) hot 0) :cool (if (= level next-level) cool 0)))))

(defn govern!
  "Called once with actual frame cost. Degrade visual keyframes first and
  release the live throttle gate if pressure persists at minimum visual rate."
  [service elapsed period]
  (when (and service (pos? (:clients @(:governor service))))
    (:defer-live? (swap! (:governor service) elapsed-policy elapsed period))))

(defn- page-key [pages page]
  (cond (contains? pages page) page
        (string? page) (keyword page)
        :else (subs (str page) 1)))

(defn- region-ids [model [_ page [left bottom] rows]]
  (let [pages (:pages model) cells (get-in pages [(page-key pages page) :cells])]
    (into #{} (keep (fn [[x y]] (get cells [x y])))
          (for [x (range left (+ left 8)) y (range bottom (+ bottom rows))] [x y]))))

(defn- visible-instances [model ids]
  (reduce (fn [out inst] (if (ids (:cue-id inst)) (assoc out (:cue-id inst) inst) out))
          {} (views/active-instances model)))

(defn- fixture-for [model definition cue]
  (let [rig (get-in model [:rig :fixtures])
        explicitly (set (:targets definition))
        requested (get-in cue [:pad-preview :fixture-id])
        targets (into #{} (mapcat :targets) (filter #(or (= :dimmer (:attribute %))
                                                     (= :color (:attribute %))
                                                     (registry/emitter-attributes (:attribute %)))
                                                 (registry/output-entries (:outputs definition))))]
    (if (and (nil? requested) (empty? explicitly) (empty? targets))
      (first rig)
      (some (fn [{:keys [id] :as fixture}]
              (when (and (or (nil? requested) (= id requested))
                         (or (empty? explicitly) (explicitly id))
                         (or requested (empty? targets) (targets id))) fixture)) rig))))

(defn- definition-for [model cue instance]
  (or (:definition instance)
      (registry/cue-effect (:registry model) cue)))

(defn- build-plan [model cue definition]
  (when (and definition (not= false (:pad-preview cue)))
    (let [custom (:pad-preview cue)
          overrides (when (map? custom) (select-keys custom [:color :dimmer]))
          definition (if (seq overrides)
                       (-> definition (assoc :outputs overrides) (dissoc :blocks :envelope))
                       definition)
          attrs (map :attribute (registry/output-entries (:outputs definition)))]
      (when (some (into registry/emitter-attributes #{:color :dimmer}) attrs)
        (when-let [fixture (fixture-for model definition cue)]
          (let [rig [fixture]]
            {:rig rig :patch (frame/compile-patch rig)
             :dimmer-tint (when (and (some #{:dimmer} attrs)
                                     (not-any? (conj registry/emitter-attributes :color) attrs))
                            (or (views/swatch-rgb (:swatch cue)) [1.0 1.0 1.0]))
             :step (compiler/compile-effect definition nil nil rig)}))))))

(defn- interpolate [a b fraction]
  (mapv (fn [x y] (+ x (* fraction (- y x)))) a b))

(defn- virtual-instance [cue]
  {:id ::audition :cue-id (:id cue) :phase :running :started-at-beat 0 :started-at-ms 0
   :params (merge (:params cue) (:saved-params cue))})

(defn- sample [snapshot plan inst t beat]
  (let [model (:model snapshot)
        transport (assoc (:transport model) :beat beat)
        clock (system/private-clock inst {:time-ms t :transport transport})
        input {:time-ms (:time-ms clock) :fixtures (:rig plan) :variables (:variables model)
               :transport (merge transport clock)}
        context {:frame input :instance inst :state (:state inst {}) :assignments [] :intents []}
        rows (:assignments ((:step plan) context))
        preview-rows (cond-> rows (and (:dimmer-tint plan) (seq rows))
                       (conj {:attribute :color :value (:dimmer-tint plan) :blend :replace
                              :target-id (:id (first (:rig plan)))}))
        opacity (* (get-in inst [:system :opacity] 1.0) (system/gain inst clock))
        weighted (if (== opacity 1.0) preview-rows (mapv #(assoc % :opacity opacity) preview-rows))
        values (get-in (frame/resolve-frame (:patch plan) weighted)
                       [:values (:id (first (:rig plan)))])]
    (when (seq rows) (:emitted-color values))))

(defn- plan-for [cached model cue definition]
  (if (and (identical? definition (:definition cached)) (identical? cue (:cue cached))
           (identical? (:rig model) (:rig-model cached)))
    cached
    {:cue cue :definition definition :rig-model (:rig model)
     :plan (try (build-plan model cue definition) (catch Exception _ nil))}))

(defn- predict [snapshot plan instance t beat]
  (try (sample snapshot plan instance t beat)
       (catch Exception _ nil)))

(defn- window-valid? [window signature t beat rate]
  (and (= signature (:signature window))
       (<= (:at window) t)
       (< t (:until window))
       (< (abs (- beat (+ (:beat window) (* (- t (:at window)) rate)))) 0.1)))

(defn- next-window [snapshot plan cue instance previous signature t beat rate interval]
  (let [inst (if instance (assoc instance :params (waveforms/effective-params snapshot instance))
                 (virtual-instance cue))
        consecutive? (and (= signature (:signature previous)) (= t (:until previous)))
        from (if consecutive? (:to previous) (predict snapshot plan inst t beat))
        until (+ t interval)]
    {:signature signature :at t :until until :beat beat :from from
     :to (predict snapshot plan inst until (+ beat (* interval rate)))}))

(defn- advance-cue [cached snapshot cue-id instance level]
  (let [model (:model snapshot) cue (get-in model [:registry :cues cue-id])
        definition (when cue (definition-for model cue instance))
        cached (plan-for cached model cue definition)]
    (if-not (:plan cached) cached
      (let [{:keys [beat bpm playing?] :or {beat 0.0 bpm 120.0}} (:transport model)
            t (double (or (:last-frame-time-ms model) (:time-ms model) 0))
            rate (if playing? (/ bpm 60000.0) 0)
            previous (:window cached)
            signature [(:epoch model) (:id instance) (:phase instance) (get-in instance [:system :paused?])
                       (:started-at-ms instance) (:release-at-ms instance) bpm playing?]]
        (if (window-valid? previous signature t beat rate) cached
          (let [interval (* (bit-shift-left 1 level) (max 60.0 (min 250.0 (/ 15000.0 bpm))))]
            (assoc cached :window (next-window snapshot (:plan cached) cue instance previous
                                               signature t beat rate interval))))))))

(defn- color-at [entry time]
  (let [{:keys [from to at until]} (:window entry)]
    (when (and from to)
      (interpolate from to (min 1.0 (max 0.0 (/ (- time at) (- until at))))))))

(defn present!
  "Return normalized RGB values for one mounted page rectangle. Identical cues
  share keyframes across web and pad consumers. A different rectangle replaces
  that lease's previous demand. Call release! when the surface unmounts."
  [service lease snapshot query]
  (when-not (region? query) (throw (ex-info "Expected a cue page, integer origin and 1–8 rows" {})))
  (locking (:lock service)
    (let [model (:model snapshot)
          ids (region-ids model query)
          current @(:leases service)]
      (when-not (= ids (get current lease))
        (let [new (assoc current lease ids)]
          (when (or (> (count new) 64) (> (count (into #{} (mapcat val) new)) 512))
            (throw (ex-info "Visible cue preview limit reached" {})))
          (vreset! (:leases service) new)
          (update-demand! service)))
      (let [model (variable-blocks/resolved model (:variable-observation snapshot))
            snapshot (assoc snapshot :model model)
            active (visible-instances model ids) level (:level (status service))
            time (or (:last-frame-time-ms model) (:time-ms model) 0)]
        (let [old @(:cache service)
              entries (reduce (fn [entries id]
                                (assoc entries id (advance-cue (get old id) snapshot id (get active id) level)))
                              old ids)]
          (vreset! (:cache service) entries)
          (when (some #(not= (boolean (:plan (get old %))) (boolean (:plan (get entries %)))) ids)
            (update-governor! service))
          (into {} (keep (fn [id] (when-let [color (color-at (get entries id) time)] [id color]))) ids))))))

(defn push-feedback [service snapshot!]
  {:open! #(Object.)
   :project!
   (fn [lease local model]
     (let [touch (get (:touches local) (peek (:touch-order local)))
           type (get-in model [:instances (:instance-id touch) :definition :params (:param touch) :type])
           snapshot (snapshot!)]
       (if (and (:ready? local) (not (:target-picker local)) (not (#{:color :enum} type))
                (= [(:show-id model) (:epoch model)] ((juxt :show-id :epoch) (:model snapshot))))
         (assoc model ::views/pad-colors
                (present! service lease (assoc snapshot :model model) [query-id (:page-id local) (:origin local) 8]))
         (do (release! service lease) model))))
   :close! #(release! service %)})
