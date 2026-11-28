(ns tolglow.visualization.cue-pads
  "Sample the visible cue grid on presentation workers. All previews are
  private scalar evaluations over a single representative head."
  (:require [afterglow.control.pages :as pages]
            [afterglow.control.views :as views]
            [afterglow.time :as time]
            [afterglow.control.system :as system]
            [afterglow.render.frame :as frame]
            [tolglow.compiler :as compiler]
            [tolglow.effects.variable-blocks :as variable-blocks]
            [tolglow.registry :as registry]
            [tolglow.lighting-match :as matching]
            [tolglow.spatial.targets :as targets]
            [tolglow.waveforms :as waveforms]))

(def query-id :visualization/cue-pads)

(def default-samples-per-frame
  "Predictions all readers together may spend on one committed frame."
  32)

(defn manager
  "A shared pad preview service. :samples-per-frame bounds the effect
  predictions that every reader of one committed frame spends together."
  ([] (manager {}))
  ([{:keys [samples-per-frame] :or {samples-per-frame default-samples-per-frame}}]
   {:lock (Object.) :cache (volatile! {}) :leases (volatile! {}) :regions (volatile! {})
    :samples-per-frame samples-per-frame
    :frame (volatile! {:key nil :spent 0 :deferred 0 :dropped 0})
    :totals (volatile! {:predictions 0 :deferred 0 :dropped 0})
    :governor (atom {:level 0 :clients 0})}))

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
    (vswap! (:regions service) dissoc lease)
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

(defn- lease-region
  "The cue IDs a lease's rectangle shows, recomputed only when its query or
  the show's pages change."
  [service lease model query]
  (let [pages (:pages model) cached (get @(:regions service) lease)]
    (if (and cached (identical? pages (:pages cached)) (= query (:query cached)))
      (:ids cached)
      (let [ids (region-ids model query)]
        (vswap! (:regions service) assoc lease {:pages pages :query query :ids ids})
        ids))))

(defn- visible-instances [model ids]
  (reduce (fn [out inst] (if (ids (:cue-id inst)) (assoc out (:cue-id inst) inst) out))
          {} (views/active-instances model)))

(defn- fixture-for [model definition cue selected]
  (let [rig (get-in model [:rig :fixtures])
        explicitly (set (:targets definition))
        requested (get-in cue [:pad-preview :fixture-id])
        targets (into #{} (mapcat :targets) (filter #(or (= :dimmer (:attribute %))
                                                     (= :color (:attribute %))
                                                     (registry/emitter-attributes (:attribute %)))
                                                 (registry/output-entries (:outputs definition))))]
    (if (and (nil? selected) (nil? requested) (empty? explicitly) (empty? targets))
      (first rig)
      (some (fn [{:keys [id] :as fixture}]
              (when (and (or (nil? selected) (selected id)) (or (nil? requested) (= id requested))
                         (or (empty? explicitly) (explicitly id))
                         (or requested (empty? targets) (targets id))) fixture)) rig))))

(defn- definition-for [model cue instance]
  (or (:definition instance)
      (registry/cue-effect (:registry model) cue)))

(defn- build-plan [model cue definition selected]
  (when (and definition (not= false (:pad-preview cue)))
    (let [custom (:pad-preview cue)
          overrides (when (map? custom) (select-keys custom [:color :dimmer]))
          definition (if (seq overrides)
                       (-> definition (assoc :outputs overrides) (dissoc :blocks :envelope))
                       definition)
          attrs (map :attribute (registry/output-entries (:outputs definition)))]
      (when (some (into registry/emitter-attributes #{:color :dimmer}) attrs)
        (when-let [fixture (fixture-for model definition cue selected)]
          (let [rig [fixture]
                match-plan (when (matching/active? definition)
                             (matching/prepare (get-in model [:rig :fixtures]) #{(:id fixture)}))]
            {:rig rig :patch (frame/compile-patch rig)
             :dimmer-tint (when (and (every? (:channels fixture) [:red :green :blue])
                                     (some #{:dimmer} attrs)
                                     (not-any? (conj registry/emitter-attributes :color) attrs))
                            (or (views/swatch-rgb (:swatch cue)) [1.0 1.0 1.0]))
             :step (compiler/compile-effect definition nil match-plan rig)}))))))

(defn- interpolate [a b fraction]
  (cond (or (identical? a b) (<= fraction 0.0)) a
        (>= fraction 1.0) b
        (and (== 3 (count a)) (== 3 (count b)))
        (let [f (double fraction)
              at (fn [i] (let [x (double (nth a i))] (+ x (* f (- (double (nth b i)) x)))))]
          [(at 0) (at 1) (at 2)])
        :else (mapv (fn [x y] (+ x (* fraction (- y x)))) a b)))

(defn- virtual-instance [cue meter]
  {:id ::audition :cue-id (:id cue) :phase :running :started-at-beat 0 :started-at-ms 0
   :timing-meter (time/normalize-meter meter) :params (merge (:params cue) (:saved-params cue))})

(defn- sample [snapshot plan inst t beat]
  (let [model (:model snapshot)
        transport (assoc (:transport model) :beat beat)
        transport-clock (time/frame-clock {:time-ms t :transport transport})
        clock (system/private-clock inst transport-clock)
        input {:hdr? (boolean (:preview-hdr snapshot)) :time-ms (:time-ms clock) :fixtures (:rig plan) :variables (:variables model)
               :transport (merge transport clock) :transport-clock transport-clock}
        context {:frame input :instance inst :state (:state inst {}) :assignments [] :intents []}
        rows (:assignments ((:step plan) context))
        preview-rows (cond-> rows (and (:dimmer-tint plan) (seq rows))
                       (conj {:attribute :color :value (:dimmer-tint plan) :blend :replace
                              :target-id (:id (first (:rig plan)))}))
        opacity (* (get-in inst [:system :opacity] 1.0) (system/gain inst clock))
        weighted (if (== opacity 1.0) preview-rows (mapv #(assoc % :opacity opacity) preview-rows))
        gain (get-in inst [:system :gain] 1.0)
        block (cond-> {:kind :assignments :assignments weighted :source {:instance-id (:id inst) :cue-id (:cue-id inst)}}
                (not (== gain 1.0)) (assoc :emission-gain gain))
        options (when-let [hdr (:preview-hdr snapshot)] {:hdr hdr})
        values (get-in (frame/resolve-contributions (:patch plan) [block] options)
                       [:values (:id (first (:rig plan)))])]
    (when (seq rows) (:emitted-color values))))

(def ^:private fault-message-limit 240)

(defn- fault [stage ^Throwable error]
  (let [message (or (.getMessage error) (.getName (class error)))]
    {:stage stage
     :message (if (> (count message) fault-message-limit) (subs message 0 fault-message-limit) message)}))

(defn- plan-for [cached model cue definition instance]
  (let [launch-targets (:targets instance)
        selector (when-not launch-targets (:selector cue))
        ;; Most cues have neither, so they share one scope and compare it cheaply.
        scope (when (or launch-targets selector)
                (let [groups (when (:group-id selector) (get-in model [:extensions :tolglow.lighting-scenes/control :groups]))
                      members (when (:volume-id selector) (get-in model [:variables :spatial :members]))
                      volume (when-let [id (:volume-id selector)]
                               (targets/volume-id (get-in model [:settings :spatial :volumes] members) id))]
                  [launch-targets groups volume (get members volume)]))]
    (if (and cached (identical? definition (:definition cached)) (identical? cue (:cue cached))
             (identical? (:rig model) (:rig-model cached)) (= scope (:scope cached)))
      cached
      (let [built (try
                    (let [selected (cond launch-targets (set launch-targets)
                                         selector (set (targets/resolve-ids model selector)))]
                      {:plan (build-plan model cue definition selected)})
                    (catch Exception error {:fault (fault :plan error)}))]
        (merge {:cue cue :definition definition :rig-model (:rig model) :scope scope} built)))))

(defn- predict!
  "Run one prediction and count it against the frame's budget."
  [service snapshot plan instance t beat]
  (vswap! (:frame service) update :spent inc)
  (vswap! (:totals service) update :predictions inc)
  (sample snapshot plan instance t beat))

(defn- continuous? [window t beat rate]
  (< (abs (- beat (+ (:beat window) (* (- t (:at window)) rate)))) 0.1))

(defn- window-valid? [window signature t beat rate]
  (and (= signature (:signature window))
       (<= (:at window) t)
       (< t (:until window))
       (continuous? window t beat rate)))

(defn- chainable?
  "Whether a window that has just expired on unchanged inputs can hand its
  last keyframe to the next one."
  [window signature t beat rate interval]
  (and window (= signature (:signature window)) (some? (:to window)) (nil? (:fault window))
       (<= (:until window) t) (< t (+ (:until window) interval))
       (continuous? window t beat rate)))

(defn- next-window
  "The next keyframe window. A chained window starts where the expired one
  ended and reuses its last prediction, so it costs one prediction; any other
  starts now and costs two. A failure is kept as a fault window that retries
  once the interval passes."
  [service snapshot plan cue instance previous signature t beat rate interval]
  (let [chain? (chainable? previous signature t beat rate interval)
        at (if chain? (:until previous) t)
        at-beat (if chain? (+ (:beat previous) (* (- at (:at previous)) rate)) beat)
        until (+ at interval)
        base {:signature signature :at at :until until :beat at-beat}]
    (try
      (let [inst (if instance
                   (assoc instance :params (try (waveforms/effective-params snapshot instance)
                                                (catch Exception error
                                                  (throw (ex-info (.getMessage error) {::stage :parameters} error)))))
                   (virtual-instance cue (get-in snapshot [:model :transport :meter])))
            from (if chain? (:to previous) (predict! service snapshot plan inst at at-beat))]
        (assoc base :from from :to (predict! service snapshot plan inst until (+ at-beat (* interval rate)))))
      (catch Exception error
        (let [stage (or (::stage (ex-data error)) :sample)]
          (assoc base :fault (fault stage (if (= :parameters stage) (or (.getCause error) error) error))))))))

(defn- cue-demand
  "Where one visible cue stands on this frame: :current when its keyframes
  still cover t, :expired when they just ran out on unchanged inputs (its
  last colour stays meaningful while it waits), :stale when it has none or
  its inputs changed, and :static when it has no preview plan."
  [cached snapshot cue-id instance level global]
  (let [model (:model snapshot) cue (get-in model [:registry :cues cue-id])
        definition (when cue (definition-for model cue instance))
        cached (plan-for cached model cue definition instance)]
    (if-not (:plan cached)
      {:id cue-id :entry (dissoc cached :window) :state :static}
      (let [{:keys [beat bpm playing?] :or {beat 0.0 bpm 120.0}} (:transport model)
            t (double (or (:last-frame-time-ms model) (:time-ms model) 0))
            rate (if playing? (/ bpm 60000.0) 0)
            window (:window cached)
            signature (if instance
                        [global (:id instance) (:phase instance) (get-in instance [:system :paused?])
                         (:started-at-ms instance) (:release-at-ms instance)]
                        global)
            interval (* (bit-shift-left 1 level) (max 60.0 (min 250.0 (/ 15000.0 bpm))))]
        ;; Most cues are covered on most frames; keep that path to one small map.
        (if (and window (window-valid? window signature t beat rate))
          {:id cue-id :entry cached :state :current}
          {:id cue-id :entry cached :cue cue :instance instance :signature signature
           :t t :beat beat :rate rate :interval interval
           :state (if (chainable? window signature t beat rate interval) :expired :stale)})))))

(defn- refresh [service snapshot {:keys [entry cue instance signature t beat rate interval]}]
  (assoc entry :window (next-window service snapshot (:plan entry) cue instance (:window entry)
                                    signature t beat rate interval)))

(defn- frame-key [snapshot]
  (let [model (:model snapshot)]
    [(:show-id model) (:epoch model) (:frame-number model) (or (:last-frame-time-ms model) (:time-ms model) 0)]))

(defn- open-frame! [service snapshot]
  (let [key (frame-key snapshot)]
    (when-not (= key (:key @(:frame service)))
      (vreset! (:frame service) {:key key :spent 0 :deferred 0 :dropped 0}))))

(defn- spend-budget
  "Refresh the cues that need keyframes, most urgent first, until the frame's
  shared prediction budget runs out. A cue whose inputs changed comes before
  one whose keyframes merely ran out; among those, the longest waiting first.
  A stale cue left over loses its colour rather than show the old one; an
  expired cue left over holds its last keyframe for another frame."
  [service snapshot old demands]
  (let [urgent (sort-by (fn [{:keys [state entry]}]
                          [(if (= :stale state) 0 1) (or (get-in entry [:window :until]) Double/NEGATIVE_INFINITY)])
                        (filter (comp #{:stale :expired} :state) demands))
        budget (:samples-per-frame service)
        ;; Only entries that changed are written back to the shared cache.
        replanned (into {} (keep (fn [{:keys [id entry]}] (when-not (identical? entry (get old id)) [id entry]))) demands)]
    (reduce (fn [entries {:keys [state entry] :as demand}]
              (let [id (:id demand)]
                (cond
                  (< (:spent @(:frame service)) budget) (assoc entries id (refresh service snapshot demand))
                  (= :stale state) (do (vswap! (:frame service) update :dropped inc)
                                       (vswap! (:totals service) update :dropped inc)
                                       (assoc entries id (dissoc entry :window)))
                  :else (do (vswap! (:frame service) update :deferred inc)
                            (vswap! (:totals service) update :deferred inc)
                            entries))))
            replanned urgent)))

(defn- color-at [entry time]
  (let [{:keys [from to at until]} (:window entry)]
    (when (and from to)
      (interpolate from to (if (> until at) (min 1.0 (max 0.0 (/ (- time at) (- until at)))) 1.0)))))

(defn- entry-fault [entry]
  (or (:fault entry) (get-in entry [:window :fault])))

(defn- admit! [service lease ids]
  (let [current @(:leases service)]
    (when-not (= ids (get current lease))
      (let [new (assoc current lease ids)]
        (when (or (> (count new) 64) (> (count (into #{} (mapcat val) new)) 512))
          (throw (ex-info "Visible cue preview limit reached" {::stage :lease})))
        (vreset! (:leases service) new)
        (update-demand! service)))))

(defn- observe-region [service lease snapshot query]
  (let [model (:model snapshot)
        ids (lease-region service lease model query)]
    (admit! service lease ids)
    (open-frame! service snapshot)
    (let [model (variable-blocks/resolved model (:variable-observation snapshot))
          snapshot (assoc snapshot :model model)
          active (visible-instances model ids) level (:level (status service))
          time (or (:last-frame-time-ms model) (:time-ms model) 0)
          {:keys [bpm playing?] :or {bpm 120.0}} (:transport model)
          global [(:epoch model) bpm playing? (get-in model [:transport :meter]) (boolean (:preview-hdr snapshot))]
          old @(:cache service)
          demands (mapv #(cue-demand (get old %) snapshot % (get active %) level global) ids)
          changed (spend-budget service snapshot old demands)
          entries (if (seq changed) (into old changed) old)]
      (vreset! (:cache service) entries)
      (when (some #(not= (boolean (:plan (get old %))) (boolean (:plan (get entries %)))) ids)
        (update-governor! service))
      {:colors (persistent! (reduce (fn [colors id]
                                      (if-let [color (color-at (get entries id) time)] (assoc! colors id color) colors))
                                    (transient {}) ids))
       :faults (persistent! (reduce (fn [faults id]
                                      (if-let [f (entry-fault (get entries id))] (assoc! faults id f) faults))
                                    (transient {}) ids))})))

(defn- check-query! [query]
  (when-not (region? query) (throw (ex-info "Expected a cue page, integer origin and 1–8 rows" {}))))

(defn present!
  "Return normalized RGB values for one mounted page rectangle. Identical cues
  share keyframes across web and pad consumers. A different rectangle replaces
  that lease's previous demand. Call release! when the surface unmounts."
  [service lease snapshot query]
  (check-query! query)
  (locking (:lock service)
    (:colors (observe-region service lease snapshot query))))

(defn observe!
  "present! for Studio: {:colors {cue-id rgb}} plus :faults {cue-id {:stage
  :message}} for visible cues whose plan, parameters or prediction failed, or
  :fault {:stage :lease :message} when the lease itself was refused. A
  faulted cue has no colour, so Studio can say so instead of showing an old one."
  [service lease snapshot query]
  (check-query! query)
  (locking (:lock service)
    (try
      (let [{:keys [colors faults]} (observe-region service lease snapshot query)]
        (cond-> {:colors colors} (seq faults) (assoc :faults faults)))
      (catch clojure.lang.ExceptionInfo error
        (if (= :lease (::stage (ex-data error)))
          {:colors {} :fault (fault :lease error)}
          (throw error))))))

(defn studio-surface
  "The Studio query surface: each [query-id page origin rows] subscription
  holds its own lease and reads observe! on the stream worker."
  [service]
  (fn [query]
    (let [lease (Object.)]
      {:sample! #(observe! service lease % query)
       :close! #(release! service lease)})))

(defn diagnostics
  "What the pad sampler is doing, for the REPL and tests: demanded cues and
  leases, the shared per-frame prediction budget, the last frame's spending
  (:spent predictions, :deferred cues that held their last keyframe, :dropped
  stale cues left without a colour), running totals and current faults by cue.
  (cue-pads/diagnostics (:cue-pad-previews runtime))"
  [service]
  (locking (:lock service)
    (let [demand (into #{} (mapcat val) @(:leases service))]
      (merge (status service)
             {:leases (count @(:leases service)) :demand demand
              :samples-per-frame (:samples-per-frame service)
              :frame (dissoc @(:frame service) :key)
              :totals @(:totals service)
              :faults (into {} (keep (fn [id] (when-let [f (entry-fault (get @(:cache service) id))] [id f]))) demand)}))))

(defn- push-grid [service lease snapshot page origin]
  (let [previous @lease]
    (if (and (identical? snapshot (:source previous))
             (= page (:page previous)) (= origin (:origin previous)))
      (:model previous)
      (let [colors (present! service lease snapshot [query-id page origin 8])
            model (assoc (:model snapshot) ::views/pad-colors colors)]
        (vreset! lease {:source snapshot :page page :origin origin :model model})
        model))))

(defn push-feedback
  "Sample authored pages. Recent cues uses the regular pad swatches, as in Studio."
  [service snapshot!]
  (let [close! (fn [lease] (vreset! lease nil) (release! service lease))]
    {:open! #(volatile! nil)
     :project!
     (fn [lease local model]
       (let [snapshot (snapshot!) latest (:model snapshot)
             touch (get (:touches local) (peek (:touch-order local)))
             type (get-in latest [:instances (:instance-id touch) :definition :params (:param touch) :type])]
         (if (and (:ready? local) (not (:target-picker local)) (not (:pad-overlay local))
                  (not (#{:color :enum} type))
                  (not= pages/recent-page-id (:page-id local))
                  (= [(:show-id model) (:epoch model)] [(:show-id latest) (:epoch latest)]))
           (push-grid service lease snapshot (:page-id local) (:origin local))
           (do (close! lease) model))))
     :close! close!}))
