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
   {:lock (Object.) :cache (volatile! {}) :leases (volatile! {}) :regions (volatile! {}) :demand (volatile! #{})
    :samples-per-frame samples-per-frame
    :frame (volatile! {:key nil :spent 0 :deferred 0 :dropped 0 :done #{} :colors {} :faults {}})
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
    (vreset! (:demand service) demand)
    (vswap! (:cache service) select-keys demand)
    (update-governor! service)))

(defn release! [service lease]
  (locking (:lock service)
    (vswap! (:regions service) dissoc lease)
    (when (contains? @(:leases service) lease)
      (vswap! (:leases service) dissoc lease)
      (update-demand! service))))

(defn status
  "The visual governor: :level is the slower of the owner's frame-cost level
  and the sampler's own pressure level."
  [service]
  (let [{:keys [level pad-level] :as governor} @(:governor service)]
    (cond-> (dissoc governor :pad-level :pad-hot :pad-cool)
      (and pad-level (> pad-level level)) (assoc :level pad-level))))

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

(def ^:private valid-tolerance-beats
  "How far the transport may drift from a window's extrapolated beat before
  its keyframes no longer describe it."
  0.1)

(def ^:private chain-tolerance-beats
  "A chained window extrapolates its start beat, so it resyncs sooner."
  0.02)

(def ^:private max-retry-ms 2000.0)

(defn- keyframe-interval [bpm level]
  (* (bit-shift-left 1 level) (max 60.0 (min 250.0 (/ 15000.0 bpm)))))

(defn- continuous? [window t beat tolerance]
  (< (abs (- beat (+ (:beat window) (* (- t (:at window)) (:rate window 0))))) tolerance))

;; A window's :identity (show epoch, HDR mode and the instance's id and start
;; time, or nil for an idle cue) says whose preview it is: when it changes the
;; old keyframes are wrong, except that a cue launching from idle or ending
;; back to idle holds the colour it shows, the nearest one, until refreshed.
;; An epoch change, an HDR change or a relaunch over a running instance drops it.
;; Its :timing (the instance's phase, pause and release time), tempo, meter and
;; playing say only how that preview moves on: when they change the old
;; keyframes are still the nearest colour, so it is held. A paused instance's
;; colour is frozen where it was, and a release fades from the colour shown.

(defn- window-valid? [window identity timing t beat playing?]
  (and (= identity (:identity window)) (= timing (:timing window)) (= playing? (:playing? window))
       (<= (:at window) t) (< t (:until window))
       (continuous? window t beat valid-tolerance-beats)))

(defn- chainable?
  "Whether a window that has just expired while playing can hand its last
  keyframe to the next one: only when the instance's timing is unchanged,
  since that keyframe was predicted for the old timing."
  [window identity timing t beat playing? interval]
  (and window playing? (:playing? window) (= identity (:identity window)) (= timing (:timing window))
       (some? (:to window)) (nil? (:fault window))
       (<= (:until window) t) (< t (+ (:until window) interval))
       (continuous? window t beat chain-tolerance-beats)))

(defn- window-color [{:keys [from to at until]} time]
  (when (and from to)
    (interpolate from to (if (and (> until at) (< until Double/POSITIVE_INFINITY))
                           (min 1.0 (max 0.0 (/ (- time at) (- until at))))
                           0.0))))

(defn- frozen
  "Stopping the transport pauses the preview at the colour it shows, with no
  prediction, until the transport plays again."
  [window identity timing t beat]
  (let [color (window-color window t)]
    {:identity identity :timing timing :at t :until Double/POSITIVE_INFINITY :beat beat :rate 0 :playing? false
     :from color :to color}))

(defn- next-window
  "The next keyframe window. While playing, a chained window starts where the
  expired one ended and reuses its last prediction (one prediction); any other
  starts now (two). While stopped a window has one keyframe and does not
  expire. A failure becomes a fault window that retries after the interval,
  doubling up to two seconds while it keeps failing."
  [service snapshot plan cue instance previous identity timing t beat rate playing? interval]
  (let [chain? (chainable? previous identity timing t beat playing? interval)
        at (if chain? (:until previous) t)
        at-beat (if chain? (+ (:beat previous) (* (- at (:at previous)) (:rate previous))) beat)
        until (if playing? (+ at interval) Double/POSITIVE_INFINITY)
        base {:identity identity :timing timing :at at :until until :beat at-beat :rate (if playing? rate 0)
              :playing? playing?}]
    (try
      (let [inst (if instance
                   (assoc instance :params (try (waveforms/effective-params snapshot instance)
                                                (catch Exception error
                                                  (throw (ex-info (.getMessage error) {::stage :parameters} error)))))
                   (virtual-instance cue (get-in snapshot [:model :transport :meter])))
            from (if chain? (:to previous) (predict! service snapshot plan inst at at-beat))]
        (assoc base :from from
               :to (if playing? (predict! service snapshot plan inst until (+ at-beat (* interval rate))) from)))
      (catch Exception error
        (let [stage (or (::stage (ex-data error)) :sample)
              failures (inc (:failures previous 0))]
          (assoc base :at t :beat beat :failures failures
                 :until (+ t (min max-retry-ms (* interval (bit-shift-left 1 (min 5 (dec failures))))))
                 :fault (fault stage (if (= :parameters stage) (or (.getCause error) error) error))))))))

(defn- cue-demand
  "Where one demanded cue stands on this frame:
  :current  its keyframes cover t;
  :freeze   the transport stopped: pause at the colour shown, no prediction;
  :expired  its keyframes ran out, only timing (tempo, meter, playing, the
            instance's phase, pause or release) changed, or the cue launched
            or ended: it holds its colour until it is refreshed;
  :retry    its last prediction failed and the retry is due;
  :stale    it has no keyframes, or its identity changed: it has no colour
            until it is refreshed;
  :static   it has no preview plan."
  [cached snapshot cue-id instance level identity-base]
  (let [model (:model snapshot) cue (get-in model [:registry :cues cue-id])
        definition (when cue (definition-for model cue instance))
        cached (plan-for cached model cue definition instance)]
    (if-not (:plan cached)
      {:id cue-id :entry (dissoc cached :window) :state :static}
      (let [{:keys [beat bpm playing?] :or {beat 0.0 bpm 120.0}} (:transport model)
            playing? (boolean playing?)
            t (double (or (:last-frame-time-ms model) (:time-ms model) 0))
            window (:window cached)
            identity [identity-base (when instance [(:id instance) (:started-at-ms instance)])]
            timing (when instance
                     [(:phase instance) (boolean (get-in instance [:system :paused?])) (:release-at-ms instance)])]
        ;; Most cues are covered on most frames; keep that path to one small map.
        (if (and window (window-valid? window identity timing t beat playing?))
          {:id cue-id :entry cached :state :current}
          (let [same? (and window (= identity (:identity window)))
                ;; The cue launched (its idle preview becomes an instance) or
                ;; ended (the reverse): the colour shown is the nearest one.
                launched-or-ended? (and window (= identity-base (first (:identity window)))
                                        (not= (some? instance) (some? (second (:identity window)))))]
            {:id cue-id :entry cached :cue cue :instance instance :identity identity :timing timing
             :t t :beat beat :rate (if playing? (/ bpm 60000.0) 0) :playing? playing?
             :interval (keyframe-interval bpm level)
             :state (cond (nil? window) :stale
                          (:fault window) (if same? :retry :stale)
                          (and launched-or-ended? (:from window) (:to window)) :expired
                          (not same?) :stale
                          (and (not playing?) (:playing? window) (:from window) (:to window)
                               (= timing (:timing window))) :freeze
                          :else :expired)}))))))

(defn- refresh [service snapshot {:keys [entry cue instance identity timing t beat rate playing? interval]}]
  (assoc entry :window (next-window service snapshot (:plan entry) cue instance (:window entry)
                                    identity timing t beat rate playing? interval)))

(defn- frame-time [snapshot]
  (let [model (:model snapshot)] (or (:last-frame-time-ms model) (:time-ms model) 0)))

(defn- frame-key [snapshot]
  (let [model (:model snapshot)]
    [(:show-id model) (:epoch model) (:frame-number model) (frame-time snapshot)]))

(def ^:private pressure-frames
  "Frames in a row with cues waiting for keyframes before the sampler slows
  its keyframes one level."
  16)

(def ^:private relief-frames 32)

(defn- pressure-policy
  "Slow keyframes one level after sustained deferrals or drops; speed up
  again after a calm spell when the steady demand fits the budget at the
  faster level.

  The steady demand is one prediction per cue per window. Relief compares it
  with the whole budget: if it fits, the faster level keeps up, so the level
  never stays raised for a load that fits. The 16 pressed and 32 calm frames
  in a row are the hysteresis, so a page opening (whose burst of fresh cues
  can raise the level) relaxes within about a second."
  [{:keys [pad-level pad-hot pad-cool clients] :or {pad-level 0 pad-hot 0 pad-cool 0 clients 0} :as governor}
   pressed? demand-per-ms budget]
  (let [hot (if pressed? (inc pad-hot) 0)
        cool (if pressed? 0 (inc pad-cool))]
    (cond
      (and (>= hot pressure-frames) (< pad-level 4))
      (assoc governor :pad-level (inc pad-level) :pad-hot 0 :pad-cool 0)
      (and (>= cool relief-frames) (pos? pad-level) demand-per-ms
           (<= (* clients (demand-per-ms (dec pad-level))) budget))
      (assoc governor :pad-level (dec pad-level) :pad-hot 0 :pad-cool 0)
      :else (assoc governor :pad-hot hot :pad-cool cool))))

(defn- close-frame!
  "Feed the finished frame's waiting cues to the governor."
  [service frame snapshot]
  (when (:key frame)
    (let [dt (- (frame-time snapshot) (:t frame))
          bpm (get-in snapshot [:model :transport :bpm] 120.0)
          ;; Predictions a cue needs per frame at a level: one per window.
          per-frame (when (pos? dt) #(/ dt (keyframe-interval bpm %)))]
      (swap! (:governor service) pressure-policy (pos? (+ (:deferred frame) (:dropped frame)))
             per-frame (:samples-per-frame service)))))

(defn- older-frame?
  "Whether this snapshot is an earlier frame of the show the budget is open
  for: a reader lagging the others (the Push reading the frame before the one
  Studio was notified with). A show's epoch only counts up and within an epoch
  frame time never goes back, so an earlier epoch, or an earlier time in the
  same epoch, is an older frame."
  [frame snapshot]
  (let [[show-id epoch] (:key frame) model (:model snapshot) reader-epoch (:epoch model)]
    (and (some? (:key frame)) (= show-id (:show-id model))
         (or (and (number? epoch) (number? reader-epoch) (< reader-epoch epoch))
             (and (= epoch reader-epoch) (< (frame-time snapshot) (:t frame)))))))

(defn- open-frame!
  "One budget per committed frame key. A different snapshot under the same
  key (a command committed between ticks, an edited copy) shares that budget
  but is read again, so a definition edit is never answered from the old one."
  [service snapshot]
  (let [key (frame-key snapshot) frame @(:frame service)]
    (cond
      (not= key (:key frame))
      (do (close-frame! service frame snapshot)
          (vreset! (:frame service) {:key key :source snapshot :t (frame-time snapshot) :spent 0 :deferred 0 :dropped 0
                                     :done #{} :colors {} :faults {}}))
      (not (identical? snapshot (:source frame)))
      (vswap! (:frame service) assoc :source snapshot :done #{} :colors {} :faults {}))))

(defn- spend-budget
  "Refresh the cues that need keyframes until the frame's prediction budget
  runs out: stale cues first, then expired cues, then fault retries, each the
  longest waiting first. A stale cue left over has no colour rather than an
  old one; an expired cue left over holds its colour; a retry left over keeps
  its fault. Freezing on stop costs nothing."
  [service snapshot old demands]
  (let [rank {:stale 0 :expired 1 :retry 2}
        urgent (sort-by (fn [{:keys [state entry]}]
                          [(rank state) (or (get-in entry [:window :until]) Double/NEGATIVE_INFINITY)])
                        (filter (comp rank :state) demands))
        budget (:samples-per-frame service)
        ;; Only entries that changed are written back to the shared cache.
        replanned (into {} (keep (fn [{:keys [id entry state] :as demand}]
                                   (cond (= :freeze state)
                                         [id (assoc entry :window (frozen (:window entry) (:identity demand) (:timing demand)
                                                                          (:t demand) (:beat demand)))]
                                         (not (identical? entry (get old id))) [id entry])))
                        demands)
        miss! (fn [k] (vswap! (:frame service) update k inc) (vswap! (:totals service) update k inc))]
    (reduce (fn [entries {:keys [id state entry] :as demand}]
              (cond
                (< (:spent @(:frame service)) budget) (assoc entries id (refresh service snapshot demand))
                (and (= :stale state) (not (get-in entry [:window :fault])))
                (do (miss! :dropped) (assoc entries id (dissoc entry :window)))
                :else (do (miss! :deferred) entries)))
            replanned urgent)))

(defn- color-at [entry time]
  (some-> (:window entry) (window-color time)))

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

(defn- advance-frame!
  "Bring every demanded cue up to this frame once, whichever reader comes
  first, so readers share one budget in one priority order. A reader that
  joins later in the frame adds only its new cues."
  [service snapshot]
  (let [frame @(:frame service)
        todo (into #{} (remove (:done frame)) @(:demand service))]
    (when (seq todo)
      (let [model (variable-blocks/resolved (:model snapshot) (:variable-observation snapshot))
            snapshot (assoc snapshot :model model)
            active (visible-instances model todo) level (:level (status service))
            time (frame-time snapshot)
            identity-base [(:epoch model) (boolean (:preview-hdr snapshot))]
            old @(:cache service)
            demands (mapv #(cue-demand (get old %) snapshot % (get active %) level identity-base) todo)
            changed (spend-budget service snapshot old demands)
            entries (if (seq changed) (into old changed) old)]
        (vreset! (:cache service) entries)
        (when (some #(not= (boolean (:plan (get old %))) (boolean (:plan (get entries %)))) todo)
          (update-governor! service))
        (vswap! (:frame service)
                (fn [frame]
                  (assoc frame :done (into (:done frame) todo)
                         :colors (persistent! (reduce (fn [colors id]
                                                        (if-let [color (color-at (get entries id) time)]
                                                          (assoc! colors id color) colors))
                                                      (transient (:colors frame)) todo))
                         :faults (persistent! (reduce (fn [faults id]
                                                        (if-let [f (entry-fault (get entries id))]
                                                          (assoc! faults id f) faults))
                                                      (transient (:faults frame)) todo)))))))))

(defn- pick [m ids]
  (persistent! (reduce (fn [out id] (if-let [v (get m id)] (assoc! out id v) out)) (transient {}) ids)))

(defn- observe-region
  "The colours and faults of a lease's cues on this snapshot's frame. A reader
  on an older frame neither reopens the budget nor moves the shared keyframes
  back: any cue it newly shows is brought up to the current frame, and it reads
  every colour from the shared keyframes at its own time, at no cost."
  [service lease snapshot query]
  (let [ids (lease-region service lease (:model snapshot) query)]
    (admit! service lease ids)
    (if (older-frame? @(:frame service) snapshot)
      (do (advance-frame! service (:source @(:frame service)))
          (let [cache @(:cache service) t (frame-time snapshot)]
            {:colors (into {} (keep (fn [id] (when-let [c (color-at (get cache id) t)] [id c]))) ids)
             :faults (into {} (keep (fn [id] (when-let [f (entry-fault (get cache id))] [id f]))) ids)}))
      (do (open-frame! service snapshot)
          (advance-frame! service snapshot)
          (let [{:keys [colors faults]} @(:frame service)]
            {:colors (pick colors ids) :faults (pick faults ids)})))))

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
  leases, the shared per-frame prediction budget, the current frame's spending
  (:spent predictions, :deferred cues that held their colour or fault, :dropped
  stale cues left without a colour), running totals, the governor level (which
  rises after sustained deferrals) and current faults by cue.
  (cue-pads/diagnostics (:cue-pad-previews runtime))"
  [service]
  (locking (:lock service)
    (let [demand (into #{} (mapcat val) @(:leases service))]
      (merge (status service)
             {:leases (count @(:leases service)) :demand demand
              :samples-per-frame (:samples-per-frame service)
              :frame (select-keys @(:frame service) [:spent :deferred :dropped])
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
