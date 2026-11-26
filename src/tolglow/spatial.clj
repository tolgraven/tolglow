(ns tolglow.spatial
  "Show-owned spatial bindings, event delivery and query inputs. Physics and
  emitted light contacts come from Afterglow; durable show state contains data only."
  (:require [afterglow.control.model :as control]
            [tolglow.aim-paths :as aim-paths]
            [afterglow.reactive :as graph]
            [afterglow.render.frame :as render]
            [afterglow.runtime.events :as staging]
            [afterglow.spatial :as physics]
            [afterglow.spatial.field :as field]
            [afterglow.time :as time]
            [tolglow.autopilot.signal :as signal]
            [tolglow.rig.surfaces :as surfaces]
            [tolglow.spatial.actions :as actions]
            [tolglow.spatial.cues :as cues]
            [tolglow.spatial.sampling :as sampling]
            [tolglow.spatial.targets :as targets]))

(def state-path [:extensions ::world])

(defn body [context [_ id]] (get-in @(:db context) [:variables :spatial :signals id]))

(defn scene [context _] (get-in @(:db context) [:variables :spatial :signals] {}))

(defn members [context [_ id]] (get-in @(:db context) [:variables :spatial :members id] #{}))

(defn register! [system existing]
  (let [ids (into #{} (map (juxt :kind :id)) (:registrations existing))]
    (doseq [[id handler parent] [[:spatial/scene scene #'scene] [:spatial/body body #'body] [:spatial/members members #'members]]
            :when (not (ids [:sub id]))]
      (graph/reg-sub! system id handler {:kind :sub :id id :implementation {:type :var :symbol (str (symbol parent))}}))))

(defn settings [s]
  (when-not (and (map? s) (every? #{:enabled? :run-when :bounds :gravity :volumes :bindings} (keys s))
                 (boolean? (:enabled? s false)) (#{:demand :enabled} (:run-when s :demand)))
    (throw (ex-info "Invalid spatial show settings" {:settings s})))
  (physics/validate! (select-keys s [:bounds :gravity :volumes]))
  (actions/validate-bindings! (:bindings s {}))
  (doseq [[id volume] (:volumes s) :when (contains? volume :radius-source)]
    (when-not (and (#{:ball :sphere} (:shape volume)) (signal/valid? (:radius-source volume)))
      (throw (ex-info "A radius source needs a numeric signal on a sphere or ball" {:volume-id id}))))
  s)

(defn edit-settings [current patch]
  (settings
    (reduce (fn [s k]
              (if-not (contains? patch k) s
                (let [changes (get patch k)]
                  (when-not (map? changes) (throw (ex-info "Edit spatial entries by ID" {:key k})))
                  (assoc s k (reduce-kv (fn [entries id spec]
                                         (if (nil? spec) (dissoc entries id) (assoc entries id spec)))
                                       (get current k {}) changes)))))
            (merge current patch) [:volumes :bindings])))

(defn- consumes-events? [subscriber]
  (let [{:keys [type category]} (:match subscriber)]
    (and (:enabled? subscriber true) (or (nil? category) (= :spatial category))
         (or (nil? type) (= "spatial" (namespace type))))))

(defn- scene-config [settings model clock]
  (let [config (select-keys settings [:bounds :gravity :volumes])
        bounds (:bounds config physics/default-bounds)
        room-radius (* 0.5 (reduce min (map - (:max bounds) (:min bounds))))]
    (reduce-kv (fn [c id {:keys [radius-source shape]}]
                 (if (some? radius-source)
                   (assoc-in c [:volumes id :radius]
                             (max (if (= :ball shape) (min room-radius 1.0e-6) 0.0)
                                  (min (if (= :ball shape) room-radius Double/MAX_VALUE)
                                       (signal/value model radius-source {:start-clock clock})))) c))
               config (:volumes config))))

(defn selection-members
  "A launch can select fixtures even before anyone watches the volume. Sampling
  uses the last simulated instant without advancing bodies or dispatching events."
  [model payload now]
  (let [members (get-in model [:variables :spatial :members])
        launch (:launch payload)
        selector (or (:selector launch) (when-not (contains? launch :targets)
                                          (get-in model [:registry :cues (:cue-id payload) :selector])))
        settings (get-in model [:settings :spatial])]
    (cond
      (not (and (:volume-id selector) (#{:cue/launch :effect/launch} (:op payload)))) members
      (some? (get-in model [:variables :spatial :signals (:volume-id selector) :cue-id]))
      (select-keys members [(:volume-id selector)])
      (not (:enabled? settings)) {}
      :else
      (let [id (targets/volume-id (:volumes settings) (:volume-id selector))
            volume (get-in settings [:volumes id])]
        (cond
          (nil? volume) {}
          (= :hdr (:shape volume)) (select-keys members [id])
          :else
          (let [prior (get-in model state-path)
                clock (assoc (time/frame-clock model) :time-ms now :epoch (:epoch model))
                config (scene-config (assoc settings :volumes {id volume}) (assoc model :time-ms now) (:start-clock prior clock))
                body (get-in prior [:bodies id])
                saved (when body {:bodies {id body}})]
            (:members (physics/advance (physics/rebase saved clock) config clock (get-in model [:rig :fixtures]) {}))))))))

(defn apply-command
  "Resolve a cue's selected fixture scope on every Tolglow control surface,
  including subscriber callbacks, composition steps and simulated admissions.
  Every such launch also passes the synchronized aim path launch checks, and
  a musical synchronized move it launches is pinned to its launch-tempo interval."
  ([model command] (apply-command model command nil))
  ([model command authority]
   (if-not (#{:cue/launch :effect/launch} (:op command))
     (if authority (control/apply-command model command authority)
         (control/apply-command model command))
     (let [refusal (when-not (get-in model [:control :history (:command-id command)])
                     (aim-paths/launch-problem model command aim-paths/*committed-values*))
           prepared (if-let [[code message] refusal]
                      {:ack (merge (select-keys model [:show-id :epoch :revision])
                                   {:command-id (:command-id command) :status :rejected :code code :message message})}
                      (try
                        (let [members (selection-members model command (:time-ms command (:time-ms model)))]
                          {:command (targets/resolve-command model command members)})
                        (catch clojure.lang.ExceptionInfo error
                          {:ack (merge (select-keys model [:show-id :epoch :revision])
                                       {:command-id (:command-id command) :status :rejected :code :invalid-target
                                        :message (ex-message error) :details (ex-data error)})})))]
       (if-let [scoped (:command prepared)]
         (aim-paths/hold-launch-tempo (targets/remember (control/apply-command model scoped) scoped))
         {:model model :events [] :ack (:ack prepared)})))))

(defn- stage [model events]
  (reduce (fn [m e] (staging/collect m :spatial {:kind :volume :id (:volume-id e)} [e])) model events))

(defn clear-session [model]
  (cond-> model
    (get-in model cues/run-path) (update :extensions dissoc (last cues/run-path))
    (get-in model state-path) (update :extensions dissoc ::world)
    (get-in model actions/run-path) (update :extensions dissoc (last actions/run-path))
    (get-in model [:variables :spatial]) (update :variables dissoc :spatial)))

(defn restore [model ids epoch]
  (cond-> (cues/restore (actions/restore model ids epoch) ids)
    (get-in model state-path) (assoc-in (conj state-path :epoch) epoch)))

(defn- venue-optics [rig]
  (let [{:keys [width height depth]} (merge {:width 12 :height 5 :depth 10} (:room rig))]
    {:range-m (Math/hypot (Math/hypot width depth) height)
     :surfaces (surfaces/receivers rig)}))

(defn- optical-observations [before model]
  (let [rig (:rig model)
        prior (get-in before [:spatial-cache :optics])
        cached (when (identical? rig (:rig prior)) prior)
        previous (when (and (= (get-in before [:model :epoch]) (:epoch model))
                            (identical? (get-in before [:model :rig]) rig))
                   (:output before))
        extinction (get-in previous [:atmosphere :extinction-per-m] 0.0)
        venue (or (:venue cached) (venue-optics rig))
        options (if (and cached (= extinction (:extinction cached))) (:options cached)
                    (field/prepare-observations (assoc venue :extinction-per-m extinction)))
        compact (:compact previous)
        order (:target-order compact)
        index (when compact
                (if (identical? order (get-in cached [:index :order])) (:index cached)
                    (render/compact-index order)))
        frame (if (and compact (nil? (:values previous)))
                (field/compact-observations options index compact)
                (assoc options :values (:values previous)))]
    {:cache {:rig rig :venue venue :extinction extinction :options options :index index}
     :frame frame}))

(defn- live-settings [original active previous]
  (if (empty? active)
    original
    (let [overlays (into {} (keep (fn [[id {:keys [volume]}]] (when volume [id volume]))) active)
          settings (-> (if (:enabled? original) original (select-keys original [:bounds :gravity]))
                       (assoc :enabled? true :run-when :enabled)
                       (update :volumes merge overlays))]
      (if (= settings previous) previous settings))))

(defn- volume-errors [model fields now]
  (for [[key {:keys [instance-id error]}] (:views fields)
        :when (and error (not= error (get-in model [:variables :spatial :signals key :error])))]
    {:type :spatial/invalid-volume :volume-id key :instance-id instance-id :time-ms now :message error}))

(defn- optical-volume? [volume]
  (some #{:beam :aimable} (:selection volume)))

(defn- optical-sampler [rig frame active]
  (if (next (filter (comp optical-volume? :volume) (vals active)))
    (field/sampler rig frame)
    #(field/sample % rig frame)))

(defn- linked-controls [model before]
  (let [view (:modulation-view before)
        fields (get-in before [:parameter-samples :spatial-overrides])]
    (if (and (seq fields) (= (:epoch model) (get-in before [:model :epoch])))
      (let [authored-sources #(get-in % [:instances %2 :definition :spatial-volume :sources])
            retain (fn [indexed id values]
                     (let [current (authored-sources model id)
                           prior (authored-sources (:model before) id)]
                       (reduce-kv (fn [m param value]
                                    (if (and (contains? current param) (= (current param) (get prior param)))
                                      (assoc-in m [id param] value) m))
                                  indexed values)))]
        (assoc view :linked-overrides (reduce-kv retain (:overrides view {}) fields)))
      view)))

(defn advance
  "Advance the show's volumes for one frame. The optional field sampler from
  tolglow.spatial.sampling splits a large patch across its workers and returns
  the fields of the serial path."
  [model before now graph command & [sampler]]
  (let [original (get-in model [:settings :spatial])
        old-cache (:spatial-cache before)
        active (cues/volumes (assoc model :time-ms now) (linked-controls model before) (:settings old-cache))
        model (cues/reconcile model active now command)
        s (live-settings original active (:settings old-cache))
        prior (get-in model state-path)
        clock (assoc (time/frame-clock model) :time-ms now :epoch (:epoch model))]
    (cond
      (not (:enabled? s))
      (if (or prior (get-in model actions/run-path))
        (let [result (physics/advance prior {} clock [] {})]
          {:model (-> (actions/apply-frame model result {} now command) (stage (:events result)) clear-session)})
        {:model model})

      (and (not= :enabled (:run-when s)) (not-any? #(:enabled? % true) (vals (:bindings s)))
           (not-any? consumes-events? (get-in model [:event-subscribers :subscribers]))
           (not (graph/demanded? graph #{:spatial/scene :spatial/body :spatial/members})))
      ;; A removed binding must still relinquish its ownership on this tick.
      {:model (if (get-in model actions/run-path)
                (actions/apply-frame model {:members {} :signals {} :events []} {} now command) model)
       :cache {:suspended? true}}

      :else
      (let [fixtures (get-in model [:rig :fixtures])
            rig (if (identical? fixtures (:fixtures old-cache)) (:rig old-cache) (physics/prepare-rig fixtures))
            start (or (get-in prior [:start-clock]) clock)
            dynamic? (if (identical? s (:settings old-cache)) (:dynamic? old-cache)
                         (boolean (some :radius-source (vals (:volumes s)))))
            scene (if (and (identical? s (:settings old-cache)) (not dynamic?)) (:scene old-cache)
                    (physics/prepare (if dynamic? (scene-config s (assoc model :time-ms now) start) s)))
            contacts (when (and (= (:epoch model) (get-in before [:model :epoch]))
                                (identical? fixtures (get-in before [:model :rig :fixtures])))
                       (get-in before [:output :hdr-contacts]))
            prior (if (and prior (:suspended? old-cache)) (physics/rebase prior clock) prior)
            result (physics/advance prior scene clock rig contacts)
            observations (delay (optical-observations before model))
            optical-sample (delay (optical-sampler rig (:frame @observations) active))
            split? (sampling/parallel? sampler (count fixtures))
            kept (let [cached (:slices old-cache)]
                   (when (and cached (identical? rig (:rig cached)) (= (:partitions sampler) (:parts cached)))
                     cached))
            slices (delay (or kept (sampling/partition-rig fixtures rig (:partitions sampler))))
            split-optical (delay (mapv #(optical-sampler (:rig %) (:frame @observations) active)
                                       (:optical @slices)))
            sample (fn [volume]
                     (cond
                       (and split? (:sliced? @slices))
                       (sampling/sample sampler @slices volume (when (optical-volume? volume) @split-optical))
                       (optical-volume? volume) (@optical-sample volume)
                       :else (field/positions volume rig)))
            fields (when (seq active) (cues/frame-fields (assoc model :time-ms now) active result sample))
            events (into (:events result) (volume-errors model fields now))
            view (cond-> (assoc (select-keys result [:members :signals :dropped-ms]) :events events)
                   fields (update :signals #(merge-with merge % (:views fields)))
                   fields (update :members merge (:members fields)))
            model (-> model (assoc-in state-path (assoc (:state result) :start-clock start))
                      (assoc-in [:variables :spatial] view) (stage events))]
        {:model (actions/apply-frame model result (:bindings s {}) now command)
         :fields fields
         :cache (cond-> {:fixtures fixtures :rig rig :settings s :dynamic? dynamic? :scene scene}
                  (realized? observations) (assoc :optics (:cache @observations))
                  (or kept (realized? slices)) (assoc :slices @slices))
         :observe-hdr? (boolean (some #(= :hdr (:shape %)) (vals (:volumes s))))}))))

(defn on-contact
  "Build a descriptor for show/register-event-subscriber! or reg-event-subscriber!.
  The callback gets the committed event, fixture ID, volume ID and show context."
  [id type match actions]
  {:id id :match (merge match {:category :spatial :type type}) :actions (vec actions)})
