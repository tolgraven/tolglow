(ns tolglow.show
  "Executable host for the shared functional lighting engine. Requiring it is inert.
  One owner advances the model; workers return data; physical output is opt-in."
  (:require [afterglow.control.model :as control]
            [afterglow.control.pages :as cue-pages]
            [tolglow.studio.parameter-precision :as precision]
            [afterglow.control.categories :as categories]
            [afterglow.control.devices.expressive :as expressive-mapping]
            [afterglow.control.sensors :as sensor-core]
            [afterglow.runtime.engine :as engine]
            [afterglow.runtime.keyframes :as keyframes]
            [tolglow.effects.keyframes :as effect-keyframes]
            [tolglow.effects.cadence :as cadence]
            [tolglow.visualization.cue-pads :as cue-pads]
            [afterglow.runtime.events :as staging]
            [tolglow.events :as show-events]
            [tolglow.event-subscribers :as subscribers]
            [tolglow.events.callbacks :as event-callbacks]
            [afterglow.runtime.executor :as executor]
            [afterglow.runtime.reuse :as reuse]
            [afterglow.render.compute :as compute]
            [afterglow.render.frame :as render]
            [afterglow.time :as musical-time]
            [tolglow.data :as data]
            [tolglow.autopilot :as autopilot]
            [tolglow.autopilot.config :as autopilot-config]
            [tolglow.effects.authoring :as authoring]
            [tolglow.effects.block-trace :as block-trace]
            [tolglow.effects.parameter-sources :as parameter-sources]
            [tolglow.controllers.page-overview :as page-overview]
            [tolglow.effects.variable-blocks :as variable-blocks]
            [tolglow.effects.prefabs :as prefabs]
            [tolglow.effects.material-design :as material-design]
            [tolglow.effects.variable-edit :as variable-edit]
            [tolglow.agents.venue :as venue-tools]
            [clojure.java.io :as io]
            [tolglow.compiler :as compiler]
            [tolglow.compiler.batch :as batch]
            [tolglow.registry :as registry]
            [tolglow.integrations.ableton :as ableton]
            [tolglow.aim-paths :as aim-paths]
            [tolglow.rig.pixel-layout :as pixel-layout]
            [tolglow.rig.reflections :as reflections]
            [tolglow.rig.surface-aim :as surface-aim]
            [tolglow.modulation :as modulation]
            [tolglow.lighting-capacity :as capacity]
            [tolglow.lighting-scenes :as lighting-scenes]
            [tolglow.lighting-levels :as lighting-levels]
            [tolglow.clock :as show-clock]
            [tolglow.cue-timing :as cue-timing]
            [tolglow.expressive :as expressive]
            [tolglow.signals :as signals]
            [afterglow.reactive :as graph]
            [tolglow.observations :as observations]
            [afterglow.reactive.registry :as reactive-registry]
            [tolglow.spatial :as spatial]
            [tolglow.spatial.cues :as spatial-cues]
            [tolglow.spatial.targets :as targets]
            [tolglow.authoring.output-patch :as output-patch]
            [tolglow.waveforms :as waveforms]
            [tolglow.automation.policy :as policy]
            [tolglow.demo :as demo]
            [tolglow.composition :as composition]
            [tolglow.output :as output]
            [tolglow.studio.display :as display]
            [afterglow.output.retirement :as retirement]
            [afterglow.output.scheduler :as output-scheduler]
            [tolglow.output.pause-look :as pause-look]
            [tolglow.effects.faults :as effect-faults]
            [tolglow.show.preview :as preview]
            [tolglow.show.push-scene :as push-scene]
            [tolglow.show.lighting :as lighting]
            [tolglow.integrations.analysis :as analysis]
            [tolglow.integrations.section-patterns :as section-patterns]
            [tolglow.persistence :as persistence]
            [tolglow.connections :as connections]
            [tolglow.repl.sessions :as repl-manager]
            [tolglow.development :as development]
            [tolglow.integrations.audience-invitation :as audience-stage]
            [tolglow.sensors :as sensors]
            [tolglow.sensors.audience.zones :as audience-zones]
            [tolglow.sensors.osc :as sensor-osc]
            [tolglow.fixtures.library :as fixture-service]
            [tolglow.rig :as rig]
            [tolglow.fixtures.patch :as fixture-patch]
            [tolglow.rig.rdm :as rdm]
            [tolglow.rig.calibration :as calibration]
            [tolglow.rig.venue-scan :as venue-scan]
            [tolglow.rig.photometry :as photometry]
            [tolglow.rig.fog-calibration :as fog-scan]
            [tolglow.venue-fog :as venue-fog]
            [tolglow.atmosphere.control :as fog-control]
            [tolglow.atmosphere.deadline :as fog-deadline]
            [tolglow.calibration.session :as calibration-session]
            [tolglow.audio.capture :as audio]
            [tolglow.integrations.audio-return :as audio-return]
            [tolglow.integrations.audio-duplex :as duplex]
            [afterglow.sync :as sync]
            [afterglow.sync.session :as session]
            [afterglow.control.devices.transports :as midi-ports]
            [tolglow.integrations.live-sync :as live-sync]
            [tolglow.integrations.live-parameters :as live-params]
            [tolglow.routing :as routing]
            [afterglow.reactive.clock :as reactive-clock]
            [tolglow.agents :as agents]
            [afterglow.control.devices.repl :as devices]
            [tolglow.composition.response :as response]
            [tolglow.composition.envelope :as envelope])
  (:import [java.util Arrays UUID]
           [java.util.concurrent LinkedBlockingQueue]))

(defn- clock-time [clock]
  (let [{:keys [origin-nanos time-ms]} @clock]
    (+ time-ms (/ (- (System/nanoTime) origin-nanos) 1.0e6))))
(defn- now-ms [runtime] (clock-time (:clock runtime)))
(defn agent-manager [runtime] (connections/agents (:connections runtime)))
(defn reactions [runtime] (connections/reactive (:connections runtime)))
(defn repls [runtime] (connections/repls (:connections runtime)))
(defn- error-data [error] {:message (.getMessage ^Throwable error) :details (ex-data error)})

(defn- scene-eligible-ids [runtime fixtures]
  (let [cache (:scene-eligible runtime)
        previous @cache]
    (if (identical? fixtures (:fixtures previous)) (:ids previous)
      (let [ids (lighting-scenes/eligible-ids fixtures)]
        (reset! cache {:fixtures fixtures :ids ids}) ids))))

(defn- recent-page-snapshot [model]
  (let [{:keys [rows] :as page} (cue-pages/page model cue-pages/recent-page-id)]
    ;; The native pads count from the bottom. Studio counts from the top.
    (update page :cells
            (fn [cells]
              (->> cells
                   (map (fn [[[x y] cue-id]] {:x x :y (- (dec rows) y) :cue-id cue-id}))
                   (sort-by (juxt :y :x))
                   vec)))))

(defn project-snapshot [runtime {:keys [model output status modulation-view parameter-samples] :as snapshot}]
  (let [sources (parameter-sources/source-index model (assoc parameter-samples :diagnostics (:diagnostics modulation-view)))
        responses? (some #(not= false (:enabled? %)) (:responses model))
        resolve-inputs (waveforms/resolved-inputs snapshot response/monitor-middleware)
        optics (reflections/plan-for (:optics-plan runtime) (:rig model))
        fixtures (get-in model [:rig :fixtures])
        patch @(:patch runtime)
        definitions (precision/model-definitions (:parameter-precision runtime) model)
        gains (if (identical? fixtures (:fixtures patch))
                (:brightness-gains patch)
                (preview/brightness-gains fixtures))]
    (merge (select-keys model [:show-id :epoch :revision :time-ms :master :blackout? :transport :rig :settings])
           {:runtime-id (:id runtime)
            :frame-number (get model :frame-number 0)
            :visualization-clock {:sample-period-ms (cue-pads/sample-period-ms (:cue-pad-previews runtime))}
            :spatial (get-in model [:variables :spatial])
            :reactive-faults (vec (concat (graph/faults (reactions runtime)) (get-in model [:variables :runtime :handler-faults])))
            :autopilot (get-in model [:variables :lighting/autopilot] {:enabled? false})
            :audio/energy (get-in model [:variables :audio/energy])
            :lighting-scenes (lighting-scenes/snapshot model (scene-eligible-ids runtime fixtures))
            :receiver-surfaces (:receivers optics)
            :surface-aim (get-in model [:variables :surface-aim] {})
            :event-subscribers (select-keys (:event-subscribers model {:subscribers []}) [:subscribers :last-error :dropped])
            :parameter-waveforms (parameter-sources/waveform-options model)
            :waveform-revision (get-in model [:waveforms :revision] 0)
            :fixture-types (vec (sort-by (comp str :id) (vals (get-in model [:registry :fixtures]))))
            :category-colors registry/category-colors
            :effects (mapv #(assoc (get definitions [% nil]) :categories (registry/definition-categories %))
                            (sort-by (comp str :id) (vals (get-in model [:registry :effects]))))
            :cues (vec (sort-by (comp str :id) (vals (get-in model [:registry :cues]))))
            :groups (categories/group-view model)
            :instances (mapv #(-> % (dissoc :state :parameter-bindings)
                                  (assoc :definition (get definitions [(:definition %) (:targets %)])
                                         :parameter-sources (parameter-sources/project % (get sources (:id %)) resolve-inputs responses?)
                                         :categories (registry/definition-categories (:definition %)))
                                  (update :system (fn [s] (merge {:opacity 1.0 :paused? false}
                                                               (select-keys s [:opacity :gain :paused? :attack :release])))))
                             (sort-by :order (vals (:instances model))))
            :pending-cue-launches (cue-timing/pending model)
            :cue-launch-outcomes (cue-timing/outcomes model)
            :pages (mapv (fn [page] (update page :cells
                                           #(mapv (fn [[[x y] cue]] {:x x :y y :cue-id cue})
                                                  (sort-by key %)))) (sort-by (comp str :id) (vals (:pages model))))
            :recent-page (recent-page-snapshot model)
            :auto (select-keys (:auto model) [:enabled? :config :reasons])
            :responses (vec (:responses model))
            :effect-events (get-in model [:variables :runtime] {:event-sequence 0 :events []})
            :patch (routing/snapshot model)
            :modulation {:controllers (mapv #(select-keys % [:id :config :ticks :next-beat :owner-instance-id])
                                             (get-in model [:modulation :controllers]))
                         :holds (get-in model [:modulation :holds] {})
                         :overrides (:overrides modulation-view {})
                         :diagnostics (:diagnostics modulation-view [])}
            :compositions (mapv #(assoc % :running? (contains? (:composition-runs model) (:id %))
                                        :playback (get-in model [:composition-runs (:id %) :playback]))
                                (sort-by (comp str :id) (vals (:compositions model))))
            :output {:values (reflections/with-footprints optics (preview/values output fixtures gains))
                     :edge-tracking (:edge-tracking output [])
                     :atmosphere (:atmosphere output)
                     :fog-control (fog-control/view (:fog-control output))
                     :preview-only? true
                     :lighting-capacity (:lighting-capacity output)
                     :lighting-budget (:lighting-budget output)
                     :lighting-activity (:lighting-activity output)
                     :universes (vec (sort (keys (:universes output))))}
            :status (merge status {:running? @(:running runtime)
                                   :compute (compute/status (:compute runtime))
                                   :movement (compute/movement-status (:compute runtime) output)
                                   :effect-logs (effect-faults/status (:effect-logs runtime))
                                   :audio-returns (duplex/status (:audio-returns runtime))
                                   :output-scheduler (output-scheduler/status (:publisher runtime))
                                   :output (or (get-in runtime [:sink :description]) {:type :preview})})})))

(defn snapshot [runtime] (project-snapshot runtime @(:state runtime)))

(defn variables
  "All show variables from one committed frame, for headless callers and the REPL."
  [runtime]
  (let [{:keys [model variable-observation]} @(:state runtime)]
    (:variables (variable-blocks/resolved model variable-observation))))

(declare prepare-output prepare-safety-output present-output! heartbeat! submit!)

(defn attach-observer!
  "Install a frame observer once under the show owner. Its callbacks enqueue data
  only. close! runs outside the owner lock when the show stops."
  [runtime id create!]
  (locking (:lock runtime)
    (when-not @(:running runtime) (throw (ex-info "Show stopped" {})))
    (or (get @(:observers runtime) id)
        (let [observer (create!)]
          (swap! (:observers runtime) assoc id observer)
          observer))))

(defn detach-observer!
  "Remove a registered observer from future frame commits and close it once."
  [runtime id]
  (when-let [observer (locking (:lock runtime)
                        (let [[before _] (swap-vals! (:observers runtime) dissoc id)]
                          (get before id)))]
    ((:close! observer))))

(defn- compiler-context [fixtures batch? compute-owner]
  (let [options (cond-> {:enabled? batch? :bind-pure reuse/evaluator}
                  (compute/expressions-enabled? compute-owner) (assoc :prepare-group (compute/group-preparer compute-owner)))]
    {:fixtures fixtures :compiler (batch/compiler-for-rig fixtures options) :cache (atom {})}))

(defn- open-compute! [{:keys [compute-backend movement-backend batch? frame-rate gpu-budget-ms]
                       :or {batch? true frame-rate 40} :as options}]
  (when (and (contains? #{:native-gpu :distributed} compute-backend) (not batch?))
    (throw (ex-info "GPU expressions require :batch? true" {})))
  (compute/open!
   (cond-> (select-keys options [:compute-backend :movement-backend :gpu-options])
     (or (contains? #{:native-gpu :distributed} compute-backend) (= :native-gpu movement-backend))
     (assoc :budget-ms (or gpu-budget-ms (max 1 (long (/ 500.0 frame-rate))))))))

(defn- configure-runtime
  [compute-owner {:keys [model sink workers agent-options batch?] :or {workers 2 batch? true} :as options}]
   (let [model (-> (or model (demo/model))
                   prefabs/startup
                   (update :rig pixel-layout/prepare-startup)
                   calibration/disarm-restored photometry/disarm fog-scan/disarm)
         _ (when-not (or (nil? (:effect-log! options)) (ifn? (:effect-log! options)))
             (throw (ex-info "Effect log sink must be callable" {})))
         music (when-let [policy (:section-patterns options)]
                 (let [planner (section-patterns/new-planner nil policy)]
                   (when (get-in planner [:policy :enabled?]) planner)))
         _ (lighting-scenes/validate-state! model)
         _ (lighting/validate! (:settings model))
         _ (variable-blocks/compile-model model)
         _ (registry/validate-public-query-ids! (get-in model [:settings :studio :public-queries]))
         _ (when-let [config (get-in model [:settings :adaptive-keyframes] (:adaptive-keyframes options))]
             (cadence/settings config))
         _ (when-let [s (get-in model [:settings :spatial])] (spatial/settings s))
         _ (when-let [opts (get-in model [:settings :lighting-capacity])] (capacity/settings opts))
         _ (rig/validate-rig (:rig model))
         _ (venue-fog/validate-saved! model)
         _ (when-let [opts (get-in model [:settings :fog-control])] (fog-control/settings opts))
         _ (doseq [definition (vals (get-in model [:registry :effects]))] (compiler/compile-effect definition))
         fixtures (get-in model [:rig :fixtures])
         patch (render/compile-patch fixtures)
         compiler-context (atom (compiler-context fixtures batch? compute-owner))
         clock (atom {:origin-nanos (System/nanoTime) :time-ms (:time-ms model)})
         runtime-ref (atom nil)
         browser-inputs (observations/create (get options :browser-observations observations/defaults))
         rdm-state (rdm/session-state)
         owned (connections/create model (-> (select-keys options [:agent-options :fixture-options :sensor-options :credential-store :credential-path])
                                             (assoc :repl-options (assoc (:repl-options options {}) :runtime-supplier #(deref runtime-ref)))
                                             (assoc :audio-options (assoc (:audio-options options {}) :now-ms #(clock-time clock)))
                                             (assoc :sync-options (assoc (:sync-options options {}) :now-ms #(clock-time clock)))
                                             (assoc :reactive-options (:reactive-options options)
                                                    :register-reactive! #(-> % reactive-clock/register! effect-faults/register!
                                                                             effect-keyframes/register! block-trace/register!
                                                                             (observations/register! browser-inputs)
                                                                             (rdm/register-status! rdm-state) (spatial/register! nil)))))
         worker-pool (try (executor/create {:workers workers :queue-size 64
                                            :timeout-ms (:worker-timeout-ms options 500)})
                          (catch Exception error (connections/close! owned) (throw error)))]
     (let [development (try (development/create
                             (when-let [workspace-options (if (contains? options :workspace-options)
                                                            (:workspace-options options) (development/default-options))]
                               (assoc workspace-options :app-agent-options
                                      (assoc (merge (:app-agent-options workspace-options) (:app-agent-options options))
                                             :credential-store (get-in owned [:options :agent-options :credential-store])))))
                            (catch Exception error
                              (.close ^java.io.Closeable worker-pool)
                              (connections/close! owned)
                              (throw error)))
           publisher (output-scheduler/create
                      (cond-> {}
                        (:send! sink)
                        (assoc :receipt-capacity 256
                               :prepare-output (fn [buffers source]
                                                 (prepare-output @runtime-ref (:output-context source) buffers source))
                               :output-valid-until-ms (fn [source checked-at-ms]
                                                        (fog-deadline/next-cutoff (:safety-state source) checked-at-ms))
                               :prepare-safety-output (fn [buffers source] (prepare-safety-output @runtime-ref buffers source))
                               :send! (fn [buffers id] (present-output! @runtime-ref buffers id)))
                        (:physical-now-ms options) (assoc :now-ms (:physical-now-ms options))))
           runtime-id (str (UUID/randomUUID))
           runtime {:id runtime-id
      :event-admit (get options :event-admit subscribers/admit)
      :event-callbacks (event-callbacks/create
                        (merge {:trusted-source? (get-in options [:reactive-options :trusted-source?] false)}
                               (:event-callback-options options)))
      :effect-logs (effect-faults/create {:log! (:effect-log! options) :log-file (:fault-log-file options)})
      :state (atom {:model (effect-keyframes/without-checkpoints model)
                    :keyframes (effect-keyframes/restored model)
                    :output (render/resolve-frame patch [] {:include-values? (not batch?) :compact-values? batch?})
                    :status {}})
      :queue (LinkedBlockingQueue. 256) :cue-release-queue (LinkedBlockingQueue. 256)
      :ingress-order (volatile! 0) :lock (Object.) :running (atom true)
      :venue-fog (atom {}) :fog-deadlines (atom nil)
      :clock clock :sink sink :pause-layer-cache (volatile! nil)
      :adaptive-keyframes (:adaptive-keyframes options) :cue-pad-previews (cue-pads/manager)
      :capture-effect-samples? (:capture-effect-samples? options)
      :browser-observations browser-inputs
      :observers (atom {}) :frame-rate (get options :frame-rate 40)
      :publisher publisher :presentation-clock (volatile! 0)
      :independent-output? (boolean (:independent-output? options))
      :executor worker-pool
      :leases (atom {})
      :audio-returns (duplex/create) :open-audio-returns! (get options :open-audio-returns! duplex/open-java!)
      :variable-plan (volatile! nil) :scene-eligible (atom {}) :scene-plans (volatile! {})
      :capacity-plan (volatile! nil) :capacity-audio (volatile! nil)
      :audio-input (atom {:epoch (:epoch model) :events [] :features nil})
      :section-patterns music
      :sync-input (atom {:epoch (:epoch model) :events []})
      :connections owned :devices (atom {}) :development development
      :rdm (rdm/create (merge (:rdm-options options)
                             {:state rdm-state :model! #(get @(:state @runtime-ref) :model)
                              :identity! #(let [runtime @runtime-ref]
                                            {:runtime-id (:id runtime) :expected-epoch (get-in @(:state runtime) [:model :epoch])})
                              :publish! #(submit! @runtime-ref :rdm-update (merge %1 %2))}))
      :response-cache (atom {:responses [] :middleware identity})
      :compute compute-owner :compiler-context compiler-context :batch? batch?
      :parameter-precision (atom nil)
      :optics-plan (atom (reflections/plan (:rig model)))
      :patch (atom {:fixtures fixtures :compiled patch :retirement (retirement/plan nil nil patch)
                    :brightness-gains (preview/brightness-gains fixtures)})
      :compile (fn [definition]
                 (let [{:keys [compiler cache]} @compiler-context]
                   (or (get @cache definition)
                       (let [compiled (compiler definition)]
                         (swap! cache #(assoc (if (>= (count %) 256) {} %) definition compiled))
                         compiled))))}]
       (reset! runtime-ref runtime)
       runtime)))

(defn create-runtime
  "Create an unscheduled show. The CPU backend is the default. Explicit
  :compute-backend :native-gpu acquires one session owned by this show.
  :worker-timeout-ms sets the per-frame effect worker deadline (default 500).
  An enabled :section-patterns policy publishes pattern intents from opted-in
  capture at owner boundaries. Actions require their own event subscriber."
  ([] (create-runtime {}))
  ([options]
   (let [owner (open-compute! options)]
     (try (configure-runtime owner options)
          (catch Throwable error
            (try (compute/close! owner) (catch Throwable cleanup (.addSuppressed error cleanup)))
            (throw error))))))

(declare ^:private command)

(defn- authored-page-id [id]
  (when (= cue-pages/recent-page-id id)
    (throw (ex-info "Recent cues is a read-only page" {:page-id id})))
  id)

(defn- restart-model [model time-ms]
  (let [configs (mapv :config (remove :owner-instance-id (get-in model [:modulation :controllers])))
        clock (musical-time/frame-clock {:time-ms time-ms :transport (:transport model)})]
    (cond-> (-> model
                subscribers/restart
                spatial/clear-session
                (assoc :composition-runs {} :composition-tokens {} :modulation (modulation/new-state))
                (autopilot/clear-session time-ms command)
                (cond-> (get-in model [:extensions :tolglow.autopilot/hdr-clock])
                  (update :extensions dissoc :tolglow.autopilot/hdr-clock))
                (update :variables dissoc :lighting/autopilot :audio :audio/energy :lighting/activity :lighting/budget :sync :surface-aim)
                (update :responses #(mapv (fn [entry] (assoc entry :enabled? false :state (envelope/initial-state clock))) %)))
      (:auto model) (update :auto assoc :enabled? false :state nil :reasons [])
      (seq configs) (assoc-in [:settings :modulation :saved-controllers] configs)
      (:routing model) (routing/restart time-ms)
      (:sensors model) (update-in [:sensors :sources]
                                #(into {} (map (fn [[id source]]
                                                 [id (-> source (assoc :armed-intent? false)
                                                         (update :state (fn [state] (:state (sensor-core/set-armed state false)))))])) %)))))

(defn- command [model payload time-ms]
  (let [time-ms (or (get-in model [:control :history (:command-id payload) :command :time-ms]) time-ms)
        payload (prefabs/prepare-command model payload)
        ;; Declarative code may use string identities (generated pages do).
        ;; JSON keyword normalization must still address those existing objects.
        payload (reduce (fn [p [field entries]]
                          (let [id (get p field) s (when (keyword? id) (subs (str id) 1))]
                            (if (and s (not (contains? entries id)) (contains? entries s))
                              (assoc p field s) p))) payload
                        [[:cue-id (get-in model [:registry :cues])]
                         [:page-id (:pages model)]])]
    (let [result (spatial/apply-command model
                          (merge {:command-id (str (UUID/randomUUID))
                                 :show-id (:show-id model) :epoch (:epoch model)
                                 :source {:adapter :studio :session-id "studio"}}
                                 payload {:time-ms time-ms}))
          result (if (and (= :accepted (get-in result [:ack :status]))
                          (= :show/restart (:op payload)) (not= (:epoch model) (get-in result [:model :epoch])))
                   (update result :model
                           (fn [next-model]
                             (cond-> (restart-model next-model time-ms)
                               (contains? model ::staging/pending)
                               (as-> m (staging/collect (staging/begin m) :control
                                                        {:kind :command :id (or (get-in payload [:source :adapter]) :studio)}
                                                        (:events result))))))
                   result)]
      (if (and (= :accepted (get-in result [:ack :status])) (= :param/set (:op payload))
               (or (seq (get-in model [:modulation :controllers]))
                   (get-in model [:instances (:instance-id payload) :parameter-bindings (:param payload)]))
               (not= :patch (get-in payload [:source :type]))
               (not= :live-playback (get-in payload [:source :origin])))
        (update result :model update :modulation (fnil modulation/hold (modulation/new-state))
                {:instance-id (:instance-id payload) :params [(:param payload)] :until-beat nil})
        result))))

(defn- apply-sensor-result [result time-ms]
  (loop [model (:model result) pending (seq (:commands result)) receipts []]
    (if-let [intent (first pending)]
      (let [applied (command model (assoc intent :command-id (str (UUID/randomUUID))
                                                :epoch (:epoch model) :show-id (:show-id model)) time-ms)
            ack (:ack applied)
            follow (if-let [gesture (:gesture-id intent)]
                     (sensors/acknowledge (:model applied) (get-in intent [:source :device-id]) gesture ack)
                     {:model (:model applied) :commands []})]
        (recur (:model follow) (concat (next pending) (:commands follow))
               (conj receipts (select-keys ack [:status :instance-id :reason :message]))))
      (-> model
          (as-> m (reduce (fn [acc event]
                            (staging/collect acc :sensor {:kind :device :id (or (:device-id event) :sensor)} [event]))
                          m (:events result)))
          (assoc-in [:variables :sensors :events] (vec (take-last 64 (:events result))))
          (assoc-in [:variables :sensors :receipts] (vec (take-last 64 receipts)))))))

(defn- advance-sensors! [runtime model time-ms]
  (if (seq (get-in model [:sensors :sources]))
    (apply-sensor-result (sensors/advance! (connections/sensors (:connections runtime)) model
                                         (musical-time/frame-clock {:time-ms time-ms :transport (:transport model)})) time-ms)
    model))

(declare ^:private auto-plans)

(defn- edit-scoped-settings [model scope values device-id]
  (let [current (get-in model [:settings scope] {})]
    (cond
      (= scope :spatial) (spatial/edit-settings current values)
      (and (= scope :expressive) device-id)
      (let [id (or (some #(when (= (keyword %) (keyword device-id)) %) (keys (:devices current)))
                   (keyword device-id))]
        (assoc-in current [:devices id] values))
      :else (lighting/edit-settings scope current values))))

(defn- sensor-edit [model]
  (let [updated (update model :revision inc)]
    {:model updated :ack {:status :accepted :revision (:revision updated)}}))

(defn- cue-clock-identity [runtime {:keys [show-id epoch transport]}]
  [show-id epoch (:source transport) (:source-epoch transport)
   (:source-selection-epoch transport) @(:presentation-clock runtime)])

(defn- dispatch-owner-command [runtime model payload time-ms]
  (cond
    (and (= :cue/launch (:op payload)) (contains? payload :grid))
    (cue-timing/schedule model (cue-clock-identity runtime model) time-ms
                         (assoc (select-keys payload [:grid :raw]) :command (dissoc payload :grid :raw))
                         command)

    (and (cue-timing/active? model) (cue-timing/receipt model (:command-id payload)))
    {:model model :ack {:status :rejected :reason :command-id-conflict :command-id (:command-id payload)}}

    (= :cue/cancel-launch (:op payload))
    (cue-timing/cancel-owned model (cue-clock-identity runtime model) time-ms payload command)

    :else (command model payload time-ms)))

(defn- apply-message [model {:keys [type payload expected-epoch]} time-ms & [runtime]]
  (when (and (some? expected-epoch) (not= expected-epoch (:epoch model)))
    (throw (ex-info "Operation belongs to a previous show session" {:expected-epoch (:epoch model)
                                                                    :received-epoch expected-epoch})))
  (case type
    :command (dispatch-owner-command runtime model payload time-ms)
    :cue-schedule (cue-timing/schedule model (cue-clock-identity runtime model) time-ms payload command)
    :cue-cancel (cue-timing/cancel model (cue-clock-identity runtime model) payload)
    :spatial-cue (spatial-cues/spawn model payload time-ms command)
    :autopilot-authored (autopilot/accept-authored model payload time-ms command)
    :lighting-scene {:model (lighting-scenes/apply-request model payload time-ms) :ack {:status :accepted}}
    :event-subscriber (let [inline-registration (::inline-registration payload)]
                        (when (and inline-registration (nil? (get-in runtime [:event-callbacks :inline-handlers])))
                          (throw (ex-info "Restart the runtime to enable process-local callback registrations" {})))
                        {:model (subscribers/configure model payload)
                         :ack {:status :accepted}
                         :inline-registration inline-registration})
    :rdm-update (do (when (and (:rdm-session-id payload)
                              (not (rdm/session-current? (:rdm runtime) (:rdm-session-id payload))))
                      (throw (ex-info "RDM result belongs to a retired device session" {})))
                    {:model (rdm/update-model model payload) :ack {:status :accepted}})
    :venue-surface (do
                     (when (or (:armed? (calibration/current model)) (photometry/current model) (fog-scan/current model))
                       (throw (ex-info "Finish the active calibration before changing the rig" {})))
                     (venue-scan/admit model payload))
    :fog-calibration (fog-scan/operation model payload ((:now-ms (:publisher runtime))))
    :photometry (photometry/apply-operation model payload ((:now-ms (:publisher runtime)))
                                           (and (:independent-output? runtime) (some? (get-in runtime [:sink :send!]))))
    :calibration (do (when (and (= :start (:op payload)) (photometry/current model))
                       (throw (ex-info "Finish photometry calibration first" {})))
                     (calibration/apply-operation model payload time-ms ((:now-ms (:publisher runtime)))
                                              (and (:independent-output? runtime) (some? (get-in runtime [:sink :send!])))))
    :sensor-audience-zone
    (sensor-edit (audience-zones/select-zone model (:device-id payload) (:included? payload)))
    :sensor-configure
    {:model (apply-sensor-result
             (sensors/configure model (:device-id payload)
                                (if (:source payload) (data/read-edn (:source payload)) (:config payload))
                                (or (:pairing payload) (get-in model [:sensors :sources (:device-id payload) :pairing]))) time-ms)
     :ack {:status :accepted}}
    :sensor-arm
    (sensor-edit (apply-sensor-result (sensors/arm! (connections/sensors (:connections runtime)) model
                                                   (:device-id payload) (:session-id payload) (:armed? payload)) time-ms))
    :sensor-calibrate
    {:model (apply-sensor-result (sensors/calibrate (connections/sensors (:connections runtime)) model (:device-id payload)) time-ms)
     :ack {:status :accepted}}
    :patch-save
    {:model (routing/save model (data/read-edn (:source payload)) time-ms)
     :ack {:status :accepted}}
    :patch-enabled
    {:model (routing/set-enabled model (:enabled? payload) time-ms)
     :ack {:status :accepted}}
    :definition
    (let [{:keys [kind definition]} payload
          previous (get-in model [:registry kind (:id definition)])
          _ (when (and (contains? payload :expected-version) (not= (:expected-version payload) (:version previous)))
              (throw (ex-info "Definition changed since editing began" {:current-version (:version previous)})))
          definition (if-let [edit (:output-patch payload)]
                       (do (when-not (and (= kind :effects) previous (contains? payload :expected-version))
                             (throw (ex-info "An output patch needs the current effect revision" {})))
                           (output-patch/apply-patch previous edit)) definition)
          definition (cond-> definition
                       (or (contains? payload :expected-version) (nil? (:version definition)))
                       (assoc :version (inc (get previous :version 0))))
          errors (registry/validate-definition kind definition)]
      (when (seq errors) (throw (ex-info "Invalid definition" {:errors errors})))
      (when (= :effects kind) (compiler/compile-effect (prefabs/capture (prefabs/catalog model) definition)))
      (update (command model {:op :registry/publish :kind kind :definition definition} time-ms)
              :ack merge {:kind kind :id (:id definition) :version (:version definition)}))
    :material-edit
    (let [{:keys [settings] :as result} (material-design/edit model payload)]
      (if settings
        (apply-message model {:type :settings :payload settings} time-ms runtime)
        result))
    :agent-cue
    (let [cue (:cue payload)
          page-id (authored-page-id (:page-id payload :agent))
          errors (registry/validate-definition :cues cue)
          _ (when (seq errors) (throw (ex-info "Invalid cue" {:errors errors})))
          result (command model (merge (select-keys payload [:command-id :source :epoch])
                                       {:op :cue/put :cue cue}) time-ms)]
      (if (or (= :rejected (get-in result [:ack :status]))
              (some (fn [page] (some #{(:id cue)} (vals (:cells page)))) (vals (:pages (:model result)))))
        result
        (let [page (get-in result [:model :pages page-id] {:id page-id :name "Created cues" :cells {}})
              position (first (for [y (range 8) x (range 8) :when (not (contains? (:cells page) [x y]))] [x y]))]
          (if position
            (update result :model assoc-in [:pages page-id] (assoc-in page [:cells position] (:id cue)))
            (update result :ack assoc :page-full? true)))))
    :rig
    (let [{:keys [basis rig]} payload]
      (when-not (identical? basis (:rig model))
        (throw (ex-info "Rig changed while editing; reload the current patch before saving" {})))
      {:model (cond-> (-> model (assoc :rig rig) (update :revision inc))
                (get-in model [:variables :surface-aim])
                (update-in [:variables :surface-aim] select-keys
                           (for [{:keys [id channels]} (:fixtures rig) :when (and (:pan channels) (:tilt channels))] id)))
       :ack {:status :accepted}})
    :fixture-patch
    (let [types (get-in model [:registry :fixtures])
          id (:fixture-type-id payload)
          definition (or (get types id) (when (string? id) (get types (keyword id))))
          _ (when-not definition (throw (ex-info "Fixture type not found" {:id id})))
          result (fixture-service/instantiate definition payload)
          _ (when-not (:valid? result) (throw (ex-info "Fixture cannot be patched" (select-keys result [:diagnostics]))))
          fixture (:fixture result)
          fixtures (get-in model [:rig :fixtures])
          _ (when (some #(= (:id fixture) (:id %)) fixtures)
              (throw (ex-info "Fixture ID already exists" {:id (:id fixture)})))
          fixtures (conj (vec fixtures) fixture)
          _ (render/compile-patch fixtures)]
      {:model (-> model (assoc-in [:rig :fixtures] fixtures) (update :revision inc))
       :ack {:status :accepted :fixture-id (:id fixture)}})
    :composition {:model (composition/save model payload) :ack {:status :accepted}}
    :composition-run {:model (composition/start model (:id payload) time-ms) :ack {:status :accepted}}
    :composition-stop {:model (composition/stop model (:id payload) time-ms) :ack {:status :accepted}}
    :response
    (let [definition (assoc payload :enabled? true :state (envelope/initial-state
                                            (musical-time/frame-clock {:time-ms time-ms :transport (:transport model)})))
          _ (response/compile-response definition)
          responses (conj (vec (remove #(= (:id definition) (:id %)) (:responses model))) definition)]
      {:model (-> model (assoc :responses responses) (update :revision inc)) :ack {:status :accepted}})
    :response-release
    {:model (-> model
                (update :responses (fn [responses]
                                     (mapv #(if (= (:id payload) (:id %))
                                              (update % :state (fn [state]
                                                                 (envelope/release (:envelope %) state
                                                                                   (musical-time/frame-clock
                                                                                    {:time-ms time-ms :transport (:transport model)}))))
                                              %) responses)))
                (update :revision inc)) :ack {:status :accepted}}
    :response-delete
    {:model (-> model (update :responses #(vec (remove (fn [r] (= (:id payload) (:id r))) %)))
                (update :revision inc)) :ack {:status :accepted}}
    :page-generate
    (let [id (authored-page-id (:page-id payload :generated))
          page (get-in model [:pages id] {:id id :name "Effect registry" :cells {}})
          result (registry/reconcile-page (:registry model) page (:options payload {}))]
      {:model (-> model (assoc :registry (:registry result)) (assoc-in [:pages id] (:page result))
                  (update :revision inc))
       :ack (assoc (select-keys result [:placements :collisions :overflow]) :status :accepted :page-id id)})
    (:modulation-bind :modulation-unbind)
    (let [edit (if (= type :modulation-bind) parameter-sources/bind parameter-sources/unbind)]
      {:model (update (edit model payload) :revision inc)
       :ack {:status :accepted :instance-id (:instance-id payload) :param (:param payload)}})
    :modulation-start
    {:model (-> model (update :modulation #(modulation/start (or % (modulation/new-state)) payload
                                                            {:time-ms time-ms :transport (:transport model)}))
                (update :revision inc)) :ack {:status :accepted :id (:id payload)}}
    :waveform
    (let [definition (data/read-edn (:source payload))
          next-registry (signals/publish (or (:waveforms model) (signals/new-registry)) definition)]
      {:model (-> model (assoc :waveforms next-registry) (update :revision inc))
       :ack {:status :accepted :id (:id definition) :version (:version definition)}})
    :modulation-stop
    (let [id (:id payload)
          id (if (and (vector? id) (= "cue" (first id))) (assoc id 0 :cue) id)
          controller (some #(when (= id (:id %)) %) (get-in model [:modulation :controllers]))]
      (when-not controller (throw (ex-info "Modulator is not active" {:id id})))
      (if-let [owner (:owner-instance-id controller)]
        (command model {:op :instance/end :instance-id owner} time-ms)
        {:model (-> model (update :modulation modulation/stop id) (update :revision inc))
         :ack {:status :accepted}}))
    :modulation-hold
    {:model (-> model (update :modulation #(modulation/hold (or % (modulation/new-state)) payload))
                (update :revision inc)) :ack {:status :accepted}}
    :modulation-resume
    {:model (-> model (update :modulation #(modulation/release-hold (or % (modulation/new-state))
                                                                  (:instance-id payload) (:params payload)))
                (update :revision inc)) :ack {:status :accepted}}
    :surface-aim (surface-aim/apply-operation model payload)
    :aim-path
    (let [definition (aim-paths/definition model payload)
          id (:id definition)
          requested-cue-id (:cue-id payload id)
          wire-id (fn [v] (if (keyword? v) (subs (str v) 1) (str v)))
          previous (some #(when (= (wire-id requested-cue-id) (wire-id (:id %))) %)
                         (vals (get-in model [:registry :cues])))
          cue-id (:id previous requested-cue-id)
          _ (when (and previous (not= id (:effect-id previous)))
              (throw (ex-info "That cue belongs to another effect; choose a new cue ID" {:cue-id cue-id})))
          _ (compiler/compile-effect definition)
          published (command model {:op :registry/publish :kind :effects :definition definition} time-ms)
          _ (when (= :rejected (get-in published [:ack :status]))
              (throw (ex-info "Cannot publish aim path" (:ack published))))
          cue (merge previous {:id cue-id :effect-id id :name (:name definition)
                               :version (aim-paths/next-version (:registry model) :cues cue-id) :reload-policy :preserve-state})
          result (apply-message (:model published)
                                {:type :agent-cue :payload {:page-id (:page-id payload :paths) :cue cue}} time-ms)]
      (when (= :rejected (get-in result [:ack :status]))
        (throw (ex-info "Cannot publish aim path cue" (:ack result))))
      (update result :ack assoc :effect-id id :cue-id cue-id :version (:version definition)))
    :modulation-cue
    (let [id (:id payload) config (:modulation payload)
          previous (get-in model [:registry :effects id])
          definition {:id id :version (inc (:version previous 0)) :name (:name payload "Parameter rhythm")
                      :params {} :outputs {} :modulation config :categories [:modulation]
                      :tags [:modulation] :duration {:bars 4}}
          _ (compiler/compile-effect definition)
          published (command model {:op :registry/publish :kind :effects :definition definition} time-ms)
          _ (when (= :rejected (get-in published [:ack :status]))
              (throw (ex-info "Cannot publish modulation effect" (:ack published))))
          cue-result (apply-message (:model published)
                                    {:type :agent-cue :payload {:page-id (:page-id payload :agent)
                                                               :cue {:id id :effect-id id :name (:name definition)
                                                                    :version (inc (get-in model [:registry :cues id :version] 0))
                                                                    :reload-policy :preserve-state}}} time-ms)]
      (update cue-result :ack assoc :effect-id id :cue-id id))
    :variable-block (variable-edit/apply-edit model payload)
    :audio-return-configure
    (apply-message model {:type :settings :payload {:scope :audio-returns :values (:config payload)}} time-ms runtime)
    :settings
    (let [{:keys [scope values]} payload]
      (when-not (and (keyword? scope) (map? values) (registry/data? values))
        (throw (ex-info "Settings require a named scope and portable values" {})))
      (when (and (= scope :output) (contains? values :presentation))
        (output-scheduler/validate-options (:presentation values)))
      (let [values (cond-> values (and (= scope :output) (contains? values :pause-look))
                     (update :pause-look pause-look/normalize))
            settings (edit-scoped-settings model scope values (:device-id payload))
            _ (when (= scope :prefabs)
                (prefabs/validate! settings)
                (variable-blocks/compile-settings (get-in model [:settings :variable-blocks]) (:definitions settings)))
            _ (when (= scope :variable-blocks)
                (variable-blocks/compile-settings settings (prefabs/catalog model)))
            _ (when (= scope :adaptive-keyframes) (cadence/settings settings))
            _ (when (= scope :studio) (registry/validate-public-query-ids! (:public-queries settings)))
            _ (when (= scope :lighting-capacity) (capacity/settings settings))
            _ (when (= scope :audio-returns) (audio-return/validate-config model settings))
            settings (if (= scope :expressive) (expressive-mapping/normalize-config settings) settings)
            updated (-> (if (= scope :fog-control)
                          (fog-control/configure model values)
                          (assoc-in model [:settings scope] settings))
                        (update :revision inc))]
        {:model updated :ack {:status :accepted :revision (:revision updated)}}))
    :auto
    (let [old-state (get-in model [:auto :state])
          model (if old-state
                  (reduce (fn [m c] (:model (command m c time-ms))) model
                          (:commands (policy/stop-plan model old-state time-ms))) model)
          enabled? (true? (:enabled? payload))
          config (dissoc payload :enabled? :seed :expected-epoch)
          state (when enabled? (policy/initial-state (:seed payload 0) time-ms (str (UUID/randomUUID))))]
      (when enabled?
        (let [fixtures (get-in model [:rig :fixtures])
              ;; An earlier queued rig edit may still be provisional. Validation
              ;; can reuse a matching cache without changing the output patch.
              plans (when (and runtime (= fixtures (:fixtures @(:patch runtime)))) (auto-plans runtime fixtures))]
          (policy/advance (assoc model :fixtures fixtures) config state time-ms plans)))
      {:model (-> model (assoc :auto {:enabled? enabled? :config config :state state}) (update :revision inc))
       :ack {:status :accepted}})
     :import
    {:model (staging/collect (staging/begin (:model payload)) :show {:kind :studio :id :import}
                             [{:type :show/restored}])
     :restore? true :restored-output (:frame-output payload)
     :candidate (:candidate payload) :ack {:status :accepted :epoch (get-in payload [:model :epoch])}}
    (throw (ex-info "Unknown studio operation" {:type type}))))

(defn- patch! [runtime fixtures]
  (let [cached @(:patch runtime)]
    (if (= fixtures (:fixtures cached)) (:compiled cached)
        (let [patch (render/compile-patch fixtures)]
          (reset! (:patch runtime) {:fixtures fixtures :compiled patch :brightness-gains (preview/brightness-gains fixtures)
                                  :retirement (retirement/plan (:retirement cached) (:compiled cached) patch)})
          patch))))

(defn- capacity-plan! [runtime patch]
  (let [cache (:capacity-plan runtime)]
    (when-not (identical? patch (:patch @cache))
      (vreset! cache {:patch patch :plan (capacity/prepare patch)}))
    (:plan @cache)))

(defn- measure-sources [runtime {:keys [model output]} ids]
  (when (seq (:universes output))
    (let [fixtures (get-in model [:rig :fixtures])
          cached @(:patch runtime)
          patch (if (identical? fixtures (:fixtures cached)) (:compiled cached) (render/compile-patch fixtures))
          meter @(:capacity-plan runtime)
          plan (if (identical? patch (:patch meter)) (:plan meter) (capacity/prepare patch))]
      (capacity/measure-sources plan (:universes output) ids))))

(defn source-brightness
  "Measure selected fixture IDs from one committed frame, on their own and rig-wide full-white references."
  [runtime ids]
  (measure-sources runtime @(:state runtime) ids))

(defn- advance-light-scenes [runtime model patch frame now-ms bypass?]
  (if (empty? (lighting-scenes/groups model))
    (do (when (seq @(:scene-plans runtime)) (vreset! (:scene-plans runtime) {})) model)
    (let [cache (:scene-plans runtime)
          next-cache (lighting-scenes/meter-frame @cache patch (lighting-scenes/groups model) (:universes frame))
          measured (update-vals (:plans next-cache) :measurement)]
      (vreset! cache next-cache)
      (lighting-scenes/feedback model measured now-ms bypass?))))

(defn- response-middleware! [runtime responses]
  (let [cached @(:response-cache runtime)]
    (if (= responses (:responses cached)) (:middleware cached)
      (let [middleware (response/compile-middleware (filterv #(not= false (:enabled? %)) responses))]
        (reset! (:response-cache runtime) {:responses responses :middleware middleware}) middleware))))

(defn- prepare-compiler! [runtime fixtures]
  (when-not (= fixtures (:fixtures @(:compiler-context runtime)))
    (reset! (:compiler-context runtime)
            (compiler-context fixtures (:batch? runtime) (:compute runtime)))))

(defn- advance-transport [model time-ms]
  (let [transport (:transport model)
        previous (:time-ms transport (:time-ms model 0))
        delta (max 0 (- time-ms previous))
        beat (+ (:beat transport 0.0) (if (:playing? transport) (* delta (/ (:bpm transport) 60000.0)) 0.0))]
    (update model :transport assoc :time-ms time-ms :beat beat)))

(defn- auto-plans [runtime fixtures]
  (delay
    (let [patch (patch! runtime fixtures)]
      {:patch patch :meter (delay (capacity-plan! runtime patch))
       :previous-budget (get-in @(:state runtime) [:lighting-history :budget])})))

(defn- advance-auto [model time-ms runtime]
  (if (get-in model [:auto :enabled?])
    (let [fixtures (get-in model [:rig :fixtures])
          {:keys [state commands reasons]} (policy/advance (assoc model :fixtures fixtures)
                                              (get-in model [:auto :config]) (get-in model [:auto :state])
                                              time-ms (auto-plans runtime fixtures))
          [model state] (reduce (fn [[m s] c]
                                  (let [{:keys [model ack]} (command m c time-ms)]
                                    [model (if (and (= :cue/launch (:op c)) (= :accepted (:status ack)) (:instance-id ack))
                                             (policy/record-launch s (:instance-id ack)) s)]))
                                [model state] commands)]
      (-> model (assoc-in [:auto :state] state) (assoc-in [:auto :reasons] reasons))) model))

(defn- prepare-output [runtime {:keys [compiled retirement]} universes {:keys [safety-state stale-source?]}]
  (let [packet (retirement/include-retired retirement universes)]
    (if (seq safety-state)
      (fog-deadline/guard-source @(:fog-deadlines runtime) compiled packet safety-state
                                 ((get-in runtime [:publisher :now-ms])) stale-source?)
      packet)))

(defn- prepare-safety-output [runtime universes {:keys [output-context stale-source? uncertain?] :as source}]
  (let [{:keys [compiled retirement]} output-context
        off-bytes (:fog-off-bytes compiled)
        corrected (prepare-output runtime output-context universes source)
        corrected (if (and (or stale-source? uncertain?) (seq off-bytes))
                    ;; Old light bytes may now address a pump. Recover it using
                    ;; the current patch, even if its universe was absent before.
                    (let [missing (into #{} (comp (map (comp first key)) (remove #(contains? corrected %))) off-bytes)
                          packet (if (seq missing) (merge corrected (select-keys (render/idle-universes compiled) missing)) corrected)]
                      (fog-deadline/apply-off-bytes packet off-bytes))
                    corrected)]
    ;; A transport can fail after writing only part of the attempted packet.
    (when (or (and uncertain? (or (seq off-bytes) (seq (:held retirement))))
              (and (not (identical? universes corrected))
                   (some (fn [[universe ^bytes bytes]]
                           (not (Arrays/equals ^bytes (get universes universe) bytes)))
                         corrected)))
      corrected)))

(defn- present-output! [runtime packet frame-number]
  ((get-in runtime [:sink :send!]) packet frame-number)
  (when (:plan @(:venue-fog runtime))
    (venue-fog/presented! (:venue-fog runtime) ((get-in runtime [:publisher :now-ms])) packet)))

(defn- send-output! [runtime universes frame-number fog-demands]
  (when (get-in runtime [:sink :send!])
    (present-output! runtime (prepare-output runtime @(:patch runtime) universes {:safety-state fog-demands}) frame-number)))

(defn audio-callbacks
  "Capture epoch-bound callbacks for explicitly started audio. Features use a
  coalescing mailbox and events a bounded queue; only tick! writes show state."
  [runtime]
  (let [epoch (get-in @(:state runtime) [:model :epoch])
        current? #(and @(:running runtime) (= epoch (get-in @(:state runtime) [:model :epoch])))]
    {:clock! (fn []
               (let [{:keys [model audio-clock-confidence section-clock-session]} @(:state runtime)
                     {:keys [source time-ms] :as transport} (:transport model)
                     confidence (if (= :manual source) 1.0 audio-clock-confidence)]
                 (when (and @(:running runtime) (= epoch (:epoch model))
                            (some? time-ms) (some? source) (some? confidence))
                   (cond-> (assoc transport :confidence confidence)
                     section-clock-session (assoc :session-id section-clock-session)))))
     :events! (fn [events]
                (if-not (current?) false
                  (let [accepted? (volatile! false)]
                    (locking (:audio-input runtime)
                      (let [state @(:audio-input runtime)]
                        (when (and (= epoch (:epoch state)) (<= (+ (count (:events state)) (count events)) 256))
                          (reset! (:audio-input runtime) (update state :events into events))
                          (vreset! accepted? true))))
                    @accepted?)))
     :features! (fn [features]
                  (when (current?)
                    (locking (:audio-input runtime)
                      (when (= epoch (:epoch @(:audio-input runtime)))
                        (swap! (:audio-input runtime) assoc :features features
                               :arrived-at-ms ((get-in runtime [:publisher :now-ms])))))))
     :done! (fn [result]
              (when (current?)
                (swap! (:state runtime)
                       #(if (= epoch (get-in % [:model :epoch]))
                          (assoc-in % [:status :audio-result] (select-keys result [:status :error])) %))))}))

(defn- consume-audio! [runtime model time-ms]
  (let [captured (locking (:audio-input runtime)
                   (let [before @(:audio-input runtime)]
                     (reset! (:audio-input runtime) {:epoch (:epoch model) :events [] :features nil})
                     before))]
    (when (and (:features captured) (= (:epoch model) (:epoch captured)))
      (let [age (max 0 (- ((get-in runtime [:publisher :now-ms])) (:arrived-at-ms captured)))]
        (vreset! (:capacity-audio runtime) {:epoch (:epoch model) :received-ms (- time-ms age)
                                           :audio/energy (get-in captured [:features :audio/energy])})))
    (if (= (:epoch model) (:epoch captured))
      (cond-> model
        (:features captured) (routing/admit-audio (:features captured))
        (:features captured) (analysis/publish-features (:features captured))
        (seq (:events captured)) (as-> m (reduce
            (fn [acc event] (staging/collect acc :audio {:kind :audio :id (or (get-in event [:source :id]) :analysis)} [event]))
            m (:events captured)))
        (seq (:events captured)) (assoc-in [:variables :audio :events] (:events captured))
        (seq (:events captured)) (update-in [:variables :audio :event-count] (fnil + 0) (count (:events captured)))) model)))

(defn- advance-section-patterns [runtime before next-state reset?]
  (if-let [initial (:section-patterns runtime)]
    (if reset?
      (dissoc next-state :section-pattern-state :section-clock-session)
      (let [{:keys [model audio-clock-confidence]} next-state
            previous (:model before)
            {:keys [source source-epoch source-selection-epoch playing? beat]} (:transport model)
            clock-session (str (:epoch model) "/" source-selection-epoch "/" @(:presentation-clock runtime))
            connection (:connections runtime)
            capture-status (audio/status (connections/audio connection))
            {capture-state :state :keys [run-id timing-valid? closed?]} capture-status
            analyzer-options (get-in capture-status [:config :analyzer-options])
            active-clock? (and (true? playing?) (integer? source-epoch) (number? audio-clock-confidence)
                               (>= audio-clock-confidence (get analyzer-options :beat-clock-confidence-min 0.5)))
            stream (when (and run-id timing-valid? (not closed?) (#{:running :completed} capture-state)
                              (connections/restart-requested? connection :audio))
                     {:source-id (get analyzer-options :source-id :audio-input) :session-id run-id})
            causal-events (into [] (comp (filter #(= :audio (:category %))) (map :event)
                                         (filter #(and (= :audio/section-change (:type %))
                                                       (= run-id (:capture-run-id %))
                                                       (true? (get-in % [:timing :capture-clock-valid?])))))
                                (staging/pending model))
            boundaries (when (and active-clock? (= clock-session (:section-clock-session before))
                                  (true? (get-in previous [:transport :playing?])))
                         (into #{} (map (comp keyword name :type))
                               (show-events/emitted {:before previous :after model})))
            result (section-patterns/advance (or (:section-pattern-state before) initial)
                     {:now-ms (:time-ms model) :causal-stream stream :causal-events causal-events
                      :transport {:show-epoch (:epoch model) :source (or source :manual)
                                  :source-epoch (or source-epoch 0) :session-id clock-session
                                  :playing? (boolean active-clock?) :beat beat :boundaries boundaries}})]
        (-> next-state
            (assoc :section-pattern-state (:state result) :section-clock-session clock-session)
            (update :model staging/collect :music {:kind :music :id :section-patterns} (:events result)))))
    next-state))

(declare submit!)

(defn- sync-envelope [runtime epoch]
  (let [model (:model @(:state runtime))]
    (when (and @(:running runtime) (= epoch (:epoch model)))
      {:command-id (str (UUID/randomUUID)) :show-id (:show-id model)
       :epoch epoch :time-ms (now-ms runtime)})))

(defn- acknowledge-sync! [runtime manager command ack epoch]
  (when (and (= :cue/launch (:op command)) (some? (get-in command [:source :note])))
    (when-let [envelope (sync-envelope runtime epoch)]
      (live-sync/acknowledge! manager (get-in command [:source :note])
                         (assoc ack :sync-run-id (get-in command [:source :sync-run-id])) envelope))))

(defn sync-callbacks
  "Bounded external transport callbacks. Commands enter the same owner queue;
  the callback never waits for rendering or writes the show model."
  [runtime]
  (let [epoch (get-in @(:state runtime) [:model :epoch])
        manager (connections/sync (:connections runtime))
        current? #(and @(:running runtime) (= epoch (get-in @(:state runtime) [:model :epoch])))]
    {:envelope! #(sync-envelope runtime epoch)
     :pads (fn [mappings] (ableton/mapped-pads (get-in @(:state runtime) [:model :registry]) mappings))
     :resolve-instance! (fn [cue-id]
                          (when (current?)
                            (->> (vals (get-in @(:state runtime) [:model :instances]))
                                 (filter #(and (= cue-id (:cue-id %)) (= :running (:phase %))))
                                 (sort-by :order) last :id)))
     :events! (fn [event]
                (when (current?)
                  (locking (:sync-input runtime)
                    (let [before @(:sync-input runtime)]
                      (when (and (= epoch (:epoch before)) (< (count (:events before)) 256))
                        (reset! (:sync-input runtime) (update before :events conj event)) true)))))
     :commands! (fn [commands]
                  (if-not (current?) false
                    (reduce (fn [accepted? command]
                              (let [reply (submit! runtime (assoc command :expected-epoch epoch))]
                                (if (realized? reply)
                                  (let [ack @reply]
                                    (when (= :rejected (:status ack))
                                      (acknowledge-sync! runtime manager command ack epoch))
                                    (and accepted? (not= :rejected (:status ack))))
                                  accepted?))) true commands)))}))

(defn- pause-layer! [runtime model]
  (let [look (get-in model [:settings :output :pause-look])]
    (when (and (false? (get-in model [:transport :playing?])) (= :ambient (:mode look)))
      (let [fixtures (get-in model [:rig :fixtures])
            cache (:pause-layer-cache runtime)
            previous @cache]
        (if (and (identical? fixtures (:fixtures previous)) (= look (:look previous)))
          (:block previous)
          (let [block (pause-look/ambient-layer fixtures look)]
            (vreset! cache {:fixtures fixtures :look look :block block})
            block))))))

(defn- source-clock-model [runtime model time-ms]
  (let [{:keys [selected-source selection-epoch clock follow-transport?]}
        (session/clock-at (connections/sync (:connections runtime)) time-ms)
        model (assoc-in model [:transport :source-selection-epoch] selection-epoch)]
    (if-let [captured (cond
                        (= :manual selected-source)
                        (assoc (:transport (advance-transport model time-ms))
                               :source :manual :source-epoch 0 :confidence 1.0)

                        (and (false? follow-transport?) (or (nil? clock) (:stale? clock)))
                        ;; Tempo-only Link must never change local playback just
                        ;; because a peer has not arrived or has disconnected.
                        (assoc (:transport (advance-transport model time-ms))
                               :source selected-source :source-epoch nil :confidence nil)

                        clock
                        (cond-> (assoc clock :source selected-source)
                          (false? follow-transport?) (assoc :playing? (get-in model [:transport :playing?]))
                          (and (false? follow-transport?) (false? (get-in model [:transport :playing?])))
                          (assoc :beat (get-in model [:transport :beat]))))]
      (let [before (:transport model)
            discontinuity? (or (not= (:source before :manual) (:source captured))
                               (not= (:source-epoch before 0) (:source-epoch captured))
                               (< (:beat captured) (:beat before))
                               (and (some? follow-transport?) (:playing? captured)
                                    (or (false? (:playing? before)) (:link-phase-held? before))))
            confidence (:confidence captured)
            captured (select-keys captured [:time-ms :beat :bpm :playing? :meter :source :source-epoch])
            ;; Rebase from the original model once, before applying the captured beat.
            model (if discontinuity? (show-clock/rebase model captured) (update model :transport merge captured))]
        (when discontinuity? (vswap! (:presentation-clock runtime) inc))
        ;; A local Play command may already have flipped :playing? before the
        ;; subsequent Link sample arrives. Remember that musical anchors were
        ;; frozen until we have the resumed peer phase to rebase them against.
        [(if (and (false? follow-transport?) (false? (:playing? captured)))
           (assoc-in model [:transport :link-phase-held?] true)
           (update model :transport dissoc :link-phase-held?))
         confidence])
      [(update model :transport assoc :time-ms time-ms :playing? false
               :source selected-source :source-epoch nil)
       nil])))

(defn- consume-sync! [runtime model]
  (let [captured (locking (:sync-input runtime)
                   (let [before @(:sync-input runtime)]
                     (reset! (:sync-input runtime) {:epoch (:epoch model) :events []}) before))]
    (if (and (= (:epoch model) (:epoch captured)) (seq (:events captured)))
      (-> (reduce (fn [acc event]
                    (staging/collect acc :sync {:kind :sync :id (or (:source-id event) :sync)} [event]))
                  model (:events captured))
          (assoc-in [:variables :sync :events] (:events captured))
          (routing/admit-sync (:events captured))) model)))

(defn- observe-sync-commands! [runtime before after receipts events fired-outcomes]
  (let [manager (connections/sync (:connections runtime))
        old-ids (if (= (:epoch before) (:epoch after))
                  (set (get-in before [:control :history-order])) #{})
        epoch (:epoch after)
        fired (filter #(= epoch (get-in % [:ack :epoch])) fired-outcomes)
        skip-ids (into old-ids (map #(get-in % [:ack :command-id])) fired)
        history (get-in after [:control :history])
        from-history (keep #(when-not (skip-ids %) (get history %)) (get-in after [:control :history-order]))
        outcomes (if (seq fired) (sort-by #(get-in % [:ack :revision]) (concat fired from-history)) from-history)]
    (doseq [[_ ack command] receipts :when (#{:rejected :pending :cancelled} (:status ack))]
      (acknowledge-sync! runtime manager command ack epoch))
    ;; Include automation, composition and controller commands, not only HTTP.
    (doseq [{:keys [command ack]} outcomes]
      (let [instance (get-in after [:instances (:instance-id ack)])
            timing (or (:duration instance) (when-let [ms (:duration-ms instance)] {:ms ms}))]
        (live-sync/record! manager (cond-> command
                               (:instance-id command)
                               (assoc :cue-id (or (get-in after [:instances (:instance-id command) :cue-id])
                                                  (get-in before [:instances (:instance-id command) :cue-id])))
                               (and (= :cue/launch (:op command)) timing) (assoc :recording-duration timing))
                      ack))
      (acknowledge-sync! runtime manager command ack epoch))
    (doseq [{:keys [type emitter data]} events
            :when (and (= {:kind :runtime :id :cue-timing} emitter)
                       (#{:cue/launch-cancelled :cue/launch-missed} type))
            :let [source (cue-timing/source after (:command-id data))]
            :when source]
      (acknowledge-sync! runtime manager {:op :cue/launch :source source} data epoch))))

(defn- finish-resource-work! [runtime retired candidates devices restored-generation restarted-generation]
  (when (and restarted-generation @(:running runtime))
    (try (connections/rebind-restart! (:connections runtime) restarted-generation
                                      (audio-callbacks runtime) (sync-callbacks runtime))
         (catch Exception error
           (when @(:running runtime)
             (swap! (:state runtime) assoc-in [:status :restart-input-error] (error-data error))))))
  ;; User/device callbacks can dispatch while they close. Never run this under
  ;; the model owner lock, and attempt every resource even after a failure.
  (doseq [cleanup (concat (map #(fn [] (connections/retire! %)) retired)
                          (map #(fn [] (connections/abort-prepared! %)) candidates)
                          (map #(fn [] (devices/close! %)) devices))]
    (try (cleanup)
         (catch Exception error
           (swap! (:state runtime) assoc-in [:status :cleanup-error] (error-data error)))))
  (when (and restored-generation @(:running runtime)
             (= restored-generation (:generation (connections/status (:connections runtime)))))
    (try (connections/resume-audio! (:connections runtime) (audio-callbacks runtime))
         (connections/resume-sync! (:connections runtime) (sync-callbacks runtime))
         (catch Exception error
           (when @(:running runtime)
             (swap! (:state runtime) assoc-in [:status :audio-resume-error] (error-data error)))))))

(defn- capture-render-time [runtime time-ms slot]
  (if-not slot [time-ms nil]
    (let [physical ((get-in runtime [:publisher :now-ms]))
          remaining (- (:deadline-ms slot) physical)]
      (when (pos? remaining)
        [(+ (now-ms runtime) remaining) (assoc slot :inputs-at-ms physical)]))))

(defn- begin-presentation! [runtime model slot]
  (let [clock-id [@(:presentation-clock runtime) (get-in model [:transport :playing?])]
        options (merge {:period-ms (/ 1000.0 (:frame-rate runtime))}
                       (get-in model [:settings :output :presentation]))
        compiled (patch! runtime (get-in model [:rig :fixtures]))]
    (output-scheduler/begin-frame! (:publisher runtime) (:epoch model)
                                   compiled
                                   options (or (pause-look/blackout? model) (zero? (:master model)))
                                   (assoc slot :timeline-id clock-id :output-context @(:patch runtime)))))

(def ^:private ingress-order
  (reify java.util.Comparator
    (compare [_ left right]
      (Long/compare (:ingress-order left) (:ingress-order right)))))

(defn tick!
  "Advance one owner cycle; restore resources/model together, then retire IO
  outside the owner lock. Reactive actions from the previous publication enter
  this cycle with fresh command identity and the captured source epoch."
  ([runtime] (tick! runtime (now-ms runtime)))
  ([runtime time-ms] (tick! runtime time-ms nil))
  ([runtime time-ms output-slot]
   (let [frame-start-ns (System/nanoTime)
         retired (volatile! []) candidates (volatile! []) retired-devices (volatile! [])
         restored-generation (volatile! nil) restarted-generation (volatile! nil)
         committed-fault-events (volatile! [])
         author-request (volatile! nil)]
     (try
       (locking (:lock runtime)
         (when-let [[target output-slot] (when @(:running runtime) (capture-render-time runtime time-ms output-slot))]
           (let [before @(:state runtime)
                 time-ms (max target (get-in before [:model :time-ms]))
                 messages (java.util.ArrayList.)
                 inline-registrations (volatile! {})
                 ordinary-count (.drainTo ^LinkedBlockingQueue (:queue runtime) messages 256)
                 release-count (.drainTo ^LinkedBlockingQueue (:cue-release-queue runtime) messages 256)
                 _ (when (and (pos? ordinary-count) (pos? release-count))
                     (.sort messages ingress-order))
                 _ (vreset! candidates (vec (keep #(get-in % [:payload :candidate]) messages)))
                 reactive-error (volatile! nil)
                 actions (try (connections/drain-actions! (:connections runtime))
                              (catch Exception error (vreset! reactive-error (error-data error)) []))
                 _ (doseq [action actions]
                     (.add messages {:type :command :reply (promise) :expected-epoch (get-in before [:model :epoch])
                                     :payload (assoc action :command-id (str (UUID/randomUUID))
                                                     :source {:adapter :reactive :session-id (:id runtime)})}))
                 physical-receipts (output-scheduler/drain-receipts! (:publisher runtime))
                 physical-now ((:now-ms (:publisher runtime)))
                 [clock-model audio-clock-confidence] (source-clock-model runtime (:model before) time-ms)
                 [model receipts restored? restored-output]
                 (reduce
                  (fn [[model receipts restored? restored-output] {:keys [reply] :as message}]
                    (try
                      (let [result (apply-message model message (if restored? (:time-ms model) time-ms) runtime)]
                        (when-let [candidate (:candidate result)]
                          (vswap! retired conj (connections/commit-restore! (:connections runtime) candidate))
                          (vreset! restored-generation (:generation (connections/status (:connections runtime))))
                          ;; Detach atomically; opening sessions also verify their captured epoch.
                          (vswap! retired-devices into (vals (first (swap-vals! (:devices runtime) (constantly {}))))))
                        (when (and (not (:candidate result)) (not= (:epoch model) (get-in result [:model :epoch])))
                          (vreset! restarted-generation (connections/commit-restart! (:connections runtime) (:model result)))
                          (vswap! retired-devices into (vals (first (swap-vals! (:devices runtime) (constantly {}))))))
                        (when-let [[key handler] (:inline-registration result)]
                          (when (fn? handler) (vswap! inline-registrations assoc key handler)))
                        [(:model result) (conj receipts [reply (:ack result) (when (= :command (:type message)) (:payload message))])
                         (or restored? (:restore? result))
                         (if (:restore? result) (:restored-output result) restored-output)])
                      (catch Exception error
                        [model (conj receipts [reply (merge {:status :rejected} (error-data error)) (when (= :command (:type message)) (:payload message))])
                         restored? restored-output])))
                  [(-> (consume-sync! runtime (consume-audio! runtime (staging/collect (staging/begin clock-model)
                                                       :reactive {:kind :runtime :id :reactive} (:pending-reactive-faults before)) time-ms))
                       (calibration/advance physical-receipts physical-now)
                       (photometry/advance physical-receipts physical-now)
                       (fog-scan/expire physical-now)) [] false nil] messages)
                 time-ms (if restored? (:time-ms model) time-ms)
                 _ (when (or restored? @restarted-generation)
                     (when restored? (reset! (:clock runtime) {:origin-nanos (System/nanoTime) :time-ms time-ms}))
                     (reset! (:leases runtime) {})
                     (locking (:sync-input runtime)
                       (reset! (:sync-input runtime) {:epoch (:epoch model) :events []}))
                     (locking (:audio-input runtime)
                       (reset! (:audio-input runtime) {:epoch (:epoch model) :events [] :features nil})))
                 model (if (cue-timing/active? model)
                         (cue-timing/advance model (cue-clock-identity runtime model) time-ms @(:leases runtime) command)
                         model)
                 model (reduce (fn [m instance]
                                 (let [session (get-in instance [:source :session-id])
                                       seen (get @(:leases runtime) session (:started-at-ms instance))]
                                   (if (and session (#{:web "web"} (get-in instance [:source :adapter]))
                                            (= :held (:launch-mode instance))
                                            (= :running (:phase instance)) (> (- time-ms seen) 10000))
                                     (:model (command m {:op :instance/end :instance-id (:id instance)} time-ms)) m)))
                               model (vals (:instances model)))
                 photo-session (photometry/current model)
                 model (if (and photo-session (not= (:base-rig photo-session) (:rig model))) (photometry/disarm model) model)
                 photo-session (photometry/current model)
                 presentation-token (when (:independent-output? runtime) (begin-presentation! runtime model output-slot))
                 presentation-token (cond-> presentation-token
                                      presentation-token (assoc :calibration (or (photometry/marker photo-session) (calibration/marker model))))
                 sampling-session (volatile! nil)
                 started (System/nanoTime)
                 next-state
                 (if restored?
                   (let [params (parameter-sources/sample model {:time-ms time-ms :transport (:transport model)} {})
                         random (modulation/captured-overrides model)]
                     {:parameter-samples params :modulation-view {:overrides random}
                      :resolved-param-overrides (merge-with merge random (:overrides params))
                      :keyframes (effect-keyframes/restored model)
                      :model (cond-> (effect-keyframes/without-checkpoints model) (get-in model fog-deadline/saved-ids-path)
                               (update :extensions dissoc ::fog-deadline/saved-automatic-pumps))
                      :output (if restored-output
                                (update restored-output :universes
                                        #(fog-deadline/restore-packet
                                           (patch! runtime (get-in model [:rig :fixtures]))
                                           (get-in model fog-deadline/saved-ids-path)
                                           (into {} (map (fn [[u values]] [u (byte-array (map unchecked-byte values))])) %)))
                                (render/resolve-frame (patch! runtime (get-in model [:rig :fixtures])) [] {:blackout? true}))
                      :status {:restored? true :frame-ms 0}})
                   (try
                     (let [model (-> (advance-sensors! runtime model time-ms)
                                     (subscribers/advance time-ms (event-callbacks/results (:event-callbacks runtime)))
                                     (composition/advance time-ms) (advance-auto time-ms runtime)
                                     (audio-return/advance (duplex/frame-input! (:audio-returns runtime)) time-ms routing/admit-sync)
                                     (routing/advance time-ms command))
                           parameter-samples (parameter-sources/sample model {:time-ms time-ms :transport (:transport model)}
                                                                      (get-in before [:parameter-samples :plans]))
                           control-view (if (seq (:overrides parameter-samples))
                                          (update (:modulation-view before) :overrides
                                                  #(merge-with merge % (:overrides parameter-samples)))
                                          (:modulation-view before))
                           patch (patch! runtime (get-in model [:rig :fixtures]))
                           space (when (or (get-in model [:settings :spatial :enabled?]) (get-in model spatial/state-path) (spatial-cues/needed? model))
                                   (spatial/advance model (assoc before :modulation-view control-view) time-ms (reactions runtime) command))
                           model (or (:model space) model)
                           conductor? (autopilot-config/enabled? model)
                           calibrating? (or photo-session (calibration/current model) (get-in model [:extensions :tolglow.rig.fog-calibration/session]))
                           bypass? (or calibrating? (false? (get-in model [:transport :playing?])) (pause-look/blackout? model))
                           hard-off? (or (pause-look/blackout? model) (zero? (:master model)))
                           suspended? (or bypass? hard-off?)
                           meter-plan (when (or conductor? (lighting/needs-meter? (:settings model))) (capacity-plan! runtime patch))
                           resolve-frame (compute/frame-renderer (:compute runtime) frame-start-ns)
                           previous-cache (when (= (:epoch model) (get-in before [:model :epoch])) (:autopilot-cache before))
                           prior-output (when (and (= (:epoch model) (get-in before [:model :epoch]))
                                                   (identical? (:rig model) (get-in before [:model :rig])))
                                          (:output before))
                           prepared (when (or conductor? (get-in model autopilot/run-path))
                                      (autopilot/prepare model previous-cache patch meter-plan time-ms suspended?
                                        (delay (if (and (:blocks previous-cache) (identical? patch (get-in previous-cache [:plan :patch])))
                                                 (resolve-frame patch (:blocks previous-cache) (:raw-options previous-cache))
                                                 prior-output)) command control-view prior-output))
                           model (autopilot/start-hdr-clock (or (:model prepared) model) time-ms)
                           hdr? (or conductor? (get-in model [:settings :hdr :enabled?]))
                           hdr-scope (when hdr? (autopilot/standalone-scope (:hdr-scope before) model patch))
                           hdr-opts (when hdr? (or (autopilot/render-options (assoc model :time-ms time-ms) prepared suspended?
                                                                              control-view hdr-scope) {}))
                           modulation-result (modulation/advance (:modulation model) model
                                                                  {:time-ms time-ms :transport (:transport model)})
                           model (assoc model :modulation (:state modulation-result))
                           input-overrides (merge-with merge (:overrides modulation-result) (:overrides parameter-samples)
                                                       (get-in space [:fields :overrides]))
                           modulation-middleware (modulation/middleware input-overrides)
                           _ (prepare-compiler! runtime (get-in model [:rig :fixtures]))
                           variable-plan (variable-blocks/prepare! (:variable-plan runtime) (get-in model [:settings :variable-blocks]) (prefabs/catalog model))
                           variables (variable-blocks/sample variable-plan model time-ms (:variable-observation before))
                           model (variable-blocks/collect-events model (:variable-observation before) variables)
                           variable-inputs (when variables (:variables (variable-blocks/resolved model variables)))
                           sampling (vreset! sampling-session (cadence/begin runtime before model))
                           frame-input (merge {:time-ms time-ms :epoch (:epoch model) :show-id (:show-id model)
                                               :keyframes (:keyframes before) :keyframe-params input-overrides
                                               :keyframe-generation (:responses model) :spatial-aims (get-in space [:fields :aims])
                                               :hdr? (boolean (or hdr? (:observe-hdr? space))) :fixtures (get-in model [:rig :fixtures])}
                                              (:inputs sampling))
                           frame-input (cond-> frame-input variables (assoc :resolved-variables variable-inputs))
                           response (response-middleware! runtime (:responses model []))
                           inputs (comp response modulation-middleware)
                           compile-step (fn [definition]
                                          (let [step (spatial-cues/with-aims definition ((:compile runtime) definition))]
                                            ;; Certified literals have no parameter schemas, and
                                            ;; these transforms touch only declared parameters.
                                            (keyframes/with-inputs step inputs {:preserve-time-invariance? true})))
                           result (engine/advance model frame-input compile-step
                                                  (executor/mapper (:executor runtime)))
                           manual-aim (surface-aim/contribution model patch)
                           pause-layer (pause-layer! runtime model)
                           meter-opts (get-in model [:settings :lighting-capacity])
                           plan (when (:enabled? meter-opts) meter-plan)
                           audio @(:capacity-audio runtime)
                           previous (when (= (:epoch model) (get-in before [:model :epoch]))
                                      (get-in before [:output :lighting-capacity]))
                           budget (lighting/prepare-budget before model patch meter-plan calibrating? hard-off?)
                           standby (when-not bypass? (:standby-block budget))
                           observation (when (identical? (:rig model) (get-in before [:model :rig])) (get-in before [:output :atmosphere]))
                           observation (if (get-in model [:settings :fog-control :enabled?])
                                         (fog-control/with-forecast (:plan @(:venue-fog runtime)) observation) observation)
                           previous-control (get-in before [:output :fog-control])
                           fog-bypass? (or bypass? (zero? (:master model)) (get-in model [:extensions :tolglow.rig.fog-calibration/session]))
                           limit (when plan
                                   (capacity/control (merge capacity/defaults meter-opts)
                                     previous
                                     (:master model) time-ms (when (= (:epoch model) (:epoch audio)) audio)
                                     bypass?))
                           fog (when (and plan (or (:fog? meter-opts) (:fog previous)))
                                 (capacity/fog-policy meter-opts plan (:fog previous) time-ms limit fog-bypass?
                                                      (:reference-rms meter-opts (:reference-rms capacity/defaults))))
                           setpoint (fog-control/frame model patch observation previous-control
                                      (if (get-in runtime [:sink :send!]) ((get-in runtime [:publisher :now-ms])) time-ms)
                                      fog-bypass? (:contributions result) (:block fog))
                           extra-blocks (cond-> (fog-control/with-demands [] (:feedback setpoint))
                                          manual-aim (conj manual-aim)
                                          (:music setpoint) (conj (:music setpoint))
                                          (:block photo-session) (conj (:block photo-session))
                                          pause-layer (conj pause-layer))
                           render-options (cond-> {:master (get limit :effective-master (:master model))
                                                   :blackout? (pause-look/blackout? model)
                                                   :include-values? (not (:batch? runtime)) :compact-values? (:batch? runtime)
                                                   :edge-tracking (when (seq (get-in model [:rig :edge-tracking]))
                                                                    (:links (reflections/plan-for (:optics-plan runtime) (:rig model))))
                                                   :previous-values (or (get-in before [:output :values]) {})
                                                   :previous-compact (get-in before [:output :compact])}
                                            hdr? (assoc :hdr hdr-opts)
                                            (:observe-hdr? space) (assoc :observe-hdr? true))
                           resolved (effect-faults/resolve-output result
                                      #(resolve-frame patch (lighting/contributions standby % extra-blocks) render-options)
                                      (if-let [groups (get-in space [:fields :groups])]
                                        #(spatial-cues/mask-contributions % groups) identity))
                           result (:result resolved)
                           finished (when (or (get-in model [:settings :output-response :enabled?])
                                              (get-in model [:settings :lighting-budget :enabled?])
                                              (get-in model [:settings :lighting-activity :enabled?]))
                                      (lighting/finish-frame before (:model result) patch meter-plan budget (:output resolved)
                                        (lighting/contributions standby (:contributions result) extra-blocks) time-ms
                                        calibrating? hard-off?
                                        (get limit :effective-master (:master model))))
                           delivered (or (:frame finished) (:output resolved))
                           frame (cond-> delivered
                                   setpoint (assoc :fog-control (:status setpoint) :fog-demands (:leases setpoint))
                                   plan (assoc :lighting-capacity
                                               (merge limit (capacity/measure plan (:universes delivered))
                                                      (cond-> {:writers (if (or (pause-look/blackout? model) (zero? (:master model))) []
                                                                          (capacity/writers plan (into (:contributions result) extra-blocks)))}
                                                        fog (assoc :fog (:state fog))))))]
                       (let [observation (when (:plan prepared)
                                           (autopilot/observe (:model result) prepared previous-cache frame
                                             (lighting/contributions standby (:contributions result) extra-blocks)
                                             {:suspended? suspended? :calibrating? calibrating?}
                                             (assoc (select-keys render-options [:master :edge-tracking :previous-values :previous-compact])
                                               :hdr {} :compact-values? false :include-values? false)
                                             (:contributions result)))]
                       {:model (let [with-fog (cond-> (or (:model observation) (:model result))
                                                setpoint (fog-control/publish previous-control (:status setpoint)))
                                     updated (advance-light-scenes runtime (lighting/publish with-fog frame) patch frame time-ms bypass?)]
                                 (lighting-levels/publish updated (lighting-scenes/groups updated) (:lighting-capacity frame)))
                        :output frame :variable-observation variables
                        ::show-events/intent-emissions (:intent-emissions result)
                        :preview-hdr (when (:hdr? frame-input) (or hdr-opts {}))
                        :keyframes (:keyframes result) :spatial-cache (:cache space) :hdr-scope hdr-scope :autopilot-cache (:cache observation) :autopilot-author-request (:author-request prepared)
                        :lighting-history (:state finished) :fault-events (:fault-events result)
                        :parameter-samples (assoc parameter-samples :spatial-overrides (get-in space [:fields :overrides])
                                                  :live-routing (live-sync/parameter-routing (connections/sync (:connections runtime))))
                        :resolved-param-overrides input-overrides
                        :modulation-view (select-keys modulation-result [:overrides :diagnostics])
                        :status {:frame-ms (/ (- (System/nanoTime) started) 1.0e6) :last-intents (:intents result)}}))
                     (catch Exception error
                       (assoc before :model model :status {:error (error-data error)} :fault-events []
                              :output (if (or (pause-look/blackout? model) (zero? (:master model)))
                                        (render/resolve-frame (patch! runtime (get-in model [:rig :fixtures])) [] {:blackout? true})
                                        (:output before))))))
                 operations (map (fn [message [_ ack]]
                                   {:operation (:type message) :command (get-in message [:payload :op])
                                    :ack ack :emitter-id (get-in message [:payload :source :adapter])})
                                 messages receipts)
                 ;; Confidence belongs to this live clock sample and its resource generation.
                 next-state (assoc next-state :audio-clock-confidence
                                   (when-not (or restored? @restarted-generation) audio-clock-confidence))
                 fired-outcomes (cue-timing/fired-outcomes (:model next-state))
                 next-state (if (seq fired-outcomes) (update next-state :model cue-timing/clear-fired-outcomes) next-state)
                 next-state (advance-section-patterns runtime before next-state (or restored? @restarted-generation))
                 committed (show-events/commit (:event-admit runtime) (:model before) next-state operations)
                 next-state (venue-fog/commit! runtime (:state committed)
                              (patch! runtime (get-in next-state [:model :rig :fixtures])) restored?)]
             (when (or (seq (get-in next-state [:output :fog-demands])) @(:fog-deadlines runtime))
               (swap! (:fog-deadlines runtime) fog-deadline/remember
                      (or (:frame-id presentation-token) (get-in next-state [:model :frame-number] 0))
                      (when-not restored? (get-in next-state [:output :fog-demands]))))
             (reset! (:state runtime) (dissoc next-state :autopilot-author-request :pending-reactive-faults))
             (vreset! author-request (:autopilot-author-request next-state))
             (event-callbacks/publish! (:event-callbacks runtime) (:model next-state) @inline-registrations)
             (vreset! committed-fault-events
                      (into (vec (:fault-events next-state)) (keep (fn [event]
                              (when (= :event/handler-failed (:type event))
                                (assoc (:data event) :type :event/handler-failed)))) (:events committed)))
             (doseq [[reply ack] receipts] (deliver reply ack))
             (when-not restored?
               (try (observe-sync-commands! runtime (:model before) (:model next-state) receipts
                                           (:events committed) fired-outcomes)
                    (catch Exception error
                      (swap! (:state runtime) assoc-in [:status :sync-error] (error-data error)))))
             (try (if (:independent-output? runtime)
                    (when-not (get-in next-state [:status :error])
                      (output-scheduler/complete! (:publisher runtime)
                                                  (cond-> presentation-token
                                                    output-slot (assoc :show-time-ms (get-in next-state [:model :time-ms])
                                                                       :beat (get-in next-state [:model :transport :beat])))
                                                  (assoc (:output next-state) :safety-state (get-in next-state [:output :fog-demands]))))
                    (send-output! runtime (get-in next-state [:output :universes]) (get-in next-state [:model :frame-number] 0)
                                  (get-in next-state [:output :fog-demands])))
                  (catch Exception error
                    (swap! (:state runtime) assoc-in [:status :output-error] (error-data error))))
             (let [observers (vals @(:observers runtime))
                   publication (try (connections/publish! (:connections runtime) (effect-keyframes/source (block-trace/source next-state) (:keyframes next-state))
                                            (boolean (some (fn [observer] ((:tracing? observer))) observers)))
                                    (catch Exception error (vreset! reactive-error (error-data error)) nil))
                   trace (:delivery-trace publication)]
               (when (seq (:faults publication))
                 (vswap! committed-fault-events into (:faults publication))
                 (swap! (:state runtime) assoc :pending-reactive-faults (:faults publication)))
               (doseq [observer observers]
                 ((:events! observer) (show-events/clock-context (:model next-state))
                  (:events committed) (:deliveries committed) trace)))
             (when @reactive-error
               (swap! (:state runtime) assoc-in [:status :reactive-error] @reactive-error))
             (doseq [observer (vals @(:observers runtime))]
               ((:frame! observer) @(:state runtime)))
             (let [elapsed (/ (- (System/nanoTime) frame-start-ns) 1.0e6)
                   defer? (cue-pads/govern! (:cue-pad-previews runtime) elapsed (/ 1000.0 (:frame-rate runtime)))]
               (when-let [sampling @sampling-session]
                 (swap! (:state runtime) assoc :cadence
                        (cadence/finish (assoc sampling :defer-live-slowdown? defer?) (:model next-state) elapsed)))))))
       (finally
         (when-let [request @author-request]
           (authoring/request! (agent-manager runtime) request #(submit! runtime :autopilot-authored %)))
         (effect-faults/offer! (:effect-logs runtime) @committed-fault-events)
         (finish-resource-work! runtime @retired @candidates @retired-devices
                                @restored-generation @restarted-generation)
         (rdm/reconcile! (:rdm runtime)))))))

(defn- prepare-rig-edit [model proposed]
  (let [basis (:rig model)
        proposed (dissoc proposed :runtime-id :expected-epoch)
        proposed (cond-> proposed (map? (:fixtures proposed))
                   (update :fixtures (partial fixture-patch/expand (get-in model [:registry :fixtures]))))]
    {:basis basis :rig (rig/prepare-edit basis proposed)}))

(defn- ingress-queue [runtime type payload]
  (if (or (= type :cue-cancel)
          (and (= type :command) (= :cue/cancel-launch (:op payload))))
    (:cue-release-queue runtime)
    (:queue runtime)))

(defn submit!
  "Enqueue an operation. Full restores are validated and resource-prepared off
  the frame-owner lock, then committed atomically by the next owner cycle.
  Use :cue-schedule and :cue-cancel for explicit musical launch timing."
  ([runtime payload] (submit! runtime :command payload))
  ([runtime type payload]
   (let [reply (promise) candidate (volatile! nil)]
     (try
       (let [before (:model @(:state runtime))
             epoch (get payload :expected-epoch (:epoch before))]
         (when-not (and @(:running runtime) (= epoch (:epoch before))
                        (or (not (#{:photometry :fog-calibration :venue-surface} type)) (and (= epoch (:expected-epoch payload)) (= (:id runtime) (:runtime-id payload))))
                        (or (nil? (:runtime-id payload)) (= (:id runtime) (:runtime-id payload))))
           (throw (ex-info "Show stopped or session changed" {})))
         (let [payload (if (= :rig type) (prepare-rig-edit before payload) payload)
               payload (if (and (= :fog-calibration type) (= :fit (:op payload)))
                         (merge payload (fog-scan/prepare-fit before (:session-id payload) (:trace @(:venue-fog runtime))))
                         payload)
               payload (if (= :import type)
                         (let [document (persistence/read-string (:source payload))
                               calibration-armed? (or (photometry/current (:model document))
                                                      (get-in document (into [:model] (conj calibration-session/artifact-path :armed?))))
                               ;; Strip scoped ownership before thaw rekeys instance sources.
                               document (update document :model (comp fog-control/disarm-restored fog-scan/disarm photometry/disarm calibration/disarm-restored))
                               _ (venue-fog/validate-saved! (:model document))
                               restored-before (persistence/thaw before document)
                               restored (calibration/disarm-restored restored-before)
                               prepared (connections/prepare-restore (:connections runtime) restored)]
                           (vreset! candidate prepared)
                           {:model restored :frame-output (when-not calibration-armed? (:frame-output document)) :candidate prepared})
                         (if (= :calibration type)
                           (do
                             (when (= :solved (:op payload)) (throw (ex-info "Solve results must be computed by the host" {})))
                             (if (= :solve (:op payload))
                               (let [scan (calibration/current before)]
                                 (when (:armed? scan) (throw (ex-info "Cancel/finish output before fitting a stable capture" {})))
                                 (assoc payload :op :solved :expected-evidence-revision (:evidence-revision scan)
                                        :result (calibration-session/solve scan (calibration/resolve-fixture-ids before (:verified-axis-fixture-ids payload)))))
                               (dissoc payload :runtime-id :expected-epoch)))
                           (dissoc payload :runtime-id :expected-epoch)))
               queue (ingress-queue runtime type payload)
               accepted? (locking (:lock runtime)
                           (and @(:running runtime) (= epoch (get-in @(:state runtime) [:model :epoch]))
                                (.offer ^LinkedBlockingQueue queue
                                        {:type type :payload payload :reply reply :expected-epoch epoch
                                         :ingress-order (vswap! (:ingress-order runtime) inc)})))]
           (when-not accepted? (throw (ex-info "Show changed or command queue is full" {})))))
       (catch Exception error
         (when @candidate (connections/abort-prepared! @candidate))
         (let [command-id (or (:command-id payload) (get-in payload [:command :command-id]))]
           (deliver reply (cond-> (merge {:status :rejected} (error-data error))
                            command-id (assoc :command-id command-id))))))
     reply)))

(defn quantized-cue-status
  "Return a queued, fired, cancelled or missed launch receipt by command ID."
  [runtime command-id]
  (cue-timing/receipt (:model @(:state runtime)) command-id))

(defn register-event-subscriber!
  "Register show data from the REPL. A raw :middleware function is bound to its
  callback identity in this runtime. Use a Var or source for portable code."
  [runtime {:keys [id middleware] :as registration}]
  (let [inline-id (when (fn? middleware) (str (random-uuid)))
        registration (if middleware
                       (let [_ (when-not (or (fn? middleware) (var? middleware))
                                 (throw (ex-info "Expected middleware function or Var" {:id id})))
                             implementation (if (var? middleware)
                                              {:type :var :symbol (str (symbol middleware))}
                                              {:type :inline :callback-id inline-id})
                             definition (-> registration (dissoc :middleware)
                                            (update :actions (fnil conj []) {:op :callback :implementation implementation}))]
                         (subscribers/validate-definition definition)
                         definition)
                       registration)]
    (submit! runtime :event-subscriber (cond-> {:op :save :subscriber registration}
                                         inline-id (assoc ::inline-registration [[id inline-id] middleware])))))

(defmacro reg-event-subscriber!
  "Register a literal middleware fn as restorable REPL source. Supply lexical
  values through :bindings, e.g. {\"gain\" gain}. Other maps use the ordinary
  register-event-subscriber! API."
  [runtime registration]
  (let [middleware (when (map? registration) (:middleware registration))]
    (if (and (seq? middleware) (#{'fn 'fn* 'clojure.core/fn} (first middleware)))
      `(register-event-subscriber!
        ~runtime
        (update ~(dissoc registration :middleware :bindings) :actions (fnil conj [])
                {:op :callback
                 :implementation (assoc ~(reactive-registry/source-description *ns* middleware)
                                        :bindings ~(:bindings registration {}))}))
      `(register-event-subscriber! ~runtime ~registration))))

(defn stop! [runtime]
  (let [candidates (volatile! [])
        stop? (locking (:lock runtime)
                (when (compare-and-set! (:running runtime) true false)
                  (doseq [queue [(:queue runtime) (:cue-release-queue runtime)]]
                    (loop []
                      (when-let [message (.poll ^LinkedBlockingQueue queue)]
                        (when-let [candidate (get-in message [:payload :candidate])]
                          (vswap! candidates conj candidate))
                        (deliver (:reply message) {:status :rejected :message "Show stopped"})
                        (recur))))
                  true))]
    ;; No cleanup waits while holding the owner lock: a closing device or agent
    ;; may synchronously dispatch, and must receive its stopped receipt promptly.
    (when stop?
      (doseq [cleanup (concat (map :close! (vals (first (swap-vals! (:observers runtime) (constantly {})))))
                             (map #(fn [] (connections/abort-prepared! %)) @candidates)
                             [#(event-callbacks/close! (:event-callbacks runtime))
                              #(rdm/close! (:rdm runtime))
                              #(output-scheduler/close! (:publisher runtime))
                              #(send-output! runtime (render/idle-universes (:compiled @(:patch runtime))) 0 nil)
                              #(effect-faults/close! (:effect-logs runtime))
                              #(compute/close! (:compute runtime))
                              #(duplex/stop! (:audio-returns runtime))
                              #(development/close! (:development runtime))
                              #(connections/close! (:connections runtime))
                              (fn [] (locking (:devices runtime)
                                       (doseq [device (vals @(:devices runtime))]
                                         (try (devices/close! device) (catch Exception _)))
                                       (reset! (:devices runtime) {})))

                              #(.close ^java.io.Closeable (:executor runtime))
                              (fn [] (when-let [close! (get-in runtime [:sink :close!])] (close!)))])]
        (try (cleanup) (catch Exception error
                         (swap! (:state runtime) assoc-in [:status :cleanup-error] (error-data error)))))))
  nil)

(defn heartbeat! [runtime session]
  (when (and (string? session) (<= 1 (count session) 128))
    (swap! (:leases runtime)
           (fn [leases]
             (let [now (now-ms runtime)]
               (assoc (into {} (filter (fn [[_ seen]] (< (- now seen) 60000))) leases) session now))))))

(defn- show-identity [model]
  (select-keys model [:epoch :show-id]))

(defn- require-show-identity! [expected model]
  (when-not (= expected (show-identity model))
    (throw (ex-info "Agent run belongs to a previous show session" {}))))

(defn- agent-inspection [runtime state inspect]
  (let [identity (show-identity (:model state))
        result (inspect)]
    ;; Projection reads live diagnostics as well as the committed frame.
    ;; Wait for an active restore to publish before comparing identities.
    (locking (:lock runtime)
      (require-show-identity! identity (:model @(:state runtime))))
    result))

(defn dispatch-tool!
  "Agent and REPL tool boundary; all mutation uses the same owner queue as UI."
  [runtime tool arguments metadata]
  (let [state @(:state runtime)
        model (:model state)
        _ (when (and (:expected-epoch metadata) (not= (:expected-epoch metadata) (:epoch model)))
            (throw (ex-info "Agent run belongs to a previous show session" {})))
        arguments (if (= "set_lighting_scene" tool)
                    (venue-tools/scene-request model arguments)
                    (data/normalize-json arguments))
        envelope (merge (select-keys metadata [:epoch :show-id :expected-epoch])
                        {:command-id (:command-id metadata)
                         :source {:adapter :agent :session-id (:run-id metadata)}})
        [type payload]
        (case tool
          "configure_energy_scene" [:event-subscriber (merge (select-keys envelope [:expected-epoch])
                                                            (venue-tools/energy-binding model arguments))]
          "inspect_show" [:inspect nil]
          "inspect_lighting" [:inspect-lighting arguments]
          "publish_effect" [:definition (merge envelope {:kind :effects :definition (:definition arguments)})]
          "put_cue" [:agent-cue (merge envelope {:cue (:cue arguments)})]
          "launch_cue" [:command (merge envelope arguments {:op :cue/launch})]
          "stop_instance" [:command (merge envelope arguments {:op :instance/end})]
          "set_parameter" [:command (merge envelope arguments {:op :param/set})]
          "set_lighting_scene" [:lighting-scene (merge envelope arguments)]
          "configure_energy_autopilot" [:settings (merge (venue-tools/autopilot-request model arguments)
                                                         (select-keys envelope [:expected-epoch]))]
          "set_lighting_budget" [:settings (merge (venue-tools/budget-request model arguments) (select-keys envelope [:expected-epoch]))]
          "configure_auto" [:auto (merge (:configuration arguments) (select-keys envelope [:expected-epoch]))]
          (throw (ex-info "Unknown studio tool" {:tool tool})))]
    (case type
      :inspect (agent-inspection runtime state #(project-snapshot runtime state))
      :inspect-lighting (agent-inspection runtime state
                          (fn [] (venue-tools/inspect state payload #(measure-sources runtime state %))))
      (deref (submit! runtime type payload) 5000 {:status :pending :message "Command remains queued"}))))

(defn start-chat!
  "Start a chat on this show. The initial snapshot and tool calls are bound to
  the show session captured at admission. Supply :emit! and :done! callbacks.
  Returns the cancellable agent run handle, including its :result promise."
  [runtime payload {:keys [emit! done!]}]
  (let [[manager identity] (locking (:lock runtime)
                            [(agent-manager runtime) (show-identity (:model @(:state runtime)))])
        admission (assoc identity :expected-epoch (:epoch identity))]
    (agents/start-chat! manager payload
                        {:snapshot! (fn []
                                      (let [state @(:state runtime)]
                                        (require-show-identity! identity (:model state))
                                        (agent-inspection runtime state #(project-snapshot runtime state))))
                         :dispatch! #(dispatch-tool! runtime %1 %2 (merge %3 admission))
                         :emit! emit! :done! done!})))

(defn- push-parameter-projector []
  (let [cache (waveforms/create-cache)
        precision-cache (atom nil)]
    (fn [snapshot view capture]
      (->> (waveforms/project-display cache snapshot view capture)
           (precision/push-view precision-cache (:model snapshot))))))

(defn push-browser-display
  "Build a Push display transport using Studio's shared reactive components.
  Capture and waveform projection run on the display worker, not the show clock.
  Supply the browser executable and optional injected sink in options."
  [runtime options]
  (let [project-parameters (push-parameter-projector)
        pages (volatile! nil)
        scene-cache (push-scene/create-cache)]
    (display/browser-display
     (assoc options :project!
            (fn [view]
              (let [snapshot (assoc @(:state runtime) :runtime-id (:id runtime))
                    model (:model snapshot)
                    mode (or (:display-mode view) (get-in model [:settings :controllers :push-display :mode]) :parameters)
                    view (cond-> (assoc view :display-mode mode)
                           (#{:parameters :audio} mode)
                           (assoc-in [:visualization-clock :sample-period-ms]
                                     (cue-pads/sample-period-ms (:cue-pad-previews runtime))))
                    visual (case mode
                             (:parameters :audio)
                             (let [capture (when (= mode :audio)
                                             (audio/waveform (connections/audio (:connections runtime))))]
                               (project-parameters snapshot view capture))
                             :block-trace (block-trace/project-display (:id runtime) model view)
                             (:simulator :rig) (push-scene/project-display scene-cache snapshot
                                                     (assoc view :runtime-id (:id runtime)))
                             view)]
                (page-overview/project-display pages model visual)))))))

(defn- push-native-display [runtime]
  (let [project-parameters (push-parameter-projector)]
    (display/native-display
      {:project! (fn [view]
                   (project-parameters @(:state runtime) view nil))})))

(defn connect-device!
  "Explicit REPL connection; no devices open at application startup. kind is
  :push-2/:push-3/:push-3-midi/:seaboard/:x-touch/:midi/:osc/:sensor. Options select ports,
  expression modes and bindings; imported namespaces never acquire devices.
  Push 2 accepts :native-display? true for the Wayang parameter display. An
  explicit :display transport takes precedence."
  [runtime kind options]
  (let [id (:session-id options) epoch (get-in @(:state runtime) [:model :epoch])]
    (when-not (and (string? id) (seq id)) (throw (ex-info "Device requires session-id" {})))
    (locking (:devices runtime)
      (when-not @(:running runtime) (throw (ex-info "Show is stopped" {})))
      (when (get @(:devices runtime) id) (throw (ex-info "Device session already connected" {:session-id id})))
      (let [open! (case kind :push-2 devices/open-push-2! :push-3 devices/open-push-3!
                         :push-3-midi devices/open-push-3-midi! :seaboard devices/open-seaboard! :x-touch devices/open-x-touch!
                         :midi devices/open-midi! :osc devices/open-osc! :sensor devices/open-sensor!
                         (throw (ex-info "Unknown device kind" {:kind kind})))
            options (cond-> options
                      (#{:push-2 :push-3} kind) (assoc :target-search targets/search :launch-receipt cue-timing/receipt)
                      (and (= kind :push-2) (:native-display? options) (not (:display options)))
                      (assoc :display (push-native-display runtime))
                      (and (= kind :push-2) (not (contains? options :feedback-source)))
                      (assoc :feedback-source (cue-pads/push-feedback (:cue-pad-previews runtime) #(deref (:state runtime)))))
            device (open! (merge options {:model! #(let [model (:model @(:state runtime))]
                                                      (when-not (= epoch (:epoch model))
                                                        (throw (ex-info "Device belongs to a previous show epoch" {})))
                                                      model)
                                          :now-ms! #(now-ms runtime)
                                          :dispatch! (fn [command]
                                                       (heartbeat! runtime id)
                                                       (let [reply (submit! runtime (assoc command :expected-epoch epoch))
                                                             quantized? (or (= :cue/cancel-launch (:op command))
                                                                            (and (= :cue/launch (:op command))
                                                                                 (contains? command :grid)))]
                                                         (deref reply (if quantized? 0 5000)
                                                                {:status :pending :command-id (:command-id command)})))}))]
        (if (locking (:lock runtime)
              (when (and @(:running runtime) (= epoch (get-in @(:state runtime) [:model :epoch])))
                (swap! (:devices runtime) assoc id device)
                true))
          device
          (do (devices/close! device)
              (throw (ex-info "Show stopped or changed while connecting device" {}))))))))

(defn disconnect-device! [runtime id]
  (locking (:devices runtime)
    (when-let [device (get @(:devices runtime) id)]
      (try (devices/close! device) (finally (swap! (:devices runtime) dissoc id))))))

(defn expressive-preview
  "Preview the selected cue's expressive mapping using the connected device's
  axes and defaults when :device-id matches its mapping ID (or session ID when
  no mapping ID is configured). Otherwise use the requested device kind."
  [runtime request]
  (let [[model connected] (locking (:lock runtime)
                            [(:model @(:state runtime)) @(:devices runtime)])
        wire-id #(if (keyword? %) (subs (str %) 1) %)
        requested-id (wire-id (:device-id request))
        options (some (fn [[id {:keys [options]}]]
                        (when (= requested-id (wire-id (or (:mapping-id options) id)))
                          options))
                      connected)
        profile (when options
                  {:id (get-in options [:profile :id])
                   :axes (keys (get-in options [:profile :capabilities :available-axes]))
                   :defaults (select-keys options [:mapping-policy :param-mappings :cue-mappings])})]
    (expressive/preview model request profile)))

(defn- capture-show [runtime format-show]
  ;; Model/output come from one commit. Serialize after releasing the frame
  ;; owner so that large pixel runs can keep rendering during a download.
  (let [[model output windows] (locking (:lock runtime)
                                 (let [{:keys [model output keyframes]} @(:state runtime)
                                       atmosphere (venue-fog/for-save runtime output)
                                       model (merge (fog-scan/disarm model) (connections/export-state (:connections runtime) model))]
                                   [(cond-> (fog-deadline/save-ids model output)
                                      atmosphere (assoc-in venue-fog/saved-path atmosphere))
                                    output keyframes]))]
    (format-show (effect-keyframes/for-save model windows) (render/portable-frame output))))

(defn export-show
  "Freeze the full domain state and last output from one committed snapshot."
  [runtime]
  (capture-show runtime persistence/freeze))

(defn export-show-source [runtime] (capture-show runtime persistence/write-string))

(defn save-show!
  "Save a complete snapshot using a forced temporary file and atomic replacement."
  [runtime path]
  (let [file (.getAbsoluteFile (io/file path))
        text (export-show-source runtime)]
    (io/make-parents file)
    (let [temporary (java.nio.file.Files/createTempFile
                     (.toPath (.getParentFile file)) ".tolglow-show-" ".transit"
                     (make-array java.nio.file.attribute.FileAttribute 0))]
      (try
        (with-open [stream (java.io.FileOutputStream. (.toFile temporary))]
          (.write stream (.getBytes text java.nio.charset.StandardCharsets/UTF_8))
          (.force (.getChannel stream) true))
        (java.nio.file.Files/move temporary (.toPath file)
                                  (into-array java.nio.file.CopyOption
                                              [java.nio.file.StandardCopyOption/ATOMIC_MOVE
                                               java.nio.file.StandardCopyOption/REPLACE_EXISTING]))
        (.getAbsolutePath file)
        (finally (java.nio.file.Files/deleteIfExists temporary))))))

(defn load-show!
  "Read a bounded full show file and enqueue its atomic restore; returns a receipt."
  [runtime path]
  (with-open [reader (io/reader (io/file path) :encoding "UTF-8")]
    (let [buffer (char-array 8192) source (StringBuilder.)]
      (loop []
        (let [n (.read reader buffer)]
          (when (pos? n)
            (when (> (+ (.length source) n) persistence/max-document-characters)
              (throw (ex-info "Show file exceeds the document character limit" {})))
            (.append source buffer 0 n)
            (recur))))
      (submit! runtime :import {:source (str source)}))))

(defn rdm-action!
  "Explicit, independently owned RDM service. Status never touches a device."
  [runtime operation payload]
  (if (= :status operation)
    (assoc (rdm/status (:rdm runtime)) :runtime-id (:id runtime)
           :epoch (get-in @(:state runtime) [:model :epoch]))
    (let [identity (locking (:lock runtime)
                     (let [epoch (get-in @(:state runtime) [:model :epoch])]
                       (when-not (and @(:running runtime)
                                      (= (:runtime-id payload) (:id runtime))
                                      (= (:expected-epoch payload) epoch))
                         (throw (ex-info "RDM action belongs to a previous show session" {})))
                       {:runtime-id (:id runtime) :expected-epoch epoch}))]
      (case operation
        :connect (rdm/connect! (:rdm runtime) (:config payload {}) identity)
        :disconnect (rdm/disconnect-session! (:rdm runtime) (:session-id payload))
        :cancel (rdm/cancel! (:rdm runtime) (:job-id payload))
        (:associate :unassociate :adopt)
        (deref (submit! runtime :rdm-update (merge payload identity {:operation operation})) 5000 {:status :pending})
        (:discover :read :command) (rdm/submit! (:rdm runtime) operation payload)
        (throw (ex-info "Unknown RDM action" {:operation operation}))))))

(defn fixture-action!
  "Fixture library operations; publication and patching join the owner queue."
  [runtime operation payload]
  (let [manager (connections/fixtures (:connections runtime))
        identity {:expected-epoch (get-in @(:state runtime) [:model :epoch])}
        payload (merge identity payload)]
    (case operation
      :snapshot (fixture-service/snapshot manager)
      :search (fixture-service/search! manager payload)
      :refresh (fixture-service/refresh! manager payload)
      :fetch (fixture-service/fetch! manager payload)
      :import (let [result (fixture-service/import! manager payload)]
                (assoc result :source (some-> (or (:definition result) (:candidate result)) pr-str)))
      :publish (let [definition (if (:source payload) (data/read-edn (:source payload))
                                    (:definition payload))]
                 (deref (submit! runtime :definition
                                  (merge (select-keys payload [:expected-epoch :runtime-id :expected-version])
                                         {:kind :fixtures :definition definition}))
                        5000 {:status :pending :message "Publication remains queued"}))
      :patch (deref (submit! runtime :fixture-patch payload)
                    5000 {:status :pending :message "Patch remains queued"})
      (throw (ex-info "Unknown fixture operation" {:operation operation})))))

(defn- session-operation!
  "Admit short resource operations in the same transaction as restore. Device
  startup creates owned workers here; blocking shutdown is returned as work."
  [runtime payload operation!]
  (locking (:lock runtime)
    (let [epoch (get-in @(:state runtime) [:model :epoch])]
      (when-not (and @(:running runtime)
                     (or (nil? (:expected-epoch payload)) (= epoch (:expected-epoch payload)))
                     (or (nil? (:runtime-id payload)) (= (:id runtime) (:runtime-id payload))))
        (throw (ex-info "Connection operation belongs to a previous show session" {})))
      (operation! (dissoc payload :runtime-id :expected-epoch)))))

(defn audio-action! [runtime operation payload]
  (case operation
    :status (assoc (audio/status (connections/audio (:connections runtime)))
                   :received-event-count (get-in @(:state runtime) [:model :variables :audio :event-count] 0))
    :inputs (audio/list-inputs)
    :return-mixers (duplex/mixers)
    :return-status (duplex/status (:audio-returns runtime))
    :return-start (session-operation! runtime payload
                    (fn [_] (duplex/start! (:audio-returns runtime) (:model @(:state runtime))
                                           #(get @(:state runtime) :model) #(now-ms runtime)
                                           (:open-audio-returns! runtime))))
    :return-stop (let [manager (:audio-returns runtime)
                       id (session-operation! runtime payload (fn [_] (:session (duplex/status manager))))]
                   (duplex/stop! manager id))
    :start (session-operation! runtime payload
                               (fn [config]
                                 (connections/start-audio! (:connections runtime) config (audio-callbacks runtime))
                                 (audio/status (connections/audio (:connections runtime)))))
    :stop (let [generation (session-operation! runtime payload
                                             (fn [_] (:generation (connections/status (:connections runtime)))))]
            (connections/stop-audio! (:connections runtime) generation))
    (throw (ex-info "Unknown audio operation" {:operation operation}))))

(defn sync-action!
  "Explicit synchronization and Live ownership operations for Ring/REPL."
  [runtime operation payload]
  (let [operation! (fn [payload]
                     (let [manager (connections/sync (:connections runtime))]
                       (case operation
      :status (assoc (sync/status manager) :clock (session/clock-at manager (now-ms runtime)))
      :ports (midi-ports/midi-ports)
      :configure (let [config (live-sync/validate-config (:config payload payload))
                       model (:model @(:state runtime))
                       config (update config :live ableton/bind-registry-ids (:registry model))
                       pages (sort-by (comp str :id) (vals (:pages model)))
                       wire-id #(if (keyword? %) (subs (str %) 1) %)
                       page (if-let [id (:page-id payload)]
                              (some #(when (= (wire-id id) (wire-id (:id %))) %) pages)
                              (first pages))
                       map? (and page (get-in config [:live :enabled?]) (get-in config [:live :automap?]))
                       config (if map? (update config :live ableton/map-page (:registry model) page) config)]
                   (cond-> (sync/configure! manager config)
                     map? (assoc :unmapped (ableton/unmapped-fields (:live config) (:registry model) page))))
      (:learn-parameter :forget-parameter) {:accepted? (live-sync/edit-observed-parameter! manager operation (live-params/wire-id (:id payload)))}
      :arrange {:accepted? (live-sync/place-in-arrangement! manager (:song-beat payload) (:request-id payload))}
      :start (do (connections/start-sync! (:connections runtime) (sync-callbacks runtime))
                 (sync/status manager))
      :stop (let [generation (:generation (connections/status (:connections runtime)))]
              #(connections/stop-sync! (:connections runtime) generation))
      :source (sync/select-source! manager (:source-id payload))
      :handoff {:accepted? (live-sync/handoff! manager (:mode payload)
                                          (sync-envelope runtime (get-in @(:state runtime) [:model :epoch])))}
      :live (do
              (when-not (= 1 (:protocol-version payload))
                (throw (ex-info "Unsupported Live bridge protocol" {})))
              {:accepted? (live-sync/ingest-live! manager payload)})
      (throw (ex-info "Unknown synchronization operation" {:operation operation})))))]
    (if (#{:status :ports} operation) (operation! payload)
        (let [result (session-operation! runtime payload operation!)]
          (if (= :stop operation) (result) result)))))

(defn waveform-action! [runtime operation payload]
  (let [snapshot @(:state runtime)
        registry (or (get-in snapshot [:model :waveforms]) (signals/new-registry))]
    (case operation
      :registry registry
      :preview (waveforms/preview snapshot payload)
      (throw (ex-info "Unknown waveform operation" {:operation operation})))))

(defn sensor-action!
  "Authenticated sensor admission and explicit pairing/configuration. Source
  samples only enter a bounded mailbox; the lighting owner applies their intents."
  [runtime operation payload]
  (let [manager (connections/sensors (:connections runtime))]
    (case operation
      :status
      (let [model (:model @(:state runtime)) status (sensors/status manager)]
        (assoc status :runtime-id (:id runtime) :epoch (:epoch model)
               :sources (mapv (fn [[id source]]
                                (let [device (get-in status [:devices id])]
                                  {:device-id id :label (get-in source [:pairing :label] id)
                                   :config (:config source)
                                   :armed? (and (:armed-intent? source) (:armed? device)
                                                (get-in source [:state :armed?]))
                                   :session-id (:session-id device)
                                   :channels (get-in source [:state :channels])
                                   :baseline (get-in source [:state :baseline])
                                   :latest (sensors/latest manager id)}))
                              (sort-by key (get-in model [:sensors :sources])))))
      :sample
      (sensors/ingest! manager (sensor-osc/validate-observation (dissoc payload :runtime-id :expected-epoch)))
      :pair
      (let [captured (session-operation! runtime payload
                                        (fn [_] {:manager (connections/sensors (:connections runtime))
                                                 :epoch (get-in @(:state runtime) [:model :epoch])}))
            pairing (sensors/pair! (:manager captured) (:device-id payload) (:label payload))
            ack (deref (submit! runtime :sensor-configure
                               {:device-id (:device-id pairing) :pairing (:pairing pairing) :config {:version 1}
                                :runtime-id (:id runtime) :expected-epoch (:epoch captured)})
                       5000 {:status :pending :message "Pairing publication remains queued"})]
        (merge pairing ack))
      (:configure :arm :calibrate)
      (deref (submit! runtime ({:configure :sensor-configure :arm :sensor-arm :calibrate :sensor-calibrate} operation) payload)
             5000 {:status :pending :message "Sensor configuration remains queued"})
      (:stage-start :stage-stop)
      (let [current (session-operation! runtime payload (fn [_] (connections/sensors (:connections runtime))))]
        (if (= :stage-start operation)
          (sensors/start-stage! current payload)
          (audience-stage/stop-current! (:stage current) (:stage-id payload))))
      :stage-qr (audience-stage/png (:stage manager) (:id payload))
      :osc-start
      (let [manager (session-operation! runtime payload (fn [_] (connections/sensors (:connections runtime))))]
        (sensors/start-osc! manager (select-keys payload [:host :port]))
        (:udp (sensors/status manager)))
      :osc-stop
      (let [manager (session-operation! runtime payload (fn [_] (connections/sensors (:connections runtime))))]
        (sensors/stop-osc! manager))
      (throw (ex-info "Unknown sensor operation" {:operation operation})))))

(defn repl-action!
  "Session operations for the explicit development REPL. Restoring history never
  evaluates it. Mutations are admitted against the current runtime and epoch."
  [runtime operation payload]
  (let [operate! (fn [payload]
                   (let [manager (repls runtime) id (:session-id payload "main")]
                     (case operation
                       :snapshot (assoc (repl-manager/snapshot manager id)
                                        :runtime-id (:id runtime) :epoch (get-in @(:state runtime) [:model :epoch]))
                       :complete (repl-manager/complete manager id (:prefix payload ""))
                       :doc (repl-manager/lookup manager id (:symbol payload))
                       :session (repl-manager/open-session! manager id)
                       :scratch (repl-manager/set-scratch! manager id (:name payload "main") (:source payload))
                       :interrupt (repl-manager/interrupt! manager id (:request-id payload))
                       (throw (ex-info "Unknown REPL operation" {:operation operation})))))]
    (if (#{:snapshot :complete :doc} operation) (operate! payload)
        (session-operation! runtime payload operate!))))

(defn repl-eval!
  "Admit one streamed evaluation. The evaluator owns the worker and the serial
  queue; Ring output never runs on the show frame thread."
  [runtime payload emit! done!]
  (let [manager (session-operation! runtime payload (fn [_] (repls runtime)))
        id (:session-id payload "main")
        detached? (atom false)
        result (repl-manager/submit! manager id (:source payload)
                  {:request-id (:request-id payload)
                   :on-event (fn [event]
                               (when-not @detached?
                                 (try (emit! event)
                                      (catch java.io.IOException _ (reset! detached? true))))
                               (when (= :done (:type event)) (done! event)))})]
    (if (= :rejected (:status result))
      (do (emit! {:type :done :status :rejected :reason (:reason result)
                  :session-id id :request-id (:request-id payload)})
          (done! result) {})
      ;; Hiding the drawer or closing a popup detaches its stream. Accepted
      ;; source remains owned by the named session until explicit interruption.
      {:cancel! #(reset! detached? true)})))

(defn application-operation!
  "Development sessions belong to the server process, independently of show
  loads. File/container work executes outside the lighting owner lock."
  [runtime service operation payload]
  (when-not (and @(:running runtime)
                 (or (nil? (:runtime-id payload)) (= (:id runtime) (:runtime-id payload))))
    (throw (ex-info "Development operation belongs to a previous server process" {})))
  (service (:development runtime) operation payload))

(defn render-slot!
  "Take the latest inputs at render start and evaluate them for the scheduled
  output time. The physical clock and the restorable show clock have distinct
  origins; only the time remaining to the deadline crosses that boundary."
  [runtime slot]
  (tick! runtime nil slot))

(defn start!
  "Start the scheduled show in preview or with :ola or :artnet. An embedded
  host may supply its own sink opener. The show takes ownership of that sink."
  ([] (start! {}))
  ([options] (start! options output/open-sink!))
  ([{:keys [frame-rate] :or {frame-rate 40} :as options} open-sink!]
   (when-not (and (number? frame-rate) (<= 1 frame-rate 120)) (throw (ex-info "Frame rate must be 1..120" {})))
   (let [sink (open-sink! options)
         runtime (try (create-runtime (merge {:adaptive-keyframes {:enabled? true}} options
                                              {:sink sink :independent-output? true}))
                      (catch Throwable error
                        (when sink
                          (try ((:close! sink)) (catch Throwable cleanup (.addSuppressed error cleanup))))
                        (throw error)))]
     (try
       (output-scheduler/start! (:publisher runtime) (/ 1000.0 frame-rate) #(render-slot! runtime %))
       runtime
       (catch Throwable error (stop! runtime) (throw error))))))
