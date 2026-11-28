(ns tolglow.show
  "Executable host for the shared functional lighting engine. Requiring it is inert.
  One owner advances the model; workers return data; physical output is opt-in."
  (:require [clojure.string :as str]
            [afterglow.control.model :as control]
            [afterglow.control.instance-order :as instance-order]
            [afterglow.control.pages :as cue-pages]
            [tolglow.studio.parameter-precision :as precision]
            [afterglow.control.categories :as categories]
            [afterglow.control.effect-selection :as effect-selection]
            [afterglow.control.devices.expressive :as expressive-mapping]
            [afterglow.control.sensors :as sensor-core]
            [afterglow.runtime.engine :as engine]
            [afterglow.runtime.keyframes :as keyframes]
            [tolglow.effects.keyframes :as effect-keyframes]
            [tolglow.effects.cadence :as cadence]
            [tolglow.visualization.cue-pads :as cue-pads]
            [tolglow.visualization.parameter-frames :as parameter-frames]
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
            [tolglow.controllers.push-knobs :as push-knobs]
            [tolglow.param-nudge :as param-nudge]
            [tolglow.controllers.push-variations :as push-variations]
            [tolglow.surface.link :as surface]
            [tolglow.surface.push-2 :as push-2-surface]
            [tolglow.surface.push-3 :as push-3-surface]
            [tolglow.surface.x-touch :as x-touch]
            [tolglow.effects.variable-blocks :as variable-blocks]
            [tolglow.effects.prefabs :as prefabs]
            [tolglow.effects.material-design :as material-design]
            [tolglow.effects.variable-edit :as variable-edit]
            [tolglow.effects.group-channels :as group-channels]
            [tolglow.agents.venue :as venue-tools]
            [clojure.java.io :as io]
            [tolglow.compiler :as compiler]
            [tolglow.compiler.capability :as capability]
            [tolglow.compiler.gradient :as gradient]
            [tolglow.compiler.batch :as batch]
            [tolglow.registry :as registry]
            [tolglow.integrations.ableton :as ableton]
            [tolglow.integrations.live-runs :as live-runs]
            [tolglow.integrations.live-devices :as live-devices]
            [tolglow.integrations.live-launcher :as live-launcher]
            [tolglow.integrations.live-connection :as live-connection]
            [tolglow.integrations.phrase-capture :as phrase-capture]
            [tolglow.integrations.phrase-clip :as phrase-clip]
            [tolglow.aim-paths :as aim-paths]
            [tolglow.rig.pixel-layout :as pixel-layout]
            [tolglow.rig.reflections :as reflections]
            [tolglow.rig.surface-aim :as surface-aim]
            [tolglow.modulation :as modulation]
            [tolglow.knob-candidates :as knob-candidates]
            [tolglow.knobs :as knobs]
            [tolglow.audio-lfo :as audio-lfo]
            [tolglow.knob-presets :as knob-presets]
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
            [tolglow.spatial.sampling :as spatial-sampling]
            [tolglow.spatial.targets :as targets]
            [tolglow.authoring.output-patch :as output-patch]
            [tolglow.authoring.variation-control :as variations]
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
            [tolglow.effects.state-migration :as state-migration]
            [tolglow.show.preview :as preview]
            [tolglow.show.push-scene :as push-scene]
            [tolglow.show.scene-draft :as scene-draft]
            [tolglow.show.recovery :as recovery]
            [tolglow.show.lighting :as lighting]
            [tolglow.lighting-budget.agent-policy :as budget-policy]
            [tolglow.show.position-hold :as position-hold]
            [tolglow.show.programs :as programs]
            [tolglow.show.launch-prep :as launch-prep]
            [tolglow.show.publication :as publication]
            [tolglow.integrations.analysis :as analysis]
            [tolglow.integrations.mpv-session :as mpv-session]
            [tolglow.integrations.reference-position :as reference-position]
            [tolglow.integrations.section-patterns :as section-patterns]
            [tolglow.timeline :as timeline]
            [tolglow.persistence :as persistence]
            [tolglow.personalities :as personalities]
            [tolglow.operator-history :as operator-history]
            [tolglow.connections :as connections]
            [tolglow.repl.sessions :as repl-manager]
            [tolglow.development :as development]
            [tolglow.integrations.audience-invitation :as audience-stage]
            [tolglow.sensors :as sensors]
            [tolglow.sensors.audience.zones :as audience-zones]
            [tolglow.sensors.osc :as sensor-osc]
            [tolglow.fixtures.library :as fixture-service]
            [tolglow.rig.aim-calibration :as aim-calibration]
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
            [taoensso.timbre :as timbre]
            [tolglow.composition.response :as response]
            [tolglow.composition.envelope :as envelope]
            [tolglow.compiler.video :as video-output]
            [tolglow.video.hub :as video-hub]
            [tolglow.video.mapping :as video-mapping]
            [tolglow.video.preview :as video-preview]
            [tolglow.video.sampler :as video-sampler]
            [tolglow.video.shader :as video-shader]
            [tolglow.video.shader-producer :as shader-producer]
            [tolglow.video.source :as video-source]
            [tolglow.video.surface-cues :as video-surface-cues])
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

(defn- new-device-retirement-state
  "Retired device sessions (#226): :closing holds the session IDs whose close
  has not returned, :abandoned those still open when stop! stopped waiting."
  []
  {:pending 0 :retired 0 :failed 0 :reopen-failed 0 :abandoned 0 :late 0 :closing #{}})

(defn- device-session-id [device] (get-in device [:options :session-id]))

(defn- checkpoint-results [before message-receipts]
  (let [finished (keep (fn [[{::keys [checkpoint-request]} [_ ack]]]
                         (when checkpoint-request
                           (cond-> (merge checkpoint-request (select-keys ack [:status :epoch]))
                             (contains? (:details ack) :received-epoch) (assoc :reason :stale-show-session))))
                       message-receipts)]
    (if (seq finished)
      (vec (take-last 32 (concat (:checkpoint-results before) finished)))
      (:checkpoint-results before))))

(defn- scene-eligible-ids [runtime fixtures]
  (let [cache (:scene-eligible runtime)
        previous @cache]
    (if (identical? fixtures (:fixtures previous)) (:ids previous)
      (let [ids (lighting-scenes/eligible-ids fixtures)]
        (reset! cache {:fixtures fixtures :ids ids}) ids))))

(defn- personality-views
  "The library and device projections, redrawn only when the saved state changes."
  [runtime model]
  (let [cache (:personality-views runtime)
        saved (personalities/state model)
        previous @cache]
    (if (and previous (identical? saved (:state previous))) previous
      (reset! cache {:state saved :projection (personalities/projection model)
                     :push (personalities/push-view model)}))))

(defn- recent-page-snapshot [model]
  (let [{:keys [rows] :as page} (cue-pages/page model cue-pages/recent-page-id)]
    ;; The native pads count from the bottom. Studio counts from the top.
    (update page :cells
            (fn [cells]
              (->> cells
                   (map (fn [[[x y] cue-id]] {:x x :y (- (dec rows) y) :cue-id cue-id}))
                   (sort-by (juxt :y :x))
                   vec)))))

(defn- show-folder-options [value]
  (cond
    (nil? value) nil
    (and (map? value) (string? (:id value)) (seq (:id value)))
    (select-keys value [:id :file :path :new? :copy-of])
    :else (throw (ex-info "Show folder must be a map with an :id" {}))))

(defn- show-folder-view
  "The Mac show folder this show runs from (#742): its identity file, whether
  this launch wrote it (first open, or a detected copy) and the shared
  checkpoint from before per-folder recovery while it is still offered."
  [runtime show-id]
  (when-let [folder (:show-folder runtime)]
    (let [legacy (some-> (:recovery runtime) deref (recovery/legacy-offer show-id))]
      (cond-> folder legacy (assoc :legacy-checkpoints legacy)))))

(declare ^:private audition-projection response-sample-fault-view)

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
                (preview/brightness-gains fixtures))
        ;; The latest completed audition frame. Live :output stays the committed frame.
        audition (some-> (:scene-draft runtime) deref audition-projection not-empty)]
    (merge (select-keys model [:show-id :epoch :revision :time-ms :master :blackout? :transport :rig :settings])
           {:runtime-id (:id runtime)
            :preview-only (get-in runtime [:preview-only :message])
            :show-folder (show-folder-view runtime (:show-id model))
            :section-patterns-enabled? (some? (:section-patterns runtime))
            :frame-number (get model :frame-number 0)
            :transport-generation (:transport-generation snapshot 0)
            :visualization-clock {:sample-period-ms (cue-pads/sample-period-ms (:cue-pad-previews runtime))}
            :spatial (get-in model [:variables :spatial])
            :reactive-faults (vec (concat (graph/faults (reactions runtime)) (get-in model [:variables :runtime :handler-faults])))
            :autopilot (get-in model [:variables :lighting/autopilot] {:enabled? false})
            :audio/energy (get-in model [:variables :audio/energy])
            :lighting-scenes (lighting-scenes/snapshot model (scene-eligible-ids runtime fixtures))
            :video (let [shaders (when (shader-producer/active? (:shader-video runtime))
                                   (shader-producer/status (:shader-video runtime)))]
                     (when (or shaders (seq (get-in model [:settings :video :sources])) (seq (get-in model [:settings :video :mappings]))
                               (seq (get-in model [:settings :video :surfaces])))
                       (video-preview/project (:video runtime) (:video-latch snapshot) fixtures (:video-preview-plans runtime) model
                                              (:video-surface-views runtime) shaders)))
            :personalities (:projection (personality-views runtime model))
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
            :active-instance-order (mapv :id (instance-order/ordered model))
            :effective-instance-order (mapv :id (instance-order/ordered-with-priorities
                                                 model (:priority-overrides parameter-samples)))
            :layer-priority-destination {:param parameter-sources/priority-param
                                         :schema parameter-sources/priority-schema}
            :instances (mapv #(-> % (dissoc :state :parameter-bindings)
                                  (assoc :definition (get definitions [(:definition %) (:targets %)])
                                         :parameter-sources (parameter-sources/project % (get sources (:id %)) resolve-inputs responses?)
                                         :categories (registry/definition-categories (:definition %)))
                                  ;; Gradients sampled with fewer stops than their value holds (#689).
                                  (as-> instance (let [reduced (gradient/reduced-gradients (:definition %) (:params %))]
                                                   (cond-> instance (seq reduced) (assoc :gradient-reductions reduced))))
                                  (dissoc ::effect-selection/selected)
                                  (update :system (fn [s] (merge {:opacity 1.0 :paused? false}
                                                               (select-keys s [:opacity :gain :paused? :attack :release])))))
                             (sort-by :order (vals (:instances model))))
            :effect-selection (effect-selection/projection model)
            :pending-cue-launches (cue-timing/pending model)
            :cue-launch-outcomes (cue-timing/outcomes model)
            :schedule (subscribers/schedule-view model)
            :knobs (knobs/projection model) ; linked knobs (#666)
            :audio-lfos (audio-lfo/projection model) ; audio-shaped LFOs (#676)
            ;; Generated knob candidates (#667) and their Room audition.
            :knob-candidates (knob-candidates/projection
                              model (when-let [preview (get-in audition [:draft :preview])]
                                      (cond-> (assoc preview :status (:status audition)
                                                     :draft-revision (get-in audition [:draft :revision]))
                                        (:frame-revision audition) (assoc :frame-revision (:frame-revision audition)))))
            :knob-presets (knob-presets/projection model) ; knob looks (#547)
            :operator-history (operator-history/project (:id runtime) model)
            :history-command-outcomes (:history-command-outcomes snapshot [])
            :parameter-source-outcomes (:parameter-source-outcomes snapshot [])
            :pages (mapv (fn [page] (update page :cells
                                           #(mapv (fn [[[x y] cue]] {:x x :y y :cue-id cue})
                                                  (sort-by key %)))) (sort-by (comp str :id) (vals (:pages model))))
            :recent-page (recent-page-snapshot model)
            :auto (let [{:keys [started-at duration]} (get-in model [:auto :state :section :timer])
                        pending (subscribers/upcoming model :auto/configure)]
                    (cond-> (select-keys (:auto model) [:enabled? :config :reasons])
                      (get-in model [:auto :state :section]) (assoc :section (get-in model [:auto :state :section :id]))
                      ;; The beat the playing section ends: the line :next-section waits for.
                      (and (number? started-at) (= :beats (:unit duration)))
                      (assoc :section-end-beat (double (+ started-at (:value duration))))
                      (seq pending) (assoc :pending (mapv #(select-keys % [:event-id :label :due-beat :enabled? :source]) pending))))
            :responses (vec (:responses model))
            :effect-group-channels (group-channels/projection model)
            :control-surfaces (surface/with-refusals (surface/projection model) ; surfaces, links, session boxes (#705)
                                                     (some-> (:surface-refusals runtime) deref (dissoc ::sequence)))
            :effect-events (get-in model [:variables :runtime] {:event-sequence 0 :events []})
            :patch (routing/snapshot model)
            :modulation {:controllers (mapv #(select-keys % [:id :config :ticks :next-beat :owner-instance-id])
                                             (get-in model [:modulation :controllers]))
                         :holds (modulation/portable-holds (get-in model [:modulation :holds] {}))
                         :overrides (:overrides modulation-view {})
                         :diagnostics (:diagnostics modulation-view [])}
            :compositions (mapv (fn [definition]
                                  (let [run (get-in model [:composition-runs (:id definition)])]
                                    (cond-> (assoc definition :running? (some? run) :playback (:playback run))
                                      ;; A running member instance carries this layer ID and its
                                      ;; slot index, so views find its card (#795).
                                      run (assoc :layer-id (:owner-id run)
                                                 :member-slots (mapv :id (get-in run [:definition :slots]))))))
                                (sort-by (comp str :id) (vals (:compositions model))))
            :scene-draft (not-empty (dissoc audition :output))
            :draft-output (when-let [frame (:output audition)]
                            {:values (reflections/with-footprints optics (preview/values frame fixtures gains))
                             :draft (select-keys (:draft audition) [:id :revision])
                             :frame-revision (:frame-revision audition)
                             :time-ms (:time-ms audition)})
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
                                   ;; Response-sample lease predicates that threw. The count
                                   ;; ticks every frame, so Studio gets only whether it repeated;
                                   ;; the count and ex-data stay with the REPL record.
                                   :response-sample-faults (response-sample-fault-view runtime)
                                   :compute (compute/status (:compute runtime))
                                   :movement (compute/movement-status (:compute runtime) output)
                                   :effect-logs (effect-faults/status (:effect-logs runtime))
                                   :audio-returns (duplex/status (:audio-returns runtime))
                                   :output-scheduler (output-scheduler/status (:publisher runtime))
                                   :device-retirement (some-> (:device-retirement-state runtime) deref)
                                   :output (or (get-in runtime [:sink :description]) {:type :preview})})})))

(defn snapshot [runtime] (project-snapshot runtime @(:state runtime)))

(defn operator-history
  "Read the same committed operator history shown in Studio."
  [runtime]
  (operator-history/project (:id runtime) (:model @(:state runtime))))

(defn variables
  "All show variables from one committed frame, for headless callers and the REPL."
  [runtime]
  (let [{:keys [model variable-observation]} @(:state runtime)]
    (:variables (variable-blocks/resolved model variable-observation))))

(defn parameter-sample
  "The latest committed frame's resolved parameters: its identity and each
  running instance's effective parameters and response values. Reads the
  sample Studio and Push share for that frame, so it never resolves an
  instance a second time. (show/parameter-sample rt)"
  [runtime]
  (waveforms/frame-parameters @(:state runtime)))

(declare prepare-output prepare-safety-output present-output! heartbeat! submit!)

(defn attach-observer!
  "Install a frame observer once under the show owner. Its callbacks enqueue data
  only; they run under the owner lock, so they must not read the run record or
  the phrase capture (`live-runs`, `capture-action!`), which refuse there.
  close! runs outside the owner lock when the show stops."
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

(defn lease-response-samples!
  "Ask the show owner to retain exact live response inputs on committed frames
  while a display reads them. demanded? is polled once per owner tick and must
  only read state. A throwing predicate counts as not demanded and is recorded
  for response-sample-faults. Returns an idempotent release function."
  ([runtime] (lease-response-samples! runtime (constantly true)))
  ([runtime demanded?]
   (let [lease (Object.)]
     (swap! (:response-sample-readers runtime) assoc lease {:demanded? demanded?})
     (fn [] (swap! (:response-sample-readers runtime) dissoc lease) nil))))

(defn response-sample-faults
  "The first failure and failure count of each current response-sample lease whose predicate threw."
  [runtime]
  (into [] (keep :fault) (some-> (:response-sample-readers runtime) deref vals)))

(def ^:private fault-message-limit 240)

(defn- response-sample-fault-view
  "The stable part of each response-sample fault for the Studio stream: a
  bounded message and whether it repeated. Steady failures change nothing."
  [runtime]
  (mapv (fn [{:keys [message count]}]
          (let [message (str message)]
            {:message (if (> (clojure.core/count message) fault-message-limit)
                        (subs message 0 fault-message-limit) message)
             :repeated? (> (or count 1) 1)}))
        (response-sample-faults runtime)))

(defn- record-response-sample-fault! [runtime lease error]
  ;; Bounded: one entry per live lease, keeping the first failure and a count.
  (swap! (:response-sample-readers runtime)
         (fn [readers]
           (if (contains? readers lease)
             (update-in readers [lease :fault]
                        #(if % (update % :count inc) (assoc (error-data error) :count 1)))
             readers))))

(defn- interrupted-cause? [^Throwable error]
  (boolean (some #(instance? InterruptedException %) (take-while some? (iterate #(.getCause ^Throwable %) error)))))

(defn- response-samples-demanded? [runtime]
  ;; Poll every lease so each failing reader is recorded. Errors such as
  ;; StackOverflowError propagate to the tick as they would without a lease.
  (reduce (fn [demanded? [lease {reader :demanded?}]]
            (or (try (boolean (reader))
                     (catch Exception error
                       (when (interrupted-cause? error) (.interrupt (Thread/currentThread)))
                       (record-response-sample-fault! runtime lease error)
                       false))
                demanded?))
          false (some-> (:response-sample-readers runtime) deref)))

(def ^:private ^:dynamic *pure-bindings*
  "Collects the pure bindings one program compile creates, or nil."
  nil)

(defn- bind-pure
  "reuse/evaluator, recording the binding in the compile that created it."
  [declaration calculate]
  (let [bound (reuse/evaluator declaration calculate)]
    (when-let [^java.util.List collected *pure-bindings*]
      (.add collected bound))
    bound))

(defn- recording-bindings
  "Wrap a definition -> step compiler so each step carries, as ::pure-bindings
  metadata, the pure bindings its compile created (#875)."
  [compiler]
  (fn [definition]
    (let [collected (java.util.ArrayList.)
          step (binding [*pure-bindings* collected] (compiler definition))]
      (if (and (seq collected) (instance? clojure.lang.IObj step))
        (vary-meta step assoc ::pure-bindings (vec collected))
        step))))

(defn- release-pure-bindings!
  "A pure binding keeps its last output while its program stays cached, even
  after every instance running it has released. Clear the bindings of
  programs the previous frame ran and this frame did not, so a released or
  replaced cue retains nothing (#875). Runs on the frame owner, outside any
  CAS retry."
  [runtime used]
  (when-let [frame-bindings (:pure-bindings runtime)]
    (doseq [bound @frame-bindings :when (not (contains? used bound))]
      (reuse/clear! bound))
    (reset! frame-bindings used)))

(defn retained-pure-bindings
  "Pure bindings of the programs the last frame ran that still retain a value.
  For tests and diagnostics."
  [runtime]
  (filterv reuse/retains? (some-> (:pure-bindings runtime) deref)))

(defn- compiler-context
  "The rig's compiler and program cache. `submit` runs compiles off the frame
  owner (see tolglow.show.programs). GPU expression groups prepare against
  the show's compute session, so a show using them compiles on the owner."
  [fixtures batch? compute-owner submit]
  (let [gpu? (compute/expressions-enabled? compute-owner)
        options (cond-> {:enabled? batch? :bind-pure bind-pure}
                  gpu? (assoc :prepare-group (compute/group-preparer compute-owner)))]
    (assoc (programs/context (recording-bindings (batch/compiler-for-rig fixtures options)) (when-not gpu? submit))
           :fixtures fixtures)))

(defn- cached-compile [compiler-context]
  (fn [definition] (programs/compile! @compiler-context definition)))

(defn- open-compute! [{:keys [compute-backend movement-backend batch? frame-rate gpu-budget-ms]
                       :or {batch? true frame-rate 40} :as options}]
  (when (and (contains? #{:native-gpu :distributed} compute-backend) (not batch?))
    (throw (ex-info "GPU expressions require :batch? true" {})))
  (compute/open!
   (cond-> (select-keys options [:compute-backend :movement-backend :gpu-options])
     (or (contains? #{:native-gpu :distributed} compute-backend) (= :native-gpu movement-backend))
     (assoc :budget-ms (or gpu-budget-ms (max 1 (long (/ 500.0 frame-rate))))))))

(defn- preview-only-options [value]
  (cond
    (nil? value) nil
    (and (map? value) (string? (:message value)) (seq (:message value))) (select-keys value [:message :owner])
    :else (throw (ex-info "Preview-only must be a map with a :message" {}))))

(defn preview-only
  "Why this show may not open devices or arm output, as {:message …}, or nil.
  The Mac app starts a show preview-only while another window's show owns the
  Mac's devices and output (#669)."
  [runtime]
  (:preview-only runtime))

(defn require-devices!
  "Refuse a device-opening or output-arming action on a preview-only show with
  a coded refusal (:code :preview-only, HTTP 409). Device actions call this in
  the owner, so Studio, the REPL, configured modules and startup actions are
  refused alike."
  [runtime action]
  (when-let [{:keys [message]} (preview-only runtime)]
    (throw (ex-info message {:code :preview-only :action action :http/status 409}))))

(def ^:private output-arming
  "Owner operations that start driving fixtures through the physical sink."
  {:calibration #{:start} :photometry #{:start :start-surface} :fog-calibration #{:start}})

(defn- new-phrase-capture
  "Owner state for retrospective Capture (#671). options is false to disable,
  or phrase-capture/new-buffer options such as {:bars 8 :grid {:beats 1/4}}."
  [options]
  (if (false? options)
    {:enabled? false}
    {:enabled? true :buffer (phrase-capture/new-buffer options) :layout {}
     :cost {:samples 0 :total-ns 0 :max-ns 0 :last-ns 0} :captures 0 :last nil}))

(def default-video-waiters
  "Projector page requests (waiting or being served a frame) a show allows at
  once unless :max-video-waiters says otherwise; more get 503."
  8)

(declare device-writer)

(defn- new-live-runs
  "Owner state for the run record (#683). options is false to disable, or
  live-runs/new-record options; each runtime gets its own record ID so its
  run clips never share a name with another session's in one Set."
  [options]
  (if (false? options)
    {:enabled? false}
    (let [record-id (or (:record-id options) (subs (str (java.util.UUID/randomUUID)) 0 8))]
      {:enabled? true :record (live-runs/new-record (assoc options :record-id record-id)) :mirror nil
       ;; Run-clip playback decoder, made by the first sync; its lock orders
       ;; playback events with the owner's acknowledgements and moves.
       :player nil :player-lock (Object.) :inbox [] :inbox-dropped 0 :replies {} :end-replies {}
       :cost {:samples 0 :total-ns 0 :max-ns 0 :last-ns 0}
       ;; Parameter devices on run tracks (#625): slot layouts, ownership,
       ;; values reported by the companion and Tolglow writes waiting to be
       ;; sent. :device-send! replaces the bridge sender (tests capture it).
       :devices (live-devices/new-state) :device-inbox [] :device-inbox-dropped 0
       :device-outbox {} :device-flush? false :device-send! (:device-send! options) :device-writer (delay (device-writer))
       :device-sends {:requests 0 :executed 0 :failed 0 :not-connected 0}
       :device-cost {:samples 0 :total-ns 0 :max-ns 0 :max-values 0 :values 0}})))

(defn- compile-worker
  "The :background-compile option: false or nil compiles on the owner, true
  starts an owned worker on first use, and a function receives each compile
  task (tests and replay step it themselves)."
  [options]
  (let [choice (get options :background-compile false)]
    (cond
      (or (nil? choice) (false? choice)) nil
      (true? choice) {:owned (delay (programs/worker))}
      (fn? choice) {:submit choice}
      :else (throw (ex-info "Background compile must be a boolean or a function" {:background-compile choice})))))

(defn- observer-worker
  "The :background-observers option: true folds each committed model into
  the phrase-capture buffer and the run record on one owned thread after the
  frame, instead of on the owner inside it. False or nil (the default) folds
  on the owner, which keeps manual ticks single-threaded."
  [options]
  (let [choice (get options :background-observers false)]
    (cond
      (or (nil? choice) (false? choice)) nil
      (true? choice) {:pending (atom clojure.lang.PersistentQueue/EMPTY) :lock (Object.) :errors (atom {})
                      ;; Serializes tick! callers across settle and cycle.
                      :tick-lock (Object.)
                      :executor (delay (java.util.concurrent.Executors/newSingleThreadExecutor
                                        (reify java.util.concurrent.ThreadFactory
                                          (newThread [_ task]
                                            (doto (Thread. ^Runnable task "tolglow-committed-observers") (.setDaemon true))))))}
      :else (throw (ex-info "Background observers must be a boolean" {:background-observers choice})))))

(defn- compile-submit [worker]
  (cond
    (:submit worker) (:submit worker)
    (:owned worker) (fn [task] ((:submit @(:owned worker)) task))))

(defn- configure-runtime
  [compute-owner {:keys [model sink workers agent-options batch?] :or {workers 2 batch? true} :as options}]
   (let [model (-> (or model (demo/model))
                   prefabs/startup
                   (update :rig pixel-layout/prepare-startup)
                   calibration/disarm-restored photometry/disarm fog-scan/disarm)
         _ (when-not (or (nil? (:effect-log! options)) (ifn? (:effect-log! options)))
             (throw (ex-info "Effect log sink must be callable" {})))
         compile-worker (compile-worker options)
         reference (:section-reference options)
         music (when-let [policy (:section-patterns options)]
                 (let [planner (section-patterns/new-planner (:profile reference) policy)]
                   (when (get-in planner [:policy :enabled?]) planner)))
         initial-position (when reference
                            (when-not (and music (:profile reference))
                              (throw (ex-info "A section reference requires an enabled pattern policy and track profile" {})))
                            (reference-position/new-state (:provenance reference) (:position-options reference)))
         section-player (when (contains? reference :mpv)
                          (mpv-session/create (:mpv reference) (:profile reference) initial-position))
         _ (lighting-scenes/validate-state! model)
         _ (personalities/validate-state! model)
         _ (lighting/validate! (:settings model))
         _ (group-channels/validate-state! model)
         _ (when (contains? (:settings model) :section-patterns)
             (section-patterns/override-settings (get-in model [:settings :section-patterns])))
         _ (variable-blocks/compile-model model)
         _ (registry/validate-public-query-ids! (get-in model [:settings :studio :public-queries]))
         _ (when-let [config (get-in model [:settings :adaptive-keyframes] (:adaptive-keyframes options))]
             (cadence/settings config))
         _ (when-let [s (get-in model [:settings :spatial])] (spatial/settings s))
         _ (when-let [opts (get-in model [:settings :lighting-capacity])] (capacity/settings opts))
         _ (when-let [opts (get-in model [:settings :controllers])] (surface/validate-settings! opts))
         _ (surface/validate-state! model)
         _ (rig/validate-rig (:rig model))
         _ (venue-fog/validate-saved! model)
         _ (when-let [opts (get-in model [:settings :fog-control])] (fog-control/settings opts))
         _ (doseq [definition (vals (get-in model [:registry :effects]))] (compiler/compile-effect definition))
         fixtures (get-in model [:rig :fixtures])
         patch (render/compile-patch fixtures)
         compiler-context (atom (compiler-context fixtures batch? compute-owner (compile-submit compile-worker)))
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
                          (catch Exception error (connections/close! owned) (throw error)))
         field-sampler (try (spatial-sampling/create (:spatial-sampling options))
                            (catch Exception error
                              (.close ^java.io.Closeable worker-pool)
                              (connections/close! owned)
                              (throw error)))]
     (let [development (try (development/create
                             (when-let [workspace-options (if (contains? options :workspace-options)
                                                            (:workspace-options options) (development/default-options))]
                               (assoc workspace-options :app-agent-options
                                      (assoc (merge (:app-agent-options workspace-options) (:app-agent-options options))
                                             :credential-store (get-in owned [:options :agent-options :credential-store])))))
                            (catch Exception error
                              (.close ^java.io.Closeable field-sampler)
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
      ;; Owner only, under the lock: messages a cycle held back, in order (#784).
      :held-messages (atom [])
      :push-gesture-ends (volatile! {})
      :ingress-order (volatile! 0) :lock (Object.) :running (atom true)
      :venue-fog (atom {}) :fog-deadlines (atom nil)
      :clock clock :sink sink :pause-layer-cache (volatile! nil)
      :adaptive-keyframes (:adaptive-keyframes options) :cue-pad-previews (cue-pads/manager)
      :capture-effect-samples? (:capture-effect-samples? options)
      :browser-observations browser-inputs
      :observers (atom {}) :response-sample-readers (atom {}) :frame-rate (get options :frame-rate 40)
      :publisher publisher :presentation-clock (volatile! 0) :wire-retirement (atom nil)
      ;; Owner only: pure bindings of the programs the last frame ran (#875).
      :pure-bindings (atom #{})
      :independent-output? (boolean (:independent-output? options))
      :preview-only (preview-only-options (:preview-only options))
      :show-folder (show-folder-options (:show-folder options))
      ;; `start!` fills this in. It is part of the map here so that the REPL
      ;; sessions' `runtime`, captured from this map, reaches the checkpoints.
      :recovery (atom nil)
      :executor worker-pool
      :spatial-sampling field-sampler
      :leases (atom {})
      :audio-returns (duplex/create) :open-audio-returns! (get options :open-audio-returns! duplex/open-java!)
      :variable-plan (volatile! nil) :scene-eligible (atom {}) :scene-plans (volatile! {})
      :personality-views (atom nil)
      :scene-draft (atom nil) :audition-options (:audition options)
      :audition-worker (delay (java.util.concurrent.Executors/newSingleThreadExecutor
                                (reify java.util.concurrent.ThreadFactory
                                  (newThread [_ task]
                                    (doto (Thread. ^Runnable task "tolglow-room-audition") (.setDaemon true))))))
      :capacity-plan (volatile! nil) :capacity-audio (volatile! nil)
      :audio-input (atom {:epoch (:epoch model) :events [] :features nil :shapes {}})
      :section-patterns music
      :section-player section-player
      :section-reference (when initial-position
                           {:initial initial-position
                            :input (atom {:epoch (:epoch model) :position initial-position
                                          :used-sessions #{(get-in initial-position [:provenance :session-id])}})})
      :sync-input (atom {:epoch (:epoch model) :events []})
      :phrase-capture (atom (new-phrase-capture (get options :phrase-capture {})))
      :live-runs (atom (new-live-runs (get options :live-runs {})))
      ;; The cue launcher's last Live setup and pad-clip write (#228).
      :live-launcher (atom {})
      ;; Shared parameter-visualization frames (#625), started by the first view.
      :parameter-frames (delay (parameter-frames/create (merge {:snapshot! #(deref (:state @runtime-ref))}
                                                               (:parameter-frames options))))
      :connections owned :devices (atom {}) :device-reconnects (atom {}) :surface-refusals (atom {}) :development development
      ;; Sessions a restore or epoch change retires close here, off the render thread (#226).
      :device-retirement-worker (delay (java.util.concurrent.Executors/newSingleThreadExecutor
                                        (reify java.util.concurrent.ThreadFactory
                                          (newThread [_ task]
                                            (doto (Thread. ^Runnable task "tolglow-device-retirement") (.setDaemon true))))))
      :device-retirement-state (atom (new-device-retirement-state))
      :rdm (rdm/create (merge (:rdm-options options)
                             {:state rdm-state :model! #(get @(:state @runtime-ref) :model)
                              :identity! #(let [runtime @runtime-ref]
                                            {:runtime-id (:id runtime) :expected-epoch (get-in @(:state runtime) [:model :epoch])})
                              :publish! #(submit! @runtime-ref :rdm-update (merge %1 %2))}))
      :response-cache (atom {:responses [] :middleware identity})
      :compute compute-owner :compiler-context compiler-context :batch? batch?
      :state-migrations (atom {})
      :parameter-precision (atom nil)
      :optics-plan (atom (reflections/plan (:rig model)))
      :patch (atom {:fixtures fixtures :compiled patch :retirement (retirement/plan nil nil patch)
                    :brightness-gains (preview/brightness-gains fixtures)})
      :compile (cached-compile compiler-context)
      :compile-worker compile-worker
      :observer-worker (observer-worker options)
      :launch-prep (launch-prep/context options)
      ;; Video sources open on their own thread, outside the lighting frame (#672).
      :video (video-hub/create (:video-options options {}))
      ;; Shader cues render on their own cadence, outside the lighting frame (#673).
      :shader-video (shader-producer/create (:shader-options options {}))
      ;; Notified after every committed frame; projector pages wait on it.
      :video-frame-signal (Object.)
      :video-waiters (java.util.concurrent.atomic.AtomicInteger. 0)
      :video-waiter-cap (:max-video-waiters options default-video-waiters)
      :video-preview-plans (video-sampler/plan-cache 16)
      :video-surface-views (video-preview/surface-cache)}]
       (reset! runtime-ref runtime)
       runtime)))

(defn create-runtime
  "Create an unscheduled show. The CPU backend is the default. Explicit
  :compute-backend :native-gpu acquires one session owned by this show.
  :worker-timeout-ms sets the per-frame effect worker deadline (default 500).
  :background-compile true compiles the programs of published revisions and
  first launches on an owned worker, off the frame: a revision is adopted,
  by all its instances together, once its programs are ready, and a first
  launch renders from then (`start!` turns it on). The default compiles them
  on the owner before the engine runs, which keeps manual ticks
  deterministic; a function receives each compile task instead.
  :background-observers true folds each committed model into the phrase
  capture and the run record on an owned thread after the frame; the owner
  finishes any fold still waiting before its next cycle, and readers before
  they read (`start!` turns it on).
  :prepare-launches true prepares each quantised launch's targets and
  capability verdict on an owned thread as soon as it is scheduled; admission
  at the beat installs that work when nothing it read has changed, and is
  otherwise unchanged (`start!` turns it on). A function receives each task.
  :spatial-sampling {:workers n :partitions n :min-fixtures n} bounds the
  workers that sample volume fields on a large patch; {:workers 1} is serial.
  An enabled :section-patterns policy publishes pattern intents from opted-in
  capture at owner boundaries. :section-reference can supply a versioned
  :profile, initial source :provenance and :position-options. Reconnect a
  player with attach-section-reference!. Actions require an event subscriber."
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
                ;; A candidate (#667) was drafted from launches the restart ends.
                knob-candidates/restart
                surface/detach-all
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

(defn effect-group-channels
  "Read the effect mixer projection from one committed show model."
  [runtime]
  (group-channels/projection (:model @(:state runtime))))

(def ^:private manual-priority-ops
  "Operator commands that set a running layer's priority or position."
  #{:instance/set-priority :instance/move-after})

(defn- hold-manual-priority
  "A manual priority or reorder command takes precedence over a waveform bound
  to that launch's layer priority, as a parameter edit does over its source.
  The hold records why, and the operator resumes the waveform explicitly."
  [model result {:keys [op instance-id]}]
  (if (and (manual-priority-ops op)
           (get-in model [:instances instance-id :parameter-bindings parameter-sources/priority-param]))
    (update-in result [:model :modulation] (fnil modulation/hold (modulation/new-state))
               {:instance-id instance-id :params [parameter-sources/priority-param] :until-beat nil
                :reason :manual-priority})
    result))

(defn- hold-manual-params [model result {:keys [op source instance-id param control-id gesture-id] :as payload} authority]
  (let [result (hold-manual-priority model result payload)
        owner (when (#{:gesture/sample :gesture/end :gesture/cancel} op)
                [(:operator-id authority) control-id gesture-id])]
    (cond
      (#{:gesture/end :gesture/cancel} op)
      (if (get-in result [:model :modulation])
        (update-in result [:model :modulation] modulation/settle-gesture-hold instance-id param owner
                   (= :gesture/end op))
        result)

      (or authority (and (not= :patch (:type source)) (not= :live-playback (:origin source))))
      (reduce (fn [result {:keys [type instance-id param]}]
                (if (and (= :param/changed type)
                         (or (seq (get-in model [:modulation :controllers]))
                             (get-in model [:instances instance-id :parameter-bindings param])))
                  (if owner
                    (update-in result [:model :modulation] (fnil modulation/hold-gesture-param (modulation/new-state))
                               instance-id param owner)
                    (update-in result [:model :modulation] (fnil modulation/hold (modulation/new-state))
                               {:instance-id instance-id :params [param] :until-beat nil}))
                  result))
              result (:events result))

      :else result)))

(def ^:private gesture-ops #{:gesture/begin :gesture/sample :gesture/end :gesture/cancel})

(defn- override-ramps
  "A committed write outside the scheduler cancels running ramps on the same
  destination. Gestures and human surfaces record :human-override; other
  sources (agents, reactive actions) record :superseded. Writers that bypass
  this path are caught by subscribers/advance-ramps from the committed value."
  [result {:keys [op source instance-id param command-id action time-ms]} authority]
  (if-not (seq (get-in result [:model :event-subscribers :ramps]))
    result
    (let [;; A group action reports every system change; only its opacity actions write opacity.
          system-op (if (= :group/apply op) (:op action) op)
          destinations (cond-> (vec (keep (fn [{:keys [type] :as event}]
                                            (case type
                                              :param/changed {:target :param :instance-id (:instance-id event) :param (:param event)}
                                              :master/changed {:target :master}
                                              :variable/changed {:target :variable :variable (:variable event)}
                                              :instance/system-changed
                                              (when (#{:instance/opacity-set :instance/opacity-toggle} system-op)
                                                {:target :opacity :instance-id (:instance-id event)})
                                              nil))
                                          (:events result)))
                         (= :gesture/begin op) (conj {:target :param :instance-id instance-id :param param}))
          human? (or (some? authority) (gesture-ops op) (subscribers/human-source? source))]
      (if (seq destinations)
        (update result :model subscribers/override destinations (if human? :human-override :superseded)
                (cond-> {:op op :command-id command-id}
                  (map? source) (assoc :source (select-keys source [:adapter :session-id :device-id])))
                time-ms)
        result))))

(defn- committed-values
  "ID -> row lookup into the last committed frame, rich or compact. Rows are
  projected only when looked up, so ordinary launches pay nothing. tick! binds
  it to aim-paths/*committed-values* for its owner cycle only."
  [runtime]
  (when-let [{:keys [values compact]} (some-> runtime :state deref :output)]
    (let [indices (delay (when compact (zipmap (:target-order compact) (range))))]
      (fn [id]
        (or (get values id)
            (when-let [index (and compact (get @indices id))]
              (first (render/project-compact compact [index]))))))))

(defn- command
  ([model payload time-ms] (command model payload time-ms nil))
  ([model payload time-ms authority]
   (let [previous (get-in model [:control :history (:command-id payload) :command])
         time-ms (or (:time-ms previous) time-ms)
         payload (cond-> (prefabs/prepare-command model payload)
                   (::group-channels/text-addresses? (meta payload))
                   (group-channels/resolve-text-address model previous)
                   ;; A relative edit resolves against the committed value and commits as param/set (#813).
                   (= :param/nudge (:op payload))
                   (as-> p (param-nudge/as-set model p previous)))
         ;; Declarative code may use string identities (generated pages do).
         ;; JSON keyword normalization must still address those existing objects.
         payload (reduce (fn [p [field entries]]
                           (let [id (get p field) s (when (keyword? id) (subs (str id) 1))]
                             (if (and s (not (contains? entries id)) (contains? entries s))
                               (assoc p field s) p))) payload
                         [[:cue-id (get-in model [:registry :cues])]
                          [:page-id (:pages model)]])
         envelope (merge {:command-id (str (UUID/randomUUID))
                          :show-id (:show-id model) :epoch (:epoch model)
                          :source {:adapter :studio :session-id "studio"}}
                         payload {:time-ms time-ms})
         operation (:op envelope)
         variation? (variations/operations operation)
         unchanged-nudge? (and (= :param/nudge operation) (::param-nudge/unchanged envelope))
         admit (fn [model]
                 (cond
                   variation? (variations/apply-command model envelope)
                   ;; A nudge as-set could not resolve: rejected with its reason.
                   (= :param/nudge operation)
                   (control/apply-domain-command model envelope param-nudge/refuse!)
                   (group-channels/operation? operation)
                   (control/apply-domain-command model envelope group-channels/apply-operation)
                   (surface/operations operation)
                   (control/apply-domain-command model envelope surface/apply-operation)
                   (subscribers/operations operation)
                   (control/apply-domain-command model envelope subscribers/apply-operation)
                   ;; Linked knobs (#666).
                   (knobs/operations operation)
                   (control/apply-domain-command model envelope knobs/apply-operation)
                   ;; Audio-shaped LFOs (#676).
                   (audio-lfo/operations operation)
                   (control/apply-domain-command model envelope audio-lfo/apply-operation)
                   ;; Generated knob candidates (#667).
                   (knob-candidates/operations operation)
                   (control/apply-domain-command model envelope knob-candidates/apply-operation)
                   ;; Knob looks (#547).
                   (knob-presets/operations operation)
                   (control/apply-domain-command model envelope knob-presets/apply-operation)
                   :else (spatial/apply-command model envelope authority)))
         ;; A gesture takes over a ramped parameter from the ramp's current frame
         ;; value. Its begin writes nothing, so without this the parameter would
         ;; return to the ramp's start value until the first sample.
         held (if (and (= :gesture/begin operation) (nil? previous)
                       (seq (get-in model [:event-subscribers :ramps])))
                (subscribers/hold model [{:target :param :instance-id (:instance-id envelope) :param (:param envelope)}]
                                  time-ms)
                model)
         result (if unchanged-nudge?
                  ;; A nudge at a bound changes nothing and commits nothing.
                  {:model model :events [] :ack (param-nudge/unchanged-receipt model envelope)}
                  (let [result (admit held)]
                  ;; A refused gesture leaves the ramp and its destination untouched.
                  (if (or (identical? held model) (= :accepted (get-in result [:ack :status])))
                    result
                    (admit model))))
         accepted? (= :accepted (get-in result [:ack :status]))]
     (cond
       unchanged-nudge? result

       (and accepted? (= (:revision model) (get-in result [:model :revision])))
       (assoc result :replayed? true)

       (and accepted? (= :show/restart (:op payload)) (not= (:epoch model) (get-in result [:model :epoch])))
       (update result :model
               (fn [next-model]
                 (cond-> (restart-model next-model time-ms)
                   (contains? model ::staging/pending)
                   (as-> m (staging/collect (staging/begin m) :control
                                            {:kind :command :id (or (get-in payload [:source :adapter]) :studio)}
                                            (:events result))))))

       (and accepted? (not variation?))
       (-> (hold-manual-params model result payload authority)
           (override-ramps envelope authority))
       :else result))))

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

(defn- apply-composition-append [model {:keys [macro-id cue-id command-id base-revision cell] :as request}]
  (let [epoch (:epoch model)
        history (get-in model [:extensions ::composition-append])
        {:keys [floor receipts] :or {floor 0 receipts []}} (when (= epoch (:epoch history)) history)
        intent (select-keys request [:macro-id :cue-id :base-revision :cell])
        previous (some #(when (= command-id (:command-id %)) %) receipts)]
    (when-not (and (string? command-id) (not (str/blank? command-id)) (<= (count command-id) 256)
                   (integer? base-revision) (<= 0 base-revision (:revision model)))
      (throw (ex-info "Macro append needs a command ID and a valid base revision" {})))
    (if previous
      (if (= intent (:intent previous))
        {:model model :ack (:ack previous) :replayed? true}
        (throw (ex-info "Macro append command ID was already used with different data" {:command-id command-id})))
      (let [{:keys [page-id x y]} cell]
        (when (< base-revision floor)
          (throw (ex-info "Macro append is too old to confirm. Review the saved slots before starting a new append." {})))
        (when (and (contains? request :cell) (not= cue-id (get-in model [:pages page-id :cells [x y]])))
          (throw (ex-info "The cue cell changed. Open its actions again." {:cell cell})))
        (let [updated (composition/append-macro-cue model macro-id cue-id)
              ack {:status :accepted :command-id command-id :macro-id macro-id :revision (:revision updated)}
              receipts (conj receipts {:command-id command-id :intent intent :ack ack})
              evicted (when (> (count receipts) 128) (first receipts))]
          {:model (assoc-in updated [:extensions ::composition-append]
                            {:epoch epoch :floor (if evicted (get-in evicted [:ack :revision]) floor)
                             :receipts (if evicted (subvec receipts 1) receipts)})
           :ack ack})))))

(defn- cue-clock-identity [runtime {:keys [show-id epoch transport]}]
  [show-id epoch (:source transport) (:source-epoch transport)
   (:source-selection-epoch transport) @(:presentation-clock runtime)])

(defn- push-gesture-terminal? [payload {::keys [push-source]}]
  (and (#{:gesture/end :gesture/cancel} (:op payload))
       (#{:push-2 :push-3} (:adapter push-source)) (:session-id push-source)
       (= push-source (:source payload))))

(defn- push-gesture-key [{:keys [instance-id param control-id gesture-id]} {:keys [operator-id]}]
  [[:instance-param instance-id param] operator-id control-id gesture-id])

(defn- open-push-gesture [model [target actor control-id gesture-id]]
  (let [gesture (get-in model [:control :edit-history :open target])]
    (when (and gesture (= [actor control-id gesture-id] ((juxt :actor :control-id :gesture-id) gesture)))
      gesture)))

(defn- dispatch-ratio-pad-sample [runtime model payload time-ms {:keys [operator-id] ::keys [push-source] :as authority}]
  (let [session-id (:session-id push-source)
        trusted? (and (= :push-3 (:adapter push-source)) (= push-source (:source payload))
                      operator-id (= operator-id (get-in @(:devices runtime) [session-id ::ratio-pad-operator])))
        cached (get-in model [:control :history (:command-id payload)])
        failure (cond
                  (not trusted?) [:operator-required "A ratio pad sample needs its connected Push 3 session"]
                  (and (nil? cached) (not (push-variations/admissible-ratio-pad-sample? model payload)))
                  [:stale-ratio-pad "The shown ratio pad no longer matches the current cue"])]
    (if-let [[code message] failure]
      {:model model :events [] :ack {:status :rejected :command-id (:command-id payload) :code code :message message}}
      (command model payload time-ms authority))))

(def ^:private audition-state-keys
  [:keyframes :variable-observation :spatial-cache :hdr-scope :autopilot-cache :lighting-history
   :parameter-samples :modulation-view :preview-hdr :reload-waits])

(defn- audition-caches
  "Private render resources. The audition compiles for the CPU in its own
  cache and never reads or resets live caches, compute or GPU state, the
  effect worker pool or the spatial sampling pool."
  [runtime model]
  (let [context (atom (compiler-context (get-in model [:rig :fixtures]) (:batch? runtime) nil nil))]
    {:patch (atom nil) :capacity-plan (volatile! nil) :pause-layer-cache (volatile! nil)
     :response-cache (atom {:responses [] :middleware identity})
     :variable-plan (volatile! nil) :scene-plans (volatile! {})
     :optics-plan (atom (reflections/plan (:rig model)))
     :compiler-context context :compile (cached-compile context) :compile-worker nil
     :pure-bindings (atom #{})
     :state-migrations (atom {})
     :compute nil :executor nil :spatial-sampling nil}))

(defn- begin-audition
  "Start a private timeline from the committed frame state. Its first frame
  renders the staged model with the live frame's previous state, as the live
  show would after applying the draft at the staging time. :timeline names
  this timeline: a retune replaces the draft but keeps the timeline, so the
  worker and the owner's clock steps follow it rather than the draft."
  [runtime draft]
  (let [before @(:state runtime)]
    {:draft draft :timeline (Object.) :caches (audition-caches runtime (:model draft)) :next-model (:model draft)
     :frame (assoc (select-keys before audition-state-keys)
                   :model (:model before)
                   ;; Response smoothing seeds from the previous packets. Copy them so the
                   ;; audition never shares a buffer owned by live output.
                   :output (-> (:output before)
                               (dissoc :fog-demands :fog-control)
                               (update :universes update-vals #(aclone ^bytes %))))}))

(defn- draft-state
  "The owner's draft view for this cycle. Edits commit after the frame commits."
  [runtime]
  (or (::draft-view runtime) (:scene-draft runtime)))

(defn- scene-draft-command
  "Owner admission for Room drafts. Stage and discard change only owner draft
  state. Apply commits through one command transaction, now or at a grid
  boundary on the show transport."
  [runtime model payload time-ms]
  (let [command (merge {:command-id (str (UUID/randomUUID)) :show-id (:show-id model) :epoch (:epoch model)
                        :source {:adapter :studio :session-id "studio"}}
                       payload {:time-ms time-ms})
        {:keys [command-id draft-id op]} command
        state (draft-state runtime)
        {:keys [draft pending-apply]} @state
        reply (fn [status details]
                {:model model
                 :ack (merge (select-keys model [:show-id :epoch :revision])
                             {:status status :command-id command-id} details)})
        reject (fn [code message] (reply :rejected {:code code :message message}))
        cached (get-in model [:control :history command-id])]
    (cond
      (not= [(:show-id model) (:epoch model)] [(:show-id command) (:epoch command)])
      (reject :stale-epoch "The draft command belongs to another show session")

      ;; The draft is gone once applied, so a retry is answered from its receipt.
      (and cached (= op :scene-draft/apply))
      (let [intent #(select-keys % [:op :draft-id :expected-draft-revision])]
        (if (= (intent command) (intent (:command cached)))
          {:model model :ack (:ack cached) :replayed? true}
          (reject :command-id-conflict "Command ID reused with different data")))

      (and pending-apply (= command-id (get-in pending-apply [:command :command-id])))
      (reply :scheduled (merge (select-keys pending-apply [:due-beat :grid]) {:draft-id (:id draft) :duplicate? true}))

      (and pending-apply (not= op :scene-draft/discard))
      (reject :apply-pending "The draft is already scheduled to apply; discard it to cancel")

      (= op :scene-draft/stage)
      (let [{:keys [ack] staged :draft} (scene-draft/stage model draft command time-ms)]
        (when staged (reset! state (begin-audition runtime staged)))
        {:model model :ack (assoc ack :command-id command-id)})

      (nil? draft) (reject :no-draft "No Room draft is staged")

      (not= draft-id (:id draft)) (reject :wrong-draft "Another Room draft is staged")

      (= op :scene-draft/discard)
      (do (reset! state nil)
          (reply :accepted {:draft-id draft-id :draft-revision (:revision draft) :discarded? true
                            :cancelled-apply (get-in pending-apply [:command :command-id])}))

      (not= (:expected-draft-revision command) (:revision draft))
      (reject :stale-draft "The draft changed before application")

      (and (some? (:grid command)) (not= :off (:grid command)))
      (try
        (let [beat (get-in model [:transport :beat])
              due-beat (scene-draft/grid-boundary (:grid command) (:transport model))
              pending {:command (dissoc command :grid :time-ms) :grid (:grid command) :due-beat due-beat
                       :clock (cue-clock-identity runtime model) :last-beat beat
                       :requested-at {:time-ms time-ms :beat beat}}]
          (swap! state assoc :pending-apply pending)
          (reply :scheduled {:draft-id draft-id :draft-revision (:revision draft) :due-beat due-beat :grid (:grid command)}))
        (catch clojure.lang.ExceptionInfo error
          (reject (:code (ex-data error) :invalid-grid) (ex-message error))))

      :else
      (let [result (scene-draft/apply-draft model draft (dissoc command :grid))]
        (when (= :accepted (get-in result [:ack :status]))
          (reset! state {:last-outcome (assoc (select-keys (:ack result) [:command-id :revision :draft-id :draft-revision])
                                              :status :applied :time-ms time-ms)}))
        result))))

(defn- retunable?
  "True when a candidate audition at a new value can keep its private timeline:
  the same candidate on the same committed show, with nothing stale or failed."
  [model {:keys [draft retune error stale]} candidate]
  (and draft retune (not error) (not stale)
       (= knob-candidates/draft-id (:id draft))
       (= (:revision model) (get-in draft [:base :revision]))
       (= (:epoch model) (get-in draft [:base :epoch]))
       (= candidate (:candidate retune))
       (nil? (scene-draft/changed-dependency model draft))))

(defn- stage-candidate-audition
  "Stage the model's knob candidate (#667) in the Room at knob value: an
  audition-only draft whose private show has the candidate's links resolved and
  written in. Returns the staging receipt.

  Sweeping the value coalesces: while the candidate and the committed show are
  unchanged since staging, a new value bumps the draft revision and queues a
  retune that the audition worker writes into its running private model before
  its next frame. The private compile caches and timeline are kept, instead of
  copying the show and recompiling on every encoder step. Another candidate, an
  edit or any committed live change stages a fresh private copy. A retune is
  data (the candidate, the value and a sequence number), which the worker
  resolves with `knob-candidates/audition-model`."
  [runtime model value time-ms]
  (let [state (draft-state runtime)
        {:keys [draft retune] :as current} @state
        candidate (knob-candidates/candidate model)
        preview (knob-candidates/preview candidate value)]
    (if (retunable? model current candidate)
      (let [revision (inc (:revision draft))]
        (swap! state assoc
               :draft (assoc draft :revision revision :preview preview)
               :retune {:seq (inc (:seq retune)) :candidate candidate :value (double value)})
        (merge (select-keys (:base draft) [:show-id :epoch :revision])
               {:status :accepted :draft-id (:id draft) :draft-revision revision :retuned? true}))
      (let [{:keys [ack] staged :draft}
            (scene-draft/stage-preview model draft {:draft-id knob-candidates/draft-id :preview preview} time-ms
                                       #(knob-candidates/audition-model % candidate value))]
        (when staged
          (reset! state (assoc (begin-audition runtime staged)
                               :retune {:seq 0 :candidate candidate :value (double value)} :retuned 0)))
        ack))))

(defn- knob-candidate-audition
  "Owner admission for :knob-candidate/audition. Like a Room stage it leaves the
  show model untouched; the candidate's preview replaces the Room's frame."
  [runtime model payload time-ms]
  (let [{:keys [command-id value]} (merge {:command-id (str (UUID/randomUUID))} payload)
        reply (fn [status details]
                {:model model :ack (merge (select-keys model [:show-id :epoch :revision])
                                          {:status status :command-id command-id} details)})
        reject (fn [code message] (reply :rejected {:code code :message message}))]
    (cond
      (not= [(:show-id model) (:epoch model)]
            [(:show-id payload (:show-id model)) (:epoch payload (:epoch model))])
      (reject :stale-epoch "The audition belongs to another show session")
      (nil? (knob-candidates/candidate model)) (reject :no-candidate "Generate a candidate first")
      (not (and (number? value) (Double/isFinite (double value)) (<= 0 value 1)))
      (reject :invalid-value "An audition value is between 0 and 1")
      ;; A cue release (not a restart, which drops the candidate) can leave every
      ;; destination without a launch; the Room would show nothing of the knob.
      (not (knob-candidates/running-destinations? model (knob-candidates/candidate model)))
      (reject :no-running-cues "None of the candidate's cues is running; launch them or generate again")
      :else (let [ack (stage-candidate-audition runtime model value time-ms)]
              {:model model :ack (merge ack {:command-id command-id}
                                        (when (= :accepted (:status ack))
                                          {:value (double value)
                                           :candidate-id (:id (knob-candidates/candidate model))}))}))))

(defn- follow-candidate-audition!
  "After a candidate command, keep a running candidate audition on the current
  candidate at its knob value, or end it when no candidate remains."
  [runtime result time-ms]
  (let [state (draft-state runtime)
        {:keys [draft]} @state
        model (:model result)]
    (when (and (= :accepted (get-in result [:ack :status])) (= knob-candidates/draft-id (:id draft)))
      (if (knob-candidates/candidate model)
        (stage-candidate-audition runtime model (get-in draft [:preview :value] 0.5) time-ms)
        (reset! state nil)))
    result))

(def ^:private auto-envelope-keys
  "Request keys that say when and for whom an auto configure applies, not how
  auto mode plays."
  [:expected-epoch :runtime-id :at :event-id :label :command-id :source])

(defn- auto-request
  "The config an auto request installs on model, checked against the show:
  {:config .. :enabled? ..}. Studio's start and stop keep a plan authored at
  the REPL (#217). Throws when an enabled config is invalid or names a page or
  cue the show does not have."
  [model request]
  (let [enabled? (true? (:enabled? request))
        requested (policy/normalize-config (dissoc request :enabled? :seed :keep-plan?))
        config (if (:keep-plan? request)
                 (policy/keep-plan (get-in model [:auto :config]) requested enabled?)
                 requested)]
    (when enabled?
      ;; Section scopes are checked here too, not first when a section begins.
      (when-let [{:keys [path message]} (first (concat (policy/validate-references model config)
                                                       (policy/validate-config config)))]
        (throw (ex-info (str "Invalid auto config: " message (when (seq path) (str " at " (pr-str path))))
                        {:config config}))))
    {:config config :enabled? enabled?}))

(defn- configure-auto
  "Install an auto request now: end the looks the previous session still
  owns, then start a new session named owner-id. Returns the model with the
  installed :config, :enabled? and :state."
  [model request time-ms owner-id]
  (let [{:keys [config enabled?]} (auto-request model request)
        old-state (get-in model [:auto :state])
        model (if old-state
                (reduce (fn [m c] (:model (command m c time-ms))) model
                        (:commands (policy/stop-plan model old-state time-ms)))
                model)
        state (when enabled? (policy/initial-state (:seed request 0) time-ms owner-id))]
    {:config config :enabled? enabled? :state state
     :model (assoc model :auto (cond-> {:enabled? enabled? :config config :state state}
                                 ;; Settings another build wrote travel with a kept plan.
                                 (and (:keep-plan? request) (get-in model [:auto :unrecognized]))
                                 (assoc :unrecognized (get-in model [:auto :unrecognized]))))}))

(defn- auto-timing
  "Scheduler timing for an auto request's :at: a zero-based position
  {:bar n :beat n :phrase n}, or :next-bar, :next-phrase or :next-section
  (the end of the auto section now playing). JSON sends the words as text."
  [model at]
  (let [at (if (string? at) (keyword at) at)]
    (cond
      (map? at) {:at at}
      (= :next-bar at) {:grid {:bars 1}}
      (= :next-phrase at) {:grid {:phrases 1}}
      (= :next-section at)
      (let [{:keys [started-at duration]} (get-in model [:auto :state :section :timer])
            beat (get-in model [:transport :beat] 0)]
        (if (and (get-in model [:auto :enabled?]) (number? started-at) (= :beats (:unit duration)))
          ;; Relative, as the section timer is: a seek or loop moves both by
          ;; the same offset and a stop holds both.
          {:in {:beats (max 0 (- (+ started-at (:value duration)) beat))}}
          (throw (ex-info "No auto section is playing, so there is no section line; choose a bar or phrase line" {}))))
      :else
      (throw (ex-info "Auto :at is a zero-based position {:bar n :beat n :phrase n} or :next-bar, :next-phrase or :next-section"
                      {:at at})))))

(defn- late-position
  "Operator policy for a position that has already passed (#767): less than
  one bar late, the change applies now and is marked late by that many beats;
  a bar or more late it is refused. Returns the timing to post."
  [model timing]
  (let [beat (get-in model [:transport :beat] 0)
        meter (get-in model [:transport :meter])
        due (when (contains? timing :at) (subscribers/position-beat (:at timing) meter))
        late (when (and due (< due beat)) (- beat due))
        bar (:beats-per-bar (musical-time/normalize-meter meter))]
    (cond
      (nil? late) timing
      (< late bar) {:late-beats (double late)}
      :else (throw (ex-info (str "Position " (pr-str (:at timing)) " (beat " (double due) ") has already passed by "
                                 (double late) " beats, a bar or more; the transport is at beat " (double beat)
                                 ". Positions are zero-based: {:bar 0} is the first bar.")
                            {:due-beat due :beat beat})))))

(defn- auto-event
  "The :auto/configure event that carries request to its line. The request is
  checked now against the current show and again when it applies."
  [model request at label]
  (auto-request model request)
  (when (and (map? at) (not (and (seq at) (every? #{:beat :bar :phrase} (keys at))
                                 (every? #(and (number? %) (Double/isFinite (double %)) (not (neg? %))) (vals at)))))
    (throw (ex-info "A musical position needs nonnegative :beat, :bar and/or :phrase coordinates" {:at at})))
  (merge {:op :auto/configure :request request
          :label (or label (if (true? (:enabled? request)) "Auto configure" "Stop auto"))}
         (late-position model (auto-timing model at))))

(defn- schedule-auto
  "Post an auto request with :at on the beat scheduler. It applies on the
  first frame at or after its line, before the planner decides on that frame,
  so the previous settings make no decision on the line and the new ones none
  before it. The receipt names the event and its due beat."
  [model payload time-ms]
  (let [request (apply dissoc payload auto-envelope-keys)
        event (cond-> (auto-event model request (:at payload) (:label payload))
                (:event-id payload) (assoc :id (:event-id payload)))
        source (if (map? (:source payload)) (:source payload) {:adapter :repl :session-id "repl"})]
    (update (command model {:op :schedule/post :events [event] :source source
                            :command-id (or (:command-id payload) (str "auto/" (UUID/randomUUID)))}
                     time-ms)
            :ack #(cond-> (assoc % :scheduled? true)
                    (:late-beats event) (assoc :late? true :late-beats (:late-beats event))))))

(defn- prepare-auto-events
  "Check and normalize the :auto/configure events of a schedule post the way
  schedule-auto does, so a post through schedule_events cannot hold a config
  the show would refuse and meets the same late policy: a position passed by
  less than a bar applies at once, marked late; a bar or more is refused.
  Only that policy marks an event late; a post that supplies the marker is
  refused."
  [model payload]
  (if-not (and (= :schedule/post (:op payload)) (vector? (:events payload)))
    payload
    (update payload :events
            (partial mapv (fn [event]
                            (if (and (map? event) (#{:auto/configure "auto/configure"} (:op event)))
                              (let [request (:request event)]
                                (when-not (map? request)
                                  (throw (ex-info "An auto/configure event needs a request map" {})))
                                (when (or (contains? event :late-beats) (contains? event :late?))
                                  (throw (ex-info "Only the show's late policy marks an auto change late" {})))
                                (auto-request model request)
                                (cond-> (assoc event :request (policy/normalize-config request) :op :auto/configure)
                                  (map? (:at event))
                                  (as-> e (merge (dissoc e :at) (late-position model {:at (:at e)})))))
                              event))))))

(defn- trial-auto-plan
  "Plan one decision with the config and session just installed on model and
  throw if the planner fails, so a config it cannot run is refused before it
  is committed. Returns the planned {:state :commands :reasons}, or nil when
  auto is off; an immediate configure discards it, a due change commits it."
  [runtime model time-ms]
  (when (get-in model [:auto :enabled?])
    (let [fixtures (get-in model [:rig :fixtures])
          ;; An earlier queued rig edit may still be provisional. Validation
          ;; can reuse a matching cache without changing the output patch.
          plans (when (and runtime (= fixtures (:fixtures @(:patch runtime)))) (auto-plans runtime fixtures))]
      (policy/advance (assoc model :fixtures fixtures) (get-in model [:auto :config]) (get-in model [:auto :state])
                      time-ms plans))))

(declare ^:private advance-auto-plan)

(defn- scheduled-auto-configure
  "The owner's handler for a due :auto/configure step (#767). It installs the
  request through the same path as an immediate configure, checked against
  the show as it is now, and commits the new session's first decision on
  this frame, so the planner plans once (advance-auto then finds that
  decision's interval running). A request refused, by validation or by the
  planner, is rejected and recorded in the schedule's recent outcomes,
  leaving auto mode and its looks as they were. The step's command ID names
  the new session, so a replay is deterministic."
  [runtime model {:keys [request command-id time-ms]}]
  (try
    (let [{:keys [model enabled?]} (configure-auto model request time-ms command-id)
          planned (trial-auto-plan runtime model time-ms)
          model (if planned (advance-auto-plan model time-ms planned) model)]
      {:model model :events [] :result {:enabled? enabled?}})
    (catch InterruptedException error
      (.interrupt (Thread/currentThread))
      (throw error))
    (catch Exception error
      (throw (ex-info (str (or (.getMessage error) (.getName (class error))))
                      {:control/error :invalid-auto-config})))))

(defn- scheduler-host
  "Host operations the beat scheduler applies through the show owner."
  [runtime]
  {:auto/configure #(scheduled-auto-configure runtime %1 %2)})

(defn- dispatch-owner-command [runtime model payload time-ms authority]
  (cond
    (scene-draft/operations (:op payload))
    (scene-draft-command runtime model payload time-ms)

    ;; Generated knob candidates (#667): auditions stage in the Room; edits follow it.
    (knob-candidates/audition-operations (:op payload))
    (knob-candidate-audition runtime model payload time-ms)

    (knob-candidates/operations (:op payload))
    (follow-candidate-audition! runtime (command model payload time-ms authority) time-ms)

    (subscribers/schedulable? payload)
    (command model (prepare-auto-events model (subscribers/as-post payload)) time-ms authority)

    (and (= :cue/launch (:op payload)) (contains? payload :grid))
    (cue-timing/schedule model (cue-clock-identity runtime model) time-ms
                         (assoc (select-keys payload [:grid :raw]) :command (dissoc payload :grid :raw))
                         command)

    (and (cue-timing/active? model) (cue-timing/receipt model (:command-id payload)))
    {:model model :ack {:status :rejected :reason :command-id-conflict :command-id (:command-id payload)}}

    (= :schedule/post (:op payload))
    (command model (prepare-auto-events model payload) time-ms authority)

    (= :cue/cancel-launch (:op payload))
    (cue-timing/cancel-owned model (cue-clock-identity runtime model) time-ms payload command)

    (and (= :gesture/sample (:op payload)) (contains? payload :sample-context))
    (dispatch-ratio-pad-sample runtime model payload time-ms authority)

    (and (= :gesture/end (:op payload)) (push-gesture-terminal? payload authority))
    (let [gesture (open-push-gesture model (push-gesture-key payload authority))]
      (command model (cond-> payload gesture (assoc :last-sequence (:last-sequence gesture))) time-ms authority))

    ;; Linked knobs (#666): the knob value commits, then each linked write uses `command`.
    (knobs/value-operations (:op payload))
    (knobs/set-value model payload time-ms authority command)

    ;; Knob looks (#547): every stored knob commits at once, then the linked writes fan out the same way.
    (knob-presets/value-operations (:op payload))
    (knobs/set-value model payload time-ms authority command)

    :else (command model payload time-ms authority)))

(defn- require-import-device! [runtime {:keys [session-id lease]}]
  (when-not (and lease (identical? lease (get-in @(:devices runtime) [session-id ::restore-lease])))
    (throw (ex-info "Checkpoint restore belongs to a retired device session" {:code :stale-device-session}))))

(defn- prepare-publication!
  "Check a :definition `message` on the compile worker, ahead of the compiles
  queued there, and return it carrying the promise of that check (#784).
  `model` is the model to check against, or nil for the last committed one
  when the worker runs. Without a worker the message is returned unchanged
  and the owner checks it itself."
  [runtime message model]
  (if-let [submit (some-> (:compile-worker runtime) compile-submit)]
    (let [prepared (promise)
          task (programs/ahead
                (fn [] (deliver prepared (publication/prepare (or model (:model @(:state runtime))) (:payload message)))))]
      (try (submit task)
           (catch java.util.concurrent.RejectedExecutionException _ (deliver prepared {:unchecked true})))
      (assoc message ::prepared prepared))
    message))

(defn- apply-message [model {:keys [type payload expected-epoch authority] :as message} time-ms & [runtime]]
  (when (and (some? expected-epoch) (not= expected-epoch (:epoch model)))
    (throw (ex-info "Operation belongs to a previous show session" {:expected-epoch (:epoch model)
                                                                    :received-epoch expected-epoch})))
  (case type
    :command (dispatch-owner-command runtime model payload time-ms authority)
    :cue-schedule (cue-timing/schedule model (cue-clock-identity runtime model) time-ms payload command)
    :cue-cancel (cue-timing/cancel model (cue-clock-identity runtime model) payload)
    :spatial-cue (spatial-cues/spawn model payload time-ms command)
    :autopilot-authored (autopilot/accept-authored model payload time-ms command)
    :lighting-scene {:model (budget-policy/admit-scene model (lighting-scenes/apply-request model payload time-ms) (:source payload))
                     :ack {:status :accepted}}
    :video-mapping (if (= :surface-cue (:op payload))
                     (video-surface-cues/spawn model payload time-ms command)
                    (do
                     ;; Refuse a file source outside the show directory here,
                     ;; with its code, rather than saving it and listing it unavailable.
                     (when (and (= :put-source (:op payload)) (#{:image :feed} (get-in payload [:source :kind]))
                                (contains? (:source payload) :file))
                       (video-source/resolve-file (get-in payload [:source :file]) (:roots (:video runtime))))
                     {:model (update-in model [:settings :video] video-mapping/apply-operation payload)
                      :ack {:status :accepted}}))
    :personality (personalities/apply-operation model payload)
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
    (let [{:keys [kind]} payload
          definition (publication/revision model payload)
          captured (publication/captured model kind definition)
          prepared (::prepared message)
          ;; A worker checked this revision before the owner saw it (#784).
          ;; Until its verdict is ready, or when it checked another revision
          ;; than the current registry now derives, the owner holds it, and
          ;; the messages that depend on it, for one cycle. A publication
          ;; already held once, or whose second preparation is stale too, is
          ;; checked on the owner instead: a short inline check costs less
          ;; than holding input any longer.
          checked (cond
                    (nil? prepared) (publication/check kind definition captured)
                    (not (realized? prepared)) (if (::held? message)
                                                 (publication/check kind definition captured)
                                                 ::pending)
                    :else (or (publication/verdict-for @prepared definition captured)
                              (if (::reprepared? message)
                                (publication/check kind definition captured)
                                ::stale)))]
      (case checked
        ::pending {::hold (assoc message ::held? true)}
        ::stale {::hold (prepare-publication! runtime (assoc message ::reprepared? true ::held? true) model)}
        (do
          (some-> (:failure checked) throw)
          ;; A shader cue that alone exceeds the show's pixel budget could never render (#673).
          (when (and (= :effects kind) runtime (video-shader/shader? definition))
            (let [[w h] (get-in definition [:shader :size])
                  rate (* w h (get-in definition [:shader :fps] video-shader/default-fps))
                  budget (shader-producer/pixel-budget (:shader-video runtime))]
              (when (> rate budget)
                (let [message (str "This shader renders " rate " pixels per second; the show's budget is " budget)]
                  (throw (ex-info message {:code :shader/over-budget :rate rate :budget budget
                                           :errors [{:path [:shader :size] :message message}]}))))))
          (update (binding [spatial/*checked* checked]
                    (command model {:op :registry/publish :kind kind :definition definition} time-ms))
                  :ack merge {:kind kind :id (:id definition) :version (:version definition)}))))
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
    :composition-append (apply-composition-append model payload)
    :composition-cards {:model (composition/set-cards model (:id payload) (select-keys payload composition/card-changes))
                        :ack {:status :accepted :id (:id payload)}}
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
    (let [;; Validate against the definitions the last frame sampled: its
          ;; deferred compiles keep their captured definition (#774).
          hold (when runtime (:program-hold @(:state runtime)))
          updated (update (if (= type :modulation-bind)
                            ;; A move to another activation carries its phase at this command's clock.
                            (parameter-sources/bind model payload {:time-ms time-ms :transport (:transport model)}
                                                    (when hold (programs/frame-view model hold)))
                            (parameter-sources/unbind model payload))
                          :revision inc)]
      {:model updated
       :ack {:status :accepted :instance-id (:instance-id payload) :param (:param payload)
             :revision (:revision updated)}})
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
          controller (some #(when (= id (:id %)) %) (get-in model [:modulation :controllers]))
          ;; A refused controller has no schedule; stopping clears its record.
          refused (get-in model [:modulation :refused id])]
      (when-not (or controller refused) (throw (ex-info "Modulator is not active" {:id id})))
      (if-let [owner (:owner-instance-id (or controller refused))]
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
    (let [definition (aim-paths/definition model payload aim-paths/*committed-values*)
          id (:id definition)
          synchronized? (get-in definition [:editor :sync-arrival?])
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
                               :version (aim-paths/next-version (:registry model) :cues cue-id)
                               :reload-policy (if synchronized? :pinned :preserve-state)})
          result (apply-message (:model published)
                                {:type :agent-cue :payload {:page-id (:page-id payload :paths) :cue cue}} time-ms)]
      (when (= :rejected (get-in result [:ack :status]))
        (throw (ex-info "Cannot publish aim path cue" (:ack result))))
      (update result :ack merge
              (cond-> {:effect-id id :cue-id cue-id :version (:version definition)}
                synchronized?
                (assoc :arrival-duration (get-in definition [:editor :arrival-duration])
                       :arrival-duration-ms (get-in definition [:editor :arrival-duration-ms])))))
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
      (if (= scope :effect-group-channels)
        (dispatch-owner-command runtime model
                                (assoc (select-keys payload [:command-id :show-id :epoch :expected-revision :source])
                                       :op :effect-groups/configure :config-patch values)
                                time-ms authority)
        (let [values (cond-> values (and (= scope :output) (contains? values :pause-look))
                       (update :pause-look pause-look/normalize))
              settings (edit-scoped-settings model scope values (:device-id payload))
              _ (when (= scope :prefabs)
                  (prefabs/validate! settings)
                  (variable-blocks/compile-settings (get-in model [:settings :variable-blocks]) (:definitions settings)))
              _ (when (= scope :variable-blocks)
                  (variable-blocks/compile-settings settings (prefabs/catalog model)))
              _ (when (= scope :adaptive-keyframes) (cadence/settings settings))
              _ (when (= scope :section-patterns) (section-patterns/override-settings settings))
              _ (when (= scope :studio) (registry/validate-public-query-ids! (:public-queries settings)))
              _ (when (= scope :lighting-capacity) (capacity/settings settings))
              _ (when (= scope :controllers) (surface/validate-settings! settings))
              _ (when (= scope :audio-returns) (audio-return/validate-config model settings))
              settings (if (= scope :expressive) (expressive-mapping/normalize-config settings) settings)
              updated (-> (if (= scope :fog-control)
                            (fog-control/configure model values)
                            (assoc-in model [:settings scope] settings))
                          ;; An agent may only tighten the budget (agent-policy/policy).
                          (cond-> (= scope :lighting-budget) (as-> m (budget-policy/admit model m (:source payload))))
                          (update :revision inc))]
          {:model updated :ack {:status :accepted :revision (:revision updated)}})))
    :auto
    (if (some? (:at payload))
      ;; A musical :at waits on the beat scheduler and applies on its line (#767).
      (schedule-auto model payload time-ms)
      (let [request (apply dissoc payload auto-envelope-keys)
            ;; The epoch and revision name the session: unique within a show
            ;; session and the same on replay.
            {:keys [model enabled?]} (configure-auto model request time-ms (str "auto/" (:epoch model) "/" (:revision model)))
            ;; Stop auto means stop: a change still waiting on its line would start it again.
            {model :model withdrawn :event-ids} (if enabled? {:model model}
                                                  (subscribers/withdraw model #{:auto/configure} :auto-stopped
                                                                        (:source payload) time-ms))
            waiting (mapv :event-id (subscribers/upcoming model :auto/configure))]
        (trial-auto-plan runtime model time-ms)
        {:model (update model :revision inc)
         :ack (cond-> {:status :accepted}
                (seq withdrawn) (assoc :withdrawn-event-ids withdrawn)
                ;; What still waits on a line, so a stop that could not withdraw it says so.
                (seq waiting) (assoc :pending-event-ids waiting))}))
    :import
    (do
      (when authority (require-import-device! runtime authority))
      {:model (staging/collect (staging/begin (:model payload)) :show {:kind :studio :id :import}
                               [{:type :show/restored}])
       :restore? true :restored-output (:frame-output payload)
       :candidate (:candidate payload) :ack {:status :accepted :epoch (get-in payload [:model :epoch])}})
    (throw (ex-info "Unknown studio operation" {:type type}))))

(defn- adopt-equal-rig!
  "Store `fixtures` as the patched rig when it equals the stored one, so later
  identity checks against the model's vector hit again: a restore of the same
  show, or an edit that rewrites the rig to equal content, keeps the compiled
  patch and its capability index (#798)."
  [runtime fixtures]
  (swap! (:patch runtime) #(if (and (not (identical? fixtures (:fixtures %))) (= fixtures (:fixtures %)))
                             (assoc % :fixtures fixtures)
                             %)))

(defn- patch! [runtime fixtures]
  (let [cached @(:patch runtime)]
    (if (= fixtures (:fixtures cached))
      (do (when-not (identical? fixtures (:fixtures cached)) (adopt-equal-rig! runtime fixtures))
          (:compiled cached))
        (let [patch (render/compile-patch fixtures)]
          (reset! (:patch runtime) {:fixtures fixtures :compiled patch :brightness-gains (preview/brightness-gains fixtures)
                                  :retirement (retirement/plan (:retirement cached) (:compiled cached) patch)})
          patch))))

(defn- rig-indexer
  "The capability rig index for launches admitted in one owner cycle (#798),
  kept with this runtime's compiled patch: built on first use for the
  patched rig and dropped when a rig edit replaces the patch. A rig equal to
  the patched one, such as a restored copy, adopts it and keeps the index; a
  different rig is indexed for that call alone."
  [runtime]
  (fn [fixtures]
    (let [cached @(:patch runtime)
          cached (if (or (identical? fixtures (:fixtures cached)) (not= fixtures (:fixtures cached)))
                   cached
                   (adopt-equal-rig! runtime fixtures))]
      (if (identical? fixtures (:fixtures cached))
        (or (:capability-index cached)
            (let [index (capability/rig-index fixtures)]
              (swap! (:patch runtime) #(if (identical? fixtures (:fixtures %)) (assoc % :capability-index index) %))
              index))
        (capability/rig-index fixtures)))))

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
    (programs/retire! @(:compiler-context runtime))
    (reset! (:compiler-context runtime)
            (compiler-context fixtures (:batch? runtime) (:compute runtime) (compile-submit (:compile-worker runtime))))))

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

(defn- advance-auto-plan [model time-ms {:keys [state commands reasons]}]
  (let [fixtures (get-in model [:rig :fixtures])
        [model state rejected]
        (reduce (fn [[m s rejected] c]
                  (let [{:keys [model ack]} (command m c time-ms)
                        launch? (= :cue/launch (:op c))]
                    (cond
                      (and launch? (= :accepted (:status ack)) (:instance-id ack))
                      [model (policy/record-launch s (:instance-id ack) (get-in model [:instances (:instance-id ack)])) rejected]
                      ;; A launch the show refuses at dispatch is remembered too (#217).
                      (and launch? (= :rejected (:status ack)))
                      [model (policy/record-refusal s (assoc model :fixtures fixtures) (:cue-id c) ack (get-in c [:launch :params]))
                       (conj rejected {:cue-id (:cue-id c) :code (:code ack) :message (:message ack)})]
                      :else [model s rejected])))
                [model state []] commands)
        reasons (cond-> reasons (seq rejected) (conj {:reason :launch-refused :refused rejected}))]
    ;; Keep the last decision's reasons visible between decisions.
    (cond-> (assoc-in model [:auto :state] state)
      (seq reasons) (assoc-in [:auto :reasons] reasons))))

(defn- stop-failed-auto
  "Switch auto mode off after a planner failure and end the looks it still
  owns, as Stop auto would, so nothing it launched keeps running while the
  panel reads off. Looks a performer took over stay."
  [model time-ms failure]
  (let [state (get-in model [:auto :state])
        commands (try (:commands (policy/stop-plan model state time-ms)) (catch Exception _ []))
        model (reduce (fn [m c] (:model (command m c time-ms))) model commands)]
    (-> model
        (assoc-in [:auto :enabled?] false)
        (assoc-in [:auto :state] nil)
        (assoc-in [:auto :reasons] [(assoc failure :released (count commands))]))))

(defn- advance-auto [model time-ms runtime]
  (if (get-in model [:auto :enabled?])
    (let [fixtures (get-in model [:rig :fixtures])
          planned (try (policy/advance (assoc model :fixtures fixtures)
                                       (get-in model [:auto :config]) (get-in model [:auto :state])
                                       time-ms (auto-plans runtime fixtures))
                       ;; A planner failure stops auto mode with its reason; the
                       ;; show keeps rendering (#217).
                       (catch Throwable e
                         (when (instance? VirtualMachineError e) (throw e))
                         ;; Shutdown interrupts the owner: keep the interrupt, never switch auto off for it.
                         (when (instance? InterruptedException e)
                           (.interrupt (Thread/currentThread))
                           (throw e))
                         {:failed {:reason :planner-failed :message (or (.getMessage e) (.getName (class e)))}}))]
      (if-let [failure (:failed planned)]
        (stop-failed-auto model time-ms failure)
        (advance-auto-plan model time-ms planned)))
    model))

(defn- output-retirement [runtime {:keys [compiled retirement] :as context}]
  (let [{wire-context :context wire-plan :plan :keys [possible] :as wire} @(:wire-retirement runtime)]
    (if (and (identical? context wire-context) (nil? possible))
      wire
      (let [previous (:compiled wire-context)
            ;; A partial handoff can already drive a pump. Keep its off byte ahead
            ;; of light off bytes for unclaimed slots. Within fog, prefer the last
            ;; complete handoff, then the most recent possible fog encoding.
            fog-off (merge (:fog-off-bytes possible) (:fog-off-bytes wire))
            plan (if (or wire-context possible)
                   (retirement/plan {:known (into (:known retirement) (concat (:known wire-plan) (:known possible)))
                                     :held (merge (:held retirement) (:held possible) (:held wire-plan)
                                                  (render/physical-off-bytes previous) fog-off)}
                                    nil compiled)
                   retirement)]
        {:context context :plan plan
         ;; The current patch claims slots on a complete handoff, clearing any
         ;; old fog byte there. A retired, still unclaimed fog slot keeps its byte.
         :fog-off-bytes (merge (select-keys fog-off (keys (:held plan))) (:fog-off-bytes compiled))}))))

(defn- guard-output [runtime compiled retirement universes {:keys [safety-state stale-source?]}]
  (let [packet (retirement/include-retired retirement universes)]
    (if (seq safety-state)
      (fog-deadline/guard-source @(:fog-deadlines runtime) compiled packet safety-state
                                 ((get-in runtime [:publisher :now-ms])) stale-source?)
      packet)))

(defn- prepare-output [runtime context universes source]
  (let [{:keys [plan] :as wire} (output-retirement runtime context)]
    (vary-meta (guard-output runtime (:compiled context) plan universes source)
               assoc ::wire-retirement wire)))

(defn- hold-unchanged-fixtures
  "Start with current idle output. Preserve complete wire values for compatible
  channels and use current off values for fog."
  [compiled wire-patch universes]
  (let [output (render/idle-universes compiled)
        mapping [:universe :address :footprint :channels]]
    (doseq [{:keys [id compiled-channels] :as fixture} (:fixtures compiled)
            :let [previous (get-in wire-patch [:by-id id])]
            :when (and previous (= (select-keys fixture mapping) (select-keys previous mapping)))
            [attribute channel] compiled-channels
            :when (not= :fog attribute)
            :let [slots (keep channel [:coarse :fine])]
            :when (every? (fn [[universe _]] (contains? universes universe)) slots)
            [universe index] slots]
      (aset-byte ^bytes (get output universe) (int index)
                 (aget ^bytes (get universes universe) (int index))))
    output))

(defn- prepare-safety-output
  [runtime universes {:keys [output-context wire-patch stale-source? uncertain?] :as source}]
  (let [compiled (:compiled output-context)
        {retirement :plan :as wire} (output-retirement runtime output-context)
        layout-changed? (and (contains? source :wire-patch) (not= wire-patch compiled))
        off-bytes (:fog-off-bytes compiled)
        corrected (if layout-changed?
                    (retirement/include-retired retirement (hold-unchanged-fixtures compiled wire-patch universes))
                    (guard-output runtime compiled retirement universes source))
        corrected (if (and (not layout-changed?) (or stale-source? uncertain?) (seq off-bytes))
                    (let [missing (into #{} (comp (map (comp first key)) (remove #(contains? corrected %))) off-bytes)
                          packet (if (seq missing) (merge corrected (select-keys (render/idle-universes compiled) missing)) corrected)]
                      (fog-deadline/apply-off-bytes packet off-bytes))
                    corrected)]
    ;; A transport can fail after writing only part of the attempted packet.
    (when (or (and uncertain? (or layout-changed? (seq off-bytes) (seq (:held retirement))))
              (and (not (identical? universes corrected))
                   (some (fn [[universe ^bytes bytes]]
                           (not (Arrays/equals ^bytes (get universes universe) bytes)))
                         corrected)))
      (vary-meta corrected assoc ::wire-retirement wire))))

(defn- present-output! [runtime packet frame-number]
  (let [metadata (meta packet)
        {:keys [context plan] :as wire} (::wire-retirement metadata)]
    (try
      ((get-in runtime [:sink :send!]) (with-meta packet (dissoc metadata ::wire-retirement)) frame-number)
      (when (:plan @(:venue-fog runtime))
        (venue-fog/presented! (:venue-fog runtime) ((get-in runtime [:publisher :now-ms])) packet))
      (catch Exception error
        ;; A failed handoff may already have driven some universes. Remember one
        ;; possible off byte per slot and one possible fog off byte per fog slot.
        (let [compiled (:compiled context)
              off-bytes (render/physical-off-bytes compiled)]
          (swap! (:wire-retirement runtime) update :possible
                 (fn [{:keys [known held fog-off-bytes]}]
                   {:known (into (or known #{}) (:known plan))
                    :held (merge held (:held plan) off-bytes)
                    :fog-off-bytes (merge fog-off-bytes (:fog-off-bytes compiled))})))
        (throw error)))
    (reset! (:wire-retirement runtime) wire)))

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
     ;; Audio-shaped LFOs (#676): the worker reads the specs and returns the
     ;; newest extraction per LFO. The mailbox keeps the newest per LFO.
     :shape-specs! (fn []
                     (let [model (get-in @(:state runtime) [:model])]
                       (when (and @(:running runtime) (= epoch (:epoch model)))
                         (audio-lfo/extractor-specs model))))
     :shapes! (fn [shapes]
                (when (current?)
                  (locking (:audio-input runtime)
                    (when (= epoch (:epoch @(:audio-input runtime)))
                      (swap! (:audio-input runtime) update :shapes merge shapes)))))
     :done! (fn [result]
              (when (current?)
                (swap! (:state runtime)
                       #(if (= epoch (get-in % [:model :epoch]))
                          (assoc-in % [:status :audio-result] (select-keys result [:status :error])) %))))}))

(defn- consume-audio! [runtime model time-ms]
  (let [captured (locking (:audio-input runtime)
                   (let [before @(:audio-input runtime)]
                     (reset! (:audio-input runtime) {:epoch (:epoch model) :events [] :features nil :shapes {}})
                     before))]
    (when (and (:features captured) (= (:epoch model) (:epoch captured)))
      (let [age (max 0 (- ((get-in runtime [:publisher :now-ms])) (:arrived-at-ms captured)))]
        (vreset! (:capacity-audio runtime) {:epoch (:epoch model) :received-ms (- time-ms age)
                                           :audio/energy (get-in captured [:features :audio/energy])})))
    (if (= (:epoch model) (:epoch captured))
      (let [;; Stems drive show progress (#675): while the transport is stopped
            ;; their onsets launch nothing and their levels stop feeding routes,
            ;; and play resumes from fresh audio with no backlog.
            stopped? (false? (get-in model [:transport :playing?]))
            onset? #(= :audio/onset (:type %))
            suppressed (if stopped? (count (filter onset? (:events captured))) 0)
            captured (cond-> captured
                       stopped? (update :events #(vec (remove onset? %)))
                       (and stopped? (:features captured)) (update :features assoc :stems-held? true))]
      (cond-> model
        (pos? suppressed) (update-in [:variables :audio :suppressed-onsets] (fnil + 0) suppressed)
        (seq (:shapes captured)) (audio-lfo/admit (:shapes captured))
        (:features captured) (routing/admit-audio (:features captured))
        (:features captured) (analysis/publish-features (:features captured))
        (seq (:events captured)) (as-> m (reduce
            (fn [acc event] (staging/collect acc :audio {:kind :audio :id (or (get-in event [:source :id]) :analysis)} [event]))
            m (:events captured)))
        (seq (:events captured)) (assoc-in [:variables :audio :events] (:events captured))
        (seq (:events captured)) (timeline/remember-sections (:events captured))
        (seq (:events captured)) (update-in [:variables :audio :event-count] (fnil + 0) (count (:events captured))))) model)))

(defn section-reference-callbacks
  "Get callbacks for the attached reference session in the current show epoch.
  Capture timestamps with :now-ms! and submit reference-position state with
  :publish!. Construct the adapter with the returned :provenance and
  :alignment-options. :state! projects the reference lock. After reattach,
  restart or restore, old callbacks reject publication and return no state."
  [runtime]
  (when-let [input (get-in runtime [:section-reference :input])]
    (let [{:keys [epoch position] :as attached} @input
          {:keys [provenance options]} position
          now-ms! (get-in runtime [:publisher :now-ms])
          current? (fn [stored]
                     (and @(:running runtime)
                          (= epoch (:epoch stored) (get-in @(:state runtime) [:model :epoch]))
                          (= provenance (get-in stored [:position :provenance]))))]
      (when (current? attached)
        {:provenance provenance
         :alignment-options options
         :now-ms! now-ms!
         :state! (fn []
                   (let [stored @input]
                     (when (current? stored)
                       (reference-position/snapshot (:position stored) (now-ms!)))))
         :publish! (fn [{candidate-time :now-ms :keys [occurrence generation] :as candidate}]
                     (when (current? @input)
                       (let [now (now-ms!)]
                         (locking input
                           (let [{previous :position :as stored} @input]
                             (when (and (current? stored)
                                        (= provenance (:provenance candidate))
                                        (= options (:options candidate))
                                        (number? candidate-time) (Double/isFinite (double candidate-time))
                                        (<= (:now-ms previous candidate-time) candidate-time now)
                                        (integer? occurrence) (<= (:occurrence previous) occurrence)
                                        (integer? generation) (<= (:generation previous) generation))
                               ;; Verify the candidate without storing a projection for future frames.
                               (reference-position/snapshot candidate now)
                               (reset! input (assoc stored :position candidate))
                               true))))))}))))

(defn attach-section-reference!
  "Replace the attached reference session with a fresh caller-chosen ID and
  return its callbacks. Source, clock, profile and alignment options stay bound
  to the running show. Call after a player reconnect, show restart or restore.
  Session IDs are unique for the lifetime of this show runtime."
  [runtime session-id]
  (when-let [{:keys [initial input]} (:section-reference runtime)]
    (let [position (reference-position/new-state (assoc (:provenance initial) :session-id session-id)
                                                 (:options initial))]
      ;; Serialize with frame commits and restore. Old publishers also check
      ;; the exact session again under input before storing a candidate.
      (locking (:lock runtime)
        (when @(:running runtime)
          (locking input
            (let [{:keys [used-sessions]} @input]
              (when (contains? used-sessions session-id)
                (throw (ex-info "Section reference session was already used" {:session-id session-id})))
              (reset! input {:epoch (get-in @(:state runtime) [:model :epoch])
                             :position position :used-sessions (conj used-sessions session-id)})
              (section-reference-callbacks runtime))))))))

(defn section-reference-status
  "Project local player alignment for the given committed show."
  ([runtime] (section-reference-status runtime @(:state runtime)))
  ([runtime {:keys [model]}]
   (mpv-session/status (:section-player runtime)
                       (assoc (select-keys model [:show-id :epoch])
                              :runtime-id (:id runtime)
                              :override? (get-in model [:settings :section-patterns :override?] false)))))

(defn reconnect-section-reference!
  "Reconnect the configured local player through the active show's reference
  session. The request carries the runtime identity and the expected show epoch."
  [runtime {:keys [runtime-id expected-epoch] :as request}]
  (let [refuse (fn [reason message] {:status :rejected :reason reason :message message})
        result (cond
                 (not (and (map? request) (= #{:runtime-id :expected-epoch} (set (keys request)))
                           (string? runtime-id) (nat-int? expected-epoch)))
                 (refuse :invalid-request "Reconnect requires only the runtime ID and expected show epoch")

                 (nil? (:section-player runtime))
                 (refuse :not-configured "No local mpv reference is configured for this show")

                 :else
                 (mpv-session/reconnect! (:section-player runtime)
                   (fn []
                     (locking (:lock runtime)
                       (let [{:keys [show-id epoch]} (:model @(:state runtime))]
                         (if (and @(:running runtime) (= runtime-id (:id runtime)) (= epoch expected-epoch))
                           (let [session-id (str (random-uuid))]
                             {:status :accepted :runtime-id runtime-id :show-id show-id :epoch epoch
                              :session-id session-id :callbacks (attach-section-reference! runtime session-id)})
                           (refuse :stale-show-session "Reconnect belongs to a previous show session")))))))]
    (assoc result :operation :reconnect)))

(defn- reference-evidence [runtime show-epoch]
  (when-let [input (get-in runtime [:section-reference :input])]
    (let [{:keys [epoch position]} @input]
      (when (= show-epoch epoch)
        {:reference-state position
         :reference-now-ms ((get-in runtime [:publisher :now-ms]))}))))

(defn- section-clock-session [runtime model]
  (str (:epoch model) "/" (get-in model [:transport :source-selection-epoch]) "/" @(:presentation-clock runtime)))

(defn- section-capture-stream
  [connection {capture-state :state :keys [run-id timing-valid? closed? config]}]
  (let [source-id (get-in config [:analyzer-options :source-id] :audio-input)]
    (when (and run-id timing-valid? (not closed?) (#{:running :completed} capture-state)
               (not= "" source-id) (connections/restart-requested? connection :audio))
      {:source-id source-id :session-id run-id})))

(defn- advance-section-patterns [runtime before next-state reset?]
  (if-let [initial (:section-patterns runtime)]
    (if reset?
      (dissoc next-state :section-pattern-state :section-clock-session)
      (let [{:keys [model audio-clock-confidence]} next-state
            previous (:model before)
            {:keys [source source-epoch playing? beat]} (:transport model)
            clock-session (section-clock-session runtime model)
            connection (:connections runtime)
            capture-status (audio/status (connections/audio connection))
            analyzer-options (get-in capture-status [:config :analyzer-options])
            active-clock? (and (true? playing?) (integer? source-epoch) (number? audio-clock-confidence)
                               (>= audio-clock-confidence (get analyzer-options :beat-clock-confidence-min 0.5)))
            stream (section-capture-stream connection capture-status)
            causal-events (into [] (comp (filter #(= :audio (:category %))) (map :event)
                                         (filter #(and (= :audio/section-change (:type %))
                                                       (= (:run-id capture-status) (:capture-run-id %))
                                                       (true? (get-in % [:timing :capture-clock-valid?])))))
                                (staging/pending model))
            boundaries (when (and active-clock? (= clock-session (:section-clock-session before))
                                  (true? (get-in previous [:transport :playing?])))
                         (into #{} (map (comp keyword name :type))
                               (show-events/emitted {:before previous :after model})))
            metrics {:now-ms (:time-ms model) :causal-stream stream :causal-events causal-events
                     :override? (get-in model [:settings :section-patterns :override?] false)
                     :transport {:show-epoch (:epoch model) :source (or source :manual)
                                 :source-epoch (or source-epoch 0) :session-id clock-session
                                 :playing? (boolean active-clock?) :beat beat :boundaries boundaries}}
            result (section-patterns/advance (or (:section-pattern-state before) initial)
                                             (merge metrics (reference-evidence runtime (:epoch model))))]
        (-> next-state
            (assoc :section-pattern-state (:state result) :section-clock-session clock-session)
            (update :model staging/collect :music {:kind :music :id :section-patterns} (:events result)))))
    next-state))

(defn- section-pattern-run? [{:keys [event]}]
  (and (= :music (:category event)) (= :music/section-pattern (:type event))))

(defn- fence-section-pattern-runs [model runtime clock-confidence]
  ;; Subscriber actions can be due well after the pattern was admitted.
  (let [path [:event-subscribers :runs]
        runs (get-in model path)]
    (if-not (some section-pattern-run? runs)
      model
      (let [{:keys [playing? source source-epoch]} (:transport model)
            scope {:show-epoch (:epoch model) :clock [(or source :manual) source-epoch (section-clock-session runtime model)]}
            connection (:connections runtime)
            capture-status (audio/status (connections/audio connection))
            min-confidence (get-in capture-status [:config :analyzer-options :beat-clock-confidence-min] 0.5)
            active? (and (:section-patterns runtime) (true? playing?) (integer? source-epoch)
                         (number? clock-confidence) (>= clock-confidence min-confidence)
                         (not (get-in model [:settings :section-patterns :override?])))
            projection (when active?
                         (when-let [{:keys [reference-state reference-now-ms]} (reference-evidence runtime (:epoch model))]
                           (reference-position/snapshot reference-state reference-now-ms)))
            fence (select-keys projection [:provenance :occurrence :generation])
            capture-stream (section-capture-stream connection capture-status)]
        (assoc-in model path
                  (filterv (fn [{:keys [event] :as run}]
                             (or (not (section-pattern-run? run))
                                 (let [{:keys [mode reference] :as receipt} (:data event)]
                                   (and active? (= scope (:scope receipt))
                                        (case mode
                                          :causal (and (some? capture-stream) (= capture-stream (:stream receipt)))
                                          :reference (and (= :locked (:status projection))
                                                          (= fence (:fence receipt))
                                                          (= reference (:reference projection)))
                                          false)))))
                           runs))))))

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

(declare live-run-clip! live-parameter-device-value!)

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
     :run-clip! (fn [event] (when (current?) (live-run-clip! runtime event)))
     :parameter-device! (fn [event] (when (current?) (live-parameter-device-value! runtime event)))
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

(defn- observer-error!
  "Report an observer failure in status. With background observers the
  worker keeps it until the owner's next cycle publishes it, since only the
  owner writes the committed state."
  [runtime k error]
  (if-let [worker (:observer-worker runtime)]
    (swap! (:errors worker) assoc k (error-data error))
    (swap! (:state runtime) assoc-in [:status k] (error-data error))))

(defn- observe-phrase-capture!
  "Fold the committed model into the capture buffer and record what it cost.
  The fold is pure; a failure is reported in status and never stops a frame."
  [runtime model]
  (when-let [state (:phrase-capture runtime)]
    (when (:enabled? @state)
      (let [start (System/nanoTime)]
        (try
          (let [buffer (phrase-capture/observe-model (:buffer @state) model)
                ns (- (System/nanoTime) start)]
            (swap! state (fn [s] (-> s (assoc :buffer buffer)
                                     (update :cost #(-> % (update :samples inc) (update :total-ns + ns)
                                                        (update :max-ns max ns) (assoc :last-ns ns)))))))
          (catch Exception error
            (observer-error! runtime :phrase-capture-error error)))))))

(defn- run-clip-envelope [runtime model]
  {:command-id (str (UUID/randomUUID)) :show-id (:show-id model) :epoch (:epoch model) :time-ms (now-ms runtime)})

(def ^:private run-clip-inbox-limit 256)

(defn- advance-run-player!
  "Playback of run clips, after each committed frame: on the owner, or with
  :background-observers on the observer thread (or whichever thread settles
  the observers). Decode the queued run-clip events, bind
  launch receipts from their reply promises (accepted or rejected), and send
  the moves and releases playing takes have reached. The player lock is held
  only while the pure player runs; commands are submitted after it is
  released, so a callback or a sync can never wait on the owner while the
  owner waits on them. An end the owner queue refused is sent again while
  its instance still runs, so a replayed cue cannot keep running."
  [runtime state model]
  (when (:player @state)
    (let [envelope #(run-clip-envelope runtime model)
          instances (:instances model)
          ;; At most 8 attempts per end, so a refusal that persists cannot loop.
          resend (into [] (keep (fn [[_ {:keys [reply instance-id attempt]}]]
                                  (when (and (realized? reply) (= :rejected (:status @reply)) (contains? instances instance-id)
                                             (< (or attempt 1) 8))
                                    (merge (envelope) {:op :instance/end :instance-id instance-id ::attempt (inc (or attempt 1))
                                                       :source (get-in @state [:player :source])}))))
                       (:end-replies @state))
          _ (swap! state update :end-replies
                   (fn [rs] (into {} (remove (fn [[_ {:keys [reply]}]] (realized? reply))) rs)))
          commands (locking (:player-lock @state)
                     (let [[old _] (swap-vals! state assoc :inbox [])
                           replies (:replies old {})
                           played (reduce (fn [{:keys [player commands]} event]
                                            (let [r (live-runs/play player (:record old) event envelope)]
                                              {:player (:player r) :commands (into commands (:commands r))}))
                                          {:player (:player @state) :commands []}
                                          (:inbox old))
                           acked (reduce (fn [{:keys [player commands]} id]
                                           (let [reply (get replies id)]
                                             (if (and reply (realized? reply))
                                               (let [r (live-runs/acknowledge player @reply envelope)]
                                                 {:player (:player r) :commands (into commands (:commands r))})
                                               {:player player :commands commands})))
                                         {:player (:player played) :commands (:commands played)}
                                         (live-runs/pending-launches (:player played)))
                           advanced (live-runs/advance (:player acked) (get-in model [:transport :beat] 0) envelope
                                                       (set (keys (:instances model))))
                           pending (live-runs/pending-launches (:player advanced))]
                       (swap! state #(-> % (assoc :player (:player advanced))
                                         (update :replies (fn [rs] (select-keys rs pending)))))
                       (into (:commands acked) (:commands advanced))))]
      (doseq [c (into resend commands)]
        (let [reply (submit! runtime (-> c (dissoc ::attempt) (assoc :expected-epoch (:epoch model))))]
          (case (:op c)
            :cue/launch (swap! state assoc-in [:replies (:command-id c)] reply)
            :instance/end (swap! state assoc-in [:end-replies (:command-id c)]
                                 {:reply reply :instance-id (:instance-id c) :attempt (get c ::attempt 1)})
            nil))))))

(def ^:private device-inbox-limit 1024)

(def ^:private device-events-per-cycle 256)

(defn- device-catalog [model] (parameter-sources/waveform-options model))

(defn- device-layouts
  "Slot layouts follow the record's channels and the registry. They are
  refreshed only when either changed, not on every owner cycle."
  [s model]
  (let [registry (:registry model) channels (:channel-order (:record s))
        [r c] (:device-layout-key s)]
    (if (and (identical? r registry) (= c channels))
      s
      (-> s (update :devices live-devices/ensure-layouts model (:record s) (device-catalog model))
          (assoc :device-layout-key [registry channels])))))

(declare live-bridge-connection)

(defn- device-writer
  "The runtime's one writer to Live parameter devices: a single daemon thread,
  so sends leave in the order the owner accepted them and never pile up."
  []
  (java.util.concurrent.Executors/newSingleThreadExecutor
   (reify java.util.concurrent.ThreadFactory
     (newThread [_ r] (doto (Thread. ^Runnable r "tolglow-live-parameter-devices") (.setDaemon true))))))

(defn- send-device-writes!
  "Send one batch, one request per run track in track order. A write that did
  not execute forgets only its own expected echo (by sequence number)."
  [runtime state outbox]
  (let [send! (or (:device-send! @state)
                  (fn [payload]
                    (if (live-sync/bridge-connected? (connections/sync (:connections runtime)))
                      (live-connection/request! (live-bridge-connection runtime) :set-parameter-values payload)
                      {:status :rejected :code :not-connected})))]
    (doseq [[track-id {:keys [cue-id lane values]}] (sort-by key outbox)]
      (let [ordered (map val (sort-by key values))
            payload {:track-id track-id :expected-name (live-runs/lane-track-name cue-id lane)
                     :values (mapv #(dissoc % :slot :seq) ordered)}
            receipt (try (send! payload) (catch Exception error {:status :indeterminate :message (.getMessage error)}))
            status (:status receipt)]
        (locking (:player-lock @state)
          (swap! state (fn [s]
                         (cond-> (-> (update-in s [:device-sends :requests] inc)
                                     (update-in [:device-sends :last-seq] (fnil max 0) (reduce max 0 (keep :seq ordered))))
                           (= :executed status)
                           (-> (update-in [:device-sends :executed] inc)
                               ;; Slots the companion set itself (or found already showing
                               ;; the value) send no echo: stop waiting for one.
                               (update :devices live-devices/writes-confirmed track-id (get-in receipt [:result :own])
                                       (into {} (keep (fn [{:keys [slot seq]}] (when seq [slot [seq]]))) ordered)))
                           (= :not-connected (:code receipt)) (update-in [:device-sends :not-connected] inc)
                           (not= :executed status)
                           (-> (update-in [:device-sends :failed] inc)
                               (assoc-in [:device-sends :last-failure] (select-keys receipt [:status :code :message]))
                               (update :devices live-devices/forget-writes track-id
                                       (into {} (keep (fn [{:keys [slot seq]}] (when seq [slot [seq]]))) ordered)))))))))))

(defn- drain-device-writes!
  "On the writer thread: send what waits, then look again, until the outbox
  is empty; only then may the owner schedule another drain."
  [runtime state]
  (try
    (loop []
      (let [[old _] (swap-vals! state assoc :device-outbox {})]
        (when (seq (:device-outbox old))
          (send-device-writes! runtime state (:device-outbox old))))
      (let [[old _] (swap-vals! state (fn [s] (if (empty? (:device-outbox s)) (assoc s :device-flush? false) s)))]
        (when (seq (:device-outbox old)) (recur))))
    (finally
      ;; Never leave the flag set by an escaped exception: the owner would stop
      ;; scheduling drains and writes would pile up unsent.
      (swap! state assoc :device-flush? false))))

(defn- device-present?
  "From the last sync: does this run track hold parameter device d?"
  [mirror]
  (let [status (:parameter-devices mirror)]
    (fn [cue-id lane device] (boolean (some #{device} (get-in status [[cue-id lane] :present]))))))

(defn- advance-live-devices!
  "Step for parameter devices (#625) after each committed frame, on the owner
  or, with :background-observers, on the observer thread: decode slot values
  Live reported into commands, and collect Tolglow's accepted writes for Live.
  Only pure work happens here; requests to the companion leave on the
  runtime's one writer thread and commands go through submit!, so neither
  thread waits on Live."
  [runtime state model]
  (let [start (System/nanoTime)
        catalog (device-catalog model)
        lock (:player-lock @state)
        [commands writes decoded]
        (locking lock
          (let [;; At most device-events-per-cycle values are decoded per owner
                ;; step; the rest stay queued, in order, for the next one.
                [old _] (swap-vals! state update :device-inbox #(vec (drop device-events-per-cycle %)))
                events (vec (take device-events-per-cycle (:device-inbox old)))
                s (device-layouts @state model)
                ctx {:model model :record (:record s) :mirror (:mirror s) :player (:player s) :catalog catalog
                     :now-ms (now-ms runtime) :present? (device-present? (:mirror s))
                     :envelope-fn #(run-clip-envelope runtime model)}
                in (if (or (seq events) (live-devices/waiting? (:devices s)))
                     ;; Live's data never costs the cycle its Tolglow writes.
                     (try (live-devices/ingest (:devices s) ctx events)
                          (catch Exception error
                            {:state (-> (:devices s) (update-in [:counts :ingest-errors] (fnil inc 0))
                                        (assoc :last-error (.getMessage error)))
                             :commands []}))
                     {:state (:devices s) :commands []})
                out (live-devices/mirror-writes (:state in) ctx)]
            (swap! state assoc :devices (:state out) :device-layout-key (:device-layout-key s))
            [(:commands in) (:writes out) (count events)]))]
    (doseq [c commands]
      (submit! runtime (assoc c :expected-epoch (:epoch model))))
    (when (seq writes)
      (let [[old _] (swap-vals! state (fn [s]
                                        (-> (reduce (fn [s {:keys [track-id cue-id lane values]}]
                                                      (let [replaced (keep (fn [{:keys [slot]}] (when-let [q (get-in s [:device-outbox track-id :values slot :seq])] [slot [q]]))
                                                                           values)]
                                                        (-> s
                                                            ;; A queued value replaced before it was sent is never echoed.
                                                            (update :devices live-devices/forget-writes track-id (into {} replaced))
                                                            (update-in [:device-outbox track-id] merge {:cue-id cue-id :lane lane})
                                                            (update-in [:device-outbox track-id :values]
                                                                       (fnil into {}) (map (juxt :slot identity)) values))))
                                                    s writes)
                                            (assoc :device-flush? true))))]
        (when-not (:device-flush? old)
          (.execute ^java.util.concurrent.ExecutorService @(:device-writer old) #(drain-device-writes! runtime state)))))
    (let [ns (- (System/nanoTime) start)]
      (swap! state update :device-cost #(-> % (update :samples inc) (update :total-ns + ns) (update :max-ns max ns)
                                            (update :max-values (fnil max 0) decoded) (update :values (fnil + 0) decoded))))))

(defn live-parameter-device-value!
  "Queue one slot value a Tolglow parameters device reported through the
  companion (#625) for the owner, which decodes it at its next cycle
  (live-devices/ingest): automation playback steers the run its clip
  launched, a performer's turn in Live is a performance the run record keeps.
  The Live bridge calls this for `parameter-device` callbacks; the REPL can
  call it with the same event:
    {:type :parameter-device-value :track-id t :slot n :value v
     :automation-state 0|1|2 :watch-session w :serial k}
  At most 1024 events wait; more are counted and dropped."
  [runtime event]
  (let [state (:live-runs runtime)]
    (if-not (and state (:enabled? @state))
      {:status :ignored :reason :disabled}
      (let [event (update event :type #(if (string? %) (keyword %) %))
            [old _] (swap-vals! state (fn [s] (if (< (count (:device-inbox s)) device-inbox-limit)
                                                (update s :device-inbox conj event)
                                                (update s :device-inbox-dropped inc))))]
        (if (< (count (:device-inbox old)) device-inbox-limit)
          {:status :queued}
          {:status :dropped :reason :inbox-full})))))

(defn- observe-live-runs!
  "Fold the committed model into the run record. Pure fold; a failure is
  reported in status and never stops a frame."
  [runtime model]
  (when-let [state (:live-runs runtime)]
    (when (:enabled? @state)
      (let [start (System/nanoTime)]
        (try
          (let [record (live-runs/observe-model (:record @state) model)
                ns (- (System/nanoTime) start)]
            (swap! state (fn [s] (-> s (assoc :record record)
                                     (update :cost #(-> % (update :samples inc) (update :total-ns + ns)
                                                        (update :max-ns max ns) (assoc :last-ns ns))))))
            (advance-run-player! runtime state model)
            (advance-live-devices! runtime state model))
          (catch Exception error
            (observer-error! runtime :live-runs-error error)))))))

(defn- fold-observers! [runtime model]
  (observe-phrase-capture! runtime model)
  (observe-live-runs! runtime model))

(defn- settle-observers!
  "Fold every committed model the background observers have not folded yet,
  in commit order, on the calling thread. The owner calls this before each
  cycle, outside its lock, and readers call it before reading a buffer or
  record, so each sees exactly what inline folding would have produced; the
  worker has normally finished already.

  It refuses to run on a thread holding the owner lock (a frame or event
  observer, say): a fold submits commands, which takes that lock, so waiting
  there for a fold in progress could deadlock. Models are taken one at a
  time, and a fold that throws anything is reported in status and does not
  keep the models after it from being folded."
  [runtime]
  (when-let [{:keys [pending lock]} (:observer-worker runtime)]
    (when (Thread/holdsLock (:lock runtime))
      (throw (ex-info "Read the run record and phrase capture outside the show owner lock" {})))
    (locking lock
      (loop []
        (let [[queued _] (swap-vals! pending #(if (seq %) (pop %) %))]
          (when-let [model (peek queued)]
            (try (fold-observers! runtime model)
                 (catch Throwable error (observer-error! runtime :observer-error error)))
            (recur)))))))

(defn- publish-observer-errors!
  "Write failures the background observers kept into status. Only the owner
  (or the thread stopping the show) calls this."
  [runtime]
  (when-let [errors (get-in runtime [:observer-worker :errors])]
    (when-let [failed (not-empty (first (swap-vals! errors (constantly {}))))]
      (swap! (:state runtime) update :status merge failed))))

(defn- observe-committed!
  "After the frame is handed to output: fold the committed model on the
  owner, or queue it for the background observers."
  [runtime model]
  (if-let [{:keys [pending executor]} (:observer-worker runtime)]
    (do ;; Failures of earlier folds, where inline folding would have put them.
        (publish-observer-errors! runtime)
        (swap! pending conj model)
        (.execute ^java.util.concurrent.ExecutorService @executor #(settle-observers! runtime)))
    (fold-observers! runtime model)))

(defn live-run-clip!
  "Queue one run-clip playback event from the companion's run-track watch
  (live-runs/player) for the owner. The event's :beat is stamped here with the
  show transport beat at arrival, the one place Live song time meets the show
  clock; Live's :song-beat is kept only for diagnostics. Decoding and every
  command happen after the next committed frame (on the owner, or on the
  observer thread with :background-observers), so this never waits for
  rendering. Needs a previous sync-live-runs!, which binds clips to takes.
  The Live bridge calls this for `run-clip` callbacks; the REPL can call it
  too. At most 256 events wait; more are counted and dropped."
  [runtime event]
  (let [state (:live-runs runtime)]
    (if-not (and state (:enabled? @state) (:player @state))
      {:status :ignored :reason (if (and state (:enabled? @state)) :not-synced :disabled)}
      (let [model (:model @(:state runtime))
            {:keys [beat bpm playing? time-ms]} (:transport model)
            elapsed (if (and playing? time-ms) (/ (* (- (now-ms runtime) time-ms) (or bpm 120.0)) 60000.0) 0)
            event (-> event
                      (update :type #(if (string? %) (keyword %) %))
                      (assoc :beat (live-runs/snap (+ (or beat 0) (max 0 elapsed)))))
            [old _] (swap-vals! state (fn [s] (if (< (count (:inbox s [])) run-clip-inbox-limit)
                                                (update s :inbox (fnil conj []) event)
                                                (update s :inbox-dropped (fnil inc 0)))))]
        ;; A clip start hands its track's slots back to Live playback (#625).
        (when (= :clip-started (:type event))
          (swap! state (fn [s] (if (< (count (:device-inbox s)) device-inbox-limit)
                                 (update s :device-inbox conj {:type :clip-started :track-id (:track-id event)})
                                 (update s :device-inbox-dropped inc)))))
        (if (< (count (:inbox old [])) run-clip-inbox-limit)
          {:status :queued}
          {:status :dropped :reason :inbox-full})))))

(defn- accepted-since
  "IDs of the commands `after` accepted that `before` had not, in acceptance
  order. Accepted commands append to the history order, so within an epoch
  they are its tail after the last ID `before` held: reading them costs the
  new commands, not a pass over the bounded history."
  [before after]
  (let [order (get-in after [:control :history-order])]
    (if (= (:epoch before) (:epoch after))
      (let [known (get-in before [:control :history])]
        (reverse (take-while #(not (contains? known %)) (if (vector? order) (rseq order) (reverse order)))))
      order)))

(defn- observe-sync-commands! [runtime before after receipts events fired-outcomes]
  (let [manager (connections/sync (:connections runtime))
        epoch (:epoch after)
        fired (filter #(= epoch (get-in % [:ack :epoch])) fired-outcomes)
        fired-ids (into #{} (map #(get-in % [:ack :command-id])) fired)
        history (get-in after [:control :history])
        gesture-writes (into {} (keep (fn [[_ ack _ _ write]]
                                        (when write [(:command-id ack) write])))
                             receipts)
        from-history (keep #(when-not (fired-ids %) (get history %)) (accepted-since before after))
        outcomes (if (seq fired) (sort-by #(get-in % [:ack :revision]) (concat fired from-history)) from-history)]
    (doseq [[_ ack command] receipts :when (#{:rejected :pending :cancelled} (:status ack))]
      (acknowledge-sync! runtime manager command ack epoch))
    ;; Include automation, composition and controller commands, not only HTTP.
    (doseq [{:keys [command ack]} outcomes]
      (let [write (get gesture-writes (:command-id command))
            command (cond-> command write (assoc :op :param/set :value (:value write)))
            instance (get-in after [:instances (:instance-id ack)])
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

(def ^:private device-retirement-stop-ms
  "How long stop! waits for queued device retirements."
  1000)

(defn- detach-devices!
  "Owner lock only: take every connected session out of the device map and
  mark its ID :closing in the same critical section, so no connect can claim
  the ID before retire-devices! has closed the old session (#226)."
  [runtime]
  (let [detached (vals (first (swap-vals! (:devices runtime) (constantly {}))))]
    (when-let [state (:device-retirement-state runtime)]
      (swap! state update :closing into (map device-session-id detached)))
    detached))

(defn- retire-devices!
  "Close retired device sessions in order on the retirement worker, then run
  then! there. A display or MIDI write that blocks holds only that worker,
  never the render thread that committed the retirement (#226). then! follows
  every earlier retirement, so a restored device reopens after its old port
  and display have closed. Until its close returns, a session ID stays in
  :closing and connect-device! refuses it. The status :device-retirement
  counts sessions :pending, :retired, :failed (close threw, :last-error has
  :stage :close) and :reopen-failed (:stage :reopen). After stop! the closes
  run on a :late daemon thread instead."
  [runtime devices then!]
  (when (or (seq devices) then!)
    (let [state (or (:device-retirement-state runtime) (atom (new-device-retirement-state)))
          ids (mapv device-session-id devices)
          failed! (fn [state counter stage error]
                    (-> state (update counter (fnil inc 0))
                        (assoc :last-error {:stage stage
                                            :message (or (.getMessage ^Throwable error) (.getName (class error)))
                                            :class (.getName (class error))})))
          task (fn []
                 (doseq [[device id] (map vector devices ids)]
                   (let [error (try (devices/close! device) nil
                                    (catch Throwable error error))]
                     (swap! state (fn [s] (cond-> (-> s (update :pending dec) (update :closing disj id))
                                            error (failed! :failed :close error)
                                            (nil? error) (update :retired inc))))))
                 (when then!
                   (try (then!)
                        (catch Throwable error (swap! state failed! :reopen-failed :reopen error)))))]
      (swap! state #(-> % (update :pending + (count devices)) (update :closing into ids)))
      (if-let [worker (:device-retirement-worker runtime)]
        (try (.execute ^java.util.concurrent.ExecutorService @worker ^Runnable task)
             ;; stop! shut the worker down: close on a thread of its own, never the caller's.
             (catch java.util.concurrent.RejectedExecutionException _
               (swap! state update :late (fnil inc 0))
               (.start (doto (Thread. ^Runnable task "tolglow-device-retirement-late") (.setDaemon true)))))
        ;; A runtime created before a REPL reload has no retirement worker.
        (task)))))

(defn- finish-resource-work! [runtime retired candidates restored-generation restarted-generation]
  (when (and restarted-generation @(:running runtime))
    (try (connections/rebind-restart! (:connections runtime) restarted-generation
                                      (audio-callbacks runtime) (sync-callbacks runtime))
         (catch Exception error
           (when @(:running runtime)
             (swap! (:state runtime) assoc-in [:status :restart-input-error] (error-data error))))))
  ;; User/device callbacks can dispatch while they close. Never run this under
  ;; the model owner lock, and attempt every resource even after a failure.
  ;; Retired device sessions close on their own worker (retire-devices!).
  (doseq [cleanup (concat (map #(fn [] (connections/retire! %)) retired)
                          (map #(fn [] (connections/abort-prepared! %)) candidates))]
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

(defn- reopen-restoring-device! [runtime {:keys [session-id lease after-restore!]}]
  ;; Startup can acquire a display process and must stay off the show clock.
  (when (and after-restore! (identical? lease (get @(:device-reconnects runtime) session-id)))
    (.start (doto (Thread.
                   (fn []
                     (try (after-restore! lease)
                          (catch Exception error
                            (locking (:lock runtime)
                              (when (identical? lease (get @(:device-reconnects runtime) session-id))
                                (swap! (:device-reconnects runtime) dissoc session-id)
                                (when @(:running runtime)
                                  (swap! (:state runtime) assoc-in [:status :device-reconnect-error]
                                         (error-data error))))))))
                   "tolglow-restored-device")
              (.setDaemon true)))))

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

(defn- last-good-inputs
  "Sample modulation for the frame that rejects a pending hot reload, from the
  same prior controller state as the candidate samples. Return the engine's
  reload fallback and the state behind it, or nil when both outcomes read the
  same inputs. Modulation whose own controllers cannot advance for that
  outcome keeps the candidate's samples. Ramp overlays do not follow a
  candidate, so both outcomes rest on the same ones. `spatial-overrides` are
  the spatial cues' committed parameter fields; `spatial-differs?` says a
  candidate changes the spatial inputs, which always needs a fallback."
  [model candidate candidate-samples samples ramp-params spatial-overrides spatial-differs? time-ms]
  (let [result (try (modulation/advance (:modulation model) (modulation/last-good-view model)
                                        {:time-ms time-ms :transport (:transport model)})
                    (catch clojure.lang.ExceptionInfo _ nil))
        sampled #(select-keys % [:overrides :priorities :priority-overrides])
        modulation-differs? (and result
                                 (or (not= (:state result) (:state candidate)) (not= (:overrides result) (:overrides candidate))
                                     (not= (sampled samples) (sampled candidate-samples))))]
    (when (or modulation-differs? spatial-differs?)
      (let [[result samples] (if modulation-differs? [result samples] [candidate candidate-samples])
            overrides (merge-with merge ramp-params (:overrides result) (:overrides samples) spatial-overrides)]
        {:modulation result :parameter-samples samples :overrides overrides
         :frame (assoc modulation/last-good-frame
                       :keyframe-params overrides
                       ::engine/priority-overrides (:priority-overrides samples {}))}))))

(defn- cue-effect [model cue]
  (if-let [version (:effect-version cue)]
    (or (get-in model [:registry :versions [:effects (:effect-id cue) version]])
        (let [current (get-in model [:registry :effects (:effect-id cue)])]
          (when (= version (:version current)) current)))
    (get-in model [:registry :effects (:effect-id cue)])))

(defn- launch-effect [model command]
  (if-let [cue (get-in model [:registry :cues (:cue-id command)])]
    (cue-effect model cue)
    (get-in model [:registry :effects (:effect-id command)])))

(defn- launch-targets
  "The launch targets `command` resolves to, or ::unresolved."
  [model command]
  (try (get-in (targets/resolve-command model command) [:launch :targets])
       (catch Exception _ ::unresolved)))

(defn- command-program
  "The program a launch of `command` (a cue or effect launch) compiles, or nil."
  ([model command] (command-program model command (launch-targets model command)))
  ([model command targets]
   (let [effect (launch-effect model command)]
     (when (and effect (not= ::unresolved targets)) (programs/program effect {:targets targets})))))

(def ^:private scheduled-warm-limit
  "Scheduled launches added to the warm tier between rebuilds before the
  next frame rebuilds it, dropping what no longer applies (#774)."
  256)

(defn- warm-programs!
  "Compile ahead, off the frame, what a launch is likely to need (#774):
  every registered effect's untargeted program, each cue's fixture or group
  selection, each recorded live-run take's targets, and each quantized launch
  as soon as it is scheduled. The owner only compares identities and submits;
  the worker resolves selections from the immutable snapshot, reusing each
  cue's resolution while the cue, the rig and the scene groups it can name
  are unchanged. Volume selections follow moving volumes and are left to the
  launch."
  [runtime context model]
  (when (:submit context)
    (let [registry (:registry model)
          takes (some-> (:live-runs runtime) deref :record :takes)
          ;; What selector resolution reads, besides the cue (spatial.targets).
          basis [(:rig model) (get-in model [:extensions :tolglow.lighting-scenes/control :groups])]
          scheduled (cue-timing/scheduled-commands model)
          scheduled-key #(or (:command-id %) %)
          seen @(:scheduled-seen context)
          fresh (remove #(contains? seen (scheduled-key %)) scheduled)]
      (when (> (swap! (:scheduled-added context) + (count fresh)) scheduled-warm-limit)
        (reset! (:warm-key context) ::rebuild))
      (when (programs/warm! context [registry basis takes]
                            (fn []
                              (swap! (:resolutions context) select-keys (keys (:cues registry)))
                              (concat
                               ;; In a stable order, so the worker's first-in-first-out queue is reproducible.
                               (map #(programs/program (val %) {}) (sort-by (comp str key) (:effects registry)))
                               (keep (fn [[id cue]]
                                       (when (and (:selector cue) (not (:volume-id (:selector cue))))
                                         (let [command {:op :cue/launch :cue-id id}]
                                           ;; The program follows the effect's current revision.
                                           (command-program model command
                                                           (programs/resolved! context id cue basis #(launch-targets model command))))))
                                     (sort-by (comp str key) (:cues registry)))
                               (keep (fn [take] (command-program model {:op :cue/launch :cue-id (:cue-id take)
                                                                       :launch (select-keys (:launch take) [:targets])}))
                                     (vals takes))
                               (keep #(command-program model %) scheduled))))
        (reset! (:scheduled-added context) 0))
      (reset! (:scheduled-seen context) (into #{} (map scheduled-key) scheduled))
      (when (seq fresh)
        (programs/warm-more! context (fn [] (keep #(command-program model %) fresh)))))))

(defn- spatial-levels
  "Split the frame's group levels into those the spatial native-member mix
  applies to `groups` and those the engine applies itself."
  [group-levels groups]
  (let [spatial (when (and group-levels groups) (not-empty (select-keys group-levels (keys groups))))]
    {:spatial spatial :engine (if spatial (not-empty (apply dissoc group-levels (keys spatial))) group-levels)}))

(defn- render-frame
  "Render one lighting frame from a model whose inputs this cycle already owns.
  The live owner and a private Room audition call this same path. Callers
  supply `before`, the previous frame state of that same timeline. With
  :audition? the frame uses the audition's private caches and compiler,
  renders on the CPU, samples serially, and never plans fog, polls display
  leases or keeps cadence samples. An audition has no schedule of its own: it
  takes the live show's ramp overlay for this frame from its caller.
  Nothing here sends output, invokes callbacks or performs device IO."
  [runtime before model time-ms {:keys [started frame-start-ns photo-session sampling-session audition? live-ramps]}]
  (let [;; Ramp values are a frame overlay; committed values move only when a ramp commits.
        ramp-frame (if audition? live-ramps (subscribers/frame-overlays model time-ms))
        frame-master (get ramp-frame :master (:master model))
        ;; A pending revision must accept a ramped parameter's committed and
        ;; frame values alike; modulation reads which revision to follow here.
        ;; Programs are compiled ahead of launches and of pending revisions,
        ;; off the frame. Until a revision's programs are ready the engine and
        ;; modulation read its instances on their captured definition (#774).
        _ (prepare-compiler! runtime (get-in model [:rig :fixtures]))
        programs (let [context @(:compiler-context runtime)
                       frame-number (inc (get model :frame-number 0))
                       plan (programs/plan model (modulation/with-frame-params model (:params ramp-frame) (:param-targets ramp-frame))
                                           (:reload-waits before) frame-number)]
                   (warm-programs! runtime context model)
                   (programs/request! context (:programs plan))
                   (assoc (programs/hold context plan) :frame frame-number))
        attempting #(programs/frame-view % programs)
        reading #(modulation/with-frame-params (attempting %) (:params ramp-frame) (:param-targets ramp-frame))
        parameter-samples (parameter-sources/sample (reading model) {:time-ms time-ms :transport (:transport model)}
                                                   (:parameter-samples before))
        pending-reloads (let [view (reading model)]
                          (into #{} (keep #(when (modulation/reload-candidate view %) (:id %))) (vals (:instances view))))
        ;; This frame's engine pass may reject a pending hot reload. Sample the
        ;; waveforms for that outcome too, from the same prior state.
        last-good-samples (when (seq pending-reloads)
                            (parameter-sources/sample (modulation/last-good-view model)
                                                      {:time-ms time-ms :transport (:transport model)}
                                                      (:parameter-samples before)))
        control-view (fn [samples]
                       (if (seq (:overrides samples))
                         (update (:modulation-view before) :overrides #(merge-with merge % (:overrides samples)))
                         (:modulation-view before)))
        ;; Spatial, autopilot and HDR read sampled parameters before the
        ;; engine decides a pending reload, and the first two commit what
        ;; they decide. They read the revisions that run (#770).
        candidate-view (control-view parameter-samples)
        control-view (if last-good-samples (control-view last-good-samples) candidate-view)
        patch (patch! runtime (get-in model [:rig :fixtures]))
        space (when (or (get-in model [:settings :spatial :enabled?]) (get-in model spatial/state-path) (spatial-cues/needed? model))
                ;; Spatial cues read opacity and parameters from the model: give them the frame overlay.
                ;; The audition samples spatial fields serially, off the live worker pool.
                (spatial/advance (subscribers/with-frame-overlays model ramp-frame)
                                 (assoc before :modulation-view control-view) time-ms (reactions runtime) command
                                 (when-not audition? (:spatial-sampling runtime))
                                 (when last-good-samples
                                   {:candidates (spatial-cues/reload-candidates model pending-reloads)
                                    :candidate-view candidate-view})))
        ;; The committed fields serve a frame that keeps the running
        ;; revisions; candidate fields, when a pending reload changes them,
        ;; serve the frame that accepts it. The engine's decision picks one.
        committed-fields (:fields space)
        candidate-fields (:candidate-fields space)
        spatial-fields (or candidate-fields committed-fields)
        model (if (:model space) (subscribers/without-frame-overlays (:model space) model ramp-frame) model)
        conductor? (autopilot-config/enabled? model)
        calibrating? (or photo-session (calibration/current model) (get-in model [:extensions :tolglow.rig.fog-calibration/session]))
        bypass? (or calibrating? (false? (get-in model [:transport :playing?])) (pause-look/blackout? model))
        hard-off? (or (pause-look/blackout? model) (zero? frame-master))
        suspended? (or bypass? hard-off?)
        meter-plan (when (or conductor? (lighting/needs-meter? (:settings model))) (capacity-plan! runtime patch))
        ;; An audition has no compute owner: it always renders on the CPU.
        resolve-frame (compute/frame-renderer (when-not audition? (:compute runtime)) frame-start-ns)
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
        ;; HDR values apply after the engine, so they can follow its reload
        ;; decision as the spatial masks do. Autopilot decisions above cannot:
        ;; they commit before the engine runs, from the running revisions.
        candidate-hdr (when (and hdr? last-good-samples (not= candidate-view control-view))
                        (or (autopilot/render-options (assoc model :time-ms time-ms)
                                                      (some-> prepared (assoc-in [:context :modulation-view] candidate-view))
                                                      suspended? candidate-view hdr-scope) {}))
        hdr-for (fn [result] (if (and candidate-hdr (not= :fallback (:reload-inputs result))) candidate-hdr hdr-opts))
        modulation-result (modulation/advance (:modulation model) (reading model)
                                               {:time-ms time-ms :transport (:transport model)})
        last-good (when last-good-samples
                    (last-good-inputs model modulation-result parameter-samples last-good-samples
                                      (:params ramp-frame) (:overrides committed-fields) (some? candidate-fields) time-ms))
        model (assoc model :modulation (:state modulation-result))
        ;; Active modulation and waveform bindings take precedence over a ramp overlay.
        ;; A ramp does not follow a pending reload: its candidate reads only
        ;; the ramped parameters its schemas still accept.
        input-overrides (merge-with merge (modulation/candidate-overrides (reading model) (:params ramp-frame))
                                    (:overrides modulation-result) (:overrides parameter-samples)
                                    (:overrides spatial-fields))
        modulation-middleware (if last-good
                                (modulation/reload-middleware input-overrides (:overrides last-good))
                                (modulation/middleware input-overrides))
        _ (prepare-compiler! runtime (get-in model [:rig :fixtures]))
        variable-plan (variable-blocks/prepare! (:variable-plan runtime) (get-in model [:settings :variable-blocks]) (prefabs/catalog model))
        variables (variable-blocks/sample variable-plan model time-ms (:variable-observation before))
        model (variable-blocks/collect-events model (:variable-observation before) variables)
        variable-inputs (when variables (:variables (variable-blocks/resolved model variables)))
        sampling (when sampling-session (vreset! sampling-session (cadence/begin runtime before model)))
        ;; Display response leases poll external predicates; only the live owner reads them.
        response-frame (when (and (not audition?)
                                  (some #(not= false (:enabled? %)) (:responses model))
                                  (response-samples-demanded? runtime))
                         {:show-id (:show-id model) :epoch (:epoch model) :time-ms time-ms
                          :transport-generation @(:presentation-clock runtime)
                          :frame-number (inc (get model :frame-number 0))
                          :responses (:responses model)
                          :adaptive-intervals (get-in sampling [:inputs :keyframe-intervals])})
        ;; Video frames latch once per lighting frame and hold while stopped (#672).
        video-latch (video-hub/latch (:video runtime) model (:video-latch before)
                                     (shader-producer/frames (:shader-video runtime)))
        frame-input (merge {:time-ms time-ms :epoch (:epoch model) :show-id (:show-id model)
                            video-output/input-key video-latch
                            :keyframes (:keyframes before) :keyframe-params input-overrides
                            :keyframe-generation (:responses model) :spatial-aims (:aims spatial-fields)
                            :hdr? (boolean (or hdr? (:observe-hdr? space))) :fixtures (get-in model [:rig :fixtures])}
                           (connections/effect-frame-inputs (:connections runtime) (:epoch model))
                           (:inputs sampling))
        group-levels (group-channels/frame-levels model)
        ;; Spatial membership is attached after the engine runs. Keep
        ;; those contributions intact until their native member mix.
        levels (spatial-levels group-levels (:groups spatial-fields))
        committed-levels (if candidate-fields (spatial-levels group-levels (:groups committed-fields)) levels)
        engine-levels (:engine levels)
        ;; The spatial masks and their group levels for the inputs the engine read.
        masks (fn [result]
                (if (= :fallback (:reload-inputs result))
                  [(:groups committed-fields) (:spatial committed-levels)]
                  [(:groups spatial-fields) (:spatial levels)]))
        frame-input (cond-> frame-input
                      variables (assoc :resolved-variables variable-inputs)
                      (seq (:variables ramp-frame))
                      (update :resolved-variables #(merge (or % (:variables model)) (:variables ramp-frame)))
                      ;; Modulated layer priority is resolved once here and
                      ;; orders only this frame; the model keeps its own tiers.
                      (seq (:priority-overrides parameter-samples))
                      (assoc ::engine/priority-overrides (:priority-overrides parameter-samples))
                      response-frame (assoc ::waveforms/response-frame response-frame)
                      engine-levels (assoc ::engine/opacity-factors engine-levels)
                      ;; The engine reads these instead, for the whole frame,
                      ;; if it rejects any pending reload.
                      last-good (assoc ::engine/reload-fallback
                                       (cond-> (:frame last-good)
                                         candidate-fields (assoc :spatial-aims (:aims committed-fields)
                                                                 ::engine/opacity-factors (or (:engine committed-levels) {})))))
        response (response-middleware! runtime (:responses model []))
        inputs (if response-frame
                 (fn [context]
                   (let [dry (modulation-middleware context)]
                     (waveforms/capture-response dry (response dry))))
                 (comp response modulation-middleware))
        used-bindings (java.util.concurrent.ConcurrentHashMap/newKeySet)
        compile-step (fn [definition]
                       (let [compiled ((:compile runtime) definition)
                             _ (when-let [bindings (::pure-bindings (meta compiled))]
                                 (.addAll used-bindings bindings))
                             step (spatial-cues/with-aims definition compiled)]
                         ;; Certified literals have no parameter schemas, and
                         ;; these transforms touch only declared parameters.
                         ;; :migrate reloads call this pure transform once, at takeover.
                         ;; Its compiled form is cached per declaration and version.
                         (state-migration/attach (:state-migrations runtime) definition
                                                 (keyframes/with-inputs step inputs {:preserve-time-invariance? true}))))
        ;; The engine prepares a revision from the model it receives. Where the
        ;; revision refuses a ramped parameter's committed value or its target,
        ;; it receives that value, so it turns the revision down instead of
        ;; adopting one that cannot hold what the model keeps or the ramp
        ;; commits. The ramp still reaches evaluation through the overrides.
        engine-model (programs/frame-view model programs)
        engine-ramps (reduce (fn [frame id]
                               (if-let [values (modulation/ramp-preparation engine-model (get-in engine-model [:instances id])
                                                                            (get-in ramp-frame [:params id])
                                                                            (get-in ramp-frame [:param-targets id]))]
                                 (if (seq values) (assoc-in frame [:params id] values) (update frame :params dissoc id))
                                 frame))
                             ramp-frame (keys (:param-targets ramp-frame)))
        result (update (engine/advance (subscribers/with-frame-overlays engine-model engine-ramps) frame-input compile-step
                                       ;; An audition evaluates serially, off the live worker pool.
                                       (if audition? mapv (executor/mapper (:executor runtime))))
                       :model subscribers/without-frame-overlays model engine-ramps)
        _ (release-pure-bindings! runtime (set used-bindings))
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
        observation (if (and (not audition?) (get-in model [:settings :fog-control :enabled?]))
                      (fog-control/with-forecast (:plan @(:venue-fog runtime)) observation) observation)
        previous-control (get-in before [:output :fog-control])
        fog-bypass? (or bypass? (zero? frame-master) (get-in model [:extensions :tolglow.rig.fog-calibration/session]))
        limit (when plan
                (capacity/control (merge capacity/defaults meter-opts)
                  previous
                  frame-master time-ms (when (= (:epoch model) (:epoch audio)) audio)
                  bypass?))
        fog (when (and plan (not audition?) (or (:fog? meter-opts) (:fog previous)))
              (capacity/fog-policy meter-opts plan (:fog previous) time-ms limit fog-bypass?
                                   (:reference-rms meter-opts (:reference-rms capacity/defaults))))
        ;; Fog arbitration checks whether a manual source is visible.
        ;; Spatial member levels apply later, so omit muted sources here.
        fog-levels (second (masks result))
        fog-blocks (cond->> (:contributions result)
                     (some #(if (map? %) (some zero? (vals %)) (zero? %)) (vals fog-levels))
                     (filterv (fn [{{:keys [instance-id effect-version]} :source}]
                                (pos? (engine/opacity-factor fog-levels instance-id effect-version)))))
        ;; An audition never plans fog: its private frame has no fog leases.
        setpoint (when-not audition?
                   (fog-control/frame model patch observation previous-control
                     (if (get-in runtime [:sink :send!]) ((get-in runtime [:publisher :now-ms])) time-ms)
                     fog-bypass? fog-blocks (:block fog)))
        extra-blocks (cond-> (fog-control/with-demands [] (:feedback setpoint))
                       manual-aim (conj manual-aim)
                       (:music setpoint) (conj (:music setpoint))
                       (:block photo-session) (conj (:block photo-session))
                       pause-layer (conj pause-layer))
        render-options (cond-> {:master (get limit :effective-master frame-master)
                                :blackout? (pause-look/blackout? model)
                                :include-values? (not (:batch? runtime)) :compact-values? (:batch? runtime)
                                :edge-tracking (when (seq (get-in model [:rig :edge-tracking]))
                                                 (:links (reflections/plan-for (:optics-plan runtime) (:rig model))))
                                :previous-values (or (get-in before [:output :values]) {})
                                :previous-compact (get-in before [:output :compact])}
                         hdr? (assoc :hdr hdr-opts)
                         (:observe-hdr? space) (assoc :observe-hdr? true))
        followers (into #{} (keep :follower) (get-in model [:rig :edge-tracking]))
        resolved (effect-faults/resolve-output result
                   ;; A head no effect positions this frame holds its last committed pose.
                   (fn [contributions prepared]
                     (resolve-frame patch (position-hold/with-hold patch (:output before)
                                            (lighting/contributions standby contributions extra-blocks) followers)
                                    (cond-> render-options candidate-hdr (assoc :hdr (hdr-for prepared)))))
                   (fn [result]
                     (let [[groups levels] (masks result)]
                       (if (seq groups) (spatial-cues/mask-contributions result groups levels) result))))
        ;; A renderer rejection rebuilds the result from the engine's frame
        ;; model, which carries the ramp overlay: take it out of that one too.
        ;; Both results come from the engine's view, so deferred and hidden
        ;; instances are restored here, once, for either path.
        result (update (:result resolved) :model (comp #(programs/restore % model programs) subscribers/without-frame-overlays)
                       model engine-ramps)
        [adopted-intents adopted-emissions] (programs/mark-adoptions (:intents result) (:intent-emissions result) (:waits programs)
                                                                     #(get-in model [:instances % :effect-id]) (:frame programs))
        result (cond-> result
                 (seq adopted-intents) (assoc :intents adopted-intents)
                 (seq adopted-emissions) (assoc :intent-emissions adopted-emissions))
        ;; Commit the controller and waveform state behind the inputs the
        ;; engine read, so a rejected reload leaves no trace in either.
        last-good? (= :fallback (:reload-inputs result))
        modulation-result (if last-good? (:modulation last-good) modulation-result)
        parameter-samples (if last-good? (:parameter-samples last-good) parameter-samples)
        input-overrides (if last-good? (:overrides last-good) input-overrides)
        spatial-fields (if last-good? committed-fields spatial-fields)
        result (cond-> result
                 last-good? (assoc-in [:model :modulation] (:state modulation-result))
                 ;; The scene view shows the field the fixtures rendered.
                 (and (not last-good?) (:candidate-view space))
                 (update :model spatial/accept-candidate space))
        finished (when (or (get-in model [:settings :output-response :enabled?])
                           (get-in model [:settings :lighting-budget :enabled?])
                           (get-in model [:settings :lighting-activity :enabled?]))
                   (lighting/finish-frame before (:model result) patch meter-plan budget (:output resolved)
                     (lighting/contributions standby (:contributions result) extra-blocks) time-ms
                     calibrating? hard-off?
                     (get limit :effective-master frame-master)))
        delivered (or (:frame finished) (:output resolved))
        frame (cond-> delivered
                setpoint (assoc :fog-control (:status setpoint) :fog-demands (:leases setpoint))
                plan (assoc :lighting-capacity
                            (merge limit (capacity/measure plan (:universes delivered))
                                   (cond-> {:writers (if (or (pause-look/blackout? model) (zero? frame-master)) []
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
     :output frame :variable-observation variables :video-latch video-latch
     ;; The light gain the fixtures rendered with; projector outputs follow it (#673).
     :projector-gain (if (:blackout? render-options) 0.0 (double (:master render-options)))
     ::waveforms/response-frame response-frame
     ::show-events/intent-emissions (:intent-emissions result)
     :preview-hdr (when (:hdr? frame-input) (or (hdr-for result) {}))
     :keyframes (:keyframes result) :spatial-cache (:cache space) :hdr-scope hdr-scope :autopilot-cache (:cache observation) :autopilot-author-request (:author-request prepared)
     :lighting-history (:state finished) :fault-events (:fault-events result)
     :parameter-samples (assoc parameter-samples :spatial-overrides (:overrides spatial-fields)
                               :live-routing (when-not audition? (live-sync/parameter-routing (connections/sync (:connections runtime)))))
     :resolved-param-overrides input-overrides
     :reload-waits (:waits programs)
     ;; The definitions this frame sampled; binds validate against the same view.
     :program-hold (select-keys programs [:deferred :downgraded])
     :modulation-view (select-keys modulation-result [:overrides :diagnostics])
     :status (let [failures (programs/repeated-failures @(:compiler-context runtime))]
               (cond-> {:frame-ms (/ (- (System/nanoTime) started) 1.0e6) :last-intents (:intents result)}
                 ;; A program that keeps failing transiently is still retried; this says so.
                 (seq failures) (assoc :compile-failures failures)))})))

(defn- fire-scene-draft
  "Apply a quantized draft once the transport reaches its boundary. The owner
  calls this where grid cue launches fire, after this cycle's queued input.
  Steps commit at the boundary's logical time and beat when the cycle allows
  it, otherwise at the earliest consistent time and beat after it; the
  sampled transport is then restored. A clock change or a backward beat jump
  misses the boundary. Edits go to the cycle's draft view."
  [runtime state model time-ms]
  (let [{:keys [draft pending-apply]} @state
        {:keys [command due-beat clock grid last-beat]} pending-apply
        {:keys [beat playing?] :as transport} (:transport model)
        outcome (fn [status details]
                  (merge {:status status :command-id (:command-id command) :draft-id (:id draft)
                          :draft-revision (:revision draft) :grid grid :due-beat due-beat} details))
        missed (fn [code]
                 (swap! state #(-> % (dissoc :pending-apply) (assoc :last-outcome (outcome :missed {:code code}))))
                 model)]
    (cond
      (nil? pending-apply) model

      (not= clock (cue-clock-identity runtime model)) (missed :clock-changed)

      (and (number? last-beat) (< beat last-beat)) (missed :clock-moved-back)

      (or (not playing?) (< beat due-beat))
      (do (swap! state assoc-in [:pending-apply :last-beat] beat) model)

      :else
      (let [;; Time and beat stay on one transport line, even after a long stall.
            {at :time-ms at-beat :beat} (scene-draft/boundary-commit transport time-ms due-beat (:time-ms model))
            {:keys [ack] applied :model}
            (scene-draft/apply-draft (update model :transport assoc :beat at-beat :time-ms at)
                                     draft (assoc command :time-ms at))]
        (if (= :accepted (:status ack))
          (do (reset! state {:last-outcome (outcome :applied {:revision (:revision ack) :time-ms at :beat at-beat})})
              (assoc applied :transport transport))
          (do (swap! state #(-> % (dissoc :pending-apply)
                                (assoc :last-outcome (outcome :missed (select-keys ack [:code :message])))))
              model))))))

(def ^:private owner-draft-keys
  "Draft fields the owner writes. A candidate retune (#667) replaces :draft and
  :retune on the same timeline; the worker writes the frame fields and :retuned."
  [:pending-apply :last-outcome :draft :retune])

(defn- commit-draft-view!
  "Publish the cycle's draft edits after its frame has committed. Render
  fields written by the audition worker for the same timeline are kept."
  [runtime initial view]
  (when-not (identical? initial view)
    (swap! (:scene-draft runtime)
           (fn [current]
             (cond
               (nil? view) nil
               (and current (some? (:timeline view)) (identical? (:timeline view) (:timeline current)))
               (merge (apply dissoc current owner-draft-keys) (select-keys view owner-draft-keys))
               :else view)))))

(defn- audition-projection [{:keys [draft frame pending-apply last-outcome error stale skipped rendered frame-revision]}]
  (cond-> {}
    draft (assoc :draft (scene-draft/summary draft) :time-ms (get-in frame [:model :time-ms])
                 :status (cond error :error stale :stale pending-apply :scheduled :else :auditioning)
                 :frames {:rendered (or rendered 0) :skipped (or skipped 0)})
    (and draft (not error) (not stale) (pos? (or rendered 0)) (:output frame))
    (assoc :output (select-keys (:output frame) [:values :compact :target-order])
           ;; Compare with the draft revision to tell whether the frame shows the current value.
           :frame-revision frame-revision)
    pending-apply (assoc :pending-apply (assoc (select-keys pending-apply [:due-beat :grid :requested-at])
                                               :command-id (get-in pending-apply [:command :command-id])))
    last-outcome (assoc :last-outcome last-outcome)
    error (assoc :error error)
    stale (assoc :stale stale)))

(defn- audition-ramps
  "The live show's master ramp overlay for this frame, or nil. Master follows
  the live show in an audition, so its ramped frame value does too. Parameter,
  opacity and variable ramps edit state the draft's snapshot does not follow,
  and stay out of the Room like any live edit after staging. Sampling reads the
  committed live model: no ramp is advanced, committed, held or cancelled."
  [live time-ms]
  (let [master? #(= :master (get-in % [:destination :target]))]
    (when (some master? (get-in live [:event-subscribers :ramps]))
      (subscribers/frame-overlays (update-in live [:event-subscribers :ramps] #(filterv master? %)) time-ms))))

(defn- render-audition-frame
  "Render one private frame. A separate var so tests can slow the audition."
  [runtime frame model time-ms live-ramps]
  (let [started (System/nanoTime)]
    (render-frame runtime frame model time-ms
                  {:audition? true :started started :frame-start-ns started :live-ramps live-ramps})))

(defn- taken-rebases
  "Drop the first n pending clock rebases; the owner may have queued more."
  [state n]
  (let [pending (subvec (:rebases state []) n)]
    (if (seq pending) (assoc state :rebases pending) (dissoc state :rebases))))

(defn- render-audition!
  "Worker task: render one audition frame from the staged timeline. The
  result installs only while the same timeline is staged. Pending clock rebases
  and stopped-transport holds are applied first, in the order the live show
  took them, then any candidate retune queued since the last frame. `live` is the
  committed model of the frame that queued this one: its transport, master,
  blackout, master ramp and track feel reach the private timeline."
  [runtime timeline live time-ms]
  (let [state (:scene-draft runtime)
        same? #(identical? timeline (:timeline %))]
    (try
      (when-let [{:keys [draft frame caches next-model rebases retune retuned]} (let [current @state] (when (same? current) current))]
        (let [retune? (and retune (not= (:seq retune) retuned))
              ;; Shift the private musical anchors by each offset the live model took.
              model (-> (reduce (fn [model step]
                                  (if (contains? step :held-ms)
                                    (show-clock/hold model (:held-ms step))
                                    (show-clock/rebase model step)))
                                (or next-model (:model frame)) rebases)
                        (cond-> retune? (knob-candidates/audition-model (:candidate retune) (:value retune)))
                        (merge (select-keys live [:transport :master :blackout?]))
                        ;; A draft cannot stage a feel and applying replays it on the live
                        ;; model, so the Room shows the live track feel.
                        (personalities/adopt-state live)
                        (composition/advance time-ms))
              rendered (render-audition-frame (merge runtime caches) frame model time-ms
                                              (audition-ramps live time-ms))]
          (swap! state #(if (same? %)
                        (-> % (dissoc :next-model)
                            (taken-rebases (count rebases))
                            (cond-> retune? (assoc :retuned (:seq retune)))
                            ;; The draft revision this frame rendered: a retune bumps the
                            ;; revision before the worker renders the new value.
                            (assoc :frame-revision (:revision draft))
                            (update :rendered (fnil inc 0))
                            (assoc :frame (assoc (select-keys rendered audition-state-keys)
                                                 :model (staging/finish (:model rendered))
                                                 :output (:output rendered) :status (:status rendered))))
                        %))))
      (catch Exception failure
        (swap! state #(if (same? %) (assoc % :error (error-data failure)) %)))
      (finally
        (swap! state #(if (same? %) (dissoc % :in-flight) %))))))

(defn- schedule-audition!
  "After the live frame is published and its owner time measured, queue one
  audition frame on the audition worker. It runs outside the owner lock, so
  it cannot delay a live slot. A frame is skipped, and counted, while the
  previous one is still rendering or when the live frame used more than
  :max-live-share (default 0.5) of its period. `steps` are the clock steps the
  live model took this cycle: a beat rebase {:timeline :from-beat :beat} and a
  stopped-transport hold {:timeline :held-ms}. A timeline staged before the cycle
  queues them, also when this frame is skipped."
  [runtime live time-ms elapsed-ms steps]
  (let [state (:scene-draft runtime)
        _ (doseq [step steps :when (:timeline step)]
            (swap! state #(if (identical? (:timeline step) (:timeline %))
                            (update % :rebases (fnil conj []) (dissoc step :timeline))
                            %)))
        {:keys [draft timeline error stale in-flight]} @state
        share (get-in runtime [:audition-options :max-live-share] 0.5)
        period (/ 1000.0 (:frame-rate runtime))
        changed (when (and draft (not error) (not stale)) (scene-draft/changed-dependency live draft))]
    (cond
      (nil? draft) nil

      (not= (:epoch live) (get-in draft [:base :epoch])) (reset! state nil)

      (or error stale) nil

      ;; Applying would be refused with this code. Stage again to audition the changed show.
      changed (swap! state assoc :stale changed)

      (or in-flight (> elapsed-ms (* share period))) (swap! state update :skipped (fnil inc 0))

      :else
      (do (swap! state assoc :in-flight true)
          (try (.execute ^java.util.concurrent.ExecutorService @(:audition-worker runtime)
                         #(render-audition! runtime timeline live time-ms))
               (catch java.util.concurrent.RejectedExecutionException _
                 (swap! state dissoc :in-flight)))))))

(defn scene-draft
  "Read the staged Room draft, its audition status and frame counts, any
  scheduled apply and the last apply outcome. Returns nil without a draft."
  [runtime]
  (not-empty (dissoc (audition-projection @(:scene-draft runtime)) :output)))

(defn await-audition
  "Wait up to timeout-ms for an in-flight audition frame. For tests and tools."
  [runtime timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (cond (not (:in-flight @(:scene-draft runtime))) true
            (> (System/currentTimeMillis) deadline) false
            :else (do (Thread/sleep 1) (recur))))))

(def ^:private independent-commands
  "Commands that neither read nor write the registry or pages, so they never
  wait behind a held publication (#784): blackout, master, transport,
  parameters and ramps, gestures and edit history, instance controls, group
  and selection controls, knobs, audio LFOs and control-surface links. Any
  command not listed here waits, so a new command is safe until classified."
  #{:blackout/set :master/set :transport/set
    :param/set :param/nudge :param/ramp :variable/set
    :gesture/begin :gesture/sample :gesture/end :gesture/cancel
    :history/undo :history/redo :history/select
    :instance/end :instance/remove :instance/set-priority :instance/move-after :instances/shuffle
    :instance/pause :instance/resume :instance/opacity-set :instance/gain-set :instance/opacity-toggle
    :instance/envelope-set
    :group/apply :effect-selection/configure :effect-selection/toggle
    :effect-groups/configure :effect-groups/overlay-set :effect-groups/level-set :effect-groups/assign-instance
    :knob/create :knob/link :knob/nudge :knob/remove :knob/rename :knob/set :knob/unlink
    :knob-preset/capture :knob-preset/clear :knob-preset/recall
    :audio-lfo/configure :audio-lfo/remove
    :control-surface/attach :control-surface/bank :control-surface/detach :control-surface/layout
    :control-surface/link :control-surface/remove :control-surface/unlink})

(def ^:private launch-commands
  "Commands that wait only when they launch a held effect or cue, or a cue
  the show does not know yet."
  #{:cue/launch :effect/launch})

(def ^:private independent-message-types
  "Message types that neither read nor write the registry or pages: device
  and sensor input, parameter sources and surface aim. Every other type
  waits behind a held publication unless `waits-behind?` says otherwise."
  #{:rdm-update :sensor-audience-zone :sensor-configure :sensor-arm :sensor-calibrate
    :modulation-bind :modulation-unbind :modulation-hold :modulation-resume :surface-aim})

(defn- message-class
  "How a message of `type` (and command `op`) relates to a held publication:
  :independent never waits; :launch waits when it launches what is held;
  :cancel waits when a held message is a launch it could cancel; :dependent
  always waits. Unknown messages are :dependent."
  [type op]
  (cond
    (= :command type) (cond
                        (contains? independent-commands op) :independent
                        (contains? launch-commands op) :launch
                        (= :cue/cancel-launch op) :cancel
                        :else :dependent)
    (contains? independent-message-types type) :independent
    (= :cue-schedule type) :launch
    (= :cue-cancel type) :cancel
    :else :dependent))

(defn- waits-behind?
  "True when `message` must wait behind the `held` messages, a publication
  waiting for its check and what waits with it (#784). Safe by default: only
  messages `message-class` lists as independent go ahead (blackout, master,
  transport, parameters, gestures, instance and controller input), and
  launches of cues and effects nothing held touches. Everything else, any
  message that may read or write the registry included, waits in order."
  [model held {:keys [type payload]}]
  (let [held-ids (fn [kind]
                   (into #{} (keep (fn [{held-type :type held-payload :payload}]
                                     (case held-type
                                       :definition (when (= kind (:kind held-payload)) (get-in held-payload [:definition :id]))
                                       :command (when (= :cues kind)
                                                  (or (get-in held-payload [:cue :id]) (:cue-id held-payload)))
                                       nil)))
                         held))
        launch-waits? (fn [{:keys [cue-id effect-id]}]
                        (let [cue (when (some? cue-id) (get-in model [:registry :cues cue-id]))
                              effect-id (or effect-id (:effect-id cue))]
                          (or (and (some? cue-id) (nil? cue))
                              (nil? effect-id)
                              (contains? (held-ids :cues) cue-id)
                              (contains? (held-ids :effects) effect-id))))
        held-launch? (fn [{held-type :type held-payload :payload}]
                       (or (= :cue-schedule held-type)
                           (and (= :command held-type) (contains? launch-commands (:op held-payload)))))]
    (case (message-class type (:op payload))
      :independent false
      :launch (launch-waits? (if (= :cue-schedule type) (:command payload) payload))
      :cancel (boolean (some held-launch? held))
      :dependent true)))

(def ^:private ingress-order
  (reify java.util.Comparator
    (compare [_ left right]
      (Long/compare (:ingress-order left) (:ingress-order right)))))

(declare ^:private owner-cycle!)

(defn tick!
  "Advance one owner cycle; restore resources/model together, then retire IO
  outside the owner lock. Reactive actions from the previous publication enter
  this cycle with fresh command identity and the captured source epoch."
  ([runtime] (tick! runtime (now-ms runtime)))
  ([runtime time-ms] (tick! runtime time-ms nil))
  ([runtime time-ms output-slot]
   (if-let [{:keys [tick-lock]} (:observer-worker runtime)]
     ;; The previous cycle's observers finish before this cycle reads its
     ;; inputs, so commands they submit land as they would inline. The tick
     ;; lock keeps a second caller from starting a cycle between another
     ;; caller's settle and commit; it is never held by a fold.
     (locking tick-lock
       (settle-observers! runtime)
       (owner-cycle! runtime time-ms output-slot))
     (owner-cycle! runtime time-ms output-slot))))

(defn- owner-cycle!
  "The body of one owner cycle (see tick!)."
  [runtime time-ms output-slot]
   (let [frame-start-ns (System/nanoTime)
         retired (volatile! []) candidates (volatile! []) retired-devices (volatile! [])
         restored-generation (volatile! nil) restarted-generation (volatile! nil)
         restoring-device (volatile! nil)
         committed-fault-events (volatile! [])
         author-request (volatile! nil)
         committed-bound? (volatile! false)]
     (try
       (locking (:lock runtime)
         ;; Launch checks read the last committed frame only on this owner
         ;; thread, under the lock and before the next frame is resolved.
         (push-thread-bindings {#'aim-paths/*committed-values* (committed-values runtime)
                                #'capability/*rig-index* (rig-indexer runtime)
                                ;; Quantised launches prepared off the frame (#918).
                                #'spatial/*prepared-launches* (some-> (:launch-prep runtime) :bindings)})
         (vreset! committed-bound? true)
         (when-let [[target output-slot] (when @(:running runtime) (capture-render-time runtime time-ms output-slot))]
           (let [before @(:state runtime)
                 time-ms (max target (get-in before [:model :time-ms]))
                 messages (java.util.ArrayList.)
                 inline-registrations (volatile! {})
                 ordinary-count (.drainTo ^LinkedBlockingQueue (:queue runtime) messages 256)
                 release-count (.drainTo ^LinkedBlockingQueue (:cue-release-queue runtime) messages 256)
                 gesture-ends (vals @(:push-gesture-ends runtime))
                 _ (when gesture-ends
                     (vreset! (:push-gesture-ends runtime) {})
                     (doseq [message gesture-ends] (.add messages message)))
                 _ (when (or gesture-ends (and (pos? ordinary-count) (pos? release-count)))
                     (.sort messages ingress-order))
                 ;; Messages the previous cycle held back come first, in their order.
                 _ (when-let [held (seq @(:held-messages runtime))]
                     (reset! (:held-messages runtime) [])
                     (.addAll messages 0 ^java.util.Collection (vec held)))
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
                 clock-generation @(:presentation-clock runtime)
                 [clock-model audio-clock-confidence] (source-clock-model runtime (:model before) time-ms)
                 ;; Millisecond show progress stands still over an interval that
                 ;; began stopped, as beat progress does (#699).
                 held-ms (let [previous (:model before)]
                           (if (false? (get-in previous [:transport :playing?]))
                             (max 0 (- time-ms (get-in previous [:transport :time-ms] (:time-ms previous 0)))) 0))
                 clock-model (show-clock/hold clock-model held-ms)
                 ;; Draft commands edit this cycle's view; it is published only after the frame commits.
                 draft-initial @(:scene-draft runtime)
                 draft-view (atom draft-initial)
                 owner-runtime (assoc runtime ::draft-view draft-view)
                 [model receipts restored? restored-output processed held]
                 (reduce
                  (fn [[model receipts restored? restored-output processed held :as acc] {:keys [reply authority] :as message}]
                    (cond
                      ;; A message that depends on a publication waiting for
                      ;; its check waits with it, in order; everything else,
                      ;; blackout, master, transport and controller input
                      ;; included, goes ahead (#784).
                      (and held (waits-behind? model held message))
                      (assoc acc 5 (conj held (cond-> message (= :definition (:type message)) (assoc ::held? true))))
                      :else
                    (try
                      (let [result (apply-message model message (if restored? (:time-ms model) time-ms) owner-runtime)]
                       (if-let [waiting (::hold result)]
                        (assoc acc 5 (conj (or held []) waiting))
                      (let [command (when (= :command (:type message)) (:payload message))
                            ;; The reducer supplies the committed sample or rollback value.
                            gesture-write (when (#{:gesture/sample :gesture/cancel} (:op command))
                                            (first (filter (comp #{:param/changed} :type) (:events result))))]
                        (when-let [candidate (:candidate result)]
                          (vswap! retired conj (connections/commit-restore! (:connections runtime) candidate))
                          (vreset! restored-generation (:generation (connections/status (:connections runtime))))
                          (let [{:keys [session-id lease after-restore!]} authority]
                            (vreset! restoring-device (when after-restore! (assoc authority :receipt reply)))
                            (reset! (:device-reconnects runtime) (if after-restore! {session-id lease} {})))
                          ;; Detach atomically; opening sessions also verify their captured epoch.
                          (vswap! retired-devices into (detach-devices! runtime)))
                        (when (and (not (:candidate result)) (not= (:epoch model) (get-in result [:model :epoch])))
                          (vreset! restarted-generation (connections/commit-restart! (:connections runtime) (:model result)))
                          (reset! (:device-reconnects runtime) {})
                          (vswap! retired-devices into (detach-devices! runtime)))
                        (when-let [[key handler] (:inline-registration result)]
                          (when (fn? handler) (vswap! inline-registrations assoc key handler)))
                        [(:model result) (conj receipts [reply (:ack result) command (:replayed? result) gesture-write])
                         (or restored? (:restore? result))
                         (if (:restore? result) (:restored-output result) restored-output)
                         (conj processed message) held])))
                      (catch Exception error
                        [model (conj receipts [reply (merge {:status :rejected} (error-data error)) (when (= :command (:type message)) (:payload message))])
                         restored? restored-output (conj processed message) held]))))
                  [(-> (consume-sync! runtime (consume-audio! runtime (staging/collect (staging/begin clock-model)
                                                       :reactive {:kind :runtime :id :reactive} (:pending-reactive-faults before)) time-ms))
                       (calibration/advance physical-receipts physical-now)
                       (photometry/advance physical-receipts physical-now)
                       (fog-scan/expire physical-now)) [] false nil [] nil] messages)
                 ;; Held messages keep their prepared restore for a later cycle.
                 _ (reset! (:held-messages runtime) (vec held))
                 _ (when held (vreset! candidates (vec (keep #(get-in % [:payload :candidate]) processed))))
                 time-ms (if restored? (:time-ms model) time-ms)
                 _ (when (or restored? @restarted-generation)
                     (reset! draft-view nil)
                     (when restored? (reset! (:clock runtime) {:origin-nanos (System/nanoTime) :time-ms time-ms}))
                     (reset! (:leases runtime) {})
                     (locking (:sync-input runtime)
                       (reset! (:sync-input runtime) {:epoch (:epoch model) :events []}))
                     (locking (:audio-input runtime)
                       (reset! (:audio-input runtime) {:epoch (:epoch model) :events [] :features nil})))
                 model (if (cue-timing/active? model)
                         (cue-timing/advance model (cue-clock-identity runtime model) time-ms @(:leases runtime) command)
                         model)
                 ;; A quantized Room draft fires at the same point as grid cue launches.
                 model (if (and (not restored?) (:pending-apply @draft-view))
                         (fire-scene-draft runtime draft-view model time-ms)
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
                                     (fence-section-pattern-runs runtime audio-clock-confidence)
                                     (subscribers/advance time-ms (event-callbacks/results (:event-callbacks runtime)) (scheduler-host runtime))
                                     (composition/advance time-ms) (advance-auto time-ms runtime)
                                     (audio-return/advance (duplex/frame-input! (:audio-returns runtime)) time-ms routing/admit-sync)
                                     (routing/advance time-ms command)
                                     ;; Macro, automation and routing writes above can override a ramp this frame.
                                     (subscribers/advance-ramps time-ms))]
                       (render-frame runtime before model time-ms
                                     {:started started :frame-start-ns frame-start-ns
                                      :photo-session photo-session :sampling-session sampling-session}))
                     (catch Exception error
                       (assoc before :model model :status {:error (error-data error)} :fault-events []
                              :output (if (or (pause-look/blackout? model) (zero? (:master model)))
                                        (render/resolve-frame (patch! runtime (get-in model [:rig :fixtures])) [] {:blackout? true})
                                        (:output before))))))
                 message-receipts (map vector processed receipts)
                 operations (keep (fn [[message [_ ack _ replayed?]]]
                                    (when-not replayed?
                                      {:operation (:type message) :command (get-in message [:payload :op])
                                       :ack ack :emitter-id (or (get-in message [:authority :operator-id])
                                                              (get-in message [:payload :source :adapter]))}))
                                  message-receipts)
                 ;; Confidence belongs to this live clock sample and its resource generation.
                 next-state (assoc next-state
                                   :checkpoint-results (checkpoint-results before message-receipts)
                                   :transport-generation @(:presentation-clock runtime)
                                   :audio-clock-confidence (when-not (or restored? @restarted-generation) audio-clock-confidence))
                 fired-outcomes (cue-timing/fired-outcomes (:model next-state))
                 next-state (if (seq fired-outcomes) (update next-state :model cue-timing/clear-fired-outcomes) next-state)
                 next-state (advance-section-patterns runtime before next-state (or restored? @restarted-generation))
                 committed (show-events/commit (:event-admit runtime) (:model before) next-state operations)
                 next-state (venue-fog/commit! runtime (:state committed)
                              (patch! runtime (get-in next-state [:model :rig :fixtures])) restored?)
                 epoch (get-in next-state [:model :epoch])
                 previous-outcomes (if (= epoch (get-in before [:model :epoch]))
                                     (:history-command-outcomes before []) [])
                 previous-source-outcomes (if (= epoch (get-in before [:model :epoch]))
                                            (:parameter-source-outcomes before []) [])
                 next-state (assoc next-state :history-command-outcomes
                                   (operator-history/committed-outcomes previous-outcomes epoch message-receipts)
                                   :parameter-source-outcomes
                                   (parameter-sources/committed-outcomes previous-source-outcomes epoch message-receipts))]
             (when (or (seq (get-in next-state [:output :fog-demands])) @(:fog-deadlines runtime))
               (swap! (:fog-deadlines runtime) fog-deadline/remember
                      (or (:frame-id presentation-token) (get-in next-state [:model :frame-number] 0))
                      (when-not restored? (get-in next-state [:output :fog-demands]))))
             ;; Every consumer of this committed state shares one parameter
             ;; sample, held beside it rather than inside it (#533).
             (reset! (:state runtime) (waveforms/remember-frame!
                                        (:state runtime)
                                        (dissoc next-state :autopilot-author-request :pending-reactive-faults)))
             (commit-draft-view! runtime draft-initial @draft-view)
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
             ;; After the frame is handed to output, so the fold never delays it.
             (when-not restored? (observe-committed! runtime (:model next-state)))
             ;; Launches now waiting for a grid boundary are prepared off the frame (#918).
             (when-let [prep (:launch-prep runtime)] (launch-prep/submit! prep (:model next-state)))
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
                        (cadence/finish (assoc sampling :defer-live-slowdown? defer?) (:model next-state) elapsed)))
               ;; Queued after the live frame's owner time is measured and published.
               (schedule-audition! runtime (:model next-state) (get-in next-state [:model :time-ms]) elapsed
                                   ;; A transport discontinuity rebased the live model by this beat
                                   ;; step; a stopped interval held its millisecond progress.
                                   (cond-> []
                                     (not= clock-generation @(:presentation-clock runtime))
                                     (conj {:timeline (:timeline draft-initial)
                                            :from-beat (get-in before [:model :transport :beat])
                                            :beat (get-in next-state [:model :transport :beat])})
                                     (pos? held-ms) (conj {:timeline (:timeline draft-initial) :held-ms held-ms})))))))
       (finally
         (when @committed-bound? (pop-thread-bindings))
         ;; First, so a later failure cannot leave detached IDs in :closing.
         (retire-devices! runtime @retired-devices nil)
         (when-let [request @author-request]
           (authoring/request! (agent-manager runtime) request #(submit! runtime :autopilot-authored %)))
         (effect-faults/offer! (:effect-logs runtime) @committed-fault-events)
         (finish-resource-work! runtime @retired @candidates
                                @restored-generation @restarted-generation)
         ;; Queued behind those closes on the same worker.
         (retire-devices! runtime []
                          (when-let [{:keys [receipt] :as device} @restoring-device]
                            (when (and (realized? receipt) (= :accepted (:status @receipt)))
                              #(reopen-restoring-device! runtime device))))
         (rdm/reconcile! (:rdm runtime))
         (let [model (:model @(:state runtime))]
           (when @(:running runtime)
             (video-hub/request-reconcile! (:video runtime) (get-in model [:settings :video :sources])))
           ;; Hand the committed shader uniforms to their producer; it never
           ;; renders here, and a stopped producer ignores them (#673).
           (shader-producer/sync! (:shader-video runtime) (video-shader/requests model))
           (let [^Object signal (:video-frame-signal runtime)] (locking signal (.notifyAll signal))))))))

(defn- prepare-rig-edit [model proposed]
  (let [basis (:rig model)
        proposed (dissoc proposed :runtime-id :expected-epoch)
        types (get-in model [:registry :fixtures])
        proposed (cond-> proposed
                   (map? (:fixtures proposed))
                   (update :fixtures (partial fixture-patch/expand types))
                   ;; Entries marked :placement? are instantiated from their
                   ;; published type, as :fixture-patch does (#226).
                   (vector? (:fixtures proposed))
                   (update :fixtures (partial fixture-service/admit-placements types)))]
    {:basis basis :rig (rig/prepare-edit basis proposed)}))

(defn- ingress-queue [runtime type payload]
  (if (or (= type :cue-cancel)
          (and (= type :command) (= :cue/cancel-launch (:op payload))))
    (:cue-release-queue runtime)
    (:queue runtime)))

(defn- enqueue-push-gesture-end! [runtime {:keys [payload authority reply expected-epoch] :as message}]
  ;; Under the owner lock, each open gesture or queued begin can reserve only
  ;; one end or cancel. Both sources are bounded and drained in the same owner cycle.
  (let [ends (:push-gesture-ends runtime)
        key (push-gesture-key payload authority)
        queued-begin? (fn [queued]
                        (and (= :command (:type queued)) (= :gesture/begin (get-in queued [:payload :op]))
                             (= expected-epoch (:expected-epoch queued)) (= authority (:authority queued))
                             (= key (push-gesture-key (:payload queued) (:authority queued)))))]
    (cond
      (contains? @ends key)
      (deliver reply {:status :rejected :code :gesture-end-queued :command-id (:command-id payload)})

      (or (open-push-gesture (:model @(:state runtime)) key)
          (some queued-begin? (iterator-seq (.iterator ^LinkedBlockingQueue (:queue runtime))))
          (some queued-begin? @(:held-messages runtime)))
      (vswap! ends assoc key message)

      :else (deliver reply {:status :rejected :code :unknown-gesture :command-id (:command-id payload)}))))

(defn- enqueue! [runtime type payload authority checkpoint-request]
   (let [reply (promise) candidate (volatile! nil)]
     (try
       (let [before (:model @(:state runtime))
             epoch (get payload :expected-epoch (:epoch before))]
         (when-not (and @(:running runtime) (= epoch (:epoch before))
                        (or (not (#{:photometry :fog-calibration :venue-surface} type)) (and (= epoch (:expected-epoch payload)) (= (:id runtime) (:runtime-id payload))))
                        (or (nil? (:runtime-id payload)) (= (:id runtime) (:runtime-id payload))))
           (throw (ex-info "Show stopped or session changed" {})))
         (when (and (= :import type) authority) (require-import-device! runtime authority))
         (when (contains? (get output-arming type) (keyword (:op payload)))
           (require-devices! runtime [type (keyword (:op payload))]))
         (let [payload (if (= :rig type) (prepare-rig-edit before payload) payload)
               payload (if (and (= :fog-calibration type) (= :fit (:op payload)))
                         (merge payload (fog-scan/prepare-fit before (:session-id payload) (:trace @(:venue-fog runtime))))
                         payload)
               payload (if (= :import type)
                         (let [document (persistence/read-string (:source payload))
                               expected-show-id (:expected-source-show-id payload)
                               _ (when (and expected-show-id (not= expected-show-id (get-in document [:model :show-id])))
                                   (throw (ex-info "Checkpoint belongs to a different show" {:code :wrong-show})))
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
                           (when (and (= :import type) authority) (require-import-device! runtime authority))
                           (and @(:running runtime) (= epoch (get-in @(:state runtime) [:model :epoch]))
                                (let [message (cond-> {:type type :payload payload :reply reply :expected-epoch epoch
                                                       :ingress-order (vswap! (:ingress-order runtime) inc)}
                                                (and (#{:command :import} type) authority) (assoc :authority authority)
                                                checkpoint-request (assoc ::checkpoint-request checkpoint-request))]
                                  (if (and (= :command type) (push-gesture-terminal? payload authority))
                                    (do (enqueue-push-gesture-end! runtime message) true)
                                    ;; Messages a cycle held back still count against the
                                    ;; queue's 256 slots (#784).
                                    (and (< (+ (.size ^LinkedBlockingQueue queue) (count @(:held-messages runtime))) 256)
                                         (.offer ^LinkedBlockingQueue queue
                                                 (if (= :definition type) (prepare-publication! runtime message nil) message)))))))]
           (when-not accepted? (throw (ex-info "Show changed or command queue is full" {})))))
       (catch Exception error
         (when @candidate (connections/abort-prepared! @candidate))
         (let [command-id (or (:command-id payload) (get-in payload [:command :command-id]))]
           (deliver reply (cond-> (merge {:status :rejected} (error-data error))
                            (= :preview-only (:code (ex-data error))) (assoc :code :preview-only)
                            command-id (assoc :command-id command-id))))))
     reply))

(defn submit!
  "Enqueue an operation. Full restores are validated and resource-prepared off
  the frame-owner lock, then committed atomically by the next owner cycle.
  Use :cue-schedule and :cue-cancel for explicit musical launch timing.
  Host adapters pass authenticated operator authority as a separate argument.
  Serialized payloads cannot confer operator privileges."
  ([runtime payload] (submit! runtime :command payload))
  ([runtime type payload] (submit! runtime type payload nil))
  ([runtime type payload authority] (enqueue! runtime type payload authority nil)))

(defn quantized-cue-status
  "Return a queued, fired, cancelled or missed launch receipt by command ID."
  [runtime command-id]
  (cue-timing/receipt (:model @(:state runtime)) command-id))

(defn schedule
  "Read the beat-clock schedule Studio shows: upcoming events (scheduled and
  subscriber steps), active ramps and recent outcomes. Pass :compact for the
  low-token agent form. See docs/scheduler.md."
  ([runtime] (subscribers/schedule-view (:model @(:state runtime))))
  ([runtime form]
   (case form
     :compact (subscribers/compact-view (:model @(:state runtime)))
     :full (schedule runtime))))

(defn- timeline-reference
  "The matched recording's sections and the current reference lock, when a
  section reference is attached to this show."
  [runtime committed]
  (when-let [profile (:profile (or (:section-pattern-state committed) (:section-patterns runtime)))]
    (when-let [{:keys [reference-state reference-now-ms]} (reference-evidence runtime (get-in committed [:model :epoch]))]
      {:profile profile
       :lock (try (reference-position/snapshot reference-state reference-now-ms)
                  (catch clojure.lang.ExceptionInfo _ nil))})))

(defn timeline
  "Read the musical timeline Studio draws around now: scheduled and fired
  events, ramps, sections, the beat grid, the captured-audio lane and the
  level meters. Pass {:bars n} for n bars either side (default 8). The
  three-argument form projects a given committed state. See docs/timeline.md."
  ([runtime] (timeline runtime {}))
  ([runtime options] (timeline runtime @(:state runtime) options))
  ([runtime {:keys [model output] :as committed} options]
   (timeline/view {:model model :output output
                   :waveform (audio/history-waveform (connections/audio (:connections runtime)))
                   :reference (timeline-reference runtime committed)
                   ;; The active track-feel personality (#647), from the same
                   ;; cached projection the snapshot carries as :personalities.
                   :feel (get-in (personality-views runtime model) [:projection :active])}
                  options)))

(defn post-events!
  "Validate and enqueue a batch of beat-clock events atomically. Returns the
  receipt promise from submit!, whose :event-ids lists the scheduled events."
  ([runtime events] (post-events! runtime events {}))
  ([runtime events envelope]
   (submit! runtime (merge {:source {:adapter :repl :session-id "repl"}} envelope
                           {:op :schedule/post :events (vec events)}))))

(defn cancel-events!
  "Cancel scheduled events and running ramps by :event-ids, :posted-by (a
  source submap), :label or :destination. A cancelled ramp holds its current value."
  [runtime selection]
  (submit! runtime (merge {:source {:adapter :repl :session-id "repl"}} selection {:op :schedule/cancel})))

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
                  (reset! (:device-reconnects runtime) {})
                  (doseq [queue [(:queue runtime) (:cue-release-queue runtime)]]
                    (loop []
                      (when-let [message (.poll ^LinkedBlockingQueue queue)]
                        (when-let [candidate (get-in message [:payload :candidate])]
                          (vswap! candidates conj candidate))
                        (deliver (:reply message) {:status :rejected :message "Show stopped"})
                        (recur))))
                  (doseq [message @(:held-messages runtime)]
                    (when-let [candidate (get-in message [:payload :candidate])]
                      (vswap! candidates conj candidate))
                    (deliver (:reply message) {:status :rejected :message "Show stopped"}))
                  (reset! (:held-messages runtime) [])
                  (doseq [message (vals @(:push-gesture-ends runtime))]
                    (deliver (:reply message) {:status :rejected :message "Show stopped"}))
                  (vreset! (:push-gesture-ends runtime) {})
                  true))]
    ;; No cleanup waits while holding the owner lock: a closing device or agent
    ;; may synchronously dispatch, and must receive its stopped receipt promptly.
    (when stop?
      (doseq [cleanup (concat (map :close! (vals (first (swap-vals! (:observers runtime) (constantly {})))))
                             (map #(fn [] (connections/abort-prepared! %)) @candidates)
                             [#(some-> (:recovery runtime) deref recovery/close!)
                              #(waveforms/forget-frames! (:state runtime))
                              #(when (realized? (:parameter-frames runtime)) (parameter-frames/stop! @(:parameter-frames runtime)))
                              ;; The last committed folds finish here, as inline folding
                              ;; finished them in their own cycle; the worker is then
                              ;; let go without interrupting a fold, and fold failures
                              ;; reach status.
                              #(when-let [{:keys [executor]} (:observer-worker runtime)]
                                 (try (settle-observers! runtime)
                                      (finally
                                        (publish-observer-errors! runtime)
                                        (when (realized? executor) (.shutdown ^java.util.concurrent.ExecutorService @executor)))))
                              #(when-let [writer (some-> (:live-runs runtime) deref :device-writer)]
                                 (when (realized? writer) (.shutdownNow ^java.util.concurrent.ExecutorService @writer)))
                              #(event-callbacks/close! (:event-callbacks runtime))
                              #(rdm/close! (:rdm runtime))
                              #(output-scheduler/close! (:publisher runtime))
                              #(send-output! runtime (render/idle-universes (:compiled @(:patch runtime))) 0 nil)
                              #(mpv-session/close! (:section-player runtime))
                              #(effect-faults/close! (:effect-logs runtime))
                              #(compute/close! (:compute runtime))
                              #(programs/retire! @(:compiler-context runtime))
                              #(some-> (:launch-prep runtime) launch-prep/close!)
                              #(when-let [worker (get-in runtime [:compile-worker :owned])]
                                 (when (realized? worker) ((:close! @worker))))
                              #(when (realized? (:audition-worker runtime))
                                 (.shutdownNow ^java.util.concurrent.ExecutorService @(:audition-worker runtime)))
                              #(duplex/stop! (:audio-returns runtime))
                              #(video-hub/stop! (:video runtime))
                              #(shader-producer/stop! (:shader-video runtime))
                              #(development/close! (:development runtime))
                              #(connections/close! (:connections runtime))
                              (fn [] (locking (:devices runtime)
                                       (doseq [device (vals @(:devices runtime))]
                                         (try (devices/close! device) (catch Exception _)))
                                       (reset! (:devices runtime) {})))
                              ;; Queued retirements still run, for up to one second. A close that
                              ;; is still blocked after that keeps its device claimed until its
                              ;; write returns; it is reported as :abandoned. A late tick's
                              ;; retirement is rejected and closes on that thread instead.
                              (fn [] (when-let [worker (:device-retirement-worker runtime)]
                                 (let [^java.util.concurrent.ExecutorService executor @worker]
                                   (.shutdown executor)
                                   (when-not (.awaitTermination executor device-retirement-stop-ms
                                                                java.util.concurrent.TimeUnit/MILLISECONDS)
                                     (let [{:keys [pending closing]}
                                           (swap! (:device-retirement-state runtime) #(assoc % :abandoned (:pending %)))]
                                       (swap! (:state runtime) assoc-in [:status :cleanup-error]
                                              {:message "Retired device sessions were still closing when the show stopped"
                                               :details {:pending pending :closing closing}}))))))

                              #(.close ^java.io.Closeable (:executor runtime))
                              #(some-> ^java.io.Closeable (:spatial-sampling runtime) .close)
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

(defn- agent-snapshot
  "The Studio projection without the pictures a text agent cannot use."
  [runtime state]
  (update (project-snapshot runtime state) :personalities personalities/without-icons))

(defn dispatch-tool!
  "Agent and REPL tool boundary; all mutation uses the same owner queue as UI."
  [runtime tool arguments metadata]
  (let [state @(:state runtime)
        model (:model state)
        _ (when (and (:expected-epoch metadata) (not= (:expected-epoch metadata) (:epoch model)))
            (throw (ex-info "Agent run belongs to a previous show session" {})))
        ;; The envelope below names the agent; a tool argument cannot.
        arguments (apply dissoc (if (= "set_lighting_scene" tool)
                                  (venue-tools/scene-request model arguments)
                                  (data/normalize-json arguments))
                         [:source :command-id :epoch :show-id :expected-epoch])
        ;; An agent owns what its conversation posted, across runs (turns), so it
        ;; can cancel or stop a change it scheduled in an earlier turn (#767).
        envelope (merge (select-keys metadata [:epoch :show-id :expected-epoch])
                        {:command-id (:command-id metadata)
                         :source (if-let [conversation (:conversation-id metadata)]
                                   {:adapter :agent :session-id conversation :run-id (:run-id metadata)}
                                   {:adapter :agent :session-id (:run-id metadata)})})
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
          "knob_candidate" [:command (merge envelope (dissoc arguments :op)
                                            {:op (keyword "knob-candidate" (name (:op arguments)))})]
          "schedule_events" [:command (merge envelope {:op :schedule/post :events (vec (:events arguments))})]
          "cancel_scheduled" [:command (merge envelope (select-keys arguments [:event-ids :label :destination])
                                              (when (:mine? arguments) {:posted-by (select-keys (:source envelope) [:adapter :session-id])})
                                              {:op :schedule/cancel})]
          "inspect_schedule" [:inspect-schedule nil]
          "set_lighting_scene" [:lighting-scene (merge envelope arguments)]
          "configure_energy_autopilot" [:settings (merge (venue-tools/autopilot-request model arguments)
                                                         (select-keys envelope [:expected-epoch]))]
          "set_lighting_budget" [:settings (merge (venue-tools/budget-request model arguments) (select-keys envelope [:expected-epoch :source]))]
          ;; An :at waits for its line on the scheduler, posted by this agent run (#767).
          "configure_auto" [:auto (merge (:configuration arguments) (select-keys arguments [:at])
                                         (select-keys envelope [:expected-epoch :command-id :source]))]
          (throw (ex-info "Unknown studio tool" {:tool tool})))]
    (case type
      :inspect (agent-inspection runtime state #(agent-snapshot runtime state))
      :inspect-schedule (agent-inspection runtime state #(subscribers/compact-view model))
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
                                        (agent-inspection runtime state #(agent-snapshot runtime state))))
                         :dispatch! #(dispatch-tool! runtime %1 %2 (merge %3 admission))
                         :emit! emit! :done! done!})))

(defn cue-variation-preview
  "Read the running cue's two variation layouts and saved row from one commit.
  Apply a selection through submit! with :cue-variation/apply and the returned
  :basis, :instance-id, :seed, :layout, :row-page and the chosen :position."
  [runtime {:keys [runtime-id epoch] :as request}]
  (let [model (:model @(:state runtime))]
    (when (or (and runtime-id (not= runtime-id (:id runtime)))
              (and epoch (not= epoch (:epoch model))))
      (throw (ex-info "Show session changed" {:http/status 409})))
    (variations/project model request)))

(defn- push-parameter-projector []
  (let [cache (waveforms/create-cache)
        precision-cache (atom nil)]
    (fn [snapshot view capture]
      (->> (waveforms/project-display cache snapshot view capture)
           (precision/push-view precision-cache (:model snapshot))))))

(defn- leasing-response-samples
  "Retain exact response inputs on committed frames only while this display is
  open and, with demanded?, only while it shows parameter readings."
  ([runtime transport] (leasing-response-samples runtime (constantly true) transport))
  ([runtime demanded? transport]
   (assoc transport
          :open! (fn []
                   (let [release (lease-response-samples! runtime demanded?)]
                     (try (assoc ((:open! transport)) ::release-response-samples release)
                          (catch Throwable error (release) (throw error)))))
          :close! (fn [handle]
                    (try ((:close! transport) handle)
                         (finally (when-let [release (::release-response-samples handle)] (release))))))))

(defn push-browser-display
  "Build a Push display transport using Studio's shared reactive components.
  Capture and waveform projection run on the display worker, not the show clock.
  Supply the browser executable and optional injected sink in options.
  For Push 3 pad variations, set :presentation-receipts? true."
  [runtime options]
  (let [project-parameters (push-parameter-projector)
        pages (volatile! nil)
        scene-cache (push-scene/create-cache)
        ;; Whether the last projected view read parameter values. Scene,
        ;; block-trace and pad-overlay views do not, so the owner keeps no
        ;; response samples for them.
        reading? (atom true)]
    (leasing-response-samples runtime #(deref reading?)
     (display/browser-display
     (assoc options :project!
            (fn [view]
              (let [snapshot (assoc @(:state runtime) :runtime-id (:id runtime))
                    model (:model snapshot)
                    view (push-knobs/project-display model view)
                    mode (if (:pad-overlay view) :parameters
                             (or (:display-mode view) (get-in model [:settings :controllers :push-display :mode]) :parameters))
                    _ (reset! reading? (and (not (:pad-overlay view)) (contains? #{:parameters :audio} mode)))
                    view (cond-> (assoc view :display-mode mode)
                           (#{:parameters :audio} mode)
                           (assoc-in [:visualization-clock :sample-period-ms]
                                     (cue-pads/sample-period-ms (:cue-pad-previews runtime))))
                    visual (cond
                             (:pad-overlay view) (dissoc view :scene :parameters)
                             (#{:parameters :audio} mode)
                             (let [capture (when (= mode :audio)
                                             (audio/waveform (connections/audio (:connections runtime))))]
                               (project-parameters snapshot view capture))
                             (= :block-trace mode) (block-trace/project-display (:id runtime) model view)
                             (#{:simulator :rig} mode) (push-scene/project-display scene-cache snapshot
                                                      (assoc view :runtime-id (:id runtime)))
                             :else view)]
                (cond-> (page-overview/project-display pages model visual)
                  (personalities/active model) (assoc :personality (:push (personality-views runtime model)))))))))))

(defn- push-native-display [runtime]
  (let [project-parameters (push-parameter-projector)]
    (leasing-response-samples runtime
      (display/native-display
        {:project! (fn [view]
                     (let [snapshot @(:state runtime)]
                       (project-parameters snapshot (push-knobs/project-display (:model snapshot) view) nil)))}))))

(def ^:private device-command-wait-ms 5000)

(defn- external-transport-fn
  "model -> true while an external clock owns play and stop: any selected
  source other than :manual, except Ableton Link with start/stop sync off
  (tempo-only Link leaves playback local). A surface's PLAY and STOP then
  stay quiet instead of fighting Live (#706 slice 2)."
  [runtime]
  (fn [model]
    (boolean
     (when-let [manager (try (connections/sync (:connections runtime)) (catch Exception _ nil))]
       (let [{:keys [selected-source follow-transport?]}
             (session/clock-at manager (get-in model [:transport :time-ms] (now-ms runtime)))]
         (and (some? selected-source) (not= :manual selected-source) (not (false? follow-transport?))))))))

(defn- surface-command!
  "Enqueue a host surface command without waiting: the owner may be ticked by
  the caller, and a device reads a missing surface as strip 0. A watcher off
  the owner thread logs a refusal and keeps it for the snapshot's
  :control-surfaces until that surface's next accepted host command."
  [runtime epoch surface-id command]
  (let [refusals (:surface-refusals runtime)
        ;; Watchers finish in any order; only the latest command for a surface
        ;; may record or clear its refusal.
        sequence (-> (swap! refusals update-in [::sequence surface-id] (fnil inc 0)) (get-in [::sequence surface-id]))
        latest? #(= sequence (get-in % [::sequence surface-id]))
        receipt (submit! runtime (merge {:source {:adapter :surface :session-id surface-id} :expected-epoch epoch}
                                        command))]
    (.start (doto (Thread. (fn []
                             (let [{:keys [status] :as ack} (deref receipt 30000 {:status :timeout})
                                   refusal (merge {:op (:op command)} (select-keys ack [:status :code :message]))]
                               (cond
                                 (= :accepted status)
                                 (swap! refusals #(cond-> % (latest? %) (dissoc surface-id)))
                                 @(:running runtime)
                                 (do (timbre/warn "Control surface command refused" surface-id (:op command)
                                                  (select-keys ack [:status :code :message]))
                                     (swap! refusals #(cond-> % (latest? %) (assoc surface-id refusal)))))))
                           "tolglow-surface-command")
              (.setDaemon true)))
    receipt))

(declare capture-action!)

(defn connect-device!
  "Explicit REPL connection; no devices open at application startup. kind is
  :push-2/:push-3/:push-3-midi/:seaboard/:x-touch/:midi/:osc/:sensor. Options select ports,
  expression modes and bindings; imported namespaces never acquire devices.
  On Push 2 and Push 3, hold Accent as you touch a cue encoder to make a
  temporary edit that restores the previous value on lift. Shift adjusts
  the step size. Override Accent with :temporary-encoder-modifier in the
  connection options, using a button CC on Push 2 or a button ID on Push 3.
  Temporary edits cover typed cue params on Push 2 and numeric params on Push 3.
  Push 2 accepts :native-display? true for the Wayang parameter display.
  An explicit :display transport takes precedence. Set :cue-variations? true
  for exact beat-ratio edits on both models and the Push 2 Note-button overlay.
  Set :cue-variation-pads? true on Push 3 with captured pads and a dedicated
  :variation/cycle button in :button-bindings. The display must support
  presentation receipts. This option also enables exact beat-ratio edits.
  Push 3 accepts :cue-grid {:buttons {id action}} with captured, unbound
  User Mode buttons for :cue-grid/select, :cue-grid/reorder,
  :cue-grid/page-up and :cue-grid/page-down (tolglow.surface.push-3). It
  registers a control surface under its session ID whose :grid shows the
  running launches; Select commits that surface's :selected and Reorder
  sends :instance/move-after, as Studio does. :columns (8) widens the
  content for a Push extended to the right.
  Push 2 accepts the same :cue-grid with button CCs ({48 :cue-grid/select
  88 :cue-grid/reorder 62 :cue-grid/page-up 63 :cue-grid/page-down}, which
  :active-cue-grid? true also selects) and registers an
  :ableton/push-2-grid surface whose :grid runs the same Tolglow grid
  (tolglow.surface.push-2); Afterglow's own Push 2 active cue grid is not
  used. Push 2 and Push 3 grids link with each other like any cue grids.
  :capture-button (a button CC on Push 2, a captured User Mode button ID on
  Push 3) runs the retrospective Capture of capture-action! on each press,
  as Studio's Capture button and capture-phrase! do (#671).
  X-Touch accepts :effect-group-view :manual or :overlay. Group faders occupy
  the first strips, followed by any configured cue channels. Each X-Touch or
  extender registers a control surface under its session ID; its window is
  show data, moved by its bank buttons, Studio and :control-surface/bank. Join
  separately connected surfaces with link-surfaces!. X-Touch units run the
  Tolglow mixer layout (tolglow.surface.x-touch): encoder modes, selection,
  Solo/Mute and the other layout state are committed per logical surface,
  see surface-layout!. :color-match (:hue, :rgb or :off) picks how strip
  colours map to the eight scribble colours.
  Vector encoders (#813): with :vector-encoders, touching the encoder of a
  ring, XY or XY+Z vector parameter spreads its components over
  neighbouring encoders, one each, even over a full bank, until no taken
  encoder has been touched for :linger-ms (1000). Each detent sends
  {:op :param/nudge :component i :detents n}, which the owner resolves
  against the committed vector and commits as param/set. On Push 2 pass
  true (or {:linger-ms :buttons :leds}); the upper-row buttons under the
  taken encoders light in component colours from the device's palette. On Push 3 pass {:encoders
  [captured encoder IDs, left to right] :buttons [captured IDs]} and bind
  the vector to an encoder with :encoder-bindings {id {:param k}}.
  Linked knobs: on Push 3 bind a captured encoder with :encoder-bindings
  {id {:target :knob :knob-id k}}; on Push 2 set :knob-page {:button cc
  :knob-ids [k ...]} to toggle the track encoders onto up to eight knobs;
  its button is lit while the page is shown."
  [runtime kind {:keys [session-id ::restore-lease ::reconnect-lease] :as options}]
  (require-devices! runtime [:connect-device kind])
  (let [id session-id epoch (get-in @(:state runtime) [:model :epoch])]
    (when-not (and (string? id) (seq id)) (throw (ex-info "Device requires session-id" {})))
    (locking (:devices runtime)
      (locking (:lock runtime)
        (when-not @(:running runtime) (throw (ex-info "Show is stopped" {})))
        (when (get @(:devices runtime) id) (throw (ex-info "Device session already connected" {:session-id id})))
        ;; A retired session with this ID may still own its port or display (#226).
        (when (contains? (some-> (:device-retirement-state runtime) deref :closing) id)
          (throw (ex-info "Previous device session is still closing" {:session-id id :code :device/closing})))
        (if reconnect-lease
          (when-not (identical? reconnect-lease (get @(:device-reconnects runtime) id))
            (throw (ex-info "Device reconnect was retired" {:session-id id})))
          (swap! (:device-reconnects runtime) dissoc id)))
      (let [effect-group-view (when (= :x-touch kind) (:effect-group-view options))
            _ (when (and (some? effect-group-view) (not (#{:manual :overlay} effect-group-view)))
                (throw (ex-info "X-Touch effect-group view must be :manual or :overlay" {:view effect-group-view})))
            ;; Linked knobs (#666): validated before any port opens.
            knob-page (when (and (= :push-2 kind) (contains? options :knob-page))
                        (push-knobs/push-2-encoder-overlay (:knob-page options) options))
            options (cond-> options (= :push-3 kind) push-knobs/push-3-options)
            ;; The Push 3 Select/Reorder pad grid (#545), validated before any port opens.
            push-3-grid? (and (= :push-3 kind) (some? (:cue-grid options)))
            _ (when push-3-grid? (push-3-surface/validate-options! options))
            ;; Push 2 surface parts (#706): cue grid, knob page LED, Capture button.
            push-2-surface? (and (= :push-2 kind) (push-2-surface/surface? options))
            push-2-grid? (and push-2-surface? (some? (push-2-surface/grid-options options)))
            _ (when push-2-surface? (push-2-surface/validate-options! options))
            _ (when (and (= :push-3 kind) (some? (:capture-button options)))
                (push-3-surface/validate-capture! options))
            push-3-surface? (and (= :push-3 kind)
                                 (or push-3-grid? (some? (:capture-button options)) (true? (:cue-variation-pads? options))
                                     (some? (:vector-encoders options))))
            surface-grid? (or push-2-grid? push-3-grid?)
            push-source (when (#{:push-2 :push-3} kind)
                          {:adapter kind :session-id id})
            operator-id (when push-source (str (name kind) "/" id "/" (UUID/randomUUID)))
            open! (case kind :push-2 (if push-2-surface? push-2-surface/connect! devices/open-push-2!)
                         :push-3 (if push-3-surface? push-3-surface/connect! devices/open-push-3!)
                         :push-3-midi devices/open-push-3-midi! :seaboard devices/open-seaboard! :x-touch x-touch/connect!
                         :midi devices/open-midi! :osc devices/open-osc! :sensor devices/open-sensor!
                         (throw (ex-info "Unknown device kind" {:kind kind})))
            push-3-variation-pads? (and (= kind :push-3) (true? (:cue-variation-pads? options)))
            x-touch? (= :x-touch kind)
            ;; Validates the cue channel bindings before any port opens.
            _ (when x-touch?
                (x-touch/validate-channels! (vec (:channels options [])))
                ;; A fresh connection only: a reconnect or restore keeps its session,
                ;; and a strip that lost its range shows !range instead.
                (when-not (or restore-lease reconnect-lease)
                  (x-touch/validate-channel-ranges! (:model @(:state runtime)) (vec (:channels options [])))))
            surface-content (when x-touch?
                              {:type :mixer-strips :effect-group-view effect-group-view
                               :cue-strips (vec (:channels options []))})
            options (cond-> (dissoc options ::restore-lease ::reconnect-lease)
                      ;; Push 2 keeps :knob-page for its button LED (tolglow.surface.push-2).
                      (not= :push-2 kind) (dissoc :knob-page)
                      knob-page (assoc :encoder-overlay knob-page)
                      ;; A selection the Push picks itself is published without waiting on the owner.
                      surface-grid? (assoc :surface-id id :publish! #(surface-command! runtime epoch id %))
                      ;; Capture runs outside the session lock, after the press (#671).
                      ;; Once #817 lands this becomes (capture-action! runtime :send {:handoff? true}).
                      (and (#{:push-2 :push-3} kind) (some? (:capture-button options)))
                      (assoc :capture! #(try (capture-action! runtime :capture {})
                                             (catch Exception error
                                               (timbre/warn "Push Capture refused" id (.getMessage error)))))
                      x-touch?
                      ;; The Tolglow mixer layout (#706 slice 2) reads strips, window and
                      ;; layout state from the committed surface link.
                      (assoc :surface-id id
                             :rows #(surface/strip-channels % id surface-content)
                             :placement #(surface/strip-placement % id)
                             :external-transport? (external-transport-fn runtime))
                      push-source (assoc :operator-gestures? true :target-search targets/search
                                         :launch-receipt cue-timing/receipt)
                      (or push-3-variation-pads? (and push-source (true? (:cue-variations? options))))
                      (assoc :parameter-editor variations/edit-parameter)
                      push-3-variation-pads? (assoc :pad-overlay push-variations/push-3-pad-overlay)
                      (and (= kind :push-2) (true? (:cue-variations? options)))
                      (assoc :pad-overlay push-variations/pad-overlay)
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
                                                       (let [payload (assoc command :expected-epoch epoch)
                                                             gesture? (and push-source (= push-source (:source command))
                                                                           (#{:gesture/begin :gesture/sample :gesture/end :gesture/cancel}
                                                                            (:op command)))
                                                             quantized? (or (= :cue/cancel-launch (:op command))
                                                                            (and (= :cue/launch (:op command))
                                                                                 (contains? command :grid)))
                                                             reply (if gesture?
                                                                     (submit! runtime :command
                                                                              (update payload :command-id #(str operator-id "/" %))
                                                                              {:operator-id operator-id ::push-source push-source})
                                                                     (submit! runtime payload))]
                                                         (deref reply (if quantized? 0 device-command-wait-ms)
                                                                (cond-> {:status :pending :command-id (:command-id command)}
                                                                  gesture? (assoc :completion reply)))))}))
            device (cond-> device
                     (or x-touch? surface-grid?) (assoc ::surface-id id)
                     push-3-variation-pads? (assoc ::ratio-pad-operator operator-id)
                     restore-lease (assoc ::restore-lease restore-lease))]
        (if (locking (:lock runtime)
              (when (and @(:running runtime) (= epoch (get-in @(:state runtime) [:model :epoch]))
                         (or (nil? reconnect-lease)
                             (identical? reconnect-lease (get @(:device-reconnects runtime) id))))
                (swap! (:devices runtime) assoc id device)
                (swap! (:device-reconnects runtime) dissoc id)
                true))
          (do (when x-touch?
                (surface-command! runtime epoch id
                                  {:op :control-surface/attach :surface-id id
                                   :descriptor (if (:extender? options) :behringer/x-touch-ext :behringer/x-touch)
                                   :role (if (:extender? options) :extender :main)
                                   :viewports {:strips surface-content}}))
              (when push-3-grid?
                (surface-command! runtime epoch id
                                  {:op :control-surface/attach :surface-id id :descriptor :ableton/push-3
                                   :role :main :viewports {:grid (push-3-surface/content options)}}))
              (when push-2-grid?
                (surface-command! runtime epoch id
                                  {:op :control-surface/attach :surface-id id :descriptor push-2-surface/descriptor-id
                                   :role :main :viewports {:grid (push-2-surface/content options)}}))
              device)
          (do (devices/close! device)
              (throw (ex-info "Show stopped or changed while connecting device" {}))))))))

(defn disconnect-device! [runtime id]
  (locking (:devices runtime)
    (when-let [device (locking (:lock runtime)
                        (swap! (:device-reconnects runtime) dissoc id)
                        (get (first (swap-vals! (:devices runtime) dissoc id)) id))]
      (devices/close! device)
      (when (and (::surface-id device) @(:running runtime))
        (surface-command! runtime (get-in @(:state runtime) [:model :epoch]) id
                          {:op :control-surface/detach :surface-id id})))))

(defn link-surfaces!
  "Join attached surfaces into one logical surface with one window. Members
  are surface IDs or {:surface-id :viewport :offset} maps; each uses its first
  viewport unless :viewport is given. Modes:
  {:mode :extend :direction :right} (the default) packs each next surface
  beside the previous one by its actual width (:left, :down, :up too);
  {:mode :mirror} shows the leader's window on every member;
  {:mode :follow :offset {:x 1}} keeps each follower that many pages after the
  leader, which still pages by its own width. Returns the receipt promise.

    @(link-surfaces! runtime \"desk\" [\"xt-main\" \"xt-ext\"])
    @(link-surfaces! runtime \"twins\" [\"xt-main\" \"xt-ext\"] {:mode :mirror})"
  ([runtime link-id surface-ids] (link-surfaces! runtime link-id surface-ids {}))
  ([runtime link-id surface-ids options]
   (submit! runtime (merge {:op :control-surface/link :link-id link-id
                            :source {:adapter :repl :session-id "repl"}}
                           (surface/link-command surface-ids options)))))

(defn unlink-surfaces!
  "Dissolve a link; each member keeps the window it was showing."
  [runtime link-id]
  (submit! runtime {:op :control-surface/unlink :link-id link-id :source {:adapter :repl :session-id "repl"}}))

(defn bank-surface!
  "Move a surface's or link's window: a move keyword (:previous :next
  :channel-previous :channel-next :first :last :up :down) or a map with :to
  {:x :y} or :reveal cell-key. Returns the receipt promise."
  [runtime target move]
  (submit! runtime (merge {:op :control-surface/bank :target target :source {:adapter :repl :session-id "repl"}}
                          (if (keyword? move) {:move move} move))))

(defn surface-layout!
  "Change a surface's or link's committed layout state: a map with
  :encoder-mode (:pan :send :plug-in :user), :page, :selected (a cell key such
  as [:cue :ember] or [:group [:manual :mix]], or nil) and :flip?. The
  X-Touch buttons commit the same command. Returns the receipt promise.

    @(surface-layout! runtime \"desk\" {:encoder-mode :plug-in :selected [:cue :ember]})"
  [runtime target patch]
  (submit! runtime {:op :control-surface/layout :target target :set patch
                    :source {:adapter :repl :session-id "repl"}}))

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
                                       model (merge (surface/detach-all (fog-scan/disarm model))
                                                    (connections/export-state (:connections runtime) model))]
                                   [(cond-> (fog-deadline/save-ids model output)
                                      atmosphere (assoc-in venue-fog/saved-path atmosphere))
                                    output keyframes]))]
    (format-show (effect-keyframes/for-save model windows) (render/portable-frame output))))

(defn export-show
  "Freeze the full domain state and last output from one committed snapshot."
  [runtime]
  (capture-show runtime persistence/freeze))

(defn export-show-source [runtime] (capture-show runtime persistence/write-string))

(defn- capture-checkpoint [runtime]
  (capture-show runtime (fn [model output]
                          (assoc (select-keys model [:show-id :epoch :revision :frame-number])
                                 :source (persistence/write-string model output)))))

(defn- publish-checkpoint! [runtime captured replace!]
  ;; Prepared files are closed and forced. Only the final same-directory moves
  ;; must be ordered with a change of show ownership or shutdown.
  (locking (:lock runtime)
    (when (and @(:running runtime)
               (= (show-identity captured) (show-identity (:model @(:state runtime)))))
      (replace!))))

(defn recovery-status
  "Report available checkpoints for the current show, including after restart."
  [runtime]
  (when-let [owner (some-> (:recovery runtime) deref)]
    (let [{:keys [show-id] :as identity} (show-identity (:model @(:state runtime)))
          status (recovery/status owner)
          checkpoints (into {} (keep (fn [kind]
                                       (when-let [file (recovery/latest-file owner show-id kind)]
                                         [kind {:modified-at-ms (.lastModified ^java.io.File file)}])))
                            (into [:recovery :named :prior] (keys recovery/legacy-kinds)))]
      (cond-> (assoc (merge status identity) :runtime-id (:id runtime)
                     :checkpoints checkpoints :available (set (keys checkpoints)))
        (not= show-id (get-in status [:last-saved :show-id])) (dissoc :last-saved)
        (:show-folder runtime) (assoc :folder (:show-folder runtime))))))

(defn save-show!
  "Save a complete snapshot using a forced temporary file and atomic replacement."
  [runtime path]
  (let [owner (some-> (:recovery runtime) deref)
        path (if owner (recovery/manual-path! owner path) path)
        {:keys [show-id source]} (capture-checkpoint runtime)
        saved (recovery/write-source! path source)]
    (when owner
      (recovery/remember-manual! owner show-id saved))
    saved))

(defn- read-checkpoint-source [path] (recovery/read-source path))

(defn load-show!
  "Read a bounded full show file and enqueue its atomic restore; returns a receipt."
  [runtime path]
  (submit! runtime :import {:source (read-checkpoint-source path)}))

(defn- capture-checkpoint-identity [runtime expected]
  (let [identity (show-identity (:model @(:state runtime)))]
    (when (and expected (not= (assoc identity :runtime-id (:id runtime)) expected))
      (throw (ex-info "Checkpoint operation belongs to a previous show session" {:code :stale-checkpoint-session})))
    identity))

(defn- legacy-details [runtime]
  (let [{:keys [id path]} (:show-folder runtime)]
    (cond-> {} id (assoc :folder-id id) path (assoc :folder path))))

(def ^:private legacy-kinds (set (keys recovery/legacy-kinds)))

(defn- import-checkpoint! [runtime show-id epoch source receipt-id]
  (enqueue! runtime :import {:runtime-id (:id runtime) :expected-epoch epoch
                             :expected-source-show-id show-id :source source}
            nil
            (when receipt-id {:id receipt-id :identity {:runtime-id (:id runtime) :show-id show-id :epoch epoch}})))

(defn- load-legacy!
  "Restore a checkpoint from before the update while holding it, so no other
  window can apply or dismiss it meanwhile. Once the restore is accepted the
  SHA-256 of the restored bytes is recorded and no folder offers it again; a
  refused restore releases it, still offered. The reply carries
  :legacy-resolution-error when the record could not be written."
  [runtime owner show-id epoch kind receipt-id]
  (let [reservation (recovery/reserve-legacy! owner show-id)]
    (try
      (let [^java.io.File file (recovery/latest-file owner show-id kind)
            source (some-> file read-checkpoint-source)]
        (when-not source
          (throw (ex-info "Show checkpoint is unavailable" {:kind kind :show-id show-id})))
        (let [reply (import-checkpoint! runtime show-id epoch source receipt-id)
              details (assoc (legacy-details runtime) :kind kind :file (.getName file)
                             :sha256 (recovery/source-hash source))
              result (promise)]
          (future
            (let [ack @reply]
              (deliver result
                       (if (= :accepted (:status ack))
                         (try (recovery/finish-legacy! owner reservation :applied details)
                              ack
                              (catch Exception error
                                (assoc ack :legacy-resolution-error (ex-message error))))
                         (do ((:release! reservation)) ack)))))
          result))
      (catch Throwable error
        ((:release! reservation))
        (throw error)))))

(defn load-latest-show!
  "Load the active show's latest :recovery, :named, or prior-session checkpoint.
  In a Mac show folder, :legacy and :legacy-prior load the working or prior
  checkpoint frozen from before per-folder recovery and, once the restore is
  accepted, stop offering both to every folder (their files stay). Another
  window applying or dismissing them at the same time is refused. A remote
  caller can supply the exact runtime/show/epoch it received in status and a
  receipt ID for the committed result stream."
  ([runtime kind] (load-latest-show! runtime kind nil))
  ([runtime kind expected] (load-latest-show! runtime kind expected nil))
  ([runtime kind expected receipt-id]
   (let [{:keys [show-id epoch]} (capture-checkpoint-identity runtime expected)
         owner (some-> (:recovery runtime) deref)]
     (if (and owner (contains? legacy-kinds kind))
       (load-legacy! runtime owner show-id epoch kind receipt-id)
       (let [source (when owner
                      (if (= :prior kind)
                        (recovery/read-prior owner show-id read-checkpoint-source)
                        (some-> (recovery/latest-file owner show-id kind) read-checkpoint-source)))]
         (when-not source (throw (ex-info "Show checkpoint is unavailable" {:kind kind :show-id show-id})))
         (import-checkpoint! runtime show-id epoch source receipt-id))))))

(defn dismiss-legacy-recovery!
  "Stop offering the checkpoints from before per-folder recovery to this and
  every other Mac show folder, without applying them. Their files stay.
  Returns true when they were offered and are now dismissed, false when none
  was offered or they were already resolved. Refused on a stopped show and
  while another window is restoring them."
  ([runtime] (dismiss-legacy-recovery! runtime nil))
  ([runtime expected]
   (let [{:keys [show-id]} (capture-checkpoint-identity runtime expected)]
     (when-not @(:running runtime)
       (throw (ex-info "Show stopped" {:code :show-stopped})))
     (boolean
       (when-let [owner (some-> (:recovery runtime) deref)]
         (recovery/dismiss-legacy! owner show-id (legacy-details runtime)))))))

(defn discard-prior-recovery!
  "Dismiss the active show's prior-session checkpoint. Working and named saves
  keep their files. Returns true when the dismissal was published."
  ([runtime] (discard-prior-recovery! runtime nil))
  ([runtime expected]
   (let [captured (capture-checkpoint-identity runtime expected)]
     (boolean
       (when-let [owner (some-> (:recovery runtime) deref)]
         (recovery/discard-prior! owner (:show-id captured) (partial publish-checkpoint! runtime captured)))))))

(defn activate-recovery!
  "Admit background checkpoints once the configured startup has settled."
  [runtime]
  (some-> (:recovery runtime) deref recovery/activate!))

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
        :connect (do (require-devices! runtime :rdm-connect)
                     (rdm/connect! (:rdm runtime) (:config payload {}) identity))
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
      ;; Read-only aim calibration against the committed rig plus draft
      ;; overrides (#226). Live test aims use :surface-aim.
      :aim-check (let [rig (get-in @(:state runtime) [:model :rig])]
                   (aim-calibration/check (aim-calibration/fixture rig payload) (:point payload)))
      :aim-fit (let [rig (get-in @(:state runtime) [:model :rig])]
                 (aim-calibration/fit (aim-calibration/fixture rig payload) (:samples payload) (:axis-verified? payload)))
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
                    (fn [_] (require-devices! runtime :audio-return-start)
                      (duplex/start! (:audio-returns runtime) (:model @(:state runtime))
                                           #(get @(:state runtime) :model) #(now-ms runtime)
                                           (:open-audio-returns! runtime))))
    :return-stop (let [manager (:audio-returns runtime)
                       id (session-operation! runtime payload (fn [_] (:session (duplex/status manager))))]
                   (duplex/stop! manager id))
    :start (session-operation! runtime payload
                               (fn [config]
                                 (require-devices! runtime :audio-start)
                                 (connections/start-audio! (:connections runtime) config (audio-callbacks runtime))
                                 (audio/status (connections/audio (:connections runtime)))))
    :stop (let [shutdown! (session-operation! runtime payload
                             (fn [_] (connections/admit-audio-stop! (:connections runtime))))]
            (shutdown!))
    (throw (ex-info "Unknown audio operation" {:operation operation}))))

(defn- capture-summary [{:keys [id phrase layout]}]
  (let [bpb (get-in phrase [:meter :beats-per-bar] 4)]
    {:capture-id id :length-beats (double (:length-beats phrase)) :bars (long (/ (:length-beats phrase) bpb))
     :start-beat (some-> (get-in phrase [:source :start-beat]) double)
     :notes (count (:notes phrase)) :cues (vec (distinct (map :cue-id (:notes phrase))))
     :lane-points (reduce + 0 (map (comp count :points) (:lanes phrase)))
     :unassigned (:unassigned layout)}))

(defn capture-action!
  "Retrospective Capture (#671) for the REPL and Studio. The runtime folds
  every committed owner cycle into a bounded buffer (create-runtime option
  :phrase-capture, default 8 bars; false disables).
    :status  -> buffer size, running activations, per-cycle cost and last capture
    :capture -> capture the last bars ({:bars n :end-beat b} optional) as a
                phrase, extend the saved MIDI layout and keep it as the last capture
    :smf     -> {:filename :bytes} Standard MIDI File of the last capture on the
                layout's cue channel (cue notes and lanes share it by default)
  The layout is kept for the runtime's lifetime, so later captures address
  the same pitches and controllers; it is not yet saved with the show."
  [runtime operation payload]
  (settle-observers! runtime)
  (let [state (:phrase-capture runtime)]
    (when-not (and state (:enabled? @state))
      (throw (ex-info "Phrase capture is disabled for this show" {:operation operation})))
    (case operation
      :status (let [{:keys [buffer cost captures last]} @state
                    n (max 1 (:samples cost))]
                {:enabled? true :bars (:bars buffer) :grid-beats (double (:grid-beats buffer))
                 :retained-events (count (:events buffer)) :running (count (:open buffer))
                 :latest-beat (:last-beat buffer)
                 :cost {:cycles (:samples cost) :mean-us (/ (:total-ns cost) n 1000.0)
                        :max-us (/ (:max-ns cost) 1000.0) :last-us (/ (:last-ns cost) 1000.0)}
                 :captures captures :last (some-> last capture-summary)})
      :capture (let [{:keys [buffer layout captures]} @state
                     {:keys [bars end-beat] :as options} (select-keys payload [:bars :end-beat])
                     _ (when-not (and (or (nil? bars) (and (pos-int? bars) (<= bars (:bars buffer))))
                                      (or (nil? end-beat) (and (number? end-beat) (Double/isFinite (double end-beat))
                                                               (not (neg? end-beat)))))
                         (throw (ex-info (str "Capture takes 1-" (:bars buffer) " bars and a non-negative end beat")
                                         {:bars bars :end-beat end-beat})))
                     phrase (phrase-capture/capture buffer options)
                     registry (:registry (:model @(:state runtime)))
                     layout (phrase-clip/allocate-layout layout registry (phrase-clip/phrase-targets phrase))
                     entry {:id (inc captures) :phrase phrase :layout layout}]
                 (if (empty? (:notes phrase))
                   {:status :empty :message "Nothing ran in the captured bars"}
                   (do (swap! state assoc :layout layout :captures (inc captures) :last entry)
                       (assoc (capture-summary entry) :status :captured :phrase phrase))))
      :smf (let [{:keys [last]} @state]
             (when-not last (throw (ex-info "Capture a phrase first" {})))
             (let [{:keys [id phrase layout]} last
                   clips (phrase-clip/phrase->clips phrase layout)
                   channel (:channel layout)]
               {:filename (str "tolglow-capture-" id ".mid")
                :bytes (phrase-clip/clip->smf (get clips channel) channel
                                              {:bpm (or (get-in phrase [:source :bpm]) 120)})}))
      (throw (ex-info "Unknown capture operation" {:operation operation})))))

(defn capture-phrase!
  "Capture the last bars from the REPL; returns the summary with :phrase.
  Studio's Capture button runs the same operation."
  ([runtime] (capture-phrase! runtime {}))
  ([runtime options] (capture-action! runtime :capture options)))

(defn- live-runs-state [runtime operation]
  (settle-observers! runtime)
  (let [state (:live-runs runtime)]
    (when-not (and state (:enabled? @state))
      (throw (ex-info "The run record is disabled for this show" {:operation operation})))
    state))

(declare sync-live-runs! live-parameter-devices)

(defn live-runs-action!
  "The run record (#683) for the REPL and Studio. The runtime folds every
  committed owner cycle into it (create-runtime option :live-runs; false
  disables): one channel per launched cue, one take per activation, lanes for
  overlapping runs and the manual effect-group tree.
    :status -> live-runs/projection with the last Live sync, plus fold cost;
               payload {:limit n} lists the newest n takes (default 256)
    :parameter-devices -> live-parameter-devices (#625)
    :sync   -> sync-live-runs! over the running Live bridge, as Studio's
               Sync to Live runs it; payload {:groups? false} skips groups
  From the REPL, sync-live-runs! also takes any companion connection."
  [runtime operation payload]
  (let [state (live-runs-state runtime operation)]
    (case operation
      :status (let [{:keys [record mirror cost]} @state
                    n (max 1 (:samples cost))
                    limit (some-> (:limit payload) str parse-long)]
                (when (and (:limit payload) (not (and limit (<= 1 limit 4096))))
                  (throw (ex-info "limit takes 1-4096" {:limit (:limit payload)})))
                (assoc (live-runs/projection record mirror (cond-> {} limit (assoc :limit limit)))
                       :playback-events {:queued (count (:inbox @state)) :dropped (:inbox-dropped @state 0)}
                       :cost {:cycles (:samples cost) :mean-us (/ (:total-ns cost) n 1000.0)
                              :max-us (/ (:max-ns cost) 1000.0)}))
      :parameter-devices (live-parameter-devices runtime)
      ;; Studio's Sync to Live: the same sync-live-runs! over the Live bridge.
      :sync (-> (sync-live-runs! runtime (live-bridge-connection runtime) (select-keys payload [:groups?]))
                (dissoc :set)
                (update :tracks #(mapv (fn [[[_ cue-id lane] track-id]] {:cue-id cue-id :lane lane :track-id track-id}) %))
                (update :groups #(mapv (fn [[[_ group-id] track-id]] {:group-id group-id :track-id track-id}) %))
                (update :parameter-devices #(mapv (fn [[[cue-id lane] st]] (assoc (dissoc st :devices) :cue-id cue-id :lane lane)) %)))
      (throw (ex-info "Unknown run record operation" {:operation operation})))))

(defn live-runs
  "What ran, as Studio's Live runs panel shows it (see live-runs-action!)."
  [runtime]
  (live-runs-action! runtime :status {}))

(defn live-runs-record
  "The run record itself: exact beats, every take's launch values and moves."
  [runtime]
  (:record @(live-runs-state runtime :record)))

(declare live-bridge-connection)

(defn sync-live-runs!
  "Mirror the run record into a Live Set over a companion connection, keep
  the result for the projection and return it. With no connection it uses the
  running Live bridge (live-bridge-connection); a connection with its own
  session would take the companion's callbacks from a running bridge.
  options as live-runs/sync! (:groups?, :arrangement-offset-beats)."
  ([runtime] (sync-live-runs! runtime (live-bridge-connection runtime) {}))
  ([runtime conn] (sync-live-runs! runtime conn {}))
  ([runtime conn options]
   (let [state (live-runs-state runtime :sync)
         model (:model @(:state runtime))
         catalog (device-catalog model)
         ;; Parameter devices (#625): each channel's slot layout, seeded on its
         ;; first sync from the slot names its devices show in Live, so a
         ;; restarted show keeps recorded automation on its parameter.
         unseeded? (some (complement :seeded?) (vals (get-in (device-layouts @state model) [:devices :layouts])))
         before (when unseeded?
                  (let [r (live-connection/request! conn :inspect-set {})]
                    (when (= :executed (:status r)) (:result r))))
         devices (locking (:player-lock @state)
                   (let [s (device-layouts @state model)
                         record (:record s)
                         s (cond-> s
                             before (update :devices live-devices/seed-from-set model before live-runs/lane-track-name
                                            (into {} (map (fn [[cue-id ch]] [cue-id (:lanes ch)])) (:channels record))
                                            catalog))]
                     (swap! state assoc :devices (:devices s) :device-layout-key (:device-layout-key s))
                     (:devices s)))
         specs (live-devices/specs devices model catalog)
         envelopes (fn [take spec]
                     (live-devices/take-envelopes model spec (get-in devices [:layouts (:cue-id take)]) take
                                                  (live-runs/held-beats take) catalog))
         result (live-runs/sync! conn (:record @state)
                                 (merge {:parameter-devices {:specs specs :envelopes envelopes}} (dissoc options :watch?)))
         ;; Watch the bound run tracks: clip starts and stops, and every
         ;; parameter-device slot on them. Off with {:watch? false}.
         track-ids (vec (sort (distinct (vals (:tracks result)))))
         watch (when (and (not (false? (:watch? options))) (seq track-ids))
                 (let [r (live-connection/request! conn :watch-run-tracks {:track-ids track-ids})]
                   (cond-> (select-keys r [:status :result]) (:code r) (assoc :code (:code r)))))
         mirror (cond-> (dissoc result :set) watch (assoc :watch watch))]
     (locking (:player-lock @state)
       (swap! state (fn [s] (assoc s :mirror mirror :device-specs specs
                                   :player (if-let [p (:player s)] (live-runs/with-mirror p mirror) (live-runs/player mirror))))))
     (cond-> result watch (assoc :watch watch)))))

(defn parameter-frames-action!
  "Shared parameter-visualization frames (#625) for Studio, the Push display,
  a Live parameter device and the REPL. Views of the same instance,
  parameter, beat window and style at sizes within 10% share one render.
    :subscribe   {:instance-id :param :window-beats :style :width :height
                  :dpr :consumer :queue} -> {:lease :key :render :shared?}
    :frame       {:lease} -> the oldest waiting frame (removed), else nil
    :latest      {:lease} -> the key's newest frame, without reading a mailbox
    :unsubscribe {:lease} -> nil; the last view of a key stops its rendering
    :status      {} -> renders, presentations, drops, cost and views per key
  Rendering runs on its own worker thread from committed snapshots, never on
  the show owner. A lease not read for five seconds expires."
  [runtime operation payload]
  (let [service @(:parameter-frames runtime)]
    (case operation
      :subscribe (parameter-frames/subscribe! service (dissoc payload :consumer :queue)
                                              {:id (or (:consumer payload) "view") :queue (or (:queue payload) 1)})
      :frame (parameter-frames/take-frame! service (:lease payload))
      :lease? (parameter-frames/lease? service (:lease payload))
      :latest (parameter-frames/latest-frame service (:lease payload))
      :unsubscribe (parameter-frames/unsubscribe! service (:lease payload))
      :status (parameter-frames/status service)
      (throw (ex-info "Unknown parameter frame operation" {:operation operation})))))

(defn live-parameter-devices
  "Each run channel's parameter-device spec (#625): slot names, ranges,
  units, choices, Push banks and unsupported parameters, where the devices
  are on the last synced Live tracks, and the ownership counters (applied,
  echo, held, no-instance; Tolglow writes mirrored and sent). Studio's
  Connections -> Live runs panel shows the same data
  (GET /api/live-parameter-devices). sync-live-runs! configures the devices
  and writes run clips with envelopes."
  [runtime]
  (let [state (live-runs-state runtime :parameter-devices)
        model (:model @(:state runtime))
        s (locking (:player-lock @state)
            (let [s (device-layouts @state model)]
              (swap! state assoc :devices (:devices s) :device-layout-key (:device-layout-key s))
              s))
        specs (live-devices/specs (:devices s) model (device-catalog model))
        status (get-in s [:mirror :parameter-devices] {})
        tracks (get-in s [:mirror :tracks] {})
        by-cue (reduce-kv (fn [m [cue-id lane] st]
                            (update m cue-id (fnil conj [])
                                    (merge {:lane lane :track-id (get tracks [:lane cue-id lane])
                                            :track-name (live-runs/lane-track-name cue-id lane)}
                                           (select-keys st [:present :missing :stale]))))
                          {} status)
        n (max 1 (get-in s [:device-cost :samples]))]
    (assoc (live-devices/projection (:devices s) specs by-cue)
           :inbox {:queued (count (:device-inbox s)) :dropped (:device-inbox-dropped s)}
           :sends (:device-sends s)
           :watch (get-in s [:mirror :watch])
           :cost {:cycles (get-in s [:device-cost :samples]) :mean-us (/ (get-in s [:device-cost :total-ns]) n 1000.0)
                  :max-us (/ (get-in s [:device-cost :max-ns]) 1000.0)
                  ;; Work, not time: Live values decoded in all and in the busiest
                  ;; cycle (never above the per-cycle cap).
                  :values (get-in s [:device-cost :values] 0) :max-values (get-in s [:device-cost :max-values] 0)
                  :values-per-cycle-cap device-events-per-cycle})))

(defn- wire-name [x] (if (keyword? x) (subs (str x) 1) (str x)))

(defn- launcher-page
  "The cue page a Live bridge maps: page-id when given, otherwise the first
  page by ID, as sync-action! :configure chooses it."
  [model page-id]
  (let [pages (sort-by (comp str :id) (vals (:pages model)))]
    (if (some? page-id)
      (or (some #(when (= (wire-name page-id) (wire-name (:id %))) %) pages)
          (throw (ex-info "No such cue page" {:page-id page-id :http/status 404})))
      (first pages))))

(defn- launcher-for [runtime page-id]
  (let [model (:model @(:state runtime))
        live (get-in (sync/export-state (connections/sync (:connections runtime))) [:config :live])
        page (launcher-page model page-id)]
    (when-not page (throw (ex-info "This show has no cue page" {:http/status 404})))
    (live-launcher/pads (ableton/bind-registry-ids live (:registry model)) (:registry model) page)))

(defn live-bridge-connection
  "A live-connection over the runtime's running Live bridge: operations go to
  the bridge's companion under the bridge's own sync session, in order with
  its polls, so they never take the companion's callbacks from the bridge.
  Refused as :not-connected until the bridge is connected (sync-action!
  :start, or Studio's Connect enabled adapters)."
  [runtime]
  (live-connection/open (live-sync/companion-transport (connections/sync (:connections runtime)))
                        {:session-id (str "tolglow-" (subs (str (UUID/randomUUID)) 0 8))}))

(declare live-launcher-setup! live-launcher-write-clip!)

(defn live-launcher-action!
  "The cue launcher (#228) for the REPL and Studio: a cue page played from one
  Drum Rack track in Push's 64 Pads layout.
    :status     {:page-id id} -> live-launcher/pads for that page (default the
                first page, as the Live bridge maps it) under the bridge's saved
                note mappings and :pad-layout, with :setup (the last Live setup)
                and :clip (the last pad clip written).
    :setup      {:page-id id} -> live-launcher/setup! on the Live bridge's
                companion: name its track after the page, insert a Drum Rack
                after the companion, name and colour one chain per mapped pad,
                read it back and verify it (input routing included).
    :write-clip {:page-id :from-beat :to-beat :name :slot-index :looping?} ->
                the run record as a clip of pad hits on the launcher track.
  :setup and :write-clip go through the running Live bridge (see
  live-bridge-connection), so connect it first; Studio's Cue launcher panel
  and the REPL call the same operations."
  [runtime operation payload]
  (case operation
    :status (let [launcher (launcher-for runtime (:page-id payload))
                  {:keys [setup clip]} @(:live-launcher runtime)]
              (cond-> launcher
                (= (wire-name (:page-id launcher)) (wire-name (:page-id setup))) (assoc :setup (dissoc setup :page-id))
                (= (wire-name (:page-id launcher)) (wire-name (:page-id clip))) (assoc :clip (dissoc clip :page-id))))
    :setup (live-launcher-setup! runtime (live-bridge-connection runtime) payload)
    :write-clip (live-launcher-write-clip! runtime (live-bridge-connection runtime) payload)
    (throw (ex-info "Unknown launcher operation" {:operation operation :http/status 404}))))

(defn live-launcher
  "The 64 pads of a cue page as Studio's Cue launcher panel shows them (see
  live-launcher-action!)."
  ([runtime] (live-launcher runtime nil))
  ([runtime page-id] (live-launcher-action! runtime :status {:page-id page-id})))

(defn- live-launcher-setup! [runtime conn {:keys [page-id]}]
  (let [launcher (launcher-for runtime page-id)]
    ;; One setup or clip write at a time: Studio and the REPL share the record.
    (locking (:live-launcher runtime)
      (let [live (get-in (sync/export-state (connections/sync (:connections runtime))) [:config :live])
            result (assoc (live-launcher/setup! conn (live-launcher/desired launcher (select-keys live [:feed-port])))
                          :page-id (:page-id launcher))]
        (swap! (:live-launcher runtime) assoc :setup result)
        result))))

(defn- live-launcher-write-clip! [runtime conn options]
  (let [launcher (launcher-for runtime (:page-id options))]
    (locking (:live-launcher runtime)
      (let [setup (:setup @(:live-launcher runtime))
            _ (when-not (and setup (= (wire-name (:page-id setup)) (wire-name (:page-id launcher))) (:ready? setup))
                (throw (ex-info "Set the launcher up for this page first"
                                {:page-id (:page-id launcher) :problems (:problems setup) :http/status 409})))
            bpb (get-in @(:state runtime) [:model :transport :meter :beats-per-bar] 4)
            clip (live-launcher/pad-clip (live-runs-record runtime) launcher
                                         (merge {:beats-per-bar bpb} (select-keys options [:from-beat :to-beat])))
            receipt (when (:clip clip) (live-launcher/write-clip! conn setup (:clip clip) options))
            result (cond-> (-> (dissoc clip :clip)
                               (assoc :page-id (:page-id launcher) :notes (count (get-in clip [:clip :notes]))
                                      :length-beats (get-in clip [:clip :length-beats])))
                     receipt (assoc :status (:status receipt) :clip-id (get-in receipt [:result :clip-id])
                                    :slot-index (get-in receipt [:result :slot-index])
                                    :error (or (:error receipt) (when (:code receipt) {:code (:code receipt)})))
                     (nil? receipt) (assoc :status :nothing-to-write))]
        (swap! (:live-launcher runtime) assoc :clip result)
        result))))

(defn setup-live-launcher!
  "Set the cue launcher up in Live through the running Live bridge (the same
  operation as Studio's Set up in Live). Keeps the result for live-launcher
  and returns it. options: {:page-id id}."
  ([runtime] (setup-live-launcher! runtime {}))
  ([runtime options] (live-launcher-action! runtime :setup options)))

(defn write-live-launcher-clip!
  "Write the runtime's run record as a clip of pad hits on the launcher track,
  through the running Live bridge (the same operation as Studio's Write pad
  clip). options: :page-id, :from-beat/:to-beat (show-transport beats;
  default the whole bars around the takes), :name, :slot-index, :looping?.
  Returns the clip ID and the takes it holds and skipped."
  ([runtime] (write-live-launcher-clip! runtime {}))
  ([runtime options] (live-launcher-action! runtime :write-clip options)))

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
      :start (do (require-devices! runtime :sync-start)
                 (connections/start-sync! (:connections runtime) (sync-callbacks runtime))
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
        (when (= :stage-start operation) (require-devices! runtime :sensor-stage-start))
        (if (= :stage-start operation)
          (sensors/start-stage! current payload)
          (audience-stage/stop-current! (:stage current) (:stage-id payload))))
      :stage-qr (audience-stage/png (:stage manager) (:id payload))
      :osc-start
      (let [manager (session-operation! runtime payload (fn [_] (require-devices! runtime :sensor-osc-start)
                                                          (connections/sensors (:connections runtime))))]
        (sensors/start-osc! manager (select-keys payload [:host :port]))
        (:udp (sensors/status manager)))
      :osc-stop
      (let [manager (session-operation! runtime payload (fn [_] (connections/sensors (:connections runtime))))]
        (sensors/stop-osc! manager))
      (throw (ex-info "Unknown sensor operation" {:operation operation})))))

(defn projector-gain
  "Light gain a projector output follows: what the fixtures rendered with in
  the last committed frame (0 in blackout, else the effective master, which
  includes master ramps and lighting-capacity limits). Before the first frame,
  the committed blackout and master."
  [state]
  (or (:projector-gain state)
      (let [model (:model state)]
        (if (pause-look/blackout? model) 0.0 (double (:master model 1.0))))))

(defn- known-video-source? [model id]
  (or (contains? (get-in model [:settings :video :sources]) id)
      (some #(and (video-shader/shader? %) (= id (video-shader/feed-id %)))
            (vals (get-in model [:registry :effects])))))

(defn- next-video-frame!
  "Wait up to :wait-ms for a latched frame of :source other than sequence
  :after (or for a different projector gain than :gain-after). Returns
  {:status :frame :frame :gain :pixels}, :none after the wait, :unknown for a
  source the show does not have, or :busy past the show's :max-video-waiters
  (default `default-video-waiters`). Waits on
  the frame signal, never polls. :follow-master? false gives the raw feed."
  [runtime {:keys [source after gain-after wait-ms follow-master?] :or {after -1 wait-ms 1000 follow-master? true}}]
  (let [^java.util.concurrent.atomic.AtomicInteger waiters (:video-waiters runtime)
        signal (:video-frame-signal runtime)
        current (fn []
                  (let [{:keys [model video-latch] :as state} @(:state runtime)]
                    [model (get-in video-latch [:frames source])
                     (if follow-master? (projector-gain state) 1.0)]))
        fresh? (fn [f gain] (and f (or (not= (long (:sequence f 0)) (long after))
                                       (and gain-after (not= (double gain) (double gain-after))))))
        deadline (+ (System/nanoTime) (* 1000000 (long (max 0 (min 2000 wait-ms)))))
        [model f gain] (current)]
    (cond
      (not (or f (known-video-source? model source))) {:status :unknown}
      ;; Serving a frame counts as well as waiting for one: the cap bounds
      ;; both Jetty threads and frame bandwidth.
      (> (.incrementAndGet waiters) (long (:video-waiter-cap runtime default-video-waiters)))
      (do (.decrementAndGet waiters) {:status :busy})
      :else
      (try
        (if (fresh? f gain)
          {:status :frame :frame f :gain gain :pixels (video-source/scaled-pixels f gain)}
          (loop []
            (let [remaining (quot (- deadline (System/nanoTime)) 1000000)]
              (if (pos? remaining)
                (do (locking signal (.wait ^Object signal (long remaining)))
                    (let [[_ f gain] (current)]
                      (if (fresh? f gain)
                        {:status :frame :frame f :gain gain :pixels (video-source/scaled-pixels f gain)}
                        (recur))))
                {:status :none}))))
        (finally (.decrementAndGet waiters))))))

(defn video-action!
  "Video sources, strip mappings (#672) and 3D surfaces (#674) for Ring and the REPL.
  :status       source status, sampled strip colours from the latched frame and
                each surface with the heads in contact
  :edit         apply one `tolglow.video.mapping/apply-operation` edit through
                the show queue; sources reconcile after it commits.
                {:op :surface-cue :surface id} spawns and launches a cue
                projecting that surface (`tolglow.video.surface-cues/spawn`)
  :syphon       list Syphon servers through the native helper
  :frame-jpeg   bytes of a latched frame, {:source id :max-width px}
  :frame        the latched frame record of {:source id}, or nil
  :next-frame   what a projector page draws next: wait for a newer latched
                frame of {:source id :after seq}, scaled by blackout and grand
                master unless {:follow-master? false}; see next-video-frame!
  :shaders      shader producer status (#673): per instance cadence, frames,
                drops, timings and compile errors
  :audition     compile {:effect-id id :glsl text} (or a whole {:shader …}
                with {:params …}) on the GPU worker without publishing;
                returns {:compiled? …} with the driver log. Blocks the
                caller, never the lighting frame
  :save-glsl    publish {:effect-id :glsl :expected-version} as the effect's
                next version through the definition command; returns its
                receipt. Running instances reload it, and a version that
                does not compile keeps rendering the last working one"
  [runtime operation payload]
  (case operation
    :status (let [state @(:state runtime)]
              (video-preview/project (:video runtime) (:video-latch state) (get-in state [:model :rig :fixtures])
                                     (:video-preview-plans runtime) (:model state) (:video-surface-views runtime)
                                     (shader-producer/status (:shader-video runtime))))
    :frame (get-in @(:state runtime) [:video-latch :frames (:source payload)])
    :next-frame (next-video-frame! runtime payload)
    :shaders (shader-producer/status (:shader-video runtime))
    :audition (let [{:keys [effect-id shader]} payload
                    ;; Studio's projection carries ids as text.
                    effect-id (cond-> effect-id (string? effect-id) keyword)
                    effect (when effect-id (get-in @(:state runtime) [:model :registry :effects effect-id]))
                    shader (or shader (:shader effect))
                    ;; The cue's own parameters type the uniform expressions:
                    ;; the registered effect's when it names one.
                    errors (compiler/definition-errors :effects {:id :shader/audition :version 1
                                                                 :params (if effect (:params effect) (:params payload {}))
                                                                 :shader (cond-> shader (:glsl payload) (assoc :glsl (:glsl payload)))})]
                (if (seq errors)
                  {:compiled? false :message (:message (first errors)) :errors errors}
                  (shader-producer/audition! (:shader-video runtime)
                                             (video-shader/afterglow-shader (cond-> shader (:glsl payload) (assoc :glsl (:glsl payload)))))))
    :save-glsl (let [{:keys [effect-id glsl expected-version]} payload
                     effect-id (cond-> effect-id (string? effect-id) keyword)
                     effect (get-in @(:state runtime) [:model :registry :effects effect-id])]
                 (if-not (video-shader/shader? effect)
                   (doto (promise) (deliver {:status :rejected :message "No shader effect with that id"}))
                   ;; The next version through the ordinary definition command,
                   ;; guarded by the caller's session like every definition edit.
                   (submit! runtime :definition (cond-> (merge {:kind :effects
                                                                :definition (-> effect (assoc-in [:shader :glsl] glsl) (update :version inc))}
                                                               (select-keys payload [:runtime-id :expected-epoch]))
                                                  expected-version (assoc :expected-version expected-version)))))
    :edit (let [receipt (submit! runtime :video-mapping (dissoc payload :runtime-id :expected-epoch))]
            receipt)
    :syphon (video-hub/list-syphon (:video runtime))
    :frame-jpeg (video-preview/frame-jpeg (:video-latch @(:state runtime)) (:source payload) (:max-width payload 640))
    (throw (ex-info "Unknown video operation" {:operation operation}))))

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
  host may supply its own sink opener. Set :recovery false to disable the
  automatic working checkpoint, or supply :interval-ms and :directory."
  ([] (start! {}))
  ([options] (start! options output/open-sink!))
  ([{:keys [frame-rate defer-recovery?] :or {frame-rate 40} :as options} open-sink!]
   (when-not (and (number? frame-rate) (<= 1 frame-rate 120)) (throw (ex-info "Frame rate must be 1..120" {})))
   (let [recovery-options (recovery/options (:recovery options))
         sink (open-sink! options)
         runtime (try (create-runtime (merge {:adaptive-keyframes {:enabled? true} :background-compile true :background-observers true
                                              :prepare-launches true} options
                                             {:sink sink :independent-output? true}))
                      (catch Throwable error
                        (when sink
                          (try ((:close! sink)) (catch Throwable cleanup (.addSuppressed error cleanup))))
                        (throw error)))]
     (try
       (output-scheduler/start! (:publisher runtime) (/ 1000.0 frame-rate) #(render-slot! runtime %))
       (reset! (:recovery runtime)
               (recovery/start! recovery-options (not defer-recovery?)
                                #(when @(:running runtime) (capture-checkpoint runtime))
                                (partial publish-checkpoint! runtime)))
       ;; Find the shared checkpoint a Mac show folder offers before Studio asks.
       (recovery/refresh-legacy! @(:recovery runtime) (get-in @(:state runtime) [:model :show-id]))
       runtime
       (catch Throwable error (stop! runtime) (throw error))))))
