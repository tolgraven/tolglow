(ns tolglow.show
  "Executable host for the shared functional lighting engine. Requiring it is inert.
  One owner advances the model; workers return data; physical output is opt-in."
  (:require [afterglow.control.model :as control]
            [afterglow.control.categories :as categories]
            [afterglow.control.devices.expressive :as expressive-mapping]
            [afterglow.control.sensors :as sensor-core]
            [afterglow.runtime.engine :as engine]
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
            [clojure.java.io :as io]
            [tolglow.compiler :as compiler]
            [tolglow.compiler.batch :as batch]
            [tolglow.registry :as registry]
            [tolglow.integrations.ableton :as ableton]
            [tolglow.aim-paths :as aim-paths]
            [tolglow.rig.surfaces :as surfaces]
            [tolglow.rig.surface-aim :as surface-aim]
            [tolglow.modulation :as modulation]
            [tolglow.lighting-capacity :as capacity]
            [tolglow.clock :as show-clock]
            [tolglow.signals :as signals]
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
            [tolglow.persistence :as persistence]
            [tolglow.connections :as connections]
            [tolglow.repl.sessions :as repl-manager]
            [tolglow.development :as development]
            [tolglow.sensors :as sensors]
            [tolglow.sensors.osc :as sensor-osc]
            [tolglow.fixtures.library :as fixture-service]
            [tolglow.rig :as rig]
            [tolglow.fixtures.patch :as fixture-patch]
            [tolglow.rig.rdm :as rdm]
            [tolglow.rig.calibration :as calibration]
            [tolglow.rig.photometry :as photometry]
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
  (:import [java.util UUID]
           [java.util.concurrent LinkedBlockingQueue Executors ScheduledExecutorService TimeUnit]))

(defn- clock-time [clock]
  (let [{:keys [origin-nanos time-ms]} @clock]
    (+ time-ms (/ (- (System/nanoTime) origin-nanos) 1.0e6))))
(defn- now-ms [runtime] (clock-time (:clock runtime)))
(defn agent-manager [runtime] (connections/agents (:connections runtime)))
(defn reactions [runtime] (connections/reactive (:connections runtime)))
(defn repls [runtime] (connections/repls (:connections runtime)))
(defn- error-data [error] {:message (.getMessage ^Throwable error) :details (ex-data error)})

(defn project-snapshot [runtime {:keys [model output status modulation-view]}]
    (merge (select-keys model [:show-id :epoch :revision :frame-number :time-ms :master :blackout? :transport :rig :settings])
           {:runtime-id (:id runtime)
            :receiver-surfaces (surfaces/receivers (:rig model))
            :surface-aim (get-in model [:variables :surface-aim] {})
            :event-subscribers (select-keys (:event-subscribers model {:subscribers []}) [:subscribers :last-error :dropped])
            :waveform-revision (get-in model [:waveforms :revision] 0)
            :fixture-types (vec (sort-by (comp str :id) (vals (get-in model [:registry :fixtures]))))
            :category-colors registry/category-colors
            :effects (mapv #(assoc % :categories (registry/definition-categories %))
                            (sort-by (comp str :id) (vals (get-in model [:registry :effects]))))
            :cues (vec (sort-by (comp str :id) (vals (get-in model [:registry :cues]))))
            :groups (categories/group-view model)
            :instances (mapv #(-> % (dissoc :state)
                                  (assoc :categories (registry/definition-categories (:definition %)))
                                  (update :system (fn [s] (merge {:opacity 1.0 :paused? false}
                                                               (select-keys s [:opacity :paused? :attack :release])))))
                             (sort-by :order (vals (:instances model))))
            :pages (mapv (fn [page] (update page :cells
                                           #(mapv (fn [[[x y] cue]] {:x x :y y :cue-id cue})
                                                  (sort-by key %)))) (sort-by (comp str :id) (vals (:pages model))))
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
            :output {:values (let [fixtures (get-in model [:rig :fixtures])
                                   cached @(:patch runtime)]
                               (preview/values output fixtures
                                               (if (identical? fixtures (:fixtures cached))
                                                 (:brightness-gains cached)
                                                 (preview/brightness-gains fixtures))))
                     :atmosphere (:atmosphere output)
                     :fog-control (fog-control/view (:fog-control output))
                     :preview-only? true
                     :lighting-capacity (:lighting-capacity output)
                     :universes (vec (sort (keys (:universes output))))}
            :status (merge status {:running? @(:running runtime)
                                   :compute (compute/status (:compute runtime))
                                   :movement (compute/movement-status (:compute runtime) output)
                                   :effect-logs (effect-faults/status (:effect-logs runtime))
                                   :audio-returns (duplex/status (:audio-returns runtime))
                                   :output-scheduler (output-scheduler/status (:publisher runtime))
                                   :output (or (get-in runtime [:sink :description]) {:type :preview})})}))

(defn snapshot [runtime] (project-snapshot runtime @(:state runtime)))

(declare send-output! heartbeat! submit!)

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
                  (compute/expressions-enabled? compute-owner) (assoc :prepare-group compute/prepare-group))]
    {:fixtures fixtures :compiler (batch/compiler-for-rig fixtures options) :cache (atom {})}))

(defn- open-compute! [{:keys [compute-backend movement-backend batch? frame-rate gpu-budget-ms]
                       :or {batch? true frame-rate 40} :as options}]
  (when (and (= :native-gpu compute-backend) (not batch?))
    (throw (ex-info "Native GPU expressions require :batch? true" {})))
  (compute/open!
   (cond-> (select-keys options [:compute-backend :movement-backend :gpu-options])
     (or (= :native-gpu compute-backend) (= :native-gpu movement-backend))
     (assoc :budget-ms (or gpu-budget-ms (max 1 (long (/ 500.0 frame-rate))))))))

(defn- configure-runtime
  [compute-owner {:keys [model sink workers agent-options batch?] :or {workers 2 batch? true} :as options}]
   (let [model (photometry/disarm (calibration/disarm-restored (or model (demo/model))))
         _ (when-not (or (nil? (:effect-log! options)) (ifn? (:effect-log! options)))
             (throw (ex-info "Effect log sink must be callable" {})))
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
         owned (connections/create model (-> (select-keys options [:agent-options :fixture-options :sensor-options :credential-store :credential-path])
                                             (assoc :repl-options (assoc (:repl-options options {}) :runtime-supplier #(deref runtime-ref)))
                                             (assoc :audio-options (assoc (:audio-options options {}) :now-ms #(clock-time clock)))
                                             (assoc :sync-options (assoc (:sync-options options {}) :now-ms #(clock-time clock)))
                                             (assoc :reactive-options (merge {:trusted-source? true} (:reactive-options options)))))
         _ (when-not (:reactive model)
             (reactive-clock/register! (connections/reactive owned))
             (effect-faults/register! (connections/reactive owned)))
         worker-pool (try (executor/create {:workers workers :queue-size 64 :timeout-ms 500})
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
                      (cond-> {:send! (fn [universes frame-id] (send-output! @runtime-ref universes frame-id))}
                        (:send! sink) (assoc :receipt-capacity 256)
                        (:physical-now-ms options) (assoc :now-ms (:physical-now-ms options))))
           runtime-id (str (UUID/randomUUID))
           runtime {:id runtime-id
      :event-admit (get options :event-admit subscribers/admit)
      :event-callbacks (event-callbacks/create
                        (merge {:trusted-source? (get-in options [:reactive-options :trusted-source?] false)}
                               (:event-callback-options options)))
      :effect-logs (effect-faults/create {:log! (:effect-log! options)})
      :state (atom {:model model :output (render/resolve-frame patch [] {:include-values? (not batch?) :compact-values? batch?}) :status {}})
      :queue (LinkedBlockingQueue. 256) :lock (Object.) :running (atom true)
      :venue-fog (atom {}) :fog-deadlines (atom nil)
      :clock clock :sink sink :pause-layer-cache (volatile! nil)
      :observers (atom {}) :frame-rate (get options :frame-rate 40)
      :publisher publisher :independent-output? (boolean (:independent-output? options))
      :executor worker-pool
      :scheduler (atom nil) :leases (atom {})
      :audio-returns (duplex/create) :open-audio-returns! (get options :open-audio-returns! duplex/open-java!)
      :capacity-plan (volatile! nil) :capacity-audio (volatile! nil)
      :audio-input (atom {:epoch (:epoch model) :events [] :features nil})
      :sync-input (atom {:epoch (:epoch model) :events []})
      :connections owned :devices (atom {}) :development development
      :rdm (rdm/create (merge (:rdm-options options)
                             {:model! #(get @(:state @runtime-ref) :model)
                              :identity! #(let [runtime @runtime-ref]
                                            {:runtime-id (:id runtime) :expected-epoch (get-in @(:state runtime) [:model :epoch])})
                              :publish! #(submit! @runtime-ref :rdm-update (merge %1 %2))}))
      :response-cache (atom {:responses [] :middleware identity})
      :compute compute-owner :compiler-context compiler-context :batch? batch?
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
  :compute-backend :native-gpu acquires one session owned by this show."
  ([] (create-runtime {}))
  ([options]
   (let [owner (open-compute! options)]
     (try (configure-runtime owner options)
          (catch Throwable error
            (try (compute/close! owner) (catch Throwable cleanup (.addSuppressed error cleanup)))
            (throw error))))))

(defn- restart-model [model time-ms]
  (let [configs (mapv :config (remove :owner-instance-id (get-in model [:modulation :controllers])))
        clock (musical-time/frame-clock {:time-ms time-ms :transport (:transport model)})]
    (cond-> (-> model
                subscribers/restart
                (assoc :composition-runs {} :composition-tokens {} :modulation (modulation/new-state))
                (update :variables dissoc :audio :sync :surface-aim)
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
        ;; Declarative code may use string identities (generated pages do).
        ;; JSON keyword normalization must still address those existing objects.
        payload (reduce (fn [p [field entries]]
                          (let [id (get p field) s (when (keyword? id) (subs (str id) 1))]
                            (if (and s (not (contains? entries id)) (contains? entries s))
                              (assoc p field s) p))) payload
                        [[:cue-id (get-in model [:registry :cues])]
                         [:page-id (:pages model)]])]
    (let [result (control/apply-command model
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
               (seq (get-in model [:modulation :controllers]))
               (not= :patch (get-in payload [:source :type]))
               (not= :live-playback (get-in payload [:source :origin])))
        (update result :model update :modulation modulation/hold
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

(defn- apply-message [model {:keys [type payload expected-epoch]} time-ms & [runtime]]
  (when (and (some? expected-epoch) (not= expected-epoch (:epoch model)))
    (throw (ex-info "Operation belongs to a previous show session" {:expected-epoch (:epoch model)
                                                                    :received-epoch expected-epoch})))
  (case type
    :command (command model payload time-ms)
    :event-subscriber {:model (subscribers/configure model payload) :ack {:status :accepted}}
    :rdm-update (do (when (and (:rdm-session-id payload)
                              (not (rdm/session-current? (:rdm runtime) (:rdm-session-id payload))))
                      (throw (ex-info "RDM result belongs to a retired device session" {})))
                    {:model (rdm/update-model model payload) :ack {:status :accepted}})
    :photometry (photometry/apply-operation model payload ((:now-ms (:publisher runtime)))
                                           (and (:independent-output? runtime) (some? (get-in runtime [:sink :send!]))))
    :calibration (do (when (and (= :start (:op payload)) (photometry/current model))
                       (throw (ex-info "Finish photometry calibration first" {})))
                     (calibration/apply-operation model payload time-ms ((:now-ms (:publisher runtime)))
                                              (and (:independent-output? runtime) (some? (get-in runtime [:sink :send!])))))
    :sensor-configure
    {:model (apply-sensor-result
             (sensors/configure model (:device-id payload)
                                (if (:source payload) (data/read-edn (:source payload)) (:config payload))
                                (or (:pairing payload) (get-in model [:sensors :sources (:device-id payload) :pairing]))) time-ms)
     :ack {:status :accepted}}
    :sensor-arm
    {:model (apply-sensor-result (sensors/arm! (connections/sensors (:connections runtime)) model
                                             (:device-id payload) (:session-id payload) (:armed? payload)) time-ms)
     :ack {:status :accepted}}
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
          definition (cond-> definition
                       (or (contains? payload :expected-version) (nil? (:version definition)))
                       (assoc :version (inc (get previous :version 0))))
          errors (registry/validate-definition kind definition)]
      (when (seq errors) (throw (ex-info "Invalid definition" {:errors errors})))
      (when (= :effects kind) (compiler/compile-effect definition))
      (update (command model {:op :registry/publish :kind kind :definition definition} time-ms)
              :ack merge {:kind kind :id (:id definition) :version (:version definition)}))
    :agent-cue
    (let [cue (:cue payload)
          errors (registry/validate-definition :cues cue)
          _ (when (seq errors) (throw (ex-info "Invalid cue" {:errors errors})))
          result (command model (merge (select-keys payload [:command-id :source :epoch])
                                       {:op :cue/put :cue cue}) time-ms)]
      (if (or (= :rejected (get-in result [:ack :status]))
              (some (fn [page] (some #{(:id cue)} (vals (:cells page)))) (vals (:pages (:model result)))))
        result
        (let [page-id (:page-id payload :agent)
              page (get-in result [:model :pages page-id] {:id page-id :name "Created cues" :cells {}})
              position (first (for [y (range 8) x (range 8) :when (not (contains? (:cells page) [x y]))] [x y]))]
          (if position
            (update result :model assoc-in [:pages page-id] (assoc-in page [:cells position] (:id cue)))
            (update result :ack assoc :page-full? true)))))
    :rig
    (let [resolve-ids (fn [entries kind]
                        (let [existing (into {} (map (juxt :id identity)) (get-in model [:rig kind]))]
                          (if-not (and (vector? entries) (every? map? entries)) entries
                            (mapv (fn [entry]
                                  (let [id (:id entry)
                                        text (if (keyword? id) (subs (str id) 1) id)
                                        original (or (get existing id) (get existing text)
                                                     (when (string? text) (get existing (keyword text))))]
                                    (cond-> entry original (assoc :id (:id original))))) entries))))
          payload (cond-> (update payload :fixtures resolve-ids :fixtures)
                    (contains? payload :trusses) (update :trusses resolve-ids :trusses)
                    (:edge-tracking payload) surfaces/resolve-link-ids)]
      (rig/validate-rig payload)
      {:model (cond-> (-> model (assoc :rig payload) (update :revision inc))
                (get-in model [:variables :surface-aim])
                (update-in [:variables :surface-aim] select-keys
                           (for [{:keys [id channels]} (:fixtures payload) :when (and (:pan channels) (:tilt channels))] id)))
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
    (let [id (:page-id payload :generated)
          page (get-in model [:pages id] {:id id :name "Effect registry" :cells {}})
          result (registry/reconcile-page (:registry model) page (:options payload {}))]
      {:model (-> model (assoc :registry (:registry result)) (assoc-in [:pages id] (:page result))
                  (update :revision inc))
       :ack (assoc (select-keys result [:placements :collisions :overflow]) :status :accepted :page-id id)})
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
                               :version (inc (:version previous 0)) :reload-policy :preserve-state})
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
            settings (merge (get-in model [:settings scope]) values)
            _ (when (= scope :lighting-capacity) (capacity/settings settings))
            _ (when (= scope :audio-returns) (audio-return/validate-config model settings))
            settings (if (= scope :expressive) (expressive-mapping/normalize-config settings) settings)]
        {:model (-> (if (= scope :fog-control)
                      (fog-control/configure model values)
                      (assoc-in model [:settings scope] settings))
                    (update :revision inc))
         :ack {:status :accepted}}))
    :auto
    (let [old-state (get-in model [:auto :state])
          model (if old-state
                  (reduce (fn [m c] (:model (command m c time-ms))) model
                          (:commands (policy/stop-plan model old-state time-ms))) model)
          enabled? (true? (:enabled? payload))
          config (dissoc payload :enabled? :seed :expected-epoch)
          state (when enabled? (policy/initial-state (:seed payload 0) time-ms (str (UUID/randomUUID))))]
      (when enabled? (policy/advance (assoc model :fixtures (get-in model [:rig :fixtures])) config state time-ms))
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

(defn- advance-auto [model time-ms]
  (if (get-in model [:auto :enabled?])
    (let [{:keys [state commands reasons]} (policy/advance
                                          (assoc model :fixtures (get-in model [:rig :fixtures]))
                                          (get-in model [:auto :config]) (get-in model [:auto :state]) time-ms)
          [model state] (reduce (fn [[m s] c]
                                  (let [{:keys [model ack]} (command m c time-ms)]
                                    [model (if (and (= :accepted (:status ack)) (:instance-id ack))
                                             (policy/record-launch s (:instance-id ack)) s)]))
                                [model state] commands)]
      (-> model (assoc-in [:auto :state] state) (assoc-in [:auto :reasons] reasons))) model))

(defn- send-output! [runtime universes frame-number]
  (when-let [send! (get-in runtime [:sink :send!])]
    (let [packet (retirement/include-retired (:retirement @(:patch runtime)) universes)
          memory @(:fog-deadlines runtime)
          packet (if memory (fog-deadline/guard memory packet frame-number ((get-in runtime [:publisher :now-ms]))) packet)]
      (send! packet frame-number)
      (when (:plan @(:venue-fog runtime))
        (venue-fog/presented! (:venue-fog runtime) ((get-in runtime [:publisher :now-ms])) packet)))))

(defn audio-callbacks
  "Capture epoch-bound callbacks for explicitly started audio. Features use a
  coalescing mailbox and events a bounded queue; only tick! writes show state."
  [runtime]
  (let [epoch (get-in @(:state runtime) [:model :epoch])
        current? #(and @(:running runtime) (= epoch (get-in @(:state runtime) [:model :epoch])))]
    {:clock! (fn [] (when (current?) (get-in @(:state runtime) [:model :transport])))
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
                        (swap! (:audio-input runtime) assoc :features features)))))
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
      (vreset! (:capacity-audio runtime) {:epoch (:epoch model) :received-ms time-ms
                                         :energy-envelope (get-in captured [:features :energy-envelope])}))
    (if (= (:epoch model) (:epoch captured))
      (cond-> model
        (:features captured) (routing/admit-audio (:features captured))
        (:features captured) (assoc-in [:variables :audio :features] (:features captured))
        (seq (:events captured)) (as-> m (reduce
            (fn [acc event] (staging/collect acc :audio {:kind :audio :id (or (get-in event [:source :id]) :analysis)} [event]))
            m (:events captured)))
        (seq (:events captured)) (assoc-in [:variables :audio :events] (:events captured))
        (seq (:events captured)) (update-in [:variables :audio :event-count] (fnil + 0) (count (:events captured)))) model)))

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
  (let [{:keys [selected-source clock follow-transport?]} (session/clock-at (connections/sync (:connections runtime)) time-ms)]
    (if-let [captured (cond
                        (= :manual selected-source)
                        (assoc (:transport (advance-transport model time-ms)) :source :manual :source-epoch 0)

                        (and (false? follow-transport?) (or (nil? clock) (:stale? clock)))
                        ;; Tempo-only Link must never change local playback just
                        ;; because a peer has not arrived or has disconnected.
                        (assoc (:transport (advance-transport model time-ms)) :source selected-source :source-epoch nil)

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
            captured (select-keys captured [:time-ms :beat :bpm :playing? :meter :source :source-epoch])]
        ;; Rebase from the original model once; never merge the new beat first.
        (let [model (if discontinuity? (show-clock/rebase model captured) (update model :transport merge captured))]
          ;; A local Play command may already have flipped :playing? before the
          ;; subsequent Link sample arrives. Remember that musical anchors were
          ;; frozen until we have the resumed peer phase to rebase them against.
          (if (and (false? follow-transport?) (false? (:playing? captured)))
            (assoc-in model [:transport :link-phase-held?] true)
            (update model :transport dissoc :link-phase-held?))))
      (update model :transport assoc :time-ms time-ms :playing? false
              :source selected-source :source-epoch nil))))

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

(defn- observe-sync-commands! [runtime before after receipts]
  (let [manager (connections/sync (:connections runtime))
        old-ids (if (= (:epoch before) (:epoch after))
                  (set (get-in before [:control :history-order])) #{})
        epoch (:epoch after)]
    ;; Include automation, composition and controller commands, not only HTTP.
    (doseq [id (get-in after [:control :history-order]) :when (not (old-ids id))
            :let [{:keys [command ack]} (get-in after [:control :history id])]]
      (let [instance (get-in after [:instances (:instance-id ack)])
            timing (or (:duration instance) (when-let [ms (:duration-ms instance)] {:ms ms}))]
        (live-sync/record! manager (cond-> command
                               (:instance-id command)
                               (assoc :cue-id (or (get-in after [:instances (:instance-id command) :cue-id])
                                                  (get-in before [:instances (:instance-id command) :cue-id])))
                               (and (= :cue/launch (:op command)) timing) (assoc :recording-duration timing))
                      ack))
      (acknowledge-sync! runtime manager command ack epoch))
    (doseq [[_ ack command] receipts :when (= :rejected (:status ack))]
      (acknowledge-sync! runtime manager command ack epoch))))

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

(defn tick!
  "Advance one owner cycle; restore resources/model together, then retire IO
  outside the owner lock. Reactive actions from the previous publication enter
  this cycle with fresh command identity and the captured source epoch."
  ([runtime] (tick! runtime (now-ms runtime)))
  ([runtime time-ms]
   (let [frame-start-ns (System/nanoTime)
         retired (volatile! []) candidates (volatile! []) retired-devices (volatile! [])
         restored-generation (volatile! nil) restarted-generation (volatile! nil)
         committed-fault-events (volatile! [])]
     (try
       (locking (:lock runtime)
         (when @(:running runtime)
           (let [before @(:state runtime)
                 time-ms (max time-ms (get-in before [:model :time-ms]))
                 messages (java.util.ArrayList.)
                 _ (.drainTo ^LinkedBlockingQueue (:queue runtime) messages 256)
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
                        [(:model result) (conj receipts [reply (:ack result) (when (= :command (:type message)) (:payload message))])
                         (or restored? (:restore? result))
                         (if (:restore? result) (:restored-output result) restored-output)])
                      (catch Exception error
                        [model (conj receipts [reply (merge {:status :rejected} (error-data error)) (when (= :command (:type message)) (:payload message))])
                         restored? restored-output])))
                  [(-> (consume-sync! runtime (consume-audio! runtime (staging/begin (source-clock-model runtime (:model before) time-ms)) time-ms))
                       (calibration/advance physical-receipts physical-now)
                       (photometry/advance physical-receipts physical-now)) [] false nil] messages)
                 time-ms (if restored? (:time-ms model) time-ms)
                 _ (when (or restored? @restarted-generation)
                     (when restored? (reset! (:clock runtime) {:origin-nanos (System/nanoTime) :time-ms time-ms}))
                     (reset! (:leases runtime) {})
                     (locking (:sync-input runtime)
                       (reset! (:sync-input runtime) {:epoch (:epoch model) :events []}))
                     (locking (:audio-input runtime)
                       (reset! (:audio-input runtime) {:epoch (:epoch model) :events [] :features nil})))
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
                 presentation-token (when (:independent-output? runtime)
                                      (output-scheduler/begin-frame! (:publisher runtime) (:epoch model)
                                                                 (patch! runtime (get-in model [:rig :fixtures]))
                                                                 (get-in model [:settings :output :presentation] {})
                                                                 (or (pause-look/blackout? model) (zero? (:master model)))))
                 presentation-token (cond-> presentation-token
                                      presentation-token (assoc :calibration (or (photometry/marker photo-session) (calibration/marker model))))
                 started (System/nanoTime)
                 next-state
                 (if restored?
                   {:model (cond-> model (get-in model fog-deadline/saved-ids-path)
                             (update :extensions dissoc ::fog-deadline/saved-automatic-pumps))
                    :output (if restored-output
                              (update restored-output :universes
                                      #(fog-deadline/restore-packet
                                         (patch! runtime (get-in model [:rig :fixtures]))
                                         (get-in model fog-deadline/saved-ids-path)
                                         (into {} (map (fn [[u values]] [u (byte-array (map unchecked-byte values))])) %)))
                              (render/resolve-frame (patch! runtime (get-in model [:rig :fixtures])) [] {:blackout? true}))
                    :status {:restored? true :frame-ms 0}}
                   (try
                     (let [model (-> (advance-sensors! runtime model time-ms)
                                     (subscribers/advance time-ms (event-callbacks/results (:event-callbacks runtime)))
                                     (composition/advance time-ms) (advance-auto time-ms)
                                     (audio-return/advance (duplex/frame-input! (:audio-returns runtime)) time-ms routing/admit-sync)
                                     (routing/advance time-ms command))
                           modulation-result (modulation/advance (:modulation model) model
                                                                  {:time-ms time-ms :transport (:transport model)})
                           model (assoc model :modulation (:state modulation-result))
                           modulation-middleware (modulation/middleware (:overrides modulation-result))
                           _ (prepare-compiler! runtime (get-in model [:rig :fixtures]))
                           result (engine/advance model {:time-ms time-ms :epoch (:epoch model)
                                                         :show-id (:show-id model) :fixtures (get-in model [:rig :fixtures])}
                                                  (let [middleware (response-middleware! runtime (:responses model []))]
                                                    (fn [definition] (comp ((:compile runtime) definition) middleware modulation-middleware)))
                                                  (executor/mapper (:executor runtime)))
                           patch (patch! runtime (get-in model [:rig :fixtures]))
                           manual-aim (surface-aim/contribution model patch)
                           pause-layer (pause-layer! runtime model)
                           meter-opts (get-in model [:settings :lighting-capacity])
                           plan (when (:enabled? meter-opts) (capacity-plan! runtime patch))
                           audio @(:capacity-audio runtime)
                           previous (when (= (:epoch model) (get-in before [:model :epoch]))
                                      (get-in before [:output :lighting-capacity]))
                           bypass? (or photo-session (calibration/current model) (false? (get-in model [:transport :playing?])) (pause-look/blackout? model))
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
                           setpoint (when (or observation previous-control (:block fog) (get-in model [:settings :fog-control :enabled?]))
                                      (fog-control/frame model patch observation previous-control
                                        (if (get-in runtime [:sink :send!]) ((get-in runtime [:publisher :now-ms])) time-ms)
                                        fog-bypass? (:contributions result) (:block fog)))
                           extra-blocks (cond-> (fog-control/with-demands [] (:feedback setpoint))
                                          manual-aim (conj manual-aim)
                                          (:music setpoint) (conj (:music setpoint))
                                          (:block photo-session) (conj (:block photo-session))
                                          pause-layer (conj pause-layer))
                           resolve-frame (compute/frame-renderer (:compute runtime) frame-start-ns)
                           resolved (effect-faults/resolve-output result
                                     #(resolve-frame patch (into % extra-blocks)
                                     {:master (get limit :effective-master (:master model)) :blackout? (pause-look/blackout? model)
                                      :include-values? (not (:batch? runtime)) :compact-values? (:batch? runtime)
                                      :previous-values (or (get-in before [:output :values]) {})
                                      :previous-compact (get-in before [:output :compact])}))
                           result (:result resolved)
                           frame (cond-> (:output resolved)
                                   setpoint (assoc :fog-control (:status setpoint) :fog-demands (:leases setpoint))
                                   plan (assoc :lighting-capacity
                                               (merge limit (capacity/measure plan (get-in resolved [:output :universes]))
                                                      (cond-> {:writers (if (or (pause-look/blackout? model) (zero? (:master model))) []
                                                                          (capacity/writers plan (into (:contributions result) extra-blocks)))}
                                                        fog (assoc :fog (:state fog))))))]
                       {:model (cond-> (:model result) setpoint (fog-control/publish previous-control (:status setpoint)))
                        :output frame :fault-events (:fault-events result)
                        :modulation-view (select-keys modulation-result [:overrides :diagnostics])
                        :status {:frame-ms (/ (- (System/nanoTime) started) 1.0e6) :last-intents (:intents result)}})
                     (catch Exception error
                       (assoc before :model model :status {:error (error-data error)} :fault-events []
                              :output (if (or (pause-look/blackout? model) (zero? (:master model)))
                                        (render/resolve-frame (patch! runtime (get-in model [:rig :fixtures])) [] {:blackout? true})
                                        (:output before))))))
                 operations (map (fn [message [_ ack]]
                                   {:operation (:type message) :command (get-in message [:payload :op])
                                    :ack ack :emitter-id (get-in message [:payload :source :adapter])})
                                 messages receipts)
                 committed (show-events/commit (:event-admit runtime) (:model before) next-state operations)
                 next-state (venue-fog/commit! runtime (:state committed)
                              (patch! runtime (get-in next-state [:model :rig :fixtures])) restored?)]
             (when (or (seq (get-in next-state [:output :fog-demands])) @(:fog-deadlines runtime))
               (swap! (:fog-deadlines runtime) fog-deadline/remember
                      (or (:frame-id presentation-token) (get-in next-state [:model :frame-number] 0))
                      (when-not restored? (get-in next-state [:output :fog-demands]))))
             (reset! (:state runtime) next-state)
             (event-callbacks/publish! (:event-callbacks runtime) (:model next-state))
             (vreset! committed-fault-events (:fault-events next-state []))
             (doseq [[reply ack] receipts] (deliver reply ack))
             (when-not restored?
               (try (observe-sync-commands! runtime (:model before) (:model next-state) receipts)
                    (catch Exception error
                      (swap! (:state runtime) assoc-in [:status :sync-error] (error-data error)))))
             (try (if (:independent-output? runtime)
                    (when-not (get-in next-state [:status :error])
                      (output-scheduler/complete! (:publisher runtime) presentation-token (:output next-state)))
                    (send-output! runtime (get-in next-state [:output :universes]) (get-in next-state [:model :frame-number] 0)))
                  (catch Exception error
                    (swap! (:state runtime) assoc-in [:status :output-error] (error-data error))))
             (let [observers (vals @(:observers runtime))
                   trace (try (:delivery-trace (connections/publish! (:connections runtime) (:model next-state)
                                                                     (boolean (some (fn [observer] ((:tracing? observer))) observers))))
                              (catch Exception error (vreset! reactive-error (error-data error)) nil))]
               (doseq [observer observers]
                 ((:events! observer) (show-events/clock-context (:model next-state))
                  (:events committed) (:deliveries committed) trace)))
             (when @reactive-error
               (swap! (:state runtime) assoc-in [:status :reactive-error] @reactive-error))
             (doseq [observer (vals @(:observers runtime))]
               ((:frame! observer) @(:state runtime))))))
       (finally
         (effect-faults/offer! (:effect-logs runtime) @committed-fault-events)
         (finish-resource-work! runtime @retired @candidates @retired-devices
                                @restored-generation @restarted-generation)
         (rdm/reconcile! (:rdm runtime)))))))

(defn submit!
  "Enqueue an operation. Full restores are validated and resource-prepared off
  the frame-owner lock, then committed atomically by the next owner cycle."
  ([runtime payload] (submit! runtime :command payload))
  ([runtime type payload]
   (let [reply (promise) candidate (volatile! nil)]
     (try
       (let [before (:model @(:state runtime))
             epoch (get payload :expected-epoch (:epoch before))]
         (when-not (and @(:running runtime) (= epoch (:epoch before))
                        (or (not= type :photometry) (and (= epoch (:expected-epoch payload)) (= (:id runtime) (:runtime-id payload))))
                        (or (nil? (:runtime-id payload)) (= (:id runtime) (:runtime-id payload))))
           (throw (ex-info "Show stopped or session changed" {})))
         (let [payload (cond-> payload (and (= :rig type) (map? (:fixtures payload)))
                         (update :fixtures (partial fixture-patch/expand (get-in before [:registry :fixtures]))))
               payload (if (= :import type)
                         (let [document (persistence/read-string (:source payload))
                               calibration-armed? (or (photometry/current (:model document))
                                                      (get-in document (into [:model] (conj calibration-session/artifact-path :armed?))))
                               ;; Strip scoped ownership before thaw rekeys instance sources.
                               document (update document :model (comp fog-control/disarm-restored photometry/disarm calibration/disarm-restored))
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
               accepted? (locking (:lock runtime)
                           (and @(:running runtime) (= epoch (get-in @(:state runtime) [:model :epoch]))
                                (.offer ^LinkedBlockingQueue (:queue runtime)
                                        {:type type :payload payload :reply reply :expected-epoch epoch})))]
           (when-not accepted? (throw (ex-info "Show changed or command queue is full" {})))))
       (catch Exception error
         (when @candidate (connections/abort-prepared! @candidate))
         (deliver reply (merge {:status :rejected} (error-data error)))))
     reply)))

(defn register-event-subscriber!
  "Register show data from the REPL. An inline :middleware function is bound in
  this process; a qualified Var or portable :implementation can be restored."
  [runtime {:keys [id middleware] :as registration}]
  (let [registration (if middleware
                       (let [_ (when-not (or (fn? middleware) (var? middleware))
                                 (throw (ex-info "Expected middleware function or Var" {:id id})))
                             implementation (if (var? middleware)
                                              {:type :var :symbol (str (symbol middleware))}
                                              {:type :registered :id id})
                             definition (-> registration (dissoc :middleware)
                                            (update :actions (fnil conj []) {:op :callback :implementation implementation}))]
                         (subscribers/validate-definition definition)
                         (when (fn? middleware) (event-callbacks/register! (:event-callbacks runtime) id middleware))
                         definition)
                       registration)]
    (submit! runtime :event-subscriber {:op :save :subscriber registration})))

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
                 :implementation {:type :source :namespace ~(str *ns*)
                                  :form ~(pr-str middleware) :bindings ~(:bindings registration {})}}))
      `(register-event-subscriber! ~runtime ~registration))))

(defn stop! [runtime]
  (let [candidates (volatile! [])
        stop? (locking (:lock runtime)
                (when (compare-and-set! (:running runtime) true false)
                  (loop []
                    (when-let [message (.poll ^LinkedBlockingQueue (:queue runtime))]
                      (when-let [candidate (get-in message [:payload :candidate])]
                        (vswap! candidates conj candidate))
                      (deliver (:reply message) {:status :rejected :message "Show stopped"})
                      (recur)))
                  true))]
    ;; No cleanup waits while holding the owner lock: a closing device or agent
    ;; may synchronously dispatch, and must receive its stopped receipt promptly.
    (when stop?
      (doseq [cleanup (concat (map :close! (vals (first (swap-vals! (:observers runtime) (constantly {})))))
                             (map #(fn [] (connections/abort-prepared! %)) @candidates)
                             [#(event-callbacks/close! (:event-callbacks runtime))
                              #(rdm/close! (:rdm runtime))
                              #(output-scheduler/close! (:publisher runtime))
                              #(effect-faults/close! (:effect-logs runtime))
                              #(compute/close! (:compute runtime))
                              #(duplex/stop! (:audio-returns runtime))
                              #(development/close! (:development runtime))
                              #(connections/close! (:connections runtime))
                              (fn [] (locking (:devices runtime)
                                       (doseq [device (vals @(:devices runtime))]
                                         (try (devices/close! device) (catch Exception _)))
                                       (reset! (:devices runtime) {})))

                              (fn [] (when-let [^ScheduledExecutorService scheduler @(:scheduler runtime)] (.shutdownNow scheduler)))
                              #(.close ^java.io.Closeable (:executor runtime))
                              #(send-output! runtime (render/idle-universes (:compiled @(:patch runtime))) 0)
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

(defn dispatch-tool!
  "Agent and REPL tool boundary; all mutation uses the same owner queue as UI."
  [runtime tool arguments metadata]
  (when (and (:expected-epoch metadata)
             (not= (:expected-epoch metadata) (get-in @(:state runtime) [:model :epoch])))
    (throw (ex-info "Agent run belongs to a previous show session" {})))
  (let [arguments (data/normalize-json arguments)
        envelope (merge (select-keys metadata [:epoch :show-id :expected-epoch])
                        {:command-id (:command-id metadata)
                         :source {:adapter :agent :session-id (:run-id metadata)}})
        [type payload]
        (case tool
          "inspect_show" [:inspect nil]
          "publish_effect" [:definition (merge envelope {:kind :effects :definition (:definition arguments)})]
          "put_cue" [:agent-cue (merge envelope {:cue (:cue arguments)})]
          "launch_cue" [:command (merge envelope arguments {:op :cue/launch})]
          "stop_instance" [:command (merge envelope arguments {:op :instance/end})]
          "set_parameter" [:command (merge envelope arguments {:op :param/set})]
          "configure_auto" [:auto (merge (:configuration arguments) (select-keys envelope [:expected-epoch]))]
          (throw (ex-info "Unknown studio tool" {:tool tool})))]
    (if (= :inspect type) (snapshot runtime)
      (deref (submit! runtime type payload) 5000 {:status :pending :message "Command remains queued"}))))

(defn push-browser-display
  "Build a Push display transport using Studio's shared reactive components.
  Capture and waveform projection run on the display worker, not the show clock.
  Supply the browser executable and optional injected sink in options."
  [runtime options]
  (let [cache (waveforms/create-cache)]
    (display/browser-display
     (assoc options :project! #(waveforms/project-display
                                cache @(:state runtime) %
                                (audio/waveform (connections/audio (:connections runtime))))))))

(defn connect-device!
  "Explicit REPL connection; no devices open at application startup. kind is
  :push-2/:push-3/:push-3-midi/:seaboard/:x-touch/:midi/:osc/:sensor. Options select ports,
  expression modes and bindings; imported namespaces never acquire devices."
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
            device (open! (merge options {:model! #(let [model (:model @(:state runtime))]
                                                      (when-not (= epoch (:epoch model))
                                                        (throw (ex-info "Device belongs to a previous show epoch" {})))
                                                      model)
                                          :now-ms! #(now-ms runtime)
                                          :dispatch! (fn [command]
                                                       (heartbeat! runtime id)
                                                       (deref (submit! runtime (assoc command :expected-epoch epoch)) 5000
                                                              {:status :pending}))}))]
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

(defn- capture-show [runtime format-show]
  ;; Model/output come from one commit. Serialize after releasing the frame
  ;; owner so that large pixel runs can keep rendering during a download.
  (let [[model output] (locking (:lock runtime)
                         (let [{:keys [model output]} @(:state runtime)
                               atmosphere (venue-fog/for-save runtime output)]
                           [(cond-> (fog-deadline/save-ids (merge model (connections/export-state (:connections runtime) model)) output)
                              atmosphere (assoc-in venue-fog/saved-path atmosphere)) output]))]
    (format-show model (render/portable-frame output))))

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
        :disconnect (rdm/disconnect! (:rdm runtime))
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

(defn start!
  "Start the scheduled show in preview, or explicitly select :ola or :artnet output."
  ([] (start! {}))
  ([{:keys [frame-rate] :or {frame-rate 40} :as options}]
   (when-not (and (number? frame-rate) (<= 1 frame-rate 120)) (throw (ex-info "Frame rate must be 1..120" {})))
   (let [sink (output/open-sink! options)
         runtime (try (create-runtime (assoc options :sink sink :independent-output? true))
                      (catch Throwable error
                        (when sink
                          (try ((:close! sink)) (catch Throwable cleanup (.addSuppressed error cleanup))))
                        (throw error)))
         scheduler (Executors/newSingleThreadScheduledExecutor)]
     (try
       (reset! (:scheduler runtime) scheduler)
       (output-scheduler/start! (:publisher runtime) (/ 1000.0 frame-rate))
       (.scheduleAtFixedRate scheduler ^Runnable #(tick! runtime) 0 (long (/ 1000000000 frame-rate)) TimeUnit/NANOSECONDS)
       runtime
       (catch Throwable error (stop! runtime) (throw error))))))
