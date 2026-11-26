(ns tolglow.event-subscribers
  "Pure admission and playback of event-driven and beat-scheduled show actions.
  Subscriber sequences, scheduled agent/controller events and parameter ramps
  share one durable action queue on the show clock. Ramps are sampled from the
  owner's frozen frame transport; no timers or callbacks run here."
  (:require [clojure.string :as str]
            [tolglow.spatial :as spatial]
            [afterglow.reactive.registry :as reactive]
            [afterglow.time :as time]
            [afterglow.control.model :as control]
            [afterglow.control.params :as params]
            [afterglow.render.easing :as easing]
            [afterglow.runtime.events :as staging]
            [afterglow.runtime.faults :as faults]
            [tolglow.automation.random :as random]
            [tolglow.composition :as composition]
            [tolglow.composition.tween :as tween]
            [tolglow.modulation :as modulation]
            [tolglow.registry :as registry]
            [tolglow.lighting-scenes :as lighting-scenes]
            [tolglow.lighting-levels :as lighting-levels]))

(def actions [:lighting/scene :cue/launch :effect/launch :instance/end :instance/kill :instance/opacity
              :param/set :param/ramp :master/set :blackout/set :variable/set :macro/run :macro/stop :callback])

(def categories
  "Display category for each action type, shared by Studio, Push, REPL and agents."
  {:lighting/scene :scene :cue/launch :cue :effect/launch :cue :instance/end :instance :instance/kill :instance
   :instance/opacity :instance :param/set :param :param/ramp :ramp :master/set :master :blackout/set :master
   :variable/set :variable :macro/run :macro :macro/stop :macro :callback :callback})

(def operations
  "Owner domain commands: schedule edits, validated show-variable writes and
  macro start/stop. Their receipts use the shared reducer."
  #{:schedule/post :schedule/cancel :schedule/move :variable/set :macro/run :macro/stop})

(def ^:private timing-keys #{:delay :at :in :grid})

(def ^:private instance-ops #{:instance/end :instance/kill :instance/opacity})

(def ^:private run-limit 256)

(def ^:private scheduled-limit 256)

(def ^:private batch-limit 64)

(def ^:private ramp-limit 256)

(def ^:private recent-limit 64)

(def event-types
  {:spatial [:spatial/enter :spatial/exit :spatial/bounce :spatial/invalid-volume]
   :reactive [:reactive/fault :event/handler-failed]
   :atmosphere [:fog/below-target :fog/in-band :fog/above-target :fog/target-changed :fog/demand-started :fog/demand-ended]
   :lighting (into [:lighting/activity] (concat lighting-levels/group-event-types lighting-levels/rig-event-types))
   :audio [:audio/kick :audio/beat :audio/onset :audio/energy :audio/breakdown-start :audio/breakdown-end :audio/section-change]
   :music [:music/section-pattern]
   :show [:show/restored]
   :clock [:clock/beat :clock/bar :clock/phrase]
   :control [:instance/launched :instance/removed :instance/released :instance/system-changed
             :effect/retry-requested :cue/deleted :cue/params-saved :cue/saved-params-cleared
             :transport/changed :master/changed :blackout/changed :param/changed :show/restarted
             :registry/published :page/changed :page/deleted :schedule/posted :schedule/cancelled :schedule/moved]
   :sensor [:sensor/armed :sensor/calibrated :sensor/dropout :sensor/gesture :sensor/input-error
            :sensor/sample-rejected :sensor/threshold]
   :sync [:sync/clock :sync/observation :sync/error :sync/input-overflow :sync/control-unavailable
          :peer/found :peer/lost :live/execution :live/status :surface/clear-feedback :surface/mode]
   :command [:command/accepted :command/rejected]
   :effect [:effect/intent :instance/finished :effect/failed :effect/recovered :effect/reload-failed :effect/reloaded]})

(def ^:private empty-state {:subscribers [] :runs [] :next-run 0 :seed 1 :dropped 0})

(def ^:private state-keys #{:subscribers :runs :next-run :seed :dropped :last-error :ramps :recent})

(defn- state [model] (:event-subscribers model empty-state))

(defn- id-text
  "Scheduled event IDs are text so JSON, EDN keyword and string forms agree."
  [id]
  (if (keyword? id) (subs (str id) 1) id))

(defn- check [condition message data]
  (when-not condition (throw (ex-info message data))))

(defn- identifier? [id] (or (keyword? id) (and (string? id) (not (str/blank? id)))))

(defn- only-keys? [m allowed] (and (map? m) (every? allowed (keys m))))

(defn- nonnegative-int? [v] (and (integer? v) (not (neg? v))))

(defn- musical [duration]
  (check (and (map? duration) (every? #{:beats :bars :phrases} (keys duration))
              (empty? (time/validate-duration duration))) "Expected a musical duration" {:duration duration})
  duration)

(defn- position [at]
  (check (and (map? at) (seq at) (every? #{:beat :bar :phrase} (keys at))
              (every? #(and (registry/finite-number? %) (not (neg? %))) (vals at)))
         "A musical position needs nonnegative :beat, :bar and/or :phrase coordinates" {:at at})
  at)

(defn- destination-target [{:keys [op target]}]
  (case op
    (:param/set :param/ramp) (or target :param)
    :instance/opacity :opacity
    :master/set :master
    :variable/set :variable
    nil))

(defn- instance-target? [action]
  (boolean (or (instance-ops (:op action)) (#{:param :opacity} (destination-target action)))))

(defn- curve! [curve]
  (try (easing/sample curve 0.5) curve
       (catch Exception _ (throw (ex-info "Unknown easing curve" {:curve curve})))))

(defn- numeric-value? [v]
  (or (registry/finite-number? v) (and (vector? v) (seq v) (every? registry/finite-number? v))))

(def ^:private runtime-variables
  #{:audience :audio :audio-return-voices :runtime :sensors :spatial :surface-aim :sync})

(def ^:private runtime-variable-namespaces #{"audio" "fog" "lighting" "sensor" "sensors" "sync" "spatial"})

(defn reserved-variable?
  "Show variables written by the runtime (analysis, sensors, faults, lighting)."
  [k]
  (or (not (keyword? k)) (runtime-variables k) (contains? runtime-variable-namespaces (namespace k))))

(defn validate-action [{:keys [op select delay implementation launch value] :as action}]
  (check (and (some #{op} actions)
              (only-keys? action (into timing-keys
                                       (case op
                                         :lighting/scene #{:op :scene :event-path :gate}
                                         :callback #{:op :implementation}
                                         (:cue/launch :effect/launch) #{:op :select :launch}
                                         :instance/opacity #{:op :select :value}
                                         :param/set #{:op :select :target :param :variable :value}
                                         :param/ramp #{:op :select :target :param :variable :to :over :curve}
                                         (:master/set :blackout/set) #{:op :value}
                                         :variable/set #{:op :variable :value}
                                         (:macro/run :macro/stop) #{:op :macro-id}
                                         #{:op :select}))))
         "Unknown event action or option" {:op op})
  (check (<= (count (filter #(contains? action %) timing-keys)) 1)
         "Use only one of :delay, :at, :in or :grid" {:op op})
  (when (= op :lighting/scene)
    (lighting-scenes/validate-request (:scene action))
    (when-let [{:keys [path above below] :as gate} (:gate action)]
      (check (and (map? gate) (vector? path) (seq path) (every? keyword? path)
                  (registry/finite-number? above) (registry/finite-number? below) (< below above))
             "A scene gate needs an event path and distinct upper/lower thresholds" {:gate gate}))
    (when-let [path (:event-path action)]
      (check (and (vector? path) (seq path) (every? keyword? path)) "Use a path within the emitted event" {:path path})))
  (when (contains? action :delay) (musical delay))
  (when (contains? action :in) (musical (:in action)))
  (when (contains? action :at) (position (:at action)))
  (when (contains? action :grid)
    (musical (:grid action))
    (check (pos? (val (first (:grid action)))) "A grid needs a positive interval" {}))
  (when (contains? action :select)
    (check (and (only-keys? select #{:ids :categories :page-id :pick})
                (#{:all :first :random} (:pick select :random))
                (or (not (contains? select :page-id)) (identifier? (:page-id select)))
                (every? (fn [[k pred]] (or (not (contains? select k))
                                            (and (vector? (get select k)) (<= (count (get select k)) 1024)
                                                 (every? pred (get select k)))))
                        [[:ids identifier?] [:categories keyword?]]))
           "Invalid event candidate selector" {:select select}))
  (when (contains? action :launch)
    (check (and (only-keys? launch #{:mode :duration :params})
                (#{:one-shot :persistent} (:mode launch :persistent))
                (or (not (contains? launch :params)) (and (map? (:params launch)) (registry/data? (:params launch)))))
           "Invalid event launch options" {:launch launch})
    (when (contains? launch :duration)
      (musical (:duration launch))
      (check (pos? (val (first (:duration launch)))) "One-shot duration must be positive" {})))
  (when (#{:instance/opacity :master/set} op)
    (check (and (registry/finite-number? value) (<= 0 value 1)) "Level must be between zero and one" {}))
  (when (= op :blackout/set)
    (check (boolean? value) "Blackout must be true or false" {}))
  (when (#{:macro/run :macro/stop} op)
    (check (identifier? (:macro-id action)) "A macro action needs a macro ID" {}))
  (when (#{:param/set :param/ramp :variable/set} op)
    (let [target (destination-target action)
          level (if (= op :param/ramp) (:to action) value)]
      (check (#{:param :opacity :master :variable} target)
             "Destination target must be :param, :opacity, :master or :variable" {:target target})
      (check (= (boolean (#{:param :opacity} target)) (contains? action :select))
             "Instance destinations need a selector or :instance-id; global destinations do not" {:target target})
      (check (= (= target :param) (keyword? (:param action)))
             "A parameter destination needs a :param keyword" {:param (:param action)})
      (check (= (= target :variable) (keyword? (:variable action)))
             "A variable destination needs a :variable keyword" {:variable (:variable action)})
      (check (and (some? level) (registry/data? level)) "Destination values must be portable data" {})
      (when (#{:opacity :master} target)
        (check (and (registry/finite-number? level) (<= 0 level 1)) "Level must be between zero and one" {}))))
  (when (= op :param/ramp)
    (let [over (:over action)]
      (check (and (map? over) (= 1 (count over)) (#{:beats :bars :phrases :ms} (ffirst over))
                  (empty? (time/validate-duration over)) (pos? (val (first over))))
             "A ramp needs one positive :over duration in beats, bars, phrases or ms" {:over over})
      (curve! (:curve action :linear))
      (when (= :variable (destination-target action))
        (check (numeric-value? (:to action)) "A variable ramp needs a numeric target" {}))))
  (when (= :variable (destination-target action))
    (check (not (reserved-variable? (:variable action)))
           "That show variable is owned by the runtime; choose your own variable name" {:variable (:variable action)}))
  (when (= op :callback)
    (case (:type implementation)
      :registered (check (and (only-keys? implementation #{:type :id}) (keyword? (:id implementation)))
                         "A registered callback needs a keyword id" {})
      :inline (let [id (:callback-id implementation)]
                (check (and (only-keys? implementation #{:type :callback-id}) (string? id)
                            (= id (some-> (parse-uuid id) str)))
                       "A process-local callback needs a UUID" {}))
      (reactive/validate-definition {:kind :fx :id ::callback :implementation implementation})))
  action)

(defn- textual-keyword [v] (if (string? v) (keyword v) v))

(defn normalize-action
  "Expand agent shorthands to the canonical action vocabulary. :cue-id,
  :effect-id and :instance-id become selectors, a ramp's :start {:in|:at|:grid}
  becomes the action's own timing and textual enum choices become keywords."
  [action]
  (check (map? action) "An action must be a map" {})
  (let [{:keys [cue-id effect-id instance-id start]} action
        action (update action :op textual-keyword)
        ;; A cue or effect ID on an instance action addresses every running instance of it.
        pick (if (instance-target? (cond-> action (contains? action :target) (update :target textual-keyword)))
               :all :first)]
    (check (or (nil? start) (and (map? start) (= 1 (count start)) (#{:in :at :grid} (ffirst start))))
           "Ramp :start takes one of :in, :at or :grid" {:start start})
    (cond-> (dissoc action :cue-id :effect-id :instance-id :start)
      start (merge start)
      (some? cue-id) (assoc :select {:ids [cue-id] :pick pick})
      (some? effect-id) (assoc :select {:ids [effect-id] :pick pick})
      (some? instance-id) (assoc :select {:ids [instance-id] :pick :all})
      (contains? action :target) (update :target textual-keyword)
      (contains? action :param) (update :param textual-keyword)
      (contains? action :variable) (update :variable textual-keyword)
      (contains? action :curve) (update :curve textual-keyword)
      (contains? action :macro-id) (update :macro-id textual-keyword)
      (get-in action [:select :pick]) (update-in [:select :pick] textual-keyword))))

(defn validate-definition [{:keys [id enabled? match actions] :as registration}]
  (check (and (only-keys? registration #{:id :enabled? :match :actions})
              (keyword? id) (instance? Boolean (get registration :enabled? true))
              (only-keys? (or match {}) #{:type :category :emitter-id :volume-id :fixture-id :pattern})
              (every? keyword? (vals (select-keys match [:type :category])))
              (every? identifier? (vals (select-keys match [:emitter-id :volume-id :fixture-id])))
              (or (not (contains? match :pattern))
                  (and (= :music/section-pattern (:type match))
                       (let [pattern (:pattern match)]
                         (or (identifier? pattern) (integer? pattern) (uuid? pattern)))))
              (vector? actions) (<= 1 (count actions) 32))
         "Invalid event subscriber definition" {:id id})
  (run! validate-action actions)
  registration)

(defn validate-state [s]
  (check (and (map? s) (every? state-keys (keys s))
              (nonnegative-int? (:next-run s)) (integer? (:seed s)) (nonnegative-int? (:dropped s))
              (vector? (:subscribers s)) (<= (count (:subscribers s)) 128)
              (= (count (:subscribers s)) (count (set (map :id (:subscribers s)))))
              (vector? (:runs s)) (<= (count (:runs s)) (+ run-limit scheduled-limit))
              (vector? (:ramps s [])) (<= (count (:ramps s [])) ramp-limit)
              (vector? (:recent s [])) (<= (count (:recent s [])) recent-limit))
         "Invalid event subscriber state" {})
  (run! validate-definition (:subscribers s))
  (let [definitions (set (map :id (:subscribers s)))]
    (doseq [{:keys [id subscriber event steps index due-beat waiting? scheduled anchor] :as run} (:runs s)]
      (check (and (only-keys? run #{:id :subscriber :event :steps :index :due-beat :waiting? :targets :offset
                                    :scheduled :anchor})
                  (contains? #{nil :absolute :relative} anchor)
                  (or (and (nil? subscriber) (map? scheduled) (identifier? (:event-id scheduled)))
                      (and (nil? scheduled) (definitions subscriber)))
                  (or (not (contains? run :targets))
                      (and (vector? (:targets run)) (every? identifier? (:targets run))
                           (nonnegative-int? (:offset run)) (< (:offset run) (count (:targets run)))))
                  (nonnegative-int? id) (<= id (:next-run s))
                  (map? event) (keyword? (:type event)) (vector? steps) (<= 1 (count steps) 32)
                  (nonnegative-int? index) (< index (count steps))
                  (registry/finite-number? due-beat) (or (nil? waiting?) (instance? Boolean waiting?)))
             "Invalid pending event sequence" {:id id})
      (run! validate-action steps))
    (check (= (count (:runs s)) (count (set (map :id (:runs s))))) "Duplicate pending sequence id" {})
    (doseq [ramp (:ramps s [])]
      (check (and (map? ramp) (identifier? (:event-id ramp)) (#{:beats :ms} (:unit ramp))
                  (map? (:destination ramp)) (#{:param :opacity :master :variable} (get-in ramp [:destination :target]))
                  (registry/finite-number? (:start ramp)) (registry/finite-number? (:end ramp))
                  (< (:start ramp) (:end ramp)) (registry/data? (:from ramp)) (registry/data? (:to ramp)))
             "Invalid active ramp" {:event-id (:event-id ramp)})
      (curve! (:curve ramp))))
  s)

(declare candidates)

(defn catalog [model]
  {:subscribers (:subscribers (state model))
   :events (->> (concat (for [[category types] event-types type types] {:category category :type type})
                        (keep (fn [{:keys [match]}]
                                (when (:type match) (select-keys match [:type :category]))) (:subscribers (state model))))
                (reduce (fn [m row] (update m (:type row) #(or % row))) {}) vals
                (sort-by (comp str :type)) vec)
   :actions actions
   :candidates {:cues (candidates model {:op :cue/launch}) :effects (candidates model {:op :effect/launch})
                :instances (candidates model {:op :instance/end})}})

(defn configure [model {:keys [op subscriber id expected-subscriber] :as request}]
  (let [s (state model) id (if (= :save op) (:id (validate-definition subscriber)) id)
        _ (check (and (#{:save :remove} op) (keyword? id)) "Unknown subscriber operation or id" {})
        _ (when (contains? request :expected-subscriber)
            (check (= expected-subscriber (some #(when (= id (:id %)) %) (:subscribers s)))
                   "The subscriber changed since editing began" {:id id}))
        remaining (remove #(= id (:id %)) (:subscribers s))
        definitions (vec (sort-by (comp str :id) (cond-> remaining (= op :save) (conj subscriber))))
        _ (check (<= (count definitions) 128) "Maximum event subscribers reached" {})]
    (-> model (assoc :event-subscribers (assoc s :subscribers definitions
                                                :runs (filterv #(not= id (:subscriber %)) (:runs s))))
        (update :revision inc))))

(defn- matches? [{:keys [match] :as definition} {:keys [type category emitter data]}]
  (and (:enabled? definition true)
       (not (and (= :event/handler-failed type) (= (:id definition) (:subscriber-id data))))
       (or (nil? (:type match)) (= type (:type match)))
       (or (nil? (:category match)) (= category (:category match)))
       (or (nil? (:volume-id match)) (= (:volume-id data) (:volume-id match)))
       (or (nil? (:fixture-id match)) (= (:fixture-id data) (:fixture-id match)))
       (or (not (contains? match :pattern)) (= (:pattern data) (:pattern match)))
       (or (nil? (:emitter-id match)) (= (:id emitter) (:emitter-id match)))))

(defn- triggered? [{:keys [emitter data]}]
  (or (and (#{:adapter :command} (:kind emitter)) (= :event-subscriber (:id emitter)))
      (= :event-subscriber (:cause-adapter data))
      (= :event-subscriber (get-in data [:source :adapter]))))

(defn- delay-beats [action meter]
  (if-let [d (:delay action)] (time/duration->beats d meter) 0))

(defn- ceil-exact [q]
  (if (ratio? q)
    (let [n (numerator q) d (denominator q) f (quot n d)]
      (if (neg? n) f (inc f)))
    q))

(defn- position-beat [at meter]
  (let [{:keys [beats-per-bar bars-per-phrase]} (time/normalize-meter meter)]
    (+ (:beat at 0) (* (:bar at 0) beats-per-bar) (* (:phrase at 0) beats-per-bar bars-per-phrase))))

(defn grid-beat
  "Next grid boundary at or after beat, counted from beat zero in the current
  meter. An exact boundary is due now, matching cue_timing's quantized launches."
  [grid beat meter]
  (let [interval (rationalize (time/duration->beats grid meter))]
    (double (* interval (ceil-exact (/ (rationalize beat) interval))))))

(defn step-due
  "Resolve one action's timing to [due-beat anchor]. :delay follows base (the
  preceding step), :in follows origin (the triggering event or posting beat),
  :at names an absolute transport position and :grid the next boundary at or
  after base. Absolute steps do not move with a clock rebase."
  [action base origin meter]
  (cond
    (contains? action :at) [(double (position-beat (:at action) meter)) :absolute]
    (contains? action :grid) [(grid-beat (:grid action) base meter) :absolute]
    (contains? action :in) [(double (+ origin (time/duration->beats (:in action) meter))) :relative]
    :else [(+ base (delay-beats action meter)) :relative]))

(defn admit
  "Called once for a committed batch. Enqueue durable plans; no handlers run here."
  [model events]
  (if (empty? (get-in model [:event-subscribers :subscribers]))
    {:model model :deliveries {}}
    (let [meter (get-in model [:transport :meter])]
      (reduce-kv
       (fn [out index event]
         (if (triggered? event) out
           (reduce
            (fn [out {:keys [id actions]}]
              (let [s (get-in out [:model :event-subscribers]) serial (inc (:next-run s))
                    origin (get-in event [:time :beat] (get-in model [:transport :beat] 0))
                    [due anchor] (step-due (first actions) origin origin meter)]
                (if (>= (count (remove :scheduled (:runs s))) run-limit)
                  (update-in out [:model :event-subscribers :dropped] inc)
                  (-> out
                      (update-in [:deliveries index] (fnil conj []) id)
                      (assoc-in [:model :event-subscribers :next-run] serial)
                      (update-in [:model :event-subscribers :runs] conj
                                 (cond-> {:id serial :subscriber id :steps actions :index 0 :event event
                                          :due-beat due}
                                   (= :absolute anchor) (assoc :anchor anchor)))))))
            out (filter #(matches? % event) (get-in model [:event-subscribers :subscribers])))))
       {:model model :deliveries {}} events))))

(def ^:private excluded-roles #{:show-control :code :code-cue})

(defn- musical-definition? [entry]
  (and entry (not (or (excluded-roles (:role entry)) (excluded-roles (:kind entry))
                      (:scene-owner entry) (:code-cue? entry) (:show-control? entry) (some excluded-roles (:tags entry))))))

(defn candidates
  "Eligible targets in stable order. An exact running instance ID addresses a
  scene or show-control instance only for a human operator (:exact? true)."
  ([model action] (candidates model action nil))
  ([model {:keys [op select] :as action} {:keys [exact?]}]
  (let [{:keys [ids categories page-id]} select
        cues (get-in model [:registry :cues]) effects (get-in model [:registry :effects])
        by-page (when page-id (set (vals (get-in model [:pages page-id :cells]))))
        page-effects (when by-page (set (keep (fn [id] (let [cue (get cues id)] (when (musical-definition? cue) (:effect-id cue)))) by-page)))
        instances? (instance-target? action)
        rows (cond
               instances? (for [[id i] (:instances model)]
                            [id (get cues (:cue-id i)) (:definition i) (:cue-id i)])
               (= :cue/launch op) (for [[id c] cues] [id c (registry/cue-effect (:registry model) c) id])
               :else (for [[id e] effects] [id nil e nil]))
        ids (when ids (set ids)) categories (when categories (set categories))]
    (->> rows
         (keep (fn [[id cue effect cue-id]]
                 (when (and (or (and exact? instances? ids (contains? ids id)) ; a human's exact instance ID is explicit
                                (and (musical-definition? effect) (or (nil? cue) (musical-definition? cue))))
                            (or (nil? by-page) (if cue-id (contains? by-page cue-id) (contains? page-effects (:id effect))))
                            (or (nil? ids) (contains? ids id)
                                (and instances? (or (contains? ids cue-id) (contains? ids (:id effect)))))
                            (or (nil? categories) (some categories (registry/definition-categories effect))))
                   id)))
         (sort-by pr-str) vec))))

(defn- protected-instance?
  "A running scene-owner, show-control or code-cue instance stays with the operator."
  [model id]
  (when-let [instance (get-in model [:instances id])]
    (let [cue (get-in model [:registry :cues (:cue-id instance)])]
      (not (and (musical-definition? (:definition instance)) (or (nil? cue) (musical-definition? cue)))))))

(defn- chosen [seed candidates pick]
  (cond (empty? candidates) [seed []]
        (= pick :first) [seed [(first candidates)]]
        (= pick :all) [seed candidates]
        :else (let [[next-seed id] (random/choose seed candidates)] [next-seed [id]])))

(defn- launch-options [action]
  (let [launch (:launch action {:mode :persistent})]
    (cond-> launch (and (= :one-shot (:mode launch)) (nil? (:duration launch))) (assoc :duration {:beats 1}))))

(defn- command [action id]
  (case (:op action)
    :cue/launch {:op :cue/launch :cue-id id :launch (launch-options action)}
    :effect/launch {:op :effect/launch :effect-id id :launch (launch-options action)}
    :instance/end {:op :instance/end :instance-id id}
    :instance/kill {:op :instance/remove :instance-id id}
    :instance/opacity {:op :instance/opacity-set :instance-id id :value (:value action)}
    :param/set (if (= :opacity (destination-target action))
                 {:op :instance/opacity-set :instance-id id :value (:value action)}
                 {:op :param/set :instance-id id :param (:param action) :value (:value action)})))

(defn error-details [^Throwable error]
  (let [causes (->> (:causes (faults/diagnostic nil nil nil :handler error))
                    (filter (comp seq :location))
                    (mapv #(select-keys % [:class :message :location])))]
    ;; Studio can display these locations with the existing callback stack.
    (cond-> {:class (.getName (class error))
             :stacktrace (mapv str (take 24 (.getStackTrace error)))}
      (seq causes) (assoc :causes causes))))

(defn- run-event-id [run]
  (or (get-in run [:scheduled :event-id])
      (str "subscriber/" (some-> (:subscriber run) name) "/" (:id run))))

(defn- report
  ([model run message] (report model run message nil))
  ([model run message diagnostic]
   (let [message (str (or message "Event handler failed"))
         scheduled (:scheduled run)
         owner (or (:subscriber run) (:event-id scheduled))
         fault (merge (select-keys (get-in run [:event :data]) [:instance-id :effect-id :cue-id])
                      {:type :event/handler-failed :stage :handler :id owner
                       :subscriber-id (:subscriber run) :epoch (:epoch model) :time-ms (:time-ms model)
                       :run (:id run) :step (:index run) :message (subs message 0 (min 256 (count message)))}
                      (when scheduled {:event-id (:event-id scheduled)})
                      diagnostic)]
     (cond-> (-> model
                 (assoc-in [:event-subscribers :last-error] (assoc fault :subscriber (:subscriber run)))
                 (update-in [:variables :runtime :handler-faults] #(vec (take-last 64 (conj (vec %) fault)))))
       (not= :event/handler-failed (get-in run [:event :type]))
       (staging/collect :reactive {:kind (if scheduled :scheduler :subscriber) :id owner} [fault])))))

;; Ramps -----------------------------------------------------------------

(def ^:private human-adapters
  "Source adapters of operator surfaces. Device sessions use their profile ID
  (afterglow.control.devices): the X-Touch Extender is :x-touch-xt, a Seaboard
  :seaboard-m and an MPE controller :mpe."
  #{:web "web" :studio :push-2 :push-3 :x-touch :x-touch-xt :xtouch :seaboard-m :mpe :midi :osc :sensor :repl})

(defn human-source?
  "Studio, web, Push, X-Touch, Seaboard, MPE, MIDI, OSC, sensor and REPL sources
  are human operator surfaces; agents, reactive actions and subscribers are not."
  [source]
  (boolean (and (map? source) (human-adapters (:adapter source)))))

(defn- destination-key [destination]
  (select-keys destination [:target :instance-id :param :variable]))

(defn- current-value [model {:keys [target instance-id param variable]}]
  (case target
    :param (get-in model [:instances instance-id :params param])
    :opacity (get-in model [:instances instance-id :system :opacity] 1.0)
    :master (:master model 1.0)
    :variable (get-in model [:variables variable])))

(def ^:private unit-schema {:type :number :bounds [0.0 1.0]})

(defn- schema-of [model {:keys [target instance-id param]}]
  (case target
    :param (get-in model [:instances instance-id :definition :params param])
    (:opacity :master) unit-schema
    :variable nil))

(def ^:private rampable-types #{:number :integer :color :vector :range :gradient})

(defn- gradient-interpolate [from to fraction]
  (if (= (count from) (count to))
    (mapv (fn [a b] {:position (tween/interpolate (:position a) (:position b) fraction)
                     :color (tween/interpolate (:color a) (:color b) fraction)}) from to)
    (if (< fraction 0.5) from to)))

(defn ramp-value
  "Interpolate a typed destination value. Gradients with equal stop counts move
  every stop; numeric schemas are constrained to their bounds and integers."
  [schema from to fraction]
  (cond
    (nil? schema) (tween/interpolate from to fraction)
    (= :gradient (:type schema)) (gradient-interpolate from to fraction)
    :else (modulation/interpolate-value schema from to fraction)))

(defn- same-shape?
  "A number interpolates with a number, a vector with a vector of equal length."
  [a b]
  (if (vector? a) (and (vector? b) (= (count a) (count b))) (not (vector? b))))

(defn- progress [{:keys [start end]} clock]
  (double (max 0 (min 1 (/ (- clock start) (- end start))))))

(defn- ramp-clock [ramp beat now] (if (= :ms (:unit ramp)) now beat))

(defn- sample-ramp [model ramp beat now]
  (let [p (progress ramp (ramp-clock ramp beat now))]
    [p (ramp-value (schema-of model (:destination ramp)) (:from ramp) (:to ramp) (easing/sample (:curve ramp) p))]))

(defn- remember [model entry]
  (update-in model [:event-subscribers :recent]
             #(vec (take-last recent-limit (conj (vec %) entry)))))

(defn- ramp-entry [model ramp status extra]
  (merge {:event-id (:event-id ramp) :op :param/ramp :category :ramp :status status
          :destination (:destination ramp) :at-beat (double (get-in model [:transport :beat] 0))
          :time-ms (:time-ms model)}
         ;; The planned span and curve stay with the outcome so a timeline can
         ;; still draw a finished ramp as a bar (#646).
         (select-keys ramp [:label :source :to :from :curve :unit :over :start :end])
         extra))

(defn- remove-ramps [model pred status extra]
  (let [ramps (get-in model [:event-subscribers :ramps] [])
        {gone true kept false} (group-by (comp boolean pred) ramps)]
    (if (seq gone)
      (reduce #(remember %1 (ramp-entry %1 %2 status extra))
              (assoc-in model [:event-subscribers :ramps] (vec kept)) gone)
      model)))

(defn- same-destination [destination]
  (let [k (destination-key destination)] #(= k (destination-key (:destination %)))))

(defn- supersede [model destination by]
  (remove-ramps model (same-destination destination) :cancelled {:reason :superseded :by by}))

(defn override
  "Cancel running ramps on the destinations a committed non-scheduler write
  touched. The destination keeps the written value; recent records the reason."
  [model destinations reason by]
  (if (and (seq destinations) (seq (get-in model [:event-subscribers :ramps])))
    (let [keys (set (map destination-key destinations))]
      (remove-ramps model #(keys (destination-key (:destination %))) :cancelled {:reason reason :by by}))
    model))

(defn- run-source [run]
  (if-let [scheduled (:scheduled run)]
    (assoc (if (map? (:source scheduled)) (:source scheduled) {:adapter :scheduler})
           :scheduled-event-id (:event-id scheduled))
    {:adapter :event-subscriber :session-id (str (:subscriber run))}))

(defn- start-ramp [model run action destination now]
  (let [schema (schema-of model destination)
        meter (get-in model [:transport :meter])
        beat (get-in model [:transport :beat] 0)
        event-id (run-event-id run)
        [unit over] (let [[k v] (first (:over action))]
                      (if (= k :ms) [:ms v] [:beats (time/duration->beats (:over action) meter)]))
        ;; The ramp starts where it actually fires, on the frozen frame clock.
        start (if (= :ms unit) now beat)
        previous (some #(when ((same-destination destination) %) %) (get-in model [:event-subscribers :ramps]))
        base (current-value model destination)
        ;; Continue from the superseded ramp's current frame value, without a jump.
        from (if previous (second (sample-ramp model previous beat now)) base)
        to (:to action)]
    (check (or (nil? schema) (rampable-types (:type schema)))
           "This parameter type cannot ramp; use :param/set" {:destination destination})
    (check (if schema (and (params/valid-value? schema from) (params/valid-value? schema to))
               (and (numeric-value? from) (numeric-value? to)))
           "Ramp endpoints must satisfy the destination type" {:destination destination :from from :to to})
    (check (< (count (get-in model [:event-subscribers :ramps])) ramp-limit) "Too many active ramps" {})
    ;; A scalar cannot move to a vector: every later sample would throw on the owner.
    (when-not (= :gradient (:type schema))
      (check (same-shape? from to) "Ramp endpoints need equal shapes" {:from from :to to}))
    (-> model
        (supersede destination event-id)
        (update-in [:event-subscribers :ramps] (fnil conj [])
                   (cond-> {:event-id event-id :destination destination :from from :to to :base base
                            :curve (:curve action :linear) :unit unit :over (:over action)
                            :start start :end (+ start over) :source (run-source run) :category :ramp}
                     (get-in run [:scheduled :label]) (assoc :label (get-in run [:scheduled :label])))))))

(declare apply-operation)

(defn- submit-command
  "Admit one command through the shared reducer: schedule/variable/macro
  operations as domain commands, everything else through spatial/apply-command."
  [model cmd]
  (if (operations (:op cmd))
    (control/apply-domain-command model cmd apply-operation)
    (spatial/apply-command model cmd)))

(defn- commit-ramp
  "Write a ramp's value to its destination through the shared reducer: the
  exact target when it completes, or its current frame value when it is held."
  ([model ramp now] (commit-ramp model ramp now (:to ramp) nil))
  ([model ramp now value suffix]
   (let [{:keys [target instance-id param variable]} (:destination ramp)
         cmd (case target
               :param {:op :param/set :instance-id instance-id :param param :value value}
               :opacity {:op :instance/opacity-set :instance-id instance-id :value value}
               :master {:op :master/set :value value}
               :variable {:op :variable/set :variable variable :value value})]
     (:model (submit-command
              model (merge cmd {:show-id (:show-id model) :epoch (:epoch model) :time-ms now
                                :source (:source ramp)
                                :command-id (str "ramp/" (:epoch model) "/" (:event-id ramp) "/"
                                                 (pr-str (destination-key (:destination ramp))) "/" (:start ramp)
                                                 suffix)}))))))

(defn- writes-destination? [command {:keys [target instance-id param variable]}]
  (let [op (:op command)]
    (case target
      :param (and (= instance-id (:instance-id command)) (= param (:param command)))
      :opacity (and (= instance-id (:instance-id command)) (#{:instance/opacity-set :instance/opacity-toggle} op))
      :master (= :master/set op)
      :variable (and (= :variable/set op) (= variable (:variable command))))))

(defn- last-writer
  "The latest committed command that wrote this destination, from the shared
  receipt history. Macro steps, routing, automation and controllers all commit
  through that reducer."
  [model destination]
  (some (fn [id]
          (let [command (get-in model [:control :history id :command])]
            (when (and command (writes-destination? command destination))
              (cond-> {:op (:op command) :command-id id}
                (map? (:source command)) (assoc :source (select-keys (:source command) [:adapter :session-id :device-id]))))))
        (take 64 (rseq (vec (get-in model [:control :history-order] []))))))

(defn same-value?
  "Committed-value equality for override detection: numbers by ==, so an
  integer re-coerced to a double (1 vs 1.0) is not a write; vectors and maps
  (colors, gradient stops) elementwise."
  [a b]
  (cond
    (and (number? a) (number? b)) (== a b)
    (and (vector? a) (vector? b)) (and (= (count a) (count b)) (every? true? (map same-value? a b)))
    (and (map? a) (map? b)) (and (= (set (keys a)) (set (keys b))) (every? #(same-value? (get a %) (get b %)) (keys a)))
    :else (= a b)))

(defn- hold-ramps
  "Commit each cancelled ramp's current frame value, so the destination stays
  where the ramp left it instead of returning to the value it started from."
  [model ramps now]
  (let [beat (get-in model [:transport :beat] 0)]
    (reduce (fn [m ramp]
              (let [value (try (second (sample-ramp m ramp beat now)) (catch Exception _ nil))]
                (if (or (nil? value) (same-value? value (current-value m (:destination ramp))))
                  m
                  (commit-ramp m ramp now value "/hold"))))
            model ramps)))

(defn hold
  "Commit the current frame value of the running ramps on these destinations
  and keep the ramps. A gesture that begins on a ramped parameter takes over
  from where the ramp is; the override that follows cancels the ramp."
  [model destinations now]
  (let [wanted (set (map destination-key destinations))]
    (hold-ramps model (filterv #(wanted (destination-key (:destination %))) (get-in model [:event-subscribers :ramps]))
                now)))

(defn advance-ramps
  "Settle ramp lifecycles on the owner: a committed write to a ramp's
  destination by anyone cancels it with the writer's reason; a finished ramp
  commits its exact target through the shared reducer. Per-frame values are a
  frame overlay (frame-overlays), never committed."
  [model now]
  (if-not (seq (get-in model [:event-subscribers :ramps]))
    model
    (let [beat (get-in model [:transport :beat] 0)]
      (loop [model model [ramp & more :as pending] (get-in model [:event-subscribers :ramps]) kept []]
        (if-not (seq pending)
          (assoc-in model [:event-subscribers :ramps] kept)
          (let [destination (:destination ramp)]
            (cond
              (and (#{:param :opacity} (:target destination))
                   (or (nil? (get-in model [:instances (:instance-id destination)]))
                       (and (= :param (:target destination)) (nil? (schema-of model destination)))))
              (recur (remember model (ramp-entry model ramp :cancelled {:reason :destination-removed})) more kept)

              (not (same-value? (current-value model destination) (:base ramp)))
              (let [by (last-writer model destination)]
                (recur (remember model (ramp-entry model ramp :cancelled
                                                   {:reason (if (human-source? (:source by)) :human-override :superseded)
                                                    :by (or by {:writer :unknown})}))
                       more kept))

              :else
              (let [[p value] (try (sample-ramp model ramp beat now) (catch Exception _ nil))]
                (cond
                  ;; A ramp that cannot be sampled would otherwise fail every owner tick.
                  (nil? p)
                  (recur (remember model (ramp-entry model ramp :cancelled {:reason :invalid-endpoints})) more kept)

                  (>= p 1.0)
                  (recur (-> (assoc-in model [:event-subscribers :ramps] (into kept more))
                             (commit-ramp ramp now)
                             (remember (ramp-entry model ramp :completed {:value value})))
                         more kept)

                  :else (recur model more (conj kept ramp)))))))))))

(defn frame-overlays
  "Sample every active ramp for the frozen frame: {:params {id {k v}}
  :opacity {id v} :master v :variables {k v}}. Overlay values only reach the
  frame; the committed destination keeps its value until the ramp commits."
  [model now]
  (let [beat (get-in model [:transport :beat] 0)]
    (reduce (fn [out ramp]
              (let [[_ value] (sample-ramp model ramp beat now)
                    {:keys [target instance-id param variable]} (:destination ramp)]
                (case target
                  :param (assoc-in out [:params instance-id param] value)
                  :opacity (assoc-in out [:opacity instance-id] value)
                  :master (assoc out :master value)
                  :variable (assoc-in out [:variables variable] value))))
            {} (get-in model [:event-subscribers :ramps]))))

(defn- overlay-entries
  "[path value] for each overlaid model location: master, instance opacity and
  instance parameters. Variables reach the frame's resolved variables instead."
  [{:keys [master opacity params]}]
  (concat (when (some? master) [[[:master] master]])
          (for [[id v] opacity] [[:instances id :system :opacity] v])
          (for [[id values] params [k v] values] [[:instances id :params k] v])))

(defn with-frame-overlays
  "A frame-only model with ramped master, opacity and parameters applied, for
  stages that read the model directly (spatial cues, the engine)."
  [model overlays]
  (reduce (fn [m [path v]]
            (if (or (= [:master] path) (get-in m [:instances (second path)])) (assoc-in m path v) m))
          model (overlay-entries overlays)))

(defn without-frame-overlays
  "Restore committed values after a stage evaluated a frame model. A location
  the stage itself changed (no longer the overlay value) keeps that change."
  [result committed overlays]
  (reduce (fn [m [path v]]
            (cond
              (not (or (= [:master] path) (get-in m [:instances (second path)]))) m
              (not (same-value? v (get-in m path))) m
              (contains? (get-in committed (pop path)) (peek path)) (assoc-in m path (get-in committed path))
              :else (update-in m (pop path) dissoc (peek path))))
          result (overlay-entries overlays)))

;; Playback ----------------------------------------------------------------

(defn- command-base [model run now]
  {:show-id (:show-id model) :epoch (:epoch model) :time-ms now :source (run-source run)})

(defn- command-id [model run n]
  (if (:scheduled run)
    (str "sched/" (:epoch model) "/" (:id run) "/" n)
    (str "sub/" (:epoch model) "/" (:id run) "/" (:index run) "/" n)))

(defn- apply-step
  "Admit one step command. A destination names the ramp an accepted write
  replaces; a rejected write leaves that ramp running."
  ([model run base cmd-id cmd] (apply-step model run base cmd-id cmd nil))
  ([model run base cmd-id cmd destination]
   (let [result (submit-command model (merge base cmd {:command-id cmd-id}))]
     (if (= :accepted (get-in result [:ack :status]))
       (cond-> (:model result) destination (supersede destination (run-event-id run)))
       (report model run (get-in result [:ack :message] "Action rejected"))))))

(defn- perform-target [model run action id base cmd-id now]
  (let [destination (when (#{:param :opacity} (destination-target action))
                      (cond-> {:target (destination-target action) :instance-id id}
                        (= :param (destination-target action)) (assoc :param (:param action))))]
    (if (= :param/ramp (:op action))
      (try (start-ramp model run action destination now)
           (catch clojure.lang.ExceptionInfo error (report model run (.getMessage error))))
      (apply-step model run base cmd-id (command action id) destination))))

(defn- select-and-perform [model run action now budget]
  (let [seed (get-in model [:event-subscribers :seed])
        human? (human-source? (run-source run))
        [seed targets] (if (:targets run) [seed (:targets run)]
                         (chosen seed (candidates model action {:exact? human?}) (get-in action [:select :pick])))
        offset (:offset run 0) stop (min (count targets) (+ offset budget))
        base (command-base model run now)
        model (reduce (fn [model n]
                        (let [id (nth targets n)]
                          ;; Checked again when due: a non-human poster never acts on a protected instance.
                          (if (and (not human?) (instance-target? action) (protected-instance? model id))
                            (report model run "Scene and show-control instances stay with the operator")
                            (perform-target model run action id base (command-id model run n) now))))
                      (assoc-in model [:event-subscribers :seed] seed) (range offset stop))
        model (if (and (:scheduled run) (empty? targets))
                (report model run (if (and (not human?) (some #(protected-instance? model %) (get-in action [:select :ids])))
                                    "Scene and show-control instances stay with the operator"
                                    "No current target matches this scheduled action"))
                model)]
    [model (when (< stop (count targets)) (assoc run :targets targets :offset stop))
     (- budget (max 1 (- stop offset)))]))

(defn- perform-global [model run action now]
  (let [base (command-base model run now)
        cmd-id (command-id model run 0)
        target (destination-target action)
        value (:value action)]
    (case (:op action)
      :blackout/set (apply-step model run base cmd-id {:op :blackout/set :value value})
      ;; Macros run as owner commands with the poster's source in the receipt.
      (:macro/run :macro/stop) (apply-step model run base cmd-id {:op (:op action) :macro-id (:macro-id action)})
      :param/ramp (start-ramp model run action (cond-> {:target target}
                                                 (= :variable target) (assoc :variable (:variable action))) now)
      (:master/set :param/set :variable/set)
      (case target
        :master (apply-step model run base cmd-id {:op :master/set :value value} {:target :master})
        :variable (apply-step model run base cmd-id {:op :variable/set :variable (:variable action) :value value}
                              {:target :variable :variable (:variable action)})))))

(defn- perform [model run action now budget]
  (cond
    (= :lighting/scene (:op action))
    [(try
       (let [{:keys [event subscriber index]} run
             {:keys [event-path gate scene]} action
             target (when event-path (get-in event event-path))
             request (if event-path (assoc scene :target target) scene)
             {:keys [path above below]} gate
             value (when path (get-in event path))
             latch [:extensions ::lighting-gates [subscriber index (:group-id scene)]]
             armed? (not (true? (get-in model latch)))]
         (cond
           (and event-path (not (registry/finite-number? target))) model
           (nil? gate) (lighting-scenes/apply-request model request now)
           (not (registry/finite-number? value)) model
           (and armed? (>= value above)) (assoc-in (lighting-scenes/apply-request model request now) latch true)
           (<= value below) (assoc-in model latch false)
           :else model))
       (catch clojure.lang.ExceptionInfo error (report model run (.getMessage error) (error-details error))))
     nil (dec budget)]

    (or (contains? action :select) (#{:cue/launch :effect/launch} (:op action)) (instance-ops (:op action)))
    (select-and-perform model run action now budget)

    :else [(perform-global model run action now) nil (dec budget)]))

(defn- following [run meter]
  (let [i (inc (:index run))]
    (when-let [step (get (:steps run) i)]
      (let [origin (get-in run [:event :time :beat] (:due-beat run))
            [due anchor] (step-due step (:due-beat run) origin meter)]
        (cond-> (-> run (assoc :index i :due-beat due) (dissoc :waiting? :targets :offset :anchor))
          (= :absolute anchor) (assoc :anchor anchor))))))

(defn callback-token [model run] [(:epoch model) (:id run) (:index run)])

(defn- callback-result [model run result]
  (let [edits (:edits result)
        current (:variables model)
        conflict? (some (fn [[k {:keys [before]}]] (not= (find current k) before)) edits)]
    (if-let [message (or (:error result) (when conflict? "A show variable changed while the callback was running"))]
      [(report model run message (:diagnostic result)) run]
      [(reduce-kv (fn [m k {:keys [after]}]
                    (if after (assoc-in m [:variables k] (val after)) (update m :variables dissoc k)))
                  model edits)
       (let [n (inc (:index run))]
         (update run :steps #(into (subvec % 0 n) (concat (:actions result) (subvec % n)))))])))

(defn- run-entry [model run status extra]
  (let [action (get (:steps run) (:index run))
        scheduled (:scheduled run)]
    (merge (cond-> {:event-id (run-event-id run) :op (:op action) :category (get categories (:op action))
                    :status status :due-beat (double (:due-beat run))
                    :at-beat (double (get-in model [:transport :beat] 0)) :time-ms (:time-ms model)
                    :source (if scheduled (:source scheduled) (run-source run))}
             (:label scheduled) (assoc :label (:label scheduled))
             (:subscriber run) (assoc :subscriber-id (:subscriber run)))
           extra)))

(defn- record-fired [model before run]
  (if (:scheduled run)
    (let [error (get-in model [:event-subscribers :last-error])]
      (remember model (run-entry model run (if (identical? error before) :fired :rejected)
                                 (when-not (identical? error before) {:message (:message error)}))))
    model))

(defn- advance-runs [model now results]
  (if-not (seq (get-in model [:event-subscribers :runs])) model
    (let [meter (get-in model [:transport :meter]) beat (get-in model [:transport :beat])
          ;; Due order, then admission order. The queue is rewritten in due order each
          ;; cycle, so ties use the admission serial, as the projection does.
          runs (sort-by (juxt :due-beat :id) (get-in model [:event-subscribers :runs]))]
      (loop [model model pending (seq runs) keep-runs [] budget 128]
        (if-let [run (first pending)]
          (let [action (get (:steps run) (:index run))
                callback? (= :callback (:op action))
                result (when callback? (get results (callback-token model run)))]
            (cond
              (zero? budget) (assoc-in model [:event-subscribers :runs] (into keep-runs pending))
              (> (:due-beat run) beat) (recur model (next pending) (conj keep-runs run) budget)
              (and callback? (nil? result))
              (recur model (next pending) (conj keep-runs (assoc run :waiting? true)) budget)
              :else
              (let [before (get-in model [:event-subscribers :last-error])
                    [model next-run budget remainder?]
                    (try
                      (if callback?
                        (let [[model updated] (callback-result model run result)] [model (following updated meter) (dec budget) false])
                        (let [[model remainder budget] (perform model run action now budget)]
                          [model (or remainder (following run meter)) budget (some? remainder)]))
                      (catch Throwable error
                        [(report model run (.getMessage error) (error-details error)) nil (dec budget) false]))
                    model (if remainder? model (record-fired model before run))]
                (recur model (if next-run (cons next-run (next pending)) (next pending)) keep-runs budget))))
          (assoc-in model [:event-subscribers :runs] keep-runs))))))

(defn advance
  "Apply up to 128 commands per cycle, then settle ramp lifecycles. A large
  :all selection keeps its exact candidate list and cursor across ticks. Other
  runs can progress during a callback. Due runs fire in due-beat order, then in
  admission order."
  [model now results]
  (-> (advance-runs model now results) (advance-ramps now)))

;; Scheduling commands -----------------------------------------------------

(defn- portable-source [source]
  (if (map? source)
    (select-keys source [:adapter :session-id :type :device-id :operator-id :bridge-id :run-id])
    source))

(defn- registry-key [entries id]
  ;; JSON keyword normalization must still address string-keyed definitions.
  (if (contains? entries id) id
      (some #(when (= (if (keyword? id) (subs (str id) 1) (str id)) (if (keyword? %) (subs (str %) 1) (str %))) %)
            (keys entries))))

(defn- resolve-references [model action]
  (let [entries (case (:op action) :cue/launch (get-in model [:registry :cues])
                      :effect/launch (get-in model [:registry :effects]) nil)]
    (if (and entries (get-in action [:select :ids]))
      (update-in action [:select :ids]
                 (partial mapv #(or (registry-key entries %)
                                    (throw (ex-info (str "Unknown " (name (namespace (:op action))) " " %) {:id %})))))
      action)))

(defn- macro-key [model id]
  (registry-key (:compositions model) id))

(defn- check-references [model action]
  (let [{:keys [op select]} action
        ids (:ids select)]
    (when (#{:macro/run :macro/stop} op)
      (check (macro-key model (:macro-id action)) "Unknown macro" {:macro-id (:macro-id action)}))
    ;; Exact running instance IDs are checked now; cue/effect selectors resolve when due.
    (when (= :param (destination-target action))
      (doseq [id ids :let [instance (get-in model [:instances id])] :when instance
              :let [schema (get-in instance [:definition :params (:param action)])
                    value (if (= :param/ramp op) (:to action) (:value action))]]
        (check schema "Unknown parameter on that instance" {:instance-id id :param (:param action)})
        (check (params/valid-value? schema value) "Value does not satisfy the parameter type"
               {:instance-id id :param (:param action)})
        (when (= :param/ramp op)
          (check (rampable-types (:type schema)) "This parameter type cannot ramp; use :param/set"
                 {:instance-id id :param (:param action)}))))))

(defn- agent-source? [source] (and (map? source) (#{:agent "agent"} (:adapter source))))

(defn- check-authority [source action]
  ;; Human master and blackout stay authoritative over agent plans.
  (check (not (and (agent-source? source)
                   (or (#{:master/set :blackout/set} (:op action))
                       (= :master (destination-target action)))))
         "Human master and blackout stay authoritative: agents cannot schedule master or blackout changes"
         {:op (:op action)}))

(defn- check-protected [model source action]
  ;; Scene-owner and show-control instances stay with the human operator.
  (when (and (not (human-source? source)) (instance-target? action))
    (doseq [id (get-in action [:select :ids])]
      (check (not (protected-instance? model id))
             "Scene and show-control instances stay with the operator" {:instance-id id}))))

(defn- scheduled-ids
  "IDs that cannot be reused: pending events, running ramps and recent outcomes."
  [s]
  (-> (set (keep #(get-in % [:scheduled :event-id]) (:runs s)))
      (into (map :event-id) (:ramps s))
      (into (map :event-id) (:recent s))))

(defn- position-text [at] (pr-str at))

(defn- prepare-event [model command beat meter index event]
  (try
    (check (map? event) "An event must be a map" {})
    (let [id (id-text (or (:id event) (str (:command-id command) "/" index)))
          label (:label event)
          action (->> (validate-action (normalize-action (dissoc event :id :label)))
                      (resolve-references model))]
      (check (identifier? id) "Event ID must be a keyword or nonblank string" {})
      (check (or (nil? label) (and (string? label) (<= (count label) 128))) "Label must be text up to 128 characters" {})
      (check (not= :callback (:op action)) "Scheduled events use built-in actions; register middleware as a subscriber" {})
      (check-authority (:source command) action)
      (check-protected model (:source command) action)
      (check-references model action)
      (let [[due anchor] (step-due action beat beat meter)]
        (check (or (not (contains? action :at)) (>= due beat))
               (str "Position " (position-text (:at action)) " (beat " (double due) ") has already passed; the transport is at beat "
                    (double beat) ". Positions are zero-based: {:bar 0} is the first bar.")
               {:due-beat due :beat beat})
        {:id id :label label :action action :due due :anchor anchor}))
    (catch clojure.lang.ExceptionInfo error
      (throw (ex-info (str "Event " index ": " (.getMessage error))
                      {:index index :errors [{:path [:events index] :message (.getMessage error)}]})))))

(defn post
  "Validate a batch atomically and append each event as a one-step run on the
  shared action queue. One invalid event rejects the whole batch."
  [model {:keys [events source command-id] :as command}]
  (check (and (vector? events) (<= 1 (count events) batch-limit))
         (str "Post between 1 and " batch-limit " events") {})
  (let [s (state model)
        meter (get-in model [:transport :meter])
        beat (get-in model [:transport :beat] 0)
        prepared (vec (map-indexed #(prepare-event model command beat meter %1 %2) events))
        ids (mapv :id prepared)
        taken (scheduled-ids s)]
    (check (apply distinct? ids) "Event IDs must be unique within a batch" {:event-ids ids})
    (check (not-any? taken ids) "That event ID is already scheduled or recently used; choose a new ID"
           {:event-ids (filterv taken ids)})
    (check (<= (+ (count (filter :scheduled (:runs s))) (count prepared)) scheduled-limit)
           "The schedule is full" {:limit scheduled-limit})
    {:event-ids ids
     :due-beats (mapv (comp double :due) prepared)
     :model (reduce (fn [m {:keys [id label action due anchor]}]
                      (let [serial (inc (get-in m [:event-subscribers :next-run]))]
                        (-> m
                            (assoc-in [:event-subscribers :next-run] serial)
                            (update-in [:event-subscribers :runs] conj
                                       {:id serial :steps [action] :index 0 :due-beat due :anchor anchor
                                        :event {:type :schedule/posted :category :control :data {:event-id id}}
                                        :scheduled (cond-> {:event-id id :source (portable-source source)
                                                            :category (get categories (:op action))
                                                            :command-id command-id :posted-beat (double beat)}
                                                     label (assoc :label label))}))))
                    (assoc model :event-subscribers (merge empty-state s)) prepared)}))

(defn- action-destination [action]
  (let [target (destination-target action)]
    (cond-> {:target target}
      (= :param target) (assoc :param (:param action))
      (= :variable target) (assoc :variable (:variable action)))))

(defn- destination-match? [wanted destination ids]
  (and (= (:target wanted) (:target destination))
       (or (nil? (:param wanted)) (= (:param wanted) (:param destination)))
       (or (nil? (:variable wanted)) (= (:variable wanted) (:variable destination)))
       (or (nil? (:instance-id wanted)) (= (:instance-id wanted) (:instance-id destination))
           (contains? (set ids) (:instance-id wanted)))))

(defn- same-source? [wanted actual]
  (and (map? actual) (map? wanted)
       (= (update-vals wanted id-text) (update-vals (select-keys actual (keys wanted)) id-text))))

(defn- selection-match? [{:keys [event-ids posted-by label destination]} event-id run-source run-label dest ids]
  (and (or (nil? event-ids) (contains? (set (map id-text event-ids)) event-id))
       (or (nil? posted-by) (same-source? posted-by run-source))
       (or (nil? label) (= label run-label))
       (or (nil? destination) (and dest (destination-match? destination dest ids)))))

(defn- owner?
  "Did this requester post it? Same adapter and session. A requester without
  an adapter has no identity, so it owns nothing."
  [requester posted]
  (and (map? requester) (map? posted) (some? (:adapter requester))
       (= (id-text (:adapter requester)) (id-text (:adapter posted)))
       (= (id-text (:session-id requester)) (id-text (:session-id posted)))))

(defn cancel
  "Cancel scheduled events and running ramps selected by every supplied
  criterion: :event-ids, :posted-by (a source submap), :label or :destination.
  A human operator surface may cancel anything; any other requester (an agent,
  a reactive action) only what it posted itself. A cancelled ramp commits its
  current frame value, so its destination holds where the ramp left it."
  [model {:keys [event-ids reason source] :as request}]
  ;; A nil criterion is not a criterion: a request with none would match everything.
  (check (some #(some? (get request %)) [:event-ids :posted-by :label :destination])
         "Cancel by event IDs, source, label or destination" {})
  (check (or (nil? event-ids) (and (vector? event-ids) (every? identifier? event-ids))) "Event IDs must be a vector" {})
  (let [criteria (select-keys request [:event-ids :posted-by :label :destination])
        ;; JSON callers name the target, parameter and variable as text, as events do.
        textual #(cond-> % (contains? % :target) (update :target textual-keyword)
                   (contains? % :param) (update :param textual-keyword)
                   (contains? % :variable) (update :variable textual-keyword))
        _ (check (or (nil? (:destination criteria)) (map? (:destination criteria)))
                 "A destination is a map with :target and optional :instance-id, :param or :variable" {})
        criteria (cond-> criteria (:destination criteria) (update :destination textual))
        ;; A non-human requester reaches only its own work, whatever criteria it supplies.
        may-cancel? (if (human-source? source) (constantly true) #(owner? source %))
        reason (or reason :operator-cancel)
        s (state model)
        run-hit? (fn [run]
                   (when-let [{:keys [event-id label] posted :source} (:scheduled run)]
                     (let [action (first (:steps run))]
                       (and (may-cancel? posted)
                            (selection-match? criteria event-id posted label
                                              (when (destination-target action) (action-destination action))
                                              (get-in action [:select :ids]))))))
        {gone true kept false} (group-by (comp boolean run-hit?) (:runs s))
        model (reduce #(remember %1 (run-entry %1 %2 :cancelled {:reason reason}))
                      (assoc-in model [:event-subscribers :runs] (vec kept)) gone)
        ramp-hit? #(and (may-cancel? (:source %))
                        (selection-match? criteria (:event-id %) (:source %) (:label %) (:destination %) nil))
        stopped (filterv ramp-hit? (get-in model [:event-subscribers :ramps]))]
    {:model (-> (remove-ramps model ramp-hit? :cancelled {:reason reason})
                (hold-ramps stopped (:time-ms request (:time-ms model 0))))
     :event-ids (vec (distinct (concat (map run-event-id gone) (map :event-id stopped))))}))

(defn move
  "Reschedule one pending event to new timing: {:event-id .. :at|:in|:grid ..}.
  Only its poster or a human operator surface may move it."
  [model {:keys [event-id source] :as request}]
  (let [timing (select-keys request [:at :in :grid])
        meter (get-in model [:transport :meter])
        beat (get-in model [:transport :beat] 0)
        runs (get-in model [:event-subscribers :runs] [])
        index (first (keep-indexed #(when (= (id-text event-id) (get-in %2 [:scheduled :event-id])) %1) runs))]
    (check index "No pending event has that ID" {:event-id event-id})
    (check (or (human-source? source) (owner? source (get-in runs [index :scheduled :source])))
           "Only the event's poster or a human operator can move it" {:event-id event-id})
    (check (= 1 (count timing)) "Move needs one of :at, :in or :grid" {})
    (let [action (validate-action (merge (apply dissoc (first (:steps (nth runs index))) timing-keys) timing))
          [due anchor] (step-due action beat beat meter)]
      (check (or (not (contains? action :at)) (>= due beat))
             (str "Position " (position-text (:at action)) " (beat " (double due) ") has already passed") {})
      [(assoc-in model [:event-subscribers :runs index]
                 (assoc (nth runs index) :steps [action] :due-beat due :anchor anchor))
       (double due)])))

(defn apply-operation
  "Pure [model command] operation for control/apply-domain-command: schedule
  edits, validated show-variable writes and macro start/stop."
  [model {:keys [op] :as command}]
  (try
    (case op
      :schedule/post
      (let [{updated :model ids :event-ids due :due-beats} (post model command)]
        {:model updated :events [{:type :schedule/posted :event-ids ids}] :result {:event-ids ids :due-beats due}})
      :schedule/cancel
      (let [{:keys [model event-ids]} (cancel model command)]
        {:model model :events [{:type :schedule/cancelled :event-ids event-ids}] :result {:event-ids event-ids}})
      :schedule/move
      (let [[model due] (move model command)]
        {:model model :events [{:type :schedule/moved :event-ids [(id-text (:event-id command))]}]
         :result {:event-ids [(id-text (:event-id command))] :due-beats [due]}})
      :variable/set
      (let [{:keys [variable value]} command]
        (check (not (reserved-variable? variable))
               "That show variable is owned by the runtime; choose your own variable name" {:variable variable})
        (check (and (some? value) (registry/data? value)) "A variable value must be portable data" {})
        {:model (assoc-in model [:variables variable] value)
         :events [{:type :variable/changed :variable variable :value value}]})
      (:macro/run :macro/stop)
      (let [id (macro-key model (:macro-id command))]
        (check id "Unknown macro" {:macro-id (:macro-id command)})
        {:model (if (= :macro/run op)
                  (composition/start model id (:time-ms command (:time-ms model)))
                  (composition/stop model id (:time-ms command (:time-ms model))))
         :events [{:type (if (= :macro/run op) :macro/started :macro/stopped) :macro-id id}]}))
    (catch clojure.lang.ExceptionInfo error
      (if (:control/error (ex-data error))
        (throw error)
        (throw (ex-info (.getMessage error) {:control/error :invalid-schedule :details (ex-data error)}))))))

(defn schedulable?
  "A direct command that should enter the queue: a ramp, or a vocabulary action
  carrying :at/:in/:grid timing (cue launches with :grid keep cue_timing)."
  [{:keys [op] :as command}]
  (or (= :param/ramp op)
      (and (some #{op} actions) (not= :callback op)
           (or (contains? command :at) (contains? command :in)
               (and (contains? command :grid) (not= :cue/launch op))))))

(defn as-post
  "Wrap one direct command as a single-event :schedule/post."
  [command]
  (let [envelope [:command-id :show-id :epoch :source :time-ms :expected-revision]
        event (cond-> (apply dissoc command :event-id :label envelope)
                (:event-id command) (assoc :id (:event-id command))
                (:label command) (assoc :label (:label command)))]
    (assoc (select-keys command envelope) :op :schedule/post :events [event])))

;; Clock discontinuities, restart and restore -------------------------------

(def phase-tolerance
  "A forward clock correction at most this many beats (a Link phase nudge)
  is continuous: absolute events it crosses fire now instead of being missed."
  1/4)

(defn rebase-clock
  "Apply a seek, loop or clock-source change. Relative waits and beat ramps keep
  their remaining musical time. :grid steps re-quantize to the next boundary
  from the new position. Absolute (:at) events before the new beat are
  reported missed and dropped, so a later return cannot fire them twice. A
  small forward correction (<= phase-tolerance) leaves them due now."
  [model offset new-beat]
  (let [runs (get-in model [:event-subscribers :runs] [])
        meter (get-in model [:transport :meter])
        small? (<= 0 offset phase-tolerance)
        step #(get (:steps %) (:index %))
        grid? #(and (= :absolute (:anchor %)) (contains? (step %) :grid))
        missed? #(and (= :absolute (:anchor %)) (not (grid? %)) (not small?) (< (:due-beat %) new-beat))
        model (reduce #(remember %1 (run-entry %1 %2 :missed {:reason :clock-discontinuity}))
                      model (filter missed? runs))]
    (-> model
        (assoc-in [:event-subscribers :runs]
                  (into [] (comp (remove missed?)
                                 (map #(cond
                                         (and (grid? %) (not small?))
                                         (assoc % :due-beat (grid-beat (:grid (step %)) new-beat meter))
                                         (= :absolute (:anchor %)) %
                                         :else (cond-> (update % :due-beat + offset)
                                                 (get-in % [:event :time :beat]) (update-in [:event :time :beat] + offset)))))
                        runs))
        (update :event-subscribers
                (fn [s] (cond-> s
                          (seq (:ramps s))
                          (update :ramps (partial mapv #(if (= :beats (:unit %))
                                                          (-> % (update :start + offset) (update :end + offset)) %)))))))))

(defn hold-clock
  "Keep millisecond ramps where they were across held-ms of stopped transport.
  Beat ramps and waits already stand still with the transport beat."
  [model held-ms]
  (if (seq (get-in model [:event-subscribers :ramps]))
    (update-in model [:event-subscribers :ramps]
               (partial mapv #(if (= :ms (:unit %))
                                (-> % (update :start + held-ms) (update :end + held-ms)) %)))
    model))

(defn restart [model]
  (cond-> model (:event-subscribers model) (update :event-subscribers assoc :runs [] :ramps [] :recent [])))

(defn restore [s instance-ids]
  (letfn [(rekey-ids [ids] (mapv #(get instance-ids % %) ids))
          (rekey [action] (cond-> action (and (instance-target? action) (contains? (:select action) :ids))
                           (update-in [:select :ids] rekey-ids)))
          (rekey-run [run]
            (cond-> (update run :steps (partial mapv rekey))
              (and (:targets run) (instance-target? (get (:steps run) (:index run)))) (update :targets rekey-ids)))]
    (cond-> (-> s
                (update :subscribers #(mapv (fn [d] (update d :actions (partial mapv rekey))) %))
                (update :runs #(into [] (comp (remove :waiting?) (map rekey-run)) %)))
      (seq (:ramps s))
      (update :ramps (partial mapv (fn [ramp] (cond-> ramp (get-in ramp [:destination :instance-id])
                                                (update-in [:destination :instance-id] #(get instance-ids % %)))))))))

;; Projection ----------------------------------------------------------------

(defn- action-summary [action]
  (cond-> (select-keys action [:param :variable :to :over :curve :value :select :macro-id :at :in :grid :delay])
    (destination-target action) (assoc :target (destination-target action))
    (:select action) (update :select select-keys [:ids :categories :page-id :pick])))

(defn- upcoming-entry [run]
  (let [action (get (:steps run) (:index run))
        scheduled (:scheduled run)]
    (merge (action-summary action)
           (cond-> {:event-id (run-event-id run) :origin (if scheduled :scheduled :subscriber)
                    :op (:op action) :category (get categories (:op action))
                    :due-beat (double (:due-beat run)) :anchor (or (:anchor run) :relative)
                    :step (:index run) :steps (count (:steps run))
                    :source (if scheduled (:source scheduled) (run-source run))}
             (:label scheduled) (assoc :label (:label scheduled))
             (:posted-beat scheduled) (assoc :posted-beat (:posted-beat scheduled))
             (:subscriber run) (assoc :subscriber-id (:subscriber run))))))

(defn- ramp-view [ramp]
  (-> (select-keys ramp [:event-id :label :source :category :destination :from :to :curve :unit :over])
      (assoc :start (double (:start ramp)) :end (double (:end ramp)))))

(defn schedule-view
  "Stable projection shared by Studio, Push, REPL and agents (docs/scheduler.md).
  It changes only when the schedule changes; consumers derive a ramp's live
  progress from :start/:end and the transport beat (or :time-ms for :ms)."
  [model]
  (let [s (state model)
        {:keys [beat bpm meter playing?]} (:transport model)]
    {:beat (double (or beat 0)) :bpm bpm :playing? (boolean playing?)
     :meter (time/normalize-meter meter)
     :upcoming (mapv upcoming-entry (sort-by (juxt :due-beat :id) (:runs s)))
     :ramps (mapv ramp-view (:ramps s []))
     :recent (vec (:recent s []))}))

(defn ramp-progress
  "Read-time [progress value] for a ramp at the model's current clock."
  [model ramp]
  (sample-ramp model ramp (get-in model [:transport :beat] 0) (:time-ms model 0)))

(defn- short-destination [{:keys [target instance-id param variable]}]
  (case target
    :param (str instance-id "/" (name param))
    :opacity (str instance-id "/opacity")
    :variable (str "var/" (name variable))
    (name target)))

(defn- round3 [x] (if (number? x) (/ (Math/round (* 1000.0 (double x))) 1000.0) x))

(defn compact-view
  "Low-token read-back for agents planning a phrase ahead. Positions are
  zero-based transport beats; see :legend."
  [model]
  (let [{:keys [beat meter upcoming recent bpm]} (schedule-view model)
        bpb (:beats-per-bar meter)]
    {:legend {:upcoming [:due-beat :event-id :op :label] :ramps [:event-id :destination :progress :value :to :end]
              :recent [:event-id :status :reason]}
     :beat (round3 beat) :bar (long (Math/floor (/ beat bpb))) :beats-per-bar bpb :bpm bpm
     :upcoming (mapv (fn [{:keys [due-beat event-id op label]}] (cond-> [(round3 due-beat) event-id op] label (conj label)))
                     (take 64 upcoming))
     :ramps (mapv (fn [r] (let [[p value] (ramp-progress model r)]
                            [(:event-id r) (short-destination (:destination r)) (round3 p)
                             (if (vector? value) (mapv round3 value) (round3 value)) (:to r) (round3 (:end r))]))
                  (get-in model [:event-subscribers :ramps]))
     :recent (mapv (fn [{:keys [event-id status reason]}] (cond-> [event-id status] reason (conj reason)))
                   (take-last 8 recent))}))
