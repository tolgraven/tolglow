(ns tolglow.event-subscribers
  "Pure admission and playback of event-driven show actions."
  (:require [clojure.string :as str]
            [tolglow.spatial :as spatial]
            [afterglow.reactive.registry :as reactive]
            [afterglow.time :as time]
            [afterglow.runtime.events :as staging]
            [afterglow.runtime.faults :as faults]
            [tolglow.automation.random :as random]
            [tolglow.registry :as registry]
            [tolglow.lighting-scenes :as lighting-scenes]
            [tolglow.lighting-levels :as lighting-levels]))

(def actions [:lighting/scene :cue/launch :effect/launch :instance/end :instance/kill :instance/opacity :callback])
(def event-types
  {:spatial [:spatial/enter :spatial/exit :spatial/bounce :spatial/invalid-volume]
   :reactive [:reactive/fault :event/handler-failed]
   :atmosphere [:fog/below-target :fog/in-band :fog/above-target :fog/target-changed :fog/demand-started :fog/demand-ended]
   :lighting (into [:lighting/activity] (concat lighting-levels/group-event-types lighting-levels/rig-event-types))
   :audio [:audio/kick :audio/beat :audio/energy :audio/breakdown-start :audio/breakdown-end]
   :show [:show/restored]
   :clock [:clock/beat :clock/bar :clock/phrase]
   :control [:instance/launched :instance/removed :instance/released :instance/system-changed
             :effect/retry-requested :cue/deleted :cue/params-saved :cue/saved-params-cleared
             :transport/changed :master/changed :blackout/changed :param/changed :show/restarted
             :registry/published :page/changed :page/deleted]
   :sensor [:sensor/armed :sensor/calibrated :sensor/dropout :sensor/gesture :sensor/input-error
            :sensor/sample-rejected :sensor/threshold]
   :sync [:sync/clock :sync/observation :sync/error :sync/input-overflow :sync/control-unavailable
          :peer/found :peer/lost :live/execution :live/status :surface/clear-feedback :surface/mode]
   :command [:command/accepted :command/rejected]
   :effect [:effect/intent :instance/finished :effect/failed :effect/recovered :effect/reload-failed :effect/reloaded]})

(def ^:private empty-state {:subscribers [] :runs [] :next-run 0 :seed 1 :dropped 0})
(defn- state [model] (:event-subscribers model empty-state))
(defn- check [condition message data]
  (when-not condition (throw (ex-info message data))))
(defn- identifier? [id] (or (keyword? id) (and (string? id) (not (str/blank? id)))))
(defn- only-keys? [m allowed] (and (map? m) (every? allowed (keys m))))
(defn- nonnegative-int? [v] (and (integer? v) (not (neg? v))))
(defn- musical [duration]
  (check (and (map? duration) (every? #{:beats :bars :phrases} (keys duration))
              (empty? (time/validate-duration duration))) "Expected a musical duration" {:duration duration})
  duration)

(defn validate-action [{:keys [op select delay implementation launch value] :as action}]
  (check (and (some #{op} actions)
              (only-keys? action (case op
                                   :lighting/scene #{:op :scene :event-path :gate :delay}
                                   :callback #{:op :implementation :delay}
                                   (:cue/launch :effect/launch) #{:op :select :launch :delay}
                                   :instance/opacity #{:op :select :value :delay}
                                   #{:op :select :delay})))
         "Unknown event action or option" {:op op})
  (when (= op :lighting/scene)
    (lighting-scenes/validate-request (:scene action))
    (when-let [{:keys [path above below] :as gate} (:gate action)]
      (check (and (map? gate) (vector? path) (seq path) (every? keyword? path)
                  (registry/finite-number? above) (registry/finite-number? below) (< below above))
             "A scene gate needs an event path and distinct upper/lower thresholds" {:gate gate}))
    (when-let [path (:event-path action)]
      (check (and (vector? path) (seq path) (every? keyword? path)) "Use a path within the emitted event" {:path path})))
  (when (contains? action :delay) (musical delay))
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
  (when (= op :instance/opacity)
    (check (and (registry/finite-number? value) (<= 0 value 1)) "Opacity must be between zero and one" {}))
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

(defn validate-definition [{:keys [id enabled? match actions] :as registration}]
  (check (and (only-keys? registration #{:id :enabled? :match :actions})
              (keyword? id) (instance? Boolean (get registration :enabled? true))
              (only-keys? (or match {}) #{:type :category :emitter-id :volume-id :fixture-id})
              (every? keyword? (vals (select-keys match [:type :category])))
              (every? identifier? (vals (select-keys match [:emitter-id :volume-id :fixture-id])))
              (vector? actions) (<= 1 (count actions) 32))
         "Invalid event subscriber definition" {:id id})
  (run! validate-action actions)
  registration)

(defn validate-state [s]
  (check (and (map? s) (every? #{:subscribers :runs :next-run :seed :dropped :last-error} (keys s))
              (nonnegative-int? (:next-run s)) (integer? (:seed s)) (nonnegative-int? (:dropped s))
              (vector? (:subscribers s)) (<= (count (:subscribers s)) 128)
              (= (count (:subscribers s)) (count (set (map :id (:subscribers s)))))
              (vector? (:runs s)) (<= (count (:runs s)) 256))
         "Invalid event subscriber state" {})
  (run! validate-definition (:subscribers s))
  (let [definitions (set (map :id (:subscribers s)))]
    (doseq [{:keys [id subscriber event steps index due-beat waiting?] :as run} (:runs s)]
      (check (and (only-keys? run #{:id :subscriber :event :steps :index :due-beat :waiting? :targets :offset})
                  (or (not (contains? run :targets))
                      (and (vector? (:targets run)) (every? identifier? (:targets run))
                           (nonnegative-int? (:offset run)) (< (:offset run) (count (:targets run)))))
                  (nonnegative-int? id) (<= id (:next-run s)) (definitions subscriber)
                  (map? event) (keyword? (:type event)) (vector? steps) (<= 1 (count steps) 32)
                  (nonnegative-int? index) (< index (count steps))
                  (registry/finite-number? due-beat) (or (nil? waiting?) (instance? Boolean waiting?)))
             "Invalid pending event sequence" {:id id})
      (run! validate-action steps))
    (check (= (count (:runs s)) (count (set (map :id (:runs s))))) "Duplicate pending sequence id" {}))
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
       (or (nil? (:emitter-id match)) (= (:id emitter) (:emitter-id match)))))

(defn- triggered? [{:keys [emitter data]}]
  (or (= :event-subscriber (:id emitter)) (= :event-subscriber (:cause-adapter data))
      (= :event-subscriber (get-in data [:source :adapter]))))

(defn- delay-beats [action meter]
  (if-let [d (:delay action)] (time/duration->beats d meter) 0))

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
              (let [s (get-in out [:model :event-subscribers]) serial (inc (:next-run s))]
                (if (>= (count (:runs s)) 256)
                  (update-in out [:model :event-subscribers :dropped] inc)
                  (-> out
                      (update-in [:deliveries index] (fnil conj []) id)
                      (assoc-in [:model :event-subscribers :next-run] serial)
                      (update-in [:model :event-subscribers :runs] conj
                                 {:id serial :subscriber id :steps actions :index 0 :event event
                                  :due-beat (+ (get-in event [:time :beat] (get-in model [:transport :beat] 0))
                                               (delay-beats (first actions) meter))})))))
            out (filter #(matches? % event) (get-in model [:event-subscribers :subscribers])))))
       {:model model :deliveries {}} events))))

(def ^:private excluded-roles #{:show-control :code :code-cue})
(defn- musical-definition? [entry]
  (and entry (not (or (excluded-roles (:role entry)) (excluded-roles (:kind entry))
                      (:scene-owner entry) (:code-cue? entry) (:show-control? entry) (some excluded-roles (:tags entry))))))

(defn candidates [model {:keys [op select]}]
  (let [{:keys [ids categories page-id]} select
        cues (get-in model [:registry :cues]) effects (get-in model [:registry :effects])
        by-page (when page-id (set (vals (get-in model [:pages page-id :cells]))))
        page-effects (when by-page (set (keep (fn [id] (let [cue (get cues id)] (when (musical-definition? cue) (:effect-id cue)))) by-page)))
        instances? (#{:instance/end :instance/kill :instance/opacity} op)
        rows (cond
               instances? (for [[id i] (:instances model)]
                            [id (get cues (:cue-id i)) (:definition i) (:cue-id i)])
               (= :cue/launch op) (for [[id c] cues] [id c (registry/cue-effect (:registry model) c) id])
               :else (for [[id e] effects] [id nil e nil]))
        ids (when ids (set ids)) categories (when categories (set categories))]
    (->> rows
         (keep (fn [[id cue effect cue-id]]
                 (when (and (musical-definition? effect) (or (nil? cue) (musical-definition? cue))
                            (or (nil? by-page) (if cue-id (contains? by-page cue-id) (contains? page-effects (:id effect))))
                            (or (nil? ids) (contains? ids id)
                                (and instances? (or (contains? ids cue-id) (contains? ids (:id effect)))))
                            (or (nil? categories) (some categories (registry/definition-categories effect))))
                   id)))
         (sort-by pr-str) vec)))

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
    :instance/opacity {:op :instance/opacity-set :instance-id id :value (:value action)}))

(defn error-details [^Throwable error]
  (when-not (faults/recoverable? error) (throw error))
  {:class (.getName (class error))
   :stacktrace (mapv str (take 24 (.getStackTrace error)))})

(defn- report
  ([model run message] (report model run message nil))
  ([model run message diagnostic]
   (let [message (str (or message "Event handler failed"))
         fault (merge (select-keys (get-in run [:event :data]) [:instance-id :effect-id :cue-id])
                      {:type :event/handler-failed :stage :handler :id (:subscriber run)
                       :subscriber-id (:subscriber run) :epoch (:epoch model) :time-ms (:time-ms model)
                       :run (:id run) :step (:index run) :message (subs message 0 (min 256 (count message)))}
                      diagnostic)]
     (cond-> (-> model
                 (assoc-in [:event-subscribers :last-error] (assoc fault :subscriber (:subscriber run)))
                 (update-in [:variables :runtime :handler-faults] #(vec (take-last 64 (conj (vec %) fault)))))
       (not= :event/handler-failed (get-in run [:event :type]))
       (staging/collect :reactive {:kind :subscriber :id (:subscriber run)} [fault])))))

(defn- select-and-perform [model run action now budget]
  (let [seed (get-in model [:event-subscribers :seed])
        [seed targets] (if (:targets run) [seed (:targets run)]
                         (chosen seed (candidates model action) (get-in action [:select :pick])))
        offset (:offset run 0) stop (min (count targets) (+ offset budget))
        base {:show-id (:show-id model) :epoch (:epoch model) :time-ms now
              :source {:adapter :event-subscriber :session-id (str (:subscriber run))}}
        model (reduce (fn [model n]
                        (let [id (nth targets n)
                              cmd (merge base (command action id)
                                         {:command-id (str "sub/" (:epoch model) "/" (:id run) "/" (:index run) "/" n)})
                              result (spatial/apply-command model cmd)]
                          (if (= :accepted (get-in result [:ack :status])) (:model result)
                              (report model run (get-in result [:ack :message] "Action rejected")))))
                      (assoc-in model [:event-subscribers :seed] seed) (range offset stop))]
    [model (when (< stop (count targets)) (assoc run :targets targets :offset stop))
     (- budget (max 1 (- stop offset)))]))

(defn- perform [model run action now budget]
  (if (= :lighting/scene (:op action))
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
    (select-and-perform model run action now budget)))

(defn- following [run meter]
  (let [i (inc (:index run))]
    (when-let [step (get (:steps run) i)]
      (-> run (assoc :index i) (dissoc :waiting? :targets :offset)
          (update :due-beat + (delay-beats step meter))))))

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

(defn advance
  "Apply up to 128 commands per cycle. A large :all selection keeps its exact
  candidate list and cursor across ticks. Other runs can progress during a callback."
  [model now results]
  (if-not (seq (get-in model [:event-subscribers :runs])) model
    (let [meter (get-in model [:transport :meter]) beat (get-in model [:transport :beat])
          runs (get-in model [:event-subscribers :runs])]
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
              (let [[model next-run budget]
                    (try
                      (if callback?
                      (let [[model updated] (callback-result model run result)] [model (following updated meter) (dec budget)])
                      (let [[model remainder budget] (perform model run action now budget)]
                        [model (or remainder (following run meter)) budget]))
                      (catch Throwable error
                        [(report model run (.getMessage error) (error-details error)) nil (dec budget)]))]
                (recur model (if next-run (cons next-run (next pending)) (next pending)) keep-runs budget))))
          (assoc-in model [:event-subscribers :runs] keep-runs))))))

(defn restart [model]
  (cond-> model (:event-subscribers model) (assoc-in [:event-subscribers :runs] [])))

(defn restore [s instance-ids]
  (letfn [(rekey-ids [ids] (mapv #(get instance-ids % %) ids))
          (instance-action? [action] (#{:instance/end :instance/kill :instance/opacity} (:op action)))
          (rekey [action] (cond-> action (and (instance-action? action) (contains? (:select action) :ids))
                           (update-in [:select :ids] rekey-ids)))
          (rekey-run [run]
            (cond-> (update run :steps (partial mapv rekey))
              (and (:targets run) (instance-action? (get (:steps run) (:index run)))) (update :targets rekey-ids)))]
    (-> s
        (update :subscribers #(mapv (fn [d] (update d :actions (partial mapv rekey))) %))
        (update :runs #(into [] (comp (remove :waiting?) (map rekey-run)) %)))))
