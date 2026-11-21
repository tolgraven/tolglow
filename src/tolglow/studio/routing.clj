(ns tolglow.studio.routing
  "Pure owner-side admission and command dispatch for the signal patch bay.
  Raw adapters supply observations; this boundary captures show epoch/clock,
  bounds inventory/pending input, validates logical destinations and uses the
  same command reducer as other controls. No device or transport is opened."
  (:require [afterglow.control.model :as control]
            [afterglow.time :as time]
            [tolglow.registry :as registry]
            [tolglow.integrations.patch :as patch]))

(def max-ports 128)
(def max-pending 1024)
(def max-diagnostics 64)
(def audio-keys [:rms :peak :kick-rms :energy-envelope :low-rms :mid-rms :high-rms :bass-pitch-hz :bass-pitch-confidence])

(defn- fail! [message data] (throw (ex-info message (assoc data :routing/error :invalid-routing))))
(defn- id? [x] (or (keyword? x) (and (string? x) (<= 1 (count x) 128))))
(defn- state [model]
  (merge {:spec nil :state nil :enabled? false :generation 0 :ports {} :pending [] :last-audio-time nil :diagnostics []}
         (:routing model)))
(defn- clock [model time-ms]
  (time/frame-clock {:time-ms time-ms :transport (:transport model)}))
(defn- diagnostic [routing entry]
  (update routing :diagnostics #(vec (take-last max-diagnostics (conj (vec %) entry)))))

(defn- resolve-id [entries id]
  (cond (contains? entries id) id
        (and (keyword? id) (contains? entries (subs (str id) 1))) (subs (str id) 1)
        (and (string? id) (contains? entries (keyword id))) (keyword id)
        :else nil))

(defn- destination [model id destination]
  (let [cue-id (resolve-id (get-in model [:registry :cues]) (:cue-id destination))
        cue (get-in model [:registry :cues cue-id])
        current (get-in model [:registry :effects (:effect-id cue)])
        effect (if-let [version (:effect-version cue)]
                 (or (get-in model [:registry :versions [:effects (:effect-id cue) version]])
                     (when (= version (:version current)) current)) current)
        param (resolve-id (:params effect) (:param destination))
        schema (get-in effect [:params param])
        [low high] (:range destination)
        bounds (:bounds schema)]
    (when-not (and cue effect param (#{:number :integer} (:type schema))
                   (or (nil? bounds) (<= (first bounds) low high (second bounds)))
                   (or (not= :integer (:type schema))
                       (and (== low (Math/rint (double low))) (== high (Math/rint (double high))))))
      (fail! "Patch destination must name a numeric cue parameter and fit its bounds"
             {:destination-id id :cue-id (:cue-id destination) :param (:param destination)}))
    (assoc destination :cue-id cue-id :param param)))

(defn save
  "Validate a new patch graph and capture its exact revision. Save does not
  implicitly enable a new patch. Changed graphs need an increasing revision;
  an identical graph is idempotent and retains captured routing progress."
  [model specification time-ms]
  (let [specification (patch/validate-patch specification)
        specification (update specification :destinations
                              #(into {} (map (fn [[id target]] [id (destination model id target)])) %))
        old (state model) previous (:spec old)]
    (when (and previous (= (:id previous) (:id specification)) (not= previous specification)
               (<= (:revision specification) (:revision previous)))
      (fail! "Changed patch content needs a newer revision" {:revision (:revision specification)}))
    (if (= previous specification) model
      (-> model
          (assoc :routing (assoc old :spec specification :generation (inc (:generation old))
                                 :state (patch/new-state specification (clock model time-ms) (:epoch model))
                                 :pending [] :diagnostics []))
          (update :revision inc)))))

(defn set-enabled
  "Enable from a fresh capture boundary or disable without changing parameters.
  Fresh samples are required after re-enabling; old held gates are not replayed."
  [model enabled? time-ms]
  (when-not (boolean? enabled?) (fail! "Patch enabled flag must be boolean" {}))
  (let [routing (state model)]
    (when (and enabled? (nil? (:spec routing))) (fail! "Save a patch before enabling it" {}))
    (if (= enabled? (:enabled? routing)) model
      (-> model
          (assoc :routing (cond-> (assoc routing :enabled? enabled? :generation (inc (:generation routing))
                                                :pending [] :diagnostics [])
                            (:spec routing) (assoc :state (patch/new-state (:spec routing) (clock model time-ms) (:epoch model)))))
          (update :revision inc)))))

(defn restart
  "Discard active input/tails on a true show restart, retaining graph settings.
  Routing stays disabled until explicitly enabled and supplied fresh input."
  [model time-ms]
  (if-not (:routing model) model
    (let [old (state model)]
      (assoc model :routing
             (cond-> (assoc old :enabled? false :generation (inc (:generation old))
                                :ports {} :pending [] :last-audio-time nil :diagnostics [])
               (:spec old) (assoc :state (patch/new-state (:spec old) (clock model time-ms) (:epoch model))))))))

(defn- bounded-ports [ports]
  (into {} (take-last max-ports (sort-by (fn [[id value]] [(:observed-at-ms value) (str id)]) ports))))

(defn- admit [model observations]
  (let [captured (clock model (get-in model [:transport :time-ms] (:time-ms model)))
        routing
        (reduce
          (fn [routing observation]
            (let [{:keys [source-id event-id value gate? confidence observed-at-ms kind]} observation]
              (if-not (and (map? observation) (registry/data? observation) (id? source-id) (id? event-id)
                            (time/finite-number? value) (boolean? gate?)
                            (time/finite-number? confidence) (<= 0 confidence 1)
                            (time/finite-number? observed-at-ms) (<= 0 observed-at-ms)
                            (#{:midi :automation :audio :control} kind))
                (diagnostic routing {:code :invalid-observation :source-id (when (id? source-id) source-id)})
                (let [admitted (assoc observation :epoch (:epoch model) :clock captured)
                      port (-> (select-keys admitted [:source-id :kind :value :gate? :confidence :observed-at-ms :clock :port])
                               (assoc :range (or (get-in observation [:port :range]) (if (= :midi kind) [0 127] [0.0 1.0]))))
                      routing (update routing :ports #(bounded-ports (assoc % source-id port)))]
                  (cond
                    (not (:enabled? routing)) routing
                    (not (contains? (get-in routing [:spec :sources]) source-id)) routing
                    (>= (count (:pending routing)) max-pending)
                    (diagnostic routing {:code :input-overflow :source-id source-id})
                    :else (update routing :pending conj admitted))))))
          (state model) observations)]
    (assoc model :routing routing)))

(defn admit-sync
  "Admit the actual raw sync observation events, preserving their ordered
  gate transitions. Caller has already checked callback generation/show epoch."
  [model events]
  (let [observations (into [] (comp (filter #(= :sync/observation (:type %))) (map :observation)) events)]
    (if (seq observations) (admit model observations) model)))

(defn- audio-observation [ports features timestamp run-id key]
  (let [pitch? (= :bass-pitch-hz key)
        source-id (str "audio/" (name key))
        measured (get features key)
        ;; Loss of periodicity closes an open pitch gate. Keep its last Hz
        ;; value for the release envelope so silence never becomes a fake note.
        closed? (and pitch? (contains? features key) (nil? measured) (get-in ports [source-id :gate?]))
        value (if closed? (get-in ports [source-id :value]) measured)
        confidence (get features (if pitch? :bass-pitch-confidence :confidence) 1.0)]
    (when (and (time/finite-number? value) (not (neg? value))
               (time/finite-number? confidence))
      {:source-id source-id :event-id (str "audio/" run-id "/" timestamp "/" (name key))
       :kind :audio :value value :gate? (and (not closed?) (pos? value))
       :confidence (max 0.0 (min 1.0 confidence)) :origin :audio :observed-at-ms timestamp
       :port (cond-> {:signal key :transport :audio}
               pitch? (assoc :range [20.0 180.0] :unit :hz))})))

(defn admit-audio
  "Publish measured scalar ports at the captured physical time. The show clock
  and epoch govern parameter and envelope updates."
  [model features]
  (let [timestamp (:time-ms features) previous (get-in model [:routing :last-audio-time])]
    (if-not (and (time/finite-number? timestamp) (<= 0 timestamp)
                 (or (nil? previous) (> timestamp previous))) model
      (let [run-id (get-in features [:capture :run-id] "analysis")
            observations (keep #(audio-observation (get-in model [:routing :ports])
                                                   features timestamp run-id %) audio-keys)]
        (assoc-in (admit model observations) [:routing :last-audio-time] timestamp)))))

(defn- bindings [model]
  (reduce (fn [result instance]
            (if (= :running (:phase instance)) (assoc result (:cue-id instance) (:id instance)) result))
          {} (sort-by (juxt :order :id) (vals (:instances model)))))

(defn- command-value [model command]
  (let [schema (get-in model [:instances (:instance-id command) :definition :params (:param command)])
        value (:value command)]
    (if (= :integer (:type schema))
      (let [[low high] (or (:bounds schema) [Long/MIN_VALUE Long/MAX_VALUE])
            value (long (max (Math/ceil (double low)) (min (Math/floor (double high)) (Math/round (double value)))))]
        (assoc command :value value)) command)))

(defn advance
  "Evaluate routing once before modulation and dispatch changed parameter
  commands through the supplied common [model command time-ms] reducer.
  Rejected writes produce diagnostics and retry on later frames. The default
  dispatcher is the real shared control reducer; no effects are evaluated here."
  ([model time-ms] (advance model time-ms (fn [model command _] (control/apply-command model command))))
  ([model time-ms dispatch]
   (if-not (:routing model) model
     (let [routing (state model)
           fresh? (and (:spec routing) (not= (:epoch model) (get-in routing [:state :epoch])))
           routing (cond-> routing
                     fresh? (assoc :state (patch/new-state (:spec routing) (clock model time-ms) (:epoch model))
                                   :pending [] :diagnostics [{:code :show-restarted}]))
           model (assoc model :routing routing)]
       (if-not (and (:enabled? routing) (:spec routing))
         (assoc-in model [:routing :pending] [])
         (let [result (patch/step (:spec routing) (:state routing)
                                  {:clock (clock model time-ms) :epoch (:epoch model) :observations (:pending routing)
                                   :bindings (bindings model) :active-instances (set (keys (:instances model)))
                                   :envelope {:show-id (:show-id model) :epoch (:epoch model)
                                              :command-id (str "routing/" (:epoch model) "/" (:generation routing)
                                                               "/" (get-in routing [:state :frame]))}})
               model (update model :routing #(reduce diagnostic
                                                      (assoc % :state (:state result) :pending []) (:diagnostics result)))]
           (reduce
             (fn [model command]
               (let [command (command-value model command)
                     result (dispatch model command time-ms)]
                 (if (= :accepted (get-in result [:ack :status])) (:model result)
                   (-> model
                       (update :routing diagnostic {:code :command-rejected :command (select-keys command [:instance-id :param :value])
                                                    :ack (:ack result)})
                       (update-in [:routing :state :last-values]
                                  #(into {} (remove (fn [[[destination instance] _]]
                                                     (and (= instance (:instance-id command))
                                                          (= (:param command) (get-in routing [:spec :destinations destination :param]))))) %))))))
             model (:commands result))))))))

(defn snapshot [model]
  (let [routing (state model)]
    {:enabled? (:enabled? routing) :spec (:spec routing) :source (when (:spec routing) (pr-str (:spec routing)))
     :sources (mapv val (sort-by (comp str key) (:ports routing)))
     :values (mapv (fn [[[destination instance] value]] {:destination-id destination :instance-id instance :value value})
                   (sort-by (comp pr-str key) (get-in routing [:state :last-values])))
     :diagnostics (:diagnostics routing)}))

(defn validate-state
  "Validate the complete portable host routing field before save/import."
  [routing]
  (when-not (and (map? routing) (every? #{:spec :state :enabled? :generation :ports :pending :last-audio-time :diagnostics} (keys routing))
                 (integer? (:generation routing)) (<= 0 (:generation routing))
                 (boolean? (:enabled? routing)) (map? (:ports routing)) (<= (count (:ports routing)) max-ports)
                 (vector? (:pending routing)) (<= (count (:pending routing)) max-pending)
                 (vector? (:diagnostics routing)) (<= (count (:diagnostics routing)) max-diagnostics)
                 (or (nil? (:last-audio-time routing))
                     (and (time/finite-number? (:last-audio-time routing)) (<= 0 (:last-audio-time routing)))))
    (fail! "Invalid bounded host routing data" {}))
  (if (:spec routing) (patch/validate-state (:spec routing) (:state routing))
    (when (or (:state routing) (:enabled? routing) (seq (:pending routing)))
      (fail! "Unconfigured routing cannot contain active execution" {})))
  (doseq [[id port] (:ports routing)]
    (when-not (and (id? id) (= id (:source-id port)) (registry/data? port)
                   (#{:midi :automation :audio :control} (:kind port))
                   (time/finite-number? (:value port)) (boolean? (:gate? port))
                   (time/finite-number? (:confidence port)) (<= 0 (:confidence port) 1)
                   (time/finite-number? (:observed-at-ms port)) (<= 0 (:observed-at-ms port))
                   (time/finite-number? (get-in port [:clock :time-ms])) (<= 0 (get-in port [:clock :time-ms]))
                   (time/finite-number? (get-in port [:clock :beat]))
                   (vector? (:range port)) (= 2 (count (:range port)))
                   (every? time/finite-number? (:range port)) (apply < (:range port)))
      (fail! "Invalid learned source port" {:source-id id})))
  (doseq [observation (:pending routing)]
    (when-not (and (registry/data? observation) (id? (:event-id observation))
                   (contains? (get-in routing [:spec :sources]) (:source-id observation))
                   (= (get-in routing [:state :epoch]) (:epoch observation))
                   (time/finite-number? (:value observation)) (boolean? (:gate? observation))
                   (time/finite-number? (:confidence observation)) (<= 0 (:confidence observation) 1)
                   (time/finite-number? (:observed-at-ms observation)) (<= 0 (:observed-at-ms observation))
                   (time/finite-number? (get-in observation [:clock :time-ms]))
                   (<= 0 (get-in observation [:clock :time-ms]))
                   (time/finite-number? (get-in observation [:clock :beat])))
      (fail! "Invalid queued source observation" {})))
  (when-not (registry/data? (:diagnostics routing)) (fail! "Routing diagnostics must be portable data" {}))
  routing)

(defn restore-state [routing {:keys [epoch] :as identity}]
  (validate-state routing)
  (cond-> (update routing :pending #(mapv (fn [observation] (assoc observation :epoch epoch)) %))
    (:spec routing) (update :state #(patch/restore-state (:spec routing) % identity))))
