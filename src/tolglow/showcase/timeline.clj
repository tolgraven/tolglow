(ns tolglow.showcase.timeline
  "A recorded musical timeline as data, and the pure functions that play it.

  A timeline has a tempo, a meter, a length and an ordered vector of events.
  Every event has a zero-based position in `:at` and one action, in the
  vocabulary of the beat-clock scheduler (docs/scheduler.md).

  `posts` turns the events the scheduler can run into `:schedule/post` batches
  at absolute transport beats. The show owner then fires them and runs every
  ramp. A few actions have no scheduler counterpart yet, and `advance` returns
  those as ordinary show operations when their beat comes:

  - `:modulation/bind` and `:modulation/unbind`, and the `:sources` a launch
    asks for, because a waveform binding is not a scheduler action;
  - `:subscriber/save` and `:subscriber/remove`;
  - a launch or end of a show-control cue such as a spatial volume, which the
    scheduler leaves to the operator, and a launch with `:targets`.

  Neither function reads a clock or keeps state of its own."
  (:require [afterglow.render.easing :as easing]
            [afterglow.time :as time]
            [tolglow.registry :as registry]))

(def actions
  #{:cue/launch :instance/end :param/set :param/ramp :master/set :macro/run :macro/stop
    :modulation/bind :modulation/unbind :subscriber/save :subscriber/remove})

(def batch-limit
  "Events the scheduler admits in one `:schedule/post`."
  64)

(def ^:private patience
  "Frames an instance-addressed action waits for its cue to be running."
  16)

(defn position->beats
  "Zero-based `{:phrase p :bar b :beat x}` to transport beats. The parts add."
  [{:keys [phrase bar beat] :or {phrase 0 bar 0 beat 0}} {:keys [beats-per-bar bars-per-phrase]}]
  (double (+ beat (* beats-per-bar (+ bar (* bars-per-phrase phrase))))))

(defn- problem [index message] {:path [:events index] :message message})

(defn- position? [at]
  (and (map? at) (seq at) (every? #{:phrase :bar :beat} (keys at))
       (every? #(and (registry/finite-number? %) (not (neg? %))) (vals at))))

(defn- curve? [curve]
  (try (easing/sample (or curve :linear) 0.5) true (catch Exception _ false)))

(defn- event-problems [model index {:keys [op at cue-id param macro-id over] :as event}]
  (let [cue (get-in model [:registry :cues cue-id])
        effect (when cue (registry/cue-effect (:registry model) cue))
        schema (get-in effect [:params param])
        value-ok? #(and schema (registry/valid-value? schema %))]
    (cond
      (not (actions op)) [(problem index "Unknown timeline action")]
      (not (position? at)) [(problem index "Give :at as a zero-based phrase, bar and/or beat")]
      (and (#{:cue/launch :instance/end :param/set :param/ramp :modulation/bind :modulation/unbind} op) (nil? cue))
      [(problem index "The action names a cue that is not registered")]
      (and (= :param/set op) (not (value-ok? (:value event))))
      [(problem index "The value does not fit the cue parameter")]
      (and (= :param/ramp op) (not (value-ok? (:to event))))
      [(problem index "The ramp target does not fit the cue parameter")]
      (and (= :param/ramp op) (or (seq (time/validate-duration over)) (contains? over :ms)
                                  (not (pos? (val (first over)))) (not (curve? (:curve event)))))
      [(problem index "A ramp runs over positive beats, bars or phrases with a known curve")]
      (and (= :master/set op) (not (and (registry/finite-number? (:value event)) (<= 0 (:value event) 1))))
      [(problem index "The master level is between zero and one")]
      (and (= :cue/launch op) (some #(not (contains? (:params effect) %)) (keys (get-in event [:launch :params]))))
      [(problem index "The launch sets a parameter the cue does not have")]
      (and (#{:macro/run :macro/stop} op) (nil? (get-in model [:compositions macro-id])))
      [(problem index "The action names a macro or sequence that is not saved")]
      :else [])))

(defn validate
  "Path/message problems for a timeline against the show it will play on."
  [{:keys [id bpm meter length events]} model]
  (let [meter-ok? (and (map? meter) (pos? (:beats-per-bar meter 0)) (pos? (:bars-per-phrase meter 0)))]
    (vec (concat
          (when-not (keyword? id) [{:path [:id] :message "A timeline needs a keyword ID"}])
          (when-not (and (registry/finite-number? bpm) (<= 1 bpm 1000)) [{:path [:bpm] :message "BPM must be in [1,1000]"}])
          (when-not meter-ok? [{:path [:meter] :message "The meter needs beats per bar and bars per phrase"}])
          (when-not (position? length) [{:path [:length] :message "Give the length as phrases, bars and/or beats"}])
          (when-not (vector? events) [{:path [:events] :message "Events must be a vector"}])
          (when (and meter-ok? (vector? events))
            (mapcat (fn [[index event]] (event-problems model index event)) (map-indexed vector events)))))))

(defn- scheduler-launch? [launch]
  (and (every? #{:mode :duration :params} (keys launch))
       (#{:one-shot :persistent} (:mode launch :persistent))))

(defn- route
  "Where an event plays: on the beat-clock scheduler, or as a direct command."
  [model {:keys [op cue-id launch]}]
  (let [cue (get-in model [:registry :cues cue-id])
        operator-only? (or (:show-control? cue)
                           (:show-control? (registry/cue-effect (:registry model) cue)))]
    (cond
      (#{:modulation/bind :modulation/unbind :subscriber/save :subscriber/remove} op) :command
      (and (#{:cue/launch :instance/end} op) operator-only?) :command
      (and (= :cue/launch op) (not (scheduler-launch? launch))) :command
      :else :scheduler)))

(defn- source-binds
  "The binds a launch asks for with `:sources`, in parameter order."
  [{:keys [sources] :as event}]
  (map (fn [[param binding]]
         (assoc (select-keys event [:index :beat :cue-id])
                :op :modulation/bind :param param :binding binding :after-launch? true))
       (sort-by (comp str key) sources)))

(defn prepare
  "Validate a timeline and resolve every position to beats. Returns the
  scheduler's events and the direct commands, each ordered by beat and then by
  authored index."
  [timeline model]
  (let [problems (validate timeline model)]
    (when (seq problems) (throw (ex-info "Invalid timeline" {:errors problems})))
    (let [meter (:meter timeline)
          events (->> (:events timeline)
                      (map-indexed (fn [index event]
                                     (assoc event :index index :beat (position->beats (:at event) meter))))
                      (sort-by (juxt :beat :index)))
          length (position->beats (:length timeline) meter)
          routed (group-by (partial route model) events)]
      (when-not (every? #(<= (:beat %) length) events)
        (throw (ex-info "Invalid timeline" {:errors [{:path [:length] :message "An event lies beyond the timeline's length"}]})))
      {:id (:id timeline) :meter meter :length length
       :scheduled (vec (:scheduler routed))
       :direct (vec (sort-by (juxt :beat :index)
                             (concat (map #(dissoc % :sources) (:command routed))
                                     (mapcat source-binds events))))})))

(defn- label [{:keys [op cue-id param macro-id]}]
  (case op
    :cue/launch (str "Launch " (name cue-id))
    :instance/end (str "End " (name cue-id))
    :param/set (str "Set " (name cue-id) " " (name param))
    :param/ramp (str "Ramp " (name cue-id) " " (name param))
    :master/set "Master"
    :macro/run (str "Run " (name macro-id))
    :macro/stop (str "Stop " (name macro-id))))

(defn- scheduled-event [id origin pass {:keys [op index beat] :as event}]
  (merge {:id (str (name id) "/" pass "/" index) :label (label event) :op op :at {:beat (+ origin beat)}}
         (case op
           :cue/launch (cond-> (select-keys event [:cue-id])
                         (seq (:launch event)) (assoc :launch (:launch event)))
           :instance/end (select-keys event [:cue-id])
           :param/set (select-keys event [:cue-id :param :value])
           :param/ramp (select-keys event [:cue-id :param :to :over :curve])
           :master/set (select-keys event [:value])
           (:macro/run :macro/stop) (select-keys event [:macro-id]))))

(defn source
  "The provenance every command and scheduled event of a timeline carries."
  [id]
  {:adapter :timeline :session-id (name id)})

(defn posts
  "`:schedule/post` commands for one pass whose beat 0 is transport beat
  `origin`. Events keep their order, so events on the same beat fire in the
  order they were authored."
  [{:keys [id scheduled]} origin pass]
  (vec (map-indexed
        (fn [batch events]
          {:op :schedule/post :command-id (str "timeline/" (name id) "/" pass "/post/" batch)
           :source (source id) :events (mapv (partial scheduled-event id origin pass) events)})
        (partition-all batch-limit scheduled))))

(defn start
  "Direct-command state for one pass. `origin` is the transport beat of
  timeline beat 0."
  [origin pass]
  {:origin (double origin) :pass pass :cursor 0 :waiting [] :sent 0 :missed [] :done? false})

(defn- running
  "Running instances of a cue, oldest first."
  [model cue-id]
  (->> (vals (:instances model))
       (filter #(and (= cue-id (:cue-id %)) (not= :releasing (:phase %))))
       (sort-by :order)))

(defn- lower
  "Show operations for one due action, or :wait while its cue has no instance."
  [model {:keys [op cue-id param] :as action}]
  (let [instances (running model cue-id)
        newest (last instances)]
    (case op
      :cue/launch [[:command {:op :cue/launch :cue-id cue-id :launch (:launch action {})}]]
      :instance/end (mapv (fn [instance] [:command {:op :instance/end :instance-id (:id instance)}]) instances)
      :subscriber/save [[:event-subscriber {:op :save :subscriber (:subscriber action)}]]
      :subscriber/remove [[:event-subscriber {:op :remove :id (:id action)}]]
      :modulation/bind (if (and newest (>= (:started-at-beat newest 0) (- (:since action 0) 1.0e-9)))
                         [[:modulation-bind {:instance-id (:id newest) :param param :binding (:binding action)}]]
                         :wait)
      :modulation/unbind (if (and newest (get-in newest [:parameter-bindings param]))
                           [[:modulation-unbind {:instance-id (:id newest) :param param}]]
                           []))))

(defn- arm
  "A bind that follows a launch waits for an instance started at or after the
  launch's own beat, whichever of the two the owner applies first."
  [origin action]
  (cond-> action
    (:after-launch? action) (-> (dissoc :after-launch?) (assoc :since (+ origin (:beat action))))))

(defn- settle
  "Lower each action. Those that must wait are kept until `patience` runs out."
  [model actions]
  (reduce (fn [result action]
            (let [operations (lower model action)]
              (cond
                (not= :wait operations) (update result :operations into operations)
                (< (:attempts action 0) patience) (update result :waiting conj (update action :attempts (fnil inc 0)))
                :else (update result :missed conj (select-keys action [:index :op :cue-id :param])))))
          {:operations [] :waiting [] :missed []} actions))

(defn- identified
  "Give every command a stable ID from the timeline, pass and send order."
  [id pass sent operations]
  (map-indexed (fn [offset [type payload]]
                 (if (= :command type)
                   [type (assoc payload :command-id (str "timeline/" (name id) "/" pass "/" (+ sent offset))
                                :source (source id))]
                   [type payload]))
               operations))

(defn advance
  "Return `{:state next :operations [[type payload] ...]}`: the direct commands
  for the frame that will reach transport beat `horizon`. Operations are
  `tolglow.show/submit!` arguments in the order they must be applied."
  [{:keys [id length direct]} {:keys [origin pass cursor waiting sent missed done?] :as state} model horizon]
  (if done?
    {:state state :operations []}
    (let [local (- horizon origin)
          due-count (count (take-while #(<= (:beat %) local) (subvec direct cursor)))
          due (subvec direct cursor (+ cursor due-count))
          settled (settle model (into (vec waiting) (map (partial arm origin)) due))
          operations (:operations settled)]
      {:state (assoc state
                     :cursor (+ cursor due-count)
                     :waiting (:waiting settled)
                     :missed (into missed (:missed settled))
                     :sent (+ sent (count operations))
                     :done? (and (>= local length) (= (+ cursor due-count) (count direct))))
       :operations (vec (identified id pass sent operations))})))
