(ns tolglow.showcase.timeline
  "A recorded musical timeline and its pure player.

  A timeline is data: a tempo, a meter, a length and an ordered vector of
  events. Every event has a zero-based transport position in `:at` and one
  action. `advance` takes the committed show model and the beat the next frame
  will reach, and returns the show operations that are due. It reads no clock
  and keeps no state of its own, so the same model and beat always give the
  same operations.

  The event vocabulary follows the beat-clock scheduler in #645 (`:at`,
  `:cue/launch`, `:instance/end`, `:param/set`, `:param/ramp`, `:master/set`,
  `:macro/run`, `:macro/stop`), so these events can be posted to it unchanged
  once it is on master. Until then this player lowers them to the commands
  master already accepts. Three actions have no scheduler counterpart yet:
  `:modulation/bind`, `:modulation/unbind` and `:subscriber/save` /
  `:subscriber/remove`."
  (:require [afterglow.render.easing :as easing]
            [afterglow.time :as time]
            [tolglow.registry :as registry]))

(def actions
  #{:cue/launch :instance/end :param/set :param/ramp :master/set :macro/run :macro/stop
    :modulation/bind :modulation/unbind :subscriber/save :subscriber/remove})

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

(defn- event-problems [model index {:keys [op at cue-id param macro-id over] :as event}]
  (let [cue (get-in model [:registry :cues cue-id])
        effect (when cue (registry/cue-effect (:registry model) cue))
        schema (get-in effect [:params param])
        value-ok? #(and schema (registry/valid-value? schema %))]
    (cond
      (not (actions op)) [(problem index "Unknown timeline action")]
      (not (position? at)) [(problem index "Give :at as a zero-based phrase, bar and/or beat")]
      (and (#{:cue/launch :instance/end :param/set :param/ramp :modulation/bind :modulation/unbind} op)
           (not= :master (:target event)) (nil? cue))
      [(problem index "The action names a cue that is not registered")]
      (and (= :param/set op) (not= :master (:target event)) (not (value-ok? (:value event))))
      [(problem index "The value does not fit the cue parameter")]
      (and (= :param/ramp op) (not= :master (:target event)) (not (value-ok? (:to event))))
      [(problem index "The ramp target does not fit the cue parameter")]
      (and (= :param/ramp op) (or (seq (time/validate-duration over)) (contains? over :ms)
                                  (not (pos? (val (first over))))))
      [(problem index "A ramp runs over positive beats, bars or phrases")]
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

(defn prepare
  "Validate a timeline and resolve every position to beats. Events keep their
  authored index and are ordered by beat, then by that index."
  [timeline model]
  (let [problems (validate timeline model)]
    (when (seq problems) (throw (ex-info "Invalid timeline" {:errors problems})))
    (let [meter (:meter timeline)
          events (->> (:events timeline)
                      (map-indexed (fn [index event]
                                     (assoc event :index index :beat (position->beats (:at event) meter))))
                      (sort-by (juxt :beat :index))
                      vec)
          length (position->beats (:length timeline) meter)]
      (when-not (every? #(<= (:beat %) length) events)
        (throw (ex-info "Invalid timeline" {:errors [{:path [:length] :message "An event lies beyond the timeline's length"}]})))
      {:id (:id timeline) :meter meter :length length :events events})))

(defn start
  "Player state for one pass. `origin` is the transport beat of timeline beat 0."
  [origin pass]
  {:origin (double origin) :pass pass :cursor 0 :waiting [] :ramps [] :sent 0 :missed [] :done? false})

(defn- running
  "Running instances of a cue, oldest first."
  [model cue-id]
  (->> (vals (:instances model))
       (filter #(and (= cue-id (:cue-id %)) (not= :releasing (:phase %))))
       (sort-by :order)))

(defn- newest-order [model cue-id]
  (reduce max -1 (map :order (running model cue-id))))

(defn- interpolate [from to amount]
  (cond (and (integer? from) (integer? to)) (Math/round (double (+ from (* amount (- to from)))))
        (number? from) (+ from (* amount (- to from)))
        :else (mapv #(+ %1 (* amount (- %2 %1))) from to)))

(defn- lower
  "Show operations for one due action, or :wait while its cue has no instance."
  [model {:keys [op cue-id param] :as action}]
  (let [instances (running model cue-id)
        newest (last instances)
        each (fn [build] (if (seq instances) (mapv build instances) :wait))]
    (case op
      :cue/launch [[:command {:op :cue/launch :cue-id cue-id :launch (:launch action {})}]]
      :instance/end (if (seq instances)
                      (mapv (fn [instance] [:command {:op :instance/end :instance-id (:id instance)}]) instances)
                      [])
      :param/set (if (= :master (:target action))
                   [[:command {:op :master/set :value (:value action)}]]
                   (each (fn [instance]
                           [:command {:op :param/set :instance-id (:id instance) :param param :value (:value action)}])))
      :master/set [[:command {:op :master/set :value (:value action)}]]
      :macro/run [[:composition-run {:id (:macro-id action)}]]
      :macro/stop [[:composition-stop {:id (:macro-id action)}]]
      :subscriber/save [[:event-subscriber {:op :save :subscriber (:subscriber action)}]]
      :subscriber/remove [[:event-subscriber {:op :remove :id (:id action)}]]
      :modulation/bind (if (and newest (> (:order newest) (:after action -1)))
                         [[:modulation-bind {:instance-id (:id newest) :param param :binding (:binding action)}]]
                         :wait)
      :modulation/unbind (if (and newest (get-in newest [:parameter-bindings param]))
                           [[:modulation-unbind {:instance-id (:id newest) :param param}]]
                           []))))

(defn- expand
  "A launch with :sources becomes the launch plus one bind per parameter, each
  waiting for an instance newer than those already running."
  [model {:keys [op cue-id sources] :as event}]
  (if (and (= :cue/launch op) (seq sources))
    (into [(dissoc event :sources)]
          (map (fn [[param binding]]
                 (assoc (select-keys event [:index :beat :cue-id])
                        :op :modulation/bind :param param :binding binding
                        :after (newest-order model cue-id))))
          (sort-by (comp str key) sources))
    [event]))

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

(defn- sample-ramp
  "One frame of a ramp. The first frame with a running instance captures where
  each instance starts. The last frame writes the exact target."
  [model local {:keys [cue-id param target to start end curve] :as ramp}]
  (let [finished? (>= local end)
        amount (if finished? 1.0 (easing/sample (or curve :linear) (/ (- local start) (- end start))))
        master? (= :master target)
        instances (when-not master? (running model cue-id))
        from (if master?
               {:master (get-in ramp [:from :master] (:master model))}
               (reduce (fn [from instance]
                         (cond-> from (not (contains? from (:id instance)))
                                 (assoc (:id instance) (get-in instance [:params param]))))
                       (:from ramp {}) instances))
        operations (if master?
                     [[:command {:op :master/set :value (if finished? to (interpolate (:master from) to amount))}]]
                     (mapv (fn [instance]
                             [:command {:op :param/set :instance-id (:id instance) :param param
                                        :value (if finished? to (interpolate (get from (:id instance)) to amount))}])
                           instances))]
    {:operations operations :ramp (when-not finished? (assoc ramp :from from))}))

(defn- identified
  "Give every command a stable ID from the timeline, pass and send order."
  [id pass sent operations]
  (map-indexed (fn [offset [type payload]]
                 (if (= :command type)
                   [type (assoc payload :command-id (str "timeline/" (name id) "/" pass "/" (+ sent offset))
                                :source {:adapter :timeline :session-id (name id)})]
                   [type payload]))
               operations))

(defn advance
  "Return `{:state next :operations [[type payload] ...]}` for the frame that
  will reach transport beat `horizon`. Operations are `tolglow.show/submit!`
  arguments in the order they must be applied."
  [{:keys [id length events meter]} {:keys [origin pass cursor waiting ramps sent missed done?] :as state} model horizon]
  (if done?
    {:state state :operations []}
    (let [local (- horizon origin)
          due-count (count (take-while #(<= (:beat %) local) (subvec events cursor)))
          due (subvec events cursor (+ cursor due-count))
          new-ramps (for [{:keys [beat over] :as event} due :when (= :param/ramp (:op event))]
                      (assoc (select-keys event [:index :cue-id :param :target :to :curve])
                             :start beat :end (+ beat (time/duration->beats over meter))))
          actions (into (vec waiting) (mapcat (partial expand model)) (remove #(= :param/ramp (:op %)) due))
          settled (settle model actions)
          sampled (map (partial sample-ramp model local) (concat ramps new-ramps))
          operations (into (:operations settled) (mapcat :operations) sampled)
          finished? (and (>= local length) (= (+ cursor due-count) (count events)))]
      {:state (assoc state
                     :cursor (+ cursor due-count)
                     :waiting (:waiting settled)
                     :ramps (vec (keep :ramp sampled))
                     :missed (into missed (:missed settled))
                     :sent (+ sent (count operations))
                     :done? (boolean finished?))
       :operations (vec (identified id pass sent operations))})))
