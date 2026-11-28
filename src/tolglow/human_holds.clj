(ns tolglow.human-holds
  "Performer holds on show destinations (#854). A human write to a parameter,
  an instance's opacity or a show variable holds that destination against
  agents: while the performer keeps the control (an open gesture) and for a
  grace after release that ends on a bar line. A write with no gesture is
  released at once, so only the grace follows it.

  Which writes are human, and which destinations a write touched, is the
  show's existing human-override rule (`tolglow.show/override-ramps`, with
  `tolglow.event-subscribers/human-source?`); this namespace keeps the hold
  state it starts. A hold is a transport position, so a stopped transport
  holds it. A hold on an instance (any of its parameters or its opacity) also
  covers ending that instance. Pure data on the show model under
  [:event-subscribers :holds]; no clocks or callbacks."
  (:require [afterglow.time :as time]))

(def default-grace-bars
  "Proposed default (operator decision D8): the grace ends on the first bar
  line at least one bar after release."
  1)

(def max-grace-bars 16)

(def settings-scope
  "The show settings scope that holds the grace: {:grace-bars n}."
  :agent-holds)

(defn agent-source?
  "An agent session's source: Studio chat, a tool run or an external agent."
  [source]
  (boolean (and (map? source) (#{:agent "agent"} (:adapter source)))))

(defn validate-settings!
  "Check a {:grace-bars n} settings map: a whole number of bars, 0 to 16. Zero
  ends a hold at release; the hold while touched cannot be switched off."
  [settings]
  (when-not (and (map? settings) (every? #{:grace-bars} (keys settings))
                 (or (not (contains? settings :grace-bars))
                     (let [bars (:grace-bars settings)]
                       (and (integer? bars) (<= 0 bars max-grace-bars)))))
    (throw (ex-info (str "Agent hold settings take :grace-bars, a whole number of bars from 0 to " max-grace-bars)
                    {:settings settings})))
  settings)

(defn grace-bars [model]
  (get-in model [:settings settings-scope :grace-bars] default-grace-bars))

(defn- beat [model] (double (get-in model [:transport :beat] 0)))

(defn- bar-line-at-or-after
  "The first bar line at or after b, counted from beat zero in meter, as the
  scheduler's :grid {:bars 1} counts it."
  [b meter]
  (let [bpb (rationalize (time/duration->beats {:bars 1} meter))
        q (/ (rationalize b) bpb)
        whole (if (ratio? q)
                (let [n (numerator q) d (denominator q) f (quot n d)] (if (neg? n) f (inc f)))
                q)]
    (double (* bpb whole))))

(defn grace-end
  "Beat at which a hold released at release-beat ends: the first bar line at
  least grace-bars bars after release, or the release itself for a zero grace."
  [model release-beat]
  (let [bars (grace-bars model)
        meter (get-in model [:transport :meter])]
    (if (zero? bars)
      (double release-beat)
      (bar-line-at-or-after (+ release-beat (time/duration->beats {:bars bars} meter)) meter))))

(defn destination-key
  "The part of a destination that names it: {:target :param :instance-id ..
  :param ..}, {:target :opacity :instance-id ..} or {:target :variable :variable ..}."
  [destination]
  (select-keys destination [:target :instance-id :param :variable]))

(def ^:private held-targets #{:param :opacity :variable})

(defn- holds [model] (get-in model [:event-subscribers :holds] {}))

(defn- touched? [hold] (some? (:gesture hold)))

(defn- covers?
  "Does hold keep agents off at beat b? A touched control holds every beat
  until it is released; a released one until its grace ends."
  [hold b]
  (or (touched? hold) (< b (:until-beat hold))))

(defn- prune
  "Drop holds whose grace has ended at the transport beat."
  [model]
  (let [b (beat model)
        current (holds model)
        kept (into {} (filter (fn [[_ hold]] (covers? hold b))) current)]
    (cond
      (= (count kept) (count current)) model
      (empty? kept) (update model :event-subscribers dissoc :holds)
      :else (assoc-in model [:event-subscribers :holds] kept))))

(defn- portable-source [source]
  (when (map? source) (not-empty (select-keys source [:adapter :session-id :device-id :operator-id]))))

(defn touch
  "Start or extend holds on the destinations a committed human write touched,
  at the model's transport beat. With a gesture owner (a gesture's begin or
  sample) the hold stays until that gesture ends. Without one the write is
  released at once: the hold lasts for the grace, unless a gesture still
  holds the same destination."
  [model destinations source gesture]
  (let [b (beat model)
        source (portable-source source)
        model (prune model)]
    (reduce (fn [model destination]
              (let [k (destination-key destination)
                    previous (get (holds model) k)]
                (if-not (and (held-targets (:target k)) (or (:instance-id k) (= :variable (:target k))))
                  model
                  (assoc-in model [:event-subscribers :holds k]
                            (cond
                              gesture (cond-> {:destination k :touched-beat b :gesture gesture}
                                        source (assoc :source source))
                              ;; A plain write does not end another surface's open gesture.
                              (touched? previous) previous
                              :else (cond-> {:destination k :touched-beat b :until-beat (grace-end model b)}
                                      source (assoc :source source)))))))
            model destinations)))

(defn release
  "Release the holds a gesture owns: the grace starts at the transport beat.
  A gesture that holds nothing changes nothing."
  [model gesture]
  (let [b (beat model)
        owned (filter (fn [[_ hold]] (= gesture (:gesture hold))) (holds model))]
    (reduce (fn [model [k hold]]
              (assoc-in model [:event-subscribers :holds k]
                        (-> hold (dissoc :gesture) (assoc :released-beat b :until-beat (grace-end model b)))))
            model owned)))

(defn holding
  "The hold that keeps agents off destination at beat b, or nil. An instance
  destination with no parameter (ending an instance) is held by any hold on
  that instance."
  [model destination b]
  (let [k (destination-key destination)
        current (holds model)]
    (if (and (= :instance (:target k)) (:instance-id k))
      (some (fn [[hk hold]] (when (and (= (:instance-id k) (:instance-id hk)) (covers? hold b)) hold))
            (sort-by (comp pr-str key) current))
      (when-let [hold (get current k)] (when (covers? hold b) hold)))))

(defn known-end
  "The earliest beat hold could end: its grace end once released; while the
  control is touched, the grace end if it were released now. A post due
  before this beat is refused; a later one is checked again when due."
  [model hold]
  (if (touched? hold) (grace-end model (beat model)) (:until-beat hold)))

(defn refusal
  "ex-info data for an agent write a hold refuses."
  [hold]
  {:reason :human-held :destination (:destination hold)
   :held-by (:source hold) :touched? (touched? hold)
   :until-beat (:until-beat hold)})

(defn projection
  "The current holds, sorted by destination, as Studio, the REPL and agents
  read them: each with its destination, the human source, the beat it was
  touched, whether the control is still touched and the beat the hold ends
  (nil while touched)."
  [model]
  (let [b (beat model)]
    (->> (holds model)
         (keep (fn [[_ hold]]
                 (when (covers? hold b)
                   (cond-> {:destination (:destination hold) :touched? (touched? hold)
                            :touched-beat (:touched-beat hold) :until-beat (:until-beat hold)}
                     (:source hold) (assoc :source (:source hold))
                     (:released-beat hold) (assoc :released-beat (:released-beat hold))))))
         (sort-by (comp pr-str :destination))
         vec)))

(defn rebase
  "Shift released holds by a clock rebase offset, so each keeps its remaining
  musical time across a seek or loop, as relative waits do."
  [s offset]
  (if (and (seq (:holds s)) (not (zero? offset)))
    (update s :holds update-vals
            (fn [hold] (cond-> (update hold :touched-beat + offset)
                         (:until-beat hold) (update :until-beat + offset)
                         (:released-beat hold) (update :released-beat + offset))))
    s))

(defn valid-state?
  "Saved holds are a map of destination to hold record."
  [holds]
  (and (map? holds)
       (every? (fn [[k hold]]
                 (and (map? k) (held-targets (:target k)) (map? hold) (= k (:destination hold))
                      (time/finite-number? (:touched-beat hold))
                      (if (contains? hold :gesture)
                        (and (vector? (:gesture hold)) (nil? (:until-beat hold)))
                        (time/finite-number? (:until-beat hold)))))
               holds)))
