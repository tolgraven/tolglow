(ns tolglow.integrations.reference-position
  "Pure playback alignment for one source session and a caller-supplied monotonic clock.")

(def ^:private defaults
  {:confirmations 3
   :max-age-ms 750.0
   :max-lookahead-ms 250.0
   :min-score 0.8
   :min-lead 0.15
   :position-tolerance-ms 75.0
   :rate-tolerance 0.12})

(defn- finite? [x]
  (and (number? x) (Double/isFinite (double x))))

(defn- score? [x]
  (and (finite? x) (<= 0 x 1)))

(defn- identifier? [x]
  (or (keyword? x) (string? x) (uuid? x) (integer? x)))

(defn new-state
  "Start a new source session. The provenance keys are :source-id, :session-id
   and :clock-id. All times supplied to this lock use that clock, in ms."
  ([provenance] (new-state provenance {}))
  ([provenance options]
   (let [provenance (select-keys provenance [:source-id :session-id :clock-id])
         options (merge defaults options)]
     (when-not (and (= 3 (count provenance)) (every? identifier? (vals provenance)))
       (throw (ex-info "Reference position requires source, session and clock IDs" {:provenance provenance})))
     (when-not (and (= (set (keys defaults)) (set (keys options)))
                    (integer? (:confirmations options)) (<= 2 (:confirmations options))
                    (every? #(and (finite? %) (pos? %))
                            ((juxt :max-age-ms :max-lookahead-ms :position-tolerance-ms :rate-tolerance) options))
                    (score? (:min-score options)) (score? (:min-lead options)) (pos? (:min-lead options)))
       (throw (ex-info "Invalid reference position options" {:options options})))
     {:provenance provenance :options options :occurrence 0 :generation 0
      :status :unavailable :reason :waiting :confirmations 0})))

(defn- revoke [state status reason reference]
  (cond-> (-> state
              (assoc :status status :reason reason :reference reference :confirmations 0)
              (dissoc :sample))
    (= :locked (:status state)) (update :generation inc)))

(defn- project-position [{:keys [position-ms rate observed-at-ms]} now-ms]
  (+ position-ms (* rate (- now-ms observed-at-ms))))

(defn- tick [state now-ms]
  (when-not (and (finite? now-ms) (or (nil? (:now-ms state)) (<= (:now-ms state) now-ms)))
    (throw (ex-info "Reference position requires monotonic host time" {:now-ms now-ms})))
  (let [state (assoc state :now-ms now-ms)]
    (if-let [{:keys [observed-at-ms until-position-ms] :as sample} (:sample state)]
      (cond
        (>= (- now-ms observed-at-ms) (get-in state [:options :max-age-ms]))
        (revoke state :unavailable :expired (:reference state))

        (and until-position-ms (>= (project-position sample now-ms) until-position-ms))
        (revoke state :unavailable :playback-boundary (:reference state))

        :else state)
      state)))

(defn- reference? [reference]
  (and (map? reference) (identifier? (:id reference)) (identifier? (:version reference))))

(defn- match? [match]
  (when (map? match)
    (let [{:keys [reference position-ms rate score until-position-ms]} match]
      (and (reference? reference) (score? score)
           (finite? position-ms) (<= 0 position-ms)
           (finite? rate) (pos? rate)
           (or (nil? until-position-ms)
               (and (finite? until-position-ms) (< position-ms until-position-ms)))))))

(defn- mismatch [previous sample {:keys [rate-tolerance position-tolerance-ms]}]
  (when previous
    (cond
      (not= (:reference previous) (:reference sample)) :reference-change
      (> (abs (- (:rate previous) (:rate sample))) rate-tolerance) :rate-change
      (> (abs (- (:position-ms sample) (project-position previous (:observed-at-ms sample))))
         position-tolerance-ms) :position-jump)))

(defn- acquire [state sample]
  (let [options (:options state)
        change (mismatch (:sample state) sample options)
        state (if change (revoke state :acquiring change (:reference sample)) state)
        confirmations (min (:confirmations options) (inc (:confirmations state)))
        locked? (= confirmations (:confirmations options))]
    (assoc state :sample sample :reference (:reference sample) :confirmations confirmations
           :status (if locked? :locked :acquiring) :reason (when-not locked? (or change :confirming)))))

(defn- observe-match [state {:keys [match runner-up observed-at-ms]}]
  (let [{:keys [min-score min-lead]} (:options state)
        valid? (and (match? match)
                    (or (nil? runner-up) (and (reference? (:reference runner-up)) (score? (:score runner-up)))))
        reference (when valid? (select-keys (:reference match) [:id :version]))]
    (cond
      (not valid?)
      (revoke state :unavailable :invalid-match nil)

      (< (:score match) min-score)
      (revoke state :unavailable :weak-match nil)

      (and runner-up (< (- (:score match) (:score runner-up)) min-lead))
      (if (= reference (select-keys (:reference runner-up) [:id :version]))
        (revoke state :ambiguous :position-ambiguous reference)
        (revoke state :ambiguous :identity-ambiguous nil))

      :else
      (acquire state (assoc (select-keys match [:position-ms :rate :until-position-ms])
                            :reference reference :observed-at-ms observed-at-ms)))))

(defn advance
  "Advance host time, optionally processing one observation. An observation has
   :type, :provenance, :occurrence and :observed-at-ms (the capture time, rather
   than completion time). Tag asynchronous work when it is captured. For :match,
   supply :match {:reference {:id ... :version ...} :position-ms ... :rate ...
   :score ...}, and :runner-up {:reference ... :score ...} when a distinct match
   on any recording or offset competes. Scores are comparable within a result.
   Rate is reference ms per clock ms. :until-position-ms on :match optionally
   caps prediction before a known playback boundary. :unavailable revokes the
   lock. :seek, :loop and :cut revoke it and start a new occurrence."
  ([state now-ms] (tick state now-ms))
  ([state now-ms {:keys [type provenance occurrence observed-at-ms] :as observation}]
   (let [state (tick state now-ms)]
     (if-not (and (= provenance (:provenance state)) (= occurrence (:occurrence state))
                  (finite? observed-at-ms) (<= observed-at-ms now-ms))
       state
       (cond
         (#{:seek :loop :cut} type)
         (-> state
             (revoke :unavailable type nil)
             (update :occurrence inc)
             (assoc :last-observed-at-ms observed-at-ms))

         (and (#{:match :unavailable} type)
              (or (nil? (:last-observed-at-ms state)) (> observed-at-ms (:last-observed-at-ms state)))
              (< (- now-ms observed-at-ms) (get-in state [:options :max-age-ms])))
         (let [state (assoc state :last-observed-at-ms observed-at-ms)]
           (case type
             :match (tick (observe-match state observation) now-ms)
             :unavailable (revoke state :unavailable :source-unavailable nil)))

         :else state)))))

(defn snapshot
  "Project status at host time. A reference outside :locked is an identity hint.
   While locked, the usable interval is [now-ms, :lookahead-until-ms)."
  [state now-ms]
  (let [{:keys [sample status options] :as state} (tick state now-ms)
        projection (assoc (select-keys state [:provenance :occurrence :generation :status :reason
                                             :reference :confirmations])
                          :lookahead-ms 0.0)]
    (if (= :locked status)
      (let [{:keys [observed-at-ms rate until-position-ms]} sample
            position-ms (project-position sample now-ms)
            horizon (min (:max-lookahead-ms options) (- (:max-age-ms options) (- now-ms observed-at-ms)))
            horizon (if until-position-ms (min horizon (/ (- until-position-ms position-ms) rate)) horizon)]
        (assoc projection :position-ms position-ms :rate rate :observed-at-ms observed-at-ms
               :lookahead-ms horizon :lookahead-until-ms (+ now-ms horizon)))
      projection)))
