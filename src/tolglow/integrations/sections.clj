(ns tolglow.integrations.sections
  "Causal section evidence from completed audio analysis hops on a supplied beat grid.")

(def ^:private slots-per-beat 2)

(def ^:private max-hop-gap-beats 0.125)

(def ^:private min-pitch-coverage 0.45)

(defn- finite? [value]
  (and (number? value) (Double/isFinite (double value))))

(defn new-detector
  "Start a comparison on an explicit musical grid. Defaults are four beats
  per window and anchor beat zero. A section needs two consecutive matching
  windows after the reference. Start fresh on transport or input replacement."
  ([] (new-detector {}))
  ([{:keys [anchor-beat beats-per-window]
     :or {anchor-beat 0.0 beats-per-window 4}}]
   (when-not (and (finite? anchor-beat)
                  (integer? beats-per-window) (<= 1 beats-per-window 16))
     (throw (ex-info "Section grid requires a finite anchor and 1–16 beats per window" {})))
   {:anchor-beat (double anchor-beat) :beats-per-window beats-per-window}))

(defn- empty-slot []
  {:hops 0 :power 0.0 :pitch-hops 0 :pitch-confidence 0.0 :notes {}})

(defn- add-hop [slot {:keys [rms bass-pitch-hz bass-pitch-confidence]}]
  (let [slot (-> slot (update :hops inc) (update :power + (* rms rms)))]
    (if (and bass-pitch-hz (>= bass-pitch-confidence 0.88))
      (let [semitone (Math/round (+ 69.0 (* 12.0 (/ (Math/log (/ bass-pitch-hz 440.0))
                                                    (Math/log 2.0)))))]
        (-> slot (update :pitch-hops inc)
            (update :pitch-confidence + bass-pitch-confidence)
            (update-in [:notes semitone] (fnil inc 0))))
      slot)))

(defn- describe-window [{:keys [start-beat complete-start? slots]} beats-per-window]
  (let [hops (transduce (map :hops) + slots)
        power (transduce (map :power) + slots)
        pitch-coverage (mapv #(/ (:pitch-hops %) (double (max 1 (:hops %)))) slots)
        confidence (/ (transduce (map :pitch-confidence) + slots) hops)
        slot-power (mapv #(/ (:power %) (max 1 (:hops %))) slots)
        total-slot-power (reduce + slot-power)]
    {:start-beat start-beat :end-beat (+ start-beat beats-per-window)
     :rms (Math/sqrt (/ power hops))
     :pitch-coverage (/ (reduce + pitch-coverage) (count slots))
     :confidence confidence
     :eligible? (and complete-start? (every? #(>= % min-pitch-coverage) pitch-coverage))
     :bass-pattern (mapv (fn [{:keys [notes pitch-hops]}]
                           (update-vals notes #(/ % (double pitch-hops)))) slots)
     :accent-pattern (if (pos? total-slot-power)
                       (mapv #(/ % total-slot-power) slot-power)
                       slot-power)}))

(defn- pitch-distance [a b]
  (max 0.0 (- 1.0 (reduce-kv (fn [overlap note weight]
                                (+ overlap (min weight (get b note 0.0)))) 0.0 a))))

(defn- distance [a b]
  (let [bass-a (:bass-pattern a) bass-b (:bass-pattern b)]
    {:bass (/ (reduce + (map pitch-distance bass-a bass-b)) (count bass-a))
     :accents (* 0.5 (reduce + (map #(Math/abs (double (- %1 %2)))
                                    (:accent-pattern a) (:accent-pattern b))))}))

(defn- change-drivers [{:keys [bass accents]}]
  (cond-> []
    (>= bass 0.38) (conj :bass-pattern)
    (>= accents 0.20) (conj :groove-accents)))

(defn- finish-window [state window beat available-at-beat]
  (let [{:keys [reference candidate]} state]
    (cond
      (not (:eligible? window)) [(dissoc state :candidate) nil]
      (nil? reference) [(assoc state :reference window) nil]
      :else
      (let [novelty (distance reference window)
            drivers (change-drivers novelty)
            recurrence (when candidate (distance candidate window))
            confirmed-drivers (filterv (set (:drivers candidate)) drivers)]
        (cond
          (empty? drivers) [(dissoc state :candidate) nil]
          (and (seq confirmed-drivers) (<= (:bass recurrence) 0.18) (<= (:accents recurrence) 0.10))
          (let [boundary (:start-beat candidate)]
            [(-> state (assoc :reference window) (dissoc :candidate))
             {:type :audio/section-change :version 1
              :boundary-beat boundary :confirmed-at-beat beat
              :confirmation-delay-beats (- beat boundary)
              :available-at-beat available-at-beat
              :confidence (min (:confidence reference) (:confidence candidate) (:confidence window))
              :evidence {:drivers confirmed-drivers :distance novelty :repeat-distance recurrence
                         :pitch-coverage (:pitch-coverage window)}
              :level {:previous-rms (:rms reference) :current-rms (:rms window)}}])
          :else [(assoc state :candidate (assoc window :drivers drivers)) nil])))))

(defn observe-hop
  "Consume one completed hop and return {:state :window :event}. Supply
  :features from the PCM analyzer, :beat at the end of that hop on the running
  transport, and optional :available-at-beat for capture/delivery delay. Hops
  belong to the half-beat slot containing their end. A hop on the next window's
  boundary closes the prior window and returns its :window descriptor.

  Feed every completed hop exactly once in strictly increasing beat order.
  Set :discontinuity? across clock epochs, pauses and input resets. A gap over
  1/8 beat resets the comparison. Low pitch coverage clears pending changes.
  Confidence summarizes monophonic pitch evidence in the contributing windows."
  [state {:keys [beat available-at-beat features discontinuity?]}]
  (let [{:keys [last-beat anchor-beat beats-per-window]} state
        {:keys [rms bass-pitch-hz bass-pitch-confidence]} features
        available-at-beat (or available-at-beat beat)]
    (when-not (and (finite? beat) (finite? available-at-beat) (<= beat available-at-beat)
                   (or discontinuity? (nil? last-beat) (< last-beat beat))
                   (finite? rms) (<= 0 rms 16)
                   (finite? bass-pitch-confidence) (<= 0 bass-pitch-confidence 1)
                   (or (nil? bass-pitch-hz) (and (finite? bass-pitch-hz) (<= 20 bass-pitch-hz 180))))
      (throw (ex-info "Section hop requires ordered beat positions and valid audio features" {})))
    (let [beat (double beat)
          available-at-beat (double available-at-beat)
          state (if (or discontinuity? (and last-beat (> (- beat last-beat) max-hop-gap-beats)))
                  (new-detector state) state)
          window-index (Math/floor (/ (- beat anchor-beat) beats-per-window))
          start-beat (+ anchor-beat (* window-index beats-per-window))
          phase (- beat start-beat)
          active (:active state)
          close? (and active (not= (:start-beat active) start-beat))
          window (when close? (describe-window active beats-per-window))
          [state event] (if close? (finish-window state window beat available-at-beat) [state nil])
          active (if (or close? (nil? active))
                   {:start-beat start-beat :complete-start? (<= phase max-hop-gap-beats)
                    :slots (vec (repeat (* slots-per-beat beats-per-window) (empty-slot)))}
                   active)
          slot (int (* slots-per-beat phase))]
      {:state (assoc state :last-beat beat :active (update-in active [:slots slot] add-hop features))
       :window window
       :event event})))
