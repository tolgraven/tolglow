(ns tolglow.studio.timeline-model
  "Pixel mapping for the musical timeline. Every musical quantity (beats, bars,
  phrases, ramp spans, easing samples, section boundaries) arrives in the shared
  [:timeline/view] projection. This namespace only turns beats into pixels,
  smooths the playhead between committed samples and lays items out."
  (:require [clojure.string :as str]))

(def zoom-steps [1 2 4 8 16 32])

(def default-bars 8)

(def target 44)

(def line-height 48)

(def max-lines 6)

(defn zoom
  "Next window size in bars: :in shows fewer bars, :out more."
  [bars direction]
  (let [index (or (first (keep-indexed #(when (= bars %2) %1) zoom-steps)) 3)
        next-index (case direction :in (dec index) :out (inc index))]
    (nth zoom-steps (max 0 (min (dec (count zoom-steps)) next-index)))))

;; Beats to pixels -------------------------------------------------------------

(defn scale
  "Pixels per beat for a lane `width` px wide, with now at :centre."
  [width {:keys [past-beats future-beats]}]
  (when (and (number? width) (pos? width) (number? past-beats) (number? future-beats)
             (pos? (+ past-beats future-beats)))
    (let [span (+ past-beats future-beats)]
      {:width width :ppb (/ width span) :centre (* width (/ past-beats span))})))

(defn x
  "Content coordinate of a beat: pixels right of the layer's origin beat."
  [{:keys [ppb]} origin beat]
  (* ppb (- beat origin)))

(defn offset
  "Translation that puts the playhead beat under the centre line."
  [{:keys [ppb centre]} origin playhead]
  (- centre (* ppb (- playhead origin))))

(defn translation
  "CSS transform for a layer whose children were positioned at `scale` from
  `origin`, so the `playhead` beat sits under the centre line. `shift` is the
  left edge of a clipping parent. Nil until there is a playhead."
  [scale origin shift playhead]
  (when (and (number? (:ppb scale)) (number? (:centre scale)) (number? origin) (number? playhead))
    (str "translate3d(" (.toFixed (- (offset scale origin playhead) (or shift 0)) 2) "px,0,0)")))

(def ^:private rebase-px 200000)

(defn origin
  "Keep a layer's origin beat stable so positioned elements do not move in the
  DOM while the timeline scrolls. Move it only when it is unset or so far from
  the window that pixel coordinates would lose precision."
  [current {:keys [from-beat]} {:keys [ppb]}]
  (if (and (number? current) (number? from-beat) (number? ppb)
           (< (js/Math.abs (* ppb (- from-beat current))) rebase-px))
    current
    from-beat))

;; Playhead ----------------------------------------------------------------------

(def ^:private max-period-ms 250)

(def ^:private max-step-beats 4.5)

(defn position
  "Displayed beat at local time `now` (ms): a straight line from where the
  playhead was when the latest committed sample arrived to that sample's beat,
  over one sample period. It never runs ahead of a committed sample."
  [{:keys [from to start duration]} now]
  (cond
    (nil? to) nil
    (or (nil? duration) (<= duration 0)) to
    :else (+ from (* (- to from) (max 0 (min 1 (/ (- now start) duration)))))))

(defn accept
  "Fold in a committed transport sample {:beat :time-ms :playing? :epoch} that
  arrived at local time `now`. Consecutive playing samples of one show epoch
  are joined smoothly; a pause, seek, loop or gap snaps to the new beat."
  [state {:keys [beat time-ms playing? epoch] :as sample} now]
  (let [previous (:sample state)
        period (when previous (- time-ms (:time-ms previous)))
        step (when previous (- beat (:beat previous)))]
    (cond
      (not (number? beat)) state
      (and previous (= beat (:beat previous)) (= time-ms (:time-ms previous))) state
      (and previous playing? (:playing? previous) (= epoch (:epoch previous))
           (number? period) (< 0 period max-period-ms) (<= 0 step max-step-beats))
      {:sample sample :from (min beat (position state now)) :to beat :start now :duration period}
      :else {:sample sample :from beat :to beat :start now :duration 0})))

;; Grid --------------------------------------------------------------------------

(def ^:private min-line-px 7)

(def ^:private min-label-px 34)

(defn grid-marks
  "Choose which projected lines to draw and label at this zoom, from their
  pixel spacing alone. Phrase boundaries are always drawn and labelled."
  [lines {:keys [ppb]} {:keys [beats-per-bar bars-per-phrase]}]
  (let [bar-px (* ppb beats-per-bar)
        phrase-px (* bar-px bars-per-phrase)]
    (into []
          (keep (fn [{:keys [kind] :as line}]
                  (case kind
                    :beat (when (>= ppb min-line-px) line)
                    :bar (when (>= bar-px min-line-px) (assoc line :label? (>= bar-px min-label-px)))
                    :phrase (assoc line :label? true :phrase-label? (>= phrase-px 96))
                    nil)))
          lines)))

;; Events and ramps ---------------------------------------------------------------

(defn stack-id
  "Identity that keeps an item on its line while its state changes."
  [{:keys [event-id destination key]}]
  (cond (and event-id destination) (str event-id "|" (pr-str destination))
        event-id event-id
        :else key))

(def ^:private caption-limit 110)

(defn caption-width
  "Room kept for an event's caption beside its icon, estimated from its length."
  [title]
  (if (seq title) (min caption-limit (+ 8 (* 6.2 (count title)))) 0))

(defn event-box
  "A touch target centred on the event's beat, with its caption to the right."
  [scale origin {:keys [beat title] :as event}]
  (let [centre (x scale origin beat)
        caption (caption-width title)]
    (assoc event :kind :event :left (- centre (/ target 2)) :right (+ centre (/ target 2) caption)
           :centre centre :caption caption)))

(defn ramp-box
  "A ramp spans its start to its end, or to where it was stopped. It is never
  narrower than a touch target."
  [scale origin {:keys [start-beat end-beat stopped-beat state] :as ramp}]
  (let [left (x scale origin start-beat)
        end (if (and (= :cancelled state) (number? stopped-beat)) (min end-beat stopped-beat) end-beat)
        span (- (x scale origin end) left)]
    (assoc ramp :kind :ramp :left left :right (+ left (max target span)) :span (max 0 span))))

(defn- compact
  "An event without its caption: still a full touch target, a third the width."
  [{:keys [kind caption] :as item}]
  (when (and (= :event kind) (pos? caption))
    (-> item (update :right - caption) (assoc :caption 0 :compact? true))))

(defn- core
  "The part of an item nothing may cover: an event's touch target, a ramp's bar."
  [{:keys [kind left] :as item}]
  (if (= :event kind) (assoc item :right (+ left target)) item))

(def ^:private min-caption 24)

(defn- clip-before
  "Shorten a caption so it ends at x; below a readable width it is dropped."
  [item x]
  (let [caption (- x (:left item) target)]
    (if (< caption min-caption)
      (assoc item :right (+ (:left item) target) :caption 0 :compact? true)
      (assoc item :right x :caption caption))))

(defn stack
  "Give overlapping items their own lines. An item that already had a line
  keeps it while that line is free, so an event does not jump when it fires or
  when its neighbours leave the window; new items then take the lowest line
  free of everything already placed. When every line is taken, an event drops
  its caption, and it or a ramp may sit over a neighbour's caption, which is
  shortened or dropped; touch targets and ramp bars never overlap. Only what still does not fit shares
  the last line. Returns {:items :lines :assignment}."
  ([items] (stack items {} 2))
  ([items previous gap]
   (let [ordered (sort-by (juxt :left :key) items)
         kept (fn [item] (or (get previous (stack-id item)) (get previous (:event-id item))))
         overlaps? (fn [a b] (and (< (:left a) (+ (:right b) gap)) (< (:left b) (+ (:right a) gap))))
         free? (fn [placed line item] (not-any? #(and (= line (:line %)) (overlaps? item %)) placed))
         room? (fn [placed line item] (not-any? #(and (= line (:line %)) (overlaps? item (core %))) placed))
         fit (fn [ok? placed item preferred]
               (if (and preferred (ok? placed preferred item))
                 preferred
                 (first (filter #(ok? placed % item) (range max-lines)))))
         place (fn [placed item preferred]
                 (let [small (or (compact item) item)]
                   (if-let [line (fit free? placed item preferred)]
                     (conj placed (assoc item :line line))
                     (if-let [line (fit room? placed small preferred)]
                       (conj (mapv #(if (and (= line (:line %)) (< (:left %) (:left small)) (overlaps? small %))
                                      (clip-before % (- (:left small) gap))
                                      %)
                                   placed)
                             (assoc small :line line))
                       (conj placed (assoc small :line (dec max-lines)))))))
         retained (reduce (fn [placed item]
                            (if-let [line (kept item)] (place placed item (min line (dec max-lines))) placed))
                          [] ordered)
         placed (reduce (fn [placed item] (if (kept item) placed (place placed item nil))) retained ordered)]
     {:items (vec (sort-by (juxt :left :key) placed))
      :lines (if (seq placed) (inc (apply max (map :line placed))) 0)
      :assignment (into {} (mapcat (fn [item] [[(stack-id item) (:line item)] [(:event-id item) (:line item)]])) placed)})))

(defn curve-points
  "SVG polyline points for a ramp's sampled easing in a box
  (count shape - 1) wide and 1 high. A falling ramp is mirrored vertically.
  Overshoot (a spring) is kept, clipped to a tenth beyond the box."
  [shape falling?]
  (when (and (vector? shape) (<= 2 (count shape)) (every? number? shape))
    (str/join " " (map-indexed (fn [index value]
                                 (let [level (max -0.1 (min 1.1 value))]
                                   (str index "," (.toFixed (if falling? level (- 1 level)) 4))))
                               shape))))

(defn falling? [{:keys [from to]}]
  (and (number? from) (number? to) (< to from)))

;; Sections and audio ---------------------------------------------------------------

(defn section-box
  "An open section has no end yet: it is drawn to the far edge and the past
  layer clips it at now."
  [scale origin {:keys [start-beat end-beat]} {:keys [to-beat]}]
  (let [left (x scale origin start-beat)]
    {:left left :width (max 2 (- (x scale origin (or end-beat to-beat)) left))}))

(defn column-rect
  "Screen rectangle [x width y height] of one captured-audio column
  [start-beat end-beat min max ...], or nil when it is off the lane."
  [column playhead {:keys [ppb centre width]} height]
  (let [from (nth column 0) to (min (nth column 1) playhead)
        low (nth column 2) high (nth column 3)
        left (+ centre (* ppb (- from playhead)))
        right (+ centre (* ppb (- to playhead)))
        mid (/ height 2) half (- mid 1)]
    (when (and (< left right) (> right 0) (< left width))
      [left (max 1 (- right left)) (- mid (* half high)) (max 1 (* half (- high low)))])))

;; Text -------------------------------------------------------------------------------

(defn- number-text [value]
  (if (number? value)
    (let [text (.toFixed value 2)] (-> text (str/replace #"\.?0+$" "")))
    (str value)))

(defn- id-text [value] (if (keyword? value) (subs (str value) 1) (str value)))

(defn position-text
  "Zero-based transport coordinates, as the scheduler's :at takes them."
  [{:keys [bar beat]}]
  (when (number? bar) (str "bar " bar " · beat " (number-text beat))))

(defn duration-text [duration]
  (when (map? duration)
    (let [[unit amount] (first duration)]
      (str (number-text amount) " " (id-text unit)))))

(defn curve-text [curve]
  (cond (map? curve) (id-text (:type curve)) (some? curve) (id-text curve)))

(defn- value-text [value]
  (cond (number? value) (number-text value)
        (vector? value) (str "[" (str/join " " (map value-text value)) "]")
        (nil? value) nil
        :else (id-text value)))

(defn- destination-text [{:keys [target instance-id param variable]}]
  (case (id-text target)
    "param" (str (id-text instance-id) " / " (id-text param))
    "opacity" (str (id-text instance-id) " / opacity")
    "master" "master"
    "variable" (str "variable " (id-text variable))
    nil))

(defn- select-text [{:keys [ids categories page-id pick]}]
  (when (or (seq ids) (seq categories) page-id)
    (str (str/join ", " (map id-text (concat ids categories (when page-id [page-id]))))
         (when pick (str " (" (id-text pick) ")")))))

(defn- source-text [{:keys [adapter session-id]}]
  (when adapter (str (id-text adapter) (when session-id (str " · " (id-text session-id))))))

(def state-labels
  {"pending" "Scheduled" "fired" "Fired" "rejected" "Rejected" "missed" "Missed" "cancelled" "Cancelled"
   "running" "Running" "completed" "Completed"})

(defn state-label [state] (get state-labels (id-text state) (id-text state)))

(defn details
  "Rows for the selected event or ramp: [term text], empty rows left out."
  [{:keys [kind state op position start-position end-position due-beat posted-due-beat at-beat stopped-beat source remaining-ms
           destination select to value from over curve unit reason message by param variable macro-id] :as item}]
  (let [ramp? (= :ramp kind)
        row (fn [term text] (when (and text (not= "" text)) [term text]))]
    (into []
          (remove nil?)
          [(row "Action" (when op (id-text op)))
           (if ramp?
             (row "Span" (str (position-text start-position) " → " (position-text end-position)
                              (when (= "ms" (id-text unit)) " (milliseconds, at this tempo)")))
             (row (if (= "pending" (id-text state)) "Due" "At") (position-text position)))
           (when (and (not ramp?) (number? due-beat) (number? at-beat) (not= due-beat at-beat))
             ;; The beat it was posted for, even after a loop moved due-beat with the transport.
             (row "Was due" (str "beat " (number-text (if (number? posted-due-beat) posted-due-beat due-beat)))))
           (when (number? remaining-ms) (row "Ends in" (str (number-text (/ remaining-ms 1000)) " s")))
           (when (number? stopped-beat) (row "Stopped" (str "beat " (number-text stopped-beat))))
           (row "Target" (or (destination-text destination) (select-text select)
                             (when param (id-text param)) (when variable (str "variable " (id-text variable)))
                             (when macro-id (str "macro " (id-text macro-id)))))
           (when ramp? (row "Value" (str (or (value-text from) "current") " → " (value-text to))))
           (when-not ramp? (row "Value" (value-text (if (some? value) value to))))
           (row "Over" (when over (str (duration-text over) (when curve (str " · " (curve-text curve))))))
           (row "Posted by" (source-text source))
           (row "Reason" (when reason (str (id-text reason) (when by (str " by " (or (source-text (:source by))
                                                                                    (id-text (:op by))))))))
           (row "Message" message)])))

(defn item-label
  "Accessible name: what it is, its state and where it sits."
  [{:keys [title category state kind position start-position] :as item}]
  (str title ", " (id-text category) ", " (str/lower-case (state-label state))
       (when-let [where (position-text (if (= :ramp kind) start-position position))] (str ", " where))))

(defn action-label [{:keys [kind state cancellable?]}]
  (when cancellable?
    (if (and (= :ramp kind) (= "running" (id-text state))) "Stop ramp" "Cancel event")))
