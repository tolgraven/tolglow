(ns tolglow.visualization.musical-grid
  "Background lines for a signal graph, as data. A pure projection of the
  plotted window, the signal period and the transport meter. A renderer draws
  the returned lines and makes no musical decision of its own.

  Beats are the transport's quarter-note beats. A bar holds `:beats-per-bar`
  of them, so a 7/8 bar is 3.5 beats, and beat lines restart at each bar. A
  phrase holds `:bars-per-phrase` whole bars.")

(def units
  "Grid units from fine to coarse."
  [:sixteenth :beat :bar :phrase])

(def default-meter {:beats-per-bar 4 :bars-per-phrase 4})

(def ^:private epsilon 1.0e-9)

(defn- finite? [x]
  #?(:clj (and (number? x) (Double/isFinite (double x)))
     :cljs (and (number? x) (js/isFinite x))))

(defn- floor [x] #?(:clj (Math/floor (double x)) :cljs (js/Math.floor x)))

(defn- ceil [x] #?(:clj (Math/ceil (double x)) :cljs (js/Math.ceil x)))

(defn- round [x] #?(:clj (Math/round (double x)) :cljs (js/Math.round x)))

(defn- whole? [x] (< (abs (- x (round x))) 1.0e-6))

(defn unit-beats
  "The length of each grid unit in beats, or nil for an invalid meter."
  [meter]
  (let [{:keys [beats-per-bar bars-per-phrase]} (merge default-meter meter)]
    (when (and (finite? beats-per-bar) (pos? beats-per-bar)
               (finite? bars-per-phrase) (whole? bars-per-phrase) (pos? bars-per-phrase))
      (let [bar (double beats-per-bar)]
        {:sixteenth 0.25 :beat 1.0 :bar bar :phrase (* bar (round bars-per-phrase))}))))

(defn scale
  "The two units that describe a period: its subdivision and its own cycle.
  Below one bar these are sixteenths and beats. From one bar they are beats
  and bars. From two bars they are bars and phrases."
  [period-beats meter]
  (when-let [{:keys [bar]} (unit-beats meter)]
    (when (and (finite? period-beats) (pos? period-beats))
      (cond
        (< period-beats (- bar epsilon)) [:sixteenth :beat]
        (< period-beats (- (* 2 bar) epsilon)) [:beat :bar]
        :else [:bar :phrase]))))

(defn period-measure
  "A period in its largest whole unit: 16 beats in 4/4 is one phrase, 4 beats
  one bar and a quarter beat stays 0.25 beats."
  [period-beats meter]
  (when-let [{:keys [bar phrase]} (unit-beats meter)]
    (when (and (finite? period-beats) (pos? period-beats))
      (let [in (fn [unit size] (let [n (/ period-beats size)] (when (and (>= n (- 1 epsilon)) (whole? n)) {:unit unit :count (double (round n))})))]
        (or (in :phrase phrase) (in :bar bar) {:unit :beat :count (double period-beats)})))))

(defn measure-text
  "Operator text for a measured period, such as `1/4 beat` or `4 bars`."
  [{:keys [unit count]}]
  (when (and unit (finite? count))
    (let [fraction (when-not (whole? count)
                     (some (fn [d] (let [n (* count d)] (when (whole? n) (str (round n) "/" d)))) [2 3 4 6 8 16 32]))
          amount (cond (whole? count) (str (round count))
                       fraction fraction
                       :else (str (/ (round (* 1000 count)) 1000.0)))]
      (str amount " " (name unit) (when (> count (+ 1 epsilon)) "s")))))

(defn- multiples
  "Origin plus whole multiples of step inside [from, to]. Stride keeps every nth."
  [origin step from to stride]
  (let [first-index (long (ceil (- (/ (- from origin) step) epsilon)))
        last-index (long (floor (+ (/ (- to origin) step) epsilon)))]
    (for [index (range first-index (inc last-index))
          :when (zero? (mod index stride))]
      (+ origin (* index step)))))

(defn- subdivisions
  "Steps counted from each boundary until the next one."
  [boundaries step]
  (for [[from to] (partition 2 1 boundaries)
        index (range 1 (long (ceil (- (/ (- to from) step) epsilon))))]
    (+ from (* index step))))

(defn- boundaries
  "Every boundary of each drawn unit across the window, widened by one bar so
  nested beats and sixteenths start from a real bar line."
  [levels {:keys [bar phrase]} {:keys [bar-origin phrase-origin from to stride]}]
  (let [drawn (set levels)
        nested? (some drawn [:sixteenth :beat])
        bars (when (or nested? (drawn :bar))
               (vec (multiples bar-origin bar (- from bar) (+ to bar) 1)))
        beats (when nested? (vec (sort (concat bars (subdivisions bars 1.0)))))]
    (cond-> {}
      (drawn :sixteenth) (assoc :sixteenth (subdivisions beats 0.25))
      (drawn :beat) (assoc :beat beats)
      (drawn :bar) (assoc :bar bars)
      (drawn :phrase) (assoc :phrase (multiples phrase-origin phrase from to stride)))))

(defn grid
  "Lines for the window of `:span-beats` that starts at `:start-beat`.

  `:period-beats` chooses the units through `scale`. Without a period the
  window chooses: the finest unit with at most eight lines across it.

  Beats count from `:bar-origin-beat` and `:phrase-origin-beat`, both zero by
  default. A cycle plot leaves them at zero and passes a start in beats since
  the cycle began; it draws the period's two units. With `:aligned? true` the
  window lies on the transport, the origins are its bar and phrase origins,
  and coarser boundaries are also drawn where they fall.

  A unit with more than `:max-lines` lines across the window gives way to the
  next coarser one. Each line carries its position in [0,1], its beat and the
  coarsest unit that ends there. Returns nil for a window or meter that cannot
  be drawn."
  [{:keys [start-beat span-beats period-beats meter bar-origin-beat phrase-origin-beat aligned? max-lines]
    :or {start-beat 0.0 max-lines 16}}]
  (when-let [sizes (unit-beats meter)]
    (when (and (finite? start-beat) (finite? span-beats) (pos? span-beats)
               (or (nil? period-beats) (and (finite? period-beats) (pos? period-beats)))
               (finite? max-lines) (<= 2 max-lines 256))
      (let [start (double start-beat)
            span (double span-beats)
            limit (if period-beats max-lines (min max-lines 8))
            crowded? (fn [unit] (> (/ span (sizes unit)) (+ limit epsilon)))
            wanted (if period-beats (first (scale period-beats meter)) :sixteenth)
            ladder (drop-while #(not= wanted %) units)
            fitting (or (seq (drop-while crowded? ladder)) [:phrase])
            levels (vec (if aligned? fitting (take 2 fitting)))
            minor (first levels)
            stride (loop [n 1] (if (> (/ span (* n (sizes minor))) (+ limit epsilon)) (recur (* 2 n)) n))
            bar-origin (double (if (finite? bar-origin-beat) bar-origin-beat 0.0))
            phrase-origin (double (if (finite? phrase-origin-beat) phrase-origin-beat bar-origin))
            found (boundaries levels sizes {:bar-origin bar-origin :phrase-origin phrase-origin
                                            :from start :to (+ start span) :stride stride})
            tagged (reduce (fn [lines unit]
                             (reduce (fn [lines beat]
                                       (if (<= (- start epsilon) beat (+ start span epsilon))
                                         (assoc lines (round (* 65536 (- beat start))) {:beat beat :unit unit})
                                         lines))
                                     lines (get found unit)))
                           (sorted-map) levels)]
        (cond-> {:minor minor
                 :major (second levels)
                 :units levels
                 :start-beat start
                 :span-beats span
                 :lines (mapv (fn [{:keys [beat unit]}]
                                {:position (max 0.0 (min 1.0 (/ (- beat start) span))) :beat beat :unit unit})
                              (vals tagged))}
          period-beats (assoc :period (assoc (period-measure period-beats meter) :beats (double period-beats))))))))
