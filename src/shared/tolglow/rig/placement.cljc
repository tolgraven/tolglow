(ns tolglow.rig.placement
  "Pure fixture placement and mounting geometry shared by Studio and the REPL.
  World coordinates are metres, right handed, +Y up. A fixture's local +Y is its
  base-to-head (mounting) axis and local +Z is its beam at pan 0, tilt 0.
  :rotation-deg [x y z] is applied X then Y then Z, as Afterglow's renderer does."
  (:require [clojure.string :as str]
            [afterglow.render.photometry :as photometry]))

(def type-properties
  "Fixture-type data copied onto every instance."
  (into [:name :emission-color :fogger :bounds-m] photometry/fixture-keys))

(def instance-keys
  "Per-instance choices a placement may supply. Everything else comes from the type."
  [:id :name :universe :address :position :rotation-deg :rgbw-policy :calibration])

(defn- offsets [channel]
  (cond (integer? channel) [channel]
        (vector? channel) (vec channel)
        (map? channel) (vec (remove nil? [(:offset channel) (:fine-offset channel)]))
        :else []))

(defn instance
  "Compact rig instance of a fixture type. Channels, fine offsets, inversion,
  RGBW policy and explicit calibration are preserved. Performs no validation;
  the host's `tolglow.fixtures.library/instantiate` checks it before admission."
  [definition options]
  (let [channels (:channels definition)
        all-offsets (vec (mapcat offsets (vals channels)))
        policy (or (:rgbw-policy options) (:rgbw-policy definition) :none)
        policy (if (string? policy) (keyword policy) policy)]
    (merge (select-keys definition type-properties)
           (select-keys options type-properties)
           {:id (:id options) :fixture-type-id (:id definition) :fixture-type-version (:version definition)
            :position (:position options [0 0 0]) :rotation-deg (:rotation-deg options [0 0 0])
            :universe (:universe options) :address (:address options)
            :channels channels :footprint (:footprint definition (reduce max 0 (filter integer? all-offsets)))
            :rgbw-policy policy :calibration (merge (:calibration definition) (:calibration options))})))

(defn- channel-spec
  "Canonical map form of a channel specification."
  [channel]
  (cond (integer? channel) {:offset channel}
        (vector? channel) (cond-> {:offset (first channel)} (second channel) (assoc :fine-offset (second channel)))
        (map? channel) channel
        :else {:invalid channel}))

(defn same-patch?
  "Does `instance` patch exactly the type's channels? The only permitted
  difference is full-range axis reversal: an instance channel may add or drop
  `:inverted-from 0`, or keep the type's own value. Everything else, including
  offsets, ranges and functions, must match."
  [type-channels instance-channels]
  (and (map? type-channels) (map? instance-channels)
       (= (set (keys type-channels)) (set (keys instance-channels)))
       (every? (fn [attribute]
                 (let [a (channel-spec (get type-channels attribute))
                       b (channel-spec (get instance-channels attribute))
                       reversal (:inverted-from b)]
                   (and (= (dissoc a :inverted-from) (dissoc b :inverted-from))
                        (or (nil? reversal) (and (number? reversal) (zero? reversal)) (= reversal (:inverted-from a))))))
               (keys type-channels))))

(defn- footprint [fixture]
  (or (:footprint fixture) (reduce max 1 (mapcat offsets (vals (:channels fixture))))))

(defn next-address
  "[universe address] directly after the highest occupied slot. A fixture that
  would cross channel 512 starts on the next universe."
  [fixtures size]
  (let [end (reduce max 0 (map #(+ (* 512 (dec (:universe % 1))) (dec (:address % 1)) (footprint %)) fixtures))
        universe (inc (quot end 512)) address (inc (mod end 512))]
    (if (> (+ address (max 1 size) -1) 512) [(inc universe) 1] [universe address])))

(defn- wire [id] (if (keyword? id) (subs (str id) 1) (str id)))

(defn next-id
  "First free `<type>-<n>` instance ID, comparing IDs by their text. A
  namespaced type contributes its name only, so IDs never contain a slash.
  `taken` adds IDs to avoid besides the draft's, such as the committed rig's."
  ([fixtures type-id] (next-id fixtures type-id nil))
  ([fixtures type-id taken]
   (let [taken (into (set (map (comp wire :id) fixtures)) (map wire) taken)
         stem (-> (if (keyword? type-id) (name type-id) (str type-id)) (str/replace "/" "-"))]
     (first (remove taken (map #(str stem "-" %) (iterate inc 1)))))))

;; Vector algebra kept local so the namespace runs on the JVM and in the browser.

(defn- finite? [n] (and (number? n) #?(:clj (Double/isFinite (double n)) :cljs (js/isFinite n))))

(defn v3? [v] (and (vector? v) (= 3 (count v)) (every? finite? v)))

(defn- dot [a b] (reduce + (map * a b)))

(defn- cross [[a b c] [x y z]] [(- (* b z) (* c y)) (- (* c x) (* a z)) (- (* a y) (* b x))])

(defn- norm [v] (Math/sqrt (dot v v)))

(defn- unit [v] (let [n (norm v)] (when (> n 1.0e-9) (mapv #(/ % n) v))))

(defn- reject
  "Component of v perpendicular to the unit vector n, normalized; nil when parallel."
  [v n]
  (unit (mapv - v (mapv #(* (dot v n) %) n))))

(defn- radians [d] (* d (/ Math/PI 180)))

(defn- degrees [r] (* r (/ 180 Math/PI)))

(defn rotate
  "Apply a fixture's Euler rotation to a local vector (X, then Y, then Z)."
  [v rotation-deg]
  (let [[ax ay az] (mapv radians (or rotation-deg [0 0 0]))
        turn (fn [[x y z] axis angle]
               (let [c (Math/cos angle) s (Math/sin angle)]
                 (case axis
                   :x [x (- (* y c) (* z s)) (+ (* y s) (* z c))]
                   :y [(+ (* x c) (* z s)) y (- (* z c) (* x s))]
                   :z [(- (* x c) (* y s)) (+ (* x s) (* y c)) z])))]
    (-> v (turn :x ax) (turn :y ay) (turn :z az))))

(defn rotation-euler
  "Degrees [x y z] for a row-major 3x3 rotation matrix R = Rz·Ry·Rx."
  [r]
  (let [y (Math/asin (max -1.0 (min 1.0 (- (get-in r [2 0])))))
        singular? (< (Math/abs (Math/cos y)) 1.0e-7)]
    (mapv degrees
          [(if singular? (Math/atan2 (- (get-in r [1 2])) (get-in r [1 1])) (Math/atan2 (get-in r [2 1]) (get-in r [2 2])))
           y (if singular? 0.0 (Math/atan2 (get-in r [1 0]) (get-in r [0 0])))])))

(defn- clean [angles] (mapv #(let [r (/ (Math/round (* % 1.0e6)) 1.0e6)] (if (== r -180.0) 180.0 (+ r 0.0))) angles))

(defn default-forward
  "Home beam direction for a mount: towards the audience (+Z) on a floor or
  ceiling, straight down on a wall."
  [up]
  (if (> (Math/abs (double (second up))) 0.7) (reject [0 0 1] up) (reject [0 -1 0] up)))

(defn mount-rotation
  "Euler rotation whose local +Y is `up` and local +Z is the part of `forward`
  perpendicular to it. Returns nil for a zero or parallel pair."
  ([up] (when-let [u (unit up)] (mount-rotation u (default-forward u))))
  ([up forward]
   (when-let [u (unit up)]
     (when-let [f (reject forward u)]
       (let [right (cross u f)]
         ;; Columns are the images of local X, Y and Z.
         (clean (rotation-euler (mapv (fn [row] (mapv #(nth % row) [right u f])) (range 3)))))))))

(defn axes
  "World directions of a rotation's local axes."
  [rotation-deg]
  {:up (rotate [0 1 0] rotation-deg) :forward (rotate [0 0 1] rotation-deg) :right (rotate [1 0 0] rotation-deg)})

(def mounts
  "Named mounting directions (the fixture's base-to-head axis in the world)."
  {:standing [0 1 0] :hanging [0 -1 0]
   :wall-back [0 0 1] :wall-front [0 0 -1] :wall-left [1 0 0] :wall-right [-1 0 0]})

(defn mounting
  "Classify a rotation by its mounting axis: :standing, :hanging or :wall."
  [rotation-deg]
  (let [y (second (:up (axes rotation-deg)))]
    (cond (> y 0.85) :standing (< y -0.85) :hanging :else :wall)))

(defn remount
  "Turn a fixture onto another mounting axis, keeping its home beam as close
  to the previous direction as the new mount allows."
  [rotation-deg up]
  (when-let [u (unit up)]
    (let [previous (:forward (axes rotation-deg))]
      (mount-rotation u (or (reject previous u) (default-forward u))))))

(defn face
  "Spin a fixture about its mounting axis so that its home beam points at the
  world `target`. Nil when the target lies on the mounting axis."
  [rotation-deg position target]
  (let [{:keys [up]} (axes rotation-deg)]
    (when (and (v3? position) (v3? target))
      (when-let [f (reject (mapv - target position) up)] (mount-rotation up f)))))

(defn place
  "Draft placement of `definition` resting on a picked surface. `hit` has a world
  :point and the surface :normal pointing into the room. The fixture stands on the
  surface `clearance-m` away from it, mounted along the normal, takes the next free
  address and a fresh ID (also avoiding `:taken-ids`). Options override any
  instance key. The entry is marked `:placement? true`: the host instantiates
  it from the registry type when the rig is saved and removes the mark."
  [definition fixtures {:keys [point normal]} & [{:keys [clearance-m taken-ids] :or {clearance-m 0.25} :as options}]]
  (when (and (v3? point) (v3? normal) (unit normal))
    (let [up (unit normal)
          [universe address] (next-address fixtures (footprint definition))]
      (assoc (instance definition
                       (merge {:id (next-id fixtures (:id definition) taken-ids) :universe universe :address address
                               ;; Tenth-of-a-millimetre positions read cleanly in numeric fields.
                               :position (mapv #(/ (Math/round (* 1.0e4 (+ %1 (* clearance-m %2)))) 1.0e4) point up)
                               :rotation-deg (mount-rotation up)}
                              (select-keys options instance-keys)))
             :placement? true))))

(defn reversed?
  "Does the channel's DMX run against the model's angle (`:inverted-from 0`)?"
  [channel]
  (and (map? channel) (number? (:inverted-from channel)) (zero? (:inverted-from channel))))

(defn reverse-channel
  "Set or clear full-range axis reversal on a channel specification."
  [channel on?]
  (let [[offset fine] (offsets channel)
        spec (cond-> (if (map? channel) (dissoc channel :inverted-from) {:offset offset})
               (and fine (not (map? channel))) (assoc :fine-offset fine))]
    (if on? (assoc spec :inverted-from 0) spec)))

(defn axis-range
  "{:min-deg :max-deg} of an axis calibration. Legacy [centre half-circle]
  values are converted; a reversed legacy calibration has no range form (nil)."
  [calibration]
  (cond (map? calibration) (select-keys calibration [:min-deg :max-deg])
        (and (vector? calibration) (pos? (second calibration)))
        (let [[centre half] calibration]
          {:min-deg (/ (* -180.0 centre) half) :max-deg (/ (* 180.0 (- 255 centre)) half)})))

(defn home-position
  "Fraction of the axis travel at which the beam points along the fixture's
  local home (0 degrees). Values outside [0, 1] mean home is out of reach."
  [{:keys [min-deg max-deg]}]
  (when (and (finite? min-deg) (finite? max-deg) (< min-deg max-deg))
    (/ (- (double min-deg)) (- max-deg min-deg))))

(defn dmx-sent
  "8-bit DMX value a channel sends for a normalized model value, after its
  `:inverted-from` reversal (Afterglow's encoding, coarse byte)."
  [channel value]
  (let [v (* 255.0 value) from (when (map? channel) (:inverted-from channel))]
    (Math/round (double (if (and (number? from) (>= v from)) (- 255.0 (- v from)) v)))))

(defn dmx-model
  "Normalized model value that makes a channel send the 8-bit DMX value `dmx`;
  the inverse of `dmx-sent`."
  [channel dmx]
  (let [from (when (map? channel) (:inverted-from channel))]
    (/ (if (and (number? from) (>= dmx from)) (- (+ 255.0 from) dmx) (double dmx)) 255.0)))

(defn unreverse-legacy
  "Express a reversed legacy [centre -half] axis calibration as an ordinary
  range plus channel reversal that sends the same DMX for every angle. Returns
  [range channel], or nil when the calibration is not reversed legacy data."
  [calibration channel]
  (when (and (vector? calibration) (number? (second calibration)) (neg? (second calibration)))
    (let [[centre half] calibration]
      [(axis-range [(- 255 centre) (- half)]) (reverse-channel channel (not (reversed? channel)))])))
