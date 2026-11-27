(ns tolglow.rig.aim-calibration
  "Read-only moving-head aim checks for the rig editor and the REPL. Afterglow's
  renderer solves pan/tilt; `tolglow.calibration.geometry/fit-aim` fits a mounting
  rotation from jogged observations. Neither changes the show or drives output."
  (:require [afterglow.render.frame :as render]
            [tolglow.calibration.geometry :as geometry]
            [tolglow.rig.placement :as placement]))

(def overrides
  "Draft fields a check may substitute for the committed fixture's."
  [:position :rotation-deg :calibration])

(defn- fail! [message data] (throw (ex-info message (assoc data :rig/error :aim-calibration))))

(defn- wire [id] (if (keyword? id) (subs (str id) 1) (str id)))

(defn fixture
  "The committed moving head named `fixture-id`, with any draft overrides. The
  result must still pass Afterglow's patch validation."
  [rig {:keys [fixture-id] :as request}]
  (let [found (first (filter #(= (wire fixture-id) (wire (:id %))) (:fixtures rig)))
        candidate (merge found (select-keys request overrides))]
    (when-not found (fail! "Save the rig first: this fixture is not in the running show" {:fixture-id fixture-id}))
    (when-not (every? #(contains? (:channels found) %) [:pan :tilt])
      (fail! "Aim checks need a fixture with pan and tilt channels" {:fixture-id fixture-id}))
    (when-let [problems (seq (render/validate-patch [candidate]))]
      (fail! "Invalid draft position, rotation or calibration" {:fixture-id fixture-id :errors (vec problems)}))
    candidate))

(defn- dmx [value] (when (number? value) (Math/round (* 255.0 (double value)))))

(defn check
  "Solve where the calibrated head must point to hit the world `point` (metres).
  Returns normalized pan/tilt, mechanical degrees, 8-bit DMX equivalents before
  any channel reversal, and whether the travel range reaches it."
  [fixture point]
  (when-not (placement/v3? point) (fail! "Enter a finite world point in metres" {:point point}))
  (if (every? #(< (Math/abs (double %)) 1.0e-6) (map - point (:position fixture)))
    {:fixture-id (:id fixture) :point point :reachable? false :reason :at-lens}
    (let [values (get-in (render/resolve-frame [fixture] [{:target-id (:id fixture) :attribute :aim :value point}])
                         [:values (:id fixture)])]
      {:fixture-id (:id fixture) :point point :reachable? (boolean (:aim-reachable? values))
       :pan (:pan values) :tilt (:tilt values) :pan-deg (:pan-deg values) :tilt-deg (:tilt-deg values)
       :dmx {:pan (dmx (:pan values)) :tilt (dmx (:tilt values))}
       :direction (:direction values)})))

(defn fit
  "Fit the head's mounting rotation from at least three observations, each
  {:target [x y z] :pan p :tilt t} with p and t the normalized values at which
  the beam centre hit the target. `axis-verified?` states that the pan/tilt
  direction and range match the fixture's documentation; without it the result
  is a candidate that cannot be accepted."
  [fixture samples axis-verified?]
  (let [result (geometry/fit-aim fixture (:position fixture) (vec samples) (true? axis-verified?))]
    (cond-> (assoc result :fixture-id (:id fixture) :samples (count samples))
      (:rotation-deg result) (update :rotation-deg #(mapv (fn [d] (/ (Math/round (* d 1000.0)) 1000.0)) %))
      (:rotation-deg result) (assoc :mounting-kind (placement/mounting (:rotation-deg result))))))
