(in-ns 'afterglow.spatial.beam)

(defn- radial-beam-candidate
  "In clear air a radial field is strongest at the nearest point of the beam.
  If a receiver hides that point or the range clips it, use the general probes."
  [{:keys [position axis spread lens-radius-m]} {:keys [center support by-distance]} opts level]
  (let [[px py pz] position [cx cy cz] center [vx vy vz] axis
        px (double px) py (double py) pz (double pz)
        dx (- (double cx) px) dy (- (double cy) py) dz (- (double cz) pz)
        vx (double vx) vy (double vy) vz (double vz)
        z (+ (* dx vx) (* dy vy) (* dz vz))
        rx (- dx (* z vx)) ry (- dy (* z vy)) rz (- dz (* z vz))
        r (Math/sqrt (+ (* rx rx) (* ry ry) (* rz rz)))
        k (double spread) lens (double lens-radius-m)
        in? (and (>= z 0.0) (<= r (+ lens (* k z))))
        ;; Closest point to the side of the infinite frustum in its meridian.
        side-z (max 0.0 (/ (+ z (* k (- r lens))) (+ 1.0 (* k k))))
        side-r (+ lens (* side-z k))
        front-r (min r lens)
        limit (Math/nextUp (double support))
        ;; Only avoid hypot when an axis alone is clearly beyond the limit.
        ;; Leave a rounding margin at the support boundary.
        reject-above (+ limit (* 4.0 (Math/ulp limit)))
        side-depth (- side-z z) side-radius (- side-r r) front-radius (- front-r r)
        side-gap (cond in? 0.0
                       (or (> (Math/abs side-depth) reject-above)
                           (> (Math/abs side-radius) reject-above)) Double/POSITIVE_INFINITY
                       :else (Math/hypot side-depth side-radius))
        front-gap (cond in? 0.0
                        (or (> (Math/abs z) reject-above)
                            (> (Math/abs front-radius) reject-above)) Double/POSITIVE_INFINITY
                        :else (Math/hypot z front-radius))
        face? (< front-gap side-gap)
        gap (if face? front-gap side-gap)]
    (when (<= gap limit)
      (let [axial (double (cond in? z face? 0.0 :else side-z))
            radial (double (cond in? r face? front-r :else side-r))
            factor (if (pos? r) (/ radial r) 0.0)
            aperture (* factor (/ lens (+ lens (* k axial) 1.0e-300)))
            distance (Math/hypot axial (- radial (* aperture r)))
            value (by-distance (min gap (double support)))
            ox (if (zero? aperture) px (+ px (* aperture rx)))
            oy (if (zero? aperture) py (+ py (* aperture ry)))
            oz (if (zero? aperture) pz (+ pz (* aperture rz)))
            hx (if in? (double cx) (+ px (* axial vx) (* factor rx)))
            hy (if in? (double cy) (+ py (* axial vy) (* factor ry)))
            hz (if in? (double cz) (+ pz (* axial vz) (* factor rz)))
            blocked? (:occluded? opts)
            cell (::clear-cell opts)
            bounds (when (and cell (identical? blocked? (:check cell))) (:bounds cell))]
        (cond
          (not (pos? value)) nil
          (> distance (double (:range-m opts))) ::probe
          (and blocked?
               (not (and bounds (inside-cell? bounds ox oy oz) (inside-cell? bounds hx hy hz)))
               (blocked? (if (zero? aperture) position [ox oy oz]) (if in? center [hx hy hz]) distance)) ::probe
          :else {:weight (* level value)})))))
