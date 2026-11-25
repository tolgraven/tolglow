(ns tolglow.calibration.geometry
  "Pure metric geometry. Rig and AR worlds are right handed, metres, +Y up.
  Matrices are row-major; AR camera looks down -Z, image pixels have +Y down.")

(defn finite? [v] (and (number? v) (Double/isFinite (double v))))

(defn v3? [v] (and (vector? v) (= 3 (count v)) (every? finite? v)))

(defn dot [a b] (reduce + (map * a b)))

(defn add [a b] (mapv + a b))

(defn sub [a b] (mapv - a b))

(defn scale [a n] (mapv #(* n %) a))

(defn norm [v] (Math/sqrt (dot v v)))

(defn unit [v] (let [n (norm v)] (when (< n 1.0e-9) (throw (ex-info "Zero direction" {}))) (scale v (/ n))))

(defn cross [[a b c] [x y z]] [(- (* b z) (* c y)) (- (* c x) (* a z)) (- (* a y) (* b x))])

(defn rotate [r p] (mapv #(dot % p) r))

(def identity-transform [1 0 0 0, 0 1 0 0, 0 0 1 0, 0 0 0 1])

(defn rigid-transform! [m]
  (when-not (and (vector? m) (= 16 (count m)) (every? finite? m)
                 (every? #(< (Math/abs (double %)) 1.0e-5) (map - (subvec m 12) [0 0 0 1])))
    (throw (ex-info "Expected a row-major rigid 4x4 transform" {})))
  (let [r (mapv #(subvec m % (+ % 3)) [0 4 8])]
    (when-not (and (every? #(< (Math/abs (- 1.0 (norm %))) 1.0e-4) r)
                   (every? #(< (Math/abs (double %)) 1.0e-4) [(dot (r 0) (r 1)) (dot (r 0) (r 2)) (dot (r 1) (r 2))])
                   (> (dot (r 0) (cross (r 1) (r 2))) 0.999))
      (throw (ex-info "Transform must be a proper rigid rotation, not a reflection or scale" {})))) m)

(defn transform-point [m p]
  (mapv #(dot (subvec m % (+ % 4)) (conj p 1.0)) [0 4 8]))

(defn unproject
  "Depth is distance to the camera plane, NOT ray length. Pixel/intrinsics use
  captured-image resolution (not the smaller depth-map resolution)."
  [{:keys [pixel depth-m intrinsics camera-to-world world-to-rig]}]
  (let [[u v] pixel [fx fy cx cy] intrinsics]
    (when-not (and (= 2 (count pixel)) (= 4 (count intrinsics))
                   (every? finite? (concat pixel intrinsics)) (pos? fx) (pos? fy)
                   (finite? depth-m) (< 0 depth-m 30))
      (throw (ex-info "Invalid camera sample" {})))
    (rigid-transform! camera-to-world) (rigid-transform! world-to-rig)
    (transform-point world-to-rig
                     (transform-point camera-to-world
                                      [(* (/ (- u cx) fx) depth-m)
                                       (* -1 (/ (- v cy) fy) depth-m) (- depth-m)]))))

(defn- median [xs] (let [v (vec (sort xs)) n (count v)] (if (odd? n) (v (quot n 2)) (/ (+ (v (dec (quot n 2))) (v (quot n 2))) 2.0))))

(defn- weighted-mean [samples]
  (scale (reduce add [0 0 0] (map #(scale (:point %) (:weight % 1.0)) samples))
         (/ (reduce + (map #(:weight % 1.0) samples)))))

(defn fit-position
  "Robust lens centre, with retained/rejected observations and metric residuals.
  Beam landing spots must never be passed as lens observations."
  [samples]
  (when-not (and (<= 3 (count samples) 4096)
                 (every? #(and (v3? (:point %)) (finite? (:weight % 1.0)) (< 0 (:weight % 1.0) 100)) samples))
    (throw (ex-info "Position needs at least three valid depth-backed lens samples" {})))
  (let [seed (mapv #(median (map (fn [s] (nth (:point s) %)) samples)) (range 3))
        distances (map #(norm (sub (:point %) seed)) samples)
        threshold (max 0.06 (* 3.5 (median distances)))
        kept (filterv #(<= (norm (sub (:point %) seed)) threshold) samples)
        centre (weighted-mean kept)
        rms (Math/sqrt (/ (reduce + (map #(* (:weight % 1.0) (Math/pow (norm (sub (:point %) centre)) 2)) kept))
                          (reduce + (map #(:weight % 1.0) kept))))]
    {:position centre :rms-m rms :inliers (count kept) :rejected (- (count samples) (count kept))
     :accepted? (and (>= (count kept) 3) (<= rms 0.08))}))

(defn local-direction
  "Inverse of the renderer's actual normalized pan/tilt calibration, including
  negative half-circle values. No assumption that a fixture's DMX runs forward."
  [fixture pan tilt]
  (let [angle (fn [axis value]
                (let [cal (get-in fixture [:calibration axis]
                                  (if (= axis :pan) {:min-deg -180 :max-deg 180} {:min-deg -90 :max-deg 90}))
                      degrees (if (vector? cal)
                                (* 180 (/ (- (* value 255) (first cal)) (second cal)))
                                (+ (:min-deg cal) (* value (- (:max-deg cal) (:min-deg cal)))))]
                  (Math/toRadians (double degrees))))
        p (angle :pan pan) t (angle :tilt tilt)]
    [(* (Math/sin p) (Math/cos t)) (Math/sin t) (* (Math/cos p) (Math/cos t))]))

(defn- largest-eigenvector [matrix]
  ;; Jacobi rotations, bounded 4x4 symmetric eigensystem; no mutable shared state.
  (loop [a matrix v (mapv (fn [i] (mapv #(if (= i %) 1.0 0.0) (range 4))) (range 4)) iteration 0]
    (let [[p q] (apply max-key (fn [[p q]] (Math/abs (double (get-in a [p q])))) (for [p (range 4) q (range (inc p) 4)] [p q]))
          apq (get-in a [p q])]
      (if (or (= iteration 80) (< (Math/abs (double apq)) 1.0e-12))
        (let [i (apply max-key #(get-in a [% %]) (range 4))] (unit (mapv #(get-in v [% i]) (range 4))))
        (let [theta (* 0.5 (Math/atan2 (* 2 apq) (- (get-in a [q q]) (get-in a [p p]))))
              c (Math/cos theta) s (Math/sin theta)
              j (mapv (fn [i] (mapv (fn [k] (cond (= [i k] [p p]) c (= [i k] [q q]) c
                                                  (= [i k] [p q]) s (= [i k] [q p]) (- s)
                                                  (= i k) 1.0 :else 0.0)) (range 4))) (range 4))
              mul (fn [a b]
                    (mapv (fn [row]
                            (mapv (fn [k]
                                    (reduce + (map-indexed (fn [i x] (* x (get-in b [i k]))) row)))
                                  (range 4))) a))
              transpose #(apply mapv vector %)]
          (recur (mul (mul (transpose j) a) j) (mul v j) (inc iteration)))))))

(defn- fit-rotation [samples]
  (let [h (mapv (fn [i] (mapv (fn [j] (reduce + (map #(* (:weight % 1.0) (nth (:local %) i) (nth (:world %) j)) samples))) (range 3))) (range 3))
        [[xx xy xz] [yx yy yz] [zx zy zz]] h
        n [[(+ xx yy zz) (- yz zy) (- zx xz) (- xy yx)]
           [(- yz zy) (- xx yy zz) (+ xy yx) (+ zx xz)]
           [(- zx xz) (+ xy yx) (- yy xx zz) (+ yz zy)]
           [(- xy yx) (+ zx xz) (+ yz zy) (- zz xx yy)]]
        [w x y z] (largest-eigenvector n)]
    [[(- 1 (* 2 (+ (* y y) (* z z)))) (* 2 (- (* x y) (* z w))) (* 2 (+ (* x z) (* y w)))]
     [(* 2 (+ (* x y) (* z w))) (- 1 (* 2 (+ (* x x) (* z z)))) (* 2 (- (* y z) (* x w)))]
     [(* 2 (- (* x z) (* y w))) (* 2 (+ (* y z) (* x w))) (- 1 (* 2 (+ (* x x) (* y y))))]]))

(defn angular-error [a b] (Math/toDegrees (Math/acos (max -1.0 (min 1.0 (dot (unit a) (unit b)))))))

(defn rotation-euler [r]
  (let [y (Math/asin (max -1.0 (min 1.0 (- (get-in r [2 0])))))
        singular? (< (Math/abs (Math/cos y)) 1.0e-7)]
    (mapv #(Math/toDegrees %)
          [(if singular? (Math/atan2 (- (get-in r [1 2])) (get-in r [1 1])) (Math/atan2 (get-in r [2 1]) (get-in r [2 2])))
           y (if singular? 0.0 (Math/atan2 (get-in r [1 0]) (get-in r [0 0])))])))

(defn fit-aim
  "Fit mounting rotation against verified axis calibration. Does NOT guess axis
  direction and mounting simultaneously: those are ambiguous without a prior."
  [fixture position samples axis-profile-verified?]
  (when-not (and (v3? position) (<= 3 (count samples) 64)
                 (every? #(and (v3? (:target %)) (finite? (:weight % 1.0)) (< 0 (:weight % 1.0) 100) (every? (fn [x] (and (finite? x) (<= 0 x 1))) [(:pan %) (:tilt %)])) samples))
    (throw (ex-info "Aim fit needs three valid pan/tilt target observations" {})))
  (let [samples (mapv #(assoc % :local (local-direction fixture (:pan %) (:tilt %)) :world (unit (sub (:target %) position))) samples)
        spread (apply max (for [a samples b samples] (norm (cross (:local a) (:local b)))))
        ;; Deterministic pair hypotheses tolerate one mis-tapped target. Score
        ;; every hypothesis against all observations; refine its consensus.
        hypotheses (for [i (range (count samples)) j (range (inc i) (count samples))
                         :when (> (norm (cross (:local (samples i)) (:local (samples j)))) 0.15)]
                     (let [r (fit-rotation [(samples i) (samples j)])
                           kept (filterv #(< (angular-error (rotate r (:local %)) (:world %)) 3.0) samples)]
                       kept))
        kept (if (seq hypotheses) (apply max-key count hypotheses) [])]
    (if (< (count kept) 3)
      {:accepted? false :reason :insufficient-diverse-consensus :spread spread}
      (let [r (fit-rotation kept)
            rms (Math/sqrt (/ (reduce + (map #(Math/pow (angular-error (rotate r (:local %)) (:world %)) 2) kept)) (count kept)))
            up (nth (rotate r [0 1 0]) 1)]
        {:rotation-deg (rotation-euler r) :rms-deg rms :inliers (count kept) :rejected (- (count samples) (count kept))
         :accepted? (and axis-profile-verified? (> spread 0.25) (< rms 2.0))
         :axis-profile-verified? (boolean axis-profile-verified?)
         :mounting (if-not axis-profile-verified? :unknown (cond (> up 0.85) :standing (< up -0.85) :inverted :else :sideways))
         :reason (when-not axis-profile-verified? :axis-direction-mounting-ambiguity)}))))
