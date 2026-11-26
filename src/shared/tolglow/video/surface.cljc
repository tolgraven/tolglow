(ns tolglow.video.surface
  "Video projected onto 3D surfaces in the venue (#674).

  A surface places one source's crop rectangle on a plane, a wavy sheet or a
  sphere in venue metres. A head near the surface takes the colour at the
  source position its nearest surface point maps to. The JVM sampler, the
  show projection and the Studio Room mesh all use these functions, so the
  Room draws the same geometry the sampler reads.

  Surface coordinates
  - u runs 0→1 across the surface and v runs 0→1 down it, matching the
    source: (u, v) = (0, 0) shows the crop's top-left corner.
  - plane and wave: local x is u (x = (u − ½)·width), local y is up
    (y = (½ − v)·height) and +z is the face normal. A wave displaces the
    sheet along z by amplitude · sin(2π(s/wavelength + phase)), where s is
    local x (axis :u) or local y (axis :v). The sheet does not wrap: a head
    whose projection falls outside [0,1]² is not in contact.
  - sphere: u is longitude and v is latitude. The local +z meridian is
    u = ½, the seam u = 0 ≡ 1 is the local −z meridian, v = 0 is the +y pole.
    u wraps across the seam and sampling footprints wrap with it.
  - World = position + R · (scale · local), with R = Ry(yaw) · Rx(pitch) ·
    Rz(roll) from :rotation-deg [pitch yaw roll].

  Contact distance is the unsigned distance from a head to the surface: exact
  for the plane and sphere; for the wave, the first-order normal distance
  |z − d(s)| / √(1 + d′(s)²), which is exact on the crests and troughs and
  within a few percent while amplitude · 2π / wavelength stays below ½. A
  head within :distance-m takes full weight; the weight then fades to zero
  over :feather-m with the spatial volumes' :linear or :smoothstep curve
  (#407). The weight is the head's write opacity, so a lower look shows
  through the feather.

  Source crop maps surface (u, v) to source coordinates
  (crop-x + u · crop-width, crop-y + v · crop-height). Footprint sizes are
  fractions of the surface's own u and v extents, so zooming the crop zooms
  what each head averages with it."
  (:require [clojure.string :as str]
            #?(:clj [clojure.math :as math] :cljs [cljs.math :as math])))

(def shapes #{:plane :wave :sphere})

(def curves #{:linear :smoothstep})

(def filters #{:box :nearest})

(def max-surfaces 16)

(def additive-placement
  "Placement keys that add to the authored value: metres, degrees, source
  units and wave cycles."
  #{:x :y :z :pitch :yaw :roll :crop-x :crop-y :phase})

(def multiplicative-placement
  "Placement keys that scale the authored value (1 leaves it)."
  #{:scale :crop-scale :amplitude :wavelength :contact})

(def placement-keys (into additive-placement multiplicative-placement))

(def default-crop {:x 0.0 :y 0.0 :width 1.0 :height 1.0})

(def default-contact {:distance-m 0.25 :feather-m 0.0 :curve :linear})

(def default-footprint
  "Sampling extent of a head that is not part of a pixel strip, and the
  across extent of strip pixels, as fractions of the surface's u and v."
  {:width 0.0 :height 0.0})

(defn- finite? [v]
  (and (number? v) #?(:clj (Double/isFinite (double v)) :cljs (js/Number.isFinite v))))

(defn- identifier? [v] (or (keyword? v) (and (string? v) (not (str/blank? v)))))

(defn- vec3? [v] (and (vector? v) (= 3 (count v)) (every? finite? v)))

(defn- error [path message] {:path path :message message})

(defn- positive? [v] (and (finite? v) (pos? v)))

(defn- nonnegative? [v] (and (finite? v) (not (neg? v))))

(defn surface-errors
  "Path/message errors for one surface. Empty when valid."
  [surface path]
  (if-not (map? surface)
    [(error path "A surface must be a map")]
    (let [{:keys [id source shape position rotation-deg size-m radius-m crop wave contact footprint strips filter]} surface]
      (vec
       (concat
        (when-not (identifier? id) [(error (conj path :id) "Surface needs a keyword or string id")])
        (when-not (identifier? source) [(error (conj path :source) "Surface needs a source id")])
        (when-not (shapes shape) [(error (conj path :shape) "Shape is :plane, :wave or :sphere")])
        (when-not (vec3? position) [(error (conj path :position) "Position is [x y z] metres")])
        (when (and (contains? surface :rotation-deg) (not (vec3? rotation-deg)))
          [(error (conj path :rotation-deg) "Rotation is [pitch yaw roll] degrees")])
        (when (and (#{:plane :wave} shape)
                   (not (and (vector? size-m) (= 2 (count size-m)) (every? positive? size-m))))
          [(error (conj path :size-m) "A sheet needs [width height] metres above zero")])
        (when (and (= :sphere shape) (not (positive? radius-m)))
          [(error (conj path :radius-m) "A sphere needs a radius above zero")])
        (when (contains? surface :crop)
          (let [{:keys [x y width height]} (merge default-crop crop)]
            (when-not (and (map? crop) (every? #{:x :y :width :height} (keys crop))
                           (finite? x) (finite? y) (positive? width) (positive? height))
              [(error (conj path :crop) "Crop is {:x :y :width :height} in source units, with positive size")])))
        (when (contains? surface :wave)
          (let [{:keys [amplitude-m wavelength-m phase speed axis]} wave]
            (when-not (and (map? wave) (every? #{:amplitude-m :wavelength-m :phase :speed :axis} (keys wave))
                           (or (nil? amplitude-m) (nonnegative? amplitude-m))
                           (or (nil? wavelength-m) (positive? wavelength-m))
                           (or (nil? phase) (finite? phase)) (or (nil? speed) (finite? speed))
                           (or (nil? axis) (#{:u :v} axis)))
              [(error (conj path :wave) "Wave takes :amplitude-m ≥ 0, :wavelength-m > 0, :phase and :speed (cycles per beat), and :axis :u or :v")])))
        (when (contains? surface :contact)
          (let [{:keys [distance-m feather-m curve]} (merge default-contact contact)]
            (when-not (and (map? contact) (every? #{:distance-m :feather-m :curve} (keys contact))
                           (nonnegative? distance-m) (nonnegative? feather-m) (curves curve))
              [(error (conj path :contact) "Contact takes :distance-m ≥ 0, :feather-m ≥ 0 and :curve :linear or :smoothstep")])))
        (when (contains? surface :footprint)
          (let [{:keys [width height]} (merge default-footprint footprint)]
            (when-not (and (map? footprint) (every? #{:width :height} (keys footprint))
                           (nonnegative? width) (<= width 1) (nonnegative? height) (<= height 1))
              [(error (conj path :footprint) "Footprint width and height are fractions of the surface in [0,1]")])))
        (when (contains? surface :strips)
          (if-not (and (map? strips) (every? identifier? (keys strips)))
            [(error (conj path :strips) "Strip sampling is a map from pixel layout id to {:height h}")]
            (for [[layout spec] strips
                  :when (not (and (map? spec) (every? #{:height} (keys spec))
                                  (nonnegative? (:height spec)) (<= (:height spec) 1)))]
              (error (conj path :strips layout) "A strip's sampling height is a fraction of the surface height in [0,1]"))))
        (when (and (contains? surface :targets)
                   (not (and (vector? (:targets surface)) (every? identifier? (:targets surface)))))
          [(error (conj path :targets) "Targets are a vector of fixture or head ids")])
        (when (and (contains? surface :filter) (not (filters filter)))
          [(error (conj path :filter) "Filter is :box or :nearest")]))))))

(defn surfaces-errors
  "Errors for [:settings :video :surfaces], an id → surface map."
  [surfaces]
  (cond
    (nil? surfaces) []
    (not (map? surfaces)) [(error [:surfaces] "Surfaces are a map by id")]
    (> (count surfaces) max-surfaces) [(error [:surfaces] (str "A show uses at most " max-surfaces " video surfaces"))]
    :else (vec (mapcat (fn [[id surface]]
                         (concat (surface-errors surface [:surfaces id])
                                 (when (and (map? surface) (not= id (:id surface)))
                                   [(error [:surfaces id :id] "Surface id must match its key")])))
                       surfaces))))

;; Placement ------------------------------------------------------------------

(defn- radians [degrees] (* (double degrees) (/ math/PI 180.0)))

(defn resolve-surface
  "The surface after a cue's placement and the musical clock: every value the
  sampler and the Room need, in one plain map. placement keys are listed in
  `placement-keys`; elapsed-beats drives :wave :speed (nil reads as 0)."
  [surface placement elapsed-beats]
  (let [p (or placement {})
        add #(+ (double %1) (double (get p %2 0.0)))
        mul #(* (double %1) (double (get p %2 1.0)))
        [px py pz] (:position surface)
        [rp ry rr] (:rotation-deg surface [0 0 0])
        {cx :x cy :y cw :width ch :height} (merge default-crop (:crop surface))
        zoom (double (get p :crop-scale 1.0))
        cw' (* cw zoom) ch' (* ch zoom)
        wave (:wave surface)
        contact (merge default-contact (:contact surface))]
    (cond-> {:id (:id surface) :source (:source surface) :shape (:shape surface)
             :position [(add px :x) (add py :y) (add pz :z)]
             :rotation-deg [(add rp :pitch) (add ry :yaw) (add rr :roll)]
             :scale (mul 1.0 :scale)
             ;; Crop zoom keeps the crop centred; offsets move it in source units.
             :crop {:x (+ (add cx :crop-x) (* 0.5 (- cw cw'))) :y (+ (add cy :crop-y) (* 0.5 (- ch ch')))
                    :width cw' :height ch'}
             :contact {:distance-m (mul (:distance-m contact) :contact)
                       :feather-m (mul (:feather-m contact) :contact)
                       :curve (:curve contact)}
             :footprint (merge default-footprint (:footprint surface))
             :strips (:strips surface {})
             :filter (:filter surface :box)}
      (#{:plane :wave} (:shape surface)) (assoc :size-m (mapv double (:size-m surface)))
      (= :sphere (:shape surface)) (assoc :radius-m (double (:radius-m surface)))
      (= :wave (:shape surface))
      (assoc :wave {:amplitude-m (mul (:amplitude-m wave 0.25) :amplitude)
                    :wavelength-m (mul (:wavelength-m wave 2.0) :wavelength)
                    :axis (:axis wave :u)
                    :phase (+ (double (:phase wave 0.0)) (double (get p :phase 0.0))
                              (* (double (:speed wave 0.0)) (double (or elapsed-beats 0.0))))})
      (contains? surface :targets) (assoc :targets (:targets surface)))))

(defn rotation
  "Row-major 3 × 3 R = Ry(yaw) · Rx(pitch) · Rz(roll)."
  [[pitch yaw roll]]
  (let [a (radians pitch) b (radians yaw) c (radians roll)
        ca (math/cos a) sa (math/sin a) cb (math/cos b) sb (math/sin b) cc (math/cos c) sc (math/sin c)]
    ;; Ry·Rx = [[cb, sb·sa, sb·ca], [0, ca, −sa], [−sb, cb·sa, cb·ca]], then · Rz.
    [[(+ (* cb cc) (* sb sa sc)) (+ (* cb (- sc)) (* sb sa cc)) (* sb ca)]
     [(* ca sc) (* ca cc) (- sa)]
     [(+ (* (- sb) cc) (* cb sa sc)) (+ (* sb sc) (* cb sa cc)) (* cb ca)]]))

(defn- mat* [[r0 r1 r2] [x y z]]
  (let [row (fn [[a b c]] (+ (* a x) (* b y) (* c z)))] [(row r0) (row r1) (row r2)]))

(defn- mat-t* [[[a b c] [d e f] [g h i]] [x y z]]
  [(+ (* a x) (* d y) (* g z)) (+ (* b x) (* e y) (* h z)) (+ (* c x) (* f y) (* i z))])

(defn- wave-height [{:keys [amplitude-m wavelength-m phase]} s]
  (* (double amplitude-m) (math/sin (* 2.0 math/PI (+ (/ (double s) wavelength-m) phase)))))

(defn- wave-slope [{:keys [amplitude-m wavelength-m phase]} s]
  (* (double amplitude-m) (/ (* 2.0 math/PI) wavelength-m)
     (math/cos (* 2.0 math/PI (+ (/ (double s) wavelength-m) phase)))))

(defn local-point
  "Surface-local metres (before scale) of surface coordinates [u v]."
  [{:keys [shape size-m radius-m wave]} [u v]]
  (case shape
    :sphere (let [theta (* 2.0 math/PI (- u 0.5)) phi (* math/PI v) r (double radius-m)]
              [(* r (math/sin phi) (math/sin theta)) (* r (math/cos phi)) (* r (math/sin phi) (math/cos theta))])
    (let [[w h] size-m x (* (- u 0.5) w) y (* (- 0.5 v) h)]
      [x y (if (= :wave shape) (wave-height wave (if (= :v (:axis wave)) y x)) 0.0)])))

(defn world-point
  "Venue metres of surface coordinates [u v] on a resolved surface."
  [{:keys [position rotation-deg scale] :as resolved} uv]
  (let [r (rotation rotation-deg) s (double scale)]
    (mapv + position (mat* r (mapv #(* s %) (local-point resolved uv))))))

(defn project
  "Nearest surface coordinates and unsigned contact distance (venue metres) of
  a venue point: {:uv [u v] :distance d :inside? bool}. Sheets report
  :inside? false off their edges; spheres are inside everywhere but their
  centre."
  ([resolved point] (project resolved (rotation (:rotation-deg resolved)) point))
  ([{:keys [shape position scale size-m radius-m wave]} r point]
   (let [s (double scale)
         [x y z] (mapv #(/ % s) (mat-t* r (mapv - point position)))]
     (case shape
       :sphere (let [len (math/sqrt (+ (* x x) (* y y) (* z z)))]
                 (if (zero? len)
                   {:uv [0.5 0.5] :distance (* s radius-m) :inside? false}
                   {:uv [(let [u (+ 0.5 (/ (math/atan2 x z) (* 2.0 math/PI)))] (if (>= u 1.0) 0.0 u))
                         (/ (math/acos (max -1.0 (min 1.0 (/ y len)))) math/PI)]
                    :distance (* s (abs (- len radius-m)))
                    :inside? true}))
       (let [[w h] size-m
             u (+ 0.5 (/ x w)) v (- 0.5 (/ y h))
             d (if (= :wave shape)
                 (let [t (if (= :v (:axis wave)) y x)
                       slope (wave-slope wave t)]
                   (/ (abs (- z (wave-height wave t))) (math/sqrt (+ 1.0 (* slope slope)))))
                 (abs z))]
         {:uv [u v] :distance (* s d) :inside? (and (<= 0.0 u 1.0) (<= 0.0 v 1.0))})))))

(defn contact-weight
  "Write opacity of a head at distance d: 1 up to :distance-m, fading to 0
  over :feather-m with the volumes' curve; 0 beyond."
  [{:keys [distance-m feather-m curve]} d]
  (let [d (double d) inner (double distance-m) feather (double feather-m)]
    (cond
      (<= d inner) 1.0
      (or (zero? feather) (>= d (+ inner feather))) 0.0
      :else (let [x (/ (- (+ inner feather) d) feather)]
              (if (= :smoothstep curve) (* x x (- 3.0 (* 2.0 x))) x)))))

(defn source-uv
  "Source coordinates of surface coordinates on a resolved surface's crop."
  [{{:keys [x y width height]} :crop} [u v]]
  [(+ x (* u width)) (+ y (* v height))])

(defn reach-m
  "How far from its origin a resolved surface can put a qualifying head."
  [{:keys [shape scale size-m radius-m wave contact]}]
  (+ (* (double scale)
        (if (= :sphere shape) (double radius-m)
            (+ (* 0.5 (math/hypot (first size-m) (second size-m)))
               (if (= :wave shape) (double (:amplitude-m wave)) 0.0))))
     ;; A wave's first-order distance is the vertical gap shrunk by at most
     ;; √(1 + max slope²), so a qualifying head can sit that much farther out.
     (* (+ (double (:distance-m contact)) (double (:feather-m contact)))
        (if (= :wave shape)
          (math/sqrt (+ 1.0 (math/pow (/ (* 2.0 math/PI (:amplitude-m wave)) (:wavelength-m wave)) 2)))
          1.0))))

(defn bounds
  "Axis-aligned venue box containing every head the surface can touch."
  [resolved]
  (let [r (reach-m resolved) p (:position resolved)]
    {:min (mapv #(- % r) p) :max (mapv #(+ % r) p)}))

(defn contacts
  "Heads in contact with a resolved surface, in fixture order: {:id :uv
  :distance :weight} plus :layout and :pixel-index for pixel heads.
  candidate? optionally prunes ids first (the JVM passes the spatial rig's
  box candidates)."
  ([resolved fixtures] (contacts resolved fixtures nil))
  ([resolved fixtures candidate?]
   (let [r (rotation (:rotation-deg resolved))
         contact (:contact resolved)
         allowed (some-> (:targets resolved) set)]
     (into []
           (keep (fn [{:keys [id position pixel-layout-id pixel-index]}]
                   (when (and (vec3? position)
                              (or (nil? allowed) (allowed id))
                              (or (nil? candidate?) (candidate? id)))
                     (let [{:keys [uv distance inside?]} (project resolved r position)
                           w (if inside? (contact-weight contact distance) 0.0)]
                       (when (pos? w)
                         (cond-> {:id id :uv uv :distance distance :weight w}
                           pixel-layout-id (assoc :layout pixel-layout-id :pixel-index pixel-index)))))))
           fixtures))))

(defn- wrap-delta [shape du]
  (if (= :sphere shape) (- du (math/round du)) du))

(defn footprints
  "Pixel-space sampling footprints, one per contact, in the sampler's shape
  ({:center :direction :along :across}) for a frame of width × height.
  A pixel strip's along extent and direction follow its neighbouring pixels
  in contact, per pixel index step (across the sphere seam by the short
  way); its across extent is
  the layout's :strips height, else :footprint :height. Other heads use the
  :footprint width and height."
  [{:keys [shape crop footprint strips]} contacts width height]
  (let [sx (* (double width) (:width crop)) sy (* (double height) (:height crop))
        default-across (* sy (:height footprint))
        by-layout (group-by :layout (filter :layout contacts))
        neighbours (into {}
                         (mapcat (fn [[_ pixels]]
                                   (let [sorted (vec (sort-by :pixel-index pixels))]
                                     (map-indexed (fn [i c]
                                                    [(:id c) [(when (pos? i) (nth sorted (dec i)))
                                                              (when (< (inc i) (count sorted)) (nth sorted (inc i)))]])
                                                  sorted))))
                         by-layout)
        ;; Per-pixel step between two contacts of one strip. Pixels out of
        ;; contact between them divide the step evenly.
        delta (fn [a b]
                (let [[ua va] (:uv a) [ub vb] (:uv b)
                      gap (double (max 1 (abs (- (long (:pixel-index b 0)) (long (:pixel-index a 0))))))]
                  [(/ (* sx (wrap-delta shape (- ub ua))) gap) (/ (* sy (- vb va)) gap)]))]
    (mapv (fn [{:keys [id uv layout] :as contact}]
            (let [[su sv] (source-uv {:crop crop} uv)
                  center [(* (double width) su) (* (double height) sv)]]
              (if-let [[prev next] (when layout (get neighbours id))]
                (let [steps (keep identity [(when prev (delta prev contact)) (when next (delta contact next))])
                      [dx dy] (if (seq steps) (mapv + (first steps) (or (second steps) [0 0])) [1 0])
                      len (math/hypot dx dy)
                      along (if (seq steps) (/ (reduce + (map #(apply math/hypot %) steps)) (count steps)) 0.0)]
                  {:center center :direction (if (pos? len) [(/ dx len) (/ dy len)] [1.0 0.0])
                   :along along
                   :across (if-let [h (get-in strips [layout :height])] (* sy h) default-across)})
                {:center center :direction [1.0 0.0] :along (* sx (:width footprint)) :across default-across})))
          contacts)))

(defn wrap-span
  "For a sphere, the crop's pixel columns [x0 x1) that sampling wraps
  within across the seam; nil for sheets."
  [{:keys [shape crop]} width]
  (when (= :sphere shape)
    (let [x0 (math/round (* (double width) (:x crop)))
          x1 (math/round (* (double width) (+ (:x crop) (:width crop))))]
      (when (> x1 x0) [x0 x1]))))

(defn mesh
  "A triangle grid over the surface for the Room: :positions (flat venue
  xyz), :uvs (flat source u v on the crop, v down), :indices, with
  columns × rows quads. Spheres repeat the seam column so the texture wraps
  without a smear."
  ([resolved] (mesh resolved (if (= :sphere (:shape resolved)) [48 24] (if (= :wave (:shape resolved)) [48 12] [1 1]))))
  ([resolved [columns rows]]
   (let [verts (for [j (range (inc rows)) i (range (inc columns))]
                 (let [uv [(/ i (double columns)) (/ j (double rows))]]
                   [(world-point resolved uv) (source-uv resolved uv)]))
         stride (inc columns)]
     {:columns columns :rows rows
      :positions (into [] (mapcat first) verts)
      :uvs (into [] (mapcat second) verts)
      :indices (into [] (mapcat (fn [[i j]]
                                  (let [a (+ i (* j stride)) b (inc a) c (+ a stride) d (inc c)]
                                    [a c b b c d])))
                     (for [j (range rows) i (range columns)] [i j]))})))

;; Cues ----------------------------------------------------------------------

(def cue-params
  "Parameters of a spawned surface cue, one per placement key, at their
  identity values. Additive keys default to 0 and multiplicative ones to 1."
  {:crop-x {:type :number :default 0.0 :bounds [-1.0 1.0]}
   :crop-y {:type :number :default 0.0 :bounds [-1.0 1.0]}
   :crop-scale {:type :number :default 1.0 :bounds [0.05 4.0]}
   :x {:type :number :default 0.0 :bounds [-20.0 20.0]}
   :y {:type :number :default 0.0 :bounds [-20.0 20.0]}
   :z {:type :number :default 0.0 :bounds [-20.0 20.0]}
   :scale {:type :number :default 1.0 :bounds [0.05 10.0]}
   :pitch {:type :number :default 0.0 :bounds [-180.0 180.0]}
   :yaw {:type :number :default 0.0 :bounds [-180.0 180.0]}
   :roll {:type :number :default 0.0 :bounds [-180.0 180.0]}
   :amplitude {:type :number :default 1.0 :bounds [0.0 4.0]}
   :wavelength {:type :number :default 1.0 :bounds [0.1 10.0]}
   :phase {:type :number :default 0.0 :bounds [-1.0 1.0]}
   :contact {:type :number :default 1.0 :bounds [0.0 4.0]}})

(defn cue-effect
  "An effect definition that samples surface-id with every placement key
  bound to a cue parameter of the same name, so the ordinary parameter
  controls, modulation and device mappings move the crop and the surface."
  [id surface-id {:keys [name channel] :or {channel :color}}]
  {:id id :version 1 :name (or name (str "Video on " (if (keyword? surface-id) (subs (str surface-id) 1) surface-id)))
   :params cue-params
   :outputs {(if (= :luma channel) :dimmer :color)
             {:op :video :surface surface-id :channel channel
              :placement (into {} (map (fn [k] [k {:op :param :key k}])) (keys cue-params))}}})

;; Operations ----------------------------------------------------------------

(defn apply-operation
  "Apply a surface edit to [:settings :video]: {:op :put-surface :surface} or
  {:op :delete-surface :id}. Validation of the whole settings happens in
  `tolglow.video.mapping/apply-operation`."
  [settings {:keys [op] :as operation}]
  (case op
    :put-surface (assoc-in settings [:surfaces (get-in operation [:surface :id])] (:surface operation))
    :delete-surface (update settings :surfaces dissoc (:id operation))))
