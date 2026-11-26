(ns tolglow.video.sampler
  "Sample a video frame along mapped strips (#672).

  A plan resolves a mapping against the rig and one frame size into flat tap
  tables: for every target pixel, texel byte offsets and weights. Planning is
  the expensive half and is reused while geometry, placement and frame size
  stay the same; sampling is then a weighted sum per pixel.

  Filtering policy
  - :box (default) averages the pixel's whole footprint: its share of the
    strip length times the sampling height. Sub-samples are spaced evenly
    over it, about one source pixel apart up to 4 × 4 per pixel, and each is
    bilinear, so a footprint smaller than a texel still blends its neighbours
    smoothly and a larger one is approximated by 16 evenly spread reads.
  - :nearest reads the one texel under the footprint centre.
  Colour is averaged in linear light: sRGB bytes decode through a table
  before weighting, and results are linear RGB in [0,1] as the effect stack
  expects. Sub-samples outside the frame contribute black, so a strip moved
  off the source fades instead of smearing its edge."
  (:require [tolglow.video.mapping :as mapping]))

(def max-subsamples-per-axis
  "Box filtering spaces at most this many bilinear sub-samples along and
  across each pixel's footprint (16 per pixel, 64 texel taps)."
  4)

(def ^:private ^"[D" srgb->linear
  (let [table (double-array 256)]
    (dotimes [i 256]
      (let [c (/ i 255.0)]
        (aset table i (if (<= c 0.04045) (/ c 12.92) (Math/pow (/ (+ c 0.055) 1.055) 2.4)))))
    table))

(defmacro ^:private add-tap!
  "Store one texel tap at index t when it lies inside the frame; evaluate to
  the next free index. Texels outside the frame are dropped, leaving their
  share black. A positive span wraps x into [x0, x0 + span) first, so a
  sphere's footprint continues across its seam."
  [offsets weights t width height x y k x0 span]
  `(let [t# (long ~t) span# (long ~span) x# (if (pos? span#) (+ (long ~x0) (Math/floorMod (- (long ~x) (long ~x0)) span#)) (long ~x))
         y# (long ~y) k# (double ~k) w# (long ~width)]
     (if (and (> k# 0.0) (>= x# 0) (< x# w#) (>= y# 0) (< y# (long ~height)))
       (do (aset ~(with-meta offsets {:tag 'ints}) t# (unchecked-int (* 4 (+ x# (* y# w#)))))
           (aset ~(with-meta weights {:tag 'floats}) t# (float k#))
           (inc t#))
       t#)))

(defn- add-bilinear!
  "Store the four taps of a bilinear sample at pixel-space [x y] (texel
  centres at +0.5) with total weight w; return the next free index."
  [^ints offsets ^floats weights t width height x y w x0 span]
  (let [t (long t) width (long width) height (long height) w (double w) x0 (long x0) span (long span)
        fx (- (double x) 0.5) fy (- (double y) 0.5)
        x0' (long (Math/floor fx)) y0 (long (Math/floor fy))
        tx (- fx (double x0')) ty (- fy (double y0))
        t (add-tap! offsets weights t width height x0' y0 (* w (- 1.0 tx) (- 1.0 ty)) x0 span)
        t (add-tap! offsets weights t width height (inc x0') y0 (* w tx (- 1.0 ty)) x0 span)
        t (add-tap! offsets weights t width height x0' (inc y0) (* w (- 1.0 tx) ty) x0 span)]
    (add-tap! offsets weights t width height (inc x0') (inc y0) (* w tx ty) x0 span)))

(defn- subsamples ^long [^double extent]
  (max 1 (min (long max-subsamples-per-axis) (long (Math/ceil extent)))))

(defn- footprint-taps!
  "Fill taps for one footprint from index t; return the next free index."
  [^ints offsets ^floats weights t width height filter [x0 span] {:keys [center direction along across]}]
  (let [t (long t) width (long width) height (long height) x0 (long (or x0 0)) span (long (or span 0))
        cx (double (nth center 0)) cy (double (nth center 1))
        dx (double (nth direction 0)) dy (double (nth direction 1))
        along (double along) across (double across)]
    (if (= filter :nearest)
      (add-tap! offsets weights t width height (long (Math/floor cx)) (long (Math/floor cy)) 1.0 x0 span)
      (let [na (subsamples along) nc (subsamples across)
            w (/ 1.0 (* na nc))]
        (loop [i 0 t t]
          (if (= i (* na nc))
            t
            (let [a (quot i nc) c (rem i nc)
                  sa (* along (- (/ (+ a 0.5) na) 0.5))
                  sc (* across (- (/ (+ c 0.5) nc) 0.5))]
              ;; The across axis is the direction turned a quarter: (-dy, dx).
              (recur (inc i)
                     (add-bilinear! offsets weights t width height
                                    (+ cx (* sa dx) (* sc (- dy))) (+ cy (* sa dy) (* sc dx)) w x0 span)))))))))

(defn- build
  "Flat tap tables for groups of {:targets :filter :footprints :wrap}."
  [groups width height]
  (let [pixels (reduce + (map (comp count :targets) groups))
        capacity (reduce + (map (fn [{:keys [filter footprints]}]
                                  (reduce + (map (fn [{:keys [along across]}]
                                                   (if (= filter :nearest) 1
                                                       (* 4 (subsamples along) (subsamples across))))
                                                 footprints)))
                                groups))
        offsets (int-array capacity) weights (float-array capacity)
        starts (int-array (inc pixels))
        used (loop [[group & more] groups p 0 t 0]
               (if-not group
                 (do (aset starts p (int t)) t)
                 (let [[p t] (reduce (fn [[p t] print]
                                       (aset starts (int p) (int t))
                                       [(inc p) (footprint-taps! offsets weights t width height (:filter group) (:wrap group) print)])
                                     [p t] (:footprints group))]
                   (recur more p t))))]
    {:width width :height height
     :targets (into [] (mapcat :targets) groups)
     :starts starts
     :offsets (java.util.Arrays/copyOf offsets (int used))
     :weights (java.util.Arrays/copyOf weights (int used))}))

(defn plan
  "Resolve a mapping for one rig and frame size.
  Returns {:targets [id ...] :starts int[] :offsets int[] :weights float[]
  :strips [{:id :targets :footprints :unresolved?} ...]} where pixel p reads
  taps starts[p] (inclusive) to starts[p+1]. placement is the cue-driven
  mapping-wide {:x :y :scale :rotation}."
  [mapping fixtures width height placement]
  (let [aspect (/ (double width) height)
        strips (mapv (fn [strip]
                       (let [targets (mapping/strip-targets strip fixtures)
                             points (mapping/place (:points strip) placement aspect)
                             filter (or (:filter strip) (:filter mapping) :box)]
                         {:id (:id strip) :targets targets :filter filter :unresolved? (empty? targets)
                          :footprints (mapping/footprints points (count targets) (:height strip 0.0) width height)}))
                     (:strips mapping))]
    (assoc (build strips width height) :strips strips)))

(defn plan-footprints
  "A plan over precomputed pixel-space footprints, one per target, in the
  shape `tolglow.video.mapping/footprints` returns. wrap [x0 x1] wraps taps
  horizontally within those columns (a sphere's seam)."
  [targets footprints filter wrap width height]
  (build [{:targets (vec targets) :footprints footprints :filter filter
           :wrap (when wrap [(first wrap) (- (second wrap) (first wrap))])}]
         width height))

(defn sample
  "Weighted linear-light average for every planned pixel. Returns a fresh
  double array of 3 × pixel count (r g b per pixel, in [0,1])."
  ^doubles [plan frame]
  (when-not (and (= (:width plan) (:width frame)) (= (:height plan) (:height frame)))
    (throw (ex-info "Plan and frame sizes differ" {:plan [(:width plan) (:height plan)]
                                                   :frame [(:width frame) (:height frame)]})))
  (let [^ints starts (:starts plan) ^ints offsets (:offsets plan) ^floats weights (:weights plan)
        ^bytes px (:pixels frame) ^doubles lut srgb->linear
        n (dec (alength starts))
        out (double-array (* 3 n))]
    (dotimes [p n]
      (let [end (aget starts (inc p))]
        (loop [t (aget starts p) r 0.0 g 0.0 b 0.0]
          (if (< t end)
            (let [o (aget offsets t) w (double (aget weights t))]
              (recur (inc t)
                     (+ r (* w (aget lut (bit-and 0xff (aget px (+ o 2))))))
                     (+ g (* w (aget lut (bit-and 0xff (aget px (+ o 1))))))
                     (+ b (* w (aget lut (bit-and 0xff (aget px o)))))))
            (let [i (* 3 p)]
              (aset out i (min 1.0 r)) (aset out (+ i 1) (min 1.0 g)) (aset out (+ i 2) (min 1.0 b)))))))
    out))

(defn colors
  "Target id → linear [r g b] for a sample. A target listed by two strips takes
  the later strip's colour, matching replace order in an effect."
  [plan ^doubles sampled]
  (let [targets (:targets plan)]
    (persistent!
     (reduce (fn [m i]
               (let [o (* 3 i)]
                 (assoc! m (nth targets i) [(aget sampled o) (aget sampled (+ o 1)) (aget sampled (+ o 2))])))
             (transient {}) (range (count targets))))))

(defn luma
  "Rec. 709 relative luminance of a linear colour."
  ^double [[r g b]] (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b)))

(defn plan-cache
  "A small identity-keyed plan cache for one owner (a compiled effect or the
  Studio projection). Plans depend only on their key, so a race recomputes the
  same value; entries beyond `limit` drop the oldest."
  ([] (plan-cache 8))
  ([limit]
   (let [cache (atom {:order [] :plans {}})]
     (fn [key build]
       (or (get-in @cache [:plans key])
           (let [built (build)]
             (swap! cache (fn [{:keys [order plans]}]
                            (let [order (conj (filterv #(not= key %) order) key)
                                  evicted (take (max 0 (- (count order) limit)) order)]
                              {:order (vec (remove (set evicted) order))
                               :plans (assoc (apply dissoc plans evicted) key built)})))
             built))))))
