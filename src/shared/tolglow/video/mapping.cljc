(ns tolglow.video.mapping
  "Portable 2D video strip mappings (#672). A mapping places LED strips or
  fixture groups as polylines over one video source. Coordinates are
  normalized to the source: u runs 0→1 left to right, v runs 0→1 top to
  bottom, so a mapping survives a sender changing resolution. Strip direction
  is point order: the first pixel samples near the first point. Height is the
  sampling extent across the strip, as a fraction of the source height.

  The show stores mappings and source selections under [:settings :video].
  Studio and the REPL edit them with the same pure helpers and one message
  type, `:video-mapping`. Geometry here is shared with the Studio overlay, so
  the preview draws the exact footprints the sampler reads."
  (:require [clojure.string :as str]
            #?(:clj [clojure.math :as math] :cljs [cljs.math :as math])))

(def filters #{:box :nearest})

(def source-kinds #{:test-pattern :image :feed :syphon})

(def max-strips 512)

(def max-points 256)

(defn- finite? [v]
  (and (number? v) #?(:clj (Double/isFinite (double v)) :cljs (js/Number.isFinite v))))

(defn identifier? [v] (or (keyword? v) (and (string? v) (not (str/blank? v)))))

(defn- error [path message] {:path path :message message})

(defn- point-errors [points path]
  (cond
    (not (and (vector? points) (<= 2 (count points) max-points)))
    [(error path (str "A strip needs 2–" max-points " points"))]
    (not (every? #(and (vector? %) (= 2 (count %)) (every? finite? %)) points))
    [(error path "Points are [u v] pairs of finite source coordinates")]
    :else []))

(defn strip-errors [strip path]
  (if-not (map? strip)
    [(error path "A strip must be a map")]
    (let [{:keys [id layout targets height filter reverse?]} strip]
      (vec (concat
            (when-not (identifier? id) [(error (conj path :id) "Strip needs a keyword or string id")])
            (cond
              (and (contains? strip :layout) (contains? strip :targets))
              [(error path "Choose either a pixel layout or explicit targets")]
              (contains? strip :layout)
              (when-not (identifier? layout) [(error (conj path :layout) "Layout must be a pixel layout id")])
              (contains? strip :targets)
              (when-not (and (vector? targets) (seq targets) (every? identifier? targets))
                [(error (conj path :targets) "Targets are a nonempty vector of fixture or head ids")])
              :else [(error path "Name a pixel layout or explicit targets")])
            (point-errors (:points strip) (conj path :points))
            (when (and (contains? strip :height) (not (and (finite? height) (<= 0 height 1))))
              [(error (conj path :height) "Sampling height is a fraction of the source height in [0,1]")])
            (when (and (contains? strip :filter) (not (filters filter)))
              [(error (conj path :filter) "Filter is :box or :nearest")])
            (when (and (contains? strip :reverse?) (not (boolean? reverse?)))
              [(error (conj path :reverse?) "Reverse is true or false")]))))))

(defn mapping-errors [mapping path]
  (if-not (map? mapping)
    [(error path "A mapping must be a map")]
    (let [{:keys [id source strips filter]} mapping]
      (vec (concat
            (when-not (identifier? id) [(error (conj path :id) "Mapping needs a keyword or string id")])
            (when-not (identifier? source) [(error (conj path :source) "Mapping needs a source id")])
            (when (and (contains? mapping :filter) (not (filters filter)))
              [(error (conj path :filter) "Filter is :box or :nearest")])
            (if-not (and (vector? strips) (<= (count strips) max-strips))
              [(error (conj path :strips) (str "Strips are a vector of at most " max-strips))]
              (concat
               (mapcat (fn [i strip] (strip-errors strip (conj path :strips i))) (range) strips)
               (let [ids (map :id strips)]
                 (when-not (= (count ids) (count (distinct ids)))
                   [(error (conj path :strips) "Strip ids must be unique within a mapping")])))))))))

(defn source-errors [source path]
  (if-not (map? source)
    [(error path "A source must be a map")]
    (let [{:keys [kind width height app name file]} source]
      (vec (concat
            (when-not (source-kinds kind) [(error (conj path :kind) "Source kind is :test-pattern, :image, :feed or :syphon")])
            (when (= :test-pattern kind)
              (concat
               (when-not (or (nil? width) (and (integer? width) (<= 1 width 4096)))
                 [(error (conj path :width) "Width is 1–4096 pixels")])
               (when-not (or (nil? height) (and (integer? height) (<= 1 height 4096)))
                 [(error (conj path :height) "Height is 1–4096 pixels")])))
            (when (and (#{:image :feed} kind) (not (and (string? file) (not (str/blank? file)))))
              [(error (conj path :file) "Name the file to read")])
            (when (and (= :syphon kind) (not (and (string? app) (string? name))))
              [(error path "A Syphon source names its application and server")])
            ;; The helper sizes its feed file from these, so they stay bounded.
            (for [k [:max-width :max-height]
                  :let [v (get source k)]
                  :when (and (contains? source k) (not (and (integer? v) (<= 16 v 4096))))]
              (error (conj path k) "The Syphon feed limit is 16–4096 pixels")))))))

(defn settings-errors
  "Path/message errors for [:settings :video]. Empty when valid."
  [settings]
  (if-not (map? settings)
    [(error [] "Video settings must be a map")]
    (let [{:keys [sources mappings]} settings]
      (vec (concat
            (when-not (or (nil? sources) (map? sources)) [(error [:sources] "Sources are a map by id")])
            (when (map? sources)
              (mapcat (fn [[id source]]
                        (concat (when-not (identifier? id) [(error [:sources] "Source ids are keywords or strings")])
                                (source-errors source [:sources id]))) sources))
            (when-not (or (nil? mappings) (map? mappings)) [(error [:mappings] "Mappings are a map by id")])
            (when (map? mappings)
              (mapcat (fn [[id mapping]]
                        (concat (mapping-errors mapping [:mappings id])
                                (when (and (map? mapping) (not= id (:id mapping)))
                                  [(error [:mappings id :id] "Mapping id must match its key")])))
                      mappings)))))))

(defn validate!
  "Return settings or throw ex-info with :errors."
  [settings]
  (let [errors (settings-errors settings)]
    (when (seq errors)
      (throw (ex-info (str "Invalid video settings: " (:message (first errors)))
                      {:errors (mapv #(update % :path (partial into [:settings :video])) errors)})))
    settings))

;; Operations ---------------------------------------------------------------

(defn- strip-index [mapping strip-id]
  (first (keep-indexed (fn [i strip] (when (= strip-id (:id strip)) i)) (:strips mapping))))

(defn- require-mapping [settings id]
  (or (get-in settings [:mappings id])
      (throw (ex-info "Unknown video mapping" {:errors [(error [:mapping-id] "Unknown video mapping")]}))))

(defn apply-operation
  "Apply one portable edit to [:settings :video] and validate the result.
  Operations: {:op :put-source :id :source}, {:op :delete-source :id},
  {:op :put-mapping :mapping}, {:op :delete-mapping :id},
  {:op :put-strip :mapping-id :strip} (replace by :id or append),
  {:op :delete-strip :mapping-id :strip-id}."
  [settings {:keys [op] :as operation}]
  (let [settings (or settings {})
        next (case op
               :put-source (assoc-in settings [:sources (:id operation)] (:source operation))
               :delete-source (do (when (some #(= (:id operation) (:source %)) (vals (:mappings settings)))
                                    (throw (ex-info "A mapping still reads this source"
                                                    {:errors [(error [:id] "A mapping still reads this source")]})))
                                  (update settings :sources dissoc (:id operation)))
               :put-mapping (let [mapping (update (:mapping operation) :strips #(or % []))]
                              (assoc-in settings [:mappings (:id mapping)] mapping))
               :delete-mapping (update settings :mappings dissoc (:id operation))
               :put-strip (let [mapping (require-mapping settings (:mapping-id operation))
                                strip (:strip operation)
                                i (strip-index mapping (:id strip))]
                            (assoc-in settings [:mappings (:mapping-id operation) :strips]
                                      (if i (assoc (:strips mapping) i strip) (conj (:strips mapping) strip))))
               :delete-strip (let [mapping (require-mapping settings (:mapping-id operation))]
                               (assoc-in settings [:mappings (:mapping-id operation) :strips]
                                         (filterv #(not= (:strip-id operation) (:id %)) (:strips mapping))))
               (throw (ex-info "Unknown video mapping operation"
                               {:errors [(error [:op] "Unknown video mapping operation")]})))]
    (validate! next)))

;; Geometry -----------------------------------------------------------------

(defn centroid [points]
  (let [n (count points)]
    [(/ (reduce + (map first points)) n) (/ (reduce + (map second points)) n)]))

(defn translate
  "Move points by [du dv] source units."
  [points [du dv]]
  (mapv (fn [[u v]] [(+ u du) (+ v dv)]) points))

(defn rotate
  "Rotate points by degrees about pivot. Rotation happens in pixel space for a
  source of the given aspect (width / height), so a strip keeps its shape on a
  16:9 frame. Positive degrees turn clockwise on screen (v points down)."
  ([points degrees aspect] (rotate points degrees aspect (centroid points)))
  ([points degrees aspect [pu pv]]
   (let [a (* degrees (/ math/PI 180.0)) c (math/cos a) s (math/sin a)]
     (mapv (fn [[u v]]
             (let [x (* (- u pu) aspect) y (- v pv)]
               [(+ pu (/ (- (* c x) (* s y)) aspect)) (+ pv (+ (* s x) (* c y)))]))
           points))))

(defn scale
  "Scale points about pivot. This sets strip length without changing height."
  ([points factor] (scale points factor (centroid points)))
  ([points factor [pu pv]]
   (mapv (fn [[u v]] [(+ pu (* factor (- u pu))) (+ pv (* factor (- v pv)))]) points)))

(defn reverse-direction [strip] (update strip :points (comp vec rseq)))

(def default-placement {:x 0.0 :y 0.0 :scale 1.0 :rotation 0.0})

(defn place
  "Apply a mapping-wide placement (cue-drivable offset, scale and rotation in
  degrees) about the source centre, in normalized coordinates."
  [points {:keys [x y scale rotation] :or {x 0.0 y 0.0 scale 1.0 rotation 0.0}} aspect]
  (if (and (zero? x) (zero? y) (== 1 scale) (zero? rotation))
    points
    (-> points
        (tolglow.video.mapping/scale scale [0.5 0.5])
        (rotate rotation aspect [0.5 0.5])
        (translate [x y]))))

(defn- pixel-points [points width height]
  (mapv (fn [[u v]] [(* u width) (* v height)]) points))

(defn- seg-length [[ax ay] [bx by]] (math/hypot (- bx ax) (- by ay)))

(defn footprints
  "Pixel-space sampling footprints for n pixels along a polyline: one map per
  pixel with :center [x y], unit :direction along the strip, :along (pixel
  length of its share of the strip) and :across (sampling height in pixels).
  Pixel i covers arc length [i/n, (i+1)/n) and samples at its midpoint."
  [points n height-fraction width height]
  (let [pts (pixel-points points width height)
        segs (mapv vector pts (rest pts))
        lengths (mapv #(apply seg-length %) segs)
        total (reduce + lengths)
        along (if (pos? n) (/ total n) 0.0)
        across (* height-fraction height)]
    (loop [i 0, seg 0, start 0.0, out (transient [])]
      (if (= i n)
        (persistent! out)
        (let [target (* (+ i 0.5) along)
              [seg start] (loop [seg seg start start]
                            (if (and (< seg (dec (count segs))) (> target (+ start (nth lengths seg))))
                              (recur (inc seg) (+ start (nth lengths seg)))
                              [seg start]))
              [[ax ay] [bx by]] (nth segs seg)
              len (nth lengths seg)
              t (if (pos? len) (/ (- target start) len) 0.0)
              dx (if (pos? len) (/ (- bx ax) len) 1.0)
              dy (if (pos? len) (/ (- by ay) len) 0.0)]
          (recur (inc i) seg start
                 (conj! out {:center [(+ ax (* t (- bx ax))) (+ ay (* t (- by ay)))]
                             :direction [dx dy] :along along :across across})))))))

(defn strip-targets
  "Ordered target ids for a strip against the rig's expanded fixtures: a pixel
  layout's heads in wire order, or explicit targets as given. Reversal flips
  which end the first head samples."
  [strip fixtures]
  (let [ids (if (contains? strip :layout)
              (->> fixtures
                   (filter #(= (:layout strip) (:pixel-layout-id %)))
                   (sort-by :pixel-index)
                   (mapv :id))
              (let [known (set (map :id fixtures))]
                (filterv known (:targets strip))))]
    (if (:reverse? strip) (vec (rseq ids)) ids)))
