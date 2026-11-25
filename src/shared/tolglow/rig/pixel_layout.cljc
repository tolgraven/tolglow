(ns tolglow.rig.pixel-layout
  "Portable 3D pixel layouts. Wire order determines head identity and DMX slots."
  (:require [clojure.string :as str]
            #?(:clj [clojure.math :as math] :cljs [cljs.math :as math])))

(def color-layouts
  {:rgb [:red :green :blue]
   :grb [:green :red :blue]
   :rgbw [:red :green :blue :white]
   :grbw [:green :red :blue :white]
   :rgbwa [:red :green :blue :white :amber]})

;; Submitted specs can represent many more heads than there are bytes in the request.
(def max-pixels 131072)

(def ^:private last-universe 2147483647)

(defn- invalid! [field message]
  (throw (ex-info message {:rig/error :invalid-pixel-layout :path [field]})))

(defn- finite? [v]
  (and (number? v) #?(:clj (Double/isFinite (double v)) :cljs (js/Number.isFinite v))))

(defn- point [v field]
  (when-not (and (vector? v) (= 3 (count v)) (every? finite? v))
    (invalid! field "Expected three finite world coordinates in metres"))
  (mapv double v))

(defn- bounded-int [v lo hi field]
  (when-not (and (integer? v) (<= lo v hi)) (invalid! field "Integer outside the supported range"))
  v)

(defn- identifier? [v] (or (keyword? v) (and (string? v) (not (str/blank? v)))))

(defn- shape [spec]
  (let [v (get spec :shape :path)]
    (if (or (keyword? v) (string? v)) (keyword v) (invalid! :shape "Unknown pixel layout shape"))))

(defn pixel-count [spec]
  (let [n (case (shape spec)
            :grid (* (bounded-int (:rows spec) 1 max-pixels :rows)
                     (bounded-int (:columns spec) 1 max-pixels :columns))
            (:path :helix) (:pixels spec)
            (invalid! :shape "Unknown pixel layout shape"))]
    (when (and (= :grid (shape spec)) (contains? spec :pixels) (not= n (:pixels spec)))
      (invalid! :pixels "Grid pixel count must equal rows times columns"))
    (bounded-int n 1 max-pixels :pixels)))

(defn- choice [spec field default]
  (let [value (get spec field default)]
    (when-not (boolean? value) (invalid! field "Expected true or false"))
    value))

(defn- add-scaled [origin offset scale]
  (mapv (fn [a b] (+ a (* scale b))) origin offset))

(defn- distance [[x y z]]
  (math/hypot x (math/hypot y z)))

(defn- segments [points]
  (reduce (fn [result [a b]]
            (let [delta (mapv - b a)
                  length (distance delta)
                  start (:end (peek result) 0.0)]
              (if (= start (+ start length)) result
                  (conj result {:origin a :delta delta :start start :end (+ start length)}))))
          [] (partition 2 1 points)))

(defn- sample-path [parts n closed? indices]
  (let [total (:end (peek parts))]
    (when-not (and (finite? total) (pos? total))
      (invalid! :points "The path must have finite positive length"))
    (loop [remaining (seq indices) cursor 0 result (transient [])]
      (if-let [i (first remaining)]
        (let [fraction (cond closed? (/ i (double n)) (= n 1) 0.5 :else (/ i (double (dec n))))
              at (* total fraction)
              {:keys [origin delta start end]} (nth parts cursor)]
          (if (> at end)
            (recur remaining (inc cursor) result)
            (let [p (add-scaled origin delta (/ (- at start) (- end start)))]
              (recur (next remaining) cursor (conj! result p)))))
        (persistent! result)))))

(defn- path-positions [{:keys [points] :as spec} n indices]
  (when-not (and (vector? points) (<= 2 (count points) max-pixels))
    (invalid! :points "A path needs at least two control points"))
  (let [points' (mapv #(point % :points) points)
        closed? (choice spec :closed? false)]
    (sample-path (segments (cond-> points' closed? (conj (first points')))) n closed? indices)))

(defn- grid-positions [{:keys [origin columns column-step row-step] :as spec} _ indices]
  (let [base (point origin :origin)
        u (point column-step :column-step)
        v (point row-step :row-step)
        snake? (choice spec :serpentine? true)]
    (when (and (> columns 1) (every? zero? u)) (invalid! :column-step "Columns need a nonzero spacing"))
    (when (and (> (:rows spec) 1) (every? zero? v)) (invalid! :row-step "Rows need a nonzero spacing"))
    (mapv (fn [i]
            (let [row (quot i columns)
                  col (mod i columns)
                  x (if (and snake? (odd? row)) (- columns 1 col) col)]
              (-> base (add-scaled u x) (add-scaled v row))))
          indices)))

(defn- helix-positions [{:keys [origin radius height turns phase-deg] :or {phase-deg 0}} n indices]
  (let [[x y z] (point origin :origin)]
    (when-not (and (finite? radius) (pos? radius)) (invalid! :radius "Helix radius must be positive and finite"))
    (when-not (every? finite? [height turns phase-deg]) (invalid! :turns "Helix height, turns and phase must be finite"))
    (mapv (fn [i]
            (let [t (if (= n 1) 0.5 (/ i (double (dec n))))
                  angle (* math/PI (+ (/ phase-deg 180.0) (* 2.0 turns t)))]
              [(+ x (* radius (math/cos angle))) (+ y (* height t)) (+ z (* radius (math/sin angle)))]))
          indices)))

(defn- override-index [v n]
  (let [v (if (and (keyword? v) (nil? (namespace v))) (name v) v)
        v' (if (and (string? v) (re-matches #"(?:0|[1-9][0-9]*)" v)) (parse-long v) v)]
    (bounded-int v' 0 (dec n) :position-overrides)))

(defn- overrides-for [spec n]
  (let [overrides (get spec :position-overrides {})]
    (when-not (and (map? overrides) (<= (count overrides) n))
      (invalid! :position-overrides "Expected indexed positions"))
    (reduce-kv (fn [acc key p]
                 (let [i (override-index key n)]
                   (when (contains? acc i)
                     (invalid! :position-overrides "Duplicate pixel override index"))
                   (assoc acc i (point p :position-overrides))))
               {} overrides)))

(defn positions
  "Sample wire-ordered positions. Supply ascending indices to preview a large layout
  without allocating positions for unseen pixels."
  ([spec] (positions spec (range (pixel-count spec))))
  ([spec indices]
   (let [n (pixel-count spec)
         sampler (case (shape spec) :path path-positions :grid grid-positions :helix helix-positions)
         overrides (overrides-for spec n)]
     (when-not (and (sequential? indices)
                    (every? #(and (integer? %) (<= 0 % (dec n))) indices)
                    (every? (fn [[a b]] (<= a b)) (partition 2 1 indices)))
       (invalid! :indices "Expected ascending wire indices in this layout"))
     (mapv (fn [i p]
             (when-not (every? finite? p) (invalid! :points "Layout calculation exceeds finite coordinates"))
             (get overrides i p))
           indices (sampler spec n indices)))))

(defn- patch-plan [spec]
  (let [order (get color-layouts (keyword (:color-layout spec :rgbw)))
        universe (bounded-int (:universe spec 1) 0 last-universe :universe)
        address (bounded-int (:address spec 1) 1 512 :address)
        start (+ (* universe 512) (dec address))]
    (when-not order (invalid! :color-layout "Unknown pixel colour channel order"))
    (let [end (+ start (dec (* (pixel-count spec) (count order))))]
      (when (> (quot end 512) last-universe) (invalid! :universe "Pixel strip exceeds the universe range"))
      {:order order :start start :end end})))

(defn- location [slot] [(quot slot 512) (inc (mod slot 512))])

(defn patch-range
  "First and last occupied [universe address], inclusive. Individual pixels can straddle universes."
  [spec]
  (let [{:keys [start end]} (patch-plan spec)]
    {:start (location start) :end (location end)}))

(defn- head-id [id index]
  (let [suffix (str (name id) "-head-" (inc index))]
    (if (keyword? id) (keyword (namespace id) suffix) suffix)))

(defn- check-identity! [{:keys [id group]}]
  (when-not (identifier? id) (invalid! :id "Layout needs a stable string or keyword id"))
  (when (and (some? group) (not (identifier? group))) (invalid! :group "Expected a fixture group id")))

(defn fixtures
  "Expand one layout into ordinary Afterglow patch targets in wire order."
  [{:keys [id group] :as spec}]
  (check-identity! spec)
  (let [{:keys [order start]} (patch-plan spec)
        width (count order)
        channels (zipmap order (range 1 (inc width)))
        points (positions spec)]
    (mapv (fn [i position]
            (let [[universe address] (location (+ start (* i width)))]
              {:id (head-id id i) :fixture-id id :pixel-layout-id id :pixel-index i
               :groups (if group [group] []) :position position :channels channels
               :universe universe :address address :footprint width}))
          (range (count points)) points)))

(defn- spelling [id]
  (if (keyword? id) (subs (str id) 1) id))

(defn- align-layout [original {:keys [group] :as spec}]
  (cond-> spec
    (contains? spec :position-overrides) (assoc :position-overrides (overrides-for spec (pixel-count spec)))
    original (assoc :id (:id original))
    (and original group (= (spelling group) (spelling (:group original))))
    (assoc :group (:group original))))

(defn- restore-layout-ids [saved specs]
  (let [originals (into {} (map (juxt (comp spelling :id) identity)) saved)]
    (mapv #(align-layout (get originals (spelling (:id %))) %) specs)))

(defn- head-key [f] [(spelling (:pixel-layout-id f)) (:pixel-index f)])

(defn- owned-heads [fixtures]
  (into {} (comp (filter :pixel-layout-id) (map (juxt head-key identity))) fixtures))

(defn- validate-collection! [specs]
  (when-not (vector? specs) (invalid! :pixel-layouts "Expected an ordered vector of pixel layouts"))
  (when-not (= (count specs) (count (distinct (map (comp spelling :id) specs))))
    (invalid! :id "Duplicate pixel layout id"))
  (when (> (reduce + 0 (map pixel-count specs)) max-pixels)
    (invalid! :pixels "Combined pixel layout exceeds the rig budget")))

(defn- count-head [counts seen {:keys [id pixel-layout-id pixel-index]}]
  (when (and pixel-layout-id
             (not (and (contains? counts pixel-layout-id) (integer? pixel-index)
                       (<= 0 pixel-index) (< pixel-index (get counts pixel-layout-id))
                       (= id (head-id pixel-layout-id pixel-index)) (not (contains? seen id)))))
    (invalid! :fixtures "Unrecognized generated pixel head"))
  (cond-> seen pixel-layout-id (conj id)))

(defn validate-materialized!
  "Check the retained layout definitions and their wire identities in a complete
  rig. Positions and patch values may have independent measured corrections."
  [{:keys [pixel-layouts fixtures] :as rig}]
  (when (contains? rig :pixel-layouts)
    (validate-collection! pixel-layouts)
    (let [counts (into {} (map (juxt :id pixel-count)) pixel-layouts)
          expected (reduce + 0 (vals counts))]
      (doseq [spec pixel-layouts]
        (check-identity! spec)
        (patch-range spec)
        (positions spec [0 (dec (get counts (:id spec)))]))
      (when-not (= expected (count (reduce (partial count-head counts) #{} fixtures)))
        (invalid! :fixtures "Generated pixel heads are missing from the patch"))))
  rig)

(def ^:private wire-keys #{:id :fixture-id :pixel-layout-id :pixel-index})

(defn- revised-head [saved before after override-changed?]
  (reduce-kv (fn [head k v]
               (if (and (contains? saved k) (not (wire-keys k)) (= (get before k) v)
                        (not (and (= :position k) override-changed?)))
                 head
                 (assoc head k v)))
             (or saved {}) after))

(defn- expand-edited [owned originals spec]
  (let [old (get originals (:id spec))
        unchanged? (= old spec)
        old-heads (when (and old (not unchanged?)) (fixtures old))
        old-overrides (:position-overrides old)
        new-overrides (:position-overrides spec)]
    (mapv (fn [{i :pixel-index :as generated}]
            (let [saved (get owned (head-key generated))]
              (cond
                (nil? saved) generated
                (nil? old) (merge saved generated)
                :else (revised-head saved (if unchanged? generated (get old-heads i)) generated
                                    (not= (get old-overrides i) (get new-overrides i))))))
          (fixtures spec))))

(defn- keep-patch-order [current specs manual generated]
  (let [before (:fixtures current)
        stable-layouts? (= (map :id specs) (map :id (:pixel-layouts current)))]
    (if (and (seq before) stable-layouts? (= (count before) (count generated))
             (= (map :id manual) (into [] (comp (remove :pixel-layout-id) (map :id)) before)))
      (let [by-id (into {} (map (juxt :id identity)) generated)]
        (if (and (= (count by-id) (count generated)) (every? #(contains? by-id (:id %)) before))
          (mapv (comp by-id :id) before)
          generated))
      generated)))

(defn materialize
  "Regenerate owned heads from the authored layouts. A compact edit can omit
  generated heads. Their calibration is inherited from the last committed rig."
  ([rig] (materialize nil rig))
  ([current rig]
   (if-not (contains? rig :pixel-layouts)
     rig
     (let [specs (:pixel-layouts rig)
           previous (:fixtures rig [])]
       (validate-collection! specs)
       (when-not (vector? previous) (invalid! :fixtures "Expected a fixture vector"))
       (let [specs (restore-layout-ids (:pixel-layouts current) specs)
             owned (merge (owned-heads (:fixtures current)) (owned-heads previous))
             originals (into {} (map (juxt :id identity)) (:pixel-layouts current))
             manual (filterv #(not (contains? % :pixel-layout-id)) previous)
             generated (into manual (mapcat (partial expand-edited owned originals)) specs)]
         (assoc rig :pixel-layouts specs :fixtures (keep-patch-order current specs manual generated)))))))

(defn prepare-startup [rig]
  (if (and (seq (:pixel-layouts rig)) (not-any? :pixel-layout-id (:fixtures rig)))
    (materialize rig)
    rig))
