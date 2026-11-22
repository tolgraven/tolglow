(ns tolglow.fixtures.heads
  "Convert fixture-builder channel records and their heads into rig targets.
  Positions and channel offsets in a builder's heads are relative to the fixture."
  (:require [tolglow.calibration.geometry :as geometry]
            [tolglow.fixtures.library :as library]))

(defn- channel-map [channels]
  (if (map? channels) channels
    (reduce (fn [mapped {:keys [type color] :as channel}]
              (let [attribute (if (= type :color) color type)]
                (when (contains? mapped attribute)
                  (throw (ex-info "Fixture target has duplicate channels for one attribute" {:attribute attribute})))
                (assoc mapped attribute (select-keys channel [:offset :fine-offset :inverted-from]))))
            {} channels)))

(defn- spec [value]
  (cond (integer? value) {:offset value}
        (vector? value) (zipmap [:offset :fine-offset] value)
        :else value))

(defn- extent [channels]
  (mapcat (fn [value] (vals (select-keys (spec value) [:offset :fine-offset]))) (vals channels)))

(defn- rebase [channels delta]
  (update-vals channels (fn [value]
                          (let [entry (spec value)]
                            (cond-> (update entry :offset - delta)
                              (:fine-offset entry) (update :fine-offset - delta))))))

(defn- world-position [origin rotation local]
  (let [[x y z] (map #(Math/toRadians (double %)) rotation)
        c #(Math/cos %) s #(Math/sin %)]
    (->> local
         (geometry/rotate [[1 0 0] [0 (c x) (- (s x))] [0 (s x) (c x)]])
         (geometry/rotate [[(c y) 0 (s y)] [0 1 0] [(- (s y)) 0 (c y)]])
         (geometry/rotate [[(c z) (- (s z)) 0] [(s z) (c z) 0] [0 0 1]])
         (geometry/add origin))))

(defn- calibration [template]
  (reduce (fn [result axis]
            (let [center (get template (keyword (str (name axis) "-center")))
                  half (get template (keyword (str (name axis) "-half-circle")))]
              (if (and center half) (assoc result axis [center half]) result)))
          (:calibration template {}) [:pan :tilt]))

(defn- instance [definition options]
  (let [{:keys [fixture diagnostics]} (library/instantiate definition options)]
    (when-not fixture (throw (ex-info "Invalid grouped fixture" {:id (:id options) :diagnostics diagnostics})))
    (merge fixture (select-keys definition [:capabilities :tags])
           (select-keys options [:capabilities :tags :truss-id]))))

(defn targets
  "Instantiate one fixture and return its targets and complete occupied span."
  [template options slot]
  (let [channels (channel-map (:channels template))
        heads (:heads template)
        definition (-> template (dissoc :heads) (assoc :channels channels :calibration (calibration template)))]
    (if-not (seq heads)
      (let [fixture (instance definition options)]
        {:fixtures [fixture] :footprint (:footprint fixture)})
      (let [head-parts (map-indexed
                        (fn [i head]
                          {:id (let [parent (:id options) id (str (name parent) "-head-" (inc i))]
                                 (if (keyword? parent) (keyword (namespace parent) id) id))
                           :optics (select-keys head [:beam-angle :zoom-range-deg :lens-radius-m :pseudo-fixture])
                           :local (mapv #(get head % 0) [:x :y :z]) :channels (channel-map (:channels head))})
                        heads)
            parts (cond->> head-parts (seq channels) (cons {:id (:id options) :channels channels}))
            parts (mapv (fn [part]
                          (let [offsets (seq (extent (:channels part)))]
                            (when-not (and offsets (every? pos-int? offsets))
                              (throw (ex-info "Fixture head needs positive channel offsets" {:id (:id part)})))
                            (assoc part :first (apply min offsets) :last (apply max offsets)))) parts)
            sorted (vec (sort-by :first parts))
            emits? (fn [{:keys [local channels]}]
                     (or (some #(contains? channels %) [:red :green :blue :white :amber :uv])
                         (and local (contains? channels :dimmer))))
            emitters (count (filter emits? sorted))
            occupied (apply max (map :last sorted))
            footprint (:footprint template occupied)
            _ (when-not (and (pos-int? footprint) (>= footprint occupied))
                (throw (ex-info "Fixture footprint excludes a head channel" {:id (:id options)})))]
        {:footprint footprint
         :fixtures
         (mapv (fn [i {:keys [id local channels optics first] :as part}]
                 (let [start (if (zero? i) 1 first)
                       delta (dec start)
                       address (+ slot delta)
                       end (get-in sorted [(inc i) :first] (inc footprint))
                       authored (-> definition (dissoc :channel-definitions :heads)
                                    (assoc :channels (rebase channels delta) :footprint (- end start))
                                    (merge optics))
                       where (assoc options :id id :address (inc (mod address 512)) :universe (inc (quot address 512)))
                       where (cond-> where local
                               (assoc :position (world-position (:position options [0 0 0])
                                                                (:rotation-deg options [0 0 0]) local)))]
                   (cond-> (instance authored where)
                     (pos? emitters) (assoc :emission-share (if (emits? part) (/ 1.0 emitters) 0.0))
                     local (assoc :fixture-id (:id options)))))
               (range) sorted)}))))
