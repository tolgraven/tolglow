(require '[afterglow.render.optics :as optics]
         '[clojure.string :as str]
         '[tolglow.rig.surfaces :as surfaces])

(defn panel [id x y z w h]
  {:id id :point [x y z] :normal [0 0 1] :u-axis [1 0 0] :size [w h]})

(def rig
  {:surfaces [(panel :screen-left -1.5 2 -2 1 4)
              (panel :screen-right 1.5 2 -2 1 4)
              (panel :screen-top 0 3.5 -2 2 1)
              (panel :screen-bottom 0 0.5 -2 2 1)
              (panel :overlap-first 3.5 2 -2 1 2)
              (panel :overlap-second 3.75 2 -2 1 2)
              (panel :almost-far -3.5 2 -2.00001 1 2)
              (panel :almost-near -3.5 2 -2 1 2)
              (assoc (panel :oblique -4.5 2 0 1 2) :normal [1 0 1])]})

;; The explicit assertions keep a change in the CPU oracle from silently
;; redefining a case. Each ray uses the same start and direction for both tests.
(def cases
  [["opaque-strip"       [1.75 2 0] [1.75 2 -4] :screen-right true]
   ["cutout-clear"       [0 2 0] [0 2 -4] :back false]
   ["wall-behind-cutout" [0 2 0] [0 2 -6] :back true]
   ["inside-finite-edge" [1.99 2 0] [1.99 2 -4] :screen-right true]
   ["outside-finite-edge" [2.01 2 0] [2.01 2 -4] :back false]
   ["inclusive-finite-edge" [2 2 0] [2 2 -4] :screen-right true]
   ["cutout-jamb"        [1 2 0] [1 2 -4] :screen-right true]
   ["triangle-seam"      [1.5 2 0] [1.5 2 -4] :screen-right true]
   ["coplanar-overlap"   [3.6 2 0] [3.6 2 -4] :overlap-first true]
   ["near-tie-distinct"  [-3.5 2 0] [-3.5 2 -4] :almost-near true]
   ["wall-floor-tie"     [5 2 -3] [5 -1 -6] :floor true]
   ["start-on-surface"   [1.75 2 -2] [1.75 2 -4] :back false]
   ["end-on-surface"     [1.75 2 0] [1.75 2 -2] :screen-right false]
   ["before-endpoint"    [1.75 2 0] [1.75 2 -2.001] :screen-right true]
   ["coplanar-segment"   [1.5 2 -2] [1.75 2 -2] :right false]
   ["two-sided-hit"      [1.75 2 -4] [1.75 2 -1] :screen-right true]
   ["oblique-receiver"   [-4.5 2 2] [-4.5 2 -1] :oblique true]
   ["outside-room"       [6.125 2 0] [6.125 2 -8] nil false]
   ["open-ceiling"       [0 4 0] [0 7 0] nil false]
   ["stage-above-floor"  [0 3 -4] [0 -1 -4] :stage true]])

(defn row [& values]
  (println (str/join "\t" values)))

(defn triangles [{[w h] :size :keys [id point] :as receiver}]
  (let [{:keys [u v]} (surfaces/surface-axes receiver)
        corner (fn [x y] (mapv (fn [p a b] (+ p (* a x 0.5 w) (* b y 0.5 h))) point u v))
        a (corner -1 -1) b (corner 1 -1) c (corner 1 1) d (corner -1 1)]
    (doseq [triangle [[a b c] [a c d]]]
      (apply row "tri" (name id) (mapcat identity triangle)))))

(def receivers (surfaces/receivers rig))
(def first-distance (optics/surface-raycaster receivers))
(def blocked? (optics/segment-occluder receivers))
(def individual (mapv (fn [receiver] [(:id receiver) (optics/surface-raycaster [receiver])]) receivers))

(defn wire-name [id] (if id (name id) "-"))

(row "#" "tolglow-metal-rays-fixture-v1")
(row "#" "tolglow" "c1fcdb9cffcf9329e736f29cd1cbe2efcce0cc8d")
(row "#" "afterglow" "e1eb9ff50985a9e3e304b54ec7a97db735956c5a")
(doseq [receiver receivers] (triangles receiver))
(doseq [[label origin target expected-id expected-blocked] cases]
  (let [delta (mapv - target origin)
        length (Math/sqrt (reduce + (map #(* % %) delta)))
        unit (mapv #(/ % length) delta)
        beam {:position origin :direction delta :beam-angle 6.0}
        distance (first-distance origin delta)
        canonical (:surface-id (optics/nearest-footprint beam receivers))
        blocked (blocked? origin target length)
        hits (when distance
               (vec (keep (fn [[id raycaster]] (when (= distance (raycaster origin delta)) id)) individual)))]
    (when-not (and (= expected-id canonical (first hits)) (= expected-blocked blocked))
      (throw (ex-info "Unexpected fixture oracle result" {:case label :id canonical :hits hits :blocked blocked})))
    (when (> (count hits) 1)
      (let [reverse-winner (:surface-id (optics/nearest-footprint beam (reverse receivers)))]
        (when-not (= reverse-winner (peek hits))
          (throw (ex-info "The reversed receiver order did not change the tie winner" {:case label :hits hits})))))
    (apply row (concat ["ray" label] origin unit [length (or distance "-") (if blocked 1 0)
                                                (wire-name canonical)
                                                (if (seq hits) (str/join "," (map name hits)) "-")]))))
