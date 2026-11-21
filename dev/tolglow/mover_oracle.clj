(ns tolglow.mover-oracle
  "Generate the moving-head shader corpus using Afterglow's calibrated renderer."
  (:require [afterglow.render.frame :as frame]
            [afterglow.render.gpu :as gpu]
            [clojure.data.json :as json]))

(defn- fixture [i]
  (let [slot (+ 511 (* 12 i))
        upside-down? (odd? (quot i 8))]
    {:id i :position [(- (mod i 16) 7.5) (+ 2.0 (* 0.4 (mod i 4))) (- (quot i 16) 3.0)]
     :rotation-deg [(+ (if upside-down? 180 0) (- (mod (* i 17) 41) 20))
                    (- (mod (* i 47) 337) 168) (- (mod (* i 11) 37) 18)]
     :calibration {:pan (case (mod i 3) 0 {:min-deg -270 :max-deg 270}
                              1 [128 92] {:min-deg -127 :max-deg 157})
                   :tilt (if (even? i) {:min-deg -70 :max-deg 74} [121 152])}
     :universe (+ 4 (quot slot 512)) :address (inc (mod slot 512))
     :channels {:pan {:offset 1 :fine-offset 2 :inverted-from (when (zero? (mod i 5)) 0)}
                :tilt {:offset 3 :fine-offset 4 :inverted-from (when (zero? (mod i 7)) 0)}}}))

(defn- target [i beat]
  (let [phase (+ (* beat 0.71) (* i 0.37))]
    [(* 6.8 (Math/sin phase)) (+ 1.8 (* 2.4 (Math/cos (+ (* 0.73 phase) i))))
     (* 5.5 (Math/cos (* 1.17 phase)))]))

(defn- unsigned [xs] (mapv #(bit-and % 255) xs))

(defn corpus [heads samples]
  (let [fixtures (mapv fixture (range heads))
        patch (frame/compile-patch fixtures)]
    (loop [i 0 previous {} frames []]
      (if (= i samples) {:fixtures fixtures :frames frames}
          (let [aims (mapv #(target % (/ i 16.0)) (range heads))
                writes (mapv (fn [id aim] {:target-id id :attribute :aim :value aim}) (range heads) aims)
                output (frame/resolve-frame patch writes {:previous-values previous})
                values (:values output)]
            (recur (inc i) values
                   (conj frames {:aims aims :axes (mapv #(let [{:keys [pan tilt aim-reachable? pan-deg]} (values %)]
                                                           [pan tilt aim-reachable? pan-deg]) (range heads))
                                 :universes (into (sorted-map)
                                                  (map (fn [[id bytes]] [id (unsigned bytes)]))
                                                  (:universes output))})))))))

(defn -main [heads frames]
  (let [n (parse-long heads) k (parse-long frames)]
    (when-not (and n (<= 1 n 512) k (<= 2 k 256))
      (throw (ex-info "Heads must be 1..512 and frames 2..256" {:heads heads :frames frames})))
    (json/write (assoc (corpus n k)
                       :renderer {:vertex (gpu/source :moving-head-vertex)
                                  :module (gpu/source :moving-head-solver)})
                *out*)))
