(require '[afterglow.render.frame :as frame])

(defn unorm [maximum value]
  (/ (double (Math/round (* (double maximum) value))) maximum))

(defn half [value]
  (double (Float/float16ToFloat (Float/floatToFloat16 (float value)))))

(defn compare-packets [store samples inverted-from]
  (let [coarse (byte-array 512)
        fine (byte-array 512)
        buffers {12 coarse 13 fine}
        channel (cond-> {:coarse [12 511] :fine [13 0]}
                  inverted-from (assoc :inverted-from inverted-from))
        pack! (fn [value]
                (frame/pack-value! buffers channel value)
                [(bit-and 255 (aget coarse 511)) (bit-and 255 (aget fine 0))])]
    (reduce (fn [report value]
              (let [[a b :as expected] (pack! value)
                    [c d :as candidate] (pack! (store value))
                    code-delta (abs (- (+ (* 255 a) b) (+ (* 255 c) d)))]
                (-> report
                    (update :samples inc)
                    (update :changed-packets + (if (= expected candidate) 0 1))
                    (update :max-fractional-255-error max code-delta))))
            {:samples 0 :changed-packets 0 :max-fractional-255-error 0}
            samples)))

(let [samples (mapv #(/ % 65025.0) (range 65026))
      modes {:full identity :unorm8 (partial unorm 255.0)
             :unorm16 (partial unorm 65535.0) :fp16 half}
      reports (into (sorted-map)
                    (map (fn [[mode store]]
                           [mode {:pan (compare-packets store samples nil)
                                  :inverted-tilt (compare-packets store samples 64.0)}]))
                    modes)]
  (assert (every? #(zero? (:changed-packets %)) (vals (:full reports))))
  (assert (pos? (get-in reports [:fp16 :pan :changed-packets])))
  (prn reports))
