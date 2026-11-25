(in-ns 'afterglow.spatial.beam)

(defn- baseline-select-emission [fixture level pose volume {:keys [bounds] :as sampling} options]
  (let [{:keys [selection range-m intensity-threshold] :as opts}
        (if (::prepared options) options (prepare-options options))
        illuminated? (and (:beam selection) (pos? level) (>= level intensity-threshold))
        aimable? (and (:aimable selection) (every? #(contains? (:channels fixture) %) [:pan :tilt]))]
    (when (and (or illuminated? aimable?)
               (or (nil? bounds) (::origins-in-range? opts)
                   (within-range-of-box? (:position fixture [0 0 0]) bounds
                                         (+ range-m (double (:lens-radius-m fixture 0.119))))))
      (when-let [beam @pose]
        (if (and illuminated? (not aimable?) (:radial sampling) (zero? (:extinction-per-m opts)))
          ;; The exact radial test already rejects points outside the cone.
          (let [result (radial-beam beam (:radial sampling) opts level)]
            (if (= ::probe result) (select-points fixture beam volume sampling opts true false level) result))
          (let [beam? (and illuminated? (or (nil? bounds) (beam-near-box? beam bounds range-m)))]
            (when (or beam? aimable?) (select-points fixture beam volume sampling opts beam? aimable? level))))))))
