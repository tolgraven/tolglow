(in-ns 'afterglow.spatial.beam)

(def ^:private dispatch-sites (atom 0))

(defn- observed-volume-reader [volume {:keys [radial] :as sampling} options]
  (if (and radial (::origins-in-range? options) (= #{:beam} (:selection options))
           (zero? (:extinction-per-m options)))
    (let [threshold (double (:intensity-threshold options))]
      (swap! dispatch-sites inc)
      (fn [^Emission emission]
        (let [level (double (.-level emission))]
          (when (and (pos? level) (>= level threshold))
            (when-let [beam @(.-pose emission)]
              (let [result (radial-beam beam radial options level)]
                (if (= ::probe result)
                  (select-points (.-fixture emission) beam volume sampling options true false level)
                  result)))))))
    (fn [emission] (observed-contribution emission volume sampling options))))
