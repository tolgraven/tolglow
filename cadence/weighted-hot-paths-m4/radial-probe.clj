(require '[afterglow.spatial.beam]
         '[clojure.java.io :as io]
         '[tolglow.perf.weighted-live :as venue])
(import '(java.util Random))

(let [directory "target/weighted-radial-rejection-ec569ed0"
      beam-ns 'afterglow.spatial.beam
      baseline-var (ns-resolve beam-ns 'radial-beam)
      baseline @baseline-var
      candidate (var-get (binding [*ns* *ns*] (load-file (str directory "/candidate.clj"))))
      random (Random. 712407)
      sample (fn [scale] (- (* 2.0 scale (.nextDouble random)) scale))
      verify (fn [mount field opts]
               (let [lhs (baseline mount field opts 0.71)
                     rhs (candidate mount field opts 0.71)]
                 (when-not (= lhs rhs)
                   (throw (ex-info "Radial result changed" {:mount mount :field (dissoc field :by-distance)
                                                            :baseline lhs :candidate rhs})))))]
  (dotimes [index 20000]
    (let [support (Math/abs (sample 9))
          geometry {:position [(sample 20) (sample 8) (sample 20)] :axis [0.0 0.0 1.0]
                    :spread (Math/abs (sample 0.6)) :lens-radius-m (Math/abs (sample 0.2))}
          field {:center [(sample 20) (sample 8) (sample 20)] :support support
                 :by-distance (fn [gap] (if (zero? support) 1.0 (- 1.0 (/ gap support))))}
          opts (cond-> {:range-m (Math/abs (sample 24))}
                 (odd? index) (assoc :occluded? (fn [_ _ distance] (> distance 3.0))))]
      (verify geometry field opts)))
  (let [geometry {:position [0.0 0.0 0.0] :axis [0.0 0.0 1.0] :spread 0.0 :lens-radius-m 0.0}]
    (doseq [support [0.0 Double/MIN_VALUE Double/MIN_NORMAL 1.0 16.0 1024.0]
            x [(Math/nextDown (double support)) support (Math/nextUp (double support))
               (Math/nextUp (Math/nextUp (double support)))]
            z [-0.25 0.0 0.25]]
      (verify geometry {:center [x 0.0 z] :support support :by-distance (constantly 0.5)}
              {:range-m Double/MAX_VALUE})))
  (prn {:radial-parity-cases 20072})
  (doseq [[index mode] (map-indexed vector [:baseline :candidate :candidate :baseline])]
    (let [config {:pixels 20000 :warmup 160 :slots 240}
          report (if (= :candidate mode)
                   (with-redefs-fn {baseline-var candidate} #(venue/capture config))
                   (venue/capture config))
          summary (:summary report)
          filename (str directory "/" (inc index) "-" (name mode) ".edn")]
      (spit filename (pr-str report))
      (assert (:show-output-exercised? summary) filename)
      (assert (zero? (:incomplete-packets summary)) filename)
      (prn {:sample (inc index) :mode mode
            :output (select-keys summary [:nominal-slots :completed-renders :complete-within-25-ms
                                          :completed-render-ms :sent :source-counts :sink-gap-ms
                                          :incomplete-packets])})
      (flush))))
(shutdown-agents)
