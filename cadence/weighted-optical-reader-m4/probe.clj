(require '[afterglow.spatial.field :as field]
         '[tolglow.perf.weighted-live :as venue])

(let [directory "target/weighted-optical-dispatch-ec569ed0/"
      load-candidate #(binding [*ns* *ns*] (var-get (load-file (str directory %))))
      dispatch-reader (load-candidate "beam-candidate.clj")
      field-candidate (load-candidate "field-candidate.clj")
      dispatch-sites @(ns-resolve 'afterglow.spatial.beam 'dispatch-sites)
      sample-var (ns-resolve 'afterglow.spatial.field 'sample-volume)
      sample-candidate (partial field-candidate dispatch-reader)]
  (let [fixtures (field/prepare-rig [{:id :fixed :universe 0 :address 1 :channels {:dimmer 1}
                                     :position [0.0 0.0 0.0] :direction [0.0 0.0 1.0]
                                     :beam-angle 30.0 :lens-radius-m 0.1}])
        observations {:values {:fixed {:emitted-color [1.0 0.5 0.0]}}}]
    (doseq [moment (range 40)
            selection [#{:beam} #{:position :beam} #{:position} #{:beam :aimable}]
            optics [{:range-m 35.0} {:range-m 3.0} {:extinction-per-m 0.05}]]
      (let [volume {:shape :sphere :radius 1.0 :position [(* 0.2 (- moment 20)) 0.0 3.0]
                    :selection selection :falloff {:outer-m 1.0}}
            observations (merge observations optics)
            normal ((field/sampler fixtures observations) volume)
            candidate (with-redefs-fn {sample-var sample-candidate}
                        #((field/sampler fixtures observations) volume))]
        (assert (= normal candidate) (pr-str [volume optics normal candidate])))))
  (assert (pos? @dispatch-sites) "The direct reader must be used in parity cases")
  (prn {:spot-parity-cases 480 :specialized-pairs @dispatch-sites})
  (doseq [[index mode] (map-indexed vector [:baseline :candidate :candidate :baseline])]
    (reset! dispatch-sites 0)
    (let [conf {:pixels 20000 :warmup 160 :slots 240}
          report (if (= :candidate mode)
                   (with-redefs-fn {sample-var sample-candidate} #(venue/capture conf))
                   (venue/capture conf))
          summary (:summary report)
          filename (str directory (inc index) "-" (name mode) ".edn")]
      (spit filename (pr-str report))
      (assert (:show-output-exercised? summary) filename)
      (assert (zero? (:incomplete-packets summary)) filename)
      (when (= :candidate mode) (assert (pos? @dispatch-sites) "There were no direct readers in the weighted venue"))
      (prn {:sample (inc index) :mode mode :specialized-pairs @dispatch-sites
            :output (select-keys summary [:nominal-slots :completed-renders :complete-within-25-ms
                                          :completed-render-ms :sent :source-counts :sink-gap-ms
                                          :incomplete-packets])})
      (flush))))
(shutdown-agents)
