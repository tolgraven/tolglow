(require '[afterglow.spatial.field :as field]
         '[tolglow.bench-show :as bench]
         '[tolglow.perf.weighted-live :as venue]
         '[tolglow.perf.weighted-live.scene :as scene]
         '[tolglow.perf.weighted-venue :as weighted]
         '[tolglow.show :as show])
(import '(java.util Arrays))

(defn compact-parity [left right]
  (assert (= (dissoc left :fields) (dissoc right :fields)) "Compact header changed")
  (assert (= (set (keys (:fields left))) (set (keys (:fields right)))) "Compact field keys changed")
  (doseq [[key left-column] (:fields left)]
    (let [right-column (get-in right [:fields key])]
      (assert (= (dissoc left-column :values :valid) (dissoc right-column :values :valid)) (str key))
      (assert (Arrays/equals ^doubles (:values left-column) ^doubles (:values right-column)) (str key " values"))
      (assert (Arrays/equals ^booleans (:valid left-column) ^booleans (:valid right-column)) (str key " coverage")))))

(let [directory "target/weighted-combined-dmx-d5664f52/"
      source #(binding [*ns* *ns*] (var-get (load-file (str directory %))))
      sample-var (ns-resolve 'afterglow.spatial.field 'sample-volume)
      select-var (ns-resolve 'afterglow.spatial.beam 'select-emission)
      publication-var (ns-resolve 'afterglow.output.scheduler 'start-publications!)
      baseline {sample-var (source "baseline-field.clj")
                select-var (source "baseline-beam.clj")
                publication-var (source "baseline-publication.clj")}
      with-baseline #(with-redefs-fn baseline %)
      new-show @(ns-resolve 'tolglow.perf.weighted-venue 'new-show)
      pixels 20000
      {:keys [model volume-ids]} (scene/build pixels)
      indices (vec (concat (range 16) (range pixels (+ pixels 64))))
      step (fn [{:keys [runtime sink clock]} i]
             (let [shapes (weighted/volumes 4 scene/selection (/ i 20.0))
                   replies (mapv (fn [id shape]
                                   (show/submit! runtime {:op :param/set :param :position :instance-id id
                                                          :command-id (str "compare-" id "-" i)
                                                          :value (:position shape)}))
                                 volume-ids shapes)
                   rig-reply (when (= i 12)
                               (let [rig (get-in @(:state runtime) [:model :rig])]
                                 (show/submit! runtime :rig
                                               (update-in rig [:fixtures 0 :position 0] + 0.0625))))
                   snapshot (bench/projected-host-step! runtime sink clock indices i)]
               (doseq [reply (cond-> replies rig-reply (conj rig-reply))]
                 (assert (= :accepted (:status @reply)) (pr-str @reply)))
               [snapshot (get-in @(:state runtime) [:output :compact])]))]
  (let [control (new-show model false)
        trial (new-show model false)]
    (try
      (let [packets (reduce (fn [total i]
                              (let [[baseline base-compact] (with-baseline #(step control i))
                                    [candidate candidate-compact] (step trial i)]
                                (bench/check! baseline candidate)
                                (compact-parity base-compact candidate-compact)
                                (+ total (count (:packets baseline)))))
                            0 (range 24))]
        (prn {:paired-frames 24 :repatches 1 :actual-artdmx-packets packets}))
      (finally
        (show/stop! (:runtime control))
        (show/stop! (:runtime trial)))))
  (doseq [[index mode] (map-indexed vector [:candidate :baseline :baseline :candidate])]
    (let [configuration {:pixels pixels :warmup 240 :slots 600}
          report (if (= :baseline mode)
                   (with-baseline #(venue/capture configuration))
                   (venue/capture configuration))
          summary (:summary report)
          filename (str directory "reverse-" (inc index) "-" (name mode) ".edn")]
      (spit filename (pr-str report))
      (assert (:show-output-exercised? summary) filename)
      (assert (zero? (:incomplete-packets summary)) filename)
      (assert (<= 12.5 (get-in summary [:sink-gap-ms :min])) filename)
      (prn {:sample (inc index) :mode mode
            :output (select-keys summary [:nominal-slots :completed-renders :complete-within-25-ms
                                          :completed-render-ms :sent :source-counts :sink-gap-ms
                                          :incomplete-packets])})
      (flush))))
(shutdown-agents)
