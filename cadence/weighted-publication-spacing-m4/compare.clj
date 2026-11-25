(require '[afterglow.output.scheduler :as scheduler]
         '[afterglow.output.timeline :as timeline]
         '[tolglow.perf.weighted-live :as venue])

(defn simulate-delayed-handoff [start!]
  (let [clock (atom 0.0) pending (atom nil) sent (atom [])
        owner (scheduler/create {:now-ms #(deref clock)})
        grid (timeline/timeline 0.0 25.0)
        scheduler-ns 'afterglow.output.scheduler
        schedule (ns-resolve scheduler-ns 'at!)
        send (ns-resolve scheduler-ns 'send-slot!)
        timer (fn [_ _ time callback] (reset! pending [time callback]))
        handoff (fn [_ _ slot previous started]
                  (swap! sent conj {:slot slot :previous previous :started started})
                  (let [finish (+ started 1.0)]
                    (reset! clock finish)
                    {:sent-by-ms finish :finished-at-ms finish}))]
    (with-redefs-fn {schedule timer send handoff}
      #(do (start! owner nil grid)
           (assert (= 25.0 (first @pending)))
           (reset! clock 44.0)
           ((second @pending))
           (let [[due callback] @pending]
             (reset! clock due)
             (callback)
             {:second-deadline due :sent @sent})))))

(let [directory "target/weighted-publication-spacing-d5664f52/"
      baseline-var (ns-resolve 'afterglow.output.scheduler 'start-publications!)
      candidate (var-get (binding [*ns* *ns*] (load-file (str directory "candidate.clj"))))
      old-case (simulate-delayed-handoff @baseline-var)
      new-case (simulate-delayed-handoff candidate)]
  (assert (= [1 3] (mapv :slot (:sent old-case))))
  (assert (= [1 2] (mapv :slot (:sent new-case))))
  (assert (= 57.5 (:second-deadline new-case)))
  (prn {:baseline old-case :candidate new-case})
  (doseq [[index mode] (map-indexed vector [:baseline :candidate :candidate :baseline])]
    (let [configuration {:pixels 20000 :warmup 240 :slots 1000}
          report (if (= :candidate mode)
                   (with-redefs-fn {baseline-var candidate} #(venue/capture configuration))
                   (venue/capture configuration))
          summary (:summary report)
          filename (str directory (inc index) "-" (name mode) ".edn")]
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
