(require '[afterglow.sync.session :as session]
         '[clojure.data.json :as json]
         '[clojure.java.io :as io]
         '[tolglow.bench-show :as bench]
         '[tolglow.effects.parameter-sources :as sources]
         '[tolglow.perf.weighted-live.scene :as scene]
         '[tolglow.show :as show]
         '[tolglow.spatial :as spatial])

(import '[com.sun.management ThreadMXBean]
        '[java.io BufferedOutputStream]
        '[java.lang.management ManagementFactory]
        '[java.security MessageDigest])

(def output-dir (first *command-line-args*))

(load-file (str output-dir "/reference_parameter_sources.clj"))

(defn- path [name] (str output-dir "/" name))

(defn- pulse [bank sample-type]
  (let [levels (case sample-type
                 :level [0.10 0.84]
                 :ink [[0.04 0.08 0.17] (case (mod bank 4)
                                          0 [0.06 0.78 0.67] 1 [0.92 0.33 0.10]
                                          2 [0.64 0.28 0.92] 3 [0.05 0.48 0.90])])]
    {:type :lfo :waveform (if (and (= bank 15) (= sample-type :ink)) :sine :pulse)
     :period (if (< bank 8) {:beats (nth [1/2 1 3/2 2] (mod bank 4))}
                            {:bars (nth [1/4 1/2 3/4 1] (mod bank 4))})
     :phase-reference (if (even? bank) :transport :launch)
     :phase (/ bank 16)
     :inputs (if (and (= bank 15) (= sample-type :ink)) {} {:duty (/ (+ 4 (mod bank 8)) 16)})
     :from (first levels) :to (second levels)}))

(defn- banks [model]
  (let [bank-effect {:id :pulse-banks :version 1
                     :params {:level {:type :number :default 0.3 :bounds [0 1]}
                              :ink {:type :color :default [0.03 0.11 0.13]}}
                     :outputs {:dimmer {:op :param :key :level} :color {:op :param :key :ink}}}
        model (-> model
                   (bench/command :bank-effect {:op :registry/publish :kind :effects :definition bank-effect})
                   (bench/command :bank-cue {:op :cue/put :cue {:id :pulse-banks :effect-id :pulse-banks :priority 60}}))
        fixtures (get-in model [:rig :fixtures])
        groups (group-by #(mod % 16) (range (count fixtures)))]
    (reduce-kv
      (fn [{:keys [model ids]} bank indices]
        (let [targets (mapv #(get-in fixtures [% :id]) indices)
              launched (bench/command model [:launch-bank bank]
                                      {:op :cue/launch :cue-id :pulse-banks :launch {:targets targets}})
              id (:id (apply max-key :order (vals (:instances launched))))
              bind (fn [m parameter]
                     (sources/bind m {:instance-id id :param parameter :binding (pulse bank parameter)}))]
          {:model (reduce bind launched [:level :ink]) :ids (assoc ids bank id)}))
      {:model model :ids {}} groups)))

(def ^ThreadMXBean metrics (ManagementFactory/getThreadMXBean))

(defn- read-metrics []
  [(.getCurrentThreadCpuTime metrics) (.getThreadAllocatedBytes metrics (.threadId (Thread/currentThread))) (System/nanoTime)])

(defn- measure [f]
  (let [[cpu allocated now] (read-metrics)
        result (f)
        [next-cpu next-allocated next-now] (read-metrics)]
    [result {:cpu-ms (/ (- next-cpu cpu) 1e6)
             :bytes (- next-allocated allocated)
             :wall-ms (/ (- next-now now) 1e6)}]))

(defn- observe [stats key process]
  (fn [& args]
    (let [[result report] (measure #(apply process args))]
      (vswap! stats assoc key report)
      result)))

(defn- observe-source [stats sample]
  (let [measured (observe stats :parameter sample)]
    (fn [model clock previous]
      (let [result (measured model clock previous)
            plans (vals (:plans result))
            identity-prev (set (vals previous))]
        (vswap! stats assoc :plans {:captured (count (filter :cycle plans))
                                    :hits (count (filter #(and (:cycle %) (identity-prev %)) plans))
                                    :misses (count (filter #(and (:cycle %) (not (identity-prev %))) plans))
                                    :direct (count (remove :cycle plans))})
        result))))

(defn- digest [^MessageDigest hash packets]
  (doseq [^bytes packet packets] (.update hash packet))
  (format "%064x" (BigInteger. 1 (.digest hash))))

(defn- command-at [host ids frame]
  (case frame
    30 [(show/submit! host :modulation-bind
                      {:instance-id (ids 0) :param :level
                       :binding (assoc (pulse 0 :level) :inputs {:duty 1/8} :phase 3/16)})]
    45 [(show/submit! host {:op :transport/set :command-id "tempo" :transport {:bpm 79.0}})]
    62 [(show/submit! host {:op :transport/set :command-id "meter" :transport {:meter {:beats-per-bar 7/2 :bars-per-phrase 2}}})]
    90 [(show/submit! host {:op :master/set :command-id "master" :value 0.72})]
    110 [(show/submit! host {:op :blackout/set :command-id "blackout" :value true})]
    115 [(show/submit! host {:op :blackout/set :command-id "restore" :value false})]
    []))

(defn- captured-clock [ordinary skip-at]
  (fn [mgr time]
    (if (and (>= time skip-at) (< time (+ skip-at 500)))
      {:selected-source :ableton-link :selection-epoch 1 :follow-transport? true
       :clock {:beat (+ 3/8 (* (- time skip-at) (/ 79.0 60000.0))) :bpm 79.0 :playing? true
               :meter {:beats-per-bar 7/2 :bars-per-phrase 2} :source-epoch 1 :confidence 1.0}}
      (ordinary mgr time))))

(defn- run-show [{:keys [model ids]} tag sample frames warmup write-capture?]
  (let [capture (bench/capture-sink)
        runtime (show/create-runtime {:model model :workers 2 :workspace-options nil :sink capture
                                      :worker-timeout-ms 10000})
        stage-stats (volatile! {})
        hash (MessageDigest/getInstance "SHA-256")
        trace (transient [])
        write-file (if write-capture? (path (str (name tag) ".artdmx")) "/dev/null")
        original-advance spatial/advance
        original-clock session/clock-at]
    (try
      (with-open [stream (BufferedOutputStream. (io/output-stream write-file))]
        (with-redefs [sources/sample (observe-source stage-stats sample)
                      spatial/advance (observe stage-stats :spatial original-advance)
                      session/clock-at (captured-clock original-clock (* (+ warmup 75) 25))]
          (dotimes [index (+ frames warmup)]
            (let [frame (- index warmup)
                  time (* index 25)
                  pending (command-at runtime ids frame)
                  _ (vreset! stage-stats {})
                  [nil-result whole] (measure #(show/tick! runtime time))
                  {:keys [status fault-events model resolved-param-overrides]} @(:state runtime)
                  packets @(:latest capture)
                  replies (mapv #(select-keys @% [:status :code :epoch :revision]) pending)]
              (when (or (:error status) (:output-error status) (seq fault-events)
                        (empty? packets) (some #(not= :accepted (:status %)) replies))
                (throw (ex-info "Replay must render and accept all commands"
                                {:frame frame :status status :faults fault-events :replies replies})))
              (when-not (neg? frame)
                (when write-capture?
                  (doseq [^bytes packet packets] (.write stream packet 0 (alength packet))))
                (conj! trace (merge @stage-stats
                                    {:frame frame :time-ms time :beat (get-in model [:transport :beat])
                                     :bpm (get-in model [:transport :bpm]) :master (:master model)
                                     :clock-source (get-in model [:transport :source]) :blackout (:blackout? model)
                                     :whole whole :packet-digest (digest hash packets)
                                     :controls resolved-param-overrides
                                     :intents (:last-intents status) :replies replies})))))
          (persistent! trace)))
      (finally (show/stop! runtime)))))

(defn- summary [rows]
  (let [stages [:parameter :spatial :whole]
        metrics [:cpu-ms :wall-ms :bytes]]
    (into {} (for [stage stages]
               [stage (into {} (for [metric metrics]
                                 [metric (bench/distribution (keep #(get-in % [stage metric]) rows))]))]))))

(defn- verify! [reference candidate]
  (doseq [[direct replay] (map vector reference candidate)]
    (let [expected (dissoc direct :parameter :spatial :whole :plans)
          observed (dissoc replay :parameter :spatial :whole :plans)]
      (when-not (.equals ^Object expected observed)
        (throw (ex-info "Replay disagrees with the independent master source"
                        {:frame (:frame direct) :expected expected :observed observed}))))))

(let [[_ pixels frames warmup rounds] *command-line-args*
      [pixels frames warmup rounds] (mapv parse-long [pixels frames warmup rounds])
      model (banks (:model (scene/build pixels)))
      fixtures (get-in model [:model :rig :fixtures])
      candidate sources/sample
      reference @(resolve 'tolglow.effects.parameter-sources-reference/sample)
      metadata (mapv #(select-keys % [:id :universe :address :channels :position]) fixtures)]
  (spit (path "fixtures.json") (json/write-str metadata))
  (doseq [round (range rounds)]
    (let [order (if (even? round) [[:reference reference] [:replay candidate]]
                                 [[:replay candidate] [:reference reference]])
          results (into {} (for [[tag sample] order]
                             [tag (run-show model tag sample frames warmup (zero? round))]))
          expected (:reference results)
          actual (:replay results)
          stats (into {} (for [[tag measured] results] [tag (summary measured)]))]
      (verify! expected actual)
      (spit (path (str "pair-" round ".edn")) (pr-str results))
      (when (zero? round)
        (spit (path "video-trace.json")
              (json/write-str (mapv #(select-keys % [:frame :time-ms :beat :bpm :master :clock-source :blackout :plans])
                                    actual))))
      (prn {:round round :frames frames :warmup warmup :pixels pixels :moving-heads (- (count fixtures) pixels)
            :capture-plan-counts (apply merge-with + (map :plans actual)) :result stats}))))
