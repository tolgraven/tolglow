(require '[afterglow.output.artnet :as artnet]
         '[afterglow.render.gpu.native-context :as native]
         '[clojure.data.json :as json]
         '[tolglow.qa.mixed-stage :as mixed]
         '[tolglow.show :as show])

(import '[java.lang.management ManagementFactory]
        '[java.util.concurrent.locks LockSupport])

(def frame-ns 25000000)
(def frame-count 160)

(defn verify-jvm! []
  (let [runtime (Runtime/getRuntime)
        processors (.availableProcessors runtime)
        heap (.maxMemory runtime)
        collectors (mapv #(.getName %) (ManagementFactory/getGarbageCollectorMXBeans))
        expected-processors (some-> (System/getProperty "tolglow.capture.processors") parse-long)
        expected-heap (some-> (System/getProperty "tolglow.capture.heap.bytes") parse-long)
        actual {:processors processors :heap-max-bytes heap :collectors collectors}]
    (println "Effective show JVM" (json/write-str actual))
    (flush)
    (when-not (and (= expected-processors processors) (= expected-heap heap)
                   (some #(.startsWith ^String % "ZGC") collectors))
      (throw (ex-info "Capture JVM configuration mismatch" actual)))
    actual))

(defn wait-until [deadline]
  (let [remaining (- deadline (System/nanoTime))]
    (when (pos? remaining)
      (LockSupport/parkNanos remaining)
      (recur deadline))))

(defn quantiles [values]
  (when (seq values)
    (let [rows (vec (sort values))
          q (fn [p] (nth rows (dec (long (Math/ceil (* p (count rows)))))))]
      {:count (count rows) :p50 (q 0.5) :p95 (q 0.95) :max (peek rows)})))

(defn record-duration [rows f]
  (fn [& args]
    (let [start (System/nanoTime)]
      (try (apply f args)
           (finally (swap! rows conj (/ (- (System/nanoTime) start) 1.0e6)))))))

(defn run-lane [label expressions movement]
  (let [model (mixed/model)
        capture (volatile! nil)
        capture-time (atom [])
        fence-time (atom [])
        fence-var (ns-resolve 'afterglow.render.gpu.native-context 'await-fence!)
        host (show/create-runtime
              {:model model :workspace-options nil :recovery false :frame-rate 40
               :workers 2 :gpu-budget-ms 12 :compute-backend expressions
               :movement-backend movement
               :sink {:description {:type :capture :network false}
                      :send! (fn [universes id]
                               (vreset! capture
                                 {:id id :packets (into (sorted-map)
                                                    (map (fn [[universe bytes]]
                                                           [universe (artnet/artdmx-packet universe 1 bytes)]))
                                                    universes)}))}})
        clock (volatile! -25)
        tick #(show/tick! host (vswap! clock + 25))]
    (try
      (mixed/start-look! host tick)
      (while (< @clock 8000) (tick))
      (let [before (get-in (show/snapshot host) [:status :compute])
            start (System/nanoTime)
            frames (with-redefs-fn
                     {#'native/capture! (record-duration capture-time native/capture!)
                      fence-var (record-duration fence-time @fence-var)}
                     (fn []
                       (mapv (fn [i]
                               (wait-until (+ start (* i frame-ns)))
                               (vreset! capture nil)
                               (let [started (System/nanoTime)
                                     _ (tick)
                                     elapsed (/ (- (System/nanoTime) started) 1.0e6)
                                     snapshot (show/snapshot host)]
                                 (when (or (nil? @capture) (get-in snapshot [:status :error]))
                                   (throw (ex-info "Output missing or erroneous" {:lane label :time @clock
                                                    :status (:status snapshot)})))
                                 (assoc @capture :time @clock :tick-ms elapsed
                                        :movement (get-in snapshot [:status :movement])
                                        :last-fallback (get-in snapshot [:status :compute :last-fallback])
                                        :values (get-in snapshot [:output :values]))))
                             (range frame-count))))
            status (get-in (show/snapshot host) [:status :compute])
            changed (into {} (for [k [:native-dispatches :cpu-fallbacks :deadline-fallbacks]]
                               [k (- (get status k 0) (get before k 0))]))
            report {:lane label :compute (select-keys status [:active :requested :session :last-fallback])
                    :delta changed :tick-ms (quantiles (map :tick-ms frames))
                    :capture-ms (quantiles @capture-time) :fence-ms (quantiles @fence-time)
                    :movement-frames (frequencies (map (comp name :backend :movement) frames))
                    :gpu-heads (reduce + (keep (comp :gpu-heads :movement) frames))
                    :cpu-heads (reduce + (keep (comp :cpu-heads :movement) frames))
                    :first-time (:time (first frames)) :final-time (:time (peek frames))}]
        (println (json/write-str report))
        (flush)
        {:summary report :frames frames})
      (finally (show/stop! host)))))

(defn slot-differences [[universe ^bytes a] ^bytes b]
  (when-not (java.util.Arrays/equals a b)
    (assert (= (alength a) (alength b)))
    (into [] (keep (fn [i] (let [x (bit-and 255 (aget a i)) y (bit-and 255 (aget b i))]
                           (when (not= x y) {:universe universe :channel (- i 17) :cpu x :candidate y}))))
          (range (alength a)))))

(defn compare-lane [baseline lane]
  (let [index (into {}
                (for [fixture (:fixtures (:rig (mixed/model)))
                      [attribute channels] (:channels fixture)
                      offset (if (sequential? channels) channels [channels])]
                  [[(:universe fixture) (+ (:address fixture) offset -1)] [(:id fixture) attribute]]))
        changed (into []
                  (keep
                    (fn [[expected actual]]
                      (assert (= (:time expected) (:time actual)))
                      (assert (= (keys (:packets expected)) (keys (:packets actual))))
                      (when-let [changes (seq (mapcat #(slot-differences % (get (:packets actual) (key %))) (:packets expected)))]
                        {:time (:time expected) :id (:id actual) :movement (:movement actual) :last-fallback (:last-fallback actual)
                         :differences (mapv
                                       (fn [{:keys [universe channel] :as row}]
                                         (let [[fixture attribute] (index [universe channel])]
                                           (assoc row :fixture fixture :attribute attribute
                                                  :cpu-value (select-keys (get (:values expected) fixture)
                                                               [:pan :tilt :direction :aim :focus :dimmer :color])
                                                  :candidate-value (select-keys (get (:values actual) fixture)
                                                                     [:pan :tilt :direction :aim :focus :dimmer :color]))))
                                       changes)})))
                  (map vector (:frames baseline) (:frames lane)))]
    (assoc (:summary lane) :matched (- frame-count (count changed)) :different changed)))

(let [[directory] *command-line-args*
      report-path (str directory "/comparison.json")
      variants [["cpu-first" :cpu :cpu] ["expressions" :native-gpu :cpu]
                ["movement" :cpu :native-gpu] ["both" :native-gpu :native-gpu]
                ["cpu-last" :cpu :cpu]]]
  (try
    (spit (str directory "/jvm.json") (json/write-str (verify-jvm!)))
    (let [baseline (apply run-lane (first variants))]
      (doseq [[number variant] (map-indexed vector variants)]
        (let [lane (if (zero? number) baseline (apply run-lane variant))
              row (compare-lane baseline lane)]
          (spit report-path (str (json/write-str row) "\n") :append true))))
    (finally (shutdown-agents))))
(System/exit 0)
