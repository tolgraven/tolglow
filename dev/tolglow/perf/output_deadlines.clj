(ns tolglow.perf.output-deadlines
  "Wall-clock release replay of the physical sender and effect producer.
  Samples contain the actual host clock and emitted channel bytes."
  (:require [afterglow.output.scheduler :as scheduler]
            [afterglow.output.timeline :as timeline]
            [tolglow.bench-show :as bench]
            [tolglow.demo :as demo]
            [tolglow.perf.admission-cost :as admission]
            [tolglow.show :as show]))

(defn- percentile [fraction samples]
  (when-let [values (seq (sort samples))]
    (nth values (dec (long (Math/ceil (* fraction (count values))))))))

(defn- observe! [a field rows]
  (add-watch a field
             (fn [_ _ before after]
               (let [row (get after field)]
                 (when (and row (not (identical? row (get before field))))
                   (swap! rows conj row))))))

(defn- demo-scene []
  (reduce (fn [m cue] (bench/command m [cue :start] {:op :cue/launch :cue-id cue}))
          (demo/model) [:ember :sweep]))

(defn- observe-sink [clock rows]
  {:send! (fn [universes id]
            (swap! rows conj {:id id :at-ms (clock)
                              :universes (count universes)
                              :bytes (mapv #(bit-and (int %) 255) (take 12 (val (first universes))))}))})

(defn match-sink-entries
  "Associate publisher admissions and capture-sink callbacks in call order.
  A held frame can repeat an ID. An expired or sinkless attempt has no callback."
  [outputs wire]
  (loop [[output & more] outputs pending (seq wire) result []]
    (if output
      (if-some [sent (:sent-at-ms output)]
        (let [{:keys [id at-ms] :as entry} (first pending)]
          (when-not (and entry (= (:frame-id output) id) (<= sent at-ms (:sent-by-ms output)))
            (throw (ex-info "Sink receipt does not match its admission" {:output output :sink entry})))
          (recur more (next pending) (conj result (assoc output :sink-at-ms at-ms :sink-bytes (:bytes entry)))))
        (recur more pending (conj result output)))
      (if pending
        (throw (ex-info "Unexpected capture-sink call" {:entry (first pending)}))
        result))))

(defn- intervals [times]
  (map - (rest times) times))

(defn summary [renders outputs first-slot last-slot]
  (let [in-range? #(<= first-slot (:slot %) last-slot)
        renders (filter in-range? renders) outputs (filter in-range? outputs)
        delivered (filter :sink-at-ms outputs)
        physical (map :sink-at-ms delivered)
        late-sink (filter #(>= (:sink-at-ms %) (+ (:deadline-ms %) 25)) delivered)
        short-gaps (filter #(< % 12.49) (intervals physical))
        costs (map :cost-ms renders)
        age (for [{:keys [deadline-ms inputs-at-ms]} renders :when inputs-at-ms] (- deadline-ms inputs-at-ms))
        ready (count (filter #(and (:ready-at-ms %) (<= (:ready-at-ms %) (:deadline-ms %))) renders))
        fresh (count (filter #(= (:source-time-ms %) (:deadline-ms %)) delivered))]
    {:nominal-slots (inc (- last-slot first-slot)) :renders (count renders)
     :complete-within-25-ms (count (filter #(and (:ready-at-ms %) (<= (:cost-ms %) 25.0)) renders)) :ready-for-deadline ready
     :sent (count delivered) :publisher-attempts (count outputs)
     :late-sink-entries (count late-sink) :short-sink-gaps (count short-gaps)
     :meets-sink-timing? (and (empty? late-sink) (empty? short-gaps)) :expired-before-send (count (filter :expired-before-send? outputs))
     :sent-with-current-source fresh :render-wall-p95-ms (percentile 0.95 costs)
     :input-age-p50-ms (percentile 0.50 age) :input-age-p95-ms (percentile 0.95 age)
     :send-start-lag-p95-ms (percentile 0.95 (map #(- (:started-at-ms %) (:deadline-ms %)) outputs))
     :send-cost-p95-ms (percentile 0.95 (map #(- (:finished-at-ms %) (:started-at-ms %)) outputs))
     :lower-source-age-at-send-p95-ms (percentile 0.95 (keep :source-input-age-ms delivered))
     :lower-source-age-at-sink-p95-ms
     (percentile 0.95 (for [{:keys [source-input-age-ms sink-at-ms sent-at-ms]} delivered
                            :when source-input-age-ms]
                        (+ source-input-age-ms (- sink-at-ms sent-at-ms))))
     :sink-call-p95-ms (percentile 0.95 (for [{:keys [sent-at-ms sent-by-ms]} outputs :when sent-at-ms]
                                         (- sent-by-ms sent-at-ms)))}))

(defn- verify-output! [renders outputs warmup slots]
  (let [ready-by-slot (into {} (keep (fn [row]
                                       (when (and (:ready-at-ms row) (not (neg? (:slack-ms row))))
                                         [(:deadline-ms row) row]))) renders)
        measured (filter #(<= (inc warmup) (:slot %) (+ warmup slots)) outputs)
        samples (keep :source-time-ms (filter :sink-at-ms measured))
        sent (keep :sent-at-ms measured)]
    (when-not (and (seq samples) (every? ready-by-slot samples)
                   (every? (fn [{:keys [sent-at-ms deadline-ms]}]
                             (or (nil? sent-at-ms) (< sent-at-ms (+ deadline-ms 25)))) measured)
                   (every? true? (map <= samples (rest samples)))
                   (every? true? (map #(>= (- %2 %1) 12.49) sent (rest sent)))
                   (some #(some pos? (:sink-bytes %)) measured))
      (throw (ex-info "Physical replay lost its on-time source, spacing or lit output"
                      {:renders (count renders) :outputs (count measured) :deliveries (count sent)})))))

(defn- run-case [name scene warmup slots delay-ms]
  (let [origin (System/nanoTime) clock #(/ (- (System/nanoTime) origin) 1.0e6)
        sends (atom []) outputs (atom []) renders (atom []) missed-wakes (atom [])
        host (show/create-runtime {:model scene :workers 2 :workspace-options nil :batch? true
                                   :adaptive-keyframes {:enabled? true} :frame-rate 40
                                   :sink (observe-sink clock sends) :worker-timeout-ms 5000
                                   :physical-now-ms clock :independent-output? true})
        owner (:publisher host)
        work (if (pos? delay-ms)
               (update host :compile
                       (fn [compile]
                         (fn [definition]
                           (let [step (compile definition)]
                             (if (= :ember (:id definition))
                               (fn [context] (java.util.concurrent.locks.LockSupport/parkNanos
                                               (long (* 1e6 delay-ms))) (step context))
                               step))))) host)
        last-slot (+ warmup slots)]
    (try
      (observe! (:timeline owner) :last renders)
      (observe! (:timeline owner) :last-missed-wake missed-wakes)
      (observe! (:status owner) :output-timing outputs)
      (scheduler/start! owner 25 #(show/render-slot! work %))
      (Thread/sleep (long (* 25 (+ last-slot 3))))
      (scheduler/close! owner)
      (let [delivered (match-sink-entries @outputs @sends)]
        (verify-output! @renders delivered warmup slots)
        {:machine {:os (System/getProperty "os.name") :arch (System/getProperty "os.arch")
                   :java (System/getProperty "java.version") :processors (.availableProcessors (Runtime/getRuntime))}
         :scenario name :injected-effect-delay-ms delay-ms :workers 2 :period-ms 25 :warmup-slots warmup
         :summary (summary @renders delivered (inc warmup) last-slot)
         :cold (summary @renders delivered 1 warmup)
         :renders @renders :missed-wakes @missed-wakes :outputs @outputs :wire @sends})
      (finally
        (remove-watch (:timeline owner) :last)
        (remove-watch (:timeline owner) :last-missed-wake)
        (remove-watch (:status owner) :output-timing)
        (show/stop! host)))))

(defn- compare-leads [name scene warmup frames delay-ms adaptive-first?]
  (let [capture #(run-case name scene warmup frames delay-ms)
        measured-lead timeline/lead-ms
        guarded-lead (fn [grid]
                       (cond-> (measured-lead grid)
                         (seq (:costs grid)) (+ (* 0.06 (:period-ms grid)))))
        run (fn [id lead]
              (with-redefs [timeline/lead-ms lead] (assoc (capture) :policy id)))
        policies {:measured measured-lead :measured-extra-reserve guarded-lead :full-period :period-ms}
        order (if adaptive-first? [:measured :measured-extra-reserve :full-period]
                                  [:full-period :measured :measured-extra-reserve])]
    (mapv #(run % (policies %)) order)))

(defn -main [path & [warmup frames]]
  (let [warmup (parse-long (or warmup "80"))
        frames (parse-long (or frames "160"))
        movers (compare-leads :eight-movers (demo-scene) warmup frames 6 true)
        sculpture (compare-leads :pixel-sculpture (:model (admission/workload 1024 128)) warmup frames 0 false)
        revisions {:tolglow (bench/revision ".")
                   :afterglow (bench/revision (or (System/getenv "AFTERGLOW_ROOT") "../afterglow"))}
        results (mapv #(assoc % :source-revisions revisions) (concat movers sculpture))]
    (spit path (pr-str results))
    (prn (mapv #(select-keys % [:scenario :policy :summary :cold]) results))))
