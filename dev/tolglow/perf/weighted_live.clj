(ns tolglow.perf.weighted-live
  "Measure the real 40 Hz renderer and publisher with separately acknowledged volume controls."
  (:require [afterglow.output.scheduler :as scheduler]
            [afterglow.output.timeline :as timeline]
            [clojure.java.io :as io]
            [tolglow.bench-show :as bench]
            [tolglow.perf.effect-samples :as samples]
            [tolglow.perf.output-deadlines :as deadlines]
            [tolglow.perf.weighted-live.scene :as scene]
            [tolglow.perf.weighted-live.transport :as transport]
            [tolglow.show :as show])
  (:import [com.sun.management OperatingSystemMXBean ThreadMXBean]
           [java.lang.management ManagementFactory]
           [java.util.concurrent Executors ScheduledExecutorService TimeUnit]))

(defn- now-since [origin] (/ (- (System/nanoTime) origin) 1e6))

(defn- wait-until [clock deadline]
  (let [remaining (- deadline (clock))]
    (when (pos? remaining)
      (.sleep TimeUnit/NANOSECONDS (long (Math/ceil (* remaining 1e6))))
      (recur clock deadline))))

(defn- observe! [source k log project]
  (add-watch source k
             (fn [_ _ before after]
               (when-let [row (get after k)]
                 (when-not (identical? row (get before k))
                   (swap! log conj (project row)))))))

(defn- render-row [runtime ids row]
  (let [{:keys [model] :as state} @(:state runtime)]
    (cond-> (samples/capture row state)
      (:ready-at-ms row)
      (assoc :volume-positions (mapv #(get-in model [:instances % :params :position]) ids)
             :show-time-ms (:time-ms model)
             :faults (into [] (keep (fn [[id i]] (when (:fault i) id))) (:instances model))))))

(defn- counters [clock]
  (let [^ThreadMXBean threads (ManagementFactory/getPlatformMXBean ThreadMXBean)
        ^OperatingSystemMXBean os (ManagementFactory/getOperatingSystemMXBean)]
    {:at-ms (clock)
     :allocated (when (.isThreadAllocatedMemoryEnabled threads) (.getTotalThreadAllocatedBytes threads))
     :cpu-ns (.getProcessCpuTime os)
     :gc-ms (reduce + (keep #(let [n (.getCollectionTime %)] (when (<= 0 n) n))
                            (ManagementFactory/getGarbageCollectorMXBeans)))}))

(defn- resource-window [start end]
  {:from-ms (:at-ms start) :to-ms (:at-ms end)
   :process-cpu-ms (/ (- (:cpu-ns end) (:cpu-ns start)) 1e6)
   :all-thread-allocated-bytes (when (and (:allocated start) (:allocated end)) (- (:allocated end) (:allocated start)))
   :collector-time-ms (- (:gc-ms end) (:gc-ms start))})

(defn advance-controls
  "Wait for the previous batch before replacing it with current motion. A
  blocked show accumulates at most one batch of four commands."
  [state next!]
  (let [pending (:pending state)]
    (if (every? realized? pending)
      (let [responses (mapv deref pending)
            rejected (remove #(= :accepted (:status %)) responses)
            batch (next!)]
        (-> state
            (update :accepted + (- (count responses) (count rejected)))
            (update :rejected + (count rejected))
            (update :batches inc)
            (assoc :pending batch)
            (update :max-batch-size max (count batch))))
      (update state :coalesced-ticks inc))))

(defn- controls! [runtime ids grid clock]
  (let [state (volatile! {:pending [] :accepted 0 :rejected 0 :batches 0 :coalesced-ticks 0 :max-batch-size 0})
        executor (Executors/newSingleThreadScheduledExecutor)
        owner (:publisher runtime)
        {:keys [beat bpm]} (get-in @(:state runtime) [:model :transport])
        issue! (fn []
                 (let [ticket (timeline/next-render @(:timeline owner) (clock))
                       elapsed (- (:deadline-ms ticket) (:origin-ms grid))
                       target-beat (+ (or beat 0.0) (* (/ elapsed 60000.0) bpm))]
                   (scene/move! runtime ids target-beat)))
        tick (fn [] (try (vswap! state advance-controls issue!)
                         (catch Exception e (vswap! state assoc :error (.getMessage e)))))]
    (.scheduleWithFixedDelay executor ^Runnable tick 0 25 TimeUnit/MILLISECONDS)
    {:executor executor :state state}))

(defn- stop-executor! [^ScheduledExecutorService executor]
  (.shutdownNow executor)
  (when-not (.awaitTermination executor 30 TimeUnit/SECONDS)
    (throw (ex-info "Probe worker failed to finish" {}))))

(defn measured-outputs
  "Match transport receipts in order after observation attached. The scheduler
  has finished here and the host's final blackout has yet to run."
  [outputs wire]
  (let [first-sent (some :sent-at-ms outputs)
        observed-wire (if first-sent (drop-while #(< (:at-ms %) first-sent) wire) [])
        matched (deadlines/match-sink-entries outputs observed-wire)]
    (loop [[row & more] matched [delivery & rest-wire :as pending] observed-wire previous nil previous-at nil result []]
      (if row
        (if (:sink-at-ms row)
          (let [identity [(:frame-id row) (:source-time-ms row)]
                source (cond (>= (:sink-at-ms row) (+ 25 (:deadline-ms row))) :late-delivery
                             (zero? (:frame-id row)) :idle
                             (= (:source-time-ms row) (:deadline-ms row)) :current
                             (= identity previous) :held
                             :else :older)
                row (cond-> (assoc row :source source :transport (dissoc delivery :id :at-ms))
                      previous-at (assoc :sink-gap-ms (- (:at-ms delivery) previous-at)))]
            (recur more rest-wire identity (:at-ms delivery) (conj result row)))
          (recur more pending previous previous-at (conj result row)))
        result))))

(defn- worker-calls [totals row]
  (reduce-kv (fn [by-effect _ {:keys [effect-id calls-this-render]}]
               (update by-effect effect-id (fnil + 0) calls-this-render))
             totals (:effect-sampling row)))

(defn summarize [renders outputs first-slot last-slot]
  (let [window #(filterv (fn [row] (<= first-slot (:slot row) last-slot)) %)
        rendered (window renders) sent (filter :sink-at-ms (window outputs))
        active (filter #(pos? (:frame-id %)) sent)
        positions (keep :volume-positions rendered)
        poses (mapv (fn [i] (count (distinct (keep #(nth % i nil) positions)))) (range 4))
        aim-states (count (distinct (keep #(get-in % [:transport :aim :crc32]) active)))
        lit (some #(get-in % [:transport :lit-pixel]) active)
        incomplete (keep #(get-in % [:transport :incomplete-universes]) active)
        faults (set (mapcat :faults rendered))
        ready (filter :ready-at-ms rendered)
        gaps (keep :sink-gap-ms sent)
        base (deadlines/summary renders outputs first-slot last-slot)]
    (assoc base :completed-renders (count ready)
           :completed-render-ms (when (seq ready) (bench/distribution (map :cost-ms ready)))
           :sink-gap-ms (when (seq gaps) (bench/distribution gaps))
           :no-producer-row (- (:nominal-slots base) (count rendered))
           :no-publisher-row (- (:nominal-slots base) (count (window outputs)))
           :source-counts (frequencies (map :source sent))
           :distinct-volume-poses poses :distinct-delivered-aim-states aim-states
           :first-lit-pixel lit :faulted-instances faults :worker-calls (reduce worker-calls {} rendered)
           :incomplete-packets (count incomplete) :first-coverage-error (first incomplete)
           :show-output-exercised? (boolean (and lit (empty? faults) (empty? incomplete) (> aim-states 1)
                                                 (every? #(< 1 %) poses))))))

(defn- observations! [runtime ids]
  (let [owner (:publisher runtime) renders (atom []) outputs (atom []) wakes (atom [])]
    (observe! (:timeline owner) :last renders (partial render-row runtime ids))
    (observe! (:timeline owner) :last-missed-wake wakes identity)
    (observe! (:status owner) :output-timing outputs identity)
    {:renders renders :outputs outputs :wakes wakes}))

(defn- measure-window! [grid clock first-slot last-slot]
  (counters clock)
  (wait-until clock (timeline/deadline grid first-slot))
  (let [start (counters clock)]
    (wait-until clock (timeline/deadline grid (inc last-slot)))
    (resource-window start (counters clock))))

(defn- drain-publisher! [owner]
  ;; close! fences in-flight handoff. Wait for the receipt and timing observers
  ;; before show/stop! sends the final idle packet and closes the physical sink.
  (let [executor @(:scheduler owner)]
    (scheduler/close! owner)
    (stop-executor! executor)))

(defn- motion-result [control]
  (let [{:keys [pending] :as state} @(:state control)]
    (assoc (dissoc state :pending)
           :remaining (frequencies (map #(if (realized? %) (:status @%) :pending) pending)))))

(defn- record! [runtime ids clock wire first-slot last-slot]
  (let [owner (:publisher runtime) grid @(:timeline owner)
        {:keys [renders outputs wakes]} (observations! runtime ids)
        control (controls! runtime ids grid clock)]
    (try
      (let [resources (measure-window! grid clock first-slot last-slot)]
        (stop-executor! (:executor control))
        (wait-until clock (timeline/deadline grid (+ 3 last-slot)))
        (drain-publisher! owner)
        (let [deliveries (measured-outputs @outputs @wire)]
          {:first-slot first-slot :last-slot last-slot :period-ms 25
           :summary (summarize @renders deliveries first-slot last-slot)
           :window-resources resources :motion (motion-result control)
           :renders @renders :missed-wakes @wakes :outputs deliveries}))
      (finally (.shutdownNow ^ScheduledExecutorService (:executor control))))))

(defn capture [{:keys [pixels warmup slots] :or {pixels 20000 warmup 160 slots 240}}]
  (when-not (and (pos-int? pixels) (pos-int? warmup) (pos-int? slots))
    (throw (ex-info "Pixel and slot counts must be positive integers" {})))
  (let [{:keys [model volume-ids]} (scene/build pixels)
        origin (System/nanoTime) clock #(now-since origin)
        wire (atom []) sink (transport/sink pixels clock wire)
        options {:model model :frame-rate 40 :workers 2 :workspace-options nil
                 :physical-now-ms clock :capture-effect-samples? true}
        runtime (show/start! options (constantly sink))]
    (try
      (assoc (record! runtime volume-ids clock wire (inc warmup) (+ warmup slots))
             :rig {:pixels pixels :moving-heads 64 :washes 8 :volumes 4 :mixed-chart true
                   :selection scene/selection :frame-rate 40 :workers 2 :transport :in-memory-artdmx
                   :active-effect-ids (mapv :effect-id (sort-by :order (vals (:instances model))))
                   :adaptive-keyframes (:adaptive-keyframes runtime)}
             :environment {:java (System/getProperty "java.version") :clojure (clojure-version)
                           :os (System/getProperty "os.name") :arch (System/getProperty "os.arch")
                           :processors (.availableProcessors (Runtime/getRuntime))
                           :max-heap-bytes (.maxMemory (Runtime/getRuntime))}
             :source-revisions {:tolglow (bench/revision ".")
                                :afterglow (bench/revision (or (System/getenv "AFTERGLOW_ROOT") "../afterglow"))})
      (finally (show/stop! runtime)))))

(defn -main [path & [pixels warmup slots]]
  (let [config (into {} (keep (fn [[key v]] (when v [key (parse-long v)]))) [[:pixels pixels] [:warmup warmup] [:slots slots]])
        report (capture config)]
    (io/make-parents path)
    (spit path (pr-str report))
    (prn (select-keys report [:rig :summary :window-resources :motion]))))
