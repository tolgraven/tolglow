(ns tolglow.bench-shader-cues
  "Shader cue cost as cue count rises (#673), on Afterglow's native GPU path.

    lein with-profile +native-gpu run -m tolglow.bench-shader-cues [seconds] [out.edn]

  Starts a scheduled 40 Hz show with a capture sink (no physical output) and
  a 200-pixel strip. For each video rate (25 and 30 fps) and cue count (0, 1,
  2, 4, 8) it launches that many shader cues: the first renders the plasma at
  200 × 1 onto the strip pixel-for-pixel; the others alternate a ray-marched
  3D scene and a 2D plasma at 640 × 360, each with its own feed. Every cue's
  uniforms change every lighting frame (they read the beat), so every video
  slot needs a render.

  Measured per run, after a 2 s warm-up:
  - lighting: sink-entry interval mean/p50/p95/p99/max and the share of
    intervals more than 5 ms off the 25 ms period; owner frame-ms p50/p95;
  - video, per cue: slots, rendered, dropped, publish-interval p95 deviation,
    render wall time (queue + draw + readback) and draw time (first GL call
    to signalled fence, which bounds GPU execution) p50/p95;
  - GPU: summed draw time per second (GPU-busy bound) and readback;
  - process CPU share and GC count/time over the window.

  Excluded: Studio clients, JPEG previews, the projector page, network and
  any physical output. Apple's GL-over-Metal timer query returns unusable
  values on this machine, so draw time to the fence is reported instead."
  (:require [clojure.pprint :as pprint]
            [tolglow.demo :as demo]
            [tolglow.rig.pixel-layout :as layout]
            [tolglow.show :as show]
            [tolglow.video.shader-examples :as ex]
            [tolglow.video.shader-producer :as producer])
  (:import [java.lang.management ManagementFactory]))

(def strip {:id :strip-200 :shape :path :color-layout :rgb :universe 1 :address 1 :pixels 200 :points [[0 0 0] [4 0 0]]})

(defn- workload [n fps]
  (vec (for [k (range n)]
         (cond (zero? k) (ex/plasma-strip {:id :bench-strip :mapping :shader-strip :fps fps})
               (odd? k) (ex/scene {:id (keyword (str "bench-scene-" k)) :fps fps :size [640 360]})
               :else (ex/plasma {:id (keyword (str "bench-plasma-" k)) :fps fps :size [640 360]})))))

(defn- quantile [xs q]
  (when (seq xs)
    (let [v (vec (sort xs))] (nth v (min (dec (count v)) (long (Math/floor (* q (count v)))))))))

(defn- r2 [x] (when x (/ (Math/round (* 100.0 (double x))) 100.0)))

(defn- gc-totals []
  (reduce (fn [[n ms] bean] [(+ n (max 0 (.getCollectionCount bean))) (+ ms (max 0 (.getCollectionTime bean)))])
          [0 0] (ManagementFactory/getGarbageCollectorMXBeans)))

(defn- cpu-ns []
  (.getProcessCpuTime ^com.sun.management.OperatingSystemMXBean (ManagementFactory/getOperatingSystemMXBean)))

(defn- run-once [n fps seconds]
  (let [sends (atom [])
        sink {:send! (fn [_ _] (swap! sends conj (System/nanoTime))) :close! (fn [& _])}
        runtime (show/start! {:model (-> (demo/model)
                                         (assoc :rig (layout/materialize {:fixtures [] :pixel-layouts [strip]}))
                                         (assoc-in [:settings :video :mappings]
                                                   {:shader-strip (ex/strip-mapping {:source :shader/bench-strip :layout :strip-200})}))
                              :workspace-options nil :recovery false}
                             (fn [_] sink))
        reply! (fn [& args] (deref (apply show/submit! runtime args) 5000 {:status :timeout}))
        frame-ms (atom [])]
    (try
      (doseq [d (workload n fps)]
        (reply! :definition {:kind :effects :definition d})
        (reply! :definition {:kind :cues :definition {:id (:id d) :effect-id (:id d) :priority 10 :reload-policy :preserve-state}})
        (reply! {:op :cue/launch :cue-id (:id d)}))
      (Thread/sleep 2000)
      (reset! sends [])
      (let [p (:shader-video runtime)
            before (producer/samples p)
            [gc-n0 gc-ms0] (gc-totals) cpu0 (cpu-ns) t0 (System/nanoTime)
            sampler (future (while (not (Thread/interrupted))
                              (swap! frame-ms conj (get-in @(:state runtime) [:status :frame-ms]))
                              (Thread/sleep 25)))]
        (Thread/sleep (long (* 1000 seconds)))
        (future-cancel sampler)
        (let [wall-s (/ (- (System/nanoTime) t0) 1e9)
              [gc-n1 gc-ms1] (gc-totals) cpu1 (cpu-ns)
              after (producer/samples p)
              status (producer/status p)
              intervals (mapv #(/ (- %2 %1) 1e6) @sends (rest @sends))
              video (vec (for [[id s] (sort-by (comp str key) after)
                               :let [b (get before id)
                                     k (fn [key] (- (get s key 0) (get b key 0)))
                                     ;; Samples are bounded; keep only those from this window.
                                     fresh (fn [key] (let [all (get s key [])] (take-last (min (count all) (k :rendered)) all)))
                                     render (fresh :render-ms) draw (fresh :draw-ms)
                                     published (fresh :published-at)
                                     period (/ 1000.0 (:fps s))
                                     deviation (map #(Math/abs (- (- %2 %1) period)) published (rest published))]]
                           {:feed (:feed s) :fps (:fps s)
                            :slots (k :slots) :rendered (k :rendered) :dropped (k :dropped) :unchanged (k :unchanged)
                            :failures (k :failures)
                            :drop-share (r2 (/ (k :dropped) (max 1.0 (+ (k :slots) (k :dropped)))))
                            :publish-deviation-p95-ms (r2 (quantile deviation 0.95))
                            :render-ms {:p50 (r2 (quantile render 0.5)) :p95 (r2 (quantile render 0.95))}
                            :draw-ms {:p50 (r2 (quantile draw 0.5)) :p95 (r2 (quantile draw 0.95))}
                            :draw-ms-per-s (r2 (/ (reduce + 0.0 draw) wall-s))}))]
          {:cues n :fps fps :seconds (r2 wall-s)
           :renderer (select-keys (:renderer status) [:renderer :version])
           :lighting {:sends (count @sends)
                      :interval-ms {:mean (r2 (/ (reduce + 0.0 intervals) (max 1 (count intervals))))
                                    :p50 (r2 (quantile intervals 0.5)) :p95 (r2 (quantile intervals 0.95))
                                    :p99 (r2 (quantile intervals 0.99)) :max (r2 (reduce max 0.0 intervals))}
                      :off-by-5ms-share (r2 (/ (count (filter #(> (Math/abs (- % 25.0)) 5.0) intervals)) (max 1.0 (count intervals))))
                      :frame-ms {:p50 (r2 (quantile (remove nil? @frame-ms) 0.5)) :p95 (r2 (quantile (remove nil? @frame-ms) 0.95))}}
           :video video
           :gpu-draw-ms-per-s (r2 (reduce + 0.0 (keep :draw-ms-per-s video)))
           :process-cpu-share (r2 (/ (- cpu1 cpu0) 1e9 wall-s (.availableProcessors (Runtime/getRuntime))))
           :gc {:collections (- gc-n1 gc-n0) :ms (- gc-ms1 gc-ms0)}}))
      (finally (show/stop! runtime)))))

(defn run
  ([] (run 8))
  ([seconds]
   (vec (for [fps [25 30] n [0 1 2 4 8]]
          (let [result (run-once n fps seconds)]
            (println (format "fps %d cues %d: lighting p95 %.2f ms max %.2f ms off>5ms %.2f; video dropped %s; draw ms/s %.2f; cpu %.2f; gc %s"
                             fps n (double (or (get-in result [:lighting :interval-ms :p95]) 0))
                             (double (or (get-in result [:lighting :interval-ms :max]) 0))
                             (double (or (get-in result [:lighting :off-by-5ms-share]) 0))
                             (pr-str (mapv :dropped (:video result)))
                             (double (or (:gpu-draw-ms-per-s result) 0))
                             (double (or (:process-cpu-share result) 0))
                             (pr-str (:gc result))))
            (flush)
            result)))))

(defn -main [& [seconds out]]
  (let [results (run (if seconds (Double/parseDouble seconds) 8))
        report {:machine {:os (System/getProperty "os.name") :os-version (System/getProperty "os.version")
                          :arch (System/getProperty "os.arch") :java (System/getProperty "java.version")
                          :processors (.availableProcessors (Runtime/getRuntime))}
                :results results}]
    (if out
      (spit out (with-out-str (pprint/pprint report)))
      (pprint/pprint report))))
