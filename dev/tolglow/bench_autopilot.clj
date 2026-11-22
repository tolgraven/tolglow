(ns tolglow.bench-autopilot
  "Compare full show ticks with ordinary output, spatial HDR and HDR plus the
  conductor. Run lein with-profile +benchmark run -m tolglow.bench-autopilot
  [pixels movers warmup measured-frames [base|hdr|autopilot]]. The optional
  lane argument allows each measurement to run in its own JVM."
  (:require [afterglow.control.model :as control]
            [afterglow.output.artnet :as artnet]
            [clojure.set :as set]
            [clojure.string :as str]
            [tolglow.bench-show :as bench]
            [tolglow.show :as show])
  (:import [com.sun.management ThreadMXBean]
           [java.lang.management ManagementFactory]
           [java.util Arrays]))

(defn- issue [model cmd]
  (let [command (merge {:show-id (:show-id model) :epoch (:epoch model) :time-ms 0
                        :source :performance-profile :command-id (str "probes/" (:op cmd))} cmd)
        {:keys [model ack]} (control/apply-command model command)]
    (when-not (= :accepted (:status ack))
      (throw (ex-info "Benchmark setup rejected" ack)))
    model))

(defn- performance-model [pixels movers]
  (let [model (:model (bench/corpus pixels movers))
        ;; Keep two marked pixels close to each other and away from the band.
        ;; Both have explicit colors. Only the red pixel is an authored source.
        rig (mapv (fn [fixture]
                    (case (:id fixture)
                      "pixel-0" (assoc fixture :position [-101 0 0])
                      "pixel-1" (assoc fixture :position [-100 0 0])
                      (assoc fixture :groups [:band])))
                  (get-in model [:rig :fixtures]))
        probes {:id :probes :version 1 :name "HDR probe pixels" :params {}
                :targets ["pixel-0" "pixel-1"]
                :outputs [{:attribute :color :value [1.0 0.0 0.0] :targets ["pixel-0"]}
                          {:attribute :color :value [0.0 0.0 0.0] :targets ["pixel-1"]}
                          {:attribute :white :value 0.0} {:attribute :dimmer :value 1.0}]}
        conductor {:enabled? true :scope {:group-id :band} :strategy :instant :rotate? true
                   :interval {:beats 1/2} :window {:bars 1} :seed 41
                   :targets {:brightness {:waveform :triangle :period {:bars 2} :range [0.12 0.28]}
                             :activity 0.14}}]
    (-> model (assoc-in [:rig :fixtures] rig)
        (assoc :settings {:autopilot conductor
                          :hdr {:enabled? true :gain 1.5 :source-gains {"pixel-0" 3.0} :white-mix 0.8 :spill 0.35
                                :spill-white-mix 0.6 :max-excess 0.75 :radius 1.5 :falloff :smoothstep}})
        (issue {:op :registry/publish :kind :effects :definition probes})
        (issue {:op :cue/put :cue {:id :probes :effect-id :probes :priority 99}})
        (issue {:op :cue/launch :cue-id :probes}))))

(defn- counted-capture []
  ;; show/tick! calls this synchronously, before the next buffer can be reused.
  ;; Inspect the captured buffers immediately after the tick timing ends.
  (let [latest (volatile! nil)
        counter (volatile! 0)
        datagrams (volatile! 0)]
    {:latest latest :counter counter :datagrams datagrams :description {:type :artdmx-capture :network false}
     :send! (fn [universes frame-number]
              (vswap! counter inc)
              (vswap! datagrams + (count universes))
              (vreset! latest [frame-number universes]))
     :close! (fn [])}))

(defn- check-frame! [runtime sink before]
  (let [{:keys [model output status fault-events]} @(:state runtime)
        [number sent] @(:latest sink)
        universes (:universes output)]
    (assert (nil? (:error status)) (str (:error status)))
    (assert (empty? fault-events))
    (assert (= (inc before) @(:counter sink)))
    (assert (= (:frame-number model) number))
    (assert (= (set (keys universes)) (set (keys sent))))
    (doseq [[id ^bytes dmx] sent]
      (let [^bytes packet (artnet/artdmx-packet id (inc (mod number 255)) dmx)]
        (assert (= 530 (alength packet)))
        (assert (= id (inc (+ (bit-and 255 (aget packet 14))
                              (bit-shift-left (bit-and 255 (aget packet 15)) 8)))))
        (assert (Arrays/equals packet 18 530 ^bytes (get universes id) 0 512))))))

(defn- worker-ids [^ThreadMXBean bean]
  (into #{} (keep (fn [id]
                    (when-let [info (.getThreadInfo bean (long id))]
                      (when (str/starts-with? (.getThreadName info) "afterglow-runtime-") id))))
        (.getAllThreadIds bean)))

(defn- thread-sample [^ThreadMXBean bean ^longs workers]
  {:wall (System/nanoTime) :owner-cpu (.getCurrentThreadCpuTime bean)
   :owner-bytes (.getThreadAllocatedBytes bean (.getId (Thread/currentThread)))
   :worker-cpu (reduce + (.getThreadCpuTime bean workers))
   :worker-bytes (reduce + (.getThreadAllocatedBytes bean workers))})

(defn- distributions [samples]
  (when (seq samples)
    (let [ms (fn [k] (bench/distribution (map #(/ (double (k %)) 1e6) samples)))]
      {:frames (count samples) :wall-ms (ms :wall) :over-25-ms (count (filter #(> (:wall %) 25000000) samples))
       :owner-cpu-ms (ms :owner-cpu) :worker-cpu-ms (ms :worker-cpu)
       :owner-bytes (bench/distribution (map :owner-bytes samples))
       :worker-bytes (bench/distribution (map :worker-bytes samples))})))

(defn- final-output [runtime pixels movers lane]
  (let [{:keys [model output]} @(:state runtime)
        ^bytes dmx (get-in output [:universes 1])
        rgb (fn [offset] (mapv #(bit-and 255 (aget dmx (+ offset %))) (range 3)))
        source (rgb 0)
        receiver (rgb 4)
        ^booleans aim (get-in output [:compact :fields :aim :valid])
        probes {:source-rgb-bytes source :receiver-rgb-bytes receiver}]
    (assert (every? #(aget aim (int %)) (range pixels (+ pixels movers))))
    (assert (not-any? :fault (vals (:instances model))))
    (if (= :base lane)
      (do (assert (= source [255 0 0]))
          (assert (= receiver [0 0 0])))
      (do (assert (and (pos? (second source)) (pos? (last source)))
                  (str "Expected the overdriven red source to whiten, received " source))
          (assert (pos? (first receiver)) "The explicitly black neighbor must receive spill")))
    (if (= :autopilot lane)
      (let [run (get-in model [:extensions :tolglow.autopilot/run])
            controller (:controller run)
            selected (set (:active controller))
            levels (show/source-brightness runtime (keys (:membership controller)))
            emitted (into #{} (keep (fn [[id v]] (when (pos? (:local-brightness v)) id))) levels)
            visible (set/intersection emitted selected)
            stats (:statistics run)]
        (assert (= (count levels) (- (+ pixels movers) 2)))
        (assert (seq visible))
        (assert (every? #(<= 0.0 (get-in stats [% :mean]) 1.0) [:activity :brightness]))
        (assoc probes :selected (count selected) :visible-selected (count visible)
               :visible-others (count (set/difference emitted selected)) :scope-count (count levels)
               :statistics stats :targets (:targets run)))
      probes)))

(defn- profile-lane [model pixels movers warmup frames lane]
  (let [model (case lane
                :base (dissoc model :settings)
                :hdr (update model :settings dissoc :autopilot)
                :autopilot model)
        sink (counted-capture)
        ^ThreadMXBean bean (ManagementFactory/getPlatformMXBean ThreadMXBean)
        prior-workers (worker-ids bean)
        runtime (show/create-runtime {:model model :sink sink :workers 2 :workspace-options nil})
        tick #(show/tick! runtime (* 25 (inc %)))
        decision #(get-in @(:state runtime) [:model :extensions :tolglow.autopilot/run :decision :decision])]
    (try
      (dotimes [i warmup]
        (let [before @(:counter sink)] (tick i) (check-frame! runtime sink before)))
      (let [workers (long-array (set/difference (worker-ids bean) prior-workers))
            gc (bench/gc-snapshot)
            samples (mapv (fn [i]
                            (let [previous (decision)
                                  before @(:counter sink)
                                  begin (thread-sample bean workers)]
                              (tick i)
                              (let [deltas (merge-with - (thread-sample bean workers) begin)]
                                (check-frame! runtime sink before)
                                (assoc deltas :decision? (not= previous (decision))))))
                          (range warmup (+ warmup frames)))
            groups (group-by :decision? samples)]
        (assert (= 2 (alength workers)))
        (assert (= @(:datagrams sink)
                   (* (+ warmup frames) (quot (+ (* 4 pixels) (* 12 movers) 511) 512))))
        (assert (seq (get groups false)))
        (when (= :autopilot lane) (assert (seq (get groups true))))
        (cond-> {:capture-frames @(:counter sink) :capture-datagrams @(:datagrams sink)
                 :universes (count (second @(:latest sink)))
                 :worker-threads (alength workers) :all (distributions samples)
                 :gc (merge-with #(merge-with - %1 %2) (bench/gc-snapshot) gc)
                 :output (final-output runtime pixels movers lane)}
          (= :autopilot lane) (assoc :decision (distributions (get groups true))
                                     :between-decisions (distributions (get groups false)))))
      (finally (show/stop! runtime)))))

(def lanes [:base :hdr :autopilot])

(defn profile
  ([pixels movers warmup frames] (profile pixels movers warmup frames lanes))
  ([pixels movers warmup frames selected-lanes]
   (when-not (and (integer? pixels) (<= 4 pixels) (pos-int? movers)
                  (pos-int? warmup) (integer? frames) (>= frames 20)
                  (seq selected-lanes) (every? (set lanes) selected-lanes))
     (throw (ex-info "Use at least four pixels, one moving head, one warmup and 20 measured frames"
                     {:lanes selected-lanes :available lanes})))
   (let [model (performance-model pixels movers)]
     {:pixels pixels :moving-heads movers :warmup warmup :frames frames :tick-ms 25
      :lanes (into {} (map (fn [lane] [lane (profile-lane model pixels movers warmup frames lane)]))
                   selected-lanes)})))

(defn -main [& args]
  (try
    (let [[pixels movers warmup frames] (if (seq args) (mapv parse-long (take 4 args)) [2048 64 100 160])
          selected-lanes (if-let [lane (nth args 4 nil)] [(keyword lane)] lanes)]
      (prn (profile pixels movers warmup frames selected-lanes)))
    (finally (shutdown-agents))))
