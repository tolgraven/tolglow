(ns tolglow.bench-knob-candidate-sweep
  "Owner tick and Room audition frame cost while a knob candidate (#667, #737)
  is swept on the showcase rig: five cues with their authored waveforms, one
  generated candidate, a new audition value every fourth owner tick.

  Logical 23 ms ticks run back to back on one thread, and each audition frame
  is awaited with a spin, not a sleep: on Apple silicon a sleeping wait lets
  the scheduler move both threads to slower cores, which inflates every number
  over a run. Measures software CPU frames only; no output, GPU or display.

    lein with-profile +dev run -m tolglow.bench-knob-candidate-sweep [samples]
    bin/repl run tolglow.bench-knob-candidate-sweep 300

  Prints both modes: :coalesced (current) and :restage-every-value, which
  forces the earlier behaviour of a fresh private copy and compile caches per
  value through the same code path.

  Best case: the showcase commits no live revisions between values, so every
  value retunes. With routing, audio returns, compositions, auto launches or a
  second encoder committing, values restage and cost what :restage-every-value
  shows; :retuned-values reports how many retuned."
  (:require [clojure.pprint :as pprint]
            [tolglow.show :as show]
            [tolglow.showcase :as showcase]
            [tolglow.showcase.looks :as looks])
  (:import [java.lang.management GarbageCollectorMXBean ManagementFactory]))

(def ^:private cues [:beam-shafts :strobe :can-can :mover-orbit :wash-tide])

(defn- ms-since [t0] (/ (- (System/nanoTime) t0) 1.0e6))

(defn- gc-ms []
  (reduce + (map #(.getCollectionTime ^GarbageCollectorMXBean %) (ManagementFactory/getGarbageCollectorMXBeans))))

(defn- summary [xs]
  (let [v (vec (sort xs)) n (count v) at #(nth v (min (dec n) (long (* % n))))]
    {:n n :median-ms (format "%.2f" (at 0.5)) :p90-ms (format "%.2f" (at 0.9)) :max-ms (format "%.2f" (peek v))}))

(defn- spin-audition! [rt]
  (while (:in-flight @(:scene-draft rt)))
  true)

(defn sweep
  "Measure samples audition values after warmup values."
  [samples warmup]
  (let [render @#'show/render-audition-frame
        frames (atom [])
        rt (show/create-runtime {:model (showcase/model) :workspace-options nil :worker-timeout-ms 60000
                                 :audition {:max-live-share ##Inf}})
        clock (atom 0)
        tick! (fn [] (let [t0 (System/nanoTime)] (show/tick! rt (swap! clock + 23)) (ms-since t0)))
        submit! (fn [payload]
                  (let [reply (show/submit! rt :command (merge {:source {:adapter :studio :session-id "bench"}} payload))
                        owner (tick!)]
                    [(deref reply 5000 nil) owner]))]
    (try
      (let [ids (into {} (for [cue cues]
                           [cue (:instance-id (first (submit! {:op :cue/launch :cue-id cue :launch {:mode :persistent}})))]))]
        (doseq [[cue id] ids [param binding] (get looks/sources cue)]
          (let [reply (show/submit! rt :modulation-bind {:instance-id id :param param :binding binding})]
            (tick!)
            (deref reply 5000 nil))))
      (submit! {:op :knob-candidate/generate :seed 11})
      (submit! {:op :knob-candidate/audition :value 0.5})
      (spin-audition! rt)
      (with-redefs [show/render-audition-frame (fn [& args]
                                                 (let [t0 (System/nanoTime) result (apply render args)]
                                                   (swap! frames conj (ms-since t0))
                                                   result))]
        (let [gc0 (gc-ms)
              result (reduce
                      (fn [acc i]
                        (let [[ack owner] (submit! {:op :knob-candidate/audition :value (/ (mod (* 7 i) 101) 100.0)})
                              _ (spin-audition! rt)
                              first-frame (peek @frames)
                              steady (vec (repeatedly 3 #(let [owner (tick!)] (spin-audition! rt) [owner (peek @frames)])))]
                          (if (< i warmup)
                            acc
                            (-> acc
                                (update :value-tick conj owner)
                                (update :value-frame conj first-frame)
                                (update :steady-tick into (map first steady))
                                (update :steady-frame into (map second steady))
                                (update :retuned + (if (:retuned? ack) 1 0))))))
                      {:value-tick [] :value-frame [] :steady-tick [] :steady-frame [] :retuned 0}
                      (range (+ warmup samples)))]
          {:owner-tick-with-value (summary (:value-tick result))
           :owner-tick-steady (summary (:steady-tick result))
           :audition-frame-after-value (summary (:value-frame result))
           :audition-frame-steady (summary (:steady-frame result))
           :retuned-values (:retuned result)
           :gc-ms (- (gc-ms) gc0)}))
      (finally (show/stop! rt)))))

(defn -main [& [samples]]
  (let [samples (or (some-> samples parse-long) 300)
        restaging #(with-redefs [show/retunable? (constantly false)] (sweep samples 40))]
    (sweep 50 10)
    (pprint/pprint {:machine-load (.getSystemLoadAverage (ManagementFactory/getOperatingSystemMXBean))
                    :processors (.availableProcessors (Runtime/getRuntime))
                    :restage-every-value (restaging)
                    :coalesced (sweep samples 40)})))
