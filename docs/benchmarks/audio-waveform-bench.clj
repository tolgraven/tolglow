;; From repository root: clojure -M docs/benchmarks/audio-waveform-bench.clj
;; Synthetic input measures summary+immutable projection only, not audio capture,
;; existing analysis, scheduling, JSON, transport or browser rendering.
(require '[tolglow.studio.waveforms :as waveforms])
(let [samples (double-array (map #(* 0.75 (Math/sin (* 2 Math/PI (/ % 480.0)))) (range 480)))
      state (volatile! (waveforms/new-capture))
      times (loop [i 0 times []]
              (if (= i 1200) times
                  (let [start (System/nanoTime)
                        next (waveforms/capture-block @state {:samples samples :sample-rate 48000 :channels 1
                                                              :source-id "bench" :started-at-ms (* 10.0 i)})
                        view (waveforms/capture-view next)
                        elapsed (/ (- (System/nanoTime) start) 1e6)]
                    (assert (<= (count (:bins view)) 256))
                    (vreset! state next)
                    (recur (inc i) (cond-> times (>= i 200) (conj elapsed))))))
      sorted (vec (sort times))]
  (prn {:clojure (clojure-version) :java (System/getProperty "java.version")
        :samples 1000 :warmup 200 :sample-rate 48000 :block-frames 480 :max-columns 256
        :p50-ms (nth sorted 500) :p95-ms (nth sorted 950) :p99-ms (nth sorted 990) :max-ms (peek sorted)}))
(shutdown-agents)
