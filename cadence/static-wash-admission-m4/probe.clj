(require '[afterglow.runtime.keyframes :as keyframes]
         '[clojure.java.io :as io]
         '[tolglow.perf.weighted-live :as venue])

(def production-evaluate keyframes/evaluate)
(def invariant-sampler
  (ns-resolve 'afterglow.runtime.keyframes 'invariant-sampler))

(defn admit-certified-invariants [step {:keys [frame instance] :as context}]
  (let [id (:id instance)]
    (if (and (:measure-effect frame)
             (nil? (get-in frame [:keyframe-intervals id]))
             (invariant-sampler step context (get-in instance [:definition :keyframes])))
      (let [admitted (-> context
                         (update :frame #(if (empty? (:keyframes %))
                                           (keyframes/with-rig-positions %)
                                           %))
                         (assoc-in [:frame :keyframe-intervals id] {:ms 200}))]
        (assoc (production-evaluate step admitted) :frame frame))
      (production-evaluate step context))))

(let [configuration {:pixels 20000 :warmup 160 :slots 240}
      directory (io/file "target/weighted-static-admission-ec569ed0")]
  (doseq [[index mode] (map-indexed vector [:baseline :candidate :candidate :baseline])]
    (let [report (if (= :candidate mode)
                   (with-redefs [keyframes/evaluate admit-certified-invariants]
                     (venue/capture configuration))
                   (venue/capture configuration))
          summary (:summary report)
          filename (str (inc index) "-" (name mode) ".edn")]
      (spit (io/file directory filename) (pr-str report))
      (assert (:show-output-exercised? summary) filename)
      (assert (zero? (:incomplete-packets summary)) filename)
      (prn {:sample (inc index)
            :mode mode
            :output (select-keys summary [:nominal-slots :completed-renders :complete-within-25-ms
                                          :completed-render-ms :sent :source-counts :sink-gap-ms
                                          :worker-calls :incomplete-packets])})
      (flush))))

(shutdown-agents)
