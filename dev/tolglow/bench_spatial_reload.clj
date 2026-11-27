(ns tolglow.bench-spatial-reload
  "Measure owner ticks on the mixed rig with spatial cues, on ordinary frames
  and on frames where spatial cues take over (or reject) hot reloads. Capture
  sink only; no network output.

  lein run -m tolglow.bench-spatial-reload [pixels movers cues rounds]"
  (:require [tolglow.bench-show :as bench]
            [tolglow.show :as show]))

(defn- orb [i]
  {:op :spawn :id (keyword (str "orb-" i)) :page-id :bench :cell [(mod i 8) (quot i 8)]
   :volume {:shape :sphere :position [(double (* 12 i)) 0.0 10.0] :radius 2.0 :selection #{:position}
            :falloff {:inner-m 1.0 :outer-m 2.0 :curve :linear}}
   :paint {:red 1.0}
   :launch {:reload-policy :preserve-state}})

(defn- request! [rt t type payload]
  (let [reply (show/submit! rt type payload)]
    (show/tick! rt t)
    (let [ack (deref reply 5000 {:status :timeout})]
      (when-not (= :accepted (:status ack))
        (throw (ex-info "Benchmark request rejected" {:ack ack}))))))

(defn- revision
  "Version `version` of a spawned cue's effect: wider falloff, or an output
  that divides by zero so the engine rejects it at evaluation."
  [definition version reject?]
  (cond-> (-> definition (assoc :version version)
              (assoc-in [:spatial-volume :volume :falloff :outer-m] (+ 2.0 (* 0.01 version))))
    reject? (-> (assoc-in [:params :divisor] {:type :number :default 0.0 :bounds [0.0 1.0]})
                (assoc-in [:outputs :red] {:op :divide :args [{:op :param :key :red} {:op :param :key :divisor}]}))))

(defn- distribution [values] (when (seq values) (bench/distribution values)))

(defn- timed-tick [rt t]
  (let [start (System/nanoTime)]
    (show/tick! rt t)
    (/ (- (System/nanoTime) start) 1e6)))

(defn profile
  "Ordinary ticks and takeover ticks with `cues` spatial cues; `reloading` of
  them publish a revision in each takeover round."
  [pixels movers cues reloading rounds]
  (let [rt (show/create-runtime {:model (:model (bench/corpus pixels movers)) :workers 2
                                 :sink (bench/capture-sink) :workspace-options nil :effect-log! (fn [_])})
        clock (volatile! 0)
        next-t #(vswap! clock + 25)]
    (try
      (doseq [i (range cues)] (request! rt (next-t) :spatial-cue (orb i)))
      (dotimes [_ 200] (show/tick! rt (next-t)))
      (let [ordinary (mapv (fn [_] (timed-tick rt (next-t))) (range 240))
            takeovers
            (vec (for [round (range rounds)
                       :let [reject? (odd? round)
                             version (+ 2 round)
                             model (:model @(:state rt))
                             replies (mapv #(show/submit! rt :definition
                                                          {:kind :effects
                                                           :definition (revision (get-in model [:registry :effects (:id (orb %))])
                                                                                 version reject?)})
                                           (range reloading))
                             ms (timed-tick rt (next-t))
                             _ (doseq [r replies] (deref r 5000 nil))
                             ;; Let the frame after a takeover settle before the next round.
                             after (mapv (fn [_] (timed-tick rt (next-t))) (range 8))]]
                   {:reject? reject? :ms ms :after (first after)}))
            snapshot @(:state rt)]
        (assert (nil? (get-in snapshot [:status :error])) (pr-str (get-in snapshot [:status :error])))
        {:pixels pixels :movers movers :spatial-cues cues :reloading reloading :rounds rounds
         :ordinary-ms (bench/distribution ordinary)
         :accepted-takeover-ms (distribution (map :ms (remove :reject? takeovers)))
         :rejected-takeover-ms (distribution (map :ms (filter :reject? takeovers)))
         :frame-after-takeover-ms (distribution (map :after takeovers))
         :over-25-ms {:ordinary (count (filter #(> % 25.0) ordinary))
                      :takeover (count (filter #(> (:ms %) 25.0) takeovers))}})
      (finally (show/stop! rt)))))

(defn -main [& args]
  (try
    (let [[pixels movers cues rounds] (if (seq args) (mapv parse-long args) [2048 64 8 40])]
      (prn (profile pixels movers 0 0 0))
      (doseq [reloading (distinct [1 cues])]
        (prn (profile pixels movers cues reloading rounds))))
    (finally (shutdown-agents))))
