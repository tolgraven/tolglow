(require '[clojure.data.json :as json]
         '[clojure.test :as test]
         '[tolglow.show :as show]
         '[tolglow.studio.sync-host-test :as sync-test])

(let [fixture-var (ns-resolve 'tolglow.studio.sync-host-test 'with-live-host!)
      with-host! @fixture-var
      tick! show/tick!
      capture (atom {:frames []})
      active (atom nil)
      pending (atom nil)
      record-tick! (fn [runtime time]
                     (tick! runtime time nil)
                     (when (identical? runtime @active)
                       (when-let [{:keys [pitch atBeat release]} (first (reset-vals! pending nil))]
                         (let [state @(:state runtime)
                               instance (last (sort-by :order (vals (get-in state [:model :instances]))))
                               heads (mapv (fn [[_ head]] (select-keys head [:emitted-color :intensity]))
                                           (sort-by key (get-in (show/snapshot runtime) [:output :values])))]
                           (swap! capture update :frames conj
                                  {:time-ms time :beat (if release (+ atBeat 1.25) atBeat) :pitch pitch
                                   :release (true? release) :instance (:id instance) :phase (:phase instance)
                                   :level (get-in instance [:params :level]) :heads heads})))))]
  (with-redefs-fn
    {fixture-var
     (fn [runtime exercise!]
       (reset! active runtime)
       (with-host! runtime
         (fn [host! endpoint]
           (exercise!
             (fn [request]
               (let [result (host! request)]
                 (cond
                   (:play request) (reset! pending request)
                   (:edit request) (swap! capture assoc :edit request)
                   (and (:notes request) (some #(== 1.25 (:duration %)) result))
                   (swap! capture assoc (if (:edit @capture) :edited :original) result))
                 result))
             endpoint))))
     #'show/tick! record-tick!}
    #(binding [test/*report-counters* (ref test/*initial-report-counters*)]
       (test/test-vars [#'sync-test/operator-gesture-writes-and-restores-on-an-editable-session-lane])
       (let [{:keys [fail error] :as counts} @test/*report-counters*]
         (prn counts)
         (when (pos? (+ fail error)) (System/exit 1))
         (spit "/workspace/artifacts/tolglow-ableton-gestures-228/trace.json"
               (json/write-str (assoc @capture :assertions (:pass counts))))))))

