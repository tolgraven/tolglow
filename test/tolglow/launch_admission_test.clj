(ns tolglow.launch-admission-test
  "Launch admission in a beat that starts many launches (#810). The run record
  takes each lane without scanning the held ones, keeps exactly its newest
  takes without copying its take order per launch, and survives a launch
  whose source is not a map; sync observation reads only the commands a cycle
  accepted. Assertions compare against reference rules and count work; none
  reads a clock."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.integrations.live-runs :as runs]
            [tolglow.show :as show]))

(defn- model
  "A committed show model whose history holds `commands` ([command ack] with
  :command-id set) and whose running instances are `running`."
  [{:keys [beat epoch commands running] :or {epoch 0}}]
  {:epoch epoch :time-ms (* 500 beat)
   :transport {:beat (double beat) :bpm 120.0 :playing? true :time-ms (* 500 beat)}
   :registry {:cues {:a {:name "A" :version 1} :b {:name "B" :version 1}}}
   :instances (into {} (for [id running] [id {:id id :phase :running :effect-version 1}]))
   :control {:history (into {} (for [[c a] commands]
                                 [(:command-id c) {:command (assoc c :time-ms (* 500 beat))
                                                   :ack (assoc a :status :accepted)}]))
             :history-order (mapv (comp :command-id first) commands)}})

(defn- launch [id cue-id instance-id]
  [{:op :cue/launch :cue-id cue-id :command-id id :source {:adapter :push}} {:instance-id instance-id}])

(defn- reference-lane
  "The lowest lane of cue-id that no unreleased run holds, by scanning."
  [record cue-id]
  (first (remove #(contains? (:lanes record) [cue-id %]) (range))))

(deftest lanes-match-the-lowest-free-lane-through-launches-releases-and-restarts
  ;; A seeded walk of launches and ends on two cues, with a show restart: every
  ;; new take gets the lane the scanning rule gives on the record before it.
  (let [rng (java.util.Random. 810)
        steps 400]
    (loop [i 0 record (runs/new-record) running #{} epoch 0 history []]
      (when (< i steps)
        (let [roll (.nextInt rng 100)
              restart? (= 57 roll)
              epoch (if restart? (inc epoch) epoch)
              running (if restart? #{} running)
              history (if restart? [] history)
              end? (and (seq running) (< roll 45))
              cue (if (even? (.nextInt rng 2)) :a :b)
              instance (str epoch ":" i)
              ended (when end? (nth (sort running) (.nextInt rng (count running))))
              command (if end?
                        [{:op :instance/end :instance-id ended :command-id (str "c" i) :force? true :source {:adapter :push}} {}]
                        (launch (str "c" i) cue instance))
              history (conj history command)
              running (if end? (disj running ended) (conj running instance))
              expected (when-not end? (reference-lane (if restart? (assoc record :lanes {}) record) cue))
              next-record (runs/observe-model record (model {:beat (/ i 4) :epoch epoch :commands history :running running}))]
          (when-not end?
            (is (= expected (get-in next-record [:takes (get-in next-record [:open instance]) :lane]))
                (str "step " i)))
          (is (= (into #{} (for [[cue-id {:keys [lanes]}] (:channels next-record) lane (range lanes)
                                 :when (not (contains? (:lanes next-record) [cue-id lane]))]
                             [cue-id lane]))
                 (into #{} (for [[cue-id free] (:free-lanes next-record) lane free] [cue-id lane])))
              "the free lanes are exactly the unheld lanes below each channel's count")
          (recur (inc i) next-record running epoch history))))))

(deftest a-burst-of-one-cue-takes-lanes-in-order
  (let [commands (mapv #(launch (str "c" %) :a (str "0:" %)) (range 64))
        record (runs/observe-model (runs/new-record) (model {:beat 1 :commands commands :running (map #(str "0:" %) (range 64))}))]
    (is (= (range 64) (map :lane (runs/takes record))))
    (is (= 64 (get-in record [:channels :a :lanes])))
    (is (empty? (:free-lanes record)))))

(deftest a-launch-with-a-keyword-source-records-its-take
  ;; The control model admits any non-nil source. A bare keyword used to make
  ;; every fold fail, so the record never advanced and each frame folded the
  ;; whole history again.
  (let [commands [[{:op :cue/launch :cue-id :a :command-id "k1" :source :performance-profile} {:instance-id "0:1"}]
                  (launch "c2" :b "0:2")]
        record (runs/observe-model (runs/new-record) (model {:beat 1 :commands commands :running ["0:1" "0:2"]}))]
    (is (= [[:a {:type :performance-profile}] [:b {:adapter :push}]] (map (juxt :cue-id :source) (runs/takes record))))
    (is (= #{"k1" "c2"} (:seen record)))))

(defn- fill
  "2,000 one-frame runs through a record that keeps `bound` takes, after an
  optional first run that never ends. Returns the record and how many takes
  the bound looked at."
  [bound long-run?]
  (let [visits (atom 0)
        ended? @#'runs/ended?]
    (with-redefs-fn {#'runs/ended? (fn [record id] (swap! visits inc) (ended? record id))}
      (fn []
        (let [first-record (if long-run?
                             (runs/observe-model (runs/new-record {:max-takes bound})
                                                 (model {:beat 0 :commands [(launch "long" :b "0:long")] :running ["0:long"]}))
                             (runs/new-record {:max-takes bound}))
              record (reduce (fn [record i]
                               (let [record (runs/observe-model record (model {:beat (/ (inc i) 4) :commands [(launch (str "c" i) :a (str "0:" i))]
                                                                               :running (cond-> [(str "0:" i)] long-run? (conj "0:long"))}))]
                                 (is (<= (count (:take-order record)) bound) "the order stays bounded")
                                 record))
                             first-record (range 2000))]
          {:record record :visits @visits})))))

(deftest a-full-record-keeps-its-newest-takes-without-copying-the-order-per-launch
  ;; Before, each launch past the bound rebuilt the whole take order; with a
  ;; run still going from the start of the show, it did so on every launch
  ;; (1,745 times in 2,000). The order is now keyed by serial, so a launch
  ;; drops its oldest ended take with one dissoc and looks at no more takes
  ;; than are running ahead of it.
  (let [bound 256]
    (testing "every take ended"
      (let [{:keys [record visits]} (fill bound false)
            takes (runs/takes record)]
        (is (= bound (count takes) (count (:takes record))))
        (is (= (mapv #(str "run." %) (range (- 2001 bound) 2001)) (mapv :take-id takes)) "exactly the newest takes, in order")
        (is (= (- 2000 bound) (:dropped record)))
        (is (= (- 2000 bound) visits) "one take looked at per launch past the bound")))
    (testing "the first run never ends"
      (let [{:keys [record visits]} (fill bound true)
            takes (runs/takes record)]
        (is (= (cons "run.1" (map #(str "run." %) (range (- 2002 (dec bound)) 2002))) (map :take-id takes))
            "the running take stays first, then the newest ended ones")
        (is (= (- 2001 bound) (:dropped record)))
        (is (= (* 2 (- 2001 bound)) visits) "the running take and the one dropped, per launch past the bound")))))

(deftest a-record-from-before-a-reload-reuses-its-free-lanes
  ;; A record a running show held while this namespace reloaded has a vector
  ;; take order and no :free-lanes. It takes the current shape on its next
  ;; fold, so lanes released before the reload are reused.
  (let [record (runs/observe-model (runs/new-record)
                                   (model {:beat 0 :commands (mapv #(launch (str "c" %) :a (str "0:" %)) (range 3)) :running ["0:0" "0:1" "0:2"]}))
        record (runs/observe-model record (model {:beat 1 :commands (mapv #(launch (str "c" %) :a (str "0:" %)) (range 3)) :running ["0:2"]}))
        old (-> record (dissoc :free-lanes) (update :take-order (comp vec vals)))
        next-record (runs/observe-model old (model {:beat 2 :commands (conj (mapv #(launch (str "c" %) :a (str "0:" %)) (range 3)) (launch "c3" :a "0:3"))
                                                    :running ["0:2" "0:3"]}))]
    (is (= [0 1 2 0] (map :lane (runs/takes next-record))) "the new run takes lane 0 again")
    (is (= ["run.1" "run.2" "run.3" "run.4"] (map :take-id (runs/takes next-record))))
    (is (map? (:take-order next-record)))))

(def ^:private accepted-since @#'show/accepted-since)

(defn- history-model [epoch ids]
  {:epoch epoch :control {:history (into {} (map (fn [id] [id {:command {:command-id id}}])) ids)
                          :history-order (vec ids)}})

(deftest sync-observation-reads-only-the-commands-a-cycle-accepted
  (let [before (history-model 0 (map #(str "c" %) (range 1024)))
        ;; This cycle accepted 64 commands and the bounded history evicted the
        ;; 64 oldest.
        after (history-model 0 (map #(str "c" %) (range 64 1088)))
        visited (atom 0)
        counting (update-in before [:control :history]
                            (fn [h] (proxy [clojure.lang.APersistentMap] []
                                      (containsKey [k] (swap! visited inc) (contains? h k))
                                      (valAt ([k] (get h k)) ([k nf] (get h k nf)))
                                      (count [] (count h)) (seq [] (seq h))
                                      (iterator [] (.iterator ^Iterable h)))))]
    (is (= (map #(str "c" %) (range 1024 1088)) (accepted-since before after)))
    (is (= (map #(str "c" %) (range 1024 1088)) (accepted-since counting after)))
    (is (= 65 @visited) "the 64 new commands and the newest one already held")
    (testing "a cycle with no new command reads one ID"
      (reset! visited 0)
      (is (empty? (accepted-since counting before)))
      (is (= 1 @visited)))
    (testing "after a restart every command in the new epoch's history is new"
      (is (= ["r1" "r2"] (accepted-since before (history-model 1 ["r1" "r2"])))))))

;; ---------------------------------------------------------------------------
;; Background observers

(def ^:private fold-observers @#'show/fold-observers!)

(defn- observed-runtime [background?]
  (show/create-runtime {:model (afterglow.control.model/new-show
                                {:show-id :observers :transport {:playing? true :beat 0.0 :bpm 120.0}
                                 :registry {:effects {:wash {:id :wash :version 1 :params {:level {:type :number :default 0.5 :bounds [0.0 1.0]}}
                                                             :outputs {:dimmer {:op :param :key :level}}}}
                                            :cues {:a {:id :a :effect-id :wash :version 1}}}
                                 :rig {:fixtures [{:id :left :universe 1 :address 1 :channels {:dimmer 1}}]}})
                        :sink {:send! (fn [_ _])} :workspace-options nil
                        :live-runs {:record-id "r"} :background-observers background?}))

(defn- drain-worker!
  "Wait for every task queued on the observer worker so far (FIFO)."
  [rt]
  @(.submit ^java.util.concurrent.ExecutorService @(get-in rt [:observer-worker :executor]) ^Callable (fn [] true)))

(defn- stall-worker!
  "Block the observer worker until the returned promise is delivered."
  [rt]
  (let [release (promise) started (promise)]
    (.execute ^java.util.concurrent.ExecutorService @(get-in rt [:observer-worker :executor])
              (fn [] (deliver started true) @release))
    @started
    release))

(defn- launch-script [rt beat i]
  (show/submit! rt :command {:op :cue/launch :cue-id :a :command-id (str "l" i) :source {:adapter :push}
                             :launch {:mode :one-shot :duration {:beats 1/2}}}))

(deftest background-observers-fold-off-the-owner-and-match-inline-folding
  (let [inline (observed-runtime false) background (observed-runtime true)
        folds (atom {}) owner (Thread/currentThread)]
    (with-redefs-fn {#'show/fold-observers! (fn [rt model]
                                              (swap! folds update [(:observer-worker rt) (identical? owner (Thread/currentThread))] (fnil inc 0))
                                              (fold-observers rt model))}
      (fn []
        (try
          (doseq [i (range 24)]
            (doseq [rt [inline background]]
              (when (zero? (mod i 3)) (launch-script rt (/ i 2) i))
              (show/tick! rt (* 125 i)))
            (drain-worker! background)
            (is (= (show/live-runs-record inline) (show/live-runs-record background)) (str "frame " i)))
          (is (= 24 (get @folds [nil true])) "inline: every fold on the owner")
          (is (= 24 (get @folds [(:observer-worker background) false])) "background: every fold on the worker")
          (is (nil? (get @folds [(:observer-worker background) true])) "and none on the owner")
          (is (= 8 (count (:takes (show/live-runs-record background)))))
          (finally (show/stop! inline) (show/stop! background)))))))

(deftest a-fold-the-worker-has-not-reached-runs-before-the-next-cycle-or-a-read
  (let [rt (observed-runtime true) folds (atom []) owner (Thread/currentThread)]
    (with-redefs-fn {#'show/fold-observers! (fn [rt model]
                                              (swap! folds conj [(get-in model [:transport :beat]) (identical? owner (Thread/currentThread))])
                                              (fold-observers rt model))}
      (fn []
        (try
          (show/tick! rt 0)
          (drain-worker! rt)
          (let [release (stall-worker! rt)]
            (launch-script rt 0 1)
            (show/tick! rt 500)
            (is (= [[0.0 false]] @folds) "the stalled worker has not folded the new frame")
            (is (= 1 (count (:takes (show/live-runs-record rt)))) "a read folds it first")
            (is (= [[0.0 false] [1.0 true]] @folds) "on the reader's thread")
            (show/tick! rt 1000)
            (launch-script rt 2 2)
            (show/tick! rt 1500)
            (show/tick! rt 2000)
            (is (= [[2.0 true] [3.0 true]] (subvec @folds 2 4)) "each cycle first folds what the worker has not")
            (deliver release true)
            (drain-worker! rt)
            (is (= 2 (count (:takes (show/live-runs-record rt)))))
            (is (= [1.0 2.0 3.0 4.0] (mapv first (subvec @folds 1))) "every committed model is folded once, in order"))
          (finally (show/stop! rt)))))))

(deftest an-observer-failure-on-the-worker-reaches-status-on-the-next-cycle
  (let [rt (observed-runtime true)]
    (with-redefs [runs/observe-model (fn [_ _] (throw (ex-info "fold failed" {})))]
      (try
        (show/tick! rt 0)
        (drain-worker! rt)
        (is (nil? (get-in @(:state rt) [:status :live-runs-error])) "only the owner writes committed state")
        (show/tick! rt 25)
        (is (= "fold failed" (get-in @(:state rt) [:status :live-runs-error :message])))
        (finally (show/stop! rt))))))

(deftest stopping-finishes-the-last-folds-on-the-stopping-thread-and-reports-their-failures
  (testing "a fold the worker has not reached runs in stop!"
    (let [rt (observed-runtime true) folds (atom []) me (Thread/currentThread)]
      (with-redefs-fn {#'show/fold-observers! (fn [rt model]
                                                (swap! folds conj [(get-in model [:transport :beat]) (identical? me (Thread/currentThread))])
                                                (fold-observers rt model))}
        (fn []
          (show/tick! rt 0)
          (drain-worker! rt)
          (let [release (stall-worker! rt)]
            (launch-script rt 0 1)
            (show/tick! rt 500)
            (show/stop! rt)
            (is (= [[0.0 false] [1.0 true]] @folds) "the waiting frame was folded by stop!")
            (is (= 1 (count (:takes (show/live-runs-record rt)))))
            (deliver release true))))))
  (testing "a failure in that fold reaches status"
    (let [rt (observed-runtime true)]
      (show/tick! rt 0)
      (drain-worker! rt)
      (let [release (stall-worker! rt)]
        (with-redefs [runs/observe-model (fn [_ _] (throw (ex-info "last fold failed" {})))]
          (show/tick! rt 500)
          (show/stop! rt))
        (is (= "last fold failed" (get-in @(:state rt) [:status :live-runs-error :message])))
        (deliver release true)))))

(deftest a-fold-that-throws-any-throwable-is-reported-and-folding-goes-on
  (let [rt (observed-runtime true) folded (atom [])]
    (with-redefs-fn {#'show/fold-observers! (fn [rt model]
                                              (let [beat (get-in model [:transport :beat])]
                                                (when (= 1.0 beat) (throw (AssertionError. "fold assertion")))
                                                (swap! folded conj beat)
                                                (fold-observers rt model)))}
      (fn []
        (try
          (show/tick! rt 0)
          (drain-worker! rt)
          (let [release (stall-worker! rt)]
            (launch-script rt 0 1)
            (show/tick! rt 500)
            ;; This cycle first folds the waiting frame, which throws, and
            ;; publishes the failure after its own commit.
            (show/tick! rt 1000)
            (is (= "fold assertion" (get-in @(:state rt) [:status :observer-error :message])) "the failure reaches status")
            (is (= [0.0 2.0] (do (show/live-runs-record rt) @folded)) "the frame after the failing one is still folded")
            (deliver release true)
            (drain-worker! rt)
            (show/tick! rt 1500)
            (drain-worker! rt)
            (is (= [0.0 2.0 3.0] @folded) "and the worker keeps folding"))
          (finally (show/stop! rt)))))))

(deftest reading-the-record-under-the-owner-lock-is-refused
  ;; A fold may be waiting for the owner lock in submit!, so a reader holding
  ;; that lock must not wait for the fold.
  (let [rt (observed-runtime true)]
    (try
      (show/tick! rt 0)
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"outside the show owner lock"
                            (locking (:lock rt) (show/live-runs-record rt))))
      (is (map? (show/live-runs-record rt)) "outside it, the read works")
      (finally (show/stop! rt)))))
