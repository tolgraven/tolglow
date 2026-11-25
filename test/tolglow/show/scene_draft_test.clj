(ns tolglow.show.scene-draft-test
  (:require [afterglow.runtime.events :as events]
            [clojure.test :refer [deftest is testing]]
            [tolglow.demo :as demo]
            [tolglow.output-response :as response]
            [tolglow.events :as show-events]
            [tolglow.show :as show]
            [tolglow.show.scene-draft :as draft]))

(def blue
  {:id :blue :version 1
   :outputs {:color (demo/constant [0.0 0.0 1.0]) :dimmer (demo/constant 1.0)}})

(def edits
  [{:op :registry/publish :kind :effects :definition blue}
   {:op :cue/put :cue {:id :blue-a :effect-id :blue :selector {:fixture-ids [:head-1]}}}
   {:op :cue/launch :cue-id :blue-a}
   {:op :effect/launch :effect-id :blue :launch {:targets [:head-2]}}])

(defn- envelope [model command-id revision time-ms]
  {:command-id command-id :show-id (:show-id model) :epoch (:epoch model)
   :source {:adapter :repl :session-id "test"} :time-ms time-ms :expected-draft-revision revision})

(deftest staged-content-commits-as-one-transaction
  (let [base (demo/model)
        {empty-draft :draft} (draft/stage base nil {:draft-id "scene" :operations []} 0)
        {:keys [ack] staged :draft} (draft/stage base empty-draft {:draft-id "scene" :operations edits
                                                                   :expected-draft-revision 0} 0)
        before (events/begin (-> base (assoc :time-ms 500) (assoc-in [:transport :beat] 1)))
        applied (draft/apply-draft before staged (envelope before "take-1" 1 500))
        instances (sort-by :order (vals (get-in applied [:model :instances])))]
    (is (= [:accepted 1] [(:status ack) (:draft-revision ack)]))
    (is (= edits (mapv :request (:steps staged))))
    (is (empty? (:instances base)))
    (is (= :accepted (get-in applied [:ack :status])))
    (testing "one revision and one retry/history entry for the whole scene"
      (is (= (inc (:revision before)) (get-in applied [:model :revision])))
      (is (= (conj (get-in before [:control :history-order]) "take-1")
             (get-in applied [:model :control :history-order])))
      (is (= :scene-draft/apply (get-in applied [:model :control :history "take-1" :command :op]))))
    (is (= [500 500] (mapv :started-at-ms instances)))
    (is (every? #(== 1 (:started-at-beat %)) instances))
    (is (= [[:head-1] [:head-2]] (mapv :targets instances)))
    (is (= [:registry/published :registry/published :instance/launched :instance/launched :scene-draft/applied]
           (mapv (comp :type :event) (events/pending (:model applied)))))
    (is (= 1 (count (distinct (map :emitter (events/pending (:model applied)))))))
    (testing "a retried transaction returns its receipt without applying again"
      (let [again (draft/apply-draft (:model applied) staged (envelope before "take-1" 1 500))]
        (is (identical? (:model applied) (:model again)))
        (is (= (:ack applied) (:ack again)))))))

(deftest stale-or-invalid-transactions-preserve-the-owner-model
  (let [base (demo/model)
        group-edits [(assoc-in (second edits) [:cue :selector] {:group-id :blue-room})
                     (nth edits 2)]
        grouped (-> base
                    (assoc-in [:registry :effects :blue] blue)
                    (assoc-in [:extensions :tolglow.lighting-scenes/control :groups :blue-room :fixture-ids] [:head-1]))
        staged (:draft (draft/stage grouped nil {:draft-id "scene" :operations group-edits} 0))]
    (doseq [[code current] [[:stale-epoch (update grouped :epoch inc)]
                            [:stale-registry (assoc-in grouped [:registry :effects :blue :version] 2)]
                            [:stale-patch (update-in grouped [:rig :fixtures] pop)]
                            [:stale-targets (assoc-in grouped [:extensions :tolglow.lighting-scenes/control :groups
                                                              :blue-room :fixture-ids] [:head-2])]]]
      (let [current (events/begin current)
            command (assoc (envelope current "take-1" 0 0) :epoch (:epoch grouped))
            result (draft/apply-draft current staged command)]
        (is (= code (get-in result [:ack :code])) (str code))
        (is (identical? current (:model result)))))
    (let [stale (draft/apply-draft grouped staged (envelope grouped "take-1" 1 0))]
      (is (= :stale-draft (get-in stale [:ack :code])))
      (is (identical? grouped (:model stale))))
    (testing "a different draft or an outdated revision cannot replace the staged one"
      (is (= :draft-active (get-in (draft/stage grouped staged {:draft-id "other" :operations []} 0) [:ack :code])))
      (is (= :stale-draft (get-in (draft/stage grouped staged {:draft-id "scene" :operations []
                                                               :expected-draft-revision 3} 0) [:ack :code]))))
    (testing "A late invalid operation prevents any draft from being installed"
      (doseq [request [{:op :cue/launch :cue-id :missing}
                       {:op :cue/launch :cue-id :blue-a :grid {:beats 1}}
                       {:op :cue/launch :cue-id :blue-a :launch {:mode :held}}
                       {:op :cue/launch :cue-id :blue-a :launch {:targets [:unknown]}}
                       {:op :show/restart}]]
        (let [result (draft/stage base nil {:draft-id "scene" :operations (conj edits request)} 0)]
          (is (= :rejected (get-in result [:ack :status])))
          (is (nil? (:draft result))))))))

(defn- near? [expected actual] (< (abs (- expected actual)) 1.0e-6))

(deftest grid-boundaries-follow-the-transport-meter
  (is (== 4 (draft/grid-boundary {:bars 1} {:beat 1.25})))
  (is (== 4 (draft/grid-boundary {:bars 1} {:beat 4})))
  (is (== 6 (draft/grid-boundary {:beats 3} {:beat 4.5 :meter {:beats-per-bar 3}})))
  (is (near? 1990.0 (draft/boundary-time {:beat 4.02 :bpm 120} 2000 4 0)))
  (is (thrown? clojure.lang.ExceptionInfo (draft/grid-boundary {:ms 100} {:beat 0}))))

;; Runtime evidence. Each runtime renders the same deterministic show at the
;; same logical frame times; a sink captures every live packet. Tests wait for
;; each audition frame so that every live frame has a matching private one.

(def ^:private frame-ms 25)

(defn- runtime
  ([model] (runtime model {:max-live-share ##Inf}))
  ([model audition]
   (let [sent (atom [])]
     {:sent sent
      :runtime (show/create-runtime (cond-> {:workspace-options nil :audition audition
                                             :sink {:send! (fn [packets frame]
                                                             (swap! sent conj [frame (update-vals packets vec)]))}}
                                      model (assoc :model model)))})))

(defn- submit [{:keys [runtime]} command]
  (show/submit! runtime (assoc command :source {:adapter :repl :session-id "test"})))

(defn- tick [{:keys [runtime]} time-ms]
  (show/tick! runtime time-ms)
  (is (show/await-audition runtime 5000)))

(defn- receipt [reply] (deref reply 1000 :timeout))

(defn- view [{:keys [runtime]}] (show/snapshot runtime))

(defn- model-of [{:keys [runtime]}] (:model @(:state runtime)))

(defn- launch-live! [rt]
  (let [launch (submit rt {:op :cue/launch :cue-id :ember :command-id "live"})]
    (tick rt frame-ms)
    (:instance-id (receipt launch))))

(defn- operations [live-id]
  (into [{:op :instance/end :instance-id live-id}] edits))

(defn- run-capture [audition?]
  (let [rt (runtime nil)]
    (try
      (let [live-id (launch-live! rt)
            staged (when audition?
                     (submit rt {:op :scene-draft/stage :draft-id "scene" :operations (operations live-id)}))
            frames (doall (for [time-ms (range 50 301 frame-ms)]
                            (do (tick rt time-ms) (view rt))))]
        {:ack (some-> staged receipt) :frames frames :sent @(:sent rt) :model (model-of rt)
         :draft (show/scene-draft (:runtime rt))})
      (finally (show/stop! (:runtime rt))))))

(deftest an-audition-changes-room-preview-without-changing-live-packets
  (let [live (run-capture false)
        auditioned (run-capture true)
        blue? #(= [0.0 0.0 1.0] (get-in % [:values :head-1 :emitted-color]))]
    (is (= :accepted (get-in auditioned [:ack :status])))
    (is (= 12 (count (:sent live)) (count (:sent auditioned))))
    (is (= (:sent live) (:sent auditioned)) "every live packet byte is unchanged")
    (is (= (:model live) (:model auditioned)) "the committed show model is unchanged")
    (is (= (map :output (:frames live)) (map :output (:frames auditioned))) "live preview values stay live")
    (is (every? #(= {:id "scene" :revision 0} (get-in % [:draft-output :draft])) (:frames auditioned)))
    (is (every? (comp nil? :draft-output) (:frames live)))
    (is (blue? (:draft-output (last (:frames auditioned)))))
    (is (not (blue? (:output (last (:frames auditioned))))))
    (is (= :auditioning (get-in auditioned [:draft :status])))
    (is (= {:rendered 11 :skipped 0} (get-in auditioned [:draft :frames])))
    (is (= [:instance/end :registry/publish :cue/put :cue/launch :effect/launch]
           (mapv :op (get-in auditioned [:draft :draft :operations]))))))

(defn- smoothed-show []
  (assoc-in (demo/model) [:settings :output-response]
            (response/settings {:enabled? true :global {:attack {:beats 2} :release {:beats 1}}})))

(deftest audition-frames-match-the-show-after-applying-at-the-same-time
  ;; Output response keeps per-fixture state from frame to frame, and the
  ;; released live cue follows its envelope. The audition must advance both.
  (let [audition (runtime (smoothed-show)) applied (runtime (smoothed-show))]
    (try
      (let [live-id (launch-live! audition)
            _ (launch-live! applied)
            _ (doseq [time-ms [50 75 100]] (tick audition time-ms) (tick applied time-ms))
            staged (submit audition {:op :scene-draft/stage :draft-id "scene" :operations (operations live-id)})
            _ (submit applied {:op :scene-draft/stage :draft-id "scene" :operations (operations live-id)})
            taken (submit applied {:op :scene-draft/apply :draft-id "scene" :expected-draft-revision 0
                                   :command-id "take"})
            frames (doall (for [time-ms (range 125 551 frame-ms)]
                            (do (tick audition time-ms) (tick applied time-ms)
                                [(view audition) (view applied)])))]
        (is (= :accepted (:status (receipt staged))))
        (is (= :accepted (:status (receipt taken))))
        (doseq [[auditioned committed] frames]
          (is (= {:id "scene" :revision 0} (get-in auditioned [:draft-output :draft])))
          (is (nil? (:draft-output committed)))
          (is (= (get-in committed [:output :values]) (get-in auditioned [:draft-output :values]))))
        (testing "the response smoothing is still moving, so frames were not frozen"
          (let [levels (map #(get-in (first %) [:draft-output :values :head-1 :emitted-color 2]) frames)]
            (is (apply < (take 6 levels))))))
      (finally (show/stop! (:runtime audition)) (show/stop! (:runtime applied))))))

(deftest a-slow-audition-never-delays-or-changes-live-frames
  (let [render @#'show/render-audition-frame
        untouched (runtime nil {}) slowed (runtime nil {})]
    (try
      (with-redefs [show/render-audition-frame (fn [& args] (Thread/sleep 80) (apply render args))]
        (let [live-id (launch-live! untouched)
              _ (launch-live! slowed)
              staged (submit slowed {:op :scene-draft/stage :draft-id "scene" :operations (operations live-id)})
              durations (doall (for [time-ms (range 50 551 frame-ms)]
                                 (do (show/tick! (:runtime untouched) time-ms)
                                     (let [started (System/nanoTime)]
                                       (show/tick! (:runtime slowed) time-ms)
                                       (/ (- (System/nanoTime) started) 1.0e6)))))
              _ (show/await-audition (:runtime slowed) 5000)
              {:keys [rendered skipped]} (:frames (show/scene-draft (:runtime slowed)))]
          (is (= :accepted (:status (receipt staged))))
          (is (< (apply max (drop 2 durations)) 60) "no live tick waits for an 80 ms audition frame")
          (is (pos? rendered))
          (is (pos? skipped) "busy audition frames are skipped and counted")
          (is (= (+ rendered skipped) (count durations)))
          (is (= @(:sent untouched) @(:sent slowed)) "live packets are unchanged")
          (is (= (get-in @(:state (:runtime untouched)) [:cadence :state])
                 (get-in @(:state (:runtime slowed)) [:cadence :state])) "live cadence state matches")))
      (finally (show/stop! (:runtime untouched)) (show/stop! (:runtime slowed))))))

(defn- schedule-on-bar! [rt]
  (let [live-id (launch-live! rt)
        staged (submit rt {:op :scene-draft/stage :draft-id "scene" :operations (operations live-id)})
        _ (tick rt 630)
        scheduled (submit rt {:op :scene-draft/apply :draft-id "scene" :expected-draft-revision 0
                              :command-id "on-the-one" :grid {:bars 1}})]
    (tick rt 660)
    (is (= :accepted (:status (receipt staged))))
    (is (= [:scheduled 4.0] ((juxt :status (comp double :due-beat)) (receipt scheduled))))
    (doseq [time-ms (range 690 1981 30)] (tick rt time-ms))
    live-id))

(deftest a-quantized-apply-lands-on-the-bar-boundary
  (let [rt (runtime nil)]
    (try
      (let [live-id (schedule-on-bar! rt)
            before (model-of rt)]
        (is (= :scheduled (:status (show/scene-draft (:runtime rt)))))
        (is (contains? (:instances before) live-id) "nothing commits before the boundary")
        (tick rt 2010)
        (let [model (model-of rt)
              launched (filter #(= :blue (:effect-id %)) (vals (:instances model)))]
          (is (= 2 (count launched)))
          (is (every? #(near? 2000 (:started-at-ms %)) launched) "the logical time the transport crossed beat 4")
          (is (every? #(== 4 (:started-at-beat %)) launched))
          (is (= :releasing (get-in model [:instances live-id :phase])))
          (is (== 2010 (:time-ms model)))
          (is (near? 4.02 (get-in model [:transport :beat])) "the sampled transport is restored")
          (is (= (inc (:revision before)) (:revision model)) "one revision for the whole scene")
          (is (= "on-the-one" (last (get-in model [:control :history-order]))))
          (is (= {:status :applied :command-id "on-the-one" :due-beat 4.0 :beat 4.0}
                 (-> (show/scene-draft (:runtime rt)) :last-outcome
                     (select-keys [:status :command-id :due-beat :beat])
                     (update :due-beat double) (update :beat double))))
          (is (nil? (:draft-output (view rt))))))
      (finally (show/stop! (:runtime rt))))))

(deftest queued-input-before-the-boundary-is-applied-first
  (let [rt (runtime nil)]
    (try
      (schedule-on-bar! rt)
      (let [before (model-of rt)
            dropped (submit rt {:op :scene-draft/discard :draft-id "scene"})]
        (tick rt 2010)
        (is (= [:accepted "on-the-one"] ((juxt :status :cancelled-apply) (receipt dropped))))
        (is (not-any? #(= :blue (:effect-id %)) (vals (:instances (model-of rt)))))
        (is (= (:revision before) (:revision (model-of rt))))
        (is (nil? (show/scene-draft (:runtime rt)))))
      (finally (show/stop! (:runtime rt))))))

(deftest a-failed-cycle-does-not-report-an-apply
  (let [rt (runtime nil)]
    (try
      (schedule-on-bar! rt)
      (let [before (model-of rt)]
        (with-redefs [show-events/commit (fn [& _] (throw (ex-info "cycle failed" {})))]
          (is (thrown? clojure.lang.ExceptionInfo (show/tick! (:runtime rt) 2010))))
        (is (= :scheduled (:status (show/scene-draft (:runtime rt)))) "the draft still waits for its boundary")
        (is (nil? (:last-outcome (show/scene-draft (:runtime rt)))))
        (is (= before (model-of rt))))
      (finally (show/stop! (:runtime rt))))))

(deftest discarding-a-draft-leaves-no-trace
  (let [untouched (runtime nil) discarded (runtime nil)]
    (try
      (let [live-id (launch-live! untouched)
            _ (launch-live! discarded)
            staged (submit discarded {:op :scene-draft/stage :draft-id "scene" :operations (operations live-id)})
            _ (doseq [time-ms [50 75]] (tick untouched time-ms) (tick discarded time-ms))
            dropped (submit discarded {:op :scene-draft/discard :draft-id "scene"})
            _ (doseq [time-ms [100 125 150]] (tick untouched time-ms) (tick discarded time-ms))
            late (submit discarded {:op :scene-draft/apply :draft-id "scene" :expected-draft-revision 0})]
        (tick discarded 175) (tick untouched 175)
        (is (= :accepted (:status (receipt staged))))
        (is (= [:accepted true] ((juxt :status :discarded?) (receipt dropped))))
        (is (= :no-draft (:code (receipt late))))
        (is (= @(:sent untouched) @(:sent discarded)))
        (is (= (model-of untouched) (model-of discarded)))
        (is (nil? (show/scene-draft (:runtime discarded))))
        (is (nil? @(:scene-draft (:runtime discarded))))
        (is (nil? (:draft-output (view discarded))))
        (is (= (:output (view untouched)) (:output (view discarded)))))
      (finally (show/stop! (:runtime untouched)) (show/stop! (:runtime discarded))))))
