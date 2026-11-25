(ns tolglow.show.scene-draft-test
  (:require [afterglow.runtime.events :as events]
            [clojure.test :refer [deftest is testing]]
            [tolglow.demo :as demo]
            [tolglow.show :as show]
            [tolglow.show.preview :as preview]
            [tolglow.show.push-scene :as push]
            [tolglow.show.scene-draft :as draft]))

(def blue
  {:id :blue :version 1
   :outputs {:color (demo/constant [0.0 0.0 1.0]) :dimmer (demo/constant 1.0)}})

(def edits
  [{:op :registry/publish :kind :effects :definition blue}
   {:op :cue/put :cue {:id :blue-a :effect-id :blue :selector {:fixture-ids [:head-1]}}}
   {:op :cue/launch :cue-id :blue-a}
   {:op :effect/launch :effect-id :blue :launch {:targets [:head-2]}}])

(deftest staged-content-commits-at-one-logical-time
  (let [base (demo/model)
        empty-draft (:draft (draft/begin base "scene" []))
        {:keys [draft ack]} (draft/revise empty-draft edits)
        sample (draft/sample draft 25 0.05)
        before (events/begin (-> base (assoc :time-ms 500) (assoc-in [:transport :beat] 1)))
        applied (draft/apply-draft before draft {:transaction-id "take-1" :expected-draft-revision 1} 500)
        instances (sort-by :order (vals (get-in applied [:model :instances])))]
    (is (= :accepted (:status ack)))
    (is (= 1 (:draft-revision sample)))
    (is (= #{:values :target-order} (set (keys (:output sample)))))
    (is (= #{:head-1 :head-2}
           (into #{} (keep (fn [[id v]] (when (= [0.0 0.0 1.0] (:emitted-color v)) id)))
                 (get-in sample [:output :values]))))
    (is (empty? (:instances base)))
    (is (= :accepted (get-in applied [:ack :status])))
    (is (= [500 500] (mapv :started-at-ms instances)))
    (is (every? #(== 1 (:started-at-beat %)) instances))
    (is (= [[:head-1] [:head-2]] (mapv :targets instances)))
    (is (= [:registry/published :registry/published :instance/launched :instance/launched]
           (mapv (comp :type :event) (events/pending (:model applied)))))))

(deftest stale-or-invalid-transactions-preserve-the-owner-model
  (let [base (demo/model)
        group-edits [(assoc-in (second edits) [:cue :selector] {:group-id :blue-room})
                     (nth edits 2)]
        grouped (-> base
                    (assoc-in [:registry :effects :blue] blue)
                    (assoc-in [:extensions :tolglow.lighting-scenes/control :groups :blue-room :fixture-ids] [:head-1]))
        draft (:draft (draft/begin grouped "scene" group-edits))]
    (doseq [[code current] [[:stale-epoch (update grouped :epoch inc)]
                            [:stale-registry (assoc-in grouped [:registry :effects :blue :version] 2)]
                            [:stale-patch (update-in grouped [:rig :fixtures] pop)]
                            [:stale-targets (assoc-in grouped [:extensions :tolglow.lighting-scenes/control :groups
                                                              :blue-room :fixture-ids] [:head-2])]]]
      (let [staged (events/begin current)
            result (draft/apply-draft staged draft {:transaction-id "take-1" :expected-draft-revision 0} 0)]
        (is (= code (get-in result [:ack :code])))
        (is (identical? staged (:model result)))))
    (let [stale (draft/apply-draft grouped draft {:transaction-id "take-1" :expected-draft-revision 1} 0)]
      (is (= :stale-draft (get-in stale [:ack :code])))
      (is (identical? grouped (:model stale))))
    (testing "A late invalid operation prevents any draft from being installed"
      (doseq [request [{:op :cue/launch :cue-id :missing}
                       {:op :cue/launch :cue-id :blue-a :grid {:beats 1}}
                       {:op :cue/launch :cue-id :blue-a :launch {:mode :held}}
                       {:op :cue/launch :cue-id :blue-a :launch {:targets [:unknown]}}
                       {:op :show/restart}]]
        (let [result (draft/begin base "scene" (conj edits request))]
          (is (= :rejected (get-in result [:ack :status])))
          (is (nil? (:draft result))))))))

(defn- capture-run [audition?]
  (let [sent (atom [])
        runtime (show/create-runtime {:workspace-options nil
                                      :sink {:send! (fn [packets frame]
                                                      (swap! sent conj [frame (update-vals packets vec)]))}})
        cache (push/create-cache)]
    (try
      (let [launch (show/submit! runtime {:op :cue/launch :cue-id :ember :command-id "live"})]
        (show/tick! runtime 25)
        (let [live-id (:instance-id (deref launch 1000 {}))
              operations (into [{:op :instance/end :instance-id live-id :force? true}] edits)
              state @(:state runtime)
              staged (when audition? (draft/begin (:model state) "scene" operations))
              private-draft (:draft staged)
              sample (when private-draft (draft/sample private-draft 50 0.1))]
          (when audition?
            (is (= :accepted (get-in staged [:ack :status])))
            (is (identical? state @(:state runtime)))
            (is (= 1 (count @sent)))
            (is (= [0.0 0.0 1.0] (get-in (preview/values (:output sample)
                                         (get-in state [:model :rig :fixtures]))
                                        [:head-1 :emitted-color])))
            (is (= [0.0 0.0 1.0]
                   (get-in (push/project-display cache (assoc state :output (:output sample))
                                                 {:display-mode :simulator})
                           [:scene :output :values "0" :emitted-color]))))
          (doseq [time-ms (range 50 251 25)]
            (show/tick! runtime time-ms))
          (when private-draft
            (let [live (:model @(:state runtime))
                  applied (draft/apply-draft live private-draft
                                           {:transaction-id "take-1" :expected-draft-revision 0} 250)]
              (is (= :accepted (get-in applied [:ack :status])))
              (is (contains? (:instances live) live-id))
              (is (= [:blue :blue] (mapv :effect-id (vals (get-in applied [:model :instances])))))))
          @sent))
      (finally
        (show/stop! runtime)))))

(deftest an-audition-changes-room-projections-without-changing-live-packets
  (let [live (capture-run false)
        auditioned (capture-run true)]
    (is (= 10 (count live) (count auditioned)))
    (is (= live auditioned))))
