(ns tolglow.studio.aim-paths-host-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data.json :as json]
            [afterglow.control.model :as control]
            [afterglow.render.frame :as frame]
            [tolglow.aim-paths-test :as paths]
            [tolglow.show :as show]
            [tolglow.studio :as studio])
  (:import [java.io ByteArrayInputStream]))

(def path-spec
  {:id "drawn-path" :name "Across the room" :targets ["head-1"]
   :points [[-2 0 0] [2 0 0]] :duration {:beats 4} :mode "once" :curve "linear"})

(defn- submit-at! [runtime type payload t]
  (let [reply (show/submit! runtime type payload)]
    (show/tick! runtime t)
    (deref reply 1000 ::missing)))

(defn- post! [runtime path payload t]
  ;; Run the actual Ring request while the explicit host owner drains its queue.
  (let [result (future ((studio/handler runtime)
                       {:request-method :post :uri path :headers {"host" "localhost"}
                        :body (ByteArrayInputStream. (.getBytes (json/write-str payload) "UTF-8"))}))
        deadline (+ (System/nanoTime) 3000000000)]
    (try
      (loop []
        (cond
          (realized? result) (let [response @result]
                              (assoc response :data (json/read-str (:body response) :key-fn keyword)))
          (> (System/nanoTime) deadline) (throw (ex-info "Ring path request timed out" {}))
          :else (do (show/tick! runtime t) (Thread/sleep 5) (recur))))
      (finally (future-cancel result)))))

(deftest ring-publication-is-atomic-and-only-explicit-launch-moves-a-head
  (let [runtime (show/create-runtime {:workspace-options nil})]
    (try
      (let [result (post! runtime "/api/aim-path" (assoc path-spec :runtime-id (:id runtime) :expected-epoch 0) 0)
            model (:model @(:state runtime))
            definition (get-in model [:registry :effects :drawn-path])]
        (is (= 200 (:status result)))
        (is (= "accepted" (get-in result [:data :status])))
        (is (= 1 (get-in result [:data :version])))
        (is (= :aim-path (get-in definition [:editor :type])))
        (is (empty? (:instances model)))
        (is (some #{:drawn-path} (vals (get-in model [:pages :paths :cells]))))
        (let [launch (submit-at! runtime :command {:op :cue/launch :cue-id :drawn-path :launch {:mode :one-shot}} 0)
              id (:instance-id launch)
              first-values (get-in (show/snapshot runtime) [:output :values])]
          (is (= :accepted (:status launch)))
          (show/tick! runtime 500)
          (is (nil? (get-in (show/snapshot runtime) [:status :error])))
          (is (contains? (get-in @(:state runtime) [:model :instances]) id))
          (is (not= (get-in first-values [:head-1 :pan])
                    (get-in (show/snapshot runtime) [:output :values :head-1 :pan])))
          (is (= (:head-2 first-values) (get-in (show/snapshot runtime) [:output :values :head-2])))
          (let [saved (show/export-show runtime)]
            (is (= definition (get-in saved [:model :registry :effects :drawn-path])))
            (is (= :accepted (:status (submit-at! runtime :import {:source (pr-str saved)} 500))))
            (is (= definition (get-in @(:state runtime) [:model :registry :effects :drawn-path]))))
          (show/tick! runtime 2500)
          (is (empty? (:instances (:model @(:state runtime)))))))
      (finally (show/stop! runtime)))))

(deftest stale-versions-sessions-and-cue-collisions-cannot-partially-publish
  (let [runtime (show/create-runtime {:workspace-options nil})]
    (try
      (post! runtime "/api/aim-path" path-spec 0)
      (doseq [payload [(assoc path-spec :expected-version 0)
                       (assoc path-spec :id "collision" :cue-id "ember")
                       (assoc path-spec :expected-epoch 99)
                       (assoc path-spec :runtime-id "retired-runtime")]]
        (let [registry (get-in @(:state runtime) [:model :registry])
              result (post! runtime "/api/aim-path" payload 0)]
          (is (= "rejected" (get-in result [:data :status])))
          (is (= registry (get-in @(:state runtime) [:model :registry])))))
      (is (= 2 (get-in (post! runtime "/api/aim-path" (assoc path-spec :expected-version 1 :mode "ping-pong") 0)
                       [:data :version])))
      (is (empty? (:instances (:model @(:state runtime)))))
      (finally (show/stop! runtime)))))

(deftest deleted-path-cue-can-be-recreated-with-original-id
  (let [runtime (show/create-runtime {:workspace-options nil})]
    (try
      (is (= "accepted" (get-in (post! runtime "/api/aim-path" path-spec 0) [:data :status])))
      (is (= :accepted (:status (submit-at! runtime :command {:op :cue/delete :cue-id :drawn-path} 0))))
      (is (= "accepted" (get-in (post! runtime "/api/aim-path" path-spec 0) [:data :status])))
      (is (= 2 (get-in @(:state runtime) [:model :registry :cues :drawn-path :version])))
      (finally (show/stop! runtime)))))

(deftest tapped-surface-controls-use-the-current-patch-and-normal-frame-packing
  (let [runtime (show/create-runtime {:workspace-options nil})
        aim {:targets ["head-1" "head-2"] :surface-id "floor" :point [2 0 1] :mode "together"}
        post #(post! runtime "/api/surface-aim" % 100)]
    (try
      (is (= #{:stage :floor :back :left :right} (set (map :id (:receiver-surfaces (show/snapshot runtime))))))
      (is (= "accepted" (get-in (post aim) [:data :status])))
      (let [view (show/snapshot runtime)]
        (is (= {:head-1 [2 0 1] :head-2 [2 0 1]} (:surface-aim view)))
        (is (= [2.0 0.0 1.0] (get-in view [:output :values :head-1 :aim])))
        (is (= [2.0 0.0 1.0] (get-in view [:output :values :head-2 :aim])))
        (is (pos? (get-in view [:output :values :head-1 :pan])))
        (doseq [invalid [(assoc aim :expected-epoch 99) (assoc aim :point [2 3 1]) (assoc aim :targets ["missing"])
                         (assoc aim :point [2000 0 0]) (assoc aim :mode "edge-to-edge" :along "other")]]
          (is (= "rejected" (get-in (post invalid) [:data :status])))
          (is (= (:surface-aim view) (:surface-aim (show/snapshot runtime))))))
      (is (= "accepted" (get-in (post (assoc aim :point [-1 1.5 2] :surface-id nil)) [:data :status])))
      (is (= [-1 1.5 2] (get-in (show/snapshot runtime) [:surface-aim :head-1])))
      (is (= "accepted" (get-in (post (assoc aim :mode "edge-to-edge" :along "negative-u")) [:data :status])))
      (let [view (show/snapshot runtime) link {:leader :head-1 :follower :head-2 :surface-id :floor :along [-1.0 0.0 0.0]}]
        (is (= [link] (get-in view [:rig :edge-tracking])))
        (is (= {:head-1 [2 0 1]} (:surface-aim view)))
        (is (= [link] (get-in (show/export-show runtime) [:model :rig :edge-tracking])))
        (is (= "accepted" (get-in (post (assoc aim :mode "follow" :point nil)) [:data :status])))
        (is (empty? (:surface-aim (show/snapshot runtime))))
        (is (= "accepted" (get-in (post (assoc aim :mode "release" :targets ["head-1"])) [:data :status])))
        (is (empty? (get-in (show/snapshot runtime) [:rig :edge-tracking]))))
      (post aim)
      (let [saved (show/export-show runtime)]
        (is (= (:surface-aim (show/snapshot runtime)) (get-in saved [:model :variables :surface-aim])))
        (submit-at! runtime :command {:op :show/restart} 101)
        (is (empty? (:surface-aim (show/snapshot runtime)))))
      (finally (show/stop! runtime)))))

(defn- word [packet [coarse-universe coarse-index] [fine-universe fine-index]]
  (+ (* 256 (get-in packet [coarse-universe coarse-index]))
     (get-in packet [fine-universe fine-index])))

(deftest calibrated-heads-reach-their-commanded-endpoints-on-the-same-captured-frame
  (let [captured (atom nil)
        parked {:id :parked :position [0 1 -1] :universe 2 :address 10 :channels {:pan [1 2] :tilt [3 4]}}
        runtime (show/create-runtime
                 {:workspace-options nil
                  :model (control/new-show {:rig {:fixtures (conj paths/paired-heads parked)}})
                  :sink {:send! (fn [universes _]
                                  (reset! captured (update-vals universes #(mapv (partial bit-and 255) %))))}})
        pan-words #(vector (word @captured [1 510] [2 0]) (word @captured [2 4] [2 5]))
        angle-code #(Math/round (* 65535.0 (/ (+ % 180.0) 360.0)))
        start (mapv angle-code [-45.0 -42.5])
        end (mapv angle-code [45.0 2.5])]
    (try
      (let [spec (assoc paths/synchronized-spec :targets ["far-travel" "slow-motor"])
            response (post! runtime "/api/aim-path" spec 0)
            published (get-in @(:state runtime) [:model :registry])
            baseline @captured]
        (is (= "accepted" (get-in response [:data :status])))
        (is (< (abs (- 2000.0 (get-in response [:data :arrival-duration-ms]))) 1.0e-9))
        (is (empty? (get-in @(:state runtime) [:model :instances])))
        (is (= [0 0] (pan-words)))
        (is (= "rejected" (get-in (post! runtime "/api/aim-path" (assoc spec :targets ["parked"]) 0) [:data :status])))
        (is (= published (get-in @(:state runtime) [:model :registry])))
        (is (= baseline @captured))
        (let [launch (submit-at! runtime :command {:op :cue/launch :cue-id :paired} 0)]
          (is (= :accepted (:status launch)))
          (is (= :pinned (get-in @(:state runtime) [:model :instances (:instance-id launch) :reload-policy])))
          (is (= start (pan-words)))
          (show/tick! runtime 1000)
          (is (= (mapv angle-code [0.0 -20.0]) (pan-words)))
          (show/tick! runtime 1975)
          (is (every? true? (map < (pan-words) end)))
          (show/tick! runtime 2000)
          (is (= end (pan-words)))
          (is (= (subvec (get baseline 2) 9 13) (subvec (get @captured 2) 9 13)))
          (show/tick! runtime 2500)
          (is (= end (pan-words)))
          (is (nil? (get-in (show/snapshot runtime) [:status :error])))))
      (finally (show/stop! runtime)))))

(deftest synchronized-launch-refuses-stale-calibration-and-runs-in-a-timed-slot
  (let [runtime (show/create-runtime
                 {:workspace-options nil
                  :model (control/new-show {:rig {:fixtures paths/paired-heads}})
                  :sink {:send! (fn [_ _])}})
        spec (assoc paths/synchronized-spec :duration {:bars 2})]
    (try
      (let [response (post! runtime "/api/aim-path" spec 0)]
        (is (= "accepted" (get-in response [:data :status])))
        (is (< (abs (- 4000.0 (get-in response [:data :arrival-duration-ms]))) 1.0e-9))
        (let [one-shot (submit-at! runtime :command {:op :cue/launch :cue-id :paired :launch {:mode :one-shot}} 10)]
          (is (= :accepted (:status one-shot)))
          (is (= {:bars 2} (get-in @(:state runtime) [:model :instances (:instance-id one-shot) :duration]))))
        (let [rig (get-in @(:state runtime) [:model :rig])
              faster (assoc-in rig [:fixtures 1 :calibration :motor-rate-deg-per-sec :pan] 60)]
          (is (= :accepted (:status (submit-at! runtime :rig faster 20))))
          (let [before (get-in @(:state runtime) [:model :instances])
                refused (submit-at! runtime :command {:op :cue/launch :cue-id :paired} 30)]
            (is (= :rejected (:status refused)))
            (is (= :stale-calibration (:code refused)))
            (is (= (keys before) (keys (get-in @(:state runtime) [:model :instances])))))
          (is (= "accepted" (get-in (post! runtime "/api/aim-path" spec 40) [:data :status])))
          (is (= :accepted (:status (submit-at! runtime :command {:op :cue/launch :cue-id :paired} 50)))))
        (is (nil? (get-in (show/snapshot runtime) [:status :error]))))
      (finally (show/stop! runtime)))))

(defn- committed-pan-deg [runtime fixture]
  (let [{:keys [values compact]} (:output @(:state runtime))
        row (or (get values (:id fixture))
                (some #(when (= (:id fixture) (:id %)) %) (frame/project-compact compact)))]
    (:pan-deg (frame/fixture-pose fixture row))))

(deftest synchronized-start-wrap-follows-the-committed-pose
  (let [head (-> (first paths/paired-heads)
                 (assoc :position [0 0 0])
                 (assoc-in [:calibration :pan] {:min-deg -270 :max-deg 270}))
        runtime (show/create-runtime
                 {:workspace-options nil
                  :model (control/new-show {:rig {:fixtures [head]}})
                  :sink {:send! (fn [_ _])}})
        point (fn [yaw] (let [r (Math/toRadians yaw)] [(Math/sin r) 0 (Math/cos r)]))
        sync-spec {:id :wrapped :targets [:far-travel] :points [(point -170) (point -150)] :sync-arrival? true}]
    (try
      ;; Published before prepositioning, the start resolves to the neutral -170 degree wrap.
      (is (= "accepted" (get-in (post! runtime "/api/aim-path" sync-spec 0) [:data :status])))
      ;; An ordinary once path sweeps continuously from -90 to +190 degrees and holds there.
      (is (= "accepted" (get-in (post! runtime "/api/aim-path" {:id :preposition :targets [:far-travel] :mode "once"
                                                                :duration {:ms 1000}
                                                                :points (mapv point [-90 0 90 150 190])} 0)
                                [:data :status])))
      (let [preposition (submit-at! runtime :command {:op :cue/launch :cue-id :preposition} 0)]
        (doseq [t (range 50 1101 50)] (show/tick! runtime t))
        (is (< (abs (- 190.0 (committed-pan-deg runtime head))) 1.0e-3))
        (let [refused (submit-at! runtime :command {:op :cue/launch :cue-id :wrapped} 1150)]
          (is (= :rejected (:status refused)))
          (is (= :start-wrap-mismatch (:code refused))))
        (is (= "accepted" (get-in (post! runtime "/api/aim-path" sync-spec 1200) [:data :status])))
        (let [effect (get-in @(:state runtime) [:model :registry :effects :wrapped])
              start (get-in (first (:outputs effect)) [:value :spec :keyframes 0 :value])]
          (is (< (abs (- 190.0 (:pan-deg (frame/fixture-pose head {:pan start :tilt 0.5})))) 1.0e-6)))
        (is (= :accepted (:status preposition)))
        (is (= :accepted (:status (submit-at! runtime :command {:op :cue/launch :cue-id :wrapped} 1300))))
        (is (< (abs (- 190.0 (committed-pan-deg runtime head))) 1.0e-3)
            "Frame 0 commands the prepositioned wrap, not a full-turn jump"))
      (finally (show/stop! runtime)))))
