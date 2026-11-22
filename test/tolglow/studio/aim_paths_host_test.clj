(ns tolglow.studio.aim-paths-host-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data.json :as json]
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
      (is (= "accepted" (get-in (post (assoc aim :mode "edge-to-edge" :along "negative-u")) [:data :status])))
      (let [view (show/snapshot runtime) link {:leader :head-1 :follower :head-2 :surface-id :floor :along [-1.0 0.0 0.0]}]
        (is (= [link] (get-in view [:rig :edge-tracking])))
        (is (= {:head-1 [2 0 1]} (:surface-aim view)))
        (is (= [link] (get-in (show/export-show runtime) [:model :rig :edge-tracking])))
        (is (= "accepted" (get-in (post (assoc aim :mode "follow" :point nil)) [:data :status])))
        (is (empty? (:surface-aim (show/snapshot runtime))))
        (is (= "accepted" (get-in (post (assoc aim :mode "release" :targets ["head-1"])) [:data :status])))
        (is (empty? (get-in (show/snapshot runtime) [:rig :edge-tracking]))))
      (finally (show/stop! runtime)))))
