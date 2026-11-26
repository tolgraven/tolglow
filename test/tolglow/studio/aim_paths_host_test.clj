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

(def ^:private head-words
  "Packed [coarse fine] channel positions of each paired head's pan and tilt."
  {:far-travel {:pan [[1 510] [2 0]] :tilt [[1 511] [2 1]]}
   :slow-motor {:pan [[2 4] [2 5]] :tilt [[2 6] [2 7]]}})

(def ^:private frame-ms 25)

(defn- commanded-move
  "Publish the two-bar paired move at 120 BPM, launch it at 0 ms on a capture
  sink and tick every 25 ms. `changes` maps a frame time to transport changes
  admitted in that owner cycle, or to :instance/pause or :instance/resume for
  the launched instance; a change at 0 follows the launch command.
  Returns one {:t :packet :words} per frame and the launched instance.
  :twin? launches the same outputs as an ordinary effect, which has no
  synchronized-path launch handling and reads the beat clock alone.
  :stopped? stops the transport before the launch command."
  [changes & {:keys [twin? stopped? until-ms] :or {until-ms 4500}}]
  (let [captured (atom nil)
        runtime (show/create-runtime
                 {:workspace-options nil
                  :model (control/new-show {:rig {:fixtures paths/paired-heads}})
                  :sink {:send! (fn [universes _]
                                  (reset! captured (update-vals universes #(mapv (partial bit-and 255) %))))}})
        frame (fn [t]
                {:t t :packet @captured
                 :words (update-vals head-words (fn [axes] (update-vals axes #(apply word @captured %))))})
        launched (volatile! nil)
        change! (fn [t] (when-let [change (get changes t)]
                          (show/submit! runtime :command (if (keyword? change)
                                                           {:op change :instance-id @launched}
                                                           {:op :transport/set :transport change}))))]
    (try
      (let [spec (assoc paths/synchronized-spec :targets ["far-travel" "slow-motor"] :duration {:bars 2})
            published (post! runtime "/api/aim-path" spec 0)
            definition (get-in @(:state runtime) [:model :registry :effects :paired])
            _ (when twin?
                (submit-at! runtime :command {:op :registry/publish :kind :effects
                                              :definition (-> definition (dissoc :editor) (assoc :id :twin :version 1))} 0))
            _ (when stopped?
                (assert (= :accepted (:status (submit-at! runtime :command {:op :transport/set :transport {:playing? false}} 0)))))
            launch (show/submit! runtime :command (if twin? {:op :effect/launch :effect-id :twin}
                                                      {:op :cue/launch :cue-id :paired}))
            changed (change! 0)
            _ (show/tick! runtime 0)
            launch (deref launch 1000 ::missing)
            _ (vreset! launched (:instance-id launch))
            first-frame (frame 0)
            instance (get-in @(:state runtime) [:model :instances (:instance-id launch)])
            frames (into [first-frame]
                         (map (fn [t]
                                (let [changed (change! t)]
                                  (show/tick! runtime t)
                                  (when changed
                                    (assert (= :accepted (:status (deref changed 1000 ::missing)))))
                                  (frame t))))
                         (range frame-ms (inc until-ms) frame-ms))]
        (assert (= "accepted" (get-in published [:data :status])))
        (assert (= :accepted (:status launch)))
        (assert (or (nil? changed) (= :accepted (:status (deref changed 1000 ::missing)))))
        (assert (nil? (get-in (show/snapshot runtime) [:status :error])))
        {:frames frames :instance instance})
      (finally (show/stop! runtime)))))

(defn- degrees [fixture words]
  (frame/fixture-pose fixture (update-vals words #(/ % 65535.0))))

(defn- commanded-rates
  "Largest commanded mechanical rate in degrees per second between consecutive
  captured frames, per head and axis, with the rate one 16-bit step adds."
  [frames]
  (into {}
        (for [fixture paths/paired-heads [axis degree] [[:pan :pan-deg] [:tilt :tilt-deg]]]
          (let [id (:id fixture)
                value #(get (degrees fixture (get-in % [:words id])) degree)
                span (abs (- (get (degrees fixture {:pan 65535 :tilt 65535}) degree)
                             (get (degrees fixture {:pan 0 :tilt 0}) degree)))]
            [[id axis] {:max (reduce max 0.0 (map (fn [a b] (/ (abs (- (value b) (value a))) (/ (- (:t b) (:t a)) 1000.0)))
                                                  frames (rest frames)))
                        :step (/ (/ span 65535.0) (/ frame-ms 1000.0))
                        :calibrated (get-in fixture [:calibration :motor-rate-deg-per-sec axis])}]))))

(defn- rate-violations
  "Head axes whose largest commanded rate exceeds the calibrated rate by more
  than one 16-bit step, as [[id axis] degrees-per-second calibrated]."
  [frames]
  (vec (for [[axis {:keys [max step calibrated]}] (commanded-rates frames)
             :when (> max (+ calibrated step))]
         [axis max calibrated])))

(defn- pan-arrivals
  "First frame time at which each head's packed pan word is its final word."
  [frames]
  (into {} (for [id (keys head-words)]
             (let [pan #(get-in % [:words id :pan])]
               [id (:t (first (filter #(= (pan (peek frames)) (pan %)) frames)))]))))

(defn- frames-apart
  "Frame times at which two captures differ: in any packet byte, or with a
  word tolerance, in any pan or tilt word by more than that many 16-bit steps."
  ([expected actual] (vec (keep (fn [[a b]] (when (not= (:packet a) (:packet b)) (:t a))) (map vector expected actual))))
  ([expected actual steps]
   (vec (keep (fn [[a b]]
                (when (some (fn [[id axis]] (> (abs (- (get-in a [:words id axis]) (get-in b [:words id axis]))) steps))
                            (for [id (keys head-words) axis [:pan :tilt]] [id axis]))
                  (:t a)))
              (map vector expected actual)))))

(def ^:private pan-ends
  "Packed pan words of the published endpoint: +45 and +2.5 degrees."
  {:far-travel 40959 :slow-motor 33223})

(def ^:private arrive-together {:far-travel 4000 :slow-motor 4000})

(deftest raising-the-tempo-during-a-musical-synchronized-move-keeps-calibrated-rates
  (let [{:keys [frames]} (commanded-move {2000 {:bpm 480}})]
    (is (= [] (rate-violations frames))
        "No frame commands pan or tilt faster than a head's calibrated rate")
    (is (< (get-in (commanded-rates frames) [[:slow-motor :pan] :max]) 11.5)
        "The slow head keeps the 11.25 degrees per second of its launch-tempo interval")
    (is (= arrive-together (pan-arrivals frames))
        "Both heads arrive on the frame that ends the launch-tempo interval")
    (is (= pan-ends (update-vals (:words (peek frames)) :pan)))))

(deftest lowering-the-tempo-during-a-musical-synchronized-move-keeps-its-launch-interval
  (let [{:keys [frames]} (commanded-move {2000 {:bpm 60}})]
    (is (= [] (rate-violations frames)))
    (is (= arrive-together (pan-arrivals frames)))
    (is (= pan-ends (update-vals (:words (peek frames)) :pan)))))

(deftest tempo-changes-on-the-launch-and-arrival-frames-keep-calibrated-rates
  (let [unchanged (:frames (commanded-move {}))]
    (doseq [[label changes] {"raised in the launch cycle, after the launch" {0 {:bpm 480}}
                             "raised on the last frame before arrival" {3975 {:bpm 480}}
                             "raised on the arrival frame" {4000 {:bpm 480}}
                             "lowered on the arrival frame" {4000 {:bpm 30}}
                             "raised, lowered and raised again" {1000 {:bpm 480} 1500 {:bpm 40} 3000 {:bpm 1000}}}]
      (let [{:keys [frames]} (commanded-move changes)]
        (is (= [] (rate-violations frames)) label)
        (is (= arrive-together (pan-arrivals frames)) label)
        (is (= [] (frames-apart unchanged frames 1))
            (str label ": every frame commands the unchanged-tempo pan and tilt within one 16-bit step"))))))

(deftest meter-change-during-a-musical-synchronized-move-leaves-its-frames-unchanged
  (let [unchanged (:frames (commanded-move {}))
        {:keys [frames]} (commanded-move {2000 {:meter {:beats-per-bar 3 :bars-per-phrase 4}}})]
    (is (= [] (frames-apart unchanged frames)))
    (is (= arrive-together (pan-arrivals frames)))))

(deftest unchanged-tempo-synchronized-move-emits-the-beat-clock-bytes
  (let [{:keys [frames instance]} (commanded-move {})
        twin (commanded-move {} :twin? true)]
    (is (= {:beats 8 :ms 4000.0} (get-in instance [:state :tolglow.compiler/launch-tempo]))
        "The launched instance carries the interval its two bars took at launch")
    (is (nil? (get-in twin [:instance :state :tolglow.compiler/launch-tempo])))
    (is (= 181 (count frames) (count (:frames twin))))
    (is (= [] (frames-apart (:frames twin) frames))
        "Without a tempo change every captured packet equals the plain beat-clock tween")
    (is (= [] (rate-violations frames)))
    (is (= arrive-together (pan-arrivals frames)))
    (is (= {:far-travel 32767 :slow-motor 29127} (update-vals (:words (nth frames 80)) :pan))
        "The midpoint frame keeps the words master captured before the launch-tempo hold")))

(deftest stopping-the-transport-during-a-held-move-continues-on-its-launch-interval
  (let [unchanged (:frames (commanded-move {}))
        {:keys [frames]} (commanded-move {2000 {:playing? false}})]
    (is (= [] (rate-violations frames)))
    (is (= arrive-together (pan-arrivals frames))
        "A stateless sample cannot freeze and resume without a jump, so the held move keeps going")
    (is (= [] (frames-apart unchanged frames 1)))))

(deftest launching-a-held-move-while-the-transport-is-stopped-runs-it-at-once
  (let [unchanged (:frames (commanded-move {}))
        {:keys [frames instance]} (commanded-move {} :stopped? true)]
    (is (= {:beats 8 :ms 4000.0} (get-in instance [:state :tolglow.compiler/launch-tempo]))
        "A stopped transport keeps its tempo, so the launch holds two bars at 120 BPM")
    (is (= [] (rate-violations frames)))
    (is (= arrive-together (pan-arrivals frames))
        "The move does not wait for playback; it arrives on its launch interval")
    (is (= [] (frames-apart unchanged frames 1)))))

(deftest pausing-a-held-move-freezes-it-across-a-tempo-change
  (let [{:keys [frames]} (commanded-move {1000 :instance/pause 1500 {:bpm 480} 2000 :instance/resume} :until-ms 5500)
        pans (into {} (map (juxt :t #(update-vals (:words %) :pan))) frames)]
    (is (apply = (map pans (range 975 2001 25)))
        "The pause holds the pose of the last frame evaluated before it")
    (is (= [] (rate-violations frames)) "Resuming at the faster tempo keeps the launch pace")
    (is (= {:far-travel 5025 :slow-motor 5025} (pan-arrivals frames))
        "Arrival moves later by the 1025 ms the instance clock was held")))
