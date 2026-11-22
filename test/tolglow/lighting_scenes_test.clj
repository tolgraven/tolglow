(ns tolglow.lighting-scenes-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.demo :as demo]
            [tolglow.event-subscribers :as subs]
            [tolglow.lighting-scenes :as scenes]
            [tolglow.persistence :as persistence]
            [tolglow.integrations.analysis :as analysis]
            [tolglow.show :as show]))

(def fixtures
  [{:id :left :groups [:wash] :universe 1 :address 1 :max-watts 200 :channels {:red 1 :green 2 :blue 3}}
   {:id :right :groups [:wash] :universe 1 :address 4 :measured-relative-brightness 0.5 :channels {:dimmer 1}}
   {:id :other :groups [:foh] :universe 1 :address 5 :channels {:dimmer 1}}
   {:id :pump :groups [:wash] :universe 1 :address 6 :channels {:fog 1}}])

(defn local [batch?]
  (let [model (-> (demo/model)
                  (assoc-in [:rig :fixtures] fixtures)
                  (assoc-in [:variables :energy] 0.72)
                  (assoc-in [:registry :effects :ember :targets] [:left])
                  (assoc-in [:registry :effects :ember :outputs] {:color (demo/constant [1 0 0])})
                  (assoc-in [:registry :effects :manual] {:id :manual :version 1 :targets [:other] :params {}
                                                           :outputs {:dimmer (demo/constant 0.35)}})
                  (assoc-in [:registry :cues :manual] {:id :manual :effect-id :manual :version 1}))]
    (show/create-runtime {:model model :batch? batch?})))

(defn issue! [rt time type payload]
  (let [ack (show/submit! rt type payload)] (show/tick! rt (swap! time + 25)) @ack))
(defn ticks! [rt time n] (dotimes [_ n] (show/tick! rt (swap! time + 25))))
(defn group! [rt g] (some #(when (= g (:group-id %)) %) (get-in (show/snapshot rt) [:lighting-scenes :groups])))
(defn byte-at [rt i] (bit-and 255 (aget ^bytes (get-in @(:state rt) [:output :universes 1]) i)))

(deftest generated-and-borrowed-looks-with-independent-groups
  (doseq [batch? [true false]]
    (let [rt (local batch?) clock (atom 0) issue #(issue! rt clock %1 %2)
          style #(issue :lighting-scene %)]
      (try
        (is (= [:left :right :other] (get-in (show/snapshot rt) [:lighting-scenes :eligible-fixture-ids])))
        (is (= :accepted (:status (issue :command {:op :cue/launch :cue-id :manual}))))
        (is (= :accepted (:status (style {:op :apply :group-id :wash :target 0.72 :effect-ids [:ember]}))))
        (is (= :ember (:source-effect-id (group! rt :wash))))
        (ticks! rt clock 40)
        (let [g (group! rt :wash)]
          (is (= :at-target (:status g)) (pr-str g))
          (is (< (abs (- (:actual g) 0.72)) 0.016))
          (is (= [:left :right] (:fixture-ids g)))
          (is (nil? (:source-effect-id g)) "Saturated red cannot reach 72%; the owned emitter fill can"))
        (is (= 89 (byte-at rt 4)) "Manual light on another fixture was not changed")
        (is (= :rejected (:status (style {:op :apply :group-id :overlap :fixture-ids [:right] :target 1}))))
        (is (= :rejected (:status (style {:op :apply :group-id :pump :fixture-ids [:pump] :target 1}))))
        (is (= :accepted (:status (style {:op :apply :group-id :foh :target 0.1 :effect-ids []}))))
        (ticks! rt clock 40)
        (is (= :over-target (:status (group! rt :foh))) "Higher-priority manual intensity remains visible")
        (is (= 89 (byte-at rt 4)))
        (is (= :accepted (:status (style {:op :vary :group-id :wash :target 0.72}))))
        (is (= 2 (:generation (group! rt :wash))))
        (is (= :accepted (:status (style {:op :release :group-id :wash}))))
        (is (nil? (group! rt :wash)))
        (is (= 89 (byte-at rt 4)))
        (is (= 0 (byte-at rt 5)))
        (finally (show/stop! rt))))))

(deftest target-from-vars-and-event-fields
  (let [rt (local false) clock (atom 0) issue #(issue! rt clock %1 %2)
        action {:op :lighting/scene :scene {:op :apply :group-id :wash :effect-ids []}
                :event-path [:data :intensity]}]
    (try
      (is (= action (subs/validate-action action)))
      (is (= :accepted (:status (issue :lighting-scene {:op :apply :group-id :wash :target-var [:energy] :effect-ids []}))))
      (ticks! rt clock 20)
      (is (= 0.72 (:target (group! rt :wash))))
      (is (= :at-target (:status (group! rt :wash))))
      (let [restored (persistence/thaw (demo/model) (show/export-show rt))
            g (:wash (scenes/groups restored))]
        (is (some? g))
        (is (not= (:instance-id (group! rt :wash)) (:instance-id g)))
        (is (= {:adapter :lighting-scene :group-id :wash} (get-in restored [:instances (:instance-id g) :source]))))
      (let [m (-> (:model @(:state rt))
                  (assoc-in [:event-subscribers :runs]
                            [{:id 1 :subscriber :test :steps [(assoc-in action [:scene :group-id] :foh)]
                              :index 0 :due-beat 0 :event {:data {:intensity 0.6}}}]))
            changed (subs/advance m @clock {})]
        (is (= 0.6 (:target (:foh (scenes/groups changed))))))
      (is (= :accepted (:status (issue :command {:op :master/set :value 0.1}))))
      (ticks! rt clock 40)
      (is (= :limited (:status (group! rt :wash))))
      (is (< (:actual (group! rt :wash)) 0.11))
      (finally (show/stop! rt)))))

(deftest real-pcm-crossings-vary-a-scene-without-a-manual-launch
  (let [rt (local true) now (atom 0) callbacks (show/audio-callbacks rt)
        analyzer (atom (analysis/new-analyzer {:energy-event-ms 50}))
        offset (atom 0)
        action {:op :lighting/scene :scene {:op :vary :group-id :wash :target 0.72 :effect-ids []}
                :gate {:path [:data :energy-envelope] :above 0.2 :below 0.08}}
        send-audio (fn [amplitude]
                     (let [t @offset result (analysis/analyze @analyzer
                             {:samples (double-array (map #(* amplitude (Math/sin (* % (/ (* 2 Math/PI 100) 48000)))) (range 48000)))
                              :sample-rate 48000.0 :started-at-ms t :received-at-ms (+ t 1000.0)})]
                       (swap! offset + 1000) (reset! analyzer (:state result))
                       ((:features! callbacks) (:features result))
                       ((:events! callbacks) (:events result))
                       (ticks! rt now 24)))]
    (try
      (is (= :accepted (:status (issue! rt now :event-subscriber
                   {:op :save :subscriber {:id :energy-lift :match {:type :audio/energy} :actions [action]}}))))
      (send-audio 0.0)
      (is (nil? (group! rt :wash)))
      (send-audio 0.9)
      (is (= 1 (:generation (group! rt :wash))))
      (is (= :at-target (:status (group! rt :wash))))
      (send-audio 0.9)
      (is (= 1 (:generation (group! rt :wash))))
      (dotimes [_ 3] (send-audio 0.0))
      (send-audio 0.9)
      (is (= 2 (:generation (group! rt :wash))))
      (finally (show/stop! rt)))))

(deftest repeated-agent-requests-and-patch-group-identities
  (let [m (assoc-in (demo/model) [:rig :fixtures] fixtures)
        request {:op :vary :group-id "wash" :target 0.72 :effect-ids [] :command-id "turn-1"}
        first-model (scenes/apply-request m request 1)
        repeated (scenes/apply-request first-model request 2)]
    (is (= [:wash] (vec (keys (scenes/groups repeated)))))
    (is (identical? first-model repeated))
    (is (thrown? clojure.lang.ExceptionInfo
                 (persistence/validate-model (assoc-in repeated [:extensions :tolglow.lighting-scenes/control :groups :wash :gain] -1))))
    (is (thrown? clojure.lang.ExceptionInfo (scenes/apply-request repeated (assoc request :target 0.4) 2)))
    (is (= 2 (get-in (scenes/groups (scenes/apply-request repeated (assoc request :command-id "turn-2") 3))
                      [:wash :generation])))))
