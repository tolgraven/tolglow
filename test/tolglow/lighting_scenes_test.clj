(ns tolglow.lighting-scenes-test
  (:require [afterglow.render.frame :as frame]
            [clojure.test :refer [deftest is]]
            [tolglow.demo :as demo]
            [tolglow.compiler.batch :as batch]
            [tolglow.event-subscribers :as subs]
            [tolglow.lighting-scenes :as scenes]
            [tolglow.lighting-levels :as levels]
            [afterglow.runtime.events :as staging]
            [tolglow.persistence :as persistence]
            [tolglow.integrations.analysis :as analysis]
            [tolglow.show :as show]))

(def fixtures
  [{:id :left :groups [:wash] :universe 1 :address 1 :max-watts 200 :channels {:red 1 :green 2 :blue 3}}
   {:id :right :groups [:wash] :universe 1 :address 4 :measured-relative-brightness 0.5 :channels {:dimmer 1}}
   {:id :other :groups [:foh] :universe 1 :address 5 :channels {:dimmer 1}}
   {:id :pump :groups [:wash] :universe 1 :address 6 :channels {:fog 1}}])

(defn local
  ([batch?] (local batch? {}))
  ([batch? options]
   (let [model (-> (demo/model)
                  (assoc-in [:rig :fixtures] fixtures)
                  (assoc-in [:variables :energy] 0.72)
                  (assoc-in [:registry :effects :ember :targets] [:left])
                  (assoc-in [:registry :effects :ember :outputs] {:color (demo/constant [1 0 0])})
                  (assoc-in [:registry :effects :manual] {:id :manual :version 1 :targets [:other] :params {}
                                                           :outputs {:dimmer (demo/constant 0.35)}})
                  (assoc-in [:registry :cues :manual] {:id :manual :effect-id :manual :version 1}))]
    (show/create-runtime (assoc options :model model :batch? batch?)))))

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
          (is (nil? (:source-effect-id g)) "A saturated red source needs emitter fill to reach this target"))
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
                :gate {:path [:data :audio/energy] :above 0.2 :below 0.08}}
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
      (let [saved (persistence/read-string (show/export-show-source rt))
            restored (persistence/thaw (demo/model) saved)
            subscriber (first (get-in restored [:event-subscribers :subscribers]))]
        (is (= :audio/energy (get-in subscriber [:match :type])))
        (is (= [:data :audio/energy] (get-in subscriber [:actions 0 :gate :path])))
        (is (pos? (get-in restored [:variables :audio/energy]))))
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

(deftest committed-meter-cache-invalidates-by-rig-membership-and-universe-bytes
  (let [fixtures [{:id :a :universe 1 :address 512 :channels {:dimmer [1 2]}}
                  {:id :b :universe 9 :address 1 :channels {:dimmer 1}}]
        patch (frame/compile-patch fixtures)
        groups {:a {:fixture-ids [:a]} :b {:fixture-ids [:b]}}
        full #(into {} (map (fn [u] [u (byte-array 512)]) [1 2 9]))
        one (full) two (full)
        a (scenes/meter-frame {} patch groups one)
        b (scenes/meter-frame a patch groups two)
        changed (full)
        _ (aset-byte ^bytes (changed 2) 0 (unchecked-byte 255))
        c (scenes/meter-frame b patch groups changed)
        d (scenes/meter-frame c patch {:a {:fixture-ids [:b]}} changed)
        repatched (frame/compile-patch [(assoc (first fixtures) :measured-relative-brightness 2) (second fixtures)])
        e (scenes/meter-frame c repatched groups changed)]
    (is (identical? (get-in a [:plans :a]) (get-in b [:plans :a])) "Fresh equal buffers reuse the last measurement")
    (is (identical? (get-in b [:plans :b]) (get-in c [:plans :b])) "Another group's fine universe did not change this group")
    (is (< 0 (get-in c [:plans :a :actual])) "The fine byte alone changes the measured dimmer")
    (is (zero? (get-in d [:plans :a :actual])) "Reassigning membership invalidates a byte-identical frame")
    (is (not (identical? (get-in c [:plans :a :plan]) (get-in e [:plans :a :plan]))) "Calibration rebuilds the plan")
    (is (= #{:a} (set (keys (:plans d)))))))

(deftest stable-feedback-preserves-the-model-and-instance
  (let [rt (local true) t (atom 0)]
    (try
      (issue! rt t :lighting-scene {:op :apply :group-id :wash :target 0.72 :effect-ids []})
      (ticks! rt t 40)
      (let [m (:model @(:state rt))
            g (:wash (scenes/groups m))
            value (:actual g)
            next-model (scenes/feedback m {:wash value} @t false)]
        (is (= :at-target (:status g)))
        (is (identical? m next-model))
        (is (identical? (:instances m) (:instances next-model)))
        (is (= :limited (:status (:wash (scenes/groups (scenes/feedback m {:wash value} @t true))))))
        (is (not (identical? next-model (scenes/feedback m {:wash 0.1} @t false)))))
      (finally (show/stop! rt)))))

(deftest reaching-a-measured-target-launches-a-subscriber-and-publishes-vars
  (let [captured (atom [])
        rt (local true {:event-admit (fn [m batch]
                                       (swap! captured into (filter #(= :lighting (:category %)) batch))
                                       (subs/admit m batch))})
        t (atom 0)
        act #(issue! rt t %1 %2)
        model #(get @(:state rt) :model)
        variable (fn [& path] (get-in (model) (into [:variables] path)))]
    (try
      (is (= :accepted (:status (act :event-subscriber
             {:op :save :subscriber {:id :on-wash-target :match {:type :lighting/group-at-target :emitter-id :wash}
                                     :actions [{:op :lighting/scene :scene {:op :vary :group-id :foh :target 0.4 :effect-ids []}}]}}))))
      (is (= :accepted (:status (act :settings {:scope :lighting-capacity :values {:enabled? true}}))))
      (act :lighting-scene {:op :apply :group-id :wash :target 0.72 :effect-ids []})
      (ticks! rt t 50)
      (is (= 1 (:generation (group! rt :foh))))
      (is (= {:actual (:actual (group! rt :wash)) :target 0.72 :status :at-target} (variable :lighting/groups :wash)))
      (is (= (get-in (show/snapshot rt) [:output :lighting-capacity :utilization]) (variable :lighting/rig :actual)))
      (is (= :manual (variable :lighting/rig :status)))
      (let [m (staging/begin (model))
            meter (get-in @(:state rt) [:output :lighting-capacity])
            stable (levels/publish m (scenes/groups m) meter)]
        (is (identical? m stable))
        (is (empty? (staging/pending stable))))
      (ticks! rt t 30)
      (is (= 1 (:generation (group! rt :foh))) "Holding the measured target never retriggers its subscriber")
      (act :command {:op :master/set :value 0.01})
      (ticks! rt t 20)
      (is (= :limited (variable :lighting/groups :wash :status)))
      (act :lighting-scene {:op :release :group-id :wash})
      (is (nil? (variable :lighting/groups :wash)))
      (act :settings {:scope :lighting-capacity :values {:enabled? false}})
      (is (nil? (variable :lighting/rig)))
      (let [types (set (map :type @captured))
            reached (filter #(and (= :lighting/group-at-target (:type %)) (= :wash (get-in % [:emitter :id]))) @captured)]
        (is (= 1 (count reached)))
        (is (= 0.72 (get-in (first reached) [:data :target])))
        (is (every? types [:lighting/group-limited :lighting/group-released :lighting/rig-level :lighting/rig-unavailable])))
      (finally (show/stop! rt)))))

(deftest calibrated-scenes-match-unlike-fixtures-on-the-whole-rig-scale
  (doseq [batch? [true false]]
    (let [source {:id :tint :version 1 :targets [:a :b]
                  :params {:hue {:type :color :default [1.0 0.5 0.0]}}
                  :outputs {:color {:op :param :key :hue}}}
          movers (mapv (fn [id address weight] {:id id :universe 1 :address address :groups [:wash]
                                                :channels {:red 1 :green 2 :blue 3}
                                                :measured-relative-brightness weight})
                       [:a :b] [1 4] [4.0 1.0])
          rig {:fixtures (conj movers {:id :spot :groups [:others] :universe 1 :address 7
                                       :channels {:dimmer 1} :measured-relative-brightness 8.0})}
          model (-> (demo/model) (assoc :rig rig) (assoc-in [:registry :effects :tint] source))
          rt (show/create-runtime {:model model :batch? batch?})
          time (atom 0)
          edit (fn [opts] (issue! rt time :lighting-scene (merge {:op :apply :group-id :wash} opts)))
          current #(group! rt :wash)
          signal #(mapv :global-brightness (:sources (current)))
          within? (fn [wanted got] (< (abs (- wanted got)) 0.004))]
      (try
        (is (= :accepted (:status (edit {:effect-ids [:tint] :brightness-reference :global :target 0.04}))))
        (ticks! rt time 5)
        (let [g (current)
              [strong weak] (:sources g)
              dmx (mapv #(bit-and 255 %) (take 7 (get-in @(:state rt) [:output :universes 1])))]
          (is (= :at-target (:status g)))
          (is (every? #(within? 0.04 %) (signal)))
          (is (= [0.5 0.125] (mapv :global-maximum [strong weak])))
          (is (< 3.8 (/ (:local-brightness weak) (:local-brightness strong)) 4.2))
          (is (every? false? (map :limited? (:sources g))))
          (is (within? (:global-brightness g) (:actual g)))
          (is (= [0 0 0] [(dmx 2) (dmx 5) (dmx 6)]))
          (is (<= (abs (- (dmx 0) (* 2 (dmx 1)))) 1))
          (is (<= (abs (- (dmx 3) (* 2 (dmx 4)))) 1)))
        (edit {:target 0.1})
        (ticks! rt time 12)
        (is (= [false true] (mapv :limited? (:sources (current)))) "A saturated hue runs out of headroom before full white")
        (is (= :limited (:status (current))))
        (let [id (:instance-id (current))
              m (:model @(:state rt))]
          (is (= :global (get-in (persistence/validate-model m) [:extensions ::scenes/control :groups :wash :brightness-reference])))
          (is (= :accepted (:status (issue! rt time :command {:op :param/set :instance-id id :param :hue :value [0.0 0.0 0.0]}))))
          (ticks! rt time 12)
          (is (= [0.0 0.0] (signal)) "Matching a deliberately dark cue cannot turn the fixtures white")
          (is (= id (:instance-id (current)))))
        (edit {:brightness-reference :local :target 0.05})
        (ticks! rt time 20)
        (is (= :local (:brightness-reference (current))))
        (is (= :rejected (:status (edit {:brightness-reference :wrong}))))
        (finally (show/stop! rt))))))

(deftest scene-variation-picks-a-source-that-fits-the-concurrent-budget
  (let [lamps (mapv (fn [id address watts]
                      {:id id :universe 1 :address address :channels {:dimmer 1} :max-watts watts :groups [:pair]})
                    [:a :b] [1 2] [80 20])
        source (fn [id targets] {:id id :version 1 :targets targets :params {} :outputs {:dimmer 1.0}})
        m (-> (demo/model)
              (assoc :rig {:fixtures lamps})
              (assoc-in [:registry :effects :a-wide] (source :a-wide [:a :b]))
              (assoc-in [:registry :effects :z-small] (source :z-small [:b]))
              (assoc-in [:settings :lighting-budget] {:enabled? true :global {:limit 1.0 :max-active 1}}))
        rt (show/create-runtime {:model m})
        t (atom 0)]
    (try
      (is (= :accepted (:status (issue! rt t :lighting-scene {:op :vary :group-id :pair :target 0.1
                                                              :effect-ids [:a-wide :z-small]}))))
      (is (= :z-small (:source-effect-id (group! rt :pair))))
      (finally (show/stop! rt)))))


(deftest generated-global-tints-share-the-normal-typed-renderer
  (let [capture (fn [batch?]
                  (let [rt (local batch?) t (atom 0)]
                    (try
                      (issue! rt t :lighting-scene {:op :apply :group-id :wash :brightness-reference :global
                                                    :effect-ids [] :target 0.12})
                      (ticks! rt t 8)
                      (let [m (:model @(:state rt))
                            group (group! rt :wash)
                            definition (get-in m [:instances (:instance-id group) :definition])]
                        (is (< (abs (- (:actual group) 0.12)) 0.004))
                        (is (= :batch (:backend (meta (batch/compile-effect definition fixtures)))))
                        (mapv #(bit-and 255 %) (take 6 (get-in @(:state rt) [:output :universes 1]))))
                      (finally (show/stop! rt)))))]
    (is (= (capture false) (capture true)))))
