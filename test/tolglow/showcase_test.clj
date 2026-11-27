(ns tolglow.showcase-test
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.application :as app]
            [tolglow.compiler :as compiler]
            [tolglow.show :as show]
            [tolglow.showcase :as showcase]
            [tolglow.showcase.looks :as looks]
            [tolglow.showcase.rig :as rig]
            [tolglow.showcase.timeline :as timeline])
  (:import [java.security MessageDigest]
           [java.util HexFormat]))

(defn- digest
  "SHA-256 over every captured universe byte, in frame and universe order."
  [frames]
  (let [hash (MessageDigest/getInstance "SHA-256")]
    (doseq [{:keys [universes]} frames
            [universe data] universes]
      (.update hash (byte (bit-and universe 0xff)))
      (.update hash (byte-array (map unchecked-byte data))))
    (.formatHex (HexFormat/of) (.digest hash))))

(defn- saturation
  "Mean (max - min) / max of the lit heads' emitted colour. 0 is white."
  [rows ids]
  (let [lit (filter #(> (apply max %) 0.02) (keep (comp :emitted-color rows) ids))]
    (when (seq lit)
      (/ (reduce + (map #(let [high (apply max %)] (/ (- high (apply min %)) high)) lit)) (count lit)))))

(defn- probe [snapshot]
  {:running (into #{} (map :cue-id) (:instances snapshot))
   :saturation {:tubes (saturation (get-in snapshot [:output :values]) rig/tubes)
                :ribbon (saturation (get-in snapshot [:output :values]) rig/ribbon)}
   :ramps (into #{} (map (juxt (comp :param :destination) :to)) (get-in snapshot [:schedule :ramps]))
   :upcoming (count (get-in snapshot [:schedule :upcoming]))
   :faults (into [] (keep (fn [instance]
                            (when-let [fault (:fault instance)] [(:cue-id instance) (:message fault)])))
                 (:instances snapshot))
   :master (:master snapshot)
   :lit (count (filter (fn [[_ row]] (some pos? (:emitted-color row))) (get-in snapshot [:output :values])))
   :fog-level (get-in snapshot [:output :atmosphere :fog-level])})

(deftest the-rig-fits-the-room-preview-and-uses-three-universes
  (let [fixtures (rig/fixtures)
        by-group (group-by (comp first :groups) fixtures)]
    (is (= {:beam 6 :mover 4 :wash 4 :blinder 2 :hazer 1 :tubes 80 :ribbon 32}
           (update-vals by-group count)))
    (is (= 16 (count (remove #(or (:pixel-layout-id %) (get-in % [:channels :fog])) fixtures)))
        "The Room draws sixteen beams at once")
    (is (<= (count (remove #(get-in % [:channels :fog]) fixtures)) 128)
        "The Room projects at most 128 emitting targets")
    (is (= #{1 2 3} (set (map :universe fixtures))))
    (is (= (count fixtures) (count (distinct (map :id fixtures)))))
    (testing "truss heads hang inverted so their 0..180 degree tilt reaches the floor"
      (is (every? #(= [0 0 180] (:rotation-deg %)) (:beam by-group))))))

(deftest the-cue-grid-covers-every-category-with-ported-and-attached-source-cues
  (let [model (showcase/model)
        cues (get-in model [:registry :cues])
        new-with-sources (filter #(and (= :new (:origin %)) (contains? looks/sources (:id %))) looks/catalogue)]
    (is (= 24 (count cues)))
    (is (>= (count looks/catalogue) 12))
    (is (= #{:atmosphere :colour :intensity :movement :pixel} (set (map :category looks/catalogue))))
    (is (= 12 (count (filter #(= :ported (:origin %)) looks/catalogue))))
    (is (every? :legacy (filter #(= :ported (:origin %)) looks/catalogue)))
    (is (= #{:haze :wash-tide :wash-ember :beam-shafts :mover-orbit :riser-chase :aurora}
           (set (map :id new-with-sources))))
    (testing "every cue is on the grid and compiles against the rig"
      (is (= (set (keys cues)) (set (vals (get-in model [:pages :showcase :cells])))))
      (doseq [[id effect] (get-in model [:registry :effects])]
        (is (fn? (compiler/compile-effect effect)) (str id))))
    (testing "attached sources name real parameters or the layer priority"
      (doseq [[cue-id bindings] looks/sources
              param (keys bindings)]
        (is (or (= :layer/priority param) (contains? (get-in model [:registry :effects cue-id :params]) param))
            (str cue-id " " param))))
    (is (= #{:mixed-movement :heavy-colour :strobe-chase} (set (keys (:compositions model)))))
    (is (contains? (get-in model [:waveforms :waveforms]) :heartbeat))
    (is (contains? (get-in model [:waveforms :waveforms]) looks/bass-waveform) "the cones' audio LFO is configured")
    (is (empty? (:instances model)) "Loading the show launches nothing")))

(deftest timeline-positions-are-zero-based-and-validated-against-the-show
  (let [model (showcase/model)]
    (is (= 0.0 (timeline/position->beats {:bar 0} showcase/meter)))
    (is (= 78.0 (timeline/position->beats {:bar 19 :beat 2} showcase/meter)))
    (is (= 34.0 (timeline/position->beats {:phrase 2 :beat 2} showcase/meter)))
    (is (empty? (timeline/validate showcase/timeline model)))
    (is (= 128.0 (:length (timeline/prepare showcase/timeline model))))
    (doseq [[event message]
            [[{:at {:bar 0} :op :cue/launch :cue-id :missing} #"not registered"]
             [{:at {:bar 0} :op :param/set :cue-id :haze :param :level :value 2.0} #"does not fit"]
             [{:at {:bar 0} :op :param/ramp :cue-id :haze :param :level :to 0.5 :over {:ms 500}} #"positive beats"]
             [{:at {:bar 0} :op :macro/run :macro-id :missing} #"not saved"]
             [{:at {:bar -1} :op :cue/launch :cue-id :haze} #"zero-based"]
             [{:at {:bar 0} :op :cue/dance :cue-id :haze} #"Unknown timeline action"]]]
      (let [problems (timeline/validate (assoc showcase/timeline :events [event]) model)]
        (is (= [[:events 0]] (mapv :path problems)))
        (is (re-find message (:message (first problems))))))))

(def ^:private probe-timeline
  {:id :probe :bpm 120 :meter showcase/meter :length {:bar 2}
   :events [{:at {:beat 1} :op :cue/launch :cue-id :haze :launch {:params {:level 0.2}}
             :sources {:level {:type :lfo :waveform :sine :period {:beats 2} :from 0.0 :to 0.5}}}
            {:at {:beat 2} :op :param/ramp :cue-id :haze :param :level :to 1.0 :over {:beats 2} :curve :ease-in}
            {:at {:beat 2} :op :cue/launch :cue-id :rolling-volume :launch {:targets rig/tubes}}
            {:at {:beat 5} :op :master/set :value 0.5}
            {:at {:beat 6} :op :instance/end :cue-id :haze}
            {:at {:beat 6} :op :instance/end :cue-id :rolling-volume}]})

(deftest scheduler-events-are-posted-at-absolute-beats-in-authored-order
  (let [prepared (timeline/prepare probe-timeline (showcase/model))
        [post :as posts] (timeline/posts prepared 8.0 2)]
    (is (= 1 (count posts)))
    (is (= {:op :schedule/post :command-id "timeline/probe/2/post/0"
            :source {:adapter :timeline :session-id "probe"}}
           (dissoc post :events)))
    (is (= [{:id "probe/2/0" :label "Launch haze" :op :cue/launch :at {:beat 9.0} :cue-id :haze
             :launch {:params {:level 0.2}}}
            {:id "probe/2/1" :label "Ramp haze level" :op :param/ramp :at {:beat 10.0} :cue-id :haze
             :param :level :to 1.0 :over {:beats 2} :curve :ease-in}
            {:id "probe/2/3" :label "Master" :op :master/set :at {:beat 13.0} :value 0.5}
            {:id "probe/2/4" :label "End haze" :op :instance/end :at {:beat 14.0} :cue-id :haze}]
           (:events post)))
    (testing "what the scheduler cannot run stays a direct command"
      (is (= [[1.0 :modulation/bind :haze] [2.0 :cue/launch :rolling-volume] [6.0 :instance/end :rolling-volume]]
             (mapv (juxt :beat :op :cue-id) (:direct prepared)))))
    (testing "a long timeline is split into batches the scheduler admits"
      (let [many (assoc probe-timeline :events (vec (repeat 130 {:at {:beat 1} :op :master/set :value 1.0})))]
        (is (= [64 64 2] (mapv (comp count :events) (timeline/posts (timeline/prepare many (showcase/model)) 0.0 0))))))))

(deftest direct-commands-fire-once-and-binds-wait-for-the-launched-instance
  (let [model (showcase/model)
        prepared (timeline/prepare probe-timeline model)
        instance (fn [id cue started] {:id id :cue-id cue :order 0 :phase :running :started-at-beat started})
        stale (assoc-in model [:instances "0:1"] (instance "0:1" :haze 8.5))
        fresh (assoc-in model [:instances "0:2"] (instance "0:2" :haze 9.02))
        both (assoc-in fresh [:instances "0:3"] (assoc (instance "0:3" :rolling-volume 10.0) :order 1))
        step (fn [state model beat] (timeline/advance prepared state model beat))
        before (step (timeline/start 8.0 0) model 8.5)
        waiting (step (:state before) stale 9.0)
        bound (step (:state waiting) fresh 9.05)
        launched (step (:state bound) fresh 10.0)
        ended (step (:state launched) both 14.0)
        over (step (:state ended) model 16.0)]
    (is (empty? (:operations before)))
    (is (empty? (:operations waiting)) "An instance from before the launch beat is not the one to bind")
    (is (= 1 (count (get-in waiting [:state :waiting]))))
    (is (= [[:modulation-bind {:instance-id "0:2" :param :level
                               :binding {:type :lfo :waveform :sine :period {:beats 2} :from 0.0 :to 0.5}}]]
           (:operations bound)))
    (is (= [[:command {:op :cue/launch :cue-id :rolling-volume :launch {:targets rig/tubes}
                       :command-id "timeline/probe/0/1" :source {:adapter :timeline :session-id "probe"}}]]
           (:operations launched)))
    (is (= [[:command {:op :instance/end :instance-id "0:3" :command-id "timeline/probe/0/2"
                       :source {:adapter :timeline :session-id "probe"}}]]
           (:operations ended)))
    (is (false? (get-in ended [:state :done?])))
    (is (true? (get-in over [:state :done?])))
    (is (empty? (:operations (step (:state over) model 17.0))) "A finished pass sends nothing more")
    (testing "the same model and beat give the same operations"
      (is (= bound (step (:state waiting) fresh 9.05))))
    (testing "a bind whose cue never starts is reported after its patience"
      (let [stuck (reduce (fn [result beat] (step (:state result) model beat))
                          waiting (map #(+ 9.0 (* 0.01 %)) (range 1 20)))]
        (is (= [{:index 0 :op :modulation/bind :cue-id :haze :param :level}] (get-in stuck [:state :missed])))))))

(deftest replaying-the-timeline-twice-gives-byte-identical-dmx
  (let [first-run (showcase/replay {:snapshot! probe})
        second-run (showcase/replay)
        frames (:dmx first-run)
        probes (:snapshots first-run)
        bar-frame (fn [bar] (nth probes (long (Math/round (/ (* bar 1875.0) 25)))))]
    (testing "the same seed"
      (is (= 2402 (count frames) (count (:dmx second-run))))
      (is (= (digest frames) (digest (:dmx second-run))))
      (is (= frames (:dmx second-run)))
      (is (= [1 2 3] (vec (keys (:universes (first frames))))))
      (is (every? #(= [512 512 512] (mapv count (vals (:universes %)))) frames)))
    (testing "the whole timeline went through the command path cleanly"
      (let [status (:status first-run)
            prepared (timeline/prepare showcase/timeline (showcase/model))]
        (is (:done? status))
        (is (= (+ (count (timeline/posts prepared 0.0 0)) (count (:direct prepared))) (:accepted status))
            "Two scheduler posts and the direct commands, with no command per frame")
        (is (empty? (:rejected status)))
        (is (empty? (:missed status)))
        (is (empty? (:outcomes status)) "Every scheduled event fired and every ramp completed")
        (is (zero? (:pending status))))
      (testing "the owner runs the ramps"
        (is (= 64 (:upcoming (first probes))) "The three opening launches fired on frame 0")
        (is (zero? (:upcoming (peek probes))))
        (is (= #{[:rate 8.0] [:level 1.0] [:saturation 0.95]} (:ramps (bar-frame 18.5))))
        (is (empty? (:ramps (bar-frame 23)))))
      (is (empty? (distinct (mapcat :faults probes))) "No cue faulted on any frame"))
    (testing "every cue on the grid plays at some point, the tolglow-web cues in after-hours"
      (is (= (set (keys (get-in (showcase/model) [:registry :cues])))
             (reduce into looks/after-hours-cues (map :running probes)))))
    (testing "the recorded moments are in the output"
      (is (= #{:haze :aurora :wash-tide} (:running (bar-frame 1))))
      (is (contains? (:running (bar-frame 13)) :can-can))
      (is (contains? (:running (bar-frame 17)) :rolling-volume))
      (is (zero? (:master (bar-frame 19.75))) "Half a bar of darkness before the drop")
      (is (zero? (:lit (bar-frame 19.75))))
      (is (== 1.0 (:master (bar-frame 20.1))))
      (is (contains? (:running (bar-frame 20.1)) :blinder-hit))
      (is (contains? (:running (bar-frame 24.1)) :blinder-hit) "The phrase subscriber fires the second hit")
      (is (> (:lit (bar-frame 21)) 100))
      (is (<= (reduce max (map :lit probes)) 128))
      (is (empty? (:running (peek probes))))
      (is (every? #(< 0.05 (:fog-level %) 0.4) probes) "The room stays hazed without filling up"))
    (testing "the drop's pixels stay saturated rather than washing out to white"
      (let [drop (map bar-frame (range 20.5 23.5 0.25))
            mean (fn [k] (/ (reduce + (map #(get-in % [:saturation k]) drop)) (count drop)))]
        (is (< 0.75 (mean :tubes)))
        (is (< 0.6 (mean :ribbon)))))
    (testing "a different seed changes the seeded looks"
      (let [bars {:from-bar 2 :to-bar 6}]
        (is (not= (digest (:dmx (showcase/replay (assoc bars :seed 7))))
                  (digest (:dmx (showcase/replay bars)))))))))

(deftest a-looping-player-posts-each-pass-ahead-and-stop-withdraws-the-rest
  (let [short {:id :loop-probe :bpm 128 :meter showcase/meter :length {:bar 2}
               :events [{:at {:bar 0} :op :cue/launch :cue-id :haze :sources (:haze looks/sources)}
                        {:at {:bar 0} :op :param/ramp :cue-id :haze :param :level :to 0.9 :over {:beats 2}}
                        {:at {:bar 1} :op :instance/end :cue-id :haze}]}
        runtime (show/create-runtime {:model (showcase/model) :workspace-options nil :worker-timeout-ms 60000})
        launches (atom #{})]
    (try
      (let [player (showcase/play! runtime {:timeline short :loop? true :origin 0.0})]
        (is (identical? player (showcase/play! runtime {:timeline short})) "A second play keeps the first player")
        (doseq [t (range 0 9500 25)]
          (show/tick! runtime t)
          (swap! launches into (keep (fn [instance] (when (= :haze (:cue-id instance)) (:id instance))))
                 (vals (:instances (:model @(:state runtime))))))
        (let [status (showcase/status player)]
          (is (= 2 (:pass status)) "Five bars in, the third two-bar pass is playing")
          (is (= 3 (count @launches)))
          (is (empty? (:rejected status)))
          (is (empty? (:missed status)))
          (is (empty? (:outcomes status))))
        (is (seq (:upcoming (show/schedule runtime))) "The current pass still has events ahead")
        (showcase/stop! runtime player)
        (doseq [t (range 9500 12000 25)] (show/tick! runtime t))
        (is (empty? (:upcoming (show/schedule runtime))))
        (is (empty? (:instances (:model @(:state runtime))))))
      (finally (show/stop! runtime)))))

(deftest the-documented-config-starts-a-show-that-plays-its-timeline
  (let [config (-> showcase/config
                   (assoc :web-server false)
                   (update :show merge {:recovery false :workspace-options nil}))
        runtime (app/start! config)]
    (try
      (is (= [:show :showcase-timeline] (get-in runtime [:application :loaded-components])))
      (let [player (get-in runtime [:application :modules :showcase-timeline :player])
            deadline (+ (System/currentTimeMillis) 20000)]
        (while (and (< (System/currentTimeMillis) deadline)
                    (< (:accepted (showcase/status player)) 7))
          (Thread/sleep 50))
        (let [status (showcase/status player)
              snapshot (show/snapshot runtime)]
          (is (>= (:accepted status) 7) "Two posts and the five opening binds")
          (is (empty? (:outcomes status)))
          (is (empty? (:rejected status)))
          (is (= :first-light (:timeline status)))
          (is (= {:type :preview} (get-in snapshot [:status :output])))
          (is (= #{:haze :aurora :wash-tide} (into #{} (map :cue-id) (:instances snapshot))))
          (is (empty? (keep :fault (:instances snapshot))))
          (is (= 3 (count (get-in snapshot [:output :universes]))))))
      (finally (app/stop! runtime)))))
