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

(defn- probe [snapshot]
  {:running (into #{} (map :cue-id) (:instances snapshot))
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
    (is (= 20 (count cues)))
    (is (>= (count looks/catalogue) 12))
    (is (= #{:atmosphere :colour :intensity :movement :pixel} (set (map :category looks/catalogue))))
    (is (= 9 (count (filter #(= :ported (:origin %)) looks/catalogue))))
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

(deftest the-player-fires-each-event-once-and-waits-for-instances
  (let [model (showcase/model)
        source {:id :probe :bpm 120 :meter showcase/meter :length {:bar 2}
                :events [{:at {:beat 1} :op :cue/launch :cue-id :haze
                          :sources {:level {:type :lfo :waveform :sine :period {:beats 2} :from 0.0 :to 0.5}}}
                         {:at {:beat 2} :op :param/ramp :cue-id :haze :param :level :to 1.0 :over {:beats 2}}
                         {:at {:beat 6} :op :instance/end :cue-id :haze}]}
        prepared (timeline/prepare source model)
        running (assoc-in model [:instances "0:1"]
                          {:id "0:1" :cue-id :haze :order 0 :phase :running :params {:level 0.2}})
        step (fn [state model beat] (timeline/advance prepared state model beat))
        before (step (timeline/start 0.0 0) model 0.5)
        launched (step (:state before) model 1.0)
        bound (step (:state launched) running 1.5)
        ramping (step (:state bound) running 3.0)
        finished (step (:state ramping) running 4.0)
        ended (step (:state finished) running 6.0)
        over (step (:state ended) model 8.0)]
    (is (empty? (:operations before)))
    (is (= [[:command {:op :cue/launch :cue-id :haze :launch {} :command-id "timeline/probe/0/0"
                       :source {:adapter :timeline :session-id "probe"}}]]
           (:operations launched)))
    (is (= 1 (count (get-in launched [:state :waiting]))) "The bind waits for the new instance")
    (is (= [[:modulation-bind {:instance-id "0:1" :param :level
                               :binding {:type :lfo :waveform :sine :period {:beats 2} :from 0.0 :to 0.5}}]]
           (:operations bound)))
    (is (= [[:param/set "0:1" 0.6]] (mapv (fn [[_ c]] [(:op c) (:instance-id c) (:value c)]) (:operations ramping))))
    (is (= [[:param/set "0:1" 1.0]] (mapv (fn [[_ c]] [(:op c) (:instance-id c) (:value c)]) (:operations finished))))
    (is (empty? (get-in finished [:state :ramps])))
    (is (= [:instance/end] (mapv (comp :op second) (:operations ended))))
    (is (false? (get-in ended [:state :done?])))
    (is (true? (get-in over [:state :done?])))
    (is (empty? (:operations (step (:state over) model 9.0))) "A finished pass sends nothing more")
    (testing "the same model and beat give the same operations"
      (is (= ramping (step (:state bound) running 3.0))))
    (testing "an action whose cue never starts is reported after its patience"
      (let [stuck (reduce (fn [result beat] (step (:state result) model beat))
                          launched (map #(+ 1.0 (* 0.01 %)) (range 1 20)))]
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
      (let [status (:status first-run)]
        (is (:done? status))
        (is (pos? (:accepted status)))
        (is (= (:sent status) (:accepted status)))
        (is (empty? (:rejected status)))
        (is (empty? (:missed status)))
        (is (zero? (:pending status))))
      (is (empty? (distinct (mapcat :faults probes))) "No cue faulted on any frame"))
    (testing "every cue on the grid plays at some point"
      (is (= (set (keys (get-in (showcase/model) [:registry :cues])))
             (reduce into #{} (map :running probes)))))
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
    (testing "a different seed changes the seeded looks"
      (let [bars {:from-bar 2 :to-bar 6}]
        (is (not= (digest (:dmx (showcase/replay (assoc bars :seed 7))))
                  (digest (:dmx (showcase/replay bars)))))))))

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
                    (< (:accepted (showcase/status player)) 5))
          (Thread/sleep 50))
        (let [status (showcase/status player)
              snapshot (show/snapshot runtime)]
          (is (>= (:accepted status) 5))
          (is (empty? (:rejected status)))
          (is (= :first-light (:timeline status)))
          (is (= {:type :preview} (get-in snapshot [:status :output])))
          (is (= #{:haze :aurora :wash-tide} (into #{} (map :cue-id) (:instances snapshot))))
          (is (empty? (keep :fault (:instances snapshot))))
          (is (= 3 (count (get-in snapshot [:output :universes]))))))
      (finally (app/stop! runtime)))))
