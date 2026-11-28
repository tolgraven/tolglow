(ns tolglow.effects.post-test
  "Post-effects through the real show owner: launches, held release, param
  edits and transport stop, read back from captured DMX universes (#479)."
  (:require [clojure.test :refer [deftest is testing]]
            [afterglow.control.bindings :as bindings]
            [afterglow.control.model :as control]
            [afterglow.render.layers :as layers]
            [tolglow.compiler :as compiler]
            [tolglow.effects :as fx]
            [tolglow.effects.post :as post]
            [tolglow.event-subscribers :as subscribers]
            [tolglow.registry :as registry]
            [tolglow.show :as show]))

(def mud [0.6 0.45 0.3])

(defn- rgb-head [id address x groups]
  {:id id :universe 1 :address address :channels {:red 1 :green 2 :blue 3}
   :position [x 1.0 0.0] :groups groups})

(def fixtures
  (into [(rgb-head :a 1 -1.5 [:left]) (rgb-head :b 4 -0.5 [:left])
         (rgb-head :c 7 0.5 [:right]) (rgb-head :d 10 1.5 [:right])
         {:id :par :universe 1 :address 13 :channels {:dimmer 1 :red 2 :green 3 :blue 4 :white 5 :strobe 6}
          :position [0.0 3.0 0.0] :groups [:left]}]
        (map #(rgb-head (keyword (str "px" %)) (+ 19 (* 3 %)) (* 0.25 %) [:strip]) (range 16))))

(defn- wash [id color & {:as options}]
  (merge {:id id :version 1 :params {} :outputs [{:attribute :color :value color}]} options))

(defn- model [effects cues]
  (reduce (fn [m definition]
            (let [kind (if (:effect-id definition) :cues :effects)]
              (assoc-in m [:registry kind (:id definition)] definition)))
          (control/new-show {:show-id "post" :rig {:fixtures fixtures}
                             :transport {:beat 0 :bpm 120 :playing? true}})
          (concat effects cues)))

(defn- cue [id priority & {:as options}]
  (merge {:id id :version 1 :effect-id id :priority priority} options))

(defn- accept! [rt t type data]
  (let [p (show/submit! rt type data)]
    (show/tick! rt t)
    (let [result (deref p 1000 {})]
      (is (= :accepted (:status result)) (pr-str result))
      result)))

(defn- universe [rt] (mapv #(bit-and 255 %) (get-in @(:state rt) [:output :universes 1])))

(defn- head-bytes [rt address n] (subvec (universe rt) (dec address) (+ (dec address) n)))

(defn- dmx [color] (mapv #(Math/round (* 255.0 (double %))) color))

(defn- close? [expected actual] (every? #(<= (abs %) 1) (map - expected actual)))

(defn- launch [rt t cue-id & [launch]]
  (:instance-id (accept! rt t :command (cond-> {:op :cue/launch :cue-id cue-id} launch (assoc :launch launch)))))

(defn- with-runtime [m batch? f]
  (let [rt (show/create-runtime {:model m :batch? batch? :workspace-options nil})]
    (try (f rt) (finally (show/stop! rt)))))

(deftest post-effect-definitions-are-typed-portable-and-compile
  (doseq [effect [(post/color-guard :guard) (post/bloom-shade :bloom) (post/sparkle-shade :sparkle)
                  (post/group-blackout :blackout)]]
    (is (empty? (compiler/definition-errors :effects effect)) (str (:id effect)))
    (is (registry/data? effect))
    (doseq [[k schema] (:params effect)]
      (is (and (:bounds schema) (:unit schema (= k :seed)) (:name schema)) (str (:id effect) " " k))))
  (is (= [:color] (:categories (post/color-guard :guard))))
  (is (= :held (:launch-mode (post/blackout-cue :hold :blackout {:group-id :left}))))
  (is (= 0.7 (get-in (post/color-guard :g {:defaults {:min-saturation 0.7}}) [:params :min-saturation :default])))
  (is (thrown? clojure.lang.ExceptionInfo (post/color-guard :g {:defaults {:min-saturation 2.0}})))
  (is (thrown? clojure.lang.ExceptionInfo (post/color-guard :g {:defaults {:nope 0.1}}))))

(deftest the-colour-guard-filters-what-lies-beneath-its-layer-position
  (let [effects [(wash :under mud) (wash :over [0.0 0.0 1.0] :targets [:d])
                 (post/color-guard :guard {:defaults {:min-saturation 0.8}})
                 (post/color-guard :warm {:defaults {:hue-center 0.08 :hue-range 0.25 :min-saturation 0.0}})]
        cues [(cue :under 0) (cue :over 50) (cue :guard 10 :selector {:group-id :left})
              (cue :warm 20 :selector {:fixture-ids [:c]})]
        guarded (layers/color-guard mud [0.8 0.0 0.0])
        frames (for [batch? [true false]]
                 (with-runtime (model effects cues) batch?
                   (fn [rt]
                     (launch rt 0 :under)
                     (let [guard (launch rt 25 :guard)]
                       (launch rt 50 :over)
                       (let [before (universe rt)]
                         (accept! rt 75 :command {:op :param/set :instance-id guard :param :amount :value 0.0})
                         {:before before :relaxed (universe rt)})))))]
    (testing "the group under the guard reaches the saturation floor, the rest of the rig keeps its mix"
      (let [{:keys [before]} (first frames)]
        (is (close? (dmx guarded) (subvec before 0 3)))
        (is (close? (dmx guarded) (subvec before 3 6)))
        (is (= (dmx mud) (subvec before 6 9)) "Head c is outside the group")
        (is (< 0.6 (second (layers/rgb->hsv guarded)) 0.8))))
    (testing "a cue above the guard is not filtered"
      (is (= [0 0 255] (subvec (:before (first frames)) 9 12))))
    (testing "an edited parameter reaches the filter on the next frame; amount 0 passes the colour through"
      (is (= (dmx mud) (subvec (:relaxed (first frames)) 0 3))))
    (testing "the typed batch and scalar compilers write the same bytes"
      (is (= (first frames) (second frames))))
    (testing "a palette arc turns an outside hue to its edge"
      (with-runtime (model [(wash :under [0.0 0.2 1.0]) (nth effects 3)]
                           [(cue :under 0) (cue :warm 20 :selector {:fixture-ids [:c]})])
        true
        (fn [rt]
          (launch rt 0 :under)
          (launch rt 25 :warm)
          (let [expected (layers/color-guard [0.0 0.2 1.0] [0.0 0.75 0.08])]
            (is (close? (dmx expected) (head-bytes rt 7 3)))
            (is (< (abs (- 0.955 (first (layers/rgb->hsv expected)))) 1.0e-9) "Blue is nearer the cool edge")
            (is (= (dmx [0.0 0.2 1.0]) (head-bytes rt 4 3)) "Only head c is selected")))))))

(defn- strip [rt] (vec (for [i (range 16)] (head-bytes rt (+ 19 (* 3 i)) 3))))

(defn- sparkle-run
  "Frames of the strip under a darkening sparkle, and the instance state."
  [seed ticks stop-at]
  (with-runtime (model [(wash :under [1.0 0.5 0.2])
                        (post/sparkle-shade :holes {:defaults {:seed seed :chance 0.9}})]
                       [(cue :under 0) (cue :holes 30 :selector {:group-id :strip})])
    true
    (fn [rt]
      (launch rt 0 :under)
      (launch rt 25 :holes)
      (vec (for [t ticks]
             (do (when (= t stop-at)
                   (accept! rt t :command {:op :transport/set :transport {:playing? false}}))
                 (show/tick! rt t)
                 (strip rt)))))))

(deftest a-darkening-sparkle-takes-light-away-from-a-seeded-replay
  (let [ticks (range 50 2050 25)
        frames (sparkle-run 656.0 ticks 1500)
        again (sparkle-run 656.0 ticks 1500)
        other (sparkle-run 7.0 ticks 1500)
        lit (dmx [1.0 0.5 0.2])
        moving (subvec frames 0 (- (count frames) 22))
        stopped (subvec frames (- (count frames) 21))]
    (is (= frames again) "One seed replays byte for byte")
    (is (not= frames other) "Another seed draws other sparks")
    (testing "every pixel is at most as bright as the wash, and sparks darken some of them"
      (is (every? (fn [frame] (every? #(every? true? (map <= % lit)) frame)) frames))
      (is (some (fn [frame] (some #(< (first %) 128) frame)) moving))
      (is (some (fn [frame] (some #(= lit %) frame)) moving)))
    (testing "a stopped transport holds the sparks"
      (is (apply = stopped)))))

(deftest a-shading-bloom-drains-colour-without-darkening-it
  (with-runtime (model [(wash :under [1.0 0.0 0.0])
                        (post/bloom-shade :drain {:defaults {:dim 0.0 :desaturate 1.0 :min 0.0 :max 0.0
                                                              :center-x 0.0 :center-y 1.0 :reach 4.0
                                                              :width 0.1 :halo 0.0}})]
                       [(cue :under 0) (cue :drain 30 :selector {:group-id :strip})])
    true
    (fn [rt]
      (launch rt 0 :under)
      (launch rt 25 :drain)
      (let [pixels (strip rt)
            grey (dmx (layers/color-grade [1.0 0.0 0.0] [0.0 1.0 0.0]))]
        (is (= grey (first pixels)) "The band at the centre turns red to its own grey")
        (is (= [54 54 54] grey))
        (is (= [255 0 0] (peek pixels)) "Beyond the band the red is untouched")))))

(defn- op-fract [x] {:op :fract :args [x]})

(defn- accumulator-effect []
  (fx/effect :drift {:params {:beats (fx/parameter 2.0 {:bounds [0.5 8.0]})}}
             (fx/color (fx/rgb (op-fract (fx/phase-accumulator :turn (fx/param :beats))) 0.2 0.4))))

(defn- blackout-model []
  (model [(wash :under mud) (accumulator-effect)
          {:id :white-par :version 1 :params {} :targets [:par]
           :outputs [{:attribute :dimmer :value 1.0} {:attribute :white :value 0.8} {:attribute :strobe :value 0.3}]}
          (post/group-blackout :blackout)]
         [(cue :under 0) (cue :drift 5 :selector {:group-id :left}) (cue :white-par 6)
          (post/blackout-cue :left-out :blackout {:group-id :left})]))

(defn- drift-state [rt]
  (some #(when (= :drift (:cue-id %)) (:state %)) (vals (get-in @(:state rt) [:model :instances]))))

(def ^:private push {:source {:adapter :push :session-id "pads"}})

(deftest holding-a-group-blackout-darkens-the-group-and-release-restores-it-unchanged
  (doseq [batch? [true false]]
    (let [script (fn [rt hold?]
                   (launch rt 0 :under)
                   (launch rt 0 :drift)
                   (launch rt 0 :white-par)
                   (let [hold (when hold? (bindings/begin-hold {} :left-out (assoc push :gesture-id "pad-1")))
                         ;; The reference ticks at the same times: the transport beat sums frame steps.
                         receipt (if hold? (accept! rt 100 :command (first (:commands hold))) (show/tick! rt 100))
                         held (mapv (fn [t] (show/tick! rt t) [(universe rt) (drift-state rt)]) (range 125 1000 25))
                         launch-mode (when hold? (get-in @(:state rt) [:model :instances (:instance-id receipt) :launch-mode]))
                         gesture (when hold? (bindings/acknowledge-hold (:state hold) "pad-1" receipt))
                         release (when hold? (bindings/end-hold (:state gesture) "pad-1" push))]
                     (if hold? (accept! rt 1000 :command (first (:commands release))) (show/tick! rt 1000))
                     (let [after (mapv (fn [t] (show/tick! rt t) [(universe rt) (drift-state rt)]) (range 1025 1500 25))]
                       (accept! rt 1500 :command {:op :transport/set :transport {:playing? false}})
                       {:receipt (when hold? receipt) :launch-mode launch-mode :held (vec held) :after (vec after) :release release
                        :stopped (mapv (fn [t] (show/tick! rt t) [(universe rt) (drift-state rt)]) (range 1525 1700 25))})))
          reference (with-runtime (blackout-model) batch? #(script % false))
          run (with-runtime (blackout-model) batch? #(script % true))]
      (testing "the pad launches a held, operator-owned instance through the shared command path"
        (is (= :held (:launch-mode run)) (pr-str (:receipt run)))
        (is (= :instance/end (:op (first (:commands (:release run)))))))
      (testing "while held, every emitter of the left group is zero and the right group is untouched"
        (doseq [[[bytes _] [reference-bytes _]] (map vector (:held run) (:held reference))]
          (is (= [0 0 0 0 0 0] (subvec bytes 0 6)) "a and b")
          (is (= [0 0 0 0 0] (subvec bytes 12 17)) "The par's dimmer, colour and white")
          (is (= (nth reference-bytes 17) (nth bytes 17)) "The par's strobe keeps the value beneath")
          (is (= (subvec reference-bytes 6 12) (subvec bytes 6 12)) "c and d")))
      (testing "the blackout never touches the state of what runs beneath it"
        (is (= (map second (:held reference)) (map second (:held run)))))
      (testing "release restores the group exactly as if the blackout had never been held"
        (is (= (:after reference) (:after run)))
        (is (not= [0 0 0] (subvec (ffirst (:after run)) 0 3))))
      (testing "a stopped transport holds the restored group and its state"
        (is (apply = (:stopped run)))))))

(deftest the-held-blackout-stays-with-the-operator
  (with-runtime (blackout-model) true
    (fn [rt]
      (let [id (launch rt 0 :left-out {:mode :held})
            m (:model @(:state rt))]
        (is (= :held (get-in m [:instances id :launch-mode])))
        (is (not-any? #{id} (subscribers/candidates m {:op :instance/end :select {:categories [:intensity :color]}}))
            "Automatic category selections skip it")
        (is (empty? (subscribers/candidates m {:op :cue/launch :select {:ids [:left-out]}}))
            "Automation cannot launch it")
        (is (= [id] (subscribers/candidates m {:op :instance/end :select {:ids [id]}} {:exact? true}))
            "A human's exact instance selection reaches it")))))
