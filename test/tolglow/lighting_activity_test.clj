(ns tolglow.lighting-activity-test
  (:require [afterglow.render.frame :as frame]
            [afterglow.runtime.events :as events]
            [clojure.test :refer [deftest is testing]]
            [tolglow.fixtures.heads :as heads]
            [tolglow.lighting-activity :as activity]
            [tolglow.lighting-capacity :as capacity]))

(def lamps
  (mapv (fn [i]
          {:id (keyword (str "head-" i)) :universe 1 :address (inc (* 8 i)) :groups [(if (zero? i) :solo :rest)]
           :channels {:dimmer 1 :red 2 :green 3 :blue 4 :pan 5 :tilt 6 :strobe {:offset 7 :hz [0.5 20.0]}}})
        (range 8)))

(def full [1.0 1.0 1.0])

(defn- block [effect targets output]
  {:kind :assignments :source {:effect-id effect :effect-version 1}
   :assignments (vec (for [id targets [field value] output]
                       {:target-id id :attribute field :value value}))})

(defn- capture [fixtures]
  (let [patch (frame/compile-patch fixtures)
        meter (capacity/prepare patch)]
    (fn tick
      ([prior t blocks] (tick prior t blocks {}))
      ([prior t blocks opts]
       (let [frame (frame/resolve-contributions patch blocks opts)
             sources (capacity/measure-sources meter (:universes frame) (:target-order patch))]
         (activity/advance (:state prior) patch sources frame blocks t))))))

(deftest rate-and-visibility-on-an-actual-strobe-channel
  (let [ids (mapv :id lamps)
        tick (capture lamps)
        wash (block :wash ids {:color full :dimmer 1.0})
        sample (fn [color rate]
                 (:report (tick nil 0 [(if (= color full) wash (block :wash ids {:color color :dimmer 1.0}))
                                        (block :flash ids {:strobe rate})])))
        slow (sample full 0.04) fast (sample full 1.0) dark (sample [0 0 0] 1.0)]
    (is (< 0 (get-in slow [:scene :strobe]) (get-in fast [:scene :strobe])))
    (is (> (get-in fast [:scene :score]) 0.90))
    (is (zero? (get-in dark [:scene :score])))
    (is (zero? (get-in fast [:effects :wash :strobe])))
    (is (> (get-in fast [:effects :flash :strobe]) 0.9))
    (is (= 0.0 (get-in (:report (tick nil 0 [wash] {:blackout? true})) [:scene :score])))))

(deftest signed-beam-velocity-and-output-edge-shape
  (let [ids (mapv :id lamps)
        tick (capture lamps)
        step (fn [prior t dim pan]
               (tick prior t [(block :lamp ids {:dimmer dim :color full})
                              (block :move ids {:pan pan :tilt 0.5})]))
        first (step nil 0 0.0 0.5)
        soft (step first 25 0.025 0.5)
        abrupt (step first 25 1.0 0.5)
        slow (step abrupt 50 1.0 0.5001)
        turn (step abrupt 50 1.0 0.54)
        reverse (step turn 75 1.0 0.5)]
    (is (< (get-in soft [:report :scene :flashing]) (* 0.1 (get-in abrupt [:report :scene :flashing]))))
    (is (< (get-in slow [:report :scene :movement]) (get-in turn [:report :scene :movement])))
    (is (> (get-in reverse [:report :effects :move :movement]) 0.9))
    (is (zero? (get-in turn [:report :effects :lamp :movement])))
    (is (zero? (get-in (step first 25 0.0 0.57) [:report :effects :move :movement])))))

(deftest coverage-and-fade-through-regions
  (let [ids (mapv :id lamps)
        tick (capture lamps)
        black (tick nil 0 [])
        on {:color full :dimmer 1}
        one (tick black 25 [(block :spark [(first ids)] on)])
        all (tick black 25 [(block :spark ids on)])
        old (tick black 25 [(block :low ids on) (block :new ids on)])
        dissolve (tick one 50 [(block :spark [(first ids)] {:color full :dimmer 0.9})])]
    (is (= 0.125 (get-in one [:report :scene :coverage])))
    (is (> (get-in one [:report :groups :solo :score]) 0.8))
    (is (zero? (get-in one [:report :groups :rest :score])))
    (is (< (get-in one [:report :scene :score]) (* 0.15 (get-in all [:report :scene :score]))))
    (is (not (contains? (get-in old [:report :effects]) :low)))
    (is (pos? (get-in dissolve [:report :effects :spark :flashing])))))

(deftest chromatic-steps-in-rich-and-compact-frames
  (let [tick (capture [(first lamps)])
        single [(:id (first lamps))]
        rd (block :wash single {:dimmer 1 :color [1 0 0]})
        ;; Rec. 709 green drive for a roughly isoluminant red-to-green change.
        gn (block :wash single {:dimmer 1 :color [0 0.297 0]})
        check (fn [opts]
                (let [first (tick nil 0 [rd] opts)]
                  (get-in (tick first 25 [gn] opts) [:report :scene :flashing])))
        rich (check {}), compact (check {:include-values? false :compact-values? true})]
    (is (> rich 0.10)) (is (= rich compact))))

(deftest shutter-modes-and-portable-builder-data
  (let [source {:id :profile :version 1 :name "Profiled shutter" :channels [{:type :dimmer :offset 1}
                                                      {:type :shutter :offset 2 :functions
                                                       [{:type :shutter-closed :start 0 :end 20}
                                                        {:type :strobe :start 21 :end 230 :hz [1 18] :scale-fn identity}
                                                        {:type :shutter-open :start 231 :end 255}]}]}
        fixture (first (:fixtures (heads/targets source {:id :lamp :universe 1 :address 1} 0)))
        tick (capture [fixture])
        read (fn [raw] (get-in (tick nil 0 [(block :lamp [:lamp] {:dimmer 1.0 :shutter (/ raw 255.0)})])
                               [:report :scene]))]
    (is (not (contains? (get-in fixture [:channels :shutter :functions 1]) :scale-fn)))
    (is (= [1 18] (get-in fixture [:channels :shutter :functions 1 :hz])))
    (is (zero? (:brightness (read 0))))
    (is (> (:strobe (read 225)) (:strobe (read 23))))
    (is (= 0.0 (:strobe (read 250))))
    (is (= 1.0 (:brightness (read 250))))))

(deftest repeat-events-and-versioned-planner-evidence
  (let [tick (capture [(first lamps)])
        effect {:id :pulse :version 1 :outputs {:color full :strobe 1.0}}
        model (events/begin {:show-id "show" :registry {:effects {:pulse effect}} :rig {:fixtures lamps}})
        cue {:id :cue :effect-id :pulse}
        sample (:report (tick nil 0 [(block :pulse [(:id (first lamps))] {:color full :strobe 0.85})]))
        report (activity/publish model sample)
        again (activity/publish (events/begin report) sample)]
    (is (= [:lighting/activity] (mapv #(get-in % [:event :type]) (events/pending report))))
    (is (empty? (events/pending again)))
    (is (= (get-in sample [:effects :pulse :score]) (activity/predicted-score again cue)))
    (is (not= (activity/predicted-score again cue) (activity/predicted-score (assoc-in again [:registry :effects :pulse :version] 2) cue)))
    (is (nil? (activity/predicted-score model {:effect-id :missing})))
    (is (nil? (get-in (activity/publish report nil) [:variables :lighting/activity])))))

(deftest provisional-scores-honor-effect-output-scope-and-saved-dimmer
  (let [model {:fixtures lamps
               :registry {:effects {:shot {:id :shot :version 1 :params {:level {:type :number :default 1}}
                                          :outputs [{:attribute :dimmer :value {:op :param :key :level}}
                                                    {:attribute :color :value full}
                                                    {:attribute :strobe :value 1.0 :targets [:head-0]}]}}}}
        cue {:effect-id :shot}
        sparse (activity/predicted-score model cue)]
    (is (zero? (activity/predicted-score model (assoc cue :saved-params {:level 0}))))
    (is (< sparse (activity/predicted-score (update-in model [:registry :effects :shot :outputs 2] dissoc :targets) cue)))))

(deftest packed-meter-follows-ownership-changes-and-decaying-edges
  (let [patch (frame/compile-patch lamps) meter (capacity/prepare patch)
        ids (:target-order patch)
        score (fn [previous t effect dim]
                (let [blocks [(block effect ids {:color full :dimmer dim})]
                      output (frame/resolve-contributions patch blocks {:include-values? false :compact-values? true})]
                  (activity/advance-meter (:state previous) patch meter output blocks t)))
        mapped (capture lamps)
        dark (score nil 0 :wash 0.0)
        edge (score dark 25 :wash 1.0)
        held (score edge 50 :wash 1.0)
        x (score nil 0 :wash 1.0)
        y (score x 25 :wash 1.0)
        z (score y 50 :other 1.0)]
    (is (< 0.0 (get-in held [:report :scene :flashing]) (get-in edge [:report :scene :flashing])))
    (is (identical? (:report x) (:report y)) "Equivalent new DMX buffers reuse steady measurements")
    (is (= #{:other} (set (keys (get-in z [:report :effects])))))
    (is (= (:report edge)
           (:report (mapped
                     (mapped nil 0 [(block :wash ids {:color full :dimmer 0})])
                     25 [(block :wash ids {:color full :dimmer 1})]))))))

(deftest scene-meter-matches-the-full-report-across-writer-and-frame-changes
  (let [patch (frame/compile-patch lamps)
        meter (capacity/prepare patch)
        targets (:target-order patch)
        wash (block :wash targets {:color full :dimmer 1.0 :pan 0.5})
        move (block :move targets {:color [1 0 0] :dimmer 0.7 :pan 0.6 :strobe 0.25})
        frames [[0 [wash] {}] [25 [wash] {}] [50 [move] {}]
                [75 [move] {}] [100 [move] {:blackout? true}]
                [2000 [wash] {}] [2025 [(dissoc wash :source)] {}]]]
    (reduce (fn [[full-prior scene-prior] [t blocks options]]
              (let [output (frame/resolve-contributions patch blocks
                             (merge {:include-values? false :compact-values? true} options))
                    full (activity/advance-meter full-prior patch meter output blocks t)
                    scene (activity/advance-scene-meter scene-prior patch meter output blocks t)]
                (is (= (select-keys (:report full) [:scene]) (:report scene)))
                [(:state full) (:state scene)]))
            [nil nil] frames)))

(deftest operator-look-replaces-a-steady-source-at-the-same-level
  (let [tick (capture [(first lamps)]) id (:id (first lamps))
        wash (block :wash [id] {:color full :dimmer 1})
        prior (tick nil 0 [wash])
        operator-look (dissoc wash :source)
        pause-look (tick prior 25 [operator-look])
        halfway (tick prior 25 [wash (update operator-look :assignments #(mapv (fn [a] (assoc a :opacity 0.5)) %))])]
    (is (= 1.0 (get-in pause-look [:report :scene :brightness])))
    (is (empty? (get-in pause-look [:report :effects])))
    (is (= 0.5 (get-in halfway [:report :effects :wash :brightness])))))

(defn- dimmer-batch [effect ids opacity]
  {:kind :batch :source {:effect-id effect :effect-version 1}
   :batch {:target-ids ids :blend :replace :opacity opacity
           :outputs {:dimmer {:type :number :components 1 :values (double-array (repeat (count ids) 1.0))}}}})

(deftest continuous-alpha-sweep-retains-attribution-for-lower-layers
  (let [ids [:a :b :c :d]
        patch (frame/compile-patch (mapv (fn [i id] {:id id :universe 1 :address (inc i) :channels {:dimmer 1}})
                                         (range) ids))
        meter (capacity/prepare patch)
        sample (fn [prior upper-ids layer-alpha member-alpha batch-alpha scalar-alpha]
                 (let [layer {:id :compound :opacity layer-alpha}
                       blocks [(dimmer-batch :base ids 1.0)
                               (assoc (dimmer-batch :wash upper-ids 1.0) :native-layer (assoc layer :member 0))
                               (assoc (dimmer-batch :sweep upper-ids batch-alpha)
                                      :native-layer (assoc layer :member 1 :member-opacity member-alpha))
                               (update (block :trim [:b] {:dimmer 1.0}) :assignments
                                       #(mapv (fn [a] (assoc a :opacity scalar-alpha)) %))]
                       t (+ 25 (get-in prior [:state :time-ms] -25))
                       frame (frame/resolve-contributions patch blocks {:include-values? false :compact-values? true})]
                   (activity/advance-meter (:state prior) patch meter frame blocks t)))
        first-sample (sample nil [:a :b :c] 0.1 0.2 0.3 0.25)
        last-sample
        (reduce (fn [prior n]
                  (let [layer (/ n 43.0)
                        member (/ (- 42 n) 42.0)
                        batch (/ (mod (* n 7) 43) 42.0)
                        trim (/ (mod (* n 5) 41) 40.0)
                        result (sample prior [:a :b :c] layer member batch trim)
                        ;; The last scalar affects only B. The compound's outer
                        ;; opacity crosses its complete member mix with the base.
                        affected (/ (- 3.0 trim) 4.0)
                        expected {:base (+ 0.25 (* affected (- 1.0 layer)))
                                  :wash (* affected layer (- 1.0 (* member batch)))
                                  :sweep (* affected layer member batch)
                                  :trim (/ trim 4.0)}]
                    (is (identical? (get-in first-sample [:state :plan]) (get-in result [:state :plan])))
                    (doseq [[id fraction] expected]
                      (is (< (abs (- fraction (get-in result [:report :effects id :brightness] 0.0))) 1e-9)
                          (str "writer " id " at step " n)))
                    result))
                first-sample (range 43))
        retargeted (sample last-sample [:a :b] 1.0 1.0 1.0 0.0)]
    (is (= 3 (count (get-in last-sample [:state :plan :representatives]))) "A and C share the same recipe")
    (is (not (identical? (get-in last-sample [:state :plan]) (get-in retargeted [:state :plan]))))
    (is (= 0.5 (get-in retargeted [:report :effects :sweep :brightness])))
    (is (= 0.5 (get-in retargeted [:report :effects :base :brightness])))))

(deftest native-transforms-receive-addressed-targets-at-zero-assignment-opacity
  (let [tick (capture [{:id :a :universe 1 :address 1 :channels {:dimmer 1}}
                       {:id :b :universe 1 :address 2 :channels {:dimmer 1}}])
        blocks [(dimmer-batch :base [:a :b] 1.0)
                (assoc (dimmer-batch :adjust [:a] 0.0) :native-layer
                       {:id :edited :member 0 :transforms [{:attribute :dimmer :value 0.5}]})]
        report (:report (tick nil 0 blocks))]
    (is (< 0.24 (get-in report [:effects :adjust :brightness]) 0.26))
    (is (= 0.5 (get-in report [:effects :base :brightness])))))

(deftest movement-tracking-reads-the-same-coarse-fine-pose-as-rendering
  (let [fixture {:id :head :universe 1 :address 509 :channels {:dimmer 1 :pan [2 3] :tilt [4 5]}}
        tick (capture [fixture])]
    (doseq [code [255 256 32767 32768 65535]]
      (let [pan (/ code 65535.0)
            tilt (/ (- 65535 code) 65535.0)
            expected (:direction (frame/fixture-pose fixture {:pan pan :tilt tilt}))
            observed (tick nil 0 [(block :sweep [:head] {:dimmer 1.0 :pan pan :tilt tilt})])
            actual (first (get-in observed [:state :samples :poses]))]
        (is (= 3 (count actual)))
        (is (every? #(< (abs %) 1.0e-12) (map - actual expected)) code)))))
