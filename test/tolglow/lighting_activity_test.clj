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
