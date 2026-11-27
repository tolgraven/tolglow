(ns tolglow.rig-placement-test
  (:require [clojure.test :refer [deftest is testing]]
            [afterglow.render.frame :as render]
            [tolglow.calibration.geometry :as geometry]
            [tolglow.fixtures.library :as library]
            [tolglow.rig.aim-calibration :as aim]
            [tolglow.rig.placement :as placement]))

(defn- close? [a b & [tolerance]]
  (every? #(< (Math/abs (double %)) (or tolerance 1.0e-6)) (map - a b)))

(def head-type
  {:id "spot" :version 2 :name "Spot" :footprint 6
   :channels {:dimmer 1 :red 2 :green 3 :blue 4 :pan 5 :tilt 6}
   :calibration {:pan {:min-deg -180 :max-deg 180} :tilt {:min-deg -90 :max-deg 90}}})

(defn- oracle-direction
  "Afterglow's own world beam direction for normalized pan/tilt."
  [rotation pan tilt]
  (:direction (render/fixture-pose (assoc (placement/instance head-type {:id "h" :universe 1 :address 1}) :rotation-deg rotation)
                                   {:pan pan :tilt tilt})))

(deftest mount-rotations-agree-with-the-renderer
  (doseq [[mount up] placement/mounts
          forward [[0 0 1] [1 0 0] [0 -1 0] [0.3 0.2 -1]]
          :let [rotation (placement/mount-rotation up forward)]
          :when rotation]
    (testing (str mount " " forward)
      (let [expected (#'placement/reject forward up)]
        ;; Pan/tilt home (0 degrees) is the forward direction; full tilt is up.
        (is (close? expected (oracle-direction rotation 0.5 0.5)) (pr-str rotation))
        (is (close? up (oracle-direction rotation 0.5 1.0)))
        (is (close? up (:up (placement/axes rotation))))
        (is (= (case mount :standing :standing :hanging :hanging :wall) (placement/mounting rotation))))))
  (is (= [0.0 0.0 0.0] (placement/mount-rotation [0 1 0])))
  (is (= [0.0 0.0 180.0] (placement/mount-rotation [0 -1 0])))
  (is (nil? (placement/mount-rotation [0 1 0] [0 2 0])))
  (is (nil? (placement/mount-rotation [0 0 0] [0 0 1]))))

(deftest euler-decomposition-is-shared-with-the-aim-fit
  (doseq [rotation [[10 20 30] [-45 80 170] [90 0 0] [0 90 0] [33 -12 -140]]]
    (let [{:keys [right up forward]} (placement/axes rotation)
          matrix (mapv (fn [row] (mapv #(nth % row) [right up forward])) (range 3))
          recovered (placement/rotation-euler matrix)]
      (is (= recovered (geometry/rotation-euler matrix)))
      (is (close? forward (:forward (placement/axes recovered))))
      (is (close? up (:up (placement/axes recovered)))))))

(deftest facing-and-remounting-keep-the-mount
  (let [hanging (placement/mount-rotation [0 -1 0])
        faced (placement/face hanging [0 4 0] [3 0 0])]
    (is (close? [1 0 0] (:forward (placement/axes faced))))
    (is (= :hanging (placement/mounting faced)))
    (is (nil? (placement/face hanging [0 4 0] [0 0 0])) "A target on the mounting axis has no heading")
    (let [standing (placement/remount faced [0 1 0])]
      (is (close? [1 0 0] (:forward (placement/axes standing))))
      (is (= :standing (placement/mounting standing))))
    (is (close? [0 -1 0] (:forward (placement/axes (placement/remount [0 0 0] [0 0 1]))))
        "A home beam along the new mounting axis falls back to the default")))

(deftest placement-takes-a-fresh-id-address-and-surface-mount
  (let [existing [{:id "spot-1" :universe 1 :address 1 :footprint 6 :channels {:dimmer 1}}
                  {:id :par :universe 1 :address 508 :channels {:red 1 :green 2 :blue 3}}]
        ceiling (placement/place head-type existing {:point [1 5 -2] :normal [0 -1 0]})
        floor (placement/place head-type [] {:point [0 0 0] :normal [0 1 0]} {:clearance-m 0.1 :id "floor-spot"})]
    (is (= "spot-2" (:id ceiling)))
    (is (= [2 1] [(:universe ceiling) (:address ceiling)]) "The next six channels would cross 512")
    (is (close? [1 4.75 -2] (:position ceiling)))
    (is (= :hanging (placement/mounting (:rotation-deg ceiling))))
    (is (= {:id "floor-spot" :universe 1 :address 1 :position [0.0 0.1 0.0] :rotation-deg [0.0 0.0 0.0]}
           (select-keys floor [:id :universe :address :position :rotation-deg])))
    (is (= "spot" (:fixture-type-id floor)))
    (is (= 2 (:fixture-type-version floor)))
    (is (= (:channels head-type) (:channels floor)))
    (is (nil? (placement/place head-type [] {:point [0 0 0] :normal [0 0 0]})))
    (is (= (library/instantiate head-type {:id "x" :universe 1 :address 1})
           (library/instantiate head-type {:id "x" :universe 1 :address 1}))
        "instantiate builds on the shared instance")))

(deftest reversal-and-ranges
  (is (= {:offset 5 :fine-offset 6 :inverted-from 0} (placement/reverse-channel [5 6] true)))
  (is (= {:offset 5} (placement/reverse-channel {:offset 5 :inverted-from 0} false)))
  (is (placement/reversed? (placement/reverse-channel 5 true)))
  (is (not (placement/reversed? {:offset 5 :inverted-from 128})))
  (is (placement/same-patch? {:pan [5 6]} {:pan {:offset 5 :fine-offset 6 :inverted-from 0}}))
  (is (not (placement/same-patch? {:pan [5 6]} {:pan 5})))
  (is (= {:min-deg -180.0 :max-deg 180.0} (placement/axis-range [127.5 127.5])))
  (is (nil? (placement/axis-range [127 -127])))
  (is (= 0.5 (placement/home-position {:min-deg -270 :max-deg 270})))
  (is (= 0.25 (placement/home-position {:min-deg -90 :max-deg 270}))))

(def types {"spot" head-type :rgb {:id :rgb :version 1 :footprint 3 :channels {:red 1 :green 2 :blue 3}}})

(deftest new-placements-are-instantiated-from-their-published-type
  (let [existing {:id "old" :fixture-type-id "spot" :universe 1 :address 1 :channels {:dimmer 1}}
        placed {:id :spot-2 :fixture-type-id "spot" :universe 1 :address 20 :position [0 4 0]
                :rotation-deg [0 0 180] :footprint 99 :fixture-type-version 1 :name "Left spot"}
        [kept admitted] (library/admit-placements types ["old"] [existing placed])]
    (is (= existing kept) "Committed fixtures are not re-derived")
    (is (= (:channels head-type) (:channels admitted)))
    (is (= 6 (:footprint admitted)) "Type data wins over client-supplied type fields")
    (is (= 2 (:fixture-type-version admitted)))
    (is (= "Left spot" (:name admitted)))
    (is (= [0 0 180] (:rotation-deg admitted)))
    (testing "A keyword type ID is found from its wire spelling"
      (is (= {:red 1 :green 2 :blue 3}
             (:channels (first (library/admit-placements types [] [{:id "r" :fixture-type-id "rgb" :universe 2 :address 1}])))))))
  (testing "Axis reversal is the only channel difference allowed"
    (let [reversed (assoc-in (:channels head-type) [:pan] {:offset 5 :inverted-from 0})]
      (is (= reversed (:channels (first (library/admit-placements types [] [{:id "r" :fixture-type-id "spot" :universe 1 :address 1
                                                                                :channels reversed}]))))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"must match"
                          (library/admit-placements types [] [{:id "r" :fixture-type-id "spot" :universe 1 :address 1
                                                               :channels {:dimmer 1}}]))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"pan angle range"
                        (library/admit-placements {"spot" (dissoc head-type :calibration)} []
                                                  [{:id "r" :fixture-type-id "spot" :universe 1 :address 1}])))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"type not found"
                        (library/admit-placements types [] [{:id "r" :fixture-type-id "missing" :universe 1 :address 1}])))
  (let [legacy {:id "imported" :fixture-type-id "missing" :universe 1 :address 1 :channels {:red 1}}]
    (is (= [legacy] (library/admit-placements types [] [legacy])) "Fully specified fixtures keep working")))

(deftest aim-checks-and-fits-use-the-renderer
  (let [rig {:fixtures [(assoc (placement/instance head-type {:id :spot-1 :universe 1 :address 1 :position [0 4 0]})
                               :rotation-deg [0 0 180])]}
        fixture (aim/fixture rig {:fixture-id "spot-1"})
        below (aim/check fixture [0 0 4])]
    (is (:reachable? below))
    (is (close? [0 -1 1] (map #(* % (Math/sqrt 2)) (:direction below)) 1.0e-9))
    (is (= {:pan 128 :tilt 191} (:dmx below)) "Hanging, the head tilts towards its own base")
    (is (= :at-lens (:reason (aim/check fixture [0 4 0]))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Save the rig first" (aim/fixture rig {:fixture-id "nope"})))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid draft"
                          (aim/fixture rig {:fixture-id "spot-1" :calibration {:pan {:min-deg 10 :max-deg -10}}})))
    (testing "Draft overrides change the solution without changing the rig"
      (is (not= (:pan below) (:pan (aim/check (aim/fixture rig {:fixture-id "spot-1" :rotation-deg [0 90 180]}) [0 0 4])))))
    (testing "A fit recovers the hanging mount from jogged observations"
      (let [samples (for [target [[2 0 3] [-2 0 2] [1 0 -3] [0 1 5]]]
                      (let [{:keys [pan tilt]} (aim/check fixture target)] {:target target :pan pan :tilt tilt}))
            standing (assoc-in rig [:fixtures 0 :rotation-deg] [0 0 0])
            fit (aim/fit (aim/fixture standing {:fixture-id "spot-1"}) samples true)]
        (is (:accepted? fit) (pr-str fit))
        (is (= :hanging (:mounting-kind fit)))
        (is (close? [0 -1 0] (:up (placement/axes (:rotation-deg fit))) 1.0e-3))
        (is (false? (:accepted? (aim/fit fixture samples false))) "Unverified axes stay a candidate")))))
