(ns tolglow.fixtures.patch-test
  (:require [clojure.test :refer [deftest is testing]]
            [afterglow.control.model :as control]
            [afterglow.render.frame :as render]
            [tolglow.application :as application]
            [tolglow.demo :as demo]
            [tolglow.persistence :as files]
            [tolglow.fixtures.patch :as patch]
            [tolglow.registry :as registry]
            [tolglow.rig :as rig]
            [tolglow.spatial.targets :as targets]
            [tolglow.show :as show]))

(def pixel {:id :rgbw :name "RGBW" :version 1 :footprint 4
            :channels {:red 1 :green 2 :blue 3 :white 4}})

(deftest bare-names-builders-and-positioned-entries-cross-universes
  (let [calls (atom 0)
        factory #(do (swap! calls inc) pixel)
        groups {:wash {:universe 7 :offset 510 :type :led
                       :list [:rgbw factory [:rgbw 1 2 3 :y-rotation (/ Math/PI 2)]
                              ['venue/wash {:position [3 0 4]}]]}}
        result (patch/expand {:rgbw pixel} {'venue/wash factory} groups)]
    (is (= 2 @calls))
    (is (= [:wash-1 :wash-2 :wash-3 :wash-4] (mapv :id result)))
    (is (= [[7 510] [8 2] [8 6] [8 10]] (mapv (juxt :universe :address) result)))
    (is (= [[0 0 0] [0 0 0] [1 2 3] [3 0 4]] (mapv :position result)))
    (is (= [0.0 90.0 0.0] (:rotation-deg (nth result 2))))
    (is (every? #(= [:wash :led] (:groups %)) result))
    (is (identical? result (patch/expand {} result)))
    (is (= #{7 8} (set (keys (:universes (render/resolve-frame (render/compile-patch result) [] {}))))))))

(deftest separate-universe-groups-reserve-the-whole-footprint
  (let [strip (assoc pixel :footprint 1020)
        result (patch/expand {:rgbw strip} {:strip {:start-universe 10 :offset 4 :list [:rgbw :rgbw]}})]
    (is (= [[10 4] [12 4]] (mapv (juxt :universe :address) result))))
  (let [at {:universe 1 :offset 1 :list [:rgbw]}]
    (doseq [groups [{:wash at :strip at}
                    {:wash (assoc at :list [:missing])}
                    {:wash (assoc at :start-universe 1)}]]
      (is (thrown? clojure.lang.ExceptionInfo (patch/expand {:rgbw pixel} groups))))))

(deftest normal-startup-and-submitted-rig-use-the-same-stored-full-patch
  (let [runtime (application/start!
                 {:init {:components [:show] :modules [:fixtures]}
                  :show {:workers 1 :workspace-options nil}
                  :fixtures {:definitions [pixel] :patches {:wash {:offset 1 :universe 2 :list [:rgbw :rgbw]}}}})]
    (try
      (is (= [:wash-1 :wash-2] (mapv :id (get-in (show/snapshot runtime) [:rig :fixtures]))))
      (is (= :accepted (:status @(show/submit! runtime :rig {:fixtures {:led {:universe 3 :list [:rgbw]}}}))))
      (let [fixtures (get-in (show/snapshot runtime) [:rig :fixtures])]
        (is (= [:led-1] (mapv :id fixtures)))
        (is (= (get-in fixtures [0 :channels]) (:channels pixel)))
        (is (= :rejected (:status @(show/submit! runtime :rig {:fixtures {:led {:universe 3 :list [:absent]}}}))))
        (is (= fixtures (get-in (show/snapshot runtime) [:rig :fixtures]))))
      (finally (application/stop! runtime)))))

(deftest fixture-optics-survive-ordered-groups-and-head-expansion
  (let [props {:beam-angle 18.5 :measured-relative-brightness 2.0 :max-watts 160 :pseudo-fixture false
               :lens-radius-m 0.04 :zoom-range-deg [30 12]}
        single (assoc pixel :beam-angle 45 :max-watts 40 :pseudo-fixture true)
        split (merge props {:id :strip :version 1 :channels {:dimmer 1}
                            :heads [{:channels {:red 2} :beam-angle 22} {:channels {:red 3}}]})
        fixtures (patch/expand {:one single :two split}
                               {:spots {:universe 1 :list [[:one props] :two]}})
        [lamp master h1 h2] fixtures]
    (is (= props (select-keys lamp (keys props))))
    (let [model (assoc-in (demo/model) [:rig :fixtures] fixtures)
          restored (files/read-string (files/write-string model))]
      (is (= fixtures (get-in restored [:model :rig :fixtures]))))
    (is (= [18.5 22 18.5] (mapv :beam-angle [master h1 h2])))
    (is (= [0.0 0.5 0.5] (mapv :emission-share [master h1 h2])))
    (is (= [2.0 2.0 2.0] (mapv :measured-relative-brightness [master h1 h2])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (patch/expand {:one single} {:spots {:universe 1 :list [[:one {:beam-angle -3}]]}})))))

(deftest grouped-fogger-properties-follow-the-target-that-owns-its-fog-channel
  (let [profile {:flow-m3-min 4.0 :mixing-seconds 8 :direction [0 1 0]}
        fixture {:id :hybrid :version 1 :fogger profile
                 :channels {:fog 1} :heads [{:channels {:red 2}}]}
        [machine led] (patch/expand {:hybrid fixture} {:rig {:universe 1 :list [:hybrid]}})
        placed (assoc profile :flow-m3-min 12)
        panel {:id :with-jet :version 1 :channels {:cold-white 1} :heads [{:channels {:fog 2}}]}
        [white nozzle] (patch/expand {:panel panel} {:rig {:universe 1 :list [[:panel {:fogger placed}]]}})]
    (is (= profile (:fogger machine)))
    (is (not (contains? led :fogger)))
    (is (= placed (:fogger nozzle)))
    (is (not (contains? white :fogger)))
    (is (= [1.0 0.0] (mapv :emission-share [white nozzle])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"needs a target with a fog channel"
                          (patch/expand {:panel (assoc panel :heads [{:channels {:blue 2}}])}
                                        {:rig {:universe 1 :list [[:panel {:fogger placed}]]}})))))

(deftest head-only-fixture-keeps-its-body-and-head-extents-distinct
  (let [body {:min [-1 0 0] :max [1 0 0]}
        head {:min [0 0 0] :max [1 0 0]}
        panel {:id :panel :version 1 :bounds-m body
               :heads [{:x 1 :channels {:red 1} :bounds-m head}
                       {:x -1 :channels {:red 2}}]}
        strip {:id :strip :version 1
               :heads (mapv (fn [i] {:y (- i 2) :channels {:red (inc i)}}) (range 5))}
        groups {:rig {:universe 1 :list [[:panel {:rotation-deg [0 0 90]}] :strip]}}
        fixtures (patch/expand {:panel panel :strip strip} groups)
        [first-head other-head] fixtures
        model {:rig {:fixtures fixtures}}
        select (fn [reference]
                 (targets/strip-segment-selector model {:strip-id "rig-2" :reference-id reference :axis :y}))]
    (is (= head (:bounds-m first-head)))
    (is (= {:min [-2 0 0] :max [0 0 0]} (:fixture-bounds-m first-head)))
    (is (nil? (:bounds-m other-head)))
    (is (= {:fixture-ids [:rig-2-head-2 :rig-2-head-3 :rig-2-head-4]} (select :rig-1)))
    (is (= {:fixture-ids [:rig-2-head-4 :rig-2-head-5]} (select :rig-1-head-1)))
    (is (thrown? clojure.lang.ExceptionInfo (select :rig-1-head-2)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (patch/expand {:panel (assoc panel :bounds-m {:min [1 0 0] :max [0 0 0]})
                                :strip strip} groups)))))

(deftest strip-segment-includes-rotated-edges-without-selecting-nearby-pixels
  (let [plane {:min [0 1 -1] :max [0 2 1]}
        line {:min [-1 0 0] :max [1 0 0]}
        edge-pixels (fn [lo hi] [(Math/nextUp (double hi)) hi (Math/nextDown (double lo)) lo])]
    (doseq [[label box rotation origin positions]
            [["A quarter turn collapses the projected interval to zero"
              plane [0 0 90] 0 (edge-pixels 0.0 0.0)]
             ["A wrapped quarter turn maps the projected axis to an unaffected axis"
              plane [30 0 -270] 0 (edge-pixels 0.0 0.0)]
             ["An oblique projection includes authored half-metre edges"
              line [0 0 30] 0 (edge-pixels -0.5 0.5)]
             ["Venue translation does not widen the local precision allowance"
              line [0 0 30] 1.0e9 (edge-pixels 999999999.5 1000000000.5)]]]
      (testing label
        (let [panel (assoc pixel :bounds-m box)
              strip {:id :strip :version 1
                     :heads (mapv (fn [i y] {:x i :y y :channels {:red (inc i)}})
                                  (range) positions)}
              fixtures (patch/expand {:panel panel :strip strip}
                                     {:rig {:universe 1 :list [[:panel {:id :reference :position [0 origin 0]
                                                                      :rotation-deg rotation}]
                                                              [:strip {:id :strip}]]}})]
          (is (= positions (mapv (comp second :position) (rest fixtures))))
          (is (= {:fixture-ids [:strip-head-2 :strip-head-4]}
                 (targets/strip-segment-selector {:rig {:fixtures fixtures}}
                                                 {:strip-id :strip :reference-id :reference :axis :y}))))))))

(deftest published-fixture-extent-selects-only-the-aligned-strip-pixels
  (let [declared {:min [0 -0.2 -1] :max [0 0.2 1]}
        panel (assoc pixel :bounds-m declared)
        strip {:id :strip :version 1
               :heads (mapv (fn [i]
                              {:x i :channels {:red (inc (* 3 i)) :green (+ 2 (* 3 i)) :blue (+ 3 (* 3 i))}})
                            (range 5))}
        fixtures (patch/expand {:strip strip}
                               {:rig {:universe 1 :offset 506 :list [[:strip {:id :strip-a}] [:strip {:id :strip-b}]]}})
        effect {:id :red :version 1 :params {} :outputs {:red 1.0}}
        cue {:id :wash :version 1 :effect-id :red}
        model (assoc (control/new-show {:registry {:effects {:red effect} :cues {:wash cue}}})
                     :rig {:fixtures fixtures})
        captured (atom nil)
        runtime (show/create-runtime {:model model :workspace-options nil :workers 1
                                      :sink {:send! (fn [universes _] (reset! captured universes))}})
        submit (fn [operation payload now]
                 (let [receipt (show/submit! runtime operation payload)]
                   (show/tick! runtime now)
                   (deref receipt 1000 {:status :timeout})))]
    (try
      (doseq [invalid [nil '(1 2 3)]]
        (is (some #(= [:bounds-m] (:path %))
                  (registry/validate-definition :fixtures (assoc panel :bounds-m invalid)))))
      (is (some #(= [:fixtures 0 :bounds-m] (:path %))
                (rig/errors {:fixtures [(assoc (first fixtures) :bounds-m {:min [0 0 0] :max [1 1]})]})))
      (is (= :accepted (:status (submit :definition {:kind :fixtures :definition panel} 0))))
      (is (= :rejected (:status (submit :fixture-patch {:id :bad :fixture-type-id :rgbw :universe 3 :address 1
                                                       :bounds-m {:min [2 0 0] :max [1 0 0]}} 25))))
      (is (= :accepted (:status (submit :fixture-patch {:id :reference :fixture-type-id :rgbw
                                                       :universe 3 :address 1 :position [2 1 0]
                                                       :rotation-deg [0 90 0]} 50))))
      (let [model (show/snapshot runtime)
            selector (targets/strip-segment-selector model {:strip-id :strip-a :reference-id "reference"})]
        (is (= declared (:bounds-m (last (get-in model [:rig :fixtures])))))
        (is (= {:fixture-ids [:strip-a-head-2 :strip-a-head-3 :strip-a-head-4]} selector))
        (is (= {:fixture-ids []}
               (targets/strip-segment-selector model {:strip-id :strip-a :reference-id :reference :axis :y})))
        (is (= :accepted (:status (submit :command {:op :cue/launch :cue-id :wash :launch {:selector selector}} 75))))
        (is (= [[1 509 255] [1 512 255] [2 3 255]]
               (vec (for [[universe payload] (sort-by key @captured)
                          [index value] (map-indexed vector payload)
                          :when (not (zero? value))]
                      [universe (inc index) (bit-and 255 value)])))))
      (finally (show/stop! runtime)))))
