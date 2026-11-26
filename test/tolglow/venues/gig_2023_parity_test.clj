(ns tolglow.venues.gig-2023-parity-test
  "The 2023 gig rig restates legacy data that only existed as uncommitted
  edits to `tolglow/config.clj` (issue #720). These checks expand the legacy
  fixture maps with the original builder and run the legacy group patcher
  (`tolglow.util/patch-cfg!`) in a headless Afterglow show, then compare
  channel maps and the DMX channel of every patched attribute with the
  portable venue."
  (:require [afterglow.show :as show]
            [afterglow.show-context :refer [with-show]]
            [afterglow.transform :refer [degrees]]
            [clojure.test :refer [deftest is testing]]
            [tolglow.config :as config]
            [tolglow.fixtures :as fixtures]
            [tolglow.fixtures.patch :as patch]
            [tolglow.util :as util]
            [tolglow.venues.gig-2023 :as gig]))

(def ^:private legacy-data
  "Verbatim from the uncommitted legacy diff, as quoted in issue #720."
  {:rgbw-60-mh
   {:name "60w Beam MH" :calibration {:pan [127 85] :tilt [0 256]}
    :channels
    {:pan [1 2] :tilt [3 4] :dummy 5
     :dimmer 6 :strobe [7 :open 8 :start 16 :hz [0.2 10]]
     :rgbw 8 :dummy-2 12 :dummy-3 13 :dummy-4 14 :dummy-5 15}}
   :cameo-thunder-wash {:name "Cameo Thunder Wash 600 RGB"
                        :channels {:dimmer 1 :strobe 2 :strobe-duration 3 :rgb 4}}
   :cameo-thunder-wash-rgbw {:name "Cameo Thunder Wash 600 RGBW"
                             :channels {:dimmer 1 :strobe [2 :start 11 :end 250] :strobe-duration 3 :rgbw 4}}})

(def ^:private legacy-types
  (merge (select-keys config/fixture-types [:rgbw-12-12-moving :rgbw-7-12-moving])
         (update-vals legacy-data fixtures/create)
         {:rgbw-60-moving-beam (fixtures/rgbw-60-moving-beam)}))

(def ^:private portable-for
  {:rgbw-60-mh :rgbw-60-moving-beam-15ch
   :cameo-thunder-wash :cameo-thunder-wash-600-rgb
   :cameo-thunder-wash-rgbw :cameo-thunder-wash-600-rgbw
   :rgbw-12-12-moving :rgbw-12-12-moving})

(defn- channel [value]
  (cond (integer? value) {:offset value}
        (vector? value) (zipmap [:offset :fine-offset] value)
        :else value))

(defn- dummy? [attribute] (.startsWith (name attribute) "dummy"))

(defn- patched [registry id]
  (let [[fixture] (patch/expand registry {} {:probe {:universe 1 :offset 1 :list [[id 0 0 0]]}} [])]
    (-> (select-keys fixture [:footprint :calibration :channels])
        (update :channels #(into {} (remove (comp dummy? key)) (update-vals % channel))))))

(defn- offsets [channels] (update-vals channels #(select-keys % [:offset :fine-offset])))

(deftest portable-types-keep-the-legacy-channel-maps
  (doseq [[legacy-id portable-id] portable-for
          :let [legacy (patched legacy-types legacy-id)
                portable (patched gig/fixture-types portable-id)]]
    (testing (str legacy-id " -> " portable-id)
      (is (= (:footprint legacy) (:footprint portable)))
      (is (= (:calibration legacy) (:calibration portable)))
      (is (= (offsets (:channels legacy)) (offsets (:channels portable))))))
  (testing "the 15-channel beam and the HY12x12 also keep their shutter/strobe tables"
    (is (= (patched legacy-types :rgbw-60-mh) (patched gig/fixture-types :rgbw-60-moving-beam-15ch)))
    (is (= (patched legacy-types :rgbw-12-12-moving) (patched gig/fixture-types :rgbw-12-12-moving))))
  (testing "the legacy RGBW strobe started at 11, where the manual's strobe range starts"
    (is (= 11 (:start (second (get-in (patched legacy-types :cameo-thunder-wash-rgbw) [:channels :strobe :functions])))))
    (is (= {:start 11 :end 250 :type :strobe :hz [1 30]}
           (get-in gig/fixture-types [:cameo-thunder-wash-600-rgbw :channels :strobe :functions 2]))))
  (testing "the 15-channel mode differs from the 13-channel one only by unused slots and the shutter map"
    (let [fifteen (:channels (patched gig/fixture-types :rgbw-60-moving-beam-15ch))
          thirteen (:channels (patched gig/fixture-types :rgbw-60-moving-beam))]
      (is (= (dissoc thirteen :movement-speed :auto :control :shutter) (dissoc fifteen :shutter)))
      (is (= 7 (get-in fifteen [:shutter :offset]) (get-in thirteen [:shutter :offset]))))))

(defn- legacy-config
  "The gig's legacy `:fixtures :patches`, with fixture types as the legacy
  config resolved them (keywords from `tolglow.config/fixture-types`, the beam
  from the `tolglow.fixtures` builder). Positions are the legacy ones."
  [moving-offset]
  {:moving {:universe 1 :offset moving-offset :type :moving
            :list [[:rgbw-12-12-moving 1.5 1.2 -0.5 :y-rotation (degrees 180)]
                   [fixtures/rgbw-60-moving-beam 0.2 0.2 0.0]]}
   :moving-mini {:universe 1 :offset 100 :type :moving
                 :list [[:rgbw-7-12-moving -3.0 3.0 0.0] [:rgbw-7-12-moving 0.0 3.0 0.0]
                        [:rgbw-7-12-moving 3.0 3.0 0.0]
                        [:rgbw-7-12-moving 0.2 0.0 -2.0 :y-rotation (degrees 180)]]}
   :wash {:universe 1 :offset 301 :type :wash
          :list (mapv (fn [x] [:cameo-thunder-wash-rgbw x 4.0 0.0]) [-6.0 -4.0 -2.0 2.0 4.0 6.0 8.0 10.0])}
   :wash-2 {:universe 1 :offset 357 :type :wash
            :list [[:cameo-thunder-wash 0.0 4.0 0.0] [:cameo-thunder-wash -0.5 4.0 0.5]]}})

(defn- afterglow-channels
  "Attribute -> absolute coarse/fine DMX channels of a fixture Afterglow patched."
  [fixture]
  (into {} (for [{:keys [type color index fine-index]} (concat (:channels fixture) (mapcat :channels (:heads fixture)))
                 :let [attribute (if (= :color type) color type)]
                 :when (not (dummy? attribute))]
             [attribute (cond-> [(inc index)] fine-index (conj (inc fine-index)))])))

(defn- legacy-patch
  "Run the legacy group patcher `tolglow.util/patch-cfg!` on the gig patch in a
  headless Afterglow show (no universe is opened or sent) and return fixture
  key -> {:universe :channels} as Afterglow patched it."
  [moving-offset]
  (with-redefs [config/cfg (fn [& path] (when (= path [:fixtures :patches]) (legacy-config moving-offset)))
                config/fixture-types legacy-types]
    (with-show (show/show :universes [1])
      (with-out-str (util/patch-cfg!))
      (into {} (for [fixture (show/all-fixtures)]
                 [(:key fixture) {:universe (into #{} (map :universe) (:channels fixture))
                                  :channels (afterglow-channels fixture)}])))))

(defn- dmx-channels
  "Attribute -> absolute coarse/fine DMX channels for a fixture at address."
  [channels address]
  (into {} (for [[attribute value] channels :when (not (dummy? attribute))
                 :let [{:keys [offset fine-offset]} (channel value)]]
             [attribute (cond-> [(+ address offset -1)] fine-offset (conj (+ address fine-offset -1)))])))

(defn- venue-patch []
  (into {} (for [{:keys [id universe channels address]} (gig/fixtures)]
             [id {:universe #{universe} :channels (dmx-channels channels address)}])))

(deftest legacy-patch-addresses-map-to-the-same-dmx-channels
  (let [legacy (legacy-patch 0)
        current (venue-patch)]
    (testing "every fixture outside :moving lands on the same universe and DMX channels"
      (doseq [id (keys current) :when (not (#{:moving-1 :moving-2} id))]
        (is (= (legacy id) (current id)) (str id))))
    (testing ":offset 0 on :moving: Afterglow rejected the HY12x12 and the beam went to 16"
      (is (= (disj (set (keys current)) :moving-1) (set (keys legacy))))
      (is (= [16 17] (get-in legacy [:moving-2 :channels :pan])))
      (is (= (update-vals (get-in legacy [:moving-2 :channels]) #(mapv inc %))
             (get-in current [:moving-2 :channels]))))
    (testing "with the venue's :offset 1 the legacy patcher produces the venue's whole DMX map"
      (is (= (legacy-patch 1) current)))))
