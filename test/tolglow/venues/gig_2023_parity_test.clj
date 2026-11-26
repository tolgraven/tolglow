(ns tolglow.venues.gig-2023-parity-test
  "The 2023 gig rig restates legacy data that only existed as uncommitted
  edits to `tolglow/config.clj` (issue #720). These checks expand the legacy
  fixture maps with the original builder and replay the legacy group patcher
  (`tolglow.util/patch-cfg!`), then compare channel maps and the DMX channel
  of every patched attribute with the portable venue."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.config :as config]
            [tolglow.fixtures :as fixtures]
            [tolglow.fixtures.patch :as patch]
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

(def ^:private legacy-patches
  "The gig patch: group, legacy :offset and fixture list (positions omitted)."
  [[:moving 0 [:rgbw-12-12-moving :rgbw-60-moving-beam]]
   [:moving-mini 100 (repeat 4 :rgbw-7-12-moving)]
   [:wash 301 (repeat 8 :cameo-thunder-wash-rgbw)]
   [:wash-2 357 (repeat 2 :cameo-thunder-wash)]])

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

(defn- jump
  "The legacy patcher advanced by the highest channel offset of a fixture."
  [fixture]
  (apply max (mapcat (juxt :offset #(or (:fine-offset %) 0)) (:channels fixture))))

(defn- legacy-addresses
  "Fixture ID -> address as `patch-cfg!` computed it, or nil where Afterglow's
  `patch-fixture!` precondition (1 <= start-address <= 512) rejected it."
  []
  (into {}
        (for [[group offset ids] legacy-patches
              [i [id address]] (map-indexed vector
                                            (map vector ids (reductions + offset (map (comp jump legacy-types) ids))))]
          [(keyword (str (name group) "-" (inc i)))
           {:type id :address (when (<= 1 address 512) address) :computed address}])))

(defn- dmx-channels
  "Attribute -> absolute coarse/fine DMX channels for a fixture at address."
  [channels address]
  (into {} (for [[attribute value] channels :when (not (dummy? attribute))
                 :let [{:keys [offset fine-offset]} (channel value)]]
             [attribute (cond-> [(+ address offset -1)] fine-offset (conj (+ address fine-offset -1)))])))

(defn- legacy-channels [type-id address]
  (dmx-channels (:channels (patched legacy-types type-id)) address))

(deftest legacy-patch-addresses-map-to-the-same-dmx-channels
  (let [legacy (legacy-addresses)
        current (into {} (map (juxt :id identity)) (gig/fixtures))]
    (is (= (set (keys legacy)) (set (keys current))))
    (testing "every group but :moving lands on the same DMX channels"
      (doseq [[id {:keys [type address]}] legacy
              :when (not= :moving (first (:groups (current id))))]
        (is (= 1 (:universe (current id))))
        (is (= (legacy-channels type address)
               (dmx-channels (:channels (current id)) (:address (current id))))
            (str id))))
    (testing ":offset 0 on :moving: Afterglow rejected the HY12x12 and put the beam at 16"
      (is (= {:type :rgbw-12-12-moving :address nil :computed 0} (legacy :moving-1)))
      (is (= {:type :rgbw-60-moving-beam :address 16 :computed 16} (legacy :moving-2))))
    (testing "the corrected :offset 1 patches the HY12x12 at 1-16 and shifts the beam by one"
      (is (= (legacy-channels :rgbw-12-12-moving 1)
             (dmx-channels (:channels (current :moving-1)) (:address (current :moving-1)))))
      (is (= (update-vals (legacy-channels :rgbw-60-moving-beam 16) #(mapv inc %))
             (dmx-channels (:channels (current :moving-2)) (:address (current :moving-2))))))))
