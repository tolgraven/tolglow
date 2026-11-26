(ns tolglow.showcase-parity-test
  "The showcase restates the operator's fixture types as portable data. These
  checks load the original venue definitions and compare what each expands to."
  (:require [clojure.test :refer [deftest is]]
            [tolglow.config :as config]
            [tolglow.fixtures :as fixtures]
            [tolglow.fixtures.patch :as patch]
            [tolglow.showcase.rig :as rig]))

(defn- channel [value]
  (cond (integer? value) {:offset value}
        (vector? value) (zipmap [:offset :fine-offset] value)
        :else value))

(defn- patched [registry source]
  (let [[fixture] (patch/expand registry {} {:probe {:universe 1 :offset 1 :list [[source 0 0 0]]}} [])]
    (-> (select-keys fixture [:name :footprint :calibration :channels])
        (update :channels update-vals channel))))

(deftest showcase-fixture-types-match-the-venue-definitions
  (let [fogger (fixtures/create (get-in config/fixture-data [:other :af-250-fogger]))
        originals (assoc config/fixture-types :af-250-fogger fogger)]
    (doseq [id [:rgbw-7-12-moving :rgbw-18-12-par :wa-100-cob-par :af-250-fogger]]
      (is (= (patched originals id) (patched rig/fixture-types id)) (str id)))
    (is (= (patched originals fixtures/rgbw-60-moving-beam)
           (patched rig/fixture-types :rgbw-60-moving-beam)))))
