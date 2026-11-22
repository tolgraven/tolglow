(ns tolglow.studio.fog-control-test
  (:require [cljs.test :refer-macros [deftest is]]
            [tolglow.studio.fog-control :as fog]))

(deftest profiled-pump-selection-round-trips-transit-and-form-ids
  (is (fog/selected? [:house/jet] "house/jet"))
  (is (= [:other] (fog/select-pump [:other :house/jet] "house/jet")))
  (is (= [:other :floor] (fog/select-pump [:other] :floor)))
  (is (false? (fog/selected? [:other/jet] :house/jet))))
