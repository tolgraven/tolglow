(ns tolglow.studio.transit-test
  (:require [cljs.test :refer-macros [deftest is]]
            [thi.ng.color.core :as color]
            [tolglow.transit :as wire]))

(deftest edit-typed-jvm-state
  (let [sent "[\"^ \",\"~:kind\",\"~:demo\",\"~:groups\",[\"~#set\",[\"~:b\",\"~:a\"]],\"kind\",[\"~#tolglow/ratio.v1\",[\"1\",\"3\"]],\"~:color\",[\"~#thi.ng.color/hsla.v1\",[\"^ \",\"~:h\",0.125,\"~:s\",0.5,\"~:l\",0.75,\"~:a\",0.0]]]"
        value (wire/decode sent)
        edited (update value :color color/adjust-alpha 0.25)]
    (is (= :demo (:kind value)))
    (is (= #{:a :b} (:groups value)))
    (is (= (color/hsla 0.125 0.5 0.75 0) (:color value)))
    (is (= 0.25 (color/alpha (:color edited))))
    (is (wire/fraction? (get value "kind")))
    (is (= (/ 1 3) (wire/fraction-value (get value "kind"))))
    (is (= edited (wire/decode (wire/encode edited))))))

(deftest distinct-packed-colors-and-invalid-tags
  (doseq [c [(color/int32 -1) (color/int32 4294967295) (color/int24 0xffffff)
             (color/css "#224466") (color/rgba 0 0 1 0) (color/hsva 0.1 0.5 1 1)
             (color/cmyka 0.5 0.4 0.3 0.2 0)]]
    (let [source (assoc c :name :spot)]
      (is (= source (wire/decode (wire/encode source))))))
  (is (thrown? js/Error (wire/decode "[\"~#thi.ng.color/hsla.v1\",[\"^ \",\"~:a\",1]]")))
  (is (thrown? js/Error (wire/decode "[\"~#tolglow/ratio.v1\",[\"1\",\"0\"]]")))
  (is (= [{:same-key 1} {:same-key 2}]
         (wire/decode (wire/encode [{:same-key 1} {:same-key 2}])))))
