(ns tolglow.transit-test
  (:require [clojure.test :refer [deftest is]]
            [thi.ng.color.core :as color]
            [tolglow.transit :as wire]))

(deftest browser-edited-color-and-fraction
  ;; Sent by a real ClojureScript reader/writer after adjusting JVM HSLA alpha.
  (let [sent "[\"^ \",\"~:kind\",\"~:demo\",\"~:groups\",[\"~#set\",[\"~:b\",\"~:a\"]],\"kind\",[\"~#tolglow/ratio.v1\",[\"1\",\"3\"]],\"~:color\",[\"~#thi.ng.color/hsla.v1\",[\"^ \",\"~:h\",0.125,\"~:s\",0.5,\"~:l\",0.75,\"~:a\",0.25]]]"
        value (wire/decode sent)]
    (is (= {:kind :demo :groups #{:a :b} "kind" 1/3 :color (color/hsla 0.125 0.5 0.75 0.25)} value))
    (is (= 0.25 (color/alpha (:color value))))
    (is (= color/BLUE (wire/decode "[\"~#thi.ng.color/rgba.v1\",[\"^ \",\"~:r\",0,\"~:g\",0,\"~:b\",1,\"~:a\",1]]")))))

(deftest shared-color-models-retain-protocols-components-and-extension-entries
  (doseq [c [(color/rgba 0.1 0.2 0.3 0)
             (color/hsla 0.7 0.6 0.5 0.4)
             (color/hsva 0.25 1 0.5 0.75)
             (color/cmyka 0.1 0.2 0.3 0.4 0.5)
             (color/int24 0xff8030) (color/int32 -1) (color/int32 4294967295)
             (color/css "#ff8800")]]
    (let [source (assoc c :cue/id :wash)
          result (wire/decode (wire/encode source))]
      (is (= (type c) (type result)))
      (is (= source result))
      (is (= @(color/as-rgba c) @(color/as-rgba result))))))

(deftest malformed-known-tags-cannot-construct-broken-values
  (doseq [text ["[\"~#thi.ng.color/hsla.v1\",[\"^ \",\"~:a\",1]]"
                "[\"~#thi.ng.color/int24.v1\",[\"^ \",\"~:col\",16777216]]"
                "[\"~#tolglow/ratio.v1\",[\"1\",\"0\"]]"
                "[\"~#tolglow/ratio.v1\",[\"1\",\"3\",\"2\"]]"]]
    (is (thrown? Exception (wire/decode text))))
  (is (= 1/3 (wire/decode (wire/encode 1/3)))))
