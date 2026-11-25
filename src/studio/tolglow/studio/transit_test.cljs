(ns tolglow.studio.transit-test
  (:require [cljs.test :refer-macros [deftest is testing]]
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

;; Written by tolglow.transit-test/native-sample on the JVM.
(def ^:private jvm-native
  "[\"^ \",\"~:aim\",[\"~#org.jogamp.vecmath/point3d.v1\",[1.5,-2.0,4.25]],\"~:longs\",[\"~#tolglow/int64-array.v1\",\"~bAAAAAAAAAID//////////wEAAAAAACAA/////////38=\"],\"~:stamp\",\"~m1764064800123\",\"~:events\",[[\"^ \",\"~:type\",\"~:cue/launch\",\"~:emitter\",\"rig/spot\",\"~:at\",[\"~#tolglow/instant.v1\",[\"-1\",999999999]]]],\"~:pending\",[\"~#tolglow/queue.v1\",[\"~:a\",[\"~:b\",1],[\"~#set\",[2]]]],\"~:doubles\",[\"~#tolglow/float64-array.v1\",\"~bmpmZmZmZuT8AAAAAAAAAgP///////+9/AQAAAAAAAAA=\"],\"~:dmx\",\"~bAAF/gP8=\",\"~:chars\",[\"~#tolglow/uint16-array.v1\",\"~bYQD//w==\"],\"~:ints\",[\"~#tolglow/int32-array.v1\",\"~bAAAAgP////////9/\"],\"^9\",[\"^:\",[\"1764064800\",123456789]],\"~:shorts\",[\"~#tolglow/int16-array.v1\",\"~bAIAAAP9/\"],\"~:direction\",[\"~#org.jogamp.vecmath/vector3d.v1\",[0.0,0.0,-1.0]],\"~:packet\",[\"~#tolglow/byte-buffer.v1\",[\"little\",\"~bAQID/w==\"]],\"~:floats\",[\"~#tolglow/float32-array.v1\",\"~bzczMPQAAAID//39/AQAAAA==\"]]")

;; This browser re-encoding is decoded by the JVM test and compared with the original.
(def ^:private browser-native
  "[\"^ \",\"~:aim\",[\"~#org.jogamp.vecmath/point3d.v1\",[1.5,-2,4.25]],\"~:longs\",[\"~#tolglow/int64-array.v1\",\"~bAAAAAAAAAID//////////wEAAAAAACAA/////////38=\"],\"~:stamp\",\"~m1764064800123\",\"~:events\",[[\"^ \",\"~:type\",\"~:cue/launch\",\"~:emitter\",\"rig/spot\",\"~:at\",[\"~#tolglow/instant.v1\",[\"-1\",999999999]]]],\"~:pending\",[\"~#tolglow/queue.v1\",[\"~:a\",[\"~:b\",1],[\"~#set\",[2]]]],\"~:doubles\",[\"~#tolglow/float64-array.v1\",\"~bmpmZmZmZuT8AAAAAAAAAgP///////+9/AQAAAAAAAAA=\"],\"~:dmx\",\"~bAAF/gP8=\",\"~:chars\",[\"~#tolglow/uint16-array.v1\",\"~bYQD//w==\"],\"~:ints\",[\"~#tolglow/int32-array.v1\",\"~bAAAAgP////////9/\"],\"^9\",[\"^:\",[\"1764064800\",123456789]],\"~:shorts\",[\"~#tolglow/int16-array.v1\",\"~bAIAAAP9/\"],\"~:direction\",[\"~#org.jogamp.vecmath/vector3d.v1\",[0,0,-1]],\"~:packet\",[\"~#tolglow/byte-buffer.v1\",[\"little\",\"~bAQID/w==\"]],\"~:floats\",[\"~#tolglow/float32-array.v1\",\"~bzczMPQAAAID//39/AQAAAA==\"]]")

(defn- elements [typed] (mapv #(if (number? %) % (str %)) (array-seq (js/Array.from typed))))

(deftest jvm-native-values-arrive-as-browser-types
  (let [value (wire/decode jvm-native)]
    (is (= (wire/->Point3d 1.5 -2 4.25) (:aim value)))
    (is (= (wire/->Vector3d 0 0 -1) (:direction value)))
    (testing "an instant keeps nanoseconds and floors to its millisecond"
      (is (= (wire/->Instant 1764064800 123456789) (:at value)))
      (is (inst? (:at value)))
      (is (= 1764064800123 (inst-ms (:at value))))
      (is (= -1 (inst-ms (get-in value [:events 0 :at])))))
    (is (= 1764064800123 (.getTime (:stamp value))))
    (testing "DMX bytes stay unsigned"
      (is (instance? js/Uint8Array (:dmx value)))
      (is (= [0 1 127 128 255] (elements (:dmx value))))
      (is (= :little-endian (:order (:packet value))))
      (is (= [1 2 3 255] (elements (:bytes (:packet value))))))
    (is (instance? js/Int16Array (:shorts value)))
    (is (= [-32768 0 32767] (elements (:shorts value))))
    (is (= [97 65535] (elements (:chars value))))
    (is (= [-2147483648 -1 2147483647] (elements (:ints value))))
    (is (instance? js/BigInt64Array (:longs value)))
    (is (= ["-9223372036854775808" "-1" "9007199254740993" "9223372036854775807"] (elements (:longs value))))
    (is (= (js/Math.fround 0.1) (aget (:floats value) 0)))
    (is (= [0.1 -0 1.7976931348623157e308 5e-324] (elements (:doubles value))))
    (is (instance? PersistentQueue (:pending value)))
    (is (= [:a [:b 1] #{2}] (vec (:pending value))))
    (is (= browser-native (wire/encode value)))))

(deftest browser-native-values-round-trip
  (let [typed {:i8 (js/Int8Array. #js [-128 127]) :u8 (js/Uint8Array. #js [0 255])
               :u8c (js/Uint8ClampedArray. #js [0 255]) :i16 (js/Int16Array. #js [-1])
               :u16 (js/Uint16Array. #js [65535]) :i32 (js/Int32Array. #js [-1])
               :u32 (js/Uint32Array. #js [4294967295]) :f32 (js/Float32Array. #js [0.5 js/NaN])
               :f64 (js/Float64Array. #js [0.1 js/Infinity])
               :i64 (js/BigInt64Array.from #js [(js/BigInt "-9223372036854775808")])
               :u64 (js/BigUint64Array.from #js [(js/BigInt "18446744073709551615")])
               :offset (js/Int32Array. (.-buffer (js/Uint8Array. #js [9 1 0 0 0 2 0 0 0])) 4 1)}
        result (wire/decode (wire/encode typed))]
    (doseq [[k v] typed]
      (is (= (type v) (type (get result k))) (str k))
      (is (= (map str (elements v)) (map str (elements (get result k)))) (str k))))
  (let [source {:at (wire/->Instant -62135596800 1) :stamp (js/Date. 1764064800123)
                :aim (wire/->Point3d 1 2 3) :pending (conj #queue [] 1 2)}
        result (wire/decode (wire/encode source))]
    (is (= source result))
    (is (instance? js/Date (:stamp result))))
  (let [view (wire/->ByteView :big-endian (js/Uint8Array. #js [255 0]))
        result (wire/decode (wire/encode view))]
    (is (= :big-endian (:order result)))
    (is (= [255 0] (elements (:bytes result))))
    (is (= view result))
    (is (= (hash view) (hash result))))
  (doseq [text ["[\"~#tolglow/instant.v1\",[\"8640000000000\",0]]"
                "[\"~#tolglow/instant.v1\",[\"-8640000000001\",0]]"
                "[\"~#tolglow/int32-array.v1\",\"~bAQI=\"]"
                "[\"~#org.jogamp.vecmath/point3d.v1\",[1,2]]"]]
    (is (thrown? js/Error (wire/decode text)) text)))

(deftest byte-views-are-values-in-app-db
  (let [text "[\"~#tolglow/byte-buffer.v1\",[\"little\",\"~bAQID/w==\"]]"
        a (wire/decode text) b (wire/decode text)]
    (is (not (identical? (:bytes a) (:bytes b))))
    (is (= a b))
    (is (= (hash a) (hash b)))
    (is (= #{a} #{b}))
    (is (not= a (wire/->ByteView :big-endian (:bytes b))))
    (is (not= a (wire/->ByteView :little-endian (js/Uint8Array. #js [1 2 3]))))))

(deftest instants-compare-and-stay-inside-the-date-range
  (let [later (wire/->Instant 10 5) earlier (wire/->Instant 10 4) before-epoch (wire/->Instant -1 999999999)]
    (is (= [before-epoch earlier later] (sort [later before-epoch earlier])))
    (is (neg? (compare earlier later)))
    (is (zero? (compare later (wire/->Instant 10 5)))))
  (doseq [[text ms] [["[\"~#tolglow/instant.v1\",[\"8639999999999\",999999999]]" 8639999999999999]
                     ["[\"~#tolglow/instant.v1\",[\"-8640000000000\",0]]" -8640000000000000]]]
    (let [t (wire/decode text)]
      (is (= ms (inst-ms t)))
      (is (= ms (.getTime (js/Date. (inst-ms t))))))))

(deftest exact-falloff-curve-points-round-trip-as-typed-fractions
  (let [curve (wire/decode "[[0,1],[[\"~#tolglow/ratio.v1\",[\"1\",\"3\"]],[\"~#tolglow/ratio.v1\",[\"1\",\"2\"]]],[1,0]]")]
    (is (vector? curve))
    (is (wire/fraction? (get-in curve [1 0])))
    (is (= (/ 1 3) (wire/fraction-value (get-in curve [1 0]))))
    (is (= curve (wire/decode (wire/encode curve))))))
