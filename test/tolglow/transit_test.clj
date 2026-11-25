(ns tolglow.transit-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.walk :as walk]
            [thi.ng.color.core :as color]
            [tolglow.transit :as wire])
  (:import [clojure.lang PersistentQueue]
           [java.nio ByteBuffer ByteOrder]
           [java.time Instant]
           [org.jogamp.vecmath Point3d Vector3d]))

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

(defn- native-sample []
  {:aim (Point3d. 1.5 -2.0 4.25)
   :direction (Vector3d. 0.0 0.0 -1.0)
   :at (Instant/ofEpochSecond 1764064800 123456789)
   :stamp (java.util.Date. 1764064800123)
   :dmx (byte-array [0 1 127 -128 -1])
   :packet (.order (ByteBuffer/wrap (byte-array [1 2 3 -1])) ByteOrder/LITTLE_ENDIAN)
   :shorts (short-array [-32768 0 32767])
   :chars (char-array [\a (char 0xffff)])
   :ints (int-array [Integer/MIN_VALUE -1 Integer/MAX_VALUE])
   :longs (long-array [Long/MIN_VALUE -1 9007199254740993 Long/MAX_VALUE])
   :floats (float-array [0.1 -0.0 Float/MAX_VALUE Float/MIN_VALUE])
   :doubles (double-array [0.1 -0.0 Double/MAX_VALUE Double/MIN_VALUE])
   :pending (conj PersistentQueue/EMPTY :a [:b 1] #{2})
   :events [{:type :cue/launch :emitter "rig/spot" :at (Instant/ofEpochSecond -1 999999999)}]})

(defn- comparable
  "Value semantics for arrays, buffers and queues, which do not compare by value."
  [value]
  (walk/prewalk
   (fn [v]
     (cond (instance? ByteBuffer v)
           [:buffer (str (.order ^ByteBuffer v))
            (let [d (.duplicate ^ByteBuffer v) a (byte-array (.remaining d))] (.get d a) (mapv str a))]
           (and (some? v) (.isArray (class v))) [(str (.getComponentType (class v))) (mapv str v)]
           (instance? PersistentQueue v) [:queue (vec v)]
           :else v))
   value))

(defn- jvm-type
  "Class, except that a decoded buffer is read-only and so a different subclass."
  [v]
  (if (instance? ByteBuffer v) ByteBuffer (class v)))

(deftest native-values-round-trip-with-their-jvm-types
  (let [source (native-sample)
        result (wire/decode (wire/encode source))]
    (is (= (comparable source) (comparable result)))
    (doseq [k (keys source)]
      (is (= (jvm-type (get source k)) (jvm-type (get result k))) (str k)))
    (is (= (:at source) (:at result)))
    (is (= (Instant/ofEpochSecond -1 999999999) (get-in result [:events 0 :at])))
    (is (.isReadOnly ^ByteBuffer (:packet result)))
    (is (thrown? java.nio.ReadOnlyBufferException (.put ^ByteBuffer (:packet result) (byte 0))))))

(deftest inbound-commands-receive-immutable-numbers
  (let [browser {:op :param/set :levels [0.25 -0.0] :steps [-1 7]}
        decoded (wire/decode-command
                 (str "[\"^ \",\"~:op\",\"~:param/set\","
                      "\"~:levels\",[\"~#tolglow/float64-array.v1\",\"~bAAAAAAAA0D8AAAAAAAAAgA==\"],"
                      "\"~:steps\",[\"~#tolglow/int32-array.v1\",\"~b/////wcAAAA=\"],"
                      "\"~:u32\",[\"~#tolglow/uint32-array.v1\",\"~b/////w==\"],"
                      "\"~:u16\",[\"~#tolglow/uint16-array.v1\",\"~b//8=\"],"
                      "\"~:u8c\",[\"~#tolglow/uint8-clamped-array.v1\",\"~b/w==\"],"
                      "\"~:u64\",[\"~#tolglow/uint64-array.v1\",\"~b//////////8=\"],"
                      "\"~:dmx\",\"~bAP8=\",\"~:at\",[\"~#tolglow/instant.v1\",[\"1\",2]]]"))]
    (is (= browser (select-keys decoded (keys browser))))
    (is (= [0.25 -0.0] (:levels decoded)))
    (is (= [4294967295] (:u32 decoded)))
    (is (= [65535] (:u16 decoded)))
    (is (= [255] (:u8c decoded)))
    (is (= [18446744073709551615N] (:u64 decoded)))
    (is (every? vector? (map decoded [:levels :steps :u32 :u16 :u8c :u64])))
    (is (= [0 -1] (vec (:dmx decoded))) "standard binary stays byte[]")
    (is (= (Instant/ofEpochSecond 1 2) (:at decoded))))
  (is (thrown-with-msg? Exception #"Byte buffers are not accepted"
                        (wire/decode-command (wire/encode {:packet (ByteBuffer/wrap (byte-array [1]))})))))

(deftest browser-reencoded-native-values-return-to-jvm-types
  ;; The ClojureScript reader decoded native-sample and its writer produced this
  ;; text (tolglow.studio.transit-test/browser-native). Integral doubles lose
  ;; their decimal point in JavaScript; the vector reader restores doubles.
  (let [sent "[\"^ \",\"~:aim\",[\"~#org.jogamp.vecmath/point3d.v1\",[1.5,-2,4.25]],\"~:longs\",[\"~#tolglow/int64-array.v1\",\"~bAAAAAAAAAID//////////wEAAAAAACAA/////////38=\"],\"~:stamp\",\"~m1764064800123\",\"~:events\",[[\"^ \",\"~:type\",\"~:cue/launch\",\"~:emitter\",\"rig/spot\",\"~:at\",[\"~#tolglow/instant.v1\",[\"-1\",999999999]]]],\"~:pending\",[\"~#tolglow/queue.v1\",[\"~:a\",[\"~:b\",1],[\"~#set\",[2]]]],\"~:doubles\",[\"~#tolglow/float64-array.v1\",\"~bmpmZmZmZuT8AAAAAAAAAgP///////+9/AQAAAAAAAAA=\"],\"~:dmx\",\"~bAAF/gP8=\",\"~:chars\",[\"~#tolglow/uint16-array.v1\",\"~bYQD//w==\"],\"~:ints\",[\"~#tolglow/int32-array.v1\",\"~bAAAAgP////////9/\"],\"^9\",[\"^:\",[\"1764064800\",123456789]],\"~:shorts\",[\"~#tolglow/int16-array.v1\",\"~bAIAAAP9/\"],\"~:direction\",[\"~#org.jogamp.vecmath/vector3d.v1\",[0,0,-1]],\"~:packet\",[\"~#tolglow/byte-buffer.v1\",[\"little\",\"~bAQID/w==\"]],\"~:floats\",[\"~#tolglow/float32-array.v1\",\"~bzczMPQAAAID//39/AQAAAA==\"]]"
        source (native-sample)
        result (wire/decode sent)]
    (is (= (comparable source) (comparable result)))
    (doseq [k (keys source)]
      (is (= (jvm-type (get source k)) (jvm-type (get result k))) (str k)))
    (is (= -1 (aget ^bytes (:dmx result) 4)) "browser 255 is JVM byte -1")))

(deftest unsigned-browser-arrays-keep-their-bits-on-the-jvm
  (is (= [-1] (vec (wire/decode "[\"~#tolglow/uint32-array.v1\",\"~b/////w==\"]"))))
  (is (= [-128 127] (vec (wire/decode "[\"~#tolglow/int8-array.v1\",\"~bgH8=\"]"))))
  (is (= (class (int-array 0)) (class (wire/decode "[\"~#tolglow/uint32-array.v1\",\"~b/////w==\"]")))))

(deftest byte-buffer-sends-remaining-bytes-without-moving-the-source
  (let [source (doto (ByteBuffer/allocateDirect 6) (.put (byte-array [9 8 7 6 5 4])) (.position 2) (.limit 5))
        ^ByteBuffer result (wire/decode (wire/encode source))]
    (is (= [2 5] [(.position source) (.limit source)]))
    (is (= ByteOrder/BIG_ENDIAN (.order result)))
    (is (= [0 3 false true] [(.position result) (.limit result) (.isDirect result) (.isReadOnly result)]))
    (is (= source result))
    (is (= 1 (.get ^ByteBuffer (wire/decode (wire/encode (.asReadOnlyBuffer (ByteBuffer/wrap (byte-array [1]))))))))))

(deftest native-tags-reject-malformed-representations
  (doseq [text ["[\"~#org.jogamp.vecmath/point3d.v1\",[1,2]]"
                "[\"~#org.jogamp.vecmath/vector3d.v1\",[1,\"2\",3]]"
                "[\"~#tolglow/instant.v1\",[\"1\",1000000000]]"
                "[\"~#tolglow/instant.v1\",[1,0]]"
                "[\"~#tolglow/instant.v1\",[\"01\",0]]"
                "[\"~#tolglow/byte-buffer.v1\",[\"middle\",\"~bAQ==\"]]"
                "[\"~#tolglow/int16-array.v1\",\"~bAQID\"]"
                "[\"~#tolglow/float64-array.v1\",[1,2]]"
                "[\"~#tolglow/queue.v1\",\"x\"]"]]
    (is (thrown? Exception (wire/decode text)) text)))
