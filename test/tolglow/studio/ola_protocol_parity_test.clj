(ns tolglow.studio.ola-protocol-parity-test
  "Independent oracle from the actual ola-clojure generated protobuf classes.
  Runs in the parity lane; production output does not load that client's globals."
  (:require [clojure.test :refer [deftest is]]
            [tolglow.studio.output :as output])
  (:import [com.google.protobuf ByteString]
           [ola.proto Ola$DmxData]
           [ola.rpc Rpc$RpcMessage Rpc$Type]
           [java.nio ByteBuffer ByteOrder]))

(deftest native-ola-encoding-matches-the-existing-generated-protobuf-codec
  (doseq [universe [0 1 127 128 255 256 32768 65535]
          request-id [0 127 128 16383 16384 65535 4294967295]
          channels [1 2 127 128 511 512]]
    (let [values (byte-array (map unchecked-byte (range channels)))
          dmx (-> (Ola$DmxData/newBuilder)
                  (.setUniverse universe) (.setData (ByteString/copyFrom values)) .build)
          rpc (-> (Rpc$RpcMessage/newBuilder)
                  (.setType Rpc$Type/STREAM_REQUEST)
                  (.setId (unchecked-int request-id))
                  (.setName "StreamDmxData") (.setBuffer (.toByteString dmx)) .build)
          body (.toByteArray rpc)
          expected (-> (ByteBuffer/allocate (+ 4 (alength body)))
                       (.order (ByteOrder/nativeOrder))
                       (.putInt (bit-or 0x10000000 (alength body)))
                       (.put body) .array)]
      (is (= (vec expected) (vec (output/ola-stream-packet universe request-id values)))
          (str "universe=" universe " request=" request-id " channels=" channels)))))
