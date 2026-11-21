(ns tolglow.studio.output-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.studio.output :as output]))

(defn unsigned [buffer] (mapv #(bit-and 255 %) buffer))

(deftest artdmx-wire-layout-and-universe-boundaries
  (doseq [[universe sequence low high] [[1 0 0 0] [256 1 255 0] [257 255 0 1] [32768 42 255 127]]]
    (is (= [65 114 116 45 78 101 116 0 0 80 0 14 sequence 0 low high 0 2 0 255]
           (unsigned (output/artdmx-packet universe sequence [0 255])))))
  (let [packet (unsigned (output/artdmx-packet 2 17 (byte-array 512)))]
    (is (= 530 (count packet)))
    (is (= [2 0] (subvec packet 16 18)))
    (is (every? zero? (subvec packet 18)))))

(deftest owned-byte-payload-is-identical-to-validated-vector-input
  (let [source (byte-array (map unchecked-byte (range 512)))
        before (vec source)
        from-bytes (output/artdmx-packet 32768 255 source)
        from-vector (output/artdmx-packet 32768 255 (mapv #(bit-and 255 %) source))]
    (is (= (vec from-vector) (vec from-bytes)))
    (is (= before (vec source)))
    (aset-byte source 0 (byte 99))
    (is (= 0 (aget ^bytes from-bytes 18)))
    (aset-byte ^bytes from-bytes 19 (byte 88))
    (is (= 1 (aget source 1)))))

(deftest invalid-packet-inputs-reject-before-returning-a-datagram
  (doseq [[universe sequence values] [[0 0 [0 0]] [32769 0 [0 0]] [1.0 0 [0 0]]
                                      [1 -1 [0 0]] [1 256 [0 0]] [1 0.5 [0 0]]
                                      [1 0 []] [1 0 [0]] [1 0 [0 1 2]] [1 0 (byte-array 513)]
                                      [1 0 (vec (repeat 514 0))] [1 0 [-129 0]] [1 0 [0 256]]
                                      [1 0 [0 0.5]] [1 0 [0 Double/NaN]]]]
    (is (thrown? clojure.lang.ExceptionInfo (output/artdmx-packet universe sequence values)))))

(deftest loopback-transport-advances-sequence-even-when-frame-identity-repeats
  (with-open [receiver (java.net.DatagramSocket. 0 (java.net.InetAddress/getByName "127.0.0.1"))]
    (.setSoTimeout receiver 1000)
    (let [sink (output/open-artnet! {:host "127.0.0.1" :port (.getLocalPort receiver)})
          receive (fn []
                    (let [packet (java.net.DatagramPacket. (byte-array 530) 530)]
                      (.receive receiver packet)
                      (unsigned (take (.getLength packet) (.getData packet)))))]
      (try
        ((:send! sink) {32768 (byte-array [(byte 0) (unchecked-byte 255)])} 7)
        (let [first-packet (receive)]
          (is (= 1 (nth first-packet 12)))
          (is (= [255 127] (subvec first-packet 14 16)))
          (is (= [0 255] (subvec first-packet 18))))
        ((:send! sink) {32768 (byte-array 2)} 7)
        (is (= 2 (nth (receive) 12)))
        (finally ((:close! sink)))))))
