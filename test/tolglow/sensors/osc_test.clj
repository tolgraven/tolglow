(ns tolglow.sensors.osc-test
  (:require [clojure.test :refer [deftest is]] [tolglow.sensors.osc :as osc])
  (:import [java.net DatagramSocket DatagramPacket InetAddress]))
(def observation {:version 1 :device-id "phone-å" :session-id "one" :sample-id 2 :time-ms 13.5
                  :values {:accel-x 1.25 :rotation-z -0.25} :gestures [{:id "tap" :type :primary-action :phase :pulse}]
                  :reference-frame "device" :pairing-token "fake-pairing-token-test"})

(deftest real-osc-utf8-and-json-roundtrip-preserves-numeric-values
  (let [^bytes bytes (osc/encode observation)]
    (is (zero? (mod (alength bytes) 4)))
    (is (= observation (osc/decode bytes)))
    (is (= observation (osc/decode-json (clojure.data.json/write-str observation))))))

(deftest malformed-packets-reject-before-returning-partial-data
  (let [^bytes packet (osc/encode observation), ^bytes bad-utf8 (aclone packet), trailing (byte-array (+ 4 (alength packet)))]
    (aset-byte bad-utf8 0 (unchecked-byte 255))
    (System/arraycopy packet 0 trailing 0 (alength packet))
    (doseq [^bytes bytes [(byte-array 0) bad-utf8 trailing (byte-array 60004)]]
      (is (thrown? clojure.lang.ExceptionInfo (osc/decode bytes))))
    (doseq [json ["[]" "{bad" "{\"version\":1}" (str (apply str (repeat 10 "[")) "0" (apply str (repeat 10 "]")))]]
      (is (thrown? clojure.lang.ExceptionInfo (osc/decode-json json))))))

(deftest typed-observations-reject-null-sensors-and-invalid-sequence-or-gesture
  (doseq [bad [(assoc observation :values {:accel-x nil})
               (assoc observation :sample-id 1.5) (assoc observation :time-ms Double/NaN)
               (assoc observation :pairing-token "short")
               (assoc observation :values {:bad/name 1})
               (assoc observation :gestures [{:id "finger" :type :raw-finger :phase :begin}])]]
    (is (thrown? clojure.lang.ExceptionInfo (osc/encode bad)))))

(deftest owned-loopback-receiver-accepts-valid-packet-after-invalid-and-closes
  (let [received (promise), receiver (osc/open! {:host "127.0.0.1" :port 0 :receive! #(deliver received %)})]
    (try
      (with-open [sender (DatagramSocket.)]
        (doseq [^bytes bytes [(byte-array 4) (osc/encode observation)]]
          (.send sender (DatagramPacket. bytes (alength bytes) (InetAddress/getByName "127.0.0.1") (int (:port receiver))))))
      (is (= observation (deref received 2000 ::timeout)))
      (is (= 1 (:rejected ((:status receiver)))))
      (finally ((:close! receiver))))
    (is (false? (:running? ((:status receiver)))))
    ((:close! receiver))
    (with-open [reused (DatagramSocket. (int (:port receiver)))] (is (= (:port receiver) (.getLocalPort reused))))))
