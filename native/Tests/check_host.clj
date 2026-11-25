(ns tolglow-native.contract-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [tolglow.sensors.osc :as wire])
  (:import [java.net DatagramSocket DatagramPacket InetAddress]))

(def fixture-root "native/Core/Tests/TolglowSensorCoreTests/Fixtures/")

(def fixture (wire/decode-json (slurp (str fixture-root "observation.json"))))

(def packet (byte-array (map #(unchecked-byte (Integer/parseInt % 16))
                            (re-seq #".." (str/trim (slurp (str fixture-root "observation.osc.hex")))))))

(deftest native-protocol-fixture-is-admitted-by-real-host-codec
  (is (= fixture (wire/decode packet)))
  (is (= fixture (wire/decode (wire/encode fixture))))
  (is (= :primary-action (get-in fixture [:gestures 0 :type])))
  (is (= 12 (:sample-id fixture))))

(deftest malformed-native-datagrams-rejected
  (is (thrown? Exception (wire/decode (byte-array (concat packet [0 0 0 0])))))
  (let [bad (aclone packet)]
    (aset-byte bad 19 (byte 1))
    (is (thrown? Exception (wire/decode bad)))))

(deftest bounded-test-credential-and-values-enforced
  (is (thrown? Exception (wire/encode (assoc fixture :pairing-token "short"))))
  (is (thrown? Exception (wire/encode (assoc-in fixture [:values :roll] Double/NaN)))))

(deftest actual-owned-loopback-udp-fixture
  (let [received (promise)
        listener (wire/open! {:host "127.0.0.1" :port 0 :receive! #(deliver received %)})]
    (try
      (with-open [sender (DatagramSocket.)]
        (.send sender (DatagramPacket. packet (alength packet)
                                      (InetAddress/getByName "127.0.0.1") (:port listener))))
      (is (= fixture (deref received 2000 ::timeout)))
      (finally ((:close! listener))))
    (is (false? (:running? ((:status listener)))))))

(let [result (run-tests 'tolglow-native.contract-test)]
  (shutdown-agents)
  (System/exit (+ (:fail result) (:error result))))
