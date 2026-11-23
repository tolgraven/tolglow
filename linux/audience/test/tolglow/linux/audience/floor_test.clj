(ns tolglow.linux.audience.floor-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.linux.audience.floor :as floor]
            [tolglow.linux.audience.main :as main]
            [tolglow.linux.audience.stage :as stage]
            [tolglow.linux.audience.wire :as wire]
            [tolglow.sensors.osc :as osc])
  (:import [com.google.zxing BinaryBitmap RGBLuminanceSource]
           [com.google.zxing.common HybridBinarizer]
           [com.google.zxing.qrcode QRCodeReader]
           [javax.imageio ImageIO]
           [java.math BigInteger]
           [java.security KeyFactory KeyPair]
           [java.security.interfaces ECPublicKey]
           [java.security.spec ECPrivateKeySpec ECPublicKeySpec]
           [java.util Arrays HexFormat]))

(defn unhex [s] (.parseHex (HexFormat/of) s))

(deftest cryptokit-wire-is-compatible-with-independent-p256-aes-gcm-vectors
  ;; Python cryptography: stage scalar 1, phone scalar 2, challenge bytes 0..15.
  (let [params (.getParams ^ECPublicKey (.getPublic (wire/keypair))) factory (KeyFactory/getInstance "EC")
        own (KeyPair. (.generatePublic factory (ECPublicKeySpec. (.getGenerator params) params))
                      (.generatePrivate factory (ECPrivateKeySpec. BigInteger/ONE params)))
        other (unhex (str "047cf27b188d034f7e8a52380304b51ac3c08969e277f21b35a60b48fc47669978"
                          "07775510db8ed040293d9ac69f7430dbba7dade63ce982299e04b79d227873d1"))
        key (wire/session-key own other (byte-array (range 16)))
        base {:serial 43 :steps 8 :effort 0.6 :pulse 88}
        v1 (unhex "0102030405060708090a0b0c998c72fe1645dd65ee4de0b5f34688374e926ac88cba5aeb04f26f")
        v2 (unhex (str "02030405060708090a0b0c0df8cf7ee070c4e4534db2c6f432f7a6ca6f81a53244"
                       "0833e3994a7670fa2017071e1e9c164945018a0eb8f8e474260aea1ce3fb"))]
    (is (= "f4b33ec3b3b48be4f8ae648ed134e778b60cc1ca72e7bf588d41bff2b1b9b7e0" (.formatHex (HexFormat/of) key)))
    (is (= base (wire/open-sample key v1)))
    (is (= (assoc base :motion (zipmap wire/axes [1.5 -2.0 0.0 5.25 -3.0 1.0])) (wire/open-sample key v2)))
    (aset-byte v2 16 (unchecked-byte (bit-xor 1 (aget v2 16))))
    (is (thrown? javax.crypto.AEADBadTagException (wire/open-sample key v2)))
    (is (thrown? Exception (wire/sample (byte-array 35))))
    (is (thrown? Exception (wire/open-sample key (byte-array 40))))))

(deftest fresh-challenges-mtu-offsets-and-capacity
  (let [stage (stage/create) first (stage/identity! stage :a 0 1.0)
        nonce (Arrays/copyOfRange first 65 81)]
    (is (= 81 (alength first)))
    (is (Arrays/equals nonce (stage/identity! stage :a 65 1.2)))
    (is (not (Arrays/equals nonce (Arrays/copyOfRange (stage/identity! stage :a 0 1.3) 65 81))))
    (is (thrown? Exception (stage/identity! stage :b 82 1.0)))
    (is (thrown? Exception (stage/hello! stage :a (wire/public-key (wire/keypair)) 10.0)))
    (doseq [id (range 128)] (stage/identity! stage id 0 10.0))
    (is (thrown? Exception (stage/identity! stage :extra 0 10.0)))
    (is (= 81 (alength (stage/identity! stage :extra 0 19.0))))))

(deftest receipt-time-average-step-credit-expiry-and-osc-batches
  (let [base {:steps 0xffffffff :serial 0xffffffff :effort 0.4 :pulse 90}
        floor (-> floor/initial (floor/accept :a base 1)
                  (floor/accept :b {:steps 40 :serial 3 :effort 0.8 :motion (zipmap wire/axes (repeat 1.5))} 1)
                  (floor/accept :a (assoc base :serial 0 :steps 0) 1.2))
        [next levels steps] (floor/drain floor 2)
        config {:source "audience" :token (apply str (repeat 24 "g"))}
        chunks (map osc/decode (main/packets config "room" 2 2 levels 12))]
    (is (= 1 steps))
    (is (< (abs (- 0.6 (:dance-effort levels))) 1e-9))
    (is (= 2 (:audience-count levels))) (is (= 1 (:motion-count levels)))
    (is (= 90.0 (:pulse-bpm levels))) (is (= 1.5 (:rotation-z levels)))
    (is (= 0 (:pending next)))
    (is (thrown? Exception (floor/accept floor :a (assoc base :serial 1 :steps 1000) 2)))
    (is (thrown? Exception (floor/accept floor :a (assoc base :serial 0 :steps 0) 2)))
    (is (= 0 (:audience-count (second (floor/drain next 4)))))
    (is (= [2 3] (map :sample-id chunks)))
    (is (= [8 4] (map (comp count :gestures) chunks)))
    (is (= 12 (count (distinct (map :id (mapcat :gestures chunks))))))))

(deftest onset-flushes-preserve-the-five-hz-levels-and-count-once
  (let [stage (stage/create)
        sample {:serial 1 :steps 0 :effort 0.2}
        update-floor #(swap! (:state stage) update :floor floor/accept :a %1 %2)
        _ (update-floor sample 1.0)
        initial (main/publication! stage {} 1.0)
        _ (update-floor (assoc sample :serial 2 :steps 1 :effort 0.8) 1.01)
        onset (main/publication! stage initial 1.025)
        quiet (main/publication! stage onset 1.05)
        regular (main/publication! stage quiet 1.201)
        expired (main/publication! stage regular 4.0)]
    (is (= [0.2 0.2 0.8] (map #(get-in % [:levels :dance-effort]) [initial onset regular])))
    (is (:send? onset)) (is (= 1 (:steps onset)))
    (is (not (:send? quiet))) (is (zero? (:steps regular)))
    (is (:send? expired)) (is (zero? (get-in expired [:levels :audience-count])))))


(deftest printed-invitation-contains-the-ephemeral-stage-fingerprint
  (let [stage (stage/create) file (java.io.File/createTempFile "tolglow-gatt-invite" ".png")]
    (try
      (let [url (main/write-invitation! "https://stage.example" (:room stage) file)
            image (ImageIO/read file) w (.getWidth image) h (.getHeight image)
            rgb (RGBLuminanceSource. w h (.getRGB image 0 0 w h nil 0 w))
            decoded (.decode (QRCodeReader.) (BinaryBitmap. (HybridBinarizer. rgb)))]
        (is (= (str "https://stage.example/audience/" (:room stage)) url (.getText decoded)))
        (is (thrown? Exception (main/invitation "http://stage.example" (:room stage))))
        (is (thrown? Exception (main/invitation "https://stage.example/path" (:room stage)))))
      (finally (.delete file)))))
