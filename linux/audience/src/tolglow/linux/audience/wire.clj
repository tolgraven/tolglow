(ns tolglow.linux.audience.wire
  "The stage identity, CryptoKit-compatible ECDH envelope and sensor wire types."
  (:import [java.math BigInteger]
           [java.nio ByteBuffer]
           [java.nio.charset StandardCharsets]
           [java.security KeyFactory KeyPair KeyPairGenerator MessageDigest SecureRandom]
           [java.security.interfaces ECPublicKey]
           [java.security.spec ECGenParameterSpec ECPoint ECPublicKeySpec]
           [java.util Arrays HexFormat]
           [javax.crypto Cipher KeyAgreement Mac]
           [javax.crypto.spec GCMParameterSpec SecretKeySpec]))

(def uuids {:service "7d609c70-47ec-4457-8568-d443c43b9701"
            :identity "7d609c70-47ec-4457-8568-d443c43b9702"
            :hello "7d609c70-47ec-4457-8568-d443c43b9703"
            :samples "7d609c70-47ec-4457-8568-d443c43b9704"})

(def axes [:accel-x :accel-y :accel-z :rotation-x :rotation-y :rotation-z])

(defn keypair []
  (.generateKeyPair (doto (KeyPairGenerator/getInstance "EC")
                      (.initialize (ECGenParameterSpec. "secp256r1")))))

(defn join ^bytes [^bytes a ^bytes b]
  (let [out (Arrays/copyOf a (+ (alength a) (alength b)))]
    (System/arraycopy b 0 out (alength a) (alength b))
    out))

(defn- unsigned32 ^bytes [^BigInteger n]
  (let [raw (.toByteArray n) out (byte-array 32) length (min 32 (alength raw))]
    (System/arraycopy raw (- (alength raw) length) out (- 32 length) length)
    out))

(defn public-key ^bytes [^KeyPair stage]
  (let [point (.getW ^ECPublicKey (.getPublic stage))]
    (join (byte-array [4]) (join (unsigned32 (.getAffineX point)) (unsigned32 (.getAffineY point))))))

(defn room [stage]
  (.formatHex (HexFormat/of) (.digest (MessageDigest/getInstance "SHA-256") (public-key stage))))

(defn challenge ^bytes [^SecureRandom random]
  (let [nonce (byte-array 16)] (.nextBytes random nonce) nonce))

(defn- hmac ^bytes [^bytes key ^bytes data]
  (.doFinal (doto (Mac/getInstance "HmacSHA256") (.init (SecretKeySpec. key "HmacSHA256"))) data))

(defn session-key ^bytes [^KeyPair stage ^bytes remote ^bytes nonce]
  (when-not (and (= 16 (alength nonce)) (= 65 (alength remote)) (= 4 (aget remote 0)))
    (throw (ex-info "Invalid P-256 hello" {})))
  (let [field #(BigInteger. 1 (Arrays/copyOfRange remote (int %) (int (+ % 32))))
        point (ECPoint. (field 1) (field 33))
        params (.getParams ^ECPublicKey (.getPublic stage))
        peer (.generatePublic (KeyFactory/getInstance "EC") (ECPublicKeySpec. point params))
        shared (.generateSecret (doto (KeyAgreement/getInstance "ECDH")
                                  (.init (.getPrivate stage))
                                  (.doPhase peer true)))
        salt (.getBytes "Tolglow audience v1" StandardCharsets/UTF_8)]
    (hmac (hmac salt shared) (join nonce (byte-array [1])))))

(defn sample [^bytes data]
  (let [b (ByteBuffer/wrap data)
        size (.remaining b)
        version (when (pos? size) (.get b))]
    (when-not (or (and (= version 1) (= size 11)) (and (= version 2) (= size 35)))
      (throw (ex-info "Unknown audience sensor frame" {})))
    (let [serial (Integer/toUnsignedLong (.getInt b)) steps (Integer/toUnsignedLong (.getInt b))
          effort (/ (Byte/toUnsignedInt (.get b)) 255.0) pulse (Byte/toUnsignedInt (.get b))
          values (when (= version 2) (mapv (fn [_] (double (.getFloat b))) axes))]
      (when-not (and (or (zero? pulse) (<= 30 pulse 240))
                     (every? #(and (Double/isFinite %) (<= (abs %) 256)) values))
        (throw (ex-info "Invalid motion or heart-rate measurement" {})))
      (cond-> {:serial serial :steps steps :effort effort}
        (pos? pulse) (assoc :pulse pulse)
        values (assoc :motion (zipmap axes values))))))

(defn open-sample [^bytes key ^bytes packet]
  (when-not (#{39 63} (alength packet)) (throw (ex-info "Invalid sealed sensor length" {})))
  (let [cipher (doto (Cipher/getInstance "AES/GCM/NoPadding")
                 (.init Cipher/DECRYPT_MODE (SecretKeySpec. key "AES") (GCMParameterSpec. 128 packet 0 12)))]
    (sample (.doFinal cipher packet 12 (- (alength packet) 12)))))
