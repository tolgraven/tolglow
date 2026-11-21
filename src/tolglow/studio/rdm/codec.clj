(ns tolglow.studio.rdm.codec
  "The bounded OLA RPC/RDM wire subset. References and verification: docs/rdm.md."
  (:require [clojure.string :as str])
  (:import [java.io ByteArrayOutputStream]
           [java.nio ByteBuffer ByteOrder]
           [java.nio.charset StandardCharsets]))

(def max-message-bytes (* 1024 1024))
(def pids {:supported-parameters 0x50 :device-info 0x60 :model-description 0x80
           :manufacturer-label 0x81 :device-label 0x82 :software-label 0xc0
           :personality 0xe0 :personality-description 0xe1 :address 0xf0
           :sensor-definition 0x200 :sensor-value 0x201 :identify 0x1000})
(defn fail! [message data] (throw (ex-info message (assoc data :rdm/error :invalid-data))))
(defn uint? [bits n] (and (integer? n) (<= 0 n (dec (bit-shift-left 1 bits)))))
(defn uid [value]
  (when-not (and (string? value) (re-matches #"[0-9a-fA-F]{4}:[0-9a-fA-F]{8}" value))
    (fail! "RDM UID must be 1234:12345678" {:uid value}))
  (str/lower-case value))
(defn bytes-vector [value]
  (when-not (and (vector? value) (<= (count value) 231) (every? #(uint? 8 %) value))
    (fail! "RDM parameter data must contain 0..231 unsigned bytes" {})) value)
(defn u16 [data offset]
  (+ (bit-shift-left (nth data offset) 8) (nth data (inc offset))))
(defn uint16-bytes [n] [(bit-and 255 (bit-shift-right n 8)) (bit-and 255 n)])
(defn- sint16 [data offset] (let [v (u16 data offset)] (if (> v 32767) (- v 65536) v)))
(defn- text-bytes [s] (mapv #(bit-and 255 %) (.getBytes ^String s StandardCharsets/UTF_8)))
(defn- bytes-text [data] (String. (byte-array (map unchecked-byte data)) StandardCharsets/UTF_8))

(defn- varint! [^ByteArrayOutputStream out n]
  (loop [n (long n)]
    (.write out (int (bit-or (bit-and n 127) (if (> n 127) 128 0))))
    (when (> n 127) (recur (unsigned-bit-shift-right n 7)))))

(defn protobuf
  "Encode explicit [field wire-type value] triples; never accepts arbitrary objects."
  [fields]
  (let [out (ByteArrayOutputStream.)]
    (doseq [[field type value] fields]
      (varint! out (bit-or (bit-shift-left field 3) type))
      (case type
        0 (varint! out value)
        2 (let [v (if (string? value) (text-bytes value) value)]
            (varint! out (count v)) (doseq [b v] (.write out (int b))))
        5 (dotimes [i 4] (.write out (int (bit-and 255 (unsigned-bit-shift-right (long value) (* i 8))))))
        (fail! "Unsupported protobuf field type" {:wire-type type})))
    (mapv #(bit-and 255 %) (.toByteArray out))))

(defn parse-protobuf [data]
  (let [data (vec data) limit (count data)
        varint (fn [start]
                 (loop [i start shift 0 n 0]
                   (when (or (>= i limit) (> shift 63)) (fail! "Truncated or oversized protobuf integer" {}))
                   (let [b (nth data i) n (bit-or n (bit-shift-left (long (bit-and b 127)) shift))]
                     (if (zero? (bit-and b 128)) [n (inc i)] (recur (inc i) (+ shift 7) n)))))
        slice (fn [i n] (when (or (neg? n) (> (+ i n) limit)) (fail! "Truncated protobuf field" {}))
                [(subvec data i (+ i n)) (+ i n)])]
    (loop [i 0 fields {}]
      (if (= i limit) fields
        (let [[tag j] (varint i) field (unsigned-bit-shift-right tag 3) type (bit-and tag 7)
              _ (when (zero? field) (fail! "Invalid protobuf tag" {}))
              [value k] (case type
                          0 (varint j)
                          2 (let [[n k] (varint j)] (slice k n))
                          5 (let [[v k] (slice j 4)]
                              [(reduce + (map-indexed #(bit-shift-left (long %2) (* 8 %1)) v)) k])
                          1 (slice j 8)
                          (fail! "Unsupported protobuf wire type" {:wire-type type}))]
          (recur k (update fields field (fnil conj []) value)))))))

(defn field [fields n] (first (get fields n)))
(defn- uid-fields [value]
  (let [[manufacturer device] (str/split (uid value) #":")]
    [[1 0 (Long/parseLong manufacturer 16)] [2 5 (Long/parseLong device 16)]]))
(defn decode-uid [data]
  (let [m (parse-protobuf data) manufacturer (field m 1) device (field m 2)]
    (when-not (and (uint? 16 manufacturer) (uint? 32 device)) (fail! "Invalid OLA UID" {}))
    (format "%04x:%08x" manufacturer device)))

(defn validate-command [{:keys [universe uid sub-device pid data command] :as request}]
  (let [request (merge {:sub-device 0 :data [] :command :get} request)
        {:keys [sub-device data command]} request]
    (when-not (and (uint? 16 universe) (uint? 16 pid) (integer? sub-device) (<= 0 sub-device 512)
                   (#{:get :set} command))
      (fail! "RDM needs universe/PID 0..65535, subdevice 0..512 and GET or SET" {}))
    (assoc request :uid (tolglow.studio.rdm.codec/uid uid) :data (bytes-vector data))))

(defn request-body [method request]
  (case method
    "ForceDiscovery" (do (when-not (uint? 16 (:universe request)) (fail! "Invalid OLA universe" {}))
                         (protobuf [[1 0 (:universe request)] [2 0 (if (:full? request) 1 0)]]))
    "GetUIDs" (protobuf [[1 0 (:universe request)]])
    "RDMCommand" (let [{:keys [universe uid sub-device pid data command]} (validate-command request)]
                   (protobuf [[1 0 universe] [2 2 (protobuf (uid-fields uid))] [3 0 sub-device]
                              [4 0 pid] [5 2 data] [6 0 (if (= command :set) 1 0)]]))
    (fail! "Unsupported OLA method" {:method method})))

(defn rpc-packet [id method request]
  (let [body (protobuf [[1 0 1] [2 0 id] [3 2 method] [4 2 (request-body method request)]])
        buffer (doto (ByteBuffer/allocate (+ 4 (count body))) (.order (ByteOrder/nativeOrder)))]
    (.putInt buffer (bit-or 0x10000000 (count body)))
    (.put buffer (byte-array (map unchecked-byte body)))
    (.array buffer)))

(defn message-size [^bytes header]
  (let [n (.getInt (doto (ByteBuffer/wrap header) (.order (ByteOrder/nativeOrder))))
        size (bit-and n 0xfffffff)]
    (when-not (and (= 1 (unsigned-bit-shift-right n 28)) (<= 1 size max-message-bytes))
      (fail! "Invalid or oversized OLA RPC header" {:size size})) size))

(defn decode-reply [id method request data]
  (let [rpc (parse-protobuf data) type (field rpc 1)]
    (when-not (= id (field rpc 2)) (fail! "OLA RPC response ID mismatch" {:expected id :received (field rpc 2)}))
    (when-not (= 2 type)
      (throw (ex-info (case type 3 "OLA cancelled request" 4 "OLA request failed" 5 "OLA method unsupported" "Unexpected OLA RPC type")
                      {:rdm/error :rpc-failed :response-type type :message (some-> (field rpc 4) bytes-text)})))
    (let [m (parse-protobuf (or (field rpc 4) []))]
      (if (#{"ForceDiscovery" "GetUIDs"} method)
        (do (when-not (= (:universe request) (field m 1)) (fail! "Discovery universe mismatch" {}))
            {:universe (field m 1) :uids (mapv decode-uid (get m 2 []))})
        (let [response {:response-code (field m 1) :response-type (field m 2) :message-count (or (field m 3) 0)
                        :data (or (field m 4) []) :pid (field m 5) :command-class (field m 6)
                        :sub-device (field m 7) :source-uid (some-> (field m 9) decode-uid)}]
          (when-not (integer? (:response-code response)) (fail! "Missing RDM response code" {}))
          (when (zero? (:response-code response))
            (when-not (and (#{0 1 2} (:response-type response)) (uint? 16 (:pid response))
                           (#{0 1} (:command-class response)) (= (:sub-device request 0) (:sub-device response))
                           (or (nil? (:source-uid response)) (= (uid (:uid request)) (:source-uid response)))
                           (or (= 0x20 (:pid request)) (= (:pid request) (:pid response))))
              (fail! "RDM response identity or type mismatch" {:response response})))
          response)))))

(defn decode-value [pid data]
  (let [data (bytes-vector data) n (count data)
        length! (fn [lo hi] (when-not (<= lo n hi) (fail! "Unexpected RDM parameter length" {:pid pid :length n})))
        text! (fn [] (length! 0 32) (bytes-text data))]
    (case pid
      0x50 (do (when (odd? n) (fail! "Supported PID list has odd length" {})) (mapv #(u16 data %) (range 0 n 2)))
      0x60 (do (length! 19 19)
               {:protocol [(nth data 0) (nth data 1)] :model-id (u16 data 2) :category (u16 data 4)
                :software-version (reduce #(+ (bit-shift-left %1 8) %2) 0 (subvec data 6 10))
                :footprint (u16 data 10) :personality (nth data 12) :personality-count (nth data 13)
                :address (u16 data 14) :sub-device-count (u16 data 16) :sensor-count (nth data 18)})
      (0x80 0x81 0x82 0xc0) (text!)
      0xe0 (do (length! 2 2) {:current (nth data 0) :count (nth data 1)})
      0xe1 (do (length! 3 35) {:number (first data) :footprint (u16 data 1) :name (bytes-text (subvec data 3))})
      0xf0 (do (length! 2 2) (u16 data 0))
      0x1000 (do (length! 1 1) (when-not (#{0 1} (first data)) (fail! "Invalid identify value" {})) (= 1 (first data)))
      0x200 (do (length! 13 45)
                {:number (nth data 0) :type (nth data 1) :unit (nth data 2) :prefix (nth data 3)
                 :range [(sint16 data 4) (sint16 data 6)] :normal [(sint16 data 8) (sint16 data 10)]
                 :recording (nth data 12) :description (bytes-text (subvec data 13))})
      0x201 (do (length! 9 9) {:number (first data) :present (sint16 data 1) :lowest (sint16 data 3)
                              :highest (sint16 data 5) :recorded (sint16 data 7)})
      {:bytes data})))

(defn typed-command [request]
  (let [kind (keyword (:parameter request)) value (:value request)
        command (keyword (:command request :get))
        data (if (= :get command) (:data request []) (case kind
               :address (do (when-not (and (integer? value) (<= 1 value 512)) (fail! "DMX address must be 1..512" {})) (uint16-bytes value))
               :personality (do (when-not (and (integer? value) (<= 1 value 255)) (fail! "Personality must be 1..255" {})) [value])
               :identify (do (when-not (boolean? value) (fail! "Identify must be boolean" {})) [(if value 1 0)])
               :device-label (do (when-not (and (string? value) (<= (count (text-bytes value)) 32)) (fail! "Label must fit 32 UTF-8 bytes" {})) (text-bytes value))
               (bytes-vector (:data request []))))
        pid (or (pids kind) (:pid request))]
    (validate-command (assoc request :command command :pid pid :data data))))
