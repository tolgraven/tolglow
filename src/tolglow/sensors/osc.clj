(ns tolglow.sensors.osc
  "One bounded OSC string packet carrying a normalized sensor observation.
  Wire parsing and optional UDP ownership are separate from sensor middleware."
  (:require [clojure.data.json :as json])
  (:import [java.nio ByteBuffer]
           [java.nio.charset StandardCharsets CodingErrorAction]
           [java.net DatagramSocket DatagramPacket InetSocketAddress SocketTimeoutException]
           [java.util.concurrent.atomic AtomicBoolean]))

(def path "/tolglow/sensor/v1")
(def max-packet-bytes 60000)
(defn- fail! [message] (throw (ex-info message {:sensor/error :invalid-packet})))
(defn- finite? [x] (and (number? x) (Double/isFinite (double x))))
(defn- identifier? [x] (and (string? x) (<= 1 (count x) 128)))
(defn- field? [x] (and (string? x) (boolean (re-matches #"[a-z][a-z0-9-]{0,63}" x))))
(defn- keyword-field [x] (when-not (field? x) (fail! "Invalid sensor field name")) (keyword x))

(defn validate-observation
  "Canonical packet before host clock admission. Token is transport-only and
  must be removed before state/projection/export. No wall time is synthesized."
  [data]
  (when-not (and (map? data)
                 (every? #{:version :device-id :session-id :sample-id :time-ms :values :gestures
                           :pairing-token :reference-frame :calibration-id} (keys data))
                 (= 1 (:version data)) (identifier? (:device-id data)) (identifier? (:session-id data))
                 (integer? (:sample-id data)) (<= 0 (:sample-id data) 9007199254740991)
                 (finite? (:time-ms data)) (<= 0 (:time-ms data))
                 (map? (:values data)) (<= (count (:values data)) 64)
                 (every? (fn [[k v]] (and (keyword? k) (field? (name k)) (nil? (namespace k)) (finite? v))) (:values data))
                 (vector? (:gestures data [])) (<= (count (:gestures data [])) 16)
                 (or (nil? (:pairing-token data)) (and (string? (:pairing-token data)) (<= 16 (count (:pairing-token data)) 512)))
                 (or (nil? (:reference-frame data)) (identifier? (:reference-frame data)))
                 (or (nil? (:calibration-id data)) (identifier? (:calibration-id data))))
    (fail! "Invalid normalized sensor observation"))
  (doseq [gesture (:gestures data)]
    (when-not (and (map? gesture) (every? #{:id :type :phase :confidence} (keys gesture))
                   (identifier? (:id gesture)) (keyword? (:type gesture)) (field? (name (:type gesture)))
                   (= :pulse (:phase gesture))
                   (or (not (contains? gesture :confidence))
                       (and (finite? (:confidence gesture)) (<= 0 (:confidence gesture) 1))))
      (fail! "Invalid semantic sensor gesture")))
  (assoc data :gestures (:gestures data [])))

(defn- bounded-json! [^String source]
  (when (> (count source) max-packet-bytes) (fail! "Sensor JSON exceeds bound"))
  (loop [i 0 depth 0 quoted? false escaped? false]
    (when (< i (.length source))
      (let [c (.charAt source i)
            next-depth (if quoted? depth (case c (\{ \[) (inc depth) (\} \]) (dec depth) depth))]
        (when (or (> next-depth 8) (neg? next-depth)) (fail! "Sensor JSON nesting exceeds bound"))
        (recur (inc i) next-depth
               (if (and (= c \" ) (not escaped?)) (not quoted?) quoted?)
               (and quoted? (= c \\) (not escaped?)))))))

(defn decode-json [source]
  (when-not (string? source) (fail! "Sensor JSON must be text"))
  (bounded-json! source)
  (let [raw (try (json/read-str source) (catch Exception _ (fail! "Malformed sensor JSON")))
        _ (when-not (map? raw) (fail! "Sensor JSON must be an object"))
        data (into {} (map (fn [[k v]] [(keyword-field k) v])) raw)
        data (cond-> data
               (map? (:values data)) (update :values #(into {} (map (fn [[k v]] [(keyword-field k) v])) %))
               (vector? (:gestures data)) (update :gestures
                                                 #(mapv (fn [gesture]
                                                          (when-not (map? gesture) (fail! "Gesture must be an object"))
                                                          (reduce-kv (fn [m k v]
                                                                       (let [key (keyword-field k)]
                                                                         (assoc m key (if (#{:type :phase} key) (keyword-field v) v)))) {} gesture)) %)))]
    (validate-observation data)))

(defn- osc-string [^bytes bytes offset]
  (let [end (loop [index offset]
              (when (>= index (alength bytes)) (fail! "OSC string has no terminator"))
              (if (zero? (aget bytes index)) index (recur (inc index))))
        padded (* 4 (quot (+ end 4) 4))]
    (when (> padded (alength bytes)) (fail! "Truncated OSC string padding"))
    (doseq [index (range end padded)] (when-not (zero? (aget bytes index)) (fail! "Nonzero OSC string padding")))
    (let [decoder (doto (.newDecoder StandardCharsets/UTF_8)
                    (.onMalformedInput CodingErrorAction/REPORT) (.onUnmappableCharacter CodingErrorAction/REPORT))]
      [(try (str (.decode decoder (ByteBuffer/wrap bytes offset (- end offset))))
            (catch Exception _ (fail! "Malformed OSC UTF-8"))) padded])))

(defn decode [packet]
  (when-not (and (instance? (class (byte-array 0)) packet) (<= 12 (alength ^bytes packet) max-packet-bytes)
                 (zero? (mod (alength ^bytes packet) 4))) (fail! "Invalid sensor OSC packet size"))
  (let [[address at] (osc-string packet 0)
        [tags at] (osc-string packet at)
        _ (when-not (and (= path address) (= ",s" tags)) (fail! "Expected sensor path and one OSC string argument"))
        [source at] (osc-string packet at)]
    (when-not (= at (alength ^bytes packet)) (fail! "Trailing sensor OSC arguments"))
    (decode-json source)))

(defn- encode-string [^String source]
  (let [data (.getBytes source StandardCharsets/UTF_8), size (* 4 (quot (+ (alength data) 4) 4)), result (byte-array size)]
    (System/arraycopy data 0 result 0 (alength data)) result))
(defn encode [observation]
  (let [data (validate-observation observation)
        parts (mapv encode-string [path ",s" (json/write-str data)])
        result (byte-array (reduce + (map alength parts)))]
    (when (> (alength result) max-packet-bytes) (fail! "Sensor packet exceeds bound"))
    (loop [offset 0 parts (seq parts)]
      (when-let [^bytes part (first parts)]
        (System/arraycopy part 0 result offset (alength part))
        (recur (+ offset (alength part)) (next parts)))) result))

(defn open!
  "Explicitly own one UDP receiver. Defaults loopback and ephemeral port. The
  callback receives validated data; callback failures count and do not kill input.
  No socket/thread starts on namespace load or configuration restoration."
  [{:keys [host port receive!]}]
  (when-not (and (string? (or host "127.0.0.1")) (integer? (or port 0)) (<= 0 (or port 0) 65535) (ifn? receive!))
    (fail! "UDP receiver needs explicit callback and valid endpoint"))
  (let [^String bind-host (or host "127.0.0.1")
        socket (DatagramSocket. (InetSocketAddress. bind-host (int (or port 0))))
        running (AtomicBoolean. true) stats (atom {:received 0 :rejected 0})
        worker (Thread.
                ^Runnable (fn []
                            (let [buffer (byte-array 65507)]
                              (while (.get running)
                                (try
                                  (let [packet (DatagramPacket. buffer (alength buffer))]
                                    (.receive socket packet)
                                    (let [bytes (java.util.Arrays/copyOf buffer (.getLength packet))]
                                      (receive! (decode bytes))
                                      (swap! stats update :received inc)))
                                  (catch SocketTimeoutException _)
                                  (catch Exception _ (when (.get running) (swap! stats update :rejected inc)))))))
                "tolglow-sensor-osc")]
    (.setSoTimeout socket 100) (.setDaemon worker true)
    (try (.start worker)
         (catch Throwable error (.close socket) (throw error)))
    {:port (.getLocalPort socket) :status #(assoc @stats :running? (.get running))
     :close! (fn [] (when (.compareAndSet running true false) (.close socket)
                     (when-not (identical? (Thread/currentThread) worker) (.join worker 1000))))}))
