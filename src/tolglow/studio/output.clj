(ns tolglow.studio.output
  "Explicit physical output boundary. No socket exists until open-artnet! is called."
  (:import [java.net DatagramSocket DatagramPacket InetAddress]))

(def ^:private byte-array-class (class (byte-array 0)))
(def ^:private artdmx-prefix [65 114 116 45 78 101 116 0 0 80 0 14])

(defn artdmx-packet
  "Encode an owned ArtDMX datagram. byte-array payloads copy directly; numeric
  collections retain full signed/unsigned byte validation. The returned packet
  never aliases its input. Universe IDs are 1-based; Art-Net Port-Address is 15-bit."
  [universe sequence-number values]
  (let [bytes? (instance? byte-array-class values)
        n (if bytes? (alength ^bytes values) (count values))]
    (when-not (and (integer? universe) (<= 1 universe 32768)
                   (integer? sequence-number) (<= 0 sequence-number 255)
                   (<= 2 n 512) (even? n))
      (throw (ex-info "ArtDMX needs universe 1..32768, sequence 0..255 and 2..512 even channel count" {})))
    (let [port-address (dec (long universe))
          packet (byte-array (+ 18 n))]
      (dotimes [index 12] (aset-byte packet index (unchecked-byte (nth artdmx-prefix index))))
      (aset-byte packet 12 (unchecked-byte sequence-number))
      (aset-byte packet 13 (byte 0))
      (aset-byte packet 14 (unchecked-byte (bit-and port-address 255)))
      (aset-byte packet 15 (unchecked-byte (bit-shift-right port-address 8)))
      (aset-byte packet 16 (unchecked-byte (bit-shift-right n 8)))
      (aset-byte packet 17 (unchecked-byte (bit-and n 255)))
      (if bytes?
        (System/arraycopy values 0 packet 18 n)
        (doseq [[index value] (map-indexed vector values)]
          (when-not (and (integer? value) (<= -128 value 255))
            (throw (ex-info "Invalid DMX byte" {:index index :value value})))
          (aset-byte packet (+ index 18) (unchecked-byte value))))
      packet)))

(defn open-artnet! [{:keys [host port] :or {port 6454}}]
  (when-not (and (string? host) (seq host) (integer? port) (<= 1 port 65535))
    (throw (ex-info "Art-Net requires a destination host and port" {})))
  (let [address (InetAddress/getByName host) socket (DatagramSocket.) sequence-number (atom 0)]
    (.setBroadcast socket true)
    {:send! (fn [universes _frame-number]
              ;; Transport sequence advances even when a failed effect leaves the
              ;; logical frame unchanged (especially for emergency blackout).
              (let [sequence (swap! sequence-number #(inc (mod % 255)))]
                (doseq [[universe buffer] (sort-by key universes)]
                  (let [^bytes bytes (artdmx-packet universe sequence buffer)]
                    (.send socket (DatagramPacket. bytes (alength bytes) ^InetAddress address (int port)))))))
     :close! #(.close socket)
     :description {:type :art-net :host host :port port}}))
