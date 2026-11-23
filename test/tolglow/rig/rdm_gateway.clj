(ns tolglow.rig.rdm-gateway
  "Loopback-only Art-Net gateway for host and browser integration tests."
  (:require [afterglow.rdm.codec :as codec]
            [tolglow.rig.rdm-test :as fixture])
  (:import [java.net DatagramPacket DatagramSocket InetAddress SocketException]))

(defn- response [state raw]
  (case (raw 9)
    (0x80 0x82)
    (let [address (raw (if (= 0x80 (raw 9)) 24 23))]
      [(if (= 0x80 (raw 9)) :cached :discover)
       (concat (take 9 raw) [0x81 0 14 1 1] (repeat 6 0)
               [1 (raw 21) 0 address 0 1 0 1 0x12 0x34 1 2 3 4])])
    0x83
    (let [request {:command (if (= 0x30 (raw 43)) :set :get) :sub-device (codec/u16 raw 41)
                   :pid (codec/u16 raw 44) :data (subvec raw 47 (+ 47 (raw 46)))}
          {:keys [data response-type]} (fixture/fixture-peer state request)
          message (concat [1 (+ 24 (count data))] (subvec raw 32 38) (subvec raw 26 32)
                          [(raw 38) response-type 0] (subvec raw 41 43) [(inc (raw 43))]
                          (subvec raw 44 46) [(count data)] data)]
      [request (concat (take 24 raw) message (codec/uint16-bytes (reduce + 0xcc message)))])))

(defn start! []
  (let [socket (DatagramSocket. 0 (InetAddress/getByName "127.0.0.1"))
        state (atom {})
        requests (atom [])
        serve! (fn []
                 (let [buffer (byte-array 1500)
                       request (DatagramPacket. buffer (alength buffer))]
                   (.receive socket request)
                   (let [raw (mapv #(bit-and 255 (aget buffer %)) (range (.getLength request)))
                         [op response] (response state raw)
                         bytes (byte-array (map unchecked-byte response))]
                     (swap! requests conj op)
                     (.send socket (DatagramPacket. bytes (alength bytes) (.getSocketAddress request))))))
        worker (future
                 (try (while (not (.isClosed socket)) (serve!))
                      (catch SocketException error (when-not (.isClosed socket) (throw error)))))]
    {:config {:host "127.0.0.1" :transport :art-net :port (.getLocalPort socket) :local-port 0
              :controller-uid "7a70:12345678"} :requests requests :state state
     :close! (fn [] (.close socket) (deref worker 2000 nil))}))
