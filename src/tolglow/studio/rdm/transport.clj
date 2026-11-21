(ns tolglow.studio.rdm.transport
  "Owned, cancellable OLA RPC connection used only by an RDM job."
  (:require [tolglow.studio.rdm.codec :as codec])
  (:import [java.net Socket InetSocketAddress SocketTimeoutException]
           [java.io EOFException]))

(defn remaining-ms [deadline]
  (let [remaining (- deadline (System/nanoTime))]
    (when-not (pos? remaining) (throw (SocketTimeoutException. "RDM job deadline exceeded")))
    (long (max 1 (Math/ceil (/ remaining 1.0e6))))))

(defn open!
  "Open one OLA connection. register! owns cancellation before connect starts.
  Job owner also closes the socket at its absolute deadline, bounding writes."
  [{:keys [host port timeout-ms]} deadline register!]
  (let [socket (Socket.) sequence (volatile! 0)]
    (register! #(.close socket))
    (try
      (.connect socket (InetSocketAddress. ^String host (int port)) (int (min timeout-ms (remaining-ms deadline))))
      (.setTcpNoDelay socket true)
      {:close! #(.close socket)
       :request! (fn [method request]
                   (let [id (vswap! sequence inc)
                         packet (codec/rpc-packet id method request)
                         input (.getInputStream socket)
                         output (.getOutputStream socket)
                         read! (fn [n]
                                 (let [buffer (byte-array n)]
                                   (loop [offset 0]
                                     (when (< offset n)
                                       (.setSoTimeout socket (int (min timeout-ms (remaining-ms deadline))))
                                       (let [count (.read input buffer offset (- n offset))]
                                         (when (neg? count) (throw (EOFException. "OLA closed the RDM connection")))
                                         (recur (+ offset count))))) buffer))]
                     (remaining-ms deadline)
                     (.write output ^bytes packet)
                     (.flush output)
                     (let [size (codec/message-size (read! 4))]
                       (codec/decode-reply id method request (mapv #(bit-and 255 %) (read! size))))))}
      (catch Throwable error (.close socket) (throw error)))))

(def nack-reasons
  {0 :unknown-pid 1 :format-error 2 :hardware-fault 3 :proxy-reject 4 :write-protect
   5 :unsupported-command-class 6 :data-out-of-range 7 :buffer-full 8 :packet-size-unsupported
   9 :sub-device-out-of-range 10 :proxy-buffer-full 11 :action-not-supported})

(defn command!
  "Resolve ACK_TIMER by bounded QUEUED_MESSAGE polling. OLA owns ACK_OVERFLOW
  assembly and transport transaction validation. Never re-send a delayed SET."
  [request! request deadline current?]
  (let [request (codec/validate-command request)
        expected-class (if (= :set (:command request)) 1 0)]
    (loop [response (request! "RDMCommand" request) attempts 0]
      (when-not (current?) (throw (ex-info "RDM job cancelled or session changed" {:rdm/error :cancelled})))
      (remaining-ms deadline)
      (when (>= attempts 32) (throw (ex-info "RDM queued reply limit exceeded" {:rdm/error :queue-limit})))
      (if (not= 0 (:response-code response))
        (assoc response :status :transport-error)
        (case (:response-type response)
          0 (if (and (= (:pid request) (:pid response)) (= expected-class (:command-class response)))
              (assoc response :status :ack)
              (recur (request! "RDMCommand" (assoc request :pid 0x20 :command :get :data [1] :sub-device 0)) (inc attempts)))
          2 (do (when-not (= 2 (count (:data response))) (codec/fail! "Malformed RDM NACK" {}))
                (assoc response :status :nack :nack-code (codec/u16 (:data response) 0)
                       :reason (get nack-reasons (codec/u16 (:data response) 0) :unknown)))
          1 (do
              (when-not (= 2 (count (:data response))) (codec/fail! "Malformed RDM ACK_TIMER" {}))
              (let [delay (* 100 (codec/u16 (:data response) 0))]
                (when (>= delay (remaining-ms deadline))
                  (throw (SocketTimeoutException. "RDM ACK_TIMER exceeds job deadline")))
                ;; This owned IO worker is interruptible; the lighting loop never waits.
                (Thread/sleep (long delay)))
              (recur (request! "RDMCommand" (assoc request :pid 0x20 :command :get :data [1] :sub-device 0)) (inc attempts)))
          (codec/fail! "Unknown RDM response type" {:response response}))))))
