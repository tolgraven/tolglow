(ns tolglow.studio.rdm-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.studio.rdm :as rdm]
            [tolglow.studio.rdm.codec :as codec]
            [tolglow.studio.rdm.transport :as transport]
            [tolglow.studio.demo :as demo])
  (:import [java.net ServerSocket Socket InetAddress SocketException SocketTimeoutException]
           [java.io DataInputStream EOFException]
           [java.nio ByteBuffer ByteOrder]))

(def device {:universe 1 :uid "1234:01020304" :sub-device 0})
(defn ack [request data]
  {:response-code 0 :response-type 0 :pid (:pid request) :sub-device (:sub-device request 0)
   :command-class (if (= :set (:command request)) 1 0) :data data})
(defn text-data [s] (mapv #(bit-and 255 %) (.getBytes s "UTF-8")))
(defn info-bytes [address personality]
  (vec (concat [1 0 0 1 1 1 0 0 0 3 0 12 personality 2] (codec/uint16-bytes address) [0 0 1])))
(defn fixture-peer [state request]
  (if (:discovery? request) {:uids [(:uid device)] :universe (:universe request)}
    (let [{:keys [pid data command]} request]
      (if (= :set command)
        (do (case pid 0xf0 (swap! state assoc :address (codec/u16 data 0))
                  0xe0 (swap! state assoc :personality (first data))
                  0x1000 (swap! state assoc :identify (= 1 (first data)))
                  0x82 (swap! state assoc :label (String. (byte-array (map unchecked-byte data)) "UTF-8")) nil)
            (ack request []))
        (if-let [data (case pid
                       0x60 (info-bytes (:address @state 1) (:personality @state 1))
                       0x80 (text-data "Loopback moving head") 0x81 (text-data "Test Lighting")
                       0x82 (text-data (:label @state "Stage left")) 0xc0 (text-data "1.0")
                       0x50 [0 224 0 225 2 0 2 1 16 0]
                       0xe0 [(:personality @state 1) 2]
                       0xe1 (vec (concat [(first data) 0 12] (text-data "12 channel")))
                       0xf0 (codec/uint16-bytes (:address @state 1))
                       0x1000 [(if (:identify @state) 1 0)]
                       0x200 (vec (concat [0 0 0 0 255 246 0 100 0 0 0 80 0] (text-data "Temperature")))
                       0x201 [0 0 24 0 20 0 28 0 24]
                       nil)]
          (ack request data)
          (assoc (ack request [0 0]) :response-type 2))))))

(defn loopback-peer
  "Owned actual TCP/protobuf peer. split? deliberately fragments every reply."
  [respond & [{:keys [split?]}]]
  (let [server (ServerSocket. 0 8 (InetAddress/getByName "127.0.0.1"))
        closed? (atom false) sockets (atom #{}) requests (atom [])
        worker (future
                 (try
                   (while (not @closed?)
                     (with-open [socket (.accept server)]
                       (swap! sockets conj socket)
                       (try
                         (let [input (DataInputStream. (.getInputStream socket)) output (.getOutputStream socket)]
                           (loop []
                             (let [header (byte-array 4) _ (.readFully input header)
                                   body (byte-array (codec/message-size header)) _ (.readFully input body)
                                   rpc (codec/parse-protobuf (mapv #(bit-and 255 %) body))
                                   id (codec/field rpc 2) method (String. (byte-array (codec/field rpc 3)) "UTF-8")
                                   args (codec/parse-protobuf (codec/field rpc 4))
                                   request (if (= method "ForceDiscovery")
                                             {:discovery? true :universe (codec/field args 1) :full? (= 1 (codec/field args 2))}
                                             {:universe (codec/field args 1) :uid (codec/decode-uid (codec/field args 2))
                                              :sub-device (codec/field args 3) :pid (codec/field args 4)
                                              :data (codec/field args 5) :command (if (= 1 (codec/field args 6)) :set :get)})
                                   _ (swap! requests conj request) response (respond request)
                                   message (when-not (:discovery? request)
                                             (codec/protobuf [[1 0 (:response-code response)] [2 0 (:response-type response)]
                                                              [4 2 (:data response)] [5 0 (:pid response)]
                                                              [6 0 (:command-class response)] [7 0 (:sub-device response)]]))
                                   ;; Golden UID bytes: manufacturer varint0x1234 and fixed32 device0x01020304.
                                   message (if (:discovery? request) (codec/protobuf [[1 0 (:universe response)]
                                                                                     [2 2 [8 180 36 21 4 3 2 1]]]) message)
                                   rpc (codec/protobuf [[1 0 2] [2 0 id] [4 2 message]])
                                   header (.array (doto (ByteBuffer/allocate 4) (.order (ByteOrder/nativeOrder))
                                                    (.putInt (bit-or 0x10000000 (count rpc)))))
                                   bytes (byte-array (map unchecked-byte rpc))]
                               (.write output header)
                               (if split? (doseq [b bytes] (.write output (bit-and 255 b)) (.flush output)) (.write output bytes))
                               (.flush output)
                               (recur))))
                         (catch EOFException _) (catch SocketException _)
                         (finally (swap! sockets disj socket)))))
                   (catch SocketException _)))]
    {:port (.getLocalPort server) :requests requests
     :close! #(do (reset! closed? true) (.close server) (doseq [socket @sockets] (.close ^Socket socket))
                  (future-cancel worker))}))

(defn wait-until [pred]
  (let [deadline (+ (System/nanoTime) 5000000000)]
    (loop [] (if-let [value (pred)] value
               (if (> (System/nanoTime) deadline) (throw (ex-info "Test wait exceeded deadline" {}))
                   (do (Thread/sleep 5) (recur)))))))

(deftest request-wire-layout-and-response-correlation
  (is (= [8 1 18 8 8 180 36 21 4 3 2 1 24 0 32 240 1 42 2 0 100 48 1]
         (codec/request-body "RDMCommand" (merge device {:pid 0xf0 :command :set :data [0 100]}))))
  (is (= "1234:01020304" (codec/decode-uid [8 180 36 21 4 3 2 1])))
  (doseq [request [(merge device {:pid 0x60 :sub-device 513}) (merge device {:pid 0x60 :data [256]})
                   (merge device {:pid 0x60 :uid "invalid"})]]
    (is (thrown? clojure.lang.ExceptionInfo (codec/request-body "RDMCommand" request))))
  (is (thrown? clojure.lang.ExceptionInfo
               (codec/decode-reply 2 "RDMCommand" (merge device {:pid 0x60}) [8 2 16 1 34 0])))
  (is (thrown? clojure.lang.ExceptionInfo (codec/parse-protobuf [10 4 0])))
  (is (= 200 (count (codec/decode-value 0x50 (vec (repeat 400 0))))))
  (is (thrown? clojure.lang.ExceptionInfo (codec/decode-value 0x60 [1]))))

(deftest actual-loopback-connection-handles-fragmented-protobuf
  (let [state (atom {}) peer (loopback-peer (partial fixture-peer state) {:split? true}) close-ref (atom nil)
        deadline (+ (System/nanoTime) 5000000000)]
    (try
      (let [connection (transport/open! {:host "127.0.0.1" :port (:port peer) :timeout-ms 1000} deadline #(reset! close-ref %))]
        (try
          (is (= [(:uid device)] (:uids ((:request! connection) "ForceDiscovery" {:universe 1 :full? true}))))
          (let [response (transport/command! (:request! connection) (merge device {:pid 0x60}) deadline (constantly true))]
            (is (= :ack (:status response)))
            (is (= 12 (:footprint (codec/decode-value 0x60 (:data response))))))
          (finally ((:close! connection)))))
      (finally ((:close! peer))))))

(deftest queued-set-completes-without-resending-and-preserves-nack
  (let [calls (atom []) responses (atom [(assoc (ack (assoc device :sub-device 3 :pid 0x82 :command :set) [0 0]) :response-type 1)
                                       (ack (assoc device :sub-device 3 :pid 0x82 :command :set) [])])
        request! (fn [_ request] (swap! calls conj request) (let [r (first @responses)] (swap! responses subvec 1) r))
        result (transport/command! request! (assoc device :sub-device 3 :pid 0x82 :command :set :data [65])
                                   (+ (System/nanoTime) 1000000000) (constantly true))]
    (is (= :ack (:status result)))
    (is (= [:set :get] (mapv :command @calls)))
    (is (= [0x82 0x20] (mapv :pid @calls)))
    (is (= 0 (:sub-device (second @calls)))))
  (is (= :unknown-pid (:reason (transport/command! (fn [_ request] (assoc (ack request [0 0]) :response-type 2))
                                                   (assoc device :pid 0x9999) (+ (System/nanoTime) 1000000000) (constantly true)))))
  (is (thrown? clojure.lang.ExceptionInfo
               (transport/command! (fn [_ request] (assoc (ack request []) :command-class 1))
                                   (assoc device :pid 0x60) (+ (System/nanoTime) 1000000000) (constantly true)))))

(deftest manager-is-inert-and-stale-work-cannot-cross-a-show-epoch
  (let [model (atom (demo/model)) identity (atom {:runtime-id "test" :expected-epoch 0})
        calls (atom 0) entered (promise) release (promise) closed (promise)
        manager (rdm/create {:model! #(deref model) :identity! #(deref identity)
                             :publish! (fn [id payload] (when (= id @identity) (swap! model rdm/update-model payload)))
                             :open! (fn [_ _ register!]
                                      (swap! calls inc) (register! #(deliver closed true))
                                      {:close! #(deliver closed true)
                                       :request! (fn [_ _] (deliver entered true) @release {:universe 1 :uids [(:uid device)]})})})]
    (try
      (rdm/status manager)
      (is (zero? @calls))
      (let [session (rdm/connect! manager {} @identity)]
        (is (zero? @calls))
        (rdm/submit! manager :discover {:universe 1 :session-id (:session-id session)})
        (is (= true (deref entered 1000 false)))
        (swap! identity assoc :expected-epoch 1)
        (rdm/reconcile! manager)
        (deliver release true)
        (is (= true (deref closed 1000 false)))
        (is (false? (:connected? (rdm/status manager))))
        (is (nil? (:discovered (rdm/saved @model))))
        (is (thrown? clojure.lang.ExceptionInfo (rdm/submit! manager :discover {:universe 1 :session-id (:session-id session)}))))
      (finally (deliver release true) (rdm/close! manager)))))

(deftest proposed-addresses-reject-overlap-without-altering-show-data
  (let [observation {:fields {:device-info {:status :ack :value {:address 1 :footprint 12}}}}
        model (-> (demo/model)
                  (rdm/update-model {:operation :associate :fixture-id :head-1 :device device})
                  (rdm/update-model {:operation :observe :device device :observation observation}))]
    (is (thrown? clojure.lang.ExceptionInfo (rdm/validate-write! model (merge device {:command :set :parameter :address :value 13}))))
    (is (= [0 200] (:data (rdm/validate-write! model (merge device {:command :set :parameter :address :value 200})))))
    (is (= 1 (get-in model [:rig :fixtures 0 :address])))
    (is (thrown? clojure.lang.ExceptionInfo (rdm/validate-write! model (merge device {:command :set :pid 0xf0 :data []}))))))

(deftest bounded-jobs-cancel-and-release-owned-transport
  (let [model (atom (demo/model)) identity {:runtime-id "test" :expected-epoch 0}
        entered (promise) release (promise) closes (atom 0)
        manager (rdm/create {:model! #(deref model) :identity! (constantly identity)
                             :publish! (fn [_ payload] (swap! model rdm/update-model payload))
                             :open! (fn [_ _ register!]
                                      (let [close! #(swap! closes inc)]
                                        (register! close!)
                                        {:close! close! :request! (fn [_ _] (deliver entered true) @release {:universe 1 :uids []})}))})]
    (try
      (let [session (:session-id (rdm/connect! manager {} identity))
            first (rdm/submit! manager :discover {:universe 1 :session-id session})]
        (is (= true (deref entered 1000 false)))
        (dotimes [_ 7] (rdm/submit! manager :discover {:universe 1 :session-id session}))
        (is (thrown? clojure.lang.ExceptionInfo (rdm/submit! manager :discover {:universe 1 :session-id session})))
        (is (= :cancelled (:status (rdm/cancel! manager (:job-id first)))))
        (is (pos? @closes))
        (rdm/disconnect! manager)
        (is (empty? @(:active manager)))
        (is (every? #(= :cancelled (:status %)) (:jobs (rdm/status manager))))
        (is (zero? (.size (.getQueue ^java.util.concurrent.ThreadPoolExecutor (:executor manager))))))
      (finally (deliver release true) (rdm/close! manager)))))

(deftest delayed-ack-cannot-exceed-absolute-deadline
  (is (thrown? SocketTimeoutException
               (transport/command! (fn [_ request] (assoc (ack request [255 255]) :response-type 1))
                                   (assoc device :pid 0x60) (+ (System/nanoTime) 1000000000) (constantly true)))))

(deftest actual-socket-timeout-leaves-show-data-unmodified
  (let [release (promise) peer (loopback-peer (fn [request] @release (fixture-peer (atom {}) request)))
        model (atom (demo/model)) identity {:runtime-id "timeout" :expected-epoch 0}
        manager (rdm/create {:model! #(deref model) :identity! (constantly identity)
                             :publish! (fn [_ payload] (swap! model rdm/update-model payload))})]
    (try
      (let [session (:session-id (rdm/connect! manager {:port (:port peer) :timeout-ms 100 :job-timeout-ms 1000} identity))
            id (:job-id (rdm/submit! manager :discover {:universe 1 :session-id session}))
            job (wait-until #(some (fn [job] (when (and (= id (:id job)) (= :failed (:status job))) job))
                                   (:jobs (rdm/status manager))))]
        (is (string? (get-in job [:error :message])))
        (is (nil? (:discovered (rdm/saved @model))))
        (wait-until #(empty? @(:active manager)))
        (is (empty? @(:active manager))))
      (finally (deliver release true) (rdm/close! manager) ((:close! peer))))))
