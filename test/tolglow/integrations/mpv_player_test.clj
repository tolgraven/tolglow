(ns tolglow.integrations.mpv-player-test
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [tolglow.integrations.mpv-player :as player])
  (:import [java.io BufferedReader]
           [java.net StandardProtocolFamily UnixDomainSocketAddress]
           [java.nio ByteBuffer]
           [java.nio.channels Channels ServerSocketChannel SocketChannel]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:private provenance {:source-id :mpv :session-id :socket-test :clock-id :test-clock})
(def ^:private track "/tmp/reference-🎵.wav")
(def ^:private reference {:id :track :version 1})
(def ^:private catalog {track {:reference reference :duration-ms 12000.0}})

(defn- await! [ready!]
  (let [start (System/nanoTime)]
    (loop []
      (or (ready!)
          (if (< (- (System/nanoTime) start) 2000000000)
            (do (Thread/sleep 2) (recur))
            (throw (ex-info "Timed out waiting for local mpv test socket" {})))))))

(defn- send! [^SocketChannel socket data]
  (let [buffer (if (string? data)
                 (.encode StandardCharsets/UTF_8 ^String data)
                 (ByteBuffer/wrap data))]
    (while (.hasRemaining buffer)
      (.write socket buffer))))

(defn- reply! [socket request value split?]
  (let [line (str (json/write-str {"request_id" request "error" "success" "data" value}
                                  :escape-unicode false) "\n")]
    ;; Start the four-byte path character at the end of a read buffer.
    (send! socket (if split? (str (.repeat " " (- 4095 (str/index-of line "🎵"))) line) line))))

(defn- acquire! [socket ^BufferedReader input]
  (dotimes [_ 11]
    (let [{[command] "command" request "request_id"} (json/read-str (.readLine input))]
      (is (= "observe_property" command))
      (reply! socket request nil false)))
  (dotimes [sample 3]
    (dotimes [index 17]
      (let [{[command property] "command" request "request_id"} (json/read-str (.readLine input))
            value (case property
                    ("path" "stream-open-filename") track
                    ("pause" "seeking" "paused-for-cache" "eof-reached") false
                    "speed" 1.0
                    "time-pos" 2.0
                    nil)]
        (is (= "get_property" command))
        (reply! socket request value (and (zero? sample) (zero? index)))))))

(defn- exercise! [scenario!]
  (let [directory (Files/createTempDirectory "mpv-" (make-array FileAttribute 0))
        path (.resolve directory "ipc")]
    (try
      (with-open [listener (ServerSocketChannel/open StandardProtocolFamily/UNIX)]
        (.configureBlocking listener false)
        (.bind listener (UnixDomainSocketAddress/of path))
        (let [alignment (atom nil)
              clock (atom 100.0)
              owner (player/open! {:socket-path (str path) :provenance provenance :catalog catalog}
                                  {:now-ms! #(swap! clock inc)
                                   :publish! (fn [candidate] (reset! alignment candidate) true)})]
          (try
            (with-open [^SocketChannel socket (await! #(.accept listener))]
              (let [peer (future
                           (with-open [input (BufferedReader. (Channels/newReader socket StandardCharsets/UTF_8))]
                             (acquire! socket input)
                             (await! #(= :locked (:status @alignment)))
                             (is (= reference (:reference @alignment)))
                             (scenario! socket owner clock)
                             (let [{:keys [reason revoked?] :as result} (deref (:done owner) 2000 ::timeout)]
                               (is (map? result))
                               (is revoked?)
                               (is (= (:alignment result) @alignment))
                               (is (= [provenance :unavailable 1]
                                      ((juxt :provenance :status :generation) @alignment)))
                               ;; The peer can read buffered read-only requests followed by EOF.
                               (loop []
                                 (when-let [line (.readLine input)]
                                   (is (= "get_property" (first (get (json/read-str line) "command"))))
                                   (recur)))
                               reason)))]
                (deref peer 6000 ::timeout)))
            (finally (deref ((:close! owner)) 2000 ::timeout)))))
      (finally
        (Files/deleteIfExists path)
        (Files/deleteIfExists directory)))))

(deftest explicit-close-revokes-a-verified-session
  (is (= :stopped (exercise! (fn [_ owner _]
                               (let [done ((:close! owner))]
                                 (is (identical? (:done owner) done))
                                 (is (identical? done ((:close! owner))))))))))

(deftest eof-and-invalid-streams-revoke-a-verified-session
  (doseq [[reason data] [[:eof nil]
                         [:truncated-line "{\"event\":"]
                         [:truncated-utf8 (byte-array [(unchecked-byte 0xf0) (unchecked-byte 0x9f)])]
                         [:invalid-utf8 (byte-array [(unchecked-byte 0xff)])]
                         [:oversized-line (.repeat "x" 16385)]
                         [:malformed-json "{broken}\n"]]]
    (testing (name reason)
      (is (= reason (exercise! (fn [^SocketChannel socket _ _]
                                 (when data (send! socket data))
                                 (.shutdownOutput socket))))))))

(deftest rejects-a-regressing-clock-and-revokes-the-lock
  (is (= :invalid-clock (exercise! (fn [_ _ clock] (reset! clock 0.0))))))

(deftest requires-an-explicit-absolute-socket-path
  (doseq [path [nil "mpv.sock" "tcp://localhost:6600"]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"absolute UNIX socket path"
                          (player/open! {:socket-path path :provenance provenance :catalog catalog}
                                        {:now-ms! (constantly 0) :publish! (constantly true)})))))
