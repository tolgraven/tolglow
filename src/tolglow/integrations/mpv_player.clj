(ns tolglow.integrations.mpv-player
  "Own an explicit local mpv observation connection outside the show loop."
  (:require [tolglow.integrations.mpv-observations :as observations])
  (:import [java.net StandardProtocolFamily UnixDomainSocketAddress]
           [java.nio ByteBuffer CharBuffer]
           [java.nio.channels SelectionKey Selector SocketChannel]
           [java.nio.charset CharsetDecoder StandardCharsets]))

(def ^:private ^:const sample-period-ns 35000000)
(def ^:private chunk-size 4096)

(defn- socket-address [path]
  (when-not (string? path)
    (throw (ex-info "mpv requires an absolute UNIX socket path" {})))
  (let [address (UnixDomainSocketAddress/of ^String path)]
    (when-not (.isAbsolute (.getPath address))
      (throw (ex-info "mpv requires an absolute UNIX socket path" {})))
    address))

(defn- clock-ms [now-ms! state]
  (let [now (now-ms!)]
    (when-not (and (number? now) (Double/isFinite (double now))
                   (<= (get state :now-ms 0) now))
      (throw (ex-info "mpv requires monotonic host time in milliseconds" {:reason :invalid-clock})))
    now))

(defn- write-lines! [^SocketChannel channel lines]
  (when (seq lines)
    ;; Only the adapter's bounded observe_property and get_property groups reach
    ;; this socket. Close if a stalled peer cannot accept the complete group.
    (let [buffer (.encode StandardCharsets/UTF_8 ^String (apply str lines))]
      (while (.hasRemaining buffer)
        (when (zero? (.write channel buffer))
          (throw (ex-info "mpv socket stopped accepting requests" {:reason :write-stalled})))))))

(defn- decode-chunk! [^CharsetDecoder decoder ^ByteBuffer input ^CharBuffer output]
  (.flip input)
  (.clear output)
  (when (.isError (.decode decoder input output false))
    (throw (ex-info "mpv sent invalid UTF-8" {:reason :invalid-utf8})))
  ;; UTF-8 cannot produce more UTF-16 units than input bytes. Only the bytes
  ;; of a split code point can remain in the input after this bounded decode.
  (.compact input)
  (.flip output)
  (.toString output))

(defn- publish-alignment! [publish! state]
  (when-not (true? (publish! (:alignment state)))
    (throw (ex-info "The show stopped accepting mpv observations" {:reason :publication-rejected}))))

(defn- receive! [^SocketChannel channel ^ByteBuffer input output decoder adapter-state {:keys [now-ms! publish!]}]
  (let [n (.read channel input)]
    (cond
      (neg? n) (cond
                 (pos? (.position input)) :truncated-utf8
                 (seq (:fragment @adapter-state)) :truncated-line
                 :else :eof)
      (pos? n) (let [received-at (clock-ms now-ms! @adapter-state)
                     chunk (decode-chunk! decoder input output)
                     state (observations/receive @adapter-state received-at chunk)]
                 (vreset! adapter-state state)
                 (if (:connected? state)
                   (do (publish-alignment! publish! state) nil)
                   (:reason state))))))

(defn- sample! [channel adapter-state {:keys [now-ms! publish!]}]
  (let [{:keys [state lines]} (observations/request-sample @adapter-state (clock-ms now-ms! @adapter-state))]
    (vreset! adapter-state state)
    (write-lines! channel lines)
    (publish-alignment! publish! state)))

(defn- observe! [address lines adapter-state stopped? callbacks]
  (with-open [channel (SocketChannel/open StandardProtocolFamily/UNIX)
              selector (Selector/open)]
    (.connect channel address)
    (.configureBlocking channel false)
    (.register channel selector SelectionKey/OP_READ)
    (write-lines! channel lines)
    (let [input (ByteBuffer/allocate chunk-size)
          output (CharBuffer/allocate chunk-size)
          decoder (.newDecoder StandardCharsets/UTF_8)]
      (loop [sample-at (System/nanoTime)]
        (if-let [reason (if @stopped? :stopped (receive! channel input output decoder adapter-state callbacks))]
          reason
          (let [now (System/nanoTime)
                sample-at (if (<= (unchecked-subtract sample-at now) 0)
                            (do (sample! channel adapter-state callbacks)
                                (unchecked-add now sample-period-ns))
                            sample-at)
                remaining (unchecked-subtract sample-at (System/nanoTime))
                wait-ms (max 1 (long (Math/ceil (/ remaining 1000000.0))))]
            (.select selector wait-ms)
            (.clear (.selectedKeys selector))
            (recur sample-at)))))))

(defn- close-session! [state now-ms! publish!]
  (let [state (if (:connected? state)
                (let [now (try (clock-ms now-ms! state)
                               (catch Throwable _ (get state :now-ms 0)))]
                  (observations/disconnect state now))
                state)]
    (try
      (publish-alignment! publish! state)
      {:alignment (:alignment state) :revoked? true}
      (catch Throwable error
        {:alignment (:alignment state) :revoked? false :revoke-error error}))))

(defn open!
  "Start observing an existing trusted mpv socket at an absolute :socket-path.
  :provenance, :catalog, and optional :adapter-options are passed to the adapter.
  Use the callbacks, :provenance and :alignment-options supplied by
  show/section-reference-callbacks or show/attach-section-reference!. Each open
  requires its own caller-chosen session ID attached to the running show.
  Call the returned :close! to request stop. The :done promise resolves once
  the socket closes and final alignment is submitted, with :reason and :revoked?."
  [{:keys [socket-path provenance catalog adapter-options]} {:keys [now-ms! publish!] :as callbacks}]
  (when-not (and (ifn? now-ms!) (ifn? publish!))
    (throw (ex-info "mpv requires show clock and reference publication callbacks" {})))
  (let [address (socket-address socket-path)
        {:keys [state lines]} (observations/connect provenance catalog adapter-options)
        adapter-state (volatile! state)
        stopped? (atom false)
        done (promise)
        worker (Thread.
                 (fn []
                   (let [result (try
                                  {:reason (observe! address lines adapter-state stopped? callbacks)}
                                  (catch Throwable error
                                    (if @stopped?
                                      {:reason :stopped}
                                      {:reason (get (ex-data error) :reason :error) :error error})))]
                     ;; Interruption wakes select or connect. Final publication
                     ;; uses the captured show callbacks on this same worker.
                     (Thread/interrupted)
                     (deliver done (merge result (close-session! @adapter-state now-ms! publish!)))))
                 "tolglow-mpv-player")]
    (.setDaemon worker true)
    (.start worker)
    {:done done
     :close! (fn []
               (when (compare-and-set! stopped? false true)
                 (.interrupt worker))
               done)}))
