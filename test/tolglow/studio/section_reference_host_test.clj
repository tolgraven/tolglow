(ns tolglow.studio.section-reference-host-test
  (:require [afterglow.render.frame :as render]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [tolglow.integrations.mpv-player :as player]
            [tolglow.integrations.mpv-session :as mpv-session]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.studio.access :as access]
            [tolglow.transit :as transit])
  (:import [java.io BufferedReader ByteArrayInputStream]
           [java.net StandardProtocolFamily UnixDomainSocketAddress]
           [java.nio ByteBuffer]
           [java.nio.channels Channels ServerSocketChannel SocketChannel]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:private credential "test-only-local-studio-token")

(def ^:private reference {:id :test-track :version :v1})

(def ^:private profile
  {:version 1 :reference reference :duration-ms 8000.0
   :sections [{:id 0 :start-position-ms 0.0 :end-position-ms 4000.0 :start-beat 0.0
               :boundary-kind :file-start :boundary-confidence 1.0}
              {:id 1 :start-position-ms 4000.0 :end-position-ms 8000.0 :start-beat 8.0
               :boundary-kind :confirmed-change :boundary-confidence 0.9
               :features {:signature {:bass-notes [40]}}}]})

(defn- request [app method uri authorized? & [payload]]
  (let [reply (app (cond-> {:request-method method :uri uri
                            :headers (cond-> {"accept" "application/transit+json"}
                                       authorized? (assoc "authorization" (str "Bearer " credential))
                                       payload (assoc "content-type" "application/transit+json"))}
                     payload (assoc :body (ByteArrayInputStream. (.getBytes (transit/encode payload) StandardCharsets/UTF_8)))))]
    (cond-> reply
      (str/includes? (get-in reply [:headers "Content-Type"] "") "application/transit+json")
      (update :body transit/decode))))

(defn- await! [f]
  (let [end (+ (System/nanoTime) 3000000000)]
    (loop []
      (or (f)
          (if (< (System/nanoTime) end)
            (do (Thread/sleep 2) (recur))
            (throw (ex-info "Timed out waiting for local reference test" {})))))))

(defn- reply! [^SocketChannel channel request value]
  (let [bytes (ByteBuffer/wrap
                (.getBytes (str (json/write-str {"request_id" request "error" "success" "data" value}) "\n")
                           StandardCharsets/UTF_8))]
    (while (.hasRemaining bytes) (.write channel bytes))))

(defn- acquire! [socket clock track]
  (let [input (BufferedReader. (Channels/newReader socket StandardCharsets/UTF_8))
        service (future
                  (dotimes [_ 11]
                    (let [{[command] "command" id "request_id"} (json/read-str (.readLine input))]
                      (is (= "observe_property" command))
                      (reply! socket id nil)))
                  (doseq [cursor [3.76 3.78 3.80]]
                    (let [requests (mapv (fn [_] (json/read-str (.readLine input))) (range 17))]
                      (swap! clock + 20)
                      (doseq [{[command property] "command" id "request_id"} requests]
                        (is (= "get_property" command))
                        (reply! socket id
                          (case property
                            ("path" "stream-open-filename") track
                            ("pause" "seeking" "paused-for-cache" "eof-reached") false
                            "speed" 1.0
                            "time-pos" cursor
                            nil))))))]
    (is (not= ::timeout (deref service 3000 ::timeout)))
    input))

(defn- closes? [socket ^BufferedReader existing-input]
  (let [input (or existing-input (BufferedReader. (Channels/newReader socket StandardCharsets/UTF_8)))]
    (true? (deref (future
                    (loop []
                      (if-let [line (.readLine input)]
                        (do (is (#{"observe_property" "get_property"} (first (get (json/read-str line) "command"))))
                            (recur))
                        true)))
                  3000 false))))

(deftest studio-and-repl-share-the-fenced-read-only-mpv-owner
  (let [directory (Files/createTempDirectory "tolglow-reference-" (make-array FileAttribute 0))
        path (.resolve directory "ipc")
        track (str (.resolve directory "trusted-track.wav"))]
    (try
      (with-open [listener (ServerSocketChannel/open StandardProtocolFamily/UNIX)]
        (.configureBlocking listener false)
        (.bind listener (UnixDomainSocketAddress/of path))
        (let [clock (atom 27000)
              runtime (show/create-runtime {:workspace-options nil :workers 1 :physical-now-ms #(deref clock)
                                            :section-patterns {:enabled? true :on :bar :patterns [:warm]}
                                            :section-reference
                                            {:profile profile :provenance {:source-id :mpv :session-id "initial" :clock-id :physical}
                                             :mpv {:socket-path (str path) :max-rtt-ms 300
                                                   :catalog {track (select-keys profile [:reference :duration-ms])}}}})
              app (access/authenticated (studio/handler runtime) credential)
              status #(show/section-reference-status runtime)
              body #(let [{:keys [runtime-id epoch]} (status)] {:runtime-id runtime-id :expected-epoch epoch})
              reconnect #(request app :post "/api/section-reference/reconnect" true %)
              events (atom [])
              open! player/open!]
          (try
            (show/attach-observer! runtime :reference-test
              (fn [] {:events! (fn [_ emitted _ _]
                                 (swap! events into (filter #(= :music/section-pattern (:type %)) emitted)))
                      :frame! (fn [_]) :tracing? (constantly false) :close! (fn [])}))
            (show/tick! runtime 0)
            (is (= 401 (:status (request app :post "/api/section-reference/reconnect" false (body)))))
            (is (= :not-attached (:reason (status))))
            (let [response (reconnect (assoc (body) :socket-path (str path)))]
              (is (= 422 (:status response)))
              (is (= :invalid-request (get-in response [:body :reason]))))
            (is (= :stale-show-session (get-in (reconnect (assoc (body) :runtime-id "wrong")) [:body :reason])))
            (is (nil? (.accept listener)))
            (with-redefs [player/open! (fn [options callbacks]
                                         (is (not (Thread/holdsLock (:lock runtime))))
                                         (update (open! options callbacks) :close!
                                           (fn [close!]
                                             #(do (is (not (Thread/holdsLock (:lock runtime)))) (close!)))))]
              (let [first (show/reconnect-section-reference! runtime (body))
                    callbacks (show/section-reference-callbacks runtime)]
                (is (= :accepted (:status first)))
                (with-open [old-peer (await! #(.accept listener))]
                  (let [old-input (acquire! old-peer clock track)]
                    (await! #(= :aligned (:status (status))))
                    (let [public (request app :get "/api/section-reference" true)]
                      (is (= 200 (:status public)))
                      (is (= (:session-id first) (get-in public [:body :session-id])))
                      (is (not (str/includes? (pr-str public) (str directory)))))
                    (swap! clock + 50)
                    (show/tick! runtime 3850)
                    (is (= :reference (get-in @(:state runtime) [:section-pattern-state :pending :mode])))
                    (let [old-position (:position @(get-in runtime [:section-reference :input]))
                          _ (is (true? ((:publish! callbacks) old-position)))
                          response (reconnect (body))
                          second (get-in response [:body :session-id])]
                      (is (= 200 (:status response)))
                      (is (not= (:session-id first) second))
                      (is (nil? ((:state! callbacks))))
                      (is (nil? ((:publish! callbacks) old-position)))
                      (is (closes? old-peer old-input))
                      (with-open [current-peer (await! #(.accept listener))]
                        (show/tick! runtime 4000)
                        (is (empty? @events))
                        (is (= :unavailable (:status (status))))
                        (let [current-input (acquire! current-peer clock track)]
                          (await! #(= :aligned (:status (status))))
                          (swap! clock + 50)
                          (show/tick! runtime 7850)
                          (swap! clock + 150)
                          (show/tick! runtime 8000)
                          (is (= [[:warm second]]
                                 (mapv (fn [event] [(get-in event [:data :pattern])
                                                    (get-in event [:data :fence :provenance :session-id])]) @events)))
                          (let [override (show/submit! runtime :settings
                                             {:scope :section-patterns :values {:override? true}})
                                previous (body)]
                            (show/tick! runtime 8001)
                            (is (= :accepted (:status @override)))
                            (let [restore (show/submit! runtime :import {:source (show/export-show-source runtime)})]
                              (show/tick! runtime 8002)
                              (is (= :accepted (:status @restore))))
                            (is (= [:stale :show-changed true] ((juxt :status :reason :override?) (status))))
                            (is (= :stale-show-session (get-in (reconnect previous) [:body :reason])))
                            (is (closes? current-peer current-input))
                            (let [fresh (reconnect (body))]
                              (is (= 200 (:status fresh)))
                              (is (= true (:override? (status))))
                              (with-open [fresh-peer (await! #(.accept listener))]
                                (show/stop! runtime)
                                (is (closes? fresh-peer nil))))))))))))
            (finally (show/stop! runtime)))))
      (finally (Files/deleteIfExists path) (Files/deleteIfExists directory)))))

(deftest blocked-player-close-follows-idle-output-without-show-or-publisher-locks
  (let [packets (atom [])
        entered (promise)
        release (promise)
        close! mpv-session/close!
        runtime (show/create-runtime
                  {:workspace-options nil :workers 1
                   :sink {:send! (fn [universes frame]
                                   (swap! packets conj [frame (update-vals universes vec)]))}
                   :section-patterns {:enabled? true :patterns [:warm]}
                   :section-reference
                   {:profile profile :provenance {:source-id :mpv :session-id "shutdown" :clock-id :physical}
                    :mpv {:socket-path "/tmp/tolglow-reference-unused.sock"
                          :catalog {"/tmp/tolglow-reference.wav" (select-keys profile [:reference :duration-ms])}}}})]
    (try
      (let [launch (show/submit! runtime {:op :cue/launch :cue-id :ember})
            idle (update-vals (render/idle-universes (:compiled @(:patch runtime))) vec)]
        (show/tick! runtime 0)
        (show/tick! runtime 200)
        (is (= :accepted (:status (deref launch 1000 ::timeout))))
        (is (not= idle (second (peek @packets))))
        (with-redefs [mpv-session/close! (fn [owner]
                                          (deliver entered [(identical? owner (:section-player runtime))
                                                            (Thread/holdsLock (:lock runtime))
                                                            (Thread/holdsLock (get-in runtime [:publisher :lock]))])
                                          (deref release 3000 ::timeout)
                                          (close! owner))]
          (let [stopping (future (show/stop! runtime))]
            (try
              (is (= [true false false] (deref entered 3000 ::timeout)))
              (is (= [0 idle] (peek @packets)))
              (finally (deliver release true)))
            (is (nil? (deref stopping 3000 ::timeout))))))
      (finally (deliver release true) (show/stop! runtime)))))
