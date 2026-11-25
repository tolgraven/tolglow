(ns tolglow.integrations.mpv-observations-test
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [tolglow.integrations.mpv-observations :as mpv]
            [tolglow.integrations.reference-position :as position]))

(def ^:private provenance {:source-id :mpv :session-id "local-player-1" :clock-id :host-monotonic})
(def ^:private a {:id "sha256:track-a" :version "sha256:profile-a"})
(def ^:private b {:id "sha256:track-b" :version "sha256:profile-b"})
(def ^:private catalog {"/tracks/A.wav" {:reference a :duration-ms 12000.0}
                       "/tracks/B.wav" {:reference b :duration-ms 15000.0}})
(def ^:private baseline
  {"path" "/tracks/./A.wav" "stream-open-filename" "/tracks/A.wav"
   "pause" false "speed" 1.0 "seeking" false "paused-for-cache" false
   "eof-reached" false "ab-loop-a" "no" "ab-loop-b" "no" "remaining-ab-loops" 0
   "remaining-file-loops" 0})

(defn- wire [messages] (apply str (map #(str (json/write-str %) "\n") messages)))
(defn- receive [state now & messages] (mpv/receive state now (wire messages)))
(defn- snapshot [state now] (position/snapshot (:alignment state) now))

(defn- reply [message value]
  {:request_id (get message "request_id") :error "success" :data value})

(defn- connected []
  (let [{:keys [state lines]} (mpv/connect provenance catalog)
        commands (mapv json/read-str lines)]
    (mpv/receive state 0 (wire (map #(reply % nil) commands)))))

(defn- responses [lines values]
  (mapv (fn [line]
          (let [message (json/read-str line)]
            (reply message (get values (second (get message "command")))))) lines))

(defn- sample [state sent-at received-at cursor & [more]]
  (let [{:keys [state lines]} (mpv/request-sample state sent-at)
        values (merge baseline {"time-pos" cursor}
                      (when-let [path (get more "path")] {"stream-open-filename" path}) more)]
    (mpv/receive state received-at (wire (responses lines values)))))

(defn- event [name] {:event name})
(defn- property [id name data] {:event "property-change" :id id :name name :data data})

(defn- locked []
  (-> (connected)
      (sample 100 110 1.0)
      (sample 150 160 1.05)
      (sample 200 210 1.10)))

(deftest read-only-ipc-subscriptions-and-position-sampling
  (let [{:keys [state lines]} (mpv/connect provenance catalog)
        subscriptions (mapv json/read-str lines)
        started (mpv/request-sample (connected) 100)
        replies (responses (:lines started) (assoc baseline "time-pos" 1.0 "speed" 1.25))
        text (wire replies)
        split (quot (count text) 2)
        partial (mpv/receive (:state started) 105 (subs text 0 split))
        first-sample (mpv/receive partial 110 (subs text split))
        second-sample (sample first-sample 150 160 1.0625 {"speed" 1.25})
        third-sample (sample second-sample 200 210 1.125 {"speed" 1.25})
        view (snapshot third-sample 210)]
    (is (every? #(= "observe_property" (first (get % "command"))) subscriptions))
    (is (= {"command" ["observe_property" 7 "time-pos"] "request_id" 7} (nth subscriptions 6)))
    (is (empty? (:lines (mpv/request-sample state 0))))
    (is (every? #(= "get_property" (first (get (json/read-str %) "command"))) (:lines started)))
    (is (empty? (:lines (mpv/request-sample partial 106))))
    (is (= [:unavailable :acquiring :acquiring :locked]
           (mapv :status [(snapshot partial 105) (snapshot first-sample 110) (snapshot second-sample 160) view])))
    (is (= a (:reference view)))
    (is (= provenance (:provenance view)))
    (is (= [1125.0 1.25 200] ((juxt :position-ms :rate :observed-at-ms) view)))
    (is (= 1175.0 (:position-ms (snapshot third-sample 250))))
    (is (= 3 (:confirmations (snapshot (mpv/receive third-sample 220 text) 220))))))

(deftest pause-rate-and-timed-out-queries-revoke-prediction-before-fresh-confirmation
  (let [initial (locked)
        in-flight (mpv/request-sample initial 220)
        paused (receive (:state in-flight) 222 (property 2 "pause" true))
        late (mpv/receive paused 223 (wire (responses (:lines in-flight) (assoc baseline "time-pos" 1.12))))
        still-paused (sample late 225 230 1.12 {"pause" true})
        reacquired (-> still-paused (sample 250 260 1.13) (sample 300 310 1.18) (sample 350 360 1.23))
        paused-again (receive reacquired 361 (property 2 "pause" true))
        new-rate (receive reacquired 361 (property 3 "speed" 1.5))
        faster (-> new-rate (sample 370 380 1.245 {"speed" 1.5})
                   (sample 420 430 1.32 {"speed" 1.5}) (sample 470 480 1.395 {"speed" 1.5}))
        waiting (mpv/request-sample reacquired 370)
        retried (mpv/request-sample (:state waiting) 421)
        after-timeout (mpv/receive (:state retried) 422
                        (wire (responses (:lines waiting) (assoc baseline "time-pos" 1.25))))]
    (is (= [:locked :unavailable :unavailable :unavailable :locked :unavailable :unavailable]
           (mapv :status [(snapshot initial 210) (snapshot paused 222) (snapshot late 223)
                          (snapshot still-paused 230) (snapshot reacquired 360) (snapshot new-rate 361)
                          (snapshot after-timeout 422)])))
    (is (= 1 (:generation (snapshot reacquired 360))))
    (is (= :unavailable (:status (snapshot paused-again 361))))
    (is (= 2 (:generation (snapshot new-rate 361))))
    (is (= [:locked 0 2 1.5] ((juxt :status :occurrence :generation :rate) (snapshot faster 480))))
    (is (= 0 (:occurrence (snapshot after-timeout 422))))
    (is (= :sample-timeout (:reason after-timeout)))
    (is (not= (:lines waiting) (:lines retried)))
    (is (thrown? clojure.lang.ExceptionInfo (mpv/request-sample reacquired 359)))))

(deftest seek-cut-and-same-file-repeat-advance-an-occurrence-once
  (let [initial (locked)
        seeking (receive initial 220 (event "seek") (event "seek"))
        restarted (receive seeking 230 (event "playback-restart"))
        sought (sample restarted 240 250 4.1)
        relocked (-> sought (sample 260 270 4.12) (sample 300 310 4.16) (sample 350 360 4.21))
        loading (receive relocked 370 {:event "end-file" :reason "eof" :playlist_entry_id 4}
                         {:event "start-file" :playlist_entry_id 4} (event "file-loaded"))
        repeated (sample (receive loading 380 (event "playback-restart")) 390 400 0.10)
        cut (sample (receive repeated 420 {:event "start-file" :playlist_entry_id 6}
                             (event "file-loaded") (event "playback-restart"))
                    440 450 0.10 {"path" "/tracks/B.wav"})
        next-track (-> cut
                      (sample 460 470 0.12 {"path" "/tracks/B.wav"})
                      (sample 500 510 0.16 {"path" "/tracks/B.wav"})
                      (sample 550 560 0.21 {"path" "/tracks/B.wav"}))]
    (is (empty? (:lines (mpv/request-sample seeking 221))))
    (is (empty? (:lines (mpv/request-sample loading 371))))
    (is (= :unavailable (:status (snapshot seeking 220))))
    (is (= [:seek :loop :cut] (mapv :reason [sought repeated cut])))
    (is (= [0 1 2 3 3] (mapv :occurrence [(snapshot initial 210) (snapshot sought 250)
                                         (snapshot repeated 400) (snapshot cut 450) (snapshot next-track 560)])))
    (is (= [:locked b] ((juxt :status :reference) (snapshot next-track 560))))
    (is (= 2 (:generation (snapshot next-track 560))))))

(deftest known-loop-boundaries-limit-lookahead-and-confirm-a-new-occurrence
  (let [looping {"ab-loop-a" 1.0 "ab-loop-b" 3.0 "remaining-ab-loops" 2}
        before (-> (connected)
                   (sample 100 110 2.84 looping)
                   (sample 150 160 2.89 looping)
                   (sample 200 210 2.94 looping))
        after (sample (receive before 272 (event "seek") (event "playback-restart"))
                      280 290 1.02 (assoc looping "remaining-ab-loops" 1))]
    (is (= :locked (:status (snapshot before 210))))
    (is (= 50.0 (:lookahead-ms (snapshot before 210))))
    (is (= :unavailable (:status (snapshot before 260))))
    (is (= :loop (:reason after)))
    (is (= 1 (:occurrence (snapshot after 290))))))

(deftest unidentified-or-unavailable-playback-keeps-the-reference-gate-closed
  (let [unknown (sample (connected) 100 110 1.0 {"path" "/tracks/other.wav"})
        remote (sample (connected) 100 110 1.0 {"path" "https://example.invalid/track.wav"})
        redirected (sample (connected) 100 110 1.0 {"stream-open-filename" "https://example.invalid/track.wav"})
        batch (mpv/request-sample (locked) 220)
        replies (responses (:lines batch) (assoc baseline "time-pos" 1.12))
        path-reply (first (keep-indexed (fn [i message]
                                        (when (= ["get_property" "path"] (get (json/read-str message) "command")) i))
                                      (:lines batch)))
        mixed (mpv/receive (:state batch) 230 (wire (assoc-in replies [path-reply :data] "/tracks/B.wav")))
        buffering (sample (locked) 220 230 1.12 {"paused-for-cache" true})
        partial (mpv/receive (locked) 230 "{\"event\":\"seek\"")
        disconnected (mpv/disconnect partial 231)
        corrupt (mpv/receive (locked) 230 "{} unexpected\n")]
    (is (= :unknown-reference (:reason unknown)))
    (is (= :incoherent-sample (:reason remote) (:reason mixed) (:reason redirected)))
    (is (= :not-playing (:reason buffering)))
    (is (= :truncated-line (:reason disconnected)))
    (is (= :malformed-json (:reason corrupt)))
    (is (empty? (:lines (mpv/request-sample disconnected 232))))
    (is (every? #(= :unavailable (:status (snapshot % 232))) [unknown remote redirected mixed buffering disconnected corrupt]))
    (testing "Catalog entries name absolute, local paths and an explicit profile revision"
      (doseq [config [{"track.wav" {:reference a :duration-ms 2000}}
                      {"https://example.invalid/track.wav" {:reference a :duration-ms 2000}}
                      {"/tracks/symlink/../A.wav" {:reference a :duration-ms 2000}}
                      {"/tracks/A.wav" {:reference {:id :missing-version} :duration-ms 2000}}]]
        (is (thrown? clojure.lang.ExceptionInfo (mpv/connect provenance config)))))))
