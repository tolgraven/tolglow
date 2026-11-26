(ns tolglow.integrations.live-launcher-test
  "A cue page played from one Drum Rack track (#228), on a real show runtime.
  Pad hits enter as Live bridge callbacks exactly as Studio's /api/live
  receives them from the companion, launch cues through the shared control
  model and become takes. A clip of those hits (recorded by the simulated Live
  from the performer's input, or written by Tolglow from the run record)
  replays the same launches as playback, without new takes or clip writes.
  The Set is tolglow.integrations.fake-live; no Live, Max or MIDI device is
  used."
  (:require [afterglow.control.model :as control]
            [afterglow.sync :as sync]
            [afterglow.sync.session :as session]
            [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [tolglow.connections :as connections]
            [tolglow.integrations.ableton :as ableton]
            [tolglow.integrations.fake-live :as fake]
            [tolglow.integrations.live-connection :as live]
            [tolglow.integrations.live-launcher :as launcher]
            [tolglow.integrations.live-runs :as runs]
            [tolglow.integrations.live-sync :as live-sync]
            [tolglow.show :as show]
            [tolglow.studio :as studio])
  (:import [java.io ByteArrayInputStream]))

;; ---------------------------------------------------------------------------
;; Push's 64 Pads layout

(def push-64-pads-grid
  "Push's 64 Pads drum grid with the pad overview at note 36, as notes, top
  row first. Still to be confirmed on Push 3 hardware."
  [[64 65 66 67 96 97 98 99]
   [60 61 62 63 92 93 94 95]
   [56 57 58 59 88 89 90 91]
   [52 53 54 55 84 85 86 87]
   [48 49 50 51 80 81 82 83]
   [44 45 46 47 76 77 78 79]
   [40 41 42 43 72 73 74 75]
   [36 37 38 39 68 69 70 71]])

(deftest push-64-pads-follows-the-push-note-grid
  (testing "every cell has the note of the pad in the same place on Push's 64 Pads grid"
    (doseq [y (range 8) x (range 8)]
      (is (= (get-in push-64-pads-grid [y x]) (ableton/pad-note :push-64-pads [x y])) (pr-str [x y]))))
  (testing "four 4x4 blocks: left half 36-67, right half 68-99"
    (is (= [36 39 48 52 64 67 68 71 83 84 99]
           (mapv #(ableton/pad-note :push-64-pads %) [[0 7] [3 7] [0 4] [0 3] [0 0] [3 0] [4 7] [7 7] [7 4] [4 3] [7 0]])))
    (is (= (set (range 36 100)) (set (for [y (range 8) x (range 8)] (ableton/pad-note :push-64-pads [x y]))))))
  (testing "the earlier row layout is unchanged and both invert"
    (is (= 92 (ableton/pad-note :rows [0 0])))
    (is (= 43 (ableton/pad-note :rows [7 7])))
    (doseq [layout [:push-64-pads :rows] y (range 8) x (range 8)]
      (is (= [x y] (ableton/pad-cell layout (ableton/pad-note layout [x y])))))
    (is (nil? (ableton/pad-cell :push-64-pads 100))))
  (is (= 9 ableton/push-64-pads-scroll-row) "row 9 of four pads starts at note 36")
  (is (thrown? clojure.lang.ExceptionInfo (ableton/pad-note :push-64-pads [8 0])))
  (is (thrown? clojure.lang.ExceptionInfo (ableton/pad-note :grid [0 0]))))

(deftest new-settings-use-push-64-pads-and-saved-mappings-keep-rows
  (let [layout #(get-in (live-sync/validate-config {:live %}) [:live :pad-layout])]
    (is (= :push-64-pads (layout {})) "a new bridge follows Push's 64 Pads layout")
    (is (= :rows (layout {:mappings {92 {:cue-id :red :mode :held}}}))
        "settings saved before the layout existed keep their row allocation")
    (is (= :push-64-pads (layout {:pad-layout "push-64-pads" :mappings {64 {:cue-id :red}}})))
    (is (= :rows (layout {:pad-layout :rows})))
    (is (thrown? clojure.lang.ExceptionInfo (layout {:pad-layout :launchpad}))))
  (testing "map-page places a cell's cue on the pad under that cell"
    (let [registry {:effects {:fx {:id :fx :params {}}} :cues {:red {:effect-id :fx} :blue {:effect-id :fx}}}
          page {:cells {[0 0] :red [7 7] :blue}}]
      (is (= {64 :red 71 :blue} (update-vals (:mappings (ableton/map-page {:pad-layout :push-64-pads} registry page)) :cue-id)))
      (is (= {92 :red 43 :blue} (update-vals (:mappings (ableton/map-page {} registry page)) :cue-id))
          "without a layout map-page keeps rows"))))

;; ---------------------------------------------------------------------------
;; A real runtime with the Live bridge

(defn- effect [id]
  {:id id :version 1 :category :light
   :params {:level {:type :number :default 0.0 :bounds [0.0 1.0]}}
   :outputs {:dimmer {:op :param :key :level}}})

(def registry
  {:effects {:wash (effect :wash)}
   :cues {:red {:id :red :effect-id :wash :version 1 :name "Red" :swatch "#ff0000"}
          :blue {:id :blue :effect-id :wash :version 1 :name "Blue" :swatch "#0000ff"}
          :amber {:id :amber :effect-id :wash :version 1 :name "Amber" :swatch "#ffbf00"}}})

(def page
  "Red on the bottom-left pad, blue on the first pad of the right half's
  bottom row, amber on the top-left pad."
  {:id :pads :cells {[0 7] :red [4 7] :blue [0 0] :amber}})

(def pad-of {:red 36 :blue 68 :amber 64})

(defn- bridge-factory
  "The Live bridge's companion transport, backed by a simulated Set: status
  polls (:inspect) and other bridge-only operations answer executed, Set
  operations reach the simulated Live. Every operation is logged."
  [ls requests]
  (fn [_]
    (let [inner (fake/transport ls {:json? true})]
      {:open! (fn [_] :bridge) :close! (fn [_])
       :request! (fn [handle id op payload]
                   (swap! requests conj op)
                   (if (contains? live/operations op)
                     ((:request! inner) handle id op payload)
                     {:id id :status :executed :result {}}))})))

(defn- fixture
  ([] (fixture (fake/create {:tracks [{:name "Bass" :arm? true}]})))
  ([ls]
   (let [requests (atom [])
         rt (show/create-runtime
             {:model (control/new-show {:show-id :launcher :transport {:playing? true :beat 0.0 :bpm 120.0}
                                        :registry registry :pages {:pads page}
                                        :rig {:fixtures [{:id :left :universe 1 :address 1 :channels {:dimmer 1}}]}})
              :sink {:send! (fn [_ _])} :workspace-options nil :phrase-capture false
              :live-runs {:record-id "pads"}
              :sync-options {:live-factory (bridge-factory ls requests)}})]
     {:rt rt :ls ls :requests requests :manager (connections/sync (:connections rt)) :serial (atom 0)})))

(defn- start-bridge! [{:keys [rt]}]
  (let [configured (show/sync-action! rt :configure {:config {:live {:enabled? true}} :page-id "pads"})]
    (show/sync-action! rt :start {})
    configured))

(defn- at! [{:keys [rt manager]} beat]
  ;; 120 BPM from beat 0: a beat is 500 ms. Drain the bridge, run the owner
  ;; cycle, then let acknowledgements reach the bridge and run once more.
  (is (session/flush! manager))
  (show/tick! rt (long (* 500 beat)))
  (is (session/flush! manager))
  (show/tick! rt (long (* 500 beat))))

(defn- companion!
  "One MIDI callback from the companion, as Studio's /api/live receives it."
  [{:keys [rt manager serial]} event]
  (show/sync-action! rt :live {:protocol-version 1 :bridge-id "tolglow-m4l"
                               :sync-session-id (sync/active-run-id manager)
                               :event-id (str "companion:" (swap! serial inc)) :kind "midi"
                               :event (merge {:channel 1} event)}))

(def performance
  "Pad hits as [beat cue on|off]: red held 1-3, blue 2-4, amber 5-7, red 6-13/2."
  [[1 :red :on] [2 :blue :on] [3 :red :off] [4 :blue :off] [5 :amber :on] [6 :red :on] [13/2 :red :off] [7 :amber :off]])

(defn- hit-event [cue state origin]
  {:type (if (= :on state) "note-on" "note-off") :note (pad-of cue) :velocity (if (= :on state) 100 0) :origin origin})

(defn- ableton-launches
  "Accepted Live bridge launches in the show's command history, oldest first:
  [cue-id beat origin pad-note instance-id]."
  [rt]
  (let [{:keys [history history-order]} (get-in @(:state rt) [:model :control])]
    (vec (for [id history-order
               :let [{:keys [command ack]} (get history id)]
               :when (and (= :cue/launch (:op command)) (= :ableton (get-in command [:source :type]))
                          (= :accepted (:status ack)))]
           [(:cue-id command) (/ (:time-ms command) 500.0) (get-in command [:source :origin])
            (get-in command [:source :note]) (:instance-id ack)]))))

(defn- request [app uri & [query]]
  (app {:request-method :get :uri uri :query-string query :headers {"host" "localhost"}
        :body (ByteArrayInputStream. (.getBytes "" "UTF-8"))}))

(defn- post [app uri body]
  (let [response (app {:request-method :post :uri uri :headers {"host" "localhost" "content-type" "application/json"}
                       :body (ByteArrayInputStream. (.getBytes (json/write-str body) "UTF-8"))})]
    (assoc response :json (try (json/read-str (:body response) :key-fn keyword) (catch Exception _ nil)))))

(defn- replay! [{:keys [rt] :as f} ls clip-id from]
  ;; Live plays the clip from song beat `from`, aligned here with the show
  ;; beat. The companion's track is playing a clip, so it marks every note
  ;; live-playback (live_session.test.js covers that classification).
  (fake/set-song-beat! ls from)
  (is (= :executed (:status (live/request! (live/open (fake/transport ls) {}) :fire-clip {:clip-id clip-id}))))
  (doseq [[beat events] (sort-by key (group-by :song-beat (fake/playback ls from (+ from 8))))]
    (doseq [e events]
      (companion! f {:type (name (:type e)) :note (:note e) :velocity (:velocity e) :origin "live-playback"}))
    (at! f beat))
  (live/request! (live/open (fake/transport ls) {}) :stop-clips {:track-ids [(:track-id (first (filter :companion? (:tracks @ls))))]})
  (at! f (+ from 9)))

(deftest a-cue-page-plays-from-pads-and-its-clip-replays-without-new-takes
  (let [{:keys [rt requests ls] :as f} (fixture)
        app (studio/handler rt)
        ;; The test plays Live's part (arming, recording, firing) directly on the simulated Set.
        conn (live/open (fake/transport ls {:json? true}) {:session-id "live-ui"})]
    (try
      (testing "setup needs the running Live bridge, from Studio and the REPL alike"
        (let [refused (:json (post app "/api/live-launcher/setup" {:page-id "pads"}))]
          (is (= [["ensure-launcher" "rejected"]] (:applied refused)) (pr-str refused))
          (is (= "not-connected" (:code (first (:problems refused)))))
          (is (= [[:ensure-launcher :rejected]] (:applied (show/setup-live-launcher! rt {:page-id :pads}))))
          (is (= "MIDI" (:name (first (filter :companion? (:tracks @ls))))) "nothing reached the Set")))
      (let [configured (start-bridge! f)]
        (testing "saving the bridge maps each page cell to the pad under it in Push's 64 Pads layout"
          (is (= :push-64-pads (get-in configured [:config :live :pad-layout])))
          (is (= {36 :red 68 :blue 64 :amber} (update-vals (get-in configured [:config :live :mappings]) :cue-id)))
          (is (every? #(= :held (:mode %)) (vals (get-in configured [:config :live :mappings]))))))
      (testing "the REPL and Studio read the same 64 pads"
        (let [repl (show/live-launcher rt "pads")
              response (request app "/api/live-launcher" "page-id=pads")
              ring (json/read-str (:body response) :key-fn keyword)]
          (is (= 200 (:status response)) (pr-str response))
          (is (= 64 (count (:pads repl)) (count (:pads ring))))
          (is (= [[[0 7] 36 :red "Red" 0xff0000] [[4 7] 68 :blue "Blue" 0x0000ff] [[0 0] 64 :amber "Amber" 0xffbf00]]
                 (for [cell [[0 7] [4 7] [0 0]]
                       :let [p (first (filter #(= cell (:cell %)) (:pads repl)))]]
                   [(:cell p) (:note p) (:cue-id p) (:label p) (:live-color p)])))
          (is (every? #(= (:cue-id %) (:page-cue-id %)) (:pads repl)) "every pad plays the cue its cell shows")
          (is (= (mapv (juxt :note (comp #(some-> % name) :cue-id)) (:pads repl))
                 (mapv (juxt :note :cue-id) (:pads ring))))
          (is (= [[] [] []] ((juxt :elsewhere :unmapped :off-grid) repl)))
          (is (= "push-64-pads" (:layout ring)))
          (is (= 404 (:status (request app "/api/live-launcher" "page-id=nope"))) "an unknown page is refused in Studio")
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"No such cue page" (show/live-launcher rt "nope"))
              "and at the REPL")))
      (testing "setup through the running bridge puts a Drum Rack after the companion and names a chain per pad"
        (let [setup (:json (post app "/api/live-launcher/setup" {:page-id "pads"}))
              launcher-track (first (filter :companion? (:tracks @ls)))]
          (is (true? (:ready? setup)) (pr-str setup))
          (is (= "Tolglow launcher pads" (:name launcher-track) (:name setup)))
          (is (= [["ensure-launcher" "executed"] ["write-pads" "executed"]] (:applied setup)) "Studio's button")
          (is (= {:port "All Ins" :channel nil} (get-in setup [:launcher :input])) "pad hits can reach the companion")
          (is (= [] (:warnings setup)))
          (is (= {36 {:name "Red" :color 0xff0000} 64 {:name "Amber" :color 0xffbf00} 68 {:name "Blue" :color 0x0000ff}}
                 (get-in launcher-track [:rack :chains])))
          (is (= 9 (get-in launcher-track [:rack :scroll-row])))
          (is (= "Bass" (:name (first (:tracks @ls)))) "the musician's track is untouched")
          (let [rerun (show/setup-live-launcher! rt {:page-id :pads})]
            (is (:ready? rerun) "the REPL runs the same operation")
            (is (true? (get-in (json/read-str (:body (request app "/api/live-launcher" "page-id=pads")) :key-fn keyword) [:setup :ready?]))
                "Studio shows the REPL's setup")
            (is (= (:track-id setup) (:track-id rerun)))
            (is (= (get-in launcher-track [:rack :chains]) (get-in (first (filter :companion? (:tracks @ls))) [:rack :chains]))
                "a rerun finds the same track and Drum Rack"))
          (live/request! conn :configure-track {:track-id (:track-id setup) :expected-name (:name setup) :arm? true})))
      (let [track-id (:track-id (first (filter :companion? (:tracks @ls))))]
        (testing "pad hits from the performer launch the pads' cues and become takes"
          (doseq [[beat cue state] performance]
            (fake/set-song-beat! ls beat)
            (fake/record-input! ls track-id [{:type (if (= :on state) :note-on :note-off) :note (pad-of cue)
                                              :velocity (if (= :on state) 100 0) :channel 1}])
            (companion! f (hit-event cue state "live-input"))
            (at! f beat))
          (at! f 8)
          (let [launches (ableton-launches rt)
                takes (runs/takes (show/live-runs-record rt))]
            (is (= [[:red 1.0 :live-input 36] [:blue 2.0 :live-input 68] [:amber 5.0 :live-input 64] [:red 6.0 :live-input 36]]
                   (mapv #(subvec % 0 4) launches)))
            (is (= [[:red 1 3] [:blue 2 4] [:amber 5 7] [:red 6 13/2]] (mapv (juxt :cue-id :start-beat :release-beat) takes)))
            (testing "definition, instance and take stay distinct"
              (is (= (mapv #(nth % 4) launches) (mapv :instance-id takes)))
              (is (= ["pads.1" "pads.2" "pads.3" "pads.4"] (mapv :take-id takes)))
              (is (= [1 1 1 2] (mapv :run takes)))
              (is (every? #(= :live-input (get-in % [:source :origin])) takes)))))
        (let [performed (ableton-launches rt)
              takes-before (count (runs/takes (show/live-runs-record rt)))
              recorded (fake/session-record! ls track-id 0 8)
              pad-clip (launcher/pad-clip (show/live-runs-record rt) (show/live-launcher rt :pads))]
          (testing "the clip Live recorded holds the same pad hits Tolglow writes from its takes"
            (is (= (get-in (live/request! conn :read-clip {:clip-id recorded}) [:result :clip :notes])
                   (:notes (:clip pad-clip))
                   [{:pitch 36 :start 1 :duration 2 :velocity 100} {:pitch 68 :start 2 :duration 2 :velocity 100}
                    {:pitch 64 :start 5 :duration 2 :velocity 100} {:pitch 36 :start 6 :duration 1/2 :velocity 100}])
                "pitch, timing and how hard each pad was hit")
            (is (= [0 8 8] ((juxt :from-beat :to-beat (comp :length-beats :clip)) pad-clip))))
          (testing "replaying Live's recorded clip launches the same cues on the same beats as playback, with no new takes"
            (replay! f ls recorded 16)
            (let [replayed (drop (count performed) (ableton-launches rt))]
              (is (= (mapv (fn [[cue beat _ note]] [cue (+ 16 beat) :live-playback note]) performed)
                     (mapv #(subvec % 0 4) replayed)))
              (is (= takes-before (count (runs/takes (show/live-runs-record rt)))) "playback is never a performance")
              (is (empty? (:instances (show/snapshot rt))) "each replayed hit was released by its note-off")))
          (testing "a clip Tolglow writes from the run record replays the same way"
            (let [written (show/write-live-launcher-clip! rt {:page-id :pads :name "Pads take"})
                  before (count (ableton-launches rt))]
              (is (= :executed (:status written)) (pr-str written))
              (is (= [4 ["pads.1" "pads.2" "pads.3" "pads.4"] []] ((juxt :notes :takes :skipped) written)))
              (is (= ["Recorded" "Pads take"]
                     (mapv :name (:clips (first (filter :companion? (:tracks (:result (live/request! conn :inspect-set {})))))))))
              (is (= "executed" (get-in (json/read-str (:body (request app "/api/live-launcher" "page-id=pads")) :key-fn keyword)
                                        [:clip :status]))
                  "Studio shows the clip the REPL wrote")
              (is (= "executed" (:status (:json (post app "/api/live-launcher/write-clip" {:page-id "pads" :name "Pads again"}))))
                  "Studio writes one the same way")
              (is (= ["Recorded" "Pads take" "Pads again"]
                     (mapv :name (:clips (first (filter :companion? (:tracks (:result (live/request! conn :inspect-set {})))))))))
              (replay! f ls (:clip-id written) 32)
              (is (= (mapv (fn [[cue beat _ note]] [cue (+ 32 beat) :live-playback note]) performed)
                     (mapv #(subvec % 0 4) (drop before (ableton-launches rt)))))
              (is (= takes-before (count (runs/takes (show/live-runs-record rt)))))))
          (testing "nothing was ever written back into Live through the bridge's recording route"
            (is (not-any? #{:record-event} @requests))
            (is (= #{:inspect :ensure-launcher :write-pads :inspect-launcher :write-clip} (set @requests))
                "the bridge carried its status polls and the launcher's operations, under one session"))))
      (finally (show/stop! rt)))))

(deftest a-live-without-insert-calls-is-reported-and-nothing-is-written
  (let [{:keys [rt ls] :as f} (fixture (fake/create {:device-insert? false}))]
    (try
      (start-bridge! f)
      (let [setup (show/setup-live-launcher! rt {:page-id :pads})]
        (is (false? (:ready? setup)))
        (is (= [[:ensure-launcher :rejected]] (:applied setup)))
        (is (= #{:drum-rack-creation-unavailable :not-launcher-track :no-drum-rack} (set (map :code (:problems setup)))))
        (is (= "MIDI" (:name (first (filter :companion? (:tracks @ls))))) "the track is not renamed")
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Set the launcher up" (show/write-live-launcher-clip! rt {:page-id :pads}))))
      (finally (show/stop! rt))))
  (testing "a performer's own Drum Rack on that Live gets names, empty pads are reported, other filled pads warned about"
    (let [{:keys [rt ls] :as f} (fixture (fake/create {:device-insert? false :companion? false
                                                       :tracks [{:name "Drums" :companion? true :drum-rack {36 "Kick" 40 "Snare"}}]}))]
      (try
        (start-bridge! f)
        (let [setup (show/setup-live-launcher! rt {:page-id :pads})]
          (is (false? (:ready? setup)))
          (is (= [{:note 64 :code :chain-creation-unavailable} {:note 68 :code :chain-creation-unavailable}] (:skipped setup)))
          (is (= [{:code :empty-pad :note 64 :expected "Amber"} {:code :empty-pad :note 68 :expected "Blue"}] (:problems setup)))
          (is (= [{:code :stale-pad :note 40 :name "Snare"}] (:warnings setup)) "a filled pad that launches nothing")
          (is (= {36 {:name "Red" :color 0xff0000} 40 {:name "Snare" :color 0x777777}} (get-in @ls [:tracks 0 :rack :chains]))))
        (finally (show/stop! rt))))))

(deftest the-launcher-never-takes-captures-track-and-checks-where-pad-hits-come-from
  (testing "the companion hosting Capture's cue track (#680) is refused before any write"
    (let [{:keys [rt ls] :as f} (fixture (fake/create {:companion? false
                                                       :tracks [{:name "Tolglow Cues" :companion? true :arm? true
                                                                 :input {:port "Tolglow" :channel 16}
                                                                 :slots {0 {:clip-id "clip:1" :name "Tolglow capture"}}}]}))]
      (try
        (start-bridge! f)
        (let [setup (show/setup-live-launcher! rt {:page-id :pads})]
          (is (false? (:ready? setup)))
          (is (some #{:tolglow-track-in-use} (map :code (:problems setup))))
          (is (= "Tolglow Cues" (get-in @ls [:tracks 0 :name])) "Capture keeps its track")
          (is (nil? (get-in @ls [:tracks 0 :rack]))))
        (finally (show/stop! rt)))))
  (testing "a launcher track listening to Tolglow's feed cannot carry pad hits, so it is not ready"
    (let [{:keys [rt] :as f} (fixture (fake/create {:companion? false
                                                    :tracks [{:name "Tolglow launcher pads" :companion? true
                                                              :input {:port "Tolglow" :channel 16}}]}))]
      (try
        (start-bridge! f)
        (let [setup (show/setup-live-launcher! rt {:page-id :pads})]
          (is (false? (:ready? setup)))
          (is (= [{:code :input-is-tolglow-feed :port "Tolglow"} {:code :input-feed-channel :channel 16}] (:problems setup))))
        (finally (show/stop! rt))))))

(deftest a-replay-marked-live-input-would-record-takes
  ;; Negative control for the replay above: the playback origin the companion
  ;; gives a clip's notes is what keeps a replayed pad clip out of the record.
  (let [{:keys [rt ls] :as f} (fixture)
        conn (live/open (fake/transport ls) {})]
    (try
      (start-bridge! f)
      (is (:ready? (show/setup-live-launcher! rt {:page-id :pads})))
      (doseq [[beat cue state] (take 4 performance)]
        (companion! f (hit-event cue state "live-input"))
        (at! f beat))
      (at! f 8)
      (is (= 2 (count (runs/takes (show/live-runs-record rt)))))
      (let [written (show/write-live-launcher-clip! rt {:page-id :pads})]
        (fake/set-song-beat! ls 16)
        (live/request! conn :fire-clip {:clip-id (:clip-id written)})
        (doseq [[beat events] (sort-by key (group-by :song-beat (fake/playback ls 16 20)))]
          (doseq [e events]
            (companion! f {:type (name (:type e)) :note (:note e) :velocity (:velocity e) :origin "live-input"}))
          (at! f beat))
        (at! f 25)
        (is (= 4 (count (runs/takes (show/live-runs-record rt)))) "marked as input, the replay records itself again"))
      (finally (show/stop! rt)))))

(deftest a-pad-clip-cuts-overlaps-on-one-pad-and-skips-what-has-no-pad
  (let [launcher {:pads [{:note 36 :cue-id :red} {:note 68 :cue-id "blue"}]}
        take (fn [id cue start release] {:take-id id :cue-id cue :start-beat start :release-beat release})
        record {:take-order ["t1" "t2" "t3" "t4" "t5"]
                :takes {"t1" (take "t1" :red 4 8) "t2" (take "t2" :red 6 7) "t3" (take "t3" :blue 5 6)
                        "t4" (take "t4" :green 5 6) "t5" (take "t5" :blue 7 nil)}}
        {:keys [clip skipped shortened takes]} (launcher/pad-clip record launcher)]
    (is (= [{:pitch 36 :start 0 :duration 2 :velocity 127} {:pitch 68 :start 1 :duration 1 :velocity 127}
            {:pitch 36 :start 2 :duration 1 :velocity 127}]
           (:notes clip))
        "a second hit on a held pad ends the first: one pad plays one cue at a time")
    (is (= ["t1"] shortened))
    (is (= ["t1" "t3" "t2"] takes))
    (is (= [{:take-id "t4" :reason :no-pad} {:take-id "t5" :reason :running}] skipped))
    (is (= 4 (:length-beats clip)) "whole bars around the takes")
    (is (= [{:pitch 36 :start 0 :duration 1 :velocity 127}]
           (:notes (:clip (launcher/pad-clip record launcher {:from-beat 6 :to-beat 7}))))
        "an explicit range cuts notes at the clip end")
    (is (nil? (:clip (launcher/pad-clip record launcher {:from-beat 20 :to-beat 24}))))))
