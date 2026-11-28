(ns tolglow.integrations.live-capture-test
  "Capture to Live from a real show runtime (#671, #680): one validated
  command from the REPL and Studio sets the capture tracks up, writes the
  phrase as a clip and focuses it; clip playback from the capture track comes
  back through the running Live bridge into the capture decoder, never also
  into the pad mappings; and the MIDI layout and capture tracks are saved
  with the show. Live is tolglow.integrations.fake-live behind the bridge's
  companion transport; no Live, Max or MIDI device is used."
  (:require [afterglow.control.model :as control]
            [afterglow.sync :as sync]
            [afterglow.sync.session :as session]
            [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [tolglow.connections :as connections]
            [tolglow.integrations.fake-live :as fake]
            [tolglow.integrations.live-capture :as live-capture]
            [tolglow.integrations.live-connection :as live]
            [tolglow.integrations.live-sync :as live-sync]
            [tolglow.integrations.phrase-clip :as clip]
            [tolglow.persistence :as persistence]
            [tolglow.show :as show]
            [tolglow.studio :as studio])
  (:import [java.io ByteArrayInputStream]))

(defn- effect [id]
  {:id id :version 1 :category :light
   :params {:level {:type :number :default 0.0 :bounds [0.0 1.0]}}
   :outputs {:dimmer {:op :param :key :level}}})

(def registry
  {:effects {:wash (effect :wash)}
   :cues {:red {:id :red :effect-id :wash :version 1 :name "Red"}
          :blue {:id :blue :effect-id :wash :version 1 :name "Blue"}
          :amber {:id :amber :effect-id :wash :version 1 :name "Amber"}}})

(def page
  "Red's pad is note 36 in Push's 64 Pads layout, the pitch the capture layout
  also gives the first captured cue: playback must not launch it twice."
  {:id :pads :cells {[0 7] :red [4 7] :blue [0 0] :amber}})

(defn- bridge-factory
  "The Live bridge's companion transport over a simulated Set: Set operations
  and the surface handoff reach the simulated Live (which then forwards MIDI
  as the companion does in that mode, fake/companion-midi), status polls
  answer executed. Every operation is logged."
  [ls requests]
  (fn [_]
    (let [inner (fake/transport ls {:json? true})]
      {:open! (fn [_] :bridge) :close! (fn [_])
       :request! (fn [handle id op payload]
                   (swap! requests conj op)
                   (if (or (contains? live/operations op) (#{:handoff :inspect} op))
                     ;; Keyword statuses, as live-transport reads the companion's replies.
                     (update ((:request! inner) handle id op payload) :status keyword)
                     {:id id :status :executed :result {}}))})))

(defn- fixture
  ([] (fixture (fake/create {:tracks [{:name "Bass" :arm? true :input {:port "Keyboard" :channel 1}}]
                             :envelope-lanes? true})))
  ([ls]
   (let [requests (atom [])
         rt (show/create-runtime
             {:model (control/new-show {:show-id :capture-live :transport {:playing? true :beat 0.0 :bpm 120.0}
                                        :registry registry :pages {:pads page}
                                        :rig {:fixtures [{:id :left :universe 1 :address 1 :channels {:dimmer 1}}]}})
              :sink {:send! (fn [_ _])} :workspace-options nil :live-runs false
              :sync-options {:live-factory (bridge-factory ls requests)}})]
     {:rt rt :ls ls :requests requests :manager (connections/sync (:connections rt)) :serial (atom 0)})))

(defn- start-bridge! [{:keys [rt manager]}]
  (show/sync-action! rt :configure {:config {:live {:enabled? true}} :page-id "pads"})
  (show/sync-action! rt :start {})
  (let [deadline (+ (System/currentTimeMillis) 10000)]
    (while (and (not (live-sync/bridge-connected? manager)) (< (System/currentTimeMillis) deadline))
      (Thread/sleep 5)))
  (is (live-sync/bridge-connected? manager) "the bridge opened its companion connection"))

(defn- at! [{:keys [rt manager]} beat]
  ;; 120 BPM: a beat is 500 ms. Drain the bridge, run the owner cycle, let
  ;; receipts settle and run it again.
  (session/flush! manager)
  (show/tick! rt (long (* 500 beat)))
  (session/flush! manager)
  (show/tick! rt (long (* 500 beat))))

(def performer {:adapter :push :session-id "performer"})

(defn- perform!
  "Red from beat 1 at level 0.5, moved to 0.75 at beat 2 and ended at 5;
  blue from 3 to 6. Two bars ending at beat 8."
  [{:keys [rt] :as f}]
  (let [submit! (fn [beat payload] (let [r (show/submit! rt :command payload)] (at! f beat) (deref r 1000 ::missing)))
        red (:instance-id (submit! 1 {:op :cue/launch :cue-id :red :source performer
                                      :launch {:mode :persistent :params {:level 0.5}}}))]
    (submit! 2 {:op :param/set :instance-id red :param :level :value 0.75 :source performer})
    (let [blue (:instance-id (submit! 3 {:op :cue/launch :cue-id :blue :source performer :launch {:mode :persistent}}))]
      (submit! 5 {:op :instance/end :instance-id red :source performer})
      (submit! 6 {:op :instance/end :instance-id blue :source performer}))
    (at! f 8)))

(defn- companion!
  "One MIDI callback from the companion, as Studio's /api/live receives it."
  [{:keys [rt manager serial]} event]
  (show/sync-action! rt :live {:protocol-version 1 :bridge-id "tolglow-m4l"
                               :sync-session-id (sync/active-run-id manager)
                               :event-id (str "companion:" (swap! serial inc)) :kind "midi"
                               :event (merge {:channel 1} event)}))

(defn- close? [a b] (and (number? a) (number? b) (<= (Math/abs (- (double a) (double b))) (/ 1.0 16383))))

(defn- companion-callback!
  "One non-MIDI callback from the companion (a clip stop on the capture host)."
  [{:keys [rt manager serial]} {:keys [kind event]}]
  (show/sync-action! rt :live {:protocol-version 1 :bridge-id "tolglow-m4l"
                               :sync-session-id (sync/active-run-id manager)
                               :event-id (str "companion:" (swap! serial inc)) :kind kind :event event}))

(defn- forward!
  "Live plays the events of song beats [from to): each reaches Tolglow only
  if the simulated companion forwards it in its current mode, then the owner
  runs at show beat (+ offset beat)."
  [{:keys [ls] :as f} from to offset]
  (doseq [[beat events] (sort-by key (group-by :song-beat (fake/playback ls from to)))]
    (doseq [e events]
      (when-let [out (fake/companion-midi ls (dissoc e :song-beat))]
        (companion! f (update out :type name))))
    (at! f (+ offset beat))))

(defn- capture-instances
  "Running instances the capture decoder or a pad launched: [cue-id adapter]."
  [rt]
  (vec (sort (for [[_ i] (get-in @(:state rt) [:model :instances])
                   :when (and (= :running (:phase i)) (= :ableton (get-in i [:source :type])))]
               [(:cue-id i) (get-in i [:source :adapter])]))))

(defn- note!
  "One note the companion forwards from its track, as Live plays it."
  [{:keys [ls] :as f} type pitch]
  (when-let [out (fake/companion-midi ls {:type type :note pitch :velocity (if (= :note-on type) 127 0) :channel 1})]
    (companion! f (update out :type name))))

(defn- history [rt pred]
  (let [{:keys [history history-order]} (get-in @(:state rt) [:model :control])]
    (vec (for [id history-order :let [{:keys [command ack]} (get history id)] :when (pred command ack)]
           [command ack]))))

(defn- request [app method uri & [body]]
  (let [response (app {:request-method method :uri uri :headers {"host" "localhost" "content-type" "application/json"}
                       :body (ByteArrayInputStream. (.getBytes (if body (json/write-str body) "") "UTF-8"))})]
    (assoc response :json (try (json/read-str (:body response) :key-fn keyword) (catch Exception _ nil)))))

(deftest one-validated-command-from-the-repl-studio-and-push
  (let [{:keys [rt]} (fixture)
        app (studio/handler rt)]
    (try
      (testing "the command shape is checked before anything runs"
        (is (= [:send {:bars 2 :handoff? true}] (live-capture/validate-command :send {"bars" 2 :handoff? true :name nil})))
        (is (= [:focus {:clip-id "clip:1"}] (live-capture/validate-command "focus" {:clip-id "clip:1"})))
        (doseq [[op payload] [[:send {:bars 0}] [:send {:end-beat -1}] [:send {:focus? "yes"}] [:send {:slot-index 1.5}]
                              [:focus {:handoff? 1}] [:capture {:name "x"}] [:setup {:bars 2}]]]
          (is (= 400 (:http/status (ex-data (is (thrown? clojure.lang.ExceptionInfo (show/capture-action! rt op payload))))))
              (pr-str op payload)))
        (is (= 404 (:http/status (ex-data (is (thrown? clojure.lang.ExceptionInfo (show/capture-action! rt :record {}))))))))
      (testing "Studio refuses the same payloads with the same message"
        (let [r (request app :post "/api/capture/send" {:bars 0})]
          (is (= 400 (:status r)))
          (is (re-find #"bars" (get-in r [:json :message]))))
        (is (= 400 (:status (request app :post "/api/capture/send" {:volume 3})))))
      (finally (show/stop! rt)))))

(deftest send-to-live-replays-through-the-bridge-into-the-capture-decoder
  (let [{:keys [rt ls requests] :as f} (fixture)
        app (studio/handler rt)]
    (try
      (at! f 0)
      (perform! f)
      (testing "without the Live bridge the capture is kept and the send reports why nothing reached Live"
        (let [r (show/send-capture! rt {:bars 2 :end-beat 8})]
          (is (= :captured (:status r)))
          (is (= 2 (:notes r)))
          (is (= :not-ready (get-in r [:live :status])))
          (is (= [{:code :not-connected :status :rejected :op :inspect-set}] (get-in r [:live :setup :problems])))
          (is (empty? @requests) "nothing was sent")))
      (start-bridge! f)
      (let [sent (show/send-capture! rt {:capture? false})
            clip-id (get-in sent [:live :clips 0 :clip-id])
            host (first (filter :companion? (:tracks @ls)))]
        (testing "one REPL call sets the capture track up, writes the clip and focuses it"
          (is (= :sent (get-in sent [:live :status])) (pr-str (:live sent)))
          (is (get-in sent [:live :setup :ready?]))
          (is (= [{:channel 1 :track-id (:track-id host) :name "Tolglow Capture"}] (get-in sent [:live :setup :tracks])))
          (is (= {:arm? true :monitor :auto :input {:port "Tolglow" :channel 16}} (select-keys host [:arm? :monitor :input])))
          (is (= {:port "Keyboard" :channel 1} (:input (first (:tracks @ls)))) "the musician's track is untouched")
          (is (= {:status :executed :clip-id clip-id :view "Detail/Clip"} (get-in sent [:live :focus])))
          (is (= clip-id (:selected @ls)))
          (is (not-any? #{:capture-midi} @requests) "one capture route only"))
        (testing "the status shows the bound playback return"
          (let [live (:live (show/capture-action! rt :status {}))]
            (is (= {:bound? true :tracks 1 :running 0 :pending 0 :queued 0 :dropped 0 :unbound-dropped 0 :events 0 :commands 0
                    :surface-mode :lighting :live-mode-return? false}
                   (dissoc (:playback live) :companion-host?)))
            (is (= "Tolglow Capture" (get-in live [:desired :tracks 0 :name])))
            (is (= clip-id (get-in live [:sent :clips 0 :clip-id])))))
        (testing "Live's playback of the clip launches each cue once, from the capture decoder only"
          (let [before (count (history rt (constantly true)))]
            (fake/set-song-beat! ls 0)
            (is (= :executed (:status (live/request! (live/open (fake/transport ls) {}) :fire-clip {:clip-id clip-id}))))
            (doseq [[beat events] (sort-by key (group-by :song-beat (fake/playback ls 0 8)))]
              (doseq [e events]
                (is (true? (:accepted? (companion! f (cond-> {:type (name (:type e)) :origin "live-playback"
                                                                :track-id (:track-id e)}
                                                         (:note e) (assoc :note (:note e) :velocity (:velocity e))
                                                         (:controller e) (assoc :controller (:controller e) :value (:value e))))))))
              (at! f (+ 12 beat)))
            (at! f 21)
            (let [played (drop before (history rt (constantly true)))
                  launches (filter (fn [[c a]] (and (= :cue/launch (:op c)) (= :accepted (:status a)))) played)
                  by-id (into {} (map (fn [[c a]] [(:instance-id a) (:cue-id c)])) launches)]
              (is (= [:red :blue] (mapv (comp :cue-id first) launches)) "one launch per recorded activation")
              (is (every? #(= {:type :ableton :adapter :live-capture-clip :origin :live-playback} (:source (first %))) launches)
                  "the pad mapping for note 36 did not launch red a second time")
              (is (close? 0.5 (get-in (first (first launches)) [:launch :params :level])) "the launch value, to 14 bits")
              (is (some (fn [[c a]] (and (= :param/set (:op c)) (= :red (by-id (:instance-id c))) (close? 0.75 (:value c))
                                         (= :accepted (:status a))))
                        played)
                  "the level move came back")
              (is (= #{:red :blue} (set (keep (fn [[c a]] (when (and (= :instance/end (:op c)) (= :accepted (:status a)))
                                                            (by-id (:instance-id c))))
                                              played)))
                  "both runs ended with their notes")
              (is (empty? (filter #(= :live-playback (:origin %)) (vals (get-in @(:state rt) [:model :instances])))))
              (is (pos? (get-in (show/capture-action! rt :status {}) [:live :playback :events]))))))
        (testing "playback from a track that is not a capture track goes to the pad mappings"
          (is (false? (show/live-capture-playback! rt {:type :note-on :note 36 :velocity 100 :channel 1
                                                       :origin :live-playback :track-id "track:elsewhere"})))
          (is (false? (show/live-capture-playback! rt {:type :note-on :note 36 :velocity 100 :channel 1
                                                       :origin :live-input :track-id (:track-id host)}))
              "the performer's live input on the capture track is not playback")
          (let [before (count (history rt (constantly true)))]
            (companion! f {:type "note-on" :note 36 :velocity 100 :origin "live-input" :track-id (:track-id host)})
            (at! f 22)
            (companion! f {:type "note-off" :note 36 :velocity 0 :origin "live-input" :track-id (:track-id host)})
            (at! f 23)
            (is (= [[:red 36]] (vec (for [[c a] (drop before (history rt (constantly true)))
                                          :when (and (= :cue/launch (:op c)) (= :accepted (:status a)))]
                                      [(:cue-id c) (get-in c [:source :note])])))
                "the same note played live launches red through its pad, so the claim is what kept playback single")))
        (testing "the clip-focus handoff waits for the companion's receipt, from Studio as from the REPL"
          (let [r (:json (request app :post "/api/capture/focus" {:handoff? true}))]
            (is (= {:status "executed" :clip-id clip-id :view "Detail/Clip" :handoff {:status "executed"}} r) (pr-str r))
            (is (some #{:handoff} @requests))
            (is (= :live (get-in (sync/export-state (connections/sync (:connections rt))) [:config :live :mode])))))
        (testing "a second capture sent from Studio reuses the capture track and the same pitches"
          (let [r (:json (request app :post "/api/capture/send" {:capture? false :name "Take 2"}))]
            (is (= "sent" (get-in r [:live :status])) (pr-str r))
            (is (= [] (get-in r [:live :setup :applied]))
                "setup wrote nothing on the rerun")
            (is (= 1 (count (filter #(= "Tolglow Capture" (:name %)) (:tracks @ls)))))
            (is (nil? (:phrase r)) "the phrase stays on the host"))))
      (finally (show/stop! rt)))))

(deftest a-refused-handoff-leaves-the-clip-unfocused
  (let [ls (fake/create {:tracks [] :envelope-lanes? true})
        {:keys [rt] :as f} (fixture ls)]
    (try
      (at! f 0)
      (perform! f)
      (testing "without a bridge the handoff is refused and Live's view is left alone"
        (show/capture-phrase! rt {:bars 2 :end-beat 8})
        (is (= {:status :rejected :code :not-connected :clip-id "clip:9" :handoff {:status :rejected :code :not-connected}}
               (show/focus-capture! rt {:clip-id "clip:9" :handoff? true})))
        (is (nil? (:selected @ls))))
      (testing "focus needs a sent clip or a named one"
        (is (= 409 (:http/status (ex-data (is (thrown? clojure.lang.ExceptionInfo (show/focus-capture! rt))))))))
      (finally (show/stop! rt)))))

(deftest the-midi-layout-and-capture-tracks-are-saved-with-the-show
  (let [{:keys [rt] :as f} (fixture)]
    (try
      (at! f 0)
      (perform! f)
      (start-bridge! f)
      (is (= :sent (get-in (show/send-capture! rt {:bars 2 :end-beat 8}) [:live :status])))
      (let [source (show/export-show-source rt)
            saved (get-in (persistence/read-string source) [:model :integrations :live-capture])]
        (testing "the show file carries the layout and the capture tracks, but no Live session IDs"
          (is (= 1 (:version saved)))
          (is (= {:red {:pitch 36} :blue {:pitch 37}} (get-in saved [:layout :cues])))
          (is (= [[:red :level]] (mapv (juxt :cue-id :param) (get-in saved [:layout :lanes]))))
          (is (= [{:channel 1 :name "Tolglow Capture" :kind :midi :arm? true :monitor :auto
                   :input {:port "Tolglow" :channel 16} :feed-channel 16 :host? true}]
                 (get-in saved [:desired :tracks])))
          (is (not (re-find #"track:" (pr-str saved)))))
        (testing "loading the show restores them, and later captures keep every address"
          (let [ls2 (fake/create {:tracks [] :envelope-lanes? true})
                {rt2 :rt :as f2} (fixture ls2)]
            (try
              (at! f2 0)
              (let [reply (show/submit! rt2 :import {:source source})]
                (at! f2 1)
                (is (= :accepted (:status (deref reply 2000 ::missing)))))
              (let [live (:live (show/capture-action! rt2 :status {}))]
                (is (= (:desired saved) (select-keys (:desired live) [:route :tracks])
                       (select-keys (:desired saved) [:route :tracks])))
                (is (nil? (:setup live)) "track bindings are not restored")
                (is (false? (get-in live [:playback :bound?]))))
              (is (= saved (get-in (persistence/read-string (show/export-show-source rt2)) [:model :integrations :live-capture]))
                  "saving again writes the same capture data")
              (let [base (long (/ (get-in @(:state rt2) [:model :transport :beat]) 1))
                    amber (:instance-id (deref (show/submit! rt2 :command {:op :cue/launch :cue-id :amber :source performer
                                                                           :launch {:mode :persistent}}) 1000 ::missing))]
                (at! f2 (+ base 2))
                (deref (show/submit! rt2 :command {:op :instance/end :instance-id amber :source performer}) 1000 ::missing)
                (at! f2 (+ base 3))
                (is (= :captured (:status (show/capture-phrase! rt2 {:bars 1}))))
                (is (= {:red {:pitch 36} :blue {:pitch 37} :amber {:pitch 38}}
                       (get-in (persistence/read-string (show/export-show-source rt2)) [:model :integrations :live-capture :layout :cues]))))
              (finally (show/stop! rt2))))))
      (finally (show/stop! rt)))))

(deftest saved-capture-data-is-validated
  (let [layout (clip/allocate-layout {} registry {:cues [:red :blue] :params [[:red :level]]})
        desired (live/desired-setup layout {})
        valid {:version 1 :layout (dissoc layout :unassigned) :desired desired}]
    (is (= valid (live-capture/validate-saved valid)))
    (doseq [[label bad] [["two cues on one pitch" (assoc-in valid [:layout :cues :blue :pitch] 36)]
                         ["two components on one controller"
                          (update-in valid [:layout :lanes] conj {:cue-id :blue :param :level
                                                                  :components [{:type :number :bounds [0.0 1.0] :cc 1}]})]
                         ["channel 17" (assoc-in valid [:layout :channel] 17)]
                         ["a feed channel that is a playback channel" (assoc-in valid [:desired :tracks 0 :feed-channel] 1)]
                         ["tracks for other channels" (assoc-in valid [:desired :tracks 0 :channel] 2)]
                         ["an unknown route" (assoc-in valid [:desired :route] :both)]
                         ["an unknown key" (assoc valid :track-ids {1 "track:3"})]]]
      (testing label
        (is (thrown? clojure.lang.ExceptionInfo (live-capture/validate-saved bad)))))
    (testing "a show file with an invalid layout does not load"
      (let [model (assoc-in (control/new-show {:show-id :bad :registry registry}) [:integrations :live-capture]
                            (assoc-in valid [:layout :cues :blue :pitch] 36))]
        (is (thrown? clojure.lang.ExceptionInfo (persistence/write-string model)))))))

;; ---------------------------------------------------------------------------
;; Review of #817: bindings, handoff, overflow, retries and stale tracks

(defn- sent!
  "Perform, connect and send a capture without focusing it, then start its
  clip playing on the capture host at song beat 0. Returns [host clip-id]."
  [{:keys [rt ls] :as f}]
  (at! f 0)
  (perform! f)
  (start-bridge! f)
  (let [r (show/send-capture! rt {:bars 2 :end-beat 8 :focus? false})
        clip-id (get-in r [:live :clips 0 :clip-id])]
    (is (= :sent (get-in r [:live :status])) (pr-str (:live r)))
    (fake/set-song-beat! ls 0)
    (is (= :executed (:status (live/request! (live/open (fake/transport ls) {}) :fire-clip {:clip-id clip-id}))))
    [(first (filter :companion? (:tracks @ls))) clip-id]))

(defn- playback [rt] (get-in (show/capture-action! rt :status {}) [:live :playback]))

(defn- await-companion!
  "Run the owner at show beat `beat` until the companion's status poll (every
  500 ms of wall time) satisfies pred, bounded."
  [{:keys [manager] :as f} beat pred]
  (let [deadline (+ (System/currentTimeMillis) 5000)]
    (while (and (not (pred (live-sync/companion-capture manager))) (< (System/currentTimeMillis) deadline))
      (at! f beat)
      (Thread/sleep 25)))
  (is (pred (live-sync/companion-capture manager)) "the companion's status poll arrived"))

(deftest a-setup-that-cannot-inspect-the-set-keeps-the-binding
  (let [{:keys [rt ls] :as f} (fixture)]
    (try
      (sent! f)
      (note! f :note-on 37)
      (at! f 12)
      (is (= [[:blue :live-capture-clip]] (capture-instances rt)))
      (testing "a refused inspection proves nothing: the binding and its run stay"
        (fake/inject-fault! ls :inspect-set :refuse)
        (let [setup (show/setup-live-capture! rt)]
          (is (false? (:ready? setup)))
          (is (= [:injected] (mapv :code (:problems setup)))))
        (is (true? (:bound? (playback rt)))))
      (testing "the clip's note-off still ends the decoder's run and the next loop launches through the decoder, not pad 37"
        (note! f :note-off 37)
        (at! f 13)
        (is (= [] (capture-instances rt)))
        (note! f :note-on 37)
        (at! f 14)
        (is (= [[:blue :live-capture-clip]] (capture-instances rt)))
        (note! f :note-off 37)
        (at! f 15)
        (is (= [] (capture-instances rt))))
      (testing "a read-back that no longer finds the capture track drops the binding and releases its runs"
        (note! f :note-on 36)
        (at! f 16)
        (is (= [[:red :live-capture-clip]] (capture-instances rt)))
        (swap! ls update :tracks (fn [ts] (mapv #(if (:companion? %) (assoc % :name "Somebody's synth") %) ts)))
        (fake/inject-fault! ls :ensure-track :refuse)
        (is (false? (:ready? (show/setup-live-capture! rt))))
        (at! f 17)
        (is (= [] (capture-instances rt)) "the run whose track is gone was released")
        (is (false? (:bound? (playback rt)))))
      (finally (show/stop! rt)))))

(deftest send-to-live-never-takes-over-the-cue-launchers-track
  (let [ls (fake/create {:companion? false :envelope-lanes? true
                         :tracks [{:name "Tolglow launcher pads" :companion? true :drum-rack {36 "Red"} :arm? true
                                   :input {:port "Ableton Push 3" :channel nil}}]})
        {:keys [rt] :as f} (fixture ls)]
    (try
      (at! f 0)
      (perform! f)
      (start-bridge! f)
      (let [before (first (:tracks @ls))
            r (show/send-capture! rt {:bars 2 :end-beat 8})]
        (is (= :not-ready (get-in r [:live :status])))
        (is (= {:op :ensure-track :status :rejected :code :tolglow-track-in-use} (first (get-in r [:live :setup :problems])))
            (pr-str (get-in r [:live :setup])))
        (is (= before (first (:tracks @ls))) "the launcher track is not renamed, armed or rerouted")
        (is (nil? (:capture-host @ls)))
        (is (false? (:bound? (playback rt)))))
      (testing "the launcher's own pad clip still plays through its pad mappings"
        (swap! ls assoc-in [:playing (:track-id (first (:tracks @ls)))] {:clip-id "none" :from 0})
        (let [before (count (history rt (constantly true)))]
          (note! f :note-on 36)
          (at! f 12)
          (note! f :note-off 36)
          (at! f 13)
          (is (= [[:red 36]] (vec (for [[c a] (drop before (history rt (constantly true)))
                                        :when (and (= :cue/launch (:op c)) (= :accepted (:status a)))]
                                    [(:cue-id c) (get-in c [:source :note])]))))))
      (finally (show/stop! rt)))))

(deftest a-handoff-releases-decoder-runs-and-live-mode-forwards-only-capture-playback
  (let [{:keys [rt ls] :as f} (fixture)]
    (try
      (let [[host clip-id] (sent! f)]
        (note! f :note-on 36)
        (at! f 12)
        (is (= [[:red :live-capture-clip]] (capture-instances rt)))
        (testing "a handoff into Live mode while the companion forwards the playing capture clip keeps its runs"
          (await-companion! f 12 #(and (= (:track-id host) (:capture-host %)) (:capture-playing? %)))
          (is (= :executed (:status (show/focus-capture! rt {:handoff? true}))))
          (at! f 13)
          (at! f 14)
          (is (= [[:red :live-capture-clip]] (capture-instances rt)))
          (is (= :live (:mode @ls)))
          (is (true? (:live-mode-return? (playback rt))))
          (note! f :note-off 36)
          (at! f 15)
          (is (= [] (capture-instances rt)) "the clip's own note-off, forwarded in Live mode, ended it"))
        (testing "a companion that reloaded forgets its capture host: the indicator stops claiming the return and a handoff releases"
          (show/sync-action! rt :handoff {:mode :lighting})
          (at! f 16)
          (note! f :note-on 36)
          (at! f 17)
          (is (= [[:red :live-capture-clip]] (capture-instances rt)))
          (swap! ls assoc :capture-host nil)
          (await-companion! f 17 #(and % (nil? (:capture-host %))))
          (is (= :executed (:status (show/focus-capture! rt {:handoff? true}))))
          (at! f 18)
          (at! f 19)
          (is (= [] (capture-instances rt)))
          (is (= {:bound? true :surface-mode :live :live-mode-return? false :companion-host? false}
                 (select-keys (playback rt) [:bound? :surface-mode :live-mode-return? :companion-host?])))
          (is (:ready? (show/setup-live-capture! rt)) "a rerun of setup lets the companion adopt its track again")
          (await-companion! f 19 #(= (:track-id host) (:capture-host %)))
          (is (true? (:live-mode-return? (playback rt)))))
        (testing "in Live mode the capture clip still plays into the show, from the decoder only"
          (forward! f 0 8 20)
          (is (some #{[:red :live-capture-clip]} (map (fn [[c]] [(:cue-id c) (get-in c [:source :adapter])])
                                                      (history rt (fn [c a] (and (= :cue/launch (:op c)) (= :accepted (:status a))
                                                                                 (= :live-capture-clip (get-in c [:source :adapter])))))))))
        (testing "a note heard while no clip plays (auditioning in Clip View) is not forwarded"
          (live/request! (live/open (fake/transport ls) {}) :stop-clips {:track-ids [(:track-id host)]})
          (doseq [e (fake/companion-events! ls)] (companion-callback! f e))
          (at! f 30)
          (is (nil? (fake/companion-midi ls {:type :note-on :note 36 :velocity 100 :channel 1}))))
        (testing "a clip stop on the capture host ends what its playback started"
          (live/request! (live/open (fake/transport ls) {}) :fire-clip {:clip-id clip-id})
          (note! f :note-on 37)
          (at! f 31)
          (is (= [[:blue :live-capture-clip]] (capture-instances rt)))
          (live/request! (live/open (fake/transport ls) {}) :stop-clips {:track-ids [(:track-id host)]})
          (let [events (fake/companion-events! ls)]
            (is (= [{:kind "run-clip" :event {:type "clip-stopped" :track-id (:track-id host) :capture? true}}] events))
            (doseq [e events] (companion-callback! f e)))
          (at! f 32)
          (at! f 33)
          (is (= [] (capture-instances rt))))
        (testing "the surface coming back to lighting releases runs too"
          (live/request! (live/open (fake/transport ls) {}) :fire-clip {:clip-id clip-id})
          (note! f :note-on 37)
          (at! f 34)
          (is (= [[:blue :live-capture-clip]] (capture-instances rt)))
          (show/sync-action! rt :handoff {:mode :lighting})
          (at! f 35)
          (at! f 36)
          (is (= [] (capture-instances rt)))
          (is (= :lighting (:mode @ls)))))
      (finally (show/stop! rt)))))

(deftest a-refused-or-lost-handoff-takes-the-surface-back
  (let [{:keys [rt ls manager] :as f} (fixture)]
    (try
      (sent! f)
      (let [mode #(get-in (sync/export-state manager) [:config :live :mode])]
        (fake/inject-fault! ls :handoff :refuse)
        (let [r (show/focus-capture! rt {:handoff? true})]
          (is (= {:status :rejected :code :injected} (:handoff r)))
          (is (nil? (:view r)) "the clip was not focused"))
        (session/flush! manager)
        (is (= :lighting (mode)) "Tolglow rolled its surface back")
        (is (= :lighting (:mode @ls)))
        (fake/inject-fault! ls :handoff :lose-ack)
        (is (= :indeterminate (get-in (show/focus-capture! rt {:handoff? true}) [:handoff :status])))
        (session/flush! manager)
        (let [deadline (+ (System/currentTimeMillis) 5000)]
          (while (and (not= :lighting (:mode @ls)) (< (System/currentTimeMillis) deadline)) (Thread/sleep 10)))
        (is (= :lighting (mode)))
        (is (= :lighting (:mode @ls)) "a lost handoff tells the companion to come back to lighting"))
      (finally (show/stop! rt)))))

(deftest an-inbox-overflow-never-drops-a-release
  (let [{:keys [rt ls] :as f} (fixture)]
    (try
      (let [[host] (sent! f)
            ev (fn [e] (show/live-capture-playback! rt (merge {:channel 1 :origin :live-playback :track-id (:track-id host)
                                                               :track-name (:name host) :time-ms 0} e)))]
        (ev {:type :note-on :note 36 :velocity 127})
        (at! f 12)
        (is (= [[:red :live-capture-clip]] (capture-instances rt)))
        (dotimes [_ 512] (ev {:type :cc :controller 1 :value 64}))
        (is (true? (ev {:type :note-off :note 36 :velocity 0})))
        (is (= {:queued 512 :dropped 1} (select-keys (playback rt) [:queued :dropped])))
        (at! f 13)
        (is (= [] (capture-instances rt)) "the note-off was kept and ended the run")
        (is (some #(= {:type :live/capture-playback-dropped :dropped 1 :unbound-dropped 0} (select-keys % [:type :dropped :unbound-dropped]))
                  (get-in @(:state rt) [:model :variables :sync :events]))
            "the drop is announced as an event"))
      (finally (show/stop! rt)))))

(deftest a-controller-flood-never-costs-a-launch
  (let [{:keys [rt ls] :as f} (fixture)]
    (try
      (let [[host] (sent! f)
            ev (fn [e] (show/live-capture-playback! rt (merge {:channel 1 :origin :live-playback :track-id (:track-id host)
                                                               :track-name (:name host) :time-ms 0} e)))]
        (ev {:type :note-on :note 36 :velocity 127})
        (dotimes [i 600] (ev {:type :cc :controller (if (even? i) 1 33) :value (mod i 128)}))
        (let [p (playback rt)]
          (is (= 512 (:queued p)))
          (is (= 89 (:dropped p)) "superseded controller values went, the launch stayed"))
        (at! f 12)
        (is (= [[:red :live-capture-clip]] (capture-instances rt))))
      (let [{:keys [inbox]} (#'show/enqueue-capture {:inbox (into [{:type :cc :channel 1 :controller 7 :value 1}
                                                                   {:type :note-on :note 36 :velocity 9 :channel 1}]
                                                                  (repeat 510 {:type :note-off :note 40 :channel 1}))
                                                     :inbox-dropped 0}
                                                    {:type :cc :channel 1 :controller 7 :value 2})]
        (is (= [{:type :note-on :note 36 :velocity 9 :channel 1}] (filter #(= :note-on (:type %)) inbox)))
        (is (= [2] (map :value (filter #(= :cc (:type %)) inbox)))
            "a value a launch took is not coalesced; with nothing superseded the oldest controller still goes before the launch"))
      (finally (show/stop! rt)))))

(deftest a-refused-end-is-sent-again
  (let [{:keys [rt] :as f} (fixture)
        refused (atom 0)
        submit! show/submit!]
    (try
      (sent! f)
      (note! f :note-on 36)
      (at! f 12)
      (is (= [[:red :live-capture-clip]] (capture-instances rt)))
      ;; The owner queue refuses the decoder's first end, as a full queue would.
      (with-redefs [show/submit! (fn [runtime & args]
                                   (let [payload (last args)]
                                     (if (and (map? payload) (= :instance/end (:op payload))
                                              (= :live-capture-clip (get-in payload [:source :adapter])) (zero? @refused))
                                       (do (swap! refused inc) (deliver (promise) nil)
                                           (doto (promise) (deliver {:status :rejected :command-id (:command-id payload)})))
                                       (apply submit! runtime args))))]
        (note! f :note-off 36)
        (at! f 13)
        (is (= 1 @refused))
        (at! f 14)
        (at! f 15))
      (is (= [] (capture-instances rt)) "the second attempt ended the run")
      (finally (show/stop! rt)))))

(deftest stale-bindings-stop-claiming-and-capture-playback-never-reaches-the-pads
  (let [{:keys [rt ls] :as f} (fixture)]
    (try
      (let [[host] (sent! f)]
        (note! f :note-on 36)
        (at! f 12)
        (is (= [[:red :live-capture-clip]] (capture-instances rt)))
        (testing "after a Set reload the capture track comes back under another ID: the binding is dropped and its run released"
          (swap! ls (fn [s] (-> s (update :tracks (fn [ts] (mapv #(if (:companion? %) (assoc % :track-id "track:500") %) ts)))
                                (assoc :playing {"track:500" (get-in s [:playing (:track-id host)])}))))
          (let [before (count (history rt (constantly true)))]
            (note! f :note-on 37)
            (at! f 13)
            (at! f 14)
            (is (= [] (capture-instances rt)) "red released, blue not launched through pad 37 or the decoder")
            (is (empty? (for [[c a] (drop before (history rt (constantly true)))
                              :when (and (= :cue/launch (:op c)) (= :accepted (:status a)))] c)))
            (let [p (playback rt)]
              (is (false? (:bound? p)))
              (is (= {:reason :track-changed :track-id "track:500" :track-name "Tolglow Capture"} (:stale p)))
              (is (= 1 (:unbound-dropped p))))))
        (testing "a rerun of setup binds the track again by name"
          (is (:ready? (show/setup-live-capture! rt)))
          (note! f :note-on 37)
          (at! f 15)
          (is (= [[:blue :live-capture-clip]] (capture-instances rt)))))
      (finally (show/stop! rt)))))

(deftest a-loaded-show-drops-capture-playback-until-setup
  (let [{:keys [rt ls] :as f} (fixture)]
    (try
      (sent! f)
      (let [source (show/export-show-source rt)
            ls2 (fake/create {:tracks [{:name "Tolglow Capture" :arm? true :companion? true :input {:port "Tolglow" :channel 16}}]
                              :companion? false :envelope-lanes? true})
            {rt2 :rt :as f2} (fixture ls2)
            ;; A load replaces the sync manager.
            f2* #(assoc f2 :manager (connections/sync (:connections rt2)))]
        (try
          (at! f2 0)
          (let [reply (show/submit! rt2 :import {:source source})]
            (at! f2 1)
            (is (= :accepted (:status (deref reply 2000 ::missing)))))
          ;; The show file wanted its Live bridge running: it resumes on load.
          (let [deadline (+ (System/currentTimeMillis) 5000)]
            (while (and (not (live-sync/bridge-connected? (:manager (f2*)))) (< (System/currentTimeMillis) deadline))
              (Thread/sleep 5)))
          (when-not (live-sync/bridge-connected? (:manager (f2*))) (start-bridge! (f2*)))
          (swap! ls2 assoc :playing {"track:1" {:clip-id "kept" :from 0}})
          (let [before (count (history rt2 (constantly true)))]
            (note! (f2*) :note-on 36)
            (at! (f2*) 2)
            (is (empty? (for [[c a] (drop before (history rt2 (constantly true)))
                              :when (and (= :cue/launch (:op c)) (= :accepted (:status a)))] c))
                "a kept capture clip does not launch whatever sits on pad 36")
            (is (= 1 (:unbound-dropped (playback rt2)))))
          (is (:ready? (show/setup-live-capture! rt2)))
          (note! (f2*) :note-off 36)
          (note! (f2*) :note-on 36)
          (at! (f2*) 3)
          (is (= [[:red :live-capture-clip]] (capture-instances rt2)))
          (finally (show/stop! rt2))))
      (finally (show/stop! rt)))))

(deftest saved-capture-data-carries-no-live-ids-and-refuses-newer-versions
  (let [layout (clip/allocate-layout {} registry {:cues [:red] :params []})
        desired (live/desired-setup layout {})
        valid {:version 1 :layout (dissoc layout :unassigned) :desired desired}]
    (doseq [bad [(assoc-in valid [:desired :tracks 0 :track-id] "track:3")
                 (assoc-in valid [:layout :clip-id] "clip:9")
                 (assoc-in valid [:layout :cues :red :clip-id] "clip:9")]]
      (is (re-find #"Live session IDs are never saved"
                   (ex-message (is (thrown? clojure.lang.ExceptionInfo (live-capture/validate-saved bad)))))))
    (testing "a migration step that does not raise the version is refused instead of looping"
      (with-redefs [live-capture/saved-version 2]
        (binding [live-capture/*migrations* {1 identity}]
          (is (= :migration-without-progress
                 (:code (ex-data (is (thrown? clojure.lang.ExceptionInfo (live-capture/migrate-saved valid))))))))))
    (let [e (is (thrown? clojure.lang.ExceptionInfo (live-capture/validate-saved (assoc valid :version 2))))]
      (is (re-find #"saved by a newer Tolglow" (ex-message e)))
      (is (= :newer-capture-data (:code (ex-data e)))))
    (is (= valid (live-capture/saved-state {:layout (-> layout (assoc :clip-id "clip:9") (assoc-in [:cues :red :track-id] "track:3"))
                                            :desired (assoc-in desired [:tracks 0 :track-id] "track:3")}))
        "saving keeps only the documented keys")))

(deftest a-late-receipt-never-rolls-back-a-newer-handoff
  (let [ls (fake/create {:tracks [] :envelope-lanes? true})
        gate (promise)
        first-handoff (atom true)
        requests (atom [])
        inner (bridge-factory ls requests)
        ;; The first handoff's receipt is held back and then lost, as a slow
        ;; companion would answer it after the operator handed the surface on.
        factory (fn [settings]
                  (let [t (inner settings)]
                    (assoc t :request! (fn [handle id op payload]
                                         (if (and (= :handoff op) (compare-and-set! first-handoff true false))
                                           (do (deref gate 5000 nil) {:id id :status :indeterminate})
                                           ((:request! t) handle id op payload))))))
        rt (show/create-runtime
            {:model (control/new-show {:show-id :late-receipt :transport {:playing? true :beat 0.0 :bpm 120.0}
                                       :registry registry :pages {:pads page}
                                       :rig {:fixtures [{:id :left :universe 1 :address 1 :channels {:dimmer 1}}]}})
             :sink {:send! (fn [_ _])} :workspace-options nil :live-runs false
             :sync-options {:live-factory factory}})
        manager (connections/sync (:connections rt))
        f {:rt rt :ls ls :manager manager :serial (atom 0)}
        mode #(get-in (sync/export-state manager) [:config :live :mode])]
    (try
      (at! f 0)
      (start-bridge! f)
      (show/sync-action! rt :handoff {:mode :live})      ; A: its receipt is late
      (show/sync-action! rt :handoff {:mode :lighting})  ; B
      (show/sync-action! rt :handoff {:mode :live})      ; C
      (session/flush! manager)
      (deliver gate true)
      (let [deadline (+ (System/currentTimeMillis) 5000)]
        (while (and (not= 2 (count (filter #{:handoff} @requests))) (< (System/currentTimeMillis) deadline))
          (Thread/sleep 10)))
      (session/flush! manager)
      (Thread/sleep 50)
      (session/flush! manager)
      (is (= :live (mode)) "A's lost receipt did not undo C")
      (is (= :live (:mode @ls)))
      (finally (show/stop! rt)))))
