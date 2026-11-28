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
  reach the simulated Live, bridge-only operations (status polls, handoff)
  answer executed. Every operation is logged."
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
            (is (= {:bound? true :tracks 1 :running 0 :pending 0 :queued 0 :dropped 0 :events 0 :commands 0} (:playback live)))
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
