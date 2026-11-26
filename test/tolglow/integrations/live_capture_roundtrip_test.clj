(ns tolglow.integrations.live-capture-roundtrip-test
  "Perform on a real show runtime, capture the last bars, write them to a
  simulated Live Set, edit the clip there, read it back and replay it through
  Live's playback stream. The replayed show must reproduce the edited clip
  exactly at 70, 120 and 180 BPM (#671, #680). Live itself is simulated by
  tolglow.integrations.fake-live; no Live, Link or MIDI device is touched."
  (:require [afterglow.control.model :as control]
            [clojure.test :refer [deftest is testing]]
            [tolglow.integrations.fake-live :as fake]
            [tolglow.integrations.live-connection :as live]
            [tolglow.integrations.phrase-capture :as capture]
            [tolglow.integrations.phrase-clip :as clip]
            [tolglow.show :as show]))

(defn- effect [id params]
  {:id id :version 1 :category :light
   :params (assoc params :level {:type :number :default 0.0 :bounds [0.0 1.0]})
   :outputs {:dimmer {:op :param :key :level}}})

(def registry
  {:effects {:wash (effect :wash {:mode {:type :enum :values [:soft :hard :pulse] :default :soft}})
             :strobe (effect :strobe {:rate {:type :integer :default 1 :bounds [1 20]}})
             :aim (effect :aim {:direction {:type :vector :size 3 :bounds [-1.0 1.0] :default [0.0 0.0 1.0]}})}
   :cues {:wash {:id :wash :effect-id :wash :version 1}
          :strobe {:id :strobe :effect-id :strobe :version 1}
          :aim {:id :aim :effect-id :aim :version 1}}})

(defn- runtime [bpm]
  (show/create-runtime
   {:model (control/new-show {:show-id :capture-test :transport {:playing? true :beat 0.0 :bpm (double bpm)}
                              :registry registry
                              :rig {:fixtures [{:id :left :universe 1 :address 1 :channels {:dimmer 1}}]}})
    :sink {:send! (fn [_ _])} :workspace-options nil}))

(defn- ms [bpm beat] (Math/round (/ (* 60000.0 (double beat)) (double bpm))))

(def performer {:adapter :push :session-id "performer"})

(def ^:private echo-source {:type :ableton :adapter :live-clip :origin :live-playback})

(defn- harness
  "A show at bpm with a capture buffer folded after every owner cycle and an
  optional feed callback for each newly captured event."
  [bpm & {:keys [feed buffer-options]}]
  (let [rt (runtime bpm)
        state (atom {:buffer (capture/new-buffer (merge {:bars 4} buffer-options)) :fed 0})
        observe! (fn []
                   (let [b (capture/observe-model (:buffer @state) (:model @(:state rt)))]
                     (when feed
                       (doseq [e (capture/events-after b (:fed @state))] (feed e)))
                     (swap! state assoc :buffer b :fed (:serial b))))
        tick! (fn [beat] (show/tick! rt (ms bpm beat)) (observe!))
        submit! (fn [beat payload]
                  (let [reply (show/submit! rt :command payload)]
                    (tick! beat)
                    (deref reply 1000 ::missing)))]
    {:rt rt :state state :tick! tick! :submit! submit!}))

(defn- perform!
  "Two bars of cue launches, releases and parameter moves, plus a Live
  playback echo that must not be captured. Returns the harness."
  [{:keys [submit! tick!] :as h}]
  (let [launch (fn [beat cue params]
                 (:instance-id (submit! beat {:op :cue/launch :cue-id cue :source performer
                                              :launch (cond-> {:mode :persistent} params (assoc :params params))})))
        move! (fn [beat id param value] (submit! beat {:op :param/set :instance-id id :param param :value value :source performer}))
        end! (fn [beat id] (submit! beat {:op :instance/end :instance-id id :source performer}))]
    (tick! 0)
    (let [wash (launch 0 :wash {:level 0.5 :mode :soft})]
      (move! 3/2 wash :level 0.75)
      (let [strobe (launch 2 :strobe {:rate 4})]
        (move! 13/4 strobe :rate 12)
        (end! 7/2 strobe))
      (let [aim (launch 4 :aim {:direction [0.0 0.5 -0.5]})]
        (move! 5 aim :direction [1.0 -1.0 0.25])
        (move! 6 wash :mode :pulse)
        (end! 31/4 wash)
        (let [wash-2 (launch 8 :wash nil)]
          (move! 9 wash-2 :level 1.0)
          (let [echo (:instance-id (submit! 21/2 {:op :cue/launch :cue-id :strobe :source echo-source
                                                  :launch {:mode :persistent}}))]
            (submit! 21/2 {:op :param/set :instance-id echo :param :rate :value 20 :source performer})
            (submit! 11 {:op :instance/end :instance-id echo :source echo-source}))
          (end! 12 aim)
          (tick! 63/4))))
    h))

(def expected-capture
  {:length-beats 16
   :notes [{:id "n0" :cue-id :wash :start 0 :length 31/4 :params {:level 0.5 :mode :soft}}
           {:id "n1" :cue-id :strobe :start 2 :length 3/2 :params {:rate 4}}
           {:id "n2" :cue-id :aim :start 4 :length 8 :params {:direction [0.0 0.5 -0.5]}}
           {:id "n3" :cue-id :wash :start 8 :length 8 :params {}}]
   :lanes [{:cue-id :aim :param :direction :points [{:beat 5 :value [1.0 -1.0 0.25] :note "n2"}]}
           {:cue-id :strobe :param :rate :points [{:beat 13/4 :value 12 :note "n1"}]}
           {:cue-id :wash :param :level :points [{:beat 3/2 :value 0.75 :note "n0"} {:beat 9 :value 1.0 :note "n3"}]}
           {:cue-id :wash :param :mode :points [{:beat 6 :value :pulse :note "n0"}]}]})

(defn- close-to? [a b]
  (cond (and (number? a) (number? b)) (<= (Math/abs (- (double a) (double b))) (/ 1.0 16383))
        (and (sequential? a) (sequential? b)) (and (= (count a) (count b)) (every? true? (map close-to? a b)))
        (and (map? a) (map? b)) (and (= (set (keys a)) (set (keys b))) (every? #(close-to? (a %) (b %)) (keys a)))
        :else (= a b)))

(defn- replay!
  "Fire every captured clip in the simulated Set at song beat 0 and feed the
  MIDI Live plays for [0 16) beats through the playback decoder into a fresh
  show at bpm. Returns what a playback-observing buffer captured, the default
  buffer (which must ignore playback) and the decoded commands."
  [live conn clips layout bpm]
  (let [{:keys [rt state tick!]} (harness bpm :buffer-options {:ignore-origins #{}})
        default-buffer (atom (capture/new-buffer {:bars 4}))
        n (atom 0)
        envelope (fn [_] {:command-id (str "playback-" (swap! n inc))})
        player (atom (clip/player layout {:bridge-id "fake"}))
        decoded (atom [])
        submit-all! (fn [beat commands]
                      (let [replies (mapv #(vector % (show/submit! rt :command %)) commands)]
                        (tick! beat)
                        (swap! default-buffer capture/observe-model (:model @(:state rt)))
                        (swap! decoded into commands)
                        (mapv (fn [[c r]] [c (deref r 1000 ::missing)]) replies)))]
    (try
      (tick! 0)
      (fake/set-song-beat! live 0)
      (doseq [c clips] (is (= :executed (:status (live/request! conn :fire-clip {:clip-id (:clip-id c)})))))
      (doseq [[beat events] (sort-by key (group-by :song-beat (fake/playback live 0 16)))]
        (let [{p :player commands :commands} (clip/play @player events envelope)]
          (reset! player p)
          (doseq [[command ack] (submit-all! beat commands)
                  :when (= :cue/launch (:op command))]
            (is (= :accepted (:status ack)) (pr-str ack))
            (let [{p :player deferred :commands} (clip/acknowledge @player ack envelope)]
              (reset! player p)
              (when (seq deferred) (submit-all! beat deferred))))))
      (tick! 63/4)
      {:replayed (capture/capture (:buffer @state) {:end-beat 16})
       :ignored (capture/capture @default-buffer {:end-beat 16})
       :commands @decoded}
      (finally (show/stop! rt)))))

(defn- edit
  "What the performer does in Live's clip editor: move the strobe note half a
  beat later together with its launch value, and pull the first wash level
  move down to a quarter."
  [clips layout]
  (let [cue-ch (:channel layout) lane-ch (clip/lane-channel layout)
        strobe-pitch (get-in layout [:cues :strobe :pitch])
        rate-cc (get-in (first (filter #(= :rate (:param %)) (:lanes layout))) [:components 0 :cc])
        level-cc (get-in (first (filter #(= [:wash :level] ((juxt :cue-id :param) %)) (:lanes layout))) [:components 0 :cc])
        quarter (first (clip/encode-value (first (filter #(= :level (:param %)) (:lanes layout))) 0.25))]
    [[cue-ch (fn [c] (update c :notes (fn [ns] (mapv #(if (and (= strobe-pitch (:pitch %)) (== 2 (:start %)))
                                                         (assoc % :start 5/2 :duration 1) %) ns))))]
     [lane-ch (fn [c] (update c :controllers
                              (fn [cs] (mapv (fn [{:keys [beat controller] :as e}]
                                               (cond
                                                 (and (== 2 beat) (#{rate-cc (+ 32 rate-cc)} controller)) (assoc e :beat 5/2)
                                                 (and (== 3/2 beat) (= level-cc controller)) (assoc e :value (bit-shift-right quarter 7))
                                                 (and (== 3/2 beat) (= (+ 32 level-cc) controller)) (assoc e :value (bit-and quarter 127))
                                                 :else e))
                                             cs))))]]))

(defn- round-trip [bpm replay-bpm]
  (let [live (fake/create {:tracks [{:name "Bass" :arm? true :input {:port "Keyboard" :channel 1}}]})
        conn (live/open (fake/transport live {:json? true}) {:session-id (str "capture-" bpm)})
        h (perform! (harness bpm))
        phrase (try (capture/capture (:buffer @(:state h)) {:end-beat 16})
                    (finally (show/stop! (:rt h))))
        layout (clip/allocate-layout {} registry (assoc (clip/phrase-targets phrase) :channel 1 :lane-channel 2))
        setup (live/setup! conn (live/desired-setup layout {:port "Tolglow"}))
        captured (live/capture! conn setup {:phrase phrase :layout layout})
        decode #(live/import-clips! conn (:clips captured) layout {:meter (:meter phrase)})
        before-edit (decode)
        _ (doseq [[ch f] (edit nil layout)]
            (fake/edit-clip! live (:clip-id (first (filter #(= ch (:channel %)) (:clips captured)))) f))
        edited (decode)
        replay (replay! live conn (:clips captured) layout replay-bpm)]
    {:phrase phrase :layout layout :setup setup :captured captured :before-edit before-edit
     :edited edited :replay replay :live live :conn conn}))

(deftest perform-capture-edit-and-replay-at-70-120-and-180-bpm
  (doseq [[bpm replay-bpm] [[70 120] [120 180] [180 70] [120 120]]]
    (testing (str "captured at " bpm " BPM, replayed at " replay-bpm " BPM")
      (let [{:keys [phrase layout setup captured before-edit edited replay live conn]} (round-trip bpm replay-bpm)]
        (testing "the capture holds the performed bars on the beat grid, without the Live playback echo"
          (is (= expected-capture (clip/comparable phrase)))
          (is (= {:start-beat 0 :end-beat 16} (select-keys (:source phrase) [:start-beat :end-beat])))
          (is (= [false true] (mapv :held-at-end? [(first (:notes phrase)) (last (:notes phrase))]))))
        (testing "setup created and armed two capture tracks and left the musician's armed track alone"
          (is (:ready? setup) (pr-str (:problems setup)))
          (is (= ["Bass" "Tolglow Cues" "Tolglow Lanes"] (mapv :name (get-in setup [:set :tracks]))))
          (is (= [true true true] (mapv :arm? (get-in setup [:set :tracks]))))
          (is (= {:port "Keyboard" :channel 1} (:input (first (get-in setup [:set :tracks]))))))
        (testing "one clip per capture track through the single buffer route, focused for editing"
          (is (= :tolglow-buffer (:route captured)))
          (is (:complete? captured))
          (is (= [1 2] (mapv :channel (:clips captured))))
          (is (= :executed (:shown captured)))
          (is (= (:clip-id (first (:clips captured))) (:selected @live)))
          (is (not-any? #{:capture-midi} (live/operations-sent conn))))
        (testing "reading the unedited clip back gives the capture within 14-bit resolution"
          (is (close-to? (clip/comparable phrase) (clip/comparable before-edit)))
          (is (= (clip/phrase->clips before-edit layout) (clip/phrase->clips phrase layout))
              "decoding is a fixed point of encoding"))
        (testing "the import reflects exactly the edits made in Live"
          (let [level (clip/decode-value (first (filter #(= :level (:param %)) (:lanes layout)))
                                         [(first (clip/encode-value (first (filter #(= :level (:param %)) (:lanes layout))) 0.25))])]
            (is (= (-> (clip/comparable before-edit)
                       (assoc-in [:notes 1 :start] 5/2) (assoc-in [:notes 1 :length] 1)
                       (assoc-in [:lanes 2 :points 0 :value] level))
                   (clip/comparable edited)))))
        (testing "Live's playback replays the edited clip exactly"
          (is (= (clip/comparable edited) (clip/comparable (:replayed replay))))
          (is (empty? (:notes (:ignored replay))) "the default capture buffer does not re-record playback")
          (is (every? #(= :live-playback (get-in % [:source :origin])) (:commands replay))))))))

(deftest live-capture-midi-route-captures-the-same-phrase-from-the-continuous-feed
  (doseq [bpm [70 120 180]]
    (testing (str bpm " BPM")
      (let [live (fake/create {:tracks [{:name "Drums" :arm? true :input {:port "Keyboard" :channel 1}}]})
            conn (live/open (fake/transport live {:json? true}) {:session-id "feed"})
            layout (clip/allocate-layout {} registry {:cues [:wash :strobe :aim] :channel 1 :lane-channel 2
                                                      :params [[:wash :level] [:wash :mode] [:strobe :rate] [:aim :direction]]})
            port (fake/midi-port live "Tolglow")
            desired (live/desired-setup layout {:port "Tolglow" :route :live-capture-midi})
            setup (live/setup! conn desired)
            grid 1/4
            feed (fn [{:keys [beat] :as event}]
                   ;; Live stamps MIDI on arrival; this feed has no transport jitter.
                   (fake/set-song-beat! live (* grid (Math/round (/ (double beat) grid))))
                   ((:send! port) nil (live/feed-routing desired (clip/feed-messages layout event))))
            h (perform! (harness bpm :feed feed))
            phrase (try (capture/capture (:buffer @(:state h)) {:end-beat 16})
                        (finally (show/stop! (:rt h))))
            _ (fake/set-song-beat! live 16)
            captured (live/capture! conn setup {:phrase phrase :layout layout})
            imported (live/import-clips! conn (:clips captured) layout {:meter (:meter phrase)})]
        (is (:ready? setup) (pr-str (:problems setup)))
        (is (= :live-capture-midi (:route captured)))
        (is (= [1 2] (sort (map :channel (:clips captured)))))
        (is (not-any? #{:write-clip} (live/operations-sent conn)) "never both routes")
        (is (empty? (get-in @live [:tracks 0 :history])) "the musician's own track received none of the feed")
        (is (close-to? (clip/comparable phrase) (clip/comparable imported)))))))
