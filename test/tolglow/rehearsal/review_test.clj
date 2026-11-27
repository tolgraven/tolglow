(ns tolglow.rehearsal.review-test
  "The rehearsal review's detectors on hand-built frames, so a clean real
  take (zero overwritten gestures, no repeats) is not the only evidence that
  they can fire."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.rehearsal.review :as review]))

(def passage
  {:id :concert :mode :concert :seed 1 :take :a :take-seed 9 :bpm 120.0
   :sections [{:id :verse-1 :kind :verse :bars 4 :start-bar 0 :anchor :verse-wash}
              {:id :chorus-1 :kind :chorus :bars 4 :start-bar 4 :anchor :chorus-wash}]
   :stops []})

(defn- frame [beat instances & {:as more}]
  (merge {:t (* beat 500.0) :beat beat :playing? true :music (if (< beat 16) :verse-1 :chorus-1)
          :instances instances :lit #{:a :b} :brightness 0.5 :strobe? false :auto-tick 0}
         more))

(def anchor {:cue-id :chorus-wash :adapter :agent :phase :running :started-at-beat 16.0 :params {:level 0.6}})

(defn- take-with [frames gestures]
  {:passage passage :frame-ms 125.0 :frames frames :agent [] :gestures gestures :transport []
   :schedule {:recent []} :status {}})

(deftest an-overwritten-gesture-is-reported-with-its-beat-and-value
  (let [frames (concat [(frame 0.0 {"0:1" (assoc anchor :cue-id :verse-wash :started-at-beat 0.0)})]
                       (for [b (range 16 24)]
                         (frame (double b) {"0:2" (assoc anchor :params {:level (cond (< b 18) 0.6 (< b 21) 0.3 :else 0.8)})})))
        gesture {:label "Grab" :beat 17.5 :op :param/set :param :level :value 0.3 :target "0:2"
                 :status :accepted :accepted-beat 18.0}
        result (review/concert (take-with frames [gesture]) (take-with frames []))
        [a] (:takes result)]
    (is (= [{:label "Grab" :target "0:2" :param :level :value 0.3 :at-beat 21.0 :became 0.8}] (:overwritten a)))
    (is (some #(re-find #"Grab was overwritten at beat 21" %) (:flags result)))
    (testing "the performer writing again ends the first gesture's hold"
      (let [again (assoc gesture :label "Again" :beat 20.5 :value 0.8 :accepted-beat 21.0)
            [a] (:takes (review/concert (take-with frames [gesture again]) (take-with frames [])))]
        (is (empty? (:overwritten a)))))))

(deftest anchors-missing-or-late-are-flagged-and-compared
  (let [on-time [(frame 0.0 {"0:1" (assoc anchor :cue-id :verse-wash :started-at-beat 0.0)})
                 (frame 16.0 {"0:2" anchor})]
        late [(frame 0.0 {"0:1" (assoc anchor :cue-id :verse-wash :started-at-beat 0.0)})
              (frame 16.5 {"0:2" (assoc anchor :started-at-beat 16.5)})]
        missing [(frame 0.0 {}) (frame 16.0 {"0:2" anchor})]
        result (review/concert (take-with on-time []) (take-with late []))
        missed (review/concert (take-with missing []) (take-with on-time []))]
    (is (= [true false] (mapv :kept? (get-in result [:comparison :anchors]))))
    (is (= 0.5 (get-in result [:takes 1 :anchors 1 :late-beats])))
    (is (some #(re-find #"chorus-wash anchor 0.5 beats late" %) (:flags result)))
    (is (true? (get-in missed [:takes 0 :anchors 0 :missed?])))
    (is (some #(re-find #"Take a missed the verse-wash anchor at bar 0" %) (:flags missed)))))

(def club-passage
  {:id :club :mode :club :seed 1 :bpm 120.0 :take nil :agent-lag-beats 0
   :sections [{:id :groove :kind :groove :bars 16 :start-bar 0}
              {:id :quiet :kind :quiet :bars 4 :start-bar 16}]
   :stops []})

(defn- club-frame [beat cues & {:keys [session] :or {session "groove"} :as more}]
  (merge {:t (* beat 500.0) :beat beat :playing? true :music (if (< beat 64) :groove :quiet)
          :instances (into {} (map-indexed (fn [i cue] [(str beat ":" i) {:cue-id cue :adapter :auto :phase :running :session session
                                                                          :started-at-beat beat :params {:color [1.0 0.0 0.0]}}])
                                           cues))
          :lit #{:a :b :c} :brightness 0.4 :strobe? false :auto-tick (long beat) :auto-session session}
         (dissoc more :session)))

(deftest repeated-phrases-a-held-stop-and-a-bright-quiet-section-are-measured
  (let [frames (concat (for [b (range 0 64)] (club-frame (double b) [:wash]))
                       ;; A stop at beat 64 held for three frames, then one decision per beat.
                       (for [_ (range 3)] (club-frame 64.0 [] :playing? false :auto-tick 63))
                       (for [b (range 64 80)] (club-frame (double b) [:spot] :strobe? true :brightness 0.6 :lit #{:a})))
        review (review/club {:passage club-passage :frame-ms 500.0 :frames (vec frames) :transport [{:op :stop :at-beat 64.0}]
                             :agent [] :gestures [] :schedule {:recent []} :status {}})]
    (is (= [1 2 3] (:repeat-phrases (:repetition review))))
    (is (= 3 (get-in review [:repetition :longest-repeat-run :length])))
    (is (= [{:frames-stopped 3 :beat-held? true :decisions-while-stopped 0 :launches-while-stopped 0}]
           (mapv #(select-keys % [:frames-stopped :beat-held? :decisions-while-stopped :launches-while-stopped]) (:stops review))))
    (is (some #(re-find #"repeat the same look" %) (:flags review)))
    (is (some #(re-find #"Strobe ran in the quiet section" %) (:flags review)))
    (is (some #(re-find #"quiet section is not darker" %) (:flags review)))
    (is (re-find #"Operator note" (:report review)))))

(deftest silent-phrases-are-not-repeats
  (let [frames (concat (for [b (range 0 16)] (club-frame (double b) [:wash]))
                       (for [b (range 16 64)] (club-frame (double b) [] :lit #{} :brightness 0.0))
                       (for [b (range 64 80)] (club-frame (double b) [] :lit #{} :brightness 0.0)))
        review (review/club {:passage club-passage :frame-ms 500.0 :frames (vec frames) :transport []
                             :agent [] :gestures [] :schedule {:recent []} :status {}})]
    (is (= [nil 1.0 nil nil nil] (mapv :novelty (:phrases review))))
    (is (empty? (:repeat-phrases (:repetition review))))
    (is (not-any? #(re-find #"repeat the same look" %) (:flags review)))))

(deftest an-interval-that-is-not-a-whole-number-of-steps-is-on-its-grid
  ;; Review of #766: {:beats 3/2} on a beat grid falls due at 0, 1.5, 3, ...
  ;; which the grid rule allows; it is not an off-grid decision.
  (let [frames (for [b (range 0 64 3/2)]
                 (club-frame (double b) [:wash] :auto-interval {:started-at (double b) :value 1.5 :step 1}))
        review (review/club {:passage club-passage :frame-ms 500.0 :frames (vec frames) :transport []
                             :agent [] :gestures [] :schedule {:recent []} :status {}})]
    (is (= (get-in review [:launch-timing :measured]) (get-in review [:launch-timing :due-on-own-grid])))
    (is (not-any? #(re-find #"off their interval's grid" %) (:flags review))))
  (testing "a whole-number interval off its grid is still flagged"
    (let [frames (for [b (range 0.5 64 1)]
                   (club-frame (double b) [:wash] :auto-interval {:started-at (double b) :value 1 :step 1}))
          review (review/club {:passage club-passage :frame-ms 500.0 :frames (vec frames) :transport []
                               :agent [] :gestures [] :schedule {:recent []} :status {}})]
      (is (some #(re-find #"off their interval's grid" %) (:flags review))))))

(deftest a-change-on-its-line-is-measured-where-it-applied
  ;; The agent heard of the quiet line at beat 60 and put its change :at beat
  ;; 64; it applied on the frame at 64.25, before the planner decided there.
  (let [frames (concat (for [b (range 0 64)] (club-frame (double b) [:wash]))
                       [(club-frame 64.25 [:spot] :session "quiet" :auto-section :hush :auto-section-start 64.0 :auto-section-length 8)]
                       (for [b (range 65 80)] (club-frame (double b) [:spot] :session "quiet" :auto-section :hush
                                                          :auto-section-start 64.0 :auto-section-length 8)))
        agent [{:section :quiet :section-beat 64.0 :suggested-beat 60.0 :accepted-beat 60.0 :scheduled? true
                :applied-beat 64.25 :status :completed :tools ["configure_auto"] :receipts {"configure_auto" {:status :accepted}}}]
        review (review/club {:passage club-passage :frame-ms 500.0 :frames (vec frames) :transport []
                             :agent agent :gestures [] :schedule {:recent []} :status {}})
        [r] (:responses review)]
    (is (= {:scheduled? true :applied-beat 64.25 :late-beats 0.25 :carry-over-launches 0 :early-launches 0 :first-launch-beat 64.25}
           (select-keys r [:scheduled? :applied-beat :late-beats :carry-over-launches :early-launches :first-launch-beat])))
    (is (= [{:auto-section :hush :beat 64.25 :with-agent-change? true}]
           (mapv #(select-keys % [:auto-section :beat :with-agent-change?]) (:boundaries review)))
        "the new configuration's first section is not counted as a plan boundary")
    (is (re-find #"\| hush \| bar 16 beat 0.25 \| 8 \| true" (:report review))))
  (testing "a launch is attributed to the session that made it, not to when it was seen"
    ;; As if the planner had decided before the change on the line frame: the
    ;; groove session's launch on that frame is carry-over.
    (let [frames (concat (for [b (range 0 64)] (club-frame (double b) [:wash]))
                         [(assoc (club-frame 64.25 [:wash]) :auto-session "quiet")
                          (club-frame 64.5 [:spot] :session "quiet")])
          agent [{:section :quiet :section-beat 64.0 :suggested-beat 60.0 :accepted-beat 60.0 :scheduled? true
                  :applied-beat 64.25 :status :completed :tools ["configure_auto"] :receipts {"configure_auto" {:status :accepted}}}]
          [r] (:responses (review/club {:passage club-passage :frame-ms 500.0 :frames (vec frames) :transport []
                                        :agent agent :gestures [] :schedule {:recent []} :status {}}))]
      (is (= [1 0 64.5] [(:carry-over-launches r) (:early-launches r) (:first-launch-beat r)]))))
  (testing "a launch made and ended within the change's own tick is read from receipts"
    ;; No frame shows the groove session's launch on the line frame: installing
    ;; the change ended it in that tick. The committed launch receipts do.
    (let [frames (concat (for [b (range 0 64)] (club-frame (double b) [:wash]))
                         [(club-frame 64.25 [:spot] :session "quiet")])
          receipts (concat (for [b (range 0 64)] {:id (str b ":0") :cue-id :wash :adapter :auto :session "groove"
                                                  :started-at-beat (double b) :seen-beat (double b)})
                           [{:id "late:0" :cue-id :wash :adapter :auto :session "groove" :started-at-beat 64.25 :seen-beat 64.25}
                            {:id "64.25:0" :cue-id :spot :adapter :auto :session "quiet" :started-at-beat 64.25 :seen-beat 64.25}])
          agent [{:section :quiet :section-beat 64.0 :suggested-beat 60.0 :accepted-beat 60.0 :scheduled? true
                  :applied-beat 64.25 :status :completed :tools ["configure_auto"] :receipts {"configure_auto" {:status :accepted}}}]
          take {:passage club-passage :frame-ms 500.0 :frames (vec frames) :transport [] :agent agent :gestures []
                :schedule {:recent []} :status {}}]
      (is (zero? (:carry-over-launches (first (:responses (review/club take))))) "frames alone miss it")
      (is (= 1 (:carry-over-launches (first (:responses (review/club (assoc take :auto-launches (vec receipts))))))))))
  (testing "the same frame counts as carry-over when the change applied on arrival after it"
    (let [frames (concat (for [b (range 0 64)] (club-frame (double b) [:wash]))
                         [(club-frame 64.25 [:wash]) (club-frame 64.5 [:spot] :session "quiet")])
          agent [{:section :quiet :section-beat 64.0 :suggested-beat 64.25 :accepted-beat 64.25 :scheduled? false
                  :applied-beat 64.25 :status :completed :tools ["configure_auto"] :receipts {"configure_auto" {:status :accepted}}}]
          [r] (:responses (review/club {:passage club-passage :frame-ms 500.0 :frames (vec frames) :transport []
                                        :agent agent :gestures [] :schedule {:recent []} :status {}}))]
      (is (= [1 0] [(:carry-over-launches r) (:early-launches r)])))))
