(ns tolglow.rehearsal-test
  "Rehearsals through the real show owner (#670): the recorded agent's tool
  rounds through agent/run! and show/dispatch-tool!, stems through the
  capture callbacks, Push captures as Push 3 session commands, captured
  output only. The checks read committed frames and receipts. Whether the
  passages are musical is for the operator to judge."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [tolglow.rehearsal :as rehearsal]
            [tolglow.rehearsal.agent-script :as script]
            [tolglow.rehearsal.passage :as passage]
            [tolglow.show :as show]))

(def club-take (delay (rehearsal/run-take (passage/club 670))))

(def club-review (delay (rehearsal/club 670)))

(deftest the-club-passage-replays-exactly-from-its-seed
  (is (= (rehearsal/replay-signature @club-take) (rehearsal/replay-signature (rehearsal/run-take (passage/club 670)))))
  (is (not= (map :dmx-hash (:frames @club-take)) (map :dmx-hash (:frames (rehearsal/run-take (passage/club 671)))))
      "another seed is another performance"))

(deftest an-unattended-club-passage-is-reviewed-for-phrasing-repetition-contrast-and-stops
  (let [{:keys [responses boundaries launch-timing phrases contrast stops flags report error repetition take]} @club-review
        by-kind (into {} (map (juxt :kind identity)) contrast)]
    (is (nil? error))
    (testing "the agent puts every change on its section line through the tool path (#767)"
      (is (= [:groove :breakdown :return :quiet] (mapv :section responses)))
      (is (every? #(= {"configure_auto" :accepted} (:receipts %)) responses) (pr-str (map :receipts responses)))
      (is (every? :scheduled? responses))
      (is (every? #(<= (- (:section-beat %) 4.3) (:accepted-beat %) (:section-beat %)) responses) "heard a bar ahead")
      (is (every? #(<= 0 (:late-beats %) 0.26) responses) "applied on the first frame at or past the line")
      (is (every? #(zero? (:carry-over-launches %)) responses) (pr-str (map (juxt :section :carry-over-launches) responses)))
      (is (every? #(zero? (:early-launches %)) responses))
      (is (every? #(= (:applied-beat %) (:first-launch-beat %)) responses) "the new mood decides on the line's own frame")
      (is (not-any? #(re-find #"previous mood launched|new mood launched" %) flags)))
    (testing "plan boundaries sit on the bar grid and launches start within one frame of their beat"
      (is (seq (remove :with-agent-change? boundaries)))
      (is (= [[:drive 0.0] [:drive (:applied-beat (nth responses 2))]]
             (mapv (juxt :auto-section :beat) (filter :with-agent-change? boundaries)))
          "the groove's and the return's first sections begin with the agent's change, not at a plan boundary")
      (is (every? :on-bar? (remove :with-agent-change? boundaries)) (pr-str boundaries))
      (is (pos? (:launches launch-timing)))
      (is (zero? (:beyond-one-frame launch-timing)) (pr-str launch-timing))
      (is (zero? (:before-due launch-timing)))
      (is (= (:measured launch-timing) (:due-on-own-grid launch-timing)) "bar intervals fall due on bar lines, beats on beats"))
    (testing "every phrase is measured with novelty against the one before"
      (is (= (range 12) (map :phrase phrases)))
      (is (every? number? (keep :novelty (rest phrases))))
      (is (number? (:mean-novelty repetition))))
    (testing "the quiet passage is dark, the breakdown dimmer than the groove, and neither strobes"
      (is (< (get-in by-kind [:quiet :mean-brightness]) (get-in by-kind [:breakdown :mean-brightness])
             (get-in by-kind [:groove :mean-brightness])))
      (is (< 0.9 (get-in by-kind [:quiet :dark-fraction])))
      (let [quiet (last responses)]
        (is (not-any? :strobe? (filter #(and (= :quiet (:music %)) (>= (:beat %) (:applied-beat quiet))) (:frames take)))
            "no strobe once the quiet change applied"))
      (is (zero? (get-in by-kind [:quiet :strobe-fraction])) "the change applied on the line, so not a frame of strobe in the quiet")
      (is (zero? (get-in by-kind [:breakdown :strobe-fraction]))))
    (testing "the stop in the return holds the plan and resumes without catching up"
      (is (= 1 (count stops)))
      (is (= {:beat-held? true :decisions-while-stopped 0 :launches-while-stopped 0}
             (select-keys (first stops) [:beat-held? :decisions-while-stopped :launches-while-stopped])))
      (is (<= (:decisions-in-first-resumed-beat (first stops)) 1)))
    (testing "the report says it is headless and leaves the judgement to the operator"
      (is (re-find #"Headless rehearsal" report))
      (is (re-find #"Operator note" report))
      (is (not-any? #(re-find #"off the bar grid|not darker|did not hold|off their interval" %) flags) (pr-str flags)))))

(deftest a-late-agent-is-measured-and-a-stale-one-is-named
  (let [late (rehearsal/club 670 {:passage {:agent-lag-beats 2}})
        stale (rehearsal/club 670 {:passage {:agent-lag-beats 40}})]
    (is (every? #(<= 2.0 (:late-beats %) 2.3) (:responses late)))
    (is (some #(pos? (:carry-over-launches %)) (:responses late)) "the previous mood keeps launching into the new section")
    (is (some #(re-find #"landed 2\.\d+ beats after the section line; the previous mood launched" %) (:flags late)))
    (is (some :stale? (:responses stale)))
    (is (some #(re-find #"suggestion for breakdown landed in return" %) (:flags stale))))
  (testing "a reply that takes a beat still lands on the line; one that takes five lands late and is measured"
    (let [beat (rehearsal/club 670 {:passage {:agent-latency-beats 1}})
          slow (rehearsal/club 670 {:passage {:agent-latency-beats 5}})
          late (rest (:responses slow))]
      (is (every? #(and (zero? (:carry-over-launches %)) (zero? (:early-launches %)) (<= 0 (:late-beats %) 0.26))
                  (:responses beat)))
      (is (every? :scheduled? late))
      (is (every? #(< 1.0 (:arrived-late-beats %) 1.5) late) "a bar ahead, five beats to reply: a beat and a bit late")
      (is (every? #(= (:accepted-beat %) (:applied-beat %)) late) "applied when it arrived")
      (is (some #(pos? (:carry-over-launches %)) late) "the previous mood went on deciding until then")
      (is (every? #(zero? (:early-launches %)) late))
      (is (= (count late) (count (filter #(re-find #"beats after the section line" %) (:flags slow)))))))
  (testing "an agent answering on arrival a frame early launches the new mood before the line"
    (let [early (rehearsal/club 670 {:passage {:agent-lag-beats -0.3 :agent-schedules? false}})
          lines (rest (:responses early))]
      (is (not-any? :scheduled? lines))
      (is (every? #(neg? (:late-beats %)) lines))
      (is (every? #(zero? (:carry-over-launches %)) lines))
      (is (every? #(pos? (:early-launches %)) lines))
      (is (not-any? :stale? lines))
      (is (some #(re-find #"beats before the line; the new mood launched" %) (:flags early)))))
  (testing "an agent answering on arrival at the line leaves the previous mood one decision on it"
    (let [{:keys [responses flags take]} (rehearsal/club 670 {:passage {:agent-lag-beats 0 :agent-schedules? false}})]
      (is (every? #(<= 0 (:late-beats %) 0.26) responses))
      (is (some #(pos? (:carry-over-launches %)) responses))
      (is (every? #(<= (:carry-over-launches %) 4) responses) "one decision: up to two cues and their emission companions")
      (is (= (count (filter (comp pos? :carry-over-launches) responses))
             (count (filter #(re-find #"previous mood launched" %) flags))))
      (let [quiet (last responses)]
        (is (not-any? :strobe? (filter #(and (= :quiet (:music %)) (> (:beat %) (:applied-beat quiet))) (:frames take)))
            "a carry-over strobe ends when the change applies")))))

(def concert-review (delay (rehearsal/concert 670)))

(deftest two-concert-takes-keep-their-anchors-and-vary-fills-stems-and-gestures
  (let [{:keys [takes comparison flags report]} @concert-review
        [a b] takes
        sections (into {} (map (juxt :section identity)) (:sections comparison))]
    (is (every? (comp nil? :error) takes))
    (testing "every change lands on its line: no flags, no carry-over, no early launches (#767)"
      (is (empty? flags) (pr-str flags))
      (is (every? :scheduled? (mapcat :agent takes)))
      (is (every? #(zero? (:carry-over-launches %)) (mapcat :agent takes)))
      (is (every? #(zero? (:early-launches %)) (mapcat :agent takes)))
      (is (every? #(<= 0 (:late-beats %) 0.26) (mapcat :agent takes))))
    (testing "the agent posts every anchor once and both takes start them on their bar"
      (is (every? #(= {"schedule_events" :accepted "configure_auto" :accepted} (:receipts %))
                  (map (comp first :agent) takes)))
      (is (= [0.0 32.0 64.0 96.0] (mapv :a (:anchors comparison)) (mapv :b (:anchors comparison))))
      (is (every? :kept? (:anchors comparison))))
    (testing "fills, stem hits and parameters vary between takes"
      (is (pos? (get-in sections [:chorus-1 :fill-distance])))
      (is (pos? (get-in sections [:chorus-2 :fill-distance])))
      (is (pos? (get-in sections [:verse-1 :shared-fills-with-other-params])) "same cues, other seeded values")
      (is (some #(pos? (:stem-distance %)) (:sections comparison)))
      (is (every? #(< (:stem-distance %) 0.5) (:sections comparison)) "the kicks and backbeat are the same song"))
    (testing "every Push gesture is accepted as a Push 3 session command"
      (is (every? #(= :accepted (:status %)) (mapcat :gestures takes)) (pr-str (mapcat :gestures takes)))
      (is (= [:spot] (mapv :cue-id (:push-launches a))))
      (is (= [:strobe-hit] (mapv :cue-id (:push-launches b)))))
    (testing "the performer's grab cancels the agent's build ramp and is not overwritten"
      (is (= {:event-id "build-chorus-1" :status :cancelled :reason :human-override :by :push-3}
             (select-keys (first (:ramps b)) [:event-id :status :reason :by])))
      (is (= :completed (:status (first (:ramps a)))))
      (is (every? (comp empty? :overwritten) takes)))
    (testing "a taken auto sweep stays with the performer until they release it"
      (doseq [[raw review] (map vector (:raw @concert-review) takes)]
        (let [[take release] (filter #(= :sweep (:cue-id %)) (:gestures review))
              id (:target take)
              frames (:frames raw)
              held (filter #(< (:accepted-beat take) (:beat %) (:accepted-beat release)) frames)]
          (is (= id (:target release)))
          (is (every? #(= :running (get-in % [:instances id :phase])) held))
          (is (every? #(= (:value take) (get-in % [:instances id :params :span])) held))
          (is (not= :running (get-in (some #(when (> (:beat %) (+ 0.5 (:accepted-beat release))) %) frames)
                                     [:instances id :phase]))
              "the performer's release ends it")
          (is (true? (boolean (some (fn [[other i]] (and (not= id other) (= :sweep (:cue-id i)) (= :auto (:adapter i))
                                                         (< (:accepted-beat take) (:started-at-beat i) 96.0)))
                                    (mapcat :instances frames))))
              "auto goes on launching its own sweeps in the verse"))))
    (is (re-find #"Section anchors" report))))

(deftest a-concert-take-replays-exactly
  (let [p (passage/concert 670 :b)]
    (is (= (rehearsal/replay-signature (rehearsal/run-take p)) (rehearsal/replay-signature (rehearsal/run-take p))))))

(deftest the-review-is-written-as-data-and-text
  (let [directory (doto (io/file (System/getProperty "java.io.tmpdir") (str "rehearsal-" (System/nanoTime))) (.mkdirs))
        paths (rehearsal/write! (str directory) @club-review @concert-review)]
    (try
      (is (= #{"club.edn" "club.md" "concert.edn" "concert.md"} (set (map #(.getName (io/file %)) paths))))
      (let [club (edn/read-string (slurp (io/file directory "club.edn")))
            concert (edn/read-string (slurp (io/file directory "concert.edn")))]
        (is (= 12 (count (:phrases club))))
        (is (< 180 (count (:timeline club))))
        (is (not (contains? club :take)) "per-frame records stay out of the artefact")
        (is (= 2 (count (:timelines concert)))))
      (is (re-find #"# Concert rehearsal" (slurp (io/file directory "concert.md"))))
      (finally (doseq [f (reverse (file-seq directory))] (.delete ^java.io.File f))))))

(deftest a-room-rehearsal-plays-in-real-time-on-a-running-show
  ;; Two bars of verse and two of chorus at 180 BPM: about 5.3 s of real time.
  (let [p (-> (passage/concert 670 :b {:bpm 180.0 :sections [{:id :verse-1 :kind :verse :bars 2 :anchor :verse-wash}
                                                              {:id :chorus-1 :kind :chorus :bars 2 :anchor :chorus-wash}]})
              (assoc :gestures [{:beat 9.0 :label "Spotlight" :command {:op :cue/launch :cue-id :spot :launch {:mode :persistent}}}]))
        launched (atom [])
        rt (show/start! {:model (rehearsal/room-model p) :sink {:send! (fn [_ _])} :workspace-options nil :recovery false})]
    (try
      (show/attach-observer! rt :rehearsal-launches
        (fn [] {:events! (fn [_ events _ _]
                          (swap! launched into (keep #(when (= :instance/launched (:type %)) (get-in % [:data :cue-id]))) events))
                :frame! (fn [_]) :tracing? (constantly false) :close! (fn [])}))
      (let [result (rehearsal/play! rt p)]
        (is (false? (:timed-out? result)))
        (is (= {"schedule_events" :accepted "configure_auto" :accepted}
               (update-vals (:receipts (first (:agent result))) :status))
            "the anchors are posted before the transport starts")
        (is (= [:verse-1 :chorus-1] (mapv :section (:agent result))))
        (is (= [:accepted] (mapv :status (:gestures result))))
        (let [counts (frequencies @launched)]
          (is (= 1 (counts :verse-wash)))
          (is (= 1 (counts :chorus-wash)))
          (is (<= 6 (counts :kick-hit 0)) (pr-str counts))
          (is (= 1 (counts :spot)))))
      (finally (show/stop! rt)))))

(deftest the-recorded-agent-never-answers-an-extra-round
  (let [transport (rehearsal/recorded-transport ["data: [DONE]\n\n"])
        chunks (atom [])]
    (is (= {:status 200} (transport {} #(swap! chunks conj %) {})))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no further provider round" (transport {} identity {}))))
  (testing "one taste plays both modes"
    (is (= (:groove script/moods) (dissoc (script/auto-configuration (passage/club 1) {:id :g :kind :groove} 0) :enabled? :seed)))
    (is (every? #(contains? script/moods (:kind %)) (concat (:sections (passage/club 1)) (:sections (passage/concert 1 :a)))))))
