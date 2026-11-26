(ns tolglow.integrations.phrase-clip-test
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.integrations.phrase-clip :as clip])
  (:import [javax.sound.midi MidiSystem ShortMessage]
           [java.io ByteArrayInputStream]))

(def registry
  {:effects {:wash {:id :wash :version 1
                    :params {:level {:type :number :bounds [0.0 1.0]}
                             :mode {:type :enum :values [:soft :hard :pulse]}
                             :tint {:type :color}
                             :span {:type :range :bounds [0 10]}
                             :fill {:type :gradient}
                             :hold? {:type :boolean}
                             :steps {:type :integer :recommended [1 64]}}}}
   :cues {:wash {:id :wash :effect-id :wash :version 1}
          :wash-b {:id :wash-b :effect-id :wash :version 1}}})

(defn- lane [layout param] (first (filter #(= param (:param %)) (:lanes layout))))

(deftest allocation-is-stable-and-lists-what-cannot-be-encoded
  (let [layout (clip/allocate-layout {} registry {:cues [:wash] :params [[:wash :level] [:wash :tint] [:wash :fill]]
                                                  :pad-notes {:wash 92}})]
    (is (= {:wash {:pitch 92}} (:cues layout)) "a cue keeps the Drum Rack pad it already plays")
    (is (= [1] (mapv :cc (:components (lane layout :level)))))
    (is (= [2 3 4] (mapv :cc (:components (lane layout :tint)))) "colour is three paired controllers")
    (is (= [{:cue-id :wash :param :fill :reason :no-reversible-encoding}] (:unassigned layout)))
    (let [grown (clip/allocate-layout layout registry {:cues [:wash-b :wash] :params [[:wash :span] [:wash :level]]})]
      (is (= {:wash {:pitch 92} :wash-b {:pitch 36}} (:cues grown)))
      (is (= (:lanes layout) (take 2 (:lanes grown))) "earlier lanes keep their controllers")
      (is (= [5 7] (mapv :cc (:components (lane grown :span)))) "controller 6 (data entry) is skipped"))
    (testing "running out of controllers is reported, not silently dropped"
      (let [full (reduce (fn [l n] (clip/allocate-layout l (assoc-in registry [:effects :wash :params (keyword (str "p" n))]
                                                                     {:type :number :bounds [0 1]})
                                                         {:params [[:wash (keyword (str "p" n))]]}))
                         {} (range 31))]
        (is (= 30 (count (:lanes full))))
        (is (= :no-free-controller (:reason (last (:unassigned full)))))))))

(deftest values-round-trip-through-14-bit-controllers
  (let [layout (clip/allocate-layout {} registry {:params [[:wash :level] [:wash :mode] [:wash :tint] [:wash :span]
                                                           [:wash :hold?] [:wash :steps]]})]
    (doseq [[param value] [[:level 0.0] [:level 1.0] [:mode :hard] [:mode :pulse] [:tint [1.0 0.5 0.0]]
                           [:span [2 8]] [:hold? true] [:hold? false] [:steps 17]]]
      (let [l (lane layout param)
            once (clip/decode-value l (clip/encode-value l value))]
        (is (= once (clip/decode-value l (clip/encode-value l once))) (str param " is a fixed point after one encode"))
        (if (number? value)
          (is (== value once))
          (is (if (vector? value) (every? true? (map #(< (Math/abs (- (double %1) (double %2))) 1e-3) value once)) (= value once))))))
    (is (nil? (clip/encode-value (lane layout :mode) :missing)))
    (is (nil? (clip/encode-value (lane layout :tint) [1.0 0.5])))))

(def phrase
  {:phrase/version 1 :length-beats 8 :meter {:beats-per-bar 4 :bars-per-phrase 4} :grid-beats 1/4
   :notes [{:id "n0" :cue-id :wash :start 0 :length 3/2 :params {:level 0.5 :mode :soft} :carried-in? false :held-at-end? false}
           {:id "n1" :cue-id :wash-b :start 2 :length 6 :params {} :carried-in? false :held-at-end? true}]
   :lanes [{:cue-id :wash :param :level :points [{:beat 1 :value 0.25 :note "n0"}]}
           {:cue-id :wash-b :param :tint :points [{:beat 3 :value [0.0 1.0 0.5] :note "n1"}]}]})

(defn- layout-for [p] (clip/allocate-layout {} registry (clip/phrase-targets p)))

(deftest a-phrase-becomes-a-clip-and-back
  (let [layout (layout-for phrase)
        clips (clip/phrase->clips phrase layout)
        back (clip/clips->phrase clips layout {})]
    (is (= [1] (keys clips)))
    (is (= [{:pitch 36 :start 0 :duration 3/2 :velocity 127} {:pitch 37 :start 2 :duration 6 :velocity 127}]
           (get-in clips [1 :notes])))
    (is (= [[0 1] [0 33] [0 2] [0 34] [1 1] [1 33]] (take 6 (map (juxt :beat :controller) (get-in clips [1 :controllers])))))
    (is (= (mapv #(select-keys % [:id :cue-id :start :length]) (:notes phrase))
           (mapv #(select-keys % [:id :cue-id :start :length]) (:notes back))))
    (is (= :soft (get-in back [:notes 0 :params :mode])))
    (is (= (clip/phrase->clips back layout) clips) "an imported clip renders identically")
    (testing "a value with no running activation is an orphan, not a guess"
      (let [moved (update-in clips [1 :notes] #(vec (rest %)))
            imported (clip/clips->phrase moved layout {})]
        (is (= [:wash-b] (mapv :cue-id (:notes imported))))
        (is (= #{:level :mode} (set (map :param (:orphans (meta imported))))))))))

(deftest standard-midi-files-carry-the-clip
  (let [layout (layout-for phrase)
        clips (clip/phrase->clips phrase layout)
        bytes (clip/clip->smf (get clips 1) 1 {:bpm 128})
        sequence (MidiSystem/getSequence (ByteArrayInputStream. bytes))]
    (is (= 960 (.getResolution sequence)))
    (is (= (get clips 1) (clip/smf->clip bytes {:channel 1 :length-beats 8})))
    (is (= 8 (:length-beats (clip/smf->clip bytes {:channel 1}))) "the end of track marks the clip length")
    (testing "within one tick, releases precede controllers precede note-ons"
      (let [retrigger {:clip/version 1 :length-beats 4 :meter {:beats-per-bar 4 :bars-per-phrase 4}
                       :notes [{:pitch 36 :start 0 :duration 1 :velocity 127} {:pitch 36 :start 1 :duration 1 :velocity 127}]
                       :controllers [{:beat 1 :controller 1 :value 9}]}
            track (first (.getTracks (MidiSystem/getSequence (ByteArrayInputStream. (clip/clip->smf retrigger 1)))))
            at-1 (for [i (range (.size track)) :let [e (.get track i) m (.getMessage e)]
                       :when (and (= 960 (.getTick e)) (instance? ShortMessage m))]
                   (.getCommand ^ShortMessage m))]
        (is (= [ShortMessage/NOTE_OFF ShortMessage/CONTROL_CHANGE ShortMessage/NOTE_ON] at-1))
        (is (= retrigger (clip/smf->clip (clip/clip->smf retrigger 1) {:channel 1})))))
    (testing "seven-eight writes a 7/8 time signature and reads back 7/2 beats per bar"
      (let [odd {:clip/version 1 :length-beats 7 :meter {:beats-per-bar 7/2 :bars-per-phrase 4}
                 :notes [{:pitch 40 :start 1/2 :duration 3 :velocity 127}] :controllers []}
            read (clip/smf->clip (clip/clip->smf odd 1) {:channel 1})]
        (is (= 7/2 (get-in read [:meter :beats-per-bar])))
        (is (= 7 (:length-beats read)))
        (is (= (:notes odd) (:notes read)))))))

(deftest a-separate-lane-channel-splits-the-clip
  (let [layout (clip/allocate-layout {} registry (assoc (clip/phrase-targets phrase) :channel 1 :lane-channel 2))
        clips (clip/phrase->clips phrase layout)]
    (is (= [1 2] (sort (keys clips))))
    (is (empty? (get-in clips [1 :controllers])))
    (is (empty? (get-in clips [2 :notes])))
    (is (= (clip/comparable (clip/clips->phrase (clip/phrase->clips phrase (layout-for phrase)) (layout-for phrase) {}))
           (clip/comparable (clip/clips->phrase clips layout {}))))))

(defn- envelope [prefix] (fn [n] {:command-id (str prefix "/" n)}))

(deftest the-player-binds-launch-receipts-and-defers-what-arrives-first
  (let [layout (layout-for phrase)
        level (lane layout :level)
        [coarse fine] (let [v (first (clip/encode-value level 0.75))] [(bit-shift-right v 7) (bit-and v 127)])
        p0 (clip/player layout {:bridge-id "b"})
        {p1 :player launch :commands} (clip/play p0 [{:type :note-on :note 36 :velocity 127 :channel 1}
                                                     {:type :cc :controller 1 :value coarse :channel 1}
                                                     {:type :cc :controller 33 :value fine :channel 1}
                                                     {:type :note-on :note 99 :velocity 127 :channel 1}
                                                     {:type :note-on :note 37 :velocity 127 :channel 9}]
                                                 (envelope "t0"))]
    (is (= [{:command-id "t0/0" :op :cue/launch :cue-id :wash
             :launch {:mode :persistent :params {:level (clip/decode-value level [(+ (* 128 coarse) fine)])}}
             :source {:type :ableton :adapter :live-clip :bridge-id "b" :origin :live-playback}}]
           launch)
        "controllers at the note's beat become launch values; unmapped pitches and other channels are ignored")
    (let [{p2 :player values :commands} (clip/play p1 [{:type :cc :controller 1 :value 0 :channel 1}
                                                       {:type :cc :controller 33 :value 0 :channel 1}]
                                                   (envelope "t1"))
          {p2 :player release :commands} (clip/play p2 [{:type :note-off :note 36 :velocity 0 :channel 1}] (envelope "t2"))
          pending (concat values release)
          {p3 :player deferred :commands} (clip/acknowledge p2 {:command-id "t0/0" :status :accepted :instance-id "0:7"}
                                                            (envelope "ack"))]
      (is (empty? pending) "nothing addresses an unacknowledged launch")
      (is (= [[:param/set "0:7" :level 0.0] [:instance/end "0:7" nil nil]]
             (mapv (juxt :op :instance-id :param :value) deferred)))
      (is (empty? (:runs p3))))
    (testing "a bound run receives values directly; coarse and fine collapse to one write"
      (let [{p :player} (clip/play p0 [{:type :note-on :note 36 :velocity 127 :channel 1}] (envelope "a"))
            {p :player} (clip/acknowledge p {:command-id "a/0" :status :accepted :instance-id "0:1"} (envelope "x"))
            {p :player sets :commands} (clip/play p [{:type :cc :controller 1 :value coarse :channel 1}
                                                     {:type :cc :controller 33 :value fine :channel 1}] (envelope "b"))
            {released :commands} (clip/release-all p (envelope "stop"))]
        (is (= [[:param/set "0:1"]] (mapv (juxt :op :instance-id) sets)))
        (is (= [[:instance/end "0:1"]] (mapv (juxt :op :instance-id) released)))))
    (testing "with a track map, playback tagged with another track is not a capture track"
      (let [p (clip/player layout {:bridge-id "b"} {"track:7" 1})
            launches #(mapv :cue-id (:commands (clip/play p [(merge {:type :note-on :note 36 :velocity 127 :channel 1} %)]
                                                          (envelope "m"))))]
        (is (= [:wash] (launches {:track-id "track:7"})))
        (is (= [] (launches {:track-id "track:9"})) "the companion moved onto a musician's track")
        (is (= [:wash] (launches {})) "untagged events keep their channel")))
    (testing "a rejected launch drops its run"
      (let [{p :player} (clip/play p0 [{:type :note-on :note 36 :velocity 127 :channel 1}] (envelope "r"))]
        (is (empty? (:runs (:player (clip/acknowledge p {:command-id "r/0" :status :rejected} (envelope "x"))))))))))

(deftest the-feed-uses-the-clip-encoding
  (let [layout (clip/allocate-layout {} registry (assoc (clip/phrase-targets phrase) :lane-channel 2))]
    (is (= [{:controller 1 :value 64 :type :cc :channel 2} {:controller 33 :value 0 :type :cc :channel 2}
            {:type :note-on :note 36 :velocity 127 :channel 1}]
           (clip/feed-messages layout {:kind :launch :cue-id :wash :params {:level (/ 8192.0 16383)}})))
    (is (= [{:type :note-off :note 36 :velocity 0 :channel 1}] (clip/feed-messages layout {:kind :end :cue-id :wash})))
    (is (= [] (clip/feed-messages layout {:kind :launch :cue-id :unknown})))
    (is (re-find #"ch 2 cc  1/33 :wash :level" (clip/describe-layout layout)))))

(deftest decoded-numbers-stay-inside-their-bounds
  (let [lane (fn [bounds] {:components (clip/components {:type :number :bounds bounds})})]
    (testing "the top code decodes to the upper bound, not one ulp past it"
      (let [l (lane [-3.7926394860949153 4.70649252237781])]
        (is (= 4.70649252237781 (clip/decode-value l [16383])))
        (is (= -3.7926394860949153 (clip/decode-value l [0])))))
    (testing "a value outside a recommended-only range is not clamped into it"
      (let [l {:components (clip/components {:type :number :recommended [0.0 1.0]})}]
        (is (= [16383] (clip/encode-value l 1.0)))
        (is (nil? (clip/encode-value l 2.5)))))))
