(ns tolglow.audio.stems-test
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.audio.capture :as capture]
            [tolglow.audio.stem-fixtures :as fixtures]
            [tolglow.audio.stems :as stems]))

(defn- by-stem [events] (group-by :stem events))

(defn- score [rendered events id tolerance-ms]
  (stems/score-onsets (map :time-ms (get (by-stem events) id)) (map :ms (get-in rendered [:onsets id])) tolerance-ms))

(deftest configuration-is-canonical-and-validated
  (let [[kick bass] (stems/validate-config [{:id :kick :channel 1 :preset :kick}
                                            {:id "bass" :channel 3 :preset "bass" :band "bass" :label "Bass DI"}] 4)]
    (is (= [:kick 1 [30.0 160.0] :kick] ((juxt :id :channel :band :preset) kick)))
    (is (= [:bass 3 :bass "Bass DI"] ((juxt :id :channel :band :label) bass)))
    (is (= {:beats 1/4} (:min-interval bass)))
    (is (= 18.0 (:isolation-db kick))))
  (doseq [[stems channels] [[[] 4] [[{:id :kick :channel 5}] 4] [[{:id :kick :channel 0}] 4]
                            [[{:id "Kick!" :channel 1}] 4] [[{:id :kick :channel 1} {:id :kick :channel 2}] 4]
                            [[{:id :kick :channel 1 :preset :tuba}] 4] [[{:id :kick :channel 1 :band [500.0 100.0]}] 4]
                            [[{:id :kick :channel 1 :min-interval {:bars 1}}] 4] [[{:id :kick :channel 1 :colour :red}] 4]
                            [(vec (for [i (range 17)] {:id (keyword (str "s" i)) :channel 1})) 4]]]
    (is (thrown? clojure.lang.ExceptionInfo (stems/validate-config stems channels)) (pr-str stems))))

(deftest the-onset-gate-follows-tempo-inside-the-design-range
  (is (= 120.0 (stems/effective-bpm nil)))
  (is (= [70.0 128.0 180.0] (mapv #(stems/effective-bpm {:bpm %}) [40.0 128.0 260.0])))
  (is (< (Math/abs (- 142.857 (stems/interval-ms {:beats 1/6} 70.0))) 0.001))
  (is (< (Math/abs (- 55.556 (stems/interval-ms {:beats 1/6} 180.0))) 0.001))
  (is (= 30.0 (stems/interval-ms {:ms 30} 180.0)))
  ;; Two short hits 100 ms apart: a retrigger at 70 BPM, two hits at 180.
  (let [spec (stems/validate-config [{:id :hit :channel 1 :preset :full}] 1)
        hit (fn [^doubles out start-ms]
              (dotimes [i 4800]
                (let [t (/ i 48000.0) at (+ i (long (* 48 start-ms)))]
                  (aset out at (+ (aget out at) (* 0.6 (Math/exp (/ (- t) 0.012)) (Math/sin (* 2 Math/PI 220 t))))))))
        hits (fn [bpm]
               (let [out (double-array 48000)]
                 (hit out 500) (hit out 600)
                 (mapv :time-ms (:events (stems/replay spec {:samples {:hit out} :sample-rate 48000
                                                            :clock-at (fn [ms] {:time-ms ms :beat 0.0 :bpm bpm :playing? true})})))))]
    (is (= [500.0] (hits 70.0)))
    (is (= [500.0 600.0] (hits 180.0)))))

(deftest synthesized-stems-are-detected-on-time-without-false-positives-at-70-120-and-180-bpm
  (doseq [bpm [70.0 120.0 180.0]]
    (testing (str bpm " BPM")
      (let [rendered (fixtures/render {:bpm bpm})
            {:keys [events frames]} (fixtures/replay rendered)]
        (doseq [id [:kick :snare :bass]]
          (let [{:keys [matched missed false-positives max-abs-error-ms]} (score rendered events id 25.0)]
            (is (= (count (get-in rendered [:onsets id])) matched) [id missed])
            (is (empty? false-positives) [id false-positives])
            (is (<= max-abs-error-ms 8.0) [id max-abs-error-ms])))
        ;; Plucks over a ringing string are soft; at 180 BPM one in eight is
        ;; missed (observed), never doubled.
        (let [{:keys [matched false-positives max-abs-error-ms]} (score rendered events :guitar 25.0)
              expected (count (get-in rendered [:onsets :guitar]))]
          (is (= 8 expected))
          (is (>= matched (if (= bpm 180.0) 7 8)))
          (is (empty? false-positives) false-positives)
          (is (<= max-abs-error-ms 15.0)))
        (testing "onsets carry their transport beat"
          (doseq [{:keys [beat]} (get (by-stem events) :kick)]
            (is (< (Math/abs (- beat (Math/rint beat))) 0.03) beat)))
        (testing "neighbours at -24 dB are rejected as bleed, not ignored silently"
          (is (pos? (reduce + (map :bleed-count (vals (:features (last frames)))))))
          (is (every? #(= (count (get-in rendered [:onsets %])) (get-in (last frames) [:features % :onset-count]))
                      [:kick :snare :bass])))
        (testing "trailing silence releases every level to zero with no events"
          (let [silent-from (* (/ 60000.0 bpm) (+ 1 16 2))]
            (is (empty? (filter #(> (:time-ms %) silent-from) events)))
            (is (every? zero? (map :level (vals (:features (last frames))))))))))))

(deftest a-dropout-on-one-stem-removes-its-onsets-and-drops-its-level
  (let [dropout {:stem :bass :from-beat 9 :to-beat 13}
        rendered (fixtures/render {:dropout dropout})
        {:keys [events frames]} (fixtures/replay rendered)
        {:keys [matched false-positives]} (score rendered events :bass 25.0)
        level-at (fn [ms] (get-in (first (filter #(>= (:time-ms %) ms) frames)) [:features :bass :level-db]))]
    (is (= 12 matched (count (get-in rendered [:onsets :bass]))))
    (is (empty? false-positives))
    (is (empty? (filter #(and (= :bass (:stem %)) (<= 4500 (:time-ms %) 6500)) events)))
    ;; Only bleed remains in the gap: at least 15 dB under the playing stem.
    (is (>= (- (level-at 4300) (level-at 6200)) 15.0) [(level-at 4300) (level-at 6200)])
    (testing "the other stems keep their timing through the dropout"
      (doseq [id [:kick :snare :guitar]]
        (is (empty? (:false-positives (score rendered events id 25.0))))
        (is (= (count (get-in rendered [:onsets id])) (:matched (score rendered events id 25.0))))))))

(deftest a-stopped-clock-marks-onsets-and-holds-their-beat
  (let [rendered (fixtures/render {:bars 1})
        spec (stems/validate-config fixtures/stem-layout 4)
        {:keys [events]} (stems/replay spec {:samples (:samples rendered) :sample-rate 48000
                                             :clock-at (fn [_] {:time-ms 0.0 :beat 7.0 :bpm 120.0 :playing? false})})]
    (is (seq events))
    (is (every? false? (map :transport-playing? events)))
    (is (every? #(= 7.0 %) (map :beat events)))))

(deftest gaps-restart-detectors-instead-of-inventing-onsets
  (let [spec (stems/validate-config [{:id :kick :channel 1 :preset :kick}] 1)
        tone (double-array (for [i (range 480)] (* 0.5 (Math/sin (/ (* 2 Math/PI 60 i) 48000.0)))))
        first-block (stems/analyze (stems/new-state spec) {:blocks {:kick tone} :sample-rate 48000 :started-at-ms 0.0
                                                          :received-at-ms 10.0})
        resumed (stems/analyze (:state first-block) {:blocks {:kick tone} :sample-rate 48000 :started-at-ms 500.0
                                                    :received-at-ms 510.0})]
    (is (empty? (:events first-block)))
    (is (:discontinuity? resumed))
    (is (empty? (:events resumed)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (stems/analyze (:state resumed) {:blocks {} :sample-rate 48000 :started-at-ms 510.0 :received-at-ms 520.0})))))

(deftest scoring-matches-each-expected-onset-once
  (is (= {:matched 2 :missed [300] :false-positives [104 500] :errors-ms [2 -3]
          :max-abs-error-ms 3.0 :mean-abs-error-ms 2.5}
         (stems/score-onsets [102 104 197 500] [100 200 300] 5))))

(deftest a-multichannel-recording-replays-through-capture-as-named-stems
  (let [file (java.io.File/createTempFile "tolglow-stems-" ".wav")
        rendered (fixtures/render {:bars 2})
        events (atom [])]
    (try
      (is (= {:channels 4 :sample-rate 48000} (select-keys (fixtures/write-wav! rendered file) [:channels :sample-rate])))
      (let [config (capture/file-config (str file) {:stems fixtures/stem-layout :file-paced? false})
            manager (capture/create-manager)]
        (is (= {:file (str file) :sample-rate 48000.0 :channels 4 :input-channel 1}
               (select-keys config [:file :sample-rate :channels :input-channel])))
        (is (= [:kick :snare :bass :guitar] (mapv :id (:stems config))))
        (is (thrown? clojure.lang.ExceptionInfo (capture/validate-config (assoc config :stems [{:id :kick :channel 9}]))))
        (is (thrown? clojure.lang.ExceptionInfo (capture/validate-config (assoc config :default? true))))
        (try
          (let [handle (capture/start! manager config {:events! #(do (swap! events into %) true)})
                result (deref (:result handle) 10000 ::timeout)
                stems (get-in (capture/status manager) [:features :stems])]
            (is (= :completed (:status result)) (pr-str result))
            (is (= {:kick 8 :snare 4 :bass 8 :guitar 4} (update-vals stems :onset-count)))
            (is (= 24 (count (filter #(= :audio/onset (:type %)) @events)))
                "every onset that reached the features also reached the event handler")
            (is (every? #(= (keyword "stem" (name (:stem %))) (get-in % [:source :id]))
                        (filter #(= :audio/onset (:type %)) @events))))
          (finally (capture/close! manager))))
      (finally (.delete file)))))
