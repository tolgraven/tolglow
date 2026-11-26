(ns tolglow.integrations.phrase-capture-test
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.integrations.phrase-capture :as capture]))

(def ^:private performer {:adapter :push})

(defn- accepted [id op data & [instance-id]]
  {:command (merge {:command-id id :op op :epoch 0 :source performer} data)
   :ack (cond-> {:command-id id :status :accepted :epoch 0} instance-id (assoc :instance-id instance-id))})

(defn- feed [buffer & observations]
  (reduce (fn [b [beat obs]] (capture/observe b (assoc obs :beat beat :bpm 120.0))) buffer (partition 2 observations)))

(defn- launch [id instance cue & [params]]
  (accepted id :cue/launch (cond-> {:cue-id cue :launch {:mode :persistent}} params (assoc-in [:launch :params] params)) instance))

(defn- set-param [id instance param value] (accepted id :param/set {:instance-id instance :param param :value value}))

(defn- end [id instance] (accepted id :instance/end {:instance-id instance} instance))

(deftest captures-quantised-notes-and-lanes-ending-on-the-nearest-bar
  (let [buffer (feed (capture/new-buffer {:bars 2})
                     0.02 (launch "a" "i1" :wash {:level 0.2})
                     1.49 (set-param "b" "i1" :level 0.6)
                     1.51 (set-param "c" "i1" :level 0.7)
                     3.13 (end "d" "i1")
                     4.0 (launch "e" "i2" :wash)
                     8.2 (set-param "f" "i2" :level 1.0))
        phrase (capture/capture buffer)]
    (is (= [0 8] (capture/phrase-window buffer {})) "pressed at 8.2, the capture ends on bar line 8")
    (is (= [{:id "n0" :cue-id :wash :start 0 :length 13/4 :params {:level 0.2} :carried-in? false :held-at-end? false}
            {:id "n1" :cue-id :wash :start 4 :length 4 :params {} :carried-in? false :held-at-end? true}]
           (:notes phrase)))
    (is (= [{:cue-id :wash :param :level :points [{:beat 3/2 :value 0.7 :note "n0"}]}] (:lanes phrase))
        "two writes in one sixteenth keep the later one")
    (is (= phrase (capture/validate-phrase phrase)))))

(deftest a-value-written-as-an-activation-starts-is-part-of-its-launch
  (let [phrase (capture/capture (feed (capture/new-buffer {:bars 1})
                                      1.0 (launch "a" "i1" :wash)
                                      1.05 (set-param "b" "i1" :level 0.4)
                                      2.0 (set-param "c" "i1" :level 0.8))
                                {:end-beat 4})]
    (is (= {:level 0.4} (:params (first (:notes phrase)))))
    (is (= [{:beat 2 :value 0.8 :note "n0"}] (get-in phrase [:lanes 0 :points])))))

(deftest playback-and-feedback-origins-are-not-captured
  (let [echo (fn [obs] (assoc-in obs [:command :source] {:type :ableton :origin :live-playback}))
        buffer (feed (capture/new-buffer {:bars 1})
                     0.0 (echo (launch "a" "i1" :wash))
                     0.5 (set-param "b" "i1" :level 1.0)
                     1.0 (end "c" "i1")
                     1.0 (launch "d" "i2" :strobe)
                     2.0 (echo (set-param "e" "i2" :rate 9)))
        phrase (capture/capture buffer {:end-beat 4})]
    (is (= [:strobe] (mapv :cue-id (:notes phrase))))
    (is (empty? (:lanes phrase)))
    (testing "an empty ignore set observes playback, for verifying a replay"
      (let [all (feed (capture/new-buffer {:bars 1 :ignore-origins #{}})
                      0.0 (echo (launch "a" "i1" :wash)) 1.0 (end "c" "i1"))]
        (is (= [:wash] (mapv :cue-id (:notes (capture/capture all {:end-beat 4})))))))))

(deftest rejected-duplicate-and-unknown-commands-are-ignored
  (let [rejected (assoc-in (launch "x" "i9" :wash) [:ack :status] :rejected)
        buffer (feed (capture/new-buffer {:bars 1})
                     0.0 (launch "a" "i1" :wash)
                     0.25 rejected
                     0.5 (launch "a" "i1" :wash)
                     0.75 (accepted "m" :master/set {:value 0.5})
                     1.0 (set-param "z" "unknown-instance" :level 1.0))]
    (is (= 1 (count (:events buffer))))))

(deftest activations-older-than-retention-are-carried-in-with-their-current-values
  (let [buffer (feed (capture/new-buffer {:bars 1})
                     0.0 (launch "a" "i1" :wash {:level 0.1})
                     1.0 (set-param "b" "i1" :level 0.3)
                     2.0 (launch "c" "i2" :strobe)
                     3.0 (end "d" "i2")
                     9.0 (set-param "e" "i1" :level 0.9)
                     10.0 (launch "f" "i3" :strobe))]
    (is (every? #(>= (:beat %) 5.0) (:events buffer)) "events older than one bar before beat 10 are folded away")
    (let [phrase (capture/capture buffer {:end-beat 12})]
      (is (= [{:cue-id :wash :start 0 :length 4 :params {:level 0.3} :carried-in? true :held-at-end? true}
              {:cue-id :strobe :start 2 :length 2 :params {} :carried-in? false :held-at-end? true}]
             (mapv #(dissoc % :id) (:notes phrase))))
      (is (= [{:beat 1 :value 0.9 :note "n0"}] (get-in phrase [:lanes 0 :points]))))))

(deftest a-backwards-seek-starts-a-new-phrase-and-keeps-running-activations
  (let [buffer (feed (capture/new-buffer {:bars 1})
                     8.0 (launch "a" "i1" :wash)
                     9.0 (launch "b" "i2" :strobe)
                     9.5 (end "c" "i2")
                     2.0 (set-param "d" "i1" :level 0.5))
        phrase (capture/capture buffer {:end-beat 4})]
    (is (= 1 (:discontinuities buffer)))
    (is (= [[:wash 2 {:level 0.5}]] (mapv (juxt :cue-id :start :params) (:notes phrase)))
        "the running wash continues from the seek target; the released strobe is gone")
    (is (empty? (:lanes phrase)))))

(deftest a-new-show-epoch-resets-the-buffer
  (let [buffer (feed (capture/new-buffer {:bars 1}) 0.0 (launch "a" "i1" :wash))
        restarted (capture/observe buffer (-> (launch "b" "i1" :strobe) (assoc-in [:command :epoch] 1)
                                              (assoc-in [:ack :epoch] 1) (assoc :beat 1.0)))]
    (is (= [:strobe] (mapv :cue-id (vals (:open restarted)))))
    (is (= 1 (count (:events restarted))))))

(deftest a-release-in-the-launch-cell-keeps-one-grid-step
  (let [phrase (capture/capture (feed (capture/new-buffer {:bars 1}) 1.0 (launch "a" "i1" :strobe) 1.05 (end "b" "i1"))
                                {:end-beat 4})]
    (is (= [[1 1/4]] (mapv (juxt :start :length) (:notes phrase))))))

(deftest events-after-reports-what-to-feed-next
  (let [buffer (feed (capture/new-buffer {:bars 1}) 0.0 (launch "a" "i1" :wash) 1.0 (end "b" "i1"))]
    (is (= [:launch :end] (mapv :kind (capture/events-after buffer 0))))
    (is (= [:end] (mapv :kind (capture/events-after buffer 1))))
    (is (= :wash (:cue-id (last (:events buffer)))) "a release names its cue for the MIDI feed")))

(deftest invalid-phrases-are-refused
  (is (thrown? clojure.lang.ExceptionInfo
               (capture/validate-phrase {:phrase/version 1 :length-beats 4 :notes [{:id "n0" :cue-id :a :start 3 :length 2 :params {}}] :lanes []})))
  (is (thrown? clojure.lang.ExceptionInfo
               (capture/validate-phrase {:phrase/version 1 :length-beats 4 :notes [{:id "n0" :cue-id :a :start 0 :length 1 :params {}}]
                                         :lanes [{:cue-id :b :param :x :points [{:beat 0 :value 1 :note "n0"}]}]}))))

(deftest observe-model-applies-a-seek-before-the-commands-that-follow-it
  (let [model (fn [beat time-ms order history]
                {:epoch 0 :transport {:beat beat :bpm 120.0 :time-ms time-ms}
                 :instances (into {} (for [{:keys [ack]} (vals history) :when (:instance-id ack)] [(:instance-id ack) {:phase :running}]))
                 :control {:history-order order :history history}})
        a (launch "a" "i1" :wash)
        b (assoc-in (launch "b" "i2" :strobe) [:command :time-ms] 1000)
        buffer (-> (capture/new-buffer {:bars 1})
                   (capture/observe-model (model 9.0 4500 ["a"] {"a" (assoc-in a [:command :time-ms] 4500)}))
                   (capture/observe-model (model 2.0 1000 ["a" "b"] {"a" (assoc-in a [:command :time-ms] 4500) "b" b})))]
    (is (= 1 (:discontinuities buffer)))
    (is (= [[:launch 2.0]] (mapv (juxt :kind :beat) (:events buffer))) "the launch after the seek keeps its own beat")))
