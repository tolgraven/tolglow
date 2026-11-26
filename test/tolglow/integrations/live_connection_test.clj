(ns tolglow.integrations.live-connection-test
  "Automatic Live setup (#680) against the simulated Set: empty and musical
  Sets, idempotent reruns, interrupted setup, user changes and Exclusive Arm."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.integrations.fake-live :as fake]
            [tolglow.integrations.live-connection :as live]
            [tolglow.integrations.phrase-clip :as clip]))

(def layout
  {:layout/version 1 :channel 1 :lane-channel 2 :cues {:wash {:pitch 36}}
   :lanes [{:cue-id :wash :param :level :components [{:type :number :bounds [0 1] :cc 1}]}]})

(def desired (live/desired-setup layout {:port "Tolglow"}))

(defn- connect [live] (live/open (fake/transport live) {:session-id "t"}))

(defn- writes [conn] (remove #{:inspect-set} (live/operations-sent conn)))

(deftest an-empty-set-is-prepared-and-a-rerun-changes-nothing
  (let [set (fake/create)
        conn (connect set)
        first-run (live/setup! conn desired)]
    (is (:ready? first-run) (pr-str (:problems first-run)))
    (is (= [[:ensure-track :executed] [:configure-track :executed] [:ensure-track :executed] [:configure-track :executed]]
           (:applied first-run)))
    (is (= [{:name "Tolglow Cues" :arm? true :monitor :auto :input {:port "Tolglow" :channel 1}}
            {:name "Tolglow Lanes" :arm? true :monitor :auto :input {:port "Tolglow" :channel 2}}]
           (mapv #(select-keys % [:name :arm? :monitor :input]) (get-in first-run [:set :tracks]))))
    (let [conn (connect set)
          again (live/setup! conn desired)]
      (is (:ready? again))
      (is (empty? (writes conn)) "a ready Set needs no writes")
      (is (= (:tracks first-run) (:tracks again)) "the same tracks are found again"))))

(deftest a-musical-set-keeps-its-own-armed-tracks-and-routing
  (let [set (fake/create {:tracks [{:name "Bass" :arm? true :monitor :in :input {:port "Keystep" :channel 3}}
                                   {:name "Drums" :arm? true}]})
        result (live/setup! (connect set) desired)]
    (is (:ready? result))
    (is (= [["Bass" true :in {:port "Keystep" :channel 3}] ["Drums" true :auto {:port "Keyboard" :channel 1}]]
           (mapv (juxt :name :arm? :monitor :input) (take 2 (get-in result [:set :tracks])))))))

(deftest exclusive-arm-is-caught-by-the-read-back
  (let [set (fake/create {:exclusive-arm? true :tracks [{:name "Bass" :arm? true}]})
        result (live/setup! (connect set) desired)]
    (is (not (:ready? result)) "arming the second capture track disarmed the first")
    (is (= [{:channel 1 :code :not-configured}]
           (mapv #(select-keys % [:channel :code]) (:problems result))))
    (is (false? (get-in result [:problems 0 :actual :arm?])))))

(deftest an-interrupted-setup-is-repaired-without-duplicates
  (let [set (fake/create)]
    (fake/inject-fault! set :ensure-track :lose-ack)
    (let [conn (connect set)
          broken (live/setup! conn desired)]
      (is (not (:ready? broken)))
      (is (= [[:ensure-track :indeterminate]] (:applied broken)) "a lost acknowledgement stops setup; nothing is retried")
      (is (= 1 (count (get-in broken [:set :tracks]))) "the track was created although the receipt was lost"))
    (let [repaired (live/setup! (connect set) desired)]
      (is (:ready? repaired))
      (is (= ["Tolglow Cues" "Tolglow Lanes"] (mapv :name (get-in repaired [:set :tracks])))))))

(deftest user-changes-are-repaired-or-reported
  (let [set (fake/create)
        setup (live/setup! (connect set) desired)
        lanes (get-in setup [:tracks 2])]
    (testing "a deleted capture track is recreated alone"
      (fake/delete-track! set lanes)
      (let [conn (connect set) repaired (live/setup! conn desired)]
        (is (:ready? repaired))
        (is (= [:ensure-track :configure-track] (writes conn)))
        (is (= (get-in setup [:tracks 1]) (get-in repaired [:tracks 1])))))
    (testing "a disarmed track is rearmed in place"
      (let [track (get-in @set [:tracks 0 :track-id])]
        (live/request! (connect set) :configure-track {:track-id track :arm? false})
        (let [conn (connect set)]
          (is (:ready? (live/setup! conn desired)))
          (is (= [:configure-track] (writes conn))))))
    (testing "two tracks with a capture name are left for the performer"
      (swap! set update :tracks conj {:track-id "track:dup" :name "Tolglow Cues" :kind :midi :arm? false
                                      :monitor :auto :input {} :slots {} :history []})
      (let [conn (connect set) result (live/setup! conn desired)]
        (is (= [:duplicate-tracks] (mapv :code (:problems result))))
        (is (empty? (writes conn)))))))

(def phrase
  {:phrase/version 1 :length-beats 4 :meter {:beats-per-bar 4 :bars-per-phrase 4}
   :notes [{:id "n0" :cue-id :wash :start 0 :length 2 :params {:level 0.5}}] :lanes []})

(deftest writes-name-the-track-they-expect
  (let [set (fake/create)
        conn (connect set)
        setup (live/setup! conn desired)]
    (fake/rename-track! set (get-in setup [:tracks 2]) "My lanes")
    (let [result (live/capture! conn setup {:phrase phrase :layout layout})]
      (is (not (:complete? result)))
      (is (= [:executed :rejected] (mapv :status (:receipts result))))
      (is (= :track-renamed (:code (second (:receipts result)))))
      (is (= :executed (:shown result)) "the clip that was written is still shown"))))

(deftest capture-routes-are-exclusive-and-honest
  (testing "the buffer route never invokes Capture MIDI"
    (let [set (fake/create) conn (connect set)
          result (live/capture! conn (live/setup! conn desired) {:phrase phrase :layout layout})]
      (is (:complete? result))
      (is (some #{:write-clip} (live/operations-sent conn)))
      (is (not-any? #{:capture-midi} (live/operations-sent conn)))))
  (testing "Capture MIDI with nothing received reports it instead of writing"
    (let [set (fake/create) conn (connect set)
          setup (live/setup! conn (live/desired-setup layout {:port "Tolglow" :route :live-capture-midi}))
          result (live/capture! conn setup {:phrase phrase :layout layout})]
      (is (not (:complete? result)))
      (is (= :nothing-to-capture (get-in result [:receipt :code])))
      (is (nil? (:shown result)))
      (is (not-any? #{:write-clip} (live/operations-sent conn)))))
  (testing "a Set without Capture MIDI is not ready for that route"
    (let [set (fake/create {:can-capture-midi? false})
          setup (live/setup! (connect set) (live/desired-setup layout {:route :live-capture-midi}))]
      (is (= [:capture-midi-unavailable] (mapv :code (:problems setup))))
      (is (thrown? clojure.lang.ExceptionInfo (live/capture! (connect set) setup {:phrase phrase :layout layout})))))
  (testing "unknown routes and operations are refused before anything is sent"
    (is (thrown? clojure.lang.ExceptionInfo (live/desired-setup layout {:route :both})))
    (is (thrown? clojure.lang.ExceptionInfo (live/request! (connect (fake/create)) :save-set {})))))

(deftest a-single-channel-layout-needs-one-capture-track
  (let [single (dissoc layout :lane-channel)]
    (is (= [{:channel 1 :name "Tolglow Capture"}]
           (mapv #(select-keys % [:channel :name]) (:tracks (live/desired-setup single {})))))
    (is (= [1] (clip/channels single)))))

(deftest setup-capture-and-read-back-survive-the-companion-json-wire
  ;; The companion answers over loopback HTTP as JSON: enumerations arrive as
  ;; strings and beats as doubles.
  (let [set (fake/create {:tracks [{:name "Bass" :arm? true :monitor :in :input {:port "Keystep" :channel 3}}]})
        connect-json #(live/open (fake/json-transport set) {:session-id "json"})
        setup (live/setup! (connect-json) desired)]
    (is (:ready? setup) (pr-str (:problems setup)))
    (is (= [:in :auto :auto] (mapv :monitor (get-in setup [:set :tracks]))))
    (let [conn (connect-json)]
      (is (:ready? (live/setup! conn desired)))
      (is (empty? (writes conn)) "a ready Set needs no writes over the wire either"))
    (let [phrase (assoc phrase :notes [{:id "n0" :cue-id :wash :start 1/2 :length 5/4 :params {:level 0.5}}]
                        :lanes [{:cue-id :wash :param :level :points [{:beat 3/2 :value 0.25 :note "n0"}]}])
          conn (connect-json)
          result (live/capture! conn setup {:phrase phrase :layout layout})
          back (live/import-clips! conn (:clips result) layout {:meter (:meter phrase)})]
      (is (:complete? result))
      (is (= (clip/comparable (clip/clips->phrase (clip/phrase->clips phrase layout) layout {:meter (:meter phrase)}))
             (clip/comparable back))
          "exact beats and 14-bit values come back")
      (fake/rename-track! set (get-in setup [:tracks 2]) "My lanes")
      (is (= :track-renamed (:code (second (:receipts (live/capture! (connect-json) setup {:phrase phrase :layout layout})))))))))
