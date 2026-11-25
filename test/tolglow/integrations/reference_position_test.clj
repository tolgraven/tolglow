(ns tolglow.integrations.reference-position-test
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.integrations.reference-position :as position]))

(def ^:private provenance {:source-id :deck-a :session-id :one :clock-id :capture})
(def ^:private recording {:id :track-a :version 1})

(defn- event [type time-ms occurrence]
  {:type type :provenance provenance :occurrence occurrence :observed-at-ms time-ms})

(defn- matching
  ([time-ms position-ms] (matching time-ms position-ms 0 {}))
  ([time-ms position-ms occurrence options]
   (assoc (event :match time-ms occurrence)
          :match (merge {:reference recording :position-ms position-ms :rate 1.0 :score 0.98} options))))

(defn- feed [state observations]
  (reduce (fn [state observation]
            (position/advance state (:observed-at-ms observation) observation))
          state observations))

(defn- locked []
  (feed (position/new-state provenance)
        [(matching 0 1000) (matching 100 1100) (matching 200 1200)]))

(deftest confirms-straight-play-and-projects-at-the-observed-rate
  (let [one (position/advance (position/new-state provenance) 15 (matching 0 1000 0 {:rate 1.25}))
        duplicate (position/advance one 25 (matching 0 1000 0 {:rate 1.25}))
        two (position/advance duplicate 115 (matching 100 1125 0 {:rate 1.25}))
        three (position/advance two 215 (matching 200 1250 0 {:rate 1.25}))
        view (position/snapshot three 250)]
    (is (= 1 (:confirmations (position/snapshot duplicate 25))))
    (is (= :acquiring (:status (position/snapshot two 115))))
    (is (zero? (:lookahead-ms (position/snapshot two 115))))
    (is (= :locked (:status view)))
    (is (= recording (:reference view)))
    (is (= 1312.5 (:position-ms view)))
    (is (= 1.25 (:rate view)))
    (is (= 200 (:observed-at-ms view)))
    (is (= 500.0 (:lookahead-until-ms view)))))

(deftest expires-from-capture-time-and-reacquires-on-fresh-evidence
  (let [state (locked)
        close-to-expiry (position/snapshot state 900)
        expired (position/advance state 950)
        stale (position/advance expired 1000 (matching 225 1225))
        recovered (feed stale [(matching 1010 2010) (matching 1100 2100) (matching 1200 2200)])]
    (is (= 50.0 (:lookahead-ms close-to-expiry)))
    (is (= 950.0 (:lookahead-until-ms close-to-expiry)))
    (is (= :expired (:reason (position/snapshot expired 950))))
    (is (= 0 (:confirmations (position/snapshot stale 1000))))
    (is (= :locked (:status (position/snapshot recovered 1200))))
    (is (= 0 (:occurrence (position/snapshot recovered 1200))))
    (is (= 1 (:generation (position/snapshot recovered 1200))))))

(deftest same-recording-rival-revokes-position-while-retaining-identity
  (let [state (locked)
        repeated (assoc (matching 300 1300) :runner-up {:reference recording :score 0.97})
        ambiguity (position/advance state 300 repeated)
        view (position/snapshot ambiguity 300)
        recovered (feed ambiguity [(matching 400 1400) (matching 500 1500) (matching 600 1600)])
        competing (assoc repeated :runner-up {:reference {:id :track-b :version 1} :score 0.97})
        identity-view (position/snapshot (position/advance state 300 competing) 300)]
    (is (= :ambiguous (:status view)))
    (is (= :position-ambiguous (:reason view)))
    (is (= recording (:reference view)))
    (is (zero? (:lookahead-ms view)))
    (is (nil? (:position-ms view)))
    (is (= :identity-ambiguous (:reason identity-view)))
    (is (nil? (:reference identity-view)))
    (is (= :locked (:status (position/snapshot recovered 600))))
    (is (= 0 (:occurrence (position/snapshot recovered 600))))
    (is (= 1 (:generation (position/snapshot recovered 600))))))

(deftest seek-loop-and-cut-fence-old-match-completions
  (doseq [cause [:seek :loop :cut]]
    (testing (name cause)
      (let [state (locked)
            transitioned (position/advance state 310 (event cause 300 0))
            late (position/advance transitioned 330 (matching 315 1315))
            duplicate (position/advance late 340 (event cause 300 0))
            fresh (feed duplicate [(matching 350 1000 1 {}) (matching 450 1100 1 {})
                                   (matching 550 1200 1 {})])
            lost-view (position/snapshot duplicate 340)
            restored-view (position/snapshot fresh 550)]
        (is (= cause (:reason lost-view)))
        (is (= 1 (:occurrence lost-view)))
        (is (= 0 (:confirmations lost-view)))
        (is (zero? (:lookahead-ms lost-view)))
        (is (= :locked (:status restored-view)))
        (is (= recording (:reference restored-view)))
        (is (= 1 (:occurrence restored-view)))
        (is (= 1200.0 (:position-ms restored-view)))))))

(deftest known-loop-end-caps-horizon-at-playback-rate
  (let [loop-end {:rate 2.0 :until-position-ms 1700}
        state (feed (position/new-state provenance)
                    [(matching 0 1000 0 loop-end) (matching 100 1200 0 loop-end)
                     (matching 200 1400 0 loop-end)])
        view (position/snapshot state 300)
        past-boundary (position/snapshot state 350)]
    (is (= 1600.0 (:position-ms view)))
    (is (= 50.0 (:lookahead-ms view)))
    (is (= 350.0 (:lookahead-until-ms view)))
    (is (= :playback-boundary (:reason past-boundary)))
    (is (zero? (:lookahead-ms past-boundary)))))

(deftest fresh-position-and-rate-discontinuities-drop-the-lock
  (let [state (locked)]
    (doseq [[observed reason] [[(matching 300 2200) :position-jump]
                               [(matching 300 1300 0 {:rate 1.5}) :rate-change]
                               [(matching 300 1300 0 {:reference (assoc recording :version 2)}) :reference-change]]]
      (let [view (position/snapshot (position/advance state 300 observed) 300)]
        (is (= :acquiring (:status view)))
        (is (= reason (:reason view)))
        (is (= 1 (:generation view)))
        (is (zero? (:lookahead-ms view)))))))

(deftest validates-provenance-chronology-and-match-data
  (let [state (locked)
        unavailable (event :unavailable 300 0)]
    (testing "old, future or unrelated input cannot change the active lock"
      (doseq [ignored [(event :unavailable 199 0) (event :unavailable 310 0)
                       (assoc-in unavailable [:provenance :clock-id] :another-clock)
                       (assoc-in unavailable [:provenance :session-id] :another-session)]]
        (is (= :locked (:status (position/snapshot (position/advance state 300 ignored) 300))))))
    (testing "a current unavailable or invalid match revokes immediately"
      (doseq [[invalid reason] [[unavailable :source-unavailable]
                                [(matching 300 1300 0 {:rate ##NaN}) :invalid-match]
                                [(matching 300 1300 0 {:reference "malformed"}) :invalid-match]
                                [(matching 300 1300 0 {:score 0.4}) :weak-match]]]
        (let [view (position/snapshot (position/advance state 300 invalid) 300)]
          (is (= reason (:reason view)))
          (is (zero? (:lookahead-ms view))))))
    (is (thrown? clojure.lang.ExceptionInfo (position/advance state 199)))
    (is (thrown? clojure.lang.ExceptionInfo (position/new-state {})))
    (is (thrown? clojure.lang.ExceptionInfo (position/new-state provenance {:confirmations 1})))))
