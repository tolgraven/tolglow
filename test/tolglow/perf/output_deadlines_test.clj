(ns tolglow.perf.output-deadlines-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.perf.output-deadlines :as replay]))

(defn- attempt [slot source sent returned]
  (let [due (* 25 slot)]
    {:slot slot :frame-id (when sent 1) :source-time-ms source :deadline-ms due
     :started-at-ms due :finished-at-ms (or returned (inc due))
     :sent-at-ms sent :sent-by-ms returned}))

(deftest measurements-follow-actual-paired-sink-calls
  (let [outputs [(attempt 1 25 25 26)
                 (attempt 2 50 nil nil)
                 (assoc (attempt 3 nil nil nil) :expired-before-send? true)
                 (attempt 4 100 124 130)
                 (attempt 5 25 138 139)]
        wire [{:id 1 :at-ms 26 :bytes [12]} {:id 1 :at-ms 128 :bytes [12]} {:id 1 :at-ms 139 :bytes [12]}]
        paired (replay/match-sink-entries outputs wire)
        report (replay/summary [] paired 1 5)]
    (is (= [26 nil nil 128 139] (mapv :sink-at-ms paired)))
    (is (= [3 1 1 1 1 false]
           ((juxt :sent :sent-with-current-source :expired-before-send
                  :late-sink-entries :short-sink-gaps :meets-sink-timing?) report)))
    (is (= 2 (:matching-source-delivered report)))
    (is (nil? (:meets-sink-timing? (replay/summary [] paired 2 3))))
    (is (thrown? clojure.lang.ExceptionInfo (replay/match-sink-entries outputs (rest wire))))
    (is (thrown? clojure.lang.ExceptionInfo (replay/match-sink-entries outputs (conj wire (peek wire)))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (replay/match-sink-entries outputs (assoc-in wire [1 :id] 99))))))
