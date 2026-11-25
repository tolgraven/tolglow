(ns tolglow.studio.pad-breath-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [clojure.string :as str]
            [reagent.core :as r]
            ["react-dom/server" :refer [renderToStaticMarkup]]
            [tolglow.studio.pad-breath :as breath]
            [tolglow.studio.views :as views]))

(defn- close? [a b] (< (js/Math.abs (- a b)) 1e-9))

(deftest interpolates-the-projected-transport-between-samples
  (let [anchor (breath/next-anchor nil {:beat 8 :bpm 120 :playing? true :time-ms 1000} 0 5000)]
    (is (close? 0 (breath/breath-phase anchor 5000)))
    (is (close? 0.25 (breath/breath-phase anchor 5125)) "120 bpm advances a quarter beat in 125 ms")
    (is (close? 0.5 (breath/breath-phase anchor 5250)))
    (is (close? 1 (:breath (breath/levels 0))) "the breath peaks on the beat, as Push and Afterglow do")
    (is (close? 0 (:breath (breath/levels 0.5))))
    (testing "a sample that has not changed keeps its anchor, so re-renders do not drift the clock"
      (is (identical? anchor (breath/next-anchor anchor {:beat 8 :bpm 120 :playing? true :time-ms 1000} 0 5400))))
    (testing "a silent stream holds rather than inventing progress"
      (is (= (breath/beat-at anchor 60000) (breath/beat-at anchor (+ 5000 breath/max-extrapolation-ms)))))
    (testing "a paused transport holds its phase"
      (let [paused (breath/next-anchor nil {:beat 8.3 :bpm 120 :playing? false :time-ms 1000} 0 5000)]
        (is (close? 0.3 (breath/breath-phase paused 9000)))))))

(deftest a-tempo-change-follows-the-new-sample
  (let [fast (breath/next-anchor nil {:beat 4 :bpm 120 :playing? true :time-ms 0} 0 0)
        ;; The server moved to 60 bpm. Its next sample, 200 ms later, says beat 4.3.
        slow (breath/next-anchor fast {:beat 4.3 :bpm 60 :playing? true :time-ms 200} 0 200)]
    (is (not (identical? fast slow)))
    (is (close? 0.3 (breath/breath-phase slow 200)) "the projected beat wins over the old extrapolation (0.4)")
    (is (close? 0.55 (breath/breath-phase slow 450)) "60 bpm advances a quarter beat in 250 ms, not half")
    (is (close? 0.05 (breath/breath-phase slow 950)) "the phase wraps at the next downbeat")))

(deftest a-seek-re-anchors-at-once
  (let [before (breath/next-anchor nil {:beat 12.9 :bpm 120 :playing? true :time-ms 0} 3 0)
        ;; A seek bumps the transport generation. The beat may even go backwards.
        after (breath/next-anchor before {:beat 2.25 :bpm 120 :playing? true :time-ms 40} 4 40)]
    (is (close? 0.98 (breath/breath-phase before 40)))
    (is (= 4 (:generation after)))
    (is (close? 0.25 (breath/breath-phase after 40)))
    (is (close? 0.5 (breath/breath-phase after 165)))
    (testing "a seek to the same beat at the same frame time still counts as a new sample"
      (let [same (breath/next-anchor after {:beat 2.25 :bpm 120 :playing? true :time-ms 40} 5 900)]
        (is (not (identical? after same)))
        (is (close? 0.25 (breath/breath-phase same 900)))))))

(deftest release-and-queued-blinks-keep-push-phase
  (is (= 1 (:blink (breath/levels 0.1))) "bright through Afterglow's first 40% of the beat")
  (is (= 1 (:blink (breath/levels 0.4))))
  (is (= 0 (:blink (breath/levels 0.41))))
  (is (= [1 0 1 0] (mapv #(:flash (breath/levels %)) [0 0.3 0.5 0.8])) "queued pads blink twice per beat"))

(def ^:private cues
  [{:id :ember :name "Ember"} {:id :haze :name "Haze"} {:id :strobe :name "Strobe"}
   {:id :wash :name "Wash"} {:id :gate :name "Gate"} {:id :idle :name "Idle"}])

(defn- grid-db []
  {:connection {:connected? true} :ui {:view :perform}
   :show {}
   :server-projection
   {:view {:cues cues
           :instances [{:id "1" :cue-id :ember :phase :running}
                       {:id "2" :cue-id :haze :phase :releasing}
                       {:id "3" :cue-id :strobe :phase :running}
                       {:id "4" :cue-id :gate :phase "running" :launch-mode "held"}]
           :pending-cue-launches [{:command-id "q1" :cue-id :strobe :due-beat 8}
                                  {:command-id "q2" :cue-id :wash :due-beat 8}]}}})

(def ^:private page
  {:id :main :cells (mapv (fn [x cue] {:x x :y 0 :cue-id (:id cue)}) (range) cues)})

(defn- pad [db x]
  (let [html (renderToStaticMarkup (r/as-element [@#'views/cue-cell db page x 0 nil]))
        classes (set (str/split (second (re-find #"class=\"([^\"]*)\"" html)) #"\s+"))
        label (second (re-find #"aria-label=\"([^\"]*)\"" html))]
    {:classes classes :label label}))

(deftest cue-pads-distinguish-running-queued-held-and-releasing
  (let [db (grid-db)
        [ember haze strobe wash gate idle] (map #(pad db %) (range 6))]
    (is (every? (:classes ember) ["cue-cell" "active" "running"]))
    (is (not-any? (:classes ember) ["queued" "releasing" "held"]))
    (is (str/includes? (:label ember) "row 1, running"))
    (is (every? (:classes haze) ["active" "releasing"]))
    (is (not ((:classes haze) "running")) "a releasing pad blinks instead of breathing")
    (is (every? (:classes strobe) ["active" "running" "queued"]) "running and queued at once")
    (is (str/includes? (:label strobe) "running, queued"))
    (is (every? (:classes wash) ["queued"]))
    (is (not-any? (:classes wash) ["active" "running"]))
    (is (every? (:classes gate) ["active" "running" "held"]))
    (is (str/includes? (:label gate) "row 1, held"))
    (is (= #{"cue-cell"} (:classes idle)))
    (testing "the shared clock runs only while this page has something to animate"
      (is (true? (@#'views/pad-breath-wanted? db page)))
      (is (false? (@#'views/pad-breath-wanted? (assoc-in db [:server-projection :view]
                                                            {:cues cues :instances [] :pending-cue-launches []})
                                                page))))))

(deftest pad-state-accepts-wire-strings-and-keywords
  (is (= #{:active :running} (breath/pad-state [{:phase "running"}] false)))
  (is (= #{:active :releasing :queued} (breath/pad-state [{:phase :releasing}] true)))
  (is (breath/animated? #{:queued}))
  (is (not (breath/animated? #{}))))
