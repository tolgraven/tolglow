(ns tolglow.automation.sections-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.automation.sections :as sections]))

(def plan
  [{:id :verse :weight 1 :length {:bars 2} :max-run 1}
   {:id :chorus :weight 1 :length {:phrases 1}}])

(defn- clock [beat] {:beat beat :playing? true :meter {:beats-per-bar 4 :bars-per-phrase 2}})

(deftest boundaries-stay-on-the-bar-grid-and-skip-without-catching-up
  (let [{first-state :state} (sections/step plan nil 3 (clock 0.0))]
    (is (= 0.0 (get-in first-state [:timer :started-at])))
    (is (== 8 (get-in first-state [:timer :duration :value])) "two bars and one two-bar phrase are both eight beats")
    (is (false? (:changed? (sections/step plan first-state 3 (clock 7.75)))))
    (let [late (sections/step plan first-state 3 (clock 8.5))]
      (is (= 8.0 (get-in late [:state :timer :started-at])) "a slightly late tick keeps the grid")
      (is (= 1 (get-in late [:state :index]))))
    (let [skipped (sections/step plan first-state 3 (clock 40.0))]
      (is (= 40.0 (get-in skipped [:state :timer :started-at])) "a long gap restarts from now, once")
      (is (= 1 (get-in skipped [:state :index]))))))

(deftest max-run-and-seeded-sequences
  (let [walk (fn [seed] (->> (iterate (fn [[state beat]] [(:state (sections/step plan state seed (clock beat))) (+ beat 8)])
                                      [nil 0.0])
                             (drop 1) (take 40) (map (comp :id first))))]
    (is (= (walk 11) (walk 11)))
    (is (not= (walk 11) (walk 12)))
    (is (not-any? (fn [[a b]] (= :verse a b)) (partition 2 1 (walk 11))) "verse never runs twice in a row")))

(deftest invalid-plans-are-reported
  (let [ok (constantly [])]
    (is (seq (sections/validate [] ok)))
    (is (seq (sections/validate [{:id :a :length {:ms 500}}] ok)))
    (is (seq (sections/validate [{:id :a :length {:bars 1}} {:id :a :length {:bars 1}}] ok)))
    (is (seq (sections/validate [{:id :a :length {:bars 1} :transition :fade}] ok)))
    (is (empty? (sections/validate [{:id :a :length {:bars 1} :weight 1/2 :transition :cut}] ok)))))
