(ns tolglow.automation.launch-params-test
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.automation.launch-params :as launch-params]
            [tolglow.automation.random :as random]
            [tolglow.registry :as registry]))

(def effect
  {:id :wash :version 1
   :params {:level {:type :number :default 0.5 :bounds [0 1] :recommended [0.2 0.9] :random {:enabled? true}}
            :hue-shift {:type :number :default 0.0 :bounds [-1 1]}
            :color {:type :color :default [1 1 1]}
            :period {:type :number :default 1 :bounds [0.1 20] :unit :bars :random {:enabled? true}}
            :steps {:type :integer :default 4 :bounds [1 16] :unit :beats :random {:enabled? true}}
            :shape {:type :enum :default :sine :values [:sine :square :saw]
                    :random {:enabled? true :weights {:sine 0 :square 1 :saw 0}}}
            :invert? {:type :boolean :default false :random {:enabled? true}}
            :seed {:type :integer :default 1 :bounds [0 100] :random {:enabled? true}}
            :locked {:type :number :default 0.3 :bounds [0 1] :random {:enabled? true :locked? true}}
            :off {:type :number :default 0.3 :bounds [0 1] :random {:enabled? false}}}
   :outputs {:color [1 0 0] :dimmer {:op :param :key :level}}})

(def cue {:id :wash :effect-id :wash})

(deftest marked-parameters-stay-in-their-typed-domains
  (doseq [seed (range 40)]
    (let [{:keys [params excluded]} (launch-params/choose effect cue {} seed 0 {})]
      (is (<= 0.2 (:level params) 0.9) "recommended range narrows the hard bounds")
      (is (contains? #{0.125 0.25 0.5 1.0 2.0 4.0 8.0 16.0} (:period params))
          "a musical period is a legal power-of-two division inside its bounds")
      (is (contains? #{1 2 4 8 16} (:steps params)) "an integer beat count stays integral")
      (is (= :square (:shape params)) "enum weights exclude zero-weighted values")
      (is (boolean? (:invert? params)))
      (is (= {:hue-shift :not-marked :color :not-marked :seed :seed-parameter :locked :locked :off :locked}
             (select-keys excluded [:hue-shift :color :seed :locked :off])))
      (doseq [[k v] params] (is (registry/valid-value? (get-in effect [:params k]) v) (str k))))))

(deftest palettes-choose-whole-colors-by-weight
  (let [red [1.0 0.0 0.0] purple [0.42 0.18 0.5]
        config {:palette [{:color red :weight 9} {:color purple :weight 1}]}
        colors (map #(get-in (launch-params/choose effect cue config 7 % {}) [:params :color]) (range 400))
        counts (frequencies colors)]
    (is (= #{red purple} (set colors)) "never a componentwise blend")
    (is (< 300 (counts red) 400))
    (is (< 10 (counts purple) 80))))

(deftest seeded-replay-and-independent-substreams
  (let [config {:params :all :ranges {:hue-shift [-0.25 0.25]}}
        once (launch-params/choose effect cue config 42 3 {})
        fewer (launch-params/choose (update effect :params dissoc :level :period) cue config 42 3 {})]
    (is (= once (launch-params/choose effect cue config 42 3 {})))
    (is (not= (:params once) (:params (launch-params/choose effect cue config 42 4 {}))))
    (is (<= -0.25 (get-in once [:params :hue-shift]) 0.25) "an authored range opts a parameter in")
    (testing "removing parameters leaves every other choice unchanged"
      (is (= (dissoc (:params once) :level :period) (:params fewer))))))

(deftest performer-values-locks-launch-values-and-categories-win
  (let [saved (assoc cue :saved-params {:level 0.75})
        result (launch-params/choose effect saved {} 1 0 {:invert? true})]
    (is (= :performer-saved (get-in result [:excluded :level])))
    (is (= :fixed-by-launch (get-in result [:excluded :invert?])))
    (is (not (contains? (:params result) :level)))
    (is (contains? (:params (launch-params/choose effect saved {:saved :randomize} 1 0 {})) :level)))
  (testing "a category selection skips cues outside it; a cue in several categories is drawn once"
    (is (empty? (:params (launch-params/choose effect cue {:categories [:movement]} 1 0 {}))))
    (is (= (:params (launch-params/choose effect cue {} 1 0 {}))
           (:params (launch-params/choose effect cue {:categories [:color :intensity]} 1 0 {}))))))

(deftest infeasible-domains-are-reported-not-looped
  (let [narrow (update-in effect [:params :period] assoc :bounds [0.3 0.4] :default 0.35)
        result (launch-params/choose narrow cue {:params [:period :level] :ranges {:level [2 3]}} 1 0 {})]
    (is (= {:period :infeasible :level :infeasible} (select-keys (:excluded result) [:period :level])))
    (is (empty? (:params result))))
  (is (seq (launch-params/validate {:palette [{:color [2 0 0]}]})))
  (is (seq (launch-params/validate {:params :some})))
  (is (empty? (launch-params/validate {:params "marked" :ranges {:level [0 1]}}))))

(deftest weighted-choice-and-derived-seeds-are-stable
  (is (= (random/derive-seed 42 :cue :level) (random/derive-seed 42 :cue :level)))
  (is (not= (random/derive-seed 42 :cue :level) (random/derive-seed 42 :cue :hue)))
  (is (= [1013904223 :b] (random/choose-weighted 0 [:a :b :c] {:a 0 :b 1 :c 0})))
  (is (thrown? clojure.lang.ExceptionInfo (random/choose-weighted 0 [:a] {:a 0}))))
