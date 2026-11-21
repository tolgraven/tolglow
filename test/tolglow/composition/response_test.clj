(ns tolglow.composition.response-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.composition.response :as response]
            [tolglow.compiler :as compiler]))

(def effect
  {:id :wash :version 1
   :params {:level {:type :number :default 0.8 :bounds [0 1]}
            :pan {:type :number :default 0.6 :bounds [0 1]}
            :steps {:type :integer :default 5 :bounds [1 10]}
            :color {:type :color :default [1.0 0.5 0.0]}}
   :outputs {:dimmer {:op :param :key :level}
             :pan {:op :param :key :pan}
             :color {:op :param :key :color}}})

(def context
  {:frame {:time-ms 50 :fixtures [{:id :head :position [0 0 0]}]}
   :instance {:id "instance-1" :cue-id :red :effect-id :wash :definition effect
              :started-at-ms 0 :phase :running :params {:level 0.8 :pan 0.6}}
   :state {} :assignments [] :intents [] :done? false})

(def gate {:id :global-attack :params [:level]
           :envelope {:attack-ms 100 :release-ms 200} :state {:started-at-ms 0}})

(deftest musical-global-response-freezes-with-transport
  (let [gate {:id :musical :params [:level] :envelope {:attack {:bars 1} :release {:beats 1/2}}
              :state {:started-at-ms 0 :started-at-beat 8 :timing-meter {:beats-per-bar 3}}}
        frame (assoc-in context [:frame :transport] {:beat 9.5 :bpm 120})
        result (response/apply-responses [gate] frame)
        paused (response/apply-responses [gate] (-> frame (assoc-in [:frame :time-ms] 90000)
                                                       (assoc-in [:frame :transport :playing?] false)))]
    (is (= 0.4 (get-in result [:instance :params :level])))
    (is (= (get-in result [:instance :params]) (get-in paused [:instance :params])))
    (is (= 0.8 (get-in context [:instance :params :level])))))

(deftest global-response-modulates-frame-copy-without-changing-model-params
  (let [result (response/apply-responses [gate] context)
        middleware (response/compile-middleware [gate])
        compiled (comp (compiler/compile-effect effect) middleware)
        values (into {} (map (juxt :attribute :value)) (:assignments (compiled context)))]
    (is (= 0.4 (get-in result [:instance :params :level])))
    (is (= 0.8 (get-in context [:instance :params :level])))
    (is (= 0.6 (get-in result [:instance :params :pan])))
    (is (= 0.4 (:dimmer values)))
    (is (= [1.0 0.5 0.0] (:color values)))
    (is (= result (middleware context)))))

(deftest selectors-intersect-and-empty-selection-matches-nothing
  (let [selected (assoc gate :select {:effect-ids [:wash] :cue-ids [:red] :instance-ids ["instance-1"]})
        missed (assoc selected :select {:effect-ids [:wash] :cue-ids [:other]})]
    (is (= 0.4 (get-in (response/apply-responses [selected] context) [:instance :params :level])))
    (is (= context (response/apply-responses [missed] context)))
    (is (= context (response/apply-responses [(assoc gate :select {:effect-ids []})] context)))))

(deftest ordered-responses-respect-vector-integer-and-hard-domain-bounds
  (let [gain {:id :gain :params :all :operation :multiply :range [2 2] :state {:started-at-ms 0}}
        offset {:id :offset :params [:level] :operation :add :range [-0.25 -0.25] :state {:started-at-ms 0}}
        result (response/apply-responses [gain offset] context)]
    (is (= 0.75 (get-in result [:instance :params :level])))
    (is (= 1 (get-in result [:instance :params :pan])))
    (is (= 10 (get-in result [:instance :params :steps])))
    (is (= [1.0 1.0 0.0] (mapv double (get-in result [:instance :params :color]))))
    (is (= [:gain :offset] (mapv :id (:response-values result))))))

(deftest released-response-holds-terminal-value-without-ending-other-effects
  (let [released (assoc-in gate [:state :released-at-ms] 50)
        tail (response/apply-responses [released] (assoc-in context [:frame :time-ms] 150))
        done (response/apply-responses [released] (assoc-in context [:frame :time-ms] 250))]
    (is (= 0.2 (get-in tail [:instance :params :level])))
    (is (= 0.0 (get-in done [:instance :params :level])))
    (is (true? (get-in done [:response-values 0 :done?])))
    (is (false? (:done? done)))
    (is (= context (response/apply-responses [] context)))))

(deftest invalid-response-configurations-fail-before-effect-evaluation
  (doseq [candidate [(assoc gate :params :unknown)
                     (assoc gate :select {:effect-ids :wash})
                     (assoc gate :range [0 ##Inf])
                     (assoc-in gate [:state :started-at-ms] nil)]]
    (is (thrown? clojure.lang.ExceptionInfo (response/compile-response candidate))))
  (is (thrown? clojure.lang.ExceptionInfo (response/compile-middleware [gate gate])))
  (is (thrown? clojure.lang.ExceptionInfo (response/apply-responses [gate gate] context))))
