(ns tolglow.studio.effect-inspector-test
  (:require [cljs.test :refer-macros [deftest is]]
            [re-frame.db :as rf-db]
            [tolglow.studio.effect-inspector :as inspect]))

(deftest keeps-constituents-with-their-own-run-and-attributes-errors
  (let [first {:id "1" :cue-id "blue" :source {:native-layer {:id "song-A"}}}
        second {:id "2" :cue-id "red" :source {:native-layer {:id "song-A"}}}
        other {:id "3" :cue-id "blue" :source {:native-layer {:id "song-B"}}}
        child {:id "4" :source {:parent-instance-id "1"}}
        bad {:instance-id "3" :cue-id "blue" :message "bad"}]
    (is (= [first second child] (inspect/related {:instances [first second other child]} first)))
    (is (empty? (inspect/faults-for [bad] first)))
    (is (= [bad] (inspect/faults-for [bad] other)))))

(deftest bundles-only-independent-axes-read-by-the-same-geometry-expression
  (let [schema {:type "number"} params [[:x schema] [:y schema] [:z schema] [:speed schema]]
        make-def (fn [ids] {:outputs {:aim {:op "vec3" :args (mapv #(hash-map :op "param" :key %) ids)}}})]
    (is (= [{:attribute :aim :components (subvec params 0 3)} {:parameter :speed :schema schema}]
           (inspect/xyz-controls (make-def ["x" "y" "z"]) params)))
    (is (= 4 (count (inspect/xyz-controls (make-def ["x" "x" "z"]) params))))))

(deftest mutation-carries-typed-identity-source-and-optimistic-version
  (let [context {:runtime-id "r" :epoch 3}
        db {:show context :server-projection {:view {:instances [{:id "3:1" :effect-id :show/a
                                            :definition {:version 7 :params {"p" {:type :number}}
                                                         :outputs {:dimmer 0}}}]}}}
        draft {:ref :dimmer :sources ["p"] :offset 0.1}
        run #(inspect/patch-request db ["3:1" % :dimmer] context draft)
        request (:studio/http (run 7))]
    (is (= true (:typed? request)))
    (is (= {:kind :effects :definition {:id :show/a} :expected-version 7 :output-patch draft}
           (:body request)))
    (is (thrown? js/Error (run 6)))))

(deftest observed-badges-use-the-captured-instance-schema
  (let [schema {:type :number :bounds [0 1]}
        instance {:id "precision:1" :cue-id :precision :effect-id :head :effect-version 1
                  :params {:pan (/ 32767 65535) :rate 0.123456789}
                  :definition {:version 1 :params {:pan (assoc schema :dmx-bits 16) :rate schema}
                               :outputs {:pan {:op :param :key :pan} :dimmer {:op :param :key :rate}}}}
        previous @rf-db/app-db]
    (try
      (reset! rf-db/app-db {:server-projection {:view {:instances [instance]
                                                      :modulation {:overrides {"precision:1" {:pan (/ 32768 65535)}}}}}})
      (let [[_ _ _ effects] (inspect/panel {:instances [instance]} instance [])
            [view & args] (first effects)
            nodes (tree-seq sequential? seq (apply view args))]
        (is (some #{[:span "pan" [:b "0.50001"]]} nodes))
        (is (some #{[:span "rate" [:b "0.123"]]} nodes)))
      (finally (reset! rf-db/app-db previous)))))
