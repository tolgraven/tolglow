(ns tolglow.studio.effect-inspector-test
  (:require [cljs.test :refer-macros [deftest is]]
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
