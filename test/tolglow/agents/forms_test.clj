(ns tolglow.agents.forms-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.agents.forms :as forms]
            [tolglow.demo :as demo]
            [tolglow.effects :as fx]
            [tolglow.show :as show]))

(deftest trusted-values-use-the-captured-model-without-string-evaluation
  (let [model (demo/model)
        result (forms/request model [:registry/publish :effects :warm {:outputs {:dimmer [:local :amount]}}]
                              {:amount (fn [captured] (fx/constant (:master captured)))})]
    (is (= :definition (first result)))
    (is (= (fx/constant 1) (-> result second :definition :outputs first :value)))
    (is (= (:epoch model) (:expected-epoch (second result))))
    (is (thrown? clojure.lang.ExceptionInfo (forms/request model [:master/set [:local :missing]])))
    (is (thrown? clojure.lang.ExceptionInfo (forms/request model [:master/set [:local :a]] {:a [:local :a]})))
    (is (thrown? clojure.lang.ExceptionInfo (forms/request model '(+ 1 2))))
    (is (thrown? clojure.lang.ExceptionInfo (forms/request model [:cue/put :broken :absent])))
    (is (thrown? clojure.lang.ExceptionInfo (forms/request model [:cue/put :bad :ember {:params {:level 3}}])))
    (is (thrown? clojure.lang.ExceptionInfo (forms/request model [:cue/launch :ember {} :extra])))))

(deftest forms-use-the-actual-owner-queue-to-publish-launch-edit-and-release
  (let [runtime (show/create-runtime {:workspace-options nil :workers 1})
        commit! (fn [form t]
                  (let [receipt (forms/submit! runtime form)]
                    (show/tick! runtime t) (deref receipt 1000 {:status :timeout})))]
    (try
      (is (= :accepted (:status (commit! [:registry/publish :effects :blush
                                          {:template :wash :color [0.8 0.2 0.1] :level 0.36}] 0))))
      (is (= :accepted (:status (commit! [:cue/put :wall :blush {:params {:level 0.46}}] 20))))
      (let [id (:instance-id (commit! [:cue/launch :wall {:mode :one-shot :duration {:bars 2}}] 40))]
        (is (some? id))
        (is (= 0.46 (get-in @(:state runtime) [:model :instances id :params :level])))
        (is (= :accepted (:status (commit! [:param/set [:instance/of :wall] :level 0.25] 60))))
        (is (= :rejected (:status (commit! [:param/set [:instance/of :wall] :level 1.8] 80))))
        (is (= 0.25 (get-in @(:state runtime) [:model :instances id :params :level])))
        (is (= :accepted (:status (commit! [:instance/end [:instance/of :wall]] 100)))))
      (finally (show/stop! runtime)))))
