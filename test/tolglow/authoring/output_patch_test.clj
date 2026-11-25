(ns tolglow.authoring.output-patch-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.authoring.output-patch :as patch]
            [tolglow.demo :as demo]
            [tolglow.show :as show]))

(def scalar {:type :number :bounds [0 1] :default 0.6})

(def effect {:id :patched :version 1 :params {:x scalar :y scalar :z scalar :skew/xyz {:type :vector :size 3 :default [0 0 0]}}
             :duration {:beats 7/11} :outputs [{:attribute :color :value [1 0 0] :blend :max :targets [:head-1]}
                                               {:attribute :dimmer :value 0.0 :targets [:head-1]}]})

(deftest patch-one-output-keeps-siblings-targets-and-exact-duration
  (let [new (patch/apply-patch effect {:ref 0 :sources [:x :y :z] :scale 0.5 :offset 0.1 :clamp [0 1]})
        expr (get-in new [:outputs 0 :value])]
    (is (= [:x :y :z] (patch/sources expr)))
    (is (= [:clamp :add :multiply :vec3]
           (mapv :op (take 4 (iterate #(first (:args %)) expr)))))
    (is (= effect (assoc-in new [:outputs 0 :value] [1 0 0])))
    (is (= :skew/xyz (get-in (patch/apply-patch effect {:ref 0 :sources [:skew/xyz]}) [:outputs 0 :value :key])))
    (doseq [bad [{:ref 0 :sources [:x]} {:ref 1 :sources [:missing]} {:ref 2 :sources [:x]}
                 {:ref 1 :sources [:x] :clamp [1 0]} {:ref 1 :sources [:x] :scale Double/NaN}]]
      (is (thrown? clojure.lang.ExceptionInfo (patch/apply-patch effect bad))))))

(deftest preserved-instance-follows-published-patch-and-concurrent-edit-rejects
  (let [runtime (show/create-runtime {:model (demo/model) :workspace-options nil})
        clock (atom 0)
        issue (fn [op body] (let [p (show/submit! runtime op body)] (show/tick! runtime (swap! clock + 25)) @p))]
    (try
      (is (= :accepted (:status (issue :definition {:kind :effects :definition (dissoc effect :duration)}))))
      (is (= :accepted (:status (issue :definition {:kind :cues :definition {:id :patched :version 1 :effect-id :patched
                                                                             :reload-policy :preserve-state}}))))
      (let [id (:instance-id (issue :command {:op :cue/launch :cue-id :patched}))
            send-patch #(issue :definition {:kind :effects :definition {:id :patched} :expected-version 1
                                            :output-patch {:ref 1 :sources [:x] :scale 0.5}})
            value #(bit-and 255 (aget ^bytes (get-in @(:state runtime) [:output :universes 1]) 0))]
        (is (string? id))
        (is (zero? (value)))
        (is (= :accepted (:status (send-patch))))
        (is (= 2 (get-in @(:state runtime) [:model :instances id :effect-version])))
        (is (<= 76 (value) 77))
        (is (= :rejected (:status (send-patch)))))
      (finally (show/stop! runtime)))))
