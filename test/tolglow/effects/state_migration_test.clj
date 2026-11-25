(ns tolglow.effects.state-migration-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [afterglow.reactive :as reactive]
            [tolglow.demo :as demo]
            [tolglow.registry :as registry]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.effects.state-migration :as state-migration]))

(deftest declarations-are-validated-portable-data
  (is (empty? (state-migration/errors {:from [1] :steps [{:op :rename :from [:count] :to [:frames]}
                                                         {:op :assoc :path [:mode] :value :smooth}
                                                         {:op :default :path [:phase] :value 0}
                                                         {:op :dissoc :path [:legacy]}
                                                         {:op :scale :path [:frames] :by 2}]})))
  (is (= [[:state-migration] [:state-migration :extra] [:state-migration :from]
          [:state-migration :steps 0 :op] [:state-migration :steps 1 :path]
          [:state-migration :steps 2 :value] [:state-migration :steps 3 :by]]
         (mapv :path (concat (state-migration/errors inc)
                             (state-migration/errors {:steps [] :extra 1})
                             (state-migration/errors {:from [0] :steps []})
                             (state-migration/errors {:steps [{:op :eval}
                                                              {:op :dissoc :path []}
                                                              {:op :assoc :path [:x] :value inc}
                                                              {:op :scale :path [:x] :by ##Inf}]}))))))

(deftest compiled-migrations-are-pure-ordered-transforms
  (let [migrate (state-migration/compile {:from [1 2]
                                          :steps [{:op :rename :from [:count] :to [:frames]}
                                                  {:op :default :path [:phase] :value 0}
                                                  {:op :default :path [:mode] :value :smooth}
                                                  {:op :dissoc :path [:legacy :old]}
                                                  {:op :scale :path [:frames] :by 2}]})
        state {:count 3 :mode :hard :legacy {:old 1 :kept 2}}]
    (is (= {:frames 6 :phase 0 :mode :hard :legacy {:kept 2}} (migrate state 1 3 {})))
    (is (= state {:count 3 :mode :hard :legacy {:old 1 :kept 2}}) "Input state is never mutated")
    (doseq [[label args path] [["unlisted version" [state 4 5 {}] [:state-migration :from]]
                               ["missing scaled value" [{} 1 3 {}] [:state-migration :steps 4 :path]]
                               ["non-map state" [[1 2] 1 3 {}] [:state-migration]]]]
      (testing label
        (let [error (try (apply migrate args) nil (catch clojure.lang.ExceptionInfo e e))]
          (is (= path (:path (ex-data error)))))))))

(deftest publication-rejects-invalid-policies-and-migrations
  (is (= [[:state-migration :steps 0 :op]]
         (mapv :path (registry/validate-definition :effects {:id :x :version 1 :params {} :outputs {:dimmer {:op :const :value 1.0}}
                                                             :state-migration {:steps [{:op :eval}]}}))))
  (is (= [[:reload-policy]]
         (mapv :path (registry/validate-definition :cues {:id :x :version 1 :effect-id :x :reload-policy :later}))))
  (is (empty? (registry/validate-definition :cues {:id :x :version 1 :effect-id :x :reload-policy :migrate}))))

(def counter
  {:id :counter :version 1 :name "Counter wash" :targets [:head-1]
   :params {:level {:type :number :default 0.4 :bounds [0.0 1.0]}}
   :outputs {:dimmer {:op :param :key :level}}})

(defn- show-model []
  (-> (demo/model)
      (assoc-in [:registry :effects :counter] counter)
      (assoc-in [:registry :cues :counter] {:id :counter :version 1 :effect-id :counter
                                           :priority 20 :reload-policy :migrate})))

(defn- runtime [output]
  (show/create-runtime {:model (show-model) :workspace-options nil :effect-log! (fn [_])
                        :sink {:send! (fn [universes frame] (swap! output conj [frame (vec (get universes 1))]))}}))

(defn- apply! [runtime payload t]
  (let [reply (if (contains? payload :op) (show/submit! runtime payload)
                  (show/submit! runtime :definition payload))]
    (show/tick! runtime t)
    (deref reply 2000 ::timeout)))

(defn- publish [version migration]
  {:kind :effects :definition (cond-> (assoc counter :version version)
                                migration (assoc :state-migration migration))})

(defn- studio-state [runtime]
  (json/read-str (:body ((studio/handler runtime) {:request-method :get :uri "/api/state"
                                                   :headers {"host" "localhost"}}))
                 :key-fn keyword))

(deftest migrate-policy-through-the-show-owner-studio-and-repl
  (let [broken-output (atom []) twin-output (atom [])
        broken (runtime broken-output) twin (runtime twin-output)]
    (try
      (doseq [r [broken twin]]
        (apply! r {:op :cue/launch :cue-id :breathe} 0)
        (apply! r {:op :cue/launch :cue-id :counter} 0))
      (let [id (some #(when (= :counter (:cue-id %)) (:id %)) (:instances (show/snapshot broken)))
            state #(get-in @(:state %) [:model :instances id])]
        (testing "an accepted migration changes state at takeover"
          (doseq [r [broken twin]]
            (is (= :accepted (:status (apply! r (publish 2 {:steps [{:op :default :path [:frames] :value 4}
                                                                     {:op :scale :path [:frames] :by 10}]}) 100))))
            (show/tick! r 150))
          (is (= 2 (:effect-version (state broken))))
          (is (= {:frames 40} (:state (state broken))))
          (is (nil? (:reload-fault (state broken)))))
        (testing "a throwing migration keeps the last good definition and state"
          (is (= :accepted (:status (apply! broken (publish 3 {:steps [{:op :scale :path [:missing] :by 2}]}) 200))))
          (show/tick! twin 200)
          (doseq [t [300 400]] (show/tick! broken t) (show/tick! twin t))
          (let [fault (:reload-fault (state broken))]
            (is (= 2 (:effect-version (state broken))))
            (is (= {:frames 40} (:state (state broken))))
            (is (= [:migrate 3 2 id] ((juxt :stage :effect-version :from-version :instance-id) fault)))
            (is (= "Scaled state must be a finite number" (:message fault)))
            (is (= [[:state-migration] [:state-migration :steps 0 :path]] (mapv (comp :path :location) (:causes fault))))
            (is (<= 3 (count @broken-output)))
            (is (not= (second (last @broken-output)) (second (first @broken-output))) "Captured DMX is live")
            (is (= (map second (take-last 3 @twin-output)) (map second (take-last 3 @broken-output)))
                "Other cues and the fallback revision write identical DMX bytes")
            (is (nil? (get-in (show/snapshot broken) [:status :error])))
            (is (= 1 (get-in (show/snapshot broken) [:effect-events :event-sequence])) "One bounded event, not one per frame")
            (testing "the REPL subscription and Studio's Ring state show the same fault"
              (let [repl (get @(reactive/subscribe (show/reactions broken) [:runtime/effect-faults]) id)
                    instance (some #(when (= id (:id %)) %) (:instances (studio-state broken)))
                    wire (:reload-fault instance)]
                (is (= fault (:reload-fault repl)))
                (is (= ["migrate" 3 2 (:message fault)]
                       ((juxt :stage :effect-version :from-version :message) wire)))
                (is (= [["state-migration"] ["state-migration" "steps" 0 "path"]]
                       (mapv (comp :path :location) (:causes wire))))
                (is (= :effect/reload-failed (-> (show/snapshot broken) :effect-events :events last :type)))))))
        (testing "a newer working migration recovers from the last good state"
          (is (= :accepted (:status (apply! broken (publish 4 {:from [2] :steps [{:op :rename :from [:frames] :to [:ticks]}]}) 500))))
          (show/tick! broken 600)
          (is (= 4 (:effect-version (state broken))))
          (is (= {:ticks 40} (:state (state broken))))
          (is (nil? (:reload-fault (state broken))))
          (is (= :effect/recovered (-> (show/snapshot broken) :effect-events :events last :type)))))
      (finally (show/stop! broken) (show/stop! twin)))))
