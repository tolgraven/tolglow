(ns tolglow.effects.state-migration-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [afterglow.reactive :as reactive]
            [tolglow.demo :as demo]
            [tolglow.registry :as registry]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [afterglow.control.model :as control]
            [afterglow.render.frame :as render]
            [afterglow.runtime.engine :as engine]
            [tolglow.automation.prediction :as prediction]
            [tolglow.effects.state-migration :as state-migration]))

(defn- failure-safe [migrate state] (migrate state 1 2 {}))

(defn- paths [problems] (mapv :path problems))

(deftest declarations-are-validated-portable-data
  (is (empty? (state-migration/errors {:from {1 [{:op :rename :from [:count] :to [:frames]}
                                               {:op :assoc :path [:mode] :value :smooth}]
                                            2 []
                                            3 [{:op :default :path [:phase] :value 0}
                                               {:op :dissoc :path [:legacy]}
                                               {:op :scale :path [:frames] :by 2}]}} 4)))
  (is (= [[:state-migration] [:state-migration :extra] [:state-migration :from] [:state-migration :from]
          [:state-migration :from 0] [:state-migration :from 1 0 :op] [:state-migration :from 1 1 :path]
          [:state-migration :from 1 2 :value] [:state-migration :from 1 3 :by] [:state-migration :from 2]]
         (paths (concat (state-migration/errors inc 2)
                        (state-migration/errors {:from {1 []} :extra 1} 2)
                        (state-migration/errors {:from [1]} 2)
                        (state-migration/errors {:from {}} 2)
                        (state-migration/errors {:from {0 []}} 2)
                        (state-migration/errors {:from {1 [{:op :eval}
                                                           {:op :dissoc :path []}
                                                           {:op :assoc :path [:x] :value inc}
                                                           {:op :scale :path [:x] :by ##Inf}]}} 2)
                        (state-migration/errors {:from {2 []}} 2)))))
  (testing "source versions must be below the definition's own version"
    (is (= [[:state-migration :from 3]] (paths (state-migration/errors {:from {1 [] 3 []}} 2))))
    (is (= [[:state-migration :from 2]] (paths (state-migration/errors {:from {2 []}} 2)))))
  (testing "a rename cannot target a path that contains or lies inside its source"
    (is (= [[:state-migration :from 1 0 :to] [:state-migration :from 1 1 :to]]
           (paths (state-migration/errors {:from {1 [{:op :rename :from [:a] :to [:a :b]}
                                                     {:op :rename :from [:a :b] :to [:a]}]}} 2)))))
  (is (= [[:state-migration :from]]
         (paths (state-migration/errors {:from {1 (vec (repeat 257 {:op :dissoc :path [:x]}))}} 2)))))

(def chain
  {:from {1 [{:op :rename :from [:count] :to [:frames]}]
          2 []
          3 [{:op :default :path [:phase] :value 0}
             {:op :scale :path [:frames] :by 2}]}})

(deftest skipped-versions-chain-through-each-listed-source
  (let [migrate (state-migration/compile chain 4)]
    (is (= {:frames 6 :phase 0} (migrate {:count 3} 1 4 {})) "v1 runs the lists for 1, 2 and 3")
    (is (= {:frames 6 :phase 0} (migrate {:frames 3} 2 4 {})) "v2 skips v1's rename")
    (is (= {:frames 6 :phase 0} (migrate {:frames 3} 3 4 {})))
    (let [missing (try (migrate {} 0 4 {}) nil (catch clojure.lang.ExceptionInfo e (ex-data e)))]
      (is (= [[:state-migration :from] 0] ((juxt :path :from-version) missing))))
    (is (thrown? clojure.lang.ExceptionInfo (state-migration/compile chain 3)) "Compile validates against the version")))

(defn- failure [migrate state]
  (try (migrate state 1 2 {}) nil (catch clojure.lang.ExceptionInfo e e)))

(deftest every-step-failure-carries-the-authored-step-path
  (doseq [[label steps state path message]
          [["assoc through a number" [{:op :assoc :path [:n :x] :value 1}] {:n 5} [:state-migration :from 1 0] #"assoc failed"]
           ["assoc past a vector end" [{:op :dissoc :path [:z]} {:op :assoc :path [:v 5] :value 1}] {:v [0]}
            [:state-migration :from 1 1] #"assoc failed"]
           ["scale missing value" [{:op :scale :path [:missing] :by 2}] {} [:state-migration :from 1 0 :path] #"finite number"]
           ["scale long overflow" [{:op :scale :path [:n] :by 4}] {:n Long/MAX_VALUE} [:state-migration :from 1 0 :by] #"64-bit"]
           ["scale double overflow" [{:op :scale :path [:n] :by 1.0e300}] {:n 1.0e300} [:state-migration :from 1 0 :by] #"finite"]
           ["rename onto a value" [{:op :rename :from [:a] :to [:b]}] {:a 1 :b 2} [:state-migration :from 1 0 :to] #"already holds"]]]
    (testing label
      (let [error (failure (state-migration/compile {:from {1 steps}} 2) state)]
        (is (= path (:path (ex-data error))))
        (is (re-find message (.getMessage ^Exception error)))))))

(deftest nested-removal-and-rename-leave-absent-parents-alone
  (let [migrate (state-migration/compile {:from {1 [{:op :dissoc :path [:a :b]}
                                                    {:op :dissoc :path [:n :x]}
                                                    {:op :rename :from [:p :q] :to [:r]}
                                                    {:op :rename :from [:m :old] :to [:m :new]}]}} 2)]
    (is (= {:n 3} (migrate {:n 3} 1 2 {})) "No {:a nil} or {:p nil} is inserted")
    (is (= {:m {:new 1 :kept 2}} (migrate {:m {:old 1 :kept 2}} 1 2 {})))))

(deftest scale-keeps-whole-numbers-exact-and-coerces-the-rest-to-double
  (let [migrate #(get (failure-safe (state-migration/compile {:from {1 [{:op :scale :path [:n] :by %2}]}} 2) {:n %1}) :n)]
    (is (= 6 (migrate 3 2)))
    (is (instance? Long (migrate 3 2)))
    (is (= 1.5 (migrate 3 0.5)))
    (is (= 1.0 (migrate 1/2 2)) "A ratio becomes a double")
    (is (= 5.0 (migrate 2.5M 2)))))

(deftest publication-rejects-invalid-policies-and-migrations
  (is (= [[:state-migration :from 1 0 :op]]
         (paths (registry/validate-definition :effects {:id :x :version 2 :params {} :outputs {:dimmer {:op :const :value 1.0}}
                                                         :state-migration {:from {1 [{:op :eval}]}}}))))
  (is (= [[:state-migration :from 3]]
         (paths (registry/validate-definition :effects {:id :x :version 2 :params {} :outputs {:dimmer {:op :const :value 1.0}}
                                                         :state-migration {:from {3 []}}}))))
  (is (= [[:reload-policy]]
         (paths (registry/validate-definition :cues {:id :x :version 1 :effect-id :x :reload-policy :later}))))
  (is (empty? (registry/validate-definition :cues {:id :x :version 1 :effect-id :x :reload-policy nil}))
      "An explicit nil policy from older data means :pinned")
  (is (empty? (registry/validate-definition :cues {:id :x :version 1 :effect-id :x :reload-policy :migrate}))))

(deftest attach-compiles-once-per-declaration-without-a-variadic-wrapper
  (let [cache (atom {}) step (fn [context] (assoc context :ran true))
        definition {:id :x :version 2 :state-migration {:from {1 []}}}
        first-step (state-migration/attach cache definition step)
        second-step (state-migration/attach cache definition step)]
    (is (identical? (engine/state-migration first-step) (engine/state-migration second-step)))
    (is (= 1 (count @cache)))
    (is (not (instance? clojure.lang.RestFn first-step)))
    (is (= {:ran true} (first-step {})))
    (is (identical? step (state-migration/attach cache {:id :y :version 1} step)))))

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
            (is (= :accepted (:status (apply! r (publish 2 {:from {1 [{:op :default :path [:frames] :value 4}
                                                                          {:op :scale :path [:frames] :by 10}]}}) 100))))
            (show/tick! r 150))
          (is (= 2 (:effect-version (state broken))))
          (is (= {:frames 40} (:state (state broken))))
          (is (nil? (:reload-fault (state broken)))))
        (testing "a throwing migration keeps the last good definition and state"
          (is (= :accepted (:status (apply! broken (publish 3 {:from {2 [{:op :scale :path [:missing] :by 2}]}}) 200))))
          (show/tick! twin 200)
          (doseq [t [300 400]] (show/tick! broken t) (show/tick! twin t))
          (let [fault (:reload-fault (state broken))]
            (is (= 2 (:effect-version (state broken))))
            (is (= {:frames 40} (:state (state broken))))
            (is (= [:migrate 3 2 id] ((juxt :stage :effect-version :from-version :instance-id) fault)))
            (is (= "Scaled state must be a finite number" (:message fault)))
            (is (= [[:state-migration] [:state-migration :from 2 0 :path]] (mapv (comp :path :location) (:causes fault))))
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
                (is (= [["state-migration"] ["state-migration" "from" 2 0 "path"]]
                       (mapv (comp :path :location) (:causes wire))))
                (is (= :effect/reload-failed (-> (show/snapshot broken) :effect-events :events last :type)))))))
        (testing "a newer working migration recovers from the last good state"
          (is (= :accepted (:status (apply! broken (publish 4 {:from {2 [{:op :rename :from [:frames] :to [:ticks]}] 3 []}}) 500))))
          (show/tick! broken 600)
          (is (= 4 (:effect-version (state broken))))
          (is (= {:ticks 40} (:state (state broken))))
          (is (nil? (:reload-fault (state broken))))
          (is (= :effect/recovered (-> (show/snapshot broken) :effect-events :events last :type)))))
      (finally (show/stop! broken) (show/stop! twin)))))

(deftest prediction-applies-the-same-migrations-as-live
  (let [launched (control/apply-command (show-model)
                                        {:op :cue/launch :cue-id :counter :command-id "launch" :show-id (:show-id (show-model))
                                         :epoch (:epoch (show-model)) :time-ms 0 :source {:adapter :test}})
        model (:model launched)
        id (get-in launched [:ack :instance-id])
        v2 (assoc counter :version 2 :outputs {:dimmer {:op :const :value 0.6}}
                  :state-migration {:from {1 [{:op :assoc :path [:migrated] :value true}]}})
        pending (assoc-in model [:registry :effects :counter] v2)
        committed (update-in pending [:instances id] assoc :definition v2 :effect-version 2 :state {:migrated true})
        patch (render/compile-patch (get-in model [:rig :fixtures]))
        universes #(update-vals (:universes (prediction/frame % patch [] 10)) vec)]
    (is (= (universes committed) (universes pending)) "The pending :migrate reload renders v2 in prediction")
    (is (not= (universes model) (universes pending)))))
