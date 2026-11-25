(ns tolglow.compiler-test
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.compiler :as compiler]
            [afterglow.control.model :as control]
            [afterglow.runtime.engine :as engine]
            [afterglow.runtime.executor :as executor]
            [afterglow.render.frame :as frame]))

(def fixture {:id :a :position [0.25 2 3] :universe 1 :address 1
              :channels {:red 1 :green 2 :blue 3 :dimmer 4 :pan 5 :tilt 6}})

(defn context [time-ms]
  {:frame {:time-ms time-ms :fixtures [fixture (assoc fixture :id :b :address 10)]}
   :instance {:id "i" :started-at-ms 100 :params {} :phase :running}
   :state {:owned 1} :assignments [] :intents [] :done? false})

(deftest musical-envelope-and-tween-use-captured-instance-clock
  (let [definition {:id :musical :version 1
                    :envelope {:attack {:bars 1} :release {:beats 1/2}}
                    :outputs {:dimmer 1 :pan {:op :tween :spec {:keyframes [{:at {:beats 0} :value 0}
                                                                          {:at {:phrases 1} :value 1}]}}}}
        step (compiler/compile-effect definition)
        input (-> (context 800)
                  (assoc-in [:frame :transport] {:beat 9.5 :bpm 240 :meter {:beats-per-bar 7}})
                  (update :instance assoc :started-at-beat 8 :timing-meter {:beats-per-bar 3 :bars-per-phrase 2}))
        values #(into {} (map (juxt :attribute :value)) (:assignments (step %)))
        releasing (-> input (assoc-in [:instance :phase] :releasing)
                      (assoc-in [:instance :release-at-beat] 9.5)
                      (assoc-in [:instance :release-at-ms] 800))]
    (is (= {:dimmer 0.5 :pan 0.25} (values input)))
    (is (= (values input) (values (assoc-in input [:frame :time-ms] 90000))))
    (is (= 0.25 (:dimmer (values (assoc-in releasing [:frame :transport :beat] 9.75)))))
    (is (:done? (step (assoc-in releasing [:frame :transport :beat] 10))))))

(deftest procedural-demo-motion-is-musical-and-pause-stable
  (let [definition (last (compiler/demo-definitions))
        step (compiler/compile-effect definition)
        input (-> (context 600) (assoc-in [:frame :transport] {:beat 9 :bpm 120})
                  (update :instance assoc :started-at-beat 8 :timing-meter {:beats-per-bar 4 :bars-per-phrase 4}))
        pan #(->> (step %) :assignments (filter (comp #{:pan} :attribute)) first :value)]
    (is (= 1.0 (pan input)))
    (is (= (pan input) (pan (-> input (assoc-in [:frame :time-ms] 90000)
                              (assoc-in [:frame :transport :playing?] false)))))
    (is (< (Math/abs (- 0.5 (pan (assoc-in input [:frame :transport :beat] 10)))) 1.0e-12))))

(deftest declarative-params-position-targets-and-middleware
  (let [definition {:id :position :version 1 :targets [:a]
                    :params {:level {:type :number :default 0.7 :bounds [0 1]}}
                    :outputs [{:attribute :dimmer :value {:op :param :key :level}}
                              {:attribute :pan :value {:op :position :axis :x}}]}
        step (compiler/compile-effect definition)
        input (assoc (context 200) :assignments [{:previous true}])
        output (step input)]
    (is (= [{:previous true}
            {:target-id :a :attribute :dimmer :value 0.7 :blend :replace}
            {:target-id :a :attribute :pan :value 0.25 :blend :replace}]
           (:assignments output)))
    (is (= {:owned 1} (:state output)))
    (is (= output (step input)))
    (is (= [{:previous true}] (:assignments input)))))

(deftest expression-errors-identify-authored-paths
  (doseq [value [{:op :param :key :missing}
                 {:op :wat}
                 {:op :vec3 :args [0 1 0]}]]
    (let [errors (try (compiler/compile-effect {:id :x :version 1 :outputs {:dimmer value}}) nil
                      (catch clojure.lang.ExceptionInfo e (:errors (ex-data e))))]
      (is (seq errors))
      (is (= :outputs (first (:path (first errors)))))))
  (is (thrown? clojure.lang.ExceptionInfo
               ((compiler/compile-effect {:id :x :version 1 :outputs {:dimmer 4}}) (context 200)))))

(deftest envelope-and-tween-errors-have-source-paths
  (doseq [[extra expected]
          [[{:envelope {:release-ms -1}} [:envelope :release-ms]]
           [{:envelope {:attack {:bars -1}}} [:envelope :attack :bars]]
           [{:envelope {:attributes 5}} [:envelope :attributes]]
           [{:envelope :bad} [:envelope]]
           [{:outputs {:dimmer {:op :tween :spec {:keyframes []}}}} [:outputs 0 :value :spec :keyframes]]
           [{:outputs {:dimmer {:op :tween :spec {:keyframes [{:at {:beats 0} :value 0}
                                                              {:at {:beats -1} :value 1}]}}}}
            [:outputs 0 :value :spec :keyframes 1 :at :beats]]]]
    (let [errors (try (compiler/compile-effect (merge {:id :x :version 1 :outputs {:dimmer 0.5}} extra)) nil
                      (catch clojure.lang.ExceptionInfo cause (:errors (ex-data cause))))]
      (is (= expected (:path (first errors)))))))

(deftest tween-output-and-envelope-release-are-time-sampled
  (let [definition {:id :fade :version 1
                    :envelope {:attack-ms 100 :release-ms 200}
                    :outputs {:dimmer 1 :color [1 0 0]}}
        step (compiler/compile-effect definition)
        releasing (-> (context 250)
                       (assoc-in [:instance :phase] :releasing)
                       (assoc-in [:instance :release-at-ms] 150))
        values (into {} (map (juxt :attribute :value)) (:assignments (step releasing)))
        tween-step (compiler/compile-effect
                    {:id :tween :version 1
                     :outputs {:dimmer {:op :tween :spec {:keyframes [{:at-ms 0 :value 0}
                                                                     {:at-ms 100 :value 1}]}}}})]
    (is (= 0.25 (:dimmer values)))
    (is (= [1 0 0] (:color values)))
    (is (= 0.5 (-> (tween-step (context 150)) :assignments first :value)))
    (is (:done? (step (assoc-in releasing [:frame :time-ms] 350))))))

(defn command [model id op & fields]
  (control/apply-command model
    (merge {:show-id (:show-id model) :epoch (:epoch model)
            :command-id id :source :authoring-test :op op} (apply hash-map fields))))

(deftest musical-one-shot-and-release-follow-real-engine-transport
  (let [effect {:id :musical-shot :version 1 :duration {:bars 1}
                :envelope {:attack {:beats 1} :release {:beats 1/2}}
                :outputs {:dimmer 1 :color [1 0 0]}}
        model (control/new-show {:show-id "musical" :transport {:beat 8 :bpm 120 :playing? true
                                                                :meter {:beats-per-bar 3}}})
        published (command model :publish :registry/publish :kind :effects :definition effect)
        cued (command (:model published) :cue :cue/put :cue {:id :shot :effect-id :musical-shot})
        launched (command (:model cued) :launch :cue/launch :cue-id :shot :time-ms 0 :launch {:mode :one-shot})
        id (get-in launched [:ack :instance-id])
        advance (fn [model beat ms]
                  (engine/advance (assoc-in model [:transport :beat] beat)
                                  {:time-ms ms :fixtures [fixture]} compiler/compile-effect))
        attack (advance (:model launched) 8.5 250)
        tail (advance (-> (:model attack) (assoc-in [:transport :bpm] 240)
                          (assoc-in [:transport :meter :beats-per-bar] 7)) 11.25 1000)
        paused (advance (assoc-in (:model tail) [:transport :playing?] false) 11.25 90000)
        end (advance (assoc-in (:model paused) [:transport :playing?] true) 11.5 90125)
        dimmer #(-> (frame/resolve-frame [fixture] (:assignments %)) :values :a :dimmer)]
    (is (= :accepted (get-in launched [:ack :status])))
    (is (= 3 (get-in launched [:model :instances id :duration-timer :duration :value])))
    (is (= 0.5 (dimmer attack)))
    (is (= :releasing (get-in tail [:model :instances id :phase])))
    (is (= 0.5 (dimmer tail) (dimmer paused)))
    (is (empty? (get-in end [:model :instances])))
    (is (empty? (:assignments end)))))

(deftest real-control-compiler-runtime-and-output-work-together
  (let [effect (assoc (first (compiler/demo-definitions)) :envelope {:attack-ms 100 :release-ms 200})
        published (command (control/new-show {:show-id "authored"}) :publish :registry/publish
                           :kind :effects :definition effect)
        cued (command (:model published) :cue :cue/put :cue {:id :red :effect-id :solid-red})
        launched (command (:model cued) :launch :cue/launch :cue-id :red :time-ms 0
                          :launch {:mode :timed :duration-ms 200 :params {:level 0.8}})
        id (get-in launched [:ack :instance-id])
        first-frame (engine/advance (:model launched) {:time-ms 100 :fixtures [fixture]} compiler/compile-effect)
        tail (engine/advance (:model first-frame) {:time-ms 300 :fixtures [fixture]} compiler/compile-effect)
        resolved (frame/resolve-frame [fixture] (:assignments tail))
        finished (engine/advance (:model tail) {:time-ms 400 :fixtures [fixture]} compiler/compile-effect)]
    (doseq [result [published cued launched]] (is (= :accepted (get-in result [:ack :status]))))
    (is (= 200 (get-in launched [:model :instances id :release-duration-ms])))
    (is (= 0.4 (get-in resolved [:values :a :dimmer])))
    (is (= 102 (bit-and 255 (aget ^bytes (get-in resolved [:universes 1]) 3))))
    (is (empty? (get-in finished [:model :instances])))
    (with-open [pool (executor/create {:workers 2 :queue-size 4 :timeout-ms 1000})]
      (is (= first-frame (engine/advance (:model launched) {:time-ms 100 :fixtures [fixture]}
                                        compiler/compile-effect (executor/mapper pool)))))))
