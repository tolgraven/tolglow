(ns tolglow.knob-candidates.golden-test
  "Recipes made by generator versions 1–3 must replay exactly as they were
  made. v3_golden.edn holds 1080 candidates that master's generator (before
  version 4, #667) drew for one show with every parameter type, including a
  number whose :step is wider than its range, which version 4 leaves out."
  (:require [afterglow.control.model :as control]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [tolglow.knob-candidates.generate :as generate]))

(def ^:private golden
  (edn/read-string (slurp (io/resource "tolglow/knob_candidates/v3_golden.edn"))))

(defn- model []
  (let [effect (:effect golden)
        {:keys [model ack]}
        (control/apply-command
         (control/new-show {:show-id :golden :transport {:playing? true :beat 0.0 :bpm 120.0}
                            :registry {:effects {:golden effect} :cues {:golden {:id :golden :effect-id :golden :version 1}}}
                            :rig {:fixtures [{:id :head :universe 1 :address 1 :channels {:dimmer 1}}]}})
         {:op :cue/launch :cue-id :golden :launch {:mode :persistent} :command-id "launch" :show-id :golden
          :epoch 0 :source {:adapter :test} :time-ms 0})]
    (assert (= :accepted (:status ack)) (pr-str ack))
    model))

(deftest versions-1-to-3-replay-the-golden-candidates
  (let [eligibility (generate/eligibility (model))]
    (is (= 1080 (count (:cases golden))))
    (is (some (fn [[_ c]] (some #(= :gain (get-in % [:destination :param])) (:links c))) (:cases golden))
        "the fixture exercises the number whose step is wider than its range")
    (doseq [[[version seed size character :as key] expected] (:cases golden)]
      (is (= expected (select-keys (generate/generate eligibility nil (cond-> {:seed seed :generator version}
                                                                         size (assoc :size size)
                                                                         character (assoc :character character)))
                                   [:status :character :links :message]))
          (pr-str key)))))
