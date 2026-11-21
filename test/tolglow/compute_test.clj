(ns tolglow.compute-test
  (:require [afterglow.render.gpu.session :as gpu]
            [clojure.test :refer [deftest is]]
            [tolglow.compute :as compute]
            [tolglow.show :as show]))

(deftest unavailable-gpu-starts-a-working-cpu-show
  (with-redefs [gpu/open! (fn [_] (throw (UnsatisfiedLinkError. "Native libraries unavailable")))]
    (let [show (show/create-runtime {:compute-backend :native-gpu})]
      (try
        (is (= {:requested :native-gpu :active :cpu :gpu-budget-ms 12
                :native-dispatches 0 :cpu-fallbacks 0 :deadline-fallbacks 0
                :last-fallback {:type "UnsatisfiedLinkError" :stage :open :message "Native libraries unavailable"}}
               (get-in (show/snapshot show) [:status :compute])))
        (let [reply (show/submit! show :command {:op :cue/launch :cue-id :ember})]
          (show/tick! show 0.0)
          (is (= :accepted (:status @reply)))
          (show/tick! show 100.0)
          (is (pos? (first (get-in (show/snapshot show) [:output :values :head-1 :emitted-color])))))
        (finally (show/stop! show))))))

(deftest only-an-explicit-valid-gpu-option-acquires-a-session
  (with-redefs [gpu/open! (fn [_] (throw (AssertionError. "Unexpected GPU acquisition")))]
    (is (= {:requested :cpu :active :cpu} (compute/status (compute/open! {}))))
    (doseq [options [{:compute-backend :unknown}
                     {:movement-backend :unknown}
                     {:compute-backend :native-gpu :batch? false}
                     {:compute-backend :native-gpu :gpu-budget-ms 0}
                     {:compute-backend :native-gpu :gpu-options {:queue-capacity -2}}]]
      (is (thrown? clojure.lang.ExceptionInfo (compute/open! options))))))
