(ns tolglow.perf.mixed-rig-test
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.perf.mixed-rig :as mixed]
            [tolglow.registry :as registry]))

(def ^:private small {:dimmers 6 :pars 6 :movers 4 :layers 8 :workers [2] :warmup 4 :samples 12})

(defn- reference
  "The per-frame parameter resolution compiled effects used before #387."
  [schemas defaults overrides]
  (try {:values (registry/resolve-params schemas (merge defaults overrides))}
       (catch clojure.lang.ExceptionInfo error {:message (ex-message error) :data (ex-data error)})))

(defn- candidate [schemas defaults overrides]
  (try {:values (registry/resolve-accepted-params schemas defaults overrides)}
       (catch clojure.lang.ExceptionInfo error {:message (ex-message error) :data (ex-data error)})))

(deftest a-mixed-rig-renders-every-frame-like-the-scalar-oracle
  (let [data (mixed/corpus small 40)
        calls (atom [])
        resolve registry/resolve-accepted-params
        runs (with-redefs [registry/resolve-accepted-params
                           (fn [schemas defaults overrides]
                             (swap! calls conj [schemas defaults overrides])
                             (resolve schemas defaults overrides))]
               ;; Each measured frame is checked against the serial scalar
               ;; runtime: model, intents, every packet byte and preview.
               (mixed/host-run data small))
        result (:result (first runs))]
    (testing "the rig mixes every fixture kind, many cues and modulation"
      (is (= {:pixels 40 :dimmers 6 :pars 6 :movers 4} (:counts data)))
      (is (<= 18 (count (:definitions data))))
      (is (= 8 (:modulation-bindings (first runs))))
      (is (zero? (:over-25ms result 0)) "a 62-fixture rig ticks well inside one 40 Hz period")
      (is (pos? (get-in result [:milliseconds :p50]))))
    (testing "recorded per-frame parameter resolution matches the earlier composition"
      (is (< 100 (count @calls)))
      (is (some (fn [[_ defaults overrides]] (not= defaults (merge defaults overrides))) @calls)
          "LFO overrides reached the compiled effects")
      (doseq [[schemas defaults overrides] (distinct @calls)]
        (let [expected (reference schemas defaults overrides)
              actual (candidate schemas defaults overrides)]
          (is (= expected actual))
          ;; Key order is observable to anything that iterates the map.
          (is (= (seq (:values expected)) (seq (:values actual)))))))))
