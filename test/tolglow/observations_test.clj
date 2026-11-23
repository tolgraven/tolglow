(ns tolglow.observations-test
  (:require [afterglow.reactive :as rx]
            [clojure.test :refer [deftest is]]
            [tolglow.observations :as obs]))

(def ^:private scene {:show-id "night" :epoch 1})
(def ^:private source :simulation/position)
(def ^:private query [:browser/observation source])
(def ^:private demand [obs/demand-query :rig])

(deftest latest-completed-input-has-a-consumer-and-one-visible-producer
  (let [manager (obs/create {source {:owner :rig :sampler :local/vector :interval-ms 25
                                     :valid? #(and (vector? %) (= 3 (count %)) (every? number? %))}})
        system (obs/register! (rx/create-system scene) manager)
        changes (atom [])
        upstream (obs/scope-query demand)
        alternate (obs/scope-query demand)
        model (atom scene)
        a (obs/provider-lease manager #(deref model) upstream)
        b (obs/provider-lease manager #(deref model) alternate)
        grant (fn [lease current] (reset! model current) (get ((:sample! lease) nil) source))
        report (fn [g seq ms v] (merge (select-keys g [:show :token]) {:source source :sequence seq
                                              :frame-number 0 :sampled-ms (- ms 1) :completed-ms ms :value v}))]
    (try
      (is (nil? (grant a scene)))
      (is (thrown? clojure.lang.ExceptionInfo (obs/scope-query (conj demand "chosen-by-client"))))
      (is (= "tolglow.observations/latest-observation"
             (get-in (rx/export-registry system) [:registrations 0 :implementation :symbol])))
      (let [one (rx/acquire! system query #(swap! changes conj %))
            two (rx/acquire! system query identity)
            spec (grant a scene)
            accept #(obs/accept! manager scene upstream %)]
        (is (= [:local/vector 25] ((juxt :sampler :interval-ms) spec)))
        (is (nil? (grant b scene)))
        (is (nil? (obs/accept! manager scene alternate (report spec 1 3 [0 0 0]))))
        (is (true? (accept (report spec 1 3 [0 0 0]))))
        (rx/publish! system scene)
        (let [first-value (peek @changes)]
          (is (= [0 0 0] (:value first-value)))
          (is (integer? (:received-ns first-value)))
          (is (= 3 (:completed-ms first-value)))
          (is (true? (accept (report spec 2 9 [0 0 0]))))
          (is (nil? (accept (report spec 2 12 [1 1 1]))))
          (is (nil? (accept (report spec 3 5 [1 1 1]))))
          (is (nil? (accept (report spec 3 10 [:oops]))))
          (is (nil? (accept (assoc (report spec 3 10 [1 2 3]) :frame-number 9))))
          (rx/publish! system scene)
          (is (= [first-value] @changes)))
        ((:close! one))
        (is (= spec (grant a scene)))
        ((:close! two))
        (is (nil? (grant a scene)))
        (is (nil? (accept (report spec 4 14 [2 3 4]))))
        (let [reader (rx/acquire! system query identity)
              spec2 (grant a scene)
              next-scene (update scene :epoch inc)]
          (is (nil? (:value reader)))
          (is (not= (:token spec) (:token spec2)))
          (is (true? (accept (report spec2 1 15 [1 1 1]))))
          (rx/publish! system next-scene)
          (is (nil? @(:signal reader)) "Previous epoch cannot be consumed before a new producer result")
          (is (nil? (obs/accept! manager next-scene upstream (report spec2 2 16 [2 3 4]))))
          ((:close! a))
          (let [spec3 (grant b next-scene)]
            (is (= next-scene (:show spec3)))
            (is (true? (obs/accept! manager next-scene alternate (report spec3 1 2 [3 3 3])))
                "Failover starts a new monotonic clock domain"))
          ((:close! reader))))
      (finally ((:close! a)) ((:close! b)) (rx/close! system)))))
