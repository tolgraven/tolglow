(ns tolglow.output.scheduler-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.show :as show]
            [tolglow.demo :as demo]
            [afterglow.output.scheduler :as scheduler]))

(defn- bytes-copy [universes]
  (into {} (map (fn [[u data]] [u (mapv #(bit-and (int %) 255) data)])) universes))
(deftest real-effect-worker-can-block-while-output-keeps-advancing
  (let [physical (atom 0) sent (atom []) blocked? (atom false)
        entered (promise) release (promise)
        base (show/create-runtime {:model (update-in (demo/model) [:registry :effects :ember] dissoc :envelope)
                                     :independent-output? true :physical-now-ms #(deref physical)
                                     :workspace-options nil
                                     :sink {:send! (fn [universes id] (swap! sent conj {:id id :bytes (bytes-copy universes)}))}})
        runtime (assoc base :compile (fn [definition]
                                      (let [step ((:compile base) definition)]
                                        (fn [context]
                                          (when @blocked? (deliver entered true) (deref release 2000 nil))
                                          (step context)))))]
    (try
      (show/submit! runtime :settings {:scope :output :values {:presentation {:mode :hold-fade}}})
      (show/submit! runtime {:op :cue/launch :cue-id :ember})
      (show/tick! runtime 0)
      (scheduler/publish! (:publisher runtime))
      (let [initial (last @sent) frame-number (get-in @(:state runtime) [:model :frame-number])]
        (reset! blocked? true) (reset! physical 25)
        (let [worker (future (show/tick! runtime 25))]
          (is (= true (deref entered 1000 :timeout)))
          (reset! physical 75)
          (scheduler/publish! (:publisher runtime))
          (is (not (realized? worker)))
          (is (= frame-number (get-in @(:state runtime) [:model :frame-number])))
          (is (get-in (scheduler/status (:publisher runtime)) [:last :fallback?]))
          (is (< (reduce + (mapcat val (:bytes (last @sent))))
                 (reduce + (mapcat val (:bytes initial)))))
          (reset! physical 100) (deliver release true)
          (is (not= :timeout (deref worker 2000 :timeout)))
          (scheduler/publish! (:publisher runtime))
          (is (= [25 nil] (get-in (scheduler/status (:publisher runtime)) [:last :source-times])))
          (is (= (inc frame-number) (get-in @(:state runtime) [:model :frame-number])))))
      (finally (deliver release true) (show/stop! runtime)))))

(deftest settings-validate-before-entering-the-output-clock-and-roundtrip
  (let [runtime (show/create-runtime {:workspace-options nil})]
    (try
      (let [bad (show/submit! runtime :settings {:scope :output :values {:presentation {:mode :no-such-mode}}})]
        (show/tick! runtime 0)
        (is (= :rejected (:status @bad)))
        (is (nil? (get-in @(:state runtime) [:model :settings :output :presentation]))))
      (let [good (show/submit! runtime :settings {:scope :output :values {:presentation {:mode :buffered-interpolation :latency-ms 25}}})]
        (show/tick! runtime 25)
        (is (= :accepted (:status @good)))
        (is (= {:mode :buffered-interpolation :latency-ms 25}
               (get-in (show/export-show runtime) [:model :settings :output :presentation]))))
      (finally (show/stop! runtime)))))
