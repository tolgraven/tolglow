(ns tolglow.native.repl-test
  (:require [clojure.test :refer [deftest is]]
            [sci.core :as sci]
            [tolglow.native.repl :as native]
            [tolglow.native.show-source :as source]
            [tolglow.show :as show]
            [tolglow.repl :as repl]))

(defn- execute [session source]
  (deref (:completion (repl/submit! session source)) 5000 ::timeout))

(deftest interactive-interpreted-sessions-share-the-existing-repl-contract
  (let [events (atom [])
        open #(repl/open! {:session-id % :runtime {:name "Stage"} :bindings {'knob 3}
                           :on-event (partial swap! events conj)
                           :evaluator-factory (partial native/evaluator (sci/init {}))})
        first-show (open "first") second-show (open "second")]
    (try
      (is (= "9" (:result (execute first-show "(defn scale [v] (* knob v))\n(println (:name runtime)) (scale 3)"))))
      (is (= "Stage\n" (apply str (map :text (filter #(= :out (:type %)) @events)))))
      (is (= "11" (:result (execute first-show "(+ *1 2)"))))
      (is (= :error (:status (execute second-show "(scale 22)"))))
      (is (= :error (:status (execute first-show "(throw (ex-info \"bad cue\" {}))"))))
      (is (= "true" (:result (execute first-show "(some? *e)"))))
      (is (= "7" (:result (execute first-show "(ns show.scratch) (defn gain [x] (+ x 3)) (gain 4)"))))
      (is (= "show.scratch" (:namespace (repl/snapshot first-show))))
      (is (= "gain" (:candidate (first (repl/complete first-show "gai")))))
      (is (= "([x])" (:arglists (repl/lookup first-show "gain"))))
      (execute first-show "(let [x 5 y 6] (tolglow.repl/capture-locals!))")
      (let [debug (:debug-namespace (repl/snapshot first-show))]
        (is (= "11" (:result (execute first-show (str "(+ " debug "/x " debug "/y)"))))))
      (let [saved (repl/export-state first-show)]
        (repl/restore-history! second-show saved)
        (is (= :error (:status (execute second-show "(show.scratch/gain 1)")))))
      (finally (repl/close! first-show) (repl/close! second-show)))))

(deftest show-snapshot-follows-commands-across-repl-namespaces
  (let [runtime (show/create-runtime {:workspace-options nil})
        session (repl/open! {:session-id "on-device" :runtime runtime
                             :evaluator-factory (source/default-evaluator)})]
    (try
      (is (= ":queued"
             (:result (execute session "(do (tolglow.show/submit! runtime {:op :cue/launch :cue-id :ember}) :queued)"))))
      (show/tick! runtime)
      (let [result (execute session "(ns show.lighting)
                                     (mapv :cue-id (:instances (tolglow.show/snapshot tolglow.repl/*runtime*)))")]
        (is (= "[:ember]" (:result result))))
      (finally (repl/close! session) (show/stop! runtime)))))
