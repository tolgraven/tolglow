(ns tolglow.repl-test
  (:refer-clojure :exclude [run!])
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [tolglow.repl :as repl])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn result [receipt] (deref (:completion receipt) 3000 ::timeout))

(defn run! [session source] (result (repl/submit! session source)))

(defn entered? [^CountDownLatch latch] (.await latch 2 TimeUnit/SECONDS))

(deftest real-evaluation-is-serial-and-namespaces-and-values-are-owned
  (let [runtime (atom 0) a (repl/open! {:session-id "a" :runtime runtime}) b (repl/open! {:session-id "b"})]
    (try
      (is (= :completed (:status (run! a "(def local-value 7) (swap! runtime inc) (+ local-value 3)"))))
      (is (= 1 @runtime))
      (is (= "11" (:result (run! a "(inc *1)"))))
      (is (= :error (:status (run! b "local-value"))))
      (is (not= (:namespace (repl/snapshot a)) (:namespace (repl/snapshot b))))
      (is (= :error (:status (run! a "(/ 1 0)"))))
      (is (= "true" (:result (run! a "(some? *e)"))))
      (finally (repl/close! a) (repl/close! b)))))

(deftest streaming-output-is-bounded-and-result-events-do-not-recurse-through-observers
  (let [events (atom []) session (repl/open! {:session-id "output" :max-output-chars 32
                                            :on-event #(swap! events conj %)})]
    (try
      (let [done (run! session "(print \"hello\") (binding [*out* *err*] (print \"problem\")) (print (apply str (repeat 100 \"x\"))) :end")]
        (is (= :completed (:status done)))
        (is (= [:queued :started] (mapv :type (take 2 @events))))
        (is (some #(= :err (:type %)) @events))
        (is (= 1 (count (filter #(= :output-truncated (:type %)) @events))))
        (is (<= (reduce + (map #(count (or (:text %) (:value %) "")) @events)) 32))
        (is (= :done (:type (last @events))))
        (is (apply < (map :sequence @events))))
      (finally (repl/close! session)))))

(deftest bounded-queue-rejects-extra-work-without-evaluating-it
  (let [entered (CountDownLatch. 1) release (CountDownLatch. 1) counter (atom 0)
        session (repl/open! {:session-id "queue" :queue-size 1
                             :bindings {'entered entered 'release release 'counter counter}})]
    (try
      (let [first (repl/submit! session "(.countDown entered) (.await release) (swap! counter inc)")]
        (is (entered? entered))
        (let [second (repl/submit! session "(swap! counter + 10)")
              third (repl/submit! session "(swap! counter + 100)")]
          (is (= {:status :rejected :reason :queue-full} third))
          (.countDown release)
          (is (= :completed (:status (result first))))
          (is (= "11" (:result (result second))))
          (is (= 11 @counter))))
      (finally (.countDown release) (repl/close! session)))))

(deftest interrupt-is-cooperative-and-the-worker-continues-after-sleep-interruption
  (let [entered (CountDownLatch. 1)
        session (repl/open! {:session-id "interrupt" :bindings {'entered entered}})]
    (try
      (let [job (repl/submit! session "(.countDown entered) (Thread/sleep 30000)")]
        (is (entered? entered))
        (is (= :idle-or-stale (:status (repl/interrupt! session "stale"))))
        (is (= :interrupt-requested (:status (repl/interrupt! session (:request-id job)))))
        (is (= :interrupted (:status (result job))))
        (is (= "42" (:result (run! session "(* 6 7)")))))
      (finally (repl/close! session)))))

(deftest code-that-ignores-interruption-is-reported-as-closing-until-it-returns
  (let [entered (CountDownLatch. 1) release (CountDownLatch. 1)
        session (repl/open! {:session-id "uncooperative" :close-timeout-ms 20
                             :bindings {'entered entered 'release release}})]
    (try
      (let [job (repl/submit! session "(.countDown entered) (while (pos? (.getCount release)) (try (Thread/sleep 5) (catch InterruptedException _ nil))) :finished")]
        (is (entered? entered))
        (let [queued (repl/submit! session ":never-runs")]
          (is (= :closing (:status (repl/close! session))))
          (is (= :cancelled (:status (result queued))))
          (is (thrown? clojure.lang.ExceptionInfo (repl/submit! session ":new")))
          (.countDown release)
          (is (= :completed (:status (result job))))))
      (finally (.countDown release) (repl/close! session)))))

(deftest lexical-local-capture-runs-only-when-the-form-executes
  (let [session (repl/open! {:session-id "locals"}) debug-name (:debug-namespace (repl/snapshot session))]
    (try
      (is (= :completed (:status (run! session "(let [x 7] (when false (tolglow.repl/capture-locals!)))"))))
      (is (nil? (ns-resolve (symbol debug-name) 'x)))
      (is (= :completed (:status (run! session "((fn [x] (let [y (+ x 2)] (tolglow.repl/capture-locals!))) 7)"))))
      (is (= 7 (var-get (ns-resolve (symbol debug-name) 'x))))
      (is (= 9 (var-get (ns-resolve (symbol debug-name) 'y))))
      (is (not (contains? (repl/export-state session) :values)))
      (finally (repl/close! session)))
    (is (nil? (find-ns (symbol debug-name))))))

(deftest history-and-scratch-round-trip-without-evaluation-or-live-objects
  (let [runtime (atom 0) a (repl/open! {:session-id "history-a" :runtime runtime})
        b (repl/open! {:session-id "history-b" :runtime runtime})]
    (try
      (run! a "(swap! runtime inc)")
      (repl/set-scratch! a "scratch.clj" "(swap! runtime + 100)")
      (let [saved (edn/read-string (pr-str (repl/export-state a)))]
        (is (= [] (repl/validate-state saved)))
        (is (= saved (repl/restore-history! b saved)))
        (is (= 1 @runtime))
        (is (seq (repl/validate-state (assoc-in saved [:history 0 :request-id] (atom 1)))))
        (is (nil? (:values (repl/snapshot b))))
        (is (not= (:request-id (run! a "1")) (:request-id (run! b "1")))))
      (finally (repl/close! a) (repl/close! b)))))

(deftest completion-and-docs-inspect-loaded-var-metadata-without-evaluating
  (let [session (repl/open! {:session-id "docs"})]
    (try
      (run! session "(require '[clojure.string :as s])")
      (let [before (count (:history (repl/snapshot session)))]
        (is (some #(= "s/upper-case" (:candidate %)) (repl/complete session "s/upp")))
        (is (= "map" (:name (repl/lookup session "clojure.core/map"))))
        (is (string? (:doc (repl/lookup session "clojure.core/map"))))
        (is (= before (count (:history (repl/snapshot session))))))
      (finally (repl/close! session)))
    (is (thrown? clojure.lang.ExceptionInfo (repl/complete session "map")))))

(deftest reader-evaluation-is-disabled-and-form-count-is-bounded
  (let [counter (atom 0) session (repl/open! {:session-id "reader" :runtime counter :max-forms 2})]
    (try
      (is (= :error (:status (run! session "#=(swap! runtime inc)"))))
      (is (= 0 @counter))
      (is (= :error (:status (run! session "(swap! runtime inc) (swap! runtime inc) (swap! runtime inc)"))))
      (is (= 2 @counter))
      (finally (repl/close! session)))))

(deftest closing-from-inside-the-owning-repl-does-not-wait-on-itself
  (let [session (repl/open! {:session-id "self-close"})]
    (try
      (is (= :completed (:status (run! session "(tolglow.repl/close! tolglow.repl/*session*)"))))
      (is (.await ^CountDownLatch (:stopped session) 2 TimeUnit/SECONDS))
      (is (= :closed (:status (repl/snapshot session))))
      (finally (repl/close! session)))))

(deftest client-request-ids-are-correlated-and-duplicates-never-evaluate
  (let [counter (atom 0) events (atom [])
        session (repl/open! {:session-id "client-ids" :runtime counter :on-event #(swap! events conj %)})]
    (try
      (let [done (result (repl/submit! session "(swap! runtime inc)" {:request-id "client-request"}))]
        (is (= "client-request" (:request-id done)))
        (is (every? #(= "client-request" (:request-id %)) @events))
        (is (= :duplicate-request-id (:reason (repl/submit! session "(swap! runtime inc)" {:request-id "client-request"})))))
      (is (= 1 @counter))
      (is (thrown? clojure.lang.ExceptionInfo (repl/submit! session "1" {:request-id ""})))
      (finally (repl/close! session)))))

(deftest queued-cancellation-removes-exact-work-without-interrupting-the-active-request
  (let [entered (CountDownLatch. 1) release (CountDownLatch. 1) counter (atom 0)
        session (repl/open! {:session-id "queued-cancel" :runtime counter :bindings {'entered entered 'release release}})]
    (try
      (let [active (repl/submit! session "(.countDown entered) (.await release) :active" {:request-id "active"})]
        (is (entered? entered))
        (let [queued (repl/submit! session "(swap! runtime inc)" {:request-id "queued"})]
          (is (= :duplicate-request-id (:reason (repl/submit! session "1" {:request-id "active"}))))
          (is (= :duplicate-request-id (:reason (repl/submit! session "1" {:request-id "queued"}))))
          (is (= :cancelled (:status (repl/cancel-request! session "queued"))))
          (is (= :cancelled (:status (result queued))))
          (.countDown release)
          (is (= :completed (:status (result active))))
          (is (= 0 @counter))))
      (finally (.countDown release) (repl/close! session)))))

(deftest cancellation-before-admission-callback-finishes-prevents-evaluation
  (let [entered (CountDownLatch. 1) release (CountDownLatch. 1) counter (atom 0)
        session (repl/open! {:session-id "admission-cancel" :runtime counter})
        submitting (future (repl/submit! session "(swap! runtime inc)"
                             {:request-id "pending-admission"
                              :on-event (fn [event] (when (= :queued (:type event)) (.countDown entered) (.await release)))}))]
    (try
      (is (entered? entered))
      (is (#{:cancelled :cancel-requested} (:status (repl/cancel-request! session "pending-admission"))))
      (.countDown release)
      (is (= :cancelled (:status (result (deref submitting 3000 ::timeout)))))
      (is (= 0 @counter))
      (finally (.countDown release) (future-cancel submitting) (repl/close! session)))))
