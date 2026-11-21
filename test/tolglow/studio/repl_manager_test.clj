(ns tolglow.studio.repl-manager-test
  (:require [clojure.test :refer [deftest is]]
            [afterglow.control.model :as model]
            [tolglow.studio.connections :as connections]
            [tolglow.studio.repl :as repl]
            [tolglow.studio.repl-manager :as manager])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn result [receipt] (deref (:completion receipt) 3000 ::timeout))
(defn entered? [^CountDownLatch latch] (.await latch 2 TimeUnit/SECONDS))

(deftest manager-is-inert-and-owns-a-bounded-set-of-distinct-live-sessions
  (let [calls (atom 0) runtime (atom 0)
        owner (manager/create-manager {:max-sessions 2 :runtime-supplier #(do (swap! calls inc) runtime)})]
    (try
      (is (= 0 @calls))
      (is (= :not-started (:status (manager/snapshot owner "main"))))
      (manager/set-scratch! owner "main" "scratch.clj" "(swap! runtime inc)")
      (is (= 0 @calls))
      (is (= "1" (:result (result (manager/submit! owner "main" "(swap! runtime inc)")))))
      (manager/open-session! owner "other")
      (is (= 2 @calls))
      (is (not= (:namespace (manager/snapshot owner "main")) (:namespace (manager/snapshot owner "other"))))
      (is (thrown? clojure.lang.ExceptionInfo (manager/open-session! owner "third")))
      (is (= 2 @calls))
      (let [session (manager/session! owner "main")]
        (is (:stopped? (manager/close! owner)))
        (is (thrown? clojure.lang.ExceptionInfo (repl/submit! session "(+ 1 2)"))))
      (is (thrown? clojure.lang.ExceptionInfo (manager/submit! owner "main" "(+ 1 2)")))
      (finally (manager/close! owner)))))

(deftest saved-text-restores-without-opening-or-evaluating-and-live-restore-is-rejected
  (let [runtime (atom 0) calls (atom 0)
        a (manager/create-manager {:runtime-supplier #(do (swap! calls inc) runtime)})]
    (try
      (result (manager/submit! a "main" "(swap! runtime inc)"))
      (manager/set-scratch! a "main" "scratch.clj" "(swap! runtime + 100)")
      (let [saved (manager/export-state a) b (manager/create-manager {:state saved :runtime-supplier #(do (swap! calls inc) runtime)})]
        (try
          (is (= saved (manager/export-state b)))
          (is (= 1 @calls)) (is (= 1 @runtime))
          (is (= :not-started (:status (manager/snapshot b "main"))))
          (manager/session! b "main")
          (is (= 2 @calls)) (is (= 1 @runtime))
          (is (thrown? clojure.lang.ExceptionInfo (manager/restore! b saved)))
          (is (thrown? clojure.lang.ExceptionInfo (manager/validate-state (assoc-in saved [:sessions "main" :history 0 :result] (Object.)))))
          (finally (manager/close! b))))
      (finally (manager/close! a)))))

(deftest concurrent-open-requests-share-one-owned-namespace
  (let [entered (CountDownLatch. 1) release (CountDownLatch. 1) calls (atom 0)
        owner (manager/create-manager {:runtime-supplier #(do (swap! calls inc) (.countDown entered) (.await release) nil)})
        a (future (manager/session! owner "main"))]
    (try
      (is (entered? entered))
      (let [b (future (manager/session! owner "main"))]
        (.countDown release)
        (is (identical? (deref a 3000 ::timeout) (deref b 3000 ::timeout))))
      (is (= 1 @calls))
      (finally (.countDown release) (manager/close! owner) (future-cancel a)))))

(deftest retiring-during-lazy-open-cannot-publish-a-session-to-closed-owner
  (let [entered (CountDownLatch. 1) release (CountDownLatch. 1)
        owner (manager/create-manager {:runtime-supplier #(do (.countDown entered) (.await release) nil)})
        opening (future (try (manager/session! owner "main") :unexpected
                            (catch clojure.lang.ExceptionInfo e (:repl/error (ex-data e)))))]
    (try
      (is (entered? entered))
      (is (false? (:stopped? (manager/close! owner))))
      (.countDown release)
      (is (= :closed (deref opening 3000 ::timeout)))
      (is (:stopped? (manager/close! owner)))
      (finally (.countDown release) (future-cancel opening) (manager/close! owner)))))

(deftest connection-save-restore-preflights-repl-text-and-retires-old-owner
  (let [runtime (atom 0) lookups (atom 0) show (model/new-show {:show-id :repl-test})
        conn (connections/create show {:repl-options {:runtime-supplier #(do (swap! lookups inc) runtime)}})
        old (connections/repls conn)]
    (try
      (is (= 0 @lookups))
      (result (manager/submit! old "main" "(swap! runtime inc)"))
      (manager/set-scratch! old "main" "scratch.clj" "(swap! runtime + 100)")
      (let [saved (connections/export-state conn)
            next-model (merge (assoc show :epoch 1) saved)
            invalid (assoc-in next-model [:integrations :repl :sessions "main" :history 0 :status] :invented)]
        (is (thrown? clojure.lang.ExceptionInfo (connections/prepare-restore conn invalid)))
        (is (identical? old (connections/repls conn)))
        (let [candidate (connections/prepare-restore conn next-model)]
          (is (= 1 @lookups))
          (let [retired (connections/commit-restore! conn candidate)
                new (connections/repls conn)]
            (is (not (identical? old new)))
            (is (= :not-started (:status (manager/snapshot new "main"))))
            (is (= 1 @runtime))
            (is (= (get-in saved [:integrations :repl]) (manager/export-state new)))
            (is (= [] (:cleanup-errors (connections/retire! retired))))
            (is (thrown? clojure.lang.ExceptionInfo (manager/submit! old "main" ":stale"))))))
      (finally (connections/close! conn)))))
