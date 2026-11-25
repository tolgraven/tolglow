(ns tolglow.studio.repl-test
  (:require [cljs.test :refer-macros [deftest is]]
            [re-frame.db :as rf-db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [tolglow.studio.repl :as repl]
            [tolglow.studio.repl-code :as code]
            [tolglow.studio.events]))
(defn transition [db event]
  (reset! rf-db/app-db db)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %)) (registrar/get-handler :event (first event))))))

(def base {:repl (assoc repl/defaults :snapshot {:runtime-id "one" :epoch 2} :open? true)})

(deftest opening-and-restoring-a-console-never-evaluates
  (let [open (transition {} [:repl/open true])
        poll (transition (:db open) [:repl/poll])
        loaded (transition (:db poll) [:repl/loaded 0 0 {:runtime-id "one" :epoch 2 :history [{:request-id "past" :source "(+ 1 2)" :result "3"}] :scratch {:scratch.clj "(println :saved)"}}])]
    (is (nil? (:repl/stream open)))
    (is (= "/api/repl?session-id=main" (get-in poll [:studio/http :path])))
    (is (= "(println :saved)" (get-in loaded [:db :repl :source])))
    (is (= "3" (get-in loaded [:db :repl :entries 0 :events 0 :value])))
    (is (nil? (:repl/stream loaded)))
    (is (nil? (:dispatch loaded)))))

(deftest asynchronous-repl-events-cannot-cross-request-or-session-boundaries
  (let [db (assoc-in base [:repl :run] "new") token (repl/token db)
        old (transition db [:repl/event token "old" {:type "out" :request-id "old" :text "stale"}])
        wrong (transition db [:repl/event token "new" {:type "out" :request-id "other" :text "stale"}])
        closed (transition db [:repl/close])
        late (transition (:db closed) [:repl/event token "new" {:type "out" :request-id "new" :text "stale"}])]
    (is (nil? (:db old))) (is (nil? (:db wrong))) (is (nil? (:db late)))
    (is (:repl/abort closed))
    (is (nil? (:studio/http closed)))))

(deftest repl-requests-retain-server-identity-and-current-source
  (let [save (transition (assoc-in base [:repl :source] "(+ 2 3)") [:repl/save])
        interrupt (transition (assoc-in base [:repl :run] "running") [:repl/interrupt])]
    (is (= {:session-id "main" :name "scratch.clj" :source "(+ 2 3)" :runtime-id "one" :expected-epoch 2} (get-in save [:studio/http :body])))
    (is (= "running" (get-in interrupt [:studio/http :body :request-id])))
    (is (= "one" (get-in interrupt [:studio/http :body :runtime-id])))))

(deftest histories-keep-live-output-without-duplicating-stream-events
  (let [event {:request-id "x" :sequence 4 :type "out" :text "hello"}
        entries (repl/event-entry [] event)
        merged (repl/merge-history entries [{:request-id "x" :source "(println :hi)" :result "nil"}])]
    (is (= entries (repl/event-entry entries event)))
    (is (= "hello" (get-in merged [0 :events 0 :text])))
    (is (= "(println :hi)" (get-in merged [0 :source])))
    (is (= 100 (count (reduce repl/event-entry [] (for [n (range 120)] {:request-id (str n) :sequence n :type "result" :value "1"})))))))

(deftest balanced-form-selection-respects-clojure-text
  (is (= "(+ 2 3)" (code/form-at "(map (fn [x] (+ 2 3)) [1])" 15)))
  (is (= "(+ 1 2)" (code/form-at "(+ 1 2)" 7)))
  (is (= "[1 2]" (code/form-at "[1 2] ; ignored (" 3)))
  (is (= "(str \\( \\) x)" (code/form-at "(str \\( \\) x)" 2)))
  (is (= "\"a ) [ b\"" (code/form-at "(str \"a ) [ b\")" 7)))
  (is (= "'(:a :b)" (code/form-at "'(:a :b)" 3)))
  (is (thrown? js/Error (code/form-at "(inc [1)" 4)))
  (is (= {:start 1 :end 5 :prefix "ma" :symbol "mapv"} (code/symbol-at "(mapv inc xs)" 3))))

(deftest scratch-save-acknowledgement-does-not-clear-newer-edits
  (let [db (assoc-in base [:repl :source] "new")
        saved (transition db [:repl/saved (repl/token db) "old" {}])]
    (is (= "new" (get-in saved [:db :repl :source])))))

(deftest changing-sessions-keeps-each-unsaved-draft
  (let [initial (assoc base :repl (assoc (:repl base) :source "(def unsaved 1)" :dirty? true))
        other (:db (transition initial [:repl/session "other"]))
        back (:db (transition (assoc-in other [:repl :opening?] false) [:repl/session "main"]))]
    (is (= "(+ 1 2)" (get-in other [:repl :source])))
    (is (= "(def unsaved 1)" (get-in back [:repl :source])))
    (is (true? (get-in back [:repl :dirty?])))))

(deftest a-clicked-evaluation-keeps-its-source-while-later-typing-survives
  (let [db (assoc base :repl (assoc (:repl base) :source "newer draft" :dirty? true :edit-revision 2))
        result (transition db [:repl/read-result [(repl/token db) 1] :buffer {:source "(+ 1 2)" :selected "" :cursor 0}])
        saved (transition db [:repl/read-result [(repl/token db) 1] :save {:source "(+ 1 2)" :selected "" :cursor 0}])]
    (is (= "newer draft" (get-in result [:db :repl :source])))
    (is (= [:repl/evaluate "(+ 1 2)"] (:dispatch result)))
    (is (= [:repl/save "(+ 1 2)"] (:dispatch saved)))))

(deftest opening-a-named-session-is-an-explicit-guarded-operation
  (let [opened (transition base [:repl/session "scratch-session"])]
    (is (= "/api/repl/session" (get-in opened [:studio/http :path])))
    (is (= {:session-id "scratch-session" :runtime-id "one" :expected-epoch 2} (get-in opened [:studio/http :body])))
    (is (nil? (:repl/stream opened)))
    (is (:opening? (get-in opened [:db :repl])))))

(deftest nonterminal-stream-events-omit-empty-dispatch-effects
  (let [db (assoc-in base [:repl :run] "current")
        result (transition db [:repl/event (repl/token db) "current" {:type "out" :request-id "current" :sequence 1 :text "live"}])]
    (is (not (contains? result :dispatch)))
    (is (= "live" (get-in result [:db :repl :entries 0 :events 0 :text])))))

(deftest a-pre-evaluation-poll-cannot-replace-new-streamed-history
  (let [entries [{:request-id "latest" :source "(println :hi)" :events [{:type "out" :text ":hi"}]}]
        db (assoc base :repl (assoc (:repl base) :activity 2 :entries entries :polling? true))
        response (transition db [:repl/loaded 0 0 {:runtime-id "one" :epoch 2 :history []}])]
    (is (= entries (get-in response [:db :repl :entries])))
    (is (= [:repl/poll] (:dispatch response)))
    (is (false? (get-in response [:db :repl :polling?])))))
