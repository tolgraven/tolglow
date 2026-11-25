(ns tolglow.studio.application-agents-test
  (:require [cljs.test :refer-macros [deftest is]]
            [re-frame.db :as db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [tolglow.studio.application-agents :as agents]
            [tolglow.studio.workspace :as workspace]))

(def base {:show {:runtime-id "server-a" :epoch 4}})

(defn transition [state event]
  (reset! db/app-db state)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %))
                                               (registrar/get-handler :event (first event))))))

(defn respond [sent value]
  (transition (:db sent) (conj (get-in sent [:studio/http :on-success]) value)))

(deftest application-task-start-is-explicit-and-always-a-new-task
  (let [opened (transition base [:application-agents/open])
        request (transition (:db opened) [:application-agents/list])
        form {:provider "codex" :repo "afterglow" :policy "after-turn" :prompt "Improve the color picker."}
        sent (transition (assoc-in base [:application-agents :form] form) [:application-agents/start])]
    (is (= "/api/application-agents" (get-in request [:studio/http :path])))
    (is (nil? (get-in request [:studio/http :body])))
    (is (= "/api/application-agents/start" (get-in sent [:studio/http :path])))
    (is (= (assoc form :runtime-id "server-a") (get-in sent [:studio/http :body])))
    (is (nil? (get-in sent [:studio/http :body :workspace-id]))))
  (doseq [form [{:provider "codex" :repo "tolglow" :policy "preview" :prompt "Change something"}
               {:provider "codex" :repo "tolglow" :policy "manual" :prompt "  "}]]
    (is (thrown? js/Error (agents/start-payload form)))))

(deftest credentials-stay-in-the-transient-http-effect
  (let [sent (transition base [:application-agents/configure {:provider "codex" :api-key "test-only-secret" :model "test-model"}])
        accepted (respond sent {:providers [{:id "codex" :configured? true}] :configuration-persisted? true})]
    (is (= "test-only-secret" (get-in sent [:studio/http :body :api-key])))
    (is (not (.includes (pr-str (:db sent)) "test-only-secret")))
    (is (not (.includes (pr-str (:db accepted)) "test-only-secret")))
    (is (= [:application-agents/list] (:dispatch accepted)))))

(deftest event-stream-updates-items-in-place-and-bounds-visible-history
  (let [selected (assoc agents/defaults :selected "task-a")
        first (agents/consume-events selected {:task-id "task-a" :cursor 1 :status "running"
                                               :events [{:type "agent/item" :item-id "message-one" :item-type "agent_message" :text "First"}]})
        updated (agents/consume-events first {:task-id "task-a" :cursor 2 :status "running"
                                             :events [{:type "agent/item" :item-id "message-one" :item-type "agent_message" :text "First, then more."}]})
        many (agents/consume-events updated {:cursor 300 :status "running"
                                             :events (mapv #(hash-map :type "agent/item" :item-id (str %) :text "message") (range 250))})]
    (is (= ["message-one"] (:order updated)))
    (is (= "First, then more." (get-in updated [:items "message-one" :text])))
    (is (= 100 (count (:items many))))
    (is (= 100 (count (:order many))))
    (is (= 300 (:cursor many)))))

(deftest a-final-task-drains-retained-pages-and-closing-drops-in-flight-results
  (let [selected (:db (transition (assoc base :application-agents (assoc agents/defaults :open? true))
                                  [:application-agents/select "task-a"]))
        page (fn [cursor next available] [:application-agents/progress (agents/token selected) "task-a" cursor
                                          {:value {:task-id "task-a" :status "completed" :events []
                                                   :cursor next :available-cursor available}}])
        partial (transition selected (page 0 2 3))
        final (transition (:db partial) (page 2 3 3))
        duplicate (transition (:db final) (page 2 3 3))
        closed (transition selected [:application-agents/close])
        stale (transition (:db closed) (page 0 2 3))]
    (is (false? (get-in partial [:db :application-agents :stream-complete?])))
    (is (true? (get-in final [:db :application-agents :stream-complete?])))
    (is (= [:application-agents/status "task-a"] (:dispatch final)))
    (is (nil? (:db duplicate)))
    (is (= #{:db} (set (keys closed))))
    (is (nil? (:db stale)))))

(deftest check-results-arrive-before-task-completion-and-only-explicit-ready-candidates-are-described-as-ready
  (let [selected (assoc agents/defaults :selected "task-a" :tasks [{:id "task-a" :status "checking"}])
        progress (agents/consume-events selected {:task-id "task-a" :cursor 4 :status "checking"
                                                  :events [{:type "workspace/check" :profile "unit" :result {:status "passed" :exit-code 0}}
                                                           {:type "workspace/check" :profile "browser" :result {:status "failed" :exit-code 1}}]})
        summary (agents/check-summary (agents/task progress))]
    (is (= [{:profile "browser" :status "failed" :exit-code 1} {:profile "unit" :status "passed" :exit-code 0}] (:checks summary)))
    (is (false? (:candidate-ready? summary)))
    (is (false? (:candidate-ready? (agents/check-summary {:status "completed" :checks {:unit {:status "passed"}}})))))
  (is (:candidate-ready? (agents/check-summary {:candidate {:status "ready"}}))))

(deftest task-identity-is-process-scoped-and-review-keeps-unsaved-file-drafts
  (let [a (agents/token base) same-process (assoc-in base [:show :epoch] 99)
        replacement (assoc-in base [:show :runtime-id] "server-b")
        file-key ["old" "tolglow" "src/example.clj"]
        with-draft (assoc-in base [:ui :workspace :files file-key] {:source "unsaved" :baseline "old"})
        reviewed (transition with-draft [:workspace/review-task "new-workspace"])]
    (is (= a (agents/token same-process)))
    (is (not= a (agents/token replacement)))
    (is (= "new-workspace" (get-in reviewed [:db :ui :workspace :selected-id])))
    (is (= :diff (get-in reviewed [:db :ui :workspace :pane])))
    (is (= "unsaved" (get-in reviewed [:db :ui :workspace :files file-key :source])))))

(deftest a-starting-task-belongs-to-the-runtime-while-its-pane-is-closed
  (let [opened (:db (transition base [:application-agents/open]))
        draft (:db (transition opened [:application-agents/edit :prompt "Review the fixtures."]))
        start (transition draft [:application-agents/start])
        ack (conj (get-in start [:studio/http :on-success]) {:id "task-a" :status "running"})
        close (:db (transition (:db start) [:application-agents/close]))
        reopened (:db (transition close [:application-agents/open]))
        edited (:db (transition reopened [:application-agents/edit :prompt "A second change."]))
        accepted (transition close ack)
        selected (transition (:db accepted) [:application-agents/select "task-a"])
        replaced (:db (transition (assoc-in edited [:show :runtime-id] "server-b") [:application-agents/runtime-change]))]
    (is (nil? (:studio/http (transition reopened [:application-agents/start]))))
    (is (= "" (get-in accepted [:db :application-agents :form :prompt])))
    (is (= "A second change." (get-in (transition edited ack) [:db :application-agents :form :prompt])))
    (is (= "task-a" (get-in selected [:db :application-agents :selected])))
    (is (nil? (:fx selected)))
    (is (nil? (:db (transition replaced ack))))
    (is (nil? (:db (transition (:db accepted) ack))))))
