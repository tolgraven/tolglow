(ns tolglow.studio.repl-host-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [ring.core.protocols :as protocols]
            [tolglow.studio :as studio]
            [tolglow.repl.sessions :as repl-manager])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream]))

(defn- request [runtime path payload]
  ((studio/handler runtime)
   {:request-method :post :uri path :headers {"host" "localhost"}
    :body (ByteArrayInputStream. (.getBytes (json/write-str payload) "UTF-8"))}))

(defn- evaluate! [runtime payload]
  (let [response (request runtime "/api/repl/eval" payload)
        output (ByteArrayOutputStream.)]
    (is (= 200 (:status response)))
    (protocols/write-body-to-stream (:body response) response output)
    (->> (str/split-lines (.toString output "UTF-8"))
         (filter #(str/starts-with? % "data: "))
         (mapv #(json/read-str (subs % 6) :key-fn keyword)))))

(deftest repl-streams-real-runtime-evaluation-with-stable-identities
  (let [runtime (studio/create-runtime)]
    (try
      (let [events (evaluate! runtime {:source "(println \"hello λ\") (:id runtime)"
                                      :request-id "browser-1" :session-id "main"
                                      :runtime-id (:id runtime) :expected-epoch 0})]
        (is (= "queued" (:type (first events))))
        (is (= "done" (:type (last events))))
        (is (= "completed" (:status (last events))))
        (is (every? #(= "browser-1" (:request-id %)) events))
        (is (str/includes? (apply str (map :text events)) "hello λ"))
        (is (= (pr-str (:id runtime)) (:value (last (filter #(= "result" (:type %)) events)))))
        (is (not-any? #(= "chat/done" (:type %)) events)))
      (let [snapshot (studio/repl-action! runtime :snapshot {:session-id "main"})]
        (is (= (:id runtime) (:runtime-id snapshot)))
        (is (= 1 (count (:history snapshot)))))
      (is (= 400 (:status (request runtime "/api/repl/eval"
                                  {:source "(throw (Exception. \"must not run\"))"
                                   :expected-epoch 99 :runtime-id (:id runtime)}))))
      (is (= 1 (count (:history (repl-manager/snapshot (studio/repls runtime) "main")))))
      (finally (studio/stop! runtime)))))

(deftest show-files-restore-repl-text-without-replaying-code
  (let [runtime (studio/create-runtime)]
    (try
      (evaluate! runtime {:source "(def saved-value 42) saved-value" :request-id "saved"})
      (studio/repl-action! runtime :scratch {:source "(+ saved-value 1)" :name "scratch.clj"})
      (let [saved (studio/export-show runtime)
            before (get-in saved [:model :integrations :repl])
            receipt (studio/submit! runtime :import {:source (pr-str saved)})]
        (studio/tick! runtime 0)
        (is (= :accepted (:status @receipt)))
        (let [restored (studio/repl-action! runtime :snapshot {})]
          (is (= :not-started (:status restored)))
          (is (= "(+ saved-value 1)" (get-in restored [:scratch "scratch.clj"])))
          (is (= (get-in before [:sessions "main" :history]) (:history restored))))
        (let [events (evaluate! runtime {:source "(resolve 'saved-value)" :request-id "after-restore"})]
          (is (= "nil" (:result (last events))) "Saved source was not evaluated on restore")))
      (finally (studio/stop! runtime)))))

(deftest detaching-a-popup-does-not-interrupt-its-owned-evaluation
  (let [gate (promise) started (promise) done (promise) observed (atom [])
        runtime (studio/create-runtime {:repl-options {:session-options {:bindings {'gate gate}}}})]
    (try
      (let [handle (studio/repl-eval! runtime {:source "@gate 42" :request-id "detach"}
                                     #(do (swap! observed conj %) (when (= :started (:type %)) (deliver started true)))
                                     #(deliver done %))]
        (is (= true (deref started 2000 false)))
        ((:cancel! handle))
        (deliver gate :continue)
        (is (= :completed (:status (deref done 2000 {})))))
      (is (not-any? #(= :result (:type %)) @observed))
      (is (= "42" (:result (last (:history (studio/repl-action! runtime :snapshot {}))))))
      (finally (deliver gate :continue) (studio/stop! runtime)))))
