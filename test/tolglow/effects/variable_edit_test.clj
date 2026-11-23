(ns tolglow.effects.variable-edit-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.demo :as demo]
            [tolglow.effects.variable-edit :as edit]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.transit :as wire])
  (:import [java.io ByteArrayInputStream]))

(defn- post! [runtime handler path payload]
  (let [request {:request-method :post :uri path
                 :headers {"host" "localhost" "accept" "application/transit+json" "content-type" "application/transit+json"}
                 :body (ByteArrayInputStream. (.getBytes (wire/encode payload) "UTF-8"))}
        response (future (handler request)) deadline (+ (System/nanoTime) 5000000000)]
    (try
      (loop []
        (cond
          (realized? response) (let [{:keys [status body]} @response] [status (wire/decode body)])
          (> (System/nanoTime) deadline) (throw (ex-info "Timed out waiting for the show owner" {:path path}))
          :else (do (show/tick! runtime 25.0) (Thread/sleep 5) (recur))))
      (finally (future-cancel response)))))

(deftest inventory-keeps-real-numeric-leaves-without-projecting-their-contents
  (let [vars {:rig {:haze 0.2 :target [1 2 3] "private" 42 :many (vec (range 200))}
              :enabled? true :text "a" :invalid ##Inf}]
    (is (= [{:path [:variables :rig :haze] :type :number}
            {:path [:variables :rig :target] :type :vec3}] (:sources (edit/sources vars "RIG"))))
    (is (= [{:path [:variables :rig :haze] :type :number}] (:sources (edit/sources vars "haze"))))
    (is (thrown? clojure.lang.ExceptionInfo (edit/sources vars [:rig])))))

(deftest disjoint-client-writes-survive-and-stale-same-variable-edits-are-rejected
  (let [runtime (show/create-runtime {:workspace-options nil}) handler (studio/handler runtime)
        first-path [:variables :room :x] second-path [:variables :room :y]
        original {:base 1.0 :stages [{:block :optics/through-water}]}
        updated (assoc original :base 0.55)
        command (fn [op path previous value]
                  (post! runtime handler "/api/variable-block"
                         (cond-> {:runtime-id (:id runtime) :expected-epoch 0 :op op :path path :expected previous}
                           value (assoc :binding value))))
        settings #(get-in @(:state runtime) [:model :settings :variable-blocks :bindings])]
    (try
      (is (= 200 (first (post! runtime handler "/api/variable-block/sources" {:query ""}))))
      (is (= :accepted (get-in (command :put first-path nil original) [1 :status])))
      (is (= :accepted (get-in (command :put second-path nil original) [1 :status])))
      (is (= :accepted (get-in (command :put first-path original updated) [1 :status])))
      (let [[status ack] (command :remove first-path original nil)]
        (is (= 422 status))
        (is (= :stale-binding (:reason ack))))
      (is (= {first-path updated second-path original} (settings)))
      (is (= 422 (first (command :put second-path original {:base 1.0 :stages [{:block :missing}]}))))
      (is (= original (get (settings) second-path)))
      (is (= 200 (first (command :remove first-path updated nil))))
      (is (= {second-path original} (settings)))
      (finally (show/stop! runtime)))))


(deftest variable-specific-material-defaults-survive-guarded-binding-edits
  (let [material {:name "Dusk transmission" :mode :multiply :inputs {:factor 0.25} :glsl "factor"}
        model (assoc-in (demo/model)
                        [:settings :variable-blocks :prefabs :venue/dusk] material)
        runtime (show/create-runtime {:model model :workspace-options nil})
        handler (studio/handler runtime)
        binding {:base 0.8 :stages [{:block :venue/dusk}]}
        edit {:op :put :path [:variables :wash] :expected nil :binding binding
              :runtime-id (:id runtime) :expected-epoch 0}]
    (try
      (is (= :accepted (get-in (post! runtime handler "/api/variable-block" edit) [1 :status])))
      (is (< (abs (- 0.2 (:wash (show/variables runtime)))) 1.0e-9))
      (is (= material (get-in @(:state runtime) [:model :settings :variable-blocks :prefabs :venue/dusk])))
      (finally (show/stop! runtime)))))
