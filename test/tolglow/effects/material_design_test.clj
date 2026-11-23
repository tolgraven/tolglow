(ns tolglow.effects.material-design-test
  (:require [afterglow.render.blocks :as blocks]
            [clojure.test :refer [deftest is]]
            [tolglow.effects.material-design :as design]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.transit :as wire])
  (:import [java.io ByteArrayInputStream]))

(defn- request [method path body]
  (cond-> {:request-method method :uri path
           :headers {"host" "localhost" "accept" "application/transit+json" "content-type" "application/transit+json"}}
    body (assoc :body (ByteArrayInputStream. (.getBytes (wire/encode body) "UTF-8")))))

(defn- submit [rt handler path payload]
  (let [response (future (handler (request :post path payload))) deadline (+ (System/nanoTime) 5000000000)]
    (try
      (loop []
        (cond (realized? response) [(:status @response) (wire/decode (:body @response))]
              (> (System/nanoTime) deadline) (throw (ex-info "Show did not acknowledge material edit" {}))
              :else (do (show/tick! rt 25.0) (Thread/sleep 4) (recur))))
      (finally (future-cancel response)))))

(deftest checked-expression-preview-retains-raw-output-and-samples-venue-space
  (let [shader {:name "Ramp" :mode :replace :inputs {:phase {:op :bar}} :glsl "position.x + phase"}
        samples (design/preview {:material shader :beat 4})
        gel (design/preview {:material (blocks/library :optics/gel)})]
    (is (= [1.0 5.0] ((juxt :min :max) samples)))
    (is (= (* (:columns samples) (:rows samples)) (count (:rgb samples))))
    (is (= [[166 219 255]] (distinct (:rgb gel))))
    (is (not= (:rgb (design/preview {:material (blocks/library :optics/through-water) :beat 0}))
              (:rgb (design/preview {:material (blocks/library :optics/through-water) :beat 1}))))
    (is (thrown? Exception (design/preview {:material shader :beat 5})))
    (is (thrown? Exception (design/preview {:material (assoc shader :glsl "unknown") :beat 0})))))

(deftest edits-cross-ring-and-preserve-independent-operators-and-live-variable-chains
  (let [runtime (show/create-runtime {:workspace-options nil})
        handler (studio/handler runtime)
        id :tour/glass material (blocks/library :optics/through-water)
        changed (assoc-in material [:inputs :depth] 0.8)
        put! (fn [id prev next] (submit runtime handler "/api/materials" {:id id :expected prev :material next}))
        inventory #(wire/decode (:body (handler (request :get "/api/materials" nil))))]
    (try
      (is (contains? (:presets (inventory)) :optics/gel))
      (is (= 200 (:status (handler (request :post "/api/materials/preview" {:material changed :beat 3})))))
      (is (empty? (:materials (inventory))))
      (is (= 200 (first (put! id nil material))))
      (is (= 200 (first (put! :tour/gel nil (blocks/library :optics/gel)))))
      (is (= 200 (first (put! id material changed))))
      (is (= :stale-material (:reason (second (put! id material nil)))))
      (is (= 200 (first (put! id material changed))) "A retried write is idempotent")
      (is (= 200 (first (submit runtime handler "/api/settings"
                                {:scope :variable-blocks
                                 :values {:bindings {[:variables :air] {:base 1.0 :stages [{:block id}]}}}}))))
      (is (= 422 (first (put! id changed nil))) "Removing a required stage leaves live bindings valid")
      (is (= changed (get-in (inventory) [:materials id])))
      (is (= 200 (first (put! :tour/gel (blocks/library :optics/gel) nil))))
      (is (= #{id} (set (keys (:materials (inventory))))))
      (finally (show/stop! runtime)))))
