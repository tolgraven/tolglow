(ns tolglow.effects.material-design-test
  (:require [afterglow.render.blocks :as blocks]
            [clojure.test :refer [deftest is]]
            [tolglow.demo :as demo]
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
    (is (= [[83 110 128]] (distinct (:rgb (design/preview {:material (blocks/library :optics/gel) :dry 0.5})))))
    (is (= [[166 219 255]] (distinct (:rgb gel))))
    (is (not= (:rgb (design/preview {:material (blocks/library :optics/through-water) :beat 0}))
              (:rgb (design/preview {:material (blocks/library :optics/through-water) :beat 1}))))
    (let [dependent (assoc shader :inputs {:phase {:op :param :key :fader}})]
      (is (true? (:unavailable? (design/preview {:material dependent}))))
      (is (nil? (:rgb (design/preview {:material dependent})))))
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

(deftest draft-renders-the-committed-resolved-input-and-leaves-the-show-untouched
  (let [model (-> (demo/model)
                  (assoc-in [:variables :audio :features :low-rms] 0.25)
                  (assoc-in [:settings :variable-blocks :bindings [:variables :stage :level]]
                            {:base {:op :variable :path [:audio :features :low-rms]} :stages []}))
        rt (show/create-runtime {:model model :workspace-options nil})
        handler (studio/handler rt)
        material {:name "Live transmission" :mode :multiply
                  :inputs {:level {:op :variable :path [:stage :level]}} :glsl "level"}
        inspect (fn [draft] (let [response (handler (request :post "/api/materials/preview" {:material draft :dry 0.5}))]
                              (is (= 200 (:status response))) (wire/decode (:body response))))]
    (try
      (show/tick! rt 25.0)
      (let [before @(:state rt)
            view (inspect material)]
        (is (= 0.125 (:min view) (:max view)))
        (is (= [[32 32 32]] (distinct (:rgb view))))
        (let [pending (inspect (assoc-in material [:inputs :level :path] [:not-yet-connected]))]
          (is (:unavailable? pending))
          (is (re-find #":not-yet-connected" (:message pending))))
        (is (identical? before @(:state rt))))
      (finally (show/stop! rt)))))
