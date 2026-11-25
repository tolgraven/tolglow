(ns tolglow.personalities-host-test
  "The same personality commands through the show owner, the REPL entry point
  and Studio's Ring routes."
  (:require [afterglow.control.model :as control]
            [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [tolglow.personalities :as personalities]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.transit :as transit])
  (:import [java.io ByteArrayInputStream]))

(def ^:private offsets [-0.9 -0.45 0.0 0.45 0.9])

(def fixtures
  "A 5 by 5 wall of single-channel lamps, 0.45 m apart, centred on [0 1 0]."
  (vec (for [[row y] (map-indexed vector offsets) [column x] (map-indexed vector offsets)
             :let [index (+ column (* 5 row))]]
         {:id (keyword (str "lamp-" index)) :position [x (+ 1.0 y) 0.0]
          :universe 1 :address (inc index) :channels {:red 1}})))

(defn- runtime [batch?]
  (let [captured (atom nil)
        capture! (fn [universes _frame-id]
                   (reset! captured (mapv #(bit-and 255 %) (take 25 (get universes 1)))))]
    (assoc (show/create-runtime {:model (assoc (control/new-show {}) :rig {:fixtures fixtures})
                                 :batch? batch? :workspace-options nil :sink {:send! capture!}})
           ::captured captured)))

(defn- tick! [rt now]
  (show/tick! rt now)
  (is (nil? (get-in @(:state rt) [:status :error])) (pr-str (:status @(:state rt)))))

(defn- submit! [rt now type payload]
  (let [receipt (show/submit! rt type payload)]
    (tick! rt now)
    (deref receipt 2000 {:status :timeout})))

(defn- accept! [rt now type payload]
  (let [ack (submit! rt now type payload)]
    (is (= :accepted (:status ack)) (pr-str ack))
    ack))

(defn- output [rt] @(::captured rt))

(defn- request! [rt now method path content-type accept body]
  ;; Run the actual Ring request while the explicit host owner drains its queue.
  (let [result (future ((studio/handler rt)
                        (cond-> {:request-method method :uri path
                                 :headers (cond-> {"host" "localhost" "accept" accept}
                                            content-type (assoc "content-type" content-type))}
                          body (assoc :body (ByteArrayInputStream. (.getBytes ^String body "UTF-8"))))))
        deadline (+ (System/nanoTime) 5000000000)]
    (try
      (loop []
        (cond
          (realized? result) @result
          (> (System/nanoTime) deadline) (throw (ex-info "Ring request timed out" {:path path}))
          :else (do (tick! rt now) (Thread/sleep 5) (recur))))
      (finally (future-cancel result)))))

(defn- post-transit! [rt now path payload]
  (let [response (request! rt now :post path "application/transit+json" "application/transit+json" (transit/encode payload))]
    (assoc response :data (transit/decode (:body response)))))

(defn- post-json! [rt now path payload]
  (let [response (request! rt now :post path "application/json" "application/json" (json/write-str payload))]
    (assoc response :data (json/read-str (:body response) :key-fn keyword))))

(defn- studio-state [rt now]
  (transit/decode (:body (request! rt now :get "/api/state" nil "application/transit+json" nil))))

(deftest repl-and-studio-share-one-validated-command-and-projection
  (let [rt (runtime true)]
    (try
      (testing "a fresh show lists the built-ins and has no active feel"
        (let [view (:personalities (show/snapshot rt))]
          (is (= (mapv :id personalities/builtins) (mapv :id (:library view))))
          (is (nil? (:active view)))))
      (testing "the REPL selects through the owner queue"
        (let [before (:revision (show/snapshot rt))
              ack (accept! rt 0 :personality {:op :select :id :squircle})]
          (is (= {:mix [{:id :squircle :version 1 :weight 1.0}] :seed 0} (:active ack)))
          (is (= (inc before) (:revision ack) (:revision (show/snapshot rt))))
          (is (= :squircle (get-in (show/snapshot rt) [:personalities :active :id])))))
      (testing "Studio reads the committed selection, icon included, from its own route"
        (let [state (studio-state rt 10)]
          (is (= (:personalities (show/snapshot rt)) (:personalities state)))
          (is (re-find #"^<svg " (get-in state [:personalities :active :icon])))))
      (testing "Studio's typed route submits the same operation"
        (let [{:keys [status data]} (post-transit! rt 20 "/api/personality"
                                                   {:op :blend :mix [{:id :squircle :weight 3} {:id :spirals :weight 1}] :seed 4
                                                    :runtime-id (:id rt) :expected-epoch 0})
              active (get-in (show/snapshot rt) [:personalities :active])]
          (is (= 200 status) (pr-str data))
          (is (= :accepted (:status data)))
          (is (= {:mix [{:id :squircle :version 1 :weight 0.75} {:id :spirals :version 1 :weight 0.25}] :seed 4}
                 (:active data)))
          (is (= [personalities/blend-id 4] ((juxt :id :seed) active)))
          (is (= active (get-in (studio-state rt 30) [:personalities :active])))))
      (testing "a plain JSON client can use the same route with textual IDs"
        (let [{:keys [status data]} (post-json! rt 40 "/api/personality" {:op "select" :id "still-water"})]
          (is (= 200 status) (pr-str data))
          (is (= "accepted" (:status data)))
          (is (= :still-water (get-in (show/snapshot rt) [:personalities :active :id])))
          (is (= 4 (get-in (show/snapshot rt) [:personalities :active :seed])) "the seed carries over")))
      (testing "both paths report the same rejection and leave the selection alone"
        (let [revision (:revision (show/snapshot rt))
              from-repl (submit! rt 50 :personality {:op :select :id :no-such-feel})
              from-studio (post-transit! rt 60 "/api/personality" {:op :select :id :no-such-feel})]
          (is (= :rejected (:status from-repl)))
          (is (= 422 (:status from-studio)))
          (is (= "Unknown personality" (:message from-repl) (get-in from-studio [:data :message])))
          (is (= {:id :no-such-feel} (:details from-repl) (get-in from-studio [:data :details])))
          (is (= revision (:revision (show/snapshot rt))))
          (is (= :still-water (get-in (show/snapshot rt) [:personalities :active :id])))))
      (testing "a previous show session cannot change the feel"
        (is (= :rejected (:status @(show/submit! rt :personality {:op :clear :expected-epoch 99})))))
      (testing "an authored personality is validated on the way in and can be selected from either side"
        (let [mine {:id :mine :version 1 :schema 1 :name "Mine" :palette [[0.2 0.4 1.0]]
                    :concurrency {:fixture-fraction 1.0 :max-groups 1}
                    :geometry {:forms {:zigzag 1.0}}
                    :motion {:speed 0.5 :smoothness 0.5 :jitter 0.5 :symmetry 0.5}}
              refused (post-transit! rt 70 "/api/personality" {:op :put :personality (assoc-in mine [:motion :speed] 3)})]
          (is (= 422 (:status refused)))
          (is (= [{:path [:motion :speed] :message "Motion character must be within [0,1]"}]
                 (get-in refused [:data :details :errors])))
          (is (= {:id :mine :version 1} (select-keys (accept! rt 80 :personality {:op :put :personality mine}) [:id :version])))
          (is (= :mine (:id (last (get-in (studio-state rt 90) [:personalities :library])))))
          (accept! rt 100 :personality {:op :select :id :mine})
          (is (= "Mine" (get-in (show/snapshot rt) [:personalities :active :name])))))
      (testing "an agent inspects the same data without the pictures"
        (let [seen (:personalities (show/dispatch-tool! rt "inspect_show" {} {}))]
          (is (= :mine (get-in seen [:active :id])))
          (is (not (re-find #"<svg" (pr-str seen))))
          (is (= (personalities/without-icons (:personalities (show/snapshot rt))) seen))))
      (finally (show/stop! rt)))))

(deftest a-saved-show-reopens-with-its-feel
  (let [rt (runtime true) reopened (runtime true)]
    (try
      (accept! rt 0 :personality {:op :put :personality (assoc (first personalities/builtins) :id "house" :name "House feel")})
      (accept! rt 10 :personality {:op :blend :mix [{:id "house" :weight 1} {:id :random-pink :weight 1}] :seed 12})
      (let [saved (show/export-show-source rt)
            before (:personalities (show/snapshot rt))]
        (is (= :accepted (:status (submit! reopened 0 :import {:source saved}))))
        (tick! reopened 25)
        (is (= before (:personalities (show/snapshot reopened))))
        (is (= (personalities/state (:model @(:state rt))) (personalities/state (:model @(:state reopened)))))
        (testing "a restart keeps the selection, as it keeps settings"
          (accept! reopened 50 :command {:op :show/restart})
          (is (= (:active before) (get-in (show/snapshot reopened) [:personalities :active])))))
      (finally (show/stop! reopened) (show/stop! rt)))))
