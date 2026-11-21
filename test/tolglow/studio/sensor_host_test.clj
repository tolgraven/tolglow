(ns tolglow.studio.sensor-host-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data.json :as json]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.studio.sensors-test :as fixture])
  (:import [java.io ByteArrayInputStream]
           [java.util.concurrent LinkedBlockingQueue]))

(defn- apply! [runtime type payload time-ms]
  (let [receipt (show/submit! runtime type payload)]
    (show/tick! runtime time-ms) (deref receipt 1000 {:status :missing})))
(defn- http [runtime path payload]
  (let [response ((studio/handler runtime)
                  {:request-method :post :uri (str "/api/sensors/" path) :headers {"host" "localhost"}
                   :body (ByteArrayInputStream. (.getBytes (json/write-str payload) "UTF-8"))})]
    {:status (:status response) :data (json/read-str (:body response) :key-fn keyword)}))
(defn- queued-http [runtime path payload time-ms]
  (let [result (future (http runtime path payload)) deadline (+ (System/nanoTime) 3000000000)]
    (loop []
      (when (and (not (realized? result)) (zero? (.size ^LinkedBlockingQueue (:queue runtime))) (< (System/nanoTime) deadline))
        (Thread/sleep 1) (recur)))
    (show/tick! runtime time-ms)
    (deref result 4000 {:status :timeout})))
(defn- observation [pair sequence values & [gestures]]
  {:version 1 :device-id "phone" :session-id "motion-one" :sample-id sequence
   :time-ms (* sequence 20) :values values :gestures (or gestures []) :pairing-token (:pairing-token pair)})

(deftest phone-mapping-and-semantic-gesture-run-through-the-actual-ring-owner-engine
  (let [physical (atom 0) runtime (show/create-runtime {:credential-store (fixture/vault)
                                                        :sensor-options {:now-ms #(deref physical)}})]
    (try
      (let [pair (:data (queued-http runtime "pair" {:device-id "phone" :label "Conducting phone"} 0))
            id (:instance-id (apply! runtime :command {:op :cue/launch :cue-id :ember} 0))
            config {:version 1 :channels [{:id :pitch :input :orientation-beta}]
                    :bindings [{:id :level :channel :pitch :command {:op :param/set :instance-id id :param :level}
                                :schema {:type :number :default 0.7 :bounds [0 1]}}]
                    :gestures [{:id :tap :type :primary-action :cooldown {:beats 1/4}
                                :command {:op :cue/launch :cue-id :flash :launch {:mode :one-shot :duration {:beats 1/8}}}}]}]
        (is (= "accepted" (:status pair)))
        (is (= "accepted" (get-in (queued-http runtime "configure" {:device-id "phone" :source (pr-str config)} 0) [:data :status])))
        (is (:accepted? (:data (http runtime "sample" (observation pair 0 {:orientation-beta 0.2})))))
        (is (= "accepted" (get-in (queued-http runtime "arm" {:device-id "phone" :session-id "motion-one" :armed? true} 100) [:data :status])))
        (http runtime "sample" (observation pair 1 {:orientation-beta 0.6}))
        (show/tick! runtime 250)
        (is (= 0.6 (get-in @(:state runtime) [:model :instances id :params :level])))
        (is (= 0.6 (first (get-in (show/snapshot runtime) [:output :values :head-1 :emitted-color]))))
        (let [packet (observation pair 2 {:orientation-beta 0.6} [{:id "double-tap-1" :type "primary-action" :phase "pulse"}])]
          (is (:accepted? (:data (http runtime "sample" packet))))
          (show/tick! runtime 300)
          (is (= 1 (count (filter #(= :flash (:cue-id %)) (:instances (show/snapshot runtime))))))
          (is (= "stale-sample" (get-in (http runtime "sample" packet) [:data :reason]))))
        (is (not (.contains (pr-str (show/export-show runtime)) (:pairing-token pair))))
        (is (not (.contains (pr-str (show/sensor-action! runtime :status {})) (:pairing-token pair))))
        (is (= 422 (:status (queued-http runtime "arm" {:device-id "phone" :session-id "motion-one" :armed? false :expected-epoch 999} 300))))
        (reset! physical 2000)
        (show/tick! runtime 2000)
        (is (false? (:armed? (first (:sources (show/sensor-action! runtime :status {})))))))
      (finally (show/stop! runtime)))))

(deftest saved-held-sensor-cue-is-remapped-and-released-without-ending-the-human-cue
  (let [runtime (show/create-runtime {:credential-store (fixture/vault) :sensor-options {:now-ms (constantly 0)}})]
    (try
      (let [pair (:data (queued-http runtime "pair" {:device-id "phone"} 0))
            human (:instance-id (apply! runtime :command {:op :cue/launch :cue-id :ember :source {:adapter :web :session-id "human"}} 0))
            config {:version 1 :channels [{:id :height :input :pitch}]
                    :triggers [{:id :hold :channel :height :on 0.8 :off 0.2
                                :command {:op :cue/launch :cue-id :ember :launch {:mode :held}}}]}]
        (queued-http runtime "configure" {:device-id "phone" :source (pr-str config)} 0)
        (http runtime "sample" (observation pair 0 {:pitch 0}))
        (queued-http runtime "arm" {:device-id "phone" :session-id "motion-one" :armed? true} 0)
        (http runtime "sample" (observation pair 1 {:pitch 1}))
        (show/tick! runtime 100)
        (let [before (:model @(:state runtime))
              held (some #(when (= :sensor (get-in % [:source :adapter])) (:id %)) (vals (:instances before)))
              saved (show/export-show runtime)]
          (is held)
          (is (not= human held))
          (is (= held (-> before :sensors :sources (get "phone") :state :holds :gestures vals first :instance-id)))
          (is (= :accepted (:status (apply! runtime :import {:source (pr-str saved)} 200))))
          (let [restored (:model @(:state runtime))
                ;; Restored external sessions deliberately lose live source
                ;; authority; preserved launch order identifies these instances.
                remapped-held (some #(when (= (get-in before [:instances held :order]) (:order %)) (:id %)) (vals (:instances restored)))
                remapped-human (some #(when (= (get-in before [:instances human :order]) (:order %)) (:id %)) (vals (:instances restored)))]
            (is (not= held remapped-held))
            (is (= remapped-held (-> restored :sensors :sources (get "phone") :state :holds :gestures vals first :instance-id)))
            (show/tick! runtime 250)
            (is (= :releasing (get-in @(:state runtime) [:model :instances remapped-held :phase])))
            (is (= :running (get-in @(:state runtime) [:model :instances remapped-human :phase]))))))
      (finally (show/stop! runtime)))))
