(ns tolglow.integrations.phrase-capture-host-test
  "The show runtime owns the capture buffer: every owner cycle folds the
  committed model in, Capture reads the last bars, and Studio's HTTP routes
  run the same operation."
  (:require [afterglow.control.model :as control]
            [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [tolglow.integrations.phrase-clip :as clip]
            [tolglow.show :as show]
            [tolglow.studio :as studio])
  (:import [java.io ByteArrayInputStream]
           [javax.sound.midi MidiSystem]))

(def registry
  {:effects {:wash {:id :wash :version 1 :category :light
                    :params {:level {:type :number :default 0.0 :bounds [0.0 1.0]}}
                    :outputs {:dimmer {:op :param :key :level}}}}
   :cues {:wash {:id :wash :effect-id :wash :version 1}}})

(defn- runtime [options]
  (show/create-runtime
   (merge {:model (control/new-show {:show-id :capture-host :transport {:playing? true :beat 0.0 :bpm 120.0}
                                     :registry registry
                                     :rig {:fixtures [{:id :left :universe 1 :address 1 :channels {:dimmer 1}}]}})
           :sink {:send! (fn [_ _])} :workspace-options nil}
          options)))

(def performer {:adapter :repl :session-id "capture"})

(defn- at! [rt beat payload]
  (let [reply (show/submit! rt :command (assoc payload :source performer))]
    (show/tick! rt (long (* 500 beat)))
    (deref reply 1000 ::missing)))

(defn- perform! [rt]
  (show/tick! rt 0)
  (let [id (:instance-id (at! rt 1 {:op :cue/launch :cue-id :wash :launch {:mode :persistent :params {:level 0.25}}}))]
    (at! rt 2 {:op :param/set :instance-id id :param :level :value 0.5})
    (at! rt 3 {:op :instance/end :instance-id id})
    (doseq [b (range 3 8)] (show/tick! rt (* 500 b)))))

(deftest the-runtime-folds-each-cycle-and-captures-the-last-bars
  (let [rt (runtime {:phrase-capture {:bars 2}})]
    (try
      (perform! rt)
      (let [status (show/capture-action! rt :status {})]
        (is (= 2 (:bars status)))
        (is (= 3 (:retained-events status)))
        (is (pos? (get-in status [:cost :cycles])))
        (is (number? (get-in status [:cost :mean-us])) "the per-cycle cost is measured"))
      (let [{:keys [status phrase notes cues bars capture-id]} (show/capture-phrase! rt)]
        (is (= [:captured 1 2 1 [:wash]] [status capture-id bars notes cues]))
        (is (= {:length-beats 8
                :notes [{:id "n0" :cue-id :wash :start 1 :length 2 :params {:level 0.25}}]
                :lanes [{:cue-id :wash :param :level :points [{:beat 2 :value 0.5 :note "n0"}]}]}
               (clip/comparable phrase))))
      (testing "the Standard MIDI File of the last capture holds its note"
        (let [{:keys [filename bytes]} (show/capture-action! rt :smf {})
              clip (clip/smf->clip bytes {:channel 1 :length-beats 8})]
          (is (= "tolglow-capture-1.mid" filename))
          (is (= [[36 1 2]] (mapv (juxt :pitch :start :duration) (:notes clip))))
          (is (pos? (.getTickLength (MidiSystem/getSequence (ByteArrayInputStream. bytes)))))))
      (testing "capturing silence keeps the previous capture"
        (doseq [b (range 8 20)] (show/tick! rt (* 500 b)))
        (is (= :empty (:status (show/capture-phrase! rt))))
        (is (= 1 (get-in (show/capture-action! rt :status {}) [:last :capture-id]))))
      (finally (show/stop! rt)))))

(deftest capture-can-be-disabled
  (let [rt (runtime {:phrase-capture false})]
    (try
      (show/tick! rt 0)
      (is (thrown? clojure.lang.ExceptionInfo (show/capture-action! rt :status {})))
      (finally (show/stop! rt)))))

(defn- request [app method uri]
  (app (cond-> {:request-method method :uri uri :headers {"host" "localhost"}}
         (= :post method) (assoc :body (ByteArrayInputStream. (.getBytes "{}" "UTF-8"))))))

(defn- data [response] (json/read-str (:body response) :key-fn keyword))

(deftest studio-runs-the-same-capture
  (let [rt (runtime {})
        app (studio/handler rt)]
    (try
      (perform! rt)
      (let [captured (request app :post "/api/capture/capture")
            status (request app :get "/api/capture")
            midi (request app :get "/api/capture/last.mid")]
        (is (= 200 (:status captured)) (pr-str captured))
        (is (= {:status "captured" :capture-id 1 :notes 1 :cues ["wash"]}
               (select-keys (data captured) [:status :capture-id :notes :cues])))
        (is (not (contains? (data captured) :phrase)) "the browser gets a summary; the phrase stays on the host")
        (is (= 1 (get-in (data status) [:last :capture-id])))
        (is (= 200 (:status midi)))
        (is (= "audio/midi" (get-in midi [:headers "Content-Type"])))
        (is (java.util.Arrays/equals ^bytes (:bytes (show/capture-action! rt :smf {})) ^bytes (:body midi))))
      (finally (show/stop! rt)))))
