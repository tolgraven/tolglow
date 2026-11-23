(ns tolglow.parameter-sources-test
  (:require [afterglow.control.model :as control]
            [afterglow.control.params :as params]
            [clojure.test :refer [deftest is]]
            [tolglow.effects.parameter-sources :as sources]
            [tolglow.modulation-test :as fixture]
            [tolglow.show :as show]
            [tolglow.signals :as signals]
            [tolglow.studio.web :as web]
            [tolglow.transit :as transit])
  (:import [java.io ByteArrayInputStream]))

(def saw {:type :lfo :waveform :saw :period {:beats 1} :from 0.0 :to 1.0})

(defn- bind-to [model param binding]
  (sources/bind model {:instance-id "0:1" :param param :binding binding}))

(deftest every-declared-parameter-type-and-musical-period
  (let [model (fixture/model fixture/all-types)
        endpoints {:level [0.0 1.0] :steps [1 4] :gate [false true] :shape [:sine :square]
                   :color [[0 1 0] [1 0 1]] :position [[-2 0 2] [2 0 -2]] :spread [[0 1] [4 5]]}
        bound (reduce-kv (fn [m k [lo hi]] (bind-to m k (assoc saw :from lo :to hi :period {:bars 1})))
                         model endpoints)
        frame #(sources/sample bound {:beat % :time-ms 0} {})
        half (get-in (frame 2) [:overrides "0:1"])]
    (is (= (set (keys endpoints)) (set (keys half))))
    (is (empty? (params/validate-values (:params fixture/all-types) half)))
    (is (= [0.5 3 true :square [0.5 0.5 0.5] [0.0 0.0 0.0] [2.0 3.0]]
           (mapv half [:level :steps :gate :shape :color :position :spread])))
    (is (= false (get-in (frame 1) [:overrides "0:1" :gate])))
    (doseq [bad [(assoc saw :period {:beats 0}) (assoc saw :period {:ms 50}) (assoc saw :phase ##NaN)
                 (assoc saw :from 2) (assoc saw :inputs {:bogus 1}) (assoc saw :waveform :missing)]]
      (is (thrown? clojure.lang.ExceptionInfo (bind-to model :level bad))))))

(deftest registered-waveform-hot-reload-and-unavailable-targets-isolate-one-binding
  (let [library (signals/new-registry)
        constant (fn [v value] {:id :custom :version v :range [0 1] :params {} :expression {:op :const :value value}})
        model (assoc (fixture/model) :waveforms (signals/publish library (constant 1 0.2)))
        bound (bind-to model :level (assoc saw :waveform :custom))
        frame {:time-ms 0 :beat 0} first-sample (sources/sample bound frame {})
        updated (update bound :waveforms signals/publish (constant 2 0.6))
        next-sample (sources/sample updated frame (:plans first-sample))
        incompatible (assoc-in updated [:instances "0:1" :definition :params :level] {:type :boolean :default true})
        failed (sources/sample incompatible frame (:plans next-sample))]
    (is (= 0.2 (get-in first-sample [:overrides "0:1" :level])))
    (is (= 0.6 (get-in next-sample [:overrides "0:1" :level])))
    (is (string? (get-in failed [:sources "0:1" :level :error])))
    (is (empty? (:overrides failed)))
    (is (empty? (:plans (sources/sample (assoc updated :instances {}) frame (:plans next-sample)))))))

(defn- live-model []
  (control/new-show
    {:show-id :test :transport {:playing? true :beat 0.0 :bpm 120.0}
     :registry {:effects {:wash (assoc-in fixture/effect [:params :level :default] 0.25)}
                :cues {:wash {:id :wash :effect-id :wash :version 1}}}
     :rig {:fixtures (mapv (fn [id address] {:id id :universe 1 :address address :channels {:dimmer 1}})
                           [:left :right] [1 2])}}))

(deftest ring-through-render-two-instances-base-hold-and-transit-restore
  (let [packet (atom nil) t (atom 0)
        rt (show/create-runtime {:model (live-model) :sink {:send! (fn [bytes _] (reset! packet (mapv #(bit-and 255 %) (take 2 (get bytes 1)))))}})
        submit! (fn [op body] (let [ack (show/submit! rt op body)] (show/tick! rt (swap! t inc)) ack))
        app (web/app {:submit! submit! :heartbeat! (constantly nil)})
        post (fn [route data]
               (:status (app {:request-method :post :uri (str "/api/modulation/" route)
                              :headers {"host" "localhost" "content-type" "application/transit+json"}
                              :body (ByteArrayInputStream. (.getBytes (transit/encode data) "UTF-8"))})))
        instance (fn [id] (some #(when (= id (:id %)) %) (:instances (show/snapshot rt))))
        launch (fn [target] (:instance-id @(submit! :command {:op :cue/launch :cue-id :wash :launch {:targets [target]}})))]
    (try
      (let [left (launch :left) right (launch :right)
            addr {:instance-id left :param :level} hold {:instance-id left :params [:level]}]
        (is (not= left right))
        (is (= 200 (post "bind" (assoc addr :binding (assoc saw :from 0.8 :to 0.8)))))
        (is (= [204 64] @packet))
        (is (= 0.25 (get-in (instance left) [:params :level])))
        (is (= 0.8 (get-in (instance left) [:parameter-sources :level :effective-value])))
        (is (empty? (:parameter-sources (instance right))))
        (is (= 200 (post "hold" hold)))
        (is (= [64 64] @packet))
        (is (true? (get-in (instance left) [:parameter-sources :level :sources 0 :held?])))
        (is (= 200 (post "resume" hold)))
        (is (= [204 64] @packet))
        (let [source (show/export-show-source rt)
              receipt @(submit! :import {:source source})
              restored-id (:id (first (:instances (show/snapshot rt))))
              restored {:instance-id restored-id :param :level}]
          (is (= :accepted (:status receipt)) (pr-str receipt))
          (is (not= left restored-id))
          (is (= 0.8 (get-in (instance restored-id) [:parameter-sources :level :effective-value])))
          (is (= 200 (post "bind" (assoc restored :binding (assoc saw :period {:beats 1/2})))))
          (let [before @packet]
            (show/tick! rt (swap! t + 125))
            (is (not= (first before) (first @packet)))
            (is (= 64 (second @packet))))
          (is (= 200 (post "unbind" restored)))
          (is (= [64 64] @packet))
          (is (empty? (:parameter-sources (instance restored-id))))))
      (finally (show/stop! rt)))))
