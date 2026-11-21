(ns tolglow.studio.link-pause-test
  (:require [afterglow.render.frame :as render]
            [tolglow.output.pause-look :as pause-look]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [tolglow.show :as show]
            [tolglow.studio.persistence :as files])
  (:import [java.net InetAddress ServerSocket]))

(defn- commit! [r type payload at]
  (let [ack (show/submit! r type payload)]
    (show/tick! r at)
    (deref ack 1000 ::timeout)))

(defn- eventually [pred]
  (let [deadline (+ (System/nanoTime) 3000000000)]
    (loop []
      (or (pred)
          (when (< (System/nanoTime) deadline)
            (Thread/sleep 5)
            (recur))))))

(defn- clock [r] (:clock (show/sync-action! r :status {})))

(defn- status! [writer message]
  (.write writer (str "status " (pr-str message) "\n"))
  (.flush writer))

(deftest loopback-link-status-reaches-the-owned-show-transport
  (doseq [follow? [true false]]
    (with-open [server (ServerSocket. 0 1 (InetAddress/getByName "127.0.0.1"))]
      (.setSoTimeout server 3000)
      (let [r (show/create-runtime {:workspace-options nil})
            config {:selected-source :ableton-link :link {:enabled? true :follow-transport? follow?
                                                           :port (.getLocalPort server)}}
            base {:peers 1 :start 123456 :bpm 105.0}]
        (try
          (show/sync-action! r :configure {:config config})
          (is (= (:link config) (select-keys (get-in (show/export-show r) [:model :integrations :sync :config :link])
                                             [:enabled? :follow-transport? :port])))
          (show/sync-action! r :start {})
          (with-open [socket (.accept server) reader (io/reader socket) writer (io/writer socket)]
            (.setSoTimeout socket 3000)
            (when follow? (is (= "enable-start-stop-sync" (.readLine reader))))
            (is (= "status" (.readLine reader)))
            (show/tick! r)
            (is (= (not follow?) (get-in (show/snapshot r) [:transport :playing?]))
                "Tempo-only pairing retains local playback while waiting for the first peer.")
            (status! writer (assoc base :beat 8.25 :playing true))
            (is (eventually #(= :locked (:status (clock r)))))
            (show/tick! r)
            (is (= :ableton-link (get-in (show/snapshot r) [:transport :source])))
            (is (= 105.0 (get-in (show/snapshot r) [:transport :bpm])))
            (is (true? (get-in (show/snapshot r) [:transport :playing?])))
            (status! writer (assoc base :beat 10.5 :playing false))
            (is (eventually #(if follow? (= :paused (:status (clock r))) (>= (get-in (clock r) [:clock :beat]) 10.5))))
            (show/tick! r)
            (is (= (not follow?) (get-in (show/snapshot r) [:transport :playing?])))
            (when-not follow?
              (let [ack (show/submit! r {:op :transport/set :transport {:playing? false}})]
                (show/tick! r) (is (= :accepted (:status @ack)))))
            (when-not follow?
              (let [ack (show/submit! r {:op :cue/launch :cue-id :ember :launch {:mode :timed :duration {:beats 3}}})]
                (show/tick! r) (is (= :accepted (:status @ack)))))
            (let [paused (get-in (show/snapshot r) [:transport :beat])
                  revision (:revision (clock r))]
              (status! writer (assoc base :beat 14.75 :playing false))
              (is (eventually #(> (:revision (clock r)) revision)))
              (show/tick! r)
              (is (= paused (get-in (show/snapshot r) [:transport :beat]))))
            (status! writer (assoc base :beat 15.5 :playing true))
            (is (eventually #(>= (get-in (clock r) [:clock :beat]) 15.5)))
            (show/tick! r)
            (is (= follow? (get-in (show/snapshot r) [:transport :playing?])))
            (when-not follow?
              (let [reply (show/submit! r {:op :transport/set :transport {:playing? true}})]
                (show/tick! r)
                (is (= :accepted (:status @reply)))
                (show/tick! r)
                (is (some #(= :ember (:cue-id %)) (:instances (show/snapshot r)))
                    "A local resume rebases timed cues over the Link phase gap.")))
            (status! writer (assoc base :peers 0))
            (is (eventually #(= :disconnected (:status (clock r)))))
            (show/tick! r)
            (is (= (not follow?) (get-in (show/snapshot r) [:transport :playing?]))))
          (finally (show/stop! r)))))))

(deftest manual-pause-output-is-configurable-and-serialized
  (let [frames (atom []) r (show/create-runtime {:workspace-options nil
                    :sink {:send! (fn [buffers _] (swap! frames conj (mapv #(bit-and 255 %) (take 12 (get buffers 1)))))}})
        at (atom 0)
        apply! (fn [type payload] (commit! r type payload (swap! at + 100)))
        output #(last @frames)
        pause! #(apply! :command {:op :transport/set :transport {:playing? %}})
        settings! #(apply! :settings {:scope :output :values {:pause-look %}})]
    (try
      (is (= :accepted (:status (apply! :command {:op :cue/launch :cue-id :ember}))))
      (pause! false)
      (is (pos? (first (output))) "default pause keeps active cue output")
      (is (= :rejected (:status (settings! {:mode :ambient :level 1.1}))))
      (is (= :accepted (:status (settings! {:mode :ambient :level 0.1 :color [0.8 0.2 0.1]}))))
      (is (= [26 179 26 0 26] (subvec (output) 0 5)) "real coarse DMX includes physical dimmer and extracted white")
      (let [{:keys [model frame-output]} (show/export-show r)
            source (files/write-string model frame-output)]
        (settings! {:mode :blackout})
        (is (every? zero? (take 5 (output))))
        (let [reply (show/submit! r :import {:source source})]
          (show/tick! r (swap! at inc))
          (is (= :accepted (:status @reply))))
        (is (= :ambient (get-in (show/snapshot r) [:settings :output :pause-look :mode])))
        (is (= [26 179 26 0 26] (subvec (output) 0 5))))
      (apply! :command {:op :master/set :value 0.5})
      (is (= 13 (first (output))))
      (apply! :command {:op :blackout/set :value true})
      (is (every? zero? (take 5 (output))))
      (apply! :command {:op :blackout/set :value false})
      (pause! true)
      (is (not= 13 (first (output))) "running cue regains its own intensity")
      (finally (show/stop! r)))))


(deftest ambient-layer-replaces-emission-and-preserves-fixture-control
  (let [rig [{:id :led :universe 1 :address 1 :channels {:red 1 :green 2 :blue 3 :strobe 4}}
             {:id :spot :universe 1 :address 5 :channels {:dimmer 1 :pan 2 :focus 3}}]
        cue {:kind :assignments :assignments [{:target-id :led :attribute :red :value 1.0}
                                              {:target-id :led :attribute :strobe :value 1.0}
                                              {:target-id :spot :attribute :pan :value 0.25}
                                              {:target-id :spot :attribute :focus :value 0.75}]}
        ambient (pause-look/ambient-layer rig {:color [0 1 0] :level 0.5})
        frame (render/resolve-contributions rig [cue ambient] {:master 0.5})]
    (is (= [0 64 0 0 64 64 191] (mapv #(bit-and 255 %) (take 7 (get-in frame [:universes 1])))))))
