(ns tolglow.application-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [tolglow.application :as app]
            [tolglow.studio.agents :as agents]
            [tolglow.modules :as modules]
            [tolglow.show :as show])
  (:import [java.nio.file Files]))

(defn setup-extension [context options]
  {:value [(:loaded-components context) options]})

(deftest ordered-config-uses-real-namespace-registration-and-cleans-up
  (let [log (atom [])
        setup (fn [ctx {:keys [label]}]
                (swap! log conj [:open label])
                {:value (:loaded-components ctx)
                 :stop! #(swap! log conj [:close label])})
        registry (-> {} (modules/register :required setup) (modules/register :enabled setup))
        config {:init {:components [:required] :modules [:disabled :enabled] :post [:setup-extension]
                       :search-in-namespaces ['tolglow.application-test]}
                :required {:enabled false :label "first"} :disabled false :enabled {:label "second"}
                :setup-extension {:beat 1/8}}
        result (modules/start! registry config)]
    (is (= [:required :enabled :setup-extension] (:loaded-components result)))
    (is (= [[:required :enabled] {:beat 1/8}] (get-in result [:modules :setup-extension])))
    (is (not (contains? (:modules result) :disabled)))
    (is (thrown? Exception (modules/register registry :required setup)))
    (is (thrown? Exception (modules/plan registry (assoc-in config [:init :modules] [:required]))))
    ((:stop! result))
    ((:stop! result))
    (is (= [[:open "first"] [:open "second"] [:close "second"] [:close "first"]] @log))
    (reset! log [])
    (let [bad (assoc registry :broken (fn [_ _] (throw (ex-info "bad setup" {}))))]
      (is (thrown-with-msg? Exception #"bad setup" (modules/start! bad (assoc-in config [:init :post] [:broken]))))
      (is (= [[:open "first"] [:open "second"] [:close "second"] [:close "first"]] @log)))
    (reset! log [])
    (is (thrown? Exception (modules/start! registry (assoc-in config [:init :post] [:unknown]))))
    (is (empty? @log))))

(deftest cli-config-starts-a-headless-show-and-restores-its-transit-state
  (let [dir (.toFile (Files/createTempDirectory "tolglow-app-" (make-array java.nio.file.attribute.FileAttribute 0)))
        config-file (io/file dir "config.edn") saved (io/file dir "show.transit")
        config (assoc (app/default-config)
                      :show {:workers 1 :workspace-options nil}
                      :web-server true :hz 24
                      :osc {:enabled true :address "127.0.0.1" :port-in 0}
                      :expressive {:enabled true :devices {"test-pad" {:enabled? false}}}
                      :show-state {:enabled true :save-file (.getPath saved)})]
    (try
      (spit config-file (pr-str config))
      (let [runtime (app/launch! ["--config" (.getPath config-file) "--headless" "true"])]
        (try
          (is (= [:show :show-state :osc :expressive] (get-in runtime [:application :loaded-components])))
          (is (empty? @(:observers runtime)))
          (is (pos? (get-in runtime [:application :modules :osc :port])))
          (is (false? (get-in @(:state runtime) [:model :settings :expressive :devices "test-pad" :enabled?])))
          (is (= :accepted (:status (deref (show/submit! runtime {:op :master/set :value 0.42}) 4000 {}))))
          (is (pos? (get-in @(:state runtime) [:model :frame-number])))
          (finally (app/stop! runtime) (app/stop! runtime))))
      (is (pos? (.length saved)))
      (let [runtime (app/start! (-> config
                                    (assoc :osc false :expressive false :web-server {:port 0})
                                    (assoc :show-state {:load-file (.getPath saved)})))]
        (try
          (is (= 0.42 (get-in @(:state runtime) [:model :master])))
          (let [port (get-in runtime [:application :modules :web-server :port])
                state (slurp (str "http://127.0.0.1:" port "/api/state"))]
            (is (pos? port))
            (is (.contains state (:id runtime))))
          (finally (app/stop! runtime))))
      (finally (doseq [file [saved config-file dir]] (io/delete-file file true))))))

(def configured-venue
  {:init {:components [:show] :modules [:fixtures :cue-pages :waveforms :agents :web-server] :post []}
   :show {:workers 1 :workspace-options nil}
   :web-server false
   :fixtures {:patches [{:id :desk :position [0 0 0] :address 1 :universe 20 :channels {:dimmer 1}}]}
   :cue-pages {:definitions [{:id :desk-page :name "Desk" :cells {}}]}
   :waveforms {:definitions [{:id :flat :version 1 :name "Flat" :params {} :range [0 1]
                              :expression {:op :const :value 0.25}}]}
   :agents {:provider :openai :model "venue-model"}})

(deftest venue-var-installs-waveforms-and-agent-settings
  (let [runtime (app/launch! ["--config" "tolglow.application-test/configured-venue"])]
    (try
      (is (= [:desk] (mapv :id (get-in (show/snapshot runtime) [:rig :fixtures]))))
      (is (= "Desk" (get-in @(:state runtime) [:model :pages :desk-page :name])))
      (is (= [:show :fixtures :cue-pages :waveforms :agents] (get-in runtime [:application :loaded-components])))
      (is (= :flat (get-in (show/waveform-action! runtime :registry {}) [:waveforms :flat :id])))
      (is (= "venue-model" (:model (agents/public-config (show/agent-manager runtime)))))
      (finally (app/stop! runtime)))))

(deftest declarative-audio-sync-and-sensor-modules-own-their-inputs
  (let [opened-audio (promise) opened-clock (promise) released (promise)
        closed-audio (atom 0) closed-clock (atom 0)
        config {:init {:components [:show] :modules [:sensors :clock-sync :audio] :post []}
                :show {:workers 1 :workspace-options nil
                       :audio-options {:open-source! (fn [cfg]
                                                       (deliver opened-audio cfg)
                                                       {:read! (fn [& _] @released -1)
                                                        :close! #(do (swap! closed-audio inc) (deliver released true))})}
                       :sync-options {:midi-factory (fn [_ _]
                                                      {:open! (fn [receive!] (deliver opened-clock receive!) :clock)
                                                       :close! (fn [_] (swap! closed-clock inc))})}}
                :audio {:enabled true :default? true :block-frames 240}
                :clock-sync {:midi {:enabled? true :input-index 0} :selected-source :midi-clock}
                :sensors {:sources {"watch" {:config {:version 1}
                                             :pairing {:credential-ref (str "credential-" (java.util.UUID/randomUUID))}}}}}
        runtime (app/start! config)]
    (try
      (is (= [:show :sensors :clock-sync :audio] (get-in runtime [:application :loaded-components])))
      (is (= 240 (:block-frames (deref opened-audio 2000 nil))))
      (is (fn? (deref opened-clock 2000 nil)))
      (is (= :midi-clock (get-in (show/sync-action! runtime :status {}) [:clock :selected-source])))
      (is (= {:version 1} (get-in @(:state runtime) [:model :sensors :sources "watch" :config])))
      (finally (app/stop! runtime)))
    (is (= 1 @closed-clock @closed-audio))))
