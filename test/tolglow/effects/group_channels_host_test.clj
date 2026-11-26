(ns tolglow.effects.group-channels-host-test
  (:require [afterglow.control.model :as control]
            [afterglow.control.devices.session :as device-session]
            [afterglow.render.frame :as render]
            [clojure.data.json :as json]
            [clojure.test :refer [deftest is]]
            [tolglow.atmosphere.control :as fog-control]
            [tolglow.persistence :as persistence]
            [tolglow.show :as show]
            [tolglow.spatial.cues :as spatial]
            [tolglow.surface.link :as surface]
            [tolglow.studio :as studio]
            [tolglow.studio.web :as web]
            [tolglow.transit :as transit])
  (:import [java.io StringReader]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- model []
  (let [head {:id :head :universe 1 :address 1 :channels {:dimmer 1}}
        cues [[:first 0 {:dimmer 0.25}]
              [:between 1 {:dimmer 0.5}]
              [:last 2 [{:attribute :dimmer :blend :add :value 0.25}]]
              [:transform 3 {:dimmer 0}]]]
    (reduce (fn [m [id priority outputs]]
              (-> m
                  (assoc-in [:registry :effects id] {:id id :version 1 :params {} :outputs outputs})
                  (assoc-in [:registry :cues id] {:id id :version 1 :effect-id id :priority priority})))
            (control/new-show {:rig {:fixtures [head]}
                               :pages {:front {:id :front :cells {[0 0] :last}}
                                       :encore {:id :encore :cells {[0 0] :last}}}})
            cues)))

(def grouping
  {:manual [{:id :mix :cue-ids [:first]
             :groups [{:id :accent :cue-ids [:last]}]}]
   :overlays {:cue-pages [{:id :front :page-ids [:front] :level 0.5}
                          {:id :encore :page-ids [:encore] :level 0.25}]}})

(defn- submit! [runtime time payload]
  (let [receipt (show/submit! runtime (merge {:source {:adapter :repl :session-id "groups-test"}} payload))]
    (show/tick! runtime time)
    (deref receipt 1000 ::missing)))

(defn- accepted! [runtime time op]
  (let [result (submit! runtime time op)]
    (is (= :accepted (:status result)) (pr-str result))
    result))

(defn- launch! [runtime time cue]
  (:instance-id (accepted! runtime time {:op :cue/launch :cue-id cue})))

(defn- level [id value] {:op :effect-groups/level-set :channel-id id :value value})

(defn- configure [config] {:op :effect-groups/configure :config config})

(defn- committed-model [runtime] (:model @(:state runtime)))

(defn- dmx-bytes [runtime n]
  (let [{:keys [output status]} @(:state runtime)]
    (is (nil? (:error status)) (pr-str status))
    (mapv #(bit-and 255 %) (take n (get-in output [:universes 1])))))

(defn- dmx [runtime] (first (dmx-bytes runtime 1)))

(deftest studio-projects-group-channels-from-the-same-committed-frame
  (let [runtime (show/create-runtime {:model (model) :workspace-options nil})]
    (try
      (accepted! runtime 0 (configure (assoc grouping :active-overlay :cue-pages)))
      (let [instance-id (launch! runtime 1 :last)
            before @(:state runtime)
            receipt (accepted! runtime 2 (level [:manual :accent] 0.4))
            snapshot (show/snapshot runtime)
            query {"runtime-id" (:runtime-id snapshot) "epoch" (str (:epoch snapshot))
                   "revision" (str (:revision snapshot))}
            current (:effect-group-channels (studio/stream-view runtime @(:state runtime)))
            previous (:effect-group-channels (studio/stream-view runtime before))]
        (is (= (:revision receipt) (:revision snapshot)))
        (is (= current (show/effect-group-channels runtime)
               (:effect-group-channels (studio/browser-snapshot runtime query))))
        (is (= {:manual-path [:mix :accent] :overlay-candidates [:front :encore]
                :overlay-group :front :member-level 0.2}
               (select-keys (first (:instances current)) [:manual-path :overlay-candidates :overlay-group :member-level])))
        (is (= [instance-id] (get-in current [:overlay 1 :matching-instance-ids])))
        (is (= [] (get-in current [:overlay 1 :instance-ids])))
        (is (= 1.0 (get-in previous [:manual 0 :groups 0 :level])))
        (is (= 0.4 (get-in current [:manual 0 :groups 0 :level]))))
      (finally (show/stop! runtime)))))

(deftest fixture-overlay-follows-the-definition-rendered-on-the-reload-frame
  (doseq [policy [:preserve-state :restart], batch? [false true]]
    (let [definition {:id :first :version 1 :params {} :targets [:head] :outputs {:dimmer 1}}
          rig [{:id :head :universe 1 :address 1 :channels {:dimmer 1}}
               {:id :muted :universe 1 :address 2 :channels {:dimmer 1}}]
          model (-> (model) (assoc-in [:rig :fixtures] rig) (assoc-in [:registry :effects :first] definition))
          runtime (show/create-runtime {:model model :batch? batch? :workspace-options nil :workers 2
                                        :effect-log! (fn [_])})
          publish! (fn [time version targets overrides]
                     (accepted! runtime time {:op :registry/publish :kind :effects
                                              :definition (merge definition overrides {:version version :targets targets})}))]
      (try
        (accepted! runtime 0 (configure {:active-overlay :fixtures
                                          :overlays {:fixtures [{:id :silent :fixture-ids [:muted] :level 0}]}}))
        (let [id (:instance-id (accepted! runtime 10 {:op :cue/launch :cue-id :first :launch {:reload-policy policy}}))
              instance #(get-in (committed-model runtime) [:instances id])
              overlay #(get-in (show/effect-group-channels runtime) [:instances 0 :overlay-group])]
          (is (= [255 0] (dmx-bytes runtime 2)))
          (publish! 20 2 [:muted] {})
          (is (= 2 (:effect-version (instance))))
          (is (= :silent (overlay)))
          (is (= [0 0] (dmx-bytes runtime 2)))
          (is (= (if (= :restart policy) 20 10) (:started-at-ms (instance))))
          (publish! 30 3 [:head] {:params {:divisor {:type :number :default 0}}
                                  :outputs {:dimmer {:op :divide :args [1 {:op :param :key :divisor}]}}})
          (is (= 2 (:effect-version (instance))))
          (is (= {:effect-version 3 :stage :evaluate} (select-keys (:reload-fault (instance)) [:effect-version :stage])))
          (is (= :silent (overlay)))
          (is (= [0 0] (dmx-bytes runtime 2)))
          (publish! 40 4 [:head] {})
          (is (= 4 (:effect-version (instance))))
          (is (nil? (overlay)))
          (is (= [255 0] (dmx-bytes runtime 2)))
          (is (= (if (= :restart policy) 40 10) (:started-at-ms (instance)))))
        (finally (show/stop! runtime))))))

(defn- studio-post [runtime time]
  (let [app (web/app {:heartbeat! (constantly nil)
                      :cue-variation-preview! #(show/cue-variation-preview runtime %)
                      :submit! (fn [operation payload]
                                 (let [receipt (show/submit! runtime operation payload)]
                                   (show/tick! runtime (swap! time inc))
                                   receipt))})]
    (fn [uri media encode payload]
      (let [response (app {:request-method :post :uri uri
                           :headers {"host" "localhost" "accept" "application/transit+json"
                                     "content-type" media}
                           :body (StringReader. (encode payload))})]
        [(:status response) (transit/decode (:body response))]))))

(deftest xtouch-faders-use-committed-group-levels-across-banks-and-reconnect
  (let [effect [:registry :effects :first]
        model (-> (model)
                  (assoc-in (conj effect :params :level) {:type :number :default 0.5 :bounds [0 1]})
                  (assoc-in (conj effect :outputs :dimmer) {:op :param :key :level}))
        runtime (show/create-runtime {:model model :workspace-options nil})
        config (update grouping :manual into
                       (mapv #(hash-map :id (keyword (str "spare-" %))) (range 7)))
        time (atom 0)
        receiver (atom nil)
        sent (atom [])
        transport {:open! (fn [receive!] (reset! receiver receive!) :captured)
                   :send! (fn [_ batch] (swap! sent into batch)) :close! (fn [_])}
        submit! show/submit!
        input! (fn [bytes]
                 (with-redefs [show/now-ms (constantly @time)
                               show/submit! (fn [owner command]
                                              (let [receipt (submit! owner :command command nil)]
                                                (show/tick! owner (swap! time inc))
                                                receipt))]
                   (@receiver bytes)))
        mixer #(show/effect-group-channels runtime)
        motors #(filterv (fn [bytes] (= % (first bytes))) @sent)
        post! (studio-post runtime time)]
    (try
      (accepted! runtime @time (configure config))
      (let [device (show/connect-device! runtime :x-touch
                                         {:session-id "group-mcu" :effect-group-view :manual :schedule! nil
                                          :transport transport :channels [{:cue-id :first}]})
            ;; The bank is the surface's committed window (#705).
            bank #(surface/strip-offset (committed-model runtime) "group-mcu")
            ack #(:last-ack (device-session/snapshot device))
            first-id (launch! runtime (swap! time inc) :first)
            last-id (launch! runtime (swap! time inc) :last)]
        (is (some #{[224 127 127]} @sent))
        (device-session/refresh! device)
        (is (some #{[144 25 127]} @sent) "Nested group member lights its strip")
        (is (= 191 (dmx runtime)))
        (input! [225 0 0])
        (is (= 1.0 (get-in (mixer) [:manual 0 :groups 0 :level])))
        (input! [144 105 127])
        (reset! sent [])
        (input! [225 0 0])
        (is (= [:manual :accent] (:channel-id (ack))))
        (is (= 0.0 (get-in (mixer) [:manual 0 :groups 0 :level])))
        (is (= 128 (dmx runtime)))
        (is (empty? (motors 225)))
        (input! [128 105 0])
        (is (= [[225 0 0]] (motors 225)))
        (input! [144 104 127])
        (input! [144 47 127])
        (is (= 8 (bank)))
        (reset! sent [])
        (input! [144 104 127])
        (input! [224 0 0])
        (is (= [:manual :mix] (:channel-id (ack))))
        (is (= [0.0 1.0] (mapv :level ((juxt first peek) (:manual (mixer))))))
        (is (empty? (motors 224)))
        (input! [128 104 0])
        (is (= [[224 127 127]] (motors 224)) "Release reasserts the new bank even at the cached level")
        (input! [144 105 127])
        (input! [225 127 127])
        (input! [128 105 0])
        (is (= 1.0 (get-in (committed-model runtime) [:instances first-id :params :level])))
        (input! [144 46 127])
        (is (= 0 (bank)))
        (input! [144 104 127])
        (reset! sent [])
        (is (= 200 (first (post! "/api/command" "application/transit+json" transit/encode
                                 (level [:manual :mix] 0.5)))))
        (is (= 128 (dmx runtime)))
        (device-session/refresh! device)
        (is (empty? (motors 224)))
        (input! [128 104 0])
        (is (= [[224 0 64]] (motors 224)))
        (accepted! runtime (swap! time inc) {:op :instance/end :instance-id last-id :force? true})
        (reset! sent [])
        (device-session/refresh! device)
        (is (some #{[144 25 0]} @sent))
        (is (= [:manual :accent] (get-in (mixer) [:manual 0 :groups 0 :id])))
        (input! [144 104 127])
        (device-session/suspend! device)
        (reset! sent [])
        (input! [224 127 127])
        (is (empty? @sent))
        (device-session/resume! device)
        (is (= [[224 0 64]] (motors 224)))
        (input! [224 127 127])
        (let [old @receiver]
          (device-session/reconnect! device)
          (old [144 104 127])
          (input! [224 127 127])
          (is (= 0.5 (get-in (mixer) [:manual 0 :level]))))
        (is (empty? (:errors (device-session/snapshot device)))))
      (show/disconnect-device! runtime "group-mcu")
      (accepted! runtime (swap! time inc) {:op :effect-groups/overlay-set :overlay :cue-pages})
      (let [device (show/connect-device! runtime :x-touch
                                         {:session-id "group-mcu" :effect-group-view :overlay
                                          :schedule! nil :transport transport})]
        (input! [144 104 127])
        (input! [224 0 0])
        (input! [128 104 0])
        (is (= [:cue-pages :front] (get-in (device-session/snapshot device) [:last-ack :channel-id])))
        (is (= [0.0 0.25] (mapv :level (:overlay (mixer)))))
        (is (= 0.5 (get-in (mixer) [:manual 0 :level]))))
      (finally (show/stop! runtime)))))

(deftest studio-json-commands-address-committed-groups
  (let [runtime (show/create-runtime {:model (model) :workspace-options nil})
        time (atom 0)
        post! (studio-post runtime time)
        command! #(post! "/api/command" "application/json" json/write-str %)
        typed! #(post! "/api/command" "application/transit+json" transit/encode %)]
    (try
      (let [id (launch! runtime 0 :last)
            config {:manual [{:id "looks/mix"}]
                    :overlays {:fixtures [{:id "front" :fixture-ids ["head"]}]}
                    :active-overlay "fixtures"}
            level-command {:op "effect-groups/level-set" :channel-id ["manual" "looks/mix"]
                           :command-id "mix-level" :value 0.5}]
        (is (= :fixtures (get-in (command! {:op "effect-groups/configure" :config config}) [1 :active-overlay])))
        (let [first-level (command! level-command)]
          (is (= [200 [:manual :looks/mix]] [(first first-level) (get-in first-level [1 :channel-id])]))
          (is (= 0.5 (get-in (show/effect-group-channels runtime) [:manual 0 :level])))
          (is (= [:fixtures :front]
                 (get-in (command! {:op "effect-groups/level-set" :channel-id ["fixtures" "front"] :value 0.25})
                         [1 :channel-id])))
          (is (= :looks/mix (get-in (command! {:op "effect-groups/assign-instance" :instance-id id :group-id "looks/mix"})
                                  [1 :group-id])))
          (is (= [:looks/mix] (get-in (show/effect-group-channels runtime) [:instances 0 :manual-path])))
          (is (= 200 (first (command! {:op "effect-groups/assign-instance" :instance-id id :group-id nil}))))
          (is (= [] (get-in (show/effect-group-channels runtime) [:instances 0 :manual-path])))
          (is (= 200 (first (command! {:op "effect-groups/overlay-set" :overlay nil}))))
          (is (= :fixtures (get-in (command! {:op "effect-groups/overlay-set" :overlay "fixtures"}) [1 :active-overlay])))
          (is (= 200 (first (command! {:op "effect-groups/configure" :config-patch {:active-overlay nil}}))))
          (is (= :fixtures (get-in (command! {:op "effect-groups/configure" :config-patch {:active-overlay "fixtures"}})
                                  [1 :active-overlay])))
          (is (= :fixtures (get-in (post! "/api/settings" "application/json" json/write-str
                                          {:scope "effect-group-channels" :values {:active-overlay "fixtures"}})
                                  [1 :active-overlay])))
          (accepted! runtime (swap! time inc) (configure {:manual [{:id "looks/mix"}]}))
          (is (= first-level (command! level-command)))
          (is (= 1.0 (get-in (show/effect-group-channels runtime) [:manual 0 :level])))
          (is (= [:manual "looks/mix"] (get-in (command! (assoc level-command :command-id "string-mix-level"))
                                            [1 :channel-id])))
          (is (= "looks/mix" (get-in (command! {:op "effect-groups/assign-instance" :instance-id id :group-id "looks/mix"})
                                   [1 :group-id])))
          (is (= 200 (first (typed! (level [:manual "looks/mix"] 0.65)))))
          (is (= :unknown-group (get-in (typed! (level [:manual :looks/mix] 0.75)) [1 :code])))
          (is (= :invalid-value (get-in (typed! (level ["manual" "looks/mix"] 0.75)) [1 :code])))
          (is (= 0.65 (get-in (show/effect-group-channels runtime) [:manual 0 :level])))))
      (finally (show/stop! runtime)))))

(deftest studio-json-cue-page-selectors-use-exact-ids-before-keywords
  (let [runtime (show/create-runtime {:model (model) :workspace-options nil})
        time (atom 0)
        post! (studio-post runtime time)
        command! #(post! "/api/command" "application/json" json/write-str %)
        typed! #(post! "/api/command" "application/transit+json" transit/encode %)
        config (fn [id] {:active-overlay :cue-pages
                         :overlays {:cue-pages [{:id :selected :page-ids [id] :level 0}]}})
        command {:op "effect-groups/configure" :command-id "page-group" :config (config "front")}
        ids #(get-in (committed-model runtime) [:settings :effect-group-channels :overlays :cue-pages 0 :page-ids])
        selected #(get-in (show/effect-group-channels runtime) [:instances 0 :overlay-group])]
    (try
      (launch! runtime 0 :last)
      (let [receipt (command! command)]
        (is (= 200 (first receipt)))
        (is (= [:front] (ids)))
        (is (= :selected (selected)))
        (is (= 0 (dmx runtime)))
        (accepted! runtime (swap! time inc) {:op :page/put :page {:id "front" :cells {[0 0] :first}}})
        (is (= receipt (command! command)))
        (is (= [:front] (ids))))
      (is (= 200 (first (command! {:op "effect-groups/configure" :config-patch (config "front")}))))
      (is (= ["front"] (ids)))
      (is (nil? (selected)))
      (is (= 64 (dmx runtime)))
      (is (= 200 (first (post! "/api/settings" "application/json" json/write-str
                              {:scope :effect-group-channels :values (config "encore")}))))
      (is (= [:encore] (ids)))
      (is (= :selected (selected)))
      (is (= 200 (first (typed! (configure (config :front))))))
      (is (= [:front] (ids)))
      (is (= :selected (selected)))
      (accepted! runtime (swap! time inc) (configure (config "front")))
      (is (= ["front"] (ids)))
      (is (nil? (selected)))
      (finally (show/stop! runtime)))))

(deftest restored-cue-variations-keep-the-assigned-group-level
  (let [effect [:registry :effects :first]
        model (-> (model)
                  (assoc-in (conj effect :params :level) {:type :number :default 0.5 :bounds [0 1]})
                  (assoc-in (conj effect :outputs :dimmer) {:op :param :key :level}))
        runtime (show/create-runtime {:model model :workspace-options nil})
        time (atom 0)
        post! (studio-post runtime time)
        json! #(post! %1 "application/json" json/write-str %2)
        command! #(post! "/api/command" "application/transit+json" transit/encode %)
        preview! (fn [id]
                   (let [[status preview] (json! "/api/cue-variations/preview" {:instance-id id :layout "rows"})]
                     (is (= [200 :rows] [status (:layout preview)]))
                     (select-keys preview [:basis :layout :seed :row-page :instance-id])))]
    (try
      (let [id (launch! runtime 0 :first)
            group-level! #(json! "/api/command" {:op "effect-groups/level-set" :channel-id ["manual" "mix"] :value %})]
        (is (= 200 (first (command! (configure {:manual [{:id :mix}]})))))
        (is (= 200 (first (json! "/api/command" {:op "effect-groups/assign-instance"
                                               :instance-id id :group-id "mix"}))))
        (is (= 200 (first (group-level! 0.5))))
        (is (= 64 (dmx runtime)))
        (let [[status receipt] (command! (assoc (preview! id) :op :cue-variation/append))]
          (is (= [200 0] [status (:slot receipt)])))
        (is (= 200 (first (command! {:op :param/set :instance-id id :param :level :value 1.0}))))
        (is (= 128 (dmx runtime)))
        (let [source (show/export-show-source runtime)]
          (is (= 200 (first (group-level! 0))))
          (is (= 0 (dmx runtime)))
          (let [restore (show/submit! runtime :import {:source source})]
            (show/tick! runtime (swap! time inc))
            (is (= :accepted (:status (deref restore 1000 ::missing)))))
          (let [{:keys [instance-id manual-path member-level]} (first (:instances (show/effect-group-channels runtime)))]
            (is (not= id instance-id))
            (is (= [[:mix] 0.5] [manual-path member-level]))
            (is (= 128 (dmx runtime)))
            (is (= 200 (first (command! (assoc (preview! instance-id) :op :cue-variation/apply :position [0 0])))))
            (is (= 0.5 (get-in (committed-model runtime) [:instances instance-id :params :level])))
            (is (= 64 (dmx runtime))))))
      (finally (show/stop! runtime)))))

(deftest group-levels-use-owner-receipts-and-preserve-interleaved-output
  (doseq [batch? [true false]]
    (let [runtime (show/create-runtime {:model (model) :batch? batch? :workspace-options nil :workers 2})]
      (try
        (let [last-id (launch! runtime 0 :last)
              _ (launch! runtime 10 :first)
              _ (launch! runtime 20 :between)
              base (dmx runtime)]
          (is (= 191 base))
          (accepted! runtime 30 (configure grouping))
          (is (= base (dmx runtime)))
          (let [command (assoc (level [:manual :mix] 0.5) :command-id "mix-level"
                               :expected-revision (:revision (committed-model runtime)))
                accepted (accepted! runtime 40 command)
                revision (:revision (committed-model runtime))]
            (is (= {:status :accepted :revision revision :channel-id [:manual :mix] :level 0.5}
                   (select-keys accepted [:status :revision :channel-id :level])))
            (is (= 159 (dmx runtime)))
            (is (= accepted (submit! runtime 50 command)))
            (is (= revision (:revision (committed-model runtime))))
            (is (= :command-id-conflict (:code (submit! runtime 60 (assoc command :value 0.25)))))
            (is (= :stale-revision (:code (submit! runtime 70 (assoc command :command-id "stale-revision")))))
            (is (= :invalid-value (:code (submit! runtime 80 (level [:manual :mix] 2)))))
            (is (= revision (:revision (committed-model runtime)))))
          (accepted! runtime 90 {:op :effect-groups/overlay-set :overlay :cue-pages})
          (is (= 143 (dmx runtime)))
          (let [row (some #(when (= last-id (:instance-id %)) %) (:instances (show/effect-group-channels runtime)))]
            (is (= {:manual-path [:mix :accent] :overlay-candidates [:front :encore] :overlay-group :front
                    :member-level 0.25 :opacity 1.0}
                   (select-keys row [:manual-path :overlay-candidates :overlay-group :member-level :opacity]))))
          (accepted! runtime 100 {:op :instance/opacity-set :instance-id last-id :value 0.5})
          (is (= 135 (dmx runtime)))
          (is (= 0.5 (get-in (committed-model runtime) [:instances last-id :system :opacity]))))
        (finally (show/stop! runtime))))))

(deftest group-opacity-runs-before-a-later-native-member-transform
  (let [runtime (show/create-runtime {:model (model) :batch? true :workspace-options nil :workers 2})]
    (try
      (launch! runtime 0 :first)
      (accepted! runtime 10 {:op :cue/launch :cue-id :between
                              :source {:adapter :repl :native-layer {:id :compound :member 0 :opacity 0.5}}})
      (accepted! runtime 20 {:op :cue/launch :cue-id :transform
                              :source {:adapter :repl
                                       :native-layer {:id :compound :member 1 :opacity 0.5 :targets :previous
                                                      :transforms [{:attribute :dimmer
                                                                    :value {:op :multiply :args [{:op :previous :attribute :dimmer} 2]}}]}}})
      (is (= 159 (dmx runtime)))
      (accepted! runtime 30 (configure {:manual [{:id :base :cue-ids [:between]}]}))
      (is (= 159 (dmx runtime)))
      (accepted! runtime 40 (level [:manual :base] 0.5))
      (is (= 128 (dmx runtime)))
      (accepted! runtime 50 (level [:manual :base] 0))
      (is (= 96 (dmx runtime)))
      (finally (show/stop! runtime)))))

(deftest group-levels-and-instance-placement-survive-standalone-show-restore
  (let [runtime (show/create-runtime {:model (model) :batch? false :workspace-options nil})
        file (Files/createTempFile "tolglow-mixer-" ".transit" (make-array FileAttribute 0))]
    (try
      (launch! runtime 0 :first)
      (launch! runtime 10 :between)
      (let [old-id (launch! runtime 20 :last)]
        (accepted! runtime 30 (configure grouping))
        (accepted! runtime 40 (level [:manual :mix] 0.5))
        (accepted! runtime 50 (level [:manual :accent] 0.25))
        (accepted! runtime 60 {:op :effect-groups/assign-instance :instance-id old-id :group-id :mix})
        (accepted! runtime 70 {:op :effect-groups/overlay-set :overlay :cue-pages})
        (let [actual (dmx runtime)
              old-epoch (:epoch (committed-model runtime))
              path (str file)]
          (is (= 143 actual))
          (show/save-show! runtime path)
          (accepted! runtime 80 (level [:manual :mix] 0))
          (is (= 128 (dmx runtime)))
          (let [receipt (show/load-show! runtime path)]
            (show/tick! runtime 90)
            (is (= :accepted (:status (deref receipt 1000 {})))))
          (is (= actual (dmx runtime)))
          (let [projected (show/effect-group-channels runtime)
                row (some #(when (= :last (:cue-id %)) %) (:instances projected))]
            (is (< old-epoch (:epoch (committed-model runtime))))
            (is (not= old-id (:instance-id row)))
            (is (= [:mix] (:manual-path row)))
            (is (= :front (:overlay-group row)))
            (is (= 0.25 (:member-level row)))
            (is (= 0.25 (get-in projected [:manual 0 :groups 0 :level]))))
          (is (= :stale-epoch (:code (submit! runtime 100 (assoc (level [:manual :mix] 0) :epoch old-epoch)))))
          (is (= actual (dmx runtime)))))
      (finally (show/stop! runtime)
               (Files/deleteIfExists file)))))

(deftest show-files-reject-invalid-groups-and-stale-manual-placements
  (let [runtime (show/create-runtime {:model (model) :workspace-options nil})]
    (try
      (let [id (launch! runtime 0 :last)
            config {:manual [{:id :mix}]}]
        (accepted! runtime 10 (configure config))
        (accepted! runtime 20 {:op :effect-groups/assign-instance :instance-id id :group-id :mix})
        (let [receipt (show/submit! runtime :settings {:scope :effect-group-channels
                                                       :values {:manual [{:id :bad :level 2}]}})]
          (show/tick! runtime 25)
          (is (= :invalid-group (:code (deref receipt 1000 {})))))
        (let [patch {:scope :effect-group-channels :command-id "group-settings" :values {:manual (:manual config)}}
              apply-patch! (fn [time]
                             (let [receipt (show/submit! runtime :settings patch)]
                               (show/tick! runtime time)
                               (deref receipt 1000 {})))
              applied (apply-patch! 26)]
          (is (= :accepted (:status applied)))
          (accepted! runtime 27 (level [:manual :mix] 0.75))
          (is (= applied (apply-patch! 28)))
          (is (= 0.75 (get-in (show/effect-group-channels runtime) [:manual 0 :level]))))
        (let [saved (committed-model runtime)
              error-path (fn [model]
                           (try (persistence/freeze model) nil
                                (catch clojure.lang.ExceptionInfo error
                                  (get-in (ex-data error) [:errors 0 :path]))))]
          (is (= [:settings :effect-group-channels]
                 (error-path (assoc-in saved [:settings :effect-group-channels :manual 0 :level] 1.01))))
          (is (= [:instances id :tolglow.effects.group-channels/manual-group]
                 (error-path (assoc-in saved [:instances id :tolglow.effects.group-channels/manual-group] :stale)))))
        (accepted! runtime 30 (configure {}))
        (accepted! runtime 40 (configure config))
        (is (= [] (get-in (show/effect-group-channels runtime) [:instances 0 :manual-path]))))
      (finally (show/stop! runtime)))))

(deftest spatial-masks-retain-the-group-factor-once
  (let [rig (get-in (model) [:rig :fixtures])
        mask {"0:1" {:id :spatial :member 0 :member-opacity 1.0 :opacity 0.5 :target-weights {:head 1.0}}}
        block {:kind :assignments :source {:instance-id "0:1"}
               :assignments [{:target-id :head :attribute :dimmer :value 1.0}]}
        native (assoc block :native-layer {:id :native :member 0 :member-opacity 0.5})
        run (fn [b masks factors]
              (let [prepared (spatial/mask-contributions {:contributions [b]} masks factors)]
                (get-in (render/resolve-contributions rig (:contributions prepared)) [:values :head :channels :dimmer])))]
    (is (== 0.5 (run native mask nil)))
    (is (== 0.125 (run native mask {"0:1" 0.25})))
    (is (== 0.125 (run block mask {"0:1" 0.25})))
    (is (== 0.0625 (run native (update mask "0:1" dissoc :member-opacity) {"0:1" 0.25})))))

(defn- spatial-runtime [batch? channels outputs]
  (show/create-runtime {:batch? batch? :workspace-options nil
                        :model (-> (model)
                                   (assoc-in [:rig :fixtures 0 :position] [0 1 0])
                                   (assoc-in [:rig :fixtures 0 :channels] channels)
                                   (assoc-in [:registry :effects :transform :outputs] outputs))}))

(defn- spawn-spatial! [runtime time paint member]
  (let [receipt (show/submit! runtime :spatial-cue
                             {:op :spawn :id :region :priority 0 :paint paint
                              :volume {:shape :sphere :position [0 1 0] :radius 1 :selection #{:position}}
                              :members [(assoc member :cue-id :transform)]})]
    (show/tick! runtime time)
    (let [ack (deref receipt 1000 ::missing)]
      (is (= :accepted (:status ack)) (pr-str ack)))))

(deftest fixture-overlay-uses-the-reloaded-version-of-a-spatial-child
  (let [definition {:id :transform :version 1 :params {} :targets [:head] :outputs {:dimmer 1}}
        rig [{:id :head :universe 1 :address 1 :position [0 1 0] :channels {:dimmer 1}}
             {:id :muted :universe 1 :address 2 :position [0 1 0] :channels {:dimmer 1}}]
        model (-> (model)
                  (assoc-in [:rig :fixtures] rig)
                  (assoc-in [:registry :effects :transform] definition)
                  (assoc-in [:registry :cues :transform :reload-policy] :preserve-state))
        runtime (show/create-runtime {:model model :workspace-options nil :batch? true})]
    (try
      (accepted! runtime 0 (configure {:active-overlay :fixtures
                                        :overlays {:fixtures [{:id :silent :fixture-ids [:muted] :level 0}]}}))
      (spawn-spatial! runtime 10 {:dimmer 0.2} {})
      (is (= [255 0] (dmx-bytes runtime 2)))
      (accepted! runtime 20 {:op :registry/publish :kind :effects
                             :definition (assoc definition :version 2 :targets [:muted])})
      (is (= 2 (some #(when (= :transform (:cue-id %)) (:effect-version %))
                     (vals (:instances (committed-model runtime))))))
      (is (= [0 0] (dmx-bytes runtime 2)))
      (finally (show/stop! runtime)))))

(deftest spatial-child-group-level-fades-its-assignment-and-transform-once
  (doseq [batch? [false true]]
    (let [runtime (spatial-runtime batch? {:dimmer 1 :focus 2} {:dimmer 0.2 :focus 0.7})
          focus-byte #(bit-and 255 (second (get-in @(:state runtime) [:output :universes 1])))]
      (try
        (spawn-spatial! runtime 0 {:dimmer 0.2 :focus 0.2}
                        {:targets :previous :transforms [{:attribute :dimmer :value 0.8}]})
        (let [child (some #(when (= :transform (:cue-id %)) %) (vals (:instances (committed-model runtime))))]
          (is (= :spatial-cue (get-in child [:source :adapter])))
          (is (nil? (get-in child [:source :native-layer])))
          (is (nil? (get-in child [:definition :layer]))))
        (is (= [204 179] [(dmx runtime) (focus-byte)]))
        (accepted! runtime 10 (configure {:manual [{:id :child :cue-ids [:transform]}]}))
        (is (= [204 179] [(dmx runtime) (focus-byte)]))
        (accepted! runtime 20 (level [:manual :child] 0.5))
        (is (= [128 115] [(dmx runtime) (focus-byte)]))
        (finally (show/stop! runtime))))))

(deftest zero-level-spatial-child-keeps-the-layer-after-an-interleaved-effect
  (doseq [batch? [false true]]
    (let [runtime (spatial-runtime batch? {:dimmer 1} {:dimmer 0.6})]
      (try
        (launch! runtime 0 :between)
        (is (= 128 (dmx runtime)))
        (spawn-spatial! runtime 10 {:dimmer 0.8} {})
        (is (= 153 (dmx runtime)))
        (accepted! runtime 20 (configure {:manual [{:id :child :cue-ids [:transform]}]}))
        (is (= 153 (dmx runtime)))
        (accepted! runtime 30 (level [:manual :child] 0.000001))
        (is (= 204 (dmx runtime)))
        (accepted! runtime 40 (level [:manual :child] 0))
        (is (= 204 (dmx runtime)))
        (accepted! runtime 50 (level [:manual :child] 1))
        (is (= 153 (dmx runtime)))
        (finally (show/stop! runtime))))))

(deftest grouped-spatial-fog-yields-the-shared-pump-at-zero
  (let [runtime (spatial-runtime false {:dimmer 1 :fog 2} {:fog 0.7})
        frame fog-control/frame
        fallback {:active? true :mode :automatic :demand 0.4}
        pump (atom nil)]
    (try
      (with-redefs [fog-control/frame
                    (fn [model patch observation previous now bypass? blocks music]
                      (reset! pump (get-in (fog-control/arbitrate {:enabled? true :pumps {:head fallback}} blocks)
                                           [:pumps :head]))
                      (frame model patch observation previous now bypass? blocks music))]
        (spawn-spatial! runtime 0 {:dimmer 0.2} {})
        (is (= :manual (:mode @pump)))
        (accepted! runtime 10 (configure {:manual [{:id :child :cue-ids [:transform]}]}))
        (is (= :manual (:mode @pump)))
        (accepted! runtime 20 (level [:manual :child] 0.5))
        (is (= :manual (:mode @pump)))
        (accepted! runtime 30 (level [:manual :child] 0))
        (is (= fallback @pump)))
      (finally (show/stop! runtime)))))
