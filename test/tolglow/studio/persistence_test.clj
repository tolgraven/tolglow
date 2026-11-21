(ns tolglow.studio.persistence-test
  (:require [clojure.test :refer :all]
            [afterglow.control.model :as control]
            [tolglow.studio.transit :as wire]
            [thi.ng.color.core :as color]
            [afterglow.runtime.engine :as engine]
            [afterglow.render.frame :as render]
            [afterglow.time :as time]
            [tolglow.compiler :as compiler]
            [tolglow.automation.policy :as policy]
            [tolglow.integrations.ableton :as ableton]
            [tolglow.studio.composition :as composition]
            [tolglow.studio.demo :as demo]
            [tolglow.studio.agents :as agents]
            [tolglow.studio.fixtures :as fixture-library]
            [tolglow.studio.persistence :as files]))

(defn command [model n payload]
  (let [result (control/apply-command model
                  (merge {:show-id (:show-id model) :epoch (:epoch model) :command-id (str n)
                          :time-ms (:time-ms model) :source {:adapter :studio :session-id "old-browser"}}
                         payload))]
    (is (= :accepted (get-in result [:ack :status])) (pr-str (:ack result)))
    result))

(defn rich-model []
  (let [model (-> (demo/model) (assoc :time-ms 4000)
                  (assoc-in [:transport :time-ms] 4000) (assoc-in [:transport :beat] 8))
        launched (command model 1 {:op :cue/launch :cue-id :ember
                                   :launch {:mode :timed :duration {:bars 2} :params {:level 0.4}}})
        id (get-in launched [:ack :instance-id])
        model (-> (:model launched)
                  (assoc-in [:instances id :state] {:counter 12 :path [1/3 0.25] :flags #{:a :b}})
                  (assoc-in [:instances id :source] {:adapter :auto :session-id "auto-before"}))
        published (command model 2 {:op :registry/publish :kind :effects
                                    :definition (-> (get-in model [:registry :effects :ember])
                                                    (assoc :version 2 :name "Edited ember"))})
        macro {:id :stack :mode :macro :slots [{:id :one :cue-id :orchid :params {}}]}
        sequence {:id :steps :mode :sequence :loop :loop :timebase :beats
                  :slots [{:id :a :cue-id :ice :params {} :duration {:beats 1}}
                          {:id :b :cue-id :sage :params {} :duration {:beats 2}}]}
        model (-> (:model published) (composition/save macro) (composition/start :stack 4000)
                  (composition/save sequence) (composition/start :steps 4000) (composition/advance 4000))
        held (command model 3 {:op :cue/launch :cue-id :spot :launch {:mode :held}})
        bridge (-> (ableton/new-bridge {:mappings {36 {:cue-id :ember :mode :held}}
                                       :parameters {12 {:instance-id id :cue-id :ember :param :level :bounds [0 1]}}})
                   (assoc :instances {id :ember "0:ended" :ember}
                          :held {36 {:instance-id id :command-id "pending"}}
                          :clip-links {:ember {:track-id "lighting" :clip-id "verse"}}
                          :recorded #{[:main 0 "old"]} :record-order [[:main 0 "old"]]))
        auto (assoc (policy/initial-state 12345 4000 "auto-before")
                    :tick 7 :epoch 0 :owned-instance-ids [id "0:ended"]
                    :interval-timer (time/start {:bars 2} {:beat 8 :time-ms 4000 :meter time/default-meter}))]
    (assoc (:model held)
           :auto {:enabled? true :config {:interval {:bars 2} :count 1 :scope :all} :state auto}
           :integrations {:ableton bridge :audio {:input-gain 0.8} :extensions {:vendor/mapping {:version 3}}}
           :responses [{:id :response :select {:instance-ids [id]} :params [:level]
                        :envelope {:attack {:beats 1}} :state {:started-at-ms 4000 :started-at-beat 8}}]
           :variables {:palette :winter :fraction 1/3}
           :extensions {:vendor/plugin {:opaque-id id :preferences {:theme "dim"}}})))

(defn restored [model] (files/thaw (assoc (demo/model) :epoch 9 :revision 200 :time-ms 9000) (files/freeze model)))

(deftest cue-presets-survive-roundtrip-and-reject-invalid-file-values
  (let [launched (command (demo/model) :preset-launch {:op :cue/launch :cue-id :ember
                                                      :launch {:params {:level 0.37}}})
        saved (:model (command (:model launched) :preset-save
                               {:op :cue/save-params :cue-id :ember :instance-id (get-in launched [:ack :instance-id])}))
        cue (get-in saved [:registry :cues :ember])
        result (restored saved)
        relaunched (command result :preset-relaunch {:op :cue/launch :cue-id :ember})]
    (is (= cue (get-in result [:registry :cues :ember])))
    (is (= 0.37 (get-in relaunched [:model :instances (get-in relaunched [:ack :instance-id]) :params :level])))
    (doseq [bad [nil [] {:level 2} {:missing 1}]]
      (let [document (-> (files/freeze saved)
                         (assoc-in [:model :registry :cues :ember :saved-params] bad)
                         (assoc-in [:model :registry :versions [:cues :ember (:version cue)] :saved-params] bad))]
        (is (thrown? clojure.lang.ExceptionInfo (files/thaw (demo/model) document)))))))

(deftest complete-portable-roundtrip-preserves-musical-progress
  (let [model (rich-model)
        source (files/write-string model)
        document (files/read-string source)
        result (restored model)]
    (is (= model (:model document)))
    (is (= 2 (:schema-version document)))
    (is (= 10 (:epoch result)))
    (is (= 201 (:revision result)))
    (is (= 4000 (:time-ms result)))
    (is (= (:transport model) (:transport result)))
    (is (= (:registry model) (:registry result)))
    (is (= (:pages model) (:pages result)))
    (is (= (:rig model) (:rig result)))
    (is (= (:variables model) (:variables result)))
    (is (= (:extensions model) (:extensions result)))
    (is (= (:compositions model) (:compositions result)))
    (is (= (count (:instances model)) (count (:instances result))))
    (is (= (map :state (sort-by :order (vals (:instances model))))
           (map :state (sort-by :order (vals (:instances result))))))
    (is (= (map :duration-timer (sort-by :order (vals (:instances model))))
           (map :duration-timer (sort-by :order (vals (:instances result))))))
    (is (every? #(clojure.string/starts-with? % "10:") (keys (:instances result))))
    (is (empty? (get-in result [:control :history])))
    (is (empty? (get-in result [:control :history-order])))))

(deftest captured-old-version-survives-a-new-current-definition
  (let [model (rich-model) result (restored model)
        instance (first (filter #(= :ember (:effect-id %)) (vals (:instances result))))]
    (is (= 2 (get-in result [:registry :effects :ember :version])))
    (is (= 1 (:effect-version instance)))
    (is (= 1 (get-in instance [:definition :version])))
    (is (= {:counter 12 :path [1/3 0.25] :flags #{:a :b}} (:state instance)))))

(deftest all-domain-ownership-rekeys-without-external-leases
  (let [model (rich-model) result (restored model)
        ids (set (keys (:instances result)))
        owners (set (map :owner-id (vals (:composition-runs result))))
        auto-state (get-in result [:auto :state])
        held (first (filter #(= :spot (:cue-id %)) (vals (:instances result))))]
    (is (every? ids (vals (:composition-tokens result))))
    (is (every? owners (map first (keys (:composition-tokens result)))))
    (is (every? #(= 10 (:epoch %)) (vals (:composition-runs result))))
    (is (= (get-in result [:composition-runs :steps :owner-id])
           (get-in result [:composition-runs :steps :state :owner-id])))
    (is (= (get-in result [:composition-runs :steps :owner-id])
           (first (get-in result [:composition-runs :steps :state :active-token]))))
    (is (= 12345 (:seed auto-state)))
    (is (= 7 (:tick auto-state)))
    (is (= 10 (:epoch auto-state)))
    (is (= 1 (count (:owned-instance-ids auto-state))))
    (is (every? ids (:owned-instance-ids auto-state)))
    (is (= {:adapter :restore :epoch 10} (:source held)))
    (is (= :held (:launch-mode held)))
    (is (= (get-in model [:auto :state :interval-timer]) (:interval-timer auto-state)))
    (is (every? ids (map :instance-id (:commands (policy/stop-plan result auto-state 4000)))))))

(deftest ableton-links-and-configuration-roundtrip-with-fresh-live-session
  (let [model (rich-model) result (restored model)
        before (get-in model [:integrations :ableton])
        bridge (get-in result [:integrations :ableton])
        parameter-id (get-in bridge [:parameters 12 :instance-id])]
    (is (= (:mappings before) (:mappings bridge)))
    (is (= (:clip-links before) (:clip-links bridge)))
    (is (= {:input-gain 0.8} (get-in result [:integrations :audio])))
    (is (= :ember (get-in result [:instances parameter-id :cue-id])))
    (is (= #{parameter-id} (set (keys (:instances bridge)))))
    (is (= {} (:held bridge)))
    (is (= #{} (:recorded bridge)))
    (is (= [] (:record-order bridge)))
    (is (= [parameter-id] (get-in result [:responses 0 :select :instance-ids])))))

(deftest restored-sequence-continues-at-its-saved-position
  (let [model (restored (rich-model))
        old-id (get (:composition-tokens model) (get-in model [:composition-runs :steps :state :active-token]))
        next-model (composition/advance (assoc-in model [:transport :beat] 9) 4500)
        run (get-in next-model [:composition-runs :steps])
        new-id (get (:composition-tokens next-model) (get-in run [:state :active-token]))]
    (is (= 1 (get-in run [:state :step-index])))
    (is (= :sage (get-in next-model [:instances new-id :cue-id])))
    (is (not= old-id new-id))
    (is (clojure.string/starts-with? new-id "10:"))
    (is (= :releasing (get-in next-model [:instances old-id :phase])))))

(deftest restoring-a-paused-transport-does-not-fast-forward-musical-timers
  (let [model (-> (rich-model) (assoc-in [:transport :playing?] false))
        result (restored model)
        frame (:model (engine/advance result {:time-ms 14000 :transport (:transport result)} compiler/compile-effect))]
    (is (= 8 (get-in frame [:transport :beat])))
    (is (= (count (:instances result)) (count (:instances frame))))
    (is (= :running (:phase (first (filter #(= :ember (:effect-id %)) (vals (:instances frame)))))))))

(deftest restore-epoch-exceeds-both-models-and-input-is-unchanged
  (let [saved (assoc (demo/model) :epoch 23 :revision 40)
        before (pr-str saved)
        result (files/thaw (assoc (demo/model) :epoch 8 :revision 7) (files/freeze saved))]
    (is (= 24 (:epoch result)))
    (is (= 41 (:revision result)))
    (is (= before (pr-str saved)))
    (is (= saved (files/validate-model saved)))))

(deftest invalid-files-never-return-a-partial-model
  (let [model (rich-model) document (files/freeze model) before (pr-str model)]
    (doseq [broken [(assoc document :schema-version 1)
                    (assoc document :unexpected true)
                    (assoc-in document [:model :transport :bpm] 0)
                    (assoc-in document [:model :transport :beat] Double/NaN)
                    (assoc-in document [:model :rig :fixtures 1 :address] 1)
                    (assoc-in document [:model :pages :main :cells [0 0]] :missing)
                    (assoc-in document [:model :registry :effects :ember :outputs :color] {:op :unknown})
                    (assoc-in document [:model :composition-runs :steps :epoch] 99)
                    (assoc-in document [:model :auto :state :seed] 1.2)]]
      (is (thrown? Exception (files/thaw model broken))))
    (is (= before (pr-str model)))))

(deftest rejects-nonportable-state-and-secrets-without-silently-dropping-them
  (let [model (demo/model)]
    (doseq [value [(atom 1) (fn [] 1) (Object.) 'invoke]]
      (is (thrown? Exception (files/freeze (assoc-in model [:variables :bad] value)))))
    (doseq [change [{:provider {:api-key "do-not-persist"}}
                    {:authorization "Bearer secret"}]]
      (is (thrown? Exception (files/freeze (assoc model :integrations change)))))
    (is (thrown? Exception (files/freeze (assoc model :leases {}))))
    (is (thrown? Exception (files/freeze (assoc model :unknown-extension {}))))
    (is (= {:vendor/data {:a [1 2 3]}} (:extensions (:model (files/freeze (assoc model :extensions {:vendor/data {:a [1 2 3]}}))))))))

(deftest connection-descriptors-and-chat-history-are-portable-data
  (let [model (rich-model)
        settings {:version 1 :config {:provider :self-hosted :model "local-model"
                                      :endpoint "http://127.0.0.1:1234/v1/chat/completions"}
                  :credential-ref nil :selected-conversation-id "chat-1"
                  :conversations [{:id "chat-1" :title "Fixture test"
                                   :messages [{:role :user :content "Historical instance 0:1"}
                                              {:role :assistant :content "Ready"}]
                                   :status :completed :updated-at-ms 0.0}]}
        model (-> model
                  (assoc-in [:integrations :agents] settings)
                  (assoc :settings {:ui {:page :main :panel :rig} :beat-detection {:enabled? true}})
                  (assoc-in [:integrations :ableton :device-ports] ["Push 2 User Port"])
                  (assoc-in [:integrations :beat-link] {:peer-id "deck-2" :enabled? true :restore-policy :discover}))
        result (restored model)]
    (is (= (agents/validate-state settings) (get-in result [:integrations :agents])))
    (is (= (:settings model) (:settings result)))
    (is (= ["Push 2 User Port"] (get-in result [:integrations :ableton :device-ports])))
    (is (= (get-in model [:integrations :beat-link]) (get-in result [:integrations :beat-link])))))

(deftest reader-rejects-eval-tags-multiple-forms-and-invalid-schema
  (doseq [source ["#=(System/exit 1)" "#foreign/object {}" "#inst \"2026-01-01\""
                  "{:schema-version 1 :model {}}" "{} {}" ""]]
    (is (thrown? Exception (files/read-string source))))
  (let [source (files/write-string (demo/model))]
    (is (thrown? Exception (files/read-string (str source " {}")))))
  (is (thrown? Exception (files/read-string (String. (char-array (inc files/max-document-characters) \space))))))

(deftest corrupted-captured-definitions-and-timers-reject
  (let [model (rich-model) id (:id (first (sort-by :order (vals (:instances model)))))
        doc (files/freeze model)]
    (doseq [broken [(assoc-in doc [:model :instances id :params :level] 2)
                    (assoc-in doc [:model :instances id :definition :version] 99)
                    (assoc-in doc [:model :instances id :duration-timer :duration :value] 999)
                    (assoc-in doc [:model :instances id :id] "wrong")
                    (assoc-in doc [:model :instances id :state :invalid] (atom 1))]]
      (is (thrown? Exception (files/thaw model broken))))))

(deftest captured-instances-can-outlive-a-deleted-cue
  (let [launch (command (demo/model) 1 {:op :cue/launch :cue-id :ember})
        removed (command (:model launch) 2 {:op :cue/delete :cue-id :ember})
        model (:model removed)
        result (files/thaw model (files/freeze model))]
    (is (nil? (get-in result [:registry :cues :ember])))
    (is (= 1 (count (:instances result))))
    (is (= :ember (:cue-id (first (vals (:instances result))))))))

(deftest committed-frame-can-roundtrip-without-sampling-effects
  (let [model (rich-model)
        rendered (render/resolve-frame (get-in model [:rig :fixtures]) [] {})
        output (-> (select-keys rendered [:values :target-order :universes])
                   (update :universes #(into {} (map (fn [[u bytes]] [u (mapv int bytes)]) %))))
        document (files/freeze model output)
        parsed (files/read-string (files/write-string model output))
        result (files/thaw (demo/model) parsed)]
    (is (= output (:frame-output document)))
    (is (= document parsed))
    (is (= (map :state (sort-by :order (vals (:instances model))))
           (map :state (sort-by :order (vals (:instances result))))))
    (is (= (:time-ms model) (:time-ms result)))
    (is (not (contains? result :frame-output)))))

(deftest invalid-output-rejects-before-a-restored-model-is-returned
  (let [model (demo/model)
        rendered (render/resolve-frame (get-in model [:rig :fixtures]) [] {})
        output (-> (select-keys rendered [:values :target-order :universes])
                   (update :universes #(into {} (map (fn [[u bytes]] [u (mapv int bytes)]) %))))
        document (files/freeze model output)]
    (doseq [bad [(assoc-in document [:frame-output :universes 1 0] 256)
                 (assoc-in document [:frame-output :universes 1 0] -129)
                 (assoc-in document [:frame-output :universes 1] [0])
                 (assoc-in document [:frame-output :universes 2] (vec (repeat 512 0)))
                 (assoc-in document [:frame-output :values :unknown-fixture] {})
                 (assoc-in document [:frame-output :target-order] [])
                 (assoc-in document [:frame-output :universes 1] (byte-array 512))]]
      (is (thrown? Exception (files/thaw model bad))))
    (let [unsigned (update output :universes #(into {} (map (fn [[u bytes]] [u (mapv (fn [b] (bit-and 255 b)) bytes)]) %)))]
      (is (= unsigned (files/validate-frame-output model unsigned))))
    (let [compact-preview (assoc output :values (select-keys (:values output) [:head-1]))]
      (is (= compact-preview (files/validate-frame-output model compact-preview))))))

(deftest reactive-data-restores-without-resolving-or-evaluating-code
  (let [model (rich-model)
        old-id (:id (first (sort-by :order (vals (:instances model)))))
        registry {:registrations
                  [{:id :show/beat :kind :sub
                    :implementation {:type :source :namespace "user" :form "(fn [context query] (:beat context))" :bindings {}}}
                   {:id :show/action :kind :fx
                    :implementation {:type :source :namespace "user" :form "(fn [context value] (throw (Exception. \"must not run\")))"
                                     :bindings {"target" {:instance-id old-id}}}}]
                  :state {:cells {:offset 1/2 :enabled? true}
                          :pending-actions [{:op :instance/end :instance-id old-id}]
                          :watchers [{:id :beat-watcher :query [:show/beat] :effect-id :show/action
                                      :initialized? true :value 8}]}}
        model (assoc model :reactive registry)
        result (restored model)
        new-id (:id (first (sort-by :order (vals (:instances result)))))]
    (is (= (:registrations registry) (get-in (files/read-string (files/write-string model)) [:model :reactive :registrations])))
    (is (= {:offset 1/2 :enabled? true} (get-in result [:reactive :state :cells])))
    (is (= 8 (get-in result [:reactive :state :watchers 0 :value])))
    (is (= new-id (get-in result [:reactive :state :pending-actions 0 :instance-id])))
    (is (= new-id (get-in result [:reactive :registrations 1 :implementation :bindings "target" :instance-id])))
    (is (thrown? Exception (files/freeze (assoc-in model [:reactive :state :pending-actions 0 :epoch] 0))))
    (is (thrown? Exception (files/freeze (assoc-in model [:reactive :state :watchers 0 :effect-id] :missing/handler))))
    (is (thrown? Exception (files/freeze (assoc-in model [:reactive :registrations 0 :implementation :form] "#=(println :bad)"))))
    (is (thrown? Exception (files/freeze (assoc-in model [:reactive :state :cells :db] {}))))))

(deftest fixture-library-and-limits-are-validated-before-saving
  (let [cache (assoc (fixture-library/new-library) :manufacturers {"robe" {"name" "Robe"}})
        model (assoc-in (demo/model) [:integrations :fixtures] cache)]
    (is (= cache (get-in (restored model) [:integrations :fixtures])))
    (is (thrown? Exception (files/freeze (assoc-in model [:integrations :fixtures :revision] -1))))
    (with-redefs [files/max-document-characters 100]
      (is (thrown? Exception (files/freeze model)))
      (is (thrown? Exception (files/write-string model))))))

(deftest malformed-agent-history-is-rejected-before-restore
  (let [model (assoc-in (demo/model) [:integrations :agents]
                         {:version 1 :config nil :credential-ref nil :selected-conversation-id "missing"
                          :conversations []})]
    (is (thrown? Exception (files/freeze model))))
  (is (thrown? Exception
               (files/freeze (assoc-in (demo/model) [:integrations :agents]
                                      {:version 1 :config nil :credential-ref "raw-key"
                                       :selected-conversation-id nil :conversations []})))))

(deftest saved-callback-validation-never-invokes-custom-readers
  (let [calls (atom 0)]
    (binding [*data-readers* {'sideeffect (fn [_] (swap! calls inc))}
              *default-data-reader-fn* (fn [_ _] (swap! calls inc))]
      (doseq [source ["(fn [] #sideeffect {})" "(fn [] #unknown/tag {})"]]
        (is (thrown? Exception
                     (files/freeze
                      (assoc (demo/model) :reactive
                             {:registrations [{:id :example/source :kind :sub
                                               :implementation {:type :source :namespace "user"
                                                                :form source :bindings {}}}]}))))))
    (is (zero? @calls))))

(deftest show-files-use-transit-values-and-own-their-snapshot
  (let [stamp (java.util.Date. 2222)
        packet (byte-array [0 1 -1])
        values {:received-at stamp :dmx packet :aim [-1.25 3.5 4] :id (random-uuid)
                :ink (color/hsla 0.25 0.5 0.75 0.4) :steps (list :down :up)}
        model (assoc-in (demo/model) [:extensions :trace] values)
        document (files/freeze model)
        source (files/write-string model)
        decoded (get-in (wire/decode source) [:model :extensions :trace])]
    (is (= (dissoc values :dmx) (dissoc decoded :dmx)))
    (is (= [0 1 -1] (vec (:dmx decoded))))
    (.setTime stamp 3333)
    (aset-byte packet 0 (byte 55))
    (is (= 2222 (.getTime (get-in document [:model :extensions :trace :received-at]))))
    (is (= [0 1 -1] (vec (get-in document [:model :extensions :trace :dmx]))))
    (is (= (:ink values) (get-in (files/thaw (demo/model) (files/read-string source)) [:extensions :trace :ink])))))

(deftest previous-schema-two-files-open-and-next-save-is-transit
  (let [model (demo/model)
        previous (pr-str {:schema-version 2 :model model})]
    (is (= model (:model (files/read-string previous))))
    (is (= model (:model (wire/decode (files/write-string model)))))))
