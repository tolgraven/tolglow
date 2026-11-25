(ns tolglow.controllers.content-browser-test
  (:require [afterglow.control.model :as control]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [tolglow.controllers.content-browser :as browser]
            [tolglow.effects.parameter-sources :as parameter-sources]
            [tolglow.registry :as registry]
            [tolglow.signals :as signals]))

(def ^:private wash
  {:id :wash :name "Wash" :version 1
   :params {:level {:type :number :default 0.25 :bounds [0 1]}}
   :outputs {:dimmer {:op :param :key :level}}})

(defn- model []
  (let [registry (-> (registry/new-registry)
                     (registry/publish :effects wash)
                     (registry/publish :effects (assoc wash :id "wash"))
                     (registry/publish :cues {:id :look :name "Opening" :effect-id :wash})
                     (registry/publish :cues {:id "look" :name "Opening" :effect-id "wash"}))]
    (control/new-show {:show-id :live :epoch 7 :registry registry
                       :pages {:opening {:id :opening :cells {[0 0] :look [1 0] :look}}
                               :alternate {:id :alternate :cells {[0 0] :look}}}})))

(def ^:private saves
  [{:show-id :night :source-kind :recovery :handle "night-recovery" :revision 3
    :name "Recovery label" :path "/private/recovery.transit"}
   {:show-id :night :source-kind :named :handle :night-manual :revision 1
    :name "Night" :path "/private/manual.transit"}
   {:show-id "night" :source-kind :named :handle :second-night :name "Night"}])

(defn- row [model saves [kind :as key]]
  (let [page (browser/project (browser/catalog model saves) {:kind kind})]
    (some #(when (= key (:key %)) %) (:items page))))

(defn- preview [model saves entry]
  (browser/plan model saves {:op :preview :selection (:selection entry)}))

(deftest bounded-search-and-exact-selection-across-refreshes
  (let [m (reduce (fn [m n]
                    (update m :registry registry/publish :effects
                            (assoc wash :id (keyword (str "beam-" n)) :name (format "Beam %02d" n))))
                  (model) (range 21))
        catalog (browser/catalog m saves)
        page (fn [opts] (browser/project catalog (merge {:kind :effect :query "  BEAM "} opts)))
        first-page (page {})
        second-page (page {:page 1 :selected-key [:effect :beam-10]})
        final-page (page {:page 1000000000000000N :selected-key [:effect :beam-10]})]
    (is (= {:page 0 :page-count 3 :total 21 :query "BEAM"}
           (select-keys first-page [:page :page-count :total :query])))
    (is (= [8 8 5] (mapv #(count (:items %)) [first-page second-page final-page])))
    (is (= [:beam-0 :beam-8 :beam-16] (mapv #(-> % :items first :id) [first-page second-page final-page])))
    (is (:selected-visible? second-page))
    (is (= [:effect :beam-10] (:selected-key final-page)))
    (is (false? (:selected-visible? final-page)))
    (is (= 23 (:total (page {:query "INTENSITY" :page -1}))))
    (let [empty-page (page {:query (str/join (repeat 90 "x")) :selected-key [:effect :beam-10]})]
      (is (= 64 (count (:query empty-page))))
      (is (= {:total 0 :page 0 :items [] :selected-key [:effect :beam-10] :selected-visible? false}
             (select-keys empty-page [:total :page :items :selected-key :selected-visible?]))))
    (let [updated (-> m (update-in [:registry :effects] dissoc :beam-10) (browser/catalog saves))]
      (is (nil? (:selected-key (browser/project updated {:kind :effect :selected-key [:effect :beam-10]}))))
      (is (nil? (:selected-key (browser/project updated {:kind :cue :selected-key [:effect :wash]})))))
    (is (= [:wash "wash"] (mapv :id (:items (page {:query "wash"})))))))

(deftest grouped-save-metadata-and-exact-artifact-load-plans
  (let [m (model)
        night (row m saves [:show :night])
        manual (get-in night [:sources :named])
        recovery (get-in night [:sources :recovery])
        load #(browser/plan m %1 {:op :load :selection (:selection night) :source %2})
        changed (mapv #(if (= :recovery (:source-kind %)) (update % :revision inc) %) saves)]
    (is (= "Night" (:name night)))
    (is (= #{:named :recovery} (set (keys (:sources night)))))
    (is (= 2 (:total (browser/project (browser/catalog m saves) {:kind :show :query "night"}))))
    (is (= [:night] (mapv :id (:items (browser/project (browser/catalog m saves)
                                                     {:kind :show :query "recovery"})))))
    (is (= {:status :preview :entry night} (preview m saves night)))
    (is (not (str/includes? (pr-str (browser/catalog m saves)) "/private/")))
    (is (= {:type :load-show :payload {:target-show-id :night :expected-show-id :live :expected-epoch 7
                                     :source-kind :named :handle :night-manual :revision 1}}
           (:request (load changed manual))))
    (is (= :planned (:status (load saves recovery))))
    (is (= {:status :rejected :reason :stale-save-source} (load changed recovery)))
    (is (= :stale-save-source (:reason (load saves (assoc manual :path "/private/other.transit")))))
    (is (= :stale-save-source (:reason (load saves (assoc manual :handle :second-night)))))
    (is (= :stale-session (:reason (preview (update m :epoch inc) saves night))))
    (let [reopened (update m :epoch inc)
          restored-saves (vec (reverse saves))
          reopened-night (row reopened restored-saves [:show :night])
          plan (browser/plan reopened restored-saves {:op :load :selection (:selection reopened-night) :source manual})]
      (is (= (get-in night [:sources :named]) (get-in reopened-night [:sources :named])))
      (is (= 8 (get-in plan [:request :payload :expected-epoch])))
      (is (= :night-manual (get-in plan [:request :payload :handle]))))
    (is (= :stale-session (:reason (browser/plan m saves {:op :preview :selection 3}))))
    (is (thrown? clojure.lang.ExceptionInfo (browser/catalog m (conj saves (first saves)))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (browser/catalog m [(assoc (first saves) :handle "../show.transit")])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (browser/catalog m (mapv #(assoc (first saves) :show-id (str %)) (range 129)))))))

(deftest registry-versions-and-launch-intents-stay-exact
  (let [m (model)
        cue (row m saves [:cue "look"])
        effect (row m saves [:effect :wash])
        pinned (update m :registry registry/publish :cues
                       {:id :look :name "Opening" :effect-id :wash :effect-version 1})
        pinned-entry (row pinned saves [:cue :look])
        newer (update pinned :registry registry/publish :effects
                      (assoc wash :version 2 :name "Updated wash"))
        launch (fn [model row]
                 (browser/plan model saves {:op :launch :selection (:selection row)}))
        plan (launch m cue)
        applied (control/apply-command m (merge (get-in plan [:request :payload])
                                                 {:command-id "test-launch" :time-ms 0
                                                  :source {:adapter :push :session-id "fixture"}}))]
    (is (= {:cue-count 1 :running-count 0} (:usage effect)))
    (is (= [:intensity] (:categories effect)))
    (is (= {:page-count 2 :running-count 0} (:usage pinned-entry)))
    (is (= :stale-selection (:reason (launch newer effect))))
    (is (= 1 (:effect-version (:entry (preview newer saves pinned-entry)))))
    (is (= :planned (:status (launch newer pinned-entry))))
    (is (= :stale-selection (:reason (launch (update m :registry registry/publish :effects
                                                    (assoc wash :id "wash" :version 2)) cue))))
    (is (= {:op :effect/launch :effect-id :wash :show-id :live :epoch 7}
           (get-in (launch m effect) [:request :payload])))
    (is (= {:op :cue/launch :cue-id "look" :show-id :live :epoch 7}
           (get-in plan [:request :payload])))
    (is (= :accepted (get-in applied [:ack :status])))
    (is (= "look" (get-in applied [:model :instances "7:1" :cue-id])))
    (is (= 1 (get-in (row (:model applied) saves [:cue "look"]) [:usage :running-count])))
    (is (empty? (:instances m)))
    (is (nil? (:request (preview m saves effect))))
    (is (= :action-unavailable (:reason (browser/plan m saves {:op :load :selection (:selection cue)}))))))

(deftest registered-waveforms-use-the-studio-binding-contract
  (let [wave {:id "show/curve" :name "Curve" :category :smooth :version 1
              :range [0 1] :params {} :expression {:op :param :key signals/phase-input}}
        m (model)
        m (assoc m :waveforms (signals/new-registry [wave (assoc wave :id :show/curve)]))
        m (:model (control/apply-command m {:op :cue/launch :cue-id :look :show-id :live :epoch 7
                                           :command-id "test-instance" :time-ms 0
                                           :source {:adapter :push :session-id "fixture"}}))
        item (row m saves [:waveform "show/curve"])
        request {:op :attach :selection (:selection item) :instance-id "7:1" :param :level
                 :binding-options {:from 0 :to 1 :period {:bars 2} :phase-reference :transport}}
        {:keys [status] {:keys [type payload]} :request} (browser/plan m saves request)
        attached (parameter-sources/bind m payload)
        reloaded (update m :waveforms signals/publish (assoc wave :version 2))]
    (is (= :planned status))
    (is (= :modulation-bind type))
    (is (= {:expected-epoch 7 :instance-id "7:1" :param :level
            :binding {:type :lfo :waveform "show/curve" :from 0 :to 1 :period {:bars 2}
                      :phase-reference :transport :phase 0.0 :inputs {}}} payload))
    (is (= {:input-count 0 :attached-count 1}
           (:usage (row attached saves [:waveform "show/curve"]))))
    (is (= 0 (get-in (row attached saves [:waveform :show/curve]) [:usage :attached-count])))
    (is (= [:smooth] (:categories item)))
    (is (= [0 1] (:range item)))
    (is (nil? (get-in m [:instances "7:1" :parameter-bindings])))
    (is (= :stale-selection (:reason (browser/plan reloaded saves request))))
    (is (= :ineligible-parameter (:reason (browser/plan m saves (assoc request :param "level")))))
    (is (= :ineligible-parameter
           (:reason (browser/plan (assoc-in m [:instances "7:1" :phase] :releasing) saves request))))
    (doseq [bad [{:to 2} {:waveform :show/curve} {:period {:ms 500}} {:inputs {:unregistered 0.5}}]]
      (is (= :invalid-binding (:reason (browser/plan m saves (update request :binding-options merge bad))))))
    (testing "builtins apply only when the show has no explicit registry"
      (is (some? (row (model) saves [:waveform :pulse])))
      (is (nil? (row (assoc m :waveforms (signals/new-registry [])) saves [:waveform :pulse]))))))
