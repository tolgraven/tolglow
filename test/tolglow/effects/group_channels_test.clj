(ns tolglow.effects.group-channels-test
  (:require [afterglow.control.model :as control]
            [afterglow.render.frame :as frame]
            [afterglow.runtime.engine :as engine]
            [clojure.test :refer [deftest is]]
            [tolglow.compiler :as compiler]
            [tolglow.effects.group-channels :as groups]))

(def fixtures [{:id :front :fixture-id :bar :universe 1 :address 1 :channels {:dimmer 1}}
               {:id :rear :universe 1 :address 2 :channels {:dimmer 1}}])

(defn- show [definitions & [options]]
  (control/new-show
   (merge {:rig {:fixtures fixtures}
           :registry {:effects (into {} (map (juxt :id identity)) definitions)
                      :cues (into {} (map (fn [{:keys [id priority]}]
                                           [id {:id id :name (name id) :version 1 :effect-id id
                                                :priority (or priority 0)}])) definitions)}}
          options)))

(defn- effect [id outputs & [fields]]
  (merge {:id id :version 1 :params {} :outputs outputs} fields))

(defn- command [model command]
  (let [{:keys [model ack]} (control/apply-command model
                               (merge {:command-id (str (:revision model))
                                       :show-id (:show-id model) :epoch (:epoch model)
                                       :time-ms (:time-ms model) :source {:adapter :test}}
                                      command))]
    (is (= :accepted (:status ack)) (pr-str ack))
    model))

(defn- launch [model id & [options]]
  (command model (merge {:op :cue/launch :cue-id id} options)))

(defn- configure [model config]
  (:model (groups/apply-operation model {:op :effect-groups/configure :config config})))

(defn- set-level [model channel-id level]
  (:model (groups/apply-operation model {:op :effect-groups/level-set :channel-id channel-id :value level})))

(defn- set-overlay [model overlay]
  (:model (groups/apply-operation model {:op :effect-groups/overlay-set :overlay overlay})))

(deftest manual-hierarchy-and-overlay-levels-have-explicit-membership
  (let [model (-> (show [(effect :wash {:dimmer 1 :color [1 0 0]} {:priority 20})
                         (effect :aim {:aim [1 2 3]} {:priority 10})])
                  (launch :wash) (launch :aim) (launch :wash)
                  (command {:op :instance/opacity-set :instance-id "0:1" :value 0.8})
                  (configure {:manual [{:id :stage :level 1/2 :cue-ids [:aim]
                                        :groups [{:id :washes :level 1/2 :cue-ids [:wash]}]}]
                              :active-overlay :categories
                              :overlays {:categories [{:id :color :level 1/2 :categories [:color]}
                                                      {:id :wash :level 1/4 :categories [:color :intensity] :match :all}
                                                      {:id :movement :categories [:movement]}]}}))
        projection (groups/projection model)
        rows #(mapv (juxt :id :depth) (groups/channel-rows projection %1 %2))]
    (is (= ["0:2" "0:1" "0:3"] (mapv :instance-id (:instances projection))))
    (is (= {"0:2" 0.5 "0:1" 0.125 "0:3" 0.125} (:member-levels projection)))
    (is (= {:opacity 0.8 :manual-path [:stage :washes] :overlay-candidates [:color :wash] :overlay-group :color}
           (select-keys (second (:instances projection)) [:opacity :manual-path :overlay-candidates :overlay-group])))
    (is (= ["0:2" "0:1" "0:3"] (get-in projection [:manual 0 :instance-ids])))
    (is (= ["0:1" "0:3"] (get-in projection [:overlay 0 :instance-ids])))
    (is (= [] (get-in projection [:overlay 1 :instance-ids])))
    (is (= ["0:1" "0:3"] (get-in projection [:overlay 1 :matching-instance-ids])))
    (is (= [[[:manual :stage] 0]] (rows :manual #{})))
    (is (= [[[:manual :stage] 0] [[:manual :washes] 1] [[:instance "0:1"] 2]
            [[:instance "0:3"] 2] [[:instance "0:2"] 1]]
           (rows :manual #{[:manual :stage] [:manual :washes]})))
    (is (= [[[:categories :color] 0] [[:instance "0:1"] 1] [[:instance "0:3"] 1]
            [[:categories :wash] 0] [[:categories :movement] 0]]
           (rows :overlay #{[:categories :color]})))
    (let [changed (set-level model [:manual :washes] 1/4)]
      (is (= (:instances model) (:instances changed)))
      (is (= {"0:2" 0.5 "0:1" 0.0625 "0:3" 0.0625} (:member-levels (groups/projection changed)))))
    (let [assign (fn [m id]
                   (:model (groups/apply-operation m {:op :effect-groups/assign-instance
                                                       :instance-id "0:3" :group-id id})))
          moved (assign model :stage)]
      (is (= [[:stage] [:stage :washes] [:stage]] (mapv :manual-path (:instances (groups/projection moved)))))
      (is (= projection (groups/projection (assign moved nil)))))))

(deftest fixture-overlay-tracks-patch-and-explicit-target-evidence
  (let [model (-> (show [(effect :front-only [{:attribute :dimmer :value 1 :targets [:front]}]
                                         {:targets [:front :rear]})
                         (effect :all {:dimmer 1})
                         (effect :modifier {} {:layer {:targets :previous
                                                        :transforms [{:attribute :dimmer :value 1/2}]}})])
                  (launch :front-only {:launch {:targets [:front :rear]}})
                  (launch :all {:launch {:targets [:rear]}})
                  (launch :modifier)
                  (configure {:active-overlay :fixtures
                              :overlays {:fixtures [{:id :bar :level 1/2 :fixture-ids [:bar]}
                                                    {:id :stage :level 1/4 :fixture-ids [:front :rear]}]}}))
        groups-for #(mapv (juxt :target-source :target-ids :overlay-candidates :overlay-group) (:instances %))]
    (is (= [[:declared #{:front} [:bar :stage] :bar]
            [:declared #{:rear} [:stage] :stage]
            [:unresolved #{} [] nil]]
           (groups-for (groups/projection model))))
    (is (= [:captured #{:front :rear} [:bar :stage] :bar]
           (last (groups-for (groups/projection model {:target-ids-by-instance {"0:3" #{:front :rear :gone}}})))))
    (is (= [:captured #{} [] nil]
           (last (groups-for (groups/projection model {:target-ids-by-instance {"0:3" []}})))))
    (let [repatch (assoc-in model [:rig :fixtures] [(second fixtures)])]
      (is (= [[:declared #{} [] nil] [:declared #{:rear} [:stage] :stage] [:unresolved #{} [] nil]]
             (groups-for (groups/projection repatch))))
      (is (= [:captured #{:rear} [:stage] :stage]
             (last (groups-for (groups/projection repatch {:target-ids-by-instance {"0:3" [:front :rear]}}))))))))

(deftest page-overlay-switch-launch-repatch-and-release-use-one-winner
  (let [model (-> (show [(effect :shared {:dimmer 1}) (effect :unplaced {:dimmer 1})]
                        {:pages {:opening {:cells {[0 0] :shared [0 1] :shared}}
                                 "encore" {:cells {[0 0] :shared}}}})
                  (launch :shared {:launch {:release-duration-ms 100}})
                  (launch :unplaced)
                  (configure {:active-overlay :cue-pages
                              :overlays {:cue-pages [{:id :encore :page-ids ["encore"] :level 1/4}
                                                     {:id :opening :page-ids [:opening] :level 1/2}]
                                         :categories [{:id :intensity :categories [:intensity] :level 1/8}]}}))
        projection (groups/projection model)
        releasing (command model {:op :instance/end :instance-id "0:1"})
        after-tail (:model (engine/advance releasing {:time-ms 100 :fixtures fixtures} compiler/compile-effect))]
    (is (= [[:encore :opening] []] (mapv :overlay-candidates (:instances projection))))
    (is (= {"0:1" 0.25} (:member-levels projection)))
    (is (= {"0:1" 0.125 "0:2" 0.125} (:member-levels (groups/projection (set-overlay model :categories)))))
    (is (= {} (:member-levels (groups/projection (set-overlay model nil)))))
    (is (= {"0:1" 0.5} (:member-levels (groups/projection (update model :pages dissoc "encore")))))
    (is (= :releasing (get-in (groups/projection releasing) [:instances 0 :phase])))
    (is (= {"0:1" 0.25} (:member-levels (groups/projection releasing))))
    (is (= ["0:2"] (mapv :instance-id (:instances (groups/projection after-tail)))))
    (is (= [["0:3"] []] (mapv :instance-ids (:overlay (groups/projection (launch after-tail :shared))))))))

(deftest invalid-and-repeated-edits
  (let [model (configure (control/new-show) {:manual [{:id :first :cue-ids [:wash]}]})
        error (fn [command]
                (try (groups/apply-operation model command) nil
                     (catch clojure.lang.ExceptionInfo e (:control/error (ex-data e)))))]
    (is (= [] (:events (groups/apply-operation model {:op :effect-groups/level-set
                                                      :channel-id [:manual :first] :value 1}))))
    (is (= :unknown-group (error {:op :effect-groups/level-set :channel-id [:manual :gone] :value 1})))
    (doseq [bad [nil -0.001 1.0000000000000000001M Double/NaN Double/POSITIVE_INFINITY]]
      (is (= :invalid-value (error {:op :effect-groups/level-set :channel-id [:manual :first] :value bad}))))
    (is (= :invalid-overlay (error {:op :effect-groups/overlay-set :overlay :fixtures})))
    (is (= :unknown-instance (error {:op :effect-groups/assign-instance :instance-id "stale" :group-id :first})))
    (is (= :duplicate-membership
           (error {:op :effect-groups/configure
                   :config {:manual [{:id :first :cue-ids [:wash]
                                      :groups [{:id :second :cue-ids [:wash]}]}]}})))
    (doseq [bad [[] 42 [:front :front]]]
      (is (= :invalid-selector (error {:op :effect-groups/configure
                                       :config {:overlays {:fixtures [{:id :none :fixture-ids bad}]}}}))))
    (is (= :invalid-group (error {:op :effect-groups/configure
                                  :config {:manual [{:id :invalid :groups {:not :a-vector}}]}})))))

(defn- contributions [model]
  (:contributions (engine/advance model {:time-ms 0 :fixtures fixtures} compiler/compile-effect)))

(defn- contributions-at-mixer-levels [model]
  (:contributions (engine/advance model {:time-ms 0 :fixtures fixtures
                                         ::engine/opacity-factors (:member-levels (groups/projection model))}
                                  compiler/compile-effect)))

(defn- resolved [blocks] (frame/resolve-contributions fixtures blocks))

(defn- front-level [blocks] (get-in (resolved blocks) [:values :front :channels :dimmer]))

(deftest unity-preserves-interleaved-blend-order-and-packed-bytes
  (let [model (-> (show [(effect :first {:dimmer 1/4} {:priority 0})
                         (effect :between {:dimmer 1/2} {:priority 1})
                         (effect :last [{:attribute :dimmer :value 1/4 :blend :add}] {:priority 2})])
                  (launch :last) (launch :first) (launch :between))
        configured (configure model {:manual [{:id :group :cue-ids [:first :last]}]})
        original (contributions model)
        unity (contributions-at-mixer-levels configured)
        regrouped (mapv (fn [block]
                          (case (get-in block [:source :cue-id])
                            :first (assoc block :native-layer {:id :wrong-anchor :member 0})
                            :last (assoc block :native-layer {:id :wrong-anchor :member 1})
                            block)) original)]
    (is (= original unity))
    (is (= (vec (get-in (resolved original) [:universes 1]))
           (vec (get-in (resolved unity) [:universes 1]))))
    (is (== 0.75 (front-level unity)))
    (is (== 0.5 (front-level regrouped)))
    (is (== 0.625 (front-level (contributions-at-mixer-levels (set-level configured [:manual :group] 0.5)))))))

(deftest mixer-level-acts-at-native-member-opacity-before-a-later-transform
  (let [model (-> (show [(effect :backdrop {:dimmer 1/4} {:priority 0})
                         (effect :base {:dimmer 1/2} {:priority 1})
                         (effect :transform {:dimmer 0} {:priority 2})])
                  (launch :backdrop)
                  (launch :base {:source {:native-layer {:id :authored :member 0 :opacity 1/2}}})
                  (launch :transform {:source {:native-layer
                                               {:id :authored :member 1 :opacity 1/2 :targets :previous
                                                :transforms [{:attribute :dimmer
                                                              :value {:op :multiply :args [{:op :previous :attribute :dimmer} 2]}}]}}})
                  (configure {:manual [{:id :base-fader :cue-ids [:base]}]}))
        mixed (set-level model [:manual :base-fader] 1/2)
        original (contributions model)
        at-member (contributions-at-mixer-levels mixed)
        after-composite (mapv (fn [block] (cond-> block
                                           (:native-layer block) (assoc-in [:native-layer :opacity] 1/4))) original)]
    (is (== 0.625 (front-level original)))
    (is (== 0.5 (front-level at-member)))
    (is (== 0.4375 (front-level after-composite)))
    (is (= (:instances model) (:instances mixed)))
    (is (== 0.375 (front-level (contributions-at-mixer-levels (set-level model [:manual :base-fader] 0)))))))
