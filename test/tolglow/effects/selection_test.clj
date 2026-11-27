(ns tolglow.effects.selection-test
  (:require [clojure.test :refer [deftest is]]
            [afterglow.control.effect-selection :as selection]
            [afterglow.control.model :as control]
            [afterglow.control.views :as views]
            [afterglow.runtime.engine :as engine]
            [tolglow.compiler :as compiler]
            [tolglow.composition :as composition]
            [tolglow.persistence :as persistence]
            [tolglow.show :as show]))

(defn- submit [model payload]
  (control/apply-command model (merge {:command-id (str "test:" (:revision model))
                                      :show-id (:show-id model) :epoch (:epoch model)
                                      :source {:adapter :repl} :time-ms (:time-ms model)}
                                     payload)))

(defn- command [model payload]
  (let [{:keys [ack model]} (submit model payload)]
    (when-not (= :accepted (:status ack)) (throw (ex-info "Control command failed" ack)))
    model))

(defn- scene
  ([] (scene []))
  ([fixtures]
   (reduce (fn [model [id priority level]]
             (-> model
                 (command {:op :registry/publish :kind :effects
                           :definition (cond-> {:id id :version 1
                                                :params {:level {:type :number :bounds [0 1] :default level}}
                                                :outputs {:dimmer {:op :param :key :level}}}
                                         (seq fixtures) (assoc :targets [id :blend]))})
                 (command {:op :cue/put :cue {:id id :effect-id id :priority priority}})))
           (control/new-show {:rig {:fixtures fixtures}})
           [[:wash 20 0.25] [:side -3 0.5] [:accent 20 0.75]])))

(defn- launch [model cue]
  (command model {:op :cue/launch :cue-id cue :launch {:system {:opacity 0.6}}}))

(defn- three-effects []
  (-> (scene) (launch :wash) (launch :side) (launch :accent)))

(defn- operation [control id & [additive?]]
  (cond-> {:op :effect-selection/toggle :control control :selector {:instance-ids [id]}}
    (some? additive?) (assoc :additive? additive?)))

(defn- included [model]
  (:included-instance-ids (selection/projection model)))

(def ^:private selection-actions
  [(operation :solo "0:1")
   (operation :solo "0:2" true)
   (operation :mute "0:2")
   (operation :mute "0:3" true)
   (operation :solo "0:2")
   (operation :solo "0:2")
   (operation :mute "0:3")
   (operation :mute "0:3")])

(deftest ordered-selection-replay-preserves-effect-data
  (let [model (three-effects)
        replay (fn []
                 (reductions (fn [{:keys [model]} payload] (submit model payload))
                             {:model model} selection-actions))
        captured (replay)
        models (mapv :model captured)
        base (mapv #(select-keys % [:id :params :system :state]) (views/active-instances model))]
    (is (= captured (replay)))
    (is (= [["0:2" "0:1" "0:3"] ["0:1"] ["0:2" "0:1"] ["0:1"] ["0:1"]
            [] ["0:1"] ["0:2" "0:1"] ["0:2" "0:1" "0:3"]]
           (mapv included models)))
    (is (= [[:mute :solo :mute] [:mute nil :mute]]
           (mapv #(mapv :suppressed-by (:instances (selection/projection %)))
                 (take 2 (drop 5 models)))))
    (is (= ["0:2" "0:1"] (get-in (nth captured 2) [:ack :selected-instance-ids])))
    (is (= ["0:1"] (get-in (nth captured 5) [:events 0 :changed-instance-ids])))
    (is (= [{:id "0:2" :params {:level 0.5}} {:id "0:1" :params {:level 0.25}}
            {:id "0:3" :params {:level 0.75}}]
           (mapv #(select-keys % [:id :params]) base)))
    (doseq [state models]
      (is (= base (mapv #(select-keys % [:id :params :system :state]) (views/active-instances state)))))))

(deftest show-owner-composes-only-selected-effects-in-render-order
  (let [fixtures (mapv (fn [id address] {:id id :universe 1 :address address :channels {:dimmer 1}})
                       [:wash :side :accent :blend] (range 1 5))
        start (reduce #(command %1 {:op :cue/launch :cue-id %2}) (scene fixtures) [:wash :side :accent])
        expected [[64 128 191 191] [64 0 0 64] [64 128 0 64] [64 0 0 64] [64 0 0 64]
                  [0 0 0 0] [64 0 0 64] [64 128 0 64] [64 128 191 191]]]
    (doseq [batch? [true false]]
      (let [runtime (show/create-runtime {:model start :workers 1 :workspace-options nil :batch? batch?})
            captured (fn [] (let [{:keys [output status]} @(:state runtime)]
                              (is (nil? (:error status)))
                              (mapv #(bit-and 255 %) (take 4 (get-in output [:universes 1])))))]
        (try
          (show/tick! runtime 0)
          (is (= expected
                 (into [(captured)]
                       (map (fn [time-ms action]
                              (let [receipt (show/submit! runtime action)]
                                (show/tick! runtime time-ms)
                                (is (= :accepted (:status (deref receipt 1000 {:status :timeout}))))
                                (captured)))
                            (range 10 90 10) selection-actions))))
          (finally (show/stop! runtime)))))))

(deftest control-defaults-and-temporary-modifier-are-independent
  (let [model (three-effects)
        configured (command model {:op :effect-selection/configure :modes {:mute :additive}})
        first-click (submit configured (operation :solo "0:1"))
        modified (submit (:model first-click) (assoc (operation :solo "0:2" true) :source {:adapter :push}))
        after-release (submit (:model modified) (assoc (operation :solo "0:3" false) :source {:adapter :push}))
        other-source (submit (:model modified) (assoc (operation :solo "0:3") :source {:adapter :studio}))
        muted (-> configured
                  (command (operation :mute "0:1"))
                  (command (operation :mute "0:2")))]
    (is (= {:solo :exclusive :mute :exclusive} (:modes (selection/projection model))))
    (is (= {:solo :exclusive :mute :additive} (:modes (selection/projection (:model modified)))))
    (is (= [:exclusive :additive :exclusive :exclusive]
           (mapv #(get-in % [:ack :mode]) [first-click modified after-release other-source])))
    (is (= (selection/projection (:model after-release)) (selection/projection (:model other-source))))
    (is (= ["0:3"] (included (:model other-source)) (included muted)))
    (is (= ["0:2" "0:3"] (included (command muted (operation :mute "0:2")))))))

(defn- compound []
  (-> (scene)
      (launch :wash)
      (composition/save {:id :layer :mode :macro :slots [{:cue-id :wash} {:cue-id :side}]})
      (composition/start :layer 0)))

(deftest native-layer-and-category-targets-use-active-membership
  (let [model (compound)
        id (get-in model [:composition-runs :layer :owner-id])
        target {:op :effect-selection/toggle :control :solo :selector {:layer-id id}}
        partial (command model (operation :solo "0:2"))
        full (command partial (assoc target :additive? true))
        mute (command full {:op :effect-selection/toggle :control :mute
                            :selector {:categories [:intensity] :layer-id id}})
        removed (command full {:op :instance/remove :instance-id "0:3"})
        last-removed (command removed {:op :instance/end :instance-id "0:2" :force? true})
        relaunched (launch last-removed :wash)]
    (is (= [{:id id :instance-ids ["0:3" "0:2"] :solo :partial :mute :none}]
           (:layers (selection/projection partial))))
    (is (= ["0:3" "0:2"] (included full)))
    (is (= [] (included mute)))
    (is (= [{:id id :instance-ids ["0:3" "0:2"] :solo :all :mute :all}]
           (:layers (selection/projection mute))))
    (is (= ["0:3" "0:1" "0:2"] (included (command full (assoc target :additive? true)))))
    (is (= ["0:2"] (included removed)))
    (is (= [] (:layers (selection/projection last-removed))))
    (is (= ["0:1" "0:4"] (included relaunched)))
    (is (false? (:solo-active? (selection/projection relaunched))))))

(deftest release-tail-remains-selected-until-runtime-expiry
  (let [model (-> (scene)
                  (launch :wash)
                  (command {:op :cue/launch :cue-id :side :launch {:system {:release {:ms 50}}}})
                  (command (operation :solo "0:2"))
                  (command {:op :instance/end :instance-id "0:2"}))
        advance (fn [m ms]
                  (:model (engine/advance m {:time-ms ms :fixtures []} compiler/compile-effect)))
        releasing (advance model 25)
        expired (advance releasing 50)]
    (is (= :releasing (get-in releasing [:instances "0:2" :phase])))
    (is (= ["0:2"] (included releasing)))
    (is (nil? (get-in expired [:instances "0:2"])))
    (is (= ["0:1"] (included expired)))))

(deftest empty-stale-and-invalid-inputs-cannot-change-selection
  (let [model (command (three-effects) (operation :solo "0:1"))]
    (doseq [selector [{:categories [:no-matching-category]} {:instance-ids []}]]
      (let [result (submit model {:op :effect-selection/toggle :control :solo :selector selector})]
        (is (= :accepted (get-in result [:ack :status])))
        (is (identical? (:instances model) (get-in result [:model :instances])))
        (is (= [] (:events result)))))
    (doseq [[payload code] [[(operation :solo "0:99") :unknown-instance]
                            [(operation :pause "0:1") :invalid-control]
                            [(operation :solo "0:1" "true") :invalid-value]
                            [(assoc (operation :solo "0:1") :additive? nil) :invalid-value]
                            [{:op :effect-selection/toggle :control :solo :selector {:layer-id nil}} :invalid-selector]
                            [{:op :effect-selection/toggle :control :solo :selector {:layer-id :removed}} :unknown-layer]
                            [{:op :effect-selection/configure :modes {:mute :latch}} :invalid-value]]]
      (let [result (submit model payload)]
        (is (= code (get-in result [:ack :code])))
        (is (identical? model (:model result)))
        (is (empty? (:events result)))))))

(deftest schema-two-restores-selected-instances-and-their-rekeyed-native-layer
  (let [model (compound)
        old-layer (get-in model [:composition-runs :layer :owner-id])
        selected (-> model
                     (command {:op :effect-selection/configure :modes {:solo :additive}})
                     (command {:op :effect-selection/toggle :control :solo :selector {:layer-id old-layer}})
                     (command (operation :mute "0:2" true)))
        restored (persistence/thaw (control/new-show {:epoch 8})
                                   (persistence/read-string (persistence/write-string selected)))
        new-layer (get-in restored [:composition-runs :layer :owner-id])
        view (selection/projection restored)]
    (is (not= old-layer new-layer))
    (is (= :additive (get-in view [:modes :solo])))
    (is (= :exclusive (get-in view [:modes :mute])))
    (is (= [{:id new-layer :instance-ids ["9:3" "9:2"] :solo :all :mute :partial}] (:layers view)))
    (is (= ["9:3"] (:included-instance-ids view)))
    (is (= (mapv :params (views/active-instances selected)) (mapv :params (views/active-instances restored))))))

(deftest show-owner-admits-selection-retries-and-restores-it-in-memory
  (let [base (compound)
        runtime (show/create-runtime {:model base :workers 1 :workspace-options nil})
        clock (atom 0)
        send (fn [type payload]
               (let [receipt (show/submit! runtime type payload)]
                 (show/tick! runtime (swap! clock + 10))
                 (deref receipt 1000 {:status :timeout})))
        model #(get @(:state runtime) :model)
        layer (get-in base [:composition-runs :layer :owner-id])
        select-layer {:command-id :select-layer :op :effect-selection/toggle
                      :source {:adapter :repl} :control :solo :selector {:layer-id layer}}]
    (try
      (is (= :accepted (:status (send :command {:command-id :mode :op :effect-selection/configure
                                               :modes {:mute :additive}}))))
      (let [receipt (send :command select-layer)]
        (is (= :accepted (:status receipt)))
        (is (= ["0:3" "0:2"] (:selected-instance-ids receipt)))
        (is (= receipt (send :command select-layer)))
        (is (= ["0:3" "0:2"] (included (model)))))
      (is (= :accepted (:status (send :command (assoc (operation :mute "0:2") :command-id :mute-member)))))
      (is (= ["0:3"] (included (model))))
      (let [snapshot (show/snapshot runtime)]
        (is (= (selection/projection (model)) (:effect-selection snapshot)))
        (is (every? #(not (contains? % ::selection/selected)) (:instances snapshot))))
      (let [source (show/export-show-source runtime)]
        (is (= :accepted (:status (send :command (assoc select-layer :command-id :clear-solo)))))
        (is (= ["0:3" "0:1"] (included (model))))
        (is (= :accepted (:status (send :import {:source source}))))
        (is (= ["1:3"] (included (model))))
        (is (= :additive (get-in (selection/projection (model)) [:modes :mute])))
        (is (= [{:id (get-in (model) [:composition-runs :layer :owner-id])
                 :instance-ids ["1:3" "1:2"] :solo :all :mute :partial}]
               (:layers (selection/projection (model)))))
        (is (= (mapv :params (views/active-instances base))
               (mapv :params (views/active-instances (model)))))
        (is (= :unknown-layer (:code (send :command select-layer)))))
      (finally (show/stop! runtime)))))

(deftest show-owner-applies-a-one-action-exclusive-mode-from-the-repl
  (let [runtime (show/create-runtime {:model (three-effects) :workers 1 :workspace-options nil})
        clock (atom 0)
        send (fn [payload]
               (let [receipt (show/submit! runtime :command (merge {:source {:adapter :repl}} payload))]
                 (show/tick! runtime (swap! clock + 10))
                 (deref receipt 1000 {:status :timeout})))
        solo (fn [id & [mode]] (cond-> (operation :solo id) mode (assoc :mode mode)))
        snapshot #(:effect-selection (show/snapshot runtime))]
    (try
      (is (= :accepted (:status (send {:op :effect-selection/configure :modes {:solo :additive}}))))
      (send (solo "0:1"))
      (send (solo "0:3"))
      (is (= ["0:1" "0:3"] (:included-instance-ids (snapshot))))
      (let [revision (:revision (show/snapshot runtime))
            receipt (send (solo "0:2" :exclusive))]
        (is (= [:accepted :exclusive ["0:2"]] ((juxt :status :mode :selected-instance-ids) receipt)))
        (is (= (inc revision) (:revision receipt)) "One commit replaces the additive selection")
        (is (= ["0:2"] (:included-instance-ids (snapshot)))))
      (is (= :additive (get-in (snapshot) [:modes :solo])) "The override leaves the default alone")
      (is (= ["0:2" "0:1"] (:included-instance-ids (do (send (solo "0:1")) (snapshot)))))
      (is (= :invalid-value (:code (send (assoc (solo "0:3" :exclusive) :additive? true)))))
      (finally (show/stop! runtime)))))
