(ns tolglow.studio.performance-test
  (:require [cljs.test :refer-macros [deftest is]]
            [re-frame.db :as rf-db]
            [tolglow.authoring.parameter-layout :as layout]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [tolglow.studio.effect-selection :as selection]
            [tolglow.studio.effect-inspector :as inspect]
            [tolglow.studio.events :as events]
            [tolglow.studio.param-visualization :as visual]
            [tolglow.studio.performance :as performance]))

(def base
  (assoc (events/initial-db "performance-session")
         :show {:runtime-id "runtime-a" :epoch 4 :revision 7
                :groups [{:id "color" :color "#bb66dd" :instance-ids ["4:1"] :paused? false}
                         {:id "intensity" :color "#ddbb66" :instance-ids ["4:1" "4:2"] :paused? false}
                         {:id "movement" :color "#66bbdd" :instance-ids ["4:2" "4:3"] :paused? false}]
                :instances [{:id "4:1" :cue-id "user/wash" :phase "running" :system {:opacity 0.8 :paused? false}}
                            {:id "4:2" :cue-id "user/sweep" :phase "running" :system {:opacity 0.6 :paused? true :release {:beats 2}}}
                            {:id "4:3" :cue-id "user/aim" :phase "running" :system {:opacity 1 :paused? false}}]}))

(def instance-target {:instance-id "4:1"})

(def group-target {:selector {:categories ["color" "movement"] :match "any"}})

(defn transition [db event]
  (reset! rf-db/app-db db)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %))
                                               (registrar/get-handler :event (first event))))))

(defn command [result] (second (first (:fx result))))

(deftest solo-and-mute-send-shared-selectors-and-consume-the-touch-modifier
  (let [identity {:runtime-id "runtime-a" :show-id :test :epoch 4}
        view (assoc identity :revision 7 :effect-selection {:modes {:solo :exclusive :mute :exclusive}})
        db (assoc base :server-projection {:view view})
        armed (transition db [:effect-selection/add-once identity])
        selector {:instance-ids ["4:1"]}
        sent (transition (:db armed) [:effect-selection/toggle identity :solo selector false])
        payload (command sent)
        accepted (transition (:db sent) (conj (:on-success payload) {:status "accepted" :revision 8}))
        ready (assoc-in (:db accepted) [:server-projection :view :revision] 8)
        normal (command (transition ready [:effect-selection/toggle identity :mute {:layer-id :layer/test} false]))
        shifted (command (transition ready [:effect-selection/toggle identity :mute (:selector group-target) true]))
        configured (command (transition ready [:effect-selection/configure identity :mute :additive]))
        newer (assoc-in (:db sent) [:server-projection :view :epoch] 5)]
    (is (= :effect-selection/toggle (:op payload)))
    (is (= {:control :solo :selector selector :additive? true} (dissoc (:fields payload) :command-id)))
    (is (= [4 "runtime-a" "performance-session"] ((juxt :epoch :runtime-id :session) payload)))
    (is (= (:server-projection db) (get-in sent [:db :server-projection])))
    (is (nil? (:fx (transition (:db accepted) [:effect-selection/toggle identity :solo selector false]))))
    (is (= {:control :mute :selector {:layer-id :layer/test} :additive? false}
           (dissoc (:fields normal) :command-id)))
    (is (= {:control :mute :selector (:selector group-target) :additive? true}
           (dissoc (:fields shifted) :command-id)))
    (is (= :effect-selection/configure (:op configured)))
    (is (= {:mute :additive} (get-in configured [:fields :modes])))
    (is (= newer (:db (transition newer (conj (:on-failure payload) "Instance no longer exists")))))
    (is (= "Instance no longer exists"
           (get-in (transition (:db sent) (conj (:on-failure payload) "Instance no longer exists"))
                   [:db :ui :effect-selection :error])))))

(deftest solo-and-mute-button-flags-reflect-the-committed-selection
  (let [a {:id "4:1" :soloed? true :muted? true :suppressed-by :mute}
        b {:id "4:2" :soloed? false :muted? true :suppressed-by :mute}
        rows (selection/by-instance {:instances [a b]})]
    (is (= a (get rows "4:1")))
    (is (= {:solo :all :mute :all} (selection/instance-flags a)))
    (is (= {:solo :none :mute :all} (selection/instance-flags b)))
    (is (= {:solo :partial :mute :all} (selection/group-flags (vals rows))))))

(deftest visible-membership-comes-from-server-groups-and-never-infers-output-tags
  (let [show (:show base)
        any {:scope "categories" :categories ["intensity" "movement"] :match "any"}
        all (assoc any :match "all")]
    (is (= ["4:1" "4:2" "4:3"] (mapv :id (performance/selected-instances show any))))
    (is (= ["4:2"] (mapv :id (performance/selected-instances show all))))
    (is (= [] (performance/selected-instances show (assoc any :categories []))))
    (is (= [] (performance/selected-instances show (assoc all :categories ["intensity" "absent"]))))
    (is (= 3 (count (performance/selected-instances show {:scope "all"})))))
  (is (= {:categories ["user/color"] :match "any"}
         (performance/group-selector {:scope "categories" :categories [:user/color :user/color]})))
  (is (= {} (performance/group-selector {:scope "all"}))))

(deftest individual-and-group-actions-share-session-epoch-and-semantic-command-transport
  (doseq [operation ["instance/pause" "instance/resume" "instance/opacity-toggle" "instance/end" "instance/remove"]]
    (let [result (transition base [:performance/action instance-target operation {}]) payload (command result)]
      (is (= :studio/send-command (ffirst (:fx result))))
      (is (= operation (:op payload)))
      (is (= {:instance-id "4:1"} (:fields payload)))
      (is (= 4 (:epoch payload)))
      (is (= "runtime-a" (:runtime-id payload)))
      (is (= "performance-session" (:session payload)))))
  (let [result (transition base [:performance/action group-target "instance/opacity-set" {:value 0}])]
    (is (= "group/apply" (:op (command result))))
    (is (= {:selector (:selector group-target) :action {:op "instance/opacity-set" :value 0}}
           (:fields (command result))))))

(deftest empty-selection-and-vanished-instances-never-become-unintended-all-actions
  (doseq [target [{:selector {:categories [] :match "any"}} {:instance-id "4:missing"}]]
    (let [result (transition base [:performance/action target "instance/remove" {}])]
      (is (nil? (:fx result)))
      (is (= "No running instances match this selection."
             (get-in result [:db :ui :performance :errors (performance/target-key target)])))))
  (is (thrown? js/Error (performance/command-payload instance-target "show/restart" {}))))

(deftest musical-envelope-edits-are-fractional-and-partial
  (is (= {:beats 0.125} (performance/envelope-duration "1/8" "beats")))
  (is (= {:bars 2} (performance/envelope-duration "2" "bars")))
  (is (= {:phrases 0.5} (performance/envelope-duration "1/2" "phrases")))
  (is (= {:ms 0} (performance/envelope-duration "0" "ms")))
  (doseq [[value unit] [["-1" "beats"] ["1/0" "beats"] ["" "bars"] ["1" "seconds"]]]
    (is (thrown? js/Error (performance/envelope-duration value unit))))
  (let [key (performance/target-key instance-target)
        edited (transition base [:performance/envelope-edit key :release :value "1/2"])
        result (transition (:db edited) [:performance/envelope-apply instance-target :release])]
    (is (= "instance/envelope-set" (:op (command result))))
    (is (= {:instance-id "4:1" :envelope {:release {:beats 0.5}}} (:fields (command result))))))

(deftest per-instance-current-duration-and-mixed-group-values-stay-visible
  (let [instances (get-in base [:show :instances])]
    (is (= {:value "2" :unit "beats" :current {:beats 2} :mixed? false}
           (performance/envelope-form performance/defaults [:one] :release [(second instances)])))
    (is (:mixed? (performance/envelope-form performance/defaults [:all] :release instances)))
    (is (nil? (performance/opacity-value (:show base) nil instances)))
    (is (= 0.8 (performance/opacity-value (:show base) nil [(first instances)])))))

(deftest opacity-faders-coalesce-and-retain-drafts-until-acknowledged-revision
  (let [key (performance/target-key instance-target)
        first-event (transition base [:performance/opacity instance-target "0.5"])
        second-event (transition (:db first-event) [:performance/opacity instance-target "0.25"])
        sent (transition (:db second-event) (get-in first-event [:dispatch-later 0 :dispatch]))
        ack (transition (:db sent) (conj (:on-success (command sent)) {:revision 8}))
        draft (get-in ack [:db :ui :performance :opacity key])]
    (is (= 65 (get-in first-event [:dispatch-later 0 :ms])))
    (is (nil? (:dispatch-later second-event)))
    (is (= {:instance-id "4:1" :value 0.25} (:fields (command sent))))
    (is (= 0.25 (performance/opacity-value (:show base) draft [(first (get-in base [:show :instances]))])))
    (is (= 0.8 (performance/opacity-value (assoc (:show base) :revision 8) draft [(first (get-in base [:show :instances]))])))
    (is (nil? (:dispatch ack)))))

(deftest stale-opacity-ack-cannot-overwrite-a-newer-gesture
  (let [key (performance/target-key instance-target)
        first-event (transition base [:performance/opacity instance-target "0.5"])
        sent (transition (:db first-event) (get-in first-event [:dispatch-later 0 :dispatch]))
        newer (transition (:db sent) [:performance/opacity instance-target "0.25"])
        late (transition (:db newer) (conj (:on-success (command sent)) {:revision 8}))]
    (is (nil? (:dispatch-later newer)))
    (is (= 0.25 (get-in late [:db :ui :performance :opacity key :value])))
    (is (nil? (get-in late [:db :ui :performance :opacity key :revision])))
    (is (true? (get-in late [:db :ui :performance :opacity key :scheduled?])))
    (is (= 65 (get-in late [:dispatch-later 0 :ms])))
    (let [next (transition (:db late) (get-in late [:dispatch-later 0 :dispatch]))]
      (is (= 0.25 (get-in (command next) [:fields :value]))))))

(deftest invalid-opacity-and-failed-actions-remain-visible-without-mutating-parameters
  (doseq [value ["-0.1" "1.1" "NaN"]]
    (let [result (transition base [:performance/opacity instance-target value])]
      (is (nil? (:dispatch-later result)))
      (is (string? (get-in result [:db :ui :performance :errors (performance/target-key instance-target)])))))
  (let [sent (transition base [:performance/action instance-target "instance/pause" {}])
        failed (transition (:db sent) (conj (:on-failure (command sent)) "Instance disappeared"))]
    (is (= "Instance disappeared" (get-in failed [:db :ui :performance :errors (performance/target-key instance-target)])))
    (is (= (:show base) (get-in failed [:db :show])))))

(deftest runtime-replacement-clears-system-drafts-and-invalidates-commands
  (let [pending (transition base [:performance/opacity instance-target "0.5"])
        sent (transition (:db pending) (get-in pending [:dispatch-later 0 :dispatch]))
        changed (transition (:db sent) [:performance/runtime-change])
        late (transition (:db changed) (conj (:on-success (command sent)) {:revision 8}))
        timer (transition (:db changed) (get-in pending [:dispatch-later 0 :dispatch]))]
    (is (= {} (get-in changed [:db :ui :performance :opacity])))
    (is (empty? late))
    (is (empty? timer))))

(deftest compact-grid-retains-populated-coordinates-and-edit-access
  (is (= 2 (performance/visible-cue-rows {:cells []} false)))
  (is (= 2 (performance/visible-cue-rows {:cells [{:x 7 :y 1 :cue-id "last"}]} false)))
  (is (= 7 (performance/visible-cue-rows {:cells [{:x 0 :y 6 :cue-id "deep"}]} false)))
  (is (= 8 (performance/visible-cue-rows {:cells []} true))))

(deftest live-cards-use-captured-definition-and-expose-every-parameter
  (let [definition {:id "wash" :version 1 :params {:z {:type "number"} :level {:control/axis "pressure"}
                                                  :color {:ui/order 1} :a {:type "number"}}}
        show {:effects [(assoc definition :version 2 :params {})]}
        instance {:effect-id "wash" :definition definition}]
    (is (= definition (performance/card-definition show instance)))
    (is (= 2 (:version (performance/card-definition show (dissoc instance :definition)))))
    (is (= [:color :level :a :z] (mapv first (:parameters (layout/resolve-layout definition nil)))))))

(deftest group-clear-and-card-expansion-stay-local
  (let [selected (transition base [:performance/group-set :scope "all"])
        cleared (transition (:db selected) [:performance/group-clear])
        expanded (transition base [:performance/card-toggle "4:1" {:runtime-id "runtime-a" :epoch 4}])]
    (is (empty? (performance/selected-instances (:show base) (get-in cleared [:db :ui :performance :group]))))
    (is (= true (get-in expanded [:db :ui :performance :expanded "4:1"])))
    (is (= "4:1" (get-in expanded [:db :ui :selected-instance])))
    (is (= (:show base) (get-in expanded [:db :show])))
    (is (nil? (:fx expanded)))))

(deftest duplicate-launches-use-captured-instance-ids-and-committed-order
  (let [show (-> (:show base)
                 (assoc :active-instance-order ["4:2" "4:1" "4:3"])
                 (assoc-in [:instances 1 :cue-id] "user/wash"))
        db (assoc base :show show)
        selected (transition db [:performance/card-toggle "4:2" {:runtime-id "runtime-a" :epoch 4}])
        stale-card (transition (:db selected) [:performance/card-toggle "4:1" {:runtime-id "runtime-b" :epoch 4}])
        opened (:db (transition (:db selected) [:performance/reorder-toggle]))
        capture {:identity ["runtime-a" 4 ["4:2" "4:1" "4:3"]] :revision 7}
        source (:db (transition opened [:performance/reorder-pick capture "4:2"]))
        sent (transition source [:performance/reorder-pick capture "4:1"])
        payload (command sent)
        ack (transition (:db sent) (conj (:on-success payload) {:status "accepted" :revision 8}))
        rejected (transition (:db sent) (conj (:on-failure payload) "Show revision changed"))
        changed (-> source (assoc-in [:show :active-instance-order] ["4:1" "4:2" "4:3"]))
        stale (transition changed [:performance/reorder-pick capture "4:1"])]
    (is (= "4:2" (get-in selected [:db :ui :selected-instance])))
    (is (= (:db selected) (:db stale-card)))
    (is (empty? (:fx selected)))
    (is (= "instance/move-after" (:op payload)))
    (is (= {:instance-id "4:2" :after-instance-id "4:1" :expected-revision 7} (:fields payload)))
    (is (= ["runtime-a" 4] ((juxt :runtime-id :epoch) payload)))
    (is (= ["4:2" "4:1" "4:3"] (mapv :id (performance/ordered-instances (get-in ack [:db :show])))))
    (is (= ["4:1" "4:2" "4:3"] (mapv :id (performance/ordered-instances (:show changed)))))
    (is (empty? (:fx stale)))
    (is (= "Cue order changed. Choose a cue again." (get-in stale [:db :ui :performance :reorder :error])))
    (is (= "Show revision changed" (get-in rejected [:db :ui :performance :reorder :error])))
    (is (= 8 (get-in ack [:db :ui :performance :reorder :request :revision])))))

(deftest setting-layer-priority-submits-the-selected-launch-with-current-show-identity
  (let [db (assoc-in base [:show :instances] [(get-in base [:show :instances 0])])
        opened (:db (transition db [:performance/reorder-toggle]))
        capture {:identity ["runtime-a" 4 ["4:1"]] :revision 7}
        selected (:db (transition opened [:performance/reorder-pick capture "4:1"]))
        path [:ui :performance :reorder]
        source (get-in selected (conj path :source))
        drafted (:db (transition selected [:performance/reorder-priority-edit source "-3.5"]))
        invalid (transition drafted [:performance/reorder-priority-set capture "4:1" ""])
        stale (transition drafted [:performance/reorder-priority-set (assoc-in capture [:identity 0] "old") "4:1" "-3.5"])
        sent (transition drafted [:performance/reorder-priority-set capture "4:1" "-3.5"])
        payload (command sent)
        ack (transition (:db sent) (conj (:on-success payload) {:status "accepted" :revision 8 :render-priority -3.5}))]
    (is (= "-3.5" (get-in drafted (conj path :priority-draft))))
    (is (empty? (:fx invalid)))
    (is (re-find #"finite number" (get-in (:db invalid) (conj path :error))))
    (is (empty? (:fx stale)))
    (is (= "instance/set-priority" (:op payload)))
    (is (= {:instance-id "4:1" :priority -3.5 :expected-revision 7} (:fields payload)))
    (is (= ["runtime-a" 4] ((juxt :runtime-id :epoch) payload)))
    (is (= "Layer priority updated." (get-in (:db ack) (conj path :message))))
    (is (= 8 (get-in (:db ack) (conj path :request :revision))))
    (is (= (:show db) (get-in ack [:db :show])))))

(deftest rejected-layer-priority-keeps-the-draft-for-the-same-running-launch
  (let [db (assoc-in base [:show :active-instance-order] ["4:1" "4:2" "4:3"])
        opened (:db (transition db [:performance/reorder-toggle]))
        capture {:identity ["runtime-a" 4 ["4:1" "4:2" "4:3"]] :revision 7}
        selected (:db (transition opened [:performance/reorder-pick capture "4:1"]))
        sent (transition selected [:performance/reorder-priority-set capture "4:1" "-3.5"])
        payload (command sent)
        received-db (-> (:db sent)
                        (assoc-in [:show :revision] 8)
                        (assoc-in [:show :active-instance-order] ["4:2" "4:1" "4:3"]))
        rejected (:db (transition received-db (conj (:on-failure payload) "Show revision changed")))
        path [:ui :performance :reorder]
        next-capture {:identity ["runtime-a" 4 ["4:2" "4:1" "4:3"]] :revision 8}
        retried (transition rejected [:performance/reorder-priority-set next-capture "4:1" (get-in rejected (conj path :priority-draft))])
        replaced (assoc-in rejected [:show :epoch] 5)
        next-epoch (assoc-in next-capture [:identity 1] 5)]
    (is (= "-3.5" (get-in (:db sent) (conj path :priority-draft))))
    (is (= "-3.5" (get-in rejected (conj path :priority-draft))))
    (is (= "Show revision changed" (get-in rejected (conj path :error))))
    (is (nil? (get-in rejected (conj path :request))))
    (is (= {:instance-id "4:1" :priority -3.5 :expected-revision 8} (:fields (command retried))))
    (is (empty? (:fx (transition replaced [:performance/reorder-priority-set next-epoch "4:1" "-3.5"]))))))

(deftest grouped-xyz-keeps-editable-base-and-plots-effective-components
  (let [components [[:x {:default 1}] [:y {:default 2}] [:z {:default 3}]]
        instance {:id "4:1" :params {"x" 0.25 :z 0.7}}
        show {:modulation {:overrides {(keyword "4:1") {"y" 0.5 :z 0}}}}]
    (is (= {:base [0.25 2 0.7] :effective [0.25 0.5 0]} (performance/xyz-readings show instance components)))))

(deftest strip-layout-uses-schema-and-saved-precedence-without-losing-controls
  (let [definition {:tags [:movement]
                    :params {:pan {} :tilt {} :aim {} :fine {:ui/order 1} :user/level {}}
                    :ui/layout {:template :movement :monitor :pan}}
        ids (fn [cue] (mapv first (:parameters (layout/resolve-layout definition cue))))
        cue {:ui/layout {:order ["user/level" :missing :tilt :tilt] :monitor :tilt}}]
    (is (= [:fine :aim :pan :tilt :user/level] (ids nil)))
    (is (= [:user/level :tilt :fine :aim :pan] (ids cue)))
    (is (= :tilt (:monitor (layout/resolve-layout definition cue))))
    (is (= :pan (:monitor (layout/resolve-layout definition nil))))
    (is (= #{:fine :pan :tilt :aim :user/level}
           (set (ids {:ui/layout :unavailable}))))))

(deftest each-effect-has-its-own-monitor-selection
  (let [chosen (:db (transition base [:performance/monitor-select "4:2" "tilt"]))]
    (is (= {"4:2" "tilt"} (get-in chosen [:ui :performance :monitor])))
    (is (nil? (get-in (:db (transition chosen [:performance/runtime-change])) [:ui :performance :monitor])))))

(deftest grouped-xyz-and-an-independent-aim-parameter-keep-distinct-monitors
  (let [param {:parameter :aim :schema {:type :vector :size 3}}
        group {:attribute :aim :components [[:x {}] [:y {}] [:z {}]]}
        sibling (assoc group :components [[:a {}] [:b {}] [:c {}]])
        namespaced {:parameter :group/aim :schema {:type :number}}
        controls [group sibling namespaced param]]
    (is (= 4 (count (set (map performance/control-id controls)))))
    (is (= param (:selected (performance/resolve-monitor controls nil :aim))))
    (doseq [control controls]
      (is (= control (:selected (performance/resolve-monitor controls (performance/control-id control) :aim)))))
    (is (= group (:selected (performance/resolve-monitor [group] nil :aim))))))

(deftest grouped-observations-use-the-schema-of-each-declared-axis
  (let [scalar {:type :number :bounds [0 1]}
        parameters [[:z scalar] [:x (assoc scalar :dmx-bits 16)] [:y (assoc scalar :dmx-bits 8)]]
        aim {:op :vec3 :args (mapv (fn [axis] {:op :param :key axis}) [:x :y :z])}
        [group] (inspect/xyz-controls {:outputs {:aim aim}} parameters)
        schema (performance/xyz-schema group)
        point [(/ 32768 65535) (/ 127 255) 0.000123456789]
        [_ _ _ caption] (visual/space schema point point)
        [_ _ [_ readings]] (visual/history-chart schema [{:beat 0 :values point}] 4)
        all-fine (update group :components #(mapv (fn [[id axis]] [id (assoc axis :dmx-bits 16)]) %))]
    (is (= [:x :y :z] (mapv first (:components group))))
    (is (nil? (:dmx-bits schema)))
    (is (= 16 (:dmx-bits (performance/xyz-schema all-fine))))
    (is (= [:figcaption "0.50001 · 0.498 · 0.000123"] caption))
    (is (= ["X 0.50001" "Y 0.498" "Z 0.000123"]
           (mapv #(apply str (drop 2 %)) readings)))))
