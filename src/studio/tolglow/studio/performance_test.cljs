(ns tolglow.studio.performance-test
  (:require [cljs.test :refer-macros [deftest is]]
            [re-frame.db :as rf-db]
            [tolglow.authoring.parameter-layout :as layout]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [tolglow.studio.events :as events]
            [tolglow.studio.performance :as performance]))

(def base
  (assoc (events/initial-db "performance-session")
         :show {:runtime-id "runtime-a" :epoch 4 :revision 7
                :groups [{:id "color" :color "#bb66dd" :instance-ids ["4:1"] :paused? false}
                         {:id "intensity" :color "#ddbb66" :instance-ids ["4:1" "4:2"] :paused? false}
                         {:id "movement" :color "#66bbdd" :instance-ids ["4:2" "4:3"] :paused? false}]
                :instances [{:id "4:1" :cue-id "user/wash" :system {:opacity 0.8 :paused? false}}
                            {:id "4:2" :cue-id "user/sweep" :system {:opacity 0.6 :paused? true :release {:beats 2}}}
                            {:id "4:3" :cue-id "user/aim" :system {:opacity 1 :paused? false}}]}))
(def instance-target {:instance-id "4:1"})
(def group-target {:selector {:categories ["color" "movement"] :match "any"}})
(defn transition [db event]
  (reset! rf-db/app-db db)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %))
                                               (registrar/get-handler :event (first event))))))
(defn command [result] (second (first (:fx result))))

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
        expanded (transition base [:performance/card-toggle "4:1"])]
    (is (empty? (performance/selected-instances (:show base) (get-in cleared [:db :ui :performance :group]))))
    (is (= true (get-in expanded [:db :ui :performance :expanded "4:1"])))
    (is (= (:show base) (get-in expanded [:db :show])))
    (is (nil? (:fx expanded)))))

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
