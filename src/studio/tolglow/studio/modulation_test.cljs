(ns tolglow.studio.modulation-test
  (:require [cljs.test :refer-macros [deftest is]]
            [re-frame.db :as rf-db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [tolglow.studio.events :as events]
            [tolglow.studio.modulation :as modulation]))

(def effect {:id "red" :name "Red wash" :categories ["color" "intensity"]
             :params {:level {:type "number" :default 0.7 :bounds [0 1] :recommended [0.2 0.8]}
                      :tint {:type "color" :default [1 0 0]}}})

(def base
  (assoc (events/initial-db "test")
         :show {:runtime-id "runtime-a" :epoch 4 :transport {:beat 10 :playing? true}
                :effects [effect] :cues [{:id "red" :effect-id "red"}]
                :instances [{:id "4:1" :effect-id "red" :cue-id "red" :definition effect :params {:level 0.7}}]
                :modulation {:controllers [] :holds {} :overrides {}}}))

(defn transition [db event]
  (reset! rf-db/app-db db)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %))
                                               (registrar/get-handler :event (first event))))))

(deftest musical-fractions-and-namespaced-range-keys-remain-json-safe
  (let [config (modulation/build-config
                (merge modulation/defaults {:every "1/2" :unit "bars" :scope "categories" :targets ["movement"]
                                            :param-mode "selected" :selected-params [:user/width]
                                            :ranges {:user/width {:enabled? true :low "1/4" :high "2"}}}))]
    (is (= {:bars 0.5} (:every config)))
    (is (= {:categories ["movement"]} (:select config)))
    (is (= ["user/width"] (:params config)))
    (is (= {"user/width" [0.25 2]} (:ranges config)))
    (is (= "glide" (:mode config)))
    (is (= 0.5 (modulation/number-value "0.5" "Value")))))

(deftest invalid-editing-input-rejects-before-an-http-effect
  (doseq [raw ["" "1/0" "1/2/3" "Infinity" "wat"]]
    (is (thrown? js/Error (modulation/number-value raw "Interval"))))
  (doseq [change [{:every "0"} {:unit "ms"} {:seed "1/2"} {:mode "other"}
                  {:scope "instances" :targets []} {:param-mode "selected" :selected-params []}
                  {:max-change "-0.1"} {:ranges {"level" {:enabled? true :low "1" :high "0"}}}]]
    (let [result (transition (assoc-in base [:ui :modulation] (merge modulation/defaults change)) [:modulation/start])]
      (is (nil? (:studio/http result)))
      (is (string? (get-in result [:db :ui :modulation :errors :global]))))))

(deftest starting-and-saving-cues-use-real-guarded-http-contracts
  (let [db (assoc-in base [:ui :page-id] "page-a")
        started (transition db [:modulation/start])
        cue (transition db [:modulation/save-cue])]
    (is (= "/api/modulation/start" (get-in started [:studio/http :path])))
    (is (= 4 (get-in started [:studio/http :body :expected-epoch])))
    (is (= "runtime-a" (get-in started [:studio/http :body :runtime-id])))
    (is (= {:beats 1} (get-in started [:studio/http :body :every])))
    (is (= "all" (get-in started [:studio/http :body :params])))
    (is (= "/api/modulation/cue" (get-in cue [:studio/http :path])))
    (is (= "page-a" (get-in cue [:studio/http :body :page-id])))
    (is (nil? (get-in cue [:studio/http :body :modulation :id])))))

(deftest per-parameter-controls-use-recommended-bounds-and-exact-instance-identity
  (let [schema (get-in effect [:params :level])
        form (modulation/widget-form modulation/defaults "4:1" :level schema)
        config (modulation/widget-config form "4:1" :level schema "owned")
        started (transition base [:modulation/widget-start "4:1" :level schema])
        hold (transition base [:modulation/hold "4:1" :level "1/2"])
        resume (transition base [:modulation/resume "4:1" :level])]
    (is (= {"level" [0.2 0.8]} (:ranges config)))
    (is (= {:instance-ids ["4:1"]} (:select config)))
    (is (= ["level"] (:params config)))
    (is (= {:instance-ids ["4:1"]} (get-in started [:studio/http :body :select])))
    (is (= 10.5 (get-in hold [:studio/http :body :until-beat])))
    (is (= "4:1" (get-in hold [:studio/http :body :instance-id])))
    (is (= ["level"] (get-in resume [:studio/http :body :params])))))

(deftest cue-owned-controller-stops-through-its-own-instance
  (let [controller {:id ["cue" "4:owner"] :owner-instance-id "4:owner"}
        stopped (transition base [:modulation/stop controller nil])]
    (is (= [:studio/command "instance/end" {:instance-id "4:owner"}] (:dispatch stopped)))
    (is (nil? (:studio/http stopped)))))

(deftest show-replacement-ignores-late-acks-and-clears-transient-widget-state
  (let [old-token (modulation/request-token base)
        with-widget (assoc-in base [:ui :modulation :widgets ["4:1" "level"]] {:expanded? true})
        changed (transition (assoc-in with-widget [:show :epoch] 5) [:modulation/runtime-change])
        late (transition (:db changed) [:modulation/accepted old-token :global "Old success" {}])]
    (is (= {} (get-in changed [:db :ui :modulation :widgets])))
    (is (empty? late))
    (is (not= old-token (modulation/request-token (:db changed))))))

(deftest target-and-hold-projections-follow-server-data
  (let [show (:show base)
        form (merge modulation/defaults {:scope "instances" :targets ["4:1"]})
        params (modulation/parameter-options show form)
        held (assoc-in show [:modulation :holds (keyword "4:1") :level] {:until-beat 12})]
    (is (= ["color" "intensity"] (mapv :id (modulation/target-options show "categories"))))
    (is (= #{"level" "tint"} (set (map :id params))))
    (is (= [0.2 0.8] (:domain (first (filter #(= "level" (:id %)) params)))))
    (is (modulation/held? held "4:1" :level))
    (is (not (modulation/held? (assoc-in held [:transport :beat] 12) "4:1" :level)))))
