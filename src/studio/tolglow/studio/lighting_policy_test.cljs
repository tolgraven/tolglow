(ns tolglow.studio.lighting-policy-test
  (:require [cljs.test :refer-macros [deftest is]]
            [tolglow.studio.events-test :refer [transition]]
            [tolglow.studio.lighting-policy :as policy]
            [tolglow.studio.lighting-response :as response]))

(def identity-key {:epoch 2 :runtime-id "show-process" :show-id :club})

(def db {:connection {:connected? true} :server-projection {:view identity-key}})

(deftest budget-edits-are-typed-and-local-to-the-selected-scope
  (is (= {:groups {"side" {:max-active 0 :standby-level 0.04 :strategy :priority :fixture-order [:rig/wash "head"]}}}
         (policy/patch-values :lighting-budget [:groups "side"]
                              {:max-active "0" :standby-level 0.04 :strategy :priority :fixture-order [:rig/wash "head"]})))
  (is (= {:global {:max-active nil}} (policy/patch-values :lighting-budget [:global] {:max-active ""})))
  (doseq [value ["-1" "0.5" "all"]]
    (is (thrown? js/Error (policy/patch-values :lighting-budget [:global] {:max-active value})))))

(deftest only-edited-response-stages-are-sent
  (is (= {:groups {:side {:release {:beats 0.375}}}}
         (policy/patch-values :output-response [:groups :side] {:release {:value "3/8" :unit "beats"}})))
  (is (thrown? js/Error (policy/patch-values :output-response [:global] {:attack {:value "-1" :unit "beats"}}))))

(deftest policy-writes-use-the-show-identity-and-discard-stale-requests
  (let [req (policy/save-request db :lighting-budget {:groups {:wash nil}} nil)]
    (is (= {:scope :lighting-budget :values {:groups {:wash nil}} :expected-epoch 2 :runtime-id "show-process"}
           (get-in req [:studio/http :body])))
    (is (true? (get-in req [:studio/http :typed?])))
    (is (nil? (policy/save-request (:db req) :lighting-activity {:enabled? true} nil)))
    (is (nil? (policy/save-request (assoc-in db [:connection :connected?] false) :lighting-activity {:enabled? true} nil)))
    (is (some? (policy/save-request (assoc-in (:db req) [:server-projection :view :epoch] 3)
                                    :lighting-activity {:enabled? false} nil)))))

(deftest acknowledged-settings-follow-the-next-authoritative-projection
  (let [live (assoc-in db [:server-projection :view :revision] 4)
        state (:db (policy/save-request live :lighting-budget {:enabled? true} nil))
        acked (update state :lighting-policy-ui dissoc :pending)]
    (is (true? (:enabled? (policy/visible-settings acked :lighting-budget))))
    (is (nil? (policy/visible-settings (assoc-in acked [:server-projection :view :revision] 5) :lighting-budget)))
    (is (nil? (policy/visible-settings (assoc-in state [:server-projection :view :epoch] 3) :lighting-budget)))))

(deftest bound-response-stages-roundtrip-with-typed-identities-and-a-fallback
  (doseq [stored [{:beats {:cue-id :rig/feel :param "attack" :default 0.25}}
                  {:bars {:variable [:user :cue-rate] :default 0.5}}]]
    (is (= stored (response/stage-duration (response/stage-form stored)))))
  (is (= {:beats {:variable [:audio/energy] :default 0.25}}
         (response/stage-duration {:source :variable :unit "beats" :path "[:audio/energy]" :value "1/4"})))
  (is (thrown? js/Error (response/stage-duration {:source :cue :unit "beats" :value "0" :cue-id :rig/feel})))
  (is (thrown? js/Error (response/stage-duration {:source :variable :unit "beats" :value "1" :path ":audio/energy"}))))

(deftest rapid-stage-changes-edit-the-latest-draft-between-renders
  (let [section [:groups :front]
        input (fn [state fields]
                (:db (transition state [::policy/edit-stage section :release {:beats 0} fields])))
        state (-> db (input {:source :variable}) (input {:path "[:audio/energy]"})
                  (input {:value "3/8"}) (input {:unit "bars"}))
        stage (get-in state [:lighting-policy-ui :drafts [:output-response section] :release])]
    (is (= {:bars {:variable [:audio/energy] :default 0.375}} (response/stage-duration stage)))))

(deftest bound-limits-and-an-agent-cap-under-one-read-as-text
  (is (= "limit-cue · level" (policy/limit-label {:cue-id :limit-cue :param :level :default 0.4})))
  (is (= "limit-cue · level, at most 50%"
         (policy/limit-label {:op :min :args [{:cue-id :limit-cue :param :level :default 0.4} 0.5] :default 0.4}))))
