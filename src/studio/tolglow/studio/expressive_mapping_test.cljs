(ns tolglow.studio.expressive-mapping-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [re-frame.db :as rf-db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [tolglow.studio.events :as events]
            [tolglow.studio.expressive-mapping :as expressive]))

(def schema {:type "number" :bounds [0 1] :default 0.5})
(def preview {:device-id "stage/push" :cue-id "user/wash" :effect-id "wash" :effect-version 2
              :axes {:pressure {:range [0 1]} :glide {:range [-1 1]}}
              :schemas {:user/level schema :color {:type "color" :default [1 0 0]}}
              :mappings [{:param "user/level" :axis "pressure" :origin "automatic" :schema schema}]
              :samples [{:position 0 :values {:user/level 0}} {:position 1 :values {:user/level 1}}]})
(def base
  (expressive/open-state
    (assoc (events/initial-db "test") :show
           {:runtime-id "r1" :epoch 2 :cues [{:id "user/wash" :effect-id "wash"} {:id "user/move" :effect-id "move"}]
            :settings {:expressive {:devices {:stage/push {:auto? true :params {:user/level {:axis "pressure"}}}
                                             :stage/roli {:enabled? false}}}}})))
(def ready (assoc-in base [:expressive-mapping :preview] preview))
(defn transition [db event]
  (reset! rf-db/app-db db)
  (let [effects (:effects (interceptor/execute event (remove #(= :do-fx (:id %))
                                                           (registrar/get-handler :event (first event)))))]
    (cond-> effects (:studio/http effects)
      (update-in [:studio/http :body] #(js->clj (clj->js %) :keywordize-keys true)))))
(defn db-after [db event] (:db (transition db event)))

(deftest namespaced-identities-and-device-preservation
  (is (= "stage/push" (:device-id (expressive/state base))))
  (is (= "user/wash" (:cue-id (expressive/state base))))
  (let [edited (db-after ready [:expressive/mode :user/level "off"])
        request (transition edited [:expressive/save])]
    (is (nil? (get-in (expressive/device-policy edited) [:cues :user/wash :params :user/level])))
    (is (contains? (get-in (expressive/device-policy edited) [:cues :user/wash :params]) :user/level))
    (is (= "stage/push" (get-in request [:studio/http :body :device-id])))
    (is (= {:axis "pressure"} (get-in request [:studio/http :body :values :params :user/level])))
    (is (= "expressive" (get-in request [:studio/http :body :scope])))
    (is (= "r1" (get-in request [:studio/http :body :runtime-id])))
    (is (= 2 (get-in request [:studio/http :body :expected-epoch])))))

(deftest reopening-connections-keeps-save-completion-live
  (let [saving (transition ready [:expressive/save])
        reopened (db-after (:db saving) [:expressive/open])
        ok (get-in saving [:studio/http :on-success])
        fail (get-in saving [:studio/http :on-failure])]
    (is (false? (get-in (db-after reopened (conj ok {:status "accepted"})) [:expressive-mapping :saving?])))
    (is (false? (get-in (db-after reopened (conj fail "failed")) [:expressive-mapping :saving?])))))

(deftest manual-binding-comes-from-backend-plan-not-client-semantics
  (let [result (db-after ready [:expressive/mode :user/level "manual"])]
    (is (= {:axis "pressure"} (get-in (expressive/device-policy result) [:cues :user/wash :params :user/level])))
    (is (= "manual" (expressive/parameter-mode (expressive/scoped-policy result) :user/level)))
    (is (= preview (:preview (expressive/state result))))
    (is (= "inherit" (expressive/parameter-mode (expressive/scoped-policy
                                                 (db-after result [:expressive/mode :user/level "inherit"])) :user/level)))))

(deftest switching-cues-and-devices-retains-independent-drafts
  (let [wash (db-after ready [:expressive/mode :user/level "off"])
        move (db-after wash [:expressive/select :cue-id "user/move"])
        edited (db-after move [:expressive/edit [:cues :user/move :auto?] false])
        roli (db-after edited [:expressive/select :device-id "stage/roli"])
        back (db-after roli [:expressive/select :device-id "stage/push"])]
    (is (= "off" (expressive/parameter-mode (get-in (expressive/device-policy back) [:cues :user/wash]) :user/level)))
    (is (false? (get-in (expressive/device-policy back) [:cues :user/move :auto?])))
    (is (= {:enabled? false} (expressive/device-policy roli)))
    (is (= #{:stage/push} (:dirty (expressive/state back))))))

(deftest stale-previews-and-save-acks-do-not-cross-target-or-runtime
  (let [token (expressive/request-token ready)
        changes [[:expressive/select :cue-id "user/move"] [:expressive/select :device-id "stage/roli"]
                 [:expressive/select :device-kind "seaboard-m"] [:expressive/select :instance-id "2:4"]
                 [:expressive/edit [:auto?] false]]]
    (doseq [event changes :let [next (db-after ready event)]]
      (is (= next (db-after next [:expressive/previewed token preview])))
      (is (empty? (transition next [:expressive/saved token {:accepted? true}]))))
    (doseq [[key value] [[:runtime-id "r2"] [:epoch 3]] :let [changed (assoc-in ready [:show key] value)]]
      (is (= changed (db-after changed [:expressive/previewed token preview])))
      (let [reset (:db (transition changed [:expressive/sync]))]
        (is (empty? (:drafts (expressive/state reset))))
        (is (= value (get-in (expressive/state reset) [:context key])))
        (is (not (expressive/current? reset token)))))))

(deftest preview-uses-full-device-policy-and-captured-instance
  (let [db (-> ready (assoc-in [:expressive-mapping :device-kind] "mpe")
               (assoc-in [:expressive-mapping :instance-id] "2:4"))
        request (expressive/preview-request db)]
    (is (= "/api/expressive/preview" (:path request)))
    (is (= "stage/push" (get-in request [:body :device-id])))
    (is (= "user/wash" (get-in request [:body :cue-id])))
    (is (= "2:4" (get-in request [:body :instance-id])))
    (is (= ["strike" "pressure" "glide" "slide"] (get-in request [:body :axes])))
    (is (= (expressive/wire-data (expressive/device-policy db)) (get-in request [:body :policy])))
    (is (not (contains? (:body (expressive/preview-request ready)) :axes)))))

(deftest numeric-edits-preserve-incomplete-input-and-reject-nonfinite-values
  (let [manual (db-after ready [:expressive/mode :user/level "manual"])
        key [:cue "user/wash" :user/level 0 :range 0]
        event [:expressive/number-commit :user/level 0 :range 0 [0 1] key]
        editing (db-after manual [:expressive/number-edit key "-"])
        invalid (transition editing event)]
    (is (= "-" (get-in invalid [:db :expressive-mapping :numeric key])))
    (is (string? (get-in invalid [:db :expressive-mapping :error])))
    (is (nil? (:studio/http (transition editing [:expressive/save]))))
    (let [valid (db-after (db-after editing [:expressive/number-edit key "0.2"]) event)]
      (is (= [0.2 1] (get-in (expressive/device-policy valid) [:cues :user/wash :params :user/level :range])))
      (is (empty? (:numeric (expressive/state valid)))))))

(deftest compound-mapping-edits-preserve-other-components
  (let [policy {:cues {:user/wash {:params {:color [{:axis "pressure" :component 0} {:axis "slide" :component 1}]}}}}
        db (assoc-in ready [:expressive-mapping :drafts :stage/push] policy)
        updated (db-after db [:expressive/binding :color 1 :inverted? true])]
    (is (= [{:axis "pressure" :component 0} {:axis "slide" :component 1 :inverted? true}]
           (get-in (expressive/device-policy updated) [:cues :user/wash :params :color])))
    (let [removed (db-after updated [:expressive/component :color 0])]
      (is (= [{:axis "slide" :component 1 :inverted? true}]
             (get-in (expressive/device-policy removed) [:cues :user/wash :params :color]))))))

(deftest saving-freezes-edits-and-shows-rejected-outcomes
  (let [saving (:db (transition ready [:expressive/save]))
        token (expressive/request-token saving)
        rejected (transition saving [:expressive/saved token {:accepted? false :message "No such runtime"}])]
    (is (empty? (transition saving [:expressive/select :cue-id "user/move"])))
    (is (empty? (transition saving [:expressive/edit [:auto?] false])))
    (is (false? (get-in rejected [:db :expressive-mapping :saving?])))
    (is (= "No such runtime" (get-in rejected [:db :expressive-mapping :error])))))

(deftest device-and-cue-disables-remain-policy-data-only
  (let [device (transition ready [:expressive/edit [:enabled?] false])
        cue (transition ready [:expressive/edit [:cues :user/wash :enabled?] false])]
    (is (false? (:enabled? (expressive/device-policy (:db device)))))
    (is (false? (get-in (expressive/device-policy (:db cue)) [:cues :user/wash :enabled?])))
    (is (nil? (:studio/send-command device)))
    (is (nil? (:studio/send-command cue)))
    (is (nil? (:studio/http device)))))

(deftest root-inheritance-and-absent-removal-do-not-create-invalid-policy-keys
  (let [edited (db-after ready [:expressive/edit [:auto?] nil true])
        reset (db-after ready [:expressive/edit [:cues :absent] nil true])]
    (is (= {:params {:user/level {:axis "pressure"}}} (expressive/device-policy edited)))
    (is (= (expressive/device-policy ready) (expressive/device-policy reset)))
    (is (not (contains? (expressive/device-policy edited) nil)))))

(deftest policy-json-retains-namespaces-and-explicit-null
  (let [policy {:params {:user/level nil} :cues {:user/wash {:params {:user/color {:axis :pressure}}}}}
        wire (js/JSON.parse (js/JSON.stringify (clj->js (expressive/wire-data policy))))]
    (is (nil? (aget (aget wire "params") "user/level")))
    (is (= "pressure" (aget (aget (aget (aget (aget wire "cues") "user/wash") "params") "user/color") "axis")))))
