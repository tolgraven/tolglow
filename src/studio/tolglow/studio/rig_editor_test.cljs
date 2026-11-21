(ns tolglow.studio.rig-editor-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [re-frame.core :as rf]
            [re-frame.db :as rf-db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [tolglow.studio.events]
            [tolglow.studio.rig-editor :as editor]))
(def rig {:fixtures [{:id "head" :position [0 3 0] :rotation-deg [0 0 0] :address 17 :channels {:pan [1 2] :tilt [3 4]} :calibration {:pan [128 255]}}]
          :trusses [{:id "bar" :position [0 4 0] :length 4}]})
(def base {:show {:show-id "show" :runtime-id "runtime" :epoch 2 :rig rig} :ui {:rig-draft rig}})
(defn input [db type more] (editor/interaction db (merge {:type type :identity (editor/identity-key db) :token 1} more)))
(defn tool [db name] (editor/put-editor db (assoc (editor/editor db) :tool name)))
(deftest placement-is-draft-only-and-gesture-cancel-restores
  (let [start (tool base "move")
        begun (input start "begin" {:selection {:kind "fixtures" :id "head"} :point [0 3 0]})
        moved (input begun "move" {:point [2 3 1]})
        cancelled (input moved "cancel" {})]
    (is (= [2 3 1] (get-in moved [:ui :rig-draft :fixtures 0 :position])))
    (is (= rig (get-in moved [:show :rig])))
    (is (= (dissoc (first (:fixtures rig)) :position) (dissoc (first (get-in moved [:ui :rig-draft :fixtures])) :position)))
    (is (= rig (get-in cancelled [:ui :rig-draft])))
    (is (empty? (:undo (editor/editor cancelled))))
    (is (= moved (input moved "move" {:token 42 :point [9 9 9]})))
    (is (= moved (editor/interaction moved {:type "move" :identity "old-show" :token 1 :point [9 9 9]})))))
(deftest rotation-is-relative-to-gesture-start-and-chosen-plane
  (let [db (-> (tool base "rotate") (assoc-in [:ui :rig-editor :plane] "xy"))
        begun (input db "begin" {:selection {:kind "trusses" :id "bar"} :point [0 4 0]})
        first-move (input begun "move" {:angle 10 :point [0 4 0]})
        second-move (input first-move "move" {:angle 20 :point [0 4 0]})]
    (is (= [0 0 20] (get-in second-move [:ui :rig-draft :trusses 0 :rotation-deg])))
    (is (= 4 (get-in second-move [:ui :rig-draft :trusses 0 :length])))))
(deftest paths-are-editable-musical-data-and-request-is-fenced
  (let [db (tool base "draw")
        first-point (-> db (input "begin" {:point [0 0 0]}) (input "end" {}))
        second-point (-> first-point (input "begin" {:point [2 0 0]}) (input "move" {:point [3 0 0]}) (input "end" {}))
        state (assoc (editor/editor second-point) :targets ["head"])
        db (editor/put-editor second-point state)
        request (editor/publish-request db false "request")]
    (is (= [[0 0 0] [3 0 0]] (:points state)))
    (is (= rig (get-in db [:ui :rig-draft])))
    (is (= {:bars 4} (get-in request [:body :duration])))
    (is (= 2 (get-in request [:body :expected-epoch])))
    (is (= "runtime" (get-in request [:body :runtime-id])))
    (is (= ["head"] (get-in request [:body :targets])))
    (is (= [:rig-editor/published (editor/identity-key db) "request" nil] (:on-success request)))
    (is (empty? (:points (editor/editor (assoc-in db [:show :epoch] 3)))))))
(deftest undo-discard-and-stale-publication
  (let [restore (rf/make-restore-fn)]
    (try
      (reset! rf-db/app-db (editor/put-editor base (assoc (editor/editor base) :selection {:kind "fixtures" :id "head"})))
      (rf/dispatch-sync [:rig-editor/nudge 0 0.1])
      (is (= [0.1 3 0] (get-in @rf-db/app-db [:ui :rig-draft :fixtures 0 :position])))
      (rf/dispatch-sync [:rig-editor/undo])
      (is (= rig (get-in @rf-db/app-db [:ui :rig-draft])))
      (rf/dispatch-sync [:rig-editor/add-point])
      (rf/dispatch-sync [:rig-editor/cancel])
      (is (empty? (:points (editor/editor @rf-db/app-db))))
      (let [before @rf-db/app-db]
        (rf/dispatch-sync [:rig-editor/published "old" "stale" true {:cue-id "bad"}])
        (is (= before @rf-db/app-db)))
      (finally (restore)))))

(deftest show-replacement-drops-old-rig-drafts-before-rendering
  (let [old (-> (tool base "move") (assoc-in [:ui :rig-draft :fixtures 0 :position] [9 9 9]) (assoc-in [:ui :rig-dirty?] true))
        next (editor/rebase-editor (assoc-in old [:show :epoch] 3))]
    (is (= rig (get-in next [:ui :rig-draft])))
    (is (false? (get-in next [:ui :rig-dirty?])))
    (is (= (editor/identity-key next) (:identity (editor/editor next))))))

(defn transition [db event]
  (reset! rf-db/app-db db)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %)) (registrar/get-handler :event (first event))))))
(deftest publication-is-an-explicit-versioned-choice
  (let [restore (rf/make-restore-fn)]
    (try
      (let [state (assoc (editor/editor base) :publishing "request" :targets ["head"] :points [[0 0 0] [1 0 0]])
            db (editor/put-editor base state) identity (editor/identity-key db)
            request (editor/publish-request db false "request")
            ack {:effect-id "drawn-path" :cue-id "drawn-path" :version 1}
            created (transition db [:rig-editor/published identity "request" nil ack])
            run (transition db [:rig-editor/published identity "request" "one-shot" ack])]
        (is (contains? (:body request) :expected-version))
        (is (nil? (get-in request [:body :expected-version])) "New IDs cannot overwrite an existing path")
        (is (nil? (:studio/send-command created)))
        (is (= "one-shot" (get-in run [:studio/send-command :fields :launch :mode])))
        (is (= "drawn-path" (get-in run [:studio/send-command :fields :cue-id])))
        (is (nil? (:studio/send-command (transition db [:rig-editor/published identity "other-request" "one-shot" ack]))))
        (is (nil? (:studio/send-command (transition (assoc-in db [:show :epoch] 3) [:rig-editor/published identity "request" "one-shot" ack])))))
      (finally (restore)))))
(deftest reopen-saved-path-preserves-its-version-and-cue-identity
  (let [restore (rf/make-restore-fn)]
    (try
      (let [effect {:id "saved" :version 7 :name "Saved path" :targets ["head"]
                    :editor {:type "aim-path" :points [[1 0 0] [2 1 0]] :duration {:beats 3.5} :mode "ping-pong" :curve "sine" :closed? false}}
            db (-> base (assoc-in [:show :effects] [effect]) (assoc-in [:show :cues] [{:id "saved-cue" :effect-id "saved"}]))
            loaded (:db (transition db [:rig-editor/load-path "saved"]))
            state (editor/editor loaded) request (editor/publish-request loaded false "next")]
        (is (= (:points (:editor effect)) (:points state)))
        (is (= {:beats 3.5} (get-in request [:body :duration])))
        (is (= 7 (get-in request [:body :expected-version])))
        (is (= "saved-cue" (get-in request [:body :cue-id])))
        (is (= "draw" (:tool state))))
      (finally (restore)))))
