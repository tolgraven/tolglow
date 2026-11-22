(ns tolglow.studio.events-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [cljs.reader :as reader]
            [clojure.string :as str]
            [re-frame.db :as rf-db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [tolglow.studio.events :as events]
            [tolglow.studio.projection :as projection]
            [tolglow.studio.fixture-library :as fixtures]
            [tolglow.studio.connections :as connections]
            [tolglow.studio.chat]))

;; Execute actual registered event handlers/interceptors while leaving their
;; declared I/O effects unapplied. This tests transitions, not duplicate stubs.
(defn transition [db event]
  (reset! rf-db/app-db db)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %))
                                               (registrar/get-handler :event (first event))))))
(def base
  (assoc (events/initial-db "browser-session")
         :connection {:connected? true}
         :show {:epoch 7 :revision 3 :frame-number 12 :master 0.7
                :cues [{:id "ember" :effect-id "ember" :duration {:beats 4}}]
                :effects [{:id "ember" :version 2}]
                :pages [{:id "main" :cells []}]
                :instances [{:id "7:9" :cue-id "ember" :phase "running" :params {:level 0.7}}]}))
(defn with-stream [db]
  (:db (transition db [:studio/stream-opening (:session db) "connected"])))

(defn stream-event [db envelope]
  (transition db [:studio/stream-frame (:session db) "connected" envelope]))

(defn full-frame [db view]
  (stream-event (with-stream db)
                {:protocol projection/protocol :kind :full :token "full"
                 :identity (projection/identity-of view) :view view}))

(defn first-command [effects] (second (first (:fx effects))))

(deftest rapid-rotary-keypresses-use-current-app-db-instead-of-a-stale-rendered-value
  (let [schema {:type "number" :bounds [0 1]}
        initial (assoc-in base [:show :instances 0 :params :level] 0)
        updated (reduce (fn [state _] (:db (transition state [:studio/param-key "7:9" :level schema "PageUp"]))) initial (range 7))]
    (is (< (js/Math.abs (- 0.7 (get-in updated [:show :instances 0 :params :level]))) 0.000001))
    (is (< (js/Math.abs (- 0.7 (get-in updated [:drafts ["7:9" :level] :fields :value]))) 0.000001))))

(deftest released-before-acknowledgement-ends-exact-instance
  (let [db (assoc-in base [:ui :launch-mode] "held")
        down (transition db [:studio/gesture-down {:pointer-id 11 :cue-id "ember" :x 0 :y 0 :client-x 5 :client-y 5}])
        released (transition (:db down) [:studio/gesture-up 11])
        ack (transition (:db released) [:studio/held-launched 11 {:instance-id "7:10"}])]
    (is (= "cue/launch" (:op (first-command down))))
    (is (= "held" (get-in (first-command down) [:fields :launch :mode])))
    (is (true? (get-in released [:db :pointers 11 :released?])))
    (is (= "instance/end" (:op (first-command ack))))
    (is (= "7:10" (get-in (first-command ack) [:fields :instance-id])))
    (is (= 7 (:epoch (first-command ack))))
    (is (empty? (get-in ack [:db :pointers])))))

(deftest independent-pointers-and-page-exit-release
  (let [db (assoc base :pointers {1 {:held? true :instance-id "7:11" :epoch 7}
                                 2 {:held? true :instance-id "7:12" :epoch 7}})
        one (transition db [:studio/gesture-cancel 1])
        exit (transition (:db one) [:studio/release-pointers true])]
    (is (= #{2} (set (keys (get-in one [:db :pointers])))))
    (is (= "7:11" (get-in (first-command one) [:fields :instance-id])))
    (is (= "7:12" (get-in (first-command exit) [:fields :instance-id])))
    (is (true? (:beacon? (first-command exit))))
    (is (empty? (get-in exit [:db :pointers])))))

(deftest long-press-opens-actions-without-launch
  (let [down (transition base [:studio/gesture-down {:pointer-id 1 :cue-id "ember" :x 0 :y 0 :client-x 5 :client-y 5}])
        long (transition (:db down) [:studio/gesture-long 1])
        up (transition (:db long) [:studio/gesture-up 1])]
    (is (= 500 (get-in down [:dispatch-later 0 :ms])))
    (is (= :studio/open-actions (first (:dispatch long))))
    (is (nil? (:dispatch up)))
    (is (empty? (:fx up)))))

(deftest optimistic-parameter-persists-until-acknowledged-revision
  (let [edited (transition base [:studio/param-set "7:9" :level 1])
        drafts (get-in edited [:db :drafts])
        old-frame (:show base)
        overlay (events/apply-drafts old-frame (events/outstanding-drafts drafts old-frame))
        acked (transition (:db edited) [:studio/param-ack ["7:9" :level] 1 7 nil {:revision 4}])
        ack-drafts (get-in acked [:db :drafts])]
    (is (= 1 (get-in overlay [:instances 0 :params :level])))
    (is (seq (events/outstanding-drafts ack-drafts old-frame)))
    (is (empty? (events/outstanding-drafts ack-drafts (assoc old-frame :revision 4))))
    (is (empty? (events/outstanding-drafts ack-drafts (assoc old-frame :epoch 8))))
    (let [newer (transition (:db edited) [:studio/param-set "7:9" :level 0.999])
          old-ack (transition (:db newer) [:studio/param-ack ["7:9" :level] 1 7 nil {:revision 4}])]
      (is (= 0.999 (get-in old-ack [:db :drafts ["7:9" :level] :fields :value])))
      (is (nil? (get-in old-ack [:db :drafts ["7:9" :level] :revision]))))))

(deftest parameter-writes-coalesce-without-losing-latest-value
  (let [first (transition base [:studio/queue-param ["7:9" :level] "param/set" {:instance-id "7:9" :param "level" :value 0.8}])
        second (transition (:db first) [:studio/queue-param ["7:9" :level] "param/set" {:instance-id "7:9" :param "level" :value 0.9}])
        flushed (transition (:db second) [:studio/flush-params])]
    (is (= 1 (count (get-in second [:db :pending-params]))))
    (is (= 1 (count (:fx flushed))))
    (is (= 0.9 (get-in (first-command flushed) [:fields :value])))
    (is (= 7 (:epoch (first-command flushed))))))

(deftest musical-authoring-remains-data
  (let [definition (events/builder-definition (get-in base [:ui :builder]) 3)
        added (transition base [:studio/composition-add "ember"])
        changed (transition (:db added) [:studio/slot-duration 0 0.5 "bars"])]
    (is (= {:beats 4} (:duration definition)))
    (is (= {:beats 1} (get-in definition [:envelope :release])))
    (is (= {:beats 1} (get-in added [:db :ui :composition :slots 0 :duration])))
    (is (= {:bars 0.5} (get-in changed [:db :ui :composition :slots 0 :duration])))
    (is (nil? (get-in changed [:db :ui :composition :slots 0 :duration-ms])))
    (is (thrown? js/Error (events/musical-duration js/NaN "beats")))))

(deftest existing-definition-publication-is-optimistic
  (let [db (update-in base [:ui :definition] merge {:loaded-id "ember" :kind "effects" :version 2})
        publish (transition db [:studio/publish-source false nil "{:id :ember :version 2}"])]
    (is (= 2 (get-in publish [:studio/http :body :expected-version])))
    (is (= "{:id :ember :version 2}" (get-in publish [:studio/http :body :source])))
    (is (true? (get-in publish [:db :ui :definition :publishing?])))))

(deftest duration-field-edits-keep-the-other-current-field
  (let [added (transition base [:studio/composition-add "ember"])
        unit (transition (:db added) [:studio/slot-duration 0 nil "bars"])
        value (transition (:db unit) [:studio/slot-duration 0 0.5 nil])]
    (is (= {:bars 1} (get-in unit [:db :ui :composition :slots 0 :duration])))
    (is (= {:bars 0.5} (get-in value [:db :ui :composition :slots 0 :duration])))))

(deftest stale-chat-stream-cannot-change-a-new-conversation
  (let [db (assoc base :chat {:current-id "new-run" :running? true
                             :entries [{:id "new-run" :role :assistant :text "Current" :tools []}]})
        stale (transition db [:chat/event "old-run" {:type "text/delta" :text "Wrong"}])
        closed (transition (:db stale) [:chat/closed "old-run"])
        current (transition (:db closed) [:chat/event "new-run" {:type "text/delta" :text " reply"}])]
    (is (= "Current" (get-in stale [:db :chat :entries 0 :text])))
    (is (true? (get-in closed [:db :chat :running?])))
    (is (= "Current reply" (get-in current [:db :chat :entries 0 :text])))))

(deftest submitting-provider-configuration-clears-transient-credential
  (let [db (assoc-in base [:ui :provider-form] {:provider "openai" :model "test-model" :credential "test-key"})
        save (transition db [:chat/save-provider])]
    (is (= "" (get-in save [:db :ui :provider-form :credential])))
    (is (= "test-key" (get-in save [:studio/http :body :credential])))
    (is (= "/api/providers" (get-in save [:studio/http :path])))))

(deftest replacing-server-identity-accepts-a-reset-frame-order
  (let [old-show (assoc (:show base) :runtime-id "old-process" :epoch 8 :revision 40 :frame-number 200)
        new-show (assoc (:show base) :runtime-id "new-process" :epoch 0 :revision 0 :frame-number 1)
        db (assoc base :show old-show :snapshot-order [8 40 200])
        changed (full-frame db (assoc new-show :show-id :main))]
    (is (= "new-process" (get-in changed [:db :show :runtime-id])))
    (is (= [0 0 1] (get-in changed [:db :snapshot-order])))
    (is (= :studio/release-pointers (get-in changed [:fx 0 1 0])))))

(deftest old-held-release-keeps-original-process-identity
  (let [db (-> base (assoc :show (assoc (:show base) :runtime-id "replacement")
                           :pointers {1 {:held? true :instance-id "7:11" :epoch 7 :runtime-id "original"}})
               (assoc-in [:ui :rig-draft] {:fixtures []}))
        release (transition db [:studio/gesture-cancel 1])
        mutation (transition db [:studio/rig-save])]
    (is (= "original" (:runtime-id (first-command release))))
    (is (= "replacement" (get-in mutation [:studio/http :body :runtime-id])))
    (is (= 7 (get-in mutation [:studio/http :body :expected-epoch])))))

(deftest component-edits-compose-and-preserve-qualified-parameter-identities
  (let [db (assoc-in base [:show :instances 0 :params :rig/aim] [0 0 0])
        x (transition db [:studio/param-component "7:9" :rig/aim 0 0.4])
        y (transition (:db x) [:studio/param-component "7:9" :rig/aim 1 0.8])]
    (is (= [0.4 0.8 0] (get-in y [:db :show :instances 0 :params :rig/aim])))
    (is (= "rig/aim" (get-in y [:dispatch 3 :param])))))

(deftest old-process-acknowledgement-cannot-retire-a-new-process-draft
  (let [db (assoc-in base [:show :runtime-id] "replacement")
        edited (transition db [:studio/param-set "7:9" :level 1])
        stale (transition (:db edited) [:studio/param-ack ["7:9" :level] 1 7 "original" {:revision 400}])]
    (is (nil? (get-in stale [:db :drafts ["7:9" :level] :revision])))
    (is (= "replacement" (get-in stale [:db :drafts ["7:9" :level] :runtime-id])))))

(deftest incomplete-numeric-edit-survives-pushed-frames-until-blur
  (let [editing (transition base [:studio/numeric-edit "fixture-x" ""])
        frame (full-frame (:db editing) (assoc (:show base) :runtime-id "r" :show-id :main))
        ended (transition (:db frame) [:studio/numeric-blur "fixture-x"])]
    (is (= "" (get-in frame [:db :ui :numeric-edits "fixture-x"])))
    (is (nil? (get-in ended [:db :ui :numeric-edits "fixture-x"])))))

(deftest initialization-opens-one-stream-and-awaits-an-initial-full
  (let [initial (transition {} [:studio/initialize "s"])
        opened (with-stream (:db initial))
        patch (stream-event opened {:kind :patch :changes []})]
    (is (= "s" (:studio/open-stream initial)))
    (is (nil? (:studio/http initial)))
    (is (nil? (:dispatch-later initial)))
    (is (= ["connected" {:op :resync}] (:studio/stream-reply patch)))
    (is (nil? (get-in patch [:db :show])))))

(deftest delta-baseline-stays-independent-from-optimistic-knob-edits
  (let [view (assoc (:show base) :runtime-id "runtime" :show-id "main")
        full {:protocol projection/protocol :kind :full :token "base"
              :identity (projection/identity-of view) :view view}
        initial (:db (stream-event (with-stream base) full))
        edited (:db (transition initial [:studio/param-set "7:9" :level 0.9]))
        patch {:protocol projection/protocol :kind :patch :base-token "base" :token "next"
               :identity (projection/identity-of view)
               :changes [[:set [:frame-number] 13] [:set [:master] 0.3]]}
        next (:db (stream-event edited patch))
        acknowledged (-> next (assoc-in [:drafts ["7:9" :level] :revision] 4))
        echo (assoc patch :base-token "next" :token "echo"
                    :changes [[:set [:revision] 4] [:set [:instances 0 :params :level] 0.9]])
        finished (:db (stream-event acknowledged echo))]
    (is (= 0.9 (get-in next [:show :instances 0 :params :level])))
    (is (= 0.7 (get-in next [:server-projection :view :instances 0 :params :level])))
    (is (= 0.3 (get-in next [:show :master])))
    (is (= 0.9 (get-in finished [:server-projection :view :instances 0 :params :level])))
    (is (empty? (:drafts finished)))
    (is (= ["connected" {:op :ack :token "echo"}] (:studio/stream-reply (stream-event finished echo))))))

(deftest late-full-response-cannot-restore-a-replaced-runtime
  (let [view (assoc (:show base) :runtime-id "new-runtime" :show-id "main")
        db (assoc base :show view :server-projection {:token "new-base" :view view})
        stale-view (assoc view :runtime-id "old-runtime")
        stale {:protocol projection/protocol :kind :full :token "old-result"
               :identity (projection/identity-of stale-view) :view stale-view}
        opened (with-stream db)
        result (transition opened [:studio/stream-frame (:session db) "old-connection" stale])]
    (is (empty? result))
    (is (empty? (transition opened [:studio/stream-closed (:session db) "old-connection" "late close"])))))

(deftest invalid-delta-requests-a-full-view-without-dropping-visible-state
  (let [view (assoc (:show base) :runtime-id "runtime" :show-id "main")
        db (assoc base :show view :server-projection {:token "current" :view view})
        patch {:protocol projection/protocol :kind :patch :base-token "missing" :token "next"
               :identity (projection/identity-of view) :changes []}
        result (stream-event (assoc db :stream-id "connected") patch)]
    (is (= view (get-in result [:db :show])))
    (is (true? (get-in result [:db :awaiting-full?])))
    (is (= ["connected" {:op :resync}] (:studio/stream-reply result)))
    (is (nil? (:studio/http result)))))

(deftest socket-loss-during-knob-debounce-preserves-the-scheduled-command
  (let [opened (with-stream base)
        edit (transition opened [:studio/param-set "7:9" :level 0.9])
        queued (transition (:db edit) (:dispatch edit))
        closed (transition (:db queued) [:studio/stream-closed (:session base) "connected" "lost"])
        flushed (transition (:db closed) [:studio/flush-params])]
    (is (= 0.9 (get-in closed [:db :drafts ["7:9" :level] :fields :value])))
    (is (= 0.9 (get-in (first-command flushed) [:fields :value])))
    (is (= 7 (:epoch (first-command flushed))))))

(deftest saved-preferences-restore-only-supported-portable-fields
  (let [settings {:ui {:view "rig" :page-id "second" :launch-mode "held"
                       :provider-form {:credential "must-not-restore"} :definition {:source "replace"}}}
        restored (events/apply-ui-preferences base settings)
        save (transition restored [:studio/persist-ui])]
    (is (= :rig (get-in restored [:ui :view])))
    (is (= "second" (get-in restored [:ui :page-id])))
    (is (= "held" (get-in restored [:ui :launch-mode])))
    (is (nil? (get-in restored [:ui :provider-form])))
    (is (= events/initial-source (get-in restored [:ui :definition :source])))
    (is (= {:view "rig" :page-id "second" :launch-mode "held"} (get-in save [:studio/http :body :values])))
    (is (= :perform (get-in (events/apply-ui-preferences base {:ui {:view 42}}) [:ui :view])))))

(deftest restored-chat-is-server-owned-and-stale-history-cannot-replace-a-new-draft
  (let [db (assoc base :chat {:history-request "request-a"})
        history {:selected-conversation-id "conversation-a"
                 :conversations [{:id "conversation-a" :title "A quiet scene"
                                  :messages [{:role "system" :content "Private instructions"}
                                             {:role "user" :content "A quiet scene"}
                                             {:role "assistant" :content "Ready"}
                                             {:role "tool" :name "show_read" :tool-call-id "tool-a" :content "ok"}]}]}
        loaded (transition db [:chat/history-loaded "request-a" history])
        cleared (transition (:db loaded) [:chat/clear])
        stale (transition (:db cleared) [:chat/history-loaded "request-a" history])
        sent (transition (assoc-in (:db loaded) [:ui :chat-draft] "Make it red") [:chat/send])]
    (is (= ["A quiet scene" "Ready" ""] (mapv :text (get-in loaded [:db :chat :entries]))))
    (is (= "show_read" (get-in loaded [:db :chat :entries 2 :tools 0 :name])))
    (is (= "conversation-a" (get-in sent [:studio/chat-stream :body :conversation-id])))
    (is (= "Make it red" (get-in sent [:studio/chat-stream :body :message])))
    (is (not (contains? (get-in sent [:studio/chat-stream :body]) :messages)))
    (is (nil? (get-in stale [:db :chat :entries])))
    (is (nil? (get-in sent [:db :chat :history-request])))))

(deftest changing-runtime-does-not-reuse-missing-server-conversations
  (let [db (assoc base :chat {:current-id "run" :conversation-id "old" :running? false
                             :entries [{:id "run" :text "Old server"}]})
        restarted (transition db [:chat/runtime-change])
        empty-history (transition (assoc-in db [:chat :history-request] "load")
                                  [:chat/history-loaded "load" {:conversations []}])
        selecting (transition db [:chat/select "other"])
        cleared (transition (:db selecting) [:chat/clear])
        late (transition (:db cleared) (conj (get-in selecting [:studio/http :on-success]) {}))]
    (is (nil? (get-in restarted [:db :chat :conversation-id])))
    (is (nil? (get-in empty-history [:db :chat :entries])))
    (is (nil? (:dispatch late)))))

(deftest response-envelope-targets-the-current-instance-with-musical-durations
  (let [db (-> base (assoc-in [:ui :selected-instance] "7:9")
               (assoc-in [:ui :response-form :scope] "instance"))
        response (events/response-definition db)
        start (transition db [:studio/response-start])
        release (transition db [:studio/response-action :release "performance-envelope"])]
    (is (= {:instance-ids ["7:9"]} (:select response)))
    (is (= ["level"] (:params response)))
    (is (= {:beats 0.5} (get-in response [:envelope :attack])))
    (is (= {:beats 1} (get-in response [:envelope :release])))
    (is (= "/api/response" (get-in start [:studio/http :path])))
    (is (= "/api/response/release" (get-in release [:studio/http :path])))
    (is (thrown? js/Error (events/response-definition (assoc-in db [:ui :selected-instance] nil))))))

(deftest registry-page-action-uses-existing-reconciliation-api
  (let [request (transition base [:studio/generate-page])
        ack (transition base [:studio/page-generated {:page-id "registry" :placements [1 2] :overflow [:third] :collisions []}])]
    (is (= {:columns 8 :rows 8} (get-in request [:studio/http :body :options])))
    (is (= "registry" (get-in ack [:db :ui :page-id])))
    (is (= :perform (get-in ack [:db :ui :view])))
    (is (str/includes? (get-in ack [:fx 2 1 1]) "1 without room"))))

(deftest fixture-form-preserves-human-labels-and-builds-typed-channel-maps
  (let [source {:id :maker/head :name "Original label" :manufacturer "A company"
                :version 1 :footprint 8 :channels {:pan [1 2] :red 3}
                :provenance {:source "https://example.test/fixture" :license "A license with spaces"}}
        state {:draft-source (pr-str source)
               :field-edits {:id "maker/revised" :name "My moving head" :footprint "10"}
               :channel-edits {:pan {:coarse "5" :fine "8" :inversion "1"}}}
        definition (fixtures/form-definition state)]
    (is (= :maker/revised (:id definition)))
    (is (= "My moving head" (:name definition)))
    (is (= 10 (:footprint definition)))
    (is (= (:provenance source) (:provenance definition)))
    (is (= {:offset 5 :fine-offset 8 :inverted-from 1} (get-in definition [:channels :pan])))
    (is (= 3 (get-in definition [:channels :red])))
    (is (thrown? js/Error (fixtures/form-definition (assoc-in state [:channel-edits :pan :coarse] ""))))
    (is (= "plain-id" (:id (fixtures/form-definition {:draft-source (pr-str (assoc source :id "plain-id"))}))))))

(deftest fixture-publication-and-cache-responses-cannot-cross-show-replacement
  (let [db (-> base (assoc-in [:show :runtime-id] "process-a")
               (assoc-in [:ui :fixture-library] {:draft-source "{:id :my-type}" :published-id "my-type" :request-generation 0}))
        token (fixtures/request-token db)
        changed (transition (assoc-in db [:show :epoch] 8) [:fixtures/runtime-change])
        stale (transition (:db changed) [:fixtures/loaded token {:document-count 999}])]
    (is (fixtures/current-request? db token))
    (is (not (fixtures/current-request? (:db changed) token)))
    (is (nil? (get-in changed [:db :ui :fixture-library :published-id])))
    (is (= "{:id :my-type}" (get-in changed [:db :ui :fixture-library :draft-source])))
    (is (nil? (get-in stale [:db :ui :fixture-library :metadata])))))

(deftest provider-callbacks-cannot-restore-an-old-show-connection
  (let [db (assoc base :chat {:provider-request "old" :provider {:ready? true :model "old-model"}})
        changed (transition db [:chat/runtime-change])
        late (transition (:db changed) [:chat/providers-loaded "old" {:ready? true :model "old-model"}])
        error (transition (:db late) [:chat/provider-error "old" "Old server failure"])]
    (is (nil? (get-in late [:db :chat :provider])))
    (is (nil? (get-in error [:db :chat :error])))))

(deftest composition-slot-overrides-compose-with-cue-and-effect-defaults
  (let [db (-> base (assoc-in [:show :effects 0 :params]
                             {:level {:type "number" :default 0.5} :aim {:type "vector" :default [0 0 0]}})
               (assoc-in [:show :cues 0 :params] {:level 0.7})
               (assoc-in [:ui :composition :slots] [{:cue-id "ember" :params {} :duration {:beats 1}}]))
        level (transition db [:studio/slot-param 0 :level 0.4])
        x (transition (:db level) [:studio/slot-param-component 0 :aim 0 0.25])
        y (transition (:db x) [:studio/slot-param-component 0 :aim 1 0.75])
        reset (transition (:db y) [:studio/slot-params-reset 0])
        saved (transition (:db y) [:studio/composition-save false])]
    (is (= {:level 0.7 :aim [0 0 0]} (events/slot-parameters db 0)))
    (is (= {:level 0.4 :aim [0.25 0.75 0]} (events/slot-parameters (:db y) 0)))
    (is (= 0.4 (get-in saved [:studio/http :body :slots 0 :params :level])))
    (is (= 0.7 (:level (events/slot-parameters (:db reset) 0))))
    (is (= 0.7 (get-in y [:db :show :instances 0 :params :level])))))

(deftest saved-cue-defaults-apply-before-slot-overrides-and-component-edits
  (let [db (-> base
               (assoc-in [:show :effects 0 :params] {:level {:default 0.2} :aim {:default [0 0 0]}})
               (assoc-in [:show :cues 0 :params] {:level 0.4})
               (assoc-in [:show :cues 0 :saved-params] {:level 0.8 :aim [0.1 0.2 0.3]})
               (assoc-in [:ui :composition :slots] [{:cue-id "ember" :params {:level 0.6}}]))
        edited (transition db [:studio/slot-param-component 0 :aim 1 0.9])
        reset (transition (:db edited) [:studio/slot-params-reset 0])]
    (is (= {:level 0.6 :aim [0.1 0.2 0.3]} (events/slot-parameters db 0)))
    (is (= [0.1 0.9 0.3] (:aim (events/slot-parameters (:db edited) 0))))
    (is (= {:level 0.8 :aim [0.1 0.2 0.3]} (events/slot-parameters (:db reset) 0)))
    (is (= {:level 0.8 :aim [0.1 0.2 0.3]} (get-in edited [:db :show :cues 0 :saved-params])))))

(deftest graphical-rig-edits-preserve-height-and-own-their-pointer
  (let [db (assoc-in base [:ui :rig-draft]
                     {:fixtures [{:id "head" :position [1 2 3]}]
                      :trusses [{:id "rail" :length 4 :position [0 3 0]}]})
        yaw (transition db [:studio/truss-set 0 [:rotation-deg 1] 45])
        down (transition (:db yaw) [:studio/rig-drag-start :trusses 0 11 0.5 0])
        wrong (transition (:db down) [:studio/rig-drag-move 12 100 100])
        moved (transition (:db wrong) [:studio/rig-drag-move 11 1.8 2.2])
        up (transition (:db moved) [:studio/rig-drag-end 11])
        nudged (transition (:db up) [:studio/rig-nudge :fixtures 0 0.1 -0.1])]
    (is (= [0 45 0] (get-in yaw [:db :ui :rig-draft :trusses 0 :rotation-deg])))
    (is (= [0 3 0] (get-in wrong [:db :ui :rig-draft :trusses 0 :position])))
    (is (= [1.3 3 2.2] (get-in moved [:db :ui :rig-draft :trusses 0 :position])))
    (is (nil? (get-in up [:db :ui :rig-drag])))
    (is (= [1.1 2 2.9] (get-in nudged [:db :ui :rig-draft :fixtures 0 :position])))
    (is (= [-3.75 2 3] (get-in (transition (assoc-in db [:ui :rig-draft :fixtures 0 :position] [-3.85 2 3])
                                          [:studio/rig-nudge :fixtures 0 0.1 0]) [:db :ui :rig-draft :fixtures 0 :position])))
    (is (true? (get-in nudged [:db :ui :rig-dirty?])))))

(deftest rig-save-acknowledgement-keeps-newer-edits-unsaved
  (let [rig {:fixtures [] :trusses [{:id "rail" :position [0 3 0] :length 4}]}
        db (assoc-in base [:ui :rig-draft] rig)
        save (transition db [:studio/rig-save])
        newer (transition db [:studio/truss-set 0 [:length] 5])
        ack (conj (get-in save [:studio/http :on-success]) {:status "accepted"})
        received (transition (:db newer) ack)
        replacement (transition (assoc-in (:db newer) [:show :runtime-id] "replacement") ack)]
    (is (true? (get-in received [:db :ui :rig-dirty?])))
    (is (= 5 (get-in received [:db :ui :rig-draft :trusses 0 :length])))
    (is (nil? (:db replacement)))))

(deftest opening-connections-subscribes-and-enumerates-devices-on-the-first-status
  (let [opened (transition base [:connections/open])
        pushed (transition (assoc-in (:db opened) [:show :connection-status]
                                     {:sync {:config connections/sync-defaults} :audio {:state "idle"}})
                           [:connections/pushed])
        refreshed (transition (:db pushed) [:connections/refresh-devices])
        requests (map second (:fx refreshed))]
    (is (= #{:db :studio/connections-interest} (set (keys opened))))
    (is (:studio/connections-interest opened))
    (is (= [:connections/refresh-devices] (:dispatch pushed)))
    (is (= #{"/api/sync/ports" "/api/audio/inputs"} (set (map :path requests))))
    (is (every? #(nil? (:body %)) requests))
    (is (= #{:db} (set (keys (transition (:db pushed) [:connections/pushed])))))))

(deftest midi-selection-keeps-name-guards-until-explicit-reselection
  (let [form (-> connections/sync-defaults (assoc-in [:midi :enabled?] true)
                 (assoc-in [:midi :input-index] "2") (assoc-in [:midi :expected-input-name] "Original"))
        db (assoc base :connections {:sync-form form :ports [{:index 2 :name "Replacement" :input? true}]})
        selected (transition db [:connections/select-port :input "2"])
        payload (connections/sync-payload (:db selected))]
    (is (thrown? js/Error (connections/sync-payload db)))
    (is (= 2 (get-in payload [:midi :input-index])))
    (is (= "Replacement" (get-in payload [:midi :expected-input-name])))
    (is (nil? (get-in payload [:midi :output-index])))))

(deftest connection-mappings-preserve-musical-values-and-namespaced-identities
  (let [db (assoc base :connections
                  {:sync-form (assoc-in connections/sync-defaults [:live :pairing]
                                        {:track-index "2" :clip-slot-index "3" :rack-device-index "1" :expected-track-name "Lighting rack"})
                   :notes [{:control "36" :cue-id "colors/ember" :mode "one-shot" :duration-value "0.25" :duration-unit "bars"}]
                   :parameters [{:control "12" :cue-id "colors/ember" :param "rig/aim" :low "-1" :high "1"}]})
        payload (connections/sync-payload db)]
    (is (= {:cue-id "colors/ember" :mode "one-shot" :duration {:bars 0.25}} (get-in payload [:live :mappings "36"])))
    (is (= {:cue-id "colors/ember" :param "rig/aim" :bounds [-1 1]} (get-in payload [:live :parameters "12"])))
    (is (= {:track-index 2 :clip-slot-index 3 :rack-device-index 1 :expected-track-name "Lighting rack"} (get-in payload [:live :pairing])))
    (is (thrown? js/Error (connections/sync-payload (update-in db [:connections :notes] #(conj % (first %))))))))

(deftest audio-input-is-explicit-and-protected-against-enumeration-changes
  (let [none (assoc base :connections {:audio-form connections/audio-defaults})
        default (assoc-in none [:connections :audio-form :input] "system")
        stale (-> none (assoc-in [:connections :audio-form :input] "1")
                  (assoc-in [:connections :audio-form :expected-name] "Original")
                  (assoc-in [:connections :inputs] [{:mixer-index 1 :name "Replacement"}]))
        selected (transition stale [:connections/select-audio-input "1"])]
    (is (thrown? js/Error (connections/audio-payload none)))
    (is (true? (:default? (connections/audio-payload default))))
    (is (= 48000 (:sample-rate (connections/audio-payload default))))
    (is (thrown? js/Error (connections/audio-payload stale)))
    (is (= "Replacement" (:mixer-name (connections/audio-payload (:db selected)))))
    (is (= "/api/audio/start" (get-in (transition default [:connections/audio-start]) [:fx 0 1 :path])))))

(deftest closed-connections-release-interest-and-drafts-survive-status-updates
  (let [db (assoc base :connections (assoc connections/defaults :visible? true))
        old-token (connections/token db)
        closed (transition db [:connections/close])
        late (transition (:db closed) [:connections/loaded old-token :ports []])
        edited (transition db [:connections/edit [:sync-form :live :endpoint] "http://localhost:9000/live"])
        data {:sync {:config connections/sync-defaults :clock {:status "manual"}}}
        pushed (transition (assoc-in (:db edited) [:show :connection-status] data) [:connections/pushed])
        saved (transition (:db pushed) [:connections/configured old-token :configure {:config connections/sync-defaults}])]
    (is (false? (:studio/connections-interest closed)))
    (is (= (:sync data) (get-in saved [:db :connections :sync])) "HTTP acknowledgements preserve authoritative status")
    (is (nil? (:db late)))
    (is (= "http://localhost:9000/live" (get-in pushed [:db :connections :sync-form :live :endpoint])))
    (is (false? (get-in closed [:db :connections :visible?])))))

(deftest connection-actions-carry-current-runtime-identity
  (let [db (assoc-in base [:show :runtime-id] "current")
        source (transition db [:connections/action :source "beat-link"])
        configure (transition db [:connections/configure false])]
    (is (= "current" (get-in source [:fx 0 1 :body :runtime-id])))
    (is (= 7 (get-in source [:fx 0 1 :body :expected-epoch])))
    (is (= "beat-link" (get-in source [:db :connections :sync-form :selected-source])))
    (is (= "/api/sync/configure" (get-in configure [:fx 0 1 :path])))
    (is (nil? (:dispatch configure)))))

(deftest preview-settings-stay-local-bounded-and-replay-after-engine-mount
  (let [changed (transition base [:studio/preview-setting :worklight 0.4])
        invalid (transition base [:studio/preview-setting :worklight 2])
        mounted (transition (:db changed) [:studio/preview-ready true])]
    (is (= 0.4 (get-in changed [:db :ui :preview-settings :worklight])))
    (is (= [:worklight 0.4] (:studio/preview changed)))
    (is (nil? (:studio/http changed)))
    (is (nil? (:db invalid)))
    (is (some #{[:studio/preview [:worklight 0.4]]} (:fx mounted)))
    (is (true? (get-in mounted [:db :ui :preview-controls?])))))

(deftest preview-fixture-updates-are-local-and-replay-on-remount
  (let [change (transition base [:studio/preview-setting :lighting-mode "interpolate"])
        mount (transition (:db change) [:studio/preview-ready true])]
    (is (= [:lighting-mode "interpolate"] (:studio/preview change)))
    (is (nil? (:studio/http change)))
    (is (some #{[:studio/preview [:lighting-mode "interpolate"]]} (:fx mount)))
    (is (nil? (:db (transition base [:studio/preview-setting :lighting-mode "extrapolate"]))))))

(deftest reset-camera-updates-the-selector-and-the-engine-together
  (let [result (transition (assoc-in base [:ui :preview-settings :camera] "overview") [:studio/preview-reset])]
    (is (= "audience" (get-in result [:db :ui :preview-settings :camera])))
    (is (= [:reset nil] (:studio/preview result)))))

(deftest inspect-reveals-the-requested-live-card-without-changing-launch-mode
  (let [db (-> base
               (assoc-in [:show :cues] [{:id "ember" :effect-id "ember"}])
               (assoc-in [:show :instances] [{:id "7:12" :cue-id "ember"}])
               (assoc-in [:ui :action-cell] {:cue-id "ember"})
               (assoc-in [:ui :launch-mode] "held"))
        result (transition db [:studio/cue-action :inspect])]
    (is (= "live" (get-in result [:db :ui :performance-section])))
    (is (true? (get-in result [:db :ui :performance :expanded "7:12"])))
    (is (= "held" (get-in result [:db :ui :launch-mode])))
    (is (= "7:12" (:studio/focus-instance result)))))

(deftest live-connection-preserves-arrangement-and-disabled-parameter-lanes
  (let [form (-> connections/sync-defaults
                 (assoc-in [:live :automap?] false)
                 (assoc-in [:live :pairing] {:track-index "4" :arrangement-index "0" :rack-device-index ""}))
        db (assoc base :connections {:sync-form form
                                     :parameters [{:control "12" :cue-id "auto/red" :param "shape" :type "enum"
                                                   :values ["sine" "saw"] :low "0" :high "1" :disabled? true :lane-note "102"}]})
        live (:live (connections/sync-payload db))
        arrange (transition db [:connections/action :arrange "48.5"])]
    (is (= {:track-index 4 :arrangement-index 0} (:pairing live)))
    (is (false? (:automap? live)))
    (is (= {:cue-id "auto/red" :param "shape" :bounds [0 1] :lane-note 102
            :values ["sine" "saw"] :type "enum" :disabled? true} (get-in live [:parameters "12"])))
    (is (= "/api/sync/arrange" (get-in arrange [:fx 0 1 :path])))
    (is (= 48.5 (get-in arrange [:fx 0 1 :body :song-beat])))
    (is (string? (get-in arrange [:fx 0 1 :body :request-id])))))

(deftest learned-live-automation-sources-survive-connection-form-edits
  (let [descriptor {:path "live_set tracks 1 mixer_device volume" :name "Volume" :track-name "Bass"
                    :device-name "Mixer" :range [0 1] :quantized? false}
        db (assoc base :connections {:sync-form connections/sync-defaults
                                     :sync {:config {:live {:observed-parameters {"bass" descriptor}}}}})
        learn (transition db [:connections/action :learn-parameter "bass"])
        forget (transition db [:connections/action :forget-parameter "bass"])]
    (is (= {"bass" descriptor} (get-in (connections/sync-payload db) [:live :observed-parameters])))
    (is (= "/api/sync/learn-parameter" (get-in learn [:fx 0 1 :path])))
    (is (= "bass" (get-in learn [:fx 0 1 :body :id])))
    (is (= "/api/sync/forget-parameter" (get-in forget [:fx 0 1 :path])))))

(deftest fixture-optical-fields-support-fractions-removal-and-explicit-reflection-choice
  (let [s {:draft-source "{:id :spot :beam-angle 10 :pseudo-fixture true :max-watts 120}"
           :field-edits {:max-watts "" :beam-angle "14.25" :pseudo-fixture "false"
                         :measured-relative-brightness "2.2"}}
        definition (fixtures/form-definition s)]
    (is (= {:id :spot :beam-angle 14.25 :pseudo-fixture false :measured-relative-brightness 2.2} definition))
    (is (not (contains? (fixtures/form-definition (assoc-in s [:field-edits :pseudo-fixture] "")) :pseudo-fixture)))
    (is (zero? (:lens-radius-m (fixtures/form-definition (assoc-in s [:field-edits :lens-radius-m] "0")))))
    (is (thrown? js/Error (fixtures/form-definition (assoc-in s [:field-edits :beam-angle] "180"))))))

(deftest fog-profile-controls-publish-preserve-and-remove-through-re-frame
  (let [source {:id :fog-type :version 1 :channels {:fog 1} :fogger {:flow-m3-min 12 :extra "vendor"}}
        db (assoc-in base [:ui :fixture-library] {:definition source :draft-source (pr-str source)})
        edit (fn [db entry] (:db (transition db (into [:fixtures/edit-fogger] entry))))
        edited (reduce edit db [[:flow-m3-min "60.5"] [:mixing-seconds "12"]
                                [[:dmx-range 0] "255"] [[:dmx-range 1] "0"]
                                [[:direction 0] "-1"] [[:direction 2] "0"]])
        publish #(some-> (transition % [:fixtures/publish]) :studio/http :body :source reader/read-string)
        saved (publish edited)
        removed (:db (transition edited [:fixtures/remove-channel :fog]))]
    (is (= {:flow-m3-min 60.5 :mixing-seconds 12 :dmx-range [255 0] :direction [-1 0 0] :extra "vendor"}
           (:fogger saved)))
    (is (= (:channels source) (:channels saved)))
    (is (nil? (:mixing-seconds (:fogger (publish (edit edited [:mixing-seconds ""]))))))
    (is (nil? (:fogger (publish (edit edited [:enabled? false])))))
    (is (nil? (get-in removed [:ui :fixture-library :definition :fogger])))
    (is (nil? (get-in removed [:ui :fixture-library :definition :channels :fog])))
    (doseq [invalid [[[:direction 0] "0"] [[:dmx-range 1] "255"] [:flow-m3-min "0"] [:mixing-seconds "-2"]]]
      (is (= :fixtures/error (first (:dispatch (transition (edit edited invalid) [:fixtures/publish]))))))))

(deftest room-atmosphere-controls-save-the-whole-rig-via-re-frame
  (let [draft {:fixtures [{:id :head}] :trusses [] :room {:width 20 :height 8 :depth 12 :name "venue"}}
        db (assoc-in base [:ui :rig-draft] draft)
        change (fn [db k v] (:db (transition db [:studio/rig-atmosphere k v])))
        edited (-> db (change :air-changes-per-hour 1.5) (change :fog-half-life-s 150))
        saved #(dissoc (get-in (transition % [:studio/rig-save]) [:studio/http :body]) :expected-epoch)]
    (is (= (update draft :room assoc :air-changes-per-hour 1.5 :fog-half-life-s 150) (saved edited)))
    (is (= draft (saved (-> edited (change :air-changes-per-hour nil) (change :fog-half-life-s nil)))))
    (is (true? (get-in edited [:ui :rig-dirty?])))
    (doseq [[field invalid] [[:air-changes-per-hour 3601] [:fog-half-life-s 0]]]
      (let [result (transition (change edited field invalid) [:studio/rig-save])]
        (is (nil? (:studio/http result)))
        (is (= :studio/toast (first (:dispatch result))))))
    (is (= {:width 12 :height 5 :depth 10 :fog-half-life-s 60}
           (:room (saved (change (update-in db [:ui :rig-draft] dissoc :room) :fog-half-life-s 60)))))))

(deftest removing-a-room-measurement-reverts-the-rig-draft-to-manual-clearance
  (let [measurement {:clearance-half-life-s 85 :extinction-per-m 1.4 :reference-path-m 5
                     :r-squared 0.97 :method :metered-burst}
        rig {:fixtures [] :room {:width 10 :height 6 :depth 14 :air-changes-per-hour 2
                                 :fog-half-life-s 120 :measured-fog measurement}}
        db (-> base (assoc-in [:server-projection :view :rig] rig)
               (assoc-in [:ui :rig-draft] (projection/presentation rig)))
        submit #(get-in (transition % [:studio/rig-save]) [:studio/http :body :room])
        removed (:db (transition db [:studio/rig-atmosphere :measured-fog nil]))]
    (is (= (:room rig) (submit db)) "Layout saves retain the typed fit method")
    (is (true? (get-in removed [:ui :rig-dirty?])))
    (is (= rig (get-in removed [:server-projection :view :rig])) "The live calibration lasts until Save rig")
    (is (= (dissoc (:room rig) :measured-fog) (submit removed)))
    (is (= 3 (:air-changes-per-hour (submit (:db (transition removed [:studio/rig-atmosphere :air-changes-per-hour 3]))))))))

(deftest a-new-phone-fit-reaches-dirty-rig-drafts-without-discarding-placement
  (let [fit {:clearance-half-life-s 85 :method :clearance-tail}
        view (assoc (:show base) :show-id :stage :runtime-id "rig-test"
                    :rig {:fixtures [] :room {:width 12 :height 5 :depth 10 :measured-fog fit}})
        start (:db (full-frame base view))
        draft (:db (transition start [:studio/rig-set [:room :width] 15]))
        cleared (:db (transition draft [:studio/rig-atmosphere :measured-fog nil]))
        patch (fn [token next changes] {:protocol projection/protocol :kind :patch :base-token token :token next
                                        :identity (projection/identity-of view) :changes changes})
        idle (:db (stream-event cleared (patch "full" "idle" [[:set [:frame-number] 13]])))
        newer (assoc fit :clearance-half-life-s 90)
        updated (:db (stream-event idle (patch "idle" "refit" [[:set [:frame-number] 14]
                                                                [:set [:rig :room :measured-fog] newer]])))
        saved (get-in (transition updated [:studio/rig-save]) [:studio/http :body :room])]
    (is (nil? (get-in idle [:ui :rig-draft :room :measured-fog])))
    (is (= newer (:measured-fog saved)) "Saving placement retains a concurrent phone calibration")
    (is (= 15 (:width saved)))))
