(ns tolglow.studio.events-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [cljs.reader :as reader]
            [tolglow.studio.audio-returns :as returns]
            [clojure.string :as str]
            [re-frame.db :as rf-db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [reagent.core :as r]
            ["react-dom/server" :refer [renderToStaticMarkup]]
            [tolglow.studio.events :as events]
            [tolglow.studio.projection :as projection]
            [tolglow.studio.spatial :as spatial]
            [tolglow.studio.spatial-drafts :as spatial-drafts]
            [tolglow.studio.fixture-library :as fixtures]
            [tolglow.studio.connections :as connections]
            [tolglow.studio.views :as views]
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

(deftest section-pattern-override-follows-committed-show-settings
  (let [view (assoc (:show base) :runtime-id "pattern-owner" :show-id :main
                                :section-patterns-enabled? true :settings {})
        db (:db (full-frame base view))
        suspend (transition db [:studio/section-pattern-override true])
        request (:studio/http suspend)
        ack (transition (:db suspend) (conj (:on-success request) {:status "accepted" :revision 4}))
        committed (:db (stream-event (:db ack)
                         {:protocol projection/protocol :kind :patch :token "suspended" :base-token "full"
                          :identity (projection/identity-of view)
                          :changes [[:set [:revision] 4]
                                    [:set [:settings :section-patterns] {:override? true}]]}))
        resumed (:db (stream-event committed
                       {:protocol projection/protocol :kind :patch :token "resumed" :base-token "suspended"
                        :identity (projection/identity-of view)
                        :changes [[:set [:revision] 5] [:set [:settings :section-patterns :override?] false]]}))]
    (is (= "/api/settings" (:path request)))
    (is (true? (:typed? request)))
    (is (= {:scope :section-patterns :values {:override? true} :expected-epoch 7 :runtime-id "pattern-owner"}
           (:body request)))
    (is (= {:override? false :pending? true :connected? true} (events/section-pattern-control (:db ack))))
    (is (empty? (transition (:db ack) [:studio/section-pattern-override true])))
    (is (= {:override? true :pending? false :connected? true} (events/section-pattern-control committed)))
    (is (= {:scope :section-patterns :values {:override? false} :expected-epoch 7 :runtime-id "pattern-owner"}
           (get-in (transition committed [:studio/section-pattern-override false]) [:studio/http :body])))
    (is (= {:override? false :pending? false :connected? true} (events/section-pattern-control resumed)))
    (is (empty? (transition (assoc-in db [:connection :connected?] false) [:studio/section-pattern-override true])))
    (is (nil? (events/section-pattern-control (:db (full-frame base (dissoc view :section-patterns-enabled?))))))))

(deftest rejected-section-pattern-override-retains-committed-state
  (let [view (assoc (:show base) :runtime-id "pattern-owner" :show-id :main
                                :section-patterns-enabled? true :settings {:section-patterns {:override? true}})
        db (:db (full-frame base view))
        resume (transition db [:studio/section-pattern-override false])
        reply (conj (get-in resume [:studio/http :on-failure]) "Stale show epoch")
        rejection (transition (:db resume) reply)
        restored (:db (full-frame (:db resume) (assoc view :epoch 8)))]
    (is (= [:studio/failure "Section pattern override: Stale show epoch"] (:dispatch rejection)))
    (is (= {:override? true :pending? false :connected? true} (events/section-pattern-control (:db rejection))))
    (is (false? (:pending? (events/section-pattern-control restored))))
    (is (empty? (transition restored reply)))))

(deftest rapid-rotary-keypresses-use-current-app-db-instead-of-a-stale-rendered-value
  (let [schema {:type "number" :bounds [0 1]}
        initial (assoc-in base [:show :instances 0 :params :level] 0)
        updated (reduce (fn [state _] (:db (transition state [:studio/param-key "7:9" :level schema "PageUp"]))) initial (range 7))]
    (is (< (js/Math.abs (- 0.7 (get-in updated [:show :instances 0 :params :level]))) 0.000001))
    (is (< (js/Math.abs (- 0.7 (get-in updated [:drafts ["7:9" :level] :fields :value]))) 0.000001))))

(deftest reused-pointer-and-late-held-acknowledgement-stay-independent
  (let [gesture [:studio/gesture-down {:pointer-id 11 :cue-id "ember" :x 0 :y 0}]
        db (-> base (assoc-in [:ui :launch-mode] "held") (assoc-in [:show :runtime-id] "first-show"))
        first-down (transition db gesture)
        first-launch (first-command first-down)
        released (:db (transition (:db first-down) [:studio/gesture-up 11]))
        new-show (update released :show assoc :epoch 8 :runtime-id "second-show")
        second-down (transition new-show gesture)
        second-ack (conj (:on-success (first-command second-down)) {:instance-id "8:1"})
        holding (:db (transition (:db second-down) second-ack))
        late (transition holding (conj (:on-success first-launch) {:instance-id "7:10"}))
        late-failure (transition holding (conj (:on-failure first-launch) "Timed out"))
        final-release (first-command (transition holding [:studio/gesture-up 11]))]
    (is (= "held" (get-in first-launch [:fields :launch :mode])))
    (is (empty? (:pointers released)))
    (is (= "cue/launch" (:op (first-command second-down))))
    (is (= "8:1" (get-in holding [:pointers 11 :instance-id])))
    (is (nil? (:db late)))
    (is (= (:pointers holding) (get-in late-failure [:db :pointers])))
    (is (true? (get-in late-failure [:db :recover-held?])))
    (is (= {:op "instance/end" :epoch 7 :runtime-id "first-show" :fields {:instance-id "7:10"}}
           (select-keys (update (first-command late) :fields dissoc :command-id) [:op :epoch :runtime-id :fields])))
    (is (= {:epoch 8 :runtime-id "second-show" :fields {:instance-id "8:1"}}
           (select-keys (update final-release :fields dissoc :command-id) [:epoch :runtime-id :fields])))))

(deftest uncertain-held-replies-reconcile-only-the-originating-session
  (let [hold (fn [id gesture session]
               {:id id :gesture-id gesture :launch-mode "held" :phase "running"
                :source {:adapter "web" :session-id session}})
        mine (:session base)
        db (-> base (assoc-in [:show :instances] [(hold "7:1" "released" mine) (hold "7:2" "pressed" mine)
                                                  (hold "7:3" "other" "second-ipad")
                                                  (assoc (hold "7:4" "done" mine) :phase "releasing")])
               (assoc-in [:pointers 2] {:held? true :press-id "pressed" :epoch 7}))
        recovered (transition db [:studio/reconcile-held])
        end-request (first-command recovered)
        waiting (transition (:db recovered) [:studio/reconcile-held])
        failed (transition (:db recovered) (conj (:on-failure end-request) "No reply"))
        retry (transition (:db failed) (get-in failed [:dispatch-later 0 :dispatch]))
        repeated (transition (:db retry) (:dispatch retry))]
    (is (= "7:2" (get-in recovered [:db :pointers 2 :instance-id])))
    (is (= #{"released"} (get-in recovered [:db :ending-holds])))
    (is (= [{:instance-id "7:1" :command-id "released-end"}] (map (comp :fields second) (:fx recovered))))
    (is (empty? (:fx waiting)))
    (is (= (get-in end-request [:fields :command-id]) (get-in (first-command repeated) [:fields :command-id])))
    (is (empty? (transition (assoc-in db [:connection :connected?] false) [:studio/reconcile-held])))))

(deftest restored-gesture-with-the-same-id-belongs-to-the-replacement-show
  (let [owner {:press-id "saved" :epoch 7 :runtime-id "host" :held? true}
        db (-> base (assoc-in [:show :runtime-id] "host")
               (assoc :pointers {3 owner} :ending-holds #{"saved"}))
        restored (assoc (:show db) :epoch 8 :show-id :main :instances
                        [{:id "8:5" :gesture-id "saved" :launch-mode :held :phase :running
                          :source {:adapter :web :session-id (:session db)}}])
        installed (full-frame db restored)
        recovered (transition (:db installed) [:studio/reconcile-held])
        end (first-command recovered)]
    (is (some #{[:dispatch [:studio/reconcile-held]]} (:fx installed)))
    (is (nil? (get-in installed [:db :ending-holds])))
    (is (nil? (get-in recovered [:db :pointers 3 :instance-id])))
    (is (= [8 "8:5" "saved-end"] [(:epoch end) (get-in end [:fields :instance-id]) (get-in end [:fields :command-id])]))
    (is (empty? (transition (:db recovered) [:studio/retry-held-end owner])))))

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

(deftest launch-grid-captures-the-press-selection-and-allows-a-single-override
  (let [pad [:studio/gesture-down {:pointer-id 1 :cue-id "ember"}]
        one-shot (assoc-in base [:ui :launch-mode] "one-shot")
        selected (assoc-in one-shot [:ui :launch-grid] "quarter-beat")
        down (:db (transition selected pad))
        changed (assoc-in down [:ui :launch-grid] "bar")
        released (transition changed [:studio/gesture-up 1])
        launched (first-command (transition (:db released) (:dispatch released)))
        legacy (first-command (transition one-shot [:studio/launch "ember" "one-shot"]))
        override (:dispatch (transition (assoc-in selected [:ui :action-cell :cue-id] "ember")
                                        [:studio/cue-action :launch-now]))
        immediate (first-command (transition selected override))
        active-toggle (first-command (transition selected [:studio/launch "ember" "toggle"]))]
    (is (= "cue/launch" (:op launched)))
    (is (string? (get-in launched [:fields :command-id])))
    (is (= {:beats 0.25} (get-in launched [:fields :grid])))
    (is (= {:mode "one-shot" :duration {:beats 4}} (get-in launched [:fields :launch])))
    (is (not (contains? (:fields legacy) :grid)))
    (is (= "off" (get-in immediate [:fields :grid])))
    (is (= "instance/end" (:op active-toggle)))
    (is (not (contains? (:fields active-toggle) :grid)))
    (is (empty? (transition (assoc-in (:db released) [:show :epoch] 8) (:dispatch released))))
    (is (empty? (transition selected (conj (:on-success launched) {:status "pending"}))))))

(deftest gridded-held-release-before-ack-uses-the-owning-launch-id
  (let [db (-> base (assoc-in [:ui :launch-mode] "held") (assoc-in [:ui :launch-grid] "bar")
               (assoc-in [:show :runtime-id] "host"))
        gesture [:studio/gesture-down {:pointer-id 4 :cue-id "ember"}]
        down (transition db gesture)
        press (first-command down)
        launch-id (get-in press [:fields :command-id])
        release (transition (:db down) [:studio/release-pointers true])
        cancel (first-command release)
        next-press (transition (:db release) gesture)
        old-reply (transition (:db next-press) (conj (:on-success press) {:status "fired" :instance-id "7:10"}))]
    (is (= {:bars 1} (get-in press [:fields :grid])))
    (is (= launch-id (get-in press [:fields :gesture-id])))
    (is (empty? (transition (:db down) (conj (:on-success press) {:status "pending"}))))
    (is (= "cue/cancel-launch" (:op cancel)))
    (is (= {:launch-command-id launch-id :reason "held-release" :command-id (str launch-id "-held-release")} (:fields cancel)))
    (is (= [7 "host" (:session db) true] ((juxt :epoch :runtime-id :session :beacon?) cancel)))
    (is (nil? (get-in release [:db :pointers 4])))
    (is (empty? old-reply))
    (is (not= launch-id (get-in (first-command next-press) [:fields :command-id])))
    (is (empty? (:fx (transition (:db release) [:studio/gesture-cancel 4]))))))

(deftest queued-projection-drives-feedback-and-session-owned-cancellation
  (let [mine {:command-id "own" :cue-id "ember" :due-beat 4.5 :mode :held
              :source {:adapter :web :session-id (:session base)}}
        other (assoc-in (assoc mine :command-id "other") [:source :session-id] "other-browser")
        view (assoc (:show base) :show-id :main :runtime-id "host" :pending-cue-launches [mine other])
        installed (:db (full-frame base view))
        html (renderToStaticMarkup (r/as-element [views/queued-cues installed]))
        cancel (first-command (transition installed [:studio/cancel-queued-cue "own"]))
        recovered (first-command (transition installed [:studio/reconcile-held]))
        unqueued (assoc-in installed [:show :pending-cue-launches] [])]
    (is (str/includes? html "Queued launches"))
    (is (str/includes? html "ember · beat 4.5"))
    (is (= 1 (count (re-seq #"Cancel queued ember" html))))
    (is (= "cue/cancel-launch" (:op cancel)))
    (is (= {:launch-command-id "own" :reason "operator-cancel" :command-id "own-operator-cancel"} (:fields cancel)))
    (is (= [:studio/toast "Queued cue cancelled."]
           (:dispatch (transition installed (conj (:on-success cancel) {:status "cancelled"})))))
    (is (= [:studio/toast "The cue already launched."]
           (:dispatch (transition installed (conj (:on-success cancel) {:status :fired})))))
    (is (= {:launch-command-id "own" :reason "held-release" :command-id "own-held-release"} (:fields recovered)))
    (is (empty? (transition installed [:studio/cancel-queued-cue "other"])))
    (is (empty? (transition unqueued [:studio/cancel-queued-cue "own"])))
    (is (nil? (views/queued-cues unqueued)))))

(defn- launch-failures [effects]
  (keep (fn [[kind [event message]]]
          (when (and (= :dispatch kind) (= :studio/failure event)) message)) (:fx effects)))

(deftest committed-cue-misses-notify-the-owning-session-once-through-reconnect
  (let [session (:session base)
        source {:adapter :web :session-id session}
        fence {:status :missed :command-id "clock" :cue-id :violet-haze :reason :clock-changed :source source}
        deleted {:status :missed :command-id "deleted" :cue-id :ember :code :unknown-cue :source source}
        outcomes [fence deleted
                  (assoc-in fence [:source :session-id] "different-ipad")
                  (assoc-in (assoc deleted :command-id "midi") [:source :adapter] :push-3)
                  (assoc fence :status :fired :command-id "fired" :instance-id "7:unrelated")
                  (assoc fence :status :cancelled :command-id "cancelled")]
        view (-> (:show base)
                 (assoc :runtime-id "host" :show-id :main :cue-launch-outcomes [])
                 (assoc-in [:cues 0 :name] "Ember wash"))
        installed (:db (full-frame base view))
        patch {:protocol projection/protocol :kind :patch :base-token "full" :token "misses"
               :identity (projection/identity-of view)
               :changes [[:set [:frame-number] 13] [:set [:cues] []] [:set [:cue-launch-outcomes] outcomes]]}
        reported (stream-event installed patch)
        duplicate (stream-event (:db reported) patch)
        closed (:db (transition (:db duplicate) [:studio/stream-closed session "connected" "interrupted"]))
        replacement (assoc view :frame-number 14 :cues [] :cue-launch-outcomes outcomes)
        reconnected (full-frame closed replacement)
        next-outcome (assoc fence :command-id "another" :cue-id nil :source (assoc source :adapter "web"))
        next-snapshot (stream-event (:db reconnected)
                                   (assoc patch :token "next" :changes [[:set [:frame-number] 15]
                                                                       [:set [:cue-launch-outcomes] [deleted next-outcome]]]))]
    (is (= ["violet haze missed its scheduled launch because the show clock changed. Ember wash missed its scheduled launch because the cue was removed."]
           (launch-failures reported)))
    (is (= "7:9" (get-in reported [:db :ui :selected-instance])))
    (is (= ["connected" {:op :ack :token "misses"}] (:studio/stream-reply reported)))
    (is (nil? (:studio/http reported)))
    (is (empty? (launch-failures duplicate)))
    (is (empty? (launch-failures reconnected)))
    (is (= ["connected" {:op :ack :token "next"}] (:studio/stream-reply next-snapshot)))
    (is (= ["Cue missed its scheduled launch because the show clock changed."] (launch-failures next-snapshot)))))

(deftest missed-http-replies-defer-to-the-committed-stream
  (let [db (-> base (assoc-in [:show :runtime-id] "host") (assoc-in [:ui :selected-instance] "7:9"))
        launch (first-command (transition db [:studio/launch "ember" "one-shot" {:grid "off"}]))
        held (transition (-> db (assoc-in [:ui :launch-mode] "held") (assoc-in [:ui :launch-grid] "bar"))
                         [:studio/gesture-down {:pointer-id 7 :cue-id "ember"}])
        reply {:status "missed" :code "unknown-cue"}]
    (is (empty? (transition db (conj (:on-success launch) reply))))
    (is (empty? (transition (:db held) (conj (:on-success (first-command held)) reply))))))

(deftest only-the-current-press-can-open-its-cue-actions
  (let [pad {:pointer-id 1 :cue-id "ember" :x 0 :y 0 :client-x 5 :client-y 5}
        down (transition base [:studio/gesture-down pad])
        released (:db (transition (:db down) [:studio/gesture-up 1]))
        rebound (transition released [:studio/gesture-down (assoc pad :cue-id "breathe" :x 1)])
        timeout #(get-in % [:dispatch-later 0 :dispatch])
        early (transition (:db rebound) (timeout down))
        menu (transition (:db rebound) (timeout rebound))
        up (transition (:db menu) [:studio/gesture-up 1])]
    (is (= 500 (get-in rebound [:dispatch-later 0 :ms])))
    (is (empty? early))
    (is (= [:studio/open-actions {:cue-id "breathe" :x 1 :y 0}] (:dispatch menu)))
    (is (nil? (:dispatch up)))
    (is (empty? (:fx up)))))

(deftest recent-cues-use-performance-gestures-without-changing-the-authored-page
  (let [id ["afterglow.control/recent-cues"]
        recent {:id id :cells [{:x 0 :y 0 :cue-id "ember"}]}
        original (-> base
                     (assoc-in [:show :recent-page] recent)
                     (assoc-in [:ui :page-id] "main")
                     (assoc-in [:ui :edit?] true))
        selected (transition original [:studio/set-page id])
        db (:db selected)
        cell {:page-id id :cue-id "ember" :x 0 :y 0 :pointer-id 1 :client-x 5 :client-y 5}
        down (transition db [:studio/gesture-down cell])
        reordered (assoc-in (:db down) [:show :recent-page :cells 0 :cue-id] "ice")
        up (transition reordered [:studio/gesture-up 1])
        returned (transition db [:studio/set-page "main"])]
    (is (= recent (events/selected-recent-page db)))
    (is (= "main" (get-in db [:ui :page-id])))
    (is (= [[:dispatch [:studio/release-pointers]]] (:fx selected)))
    (is (empty? (transition db [:studio/toggle-edit])))
    (is (nil? (get-in (transition db [:studio/open-actions cell]) [:db :ui :action-cell])))
    (is (empty? (transition db [:studio/gesture-down (dissoc cell :cue-id)])))
    (is (nil? (:dispatch-later down)))
    (is (= [:studio/launch "ember" "toggle" {:grid nil :owner {:epoch 7}}] (:dispatch up)))
    (is (nil? (events/selected-recent-page (:db returned))))
    (is (true? (get-in returned [:db :ui :edit?])))
    (is (some #{[:dispatch [:studio/persist-ui]]} (:fx returned)))
    (is (some? (get-in (transition (:db returned) [:studio/open-actions (assoc cell :page-id "main")])
                       [:db :ui :action-cell])))))

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

(deftest automatic-cue-selection-respects-optional-visual-activity
  (let [start #(get-in (transition % [:studio/auto-start]) [:studio/http :body])
        saved (assoc-in base [:show :auto :config] {:activity-target 0.18 :seed 154})
        edited (-> saved (assoc-in [:ui :auto-form :target-activity?] true)
                         (assoc-in [:ui :auto-form :activity-target] 0))]
    (is (not (contains? (start base) :activity-target)))
    (is (= 0.18 (:activity-target (start saved))))
    (is (= 154 (:seed (start saved))))
    (is (= 0 (:activity-target (start edited))))
    (is (not (contains? (start (assoc-in edited [:ui :auto-form :target-activity?] false)) :activity-target)))))

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
        opened (transition db [:studio/definition-publish false])
        event (assoc (:dispatch opened) 4 "{:id :ember :version 2}")
        publish (transition (:db opened) event)
        stale (assoc-in (:db publish) [:show :epoch] 999)]
    (is (empty? (transition stale event)))
    (is (empty? (transition stale (conj (get-in publish [:studio/http :on-success]) {:id "ember"}))))
    (is (empty? (transition stale (conj (get-in publish [:studio/http :on-failure]) "late"))))
    (is (= 2 (get-in publish [:studio/http :body :expected-version])))
    (is (= "{:id :ember :version 2}" (get-in publish [:studio/http :body :source])))
    (is (true? (get-in publish [:db :ui :definition :publishing?])))))

(deftest changing-the-source-clears-the-previous-validation-result
  (let [db (update-in base [:ui :definition] merge {:source "{:id" :edit-revision 3
                                                   :error? true :result "EOF while reading"})
        edited (:db (transition db [:studio/definition-source "{:id :wash}"]))]
    (is (= {:source "{:id :wash}" :edit-revision 4 :status "Unpublished changes" :error? false :result ""}
           (select-keys (get-in edited [:ui :definition]) [:source :edit-revision :status :error? :result])))))

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

(deftest reconnecting-the-track-reference-sends-only-show-session-identity
  (let [reference {:configured? true :status "stale" :reason "show-changed" :override? true}
        db (-> base (assoc-in [:show :runtime-id] "reference-host")
                    (assoc-in [:show :epoch] 5)
                    (assoc-in [:show :connection-status :section-reference] reference))
        open (:db (transition db [:connections/open]))
        pushed (:db (transition open [:connections/pushed]))
        chosen (transition pushed [:connections/action :reference-reconnect])
        request (get-in chosen [:fx 0 1])
        waiting (:db chosen)
        receipt (conj (:on-failure request) "Reconnect belongs to a previous show session")
        refused (:db (transition waiting receipt))]
    (is (= reference (get-in pushed [:connections :section-reference])))
    (is (= "/api/section-reference/reconnect" (:path request)))
    (is (= {:runtime-id "reference-host" :expected-epoch 5} (:body request)))
    (is (contains? (get-in waiting [:connections :pending]) :reference-reconnect))
    (is (not (contains? (get-in refused [:connections :pending]) :reference-reconnect)))
    (is (nil? (:db (transition (assoc-in waiting [:show :epoch] 6) receipt))))
    (is (= "Reconnect belongs to a previous show session"
           (get-in refused [:connections :errors :reference-reconnect])))
    (is (true? (get-in refused [:connections :section-reference :override?])))
    (is (nil? (get-in (transition pushed [:connections/close]) [:db :connections :section-reference])))))

(deftest device-refresh-failure-leaves-a-running-stage-operable
  (let [running (-> base
                    (assoc-in [:show :runtime-id] "stage-host")
                    (assoc-in [:show :connection-status :stage-invitation]
                              {:available? true :phase "discoverable" :id "current-stage"
                               :stage-name "Dance floor" :device-id "audience"}))
        opened (:db (transition running [:connections/open]))
        refresh (transition opened [:connections/refresh-devices])
        ports-request (some (fn [[_ request]] (when (= "/api/sync/ports" (:path request)) request)) (:fx refresh))
        message "MIDI enumeration timed out"
        failed (:db (transition opened (conj (:on-failure ports-request) message)))
        recovered (:db (transition failed (conj (:on-success ports-request) [])))
        render (fn [db]
                 (reset! rf-db/app-db db)
                 (renderToStaticMarkup (r/as-element [connections/connection-view db (connections/state db)])))
        before (render opened)
        during (render failed)
        after (render recovered)]
    (is (.includes before ">Stop stage</button>"))
    (is (.includes during (str ">" message "</p>")))
    (is (.includes during ">Stop stage</button>"))
    (is (not (.includes after message)))
    (is (.includes after ">Stop stage</button>"))))

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

(deftest selected-pcm-channel-is-explicit-and-restored-in-studio
  (let [db (assoc base :connections {:visible? true :audio-form (assoc connections/audio-defaults :input "system")})
        width (:db (transition db [:connections/select-audio-width "4"]))
        invalid (transition width [:connections/audio-start])
        selected (:db (transition (:db invalid) [:connections/edit [:audio-form :input-channel] "3"]))
        start (transition selected [:connections/audio-start])
        config (dissoc (get-in start [:fx 0 1 :body]) :expected-epoch :runtime-id)
        restored (:db (transition (assoc-in db [:show :connection-status :audio] {:state "idle" :config config})
                                  [:connections/pushed]))
        stereo (:db (transition restored [:connections/select-audio-width "2"]))]
    (is (nil? (:fx invalid)))
    (is (= "Choose an input channel for this PCM channel count." (get-in invalid [:db :connections :errors :audio-start])))
    (is (= {:default? true :channels 4 :input-channel 3}
           (select-keys config [:default? :channels :input-channel])))
    (is (nil? (get-in start [:db :connections :errors :audio-start])))
    (is (= config (connections/audio-payload restored)))
    (is (= "" (get-in stereo [:connections :audio-form :input-channel])))
    (is (= 2 (:channels (connections/audio-payload stereo))))
    (is (not (contains? (connections/audio-payload stereo) :input-channel)))
    (doseq [[channels input-channel] [["65" "3"] ["4" "5"] ["4" "0"] ["4" "2.5"]]]
      (is (thrown? js/Error (connections/audio-payload
                            (update-in db [:connections :audio-form] assoc :channels channels :input-channel input-channel)))))))

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

(deftest preview-fixture-updates-default-to-interpolation-and-replay-user-selection
  (let [initial (transition base [:studio/preview-ready true])
        change (transition base [:studio/preview-setting :lighting-mode "hold"])
        remount (transition (:db change) [:studio/preview-ready true])]
    (is (= "interpolate" (get-in base [:ui :preview-settings :lighting-mode])))
    (is (some #{[:studio/preview [:lighting-mode "interpolate"]]} (:fx initial)))
    (is (= [:lighting-mode "hold"] (:studio/preview change)))
    (is (nil? (:studio/http change)))
    (is (some #{[:studio/preview [:lighting-mode "hold"]]} (:fx remount)))
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

(deftest transport-mapping-controls-keep-keyword-string-and-structured-enum-choices
  (let [values [:ember "ember" {:oscillator :square}]
        typed {:live {:parameters {22 {:type :enum :param :wave :cue-id :one :values values}}}}
        rows (connections/parameter-rows (projection/presentation typed) typed)]
    (is (= values (:values (first rows))))))

(defn- advance-settings [db view revision]
  (stream-event db {:protocol projection/protocol :kind :full :token (str "revision-" revision)
                    :identity (projection/identity-of view) :view (assoc view :revision revision)}))

(deftest pause-ack-keeps-edit-until-stream-arrives-and-preserves-subsequent-edit
  (let [old {:level 0.1 :mode "show"} requested {:level 0.35 :mode "show"}
        view (-> (:show base) (assoc :runtime-id "run" :show-id :main) (assoc-in [:settings :output :pause-look] old))
        db (:db (full-frame base view))
        db (assoc-in db [:connections :pause-form] requested)
        sent (:db (transition db [:connections/save-pause]))
        token (connections/token sent)
        acked (:db (transition sent [:connections/settings-saved token :pause {}]))
        modified (assoc-in sent [:connections :pause-form :level] 0.8)
        changed (:db (transition modified [:connections/settings-saved token :pause {}]))
        accepted (:db (advance-settings acked (assoc-in view [:settings :output :pause-look] requested) 4))
        restored (:db (advance-settings accepted view 5))]
    (is (= 0.35 (:level (connections/pause-form acked))))
    (is (= 0.8 (:level (connections/pause-form changed))))
    (is (= 0.35 (:level (connections/pause-form accepted))))
    (is (= 0.1 (:level (connections/pause-form restored))))))

(deftest audio-ack-retains-submitted-routing-only-until-the-authoritative-update
  (let [view (-> (:show base) (assoc :runtime-id "run" :show-id :main) (assoc-in [:settings :audio-returns] returns/empty-config))
        owner ["run" 7] db (:db (full-frame base view))
        edited (:db (transition db [:audio-returns/edit [:block-frames] 2048]))
        requested (transition edited [:audio-returns/request :configure])
        submitted (get-in requested [:studio/http :body :config])
        ack-event (get-in requested [:studio/http :on-success])
        acked (:db (transition (:db requested) (conj ack-event {})))
        accepted (:db (advance-settings acked (assoc-in view [:settings :audio-returns] submitted) 4))]
    (is (= 2048 (:block-frames submitted)))
    (is (= submitted (get-in acked [:audio-returns owner :submitted])))
    (is (nil? (get-in accepted [:audio-returns owner :submitted])))))

(deftest spatial-drafts-wait-for-their-acknowledged-projection
  (let [volume (spatial/default-spec :ball)
        path [:drafts :orb]
        original (-> (:show base)
                     (assoc :runtime-id "runtime" :show-id :venue)
                     (assoc-in [:settings :spatial :volumes :orb] volume))
        saved (assoc volume :radius 2)
        next-view (-> original (assoc :revision 7)
                      (assoc-in [:settings :spatial :volumes :orb] saved))
        project (fn [db view]
                  (:db (stream-event (with-stream db)
                                     {:protocol projection/protocol :kind :full
                                      :token (str (:epoch view) "-" (:revision view))
                                      :identity (projection/identity-of view) :view view})))
        event (fn [db message] (:db (transition db message)))
        draft (fn [db] (get-in (spatial-drafts/state db) path))
        edit (fn [db field value] (event db [::spatial/volume-field :orb field value]))
        edited (edit (project base original) :radius 2)
        pending (transition edited [::spatial/save {:volumes {:orb saved}} "Saved" path :orb])
        receipt (conj (get-in pending [:studio/http :on-success]) {:revision 7})]
    (doseq [http-first? [true false] edit-again? [true false]]
      (let [first-result (if http-first? (event (:db pending) receipt)
                                           (project (:db pending) next-view))
            interim (if http-first? (project first-result (assoc original :revision 6)) first-result)
            local (if edit-again? (edit interim :hardness 0.2) interim)
            completed (if http-first? (project local next-view) (event local receipt))]
        (is (= saved (draft interim)))
        (is (= (when edit-again? (assoc saved :hardness 0.2)) (draft completed)))
        (is (nil? (:pending (spatial-drafts/state completed))))))
    (let [replacement (project (:db pending) (assoc next-view :epoch 8))]
      (is (nil? (draft (event replacement receipt)))))
    (let [queued (event (:db pending) (conj (get-in pending [:studio/http :on-success])
                                            {:status "pending" :message "Command remains queued"}))]
      (is (= saved (draft queued)))
      (is (nil? (:pending (spatial-drafts/state queued)))))))
