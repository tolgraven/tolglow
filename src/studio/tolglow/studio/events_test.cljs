(ns tolglow.studio.events-test
  (:require [cljs.test :refer-macros [deftest is testing use-fixtures]]
            [cljs.reader :as reader]
            [tolglow.studio.audio-returns :as returns]
            [clojure.string :as str]
            [re-frame.db :as rf-db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [reagent.core :as r]
            ["react-dom/server" :refer [renderToStaticMarkup]]
            [tolglow.studio.events :as events]
            [tolglow.studio.checkpoints :as checkpoints]
            [tolglow.studio.operator-history :as history]
            [tolglow.studio.projection :as projection]
            [tolglow.studio.reactive-editor :as reactive-editor]
            [tolglow.studio.spatial :as spatial]
            [tolglow.studio.spatial-drafts :as spatial-drafts]
            [tolglow.studio.fixture-library :as fixtures]
            [tolglow.studio.frame-stream :as frame-stream]
            [tolglow.studio.interop :as interop]
            [tolglow.studio.connections :as connections]
            [tolglow.studio.views :as views]
            [tolglow.transit :as transit]
            [tolglow.studio.chat]))

;; Execute actual registered event handlers/interceptors while leaving their
;; declared I/O effects unapplied. This tests transitions, not duplicate stubs.
(defn transition [db event]
  (reset! rf-db/app-db db)
  (let [effects (:effects (interceptor/execute event (remove #(= :do-fx (:id %))
                                                             (registrar/get-handler :event (first event)))))]
    ;; Lane frames are installed state, like :db, not I/O. Install them as the
    ;; registered effect does so later patches find their exact baseline.
    (doseq [[id installs] (:fx effects) :when (= id :studio/frame-install)]
      (doseq [[lane frame] installs] (frame-stream/install! lane frame)))
    ;; Publishing lane frames to their readers is state as well.
    (doseq [[id lanes] (:fx effects) :when (= id :studio/frame-notify)]
      (doseq [lane lanes] (frame-stream/notify! lane)))
    effects))

(defn stream-reply
  "The socket reply of a stream event: an ACK follows lane installation in :fx."
  [effects]
  (or (:studio/stream-reply effects)
      (some (fn [[id value]] (when (= id :studio/stream-reply) value)) (:fx effects))))

;; Lane frames are global installed state. Every test starts and ends without
;; them, so test order cannot decide whether a later patch finds its baseline.
(use-fixtures :each {:before frame-stream/reset-all! :after frame-stream/reset-all!})

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
  ;; A patch inside lane-owned subtrees installs lanes and leaves app-db as it
  ;; was: report that unchanged db, so tests can chain the resulting state.
  (let [effects (transition db [:studio/stream-frame (:session db) "connected" envelope])]
    (cond-> effects
      (and (not (contains? effects :db)) (some #(= :studio/frame-install (first %)) (:fx effects)))
      (assoc :db db))))

(defn full-frame [db view]
  (stream-event (with-stream db)
                {:protocol projection/protocol :kind :full :token "full"
                 :identity (projection/identity-of view) :view view}))

(def checkpoint-owner {:runtime-id "host" :show-id :venue :epoch 7})

(defn- checkpoint-db []
  (let [db (:db (full-frame base (merge (:show base) checkpoint-owner)))
        opened (transition db [:checkpoints/open])
        loading (transition (:db opened) (:dispatch opened))
        loaded (conj (get-in loading [:studio/http :on-success])
                     (assoc checkpoint-owner :enabled? false
                            :checkpoints {:recovery {:modified-at-ms 1700000000000}
                                          :named {:modified-at-ms 1700001000000}
                                          :prior {:modified-at-ms 1600000000000}}))]
    {:loading loading :db (:db (transition (:db loading) loaded))}))

(deftest checkpoints-show-typed-catalog-and-confirm-each-file-action
  (let [{:keys [db loading]} (checkpoint-db)
        markup (renderToStaticMarkup (r/as-element [checkpoints/panel (checkpoints/view-props db)]))
        restore (:db (transition db [:checkpoints/confirm :restore :named]))
        request (transition restore [:checkpoints/submit])
        discard (:db (transition db [:checkpoints/confirm :discard :prior]))
        dismissal (transition discard [:checkpoints/submit])
        accepted (transition (:db dismissal) (conj (get-in dismissal [:studio/http :on-success]) {:status :accepted}))]
    (is (= "/api/show/checkpoints" (get-in loading [:studio/http :path])))
    (is (not (contains? (:studio/http loading) :body)))
    (is (str/includes? markup "aria-labelledby=\"show-checkpoint-title\""))
    (is (every? #(str/includes? markup %) ["Working autosave" "Named save" "Previous session"
                                         "2023-11-14T22:13:20.000Z" "Automatic recovery is off."]))
    (is (nil? (:studio/http (transition db [:checkpoints/submit]))))
    (is (nil? (get-in (transition db [:checkpoints/confirm :discard :named]) [:db :show-checkpoints :confirmation])))
    (is (= "/api/show/checkpoints/restore" (get-in request [:studio/http :path])))
    (is (= (assoc checkpoint-owner :kind :named) (get-in request [:studio/http :body])))
    (is (true? (get-in request [:studio/http :typed?])))
    (is (nil? (:studio/http (transition (:db request) [:checkpoints/submit]))))
    (is (= "/api/show/checkpoints/discard-prior" (get-in dismissal [:studio/http :path])))
    (is (= checkpoint-owner (get-in dismissal [:studio/http :body])))
    (is (= {:message "Previous session checkpoint discarded." :error? false}
           (get-in accepted [:db :show-checkpoints :feedback])))
    (is (= [:checkpoints/refresh] (:dispatch accepted)))))

(def show-folder {:id "5f0c3c1e-0000-4000-8000-000000000001" :file ".tolglow-folder-id"
                  :path "/Users/op/Shows/Club" :new? true})

(defn- legacy-checkpoint-db []
  (let [db (:db (full-frame base (merge (:show base) checkpoint-owner)))
        loading (transition (:db (transition db [:checkpoints/open])) [:checkpoints/refresh])
        loaded (conj (get-in loading [:studio/http :on-success])
                     (assoc checkpoint-owner :enabled? true :folder show-folder
                            :checkpoints {:legacy {:modified-at-ms 1600000000000}
                                          :legacy-prior {:modified-at-ms 1590000000000}}))]
    (:db (transition (:db loading) loaded))))

(deftest the-checkpoint-from-before-the-update-is-a-separate-explicit-choice
  (let [db (legacy-checkpoint-db)
        markup (renderToStaticMarkup (r/as-element [checkpoints/panel (checkpoints/view-props db)]))
        restore (transition (:db (transition db [:checkpoints/confirm :restore :legacy])) [:checkpoints/submit])
        confirming (:db (transition db [:checkpoints/confirm :discard :legacy]))
        confirm-markup (renderToStaticMarkup (r/as-element [checkpoints/panel (checkpoints/view-props confirming)]))
        dismissal (transition confirming [:checkpoints/submit])
        accepted (transition (:db dismissal) (conj (get-in dismissal [:studio/http :on-success]) {:status :accepted}))]
    (is (every? #(str/includes? markup %)
                ["Checkpoint from before the update" "data-checkpoint=\"legacy\"" "Restore Checkpoint from before the update"
                 "Dismiss Checkpoint from before the update" "applies it to this folder only"
                 "Previous session from before the update" "data-checkpoint=\"legacy-prior\""
                 "Show folder" ".tolglow-folder-id" "Copies are detected and safe"]))
    (is (= "/api/show/checkpoints/restore" (get-in restore [:studio/http :path])))
    (is (= (assoc checkpoint-owner :kind :legacy) (get-in restore [:studio/http :body])))
    (is (str/includes? confirm-markup "No show folder will offer them again"))
    (is (str/includes? confirm-markup "Dismiss checkpoints"))
    (is (= "/api/show/checkpoints/dismiss-legacy" (get-in dismissal [:studio/http :path])))
    (is (= checkpoint-owner (get-in dismissal [:studio/http :body])))
    (is (= {:message "Checkpoints from before the update dismissed." :error? false}
           (get-in accepted [:db :show-checkpoints :feedback])))
    (testing "a show that does not run from a Mac folder has no such row or folder section"
      (let [{:keys [db]} (checkpoint-db)
            markup (renderToStaticMarkup (r/as-element [checkpoints/panel (checkpoints/view-props db)]))]
        (is (not (str/includes? markup "before the update")))
        (is (not (str/includes? markup "Show folder")))
        (is (nil? (get-in (transition db [:checkpoints/confirm :discard :legacy]) [:db :show-checkpoints :confirmation])))))))

(deftest a-restore-that-could-not-be-recorded-says-so
  (let [db (legacy-checkpoint-db)
        posted (transition (:db (transition db [:checkpoints/confirm :restore :legacy-prior])) [:checkpoints/submit])
        restored-view (:db (full-frame (:db posted) (merge (:show base) checkpoint-owner {:epoch 8 :revision 4})))
        ack (conj (get-in posted [:studio/http :on-success])
                  {:status :accepted :epoch 8 :legacy-resolution-error "Read-only file system"})
        {:keys [message error?]} (get-in (transition restored-view ack) [:db :show-checkpoints :feedback])]
    (is (= (assoc checkpoint-owner :kind :legacy-prior) (get-in posted [:studio/http :body])))
    (is (true? error?))
    (is (str/includes? message "Restored Previous session from before the update, but Tolglow could not record it"))
    (is (str/includes? message "Read-only file system"))))

(deftest a-new-show-folder-explains-its-identity-file-once
  (let [render #(renderToStaticMarkup (r/as-element [checkpoints/folder-notice (checkpoints/notice-props %)]))
        with-folder #(assoc-in base [:show :show-folder] %)
        fresh (with-folder show-folder)
        closing (transition fresh [:show-folder/close-notice (checkpoints/notice-key show-folder :identity)])
        closed (:db closing)]
    (is (= [:show-folder/remember #{(checkpoints/notice-key show-folder :identity)}] (first (:fx closing)))
        "closing a note is remembered across reloads")
    (is (str/includes? (render fresh) "This show folder now has its own recovery"))
    (is (str/includes? (render fresh) ".tolglow-folder-id"))
    (is (str/includes? (render fresh) "Read this again under Restore show"))
    (is (= "" (render closed)) "closing it hides it for this folder")
    (is (= "" (render (with-folder (assoc show-folder :new? false)))) "a folder opened before says nothing")
    (is (= "" (render base)) "an ordinary show has no folder")
    (is (str/includes? (render (with-folder (assoc show-folder :copy-of "/Users/op/Shows/Original")))
                       "This folder is a copy of <code>/Users/op/Shows/Original</code>"))
    (testing "a waiting checkpoint from before the update is offered, never applied"
      (let [waiting (with-folder (assoc show-folder :new? false
                                        :legacy-checkpoints {:legacy {:modified-at-ms 1600000000000}
                                                             :legacy-prior {:modified-at-ms 1590000000000}}))
            markup (render waiting)
            later (:db (transition waiting [:show-folder/close-notice (checkpoints/notice-key show-folder :legacy)]))]
        (is (str/includes? markup "A checkpoint from before the update is waiting"))
        (is (str/includes? markup "Nothing has been applied"))
        (is (str/includes? markup "Review checkpoint"))
        (is (every? #(str/includes? markup %) ["Checkpoint from before the update, saved" "2020-09-13T12:26:40.000Z"
                                               "Previous session from before the update, saved"]))
        (is (= "" (render later)))
        (is (true? (get-in (transition waiting [:checkpoints/open]) [:db :show-checkpoints :open?])))
        (is (= [:checkpoints/open] (:dispatch (transition waiting [:view-tabs/host "checkpoints"])))
            "the Mac app's File menu opens the same dialog")))))

(deftest restore-acknowledgement-follows-the-owner-across-the-matching-new-epoch
  (let [{:keys [db]} (checkpoint-db)
        db (update db :ui assoc :rig-dirty? true :rig-draft {:name "Old unsaved rig"})
        chosen (:db (transition db [:checkpoints/confirm :restore :recovery]))
        posted (transition chosen [:checkpoints/submit])
        ack (conj (get-in posted [:studio/http :on-success]) {:status :accepted :epoch 8})
        waiting (:db (transition (:db posted) ack))
        rig {:name "Restored venue" :room {:measured-fog {:extinction-per-m 0.2}}}
        new-view (merge (:show base) checkpoint-owner {:epoch 8 :rig rig})
        newer #(:db (full-frame % (assoc new-view :revision 4)))
        early-ack (transition (newer waiting) [:checkpoints/show-changed])
        stream-first (:db (transition (newer (:db posted)) [:checkpoints/show-changed]))
        late-ack (transition stream-first ack)]
    (is (= :awaiting-view (get-in waiting [:show-checkpoints :operation :phase])))
    (is (true? (get-in waiting [:ui :rig-dirty?])))
    (doseq [effect [early-ack late-ack]]
      (is (= "Restored Working autosave." (get-in effect [:db :show-checkpoints :feedback :message])))
      (is (nil? (get-in effect [:db :show-checkpoints :operation])))
      (is (false? (get-in effect [:db :ui :rig-dirty?])))
      (is (= rig (get-in effect [:db :ui :rig-draft])))
      (is (= [:checkpoints/refresh] (:dispatch effect))))))

(deftest a-late-checkpoint-ack-keeps-edits-to-the-new-show
  (let [{:keys [db]} (checkpoint-db)
        db (update db :ui assoc :rig-dirty? true :rig-draft {:name "Old unsaved rig"})
        chosen (:db (transition db [:checkpoints/confirm :restore :named]))
        submitted (transition chosen [:checkpoints/submit])
        closed (:db (transition (:db submitted) [:checkpoints/close]))
        view (merge (:show base) checkpoint-owner {:epoch 8 :revision 4 :rig {:name "Restored rig"}})
        installed (:db (full-frame closed view))
        changed (:db (transition installed [:checkpoints/show-changed]))
        edited (:db (transition changed [:studio/rig-set [:name] "Fresh edit"]))
        ack (conj (get-in submitted [:studio/http :on-success]) {:status :accepted :epoch 8})
        completed (transition edited ack)]
    (is (= {:name "Old unsaved rig"} (get-in installed [:ui :rig-draft])))
    (is (true? (get-in installed [:ui :rig-dirty?])))
    (is (= [:studio/toast "Restored Named save." false] (:dispatch completed)))
    (is (= {:name "Fresh edit"} (get-in completed [:db :ui :rig-draft])))
    (is (true? (get-in completed [:db :ui :rig-dirty?])))))

(deftest an-early-checkpoint-ack-keeps-edits-made-while-waiting-for-the-view
  (let [{:keys [db]} (checkpoint-db)
        db (update db :ui assoc :rig-dirty? true :rig-draft {:name "Old unsaved rig"})
        chosen (:db (transition db [:checkpoints/confirm :restore :named]))
        submitted (transition chosen [:checkpoints/submit])
        ack (conj (get-in submitted [:studio/http :on-success]) {:status :accepted :epoch 8})
        waiting (:db (transition (:db submitted) ack))
        edited (:db (transition waiting [:studio/rig-set [:name] "Fresh edit"]))
        view (merge (:show base) checkpoint-owner {:epoch 8 :revision 4 :rig {:name "Restored rig"}})
        installed (:db (full-frame edited view))
        completed (:db (transition installed [:checkpoints/show-changed]))]
    (is (= "Restored Named save." (get-in completed [:show-checkpoints :feedback :message])))
    (is (= (:rig view) (get-in completed [:show :rig])))
    (is (= {:name "Fresh edit"} (get-in completed [:ui :rig-draft])))
    (is (true? (get-in completed [:ui :rig-dirty?])))))

(deftest a-peer-show-change-keeps-the-draft-when-our-restore-is-rejected
  (let [{:keys [db]} (checkpoint-db)
        draft {:name "Unconfirmed draft"}
        db (update db :ui assoc :rig-dirty? true :rig-draft draft)
        chosen (:db (transition db [:checkpoints/confirm :restore :named]))
        submitted (transition chosen [:checkpoints/submit])
        peer-view (merge (:show base) checkpoint-owner {:epoch 8 :revision 4 :rig {:name "Peer restored rig"}})
        installed (:db (full-frame (:db submitted) peer-view))
        changed (:db (transition installed [:checkpoints/show-changed]))
        failed (conj (get-in submitted [:studio/http :on-failure])
                     "Checkpoint operation belongs to a previous show session")
        rejected (:db (transition changed failed))]
    (is (= 8 (get-in installed [:show :epoch])))
    (is (= draft (get-in installed [:ui :rig-draft])))
    (is (= draft (get-in rejected [:ui :rig-draft])))
    (is (true? (get-in rejected [:ui :rig-dirty?])))
    (is (true? (get-in rejected [:show-checkpoints :feedback :error?])))))

(deftest queued-checkpoint-success-follows-the-receipt-and-its-installed-epoch
  (let [{:keys [db]} (checkpoint-db)
        db (update db :ui assoc :rig-dirty? true :rig-draft {:name "Old draft"})
        chosen (:db (transition db [:checkpoints/confirm :restore :named]))
        posted (transition chosen [:checkpoints/submit])
        pending (conj (get-in posted [:studio/http :on-success]) {:status :pending :receipt-id "own"})
        queued (:db (transition (:db posted) pending))
        rig {:name "Restored rig"}
        receipt {:id "own" :identity checkpoint-owner :status :accepted :epoch 8}
        view (merge (:show base) checkpoint-owner {:epoch 8 :rig rig :checkpoint-results [receipt]})
        installed (full-frame queued view)
        early-pending (transition (:db installed) [:checkpoints/show-changed])
        projected-first (:db (full-frame (:db posted) view))
        late-pending (transition (:db (transition projected-first [:checkpoints/show-changed])) pending)]
    (is (= :queued (get-in queued [:show-checkpoints :operation :phase])))
    (is (some #{[:dispatch [:checkpoints/show-changed]]} (:fx installed)))
    (doseq [result [early-pending late-pending]]
      (is (= "Restored Named save." (get-in result [:db :show-checkpoints :feedback :message])))
      (is (nil? (get-in result [:db :show-checkpoints :operation])))
      (is (= rig (get-in result [:db :ui :rig-draft])))
      (is (false? (get-in result [:db :ui :rig-dirty?]))))))

(deftest queued-checkpoint-rejection-follows-the-correlation-after-a-peer-restore
  (let [{:keys [db]} (checkpoint-db)
        draft {:name "Keep this draft"}
        db (update db :ui assoc :rig-dirty? true :rig-draft draft)
        chosen (:db (transition db [:checkpoints/confirm :restore :named]))
        posted (transition chosen [:checkpoints/submit])
        pending (conj (get-in posted [:studio/http :on-success]) {:status :pending :receipt-id "own"})
        queued (:db (transition (:db posted) pending))
        panel-closed (:db (transition queued [:checkpoints/close]))
        another {:id "peer" :identity checkpoint-owner :status :accepted :epoch 8}
        wrong-origin {:id "own" :identity (assoc checkpoint-owner :epoch 6) :status :accepted :epoch 8}
        peer-view (merge (:show base) checkpoint-owner {:epoch 8 :checkpoint-results [another wrong-origin]})
        peer-installed (:db (full-frame panel-closed peer-view))
        waiting (:db (transition peer-installed [:checkpoints/show-changed]))
        reopened (:db (transition waiting [:checkpoints/open]))
        rejected {:id "own" :identity checkpoint-owner :status :rejected :reason :stale-show-session}
        pushed (stream-event reopened {:protocol projection/protocol :kind :patch :base-token "full" :token "rejected"
                                       :identity (projection/identity-of peer-view)
                                       :changes [[:set [:checkpoint-results] [another rejected]]]})
        completed (:db (transition (:db pushed) [:checkpoints/receipt-pushed]))]
    (is (= :queued (get-in waiting [:show-checkpoints :operation :phase])))
    (is (nil? (checkpoints/queued-result waiting)))
    (is (some #{[:dispatch [:checkpoints/receipt-pushed]]} (:fx pushed)))
    (is (nil? (get-in completed [:show-checkpoints :operation])))
    (is (= {:message "The show changed before the queued restore could run." :error? true}
           (get-in completed [:show-checkpoints :feedback])))
    (is (= draft (get-in completed [:ui :rig-draft])))
    (is (true? (get-in completed [:ui :rig-dirty?])))))

(deftest awaiting-a-queued-checkpoint-is-bounded-by-the-request-and-runtime
  (let [{:keys [db]} (checkpoint-db)
        draft {:name "New edit"}
        db (update db :ui assoc :rig-dirty? true :rig-draft draft)
        chosen (:db (transition db [:checkpoints/confirm :restore :named]))
        posted (transition chosen [:checkpoints/submit])
        pending (conj (get-in posted [:studio/http :on-success]) {:status :pending :receipt-id "own"})
        queued (:db (transition (:db posted) pending))
        disconnected (:db (transition queued [:studio/stream-closed (:session queued) "connected" "lost"]))
        expired (transition disconnected (get-in posted [:dispatch-later 0 :dispatch]))
        receipt {:id "own" :identity checkpoint-owner :status :accepted :epoch 8}
        view (merge (:show base) checkpoint-owner {:epoch 8 :checkpoint-results [receipt]})
        reconnected (:db (full-frame disconnected view))
        accepted (transition reconnected [:checkpoints/show-changed])
        replaced (:db (full-frame disconnected (assoc view :runtime-id "replacement")))
        retired (transition replaced [:checkpoints/show-changed])]
    (is (= :queued (get-in disconnected [:show-checkpoints :operation :phase])))
    (is (= "Restored Named save." (get-in accepted [:db :show-checkpoints :feedback :message])))
    (doseq [effect [expired retired]]
      (is (nil? (get-in effect [:db :show-checkpoints :operation])))
      (is (true? (get-in effect [:db :show-checkpoints :feedback :error?])))
      (is (= draft (get-in effect [:db :ui :rig-draft])))
      (is (empty? (transition (:db effect) pending))))))

(deftest checkpoint-catalog-confirmations-and-results-fence-stale-shows
  (let [{:keys [db loading]} (checkpoint-db)
        changed #(assoc-in % [:server-projection :view :epoch] 8)
        stale-response (conj (get-in loading [:studio/http :on-success])
                             (assoc checkpoint-owner :checkpoints {:prior {:modified-at-ms 10}}))
        changed-during-load (:db (transition (changed (:db loading)) stale-response))
        chosen (:db (transition db [:checkpoints/confirm :restore :prior]))
        posted (transition chosen [:checkpoints/submit])
        stale-ack (conj (get-in posted [:studio/http :on-success]) {:status :accepted :epoch 9})
        wrong-epoch (transition (changed (:db posted)) stale-ack)
        failure (conj (get-in posted [:studio/http :on-failure]) "Checkpoint is unavailable.")
        failed (transition (:db posted) failure)]
    (is (nil? (get-in changed-during-load [:show-checkpoints :catalog])))
    (is (nil? (:studio/http (transition (changed chosen) [:checkpoints/submit]))))
    (is (= true (get-in wrong-epoch [:db :show-checkpoints :feedback :error?])))
    (is (= "Checkpoint is unavailable." (get-in failed [:db :show-checkpoints :feedback :message])))))

(defn first-command [effects] (second (first (:fx effects))))

(defn- history-frame [db runtime epoch revision cursor position & [outcomes]]
  (let [view {:runtime-id runtime :show-id "history-show" :epoch epoch :revision revision :frame-number revision
              :history-command-outcomes (or outcomes [])
              :operator-history {:identity [runtime "history-show" epoch] :current-id cursor :position position
                                 :nodes [{:id 0 :parent-id nil}
                                         {:id 1 :parent-id 0 :parameter :gain :before 0.25 :after 0.8}
                                         {:id 2 :parent-id 0 :parameter :gain :before 0.25 :after 0.4}]}}]
    (:db (stream-event db {:protocol projection/protocol :kind :full :token (str runtime "-" revision)
                           :identity (projection/identity-of view) :view view}))))

(defn- history-command [db op]
  (let [{:keys [projection view-state apply-status]} (events/history-inspector db)]
    [:operator-history/navigate op (get (history/commands projection view-state apply-status) op)]))

(deftest history-preview-is-local-and-apply-waits-for-a-committed-frame
  (let [live (history-frame (with-stream base) "runtime" 7 3 1 8)
        open (:db (transition live [:operator-history/open]))
        inspected (transition open [:operator-history/inspect [:preview 2]])
        apply-step (history-command (:db inspected) :history/select)
        submitted (transition (:db inspected) apply-step)
        pending (:db submitted)
        command (first-command submitted)
        receipt (conj (:on-success command) {:status "accepted" :command-id (get-in command [:fields :command-id])
                                            :epoch 7 :revision 4 :history-cursor 2 :history-position 9})
        confirmed (:db (transition pending receipt))
        state-first (history-frame pending "runtime" 7 4 2 9)
        both-ways [(history-frame confirmed "runtime" 7 4 2 9)
                   (:db (transition state-first receipt))]]
    (is (empty? (:fx inspected)))
    (is (= :history/select (:op command)))
    (is (= {:node-id 2 :expected-cursor 1 :expected-history-position 8 :expected-epoch 7}
           (dissoc (:fields command) :command-id)))
    (is (= "runtime" (:runtime-id command)))
    (is (string? (get-in command [:fields :command-id])))
    (is (empty? (:fx (transition pending apply-step))))
    (is (= 1 (get-in (events/history-inspector confirmed) [:projection :current-id])))
    (is (get-in (events/history-inspector confirmed) [:apply-status :pending?]))
    (doseq [db both-ways]
      (let [{:keys [projection view-state apply-status]} (events/history-inspector db)]
        (is (= 2 (:current-id projection)))
        (is (nil? (:preview view-state)))
        (is (false? (:pending? apply-status)))
        (is (= "History step applied." (:message apply-status)))))))

(deftest pending-history-http-and-transport-timeouts-wait-for-matching-pushed-results
  (let [live (history-frame (with-stream base) "runtime" 7 3 1 8)
        open (:db (transition live [:operator-history/open]))
        preview (:db (transition open [:operator-history/inspect [:preview 2]]))
        submitted (transition preview (history-command preview :history/select))
        pending (:db submitted)
        command (first-command submitted)
        id (get-in command [:fields :command-id])
        identity {:command-id id :epoch 7}
        pending-http (conj (:on-success command) (assoc identity :status "pending"))
        pending-reply (:db (transition pending pending-http))
        uncertain (:db (transition pending (:on-uncertain command)))
        deadline (:dispatch (first (:dispatch-later submitted)))
        accepted (merge identity {:status :accepted :revision 4 :history-cursor 2 :history-position 9})
        rejected (merge identity {:status :rejected :revision 3 :code :history-conflict
                                  :message "The parameter changed after this edit."})
        unrelated (history-frame pending-reply "runtime" 7 4 2 9
                                 [(assoc accepted :command-id "different") (assoc accepted :epoch 6)])]
    (is (= 7000 (:timeout-ms command)))
    (doseq [db [pending-reply uncertain unrelated]]
      (let [{:keys [apply-status view-state]} (events/history-inspector db)]
        (is (:pending? apply-status))
        (is (= "Waiting for the live show to confirm the history command." (:message apply-status)))
        (is (= 2 (get-in view-state [:preview :target-id])))))
    (doseq [[result current position] [[accepted 2 9] [rejected 1 8]]]
      (let [deliver #(history-frame % "runtime" 7 4 current position [result])
            stream-first (deliver pending)]
        (is (= stream-first (:db (transition stream-first pending-http))))
        (is (= stream-first (:db (transition stream-first (:on-uncertain command)))))
        (doseq [db [(deliver pending-reply) (deliver uncertain) stream-first]]
          (let [{:keys [apply-status view-state]} (events/history-inspector db)]
            (is (false? (:pending? apply-status)))
            (if (= :accepted (:status result))
              (do (is (= "History step applied." (:message apply-status)))
                  (is (nil? (:preview view-state))))
              (do (is (= (:message rejected) (:message apply-status) (:conflict apply-status)))
                  (is (:error? apply-status))
                  (is (= 2 (get-in view-state [:preview :target-id])))))))))
    (let [expired (:db (transition uncertain deadline))
          restored (history-frame uncertain "runtime" 8 5 1 0 [accepted])]
      (is (false? (get-in (events/history-inspector expired) [:apply-status :pending?])))
      (is (str/includes? (get-in (events/history-inspector expired) [:apply-status :message]) "could not be confirmed"))
      (is (false? (get-in (events/history-inspector restored) [:apply-status :pending?])))
      (is (= restored (:db (transition restored pending-http))))
      (is (= restored (:db (transition restored (:on-uncertain command)))))
      (is (= restored (:db (transition restored deadline)))))))

(deftest stale-or-rejected-history-commands-preserve-the-stream-projection
  (let [live (history-frame (with-stream base) "runtime" 7 3 1 8)
        open (:db (transition live [:operator-history/open]))
        preview (:db (transition open [:operator-history/inspect [:preview 2]]))
        apply-step (history-command preview :history/select)
        submitted (transition preview apply-step)
        pending (:db submitted)
        failure (conj (:on-failure (first-command submitted)) "The parameter changed after this edit.")
        declined (:db (transition pending failure))
        unrelated (history-frame preview "runtime" 7 4 1 8)
        moved (history-frame unrelated "runtime" 7 5 0 9)
        returned (history-frame moved "runtime" 7 6 1 10)
        stale (transition moved apply-step)
        replaced (history-frame pending "new-runtime" 8 5 1 1)
        late (:db (transition replaced failure))]
    (is (empty? (:fx stale)))
    (is (seq (:fx (transition unrelated apply-step))))
    (is (empty? (:fx (transition returned apply-step))))
    (is (= 0 (get-in (events/history-inspector (:db stale)) [:projection :current-id])))
    (is (= 1 (get-in (events/history-inspector declined) [:projection :current-id])))
    (is (= "The parameter changed after this edit." (get-in (events/history-inspector declined) [:apply-status :message])))
    (is (nil? (last (history-command declined :history/select))))
    (is (some? (last (history-command (:db (transition declined [:operator-history/inspect [:preview 2]])) :history/select))))
    (is (get-in (events/history-inspector late) [:open?]))
    (is (nil? (get-in (events/history-inspector late) [:view-state :preview])))
    (is (false? (get-in (events/history-inspector late) [:apply-status :pending?])))
    (is (nil? (get-in (events/history-inspector late) [:apply-status :message])))))

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

(deftest a-cue-authored-as-held-is-momentary-in-any-launch-mode
  (doseq [authored [{:launch-mode :held} {:launch-mode "held"} {:held true}]
          ui-mode ["toggle" "one-shot" "held"]]
    (let [db (-> base (assoc-in [:ui :launch-mode] ui-mode)
                 (update-in [:show :cues] conj (merge {:id "left-out" :effect-id "blackout"} authored)))
          down (transition db [:studio/gesture-down {:pointer-id 5 :cue-id "left-out"}])
          press (first-command down)
          holding (:db (transition (:db down) (conj (:on-success press) {:instance-id "7:20"})))
          release (first-command (transition holding [:studio/gesture-up 5]))]
      (is (= "held" (events/press-mode db "left-out")))
      (is (= {:cue-id "left-out" :launch {:mode "held"}} (select-keys (:fields press) [:cue-id :launch])))
      (is (= {:op "instance/end" :fields {:instance-id "7:20"}}
             (select-keys (update release :fields dissoc :command-id) [:op :fields]))
          (str authored " " ui-mode))))
  (testing "other cues keep the selected launch mode"
    (is (= "toggle" (events/press-mode (assoc-in base [:ui :launch-mode] "toggle") "ember")))
    (is (= "one-shot" (events/press-mode (assoc-in base [:ui :launch-mode] "one-shot") "ember")))))

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

(deftest schedule-projection-lists-events-and-ramps-with-cancel-by-id
  (let [schedule {:beat 2.0 :upcoming [{:event-id "drop" :origin :scheduled :op :cue/launch :category :cue
                                        :label "Drop" :due-beat 8.0}
                                       {:event-id "subscriber/kicks/3" :origin :subscriber :op :cue/launch
                                        :category :cue :due-beat 2.5}]
                  :ramps [{:event-id "swell" :label "Swell" :to 0.8 :start 2.0 :end 10.0 :unit :beats
                           :destination {:target :param :instance-id "1:1" :param :level}}
                          {:event-id "fade" :label "Fade" :to 0 :start 120000.0 :end 124000.0 :unit :ms
                           :destination {:target :master}}]
                  :recent []}
        installed (:db (full-frame base (assoc (:show base) :show-id :main :runtime-id "host" :schedule schedule
                                                         :time-ms 121000.0
                                                         :transport (assoc (get-in base [:show :transport]) :beat 4.0))))
        html (renderToStaticMarkup (r/as-element [views/scheduled-events installed]))
        cancel (first-command (transition installed [:studio/command "schedule/cancel" {:event-ids ["drop"]}]))]
    (is (str/includes? html "Scheduled"))
    (is (str/includes? html "Drop · beat 8"))
    (is (not (str/includes? html "subscriber/kicks")) "subscriber steps stay in Debug")
    (is (str/includes? html "Swell → 0.8 by beat 10"))
    (is (str/includes? html "value=\"0.25\"") "progress is derived from the transport beat (4 of 2..10)")
    (is (str/includes? html "Fade → 0 in 3 s") "a millisecond ramp shows its remaining time, not a beat")
    (is (not (str/includes? html "beat 124000")))
    (is (str/includes? html "Cancel scheduled Drop"))
    (is (str/includes? html "Stop ramp Swell"))
    (is (= "schedule/cancel" (:op cancel)))
    (is (= {:event-ids ["drop"]} (select-keys (:fields cancel) [:event-ids])))
    (is (nil? (views/scheduled-events (assoc-in installed [:show :schedule] {:upcoming [] :ramps []}))))))

(defn- launch-failures [effects]
  (keep (fn [[kind payload]]
          (when (= :dispatch kind)
            (let [[event message] payload] (when (= :studio/failure event) message)))) (:fx effects)))

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
    (is (= ["connected" {:op :ack :token "misses"}] (stream-reply reported)))
    (is (nil? (:studio/http reported)))
    (is (empty? (launch-failures duplicate)))
    (is (empty? (launch-failures reconnected)))
    (is (= ["connected" {:op :ack :token "next"}] (stream-reply next-snapshot)))
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

(deftest auto-start-varies-parameters-and-keeps-a-repl-plan
  (let [start #(get-in (transition % [:studio/auto-start]) [:studio/http :body])
        planned (assoc base :show (assoc (:show base) :auto
                                         {:enabled? true :section :quiet
                                          :config {:sections [{:id :red :length {:bars 2}} {:id :quiet :length {:bars 2}}]
                                                   :activity-target {:waveform :sine :period {:bars 4}}}
                                          :reasons [{:reason :performer-took-over :instance-ids ["1:4"]}
                                                    {:reason :intentional-darkness :section :quiet :lit 1}]}))]
    (is (not (contains? (start base) :randomize)))
    (is (not (contains? (start base) :keep-plan?)) "nothing to keep without a plan")
    (is (= {:params "marked"} (:randomize (start (assoc-in base [:ui :auto-form :randomize?] true)))))
    (is (true? (:keep-plan? (start planned))))
    (is (not (contains? (start planned) :activity-target)) "a signal target stays with the plan, not the slider")
    (is (not (contains? (start (assoc-in planned [:ui :auto-form :keep-plan?] false)) :keep-plan?)))
    (is (true? (get-in (transition planned [:studio/auto-stop]) [:studio/http :body :keep-plan?])) "stopping keeps the plan for the next start")
    (is (= {:enabled? false :expected-epoch 7} (get-in (transition base [:studio/auto-stop]) [:studio/http :body])))
    (is (= "Section quiet · Moving in darkness · handed 1 to a performer" (events/auto-summary planned)))
    (testing "Start and Stop can wait for the next bar, phrase or section line (#767)"
      (let [on-bar (assoc-in base [:ui :auto-form :applies] "next-bar")
            on-section (assoc-in planned [:ui :auto-form :applies] "next-section")]
        (is (not (contains? (start base) :at)) "now by default")
        (is (= {:at "next-bar" :source {:adapter "web" :session-id (:session base)}}
               (select-keys (start on-bar) [:at :source])))
        (is (= "next-bar" (get-in (transition on-bar [:studio/auto-stop]) [:studio/http :body :at])))
        (is (= "next-section" (:at (start on-section))) "sent without a playing section, for the show to refuse")
        (is (= "next-section" (:at (start (assoc-in on-section [:show :auto :section-end-beat] 24.0)))))
        (is (= ["Auto configure at bar 6" "Stop auto at bar 8 beat 2.00"]
               (events/auto-pending-text (assoc-in base [:show :auto :pending]
                                                   [{:event-id "a" :label "Auto configure" :due-beat 24.0}
                                                    {:event-id "b" :label "Stop auto" :due-beat 34.0}]))))))
    (is (= "" (events/auto-summary base)))
    (testing "Vary follows a saved randomizing config until the form chooses"
      (let [varied (assoc-in planned [:show :auto :config :randomize] {:params :marked :palette [{:color [1 0 0]}]})
            kept (assoc-in planned [:show :auto :config :randomize] {:params :none :palette [{:color [1 0 0]}]})]
        (is (= {:params "marked"} (:randomize (start varied))))
        (is (not (contains? (start kept) :randomize)))
        (is (not (contains? (start (assoc-in varied [:ui :auto-form :randomize?] false)) :randomize)))))))

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

(deftest building-a-reactive-effect-uses-owner-schema-and-exact-beats
  (let [draft (-> (reactive-editor/new-draft)
                  (assoc-in [:targets 0 :fixture-id] "head/one")
                  (assoc-in [:bindings 0 :address] ":wash/level")
                  (assoc :transition "1/3"))
        form (assoc events/default-builder :reactive draft :keyframes {:interval {:beats 1}} :blocks {:ignored true})
        compiled (events/builder-definition form 1)]
    (is (= ["head/one"] (:targets compiled)))
    (is (= {:inputs {:level [:source :wash/level]} :transition {:beats (transit/->BeatFraction "1" "3")}}
           (:reactive compiled)))
    (is (= {:op :reactive :key :level} (get-in compiled [:outputs :dimmer])))
    (is (every? #(not (contains? compiled %)) [:envelope :params :keyframes :blocks]))
    (is (nil? (:reactive (events/builder-definition (assoc-in form [:reactive :enabled?] false) 1))))
    (doseq [invalid [(assoc draft :transition "0/3")
                     (assoc-in draft [:targets 0 :fixture-id] nil)
                     (update draft :bindings conj {:id "duplicate" :alias "level" :kind :subscription :address "[:wash/level]"})
                     (assoc-in draft [:bindings 0 :address] ":one :two")
                     (assoc-in (assoc-in draft [:bindings 0 :kind] :subscription) [:bindings 0 :address] "[:valid] [:extra]")]]
      (is (thrown? js/Error (events/builder-definition (assoc form :reactive invalid) 1))))))

(deftest reactive-binding-edits-preserve-expressions-and-use-the-loaded-owner-version
  (let [typed {:id :ember :version 2 :name "Ember" :targets [:head-1]
               :reactive {:inputs {:level [:subscription [:wash/level (transit/->BeatFraction "1" "3")]]}}
               :outputs {:dimmer {:op :multiply :args [0.5 {:op :reactive :key :level}]}}}
        db (assoc base :server-projection {:view {:rig {:fixtures [{:id :head-1}]}}})
        request (transition db [:studio/definition-load "effects" "ember"])
        completion (conj (get-in request [:studio/http :on-success]) {:source (pr-str typed) :definition typed})
        loaded (:db (transition (:db request) completion))
        draft (get-in loaded [:ui :builder :reactive])
        form (assoc-in loaded [:ui :definition :mode] :form)
        unchanged (events/builder-definition (get-in form [:ui :builder]) 2)
        edited (:db (transition form [:studio/builder-reactive #(assoc % :transition "1/2")]))
        newer-owner (assoc-in edited [:show :effects 0 :version] 3)
        sent (transition newer-owner [:studio/definition-publish])
        publication (transition (:db sent) (:dispatch sent))
        accepted {:id "ember" :version 3}
        published (:db (transition (:db publication) (conj (get-in publication [:studio/http :on-success]) accepted)))
        republish (transition published [:studio/definition-publish])
        replaced (assoc-in edited [:show :epoch] 8)
        source-edited (assoc-in edited [:ui :definition :mode] :source)
        source-send (transition source-edited [:studio/definition-publish])
        source-publish (transition (:db source-send) (:dispatch source-send))
        source-published (:db (transition (:db source-publish)
                                         (conj (get-in source-publish [:studio/http :on-success]) accepted)))
        stale-form (assoc-in source-published [:ui :definition :mode] :form)]
    (is (= true (get-in request [:studio/http :raw-response?])))
    (is (empty? (transition (:db (transition (:db request) [:studio/definition-new])) completion)))
    (is (= typed unchanged))
    (is (= "Unpublished changes" (get-in edited [:ui :definition :status])))
    (is (= 2 (get-in publication [:studio/http :body :expected-version])))
    (is (str/includes? (get-in publication [:studio/http :body :source]) ":transition {:beats 1/2}"))
    (is (str/includes? (get-in publication [:studio/http :body :source]) "\n :version 2\n"))
    (is (= (:outputs typed) (:outputs (reader/read-string (get-in publication [:studio/http :body :source])))))
    (is (= 3 (get-in (transition (:db republish) (:dispatch republish)) [:studio/http :body :expected-version])))
    (is (= [:studio/editor-error "The show changed. Reload the definition from the registry."]
           (:dispatch (transition replaced [:studio/definition-publish]))))
    (is (= [:studio/editor-error "The definition changed. Reload it from the registry."]
           (:dispatch (transition stale-form [:studio/definition-publish]))
           (:dispatch (transition stale-form [:studio/builder-generate]))))
    (is (nil? (get-in (:db (transition edited [:studio/definition-new])) [:ui :builder :reactive])))))

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

(deftest neovim-deliveries-of-the-text-studio-holds-keep-the-status
  (let [source "{:id :ember :version 2}"
        request (transition base [:studio/definition-load "effects" "ember"])
        loaded (:db (transition (:db request) (conj (get-in request [:studio/http :on-success]) {:source source})))
        neovim (assoc-in loaded [:ui :definition :engine] :neovim)
        typed (str source " ;x")
        edited (:db (transition neovim [:studio/definition-source typed]))
        ;; Publish inside the change debounce: Neovim is read directly and its delivery comes later.
        sent (transition edited [:studio/definition-publish false])
        read (transition (:db sent) (conj (get-in sent [:studio/nvim-read :on-success]) typed))
        published (:db (transition (:db read) (conj (get-in read [:studio/http :on-success]) {:id "ember" :version 3})))
        rejected (:db (transition (:db read) (conj (get-in read [:studio/http :on-failure]) "EOF while reading")))
        status #(select-keys (get-in % [:ui :definition]) [:status :result :error?])]
    (testing "the echo of the initial load when Neovim is selected"
      (is (= "ember · v2" (get-in loaded [:ui :definition :status])))
      (is (empty? (transition neovim [:studio/definition-source source]))))
    (testing "a delivery of the published text after the publication reply"
      (is (= "Unpublished changes" (get-in edited [:ui :definition :status])))
      (is (= {:status "Published" :error? false} (dissoc (status published) :result)))
      (is (empty? (transition published [:studio/definition-source typed])))
      (is (= {:status "Publication failed" :result "EOF while reading" :error? true} (status rejected)))
      (is (empty? (transition rejected [:studio/definition-source typed]))))
    (testing "a genuinely different delivery still marks the definition unpublished"
      (is (= {:status "Unpublished changes" :result "" :error? false}
             (status (:db (transition published [:studio/definition-source (str typed "y")])))
             (status (:db (transition rejected [:studio/definition-source (str typed "y")]))))))
    (testing "returning to the text the show runs after a failed publication"
      (is (= {:status "ember · v2" :result "" :error? false}
             (status (:db (transition rejected [:studio/definition-source source]))))))))

(deftest a-delivery-during-publication-stays-unpublished
  (let [source "{:id :ember :version 2}"
        request (transition base [:studio/definition-load "effects" "ember"])
        loaded (:db (transition (:db request) (conj (get-in request [:studio/http :on-success]) {:source source})))
        neovim (assoc-in loaded [:ui :definition :engine] :neovim)
        typed (str source " ;x")
        newer (str typed "y")
        sent (transition (:db (transition neovim [:studio/definition-source typed])) [:studio/definition-publish false])
        read (transition (:db sent) (conj (get-in sent [:studio/nvim-read :on-success]) typed))
        ;; Neovim delivers a newer edit while the publication request is in flight.
        in-flight (:db (transition (:db read) [:studio/definition-source newer]))
        reply (conj (get-in read [:studio/http :on-success]) {:id "ember" :version 3})
        published (transition in-flight reply)
        status #(select-keys (get-in % [:ui :definition]) [:status :error? :source])]
    (is (= typed (get-in read [:studio/http :body :source])))
    (is (= {:status "Unpublished changes" :error? false :source newer} (status (:db published))))
    (is (= 3 (get-in published [:db :ui :definition :version])))
    (is (= {:source typed :status "Published"} (get-in published [:db :ui :definition :baseline])))
    (testing "the newer edit is the next publication"
      (let [again (transition (:db published) [:studio/definition-publish false])]
        (is (= newer (-> (transition (:db again) (conj (get-in again [:studio/nvim-read :on-success]) newer))
                         (get-in [:studio/http :body :source]))))))
    (testing "returning to the text that was published reads as published"
      (is (= {:status "Published" :error? false :source typed}
             (status (:db (transition (:db published) [:studio/definition-source typed]))))))
    (testing "without an intervening edit the reply marks the definition published"
      (is (= {:status "Published" :error? false :source typed} (status (:db (transition (:db read) reply))))))))

(deftest status-follows-the-source-against-the-published-baseline
  (let [source "{:id :ember :version 2}"
        request (transition base [:studio/definition-load "effects" "ember"])
        loaded (-> (:db (transition (:db request) (conj (get-in request [:studio/http :on-success]) {:source source})))
                   (assoc-in [:ui :definition :auto?] true))
        edited (transition loaded [:studio/definition-source (str source " ;x")])
        reverted (transition (:db edited) [:studio/definition-source source])
        status #(select-keys (get-in % [:ui :definition]) [:status :result :error?])]
    (is (= "Unpublished changes" (get-in edited [:db :ui :definition :status])))
    (is (= [{:ms 850 :dispatch [:studio/auto-publish 1]}] (:dispatch-later edited)))
    (testing "reverting to the loaded text restores its status and publishes nothing"
      (is (= {:status "ember · v2" :result "" :error? false} (status (:db reverted))))
      (is (nil? (:dispatch-later reverted)))
      (is (empty? (transition (:db reverted) [:studio/auto-publish 2]))))
    (testing "a new definition has no published baseline"
      (let [fresh (:db (transition loaded [:studio/definition-new]))]
        (is (= "Unpublished changes"
               (get-in (transition fresh [:studio/definition-source source]) [:db :ui :definition :status])))))))

(deftest auto-publish-follows-an-edit-made-during-publication
  (let [source "{:id :ember :version 2}"
        typed (str source " ;x")
        newer (str typed "y")
        request (transition base [:studio/definition-load "effects" "ember"])
        loaded (-> (:db (transition (:db request) (conj (get-in request [:studio/http :on-success]) {:source source})))
                   (assoc-in [:ui :definition :auto?] true))
        edited (:db (transition loaded [:studio/definition-source typed]))
        sent (transition edited [:studio/auto-publish 1])
        opened (transition edited (:dispatch sent))
        publish (transition (:db opened) (:dispatch opened))
        in-flight (transition (:db publish) [:studio/definition-source newer])
        ;; The newer edit's own debounce fires while the first request is pending and is dropped.
        dropped (transition (:db in-flight) [:studio/auto-publish 2])
        dropped-publish (transition (:db in-flight) (:dispatch dropped))
        published (transition (:db in-flight) (conj (get-in publish [:studio/http :on-success]) {:id "ember" :version 3}))
        follow-up (some #(when (= :dispatch-later (first %)) (second %)) (:fx published))]
    (is (= [:studio/definition-publish true] (:dispatch sent)))
    (is (empty? dropped-publish))
    (testing "the follow-up waits the same quiet time as an edit, so typing does not chain requests"
      (is (= {:ms 850 :dispatch [:studio/auto-publish 2]} follow-up))
      (is (not-any? #(= :dispatch (first %)) (:fx published))))
    (testing "an edit typed before the follow-up fires supersedes it"
      (let [typing (:db (transition (:db published) [:studio/definition-source (str newer "z")]))]
        (is (empty? (transition typing (:dispatch follow-up))))))
    (is (= [:studio/definition-publish true] (:dispatch (transition (:db published) (:dispatch follow-up)))))
    (testing "a publication that matches the current text queues nothing"
      (let [current (transition (:db publish) (conj (get-in publish [:studio/http :on-success]) {:id "ember" :version 3}))]
        (is (not-any? #(#{:dispatch :dispatch-later} (first %)) (:fx current)))))))

(deftest a-failed-publication-keeps-an-edit-made-during-the-request
  (let [source "{:id :ember :version 2}"
        typed (str source " ;x")
        newer (str typed "y")
        request (transition base [:studio/definition-load "effects" "ember"])
        loaded (-> (:db (transition (:db request) (conj (get-in request [:studio/http :on-success]) {:source source})))
                   (assoc-in [:ui :definition :auto?] true))
        edited (:db (transition loaded [:studio/definition-source typed]))
        opened (transition edited (:dispatch (transition edited [:studio/auto-publish 1])))
        publish (transition (:db opened) (:dispatch opened))
        failure (get-in publish [:studio/http :on-failure])
        in-flight (:db (transition (:db publish) [:studio/definition-source newer]))
        ;; The newer edit's own debounce fires during the request and is dropped.
        dropped (transition in-flight (:dispatch (transition in-flight [:studio/auto-publish 2])))
        refused (transition in-flight (conj failure "EOF while reading"))
        status #(select-keys (get-in % [:ui :definition]) [:status :error? :publishing?])]
    (is (= [:studio/publication-refused (get-in publish [:db :ui :definition :publication]) typed] failure))
    (is (empty? dropped))
    (testing "the newer text was never sent, so it stays unpublished and goes out after the quiet time"
      (is (= {:status "Unpublished changes" :error? true :publishing? false} (status (:db refused))))
      (is (re-find #"EOF while reading.*newer edits are not published" (get-in refused [:db :ui :definition :result])))
      (is (= [{:ms 850 :dispatch [:studio/auto-publish 2]}] (:dispatch-later refused)))
      (is (= [:studio/definition-publish true] (:dispatch (transition (:db refused) [:studio/auto-publish 2])))))
    (testing "an edit typed before the follow-up fires supersedes it"
      (is (empty? (transition (:db (transition (:db refused) [:studio/definition-source (str newer "z")]))
                              [:studio/auto-publish 2]))))
    (testing "without auto-publish the newer text waits for Publish"
      (let [manual (transition (assoc-in in-flight [:ui :definition :auto?] false) (conj failure "EOF while reading"))]
        (is (= "Unpublished changes" (get-in manual [:db :ui :definition :status])))
        (is (nil? (:dispatch-later manual)))))
    (testing "a refusal of the text Studio still holds is a failure, with nothing queued"
      (let [same (transition (:db publish) (conj failure "EOF while reading"))]
        (is (= {:status "Publication failed" :error? true :publishing? false} (status (:db same))))
        (is (= "EOF while reading" (get-in same [:db :ui :definition :result])))
        (is (nil? (:dispatch-later same)))))
    (testing "an edit back to the published baseline during the request reads as that baseline, with no follow-up"
      (let [back (:db (transition (:db publish) [:studio/definition-source source]))
            reply (transition back (conj failure "EOF while reading"))]
        (is (= {:status "ember · v2" :error? false :publishing? false} (status (:db reply))))
        (is (re-find #"EOF while reading.*holds the text the show runs" (get-in reply [:db :ui :definition :result])))
        (is (nil? (:dispatch-later reply)))))
    (testing "a stale failure changes nothing"
      (is (empty? (transition (assoc-in in-flight [:show :epoch] 999) (conj failure "late")))))))

(deftest every-source-change-derives-the-status-from-the-baseline
  (let [source "{:id :ember :version 2}"
        request (transition base [:studio/definition-load "effects" "ember"])
        loaded (:db (transition (:db request) (conj (get-in request [:studio/http :on-success]) {:source source})))
        neovim (assoc-in loaded [:ui :definition :engine] :neovim)
        status #(select-keys (get-in % [:ui :definition]) [:status :engine])]
    (testing "switching to the text editor installs an undelivered Neovim edit as an edit"
      (let [switched (transition neovim [:studio/editor-to-text (str source " ;x")])]
        (is (= {:status "Unpublished changes" :engine :textarea} (status (:db switched))))
        (is (= (str source " ;x") (get-in switched [:db :ui :definition :source])))))
    (testing "switching back with the text Studio holds keeps its status"
      (is (= {:status "ember · v2" :engine :textarea} (status (:db (transition neovim [:studio/editor-to-text source])))))
      (let [edited (:db (transition neovim [:studio/definition-source (str source " ;x")]))]
        (is (= {:status "Unpublished changes" :engine :textarea}
               (status (:db (transition edited [:studio/editor-to-text (str source " ;x")])))))
        (is (= {:status "ember · v2" :engine :textarea}
               (status (:db (transition edited [:studio/editor-to-text source])))))))
    (testing "generated EDN equal to the baseline reads as the baseline, other text as unpublished"
      (let [generated (transition base [:studio/builder-generate])
            text (get-in generated [:db :ui :definition :source])
            ;; The show now runs exactly the text the builder generates.
            request (transition (:db generated) [:studio/definition-load "effects" "after-hours"])
            running (:db (transition (:db request) (conj (get-in request [:studio/http :on-success]) {:source text})))
            again (transition running [:studio/builder-generate])]
        (is (= {:status "Unpublished changes" :kind "effects" :mode :source :result "Generated EDN from the builder."}
               (select-keys (get-in generated [:db :ui :definition]) [:status :kind :mode :result])))
        (is (= text (:studio/nvim-source generated)))
        (is (nil? (:dispatch-later (transition (assoc-in (:db generated) [:ui :definition :auto?] true)
                                               [:studio/builder-generate])))
            "Generate never publishes, even with Publish valid edits on")
        (is (= "after-hours · v1" (get-in running [:ui :definition :status])))
        (is (= text (get-in again [:db :ui :definition :source])))
        (is (= "after-hours · v1" (get-in again [:db :ui :definition :status])))))))

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
        db (:db (full-frame base (assoc old-show :show-id :main)))
        changed (full-frame db (assoc new-show :show-id :main))]
    (is (= "new-process" (get-in changed [:db :show :runtime-id])))
    (is (= 1 (get-in (frame-stream/baseline :clock) [:value :frame-number])) "the new process's lower frame number is accepted")
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

(deftest lane-only-frames-keep-the-app-db-view-token-until-the-view-changes
  (let [view (assoc (:show base) :runtime-id "runtime" :show-id :main :time-ms 0
                    :transport {:bpm 120 :beat 0} :output {:values {:head-1 {:intensity 0.2}}})
        id (projection/identity-of view)
        patch (fn [base-token token changes]
                {:protocol projection/protocol :kind :patch :base-token base-token :token token :identity id :changes changes})
        ;; Unlike stream-event, keep the handler's own effects so a missing :db shows.
        raw (fn [db envelope] (transition db [:studio/stream-frame (:session db) "connected" envelope]))
        lanes (keys frame-stream/lane-paths)
        view-tokens #(set (map (fn [lane] (:view-token (frame-stream/baseline lane))) lanes))
        current-tokens #(set (map (fn [lane] (:token (frame-stream/baseline lane))) lanes))
        t1 (full-frame base view)
        db1 (:db t1)]
    (testing "a full view writes app-db and names itself as every lane's view"
      (is (contains? t1 :db))
      (is (= "full" (get-in db1 [:server-projection :token])))
      (is (= #{"full"} (view-tokens) (current-tokens))))
    (let [t2 (raw db1 (patch "full" "T2" [[:set [:frame-number] 13] [:set [:transport :beat] 1.5]
                                          [:set [:output :values :head-1 :intensity] 0.5]]))]
      (testing "a lane-only patch writes no app-db and keeps the app-db token"
        (is (not (contains? t2 :db)))
        (is (= ["connected" {:op :ack :token "T2"}] (stream-reply t2)))
        (is (= #{"T2"} (current-tokens)))
        (is (= #{"full"} (view-tokens)))
        (is (= 1.5 (get-in (frame-stream/baseline :clock) [:value :beat])))))
    (let [t3 (raw db1 (patch "T2" "T3" [[:set [:frame-number] 14] [:set [:master] 0.4]]))
          db3 (:db t3)]
      (testing "a view-changing patch based on the lane token writes app-db at its own token"
        (is (contains? t3 :db))
        (is (= ["connected" {:op :ack :token "T3"}] (stream-reply t3)))
        (is (= "T3" (get-in db3 [:server-projection :token])))
        (is (= #{"T3"} (view-tokens) (current-tokens)))
        (is (= 0.4 (get-in db3 [:show :master])))
        (is (nil? (get-in db3 [:server-projection :view :transport :beat])) "lane fields stay out of the app-db view")
        (is (nil? (get-in db3 [:show :transport :beat])))
        (is (= 120 (get-in db3 [:show :transport :bpm]))))
      (let [t4 (raw db3 (patch "T3" "T4" [[:set [:frame-number] 15]]))]
        (testing "the next lane-only patch again leaves app-db alone"
          (is (not (contains? t4 :db)))
          (is (= #{"T4"} (current-tokens)))
          (is (= #{"T3"} (view-tokens)))
          (is (= 15 (get-in (frame-stream/baseline :clock) [:value :frame-number])))))
      (testing "a lane holding another view token requests a full view"
        (frame-stream/install! :room (assoc (frame-stream/baseline :room) :view-token "stale"))
        (let [t5 (raw db3 (patch "T4" "T5" [[:set [:frame-number] 16]]))
              db5 (:db t5)]
          (is (contains? t5 :db))
          (is (= ["connected" {:op :resync}] (stream-reply t5)))
          (is (true? (:awaiting-full? db5)))
          (is (= #{"T4"} (current-tokens)) "a refused patch installs no lane")
          (let [t6 (raw db5 {:protocol projection/protocol :kind :full :token "T6" :identity id
                             :view (assoc view :frame-number 17)})]
            (is (contains? t6 :db))
            (is (= ["connected" {:op :ack :token "T6"}] (stream-reply t6)))
            (is (= "T6" (get-in t6 [:db :server-projection :token])))
            (is (= #{"T6"} (view-tokens) (current-tokens)))))))))

(deftest room-values-install-on-their-frame-lane-at-the-acknowledged-token
  (let [view (assoc (:show base) :runtime-id "runtime" :show-id :main :time-ms 500
                    :output {:values {:head-1 {:intensity 0.2}} :atmosphere {:sources []}})
        installs (fn [effects] (some (fn [[id value]] (when (= id :studio/frame-install) value)) (:fx effects)))
        replies (fn [effects] (into [] (comp (map first) (filter #{:studio/frame-install :studio/stream-reply :studio/frame-notify})) (:fx effects)))
        patch (fn [base-token token changes]
                {:protocol projection/protocol :kind :patch :base-token base-token :token token
                 :identity (projection/identity-of view) :changes changes})
        full (full-frame base view)
        moved (stream-event (:db full) (patch "full" "moved" [[:set [:frame-number] 13]
                                                              [:set [:output :values :head-1 :intensity] 0.8]]))]
    (testing "app-db keeps the projection without the lane subtree"
      (is (nil? (get-in full [:db :server-projection :view :output :values])))
      (is (nil? (get-in full [:db :show :output :values])))
      (is (= {:sources []} (get-in full [:db :server-projection :view :output :atmosphere]))))
    (testing "the whole lane frame installs before the ACK and consumers hear of it after"
      (is (= [:studio/frame-install :studio/stream-reply :studio/frame-notify] (replies full)))
      (is (= [:studio/frame-notify [:room :clock :remote]] (last (:fx full))))
      (is (= {:token "full" :identity (projection/identity-of view) :revision 3 :frame-number 12 :time-ms 500
              :master 0.7 :blackout? nil :cut? nil :last-intents nil :atmosphere {:sources []}
              :view-token "full" :value {:values {:head-1 {:intensity 0.2}}}}
             (:room (installs full)))))
    (testing "a patch addresses lane values at the same token"
      (is (= ["connected" {:op :ack :token "moved"}] (stream-reply moved)))
      (is (= {:values {:head-1 {:intensity 0.8}}} (:value (frame-stream/baseline :room))))
      (is (= [13 "moved"] ((juxt :frame-number :token) (frame-stream/baseline :room))))
      (is (nil? (get-in moved [:db :server-projection :view :output :values])))
      (is (= 13 (get-in (frame-stream/baseline :clock) [:value :frame-number])))
      (is (nil? (get-in moved [:db :show :frame-number])) "the frame number lives on the clock lane")
      (is (not (contains? (transition (:db full) [:studio/stream-frame (:session base) "connected"
                                                   (patch "moved" "again" [[:set [:frame-number] 14]])])
                          :db))
          "a frame that changes only lane fields writes nothing to app-db"))
    (testing "a lane that is not at the projection's token requests a full view"
      (frame-stream/install! :room (assoc (frame-stream/baseline :room) :token "stale"))
      (let [refused (stream-event (:db moved) (patch "moved" "after" [[:set [:frame-number] 14]]))
            restored (stream-event (:db refused) {:protocol projection/protocol :kind :full :token "fresh"
                                                  :identity (projection/identity-of view)
                                                  :view (assoc view :frame-number 15)})]
        (is (= ["connected" {:op :resync}] (stream-reply refused)))
        (is (nil? (installs refused)))
        (is (identical? (get-in moved [:db :show]) (get-in refused [:db :show])) "visible state stays until the full view")
        (is (= ["connected" {:op :ack :token "fresh"}] (stream-reply restored)))
        (is (= ["fresh" {:values {:head-1 {:intensity 0.2}}}] ((juxt :token :value) (frame-stream/baseline :room))))))
    (testing "a view without values installs an absent lane frame, not a stale one"
      (let [bare (full-frame base (update view :output dissoc :values))]
        (is (= {} (:value (:room (installs bare)))))
        (is (= {:sources []} (get-in bare [:db :show :output :atmosphere])))))))

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
  (let [settings {:ui {:view "rig" :page-id "second" :launch-mode "held" :knob-skin "ember"
                       :provider-form {:credential "must-not-restore"} :definition {:source "replace"}}}
        restored (events/apply-ui-preferences base settings)
        save (transition restored [:studio/persist-ui])]
    (is (= :rig (get-in restored [:ui :view])))
    (is (= "second" (get-in restored [:ui :page-id])))
    (is (= "held" (get-in restored [:ui :launch-mode])))
    (is (nil? (get-in restored [:ui :provider-form])))
    (is (= events/initial-source (get-in restored [:ui :definition :source])))
    (is (= :ember (get-in restored [:ui :knob-skin])))
    (is (= {:view "rig" :page-id "second" :launch-mode "held" :knob-skin "ember"} (get-in save [:studio/http :body :values])))
    (is (= :perform (get-in (events/apply-ui-preferences base {:ui {:view 42}}) [:ui :view])))
    (testing "Ember is the default style; an unknown saved style restores it, a saved Standard is kept"
      (is (= :ember (get-in (events/apply-ui-preferences base {:ui {:knob-skin "chrome"}}) [:ui :knob-skin])))
      (is (= :standard (get-in (events/apply-ui-preferences base {:ui {:knob-skin "standard"}}) [:ui :knob-skin]))))
    (testing "choosing a knob skin persists it with the other portable preferences"
      (let [chosen (transition restored [:studio/set-knob-skin "standard"])]
        (is (= :standard (get-in chosen [:db :ui :knob-skin])))
        (is (some #{[:dispatch [:studio/persist-ui]]} (:fx chosen)))))))

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
  (let [db (-> (:db (full-frame base (merge (:show base) checkpoint-owner)))
               (assoc-in [:ui :selected-instance] "7:9")
               (assoc-in [:ui :response-form :scope] "instance"))
        response (events/response-definition db)
        start (:studio/http (transition db [:studio/response-start]))]
    (is (= {:instance-ids ["7:9"]} (:select response)))
    (is (= :performance-envelope (:id response)))
    (is (= [:level] (:params response)))
    (is (= :multiply (:operation response)))
    (is (= {:beats 0.5} (get-in response [:envelope :attack])))
    (is (= {:beats 1} (get-in response [:envelope :release])))
    (is (not (contains? (:envelope response) :decay)))
    (is (not (contains? (:envelope response) :sustain)))
    (is (= "/api/response" (:path start)))
    (is (true? (:typed? start)))
    (is (thrown? js/Error (events/response-definition (assoc-in db [:ui :selected-instance] nil))))))

(deftest response-actions-keep-the-types-of-projected-ids
  (let [ids [:live/fade "live/fade"]
        view (assoc (merge (:show base) checkpoint-owner) :responses (mapv #(hash-map :id %) ids))
        db (:db (full-frame base view))]
    (is (= ["live/fade" "live/fade"] (mapv :id (get-in db [:show :responses]))))
    (doseq [id (map :id (get-in db [:server-projection :view :responses]))
            [action path] [[:release "/api/response/release"] [:delete "/api/response/delete"]]]
      (let [request (:studio/http (transition db [:studio/response-action action id]))]
        (is (= path (:path request)))
        (is (true? (:typed? request)))
        (is (= id (get-in request [:body :id])))))))

(deftest response-envelope-submits-decay-and-sustain-through-the-existing-command
  (let [db (update-in base [:ui :response-form] merge {:attack-value 80 :decay-value 120
                                                       :sustain 0 :release-value 250 :unit "ms"})
        submitted (get-in (transition db [:studio/response-start]) [:studio/http :body])]
    (is (nil? (:select submitted)))
    (is (= {:attack {:ms 80} :decay {:ms 120} :sustain 0 :release {:ms 250} :curve :smoothstep}
           (:envelope submitted))))
  (doseq [effect-id [:ember "ember"]]
    (let [view (assoc-in (merge (:show base) checkpoint-owner) [:instances 0 :effect-id] effect-id)
          db (-> (:db (full-frame base view))
                 (assoc-in [:ui :selected-instance] "7:9")
                 (update-in [:ui :response-form] merge {:scope "effect" :decay-value 0.25 :sustain 0.6}))
          submitted (get-in (transition db [:studio/response-start]) [:studio/http :body])]
      (is (= {:effect-ids [effect-id]} (:select submitted)))
      (is (= {:beats 0.25} (get-in submitted [:envelope :decay])))
      (is (= 0.6 (get-in submitted [:envelope :sustain]))))))

(deftest response-envelope-rejects-invalid-decay-and-sustain-before-submitting
  (doseq [[field value] [[:decay-value -0.1] [:decay-value js/Infinity]
                         [:sustain -0.1] [:sustain 1.1] [:sustain js/NaN]]]
    (let [result (transition (assoc-in base [:ui :response-form field] value) [:studio/response-start])]
      (is (= :studio/failure (first (:dispatch result))))
      (is (nil? (:studio/http result))))))

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

(deftest composition-steps-name-empty-and-muted-saved-steps
  (let [cues [{:id "ember" :name "Ember"}]]
    (is (= "Ember" (views/slot-title cues {:cue-id "ember"})))
    (is (= "Ember · muted" (views/slot-title cues {:cue-id "ember" :muted? true})))
    (is (= "Empty step" (views/slot-title cues {:id "rest" :kind "empty"})))
    (is (= "Empty step · muted" (views/slot-title cues {:kind :empty :muted? true})))
    (is (not (views/empty-slot? {:cue-id "ember" :kind "cue"})))
    (is (views/sequence-only-steps? [{:cue-id "ember"} {:kind "empty"}]))
    (is (views/sequence-only-steps? [{:cue-id "ember" :muted? true}]))
    (is (not (views/sequence-only-steps? [{:cue-id "ember" :muted? false}])))))

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

(deftest the-room-light-budget-is-a-bounded-viewer-preference-that-replays-on-mount
  (let [initial (transition base [:studio/preview-ready true])
        chosen (transition base [:studio/preview-setting :light-budget "12"])
        remount (transition (:db chosen) [:studio/preview-ready true])
        quality (transition (:db chosen) [:studio/preview-setting :quality "high"])
        worklight (transition (:db chosen) [:studio/preview-setting :worklight 0.3])]
    (is (= "auto" (get-in base [:ui :preview-settings :light-budget])))
    (is (some #{[:studio/preview [:light-budget "auto"]]} (:fx initial)))
    (is (= [:light-budget "12"] (:studio/preview chosen)))
    (is (= "12" (get-in chosen [:db :ui :preview-settings :light-budget])))
    (testing "this browser keeps quality and light budget, never through the show's settings API"
      (is (= {:quality "auto" :light-budget "12"} (:studio/room-view-store chosen)))
      (is (= {:quality "high" :light-budget "12"} (:studio/room-view-store quality)))
      (is (nil? (:studio/room-view-store worklight)))
      (is (nil? (:studio/http chosen))))
    (is (some #{[:studio/preview [:light-budget "12"]]} (:fx remount)))
    (doseq [invalid ["6" 8 "32" nil "high"]]
      (is (nil? (:db (transition base [:studio/preview-setting :light-budget invalid]))) (pr-str invalid)))
    (testing "a saved choice restores at start; invalid saved values keep the defaults"
      (let [restored (events/restore-room-view base {:quality "desktop" :light-budget "4"})
            junk (events/restore-room-view base {:quality "ultra" :light-budget 99 :worklight 1})]
        (is (= "desktop" (get-in restored [:ui :preview-settings :quality])))
        (is (= "4" (get-in restored [:ui :preview-settings :light-budget])))
        (is (= (get-in base [:ui :preview-settings]) (get-in junk [:ui :preview-settings])))
        (is (= (get-in base [:ui :preview-settings]) (get-in (events/restore-room-view base nil) [:ui :preview-settings])))))))

(deftest the-light-budget-select-names-the-effective-budget-the-engine-reports
  (let [rig (:db (transition base [:studio/preview-lights :rig 4]))
        both (:db (transition rig [:studio/preview-lights :perform 8]))
        closed (:db (transition both [:studio/preview-lights :perform nil]))]
    (is (nil? (events/effective-lights base)))
    (is (= 4 (events/effective-lights rig)))
    (is (= 8 (events/effective-lights both)) "Perform's engine first")
    (is (= 4 (events/effective-lights closed)) "an unmounted engine stops reporting")
    (is (= base (:db (transition base [:studio/preview-lights :perform 99]))) "out of range is not a budget"))
  (is (= "16 (8 now)" (events/light-budget-label "16" true 8)))
  (is (= "16 full lights" (events/light-budget-label "16" true 16)))
  (is (= "16 full lights" (events/light-budget-label "16" false 8)) "only the chosen option")
  (is (= "Auto (4)" (events/light-budget-label "auto" true 4)))
  (is (= "Auto full lights" (events/light-budget-label "auto" true nil)) "before the engine reports"))

(deftest the-room-view-is-kept-by-the-browser-and-handed-to-the-mac-app-per-machine
  (let [stored (atom {}) posted (atom [])
        window #js {:localStorage #js {:getItem #(get @stored %) :setItem #(swap! stored assoc %1 %2)}
                    :webkit #js {:messageHandlers #js {:tolglowStudioWindow #js {:postMessage #(swap! posted conj (js->clj % :keywordize-keys true))}}}}
        previous (.-window js/globalThis)
        saved #(some-> (:studio/room-view-saved ((registrar/get-handler :cofx :studio/room-view-saved) {}))
                       (select-keys [:quality :light-budget]))
        host! (fn [quality stamp] (aset window "tolglowStudioWindow"
                                        #js {:runtimeId "r" :roomView (js/JSON.stringify #js {:quality quality :light-budget "16" :savedAt stamp})}))]
    (try
      (set! (.-window js/globalThis) window)
      ((registrar/get-handler :fx :studio/room-view-store) {:quality "mobile" :light-budget "12"})
      (let [text (get @stored events/room-view-storage-key)
            copy (js->clj (js/JSON.parse text) :keywordize-keys true)]
        (is (= {:quality "mobile" :light-budget "12"} (dissoc copy :savedAt)))
        (is (pos? (:savedAt copy)) "both copies carry the save time")
        (is (= [{:type "room-view" :room-view text}] @posted) "the Mac app keeps the same stamped text"))
      (is (= {:quality "mobile" :light-budget "12"} (saved)) "the browser's copy without a Mac app")
      (testing "a reload in the same launch keeps the page's newer save over the app's copy from when the window opened"
        (host! "high" 1)
        (is (= {:quality "mobile" :light-budget "12"} (saved))))
      (testing "a new launch has empty page storage, so the app's per-machine copy restores"
        (reset! stored {})
        (is (= {:quality "high" :light-budget "16"} (saved))))
      (testing "the app's copy wins when it is the newer one"
        ((registrar/get-handler :fx :studio/room-view-store) {:quality "mobile" :light-budget "4"})
        (host! "desktop" (+ (.now js/Date) 60000))
        (is (= {:quality "desktop" :light-budget "16"} (saved))))
      (testing "unreadable or non-object copies restore nothing"
        (aset window "tolglowStudioWindow" #js {:runtimeId "r" :roomView "[1]"})
        (swap! stored assoc events/room-view-storage-key "5")
        (is (nil? (saved)))
        (is (nil? (events/decode-room-view "{")))
        (is (nil? (events/decode-room-view nil))))
      (finally (if (some? previous) (set! (.-window js/globalThis) previous) (js-delete js/globalThis "window"))))))

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

(deftest cue-macro-picker-saves-typed-projection-identities-and-follows-the-receipt
  (let [view (assoc (:show base) :runtime-id "host" :show-id :main
                    :pages [{:id :main :cells [{:x 1 :y 2 :cue-id :ember}]}
                            {:id "main" :cells [{:x 1 :y 2 :cue-id "ember"}]}]
                    :cues [{:id :ember :name "Keyword Ember"} {:id "ember" :name "Literal Ember"}]
                    :compositions [{:id :stage/night :name "Night set" :mode :macro}
                                   {:id "stage/night" :name "Different macro" :mode :macro}
                                   {:id :steps :mode :sequence}])
        db (:db (full-frame base view))
        cell {:page-id "main" :typed-page-id "main" :x 1 :y 2 :cue-id "ember" :typed-cue-id "ember"}
        open-actions [:studio/open-actions cell]
        actions (:db (transition db open-actions))
        picker (:db (transition actions [:studio/macro-picker-open]))
        canceled (transition picker [:studio/macro-picker-back])
        chosen (assoc-in picker [:ui :macro-picker :macro-id] :stage/night)
        submitted (transition chosen [:studio/macro-picker-submit])
        request (:studio/http submitted)
        in-flight (:db submitted)
        failed (:db (transition in-flight (:on-uncertain request)))
        refused (:db (transition in-flight (conj (:on-failure request) "The cue cell changed. Open its actions again.")))
        after-refusal (-> refused (transition [:studio/close-actions]) :db (transition open-actions) :db
                          (transition [:studio/macro-picker-open]) :db
                          (assoc-in [:ui :macro-picker :macro-id] :stage/night)
                          (transition [:studio/macro-picker-submit]))
        closed (:db (transition failed [:studio/close-actions]))
        reopened (:db (transition (:db (transition closed open-actions)) [:studio/macro-picker-open]))
        retry (transition (assoc-in reopened [:ui :macro-picker :macro-id] :stage/night) [:studio/macro-picker-submit])
        receipt {:status :accepted :macro-id :stage/night :command-id (get-in request [:body :command-id]) :revision 8}
        reply (conj (get-in retry [:studio/http :on-success]) receipt)
        accepted (transition (:db retry) reply)
        pending (transition in-flight (conj (:on-success request) {:status :pending}))
        replaced-cell #(assoc-in % [:server-projection :view :pages 1 :cells 0 :cue-id] :ember)
        stale-open (transition (replaced-cell actions) [:studio/macro-picker-open])
        stale-submit (transition (replaced-cell chosen) [:studio/macro-picker-submit])
        replacement (:db (full-frame in-flight (assoc view :epoch 8)))]
    (is (= [:stage/night "stage/night"] (mapv :id (events/saved-macros picker))))
    (is (= actions (:db canceled)))
    (is (empty? (transition picker [:studio/macro-picker-submit])))
    (is (= "/api/composition/append" (:path request)))
    (is (true? (:typed? request)))
    (is (= "Literal Ember" (get-in picker [:ui :macro-picker :cue-name])))
    (is (= {:macro-id :stage/night :cue-id "ember" :expected-epoch 7 :runtime-id "host"
            :base-revision 3 :cell {:page-id "main" :x 1 :y 2}}
           (dissoc (:body request) :command-id)))
    (is (empty? (transition in-flight [:studio/macro-picker-submit])))
    (is (= (:body request) (get-in retry [:studio/http :body])))
    (is (= "Macro update is still processing." (get-in pending [:db :ui :macro-picker :error])))
    (is (events/macro-picker-retry? (:db pending) {:cue-id "ember" :macro-id :stage/night}))
    (is (nil? (:studio/http stale-submit)))
    (is (= "The cue cell changed. Open its actions again." (get-in stale-submit [:db :ui :macro-picker :error])))
    (is (= "The cue cell changed. Open its actions again." (get-in stale-open [:db :ui :action-cell :error])))
    (is (= [[:dispatch [:studio/toast "Added to Night set."]]] (:fx accepted)))
    (is (nil? (get-in accepted [:db :ui :action-cell])))
    (is (= (get-in db [:ui :composition]) (get-in accepted [:db :ui :composition])))
    (is (= (get-in db [:show :instances]) (get-in accepted [:db :show :instances])))
    (is (= "The show did not reply. Retry append to confirm the same change." (get-in failed [:ui :macro-picker :error])))
    (is (events/macro-picker-retry? failed {:cue-id "ember" :macro-id :stage/night}))
    (is (= "The cue cell changed. Open its actions again." (get-in refused [:ui :macro-picker :error])))
    (is (not (events/macro-picker-retry? refused {:cue-id "ember" :macro-id :stage/night})))
    (is (not= (get-in request [:body :command-id]) (get-in after-refusal [:studio/http :body :command-id]))
        "a refused command is not replayed")
    (is (not (events/macro-picker-retry? (:db accepted) {:cue-id "ember" :macro-id :stage/night})))
    (is (nil? (get-in replacement [:ui :action-cell])))
    (is (empty? (transition replacement (conj (:on-success request) receipt))))))

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

(deftest room-draft-controls-apply-or-discard-the-staged-revision
  (let [view (assoc (:show base) :runtime-id "draft-owner" :show-id :main
                    :scene-draft {:draft {:id "scene" :revision 2 :operations [{:op :cue/launch} {:op :instance/end}]}
                                  :status :auditioning})
        db (:db (full-frame base view))
        applying (transition db [:studio/scene-draft-apply {:bars 1}])
        [_ sent] (first (:fx applying))
        reply (transition (:db applying) (conj (:on-success sent) {:status "scheduled" :due-beat 8}))
        discarding (transition db [:studio/scene-draft-discard])
        [_ dropped] (first (:fx discarding))
        failed (transition (:db discarding) (conj (:on-failure dropped) "Stale draft"))
        idle (:db (full-frame base (:show base)))]
    (is (= {:id "scene" :revision 2 :operations 2 :status :auditioning :due-beat nil :stale nil :missed nil
            :pending? false :connected? true}
           (events/scene-draft-control db)))
    (is (= "scene-draft/apply" (:op sent)))
    (is (= {:draft-id "scene" :expected-draft-revision 2 :grid {:bars 1}} (dissoc (:fields sent) :command-id)))
    (is (= (get-in applying [:db :scene-draft-request :id]) (get-in sent [:fields :command-id])))
    (is (:pending? (events/scene-draft-control (:db applying))))
    (is (= [:studio/toast "Room draft applies at beat 8."] (:dispatch reply)))
    (is (nil? (get-in reply [:db :scene-draft-request])))
    (is (= "scene-draft/discard" (:op dropped)))
    (is (= [:studio/failure "Room draft: Stale draft"] (:dispatch failed)))
    (testing "only a confirmed result is reported as done"
      (let [confirmed (fn [request reply] (:dispatch (transition (:db request) (conj (:on-success (second (first (:fx request)))) reply))))]
        (is (= [:studio/toast "Room draft applied to the show."] (confirmed applying {:status "accepted"})))
        (is (= [:studio/toast "Room draft discarded."] (confirmed discarding {:status :accepted :discarded? true})))
        (is (= [:studio/failure "Room draft: Command remains queued. Check the banner before trying again."]
               (confirmed applying {:status "pending" :message "Command remains queued"})
               (confirmed discarding {:status "pending" :message "Command remains queued"})))))
    (testing "only the Room renders the audition frame"
      (let [live {:head-1 {:emitted-color [1 0 0]}} drafted {:head-1 {:emitted-color [0 0 1]}}
            staged (:db (full-frame base (assoc view :output {:values live}
                                                :draft-output {:values drafted :draft {:id "scene" :revision 2}})))]
        (is (= {:values live :audition drafted} (:value (frame-stream/baseline :room)))
            "live and audition values share one lane frame per token")
        (is (= drafted (interop/room-values :perform (frame-stream/baseline :room))))
        (is (= live (interop/room-values :rig (frame-stream/baseline :room))))
        (is (nil? (get-in staged [:show :output :values])))
        (is (nil? (get-in staged [:show :draft-output :values])))
        (is (= {:id "scene" :revision 2} (get-in staged [:server-projection :view :draft-output :draft])))
        (full-frame base (assoc view :output {:values live}))
        (is (= live (interop/room-values :perform (frame-stream/baseline :room))) "without a draft the Room shows live values")))
    (is (nil? (events/scene-draft-control idle)))
    (is (empty? (transition idle [:studio/scene-draft-discard])))))

(deftest room-draft-banner-reports-what-the-repl-reports
  (let [view (assoc (:show base) :runtime-id "draft-owner" :show-id :main)
        draft {:id "scene" :revision 2 :operations [{:op :cue/launch}]}
        control (fn [scene-draft] (events/scene-draft-control (:db (full-frame base (assoc view :scene-draft scene-draft)))))
        note (comp events/scene-draft-note control)
        missed (fn [code] {:status :missed :draft-id "scene" :draft-revision 2 :due-beat 8 :code code})]
    (is (= "" (note {:draft draft :status :auditioning})))
    (is (= " · applies at beat 8" (note {:draft draft :status :scheduled :pending-apply {:due-beat 8}})))
    (is (= " · audition failed" (note {:draft draft :status :error :error {:message "boom"}})))
    (testing "a stale draft names the dependency that changed"
      (is (= :stale-registry (:stale (control {:draft draft :status :stale :stale :stale-registry}))))
      (is (= " · stale: the rig changed. Stage it again" (note {:draft draft :status :stale :stale :stale-patch})))
      (is (= " · stale: a cue or effect changed. Stage it again"
             (note {:draft draft :status "stale" :stale "stale-registry"})))
      (is (= " · stale: the material library changed. Stage it again"
             (note {:draft draft :status :stale :stale :stale-prefabs}))))
    (testing "a scheduled apply that missed its boundary stays visible until the next attempt"
      (is (= {:due-beat 8 :code :clock-changed}
             (:missed (control {:draft draft :status :auditioning :last-outcome (missed :clock-changed)}))))
      (is (= " · the apply at beat 8 was missed: the clock source changed"
             (note {:draft draft :status :auditioning :last-outcome (missed :clock-changed)})))
      (is (= " · the apply at beat 8 was missed: the beat moved back"
             (note {:draft draft :status :auditioning :last-outcome (missed "clock-moved-back")})))
      (is (= " · stale: a cue or effect changed. Stage it again · the apply at beat 8 was missed: a cue or effect changed"
             (note {:draft draft :status :stale :stale :stale-registry :last-outcome (missed :stale-registry)})))
      (is (= " · the apply at beat 8 was missed: stale-instance"
             (note {:draft draft :status :auditioning :last-outcome (missed :stale-instance)})))
      (is (= " · applies at beat 12"
             (note {:draft draft :status :scheduled :pending-apply {:due-beat 12} :last-outcome (missed :clock-changed)}))))
    (testing "an outcome of another draft or revision is not this draft's"
      (is (nil? (:missed (control {:draft draft :status :auditioning
                                   :last-outcome (assoc (missed :clock-changed) :draft-revision 1)}))))
      (is (nil? (:missed (control {:draft draft :status :auditioning
                                   :last-outcome {:status :applied :draft-id "scene" :draft-revision 2}})))))))

(deftest the-capture-button-posts-capture-and-shows-the-server-summary
  (let [requested (transition base [:connections/capture])
        [effect request] (first (:fx requested))
        token (connections/token (:db requested))
        summary {:status "captured" :capture-id 2 :bars 4 :notes 3 :cues ["wash" "aim"] :lane-points 1}
        done (transition (:db requested) [:connections/captured token :capture summary])
        stale (transition (:db requested) [:connections/captured [:other] :capture summary])
        markup (fn [db] (renderToStaticMarkup (r/as-element [connections/connection-view db (connections/state db)])))]
    (is (= :studio/http effect))
    (is (= ["/api/capture/capture" {}] [(:path request) (:body request)]))
    (is (contains? (get-in requested [:db :connections :pending]) :capture))
    (is (= summary (get-in done [:db :connections :capture])))
    (is (not (contains? (get-in done [:db :connections :pending]) :capture)))
    (is (nil? stale) "a reply for another show session is ignored")
    (is (= "Capture 2 · 4 bars · 3 activations of wash, aim · 1 parameter move" (connections/capture-text summary)))
    (is (= "Nothing ran in the captured bars" (connections/capture-text {:status "empty" :message "Nothing ran in the captured bars"})))
    (is (str/includes? (markup (:db done)) "Download MIDI clip"))
    (is (str/includes? (markup (:db done)) "/api/capture/last.mid"))
    (is (not (str/includes? (markup base) "Download MIDI clip")))))

(deftest the-live-runs-panel-reads-the-run-record-projection
  (let [requested (transition base [:connections/live-runs-refresh])
        [effect request] (first (:fx requested))
        token (connections/token (:db requested))
        projection {:takes 3 :running 1
                    :channels [{:cue-id "a" :name "Amber sweep" :group-path ["stage" "warm"]
                                :lanes [{:lane 0 :track-name "Tolglow run a"
                                         :takes [{:take-id "r.1" :run 1 :start-beat 1 :length-beats 3 :phase "ended" :clip {:status "in-live"}}
                                                 {:take-id "r.3" :run 2 :start-beat 6 :phase "running" :clip {:status "recording"}}]}]}
                               {:cue-id "b" :name "Blue wash" :group-path ["stage"]
                                :lanes [{:lane 0 :track-name "Tolglow run b"
                                         :takes [{:take-id "r.2" :run 1 :start-beat 2 :length-beats 5 :phase "ended" :clip {:status "not-in-live"}}]}]}]
                    :mirror {:complete? false :pending 1 :problems [{:code "not-in-group" :name "Tolglow run b"}]}}
        done (transition (:db requested) [:connections/live-runs-loaded token :live-runs projection])
        stale (transition (:db requested) [:connections/live-runs-loaded [:other] :live-runs projection])
        html (renderToStaticMarkup (r/as-element [connections/connection-view (:db done) (connections/state (:db done))]))]
    (is (= :studio/http effect))
    (is (= ["/api/live-runs" nil] [(:path request) (:body request)]))
    (is (= projection (get-in done [:db :connections :live-runs])))
    (is (not (contains? (get-in done [:db :connections :pending]) :live-runs)))
    (is (nil? stale) "a reply for another show session is ignored")
    (is (= "2 launched cues · 3 takes (1 running) · 1 of 3 in Live" (connections/live-runs-text projection)))
    (is (= "Nothing has run yet" (connections/live-runs-text {:takes 0 :channels []})))
    (is (= "2 launched cues · 300 takes · 1 of 3 in Live · newest 3 shown"
           (connections/live-runs-text (assoc projection :takes 300 :running 0 :truncated? true :shown 3))))
    (is (= "2 launched cues · 3 takes (1 running) · 1 of 3 in Live · 44 playback events dropped"
           (connections/live-runs-text (assoc projection :playback-events {:queued 0 :dropped 44}))))
    (is (= "run 1 · beat 1.00 · held 3.00 · in Live" (connections/take-text (get-in projection [:channels 0 :lanes 0 :takes 0]))))
    (is (= "run 2 · beat 6.00 · running · recording" (connections/take-text (get-in projection [:channels 0 :lanes 0 :takes 1]))))
    (is (str/includes? html "Amber sweep"))
    (is (str/includes? html "in stage / warm"))
    (is (str/includes? html "Tolglow run b"))
    (is (str/includes? html "not-in-group · Tolglow run b"))
    (is (not (str/includes? (renderToStaticMarkup (r/as-element [connections/connection-view base (connections/state base)])) "Amber sweep")))))

(deftest the-live-runs-panel-shows-parameter-devices-and-syncs-through-the-bridge
  ;; #625: the projection show/live-parameter-devices returns, as Ring serves it.
  (let [requested (transition base [:connections/parameter-devices-refresh])
        [effect request] (first (:fx requested))
        token (connections/token (:db requested))
        projection {:channels [{:cue-id "a" :effect-id "beam" :devices 1
                                :slots [{:slot 0 :name "Level" :type "float" :min 0 :max 1 :places 5}
                                        {:slot 1 :name "period" :type "float" :min 0.5 :max 32 :places 3 :unit "beats"}
                                        {:slot 2 :name "mode" :type "enum" :min 0 :max 2 :choices ["soft" "hard" "strobe"]}
                                        {:slot 3 :name "Unused 4" :retired? true}]
                                :banks [[[0 1 2]]] :unsupported [{:param "fade" :reason "gradient"}]
                                :tracks [{:lane 0 :track-name "Tolglow run a" :present [0] :missing [] :stale []}
                                         {:lane 1 :track-name "Tolglow run a · 2" :present [] :missing [0] :stale []}]}]
                    :counts {:applied 3 :echo 1 :held 2} :sends {:executed 4 :failed 0}}
        done (transition (:db requested) [:connections/parameter-devices-loaded token :parameter-devices projection])
        html (renderToStaticMarkup (r/as-element [connections/connection-view (:db done) (connections/state (:db done))]))
        sync (transition (:db done) [:connections/live-runs-sync])
        [sync-effect sync-request] (first (:fx sync))]
    (is (= :studio/http effect))
    (is (= ["/api/live-parameter-devices" nil] [(:path request) (:body request)]))
    (is (= projection (get-in done [:db :connections :parameter-devices])))
    (is (= "a · 3 slots on 1 device · 1 not in Live · Tolglow run a: ready, Tolglow run a · 2: drop 1 Tolglow parameters device here"
           (connections/parameter-devices-text (first (:channels projection)))))
    (is (= ["Level · 0.00000 – 1.00000" "period · 0.500 – 32.000 beats" "mode · soft / hard / strobe" "Unused 4 (unused)"]
           (mapv connections/slot-text (get-in projection [:channels 0 :slots])))
        "Studio's decimals and units")
    (is (str/includes? html "Values from Live: 3 applied · 1 echoes dropped · 2 held by Tolglow · Tolglow writes sent: 4"))
    (is (str/includes? html "Banks: 1 2 3"))
    (is (= :studio/http sync-effect))
    (is (= ["/api/live-runs/sync" {}] [(:path sync-request) (:body sync-request)]))
    (is (contains? (get-in sync [:db :connections :pending]) :live-runs-sync))))

(deftest the-cue-launcher-panel-reads-the-launcher-projection
  (let [db (assoc-in base [:ui :page-id] "pads")
        requested (transition db [:connections/launcher-refresh])
        [effect request] (first (:fx requested))
        token (connections/token (:db requested))
        pad (fn [x y note & [cue label color]] (cond-> {:cell [x y] :note note} cue (assoc :cue-id cue :label label :live-color color)))
        pads (vec (for [y (range 8) x (range 8)]
                    (case [x y]
                      [0 7] (pad 0 7 36 "red" "Red" 0xff0000)
                      [4 7] (pad 4 7 68 "blue" "Blue" 0x0000ff)
                      (pad x y (+ 100 x (* 8 y))))))
        projection {:page-id "pads" :layout "push-64-pads" :scroll-row 9 :pads pads :elsewhere [] :unmapped [] :off-grid []
                    :setup {:ready? false :name "Tolglow launcher pads"
                            :problems [{:code "empty-pad" :note 68 :expected "Blue"}]}}
        done (transition (:db requested) [:connections/launcher-loaded token :launcher projection])
        stale (transition (:db requested) [:connections/launcher-loaded [:other] :launcher projection])
        html (renderToStaticMarkup (r/as-element [connections/connection-view (:db done) (connections/state (:db done))]))]
    (is (= :studio/http effect))
    (is (= ["/api/live-launcher?page-id=pads" nil] [(:path request) (:body request)]) "the page selected in Studio")
    (is (= projection (get-in done [:db :connections :launcher])))
    (is (nil? stale) "a reply for another show session is ignored")
    (is (= "Push 64 Pads · 2 of 64 pads play cues · 1 setup problem in Live" (connections/launcher-text projection)))
    (is (= "Rows of eight · 2 of 64 pads play cues · 1 cue is not under its pad · set up in Live as Tolglow launcher pads · last pad clip 4 hits"
           (connections/launcher-text (assoc projection :layout "rows" :elsewhere [{:cue-id "amber"}]
                                             :setup {:ready? true :name "Tolglow launcher pads" :problems []}
                                             :clip {:status "executed" :notes 4}))))
    (is (= "Push 64 Pads · 2 of 64 pads play cues · not set up in Live" (connections/launcher-text (dissoc projection :setup))))
    (testing "the grid is eight rows of eight with the bottom row last, as Push shows it"
      (let [rows (connections/pad-rows (shuffle pads))]
        (is (= 8 (count rows)))
        (is (every? #(= 8 (count %)) rows))
        (is (= (mapv #(vector % 7) (range 8)) (mapv :cell (last rows))))
        (is (= [36 68] (keep #(when (:cue-id %) (:note %)) (last rows))))))
    (is (str/includes? html "Red"))
    (is (str/includes? html "empty-pad · note 68 · Blue"))
    (is (= ["empty-pad · notes 64, 68" "no-drum-rack" "scroll-row · 9"]
           (connections/launcher-problems [{:code "empty-pad" :note 64 :expected "Amber"} {:code "scroll-row" :expected 9}
                                           {:code "no-drum-rack"} {:code "empty-pad" :note 68 :expected "Blue"}]))
        "pad problems of one kind share a line")
    (is (str/includes? html "Push 64 Pads · cells match Push"))
    (is (= "push-64-pads" (get-in (connections/sync-payload base) [:live :pad-layout])) "Studio always saves an explicit layout")
    (testing "Set up in Live and Write pad clip run the host's launcher operations, then show the projection again"
      (let [setup (transition db [:connections/launcher-action :setup])
            [_ setup-request] (first (:fx setup))
            write (transition db [:connections/launcher-action :write-clip])
            [_ write-request] (first (:fx write))
            acted (transition (:db setup) [:connections/launcher-acted (connections/token (:db setup)) :launcher {:ready? true}])]
        (is (= ["/api/live-launcher/setup" {:page-id "pads"}] [(:path setup-request) (:body setup-request)]))
        (is (= ["/api/live-launcher/write-clip" {:page-id "pads"}] [(:path write-request) (:body write-request)]))
        (is (= [:connections/launcher-refresh] (:dispatch acted)))
        (is (= 32000 (:timeout-ms setup-request)) "outlasts three bridge operations at the default 2 s timeout")
        (is (= 20000 (connections/launcher-timeout-ms {:sync {:config {:live {:timeout-ms 1000}}}})))
        (let [failed (transition (:db setup) [:connections/launcher-failed (connections/token (:db setup)) :launcher "timeout"])]
          (is (= [:connections/launcher-refresh] (:dispatch failed)) "after a failure Studio shows what the host did")
          (is (= "timeout" (get-in failed [:db :connections :errors :launcher]))))
        (is (not (contains? (get-in acted [:db :connections :pending]) :launcher)))))
    (is (str/includes? (renderToStaticMarkup (r/as-element [connections/connection-view (:db done) (connections/state (:db done))]))
                       "Set up in Live"))))

(deftest an-edit-after-new-is-what-publishes
  ;; Studio dispatches New synchronously, so an edit typed straight after it lands on the
  ;; new source rather than being replaced by it.
  (let [source "{:id :ember :version 2}"
        request (transition base [:studio/definition-load "effects" "ember"])
        loaded (:db (transition (:db request) (conj (get-in request [:studio/http :on-success]) {:source source})))
        fresh (:db (transition loaded [:studio/definition-new]))
        edited (:db (transition fresh [:studio/definition-source "{:id"]))
        sent (transition edited [:studio/definition-publish false])
        publish (transition (:db sent) (:dispatch sent))]
    (is (nil? (get-in fresh [:ui :definition :baseline])))
    (is (= "{:id" (get-in publish [:studio/http :body :source])))
    (is (nil? (get-in publish [:studio/http :body :expected-version])))))
