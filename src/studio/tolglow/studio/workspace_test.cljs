(ns tolglow.studio.workspace-test
  (:require [cljs.test :refer-macros [deftest is]]
            [clojure.string :as str]
            [re-frame.db :as rf-db]
            [re-frame.interceptor :as interceptor]
            [re-frame.registrar :as registrar]
            [tolglow.studio.events :as events]
            [tolglow.studio.workspace :as workspace]))

(def base
  (-> (events/initial-db "workspace-session")
      (assoc :show {:runtime-id "runtime-a" :epoch 4})
      (assoc-in [:ui :workspace] (assoc workspace/defaults :selected-id "lighting" :open? true))))

(def active-key ["lighting" "tolglow" "tolglow/scene.clj"])

(def file {:path "tolglow/scene.clj" :repo "tolglow" :hash "hash-a" :source "original" :baseline "original"
           :content "original" :binary? false :buffer-version 1})

(def editing (-> base (assoc-in [:ui :workspace :active-file] active-key)
                 (assoc-in [:ui :workspace :files active-key] file)))

(defn transition [db event]
  (reset! rf-db/app-db db)
  (:effects (interceptor/execute event (remove #(= :do-fx (:id %))
                                               (registrar/get-handler :event (first event))))))

(defn success [result data]
  (transition (:db result) (conj (get-in result [:studio/http :on-success]) data)))

(deftest paired-workspace-creation-uses-explicit-refs-and-no-check-or-deploy
  (let [db (assoc-in base [:ui :workspace :create] {:id "scene-dev" :tolglow "main" :afterglow "functional"})
        result (transition db [:workspace/create])]
    (is (= "/api/workspaces/create" (get-in result [:studio/http :path])))
    (is (= {:id "scene-dev" :base-refs {:tolglow "main" :afterglow "functional"}
            :runtime-id "runtime-a" :expected-epoch 4} (get-in result [:studio/http :body])))
    (is (nil? (:dispatch result)))
    (is (nil? (:fx result))))
  (let [result (transition (assoc-in base [:ui :workspace :create :id] "../bad") [:workspace/create])]
    (is (nil? (:studio/http result)))
    (is (string? (get-in result [:db :ui :workspace :errors :create])))))

(deftest restored-develop-view-waits-for-initial-runtime-before-starting-list-request
  (let [before (transition (assoc base :show nil) [:workspace/list])
        after (transition base [:workspace/list])
        closed (transition (assoc-in base [:ui :workspace :open?] false) [:workspace/list])]
    (is (nil? (:studio/http before)))
    (is (nil? (:dispatch-later before)))
    (is (= "/api/workspaces" (get-in after [:studio/http :path])))
    (is (empty? closed))))

(deftest file-paths-and-real-editor-types-are-explicit
  (is (= "/api/workspaces/file?id=lighting&repo=tolglow&path=src%2Fa%20b.cljs"
         (workspace/api-url :file (array-map :id "lighting" :repo "tolglow" :path "src/a b.cljs"))))
  (doseq [[path type] [["a.cljs" "clojure"] ["a.edn" "clojure"] ["a.js" "javascript"] ["a.css" "css"]
                       ["a.json" "json"] ["README.md" "markdown"] ["plain" "text"]]]
    (is (= type (workspace/file-type path))))
  (is (= "src/new.clj" (workspace/relative-path " src/new.clj ")))
  (doseq [path ["../bad" ".git/config" "/tmp/a" "src//a" "src\\a" ""]]
    (is (thrown? js/Error (workspace/relative-path path)))))

(deftest tree-paging-and-file-selection-retain-their-workspace-identities
  (let [first-page (success (transition base [:workspace/tree 0]) {:entries [{:path "a.clj" :editable? true}] :total 2 :next-offset 1})
        next-page (success (transition (:db first-page) [:workspace/tree 1]) {:entries [{:path "b.clj" :editable? true}] :total 2 :next-offset nil})
        opened (transition (:db next-page) [:workspace/open-file "a.clj"])
        read (success opened {:path "a.clj" :repo "tolglow" :hash "a" :content "(ns a)" :binary? false})]
    (is (= ["a.clj" "b.clj"] (mapv :path (get-in next-page [:db :ui :workspace :trees ["lighting" "tolglow"] :entries]))))
    (is (= ["lighting" "tolglow" "a.clj"] (get-in read [:db :ui :workspace :active-file])))
    (is (= "(ns a)" (get-in read [:db :ui :workspace :files ["lighting" "tolglow" "a.clj"] :source])))
    (is (= "a" (get-in read [:db :ui :workspace :files ["lighting" "tolglow" "a.clj"] :hash])))))

(deftest saves-use-the-loaded-hash-and-preserve-edits-made-during-the-request
  (let [sent (transition editing [:workspace/save-buffer (workspace/token editing) active-key "first edit"])
        newer (transition (:db sent) [:workspace/edit (workspace/token editing) active-key "second edit"])
        ack (transition (:db newer) (conj (get-in sent [:studio/http :on-success]) {:hash "hash-b" :size 10 :revision 1}))
        saved (get-in ack [:db :ui :workspace :files active-key])]
    (is (= "/api/workspaces/write" (get-in sent [:studio/http :path])))
    (is (= "hash-a" (get-in sent [:studio/http :body :expected-hash])))
    (is (= "first edit" (get-in sent [:studio/http :body :content])))
    (is (= "second edit" (:source saved)))
    (is (= "first edit" (:baseline saved)))
    (is (= "hash-b" (:hash saved)))
    (is (workspace/dirty? saved))))

(deftest real-neovim-buffer-is-read-before-save-switch-and-navigation
  (let [db (-> editing (assoc-in [:ui :workspace :editor] :neovim)
               (assoc-in [:ui :workspace :files active-key :editor-ready?] true))
        save (transition db [:workspace/save])
        navigate (transition db [:workspace/navigate [:workspace/repository "afterglow"]])
        read (transition db [:workspace/editor-read (workspace/token db) active-key :save "actual engine text" 1])]
    (is (nil? (:studio/http save)))
    (is (= :save (get-in save [:workspace/read-editor :action])))
    (is (= active-key (get-in save [:workspace/read-editor :key])))
    (is (= [:workspace/repository "afterglow"] (get-in navigate [:workspace/read-editor :action])))
    (is (= "actual engine text" (get-in read [:db :ui :workspace :files active-key :source])))
    (is (= [:workspace/save-buffer (workspace/token db) active-key "actual engine text"] (:dispatch read)))))

(deftest failed-neovim-can-return-to-accessible-editor-without-a-stuck-read
  (let [db (-> editing (assoc-in [:ui :workspace :editor] :neovim)
               (assoc-in [:ui :workspace :files active-key :editor-ready?] true))
        failed (transition db [:workspace/editor-error (workspace/token db) active-key "Worker exited" 1])
        switch (transition (:db failed) [:workspace/navigate [:workspace/editor :text]])]
    (is (false? (get-in failed [:db :ui :workspace :files active-key :editor-ready?])))
    (is (= [:workspace/editor :text] (:dispatch switch)))
    (is (nil? (:workspace/read-editor switch)))))

(deftest reloading-a-dirty-file-opens-review-and-never-overwrites-the-buffer
  (let [db (assoc-in editing [:ui :workspace :files active-key :source] "my edits")
        read (success (transition db [:workspace/reload-file]) {:content "changed on disk" :hash "hash-disk" :binary? false})
        draft (get-in read [:db :ui :workspace :files active-key])
        base (transition (:db read) [:workspace/resolve-disk active-key :baseline])
        replace (transition (:db read) [:workspace/resolve-disk active-key :replace])
        stale (transition (:db replace) [:workspace/edit (workspace/token db) active-key "old Neovim text" 1])]
    (is (= "my edits" (:source draft)))
    (is (= "hash-a" (:hash draft)))
    (is (= "changed on disk" (get-in draft [:disk :content])))
    (is (= "my edits" (get-in base [:db :ui :workspace :files active-key :source])))
    (is (= "hash-disk" (get-in base [:db :ui :workspace :files active-key :hash])))
    (is (= "changed on disk" (get-in replace [:db :ui :workspace :files active-key :source])))
    (is (empty? stale))))

(deftest a-save-ack-cannot-regress-a-newer-disk-baseline
  (let [sent (transition editing [:workspace/save-buffer (workspace/token editing) active-key "sent"])
        reread (-> (:db sent) (assoc-in [:ui :workspace :files active-key :hash] "newer-disk-hash")
                   (assoc-in [:ui :workspace :files active-key :baseline] "newer disk contents"))
        result (transition reread (conj (get-in sent [:studio/http :on-success]) {:hash "saved-hash" :revision 1}))]
    (is (= "newer-disk-hash" (get-in result [:db :ui :workspace :files active-key :hash])))
    (is (= "newer disk contents" (get-in result [:db :ui :workspace :files active-key :baseline])))))

(deftest fixed-checks-and-candidate-staging-do-not-pretend-to-start-a-server
  (let [check (transition base [:workspace/check "tolglow-unit"])
        stage (transition base [:workspace/stage])
        result (success stage {:status "ready" :required-checks ["tolglow-unit" "afterglow-unit"]})]
    (is (= "/api/workspaces/check" (get-in check [:studio/http :path])))
    (is (= "tolglow-unit" (get-in check [:studio/http :body :profile])))
    (is (nil? (get-in check [:studio/http :body :command])))
    (is (nil? (get-in stage [:studio/http :body :required-checks])))
    (is (= "ready" (get-in result [:db :ui :workspace :statuses "lighting" :preview :status])))
    (is (nil? (get-in result [:db :ui :workspace :statuses "lighting" :preview :url])))))

(deftest selection-uses-the-shared-status-subscription
  (let [local {:selected-id "lighting" :statuses {"lighting" {:state :starting}}}
        ready {:id "lighting" :state :ready :running-checks {}}
        selected (transition base [:workspace/select "lighting"])]
    (is (= ready (get-in (workspace/with-status local {:value ready}) [:statuses "lighting"])))
    (is (= local (workspace/with-status local nil)))
    (is (= local (workspace/with-status local {:value (assoc ready :id "other")})))
    (is (nil? (get-in (workspace/with-status local {:value nil}) [:statuses "lighting"])))
    (is (string? (get-in (workspace/with-status local {:error :unavailable}) [:errors [:status "lighting"]])))
    (is (= [:workspace/tree 0] (:dispatch selected)))
    (is (nil? (:dispatch-later selected)))
    (is (nil? (:studio/http selected)))))

(deftest runtime-replacement-invalidates-results-but-keeps-unsaved-local-text
  (let [db (assoc-in editing [:ui :workspace :files active-key :source] "keep my draft")
        sent (transition db [:workspace/reload-file])
        changed (transition (:db sent) [:workspace/runtime-change])
        stale (transition (:db changed) (conj (get-in sent [:studio/http :on-success]) {:content "stale" :hash "stale"}))]
    (is (empty? stale))
    (is (= "keep my draft" (get-in changed [:db :ui :workspace :files active-key :source])))))

(deftest diff-summary-reports-actual-tracked-lines-and-untracked-paths
  (is (= {:files 2 :untracked 1 :added 1 :removed 1 :paths [" M src/a.clj" "?? src/new.clj"]}
         (workspace/change-summary {:status " M src/a.clj\n?? src/new.clj\n"
                                    :diff "--- a/src/a.clj\n+++ b/src/a.clj\n@@ -1 +1 @@\n-old\n+new\n"})))
  (is (= {:added 1 :removed 1}
         (select-keys (workspace/change-summary {:diff "diff --git a/a b/a\n--- a/a\n+++ b/a\n@@ -1 +1 @@\n--- old content\n+++ new content\n"}) [:added :removed])))
  (is (= "unit" (:id (first (workspace/profiles {:unit {:repo :tolglow}}))))))

(deftest leaving-an-unmounted-neovim-pane-does-not-read-a-disposed-buffer
  (let [nvim (-> editing
                 (assoc-in [:ui :workspace :editor] :neovim)
                 (assoc-in [:ui :workspace :files active-key :editor-ready?] true))
        action [:workspace/pane :agent]]
    (is (:workspace/read-editor (transition nvim [:workspace/navigate action])))
    (doseq [previous [:agent :diff :checks :terminal]]
      (let [hidden (assoc-in nvim [:ui :workspace :pane] previous)]
        (is (= {:dispatch [:workspace/pane :files]}
               (transition hidden [:workspace/navigate [:workspace/pane :files]])))))))
