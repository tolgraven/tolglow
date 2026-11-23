(ns tolglow.studio.workspace
  "Paired application workspaces. File drafts live in app-db; Git, checks and
  writes cross the host API, and actual Neovim buffers have owned lifecycles."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.events :as events]
            [tolglow.studio.application-agents :as application-agents]
            [tolglow.studio.terminal :as terminal]))

(def defaults
  {:open? false :generation 0 :serial 0 :pending {} :errors {} :workspaces [] :statuses {}
   :profiles [] :runner-ready? false :selected-id nil :repo "tolglow" :trees {} :files {}
   :active-file nil :editor :text :pane :files :filter "" :new-path "" :diffs {}
   :create {:id "" :tolglow "HEAD" :afterglow "HEAD"} :creating? false :notice nil})
(defonce resources (atom {}))
(defn state [db] (merge defaults (get-in db [:ui :workspace])))
(defn- store [db s] (assoc-in db [:ui :workspace] s))
(defn- wire [x] (events/wire-id x))
(defn token [db] [(get-in db [:show :runtime-id]) (get-in db [:show :epoch]) (:generation (state db))])
(defn- current? [db t] (= t (token db)))
(defn dirty? [file] (and file (not= (:source file) (:baseline file))))
(defn- encode [x] (js/encodeURIComponent (str x)))
(defn api-url [operation params]
  (str "/api/workspaces" (when operation (str "/" (name operation)))
       (when (seq params) (str "?" (str/join "&" (map (fn [[k v]] (str (encode (name k)) "=" (encode v))) params))))))
(defn file-key [id repo path] [(wire id) (wire repo) path])
(defn file-type [path]
  (case (str/lower-case (last (str/split path #"\.")))
    ("clj" "cljs" "cljc" "edn") "clojure" ("js" "mjs" "cjs" "jsx") "javascript"
    ("ts" "tsx") "typescript" "css" "css" "html" "html" "json" "json"
    ("md" "markdown") "markdown" "py" "python" ("sh" "bash") "sh"
    ("yaml" "yml") "yaml" "xml" "xml" "toml" "toml" "text"))
(defn relative-path [raw]
  (let [path (str/trim raw) parts (str/split path #"/" -1)]
    (when (or (str/blank? path) (str/starts-with? path "/") (re-find #"[\\\x00-\x1f]" path)
              (some #(or (#{"" "." ".."} %) (= ".git" (str/lower-case %))) parts))
      (throw (js/Error. "Use a relative file path inside an existing repository directory."))) path))
(defn- valid-source [source]
  (when-not (and (string? source) (<= (.-length (.encode (js/TextEncoder.) source)) 2097152))
    (throw (js/Error. "Text files are limited to 2 MiB."))) source)
(defn profiles [value]
  (if (map? value) (mapv (fn [[id profile]] (assoc profile :id (wire id))) value)
      (mapv #(update % :id wire) value)))
(defn change-summary [repo]
  (let [paths (vec (remove str/blank? (str/split-lines (:status repo ""))))
        counts (reduce (fn [result line]
                         (cond
                           (str/starts-with? line "diff --git ") (assoc result :hunk? false)
                           (str/starts-with? line "@@") (assoc result :hunk? true)
                           (and (:hunk? result) (str/starts-with? line "+")) (update result :added inc)
                           (and (:hunk? result) (str/starts-with? line "-")) (update result :removed inc)
                           :else result)) {:hunk? false :added 0 :removed 0} (str/split-lines (:diff repo "")))]
    {:paths paths :files (count paths) :untracked (count (filter #(str/starts-with? % "??") paths))
     :added (:added counts) :removed (:removed counts)}))
(defn- request [db key operation params body context]
  (let [s (state db) serial (inc (:serial s)) t (token db)]
    {:db (store db (-> s (assoc :serial serial) (assoc-in [:pending key] serial) (update :errors dissoc key)))
     :studio/http {:path (api-url operation params)
                   :body (when body (merge {:runtime-id (first t) :expected-epoch (second t)} body))
                   :timeout-ms (if (= operation :create) 65000 15000)
                   :on-success [:workspace/result t key serial operation context]
                   :on-failure [:workspace/failure t key serial]}}))
(defn- fail [db key error] {:db (store db (assoc-in (state db) [:errors key] (events/error-message error)))})
(defn- accept-file [s key data reload?]
  (let [old (get-in s [:files key])]
    (if (and reload? (dirty? old))
      (assoc-in s [:files key :disk] data)
      (assoc-in s [:files key]
                (merge old data {:source (:content data) :baseline (:content data)
                                :buffer-version (inc (:buffer-version old 0)) :disk nil :editor-ready? false})))))
(defn- set-source [s key source]
  (valid-source source)
  (if (get-in s [:files key]) (-> s (assoc-in [:files key :source] source) (update :errors dissoc [:file key])) s))

(defn with-status [local result]
  (let [id (:selected-id local)
        status (:value result)]
    (cond
      (nil? result) local
      (and id (= id (:id status))) (assoc-in local [:statuses id] status)
      (nil? status) (-> local (update :statuses dissoc id)
                        (assoc-in [:errors [:status id]] "Workspace status is unavailable."))
      :else local)))

(rf/reg-sub :workspace/local (fn [db _] (state db)))
(rf/reg-sub-raw :workspace/state
  (fn [_ _]
    (let [local (rf/subscribe [:workspace/local])]
      (r/reaction
        (let [{:keys [open? selected-id] :as s} @local]
          (if (and open? selected-id)
            (with-status s @(rf/subscribe [:studio/remote-result [:development/workspace selected-id]]))
            s))))))
(rf/reg-sub :workspace/token (fn [db _] (token db)))
(rf/reg-event-db :workspace/set
  (fn [db [_ path value]]
    (store db (cond-> (assoc-in (state db) path value) (= :create (first path)) (assoc :create-open? true)))))
(rf/reg-event-fx :workspace/open
  (fn [{:keys [db]} _] {:db (store db (assoc (state db) :open? true)) :dispatch [:workspace/list]}))
(rf/reg-event-db :workspace/close
  (fn [db _] (store db (assoc (state db) :open? false))))
(rf/reg-event-fx :workspace/runtime-change
  (fn [{:keys [db]} _]
    (cond-> {:db (store db (-> (state db) (update :generation inc)
                              (assoc :pending {} :errors {} :statuses {} :runner-ready? false :profiles [] :notice nil)))}
      (:open? (state db)) (assoc :dispatch [:workspace/list]))))
(rf/reg-event-fx :workspace/list
  (fn [{:keys [db]} _]
    (cond
      (not (:open? (state db))) {}
      (nil? (get-in db [:show :runtime-id])) {}
      (get-in (state db) [:pending :list]) {}
      :else (request db :list nil {} nil {}))))
(rf/reg-event-fx :workspace/create
  (fn [{:keys [db]} _]
    (try
      (let [form (:create (state db)) id (str/trim (:id form))]
        (when (and (seq id) (not (re-matches #"[a-zA-Z0-9][a-zA-Z0-9_-]{0,63}" id)))
          (throw (js/Error. "Workspace ID must use letters, digits, hyphens or underscores, up to 64 characters.")))
        (request db :create :create {} (cond-> {:base-refs {:tolglow (:tolglow form) :afterglow (:afterglow form)}} (seq id) (assoc :id id)) {}))
      (catch :default error (fail db :create error)))))
(rf/reg-event-fx :workspace/select
  (fn [{:keys [db]} [_ id]]
    (let [id (wire id)]
      {:db (store db (assoc (state db) :selected-id id :active-file nil :filter "" :notice nil))
       :dispatch [:workspace/tree 0]})))
(rf/reg-event-fx :workspace/review-task
  (fn [{:keys [db]} [_ id]]
    (let [id (wire id)]
      {:db (store db (assoc (state db) :selected-id id :active-file nil :pane :diff :filter "" :notice nil))
       :fx [[:dispatch [:workspace/list]] [:dispatch [:workspace/diff]]]})))
(rf/reg-event-fx :workspace/repository
  (fn [{:keys [db]} [_ repo]]
    {:db (store db (assoc (state db) :repo (wire repo) :active-file nil :filter "")) :dispatch [:workspace/tree 0]}))
(rf/reg-event-fx :workspace/tree
  (fn [{:keys [db]} [_ offset]]
    (let [s (state db) id (:selected-id s) repo (:repo s) key [:tree id repo]]
      (if (or (nil? id) (get-in s [:pending key])) {}
        (request db key :tree {:id id :repo repo :offset offset :limit 500} nil {:id id :repo repo :offset offset})))))
(rf/reg-event-fx :workspace/open-file
  (fn [{:keys [db]} [_ path]]
    (let [s (state db) key (file-key (:selected-id s) (:repo s) path)
          db (store db (assoc s :active-file key :pane :files))]
      (if (get-in s [:files key]) {:db db}
        (request db [:file key] :file {:id (first key) :repo (second key) :path path} nil {:file-key key})))))
(rf/reg-event-fx :workspace/new-file
  (fn [{:keys [db]} _]
    (try
      (let [s (state db) path (relative-path (:new-path s)) key (file-key (:selected-id s) (:repo s) path)]
        (when-not (:selected-id s) (throw (js/Error. "Choose a workspace first.")))
        {:db (store db (cond-> (assoc s :active-file key :pane :files :new-path "")
                         (not (get-in s [:files key]))
                         (assoc-in [:files key] {:path path :repo (:repo s) :hash "missing" :source "" :baseline nil
                                                :binary? false :buffer-version 1})))})
      (catch :default error (fail db :new-file error)))))
(rf/reg-event-fx :workspace/reload-file
  (fn [{:keys [db]} _]
    (let [[id repo path :as key] (:active-file (state db))]
      (if key (request db [:file key] :file {:id id :repo repo :path path} nil {:file-key key :reload? true}) {}))))
(rf/reg-event-fx :workspace/edit
  (fn [{:keys [db]} [_ t key source buffer-version]]
    (if-not (and (current? db t) (or (nil? buffer-version) (= buffer-version (get-in (state db) [:files key :buffer-version])))) {}
      (try {:db (store db (set-source (state db) key source))}
           (catch :default error (fail db [:file key] error))))))
(rf/reg-event-db :workspace/resolve-disk
  (fn [db [_ key action]]
    (let [s (state db) disk (get-in s [:files key :disk])]
      (if-not disk db
        (store db (case action
                    :discard-disk (assoc-in s [:files key :disk] nil)
                    :replace (accept-file s key disk false)
                    :baseline (-> s (assoc-in [:files key :hash] (:hash disk))
                                  (assoc-in [:files key :baseline] (:content disk)) (assoc-in [:files key :disk] nil))
                    s))))))
(rf/reg-event-fx :workspace/save-buffer
  (fn [{:keys [db]} [_ t key source]]
    (if-not (current? db t) {}
      (try
        (let [[id repo path] key s (set-source (state db) key source) file (get-in s [:files key])]
          (when (:binary? file) (throw (js/Error. "Binary files cannot be edited as text.")))
          (when-not (string? (:hash file)) (throw (js/Error. "Read the file before saving so its expected hash is known.")))
          (if (get-in s [:pending [:write key]]) {}
            (request (store db s) [:write key] :write {}
                     {:id id :repo repo :path path :content source :expected-hash (:hash file)} {:file-key key :source source :expected-hash (:hash file)})))
        (catch :default error (fail db [:file key] error))))))
(defn- read-buffer [engine]
  (let [timer (atom nil)
        timeout (js/Promise. (fn [_ reject] (reset! timer (js/setTimeout #(reject (js/Error. "Neovim did not answer the buffer read.")) 3000))))]
    (-> (js/Promise.race #js [(js-invoke engine "text") timeout]) (.finally #(js/clearTimeout @timer)))))
(rf/reg-fx :workspace/read-editor
  (fn [{:keys [key token action buffer-version]}]
    (let [{:keys [engine file-key editor-token]} @resources]
      (if (and engine (= key file-key) (= token editor-token))
        (-> (read-buffer engine)
            (.then #(rf/dispatch [:workspace/editor-read token key action % buffer-version]))
            (.catch #(rf/dispatch [:workspace/editor-error token key (events/error-message %) buffer-version])))
        (rf/dispatch [:workspace/editor-error token key "Wait for Neovim to finish loading."])))))
(rf/reg-event-fx :workspace/save
  (fn [{:keys [db]} _]
    (let [s (state db) key (:active-file s) file (get-in s [:files key])]
      (cond (nil? file) {}
            (= :neovim (:editor s)) {:workspace/read-editor {:key key :token (token db) :action :save :buffer-version (:buffer-version file)}}
            :else {:dispatch [:workspace/save-buffer (token db) key (:source file)]}))))
(rf/reg-event-fx :workspace/navigate
  (fn [{:keys [db]} [_ action]]
    (let [s (state db) key (:active-file s) file (get-in s [:files key])]
      (if (and file (= :neovim (:editor s)) (:editor-ready? file))
        {:workspace/read-editor {:key key :token (token db) :action action :buffer-version (:buffer-version file)}}
        {:dispatch action}))))
(rf/reg-event-fx :workspace/editor-read
  (fn [{:keys [db]} [_ t key action source buffer-version]]
    (if-not (and (current? db t) (= buffer-version (get-in (state db) [:files key :buffer-version]))) {}
      (try {:db (store db (set-source (state db) key source))
            :dispatch (if (= action :save) [:workspace/save-buffer t key source] action)}
           (catch :default error (fail db [:file key] error))))))
(rf/reg-event-db :workspace/editor
  (fn [db [_ engine]] (store db (assoc (state db) :editor engine))))
(rf/reg-event-db :workspace/editor-status
  (fn [db [_ t key text ready? buffer-version]]
    (if (and (current? db t) (or (nil? buffer-version) (= buffer-version (get-in (state db) [:files key :buffer-version]))))
      (store db (-> (state db) (assoc-in [:files key :editor-status] text)
                                    (assoc-in [:files key :editor-ready?] (boolean ready?)))) db)))
(rf/reg-event-fx :workspace/editor-error
  (fn [{:keys [db]} [_ t key error buffer-version]]
    (if (and (current? db t) (or (nil? buffer-version) (= buffer-version (get-in (state db) [:files key :buffer-version]))))
      (fail (store db (assoc-in (state db) [:files key :editor-ready?] false)) [:file key] error) {})))
(rf/reg-event-fx :workspace/diff
  (fn [{:keys [db]} _]
    (let [s (state db) id (:selected-id s)]
      (if id (request db [:diff id] :diff {:id id} nil {:id id}) {}))))
(rf/reg-event-fx :workspace/pane
  (fn [{:keys [db]} [_ pane]]
    (cond-> {:db (store db (assoc (state db) :pane pane))} (= pane :diff) (assoc :dispatch [:workspace/diff]))))
(rf/reg-event-fx :workspace/check
  (fn [{:keys [db]} [_ profile]]
    (let [id (:selected-id (state db)) profile (wire profile)]
      (if id (request db [:check id profile] :check {} {:id id :profile profile} {:id id :profile profile}) {}))))
(rf/reg-event-fx :workspace/stage
  (fn [{:keys [db]} _]
    (let [id (:selected-id (state db))]
      (if id (request db [:stage id] :stage {} {:id id} {:id id}) {}))))

(rf/reg-event-fx :workspace/result
  (fn [{:keys [db]} [_ t key serial operation context data]]
    (if-not (and (current? db t) (= serial (get-in (state db) [:pending key]))) {}
      (let [s (-> (state db) (update :pending dissoc key) (update :errors dissoc key))
            {:keys [id repo offset file-key source reload? profile]} context]
        (case operation
          nil (cond-> {:db (store db (-> s (assoc :workspaces (:workspaces data []) :profiles (profiles (:profiles data []))
                                                 :runner-ready? (true? (:runner-ready? data)) :closed? (:closed? data))
                                        (update :statuses merge (into {} (map (juxt :id identity) (:workspaces data))))))}
                (and (nil? (:selected-id s)) (seq (:workspaces data))) (assoc :dispatch [:workspace/select (:id (first (:workspaces data)))]))
          :create {:db (store db (-> s (assoc-in [:statuses (:id data)] data) (assoc :notice "Paired workspace created.")))
                   :fx [[:dispatch [:workspace/list]] [:dispatch [:workspace/navigate [:workspace/select (:id data)]]]]}
          :tree (let [previous (get-in s [:trees [id repo] :entries])
                      entries (if (zero? offset) (:entries data) (vec (vals (into (sorted-map) (map (juxt :path identity) (concat previous (:entries data)))))))]
                  {:db (store db (assoc-in s [:trees [id repo]] (assoc data :entries entries)))})
          :file {:db (store db (accept-file s file-key data reload?))}
          :write {:db (store db (cond-> (assoc s :notice (str (last file-key) " saved."))
                                 (#{(:expected-hash context) (:hash data)} (get-in s [:files file-key :hash]))
                                 (-> (update-in [:files file-key] merge (select-keys data [:hash :size :revision]))
                                     (assoc-in [:files file-key :baseline] source)
                                     (assoc-in [:files file-key :disk] nil))))
                  :fx [[:dispatch [:workspace/tree 0]] [:dispatch [:workspace/diff]]]}
          :diff {:db (store db (assoc-in s [:diffs id] data))}
          :check {:db (store db (assoc-in s [:statuses id :running-checks profile] data))}
          :stage {:db (store db (-> s (assoc-in [:statuses id :preview] data) (assoc :notice "Review candidate prepared from the checked files.")))}
          {:db (store db s)})))))
(rf/reg-event-fx :workspace/failure
  (fn [{:keys [db]} [_ t key serial error]]
    (if-not (and (current? db t) (= serial (get-in (state db) [:pending key]))) {}
      {:db (store db (-> (state db)
                         (update :pending dissoc key)
                         (assoc-in [:errors key] (events/error-message error))))})))

(def neovim
  (r/create-class
   {:display-name "Workspace Neovim"
    :component-did-mount
    (fn [this]
      (let [[_ key file t] (r/argv this)]
        (aset this "alive" true)
        (-> (js-invoke js/globalThis "tolglowLoadEngine" "neovim")
            (.then (fn [module]
                     (when (aget this "alive")
                       (let [engine (js/Reflect.construct (aget module "NeovimEngine")
                                                         #js [(aget this "host")
                                                              (js-obj "fileName" (str "/home/user/" (last (str/split (:path file) #"/")))
                                                                      "fileType" (file-type (:path file))
                                                                      "onChange" #(when (aget this "alive") (rf/dispatch [:workspace/edit t key % (:buffer-version file)]))
                                                                      "onStatus" #(when (aget this "alive") (rf/dispatch [:workspace/editor-status t key % false (:buffer-version file)]))
                                                                      "onError" #(when (aget this "alive") (rf/dispatch [:workspace/editor-error t key % (:buffer-version file)])))])]
                         (aset this "engine" engine)
                         (reset! resources {:engine engine :file-key key :editor-token t})
                         (-> (js-invoke engine "start" (:source file ""))
                             (.then #(when (aget this "alive") (rf/dispatch [:workspace/editor-status t key "Neovim is ready. Save file reads the actual buffer; :w writes only the browser filesystem." true (:buffer-version file)])))
                             (.catch #(when (aget this "alive") (rf/dispatch [:workspace/editor-error t key (events/error-message %) (:buffer-version file)]))))))))
            (.catch #(when (aget this "alive") (rf/dispatch [:workspace/editor-error t key (events/error-message %) (:buffer-version file)]))))))
    :component-will-unmount
    (fn [this]
      (aset this "alive" false)
      (when-let [engine (aget this "engine")]
        (let [[_ key file t] (r/argv this)]
          (when (identical? engine (:engine @resources)) (reset! resources {}))
          (if (aget engine "ready")
            (-> (read-buffer engine)
                (.then #(rf/dispatch [:workspace/edit t key % (:buffer-version file)]))
                (.catch #(rf/dispatch [:workspace/editor-error t key (events/error-message %) (:buffer-version file)]))
                (.finally #(js-invoke engine "stop")))
            (js-invoke engine "stop")))))
    :reagent-render
    (fn [_key _file _token]
      (let [this (r/current-component)]
        [:div.workspace-neovim {:ref #(aset this "host" %)}]))}))
(rf/reg-fx :workspace/editor-input
  (fn [keys]
    (when-let [engine (:engine @resources)]
      (js-invoke engine "send" keys)
      (when-let [input (aget engine "input")] (.focus input)))))
(rf/reg-event-fx :workspace/editor-input (fn [_ [_ keys]] {:workspace/editor-input keys}))

(defn- set-field! [path event] (rf/dispatch-sync [:workspace/set path (.. event -target -value)]))
(defn- navigate! [action] (rf/dispatch [:workspace/navigate action]))
(defn- lookup [m key] (or (get m key) (get m (keyword (wire key))) (get m (wire key))))
(defn- error-box [s key]
  (when-let [error (get-in s [:errors key])] [:p.workspace-error {:role "alert"} error]))
(defn- short-hash [hash] (when (string? hash) (subs hash 0 (min 12 (count hash)))))
(defn- status-label [value] (if value (str/replace (wire value) "-" " ") "Not run"))
(defn- create-form [s]
  (let [busy? (boolean (get-in s [:pending :create]))]
    [:div.workspace-create
     [:button.text-button {:type "button" :disabled busy? :aria-expanded (boolean (:create-open? s))
                           :on-click #(rf/dispatch [:workspace/set [:create-open?] (not (:create-open? s))])}
      "+ Create paired workspace"]
     (when (or (:create-open? s) (empty? (:workspaces s)))
       [:div.workspace-create-fields
        [:p.small.muted "Create fresh branches for Tolglow and Afterglow from committed refs. Changes in your main checkouts are not copied."]
        [:div.workspace-form-row
         [:label "Workspace ID · optional" [:input {:type "text" :value (get-in s [:create :id]) :disabled busy?
                                                     :placeholder "lighting-architecture" :on-change #(set-field! [:create :id] %)}]]
         [:label "Tolglow base ref" [:input {:type "text" :value (get-in s [:create :tolglow]) :disabled busy?
                                             :on-change #(set-field! [:create :tolglow] %)}]]
         [:label "Afterglow base ref" [:input {:type "text" :value (get-in s [:create :afterglow]) :disabled busy?
                                               :on-change #(set-field! [:create :afterglow] %)}]]]
        [error-box s :create]
        [:button.button.primary {:type "button" :disabled busy? :on-click #(rf/dispatch [:workspace/create])}
         (if busy? "Creating paired branches…" "Create workspace")]])]))
(defn- tree-panel [s]
  (let [id (:selected-id s) repo (:repo s) tree (get-in s [:trees [id repo]])
        term (str/lower-case (:filter s))
        entries (filter #(str/includes? (str/lower-case (:path %)) term) (:entries tree))
        groups (sort-by key (group-by #(or (not-empty (str/join "/" (butlast (str/split (:path %) #"/")))) ".") entries))
        busy? (boolean (get-in s [:pending [:tree id repo]]))]
    [:aside.workspace-tree {:aria-label "Workspace files"}
     [:div.workspace-tree-top [:strong "Files"]
      [:span (str (count (:entries tree)) " / " (:total tree 0))]
      [:button.text-button {:type "button" :disabled busy? :aria-label "Refresh file tree" :on-click #(rf/dispatch [:workspace/tree 0])} "↻"]]
     [:input.workspace-file-search {:type "search" :value (:filter s) :placeholder "Filter loaded files"
                                     :aria-label "Filter loaded workspace files" :on-change #(set-field! [:filter] %)}]
     [error-box s [:tree id repo]]
     [:div.workspace-file-list
      (for [[directory files] groups]
        ^{:key directory} [:details.workspace-directory {:open true}
                           [:summary (str directory "/")]
                           (for [file files :let [key (file-key id repo (:path file)) draft (get-in s [:files key])]]
                             ^{:key (:path file)}
                             [:button.workspace-file {:type "button" :title (:path file) :disabled (not (:editable? file))
                                                      :aria-current (when (= key (:active-file s)) "true")
                                                      :on-click #(navigate! [:workspace/open-file (:path file)])}
                              [:span (last (str/split (:path file) #"/"))]
                              (when (dirty? draft) [:span.workspace-dirty {:aria-label "Unsaved edits"} "●"])
                              (when-not (:editable? file) [:span.small.muted (status-label (:type file))])])])
      (when (and (empty? entries) (not busy?)) [:p.small.muted "No matching files in the loaded page."])]
     (when-let [offset (:next-offset tree)]
       [:button.button.workspace-more {:type "button" :disabled busy? :on-click #(rf/dispatch [:workspace/tree offset])} "Load more files"])
     [:details.workspace-new-file
      [:summary "New file"]
      [:label "Path in an existing directory"
       [:input {:type "text" :value (:new-path s) :placeholder "tolglow/new_effect.clj" :on-change #(set-field! [:new-path] %)}]]
      [error-box s :new-file]
      [:button.button {:type "button" :on-click #(navigate! [:workspace/new-file])} "Open new buffer"]]]))
(defn- disk-review [key file]
  (when-let [disk (:disk file)]
    [:div.workspace-conflict
     [:strong "Review the disk version"]
     [:p "Your buffer is preserved. Compare this version before replacing it or using its hash as the base for your next save."]
     [:pre [:code (or (:content disk) "Binary file — text merge unavailable.")]]
     [:div.button-row
      [:button.button {:type "button" :disabled (boolean (:binary? disk)) :on-click #(rf/dispatch [:workspace/resolve-disk key :replace])} "Replace buffer with disk"]
      [:button.button {:type "button" :disabled (boolean (:binary? disk)) :on-click #(rf/dispatch [:workspace/resolve-disk key :baseline])} "Use disk as merge base"]
      [:button.button {:type "button" :on-click #(rf/dispatch [:workspace/resolve-disk key :discard-disk])} "Keep editing"]]]))
(defn- editor-panel [s t]
  (let [key (:active-file s) file (get-in s [:files key]) saving? (boolean (get-in s [:pending [:write key]]))
        nvim? (= :neovim (:editor s))]
    [:section.workspace-editor {:aria-label "Application file editor"}
     [error-box s [:file key]] [error-box s [:write key]]
     (if-not file
       [:div.workspace-editor-empty [:strong (if (get-in s [:pending [:file key]]) "Reading the selected file…" "Open a file from either repository.")]
        [:p "Changes stay in this development workspace. Save files, review the paired diff, then run configured checks."]]
       [:<>
        [:div.workspace-file-heading [:div [:strong (:path file)]
                                      [:span.small.muted (str (if (= "missing" (:hash file)) "New file" (str "SHA " (short-hash (:hash file))))
                                                             (when (dirty? file) " · Unsaved edits"))]]
         [:label "Editor"
          [:select {:value (name (:editor s)) :disabled (boolean (:binary? file)) :aria-label "Application file editor engine"
                    :on-change #(navigate! [:workspace/editor (keyword (.. % -target -value))])}
           [:option {:value "text"} "Accessible text"] [:option {:value "neovim"} "Neovim · WebAssembly"]]]]
        (if (:binary? file)
          [:p.workspace-binary "This is a binary file. The workspace can include it, but this text editor cannot change it."]
          (if nvim?
            ^{:key (pr-str [key (:buffer-version file) t])} [neovim key file t]
            [:textarea#workspace-source {:value (:source file "") :spell-check false :auto-capitalize "off" :auto-correct "off"
                                          :aria-label (str "Source for " (:path file))
                                          :on-change #(rf/dispatch-sync [:workspace/edit t key (.. % -target -value)])
                                          :on-key-down #(when (and (or (.-ctrlKey %) (.-metaKey %)) (= "s" (str/lower-case (.-key %))))
                                                          (.preventDefault %) (rf/dispatch [:workspace/save]))}]))
        (when nvim?
          [:<> [:div.workspace-nvim-keys
                (for [[label keys] [["Esc" "<Esc>"] ["Insert" "i"] ["Tab" "<Tab>"] ["Undo" "<Esc>u"]]]
                  ^{:key label} [:button.button {:type "button" :on-click #(rf/dispatch [:workspace/editor-input keys])} label])]
           [:p.small.muted (:editor-status file "Starting actual Neovim…")]])
        [:div.workspace-save-bar
         [:button.button.primary {:type "button" :disabled (or saving? (:binary? file) (and nvim? (not (:editor-ready? file))))
                                   :on-click #(rf/dispatch [:workspace/save])} (if saving? "Saving…" "Save file")]
         [:button.button {:type "button" :disabled (boolean (get-in s [:pending [:file key]]))
                           :on-click #(navigate! [:workspace/reload-file])} "Read disk version"]
         [:span.small.muted "Save checks the file hash; changed disk content is never overwritten silently."]]
        [disk-review key file]])]))
(defn- diff-panel [s]
  (let [id (:selected-id s) diff (get-in s [:diffs id]) busy? (boolean (get-in s [:pending [:diff id]]))]
    [:section.workspace-diffs {:aria-label "Paired workspace changes"}
     [:div.workspace-section-heading [:h2 "Changes across both repositories"]
      [:button.button {:type "button" :disabled busy? :on-click #(rf/dispatch [:workspace/diff])} "Refresh diff"]]
     [error-box s [:diff id]]
     [:p.small.muted "Git reports saved worktree changes relative to HEAD. Unsaved editor buffers are not included."]
     (for [[repo changes] (sort-by (comp wire key) (:repositories diff))
           :let [{:keys [files paths added removed untracked]} (change-summary changes)
                 lines (str/split-lines (:diff changes ""))]]
       ^{:key (wire repo)}
       [:article.workspace-repo-diff
        [:h3 (wire repo)]
        [:p.workspace-diff-summary (str files " changed paths · +" added " / −" removed " tracked lines · " untracked " untracked")]
        (if (seq paths) [:ul.workspace-changed-paths (for [[index path] (map-indexed vector paths)] ^{:key index} [:li [:code path]])]
            [:p.small.muted "No saved changes."])
        (when (seq lines)
          [:pre.workspace-diff-text {:tab-index 0 :aria-label (str (wire repo) " unified diff")}
           [:code (for [[index line] (map-indexed vector (take 6000 lines))]
                    ^{:key index} [:span {:class (cond (str/starts-with? line "@@") "diff-hunk"
                                                     (str/starts-with? line "+") "diff-added"
                                                     (str/starts-with? line "-") "diff-removed" :else nil)} (str line "\n")])]])
        (when (> (count lines) 6000) [:p.small.muted "Showing the first 6,000 diff lines. The full changes remain in the worktree."])])]))
(defn- checks-panel [s]
  (let [id (:selected-id s) status (get-in s [:statuses id]) running (:running-checks status)
        preview (:preview status)]
    [:section.workspace-checks {:aria-label "Workspace checks and review readiness"}
     [:h2 "Checks and review readiness"]
     [:p.small.muted (if (:runner-ready? s) "Checks use the host's configured isolated runner."
                         "The isolated check runner is unavailable. Configure it before running project code.")]
     [error-box s [:status id]]
     (for [profile (:profiles s)
           :let [profile-id (wire (:id profile)) result (lookup (:checks status) profile-id)
                 active (lookup running profile-id) busy? (or active (get-in s [:pending [:check id profile-id]]))]]
       ^{:key profile-id}
       [:article.workspace-check
        [:div.workspace-check-heading
         [:div [:strong (or (:label profile) profile-id)] [:span.small.muted (wire (:repo profile))]]
         [:span.workspace-check-status {:data-check-status (if active "running" (wire (:status result)))} (if active "Running" (status-label (:status result)))]
         [:button.button {:type "button" :disabled (boolean (or busy? (not (:runner-ready? s))))
                           :on-click #(rf/dispatch [:workspace/check profile-id])} (if busy? "Running…" "Run check")]]
        [error-box s [:check id profile-id]]
        (when (:error result) [:p.workspace-error (events/error-message (:error result))])
        (when (or (:output result) active)
          [:details.workspace-check-output
           [:summary (if active "Current run pending · previous output below" "Check output")]
           [:pre [:code (:output result "No completed output yet.")]]
           (when (:truncated? result) [:p.workspace-error "The runner reached its output limit; this check was not accepted as passing."])])])
     (when (empty? (:profiles s)) [:p.small.muted "No fixed check profiles are configured."])
     [:div.workspace-readiness
      [:h3 "Review candidate"]
      [:p (if (= "ready" (wire (:status preview)))
            "Prepared from the checked paired files." "No current review candidate is prepared.")]
      [:p.small.muted "Preparing a candidate verifies the configured required checks. It does not start a preview server, merge changes or deploy the application."]
      [error-box s [:stage id]]
      [:button.button.primary {:type "button" :disabled (boolean (or (empty? (:profiles s)) (seq running) (get-in s [:pending [:stage id]])))
                                :on-click #(rf/dispatch [:workspace/stage])} "Prepare review candidate"]]]))

(defn workspace-view [_db]
  (let [s @(rf/subscribe [:workspace/state]) t @(rf/subscribe [:workspace/token])
        id (:selected-id s) status (get-in s [:statuses id])
        repositories (or (seq (sort-by (comp wire key) (:repositories status))) [[:tolglow {}] [:afterglow {}]])]
    [:div.workspace-view
     [:section.panel.workspace-header
      [:div.panel-top [:span.section-label "APPLICATION DEVELOPMENT"] [:span.small.muted "Paired Git worktrees"]]
      [:div.workspace-header-body
       [:h1 "Develop the lighting system"]
       [:p.small.muted "Edit application code in its own paired branches. Cue and effect authoring remains in Definitions; running shows keep their own state."]
       [error-box s :list]
       [error-box s [:status id]]
       (when (:notice s) [:p.workspace-notice {:role "status"} (:notice s)])
       [:div.workspace-selection
        [:label "Workspace"
         [:select {:value (or id "") :aria-label "Development workspace"
                    :on-change #(navigate! [:workspace/select (.. % -target -value)])}
          (when-not id [:option {:value ""} "Choose or create a workspace"])
          (for [workspace (:workspaces s)] ^{:key (:id workspace)} [:option {:value (:id workspace)} (str (:id workspace) " · " (status-label (:state workspace)))])]]
        [:button.button {:type "button" :disabled (boolean (get-in s [:pending :list])) :on-click #(rf/dispatch [:workspace/list])} "Refresh workspaces"]]
       [create-form s]]]
     [application-agents/panel]
     (when id
       [:section.panel.workspace-main
        [:div.workspace-repositories {:role "group" :aria-label "Paired repositories"}
         (for [[repo details] repositories :let [name (wire repo)]]
           ^{:key name} [:button {:type "button" :aria-pressed (= name (:repo s)) :on-click #(navigate! [:workspace/repository name])}
                         [:strong name] [:span (or (:branch details) "Loading branch…")] [:code (short-hash (:base details))]])]
        [:nav.workspace-panes {:aria-label "Development workspace views"}
         (for [[pane label] [[:files "Files"] [:diff "Diff & summary"] [:checks "Checks & preview"] [:terminal "Terminal"]]]
           ^{:key pane} [:button {:type "button" :aria-current (when (= pane (:pane s)) "page") :on-click #(navigate! [:workspace/pane pane])} label])]
        (case (:pane s)
          :files [:div.workspace-file-layout [tree-panel s] [editor-panel s t]]
          :diff [diff-panel s]
          :checks [checks-panel s]
          :terminal [:div.workspace-terminal-slot {:data-workspace-id id :data-repository (:repo s)}
                     [terminal/panel id (keyword (:repo s))]]
          nil)])]))
(def panel
  (r/create-class
   {:display-name "Development workspace"
    :component-did-mount (fn [_] (rf/dispatch [:workspace/open]))
    :component-will-unmount (fn [_] (rf/dispatch [:workspace/close]))
    :reagent-render workspace-view}))
