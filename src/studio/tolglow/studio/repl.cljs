(ns tolglow.studio.repl
  "Explicit JVM evaluation with a bounded transcript, shared session history and
  owned stream/editor resources. Opening a console never evaluates source."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.events :as events]
            [tolglow.studio.navigation :as nav]
            [tolglow.studio.repl-code :as code]))

(def defaults {:open? false :session-id "main" :source "(+ 1 2)" :dirty? false
               :generation 0 :activity 0 :polling? false :snapshot nil :entries [] :running? false
               :editor :text :editor-status nil :editor-ready? false :completions []})

(defonce resources (atom {}))

(defn state [db] (merge defaults (:repl db)))

(defn token [db] (let [s (state db)] [(:generation s) (:runtime-id (:snapshot s)) (:epoch (:snapshot s)) (:session-id s)]))

(defn- current? [db value] (= value (token db)))

(defn- set-state [db s] (assoc db :repl s))

(defn- value-of [event] (.. event -target -value))

(defn- guard [s body] (merge {:session-id (:session-id s) :runtime-id (get-in s [:snapshot :runtime-id])
                             :expected-epoch (get-in s [:snapshot :epoch])} body))

(defn- bounded [s] (subs (str s) 0 (min 65536 (count (str s)))))

(defn event-entry [entries event]
  (let [id (:request-id event) index (first (keep-indexed #(when (= id (:request-id %2)) %1) entries))
        index (or index (count entries))
        entries (if (= index (count entries)) (conj (vec entries) {:request-id id :source (:source event) :events []}) entries)
        prior (get entries index)
        seen? (and (:sequence event) (some #(= (:sequence event) (:sequence %)) (:events prior))) ]
    (if seen? entries
      (-> entries (update-in [index :events] #(vec (take-last 300 (conj (vec %) (cond-> event (:text event) (update :text bounded) (:value event) (update :value bounded))))))
          (update index merge (when (:source event) {:source (:source event)}))
          (->> (take-last 100) vec)))))

(defn history-entries [history]
  (mapv (fn [entry]
          (assoc entry :events (or (seq (:events entry))
                                  (cond-> []
                                    (some? (:result entry)) (conj {:type "result" :value (:result entry)})
                                    (:error entry) (conj (assoc (:error entry) :type "error"))))))
        (take-last 100 history)))

(defn merge-history [entries history]
  (let [prior (into {} (map (juxt :request-id identity) entries))]
    (mapv (fn [entry] (if (seq (:events (get prior (:request-id entry))))
                       (assoc entry :events (:events (get prior (:request-id entry)))) entry)) (history-entries history))))

(rf/reg-sub :repl/state (fn [db _] (state db)))
(rf/reg-fx :repl/focus (fn [_] (js/requestAnimationFrame #(when-let [input (.getElementById js/document "repl-source")] (.focus input)))))
(rf/reg-fx :repl/scroll
  (fn [_]
    (when-let [log (.querySelector js/document ".repl-log")]
      (when (< (- (.-scrollHeight log) (.-scrollTop log) (.-clientHeight log)) 100)
        (js/requestAnimationFrame #(set! (.-scrollTop log) (.-scrollHeight log)))))))
(rf/reg-fx :repl/popup (fn [session-id] (.open js/window (nav/href :repl {:session-id session-id}) "tolglow-repl" "popup,width=1000,height=720")))
(rf/reg-fx :repl/abort (fn [_] (when-let [controller (:stream @resources)] (.abort controller)) (swap! resources dissoc :stream)))
(rf/reg-fx :repl/stream
  (fn [{:keys [body run token]}]
    (when-let [old (:stream @resources)] (.abort old))
    (let [controller (js/AbortController.) decoder (js/TextDecoder.) buffer (atom "")]
      (swap! resources assoc :stream controller)
      (-> (js/fetch "/api/repl/eval" #js {:method "POST" :headers #js {"Content-Type" "application/json"}
                                         :body (js/JSON.stringify (clj->js body)) :signal (.-signal controller)})
          (.then (fn [response]
                   (if-not (.-ok response)
                     (-> (.text response) (.then #(throw (js/Error. %))))
                     (let [reader (.getReader (.-body response))]
                       (letfn [(consume [chunk]
                                 (let [parts (str/split (str/replace (str @buffer chunk) #"\r\n" "\n") #"\n\n" -1)]
                                   (reset! buffer (last parts))
                                   (when (> (count @buffer) 1048576) (throw (js/Error. "REPL stream event exceeded the buffer limit.")))
                                   (doseq [block (butlast parts)
                                           :let [data (->> (str/split-lines block) (filter #(str/starts-with? % "data:"))
                                                           (map #(str/triml (subs % 5))) (str/join "\n"))] :when (seq data)]
                                     (rf/dispatch [:repl/event token run (js->clj (js/JSON.parse data) :keywordize-keys true)]))))
                               (read-next []
                                 (-> (.read reader)
                                     (.then (fn [part]
                                              (if (.-done part)
                                                (do (consume (.decode decoder)) (rf/dispatch [:repl/closed token run]))
                                                (do (consume (.decode decoder (.-value part) #js {:stream true})) (read-next)))))))]
                         (read-next))))))
          (.catch #(when-not (= "AbortError" (.-name %)) (rf/dispatch [:repl/failed token run (events/error-message %)])))
          (.finally #(when (identical? controller (:stream @resources)) (swap! resources dissoc :stream)))))))

(rf/reg-event-fx :repl/open
  (fn [{:keys [db]} [_ popup? session-id]]
    {:db (set-state db (cond-> (assoc (state db) :open? true :popup? (boolean popup?)) session-id (assoc :session-id session-id :session-draft session-id)))
     :fx [[:dispatch [:repl/poll]] [:repl/focus true]]}))
(rf/reg-event-fx :repl/close
  (fn [{:keys [db]} _]
    {:db (set-state db (-> (state db) (assoc :open? false :polling? false :running? false :run nil) (update :generation inc)))
     :repl/abort true}))
(rf/reg-event-fx :repl/toggle
  (fn [{:keys [db]} _] {:dispatch [(if (:open? (state db)) :repl/close :repl/open)]}))
(rf/reg-event-fx :repl/popout (fn [{:keys [db]} _] {:repl/popup (:session-id (state db))}))
(rf/reg-event-fx :repl/runtime-change
  (fn [{:keys [db]} _]
    {:db (set-state db (-> (state db) (assoc :snapshot nil :polling? false :running? false :run nil :entries []) (update :generation inc)))
     :fx [[:repl/abort true] [:dispatch [:repl/poll]]]}))
(rf/reg-event-db :repl/edit
  (fn [db [_ source]] (set-state db (-> (state db) (assoc :source source :dirty? true :error nil) (update :edit-revision (fnil inc 0))))))
(rf/reg-event-fx :repl/session
  (fn [{:keys [db]} [_ id]]
    (let [s (state db) id (str/trim id)]
      (if (or (:running? s) (:opening? s) (str/blank? id)) {}
        (let [drafts (assoc (:drafts s {}) (:session-id s) (select-keys s [:source :dirty?]))
              draft (get drafts id {:source "(+ 1 2)" :dirty? false})]
          {:db (set-state db (-> s (merge draft) (assoc :session-id id :entries [] :polling? false :opening? true :editor :text :session-draft id :drafts drafts) (update :generation inc)))
           :studio/http {:path "/api/repl/session" :body (guard s {:session-id id})
                         :on-success [:repl/session-created (inc (:generation s))]
                         :on-failure [:repl/session-failed (inc (:generation s))]}})))))
(rf/reg-event-fx :repl/session-created
  (fn [{:keys [db]} [_ generation _]]
    (let [s (state db)] (if (not= generation (:generation s)) {}
      {:db (set-state db (assoc s :opening? false :error nil)) :dispatch [:repl/poll]}))))
(rf/reg-event-db :repl/session-failed
  (fn [db [_ generation error]]
    (let [s (state db)] (if (not= generation (:generation s)) db
      (set-state db (assoc s :opening? false :error (events/error-message error)))))))
(rf/reg-event-fx :repl/poll
  (fn [{:keys [db]} _]
    (let [s (state db)]
      (if (or (not (:open? s)) (:opening? s) (:polling? s)) {}
        {:db (set-state db (assoc s :polling? true))
         :studio/http {:path (str "/api/repl?session-id=" (js/encodeURIComponent (:session-id s)))
                       :on-success [:repl/loaded (:generation s) (:activity s 0)] :on-failure [:repl/load-failed (:generation s)]}}))))
(rf/reg-event-fx :repl/loaded
  (fn [{:keys [db]} [_ generation activity snapshot]]
    (let [s (state db)]
      (cond
        (not= generation (:generation s)) {}
        (not= activity (:activity s 0))
        {:db (set-state db (assoc s :polling? false)) :dispatch [:repl/poll]}
        :else
        (let [replaced? (and (:snapshot s) (not= (select-keys (:snapshot s) [:runtime-id :epoch]) (select-keys snapshot [:runtime-id :epoch])))
              next (cond-> (assoc s :snapshot snapshot :polling? false :error nil)
                     (not (:dirty? s)) (assoc :source (or (get-in snapshot [:scratch :scratch.clj]) (get-in snapshot [:scratch "scratch.clj"]) (:source s)))
                     (or replaced? (not (:running? s))) (assoc :entries (merge-history (if replaced? [] (:entries s)) (:history snapshot)))
                     replaced? (assoc :running? false :run nil))]
          {:db (set-state db next)
           :fx (cond-> [] replaced? (conj [:repl/abort true])
                  (not= (:source s) (:source next)) (conj [:repl/editor-set {:source (:source next)}]))
           :dispatch-later (when (:open? s) [{:ms 800 :dispatch [:repl/poll]}])})))))
(rf/reg-event-fx :repl/load-failed
  (fn [{:keys [db]} [_ generation error]]
    (let [s (state db)] (if (not= generation (:generation s)) {}
      {:db (set-state db (assoc s :polling? false :error (events/error-message error)))
       :dispatch-later (when (:open? s) [{:ms 1500 :dispatch [:repl/poll]}])}))))
(rf/reg-event-fx :repl/evaluate [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} [_ source]]
    (let [s (state db) source (str source)]
      (if (or (:running? s) (:opening? s) (some? (get-in s [:snapshot :active])) (nil? (:snapshot s)) (str/blank? source)) {}
        {:db (set-state db (-> s (assoc :run unique :running? true :error nil) (update :activity (fnil inc 0))
                                (update :entries #(vec (take-last 100 (conj % {:request-id unique :source source :events []}))))))
         :repl/scroll true
         :repl/stream {:body (guard s {:source source :request-id unique}) :run unique :token (token db)}}))))
(rf/reg-event-fx :repl/event
  (fn [{:keys [db]} [_ captured run event]]
    (let [s (state db)]
      (if (or (not (current? db captured)) (not= run (:run s)) (and (:request-id event) (not= run (:request-id event)))) {}
        {:db (set-state db (cond-> (update s :entries event-entry event)
                            (= "done" (:type event)) (assoc :running? false)
                            (= "done" (:type event)) (update :activity (fnil inc 0))))
         :repl/scroll true
         :dispatch (when (= "done" (:type event)) [:repl/poll])}))))
(rf/reg-event-db :repl/closed
  (fn [db [_ captured run]]
    (let [s (state db)] (if (and (current? db captured) (= run (:run s)))
      (set-state db (assoc s :running? false :run nil)) db))))
(rf/reg-event-db :repl/failed
  (fn [db [_ captured run error]]
    (let [s (state db)] (if (and (current? db captured) (= run (:run s)))
      (set-state db (assoc s :running? false :run nil :error error)) db))))
(rf/reg-event-fx :repl/interrupt
  (fn [{:keys [db]} _]
    (let [s (state db)]
      {:studio/http {:path "/api/repl/interrupt" :body (guard s {:request-id (or (:run s) (get-in s [:snapshot :active :id]))})
                     :on-success [:repl/action-result (token db)] :on-failure [:repl/action-error (token db)]}})))
(rf/reg-event-fx :repl/save
  (fn [{:keys [db]} [_ source]]
    (let [s (state db) source (or source (:source s))]
      {:studio/http {:path "/api/repl/scratch" :body (guard s {:name "scratch.clj" :source source})
                     :on-success [:repl/saved (token db) source] :on-failure [:repl/action-error (token db)]}})))
(rf/reg-event-db :repl/saved
  (fn [db [_ captured source _]]
    (if (and (current? db captured) (= source (:source (state db))))
      (set-state db (assoc (state db) :dirty? false :notice "Scratch buffer saved.")) db)))
(rf/reg-event-db :repl/action-result
  (fn [db [_ captured result]]
    (if (current? db captured) (set-state db (assoc (state db) :notice (or (:message result) "Request sent."))) db)))
(rf/reg-event-db :repl/action-error
  (fn [db [_ captured error]]
    (if (current? db captured) (set-state db (assoc (state db) :error (events/error-message error))) db)))

(rf/reg-fx :repl/editor-set
  (fn [{:keys [source cursor]}]
    (if-let [engine (:editor @resources)]
      (-> (js-invoke engine "setText" source cursor) (.catch #(rf/dispatch [:repl/local-error (events/error-message %)])))
      (js/requestAnimationFrame #(when-let [input (.getElementById js/document "repl-source")]
                                   (.focus input) (when (number? cursor) (.setSelectionRange input cursor cursor)))))))
(rf/reg-fx :repl/read-source
  (fn [{:keys [captured mode]}]
    (if-let [engine (:editor @resources)]
      (-> (js-invoke engine "selection")
          (.then #(rf/dispatch [:repl/read-result captured mode (js->clj % :keywordize-keys true)]))
          (.catch #(rf/dispatch [:repl/action-error (first captured) (events/error-message %)])))
      (when-let [input (.getElementById js/document "repl-source")]
        (let [source (.-value input) start (.-selectionStart input) end (.-selectionEnd input)]
          (rf/dispatch [:repl/read-result captured mode {:source source :cursor start :selected (subs source start end)}]))))))
(rf/reg-event-fx :repl/read
  (fn [{:keys [db]} [_ mode]] {:repl/read-source {:captured [(token db) (:edit-revision (state db) 0)] :mode mode}}))
(rf/reg-event-fx :repl/read-result
  (fn [{:keys [db]} [_ captured mode {:keys [source selected cursor]}]]
    (if-not (current? db (first captured)) {}
      (try
        (let [s (cond-> (state db) (= (second captured) (:edit-revision (state db) 0))
                  (assoc :source source :dirty? (or (:dirty? (state db)) (not= source (:source (state db))))))
              symbol (code/symbol-at source cursor)]
          (case mode
            :save {:db (set-state db s) :dispatch [:repl/save source]}
            :text {:db (set-state db (assoc s :editor :text)) :repl/focus true}
            (:complete :doc)
            (let [query (if (= mode :complete) (:prefix symbol) (:symbol symbol))]
              {:db (set-state db (assoc s :query {:source source :cursor cursor :symbol symbol :mode mode}))
               :studio/http {:path (str "/api/repl/" (name mode) "?session-id=" (js/encodeURIComponent (:session-id s))
                                        (if (= mode :complete) "&prefix=" "&symbol=") (js/encodeURIComponent query))
                             :on-success [:repl/help (first captured) source cursor mode]
                             :on-failure [:repl/action-error (first captured)]}})
            (let [expression (case mode :selection (when (seq selected) selected)
                                       :form (or (when (seq selected) selected) (code/form-at source cursor))
                                       :shortcut (if (seq selected) selected source) source)]
              (if (str/blank? expression)
                {:db (set-state db (assoc s :error "Select or place the cursor inside a form to evaluate."))}
                {:db (set-state db s) :dispatch [:repl/evaluate expression]}))))
        (catch :default error {:db (set-state db (assoc (state db) :error (events/error-message error)))})))))
(rf/reg-event-db :repl/help
  (fn [db [_ captured source cursor mode result]]
    (let [s (state db)]
      (if (and (current? db captured) (= source (:source s)) (= cursor (get-in s [:query :cursor])))
        (set-state db (assoc s (if (= mode :complete) :completions :documentation) result
                              :notice (when (empty? result) (if (= mode :complete) "No matching completions." "No documentation found.")))) db))))
(rf/reg-event-fx :repl/choose-completion
  (fn [{:keys [db]} [_ candidate]]
    (let [s (state db) {:keys [source symbol]} (:query s)]
      (if (not= source (:source s)) {}
        (let [value (str (subs source 0 (:start symbol)) candidate (subs source (:end symbol)))
              cursor (+ (:start symbol) (count candidate))]
          {:db (set-state db (assoc s :source value :dirty? true :completions [] :query nil))
           :repl/editor-set {:source value :cursor cursor}})))))
(rf/reg-event-db :repl/editor
  (fn [db [_ editor]] (set-state db (assoc (state db) :editor editor :editor-ready? false :editor-status nil))))
(rf/reg-event-db :repl/editor-status
  (fn [db [_ text ready?]] (set-state db (assoc (state db) :editor-status text :editor-ready? (boolean ready?)))))

(def neovim
  (r/create-class
    {:display-name "ReplNeovim"
     :component-did-mount
     (fn [this]
       (aset this "alive" true)
       (let [source (second (r/argv this))]
         (-> (js-invoke js/globalThis "tolglowLoadEngine" "neovim")
             (.then (fn [module]
                      (when (aget this "alive")
                        (let [engine (js/Reflect.construct (aget module "NeovimEngine")
                                       #js [(aget this "host")
                                            (js-obj "fileName" "/home/user/scratch.clj"
                                                    "onChange" #(when (aget this "alive") (rf/dispatch [:repl/edit %]))
                                                    "onStatus" #(when (aget this "alive") (rf/dispatch [:repl/editor-status (str/replace % "Publish reads" "Evaluate reads") false]))
                                                    "onError" #(when (aget this "alive") (rf/dispatch [:repl/local-error %])))])]
                          (aset this "engine" engine) (swap! resources assoc :editor engine)
                          (-> (js-invoke engine "start" source)
                              (.then #(when (aget this "alive") (rf/dispatch [:repl/editor-status "Neovim ready · evaluate buffer, selection or form." true])))
                              (.catch #(when (aget this "alive") (rf/dispatch [:repl/local-error (events/error-message %)]))))))))
             (.catch #(when (aget this "alive") (rf/dispatch [:repl/local-error (events/error-message %)]))))))
     :component-will-unmount
     (fn [this]
       (aset this "alive" false)
       (when-let [engine (aget this "engine")]
         (js-invoke engine "stop") (when (identical? engine (:editor @resources)) (swap! resources dissoc :editor))))
     :reagent-render (fn [_source] (let [this (r/current-component)] [:div#repl-neovim {:ref #(aset this "host" %)}]))}))

(defn- event-output [event]
  (case (:type event)
    "out" [:pre.repl-out (:text event)]
    "err" [:pre.repl-err (:text event)]
    "result" [:pre.repl-value (:value event)]
    "error" [:pre.repl-error (str (:class event) "\n" (:message event) (when-let [data (:data event)] (str "\n" data)))]
    "interrupted" [:p.repl-message "Interruption requested."]
    "output-truncated" [:p.repl-message "Output limit reached; remaining output omitted."]
    "done" (when (contains? #{"rejected" "cancelled"} (:status event))
             [:p.repl-error (str "Evaluation " (:status event) (when (:reason event) (str ": " (:reason event))))])
    nil))

(defn console-view []
  (let [s @(rf/subscribe [:repl/state]) busy? (or (:running? s) (:opening? s) (some? (get-in s [:snapshot :active])))]
    (when (:open? s)
      [:section#repl-console {:class (if (:popup? s) "repl-popup" "repl-drawer") :aria-label "Clojure REPL"}
       [:header.repl-heading
        [:div [:span.section-label "LIVE CLOJURE"] [:strong "REPL"] [:span.repl-namespace (or (get-in s [:snapshot :namespace]) "user")]]
        [:div.button-row
         [:button.text-button {:on-click #(rf/dispatch [:repl/popout]) :title "Open shared session in a separate window"} "Pop out ↗"]
         (when-not (:popup? s) [:button.icon-button {:aria-label "Close REPL" :on-click #(rf/dispatch [:repl/close])} "×"])]]
       [:div.repl-session-row
        [:label "Session" [:input {:value (:session-draft s (:session-id s)) :disabled busy?
                                    :on-change #(rf/dispatch-sync [:repl/session-draft (value-of %)])
                                    :on-key-down #(when (= "Enter" (.-key %)) (rf/dispatch [:repl/session (:session-draft s (:session-id s))]))}]]
        [:button.button {:disabled busy? :on-click #(rf/dispatch [:repl/session (:session-draft s (:session-id s))])} "Open session"]
        [:span.small.muted (if busy? "Evaluating…" (if (:snapshot s) "Ready" "Connecting…"))]]
       [:div.repl-log {:aria-label "Evaluation history" :aria-live "polite"}
        (if (empty? (:entries s)) [:div.repl-welcome [:strong "A direct line to the show."] [:p "Evaluate Clojure forms in this local JVM session."]]
          (for [entry (:entries s)]
            ^{:key (:request-id entry)} [:article.repl-entry
                                       [:pre.repl-source-history (:source entry)]
                                       (for [[index event] (map-indexed vector (:events entry))]
                                         ^{:key index} [event-output event])]))]
       (when (:error s) [:p.repl-error {:role "alert"} (:error s)])
       (when (:notice s) [:p.repl-message {:role "status"} (:notice s)])
       [:div.repl-input
        {:on-key-down-capture
         (fn [event]
           (when (or (.-ctrlKey event) (.-metaKey event))
             (when (contains? #{"Enter" " " "`"} (.-key event))
               (.preventDefault event) (.stopPropagation event)
               (rf/dispatch-sync (case (.-key event)
                              "Enter" [:repl/read (if (.-shiftKey event) :form :shortcut)]
                              " " [:repl/read :complete] "`" [:repl/toggle])))))}
        [:label.sr-only {:html-for "repl-source"} "Clojure source"]
        [:div.repl-editor-switch [:label "Editor"]
         [:select {:aria-label "REPL editor" :value (name (:editor s))
                   :on-change #(if (= "text" (value-of %)) (rf/dispatch-sync [:repl/read :text]) (rf/dispatch [:repl/editor :neovim]))}
          [:option {:value "text"} "Accessible text"] [:option {:value "neovim"} "Neovim WASM"]]
         (when (:editor-status s) [:span.small.muted (:editor-status s)])]
        (if (= :neovim (:editor s)) [neovim (:source s)]
        [:textarea#repl-source {:value (:source s) :spell-check false :auto-capitalize "off" :auto-complete "off"
                                :on-change #(rf/dispatch-sync [:repl/edit (value-of %)])
                                :on-key-down (fn [event]
                                               (when (or (.-ctrlKey event) (.-metaKey event))
                                                 (case (.-key event)
                                                   "Enter" (do (.preventDefault event) (rf/dispatch-sync [:repl/read (if (.-shiftKey event) :form :shortcut)]))
                                                   " " (do (.preventDefault event) (rf/dispatch-sync [:repl/read :complete])) nil)))}])
        (when (seq (:completions s)) [:div.repl-completions
                                     (for [item (:completions s)] ^{:key (:candidate item)}
                                       [:button.button {:on-click #(rf/dispatch [:repl/choose-completion (:candidate item)])} (:candidate item)])])
        (when-let [documentation (:documentation s)]
          [:details.repl-help {:open true} [:summary (str (:candidate documentation) " " (:arglists documentation))]
           [:pre (:doc documentation)]])
        [:div.repl-actions
         [:button.button.primary {:disabled (or busy? (nil? (:snapshot s)) (and (= :neovim (:editor s)) (not (:editor-ready? s))))
                                  :on-click #(rf/dispatch-sync [:repl/read :buffer])} "Evaluate buffer"]
         [:button.button {:disabled busy? :on-click #(rf/dispatch-sync [:repl/read :form])} "Evaluate form"]
         [:button.button {:disabled busy? :on-click #(rf/dispatch-sync [:repl/read :selection])} "Evaluate selection"]
         [:button.button {:disabled (not busy?) :on-click #(rf/dispatch [:repl/interrupt])} "Interrupt"]
         [:button.text-button {:on-click #(rf/dispatch-sync [:repl/read :save])} "Save scratch"]
         [:button.text-button {:on-click #(rf/dispatch-sync [:repl/read :complete])} "Complete"]
         [:button.text-button {:on-click #(rf/dispatch-sync [:repl/read :doc])} "Documentation"]
         [:span.small.muted "Ctrl/⌘ Enter · evaluate selection or buffer"]]]])))
(rf/reg-event-db :repl/session-draft (fn [db [_ value]] (assoc-in db [:repl :session-draft] value)))
(rf/reg-event-db :repl/local-error (fn [db [_ message]] (assoc-in db [:repl :error] message)))
