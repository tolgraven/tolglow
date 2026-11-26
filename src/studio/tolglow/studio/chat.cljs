(ns tolglow.studio.chat
  "Streaming agent chat and runtime-only provider settings through Ring APIs."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [tolglow.studio.events :as events]
            [tolglow.studio.pretty :as pretty]))

(defonce stream (atom nil))

(defn- text [event] (.. event -target -value))

(defn- input! [path value] (rf/dispatch-sync [:studio/ui-set path value]))

(rf/reg-fx :studio/chat-stream
  (fn [{:keys [body token]}]
    (when-let [controller @stream] (.abort controller))
    (let [controller (js/AbortController.) decoder (js/TextDecoder.) buffer (atom "")]
      (reset! stream controller)
      (-> (js/fetch "/api/chat" #js {:method "POST" :headers #js {"Content-Type" "application/json"}
                                    :body (js/JSON.stringify (clj->js body)) :signal (.-signal controller)})
          (.then
            (fn [response]
              (if-not (.-ok response)
                (-> (.text response)
                    (.then (fn [value]
                             (let [error (try (js->clj (js/JSON.parse value) :keywordize-keys true) (catch :default _ {:message value}))]
                               (throw (js/Error. (str (events/error-message error))))))))
                (let [reader (.getReader (.-body response))]
                  (letfn [(consume! [chunk]
                            (let [parts (str/split (str/replace (str @buffer chunk) #"\r\n" "\n") #"\n\n" -1)]
                              (reset! buffer (last parts))
                              (doseq [event (butlast parts)
                                      :let [data (->> (str/split-lines event)
                                                      (filter #(str/starts-with? % "data:"))
                                                      (map #(str/triml (subs % 5))) (str/join "\n"))]
                                      :when (and (seq data) (not= data "[DONE]"))]
                                (try (rf/dispatch [:chat/event token (js->clj (js/JSON.parse data) :keywordize-keys true)])
                                     (catch :default _ (throw (js/Error. "Invalid event received from the chat stream.")))))))
                          (read! []
                            (-> (.read reader)
                                (.then (fn [result]
                                         (if (.-done result)
                                           (do (consume! (.decode decoder)) (rf/dispatch [:chat/closed token]))
                                           (do (consume! (.decode decoder (.-value result) #js {:stream true}))
                                               (when (> (count @buffer) (* 1024 1024)) (throw (js/Error. "Chat event exceeded the browser buffer limit.")))
                                               (read!)))))))]
                    (read!))))))
          (.catch (fn [error]
                    (if (= "AbortError" (.-name error))
                      (rf/dispatch [:chat/stopped token])
                      (rf/dispatch [:chat/stream-error token (events/error-message error)]))))
          (.finally #(when (identical? controller @stream) (reset! stream nil)))))))
(rf/reg-fx :studio/chat-abort (fn [_] (when-let [controller @stream] (.abort controller))))
(rf/reg-fx :chat/scroll
  (fn [enabled?]
    (when enabled?
      (js/requestAnimationFrame
        (fn [] (when-let [log (.getElementById js/document "chat-log")]
                 (set! (.-scrollTop log) (.-scrollHeight log))))))))

(rf/reg-event-fx :chat/load [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} _]
    {:db (-> db (assoc-in [:chat :provider-request] unique) (update :chat dissoc :provider-error))
     :fx [[:studio/http {:path "/api/providers" :on-success [:chat/providers-loaded unique] :on-failure [:chat/provider-error unique]}]
          [:dispatch [:chat/history-load]]]}))
(rf/reg-event-fx :chat/history-load [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} _]
    {:db (assoc-in db [:chat :history-request] unique)
     :studio/http {:path "/api/chat/history" :on-success [:chat/history-loaded unique] :on-failure [:chat/history-unavailable unique]}}))
(defn history-entries [conversation]
  (->> (:messages conversation)
       (map-indexed
         (fn [index message]
           (let [role (keyword (:role message)) id (str (:id conversation) "/" index)]
             (case role
               (:user :assistant) {:id id :role role :text (if (string? (:content message)) (:content message) "") :tools []}
               :tool {:id id :role :assistant :text ""
                      :tools [{:id (:tool-call-id message) :name (:name message) :result (:content message) :status :done}]}
               nil))))
       (remove nil?) vec))
(rf/reg-event-db :chat/history-loaded
  (fn [db [_ token history]]
    (if (not= token (get-in db [:chat :history-request])) db
    (let [conversations (:conversations history []) selected (:selected-conversation-id history)
          conversation (some #(when (= selected (:id %)) %) conversations)
          updated (-> db (assoc-in [:chat :conversations] conversations) (assoc-in [:chat :history-available?] true))]
      (cond
        (get-in db [:chat :running?]) updated
        conversation (-> updated (assoc-in [:chat :conversation-id] selected)
                         (assoc-in [:chat :conversation-status] (:status conversation))
                         (assoc-in [:chat :entries] (history-entries conversation)))
        :else (update updated :chat dissoc :conversation-id :conversation-status :entries))))))
(rf/reg-event-db :chat/history-unavailable
  (fn [db [_ token]] (if (= token (get-in db [:chat :history-request]))
                      (assoc-in db [:chat :history-available?] false) db)))
(rf/reg-event-fx :chat/select [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} [_ conversation-id]]
    (when-not (get-in db [:chat :running?])
      (if (seq conversation-id)
        {:db (assoc-in db [:chat :history-request] unique)
         :studio/http {:path "/api/chat/select" :body {:conversation-id conversation-id}
                       :on-success [:chat/selected unique] :on-failure [:chat/history-unavailable unique]}}
        {:dispatch [:chat/clear]}))))
(rf/reg-event-fx :chat/selected
  (fn [{:keys [db]} [_ token _]]
    (when (= token (get-in db [:chat :history-request])) {:dispatch [:chat/history-load]})))
(rf/reg-event-fx :chat/runtime-change
  (fn [{:keys [db]} _]
    {:db (-> db (update :chat #(-> % (assoc :running? false :saving? false) (dissoc :current-id :conversation-id :conversation-status :entries :history-request :provider-request :provider :provider-error :error)))
                (update :ui dissoc :provider-form))
     :studio/chat-abort true :dispatch [:chat/load]}))
(rf/reg-event-db :chat/providers-loaded
  (fn [db [_ token config]]
    (if (not= token (get-in db [:chat :provider-request])) db
    (-> db (assoc-in [:chat :provider] config)
        (update-in [:ui :provider-form] #(merge {:provider "openai" :model "" :endpoint "" :credential ""}
                                               (select-keys config [:provider :model :endpoint]) %))))))
(rf/reg-event-fx :chat/save-provider [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} _]
    (let [form (merge {:provider "openai" :model ""} (get-in db [:ui :provider-form]))
          payload (cond-> (select-keys form [:provider :model])
                    (seq (str/trim (:endpoint form ""))) (assoc :endpoint (:endpoint form))
                    (seq (:credential form)) (assoc :credential (:credential form))
                    (:clear-credential? form) (assoc :clear-credential? true))]
      {:db (-> db (assoc-in [:ui :provider-form :credential] "") (assoc-in [:chat :saving?] true)
               (update :chat dissoc :provider-error) (assoc-in [:chat :provider-request] unique))
       :studio/http {:path "/api/providers" :body payload :on-success [:chat/provider-saved unique] :on-failure [:chat/provider-error unique]}})))
(rf/reg-event-fx :chat/provider-saved
  (fn [{:keys [db]} [_ token config]]
    (when (= token (get-in db [:chat :provider-request]))
      {:db (-> db (assoc-in [:chat :provider] config) (assoc-in [:chat :saving?] false)
               (assoc-in [:ui :provider-form :clear-credential?] false))
       :dispatch [:studio/toast (if (:ready? config)
                                 "Model connection saved."
                                 "Model settings saved. Add an API key to send messages.")]})))
(rf/reg-event-db :chat/provider-error
  (fn [db [_ token error]]
    (if (= token (get-in db [:chat :provider-request]))
      (-> db (assoc-in [:chat :saving?] false) (assoc-in [:chat :provider-error] (str error))) db)))
(rf/reg-event-db :chat/select-provider
  (fn [db [_ provider]] (update-in db [:ui :provider-form] merge {:provider provider :endpoint "" :credential "" :clear-credential? false})))
(rf/reg-event-fx :chat/send [(rf/inject-cofx :studio/unique)]
  (fn [{:keys [db unique]} _]
    (let [message (str/trim (get-in db [:ui :chat-draft] ""))]
      (when (and (seq message) (not (get-in db [:chat :running?])))
        {:db (-> db (update :chat dissoc :history-request) (assoc-in [:ui :chat-draft] "") (assoc-in [:ui :chat-autoscroll?] true) (assoc-in [:chat :error] nil)
                 (assoc-in [:chat :running?] true) (assoc-in [:chat :current-id] unique)
                 (assoc-in [:chat :conversation-status] "running")
                 (update-in [:chat :entries] (fnil into []) [{:id (str unique "-user") :role :user :text message}
                                                            {:id unique :role :assistant :text "" :tools []}]))
         :chat/scroll true
         :studio/chat-stream {:token unique :body (cond-> {:run-id unique :message message}
                                                   (get-in db [:chat :conversation-id])
                                                   (assoc :conversation-id (get-in db [:chat :conversation-id])))}}))))
(defn- current-entry [db f]
  (update-in db [:chat :entries] (fn [entries] (mapv #(if (= (:id %) (get-in db [:chat :current-id])) (f %) %) entries))))

(defn apply-chat-event [db token event]
    (if (not= token (get-in db [:chat :current-id])) db
      (let [db (cond-> db (:conversation-id event) (assoc-in [:chat :conversation-id] (:conversation-id event)))]
      (case (:type event)
      "text/delta" (current-entry db #(update % :text str (:text event)))
      "tool/start" (current-entry db #(update % :tools conj (assoc event :status :running)))
      "tool/result" (current-entry db #(update % :tools
                                              (fn [tools] (mapv (fn [tool] (if (= (:id tool) (:id event)) (merge tool event {:status :done}) tool)) tools))))
      "agent/start" (assoc-in db [:chat :run-id] (:run-id event))
      "agent/error" (-> db (assoc-in [:chat :error] (or (:message event) (:error event)))
                           (assoc-in [:chat :running?] false))
      "chat/done" (-> db (assoc-in [:chat :running?] false)
                       (cond-> (get-in event [:result :conversation-id])
                         (assoc-in [:chat :conversation-id] (get-in event [:result :conversation-id])))
                       (cond-> (get-in event [:result :message])
                         (assoc-in [:chat :error] (get-in event [:result :message]))))
      db))))
(rf/reg-event-fx :chat/event
  (fn [{:keys [db]} [_ token event]]
    {:db (apply-chat-event db token event)
     :fx (when (= token (get-in db [:chat :current-id]))
           (cond-> [[:chat/scroll (not= false (get-in db [:ui :chat-autoscroll?]))]]
             (= "chat/done" (:type event)) (conj [:dispatch [:chat/history-load]])))}))
(rf/reg-event-db :chat/closed
  (fn [db [_ token]] (if (= token (get-in db [:chat :current-id])) (assoc-in db [:chat :running?] false) db)))
(rf/reg-event-db :chat/stopped
  (fn [db [_ token]] (if (= token (get-in db [:chat :current-id]))
                      (-> db (assoc-in [:chat :running?] false) (assoc-in [:chat :error] "Generation stopped. Accepted show commands remain in effect.")) db)))
(rf/reg-event-db :chat/stream-error
  (fn [db [_ token error]] (if (= token (get-in db [:chat :current-id]))
                            (-> db (assoc-in [:chat :running?] false) (assoc-in [:chat :error] (str error))) db)))
(rf/reg-event-fx :chat/cancel (fn [_ _] {:studio/chat-abort true}))
(rf/reg-event-db :chat/error
  (fn [db [_ error]] (-> db (assoc-in [:chat :running?] false) (assoc-in [:chat :saving?] false) (assoc-in [:chat :error] (str error)))))
(rf/reg-event-db :chat/clear
  (fn [db _] (if (get-in db [:chat :running?]) db (update db :chat dissoc :entries :history :history-request :conversation-id :conversation-status :error :current-id))))

(defn- provider-endpoint [form self-hosted?]
  [:label (if self-hosted? "Server endpoint" "Endpoint override")
   [:input#provider-endpoint {:type "url" :value (:endpoint form "")
                              :placeholder (if self-hosted? "http://localhost:11434/v1/chat/completions" "Use the provider’s default endpoint")
                              :on-change #(input! [:provider-form :endpoint] (text %))}]])

(defn- provider-fields [db]
  (let [form (get-in db [:ui :provider-form])
        config (get-in db [:chat :provider])
        provider (:provider form "openai")
        local? (= "self-hosted" provider)
        saved-key? (and (= provider (:provider config))
                        (or (:credential-configured? config) (:credential-persisted? config)))]
    [:form.form-body.provider-fields
     {:on-submit (fn [event] (.preventDefault event) (rf/dispatch [:chat/save-provider]))}
     (when (:credential-error config)
       [:p.chat-error {:role "status"} "The saved key is unavailable. Enter a key again or reconnect the server’s credential store."])
     [:div.provider-row
      [:label "Provider"
       [:select#provider-kind {:value provider :on-change #(rf/dispatch-sync [:chat/select-provider (text %)])}
        [:option {:value "openai"} "OpenAI"] [:option {:value "anthropic"} "Anthropic"]
        [:option {:value "openrouter"} "OpenRouter"] [:option {:value "self-hosted"} "Self-hosted / compatible"]]]
      [:label "Model" [:input#provider-model {:value (:model form "") :placeholder "Model identifier" :required true
                                              :on-change #(input! [:provider-form :model] (text %))}]]]
     (when local? [provider-endpoint form true])
     [:label (if local? "API key · optional" "API key")
      [:input#provider-credential {:type "password" :auto-complete "off" :value (:credential form "")
                                   :placeholder (cond saved-key? "Leave blank to keep the saved key"
                                                      local? "If your server requires a key"
                                                      :else "Provider API key")
                                   :on-change #(input! [:provider-form :credential] (text %))}]]
     (when-not local?
       [:details.provider-advanced
        [:summary "Custom endpoint"] [provider-endpoint form false]])
     (when saved-key?
       [:label.checkbox-label.provider-clear
        [:input {:type "checkbox" :checked (boolean (:clear-credential? form))
                 :on-change #(input! [:provider-form :clear-credential?] (.. % -target -checked))}]
        "Clear saved key"])
     (when-let [error (get-in db [:chat :provider-error])]
       [:p.chat-error {:role "alert"} error])
     [:div.provider-save
      [:button#save-provider.button.primary {:type "submit" :disabled (get-in db [:chat :saving?])}
       (if (get-in db [:chat :saving?]) "Saving…" "Save connection")]
      [:p.small.muted "Keys stay out of show exports. Your server stores them locally or for this session."]]]))

(defn- provider-form [db ready?]
  (let [config (get-in db [:chat :provider])]
    (if ready?
      [:details.panel.provider-panel
       [:summary [:span.section-label "Model connection"]
        [:span (str (:provider config) " · " (:model config))]]
       [provider-fields db]]
      [:section.panel.provider-panel.provider-setup {:aria-labelledby "provider-setup-title"}
       [:div.provider-intro [:h2#provider-setup-title "Connect a model"]]
       [provider-fields db]])))

(defn- tool-entry [tool]
  [:details.chat-tool
   [:summary [:span (if (= :running (:status tool)) "◌" "✓")] [:span (str/replace (:name tool "Tool") "_" " ")]
    [:span.small.muted (if (= :running (:status tool)) "running" "completed")]]
   [:pre (pretty/json (or (:result tool) (:arguments tool)))]])

(defn- conversation [db]
  (let [chat (:chat db)
        ready? (get-in chat [:provider :ready?])
        running? (:running? chat)
        has-history? (seq (:entries chat))]
    [:section.panel.chat-panel
       [:div.panel-top [:span.section-label (if (or ready? has-history?) "Conversation" "Draft a message")]
        (when (or has-history? (:conversation-id chat))
          [:button.text-button {:disabled running? :on-click #(rf/dispatch [:chat/clear])} "New conversation +"])]
       (when (or ready? has-history? running?)
         [:div#chat-log.chat-log {:role "log" :aria-live "polite" :aria-relevant "additions text"
                                :on-scroll #(let [log (.-currentTarget %)]
                                              (input! [:chat-autoscroll?] (< (- (.-scrollHeight log) (.-clientHeight log) (.-scrollTop log)) 80)))}
        (if (seq (:entries chat))
          (for [entry (:entries chat)]
            ^{:key (:id entry)}
            [:article.chat-message {:class (name (:role entry))}
             [:div.chat-author (if (= :user (:role entry)) "YOU" "TOLGLOW ASSISTANT")]
             (when (seq (:text entry)) [:div.chat-text (:text entry)])
             (for [tool (:tools entry)] ^{:key (:id tool)} [tool-entry tool])
             (when (and running? (= (:id entry) (:current-id chat)) (empty? (:text entry)) (empty? (:tools entry)))
               [:div.chat-thinking [:span "◌"] " Listening to the room…"])])
          [:div.chat-welcome [:span "✧"] [:h2 "Start with a mood."]
           [:p "Try “Build a mostly red, slowly evolving scene. Keep half the room dark and let one beam wander.”"]
           [:p.small.muted "The assistant uses validated show commands. Tool activity appears here, and you can keep performing from the cue grid."]])])
       (when-let [error (:error chat)] [:div.chat-error {:role "status"} error])
       (when (and (not running?) (contains? #{"interrupted" "cancelled" "failed" "error"} (:conversation-status chat)))
         [:div.chat-error {:role "status"} "The previous generation stopped. Send a message when you want to continue this conversation."])
       [:form.chat-compose {:on-submit (fn [event] (.preventDefault event) (rf/dispatch [:chat/send]))}
        [:label.sr-only {:html-for "chat-prompt"} "Tell the lighting assistant what to do"]
        [:textarea#chat-prompt {:value (get-in db [:ui :chat-draft] "") :rows 3
                               :placeholder (if ready? "What should the room feel like?" "Draft a message while you set up your model…")
                               :max-length 32768 :on-change #(input! [:chat-draft] (text %))
                               :on-key-down (fn [event]
                                              (when (and (= "Enter" (.-key event)) (or (.-ctrlKey event) (.-metaKey event)))
                                                (.preventDefault event) (rf/dispatch [:chat/send])))}]
        [:div.chat-compose-actions [:span.small.muted (if ready? "Ctrl / ⌘ + Enter to send" "Connect a model to send")]
         (if running?
           [:button.button {:type "button" :on-click #(rf/dispatch [:chat/cancel])} "Stop generation"]
           [:button#chat-send.button.primary {:type "submit" :disabled (or (not ready?) (str/blank? (get-in db [:ui :chat-draft] "")))} "Send"])]]]))

(defn- conversation-library [chat]
  (let [running? (:running? chat)]
    [:section.panel.conversation-library
        [:div.panel-top [:span.section-label "SAVED CONVERSATIONS"]]
        [:div.form-body
         [:label.sr-only {:html-for "chat-conversation"} "Choose a saved conversation"]
         [:select#chat-conversation {:value (:conversation-id chat "") :disabled running?
                                     :on-change #(rf/dispatch [:chat/select (text %)])}
          [:option {:value ""} "New conversation"]
          (for [conversation (:conversations chat)]
            ^{:key (:id conversation)} [:option {:value (:id conversation)} (or (:title conversation) "Untitled conversation")])]
         [:p.small.muted "Conversations are restored from the local server. Each new request receives the current show context."]]]))

(defn- show-context [db]
  [:div.chat-context.panel [:span.section-label "Shared context"]
   [:p (str (count (get-in db [:show :rig :fixtures])) " fixtures · " (count (get-in db [:show :cues])) " cues")]
   [:p.small.muted "The current rig, definitions, active effects and output mode accompany each message."]])

(defn panel [db]
  (let [chat (:chat db)
        ready? (get-in chat [:provider :ready?])]
    [:section#view-assistant.view {:hidden (not= :assistant (get-in db [:ui :view]))}
     [:div.view-heading [:h1 "Show assistant"]]
     [:div.chat-layout {:class (when-not ready? "assistant-setup")}
      (when-not ready?
        (if (:provider chat)
          [provider-form db false]
          [:section.panel.provider-setup.connection-status
           (if-let [error (:provider-error chat)]
             [:<> [:p {:role "alert"} error]
              [:button.button {:on-click #(rf/dispatch [:chat/load])} "Retry connection settings"]]
             [:p {:role "status"} "Loading model connection…"])]))
      [conversation db]
      [:aside.chat-settings
       (when ready? [provider-form db true])
       (when (or ready? (seq (:conversations chat))) [conversation-library chat])
       [show-context db]]]]))
