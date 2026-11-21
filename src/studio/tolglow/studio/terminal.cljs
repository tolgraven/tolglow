(ns tolglow.studio.terminal
  "An actual xterm.js view over owned Docker PTYs. All UI state is in re-frame;
  terminal instances and ResizeObservers have explicit component lifetimes."
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            ["@xterm/xterm" :refer [Terminal]]
            ["@xterm/addon-fit" :refer [FitAddon]]
            [tolglow.studio.events :as events]))

(def defaults {:generation 0 :session-id nil :status "closed" :cursor 0
               :input "" :sending? false :polling? false :mounted? false})
(defonce resources (atom {}))
(defn state [db key] (merge defaults (get-in db [:terminals key])))
(defn token [db key] [(get-in db [:show :runtime-id]) key (:generation (state db key)) (:session-id (state db key))])
(defn current? [db key captured] (= captured (token db key)))
(defn append-input [pending text]
  (let [next (str pending text)]
    (when (> (.-byteLength (.encode (js/TextEncoder.) next)) 65536)
      (throw (js/Error. "Terminal input is full. Wait for the current input to be delivered.")))
    next))
(defn input-prefix [text]
  (let [n (min 4096 (count text)) code (when (pos? n) (.charCodeAt text (dec n)))
        n (if (and (< n (count text)) (<= 0xD800 code 0xDBFF)) (dec n) n)]
    (subs text 0 n)))
(defn encode [text]
  (let [bytes (.encode (js/TextEncoder.) text)]
    (js/btoa (apply str (map #(js/String.fromCharCode %) (array-seq bytes))))))
(defn decode [base64]
  (let [text (js/atob base64) bytes (js/Uint8Array. (count text))]
    (dotimes [index (count text)] (aset bytes index (.charCodeAt text index))) bytes))
(defn- response-error [error] (events/error-message error))
(defn- request [db key operation body]
  {:path (str "/api/terminal/" (name operation))
   :body (merge {:runtime-id (get-in db [:show :runtime-id]) :session-id (:session-id (state db key))} body)
   :on-success [:terminal/received key (token db key) operation]
   :on-failure [:terminal/failed key (token db key) operation]})

(rf/reg-sub :terminal/state (fn [db [_ key]] (state db key)))
(rf/reg-fx :terminal/write
  (fn [{:keys [key data dropped?]}]
    (when-let [terminal (:terminal (get @resources key))]
      (when dropped? (.write terminal "\r\n\u001b[33m[Earlier terminal output expired]\u001b[0m\r\n"))
      (when (seq data) (.write terminal (decode data))))))
(rf/reg-fx :terminal/focus (fn [key] (when-let [term (:terminal (get @resources key))] (.focus term))))
(rf/reg-fx :terminal/reset (fn [_] (doseq [[_ {:keys [terminal]}] @resources] (.reset terminal))))
(rf/reg-fx :terminal/fit
  (fn [key]
    (when-let [{:keys [terminal fit]} (get @resources key)]
      (js-invoke fit "fit")
      (rf/dispatch [:terminal/resize key (.-rows terminal) (.-cols terminal)]))))
(rf/reg-event-fx :terminal/mount
  (fn [{:keys [db]} [_ key]]
    {:db (assoc-in db [:terminals key] (-> (state db key) (assoc :mounted? true :cursor 0 :polling? false :draining? true) (update :generation inc)))
     :dispatch [:terminal/poll key]}))
(rf/reg-event-db :terminal/unmount
  (fn [db [_ key]] (update-in db [:terminals key] #(-> (merge defaults %) (assoc :mounted? false :polling? false :input "" :sending? false) (update :generation inc)))))
(rf/reg-event-fx :terminal/runtime-change
  (fn [{:keys [db]} _]
    {:db (update db :terminals #(into {} (map (fn [[key s]]
                                               [key (assoc defaults :generation (inc (:generation s 0)) :mounted? (:mounted? s))])) %))
     :terminal/reset true}))
(rf/reg-event-fx :terminal/open
  (fn [{:keys [db]} [_ key workspace-id repo]]
    (let [s (state db key)]
      (when-not (#{"opening" "running"} (:status s))
        (let [db (assoc-in db [:terminals key] (merge defaults {:mounted? true :generation (inc (:generation s))
                                                              :status "opening" :workspace-id workspace-id :repo repo}))]
          {:db db :studio/http (request db key :open {:workspace-id workspace-id :repo (events/wire-id repo)
                                                     :rows 24 :cols 80})})))))
(rf/reg-event-fx :terminal/poll
  (fn [{:keys [db]} [_ key]]
    (let [s (state db key)]
      (when (and (:mounted? s) (:session-id s) (or (= "running" (:status s)) (:draining? s)) (not (:polling? s)))
        {:db (assoc-in db [:terminals key :polling?] true)
         :studio/http {:path (str "/api/terminal?session-id=" (js/encodeURIComponent (:session-id s)) "&cursor=" (:cursor s))
                       :on-success [:terminal/output key (token db key)]
                       :on-failure [:terminal/failed key (token db key) :poll]}}))))
(rf/reg-event-fx :terminal/output
  (fn [{:keys [db]} [_ key captured response]]
    (when (current? db key captured)
      (let [s (state db key) newer? (> (:cursor response 0) (:cursor s))
            s (assoc s :cursor (max (:cursor s) (:cursor response 0)) :polling? false
                       :status (:state response (:status s)) :exit-code (:exit-code response)
                       :draining? (< (:cursor response 0) (:available-cursor response 0)))]
        {:db (assoc-in db [:terminals key] s)
         :terminal/write {:key key :data (when newer? (:data response)) :dropped? (and newer? (:dropped? response))}
         :dispatch-later (when (and (:mounted? s) (or (= "running" (:status s)) (< (:cursor s) (:available-cursor response (:cursor s)))))
                           [{:ms 80 :dispatch [:terminal/poll key]}])}))))
(rf/reg-event-fx :terminal/input
  (fn [{:keys [db]} [_ key text]]
    (let [s (state db key)]
      (when (= "running" (:status s))
        (try {:db (assoc-in db [:terminals key :input] (append-input (:input s) text))
              :dispatch [:terminal/send key]}
             (catch :default error {:db (assoc-in db [:terminals key :error] (response-error error))}))))))
(rf/reg-event-fx :terminal/send
  (fn [{:keys [db]} [_ key]]
    (let [s (state db key)]
      (when (and (= "running" (:status s)) (seq (:input s)) (not (:sending? s)))
        ;; The backend accepts bounded bytes. Keep one request in flight so
        ;; pasted commands and escape sequences retain their exact order.
        (let [part (input-prefix (:input s))
              db (assoc-in db [:terminals key] (assoc s :input (subs (:input s) (count part)) :sending? true))]
          {:db db :studio/http (request db key :input {:data (encode part)})})))))
(rf/reg-event-fx :terminal/resize
  (fn [{:keys [db]} [_ key rows cols]]
    (let [s (state db key)]
      (when (and (= "running" (:status s)) (<= 2 rows 200) (<= 10 cols 400)
                 (not= [rows cols] (:dimensions s)))
        {:db (assoc-in db [:terminals key :dimensions] [rows cols])
         :studio/http (request db key :resize {:rows rows :cols cols})}))))
(rf/reg-event-fx :terminal/close
  (fn [{:keys [db]} [_ key]]
    (let [s (state db key)]
      (when (:session-id s)
        {:db (assoc-in db [:terminals key] (assoc s :status "closing" :input ""))
         :studio/http (request db key :close {})}))))
(rf/reg-event-fx :terminal/received
  (fn [{:keys [db]} [_ key captured operation response]]
    (if-not (current? db key captured)
      ;; An open acknowledgement can arrive after replacement of the runtime
      ;; projection. Dispose that exact session; never attach it to a new tab.
      (when (and (= operation :open) (:session-id response))
        {:studio/http {:path "/api/terminal/close" :body {:session-id (:session-id response) :runtime-id (first captured)}}})
      (let [s (state db key)]
        (case operation
          :open {:db (assoc-in db [:terminals key] (merge s (select-keys response [:session-id]) {:status (:state response) :error nil}))
                 :fx [[:terminal/fit key] [:terminal/focus key] [:dispatch [:terminal/poll key]]]}
          :input {:db (assoc-in db [:terminals key :sending?] false) :dispatch [:terminal/send key]}
          :close {:db (assoc-in db [:terminals key] (assoc s :status "closed" :session-id nil :input "" :polling? false :sending? false))}
          :resize {} {})))))
(rf/reg-event-fx :terminal/failed
  (fn [{:keys [db]} [_ key captured operation error]]
    (when (current? db key captured)
      (let [s (state db key)]
        {:db (assoc-in db [:terminals key]
                       (cond-> (assoc s :error (response-error error) :polling? false)
                         (= operation :open) (assoc :status "closed")
                         (= operation :input) (assoc :input "" :sending? false)))
         ;; Input is never replayed automatically: delivery may have succeeded
         ;; before its acknowledgement was lost.
         :dispatch-later (when (and (= operation :poll) (:mounted? s) (= "running" (:status s)))
                           [{:ms 1500 :dispatch [:terminal/poll key]}])}))))

(defn viewport [key]
  (let [node (atom nil)]
    (r/create-class
     {:display-name "Docker terminal"
      :component-did-mount
      (fn [_]
        (let [terminal (Terminal. #js {:cursorBlink true :fontSize 13 :scrollback 3000
                                       :fontFamily "'SFMono-Regular', Consolas, monospace"
                                       ;; Exact Bruvbox dark terminal_color_0..15 mapping;
                                       ;; native source/provenance lives in vendor/neovim/bruvbox.
                                       :theme #js {:background "#262525" :foreground "#f0e3ba" :cursor "#f0e3ba"
                                                   :selectionBackground "#46494c" :black "#3a393d" :red "#d18479"
                                                   :green "#b4b88d" :yellow "#dabd7a" :blue "#7692ab"
                                                   :magenta "#bd979d" :cyan "#91b8a4" :white "#c6bc9b"
                                                   :brightBlack "#46494c" :brightRed "#cca687" :brightGreen "#8b8c63"
                                                   :brightYellow "#d0af80" :brightBlue "#80a0b3" :brightMagenta "#af8589"
                                                   :brightCyan "#9ab499" :brightWhite "#f0e3ba"}})
              fit (FitAddon.)
              observer (js/ResizeObserver. #(rf/dispatch [:terminal/fit key]))]
          (.loadAddon terminal fit) (.open terminal @node)
          (swap! resources assoc key {:terminal terminal :fit fit :observer observer
                                      :input (.onData terminal #(rf/dispatch [:terminal/input key %]))})
          (.observe observer @node)
          (rf/dispatch [:terminal/mount key])))
      :component-will-unmount
      (fn [_]
        (when-let [{:keys [terminal input observer]} (get @resources key)]
          (.disconnect observer) (.dispose input) (.dispose terminal) (swap! resources dissoc key))
        (rf/dispatch [:terminal/unmount key]))
      :reagent-render (fn [_] [:div.docker-terminal-screen {:ref #(reset! node %) :aria-label "Docker shell terminal"}])})))

(rf/reg-event-fx :terminal/fit (fn [_ [_ key]] {:terminal/fit key}))

(defn panel [workspace-id repo]
  (let [key (str (events/wire-id workspace-id) "/" (events/wire-id repo))
        s @(rf/subscribe [:terminal/state key])]
    [:section.docker-terminal
     [:div.docker-terminal-toolbar
      [:div [:strong "Terminal"] [:span.status-pill (:status s)]]
      [:div
       (if (#{"running" "closing"} (:status s))
         [:button.text-button {:disabled (= "closing" (:status s)) :on-click #(rf/dispatch [:terminal/close key])} "End shell"]
         [:button.primary-button {:disabled (= "opening" (:status s))
                                  :on-click #(rf/dispatch [:terminal/open key workspace-id repo])} "Open Docker shell"])
       [:button.text-button {:disabled (not= "running" (:status s)) :on-click #(rf/dispatch [:terminal/input key "\u0003"])} "Ctrl-C"]]]
     [:p.muted "zsh · " (events/wire-id repo) " · commands run in this application's development workspace"]
     (when (:error s) [:p.form-error {:role "alert"} (:error s)])
     ^{:key key} [viewport key]
     (when (some? (:exit-code s)) [:p.muted "Process exited with code " (:exit-code s)])]))
