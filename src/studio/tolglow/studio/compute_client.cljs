(ns tolglow.studio.compute-client
  "Explicit, foreground-only participation in the local lighting host's compute pool."
  (:require [re-frame.core :as rf]
            [reagent.core :as r]))

(defonce ^:private current (atom nil))

(rf/reg-event-db ::status (fn [db [_ phase message]] (-> db (assoc-in [:ui ::status] [phase message]) (update :ui dissoc ::progress))))
(rf/reg-sub ::status (fn [db _] (get-in db [:ui ::status] ["stopped" "Disconnected"])))

(rf/reg-event-db ::progress (fn [db [_ replies rows]] (assoc-in db [:ui ::progress] [replies rows])))
(rf/reg-sub ::progress (fn [db _] (get-in db [:ui ::progress])))

(defn stop! [phase reason]
  (when-let [{:keys [worker on-hide on-visibility]} @current]
    (reset! current nil)
    (.removeEventListener js/document "visibilitychange" on-visibility)
    (.removeEventListener js/window "pagehide" on-hide)
    (.terminate worker))
  (rf/dispatch [::status phase reason]))

(defn- worker-message! [worker event]
  (when (identical? worker (:worker @current))
    (let [data (.-data event) phase (aget data "phase") message (aget data "message")]
      (case phase
        "progress" (rf/dispatch [::progress (aget data "replies") (aget data "rows")])
        "active" (rf/dispatch [::status phase message])
        (stop! phase message)))))

(defn- join! [token]
  (when-not (or @current (.-hidden js/document))
    (try
      (let [worker (js/Worker. "/studio/compute/runtime/transport.js")
            hide #(stop! "stopped" "Left the pool when Studio went into the background")
            visibility #(when (.-hidden js/document) (hide))]
        (reset! current {:worker worker :on-hide hide :on-visibility visibility})
        (.addEventListener js/window "pagehide" hide)
        (.addEventListener js/document "visibilitychange" visibility)
        (set! (.-onerror worker) #(when (identical? worker (:worker @current))
                                    (stop! "error" "Compute worker unavailable")))
        (set! (.-onmessage worker) (partial worker-message! worker))
        ;; Credentials only cross the local worker port. Never put them in
        ;; re-frame events, app-db, persisted settings, query strings or logs.
        (.postMessage worker #js {:op "join" :token token})
        (rf/dispatch [::status "joining" "Testing this GPU…"]))
      (catch :default _ (stop! "error" "Compute worker unavailable")))))

(defn panel []
  (r/with-let [field (atom nil)]
    (let [[phase message] @(rf/subscribe [::status])
          [replies rows] @(rf/subscribe [::progress])
          joined? (#{"joining" "active"} phase)]
      [:section.panel
       [:div.panel-top [:span.section-label "RENDER COMPUTE"]]
       [:div.form-body
        [:p {:role "status"} message]
        (when replies
          [:p (str replies " results sent · " rows " fixture rows")])
        (if joined?
          [:button.button {:type "button" :on-click #(stop! "stopped" "Disconnected")} "Leave pool"]
          [:form {:on-submit (fn [e]
                               (.preventDefault e)
                               (let [input @field token (.-value input)]
                                 (set! (.-value input) "")
                                 (join! token)))}
           [:label "Compute token"
            [:input {:type "password" :required true :min-length 24 :max-length 1024
                     :ref #(reset! field %) :auto-complete "off"}]]
           [:button.button {:type "submit"} "Contribute this GPU"]])]])))
