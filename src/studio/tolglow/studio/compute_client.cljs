(ns tolglow.studio.compute-client
  "Explicit, foreground-only participation in the local lighting host's compute pool."
  (:require [re-frame.core :as rf]
            [reagent.core :as r]))

(defonce ^:private current (atom nil))

(rf/reg-event-db ::status (fn [db [_ phase message]] (assoc-in db [:ui ::status] [phase message])))
(rf/reg-sub ::status (fn [db _] (get-in db [:ui ::status] ["stopped" "Disconnected"])))

(defn stop! [phase reason]
  (when-let [{:keys [worker on-hide on-visibility]} @current]
    (reset! current nil)
    (.removeEventListener js/document "visibilitychange" on-visibility)
    (.removeEventListener js/window "pagehide" on-hide)
    (.terminate worker))
  (rf/dispatch [::status phase reason]))

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
        (set! (.-onmessage worker)
              (fn [event]
                (when (identical? worker (:worker @current))
                  (let [message (.-data event) phase (aget message "phase") reason (aget message "message")]
                    (if (= phase "active") (rf/dispatch [::status phase reason])
                        (stop! phase reason))))))
        ;; Credentials only cross the local worker port. Never put them in
        ;; re-frame events, app-db, persisted settings, query strings or logs.
        (.postMessage worker #js {:op "join" :token token})
        (rf/dispatch [::status "joining" "Testing this GPU…"]))
      (catch :default _ (stop! "error" "Compute worker unavailable")))))

(defn panel []
  (r/with-let [field (atom nil)]
    (let [[phase message] @(rf/subscribe [::status])
          joined? (#{"joining" "active"} phase)]
      [:section.panel
       [:div.panel-top [:span.section-label "RENDER COMPUTE"]]
       [:div.form-body
        [:p {:role "status"} message]
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
