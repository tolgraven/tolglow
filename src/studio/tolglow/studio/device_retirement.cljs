(ns tolglow.studio.device-retirement
  "Device sessions a restore, import or restart retired (#905, #907). Reads
  :status :device-retirement, the same map the REPL reads from show/snapshot:
  counters, the IDs still closing, the last close or reopen error and the
  last connect refused with :device/closing, from whichever surface asked,
  in the REPL's message and code. Read-only: Studio does not open devices.
  The status changes only when a session retires, closes, is refused or
  connects, never per frame, and this panel subscribes to that slice alone."
  (:require [re-frame.core :as rf]
            [tolglow.studio.events :as events]))

(def counters
  "Counter key, label and whether a non-zero value is a problem."
  [[:pending "Closing" false] [:retired "Closed" false] [:failed "Close failed" true]
   [:reopen-failed "Reopen failed" true] [:abandoned "Abandoned at stop" true]
   [:late "Closed after stop" false] [:refused "Refused connects" false]])

(defn closing-ids
  "The session IDs still closing, sorted."
  [{:keys [closing]}]
  (vec (sort closing)))

(defn refusal
  "A projected refusal as {:message :code :session-id}."
  [data]
  (select-keys data [:message :code :session-id]))

(defn code-text
  "A :code as the REPL prints it, e.g. :device/closing."
  [code]
  (when (some? code) (str ":" (if (keyword? code) (subs (str code) 1) code))))

(rf/reg-sub ::status (fn [db _] (get-in db [:show :status :device-retirement])))

(defn- refusal-line [{:keys [message code session-id]}]
  [:span.retirement-refusal-text
   [:span message]
   (when code [:code.retirement-code (code-text code)])
   (when session-id [:span.retirement-id session-id])])

(defn panel
  "Connections panel for retired device sessions."
  []
  (let [status @(rf/subscribe [::status])
        skin (some-> @(rf/subscribe [:ui/knob-skin]) name)
        ids (closing-ids status)
        {:keys [last-error last-refusal]} status]
    [:section.panel.device-retirement {:aria-label "Device sessions" :data-skin skin}
     [:div.panel-top [:span.section-label "DEVICE SESSIONS"]
      [:span.connection-chip {:class (when (empty? ids) "active")} [:i]
       (cond (nil? status) "Waiting for the show"
             (seq ids) (str (count ids) " still closing")
             :else "None closing")]]
     [:div.form-body
      [:p.small.muted "A restore, import or restart retires every connected device session. Each closes on its own lane, so a stuck display or MIDI write holds only that session, and its ID cannot connect again until it has closed."]
      (when status
        (into [:dl.retirement-counts]
              (for [[key label problem?] counters
                    :let [value (get status key 0)]]
                ^{:key key}
                [:div.retirement-count {:class (when (and problem? (pos? value)) "problem")}
                 [:dt label] [:dd value]])))
      (when (seq ids)
        (into [:ul.retirement-sessions {:aria-label "Device sessions still closing"}]
              (for [id ids]
                ^{:key id}
                [:li.retirement-session.closing
                 [:div.retirement-session-line [:span.retirement-id id] [:span.retirement-state "Closing"]]])))
      (when last-error
        [:p.connection-error {:role "status"}
         (str (case (events/wire-id (:stage last-error))
                "reopen" "Last reopen failed: " "lane" "Last retirement could not start: " "Last close failed: ")
              (:message last-error) (when-let [c (:class last-error)] (str " (" c ")")))])
      (when last-refusal
        [:p.retirement-refusal {:role "status"} [:span.small.muted "Last refused connect · "]
         [refusal-line (refusal last-refusal)]])
      [:p.small.muted "At the REPL: (get-in (show/snapshot runtime) [:status :device-retirement])."]]]))
