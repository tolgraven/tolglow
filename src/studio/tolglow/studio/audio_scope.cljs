(ns tolglow.studio.audio-scope
  "A visible capture scope owns one bounded read request. It never opens audio."
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.events :as events]
            [tolglow.studio.parameter-widgets :as widgets]))

(defonce request (atom nil))
(defn token [db]
  [(get-in db [:show :runtime-id]) (get-in db [:show :epoch])
   (get-in db [:audio-scope :generation] 0)])
(defn current? [db request-token]
  (and (get-in db [:audio-scope :active?]) (= request-token (token db))))
(defn cancel! []
  (when-let [controller @request] (.abort controller))
  (reset! request nil))
(rf/reg-fx :audio-scope/cancel (fn [_] (cancel!)))
(rf/reg-fx :audio-scope/request
  (fn [request-token]
    (cancel!)
    (let [controller (js/AbortController.) timeout (js/setTimeout #(.abort controller) 3000)]
      (reset! request controller)
      (-> (js/fetch "/api/audio" #js {:cache "no-store" :signal (.-signal controller)})
          (.then (fn [response]
                   (if (.-ok response) (.json response)
                     (throw (js/Error. (str "Audio status returned " (.-status response)))))))
          (.then #(rf/dispatch [:audio-scope/received request-token (js->clj % :keywordize-keys true) (.-hidden js/document)]))
          (.catch #(when (identical? controller @request)
                     (rf/dispatch [:audio-scope/failed request-token (events/error-message %)])))
          (.finally #(do (js/clearTimeout timeout)
                         (when (identical? controller @request) (reset! request nil))))))))
(rf/reg-event-fx :audio-scope/open
  (fn [{:keys [db]} _]
    {:db (assoc db :audio-scope {:active? true :pending? false
                                :generation (inc (get-in db [:audio-scope :generation] 0))})
     :dispatch [:audio-scope/poll]}))
(rf/reg-event-fx :audio-scope/close
  (fn [{:keys [db]} _]
    {:db (-> db (assoc-in [:audio-scope :active?] false) (assoc-in [:audio-scope :pending?] false)
             (update-in [:audio-scope :generation] (fnil inc 0)))
     :audio-scope/cancel true}))
(rf/reg-event-fx :audio-scope/runtime-change
  (fn [{:keys [db]} _]
    {:db (update db :audio-scope dissoc :waveform :error)
     :audio-scope/cancel true
     :dispatch (when (get-in db [:audio-scope :active?]) [:audio-scope/open])}))
(rf/reg-event-fx :audio-scope/poll
  (fn [{:keys [db]} [_ expected]]
    (when (and (get-in db [:audio-scope :active?]) (not (get-in db [:audio-scope :pending?]))
               (or (nil? expected) (= expected (token db))))
      {:db (assoc-in db [:audio-scope :pending?] true) :audio-scope/request (token db)})))
(rf/reg-event-fx :audio-scope/received
  (fn [{:keys [db]} [_ request-token data hidden?]]
    (when (current? db request-token)
      {:db (update db :audio-scope #(-> % (assoc :pending? false :waveform (:waveform data)
                                               :capture-state (:state data) :running? (:running? data)) (dissoc :error)))
       :dispatch-later [{:ms (if (and (:running? data) (not hidden?)) 100 1000)
                         :dispatch [:audio-scope/poll request-token]}]})))
(rf/reg-event-fx :audio-scope/failed
  (fn [{:keys [db]} [_ request-token error]]
    (when (current? db request-token)
      {:db (update db :audio-scope assoc :pending? false :error error :waveform nil)
       :dispatch-later [{:ms 1000 :dispatch [:audio-scope/poll request-token]}]})))
(rf/reg-sub :audio-scope/state (fn [db _] (:audio-scope db)))

(def panel
  (r/create-class
   {:display-name "CapturedAudioScope"
    :component-did-mount (fn [_] (rf/dispatch [:audio-scope/open]))
    :component-will-unmount (fn [_] (rf/dispatch [:audio-scope/close]))
    :reagent-render
    (fn []
      (let [{:keys [waveform error running?]} @(rf/subscribe [:audio-scope/state])]
        [:section.capture-scope {:aria-label "Live audio capture"}
         [:div.capture-scope-heading [:span.section-label "CAPTURED AUDIO"]
          [:span (if running? "Live input" "Capture stopped")]]
         [widgets/audio-waveform waveform]
         (when error [:p.connection-error {:role "status"} error])]))}))
