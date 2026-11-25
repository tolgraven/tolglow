(ns tolglow.studio.state-socket
  "Own the Studio browser's single state subscription and reconnect timer.
  Each callback carries its original connection identity through re-frame."
  (:require [re-frame.core :as rf]
            [reagent.core :as reagent]
            [reagent.ratom :as ratom]
            [tolglow.studio.reactions]
            [tolglow.transit :as transit]))

(defonce ^:private owner (atom nil))

(defonce ^:private demand (atom {}))

(defonce ^:private connections-open? (atom false))

(defonce ^:private preview-surfaces (atom #{}))

(defn- current? [id] (and id (= id (:id @owner))))

(defn- close-socket! [reason]
  (let [{:keys [socket retry heartbeat]} @owner]
    (when @owner (swap! owner dissoc :id :socket :retry :heartbeat :recovering?))
    (js/clearTimeout retry)
    (js/clearInterval heartbeat)
    (when socket
      (set! (.-onmessage socket) nil)
      (set! (.-onclose socket) nil)
      (.close socket 1000 reason))))

(defn stop! []
  (when-let [{:keys [hide show visibility]} @owner]
    (.removeEventListener js/document "visibilitychange" visibility)
    (.removeEventListener js/window "pagehide" hide)
    (.removeEventListener js/window "pageshow" show)
    (close-socket! "Studio closed")
    (reset! owner nil)))

(defn reply! [[id message]]
  (let [{:keys [socket recovering?]} @owner
        op (:op message)]
    (when (and (current? id) socket (= 1 (.-readyState socket)))
      (try
        (if (and (= :resync op) recovering?)
          (.close socket 4000 "State resynchronization failed")
          (do
            (.send socket (transit/encode message))
            (swap! owner assoc :last-sent (.now js/performance))
            (case op
              :ack (swap! owner assoc :attempt 0 :recovering? false)
              :resync (swap! owner assoc :recovering? true)
              nil)))
        (catch :default _ (.close socket 4000 "State send failed"))))))

(rf/reg-fx :studio/report-client-fault
  (fn [fault]
    (when-let [id (:id @owner)] (reply! [id {:op :client-fault :fault fault}]))))

(defn visible-queries [requested visible? external?]
  (cond visible? requested
        external? (into #{} (filter #{[:spatial/scene]}) requested)
        :else #{}))

(defn- update-interest! []
  (let [{:keys [id socket subscribed? queries]} @owner]
    (when (and id socket (= 1 (.-readyState socket)))
      (let [visible? (not (.-hidden js/document))
            enabled? (and @connections-open? visible?)
            wanted (visible-queries (set (keys @demand)) visible? (seq @preview-surfaces))]
        (when (not= (boolean subscribed?) enabled?)
          (reply! [id {:op :connections :enabled? enabled?}])
          (swap! owner assoc :subscribed? enabled?))
        (when (not= wanted (or queries #{}))
          (reply! [id {:op :queries :queries (vec wanted)}])
          (swap! owner assoc :queries wanted))))))

(defn present-preview!
  ([enabled?] (present-preview! :pip enabled?))
  ([surface enabled?]
   (swap! preview-surfaces (if enabled? conj disj) surface)
   (update-interest!)))

(defn- release! [query]
  (swap! demand (fn [refs]
                  (let [n (get refs query 0)]
                    (if (<= n 1) (dissoc refs query) (assoc refs query (dec n))))))
  (update-interest!))

(rf/reg-sub-raw :studio/remote-result
  (fn [db [_ query]]
    (let [active? (volatile! false)
          value (reagent/cursor db [:server-projection :view :subscriptions query])]
      (ratom/make-reaction
        (fn []
          (when-not @active?
            (vreset! active? true)
            (swap! demand update query (fnil inc 0))
            (update-interest!))
          @value)
        :on-dispose (fn [_] (when @active? (release! query)))))))

(rf/reg-sub :studio/remote
  (fn [[_ query] _] (rf/subscribe [:studio/remote-result query]))
  (fn [result _] (:value result)))

(rf/reg-fx :studio/connections-interest
  (fn [enabled?] (reset! connections-open? enabled?) (update-interest!)))

(declare connect!)
(defn- disconnected! [id reason]
  (when (current? id)
    (let [{:keys [session attempt heartbeat]} @owner]
      (js/clearInterval heartbeat)
      (rf/dispatch [:studio/stream-closed session id reason])
      (swap! owner #(-> % (dissoc :socket :id :heartbeat :recovering?)
                          (assoc :attempt (min 5 (inc attempt))
                                 :retry (js/setTimeout connect! (min 5000 (* 250 (js/Math.pow 2 attempt))))))))))

(defn- connect! []
  (when-let [{:keys [session]} @owner]
    (let [id (str (random-uuid))
          url (js/URL. "/api/state/stream" (.-href js/location))]
      (set! (.-protocol url) (if (= "https:" (.-protocol url)) "wss:" "ws:"))
      (.set (.-searchParams url) "session" session)
      (swap! owner assoc :id id :retry nil :queries #{} :subscribed? false :last-sent (.now js/performance))
      (rf/dispatch [:studio/stream-opening session id])
      (try
        (let [socket (js/WebSocket. (.-href url))]
          (swap! owner assoc :socket socket)
          (set! (.-onopen socket)
                (fn []
                  (when (current? id)
                    (update-interest!)
                    (swap! owner assoc :heartbeat
                           (js/setInterval
                             (fn []
                               (when (and (current? id)
                                          (>= (- (.now js/performance) (:last-sent @owner)) 3000))
                                 (reply! [id {:op :heartbeat}]))) 3000)))))
          (set! (.-onmessage socket)
                (fn [event]
                  (when (current? id)
                    (try (rf/dispatch [:studio/stream-frame session id (transit/decode (.-data event))])
                         (catch :default _ (reply! [id {:op :resync}]))))))
          (set! (.-onclose socket)
                (fn [event] (disconnected! id (or (not-empty (.-reason event)) "Connection closed")))))
        (catch :default error (disconnected! id (.-message error)))))))

(defn start! [session]
  (stop!)
  (let [hide (fn [_] (close-socket! "Page hidden"))
        visibility (fn [_] (update-interest!))
        show (fn [event] (when (and (.-persisted event) (nil? (:id @owner))) (connect!)))]
    (reset! owner {:session session :attempt 0 :hide hide :show show :visibility visibility})
    (.addEventListener js/document "visibilitychange" visibility)
    (.addEventListener js/window "pagehide" hide)
    (.addEventListener js/window "pageshow" show)
    (connect!)))

(rf/reg-fx :studio/open-stream start!)
(rf/reg-fx :studio/stream-reply reply!)
