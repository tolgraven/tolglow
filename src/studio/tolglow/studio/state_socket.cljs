(ns tolglow.studio.state-socket
  "Own the Studio browser's single state subscription and reconnect timer.
  Each callback carries its original connection identity through re-frame."
  (:require [re-frame.core :as rf]
            [tolglow.transit :as transit]))

(defonce ^:private owner (atom nil))
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
  (when-let [{:keys [hide show]} @owner]
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
      (swap! owner assoc :id id :retry nil :last-sent (.now js/performance))
      (rf/dispatch [:studio/stream-opening session id])
      (try
        (let [socket (js/WebSocket. (.-href url))]
          (swap! owner assoc :socket socket)
          (set! (.-onopen socket)
                (fn []
                  (when (current? id)
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
        show (fn [event] (when (and (.-persisted event) (nil? (:id @owner))) (connect!)))]
    (reset! owner {:session session :attempt 0 :hide hide :show show})
    (.addEventListener js/window "pagehide" hide)
    (.addEventListener js/window "pageshow" show)
    (connect!)))

(rf/reg-fx :studio/open-stream start!)
(rf/reg-fx :studio/stream-reply reply!)
