(ns tolglow.studio.debug-socket
  "The Debug page owns one same-origin socket, its replay cursor and reconnect timer."
  (:require [re-frame.core :as rf]
            [tolglow.transit :as transit]))

(defonce ^:private owner (atom nil))

(defn- current? [handle generation]
  (and (= handle (:handle @owner)) (= generation (:generation @owner))))

(defn- suspend! []
  (let [{:keys [socket retry]} @owner]
    (swap! owner dissoc :generation :socket :retry)
    (js/clearTimeout retry)
    (when socket
      (set! (.-onmessage socket) nil)
      (set! (.-onclose socket) nil)
      (.close socket 1000 "Leaving Debug"))))

(defn- stop! []
  (when-let [{:keys [hide show]} @owner]
    (.removeEventListener js/window "pagehide" hide)
    (.removeEventListener js/window "pageshow" show)
    (suspend!)
    (reset! owner nil)))

(declare connect!)

(defn- retry! [handle generation reason]
  (when (current? handle generation)
    (let [attempt (:attempt @owner 0)]
      (swap! owner #(-> % (dissoc :socket :generation)
                        (assoc :attempt (min 5 (inc attempt))
                               :retry (js/setTimeout connect! (min 8000 (* 300 (js/Math.pow 2 attempt)))))))
      (rf/dispatch [:debug/disconnected handle reason]))))

(defn- connect! []
  (when-let [{:keys [handle cursor runtime]} @owner]
    (let [generation (random-uuid)
          url (js/URL. "/api/debug/events" (.-href js/location))]
      (set! (.-protocol url) (if (= "https:" (.-protocol url)) "wss:" "ws:"))
      (when (and runtime (some? cursor))
        (.set (.-searchParams url) "runtime" runtime)
        (.set (.-searchParams url) "after" (str cursor)))
      (swap! owner assoc :generation generation :retry nil)
      (try
        (let [socket (js/WebSocket. (.-href url))]
          (swap! owner assoc :socket socket)
          (set! (.-onopen socket)
                #(when (current? handle generation)
                   (rf/dispatch-sync [:debug/socket-open handle generation])))
          (set! (.-onmessage socket)
                (fn [event]
                  (when (current? handle generation)
                    (try (rf/dispatch-sync [:debug/frame handle generation (transit/decode (.-data event))])
                         (catch :default _ (.close socket 4000 "Unreadable Transit event batch"))))))
          (set! (.-onclose socket)
                #(retry! handle generation (or (not-empty (.-reason %)) "Waiting for the event stream"))))
        (catch :default error (retry! handle generation (.-message error)))))))

(rf/reg-fx :debug/connect
  (fn [handle]
    (stop!)
    (let [hide (fn [_] (suspend!))
          show (fn [event] (when (.-persisted event) (connect!)))]
      (reset! owner {:handle handle :hide hide :show show})
      (.addEventListener js/window "pagehide" hide)
      (.addEventListener js/window "pageshow" show)
      (connect!))))

(rf/reg-fx :debug/disconnect (fn [_] (stop!)))
(rf/reg-fx :debug/send
  (fn [[handle generation message runtime]]
    (let [socket (:socket @owner)]
      (when (and (current? handle generation) socket (= 1 (.-readyState socket)))
        (try (.send socket (transit/encode message))
             (when (= :ack (:op message))
               (swap! owner assoc :runtime runtime :cursor (:cursor message) :attempt 0))
             (catch :default _ (.close socket 4000 "Debug acknowledgement failed")))))))
