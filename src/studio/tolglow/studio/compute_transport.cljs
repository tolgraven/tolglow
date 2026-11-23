(ns tolglow.studio.compute-transport
  "Transit and GPU dispatch share a dedicated worker, away from Studio's UI."
  (:require [tolglow.transit :as transit]))

(defonce ^:private session (atom nil))
(def ^:private little-endian?
  (= 1 (aget (js/Uint8Array. (.-buffer (js/Uint16Array. #js [1]))) 0)))

(defn- status! [phase message]
  (.postMessage js/self #js {:phase phase :message message}))

(defn- close! [phase reason]
  (when-let [{:keys [socket heartbeat dispatch]} @session]
    (reset! session nil)
    (js/clearInterval heartbeat)
    (when dispatch (dispatch #js {:op "dispose" :requestId 0}))
    (when socket
      (set! (.-onclose socket) nil)
      (set! (.-onmessage socket) nil)
      (.close socket)))
  (status! phase reason))

(defn- send! [socket message]
  (when (> (.-bufferedAmount socket) 131072)
    (throw (js/Error. "Connection cannot keep up with render jobs")))
  (.send socket (transit/encode message)))

(defn- bytes->floats [bytes]
  (when-not (and (instance? js/Uint8Array bytes) (zero? (mod (.-byteLength bytes) 4)))
    (throw (js/Error. "Invalid fixture buffer")))
  (let [offset (.-byteOffset bytes) length (/ (.-byteLength bytes) 4) buffer (.-buffer bytes)]
    (if (and little-endian? (zero? (mod offset 4)))
      (js/Float32Array. buffer offset length)
      (let [values (js/Float32Array. length) view (js/DataView. buffer offset (.-byteLength bytes))]
        (dotimes [i length] (aset values i (.getFloat32 view (* i 4) true)))
        values))))

(defn- result-bytes [values]
  (when-not (and (instance? js/Float32Array values) (<= (.-length values) 8192))
    (throw (js/Error. "Invalid GPU result")))
  (if little-endian?
    (js/Uint8Array. (.-buffer values) (.-byteOffset values) (.-byteLength values))
    (let [buffer (js/ArrayBuffer. (.-byteLength values)) view (js/DataView. buffer)]
      (dotimes [i (.-length values)] (.setFloat32 view (* i 4) (aget values i) true))
      (js/Uint8Array. buffer))))

(defn- gpu-request [{:keys [op id plan positions surface-uvs start count uniforms job]}]
  (case op
    :prepare #js {:op "prepare" :requestId 1 :id id :plan (clj->js plan) :positions (bytes->floats positions)
                  :surfaceUvs (when surface-uvs (bytes->floats surface-uvs))}
    :run #js {:op "run" :requestId job :id id :start start :count count :uniforms (clj->js uniforms)}
    (throw (js/Error. "Unknown compute request"))))

(defn- reply! [socket {:keys [op job id]} response]
  (let [error (cond-> {:op :error :id id} job (assoc :job job))]
    (cond
      (not (aget response "ok")) (send! socket error)
      (= op :prepare) (do (when-let [old (aget response "evicted")] (send! socket {:op :error :id old}))
                          (send! socket {:op :ready :id id}))
      :else (send! socket {:op :result :job job :values (result-bytes (aget response "values"))}))))

(defn- accept! [socket dispatch text]
  (when-not (and (string? text) (<= (count text) (* 16 1024 1024)))
    (throw (js/Error. "Invalid host message")))
  (let [{:keys [op] :as packet} (transit/decode text)]
    (if (= :welcome op)
      (status! "active" "Contributing GPU time")
      (-> (dispatch (gpu-request packet))
          (.then #(when (identical? socket (:socket @session)) (reply! socket packet %)))
          (.catch #(close! "error" "Compute job failed"))))))

(defn- connect! [dispatch caps token]
  (let [url (js/URL. "/api/compute/stream" (.-origin js/location))
        _ (set! (.-protocol url) (if (= "https:" (.-protocol url)) "wss:" "ws:"))
        socket (js/WebSocket. (.-href url))]
    (swap! session assoc :socket socket)
    (set! (.-onopen socket)
          (fn []
            (set! (.-onopen socket) nil)
            (send! socket {:op :hello :token token
                           :caps {:max-result-floats (min 8192 (aget caps "maxResultFloats"))}})
            (swap! session assoc :heartbeat
                   (js/setInterval #(when (= js/WebSocket.OPEN (.-readyState socket))
                                      (send! socket {:op :keepalive})) 15000))))
    (set! (.-onmessage socket)
          (fn [event]
            (try (accept! socket dispatch (.-data event))
                 (catch :default _ (close! "error" "Invalid compute message")))))
    (set! (.-onclose socket) #(close! "disconnected" "Connection closed. Check Studio access and the compute token."))
    (set! (.-onerror socket) #(close! "error" "Could not connect to this Studio host"))))

(defn- join! [token]
  (when-not @session
    (try
      (js/importScripts "/studio/compute/dispatcher.js")
      (let [dispatch (js-invoke (aget js/self "TolglowCompute") "createComputeDispatcher"
                       #js {:onAvailability (fn [event]
                                             (when-not (aget event "available")
                                               (close! "error" "GPU context unavailable")))})]
        (reset! session {:dispatch dispatch})
        (-> (dispatch #js {:op "probe" :requestId 0})
            (.then (fn [response]
                     (if (and @session (aget response "ok"))
                       (connect! dispatch (aget response "capabilities") token)
                       (close! "error" "WebGL2 compute probe failed"))))
            (.catch #(close! "error" "GPU probe failed"))))
      (catch :default _ (close! "error" "Compute worker unavailable")))))

(defn init! []
  (set! (.-onmessage js/self)
        (fn [event]
          (let [message (.-data event)]
            (case (aget message "op")
              "join" (join! (aget message "token"))
              "leave" (close! "stopped" "Stopped")
              nil)))))
