(ns tolglow.studio.airplay
  "The Room component owns its encoder, read-only HLS session and route picker."
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.state-socket :as socket]
            [tolglow.transit :as transit]))

(def ^:private prefix "/api/preview/airplay/")
(def ^:private media-type "application/transit+json")

(defn- control! [route & [body]]
  (let [options (cond-> {:method (if body "POST" "GET")
                         :headers {"Accept" media-type "Content-Type" media-type}}
                  body (assoc :body (transit/encode body)))]
    (-> (js/fetch (str prefix route) (clj->js options))
        (.then (fn [response]
                 (-> (.text response)
                     (.then (fn [text]
                              (let [result (transit/decode text)]
                                (if (.-ok response) result (throw (js/Error. (:message result)))))))))))))

(defn- upload! [writer file buffer signal]
  (-> (js/fetch (str prefix writer "/" file)
                #js {:method "PUT" :body buffer :signal signal})
      (.then (fn [response]
               (when-not (.-ok response)
                 (throw (js/Error. (str "Preview upload failed (" (.-status response) ")."))))))))

(defn- external! [canvas active?]
  (socket/present-preview! :airplay active?)
  (when canvas
    (.dispatchEvent canvas (js/CustomEvent. "tolglow-preview-external"
                             #js {:detail #js {:owner "airplay" :active active?}}))))

(defn control [canvas-id]
  (let [ui (r/atom {:phase :idle})
        session (atom nil)
        alive? (atom true)
        video (atom nil)
        canvas #(.getElementById js/document canvas-id)
        notify! #(rf/dispatch [:studio/toast (or (.-message %) (str %)) true])
        stop-remote! #(-> (control! "stop" {:writer %}) (.catch (fn [_])))
        stop! (fn []
                (let [{:keys [abort writer]} @session]
                  (reset! session nil)
                  (when abort (.abort abort))
                  (when writer (stop-remote! writer))
                  (when-let [player @video]
                    (.pause player) (.removeAttribute player "src") (.load player))
                  (external! (canvas) false)
                  (reset! ui {:phase :idle})))
        failed! (fn [abort error]
                  (when (identical? abort (:abort @session)) (stop!) (notify! error)))
        ready! (fn [abort url width height]
                 (when (identical? abort (:abort @session))
                   (swap! ui assoc :phase :sharing :url url :width width :height height)
                   (when-let [player @video]
                     (when (.canPlayType player "application/vnd.apple.mpegurl")
                       (set! (.-src player) url)))))
        begin! (fn [abort module {:keys [url writer]}]
                 (if-not (identical? abort (:abort @session))
                   (stop-remote! writer)
                   (do (swap! session assoc :writer writer)
                       (js-invoke module "startVideo" (canvas) (partial upload! writer)
                                  (partial ready! abort url) (partial failed! abort) (.-signal abort)))))
        start! (fn []
                 (let [abort (js/AbortController.) address (:address @ui)]
                   (reset! session {:abort abort})
                   (swap! ui assoc :phase :starting)
                   (external! (canvas) true)
                   (-> (js-invoke js/globalThis "tolglowLoadEngine" "airplay")
                       (.then (fn [module]
                                (when (identical? abort (:abort @session))
                                  (-> (js-invoke module "waitForPreview" (canvas) (.-signal abort))
                                      (.then #(when (identical? abort (:abort @session))
                                                (-> (control! "start" {:address address})
                                                    (.then (partial begin! abort module)))))))))
                       (.catch (partial failed! abort)))))
        configure! (fn []
                     (swap! ui assoc :phase :looking)
                     (-> (control! "interfaces")
                         (.then (fn [interfaces]
                                  (when @alive?
                                    (if (seq interfaces)
                                      (reset! ui {:phase :choose :interfaces interfaces :address (:address (first interfaces))})
                                      (do (stop!) (notify! "Connect the show host and receiver to the same network."))))))
                         (.catch (fn [error] (when @alive? (stop!) (notify! error))))))
        button (fn [label action] [:button.text-button {:type "button" :on-click action} label])]
    (r/create-class
      {:display-name "airplay-preview"
       :component-will-unmount (fn [_] (reset! alive? false) (stop!))
       :reagent-render
       (fn [_]
         (let [{:keys [phase interfaces address url width height]} @ui]
           [:<>
            [:video {:ref #(reset! video %) :muted true :plays-inline true :preload "metadata"
                     :x-webkit-airplay "allow" :aria-hidden true
                     :style {:position "absolute" :width 1 :height 1 :opacity 0 :pointer-events "none"}}]
            (case phase
              :idle (button "Share video" configure!)
              :looking [:span {:role "status"} "Finding interfaces…"]
              :choose [:<>
                       [:select {:aria-label "Network for the preview receiver" :value address
                                 :on-change #(swap! ui assoc :address (.. % -target -value))}
                        (for [{:keys [address interface]} interfaces]
                          ^{:key address} [:option {:value address} (str interface " · " address)])]
                       (button "Start sharing" start!) (button "Cancel" stop!)]
              :starting [:<> [:span {:role "status"} "Encoding preview…"] (button "Cancel" stop!)]
              :sharing [:<> [:a.text-button {:href url :target "_blank" :rel "noreferrer"
                                              :title "Open the read-only HLS feed"} (str width " × " height)]
                        (when-let [player @video]
                          (when (fn? (aget player "webkitShowPlaybackTargetPicker"))
                            (button "AirPlay" #(do (-> (.play player) (.catch notify!))
                                                   (js-invoke player "webkitShowPlaybackTargetPicker")))))
                        (button "Stop sharing" stop!)])]))})))
