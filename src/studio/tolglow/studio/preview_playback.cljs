(ns tolglow.studio.preview-playback
  "The live media stream belongs to the mounted preview control."
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [tolglow.studio.state-socket :as socket]))

(defn- button [text title action]
  [:button.text-button {:type "button" :title title :aria-label title :on-click action} text])

(defn control [canvas-id]
  (let [state (r/atom "unavailable")
        capture (atom nil)
        changed! (fn [phase]
                   (reset! state phase)
                   (socket/present-preview! (= phase "active")))
        attach! (fn [video]
                  (let [alive? #(.-isConnected video)
                        failed! #(rf/dispatch [:studio/toast % true])]
                    (-> (js-invoke js/globalThis "tolglowLoadEngine" "preview")
                        (.then (fn [module]
                                 (when (alive?)
                                   ;; The Room may be in its own window: use this control's document.
                                   (let [doc (.-ownerDocument video)
                                         canvas (.getElementById doc canvas-id)]
                                     (when (js-invoke module "canPresentCanvas" canvas video doc)
                                       (reset! capture
                                               (js/Reflect.construct (aget module "CanvasPlayback")
                                                 #js [canvas video changed! failed!]))
                                       (reset! state "idle"))))))
                        (.catch #(when (alive?) (failed! (.-message %)))))))
        mount! (fn [node] (when node (attach! node)))
        invoke! #(when-let [owner @capture] (js-invoke owner %))]
    (r/create-class
      {:display-name "live-preview-playback"
       :component-will-unmount #(when-let [owner @capture] (js-invoke owner "dispose"))
       :reagent-render
       (fn [_]
         [:<>
          [:video {:muted true :plays-inline true :ref mount! :aria-hidden true
                   :style {:position "absolute" :width "1px" :height "1px" :opacity 0 :pointer-events "none"}}]
          (case @state
            "idle" [button "PiP" "Prepare live preview for Picture in Picture" #(invoke! "prepare")]
            "preparing" [button "Cancel PiP" "Cancel preview capture" #(invoke! "close")]
            "ready" [:<> [button "Open PiP" "Open Picture in Picture" #(invoke! "present")]
                         [button "Cancel" "Cancel preview capture" #(invoke! "close")]]
            "active" [button "Close PiP" "Close Picture in Picture" #(invoke! "close")]
            nil)])})))
