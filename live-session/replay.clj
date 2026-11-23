(load-file (str (System/getenv "AFTERGLOW_ROOT") "/dev/afterglow/qa/push_3_display.clj"))
(require '[clojure.java.io :as io]
         '[tolglow.qa.push-scene :as qa]
         '[tolglow.show :as show] '[tolglow.studio :as studio]
         '[tolglow.studio.display :as studio-display]
         '[afterglow.control.devices.push-3.display :as usb]
         '[afterglow.control.devices.session :as session]
         '[afterglow.qa.push-3-display :as replay])
(import '[javax.imageio ImageIO])
(.mkdirs (io/file "/tmp/push3-browser-wire-flow"))
(let [runtime (show/create-runtime {:model (#'qa/scenic-show) :workspace-options nil})
      listeners (atom []) input (atom nil) selected (atom nil) last-wire (atom nil) tick (atom nil)
      captured (atom {})
      backend {:open! (constantly :recorder) :close! (fn [_] (swap! listeners conj :closed))
               :write! (fn [_ ^bytes bs] (when (= 327680 (alength bs)) (reset! last-wire (aclone bs))))}
      raw (usb/sink {:backend backend :schedule! (fn [f] (reset! tick f) (fn []))})
      destination (assoc raw :render! (fn [handle image]
                                        ((:render! raw) handle image)
                                        (@tick)
                                        (let [view @selected m (:display-mode view) k [m (or (:camera view) (get-in view [:scene :camera]))]]
                                          (when-not (contains? @captured k)
                                            (let [decoded (#'replay/decode @last-wire image)
                                                  filename (str "/tmp/push3-browser-wire-flow/" (name m) "-" (name (or (second k) :default)) ".png")]
                                              (ImageIO/write decoded "png" (io/file filename))
                                              (swap! captured assoc k {:file filename :size (alength ^bytes @last-wire)}))))))
      capture studio-display/process-capture
      wait-for (fn [mode lens]
                 (let [deadline (+ (System/currentTimeMillis) 90000)]
                   (loop []
                     (cond
                       (some #(and (= mode (first %)) (or (nil? lens) (= lens (second %)))) (keys @captured)) true
                       (> (System/currentTimeMillis) deadline) (throw (ex-info "Timed out" {:waiting [mode lens] :captured @captured :selected @selected}))
                       :else (do (Thread/sleep 75) (recur))))))]
  (try
    (#'qa/frames! runtime)
    (let [{:keys [port stop!]} (studio/listen! runtime {:port 0})]
      (try
        (with-redefs [studio-display/process-capture
                      (fn [options]
                        (let [c (capture options)]
                          (assoc c :capture! (fn [handle view]
                                               (let [img ((:capture! c) handle view)]
                                                 (reset! selected view) img)))))]
          (let [surface (show/push-browser-display runtime {:device :push-3 :sink destination
                                  :url (str "http://127.0.0.1:" port "/push-display")
                                  :browser-executable "/usr/bin/chromium" :no-sandbox? true
                                  :playwright-module (.getAbsolutePath (io/file "node_modules/playwright-core"))
                                  :startup-timeout-ms 60000 :scene-startup-timeout-ms 60000
                                  :capture-timeout-ms 20000 :scene-capture-timeout-ms 20000})
                device (show/connect-device! runtime :push-3
                         {:session-id "push-three-capture" :layout {:buttons {:scene {:channel 4 :controller 22} :lens {:channel 4 :controller 23} :rig {:channel 4 :controller 24} :parameter {:channel 4 :controller 25}}}
                          :user-mode? true :expression-mode :poly-aftertouch
                          :button-bindings {:scene :display/simulator :lens :display/camera :rig :display/rig :parameter :display/parameters}
                          :display surface :schedule! nil
                          :transport {:open! #(do (reset! input %) :midi) :close! (fn [_])}})
                press (fn [cc] (@input [0xb4 cc 127]) (@input [0xb4 cc 0]))]
            (try (wait-for :parameters nil)
                 (press 22) (wait-for :simulator :fit)
                 (press 23) (wait-for :simulator :panorama)
                 (press 24) (wait-for :rig nil)
                 (press 25)
                 (finally (session/close! device)))
            (prn {:views @captured :released @listeners :midi-errors (:errors (session/snapshot device))})))
        (finally (stop!))))
    (finally (show/stop! runtime) (shutdown-agents))))
