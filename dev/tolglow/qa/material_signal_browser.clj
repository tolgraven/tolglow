(ns tolglow.qa.material-signal-browser
  "Run the signal-picker browser checks with live, locally analyzed bass."
  (:require [clojure.java.shell :as shell]
            [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.integrations.analysis :as audio]
            [tolglow.show :as show]))

(defn- pcm [start-ms]
  (double-array
   (for [i (range 200)
         :let [t (/ (+ (* 8 start-ms) i) 8000.0)
               sine #(Math/sin (* 2 Math/PI % t))
               amplitude (+ 0.08 (* 0.9 (Math/pow (+ 0.5 (* 0.5 (sine 0.625))) 2)))]]
     (* amplitude (sine 80)))))

(defn- send-audio! [publish!]
  (loop [analyzer (audio/new-analyzer {}), start 0]
    (let [end (+ 25 start)
          {:keys [features state]} (audio/analyze analyzer {:samples (pcm start) :sample-rate 8000
                                                             :started-at-ms start :received-at-ms end})]
      (publish! features)
      (Thread/sleep 25)
      (recur state end))))

(defn -main [& _]
  (let [model (assoc-in (demo/model) [:variables :controller "wrist" :tint] [0.35 0.8 0.92])
        host (app/start! {:init {:components [:show] :modules [:web-server]}
                          :show {:model model :workspace-options nil}
                          :web-server {:host "127.0.0.1" :port 0}})
        producer (future (send-audio! (:features! (show/audio-callbacks host))))
        origin (str "http://127.0.0.1:" (get-in host [:application :modules :web-server :port]))]
    (try
      (let [{:keys [exit out err]} (shell/sh "node" "dev/studio/signal-picker-browser-qa.mjs" origin)]
        (print out) (flush)
        (when-not (zero? exit) (throw (ex-info "Material signal browser check failed" {:error err}))))
      (finally
        (future-cancel producer)
        (app/stop! host)
        (shutdown-agents)))))
