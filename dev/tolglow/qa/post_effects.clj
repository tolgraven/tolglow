(ns tolglow.qa.post-effects
  "Run the optimized Studio against the demo show with the #479 post-effects
  published: Moon, a muddy warm white, on every head, a colour guard over the
  right half and a held blackout cue for the left half. Studio stays in its
  default toggle launch mode. `dev/studio/post-effects-qa.mjs` holds and
  releases the blackout pad, opens the Compose filter presets and saves
  screenshots. Arguments: screenshot directory and an optional port (0 picks
  one). Preview only: no output is armed."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [tolglow.application :as app]
            [tolglow.demo :as demo]
            [tolglow.effects.post :as post]
            [tolglow.show :as show]))

(def left [:head-1 :head-2 :head-3 :head-4])

(def right [:head-5 :head-6 :head-7 :head-8])

(defn model []
  (let [guard (post/color-guard :guard {:defaults {:min-saturation 0.75}})
        blackout (post/group-blackout :blackout)]
    (-> (demo/model)
        (assoc-in [:registry :effects :guard] guard)
        (assoc-in [:registry :effects :blackout] blackout)
        (assoc-in [:registry :cues :guard] {:id :guard :version 1 :name "Colour guard" :effect-id :guard
                                            :priority 900 :selector {:fixture-ids right} :swatch "#d9a441"})
        (assoc-in [:registry :cues :left-out] (post/blackout-cue :left-out :blackout {:fixture-ids left}
                                                                 :name "Left blackout"))
        (assoc-in [:pages :main :cells [0 3]] :left-out)
        (assoc-in [:pages :main :cells [1 3]] :guard))))

(defn -main [& [directory port]]
  (let [host (app/start! {:init {:components [:show] :modules [:web-server]}
                          :show {:model (model) :workspace-options nil :recovery false}
                          :web-server {:host "127.0.0.1" :port (Long/parseLong (or port "0"))}})
        origin (str "http://127.0.0.1:" (get-in host [:application :modules :web-server :port]))
        directory (or directory "target/post-effects")]
    (try
      (.mkdirs (io/file directory))
      (doseq [cue [:moon :guard]]
        (let [ack @(show/submit! host :command {:op :cue/launch :cue-id cue :launch {:mode :persistent}})]
          (when-not (= :accepted (:status ack)) (throw (ex-info "Could not launch a post-effect fixture" ack)))))
      (let [{:keys [exit out err]} (shell/sh "node" "dev/studio/post-effects-qa.mjs" origin directory)]
        (print out) (flush)
        (when-not (zero? exit)
          (binding [*out* *err*] (print err) (flush))
          (throw (ex-info "Post-effect browser check failed" {:error err}))))
      (finally (app/stop! host) (shutdown-agents)))))
