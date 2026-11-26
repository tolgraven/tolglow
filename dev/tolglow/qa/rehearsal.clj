(ns tolglow.qa.rehearsal
  "Rehearsal artefacts and the Room-only rehearsal (#670).

  Headless review, written as EDN data and a Markdown report per mode:
    bin/repl run tolglow.qa.rehearsal review [directory] [seed]
    lein with-profile +dev run -m tolglow.qa.rehearsal review target/rehearsal 670

  Room-only rehearsal in real time: a show with the rehearsal model and
  Studio on 127.0.0.1:<port>, nothing armed for physical output. Open
  Studio's Room to watch; the process ends when the passage does.
    lein with-profile +dev run -m tolglow.qa.rehearsal room 18871 club 670
    (passages: club, concert-a, concert-b)"
  (:require [clojure.data.json :as json]
            [clojure.pprint :as pprint]
            [tolglow.application :as app]
            [tolglow.rehearsal :as rehearsal]
            [tolglow.rehearsal.passage :as passage]
            [tolglow.show :as show]))

(defn- review! [directory seed]
  (let [club (rehearsal/club seed)
        concert (rehearsal/concert seed)
        paths (rehearsal/write! directory club concert)]
    (doseq [path paths] (println "wrote" path))
    (println "club flags:" (pr-str (:flags club)))
    (println "concert flags:" (pr-str (:flags concert)))))

(defn- room-passage [which seed]
  (case which
    "club" (passage/club seed)
    "concert-a" (passage/concert seed :a)
    "concert-b" (passage/concert seed :b)))

(defn- studio-state [port]
  ;; The same projection Studio's panels read.
  (with-open [stream (.openStream (java.net.URL. (str "http://127.0.0.1:" port "/api/state")))]
    (json/read-str (slurp stream) :key-fn keyword)))

(defn- room! [port which seed]
  (let [port (Long/parseLong (str port))
        p (room-passage which seed)
        host (app/start! {:init {:components [:show] :modules [:web-server]}
                          :show {:model (rehearsal/room-model p) :workspace-options nil :recovery false}
                          :web-server {:host "127.0.0.1" :port port}})
        sampled (atom [])
        sampler (future
                  (loop []
                    (Thread/sleep 4000)
                    (when-let [state (try (studio-state port) (catch Exception _ nil))]
                      (swap! sampled conj {:beat (get-in state [:transport :beat])
                                           :auto-section (get-in state [:auto :section :id] (get-in state [:auto :section]))
                                           :instances (count (:instances state))}))
                    (recur)))]
    (try
      (println (str "Room rehearsal of " which " (seed " seed "): open http://127.0.0.1:" port "/ and the Room view."))
      (println "No output is armed; the show renders into Studio only.")
      (let [result (rehearsal/play! host p)]
        (pprint/pprint (-> result
                           (update :agent (partial mapv #(select-keys % [:section :accepted-beat :wall-ms :receipts])))
                           (update :gestures (partial mapv #(select-keys % [:label :status :accepted-beat])))))
        (println "Studio /api/state samples while playing:" (pr-str @sampled)))
      (finally (future-cancel sampler) (app/stop! host)))))

(defn -main [& [mode a b c]]
  (case mode
    "review" (review! (or a "target/rehearsal") (Long/parseLong (or b "670")))
    "room" (room! (or a "18871") (or b "club") (Long/parseLong (or c "670")))
    (do (println "Usage: review [directory] [seed] | room <port> [club|concert-a|concert-b] [seed]")
        (System/exit 2))))
