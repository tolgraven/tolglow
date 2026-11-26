;; One-off: write a Room preview capture of the 2023 gig rig with one static
;; look lit, for the PR screenshot. Manual clock, capture sink, no devices.
(ns gig-capture
  (:require [clojure.java.io :as io]
            [tolglow.effects :as fx]
            [tolglow.registry :as registry]
            [tolglow.show :as show]
            [tolglow.showcase.looks :as looks]
            [tolglow.qa.showcase-capture :as cap]
            [tolglow.venues.gig-2023 :as gig]))

(def ids (mapv :id (gig/fixtures)))

(def stops [{:position 0.0 :color [0.05 0.15 1.0]}
            {:position 0.5 :color [0.75 0.05 0.9]}
            {:position 1.0 :color [1.0 0.25 0.05]}])

(def look
  (fx/effect :gig-preview {:name "Gig preview" :targets ids :duration {:bars 16}}
             (fx/color (looks/gradient stops (fx/clamp (fx/add 0.5 (fx/divide (fx/position :x) 10.0)))))
             (fx/intensity 0.9)
             (fx/output :shutter (/ 12.0 255.0) {:targets [:moving-2]})))

(def cue {:id :gig-preview :version 1 :name "Gig preview" :effect-id :gig-preview
          :swatch "#8040ff" :tags [:colour] :params {} :priority 0 :reload-policy :preserve-state})

(defn run [dir]
  (let [model (-> (gig/model)
                  (update :registry registry/publish :effects look)
                  (update :registry registry/publish :cues cue))
        frames (atom [])
        sink {:description {:type :capture :network false}
              :send! (fn [_ _] (swap! frames conj 1)) :close! (fn [])}
        runtime (show/create-runtime {:model model :workspace-options nil :worker-timeout-ms 60000 :sink sink})
        out (doto (io/file dir) .mkdirs)]
    (try
      (show/tick! runtime 0)
      (let [ack (show/submit! runtime :command {:op :cue/launch :cue-id :gig-preview})]
        (show/tick! runtime 25)
        (println "launch" (deref ack 5000 :timeout)))
      (let [snaps (vec (for [t (range 50 3050 25)] (do (show/tick! runtime t) (show/snapshot runtime))))
            kept (take-last 40 snaps)
            wire @#'cap/wire]
        (with-open [w (io/writer (io/file out "frames.ndjson"))]
          (doseq [s kept] (.write w ^String (wire (@#'cap/preview-frame s))) (.write w "\n")))
        (spit (io/file out "rig.json") (wire (@#'cap/preview-rig (:rig (last kept)))))
        (spit (io/file out "meta.json")
              (wire {:meter {:beats-per-bar 4 :bars-per-phrase 4} :frame-ms 25 :frames (count kept)
                     :output "capture sink only"}))
        (println "faults" (keep :fault (:instances (last kept))) "running" (mapv :cue-id (:instances (last kept)))
                 "beat" (get-in (last kept) [:transport :beat])
                 "values" (count (get-in (last kept) [:output :values]))))
      (finally (show/stop! runtime)))))

(run "target/gig-720")
