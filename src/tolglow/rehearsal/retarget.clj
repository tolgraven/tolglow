(ns tolglow.rehearsal.retarget
  "Replay a rehearsal take's agent decisions on a reference rig (#858). The
  recorded agent's suggestions for a passage are retargeted
  (tolglow.rig.retarget) onto the target, and the take plays through the
  real show owner with captured output on the target model, through the same
  agent path (tolglow.integrations.agent/run! and show/dispatch-tool!) as the
  source take.

    (require '[tolglow.rehearsal.retarget :as rr] '[tolglow.rehearsal.passage :as passage])
    (def p (passage/club 670))
    (println (tolglow.rig.retarget/summary (rr/record p :movers-pars)))
    (def take (rr/run-take p :movers-pars))
    (rr/output-digest take)"
  (:require [afterglow.runtime.replay :as replay]
            [tolglow.rehearsal :as rehearsal]
            [tolglow.rehearsal.agent-script :as script]
            [tolglow.rehearsal.passage :as passage]
            [tolglow.rig.retarget :as retarget]))

(defn decisions
  "The recorded agent's suggestion for every section of passage, as it answers
  a section line it hears ahead of time."
  [passage]
  (mapv #(script/suggestion passage %) (range (count (:sections passage)))))

(defn source-model [passage] (passage/model (:bpm passage)))

(defn record
  "The retargeted record of passage's agent decisions onto target (a
  reference rig ID or a show model)."
  [passage target]
  (retarget/retarget (source-model passage) target (decisions passage)))

(defn run-take
  "Play passage on target with every agent suggestion retargeted, through the
  real show owner with captured output. Options as tolglow.rehearsal/run-take."
  ([passage target] (run-take passage target {}))
  ([passage target options]
   (let [source (source-model passage)
         ctx (retarget/prepare source target)
         model (assoc (retarget/target-model ctx) :transport {:playing? true :beat 0.0 :bpm (double (:bpm passage))})]
     (rehearsal/run-take passage (assoc options :model model
                                        :decide #(retarget/retarget-suggestion ctx source %))))))

(defn output-digest
  "SHA-256 over the take's per-frame output hashes, in order."
  [take]
  (replay/digest (mapv :dmx-hash (:frames take))))
