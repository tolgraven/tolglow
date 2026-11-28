(ns tolglow.rehearsal.agent-script
  "The recorded agent for rehearsals (#670). One agent, one taste: a mood per
  music section kind, turned into the ordinary agent tool calls
  (configure_auto, schedule_events) that a provider would stream. The same
  table plays the unattended club passage and the Push-led concert, so a
  difference between them comes from the music, the performer and the seeds,
  not from a second agent.

  Nothing here talks to a provider. tolglow.rehearsal streams these rounds
  through tolglow.integrations.agent/run! with a recorded transport and
  dispatches them with show/dispatch-tool!, the path a live agent uses.
  Values are written as a provider would send them: JSON text, so namespaced
  ops are strings."
  (:require [tolglow.automation.random :as random]
            [tolglow.automation.red-show :as red-show]
            [tolglow.rehearsal.passage :as passage]))

(def club-scope [:wash-color :sweep :head-glow :accent :spot :strobe-hit])

(def moods
  "Auto settings per music section kind. Section plans inside a mood keep to
  bar lengths so their boundaries land on the bar grid after the agent's
  change applies on a section line."
  {:groove {:scope {:cue-ids club-scope}
            :interval {:beats 1} :count 2 :launch {:mode :one-shot :duration {:beats 1}}
            :randomize {:palette red-show/red-palette} :categories {:exclude [:strobe]}
            :sections [{:id :drive :weight 3 :length {:bars 2} :transition :release
                        :config {:scope {:cue-ids [:wash-color :sweep :head-glow :accent]}}}
                       {:id :lift :weight 1 :length {:bars 1} :max-run 1 :transition :release
                        :config {:scope {:cue-ids [:wash-color :head-glow]} :randomize {:palette red-show/purple-palette}}}]}
   :breakdown {:scope {:cue-ids [:wash-color :sweep :head-glow]}
               :interval {:bars 1} :count 1 :launch {:mode :timed :duration {:bars 1}}
               :randomize {:palette red-show/purple-palette} :categories {:exclude [:strobe]} :max-lit 6}
   :return {:scope {:cue-ids club-scope}
            :interval {:beats 1} :count 2 :launch {:mode :one-shot :duration {:beats 1}}
            :randomize {:palette red-show/red-palette} :categories {:exclude [:strobe]}
            :sections [{:id :drive :weight 4 :length {:bars 2} :transition :release
                        :config {:scope {:cue-ids [:wash-color :sweep :head-glow :accent]}}}
                       {:id :hit :weight 1 :length {:bars 1} :max-run 1 :transition :cut
                        :config {:scope {:cue-ids [:strobe-hit]} :categories {:include [:strobe]}
                                 :interval {:beats 2} :launch {:mode :one-shot :duration {:beats 2}}}}]}
   :quiet {:scope {:cue-ids [:spot :sweep]} :intent :darkness :max-lit 1
           :interval {:bars 1} :count 1 :launch {:mode :timed :duration {:bars 1}}}
   :verse {:scope {:cue-ids [:sweep :head-glow]}
           :interval {:bars 2} :count 2 :launch {:mode :timed :duration {:bars 2}}
           :randomize {:params :marked :palette red-show/red-palette}}
   :chorus {:scope {:cue-ids [:sweep :head-glow :accent]}
            :interval {:beats 2} :count 1 :launch {:mode :one-shot :duration {:beats 2}}
            :randomize {:palette red-show/red-palette}}})

(defn auto-configuration
  "The configure_auto configuration for section (index i) of passage. Its seed
  is derived from the passage's take seed (or seed) and the section, so the
  same take replays and another take varies."
  [passage section i]
  (let [mood (get moods (:kind section))]
    (when-not mood (throw (ex-info "The rehearsal agent has no mood for this section kind" {:kind (:kind section)})))
    (assoc mood :enabled? true
           :seed (random/derive-seed (or (:take-seed passage) (:seed passage)) :agent (:id section) i))))

(defn anchor-events
  "schedule_events entries for a concert: each section's anchor cue at its
  first bar for the section's length, and in a chorus a level ramp that
  builds over its first four bars."
  [passage]
  (vec
   (mapcat
    (fn [{:keys [id anchor start-bar bars kind]}]
      (when anchor
        (cond-> [{:id (str "anchor-" (name id)) :label (str "Anchor " (name id))
                  :op "cue/launch" :cue-id (name anchor)
                  :launch {:mode "one-shot" :duration {:bars bars}} :at {:bar start-bar}}]
          (= :chorus kind)
          (conj {:id (str "build-" (name id)) :label (str "Build " (name id))
                 :op "param/ramp" :cue-id (name anchor) :param "level" :to 1.0
                 :over {:bars 4} :curve "ease-in" :at {:bar start-bar :beat 1}}))))
    (:sections passage))))

(defn schedules?
  "Whether the agent puts its change on the section line with configure_auto's
  :at when it hears of the line before it has passed (#767). A passage can set
  :agent-schedules? false to measure an agent that applies on arrival."
  [passage]
  (:agent-schedules? passage true))

(defn suggestion
  "The agent's recorded response to section i of passage, heard at transport
  beat (the beat of the frame it was asked on): one round of tool calls with
  stable IDs. When the line is still ahead the change carries :at the
  section's first bar, so it applies on the downbeat, before the planner
  decides there; a line already passed is answered at once. A concert's
  first round also posts every section anchor ahead of time.

  Every configure_auto names what it was based on (#670): its window, from
  where the agent heard of the line (or the line, when it heard late) to the
  section's end, and the auto session the agent read when it heard of the
  line (:auto-session in seen), so the show refuses a reply that arrives
  after its section or after someone else changed auto meanwhile."
  ([passage i] (suggestion passage i (* passage/beats-per-bar (:start-bar (nth (:sections passage) i)))))
  ([passage i beat] (suggestion passage i beat {}))
  ([passage i beat seen]
   (let [section (nth (:sections passage) i)
         line (double (* passage/beats-per-bar (:start-bar section)))
         at? (and (schedules? passage) (<= beat line))
         run-id (str "rehearsal:" (name (:id passage)) (when (:take passage) (str ":" (name (:take passage))))
                     ":" (name (:id section)))]
     {:run-id run-id
      :index i
      :section-id (:id section)
      :beat line
      :scheduled? at?
      :prompt (str "Section " (name (:id section)) " (" (name (:kind section)) ", " (:bars section)
                   " bars) begins at bar " (:start-bar section) ".")
      :text (str "Playing " (name (:kind section)) (when at? (str " from bar " (:start-bar section))) ".")
      :calls (cond-> []
               (and (= :concert (:mode passage)) (zero? i))
               (conj {:id "anchors" :name "schedule_events" :arguments {:events (anchor-events passage)}})
               true
               (conj {:id "auto" :name "configure_auto"
                      :arguments (cond-> {:configuration (auto-configuration passage section i)
                                          :suggestion (cond-> {:from {:beat (min (double beat) line)}
                                                               :until {:bar (+ (:start-bar section) (:bars section))}}
                                                        (:auto-session seen) (assoc :basis {:auto-session (:auto-session seen)}))}
                                   at? (assoc :at {:bar (:start-bar section)}))}))})))
