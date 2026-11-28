(ns tolglow.operator-owned
  "Cues and instances that stay with the human operator: show-control cues
  such as a group blackout, scene owners and code cues (#479). Only human
  surfaces and named system owners may launch, edit, republish or end them;
  every automated or unknown source is refused, whatever path brought the
  command. Cues marked `:operator-only? true` also refuse a command with no
  source. The automatic selectors leave these cues out of their candidates."
  (:require [afterglow.control.categories :as categories]
            [tolglow.registry :as registry]))

(def ^:private roles #{:show-control :code :code-cue})

(defn definition?
  "Whether a cue or effect definition belongs to the operator: `:show-control?`,
  `:scene-owner` or `:code-cue?`, or a `:role`, `:kind` or tag of
  :show-control, :code or :code-cue."
  [entry]
  (boolean (and (map? entry)
                (or (:show-control? entry) (:scene-owner entry) (:code-cue? entry)
                    (roles (:role entry)) (roles (:kind entry)) (some roles (:tags entry))))))

(defn cue-owned?
  "Whether launching `cue-id` is the operator's alone, by the cue or its effect."
  [model cue-id]
  (let [cue (get-in model [:registry :cues cue-id])]
    (or (definition? cue) (definition? (when cue (registry/cue-effect (:registry model) cue))))))

(defn instance-owned?
  "Whether the running instance belongs to the operator, by the effect it runs
  or the cue that launched it."
  [model instance-id]
  (if-let [instance (get-in model [:instances instance-id])]
    (or (definition? (:definition instance)) (definition? (get-in model [:registry :cues (:cue-id instance)])))
    false))

(def human-adapters
  "Source adapters of operator surfaces. Device sessions use their profile ID
  (afterglow.control.devices): the X-Touch Extender is :x-touch-xt, a Seaboard
  :seaboard-m and an MPE controller :mpe. :surface is a linked control surface.
  Sensors are not here: a phone or watch reading is not an operator decision.
  A custom device profile's ID is not listed, so it is refused on
  operator-owned cues until it is added here."
  #{:web :studio :push-2 :push-3 :x-touch :x-touch-xt :xtouch :seaboard-m :mpe :midi :osc :repl :surface})

(def system-owner-adapters
  "Owners that run their own operator-owned cues on the operator's behalf:
  lighting scenes, rig calibration, show restore, photometry sessions and the
  showcase builder and player, for the showcase's own volume cue."
  #{:lighting-scene :rig-calibration :restore :photometry :showcase})

(def automated-adapters
  "Sources that act without a person pressing anything. They are listed so
  every adapter in the source tree is classified; the rule itself admits only
  human and system-owner sources. :live-midi is the Ableton bridge: Live
  arrangement playback is automated, while a performer's own Live input
  (origin :live-input) counts as a human surface."
  #{:agent :auto :autopilot :event-subscriber :reactive :scheduler :timeline :live-clip :live-run-clip
    :live-device :live-midi :audio-return :spatial :spatial-cue :composition :knob-candidate :scene-draft :sensor})

(defn- adapter [source]
  (let [a (when (map? source) (:adapter source))]
    (if (string? a) (keyword a) a)))

(defn classification
  "One of :human, :system-owner or :automated for a source adapter, or nil
  when the adapter is unknown (which the rule refuses)."
  [a]
  (let [a (if (string? a) (keyword a) a)]
    (cond (human-adapters a) :human
          (system-owner-adapters a) :system-owner
          (automated-adapters a) :automated)))

(defn source-class
  "How the rule reads a command source: :missing when there is none at all
  (an in-process caller), the adapter's classification, :human for a
  performer's own Live input, or :unknown for a source map that names no
  known adapter, such as a bridge that forgot to say what it is."
  [source]
  (let [a (adapter source)]
    (cond
      (or (nil? source) (and (map? source) (empty? source))) :missing
      (and (= :live-midi a) (= :live-input (:origin source))) :human
      :else (or (classification a) :unknown))))

(defn human-source?
  "Studio, web, Push, X-Touch, Seaboard, MPE, MIDI, OSC, linked surface and
  REPL sources are human operator surfaces; agents, sensors, reactive actions
  and subscribers are not."
  [source]
  (= :human (source-class source)))

(defn- acting-for
  "The source a command is judged by: its requester when it acts for one."
  [source]
  (if (map? (:requested-by source)) (recur (:requested-by source)) source))

(defn admitted-source?
  "Whether `source` may act on operator-owned cues: a human surface or a
  named system owner. A source that acts for someone else, such as a macro run
  or the showcase timeline, carries its requester in :requested-by and is
  judged by it. A missing or unknown source is not admitted."
  [source]
  (boolean (#{:human :system-owner} (source-class (acting-for source)))))

(defn- refuse [message]
  {:status :rejected :code :operator-owned :message message})

(defn- touched
  "The cue, effect and instance definitions a command would act on."
  [model {:keys [op cue-id effect-id instance-id cue kind definition selector]}]
  (let [cues (get-in model [:registry :cues]) effects (get-in model [:registry :effects])
        cue-and-effect (fn [c] (when c [c (registry/cue-effect (:registry model) c)]))
        instance-entries (fn [i] (when i [(:definition i) (get cues (:cue-id i))]))]
    (remove nil?
            (concat (when cue-id (cue-and-effect (get cues cue-id)))
                    (when (and (= :effect/launch op) effect-id) [(get effects effect-id)])
                    (when (and (= :cue/put op) (map? cue)) (cons cue (cue-and-effect (get cues (:id cue)))))
                    (when (and (= :registry/publish op) (= :effects kind) (map? definition))
                      (cons (get effects (:id definition)) (filter #(= (:id definition) (:effect-id %)) (vals cues))))
                    (when instance-id (instance-entries (get-in model [:instances instance-id])))
                    (when (= :group/apply op)
                      (mapcat instance-entries (try (categories/select-instances model selector) (catch Exception _ []))))))))

(defn refusal
  "The rejection for a `command` that would launch, rewrite or delete an
  operator-owned cue, republish an effect such a cue runs, or act on an
  operator-owned instance directly or through a group action, or nil.

  A human surface or a named system owner may act on ordinary operator-owned
  cues. An automated adapter, or a source map naming no known adapter, never
  may. A command with no source at all is an in-process caller of the show
  owner (the REPL or host code), which may act on ordinary operator-owned cues
  such as Room volumes. A cue or effect marked `:operator-only? true`, such as
  a held group blackout, admits only a human surface or show restore: no
  missing source and no other system owner. The caller adds the show identity
  and command ID."
  [model {:keys [source] :as command}]
  (let [entries (touched model command)
        owned (filter definition? entries)
        strict? (some #(true? (:operator-only? %)) entries)
        judged (acting-for source)
        class (source-class judged)
        admitted? (if strict?
                    (or (= :human class) (= :restore (adapter judged)))
                    (or (#{:human :system-owner} class) (= :missing class)))]
    (when (and (seq owned) (not admitted?))
      (refuse (str (if strict? "This blackout" "This operator-owned cue")
                   " stays with the operator: "
                   (case class
                     :missing "name a human source to act on it"
                     :unknown "the command's source names no known adapter"
                     "this source cannot launch, edit or end it"))))))
