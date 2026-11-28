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
  Sensors are not here: a phone or watch reading is not an operator decision."
  #{:web :studio :push-2 :push-3 :x-touch :x-touch-xt :xtouch :seaboard-m :mpe :midi :osc :repl :surface})

(def system-owner-adapters
  "Owners that run their own operator-owned cues on the operator's behalf:
  lighting scenes, rig calibration, show restore, photometry sessions and the
  showcase builder, which registers its volume cue while it builds the model."
  #{:lighting-scene :rig-calibration :restore :photometry :showcase})

(def automated-adapters
  "Sources that act without a person pressing anything. They are listed so
  every adapter in the source tree is classified; the rule itself admits only
  human and system-owner sources."
  #{:agent :auto :autopilot :event-subscriber :reactive :scheduler :timeline :live-clip :live-run-clip
    :live-device :audio-return :spatial :spatial-cue :composition :knob-candidate :scene-draft :sensor})

(defn- adapter [source]
  (let [a (when (map? source) (:adapter source))]
    (if (string? a) (keyword a) a)))

(defn classification
  "One of :human, :system-owner or :automated for a source adapter, or nil
  when the adapter is unknown (which the rule treats as automated)."
  [a]
  (let [a (if (string? a) (keyword a) a)]
    (cond (human-adapters a) :human
          (system-owner-adapters a) :system-owner
          (automated-adapters a) :automated)))

(defn human-source?
  "Studio, web, Push, X-Touch, Seaboard, MPE, MIDI, OSC, linked surface and
  REPL sources are human operator surfaces; agents, sensors, reactive actions
  and subscribers are not."
  [source]
  (= :human (classification (adapter source))))

(defn admitted-source?
  "Whether `source` may act on operator-owned cues: a human surface or a
  named system owner. A source that acts for someone else, such as a macro run
  or the showcase timeline, carries its requester in :requested-by and is
  judged by it. A missing or unknown source is not admitted."
  [source]
  (if (map? (:requested-by source))
    (recur (:requested-by source))
    (boolean (#{:human :system-owner} (classification (adapter source))))))

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

(defn- missing? [source] (nil? (adapter source)))

(defn refusal
  "The rejection for a `command` that would launch, rewrite or delete an
  operator-owned cue, republish an effect such a cue runs, or act on an
  operator-owned instance directly or through a group action, or nil.

  A human surface or a named system owner may always act. An automated or
  unknown adapter never may. A command with no source at all is an in-process
  caller of the show owner (the REPL or host code), which may act on ordinary
  operator-owned cues such as Room volumes, but not on a cue or effect marked
  `:operator-only? true`, such as a held group blackout: that needs an explicit
  human or system-owner source. The caller adds the show identity and command
  ID."
  [model {:keys [source] :as command}]
  (when-not (admitted-source? source)
    (let [entries (touched model command)
          owned (filter definition? entries)
          strict? (some #(true? (:operator-only? %)) entries)]
      (when (and (seq owned) (or strict? (not (missing? source))))
        (refuse (str (if strict? "This blackout" "This operator-owned cue")
                     " stays with the operator: "
                     (if (missing? source) "name a human source to act on it" "automation cannot launch, edit or end it")))))))
