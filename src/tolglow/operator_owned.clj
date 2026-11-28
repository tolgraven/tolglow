(ns tolglow.operator-owned
  "Cues and instances that stay with the human operator: show-control cues
  such as a group blackout, scene owners and code cues. Agents, auto mode, the
  energy autopilot and event subscribers may not launch, end or edit them. The
  show owner refuses such a command whatever path brought it (#479), and the
  automatic selectors leave these cues out of their candidates."
  (:require [tolglow.registry :as registry]))

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
  :seaboard-m and an MPE controller :mpe."
  #{:web "web" :studio :push-2 :push-3 :x-touch :x-touch-xt :xtouch :seaboard-m :mpe :midi :osc :sensor :repl})

(defn human-source?
  "Studio, web, Push, X-Touch, Seaboard, MPE, MIDI, OSC, sensor and REPL sources
  are human operator surfaces; agents, reactive actions and subscribers are not."
  [source]
  (boolean (and (map? source) (human-adapters (:adapter source)))))

(def automated-adapters
  "Sources that decide on their own: agent tools, auto mode, the energy
  autopilot, event subscribers and reactive actions. System owners that run
  their own operator-owned cues (scenes, calibration, restore) are not here."
  #{:agent "agent" :auto "auto" :autopilot "autopilot" :event-subscriber "event-subscriber"
    :reactive "reactive"})

(defn automated-source? [source]
  (boolean (and (map? source) (automated-adapters (:adapter source)))))

(defn refusal
  "The rejection for an automated `command` that would launch an operator-owned
  cue or address an operator-owned instance, or nil. The caller adds the
  show identity and command ID."
  [model {:keys [op source cue-id instance-id]}]
  (when (automated-source? source)
    (cond
      (and (= :cue/launch op) (some? cue-id) (cue-owned? model cue-id))
      {:status :rejected :code :operator-owned
       :message (str "Cue " (pr-str cue-id) " stays with the operator: automation cannot launch it")}

      (and (some? instance-id) (instance-owned? model instance-id))
      {:status :rejected :code :operator-owned
       :message (str "Instance " instance-id " stays with the operator: automation cannot change or end it")})))
