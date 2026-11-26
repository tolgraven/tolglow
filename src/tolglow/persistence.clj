(ns tolglow.persistence
  "Pure schema-2 show files. No eval, clocks, IO resources or live activation.
  Freeze/thaw operate on the complete domain model, never the runtime shell."
  (:refer-clojure :exclude [read-string])
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [tolglow.transit :as wire]
            [afterglow.control.model :as control]
            [afterglow.control.pages :as cue-pages]
            [afterglow.control.system :as system]
            [afterglow.control.devices.expressive :as expressive]
            [afterglow.reactive.registry :as reactive-registry]
            [afterglow.render.frame :as render]
            [afterglow.time :as time]
            [tolglow.registry :as registry]
            [tolglow.aim-paths :as aim-paths]
            [tolglow.authoring.variation-control :as variations]
            [tolglow.cue-timing :as cue-timing]
            [tolglow.persistence.audio :as saved-audio]
            [tolglow.compiler :as compiler]
            [tolglow.effects.keyframes :as effect-keyframes]
            [tolglow.effects.variable-blocks :as variable-blocks]
            [tolglow.effects.prefabs :as prefabs]
            [tolglow.effects.group-channels :as group-channels]
            [tolglow.surface.link :as surface]
            [tolglow.modulation :as modulation]
            [tolglow.effects.parameter-sources :as parameter-sources]
            [tolglow.lighting-capacity :as capacity]
            [tolglow.show.lighting :as lighting]
            [tolglow.atmosphere.control :as fog-control]
            [tolglow.lighting-scenes :as lighting-scenes]
            [tolglow.personalities :as personalities]
            [tolglow.signals :as signals]
            [tolglow.composition.response :as response]
            [tolglow.composition.sequence :as sequence]
            [tolglow.composition :as composition]
            [tolglow.agents :as agents]
            [tolglow.autopilot.actions :as conductor-actions]
            [tolglow.spatial :as spatial]
            [tolglow.spatial.actions :as spatial-actions]
            [tolglow.spatial.cues :as spatial-cues]
            [tolglow.fixtures.library :as fixture-service]
            [tolglow.rig :as rig]
            [afterglow.runtime.keyframes.storage :as keyframe-storage]
            [tolglow.rig.pixel-storage :as pixel-storage]
            [tolglow.event-subscribers :as event-subscribers]
            [tolglow.integrations.live-sync :as live-sync]
            [tolglow.repl.sessions :as repl-manager]
            [tolglow.sensors :as sensors]
            [afterglow.output.scheduler :as output-scheduler]
            [tolglow.output.pause-look :as pause-look]
            [tolglow.routing :as routing]
            [tolglow.knobs :as knobs]
            [tolglow.integrations.audio-return :as audio-return]
            [tolglow.integrations.section-patterns :as section-patterns]
            [tolglow.integrations.ableton :as ableton])
  (:import [java.io PushbackReader StringReader]
           [com.fasterxml.jackson.core JsonFactory]))

(def schema-version 2)

(def max-document-characters (* 16 1024 1024))

(def ^:private recent-cue-capacity
  (let [{:keys [columns rows]} (cue-pages/page {} cue-pages/recent-page-id)]
    (* columns rows)))

(def ^:private model-keys
  #{:show-id :epoch :revision :time-ms :registry :instances :pages :variables
    :master :blackout? :event-subscribers :transport :control :rig :compositions :composition-runs
    :composition-tokens :auto :responses :integrations :extensions :frame-number
    :last-frame-time-ms :last-frame-beat :macros :sequences :reactive :settings :modulation :waveforms :routing :sensors
    :knobs})

(def ^:private secret-keys
  #{"api-key" "api_key" "apikey" "authorization" "password" "secret"
    "access-token" "refresh-token" "provider-key" "credentials" "pairing-token"})

(defn- fail! [path message]
  (throw (ex-info message {:persistence/error :invalid-show
                          :errors [{:path path :message message}]})))

(defn- check! [condition path message] (when-not condition (fail! path message)))

(defn- identifier? [x] (or (keyword? x) (and (string? x) (not (str/blank? x)))))

(defn- natural? [x] (and (integer? x) (not (neg? x))))

(defn- nonnegative? [x] (and (registry/finite-number? x) (not (neg? x))))

(defn- check-errors! [errors path]
  (when (seq errors)
    (throw (ex-info "Invalid saved show data"
                    {:persistence/error :invalid-show
                     :errors (mapv #(update % :path (partial into path)) errors)}))))

(defn- portable! [value path depth]
  (check! (< depth 128) path "Show data exceeds nesting limit")
  (cond
    (or (nil? value) (string? value) (keyword? value) (boolean? value) (char? value)
        (instance? java.util.Date value) (uuid? value) (bytes? value)) nil
    (number? value) (check! (registry/finite-number? value) path "Numbers must be finite")
    (and (map? value) (or (not (record? value)) (wire/color-value? value)))
    (doseq [[key child] value]
      (when (or (keyword? key) (string? key))
        (check! (not (secret-keys (str/lower-case (if (keyword? key) (name key) key))))
                (conj path key) "Raw secrets are runtime-only; persist credential references instead"))
      (portable! key (conj path :map-key) (inc depth))
      (portable! child (conj path key) (inc depth)))
    (or (vector? value) (set? value) (sequential? value))
    (doseq [[index child] (map-indexed vector value)]
      (check! (< index max-document-characters) path "Show collection exceeds the document limit")
      (portable! child (conj path index) (inc depth)))
    :else (fail! path "Expected a portable show value")))

(defn- validate-registry! [r]
  (check! (map? r) [:registry] "Registry must be a map")
  (check! (every? #{:effects :cues :fixtures :versions :revision :extensions} (keys r))
          [:registry] "Unknown registry keys belong in :extensions")
  (doseq [kind [:effects :cues :fixtures :versions]]
    (check! (map? (get r kind {})) [:registry kind] "Registry entries must be maps"))
  (let [entries (concat
                 (for [kind [:effects :cues :fixtures] [id definition] (get r kind)]
                   [kind id (:version definition) definition [:registry kind id]])
                 (for [[identity definition] (:versions r)]
                   (do (check! (and (vector? identity) (= 3 (count identity)))
                               [:registry :versions identity] "Historical definition key needs kind, ID and version")
                       [(nth identity 0) (nth identity 1) (nth identity 2) definition
                        [:registry :versions identity]])))]
    (doseq [[kind id version definition path] entries]
      (check-errors! (registry/validate-definition kind definition) path)
      (check! (and (= id (:id definition)) (= version (:version definition))
                   (integer? version) (pos? version)) path "Registry identity/version does not match its entry")
      ;; Current and cue-pinned effects must compile. A superseded revision an
      ;; older owner admitted with a dangling expression (#703) stays verbatim in
      ;; history rather than blocking every later backup once it is replaced.
      (when (and (= kind :effects)
                 (or (= definition (get-in r [:effects id]))
                     (some #(and (= id (:effect-id %)) (= version (:effect-version %))) (vals (:cues r)))))
        (check-errors! (compiler/definition-errors kind definition) path)))
    (doseq [kind [:effects :cues :fixtures] [id definition] (get r kind)]
      (when-let [historical (get-in r [:versions [kind id (:version definition)]])]
        (check! (= definition historical) [:registry kind id] "Current definition conflicts with its historical version"))
      (check! (every? #(<= % (:version definition))
                     (for [[[k entry-id version] _] (:versions r) :when (and (= kind k) (= id entry-id))] version))
              [:registry kind id :version] "Current version precedes recorded history"))
    (doseq [[id cue] (:cues r)]
      (let [effect (registry/cue-effect r cue)]
        (check! effect [:registry :cues id :effect-id] "Cue references an unavailable effect version")
        (check-errors! (registry/validate-cue-params effect cue) [:registry :cues id])))))

(defn- timer! [timer path]
  (check! (and (map? timer) (registry/finite-number? (:started-at timer))) path "Timer needs a finite origin")
  (let [duration (:duration timer)
        normalized (time/normalize-duration (:source duration) (:meter duration))]
    (check! (= normalized duration) (conj path :duration) "Timer duration does not match its source and captured meter")))

(defn- validate-instance! [model id instance]
  (let [r (:registry model)
        path [:instances id]
        definition (:definition instance)]
    (check! (and (string? id) (= id (:id instance)) (map? instance)
                 (or (identifier? (:cue-id instance)) (and (contains? instance :cue-id) (nil? (:cue-id instance))))
                 (= (:effect-id instance) (:id definition))
                 (= (:effect-version instance) (:version definition))
                 (map? (:params instance)) (contains? instance :state)
                 (registry/finite-number? (:priority instance)) (natural? (:order instance))
                 (nonnegative? (:started-at-ms instance))
                 (#{:running :releasing} (:phase instance))
                 (#{:persistent :held :timed :one-shot} (:launch-mode instance))
                 (control/reload-policies (or (:reload-policy instance) :pinned)))
            path "Invalid captured running instance")
    (check-errors! (compiler/definition-errors :effects definition) (conj path :definition))
    (system/validate-state instance)
    (parameter-sources/validate-saved! model instance)
    (when-let [window (:tolglow.effects.keyframes/saved instance)]
      (try (keyframe-storage/restore window (get-in model [:rig :fixtures]) (:epoch model))
           (catch Exception e (fail! (conj path :keyframes) (.getMessage e)))))
    (registry/resolve-params (:params definition {}) (:params instance))
    (when-let [recorded (or (get-in r [:versions [:effects (:effect-id instance) (:effect-version instance)]])
                           (let [current (get-in r [:effects (:effect-id instance)])]
                             (when (= (:version current) (:effect-version instance)) current)))]
      (check! (= recorded (aim-paths/published-definition instance)) (conj path :definition)
              "Captured definition conflicts with registry version"))
    (doseq [key [:started-at-beat :release-at-beat] :when (contains? instance key)]
      (check! (registry/finite-number? (get instance key)) (conj path key) "Beat coordinate must be finite"))
    (doseq [key [:duration-ms :release-duration-ms] :when (contains? instance key)]
      (check! (and (nonnegative? (get instance key))
                   (or (= key :release-duration-ms) (pos? (get instance key))))
              (conj path key) "Invalid instance duration"))
    (doseq [key [:duration :release-duration] :when (contains? instance key)]
      (check-errors! (time/validate-duration (get instance key)) (conj path key)))
    (when (:timing-meter instance) (time/normalize-meter (:timing-meter instance)))
    (doseq [key [:duration-timer :release-timer] :when (contains? instance key)]
      (timer! (get instance key) (conj path key))
      (check! (= (get-in instance [key :duration :source])
                 (get instance (if (= key :duration-timer) :duration :release-duration)))
              (conj path key) "Timer source differs from instance duration"))
    (when (= :releasing (:phase instance))
      (check! (nonnegative? (:release-at-ms instance)) (conj path :release-at-ms) "Release needs a nonnegative time"))
    (doseq [[start-key duration-key] [[:started-at-ms :duration-ms] [:release-at-ms :release-duration-ms]]
            :when (and (contains? instance start-key) (contains? instance duration-key))]
      (check! (registry/finite-number? (+ (get instance start-key) (get instance duration-key)))
              (conj path duration-key) "Instance deadline exceeds finite time"))
    (when (and (#{:timed :one-shot} (:launch-mode instance)) (= :running (:phase instance)))
      (check! (or (:duration-ms instance) (and (:duration instance) (:duration-timer instance)))
              path "Timed instances require a duration and its timer"))))

(defn- validate-compositions! [model]
  (doseq [key [:compositions :composition-runs :composition-tokens]]
    (check! (map? (get model key {})) [key] "Composition state must be a map"))
  (doseq [[id definition] (:compositions model)]
    (check! (= id (:id definition)) [:compositions id] "Composition ID mismatch")
    (composition/save model definition))
  (let [owners (mapv :owner-id (vals (:composition-runs model)))]
    (check! (= (count owners) (count (distinct owners))) [:composition-runs] "Composition owner IDs must be distinct")
    (doseq [[id run] (:composition-runs model)]
      (check! (and (map? run) (= (:epoch model) (:epoch run))
                   (identifier? (:owner-id run)) (nonnegative? (:started-at-ms run))
                   (= id (get-in run [:definition :id]))) [:composition-runs id] "Invalid composition execution")
      (composition/save model (:definition run))
      (when-let [spec (:spec run)]
        (check-errors! (sequence/validate-sequence spec) [:composition-runs id :spec])
        (let [state (:state run) clock (:clock run)]
          (check! (and (map? state) (= (:owner-id run) (:owner-id state))
                       (natural? (:step-index state)) (< (:step-index state) (count (:steps spec)))
                       (integer? (:seed state)) (natural? (:visit state))
                       (nonnegative? (:last-time-ms state))
                       (nonnegative? (:last-time-ms clock)) (nonnegative? (:logical-time-ms clock))
                       (boolean? (:playing? clock))) [:composition-runs id :state] "Invalid sequence execution state")
          (let [key (if (= :beats (:timebase spec)) :step-end-beat :step-end-ms)]
            (check! (registry/finite-number? (get state key)) [:composition-runs id :state key] "Sequence needs a finite step boundary")))))
    (doseq [[token instance-id] (:composition-tokens model)]
      (check! (and (vector? token) (seq token) (some #{(first token)} owners) (string? instance-id))
              [:composition-tokens token] "Token must belong to a saved execution and name an instance"))))

(defn- validate-auto! [model]
  (when-let [{:keys [enabled? config state]} (:auto model)]
    (check! (and (boolean? enabled?) (map? config)
                 (or (and (false? enabled?) (nil? state))
                     (and (map? state) (integer? (:seed state)) (natural? (:tick state)) (identifier? (:owner-id state))
                          (or (nil? (:epoch state)) (= (:epoch model) (:epoch state)))
                          (vector? (:owned-instance-ids state)) (every? string? (:owned-instance-ids state)))))
            [:auto] "Invalid auto session/RNG/ownership state")
    (check! (and (integer? (:count config 1)) (<= 1 (:count config 1) 64))
            [:auto :config :count] "Auto count must be between 1 and 64")
    (let [scope (:scope config :all)]
      (check! (or (= :all scope)
                   (and (map? scope)
                        (or (and (:page-id scope) (get-in model [:pages (:page-id scope)]))
                            (and (vector? (:cue-ids scope))
                                 (every? #(get-in model [:registry :cues %]) (:cue-ids scope))))))
              [:auto :config :scope] "Auto scope must name saved cues or a saved page"))
    (let [duration (or (:interval config) (when (:interval-ms config) {:ms (:interval-ms config)}) {:beats 1})]
      (check-errors! (time/validate-duration duration) [:auto :config :interval])
      (check! (pos? (val (first duration))) [:auto :config :interval] "Auto interval must be positive"))
    (when (:interval-timer state) (timer! (:interval-timer state) [:auto :state :interval-timer]))
    (doseq [key [:next-at-ms :next-at-beat] :when (contains? state key)]
      (check! (registry/finite-number? (get state key)) [:auto :state key] "Auto boundary must be finite"))))

(defn- validate-ableton! [bridge path]
  (check! (map? bridge) path "Ableton bridge must be portable configuration/state")
  (ableton/new-bridge bridge)
  (doseq [key [:mappings :parameters :instances :held] :when (contains? bridge key)]
    (check! (map? (get bridge key)) (conj path key) "Ableton maps must remain maps"))
  (doseq [[note mapping] (:mappings bridge)]
    (check! (and (map? mapping) (identifier? (:cue-id mapping))
                 (#{:persistent :held :timed :one-shot} (:mode mapping :persistent)))
            (conj path :mappings note) "Invalid Ableton cue link"))
  (doseq [[cc mapping] (:parameters bridge)]
    (check! (and (map? mapping) (keyword? (:param mapping))
                 (or (string? (:instance-id mapping)) (identifier? (:cue-id mapping))))
            (conj path :parameters cc) "Parameter links need a cue/instance and parameter ID")
    (when-let [bounds (:bounds mapping)]
      (check! (and (vector? bounds) (= 2 (count bounds)) (every? registry/finite-number? bounds) (apply < bounds))
              (conj path :parameters cc :bounds) "Parameter bounds must increase"))))

(defn- validate-recent-cues! [control]
  (when (contains? control :recent-cue-ids)
    (let [ids (:recent-cue-ids control)
          path [:control :recent-cue-ids]]
      (check! (vector? ids) path "Recent cue history must be an ordered vector")
      (check! (<= (count ids) recent-cue-capacity) path "Recent cue history exceeds the page capacity")
      (doseq [[index id] (map-indexed vector ids)]
        (check! (identifier? id) (conj path index) "Recent cue history needs a stable cue ID"))
      (check! (= (count ids) (count (distinct ids))) path "Recent cue history must contain distinct cue IDs"))))

(defn- edit-counter? [value]
  (and (natural? value) (< value Long/MAX_VALUE)))

(defn- edit-label? [value]
  (or (keyword? value) (and (string? value) (not (empty? value)))))

(defn- saved-edit-target? [target epoch]
  (when (vector? target)
    (let [[kind id param archived-epoch] target]
      (and (string? id) (not (empty? id)) (keyword? param)
           (case kind
             :instance-param (= 3 (count target))
             :unavailable-instance-param (and (= 4 (count target)) (natural? archived-epoch)
                                              (<= archived-epoch epoch))
             false)))))

(defn- validate-edit-display! [display path]
  (check! (and (map? display) (seq display)
               (every? #{:cue-id :cue-label :effect-id :effect-label} (keys display)))
          path "Invalid saved edit target display")
  (doseq [[id label] [[:cue-id :cue-label] [:effect-id :effect-label]]
          :when (or (contains? display id) (contains? display label))]
    (check! (and (identifier? (get display id)) (string? (get display label)) (seq (get display label)))
            path "Saved edit target display needs an ID and label")))

(defn- validate-edit-history! [{:keys [epoch] :as model}]
  (let [path [:control :edit-history]
        history (get-in model path)
        {:keys [limit nodes cursor next-id position generations open]} (when (map? history) history)]
    (check! (and (map? history)
                 (every? #{:limit :nodes :cursor :next-id :position :generations :open} (keys history))
                 (or (not (contains? history :open)) (= {} open)))
            path "Saved edit history must be a closed tree")
    (when (contains? history :position)
      (check! (edit-counter? position) (conj path :position) "Invalid saved edit history position"))
    (check! (and (integer? limit) (<= 2 limit control/edit-history-limit)
                 (map? nodes) (<= 1 (count nodes) limit)
                 (map? generations) (< (count generations) (count nodes)))
            path "Saved edit history exceeds its bounds or lacks a root")
    (check! (and (edit-counter? cursor) (contains? nodes cursor) (edit-counter? next-id))
            path "Invalid saved edit history cursor or next node ID")
    (check! (= 1 (count (filter #(= {:parent nil} %) (vals nodes))))
            (conj path :nodes) "Saved edit history must have one root")
    (doseq [[target generation] generations]
      (check! (and (saved-edit-target? target epoch) (edit-counter? generation))
              (conj path :generations target) "Invalid saved edit target or generation"))
    (doseq [[id node] nodes]
      (let [node-path (conj path :nodes id)
            {:keys [parent target actor control-id before after generation]} (when (map? node) node)]
        (check! (and (edit-counter? id) (< id next-id)) node-path "Invalid saved edit node ID")
        (when-not (= {:parent nil} node)
          ;; Edits are appended and can only follow earlier nodes. With one
          ;; root this also proves each path terminates at that root.
          (check! (and (map? node) (= #{:parent :target :actor :control-id :before :after :generation}
                                     (set (keys (dissoc node :target-display))))
                       (edit-counter? parent) (< parent id) (contains? nodes parent))
                  node-path "Saved edit must refer to an earlier parent")
          (check! (and (contains? generations target) (edit-counter? generation)
                       (<= generation (get generations target)))
                  node-path "Saved edit must reference a tracked target generation")
          (check! (and (edit-label? actor) (edit-label? control-id)
                       (registry/finite-number? before) (registry/finite-number? after) (not= before after))
                  node-path "Invalid saved edit labels or values")
          (when (contains? node :target-display)
            (validate-edit-display! (:target-display node) (conj node-path :target-display))))))
    (check! (= (set (keys generations)) (into #{} (keep :target) (vals nodes)))
            (conj path :generations) "Saved edit generations must match retained history targets")))

(defn validate-model
  "Validate a complete portable model without sampling effects or publishing IO.
  Provider defaults are canonicalized. Unknown top-level keys must be explicit
  :extensions; no live resource or callback source is evaluated."
  [model]
  (portable! model [] 0)
  (check! (and (map? model) (every? model-keys (keys model))) [] "Unknown model fields belong in :extensions")
  (control/new-show model)
  (doseq [key [:registry :instances :pages :variables :transport :control :rig]]
    (check! (map? (get model key)) [key] "Complete show needs this map"))
  (validate-recent-cues! (:control model))
  (when (contains? (:control model) :edit-history) (validate-edit-history! model))
  (check! (map? (:extensions model {})) [:extensions] "Extensions must be an explicit map")
  (when (contains? (:extensions model) variations/state-key)
    (check! (variations/valid-saved-state? (get-in model [:extensions variations/state-key]))
            [:extensions variations/state-key] "Invalid cue variation banks"))
  (check! (map? (:settings model {})) [:settings] "Settings must be an explicit portable map")
  (try (group-channels/validate-state! model)
       (catch clojure.lang.ExceptionInfo error
         (fail! (:path (ex-data error) [:settings :effect-group-channels]) (ex-message error))))
  (when-let [s (get-in model [:settings :fog-control])] (fog-control/settings s))
  ;; Control surfaces and links (#705, #706): a malformed link would break every snapshot.
  (try (surface/validate-state! model)
       (when-let [s (get-in model [:settings :controllers])] (surface/validate-settings! s))
       (catch clojure.lang.ExceptionInfo error
         (fail! [:extensions :tolglow.surface.link/state] (ex-message error))))
  (lighting-scenes/validate-state! model)
  (try (personalities/validate-state! model)
       (catch clojure.lang.ExceptionInfo error
         (check-errors! (:errors (ex-data error)) [:extensions personalities/state-key])))
  (lighting/validate! (:settings model))
  (when (contains? (:settings model) :section-patterns)
    (section-patterns/override-settings (get-in model [:settings :section-patterns])))
  (prefabs/validate! (get-in model [:settings :prefabs]))
  (variable-blocks/compile-model model)
  (registry/validate-public-query-ids! (get-in model [:settings :studio :public-queries]))
  (when-let [s (get-in model [:settings :spatial])] (spatial/settings s))
  (when (contains? (:settings model) :lighting-capacity)
    (capacity/settings (get-in model [:settings :lighting-capacity])))
  (when (contains? (:settings model) :expressive)
    (try (expressive/normalize-config (get-in model [:settings :expressive]))
         (catch clojure.lang.ExceptionInfo error
           (check-errors! (:errors (ex-data error)) [:settings :expressive]))))
  (when (contains? (get-in model [:settings :output] {}) :pause-look)
    (pause-look/normalize (get-in model [:settings :output :pause-look])))
  (when (contains? (get-in model [:settings :output] {}) :presentation)
    (output-scheduler/validate-options (get-in model [:settings :output :presentation])))
  (validate-registry! (:registry model))
  (when-let [config (get-in model [:settings :audio-returns])] (audio-return/validate-config nil config))
  (check-errors! (rig/errors (:rig model)) [:rig])
  (let [transport (:transport model)]
    (check! (and (registry/finite-number? (:bpm transport)) (<= 1 (:bpm transport) 1000)
                 (boolean? (:playing? transport)) (registry/finite-number? (:beat transport)))
            [:transport] "Transport needs finite beat, supported BPM and play state")
    (time/normalize-meter (:meter transport))
    (when (contains? transport :time-ms)
      (check! (nonnegative? (:time-ms transport)) [:transport :time-ms] "Transport time must be nonnegative")))
  (doseq [key [:frame-number] :when (contains? model key)]
    (check! (natural? (get model key)) [key] "Frame count must be nonnegative"))
  (when (contains? model :last-frame-time-ms)
    (check! (nonnegative? (:last-frame-time-ms model)) [:last-frame-time-ms] "Frame time must be nonnegative"))
  (doseq [key [:next-instance :next-order :history-limit]]
    (check! (and (natural? (get-in model [:control key]))
                 (or (not= key :history-limit) (pos? (get-in model [:control key]))))
            [:control key] "Invalid control counter/history limit"))
  (doseq [[id instance] (:instances model)] (validate-instance! model id instance))
  (let [orders (map :order (vals (:instances model)))]
    (check! (= (count orders) (count (distinct orders))) [:instances] "Instance launch orders must be unique"))
  (doseq [[id page] (:pages model)]
    (check! (and (identifier? id) (= id (:id page)) (map? (:cells page))) [:pages id] "Invalid page")
    (doseq [[position cue] (:cells page)]
      (check! (and (vector? position) (= 2 (count position)) (every? natural? position)
                   (get-in model [:registry :cues cue])) [:pages id :cells position] "Invalid cue placement")))
  (validate-compositions! model)
  (validate-auto! model)
  (check! (vector? (:responses model [])) [:responses] "Responses must be an ordered vector")
  (doseq [r (:responses model)] (response/compile-response r))
  (when (contains? model :modulation)
    (check-errors! (modulation/validate-state (:modulation model)) [:modulation]))
  (when (contains? model :waveforms) (signals/validate-registry (:waveforms model)))
  (when (contains? model :routing) (routing/validate-state (:routing model)))
  (when (contains? model :sensors) (sensors/validate-state (:sensors model)))
  (when (contains? model :knobs)
    (try (knobs/validate-state (:knobs model))
         (catch clojure.lang.ExceptionInfo error (fail! (:path (ex-data error) [:knobs]) (ex-message error)))))
  (when-let [integrations (:integrations model)]
    (check! (map? integrations) [:integrations] "Integration configuration must be a map")
    (when-let [bridge (:ableton integrations)] (validate-ableton! bridge [:integrations :ableton])))
  (cond-> model
    (get-in model [:integrations :agents]) (update-in [:integrations :agents] agents/validate-state)
    (get-in model [:integrations :fixtures]) (update-in [:integrations :fixtures] fixture-service/validate-state)
    (get-in model [:integrations :sync]) (update-in [:integrations :sync] live-sync/validate-state)
    (get-in model [:integrations :repl]) (update-in [:integrations :repl] repl-manager/validate-state)
    (contains? model :event-subscribers) (update :event-subscribers event-subscribers/validate-state)
    (contains? model :reactive) (update :reactive reactive-registry/validate-registry)))

(defn validate-frame-output
  "Validate an already portable committed frame. Byte conversion happens only
  at the IO shell: signed [-128,127] and unsigned [0,255] values are accepted."
  [model output]
  (portable! output [:frame-output] 0)
  (let [patch (render/compile-patch (get-in model [:rig :fixtures]))
        targets (mapv :id (get-in model [:rig :fixtures]))
        universes (set (:universes patch))]
    (check! (and (map? output) (= #{:values :target-order :universes} (set (keys output))))
            [:frame-output] "Frame output needs values, target order and universes")
    (check! (and (map? (:values output)) (every? (set targets) (keys (:values output)))
                 (every? map? (vals (:values output))))
            [:frame-output :values] "Frame values must use saved rig target IDs")
    (check! (= targets (:target-order output)) [:frame-output :target-order]
            "Frame target order must match the saved rig")
    (check! (and (map? (:universes output)) (= universes (set (keys (:universes output)))))
            [:frame-output :universes] "Frame universe keys must match the saved patch")
    (doseq [[universe channels] (:universes output)]
      (check! (and (vector? channels) (= 512 (count channels))
                   (every? #(and (integer? %) (<= -128 % 255)) channels))
              [:frame-output :universes universe] "Each universe needs 512 signed or unsigned channel bytes")))
  output)

(defn- validate-document! [document]
  (check! (and (map? document) (= schema-version (:schema-version document))
               (contains? document :model)
               (every? #{:schema-version :model :frame-output} (keys document)))
          [] "Expected a schema-2 show document")
  (let [document (update document :model validate-model)]
    (when (contains? document :frame-output)
      (validate-frame-output (:model document) (:frame-output document)))
    document))

(defn- fits-file? [source]
  (and (string? source) (<= (count source) max-document-characters)
       (<= (alength (.getBytes ^String source java.nio.charset.StandardCharsets/UTF_8))
           max-document-characters)))

(defn- pack-universes [universes]
  (update-vals universes
               (fn [channels]
                 ;; The renderer exports unsigned channels. Signed author-supplied
                 ;; values remain vectors so their numeric convention round-trips.
                 (if (every? #(<= 0 % 255) channels)
                   (byte-array (map unchecked-byte channels))
                   channels))))

(defn- unpack-universes [universes]
  (update-vals universes #(if (bytes? %) (mapv (fn [b] (bit-and b 255)) %) %)))

(defn- encode-document
  "Encode a validated show and check the actual file size."
  [document]
  (let [source (wire/encode (cond-> (update-in document [:model :rig] pixel-storage/compact)
                              (:frame-output document)
                              (update-in [:frame-output :universes] pack-universes)))]
    (check! (fits-file? source) [] "Show document exceeds the 16 MiB UTF-8 limit")
    source))

(defn- decode-document [source]
  (let [document (wire/decode source)
        document (cond-> document (get-in document [:model :rig])
                   (update-in [:model :rig] pixel-storage/expand))]
    (cond-> document (get-in document [:frame-output :universes])
      (update-in [:frame-output :universes] unpack-universes))))

(defn- prepare-edit-history [model]
  (if-let [history (control/edit-history-snapshot model)]
    (let [receipts (get-in model [:control :history])
          gesture-ids (into #{} (keep (fn [[id receipt]]
                                        (when (#{:gesture/begin :gesture/sample :gesture/end :gesture/cancel}
                                                (get-in receipt [:command :op])) id))) receipts)]
      (cond-> (assoc-in model [:control :edit-history] history)
        (:modulation model) (update-in [:modulation :holds] modulation/portable-holds)
        (seq gesture-ids) (update-in [:control :history] #(apply dissoc % gesture-ids))
        (seq gesture-ids) (update-in [:control :history-order] #(filterv (complement gesture-ids) %))))
    model))

(defn- make-document [model frame-output]
  (let [{:keys [model settled]} (control/settle-temporary-gestures-for-save model)
        model (if (:modulation model)
                (reduce-kv (fn [model [_ id param] {:keys [actor control-id gesture-id]}]
                             (update model :modulation modulation/settle-gesture-hold id param
                                     [actor control-id gesture-id] false)) model settled)
                model)
        sampled? (some (comp pos? :last-sequence val) settled)
        model (cond-> model sampled? effect-keyframes/without-checkpoints)]
    ;; A sampled temporary value may already be in the captured frame or
    ;; predictive windows. The restore path can regenerate them safely.
    (cond-> {:schema-version schema-version :model (validate-model (prepare-edit-history (cue-timing/without-queue model)))}
      (and (some? frame-output) (not sampled?)) (assoc :frame-output (validate-frame-output model frame-output)))))

(defn freeze
  "Return a detached, size-checked snapshot including any timestamp or binary values."
  ([model] (freeze model nil))
  ([model frame-output]
   (decode-document (encode-document (make-document model frame-output)))))

(defn- remap-references [value instance-ids owner-ids]
  (letfn [(walk [value]
            (cond
              (wire/color-value? value) value
              (map? value)
              (into {} (for [[key child] value]
                         [key (case key
                                :instance-id (get instance-ids child child)
                                (:instance-ids :owned-instance-ids) (if (vector? child) (vec (keep instance-ids child)) child)
                                :owner-id (get owner-ids child child)
                                (:token :active-token) (if (and (vector? child) (seq child))
                                                        (assoc child 0 (get owner-ids (first child) (first child))) child)
                                (walk child))]))
              (vector? value) (mapv walk value)
              (set? value) (set (map walk value))
              (list? value) (apply list (map walk value))
              :else value))]
    (walk value)))

(defn- restore-edit-history [history instance-ids epoch]
  (let [rekey (fn [[kind id param :as target]]
                (if (= :instance-param kind)
                  (if-let [new-id (get instance-ids id)]
                    [kind new-id param]
                    ;; This tag is never addressable, even when the old ID
                    ;; matches an instance assigned by this or a later restore.
                    [:unavailable-instance-param id param epoch])
                  target))]
    (-> history
        (dissoc :open)
        (update :nodes update-vals #(if (:target %) (update % :target rekey) %))
        (update :generations #(into {} (map (fn [[target generation]] [(rekey target) generation])) %)))))

(defn thaw
  "Validate schema 2 and return a complete replacement model. Preserve saved
  logical time/state; caller rebases its wall/monotonic clock before publication.
  New epoch exceeds saved/current epochs. IDs and domain ownership are rekeyed;
  command receipts, external held-note leases and session sources are not revived.
  No schema-1/legacy compatibility path is provided. Failure leaves inputs intact."
  [current-model document]
  (validate-document! document)
  (check! (and (natural? (:epoch current-model)) (natural? (:revision current-model)))
          [:current-model] "Current model needs valid epoch/revision")
  ;; Device sessions never survive a restore, so every surface loads detached.
  (let [saved (-> (:model document) cue-timing/without-queue saved-audio/restore validate-model audio-return/disarm
                  surface/detach-all)
        epoch (inc (max (:epoch current-model) (:epoch saved)))
        ordered (sort-by (juxt :order :id) (vals (:instances saved)))
        ids (into {} (map-indexed (fn [i instance] [(:id instance) (str epoch ":" (inc i))]) ordered))
        owners (into {} (map-indexed (fn [i owner] [owner (str "restore:" epoch ":" i)])
                                    (distinct (concat (map :owner-id (vals (:composition-runs saved)))
                                                      (keep #(get-in % [:source :native-layer :id]) ordered)
                                                      (when-let [owner (get-in saved [:auto :state :owner-id])] [owner])))))
        model (assoc (spatial/restore (lighting-scenes/restore saved ids) ids epoch) :epoch epoch :revision (inc (max (:revision current-model) (:revision saved))))
        instances (into {} (for [instance ordered
                                 :let [id (ids (:id instance))
                                       owner (get-in instance [:source :session-id])
                                       layer (get-in instance [:source :native-layer])
                                       source (or (spatial-cues/session-source saved instance)
                                                  (spatial-actions/session-source saved instance)
                                                  (conductor-actions/session-source (get-in saved [:extensions :tolglow.autopilot/run :actions]) instance)
                                                  (lighting-scenes/session-source saved instance)
                                                  (if-let [new-owner (owners owner)]
                                                {:adapter (get-in instance [:source :adapter]) :session-id new-owner}
                                                {:adapter :restore :epoch epoch}))]]
                             [id (-> (reduce (fn [entry field]
                                               (if (get entry field)
                                                 (update entry field #(assoc % :instance-id id :epoch epoch)) entry))
                                             instance [:fault :reload-fault :retry-fault])
                                     (assoc :id id :source (cond-> source layer
                                                            (assoc :native-layer (update layer :id owners))))
                                     (dissoc :gesture-id))]))
        runs (into {} (for [[id run] (:composition-runs saved)]
                        [id (assoc (remap-references run ids owners) :epoch epoch)]))
        tokens (into {} (for [[token instance-id] (:composition-tokens saved) :when (ids instance-id)]
                          [(assoc token 0 (owners (first token))) (ids instance-id)]))
        model (cond-> (assoc model :instances instances)
                (get-in saved [:extensions :tolglow.autopilot/run])
                (update-in [:extensions :tolglow.autopilot/run]
                           (fn [run] (-> run (update :actions conductor-actions/restore ids epoch)
                                      (assoc-in [:controller :epoch] epoch)
                                      (assoc-in [:history :clock :epoch] epoch)
                                      (cond-> (:target-response run) (assoc-in [:target-response :clock :epoch] epoch)))))
                (contains? saved :composition-runs) (assoc :composition-runs runs)
                (contains? saved :composition-tokens) (assoc :composition-tokens tokens)
                (get-in saved [:auto :state]) (update :auto #(-> (remap-references % ids owners) (assoc-in [:state :epoch] epoch)))
                (:responses saved) (update :responses #(remap-references % ids owners))
                (:modulation saved) (update :modulation modulation/restore-state
                                             {:show-id (:show-id model) :epoch epoch :instance-ids ids})
                (:event-subscribers saved) (update :event-subscribers event-subscribers/restore ids)
                (:routing saved) (update :routing routing/restore-state {:epoch epoch :instance-ids ids})
                (:knobs saved) (update :knobs knobs/restore ids epoch)
                (:sensors saved) (update :sensors #(remap-references % ids owners))
                (:reactive saved) (update :reactive #(remap-references % ids owners)))
        ;; Historical chat/tool results are evidence, not live references.
        ;; Only operational integration mappings participate in ID rebinding.
        model (reduce (fn [m key]
                        (if (get-in m [:integrations key])
                          (update-in m [:integrations key] #(remap-references % ids owners)) m))
                      model [:ableton :audio :analysis])
        model (if-let [bridge (get-in model [:integrations :ableton])]
                (assoc-in model [:integrations :ableton]
                          (ableton/restore-bridge bridge (into {} (for [[old-id cue] (get-in saved [:integrations :ableton :instances])
                                                                  :when (ids old-id)] [(ids old-id) cue])))) model)
        model (assoc model :control
                     (assoc (:control saved) :next-instance (count ids)
                            :next-order (max (get-in saved [:control :next-order])
                                             (if (seq ordered) (inc (apply max (map :order ordered))) 0))
                            :history {} :history-order []))
        model (cond-> model
                (get-in saved [:control :edit-history])
                (update-in [:control :edit-history] restore-edit-history ids epoch))]
    (validate-model (sensors/prepare-restored-model model))))

(defn write-string "Freeze and serialize one schema-2 show as Transit JSON."
  ([model] (write-string model nil))
  ([model frame-output] (encode-document (make-document model frame-output))))

(def ^:private json-factory (delay (JsonFactory.)))

(defn- read-transit-file [source]
  ;; A Transit reader consumes a single value and can leave trailing input.
  ;; Scan the document boundary without creating a second in-memory JSON tree.
  (with-open [parser (.createParser ^JsonFactory @json-factory ^String source)]
    (check! (.nextToken parser) [] "Empty show document")
    (.skipChildren parser)
    (check! (nil? (.nextToken parser)) [] "Expected exactly one Transit document"))
  (decode-document source))

(defn- read-edn-file [source]
  (with-open [reader (PushbackReader. (StringReader. source))]
    (let [eof (Object.)
          options {:eof eof :readers {} :default (fn [tag _] (fail! [] (str "Unsupported EDN tag " tag)))}
          document (edn/read options reader)]
      (check! (identical? eof (edn/read options reader)) [] "Expected exactly one show document")
      document)))

(defn read-string
  "Read a complete Transit show. Previously saved schema-2 EDN maps can also be imported."
  [source]
  (check! (fits-file? source) [] "Show source must fit the 16 MiB UTF-8 limit")
  (validate-document! (if (re-find #"^\s*\{\s*:" source)
                        (read-edn-file source)
                        (read-transit-file source))))
