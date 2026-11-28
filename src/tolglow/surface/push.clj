(ns tolglow.surface.push
  "Plumbing the Push 2 and Push 3 surface mappings share (#706): one
  selection between a cue grid and the Afterglow profile's edit target, a
  host outbox that is sent outside the session lock, the Capture button, and
  the session connect scaffolding. Nothing here knows a MIDI address; the
  device namespaces (tolglow.surface.push-2, tolglow.surface.push-3) decode
  their own events and pass this namespace device-independent values.

  The grid state of a session lives under ::grid in its Afterglow profile
  state. Its selection keys: :adopted, the committed value last applied;
  :edited, the profile target last seen; :publishing, a profile target
  waiting to be committed; :outbox, the publish request for the host."
  (:require [afterglow.control.devices.session :as session]
            [afterglow.control.devices.transports :as transports]
            [tolglow.surface.link :as link]))

(def own-ops
  "Owner ops a surface sends for itself; their receipts never reach the
  Afterglow profile's :ack."
  #{:control-surface/layout :control-surface/bank :instance/move-after})

(defn running? [model id] (= :running (get-in model [:instances id :phase])))

(defn fence
  "Stamp grid command templates with the model's show identity, so the
  session refuses them after a show change."
  [model commands]
  (mapv #(merge % (select-keys model [:show-id :epoch])) commands))

(defn isolate
  "Wrap an Afterglow profile so that its functions run on the state without
  the surface's own keys, which are put back on their results. Surface
  decorators stacked on top then see and return the whole state."
  [own-keys profile]
  (let [strip #(apply dissoc % own-keys)
        own #(select-keys % own-keys)
        result (fn [f] (fn [state & args]
                         (let [r (apply f (strip state) args)]
                           (if (and r (contains? r :state)) (update r :state merge (own state)) r))))
        local (fn [f] (fn [state & args] (merge (apply f (strip state) args) (own state))))]
    (reduce (fn [p [k wrap]] (if-let [f (get p k)] (assoc p k (wrap f)) p))
            profile
            [[:input result] [:feedback result] [:stop result] [:ack result]
             [:stale-input local] [:reset-model local]])))

;; ---------------------------------------------------------------------------
;; One selection. The surface's committed :selected and the launch the
;; Afterglow profile edits (:selected-instance) are kept equal both ways.

(defn point-at
  "Make a running launch what the Push encoders and display edit. A change
  of launch drops the touches held on the old one, as the Push 2 profile
  does when it changes its own target."
  [state model id]
  (cond-> (-> state
              (assoc :selected-instance id :selected-cue (get-in model [:instances id :cue-id]))
              (assoc-in [::grid :edited] id))
    (and (contains? state :touch-order) (not= id (:selected-instance state)))
    (assoc :touches {} :touch-order [])))

(defn adopt-selection
  "A committed selection change (a pad, Studio, the REPL or a linked member)
  points the encoders at that launch and drops a pending publish, including
  a request already in the outbox."
  [state model v]
  (let [[kind id :as selected] (:selected v)]
    (if (= selected (get-in state [::grid :adopted]))
      state
      (cond-> (update state ::grid assoc :adopted selected :publishing nil :outbox nil)
        (and (= :instance kind) (running? model id)) (point-at model id)))))

(defn follow-committed
  "Before the profile's feedback: adopt a committed change, and when the
  profile's own target is no longer running (a Push pad just stopped it),
  point back at the committed launch instead of letting the display fall
  back to another one."
  [state model v]
  (let [state (adopt-selection state model v)
        [kind id] (:selected v)
        current (:selected-instance state)]
    (if (and (some? current) (not= current id) (not (running? model current))
             (= :instance kind) (running? model id))
      (point-at state model id)
      state)))

(def ^:private retry-refreshes
  "Refreshes to wait for a published selection to commit before sending it
  again (about 200 ms at the 10 ms cadence)."
  20)

(def ^:private max-retries 3)

(defn track-selection
  "After the profile's feedback, against the committed model it saw: a
  running target the profile picked itself (a launch pad and its ack, a cue
  gesture, the display's fallback to the latest launch) becomes a pending
  publish of the shared selection. It is put in the outbox once the surface
  is attached, so a target picked before attach commits is published too.
  If it has not committed after retry-refreshes refreshes (a refusal), it
  is sent again, at most max-retries times. Feedback never dispatches: the
  host sends the outbox outside the session lock (see publisher)."
  [state model v envelope]
  (let [grid (::grid state) current (:selected-instance state) committed (:selected v)
        state (if (= current (:edited grid))
                state
                (update state ::grid assoc :edited current :emitted? false :waited 0 :retries 0 :outbox nil
                        :publishing (when (and (some? current) (running? model current)
                                               (not= [:instance current] committed))
                                      [:instance current])))
        {:keys [publishing emitted? waited retries]} (::grid state)
        emit (fn [state] (update state ::grid assoc :emitted? true :waited 0
                                 :outbox {:id (:command-id envelope) :selected publishing :over committed}))]
    (cond
      (nil? publishing) state
      (= publishing committed) (update state ::grid assoc :publishing nil :outbox nil)
      (not (running? model (second publishing))) (update state ::grid assoc :publishing nil :outbox nil)
      (not (:attached? v)) state
      (not emitted?) (emit state)
      (and (>= (inc (or waited 0)) retry-refreshes) (< (or retries 0) max-retries))
      (-> state emit (update-in [::grid :retries] (fnil inc 0)))
      :else (update-in state [::grid :waited] (fnil inc 0)))))

(defn select-pressed
  "After a Select pad: point the encoders at the chosen launch, even when it
  is already the committed selection, and record it as adopted."
  [state model [kind id :as selection]]
  (cond-> state
    (and (= :instance kind) (running? model id))
    (-> (point-at model id) (assoc-in [::grid :adopted] selection))))

;; ---------------------------------------------------------------------------
;; Capture button (#671)

(defn capture-press
  "A Capture button press: queue one host capture, keyed by the command ID
  of the press, for the publisher to send outside the session lock."
  [state envelope]
  (assoc state ::capture {:id (:command-id envelope)}))

;; ---------------------------------------------------------------------------
;; Host outbox

(defn grid-state
  "A session's cue grid state: :mode, :source, :shown, held pads and the
  selection outbox."
  [device]
  (get-in (session/snapshot device) [:local ::grid]))

(defn- committed-selection [model! surface-id viewport]
  (try (get-in (link/window (model!) surface-id viewport) [:layout :selected])
       (catch Exception _ ::unavailable)))

(defn publisher
  "A function that sends a session's newest outbox requests once, even when
  the scheduler and the MIDI input thread both call it.
  - Selection: sent only while it is still the pending target and the
    committed selection is still the one it was queued over: a newer Studio,
    REPL or pad choice that landed in between wins, and the next refresh
    adopts it. Through :publish! (command -> nil, must not wait on the
    owner), or :dispatch! without it.
  - Capture: each press runs the host :capture! (no arguments) once."
  [session-ref {:keys [publish! dispatch! model! capture!] :as options} viewport]
  (let [sent-id (atom nil)
        captured-id (atom nil)
        surface-id (:surface-id options (:session-id options))]
    (fn []
      (when (realized? session-ref)
        (let [local (:local (session/snapshot @session-ref))
              {:keys [publishing] {:keys [id selected over]} :outbox} (::grid local)
              capture (get-in local [::capture :id])
              seen @sent-id
              seen-capture @captured-id]
          (when (and id (not= id seen) (= publishing selected)
                     (= over (committed-selection model! surface-id viewport))
                     (compare-and-set! sent-id seen id))
            ((or publish! dispatch!)
             {:op :control-surface/layout :target surface-id :viewport viewport :set {:selected selected}}))
          (when (and capture capture! (not= capture seen-capture) (compare-and-set! captured-id seen-capture capture))
            (capture!)))))))

(defn publish-pending!
  "Send this session's outbox, if it holds a request not yet sent. Runs
  outside the session lock, after a refresh or an input, so feedback never
  waits on the show owner. Returns the device."
  [device]
  (when-let [publish (::publish device)] (publish))
  device)

(defn refresh!
  "afterglow session/refresh!, then publish-pending!."
  [device]
  (session/refresh! device)
  (publish-pending! device))

(defn connect-session!
  "Open a session for profile with the publisher wrapped around its refresh
  schedule and MIDI input. Without :schedule! it refreshes every :refresh-ms
  (default-refresh-ms), and without :midi or :transport it opens the selected
  MIDI ports. With :schedule! nil call refresh! instead of afterglow
  session/refresh!."
  [options profile viewport default-refresh-ms]
  (let [session-ref (promise)
        publish (publisher session-ref options viewport)
        schedule (if (contains? options :schedule!) (:schedule! options)
                     (transports/scheduler (get options :refresh-ms default-refresh-ms)))
        midi (cond (:midi options) (:midi options)
                   (:transport options) (:transport options)
                   :else (transports/midi-transport options))
        options (assoc options
                       :profile profile
                       :schedule! (when schedule (fn [refresh] (schedule (fn [] (refresh) (publish)))))
                       :midi (update midi :open! (fn [open!] (fn [receive!] (open! (fn [input] (receive! input) (publish)))))))
        device (assoc (session/create options) ::publish publish)]
    (deliver session-ref device)
    (session/open! device)))
