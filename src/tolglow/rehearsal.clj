(ns tolglow.rehearsal
  "Rehearse an autonomous club set and a Push-led concert with the same agent
  (#670). A recorded passage (tolglow.rehearsal.passage) plays through the
  real show owner with captured output: the recorded agent's tool rounds
  stream through tolglow.integrations.agent/run! and show/dispatch-tool!,
  stem onsets and levels enter through the capture callbacks
  (show/audio-callbacks) and Push captures are submitted as shared commands
  with a Push source. Every committed frame is read back headlessly; nothing
  opens a device, a Room window, Live or a provider.

    (require '[tolglow.rehearsal :as rehearsal])
    (def club (rehearsal/club 670))            ; run and review
    (println (:report club))
    (def concert (rehearsal/concert 670))      ; takes :a and :b, compared
    (println (:report concert))
    (rehearsal/write! \"target/rehearsal\" club concert)

  The same inputs and seed reproduce the same accepted actions and output.
  The review measures; whether the result is musical is the operator's call."
  (:require [afterglow.render.frame :as render]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [tolglow.automation.launch-params :as launch-params]
            [tolglow.automation.grid :as grid]
            [tolglow.automation.suggestion :as suggestion]
            [tolglow.integrations.agent :as agent]
            [tolglow.lighting-capacity :as capacity]
            [tolglow.registry :as registry]
            [tolglow.rehearsal.agent-script :as script]
            [tolglow.rehearsal.passage :as passage]
            [tolglow.rehearsal.review :as review]
            [tolglow.show :as show]
            [tolglow.stems :as stems]))

(def default-frame-ms 125.0)

(def provider-config
  "A self-hosted provider entry that is never contacted: the recorded
  transport answers every round."
  {:provider :self-hosted :endpoint "http://recorded.invalid/v1/chat/completions" :model "rehearsal-recording"})

(defn- sse-event [data] (str "data: " (json/write-str data) "\n\n"))

(defn recorded-round
  "One recorded provider round in the OpenAI-compatible streaming format:
  text, then tool calls [{:id :name :arguments}]."
  [text calls]
  (str (sse-event {:choices [{:index 0 :delta {:content text}}]})
       (apply str (map-indexed (fn [index {:keys [id name arguments]}]
                                 (sse-event {:choices [{:index 0 :delta {:tool_calls [{:index index :id id
                                                                                       :function {:name name :arguments (json/write-str arguments)}}]}}]}))
                               calls))
       "data: [DONE]\n\n"))

(defn recorded-transport
  "An agent transport that replays recorded rounds in order and fails on an
  unexpected extra round. It never opens a connection."
  [rounds]
  (let [remaining (atom (vec rounds))]
    (fn [_request on-chunk! _options]
      (let [round (first @remaining)]
        (when-not round (throw (ex-info "The recording has no further provider round" {})))
        (swap! remaining subvec 1)
        (on-chunk! round)
        {:status 200}))))

(defn- tick-until!
  "Tick the owner at time t until done? holds. The agent thread enqueues tool
  commands that the owner only applies on a tick; the logical time does not
  move while it waits."
  [rt t done?]
  (loop [n 0]
    (show/tick! rt t)
    (cond (done?) n
          (> n 20000) (throw (ex-info "The rehearsal agent did not finish" {}))
          :else (do (Thread/sleep 0 50000) (recur (inc n))))))

(defn refused-transport
  "A disconnected agent's transport: every connection is refused at once."
  []
  (fn [_request _on-chunk! _options]
    (throw (java.net.ConnectException. "Connection refused (rehearsal: the agent is disconnected)"))))

(defn silent-transport
  "An agent that never answers: the request hangs until the host cancels it
  or interrupts its thread."
  []
  (fn [_request _on-chunk! {:keys [cancelled?]}]
    (loop []
      (if (and cancelled? @cancelled?)
        (throw (ex-info "The host gave up on a silent agent" {:provider/error :cancelled}))
        (do (Thread/sleep 5) (recur))))))

(defn- agent-transport
  "The transport for a suggestion: the recording, or a disconnected agent
  from the passage's :agent-outage section on."
  [passage suggestion]
  (case (passage/agent-outage passage (:index suggestion 0))
    :refused (refused-transport)
    :silent (silent-transport)
    (recorded-transport [(recorded-round (:text suggestion) (:calls suggestion))
                         (recorded-round "Done." [])])))

(defn- start-agent!
  "Start streaming one suggestion through agent/run!, dispatching its tools
  with show/dispatch-tool!. Returns a handle for finish-agent; the run itself
  is a future that never blocks the show."
  [rt passage suggestion & [cancels]]
  (let [identity (select-keys (:model @(:state rt)) [:epoch :show-id])
        events (atom [])
        cancelled? (atom false)]
    ;; Whoever owns the take can cancel every run it started (show stop, an
    ;; interrupted replay), so no silent run outlives it.
    (when cancels (swap! cancels conj cancelled?))
    {:events events :cancelled? cancelled? :started (System/nanoTime)
     :suggested-beat (:asked-beat suggestion (get-in @(:state rt) [:model :transport :beat]))
     :result (future
               (agent/run! provider-config nil [{:role :user :content (:prompt suggestion)}] agent/default-tools
                           (fn [tool arguments metadata]
                             ;; One conversation per take: the agent owns what it posted in earlier turns.
                             (show/dispatch-tool! rt tool arguments (merge metadata identity {:expected-epoch (:epoch identity)
                                                                                              :conversation-id (:conversation-id suggestion)})))
                           #(swap! events conj %)
                           {:transport (agent-transport passage suggestion)
                            :cancelled? cancelled?
                            :run-id (:run-id suggestion)}))}))

(defn- finish-agent
  "The logged suggestion for a realized run: its status, receipts and where
  the transport was when the owner accepted them. :wall-ms is the host's
  turnaround for the run, kept apart from frame timing."
  [rt suggestion {:keys [events started suggested-beat result]}]
  (let [outcome (deref result 5000 {:status :error :error :agent-unfinished})
        results (into {} (keep (fn [e] (when (= :tool/result (:type e)) [(:name e) (:result e)]))) @events)]
    (cond-> {:run-id (:run-id suggestion) :section (:section-id suggestion)
             :section-beat (:beat suggestion)
             :scheduled? (:scheduled? suggestion)
             :suggested-beat suggested-beat
             :accepted-beat (get-in @(:state rt) [:model :transport :beat])
             :status (:status outcome)
             :tools (mapv :name (:calls suggestion))
             :receipts (update-vals results #(select-keys % [:status :message :code :reason :event-ids :due-beats :late? :late-beats]))
             :wall-ms (/ (- (System/nanoTime) started) 1e6)}
      (not= :completed (:status outcome)) (assoc :error (:error outcome) :message (:message outcome)))))

(defn- give-up!
  "The host gives up on a silent agent's run: cancel it and log it as timed
  out. Nothing it would have sent reaches the show."
  [rt suggestion handle]
  (reset! (:cancelled? handle) true)
  (assoc (finish-agent rt suggestion handle) :timed-out? true))

(defn- run-agent!
  "Run one suggestion and wait for it: wait! blocks until the run is realized
  (ticking a headless owner, or simply waiting on a running one) or gives up.
  A run still unrealized after wait! is cancelled and logged as timed out.
  Returns the logged suggestion with receipts."
  [rt passage suggestion wait! & [cancels]]
  (let [handle (start-agent! rt passage suggestion cancels)]
    (wait! (:result handle))
    (if (realized? (:result handle))
      (finish-agent rt suggestion handle)
      (give-up! rt suggestion handle))))

(defn cancel-all!
  "Cancel every agent run registered in cancels."
  [cancels]
  (when cancels (doseq [c @cancels] (reset! c true))))

(defn- newest-instance [model pred]
  (->> (vals (:instances model))
       (filter #(and (= :running (:phase %)) (pred %)))
       (sort-by (juxt :started-at-beat :order :id))
       last))

(defn- resolve-select
  "The instance a recorded gesture was bound to, from its :select."
  [model taken {:keys [cue-id adapter taken?]}]
  (if taken?
    (let [id (get taken cue-id)]
      (when (= :running (get-in model [:instances id :phase])) id))
    (:id (newest-instance model #(and (= cue-id (:cue-id %))
                                      (or (nil? adapter) (= adapter (get-in % [:source :adapter]))))))))

(defn- instance-summary [categories instance]
  {:cue-id (:cue-id instance)
   :adapter (get-in instance [:source :adapter])
   ;; The auto session that launched it, so the review attributes a launch to
   ;; the settings that made it rather than to when it was seen.
   :session (get-in instance [:source :session-id])
   :phase (:phase instance)
   :params (:params instance)
   :launch-mode (:launch-mode instance)
   :duration (:duration instance)
   :started-at-beat (:started-at-beat instance)
   :categories (categories (:cue-id instance))})

(defn- cue-categories [model]
  (memoize (fn [cue-id]
             (let [cue (get-in model [:registry :cues cue-id])]
               (when cue (launch-params/cue-categories (registry/cue-effect (:registry model) cue) cue))))))

(defn- light [meter universes]
  (if-not (seq universes) {:lit #{} :brightness 0.0}
    (let [^doubles levels (capacity/local-brightnesses meter universes)
          heads (:writers-by-head meter)
          values (map (fn [[id [index _]]] [id (aget levels (int index))]) heads)]
      {:lit (into (sorted-set) (keep (fn [[id v]] (when (> v 1e-3) id))) values)
       :brightness (if (seq values) (/ (reduce + (map second values)) (count values)) 0.0)})))

(defn- frame-record [model meter universes categories passage t]
  (let [beat (get-in model [:transport :beat])
        instances (into (sorted-map) (map (fn [[id i]] [id (instance-summary categories i)])) (:instances model))
        {:keys [lit brightness]} (light meter universes)]
    {:t t :beat beat :playing? (get-in model [:transport :playing?])
     :music (:id (passage/section-at passage beat))
     :auto-section (get-in model [:auto :state :section :id])
     :auto-section-start (get-in model [:auto :state :section :timer :started-at])
     :auto-section-length (get-in model [:auto :state :section :timer :duration :value])
     :auto-interval (let [{:keys [started-at duration]} (get-in model [:auto :state :interval-timer])]
                      (when (= :beats (:unit duration))
                        {:started-at started-at :value (:value duration)
                         :step (grid/step (:source duration) (:value duration) (:meter duration))}))
     :auto-tick (get-in model [:auto :state :tick])
     :auto-session (get-in model [:auto :state :owner-id])
     :instances instances
     :lit lit :brightness brightness
     :strobe? (boolean (some #(and (= :running (:phase %)) (contains? (:categories %) :strobe)) (vals instances)))
     :dmx-hash (hash (into (sorted-map) (map (fn [[u bytes]] [u (vec bytes)])) universes))}))

(defn- route-stems!
  "Route stem onsets and levels through tolglow.stems, as Studio's stem panel
  does. after! lets a headless owner apply the receipts."
  [rt routes after!]
  (let [receipts (doall
                  (concat
                   (for [[stem cue-id] (sort-by (comp str key) (:onsets routes))]
                     (:receipt (stems/route-onset! rt stem cue-id)))
                   (mapcat (fn [{:keys [stem cue-id param]}]
                             (let [{:keys [saved enabled]} (stems/route-level! rt stem cue-id param)] [saved enabled]))
                           (:levels routes))))]
    (after!)
    (let [statuses (mapv #(:status (deref % 5000 ::missing)) receipts)]
      (when-not (every? #{:accepted} statuses)
        (throw (ex-info "Stem routes were refused" {:statuses statuses})))
      statuses)))

(defn- conversation-id
  "One agent conversation per passage take."
  [passage]
  (str "rehearsal:" (name (:id passage)) (when (:take passage) (str ":" (name (:take passage))))))

(defn- window [items prev-beat beat first?]
  (filterv #(let [b (:beat %)] (and (<= b beat) (if first? (>= b prev-beat) (> b prev-beat)))) items))

(def ^:private transport-source
  "Who stops and starts the transport in a rehearsal: the REPL replaying the
  recording, not a performer."
  {:adapter :repl :session-id "rehearsal"})

(defn- initial-progress [passage]
  {:prev-beat 0.0 :first? true :next-section 0 :in-flight nil :agent [] :gestures [] :transport []
   :onset-counts {} :taken {} :stopped nil :pending-stops (:stops passage)})

(defn- deliver!
  "Deliver what the recording holds up to beat at host time t: the agent's
  suggestion when a section line has passed (wait! blocks for it), stem
  onsets and levels through the capture callbacks, Push gestures and
  transport stops as shared commands. Inputs between the previous frame and
  this one reach the owner on its next tick, as a capture block does.
  Returns [progress pending], where pending holds unresolved receipts for
  settle!."
  [rt callbacks passage {:keys [prev-beat first? next-section in-flight onset-counts taken stopped pending-stops] :as progress}
   t beat wait!]
  (let [sections (:sections passage)
        model (:model @(:state rt))
        ;; The agent hears of a section line on the first frame at or past
        ;; the line plus its offset: a bar ahead by default, as a song map
        ;; gives it; :agent-lag-beats simulates a slow or late agent. It
        ;; decides then; its reply arrives :agent-latency-beats later.
        in-flight (or in-flight
                      (when (and (< next-section (count sections))
                                 (>= beat (+ (* passage/beats-per-bar (:start-bar (nth sections next-section)))
                                             (passage/agent-heard-offset passage))))
                        ;; The agent reads the playing auto session when it hears of the line
                        ;; and names it as its suggestion's basis (#670).
                        (assoc (script/suggestion passage next-section beat {:auto-session (suggestion/auto-session model)})
                               :asked-beat beat
                               ;; The first round is the agent's plan before the downbeat (a
                               ;; Room rehearsal takes it before play starts); later replies
                               ;; take the latency.
                               :arrives-beat (+ beat (if (zero? next-section) 0 (:agent-latency-beats passage 0)))
                               :conversation-id (conversation-id passage))))
        due? (and in-flight (nil? (:handle in-flight)) (>= beat (- (:arrives-beat in-flight) 1e-9)))
        ;; A silent agent's run is left hanging while the show plays on; the
        ;; host gives up after :agent-timeout-ms of host time (#670).
        in-flight (if (and due? (= :silent (passage/agent-outage passage (:index in-flight))))
                    (assoc in-flight :handle (start-agent! rt passage in-flight (:cancels progress)) :asked-at-ms t)
                    in-flight)
        suggestion (cond
                     (and due? (nil? (:handle in-flight))) (run-agent! rt passage in-flight wait! (:cancels progress))
                     (and (:handle in-flight)
                          (>= t (+ (:asked-at-ms in-flight) (:agent-timeout-ms passage passage/default-agent-timeout-ms))))
                     (give-up! rt (dissoc in-flight :handle) (:handle in-flight)))
        model (:model @(:state rt))
        onsets (window (:onsets passage) prev-beat beat first?)
        levels (window (:levels passage) prev-beat beat first?)
        gestures (window (:gestures passage) prev-beat beat first?)
        onset-counts (reduce #(update %1 (:stem %2) (fnil inc 0)) onset-counts onsets)
        _ (when (seq onsets)
            ((:events! callbacks)
             (mapv (fn [{:keys [stem beat]}]
                     {:type :audio/onset :time-ms t :beat beat :stem stem
                      :source {:type :stem-analysis :id (keyword "stem" (name stem))}
                      :inference :recorded-onset :confidence 1.0 :transport-playing? true})
                   onsets)))
        _ (when (seq levels)
            ((:features! callbacks)
             {:time-ms t :capture {:run-id (str "rehearsal-" (name (:id passage)) (some->> (:take passage) name (str "-")))}
              :stems (into {} (map (fn [{:keys [stem level]}]
                                     [stem {:level level :onset-count (get onset-counts stem 0)}]))
                           (vals (into {} (map (juxt :stem identity)) levels)))}))
        [gesture-entries taken]
        (reduce (fn [[entries taken] {:keys [command select label] :as gesture}]
                  (let [target (when select (resolve-select model taken select))
                        command (cond-> (assoc command :source passage/push
                                               :command-id (str "push-3:" (name (:id passage)) ":"
                                                                (some-> (:take passage) name) ":" (:beat gesture)))
                                  target (assoc :instance-id target))
                        entry {:label label :beat (:beat gesture) :op (:op command)
                               :cue-id (or (:cue-id command) (:cue-id select)) :param (:param command)
                               :value (:value command) :target target}]
                    (if (and select (nil? target))
                      [(conj entries (assoc entry :status :no-target)) taken]
                      [(conj entries (assoc entry :reply (show/submit! rt :command command)))
                       (cond-> taken
                         (and target (= :param/set (:op command)) (= :auto (:adapter select)))
                         (assoc (:cue-id select) target))])))
                [[] taken] gestures)
        ;; Transport stops: stop on the first frame at the recorded beat,
        ;; play again after the recorded hold.
        stop-now (when (and (nil? stopped) (seq pending-stops) (>= beat (:beat (first pending-stops))))
                   (first pending-stops))
        resume? (and stopped (>= t (+ (:at-ms stopped) (:hold-ms stopped))))
        transport-reply (cond
                          stop-now (show/submit! rt :command {:op :transport/set :source transport-source :transport {:playing? false}})
                          resume? (show/submit! rt :command {:op :transport/set :source transport-source :transport {:playing? true}}))]
    [(-> progress
         (assoc :prev-beat beat :first? false :onset-counts onset-counts :taken taken
                :in-flight (when-not suggestion in-flight)
                :stopped (cond stop-now (assoc stop-now :at-ms t) resume? nil :else stopped)
                :pending-stops (if stop-now (vec (rest pending-stops)) pending-stops))
         (cond-> suggestion (-> (update :next-section inc) (update :agent conj suggestion))))
     {:gestures gesture-entries
      :transport (when transport-reply {:op (if stop-now :stop :play) :at-beat beat :t t :reply transport-reply})}]))

(defn- settle
  "Resolve the receipts deliver! left pending once the owner has applied
  them; accepted-beat is the beat of the frame that applied them."
  [progress {:keys [gestures transport]} accepted-beat]
  (cond-> (update progress :gestures into
                  (map (fn [e] (if-let [reply (:reply e)]
                                 (-> (dissoc e :reply)
                                     (merge (select-keys (deref reply 5000 {:status :missing}) [:status :code :message :instance-id]))
                                     (assoc :accepted-beat accepted-beat))
                                 e)))
                  gestures)
    transport (update :transport conj (-> (dissoc transport :reply)
                                          (assoc :status (:status (deref (:reply transport) 5000 {:status :missing})))))))

(defn- applied
  "Where each agent change took effect. A change sent with :at applies when
  the scheduler fires it, on the first frame at or past its line, before the
  planner decides on that frame: :applied-beat is that frame's beat, from the
  scheduler's recent outcomes (nil, with :applied-status, if it was refused
  or missed). Any other change applied when the owner accepted it."
  [agent-log recent]
  (mapv (fn [{:keys [scheduled? receipts accepted-beat status] :as entry}]
          (cond
            ;; A disconnected or silent agent sent nothing (#670).
            (not= :completed status)
            (assoc entry :applied-beat nil :applied-status (if (:timed-out? entry) :timed-out :not-sent))
            ;; The show refused it, as stale or invalid, when it arrived.
            (= :rejected (get-in receipts ["configure_auto" :status]))
            (assoc entry :applied-beat nil
                   :applied-status (or (get-in receipts ["configure_auto" :code]) :rejected))
            (not scheduled?)
            (assoc entry :applied-beat accepted-beat)
            :else
            (let [id (first (get-in receipts ["configure_auto" :event-ids]))
                  outcome (last (filter #(= id (:event-id %)) recent))]
              (cond-> (assoc entry :applied-beat (when (= :fired (:status outcome)) (:at-beat outcome))
                             :applied-status (:status outcome :pending))
                ;; Arrived after its line, within a bar: applied at once, marked late.
                (:late-beats outcome) (assoc :late-beats-on-arrival (:late-beats outcome))))))
        agent-log))

(defn- launch-observer
  "Read accepted auto launches from the show's shared command receipts as
  they commit, including launches ended again within the same owner tick that
  no frame shows. Returns a function to call after each run of ticks; it
  stamps new launches with the transport beat they committed at and returns
  all observed so far, in commit order."
  [rt]
  (let [seen (volatile! (peek (get-in @(:state rt) [:model :control :history-order])))
        launches (volatile! [])]
    (fn []
      (let [model (:model @(:state rt))
            order (get-in model [:control :history-order] [])
            from (if-let [last-id @seen] (inc (.lastIndexOf ^java.util.List order last-id)) 0)
            beat (get-in model [:transport :beat])]
        (doseq [id (subvec order (max 0 (min from (count order))))
                :let [{:keys [command ack]} (get-in model [:control :history id])]
                :when (and (= :cue/launch (:op command)) (= :auto (get-in command [:source :adapter]))
                           (= :accepted (:status ack)) (:instance-id ack))]
          (vswap! launches conj {:id (:instance-id ack) :cue-id (:cue-id command) :adapter :auto
                                 :session (get-in command [:source :session-id])
                                 :params (get-in command [:launch :params])
                                 :started-at-beat beat :seen-beat beat}))
        (when (seq order) (vreset! seen (peek order)))
        @launches))))

(defn- close-agent!
  "At the end of a passage, give up on an agent run still hanging, so no
  future outlives the take; it is logged as still open."
  [rt progress]
  (if-let [handle (get-in progress [:in-flight :handle])]
    (-> progress
        (update :agent conj (assoc (give-up! rt (dissoc (:in-flight progress) :handle) handle) :open-at-end? true))
        (assoc :in-flight nil))
    progress))

(defn- held-state
  "What a take left waiting when it ended: auto changes still pending on the
  scheduler and agent runs the host had to close. A disconnected agent must
  leave neither; the show keeps playing the last mood (#670)."
  [rt progress]
  (let [model (:model @(:state rt))]
    {:pending-auto-changes (count (get-in (show/snapshot rt) [:auto :pending]))
     :agent-runs-open-at-end (count (filter :open-at-end? (:agent progress)))
     :auto-enabled? (boolean (get-in model [:auto :enabled?]))
     :beat (get-in model [:transport :beat])}))

(defn- capture-sink [captured]
  {:send! (fn [universes _] (reset! captured (update-vals universes aclone)))})

(defn run-take
  "Play passage through a fresh show owner with captured output, ticking it
  in logical time. Returns the frames, the agent log, gesture and transport
  receipts, and the scheduler's recent outcomes. Options: :frame-ms (default
  125) and :max-frames."
  ([passage] (run-take passage {}))
  ([passage {:keys [frame-ms max-frames] :or {frame-ms default-frame-ms max-frames 20000}}]
   (let [model (passage/model (:bpm passage))
         captured (atom nil)
         rt (show/create-runtime {:model model :sink (capture-sink captured) :workspace-options nil})
         meter (capacity/prepare (render/compile-patch (get-in model [:rig :fixtures])))
         categories (cue-categories model)
         end-beat (passage/length-beats passage)
         cancels (atom #{})]
     (try
       (show/tick! rt 0.0)
       (route-stems! rt (:routes passage) #(show/tick! rt 0.0))
       (let [callbacks (show/audio-callbacks rt)
             observe! (launch-observer rt)]
         (loop [t 0.0 progress (assoc (initial-progress passage) :cancels cancels) frames [] n 0]
           ;; A superseded review replay is cancelled by interrupting its thread.
           (when (.isInterrupted (Thread/currentThread))
             (throw (InterruptedException. "The rehearsal replay was cancelled")))
           (let [beat (get-in @(:state rt) [:model :transport :beat])]
             (if (or (>= beat end-beat) (>= n max-frames))
               (let [progress (close-agent! rt progress)
                     schedule (select-keys (show/schedule rt) [:recent :upcoming])]
                 (merge (select-keys progress [:gestures :transport])
                        {:passage passage :frame-ms frame-ms :frames frames
                         ;; From receipts, so a launch ended in its own tick counts too.
                         :auto-launches (observe!)
                         :agent (applied (:agent progress) (:recent schedule))
                         :schedule schedule
                         :held (held-state rt progress)
                         :status (select-keys (:status @(:state rt)) [:error])}))
               (let [[progress pending] (deliver! rt callbacks passage progress t beat
                                                  (fn [result] (tick-until! rt t #(realized? result)) (observe!)))
                     next-t (+ t frame-ms)
                     _ (show/tick! rt next-t)
                     _ (observe!)
                     record (frame-record (:model @(:state rt)) meter @captured categories passage next-t)]
                 (recur next-t (settle progress pending (:beat record)) (conj frames record) (inc n)))))))
       (finally (cancel-all! cancels) (show/stop! rt))))))

(defn arm!
  "Prepare passage on a running show runtime that holds (room-model passage)
  with its transport stopped: route the stems and take the agent's first
  round, which posts a concert's anchors ahead of time. In real time the
  transport would otherwise have passed bar 0 before that post arrives, and
  the scheduler refuses a position already behind it. Nothing plays and
  nothing is armed for physical output. The first round waits at most the
  passage's :agent-timeout-ms; a silent agent is then cancelled and logged
  as timed out. Options: :cancels, an atom the caller can cancel every agent
  run with (show stop). Returns the armed state for play!."
  ([runtime passage] (arm! runtime passage {}))
  ([runtime passage {:keys [cancels] :or {cancels (atom #{})}}]
   (when (get-in @(:state runtime) [:model :transport :playing?])
     (throw (ex-info "Arm a rehearsal on a stopped transport at beat 0; load (room-model passage) first" {})))
   (route-stems! runtime (:routes passage) (fn []))
   (let [model (:model @(:state runtime))
         timeout-ms (long (:agent-timeout-ms passage passage/default-agent-timeout-ms))
         first-suggestion (run-agent! runtime passage
                                      (assoc (script/suggestion passage 0 (double (get-in model [:transport :beat] 0))
                                                                {:auto-session (suggestion/auto-session model)})
                                             :index 0 :conversation-id (conversation-id passage))
                                      (fn [result] (deref result timeout-ms nil))
                                      cancels)]
     {:passage passage
      :progress (-> (initial-progress passage) (assoc :next-section 1 :cancels cancels) (update :agent conj first-suggestion))})))

(defn play!
  "Play passage in real time on a running show runtime, for a Room-only
  rehearsal: the show's own scheduler commits frames that Studio's Room (or
  any observer) renders, and nothing is armed for physical output by this
  call. The runtime must hold (room-model passage); play! arms it first
  unless :armed (from arm!) is given, then starts the transport. Polls every
  poll-ms (default 5). Returns the agent, gesture and transport logs when
  the passage ends, after :timeout-ms, or when :stop? (a function) turns
  true. :on-progress receives the progress after every poll."
  ([runtime passage] (play! runtime passage {}))
  ([runtime passage {:keys [poll-ms timeout-ms armed stop? on-progress] :or {poll-ms 5 stop? (constantly false)}}]
   (let [end-beat (passage/length-beats passage)
         started (System/currentTimeMillis)
         timeout-ms (or timeout-ms (+ 30000 (* 2 (/ (* end-beat 60000.0) (:bpm passage)))
                                      (reduce + (map :hold-ms (:stops passage)))))
         now #(double (- (System/currentTimeMillis) started))
         progress (cond armed (:progress armed)
                        (get-in @(:state runtime) [:model :transport :playing?])
                        (do (route-stems! runtime (:routes passage) (fn [])) (assoc (initial-progress passage) :cancels (atom #{})))
                        :else (:progress (arm! runtime passage)))]
     (when-not (get-in @(:state runtime) [:model :transport :playing?])
       (deref (show/submit! runtime :command {:op :transport/set :source transport-source
                                              :transport {:playing? true}}) 5000 nil))
     (let [callbacks (show/audio-callbacks runtime)
           wait! (fn [result] (deref result 10000 nil))]
       (loop [progress progress]
         (let [beat (get-in @(:state runtime) [:model :transport :beat])
               stopped? (boolean (stop?))]
           (if (or (>= beat end-beat) (> (now) timeout-ms) stopped?)
             (let [progress (close-agent! runtime progress)]
               (cancel-all! (:cancels progress))
               (assoc (select-keys progress [:gestures :transport])
                      :agent (applied (:agent progress) (:recent (show/schedule runtime)))
                      :held (held-state runtime progress)
                      :ended-at-beat beat :stopped? stopped?
                      :timed-out? (and (< beat end-beat) (not stopped?))))
             (let [[progress pending] (deliver! runtime callbacks passage progress (now) beat wait!)
                   progress (settle progress pending (get-in @(:state runtime) [:model :transport :beat]))]
               (when on-progress (on-progress progress))
               (Thread/sleep (long poll-ms))
               (recur progress)))))))))

(defn room-model
  "The rehearsal model for passage with its transport stopped at beat 0, for
  a running runtime that play! will start."
  [passage]
  (assoc-in (passage/model (:bpm passage)) [:transport :playing?] false))

(defn replay-signature
  "What two runs of the same passage and seed must share: every accepted
  action, committed instance and output hash per frame, and the agent's
  accepted beats and receipts. Wall-clock agent turnaround is left out."
  [take]
  {:frames (mapv #(dissoc % :t) (:frames take))
   :agent (mapv #(dissoc % :wall-ms) (:agent take))
   :gestures (:gestures take)
   :transport (mapv #(dissoc % :t) (:transport take))})

(defn club
  "Run and review the unattended club passage for seed."
  ([seed] (club seed {}))
  ([seed options]
   (let [take (run-take (passage/club seed (:passage options {})) options)]
     (review/club take))))

(defn concert
  "Run takes :a and :b of the concert passage for seed and compare them."
  ([seed] (concert seed {}))
  ([seed options]
   (let [a (run-take (passage/concert seed :a (:passage options {})) options)
         b (run-take (passage/concert seed :b (:passage options {})) options)]
     (review/concert a b))))

(defn write!
  "Write each review's data (EDN, without per-frame records) and its text
  report under directory. Returns the written paths."
  [directory & reviews]
  (vec
   (mapcat (fn [{:keys [id report] :as result}]
             (let [base (io/file directory (name id))
                   data (io/file (str base ".edn"))
                   text (io/file (str base ".md"))]
               (io/make-parents data)
               (spit data (with-out-str (pprint/pprint (review/artefact result))))
               (spit text report)
               [(.getPath data) (.getPath text)]))
           reviews)))
