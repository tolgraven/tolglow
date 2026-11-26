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

(defn- sse-round
  "One recorded provider round in the OpenAI-compatible streaming format."
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

(defn- run-agent!
  "Stream one recorded suggestion through agent/run! and dispatch its tools
  with show/dispatch-tool!. wait! blocks until the run is realized (ticking a
  headless owner, or simply waiting on a running one). Returns the logged
  suggestion with receipts. :wall-ms is the host's turnaround for the run,
  kept apart from frame timing."
  [rt suggestion wait!]
  (let [suggested-beat (get-in @(:state rt) [:model :transport :beat])
        identity (select-keys (:model @(:state rt)) [:epoch :show-id])
        events (atom [])
        started (System/nanoTime)
        result (future
                 (agent/run! provider-config nil [{:role :user :content (:prompt suggestion)}] agent/default-tools
                             (fn [tool arguments metadata]
                               (show/dispatch-tool! rt tool arguments (merge metadata identity {:expected-epoch (:epoch identity)})))
                             #(swap! events conj %)
                             {:transport (recorded-transport [(sse-round (:text suggestion) (:calls suggestion))
                                                              (sse-round "Done." [])])
                              :run-id (:run-id suggestion)}))]
    (wait! result)
    (let [outcome @result
          results (into {} (keep (fn [e] (when (= :tool/result (:type e)) [(:name e) (:result e)]))) @events)]
      {:run-id (:run-id suggestion) :section (:section-id suggestion)
       :section-beat (:beat suggestion)
       :scheduled? (:scheduled? suggestion)
       :suggested-beat suggested-beat
       :accepted-beat (get-in @(:state rt) [:model :transport :beat])
       :status (:status outcome)
       :tools (mapv :name (:calls suggestion))
       :receipts (update-vals results #(select-keys % [:status :message :code :event-ids :due-beats]))
       :wall-ms (/ (- (System/nanoTime) started) 1e6)})))

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

(defn- window [items prev-beat beat first?]
  (filterv #(let [b (:beat %)] (and (<= b beat) (if first? (>= b prev-beat) (> b prev-beat)))) items))

(def ^:private transport-source
  "Who stops and starts the transport in a rehearsal: the REPL replaying the
  recording, not a performer."
  {:adapter :repl :session-id "rehearsal"})

(defn- initial-progress [passage]
  {:prev-beat 0.0 :first? true :next-section 0 :agent [] :gestures [] :transport []
   :onset-counts {} :taken {} :stopped nil :pending-stops (:stops passage)})

(defn- deliver!
  "Deliver what the recording holds up to beat at host time t: the agent's
  suggestion when a section line has passed (wait! blocks for it), stem
  onsets and levels through the capture callbacks, Push gestures and
  transport stops as shared commands. Inputs between the previous frame and
  this one reach the owner on its next tick, as a capture block does.
  Returns [progress pending], where pending holds unresolved receipts for
  settle!."
  [rt callbacks passage {:keys [prev-beat first? next-section onset-counts taken stopped pending-stops] :as progress}
   t beat wait!]
  (let [sections (:sections passage)
        ;; The agent hears of a section line on the first frame at or past
        ;; the line plus its offset: a bar ahead by default, as a song map
        ;; gives it; :agent-lag-beats simulates a slow or late agent.
        suggestion (when (and (< next-section (count sections))
                              (>= beat (+ (* passage/beats-per-bar (:start-bar (nth sections next-section)))
                                          (passage/agent-heard-offset passage))))
                     (run-agent! rt (script/suggestion passage next-section beat) wait!))
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
  (mapv (fn [{:keys [scheduled? receipts accepted-beat] :as entry}]
          (if-not scheduled?
            (assoc entry :applied-beat accepted-beat)
            (let [id (first (get-in receipts ["configure_auto" :event-ids]))
                  outcome (last (filter #(= id (:event-id %)) recent))]
              (assoc entry :applied-beat (when (= :fired (:status outcome)) (:at-beat outcome))
                     :applied-status (:status outcome :pending)))))
        agent-log))

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
         end-beat (passage/length-beats passage)]
     (try
       (show/tick! rt 0.0)
       (route-stems! rt (:routes passage) #(show/tick! rt 0.0))
       (let [callbacks (show/audio-callbacks rt)]
         (loop [t 0.0 progress (initial-progress passage) frames [] n 0]
           (let [beat (get-in @(:state rt) [:model :transport :beat])]
             (if (or (>= beat end-beat) (>= n max-frames))
               (let [schedule (select-keys (show/schedule rt) [:recent])]
                 (merge (select-keys progress [:gestures :transport])
                        {:passage passage :frame-ms frame-ms :frames frames
                         :agent (applied (:agent progress) (:recent schedule))
                         :schedule schedule
                         :status (select-keys (:status @(:state rt)) [:error])}))
               (let [[progress pending] (deliver! rt callbacks passage progress t beat
                                                  (fn [result] (tick-until! rt t #(realized? result))))
                     next-t (+ t frame-ms)
                     _ (show/tick! rt next-t)
                     record (frame-record (:model @(:state rt)) meter @captured categories passage next-t)]
                 (recur next-t (settle progress pending (:beat record)) (conj frames record) (inc n)))))))
       (finally (show/stop! rt))))))

(defn play!
  "Play passage in real time on a running show runtime, for a Room-only
  rehearsal: the show's own scheduler commits frames that Studio's Room (or
  any observer) renders, and nothing is armed for physical output by this
  call. The runtime must hold (room-model passage). Polls the
  transport every poll-ms (default 5). Returns the agent, gesture and
  transport logs when the passage ends, or after :timeout-ms."
  ([runtime passage] (play! runtime passage {}))
  ([runtime passage {:keys [poll-ms timeout-ms] :or {poll-ms 5}}]
   (let [end-beat (passage/length-beats passage)
         started (System/currentTimeMillis)
         timeout-ms (or timeout-ms (+ 30000 (* 2 (/ (* end-beat 60000.0) (:bpm passage)))
                                      (reduce + (map :hold-ms (:stops passage)))))
         now #(double (- (System/currentTimeMillis) started))]
     (route-stems! runtime (:routes passage) (fn []))
     (let [callbacks (show/audio-callbacks runtime)
           wait! (fn [result] (deref result 10000 nil))
           ;; A Room rehearsal starts from a stopped transport at beat 0 (see
           ;; room-model). The agent's first suggestion, which posts a
           ;; concert's anchors ahead of time, is taken before play starts:
           ;; in real time the transport would otherwise have passed bar 0
           ;; before the post arrives, and the scheduler refuses a position
           ;; already behind it.
           progress (if (get-in @(:state runtime) [:model :transport :playing?])
                      (initial-progress passage)
                      (let [first-suggestion (run-agent! runtime (script/suggestion passage 0) wait!)]
                        (deref (show/submit! runtime :command {:op :transport/set :source transport-source
                                                               :transport {:playing? true}}) 5000 nil)
                        (-> (initial-progress passage) (assoc :next-section 1) (update :agent conj first-suggestion))))]
       (loop [progress progress]
         (let [beat (get-in @(:state runtime) [:model :transport :beat])]
           (if (or (>= beat end-beat) (> (now) timeout-ms))
             (assoc (select-keys progress [:gestures :transport])
                    :agent (applied (:agent progress) (:recent (show/schedule runtime)))
                    :ended-at-beat beat :timed-out? (< beat end-beat))
             (let [[progress pending] (deliver! runtime callbacks passage progress (now) beat wait!)
                   progress (settle progress pending (get-in @(:state runtime) [:model :transport :beat]))]
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
