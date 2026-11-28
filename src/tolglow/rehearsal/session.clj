(ns tolglow.rehearsal.session
  "Arm, start and stop a Room rehearsal on a running show and read its review
  (#670). Studio's rehearsal panel and the REPL call the same functions:

    (require '[tolglow.rehearsal.session :as session])
    (session/arm! runtime {:mode :club :seed 670 :replace-show? true :wait-ms 20000})
    (session/start! runtime)          ; plays in real time; watch the Room
    (session/stop! runtime)           ; or let the passage end
    (:review (session/status runtime))
    (session/review! runtime)         ; the seeded replay's review without playing
    (session/restore! runtime)        ; bring back the show arming replaced

  Arming runs on a background thread (status :arming, then :armed): it saves
  the open show as a named checkpoint, loads the rehearsal show (the
  passage's rig and cues, transport stopped at beat 0) through the ordinary
  :import restore and waits until that restore has committed, then takes
  the agent's first round, bounded by the agent timeout. Replacing a show
  this session did not load needs :replace-show? true. Starting plays the
  passage with tolglow.rehearsal/play! on a background thread; stopping, or
  the passage ending, stops the transport, which pauses every effect, and
  the review is computed. Nothing is armed for physical output.

  The review is the headless seeded replay of the same passage, seed and
  agent options (tolglow.rehearsal/club or concert): the same inputs replay
  the same accepted actions, so it measures what the Room showed. One replay
  runs at a time and each carries a :review-id. A concert review compares
  both takes. The real-time run's own agent, gesture and transport receipts
  are kept beside it under :run.

  The session lives with the runtime as an observer, so it closes when the
  show stops: the passage stops and every agent run it started is
  cancelled."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [tolglow.persistence :as persistence]
            [tolglow.show.recovery :as recovery]
            [tolglow.rehearsal :as rehearsal]
            [tolglow.rehearsal.agent-script :as script]
            [tolglow.rehearsal.passage :as passage]
            [tolglow.show :as show]))

(def ^:private observer-id ::session)

(def modes #{:club :concert})

(defn- session
  "The runtime's rehearsal session: {:state atom :cancels atom :review-job
  atom}, created on first use and closed with the show. Closing stops a
  playing passage, cancels every agent run the session started and the
  running review replay."
  [runtime]
  (show/attach-observer! runtime observer-id
    (fn []
      (let [state (atom {:status :idle})
            cancels (rehearsal/new-runs)
            review-job (atom nil)]
        {:state state :cancels cancels :review-job review-job
         :frame! (fn [_]) :events! (fn [& _]) :tracing? (constantly false)
         :close! (fn []
                   (swap! state assoc :stop? true :closed? true)
                   (rehearsal/close-runs! cancels)
                   (locking review-job (some-> @review-job future-cancel)))}))))

(defn- refuse [message & [details]]
  (throw (ex-info message (merge {:rehearsal/error :refused} details))))

(defn- choice
  "A validated rehearsal choice: mode, take, seed and the agent's timing."
  [{:keys [mode take seed agent] :as request}]
  (let [mode (keyword (or mode :club))
        take (when (= :concert mode) (keyword (or take :a)))
        seed (if (some? seed) seed 670)
        agent (or agent {})
        options (cond-> {}
                  (some? (:latency-beats agent)) (assoc :agent-latency-beats (:latency-beats agent))
                  (some? (:lead-beats agent)) (assoc :agent-lead-beats (:lead-beats agent))
                  (some? (:lag-beats agent)) (assoc :agent-lag-beats (:lag-beats agent))
                  (:outage agent) (assoc :agent-outage (-> (:outage agent) (update :mode keyword)))
                  (some? (:timeout-ms agent)) (assoc :agent-timeout-ms (:timeout-ms agent)))]
    (when-let [t (:agent-timeout-ms options)]
      (when-not (and (number? t) (Double/isFinite (double t)) (<= 1 t 120000))
        (refuse "The agent timeout is 1 to 120000 ms of host time" {:timeout-ms t})))
    (when-not (map? request) (refuse "A rehearsal choice is a map"))
    (when-not (modes mode) (refuse "Rehearse :club or :concert" {:mode mode}))
    (when-not (or (nil? take) (#{:a :b} take)) (refuse "A concert take is :a or :b" {:take take}))
    (when-not (and (integer? seed) (<= 0 seed Integer/MAX_VALUE)) (refuse "The seed is a nonnegative integer" {:seed seed}))
    (doseq [[k v] (select-keys options [:agent-latency-beats :agent-lead-beats :agent-lag-beats])]
      (when-not (and (number? v) (Double/isFinite (double v)) (<= -64 v 64))
        (refuse (str "Agent timing " (name k) " is a number of beats from -64 to 64") {k v})))
    {:mode mode :take take :seed seed
     :options (cond-> options (:agent-timeout-ms options) (update :agent-timeout-ms double))}))

(defn passage-for
  "The passage a choice plays."
  [{:keys [mode take seed options]}]
  (passage/agent-timing options) ; validates the outage
  (if (= :concert mode) (passage/concert seed take options) (passage/club seed options)))

(defn- current-epoch [runtime] (get-in @(:state runtime) [:model :epoch]))

(defn- check-identity!
  "A request from a page that bound an earlier show session is refused."
  [runtime {:keys [runtime-id expected-epoch]}]
  (when (and runtime-id (not= runtime-id (:id runtime)))
    (refuse "This rehearsal request belongs to another show window" {:code :stale-show-session}))
  (when (and (some? expected-epoch) (not= expected-epoch (current-epoch runtime)))
    (refuse "The show changed since this page loaded; reload and try again" {:code :stale-show-session})))

(defn- moods
  "What the agent will set at each section line of passage: the mood
  configure_auto sends, as the review lists it."
  [p]
  (mapv (fn [i {:keys [id kind start-bar bars anchor]}]
          (let [config (script/auto-configuration p {:id id :kind kind} i)]
            (cond-> {:section id :kind kind :bar start-bar :bars bars
                     :scope (get-in config [:scope :cue-ids]) :interval (:interval config) :count (:count config)
                     :launch (get-in config [:launch :mode])}
              (:max-lit config) (assoc :max-lit (:max-lit config))
              (:intent config) (assoc :intent (:intent config))
              (:sections config) (assoc :plan (mapv :id (:sections config)))
              anchor (assoc :anchor anchor))))
        (range) (:sections p)))

(defn- agent-row [entry]
  (cond-> (select-keys entry [:section :section-beat :suggested-beat :accepted-beat :applied-beat :applied-status
                              :scheduled? :status :wall-ms :timed-out? :error])
    (:receipts entry) (assoc :receipts (update-vals (:receipts entry) #(select-keys % [:status :code :message])))))

(defn- run-summary [result]
  {:agent (mapv agent-row (:agent result))
   :gestures (mapv #(select-keys % [:label :beat :status :accepted-beat :message]) (:gestures result))
   :transport (mapv #(select-keys % [:op :at-beat :status]) (:transport result))
   :held (:held result)
   :ended-at-beat (:ended-at-beat result) :stopped? (:stopped? result) :timed-out? (:timed-out? result)})

(defn- response-row [r]
  (select-keys r [:section :position :section-beat :outcome :scheduled? :accepted-beat :applied-beat :late-beats
                  :carry-over-launches :early-launches :first-launch-beat :refusal :receipts :agent-error]))

(defn review-summary
  "The parts of a review Studio shows: flags, what the agent changed and how
  each change landed, phrasing, phrase-by-phrase repetition, contrast,
  continuity and the text report. No frame records."
  [review]
  (case (:id review)
    :club {:id :club :flags (:flags review) :report (:report review)
           :responses (mapv response-row (:responses review))
           :phrases (mapv #(select-keys % [:phrase :first-bar :music :auto-sections :launches :cues :new-cues :retained-cues
                                           :novelty :mean-lit :mean-brightness :strobe-fraction])
                          (:phrases review))
           :repetition (:repetition review)
           :launch-timing (:launch-timing review)
           :boundaries {:count (count (:boundaries review))
                        :with-agent-change (count (filter :with-agent-change? (:boundaries review)))
                        :on-bar (count (filter :on-bar? (remove :with-agent-change? (:boundaries review))))
                        :on-phrase (count (filter :on-phrase? (remove :with-agent-change? (:boundaries review))))}
           :contrast (:contrast review)
           :stops (:stops review)
           :continuity (:continuity review)}
    :concert {:id :concert :flags (:flags review) :report (:report review)
              :takes (mapv (fn [t] {:take (:take t)
                                    :responses (mapv response-row (:agent t))
                                    :anchors (:anchors t)
                                    :gestures (mapv #(select-keys % [:label :beat :status :accepted-beat]) (:gestures t))
                                    :continuity (:continuity t)})
                           (:takes review))
              :comparison (:comparison review)}))

(defn- review-of [{:keys [mode seed options]}]
  (review-summary (if (= :concert mode)
                    (rehearsal/concert seed {:passage options})
                    (rehearsal/club seed {:passage options}))))

(defn- compute-review!
  "Compute the seeded replay's review on a background thread, off the
  lighting deadline, for choice. One replay runs at a time: taking the next
  :review-id, cancelling the running replay and starting the new one happen
  as one step, so concurrent requests cannot strand a review. A closed
  session starts none."
  [{:keys [state review-job]} choice]
  (locking review-job
    (when-not (:closed? @state)
      (let [id (:review-id (swap! state #(-> % (update :review-id (fnil inc 0)) (assoc :review-status :reviewing :review-error nil))))]
        (some-> @review-job future-cancel)
        (reset! review-job
                (future
                  (try
                    (let [review (assoc (review-of choice) :review-id id)]
                      (swap! state #(if (= id (:review-id %))
                                      (assoc % :review review :review-status :ready :review-choice (dissoc choice :options)
                                             :review-options (:options choice))
                                      %)))
                    (catch InterruptedException _ nil)
                    (catch java.util.concurrent.CancellationException _ nil)
                    (catch Throwable error
                      (swap! state #(if (= id (:review-id %))
                                      (assoc % :review-status :failed :review-error (or (ex-message error) (str error)))
                                      %))))))))))

(defn- stop-transport! [runtime]
  (deref (show/submit! runtime :command {:op :transport/set :source {:adapter :repl :session-id "rehearsal"}
                                         :transport {:playing? false}})
         5000 nil))

(defn- authored
  "A fingerprint of what an operator authors in a show (definitions, pages,
  rig, settings, compositions, knobs). Playing a rehearsal changes none of
  these, so a changed fingerprint means edits nobody has saved."
  [model]
  (hash (select-keys model [:registry :pages :rig :settings :compositions :knobs])))

(defn status
  "The session as portable data: :status (:idle, :arming, :armed, :playing,
  :stopping, :finished, :stopped, :failed), with :failure saying why (not
  :error, which Studio's HTTP client reads as a failed request); the choice,
  the moods the agent will set, the live or final run receipts, the current
  beat, the review with its :review-id; :loaded? when the open show is the
  rehearsal show this session loaded and :edited? when it has edits since;
  and :before-rehearsal, the checkpoint of the show arming replaced, which
  restore! loads."
  [runtime]
  (let [s @(:state (session runtime))
        model (:model @(:state runtime))
        loaded? (= (:armed-epoch s) (:epoch model))]
    (assoc (select-keys s [:status :choice :options :moods :run :progress :review :review-status :review-error
                           :review-choice :review-id :failure :armed-epoch :bars :bpm :before-rehearsal])
           :beat (get-in model [:transport :beat]) :playing? (boolean (get-in model [:transport :playing?]))
           :epoch (:epoch model) :runtime-id (:id runtime)
           :loaded? loaded?
           :edited? (boolean (and loaded? (:armed-authored s) (not= (:armed-authored s) (authored model)))))))

(defn- await-status
  "Wait up to ms for the session to leave the given statuses."
  [runtime statuses ms]
  (let [state (:state (session runtime))]
    (loop [n 0]
      (when (and (statuses (:status @state)) (< n ms))
        (Thread/sleep 1) (recur (inc n))))))

(def max-checkpoints
  "How many before-rehearsal checkpoints of one show are kept; older ones are
  pruned."
  5)

(defn- file-id
  "A show ID as a file name part: letters, digits, _ and single dashes."
  [show-id]
  (-> (if (keyword? show-id) (subs (str show-id) 1) (str show-id))
      (str/replace #"[^A-Za-z0-9_]+" "-")
      (str/replace #"^-+|-+$" "")))

(defn- checkpoint-directory
  "Where a show's before-rehearsal checkpoints go: that show's own recovery
  directory when the show has a recovery owner, else a folder in the system
  temporary directory."
  ^java.io.File [runtime show-id]
  (if-let [directory (some-> (:recovery runtime) deref :directory)]
    (recovery/show-directory directory show-id)
    (io/file (System/getProperty "java.io.tmpdir") "tolglow-before-rehearsal")))

(defn- prune-checkpoints!
  "Keep the newest max-checkpoints before-rehearsal files of show-id in directory."
  [^java.io.File directory show-id]
  (let [prefix (str "before-rehearsal-" (file-id show-id) "-")]
    (doseq [^java.io.File f (->> (.listFiles directory)
                                 (filter #(and (.isFile ^java.io.File %) (str/starts-with? (.getName ^java.io.File %) prefix)
                                               (str/ends-with? (.getName ^java.io.File %) ".transit")))
                                 (sort-by (fn [^java.io.File f] [(.lastModified f) (.getName f)]))
                                 reverse
                                 (drop max-checkpoints))]
      (.delete f))))

(defn- save-before!
  "Save the show arming is about to replace, so the operator can bring it
  back (restore!) whatever autosave does meanwhile. It is written beside the
  show's recovery files under a unique name, never over an existing file, and
  it does not touch the show's \"Latest named\" bookmark, which stays the
  operator's own save."
  [runtime path]
  (let [model (:model @(:state runtime))
        show-id (:show-id model)
        directory (checkpoint-directory runtime show-id)
        file (if path
               (io/file path)
               (io/file directory (str "before-rehearsal-" (file-id show-id) "-"
                                       (.format (java.text.SimpleDateFormat. "yyyyMMdd-HHmmss-SSS") (java.util.Date.)) "-"
                                       (subs (str (java.util.UUID/randomUUID)) 0 8) ".transit")))]
    (when (and (nil? path) (.exists file)) (refuse "A before-rehearsal checkpoint with that name already exists" {:path (str file)}))
    (let [saved (recovery/write-source! (str file) (show/export-show-source runtime))]
      (when-not path (prune-checkpoints! directory show-id))
      {:path saved :show-id show-id :epoch (:epoch model) :saved-at-ms (System/currentTimeMillis)})))

(defn- import-and-wait!
  "Restore source on the running show and wait, as long as the show runs,
  until it has committed. A restore that was accepted always commits, so
  waiting never leaves it to land later behind the session's back. Returns
  the new epoch."
  [runtime source]
  (let [reply (show/submit! runtime :import {:source source})
        ack (loop [] (let [ack (deref reply 100 ::waiting)]
                       (cond (not= ::waiting ack) ack
                             @(:running runtime) (recur)
                             :else {:status :rejected :message "The show stopped"})))]
    (when-not (= :accepted (:status ack))
      (refuse (str "The show did not load: " (:message ack)) {:ack ack}))
    (loop []
      (cond (= (:epoch ack) (current-epoch runtime)) (:epoch ack)
            @(:running runtime) (do (Thread/sleep 1) (recur))
            :else (refuse "The show stopped while loading")))))

(defn- check-edits!
  "Refuse to replace this session's rehearsal show when it has edits nobody
  saved, unless the request says :discard-edits? true."
  [runtime state request]
  (when (and (:edited? (status runtime)) (not (true? (:discard-edits? request))))
    (refuse "The rehearsal show has edits that are not saved; save them, or confirm with :discard-edits? true"
            {:code :discard-edits-required})))

(def ^:private request-envelope [:runtime-id :expected-epoch :replace-show? :discard-edits? :wait-ms :checkpoint-path])

(defn arm!
  "Load the rehearsal show for request and take the agent's first round, on a
  background thread so no lock or HTTP request waits on an agent. Returns the
  status at once (:arming), or after up to :wait-ms. request: :mode (:club or
  :concert), :take (:a or :b), :seed, :agent {:latency-beats :lead-beats
  :lag-beats :outage {:from-section i :mode :refused|:silent} :timeout-ms
  (1 to 120000)}. Replacing a show this session did not load needs
  :replace-show? true (it is saved first, :before-rehearsal; :checkpoint-path
  chooses the file); re-arming this session's show after edits needs
  :discard-edits? true. stop! cancels an arm."
  [runtime request]
  (let [{:keys [state cancels] :as s} (session runtime)
        c (locking state
            (check-identity! runtime request)
            (let [current @state
                  c (choice (apply dissoc request request-envelope))
                  own? (= (:armed-epoch current) (current-epoch runtime))]
              (passage-for c)
              (when (#{:arming :playing :stopping} (:status current))
                (refuse "Stop the rehearsal that is playing or arming before arming another"))
              (when-not (or own? (true? (:replace-show? request)))
                (refuse "Arming loads the rehearsal show and replaces the open show; confirm with :replace-show? true"
                        {:code :replace-show-required}))
              (when own? (check-edits! runtime state request))
              (swap! state assoc :status :arming :failure nil :choice (dissoc c :options) :options (:options c)
                     :stop? false :run nil :progress nil)
              (assoc c :own? own?)))]
    (future
      (try
        (let [p (passage-for c)
              before (when-not (:own? c) (save-before! runtime (:checkpoint-path request)))
              epoch (when-not (:stop? @state) (import-and-wait! runtime (persistence/write-string (rehearsal/room-model p))))
              armed (when-not (:stop? @state) (rehearsal/arm! runtime p {:cancels cancels}))]
          (cond
            (:closed? @state)
            (swap! state assoc :status :failed :failure "The show stopped while the rehearsal was arming")
            (:stop? @state)
            (swap! state assoc :status :idle :failure "Arming was stopped before it finished"
                   :armed-epoch (when epoch epoch) :armed-authored (when epoch (authored (:model @(:state runtime)))))
            (not= epoch (current-epoch runtime))
            (swap! state assoc :status :failed :failure "The show changed while the rehearsal was arming")
            :else
            (do (swap! state merge {:status :armed :passage p :armed armed :armed-epoch epoch :moods (moods p)
                                    :armed-authored (authored (:model @(:state runtime)))
                                    :bars (/ (passage/length-beats p) passage/beats-per-bar) :bpm (:bpm p)
                                    :progress {:agent (mapv agent-row (get-in armed [:progress :agent]))}})
                ;; Only an armed rehearsal is reviewed; a stopped or closed arm starts no replay.
                (compute-review! s (dissoc c :own?))))
          (when before (swap! state assoc :before-rehearsal before)))
        (catch Throwable error
          (swap! state assoc :status :failed :failure (or (ex-message error) (str error))))))
    (when-let [ms (:wait-ms request)] (await-status runtime #{:arming} ms))
    (status runtime)))

(defn start!
  "Play the armed passage in real time. Returns the status at once; the run
  continues on a background thread until the passage ends or stop! is
  called."
  ([runtime] (start! runtime {}))
  ([runtime request]
   (let [{:keys [state] :as s} (session runtime)]
     (locking state
       (check-identity! runtime request)
       (let [{:keys [status passage armed armed-epoch choice options]} @state]
         (when-not (= :armed status) (refuse "Arm a rehearsal before starting it" {:status status}))
         (when-not (= armed-epoch (current-epoch runtime))
           (refuse "The show changed since the rehearsal was armed; arm it again" {:code :stale-rehearsal}))
         (swap! state assoc :status :playing :stop? false :run nil)
         (future
           (try
             (let [result (rehearsal/play! runtime passage
                                           {:armed armed :stop? #(:stop? @state)
                                            :on-progress (fn [progress]
                                                           (swap! state assoc :progress
                                                                  {:agent (mapv agent-row (:agent progress))
                                                                   :gestures (count (:gestures progress))
                                                                   :next-section (:next-section progress)}))})]
               (stop-transport! runtime)
               (swap! state assoc :status (if (:stopped? result) :stopped :finished) :run (run-summary result))
               (when-not (= :ready (:review-status @state))
                 (compute-review! s (assoc choice :options options))))
             (catch Throwable error
               (swap! state assoc :status :failed :failure (or (ex-message error) (str error))))))))
     (status runtime))))

(defn stop!
  "Stop a playing rehearsal, or cancel one arming. A playing passage stops
  delivering, the transport stops and the run's receipts are kept. Only a
  :playing session moves to :stopping, in one step, so a passage ending at
  the same moment cannot leave it there; a run that already ended returns
  its status. Returns the status once stopped."
  ([runtime] (stop! runtime {}))
  ([runtime request]
   (let [{:keys [state cancels]} (session runtime)]
     (check-identity! runtime request)
     (let [[before _] (swap-vals! state #(case (:status %)
                                           :playing (assoc % :stop? true :status :stopping)
                                           :arming (assoc % :stop? true)
                                           %))]
       (case (:status before)
         :arming (do (rehearsal/cancel-all! cancels) (await-status runtime #{:arming} 15000))
         (:playing :stopping) (await-status runtime #{:playing :stopping} 10000)
         (:finished :stopped) nil
         (refuse "No rehearsal is playing" {:status (:status before)})))
     (status runtime))))

(defn review!
  "Compute the seeded replay's review for request (a choice as for arm!, or
  the armed choice when it names none) without playing or touching the
  show. Returns the status; the review arrives in it when ready."
  ([runtime] (review! runtime {}))
  ([runtime request]
   (let [{:keys [state] :as s} (session runtime)
         request (apply dissoc request request-envelope)
         c (if (seq (select-keys request [:mode :take :seed :agent]))
             (choice request)
             (if-let [armed (:choice @state)]
               (assoc armed :options (:options @state))
               (choice {})))]
     (passage-for c)
     (compute-review! s c)
     (status runtime))))

(defn restore!
  "Load the show arming replaced, from its checkpoint, when nothing is
  playing or arming and the open show is still the rehearsal show this
  session loaded. Edits made to the rehearsal show since it was armed need
  :discard-edits? true. A restore uses the checkpoint up: a second one is
  refused. Returns the status."
  ([runtime] (restore! runtime {}))
  ([runtime request]
   (let [{:keys [state]} (session runtime)]
     (locking state
       (check-identity! runtime request)
       (let [{:keys [status before-rehearsal armed-epoch]} @state]
         (when (#{:arming :playing :stopping} status) (refuse "Stop the rehearsal before restoring the show"))
         (when-not before-rehearsal (refuse "No show replaced by a rehearsal is waiting to be restored in this session"))
         (when-not (= armed-epoch (current-epoch runtime))
           (refuse "The open show is not the rehearsal show; restoring would replace another show" {:code :not-rehearsal-show}))
         (check-edits! runtime state request)
         (let [source (try (recovery/read-source (:path before-rehearsal))
                           (catch java.io.IOException error
                             (refuse (str "The checkpoint could not be read: " (ex-message error)) {:code :checkpoint-unreadable}))
                           (catch clojure.lang.ExceptionInfo error
                             (refuse (str "The checkpoint could not be read: " (ex-message error)) {:code :checkpoint-unreadable})))]
           (import-and-wait! runtime source)
           (swap! state assoc :status :idle :armed-epoch nil :armed-authored nil :armed nil :passage nil :moods nil
                  :progress nil :before-rehearsal nil :failure nil)))))
   (status runtime)))

(defn action!
  "Studio's entry point: op is :status, :arm, :start, :stop, :review or
  :restore. Refusals come back as {:status :rejected :message ..}."
  [runtime op payload]
  (try
    (case op
      :status (status runtime)
      :arm (arm! runtime payload)
      :start (start! runtime payload)
      :stop (stop! runtime payload)
      :review (review! runtime payload)
      :restore (restore! runtime payload))
    (catch clojure.lang.ExceptionInfo error
      (if (:rehearsal/error (ex-data error))
        (cond-> {:status :rejected :message (ex-message error)}
          (:code (ex-data error)) (assoc :code (:code (ex-data error))))
        (throw error)))))
