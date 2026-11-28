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
            cancels (atom #{})
            review-job (atom nil)]
        {:state state :cancels cancels :review-job review-job
         :frame! (fn [_]) :events! (fn [& _]) :tracing? (constantly false)
         :close! (fn []
                   (swap! state assoc :stop? true)
                   (rehearsal/cancel-all! cancels)
                   (some-> @review-job future-cancel))}))))

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
                  (some? (:timeout-ms agent)) (assoc :agent-timeout-ms (double (:timeout-ms agent))))]
    (when-not (map? request) (refuse "A rehearsal choice is a map"))
    (when-not (modes mode) (refuse "Rehearse :club or :concert" {:mode mode}))
    (when-not (or (nil? take) (#{:a :b} take)) (refuse "A concert take is :a or :b" {:take take}))
    (when-not (and (integer? seed) (<= 0 seed Integer/MAX_VALUE)) (refuse "The seed is a nonnegative integer" {:seed seed}))
    (doseq [[k v] (select-keys options [:agent-latency-beats :agent-lead-beats :agent-lag-beats])]
      (when-not (and (number? v) (Double/isFinite (double v)) (<= -64 v 64))
        (refuse (str "Agent timing " (name k) " is a number of beats from -64 to 64") {k v})))
    {:mode mode :take take :seed seed :options options}))

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
  lighting deadline, for choice. One replay runs at a time: a new request
  cancels the one still running, and each review carries its own :review-id,
  so a superseded result is never shown."
  [{:keys [state review-job]} choice]
  (let [id (:review-id (swap! state update :review-id (fnil inc 0)))]
    (swap! state assoc :review-status :reviewing :review-error nil)
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
                                  %))))))))

(defn- stop-transport! [runtime]
  (deref (show/submit! runtime :command {:op :transport/set :source {:adapter :repl :session-id "rehearsal"}
                                         :transport {:playing? false}})
         5000 nil))

(defn status
  "The session as portable data: :status (:idle, :arming, :armed, :playing,
  :stopping, :finished, :stopped, :failed), the choice, the moods the agent
  will set, the live or final run receipts, the current beat, the review
  with its :review-id, and :before-rehearsal (the checkpoint of the show
  arming replaced, which restore! loads)."
  [runtime]
  (let [s @(:state (session runtime))
        model (:model @(:state runtime))]
    (assoc (select-keys s [:status :choice :options :moods :run :progress :review :review-status :review-error
                           :review-choice :review-id :error :armed-epoch :bars :bpm :before-rehearsal])
           :beat (get-in model [:transport :beat]) :playing? (boolean (get-in model [:transport :playing?]))
           :epoch (:epoch model) :runtime-id (:id runtime)
           :loaded? (= (:armed-epoch s) (:epoch model)))))

(defn- await-status
  "Wait up to ms for the session to leave the given statuses."
  [runtime statuses ms]
  (let [state (:state (session runtime))]
    (loop [n 0]
      (when (and (statuses (:status @state)) (< n ms))
        (Thread/sleep 1) (recur (inc n))))))

(defn- checkpoint-path
  "Where the show arming replaces is saved: the show folder's recovery
  directory when the show has one, else the system temporary directory."
  [runtime model]
  (let [directory (or (some-> (:recovery runtime) deref :directory) (io/file (System/getProperty "java.io.tmpdir")))
        id (str/replace (str (:show-id model)) #"[^A-Za-z0-9_-]+" "-")]
    (str (io/file directory (str "before-rehearsal-" id "-" (:epoch model) ".transit")))))

(defn- save-before!
  "Save the show arming is about to replace as a named checkpoint, so the
  operator can bring it back (restore!) whatever autosave does meanwhile."
  [runtime path]
  (let [model (:model @(:state runtime))]
    {:path (show/save-show! runtime path) :show-id (:show-id model) :epoch (:epoch model)
     :saved-at-ms (System/currentTimeMillis)}))

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
      (refuse (str "The rehearsal show did not load: " (:message ack)) {:ack ack}))
    (loop []
      (cond (= (:epoch ack) (current-epoch runtime)) (:epoch ack)
            @(:running runtime) (do (Thread/sleep 1) (recur))
            :else (refuse "The show stopped while the rehearsal show was loading")))))

(defn arm!
  "Load the rehearsal show for request and take the agent's first round, on a
  background thread so no lock or HTTP request waits on an agent. Returns the
  status at once (:arming), or after up to :wait-ms. request: :mode (:club or
  :concert), :take (:a or :b), :seed, :agent {:latency-beats :lead-beats
  :lag-beats :outage {:from-section i :mode :refused|:silent} :timeout-ms},
  :replace-show? true to replace a show this session did not load (it is
  saved first as a named checkpoint, :before-rehearsal), and
  :checkpoint-path to choose where."
  [runtime request]
  (let [{:keys [state cancels] :as s} (session runtime)
        c (locking state
            (check-identity! runtime request)
            (let [current @state
                  c (choice (dissoc request :runtime-id :expected-epoch :replace-show? :wait-ms :checkpoint-path))
                  own? (= (:armed-epoch current) (current-epoch runtime))]
              (passage-for c)
              (when (#{:arming :playing :stopping} (:status current))
                (refuse "Stop the rehearsal that is playing or arming before arming another"))
              (when-not (or own? (true? (:replace-show? request)))
                (refuse "Arming loads the rehearsal show and replaces the open show; confirm with :replace-show? true"
                        {:code :replace-show-required}))
              (swap! state assoc :status :arming :error nil :choice (dissoc c :options) :options (:options c)
                     :stop? false :run nil :progress nil)
              (assoc c :own? own?)))]
    (future
      (try
        (let [p (passage-for c)
              before (when-not (:own? c)
                       (save-before! runtime (or (:checkpoint-path request) (checkpoint-path runtime (:model @(:state runtime))))))
              epoch (import-and-wait! runtime (persistence/write-string (rehearsal/room-model p)))
              armed (rehearsal/arm! runtime p {:cancels cancels})]
          (if (and (= epoch (current-epoch runtime)) (not (:stop? @state)))
            (swap! state merge {:status :armed :passage p :armed armed :armed-epoch epoch :moods (moods p)
                                :bars (/ (passage/length-beats p) passage/beats-per-bar) :bpm (:bpm p)
                                :progress {:agent (mapv agent-row (get-in armed [:progress :agent]))}}
                   (when before {:before-rehearsal before}))
            (swap! state assoc :status :failed :error "The show changed or stopped while the rehearsal was arming"))
          (compute-review! s (dissoc c :own?)))
        (catch Throwable error
          (swap! state assoc :status :failed :error (or (ex-message error) (str error))))))
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
               (swap! state assoc :status :failed :error (or (ex-message error) (str error))))))))
     (status runtime))))

(defn stop!
  "Stop a playing rehearsal: the passage stops delivering, the transport
  stops and the run's receipts are kept. Only a :playing session moves to
  :stopping, in one step, so a passage ending at the same moment cannot
  leave it there; a run that already ended returns its status. Returns the
  status once stopped."
  ([runtime] (stop! runtime {}))
  ([runtime request]
   (let [{:keys [state]} (session runtime)]
     (check-identity! runtime request)
     (let [[before _] (swap-vals! state #(if (= :playing (:status %)) (assoc % :stop? true :status :stopping) %))]
       (case (:status before)
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
         request (dissoc request :runtime-id :expected-epoch :replace-show? :wait-ms :checkpoint-path)
         c (if (seq (select-keys request [:mode :take :seed :agent]))
             (choice request)
             (if-let [armed (:choice @state)]
               (assoc armed :options (:options @state))
               (choice {})))]
     (passage-for c)
     (compute-review! s c)
     (status runtime))))

(defn restore!
  "Load the show arming replaced, from its named checkpoint, when nothing is
  playing or arming. Returns the status."
  ([runtime] (restore! runtime {}))
  ([runtime request]
   (let [{:keys [state]} (session runtime)]
     (locking state
       (check-identity! runtime request)
       (let [{:keys [status before-rehearsal]} @state]
         (when (#{:arming :playing :stopping} status) (refuse "Stop the rehearsal before restoring the show"))
         (when-not before-rehearsal (refuse "No show was replaced by a rehearsal in this session"))
         (import-and-wait! runtime (slurp (:path before-rehearsal)))
         (swap! state assoc :status :idle :armed-epoch nil :armed nil :passage nil :moods nil :progress nil))))
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
