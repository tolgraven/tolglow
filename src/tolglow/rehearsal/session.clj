(ns tolglow.rehearsal.session
  "Arm, start and stop a Room rehearsal on a running show and read its review
  (#670). Studio's rehearsal panel and the REPL call the same functions:

    (require '[tolglow.rehearsal.session :as session])
    (session/arm! runtime {:mode :club :seed 670 :replace-show? true})
    (session/start! runtime)          ; plays in real time; watch the Room
    (session/stop! runtime)           ; or let the passage end
    (:review (session/status runtime))
    (session/review! runtime)         ; the seeded replay's review without playing

  Arming loads the rehearsal show (the passage's rig and cues, transport
  stopped at beat 0) through the ordinary :import restore, which replaces
  the open show, so it needs :replace-show? true unless this session loaded
  the show itself. Then it takes the agent's first round, as
  tolglow.rehearsal/arm! does. Starting plays the passage with
  tolglow.rehearsal/play! on a background thread; stopping, or the passage
  ending, stops the transport, which pauses every effect, and the review is
  computed. Nothing is armed for physical output.

  The review is the headless seeded replay of the same passage, seed and
  agent options (tolglow.rehearsal/club or concert): the same inputs replay
  the same accepted actions, so it measures what the Room showed. A concert
  review compares both takes. The real-time run's own agent, gesture and
  transport receipts are kept beside it under :run.

  The session lives with the runtime as an observer, so it closes when the
  show stops."
  (:require [tolglow.persistence :as persistence]
            [tolglow.rehearsal :as rehearsal]
            [tolglow.rehearsal.agent-script :as script]
            [tolglow.rehearsal.passage :as passage]
            [tolglow.show :as show]))

(def ^:private observer-id ::session)

(def modes #{:club :concert})

(defn- session
  "The runtime's rehearsal session, created on first use and closed with the
  show."
  [runtime]
  (:session
   (show/attach-observer! runtime observer-id
     (fn []
       (let [state (atom {:status :idle})]
         {:session state
          :frame! (fn [_]) :events! (fn [& _]) :tracing? (constantly false)
          :close! (fn [] (swap! state assoc :stop? true))})))))

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
  lighting deadline, for choice; a newer request supersedes an older one."
  [state choice]
  (let [token (Object.)]
    (swap! state assoc :review-status :reviewing :review-token token :review-error nil)
    (future
      (try
        (let [review (review-of choice)]
          (swap! state #(if (identical? token (:review-token %))
                          (assoc % :review review :review-status :ready :review-choice (dissoc choice :options)
                                 :review-options (:options choice))
                          %)))
        (catch Throwable error
          (swap! state #(if (identical? token (:review-token %))
                          (assoc % :review-status :failed :review-error (or (ex-message error) (str error)))
                          %)))))))

(defn- stop-transport! [runtime]
  (deref (show/submit! runtime :command {:op :transport/set :source {:adapter :repl :session-id "rehearsal"}
                                         :transport {:playing? false}})
         5000 nil))

(defn status
  "The session as portable data: :status (:idle, :armed, :playing, :stopping,
  :finished, :stopped, :failed), the choice, the moods the agent will set,
  the live or final run receipts, the current beat and the review."
  [runtime]
  (let [s @(session runtime)
        model (:model @(:state runtime))]
    (cond-> (select-keys s [:status :choice :options :moods :run :progress :review :review-status :review-error
                            :review-choice :error :armed-epoch :bars :bpm])
      true (assoc :beat (get-in model [:transport :beat]) :playing? (boolean (get-in model [:transport :playing?]))
                  :epoch (:epoch model) :runtime-id (:id runtime)
                  :loaded? (= (:armed-epoch s) (:epoch model))))))

(defn arm!
  "Load the rehearsal show for request and take the agent's first round.
  request: :mode (:club or :concert), :take (:a or :b), :seed, :agent
  {:latency-beats :lead-beats :lag-beats :outage {:from-section i :mode
  :refused|:silent} :timeout-ms}, and :replace-show? true to replace a show
  this session did not load. Returns the status."
  [runtime request]
  (let [state (session runtime)]
    (locking state
      (check-identity! runtime request)
      (let [s @state
            c (choice (dissoc request :runtime-id :expected-epoch :replace-show?))
            p (passage-for c)
            own? (= (:armed-epoch s) (current-epoch runtime))]
        (when (#{:playing :stopping} (:status s)) (refuse "Stop the rehearsal that is playing before arming another"))
        (when-not (or own? (true? (:replace-show? request)))
          (refuse "Arming loads the rehearsal show and replaces the open show; confirm with :replace-show? true"
                  {:code :replace-show-required}))
        (let [ack (deref (show/submit! runtime :import {:source (persistence/write-string (rehearsal/room-model p))}) 10000
                         {:status :pending :message "The rehearsal show is still loading"})]
          (when-not (= :accepted (:status ack))
            (refuse (str "The rehearsal show did not load: " (:message ack)) {:ack ack}))
          ;; The restore commits on the owner's next cycle.
          (loop [n 0]
            (when (and (not= (:epoch ack) (current-epoch runtime)) (< n 2000))
              (Thread/sleep 1) (recur (inc n))))
          (let [armed (rehearsal/arm! runtime p)]
            (reset! state {:status :armed :choice (dissoc c :options) :options (:options c) :passage p :armed armed
                           :armed-epoch (current-epoch runtime) :moods (moods p)
                           :bars (/ (passage/length-beats p) passage/beats-per-bar) :bpm (:bpm p)
                           :progress {:agent (mapv agent-row (get-in armed [:progress :agent]))}})
            (compute-review! state (assoc c :options (:options c)))))))
    (status runtime)))

(defn start!
  "Play the armed passage in real time. Returns the status at once; the run
  continues on a background thread until the passage ends or stop! is
  called."
  ([runtime] (start! runtime {}))
  ([runtime request]
   (let [state (session runtime)]
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
                 (compute-review! state (assoc choice :options options))))
             (catch Throwable error
               (swap! state assoc :status :failed :error (or (ex-message error) (str error))))))))
     (status runtime))))

(defn stop!
  "Stop a playing rehearsal: the passage stops delivering, the transport
  stops and the run's receipts are kept. Returns the status once stopped."
  ([runtime] (stop! runtime {}))
  ([runtime request]
   (let [state (session runtime)]
     (check-identity! runtime request)
     (when-not (#{:playing :stopping} (:status @state))
       (refuse "No rehearsal is playing" {:status (:status @state)}))
     (swap! state assoc :stop? true :status :stopping)
     (loop [n 0]
       (when (and (= :stopping (:status @state)) (< n 5000))
         (Thread/sleep 1) (recur (inc n))))
     (status runtime))))

(defn review!
  "Compute the seeded replay's review for request (a choice as for arm!, or
  the armed choice when it names none) without playing or touching the
  show. Returns the status; the review arrives in it when ready."
  ([runtime] (review! runtime {}))
  ([runtime request]
   (let [state (session runtime)
         request (dissoc request :runtime-id :expected-epoch :replace-show?)
         c (if (seq (select-keys request [:mode :take :seed :agent]))
             (choice request)
             (if-let [armed (:choice @state)]
               (assoc armed :options (:options @state))
               (choice {})))]
     (passage-for c)
     (compute-review! state c)
     (status runtime))))

(defn action!
  "Studio's entry point: op is :status, :arm, :start, :stop or :review.
  Refusals come back as {:status :rejected :message ..}."
  [runtime op payload]
  (try
    (case op
      :status (status runtime)
      :arm (arm! runtime payload)
      :start (start! runtime payload)
      :stop (stop! runtime payload)
      :review (review! runtime payload))
    (catch clojure.lang.ExceptionInfo error
      (if (:rehearsal/error (ex-data error))
        (cond-> {:status :rejected :message (ex-message error)}
          (:code (ex-data error)) (assoc :code (:code (ex-data error))))
        (throw error)))))
