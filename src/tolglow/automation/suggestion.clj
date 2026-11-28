(ns tolglow.automation.suggestion
  "Refuse stale or late agent suggestions (#670).

  An agent decides from what it read: the auto session that was playing, the
  show revision and where the music was. Its reply can arrive after any of
  that has moved on. A suggestion names what it was based on so the show can
  refuse it instead of applying it in the wrong place:

    {:basis {:auto-session \"auto/3/17\"   ; the session it saw, or \"off\"
             :revision 42}                ; the show revision it read (strict)
     :until {:bar 16}}                    ; zero-based end of its beat window

  It is refused with code :stale-suggestion when the auto session has changed
  (an operator's start or stop, another conversation's change, or a change
  that already applied), when the show revision has moved, or when the
  transport has reached the end of the window. A change it schedules with :at
  must fall inside its window. When that change falls due it is checked again
  against the session and the window, never against the revision: posting the
  change is itself a revision. Positions are transport beats, so a stopped
  transport holds the window and a seek moves through it.

  Pure: the show owner calls refusal with its committed model."
  (:require [tolglow.event-subscribers :as subscribers]
            [tolglow.registry :as registry]))

(def code :stale-suggestion)

(def ^:private position-keys #{:beat :bar :phrase})

(defn- position? [at]
  (and (map? at) (seq at) (every? position-keys (keys at))
       (every? #(and (registry/finite-number? %) (not (neg? %))) (vals at))))

(defn validate
  "Path/message errors for a suggestion, [] when it is well formed."
  [suggestion]
  (let [{:keys [basis until]} suggestion]
    (cond
      (not (map? suggestion))
      [{:path [] :message "A suggestion is a map with :basis and/or :until"}]
      (not (every? #{:basis :until} (keys suggestion)))
      [{:path [] :message "A suggestion takes only :basis and :until"}]
      (not (or (contains? suggestion :basis) (contains? suggestion :until)))
      [{:path [] :message "A suggestion needs a :basis, an :until position, or both"}]
      :else
      (vec
       (concat
        (when (contains? suggestion :basis)
          (cond
            (not (and (map? basis) (seq basis) (every? #{:auto-session :revision} (keys basis))))
            [{:path [:basis] :message "A basis names :auto-session and/or :revision"}]
            :else
            (concat
             (when (and (contains? basis :auto-session)
                        (not (and (string? (:auto-session basis)) (seq (:auto-session basis)))))
               [{:path [:basis :auto-session] :message "The auto session is the session ID read from the show, or \"off\""}])
             (when (and (contains? basis :revision) (not (nat-int? (:revision basis))))
               [{:path [:basis :revision] :message "The revision is the show revision read, a nonnegative integer"}]))))
        (when (and (contains? suggestion :until) (not (position? until)))
          [{:path [:until] :message "Until is a zero-based position {:bar n :beat n :phrase n}"}]))))))

(defn auto-session
  "The auto session a suggestion's basis compares with: the playing session's
  ID, or \"off\" when auto mode is off."
  [model]
  (or (when (get-in model [:auto :enabled?]) (get-in model [:auto :state :owner-id])) "off"))

(defn until-beat
  "The transport beat a suggestion's window ends at, or nil."
  [model suggestion]
  (when-let [until (:until suggestion)]
    (double (subscribers/position-beat until (get-in model [:transport :meter])))))

(defn refusal
  "Why the show refuses suggestion now, or nil. phase is :arrival (the agent's
  call reaches the owner) or :due (a change it scheduled falls due). due-beat
  is the beat an arriving change is scheduled for, if any."
  ([model suggestion] (refusal model suggestion {}))
  ([model suggestion {:keys [phase due-beat] :or {phase :arrival}}]
   (when (some? suggestion)
     (when-let [{:keys [path message]} (first (validate suggestion))]
       (throw (ex-info (str "Invalid suggestion: " message (when (seq path) (str " at " (pr-str path))))
                       {:control/error :invalid-suggestion})))
     (let [{:keys [basis]} suggestion
           beat (double (get-in model [:transport :beat] 0))
           until (until-beat model suggestion)
           session (auto-session model)
           refuse (fn [reason message details] (merge {:code code :reason reason :message message} details))]
       (cond
         (and (contains? basis :auto-session) (not= (:auto-session basis) session))
         (refuse :basis-changed
                 (str "Stale suggestion: it was based on auto session " (:auto-session basis)
                      " and the show now plays " session)
                 {:basis-session (:auto-session basis) :auto-session session})

         (and (= :arrival phase) (contains? basis :revision) (not= (:revision basis) (:revision model)))
         (refuse :basis-changed
                 (str "Stale suggestion: it was based on show revision " (:revision basis)
                      " and the show is at revision " (:revision model))
                 {:basis-revision (:revision basis) :revision (:revision model)})

         (and until (>= beat until))
         (refuse :window-passed
                 (str "Stale suggestion: its window ended at beat " until " (" (pr-str (:until suggestion))
                      ") and the transport is at beat " beat)
                 {:until-beat until :beat beat})

         (and until due-beat (>= due-beat until))
         (refuse :outside-window
                 (str "Stale suggestion: its change is due at beat " (double due-beat)
                      ", at or after the end of its window at beat " until)
                 {:until-beat until :due-beat (double due-beat)}))))))

(defn refusal-ack
  "A rejected receipt for refusal r, keeping the command ID."
  [r command-id]
  (cond-> (assoc (select-keys r [:code :reason :message :until-beat :beat :due-beat
                                 :basis-session :auto-session :basis-revision :revision])
                 :status :rejected)
    command-id (assoc :command-id command-id)))
