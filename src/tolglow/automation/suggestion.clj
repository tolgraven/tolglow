(ns tolglow.automation.suggestion
  "Refuse stale or late agent suggestions (#670).

  An agent decides from what it read: the auto session that was playing and
  where the music was. Its reply can arrive after either has moved on. A
  suggestion names what it was based on so the show can refuse it instead of
  applying it in the wrong place:

    {:basis {:auto-session \"auto/3/17\"}   ; the session it read, or \"off\"
     :from {:bar 15 :beat 1}                ; zero-based start of its beat window
     :until {:bar 24}}                      ; zero-based end of its beat window

  It is refused with code :stale-suggestion when the auto session has changed
  (an operator's start, stop or plan edit, another conversation's change, or a
  change that already applied), when the transport is past the end of the
  window, or before its start (a seek or loop back to an earlier part of the
  song). A change it schedules must fall inside its window. When that change
  falls due it is checked again, against the session and the window.
  Positions are transport beats, so a stopped transport holds the window.

  The auto session is the planner's owner ID plus a count of plan edits made
  on it, so it ignores the planner's own decisions and the agent's own
  scheduled posts. Every receipt carries the basis after the change
  (`basis`), so a multi-call round can chain.

  Pure: the show owner and the scheduler call refusal with their committed
  model."
  (:require [afterglow.time :as time]
            [tolglow.registry :as registry]))

(def code :stale-suggestion)

(def ^:private position-keys #{:beat :bar :phrase})

(defn- position? [at]
  (and (map? at) (seq at) (every? position-keys (keys at))
       (every? #(and (registry/finite-number? %) (not (neg? %))) (vals at))))

(defn- position-beat
  "Beat of a zero-based {:beat :bar :phrase} position in meter."
  [at meter]
  (let [{:keys [beats-per-bar bars-per-phrase]} (time/normalize-meter meter)]
    (double (+ (:beat at 0) (* (:bar at 0) beats-per-bar) (* (:phrase at 0) beats-per-bar bars-per-phrase)))))

(defn validate
  "Path/message errors for a suggestion, [] when it is well formed."
  [suggestion]
  (let [{:keys [basis]} suggestion]
    (cond
      (not (map? suggestion))
      [{:path [] :message "A suggestion is a map with :basis, :from and/or :until"}]
      (not (every? #{:basis :from :until} (keys suggestion)))
      [{:path [] :message "A suggestion takes only :basis, :from and :until"}]
      (not (some #(contains? suggestion %) [:basis :from :until]))
      [{:path [] :message "A suggestion needs a :basis, a window (:from, :until), or both"}]
      :else
      (vec
       (concat
        (when (contains? suggestion :basis)
          (cond
            (contains? basis :revision)
            [{:path [:basis :revision] :message "The basis is the auto session; a show revision moves with every decision and is not accepted"}]
            (not (and (map? basis) (= [:auto-session] (keys basis))))
            [{:path [:basis] :message "A basis names the :auto-session read from the show"}]
            (not (and (string? (:auto-session basis)) (seq (:auto-session basis))))
            [{:path [:basis :auto-session] :message "The auto session is the session ID read from the show, or \"off\""}]))
        (for [k [:from :until] :when (and (contains? suggestion k) (not (position? (get suggestion k))))]
          {:path [k] :message (str (if (= :from k) "From" "Until") " is a zero-based position {:bar n :beat n :phrase n}")})
        (when (and (position? (:from suggestion)) (position? (:until suggestion))
                   (>= (position-beat (:from suggestion) nil) (position-beat (:until suggestion) nil)))
          [{:path [:until] :message "The window ends after it starts"}]))))))

(defn auto-session
  "The auto session a suggestion's basis compares with: the playing session's
  ID, with the number of plan edits made on it, or \"off\" when auto mode is
  off."
  [model]
  (if-let [owner (when (get-in model [:auto :enabled?]) (get-in model [:auto :state :owner-id]))]
    (let [edits (get-in model [:auto :state :plan-edits] 0)]
      (if (pos? edits) (str owner "+" edits) owner))
    "off"))

(defn basis
  "The basis an agent quotes for its next suggestion on model."
  [model]
  {:auto-session (auto-session model)})

(defn window
  "[from-beat until-beat] of a suggestion's window; either may be nil."
  [model suggestion]
  (let [meter (get-in model [:transport :meter])]
    [(some-> (:from suggestion) (position-beat meter)) (some-> (:until suggestion) (position-beat meter))]))

(defn refusal
  "Why the show refuses suggestion now, or nil. phase is :arrival (the agent's
  call reaches the owner) or :due (a change it scheduled falls due). due-beat
  is the beat an arriving change is scheduled for, if any."
  ([model suggestion] (refusal model suggestion {}))
  ([model suggestion {:keys [due-beat]}]
   (when (some? suggestion)
     (when-let [{:keys [path message]} (first (validate suggestion))]
       (throw (ex-info (str "Invalid suggestion: " message (when (seq path) (str " at " (pr-str path))))
                       {:control/error :invalid-suggestion})))
     (let [{:keys [basis]} suggestion
           beat (double (get-in model [:transport :beat] 0))
           [from until] (window model suggestion)
           session (auto-session model)
           refuse (fn [reason message details] (merge {:code code :reason reason :message message} details))]
       (cond
         (and (contains? basis :auto-session) (not= (:auto-session basis) session))
         (refuse :basis-changed
                 (str "Stale suggestion: it was based on auto session " (:auto-session basis)
                      " and the show now plays " session)
                 {:basis-session (:auto-session basis) :auto-session session})

         (and until (>= beat until))
         (refuse :window-passed
                 (str "Stale suggestion: its window ended at beat " until " (" (pr-str (:until suggestion))
                      ") and the transport is at beat " beat)
                 {:until-beat until :beat beat})

         (and from (< beat from) (nil? due-beat))
         (refuse :before-window
                 (str "Stale suggestion: its window starts at beat " from " (" (pr-str (:from suggestion))
                      ") and the transport is at beat " beat "; the song moved back")
                 {:from-beat from :beat beat})

         (and due-beat (or (and until (>= due-beat until)) (and from (< due-beat from))))
         (refuse :outside-window
                 (str "Stale suggestion: its change is due at beat " (double due-beat)
                      ", outside its window" (when from (str " from beat " from)) (when until (str " to beat " until)))
                 {:from-beat from :until-beat until :due-beat (double due-beat)}))))))

(defn refusal-ack
  "A rejected receipt for refusal r, keeping the command ID and naming the
  current basis."
  [r command-id model]
  (cond-> (assoc (select-keys r [:code :reason :message :from-beat :until-beat :beat :due-beat
                                 :basis-session :auto-session])
                 :status :rejected :basis (basis model))
    command-id (assoc :command-id command-id)))
