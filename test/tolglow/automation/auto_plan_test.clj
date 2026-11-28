(ns tolglow.automation.auto-plan-test
  "Authoring the auto plan (#217): section plans, palettes, category filters
  and the lit limit through one validated :auto-plan edit, the request
  Studio's plan editor sends to /api/auto/plan and the REPL submits. Real show
  owner on an injected clock, captured output only."
  (:require [clojure.test :refer [deftest is testing]]
            [tolglow.automation.policy :as policy]
            [tolglow.automation.red-show :as red-show]
            [tolglow.show :as show]))

(def frame-ms 125.0)

(defn- runtime []
  (doto (show/create-runtime {:model (red-show/model) :sink {:send! (fn [_ _])} :workspace-options nil})
    (show/tick! 0.0)))

(defn- submit! [rt t type payload]
  (let [reply (show/submit! rt type payload)]
    (show/tick! rt t)
    (deref reply 2000 ::missing)))

(defn- play! [rt t end]
  (loop [t t] (if (> t end) t (do (show/tick! rt t) (recur (+ t frame-ms))))))

(defn- auto [rt] (:auto @(:state rt)))

(defn- config [rt] (get-in @(:state rt) [:model :auto :config]))

(def base
  {:enabled? true :seed 11 :interval {:beats 1} :count 1 :scope {:cue-ids [:wash-color :accent :sweep]}
   :launch {:mode :one-shot :duration {:beats 1}}})

;; As Studio sends it over Transit: cue IDs stay keywords, choices from
;; selects arrive as text.
(def studio-plan
  {:sections [{:id "lift" :weight 3 :length {:bars 2} :transition "release"
               :config {:scope {:cue-ids [:wash-color :accent]} :randomize {:palette red-show/purple-palette}}}
              {:id "hush" :weight 1 :length {:bars 1} :max-run 1 :transition "cut"
               :config {:scope {:cue-ids [:spot :sweep]} :intent :darkness :max-lit 1}}]
   :categories {:exclude ["strobe"]}
   :max-lit 6
   :palette red-show/red-palette})

(deftest plan-edits-replace-or-clear-only-the-authored-parts
  (let [config (policy/with-plan (assoc base :randomize {:params :marked :ranges {:level [0 1]}})
                                 (policy/normalize-plan studio-plan))]
    (is (= {:beats 1} (:interval config)) "interval, count, scope and launch stay")
    (is (= [:lift :hush] (map (comp keyword :id) (:sections config))))
    (is (= [:release :cut] (map :transition (:sections config))))
    (is (= {:exclude [:strobe]} (:categories config)))
    (is (= {:params :marked :ranges {:level [0 1]} :palette red-show/red-palette} (:randomize config)))
    (is (empty? (policy/validate-config (dissoc config :enabled? :seed))))
    (is (= {:sections (:sections config) :categories {:exclude [:strobe]} :max-lit 6 :palette red-show/red-palette}
           (policy/plan config)))
    (testing "nil clears a part; a palette alone varies nothing until asked"
      (let [cleared (policy/with-plan config {:max-lit nil :palette nil :sections []})]
        (is (not-any? #(contains? cleared %) [:max-lit :sections]))
        (is (= {:params :marked :ranges {:level [0 1]}} (:randomize cleared))))
      (is (= {:params :none :palette red-show/red-palette} (:randomize (policy/with-plan base {:palette red-show/red-palette}))))
      (is (not (contains? (policy/with-plan (policy/with-plan base {:palette red-show/red-palette}) {:palette nil}) :randomize))))
    (is (seq (policy/validate-plan {:interval {:beats 2}})) "only the authored parts")
    (is (seq (policy/validate-plan {})))))

(deftest a-plan-saved-while-auto-is-off-is-checked-stored-and-played-by-start
  (let [rt (runtime)]
    (try
      (is (= :accepted (:status (submit! rt 0.0 :auto (assoc base :enabled? false)))))
      (let [ack (submit! rt 0.0 :auto-plan {:plan studio-plan})]
        (is (= {:status :accepted :enabled? false} (select-keys ack [:status :enabled?])))
        (is (= [:lift :hush] (mapv (comp keyword :id) (get-in ack [:plan :sections])))))
      (is (false? (get-in (auto rt) [:enabled?] false)) "saving a plan does not start auto")
      (is (= 6 (:max-lit (config rt))))
      (is (= [:lift :hush] (mapv (comp keyword :id) (get-in (show/snapshot rt) [:auto :config :sections])))
          "Studio's projection reads the saved plan")
      (testing "refusals leave the saved plan as it was"
        (let [before (config rt)]
          (is (re-find #"Invalid auto plan: Section length needs positive beats, bars or phrases at \[:sections 0 :length\]"
                       (str (:message (submit! rt 0.0 :auto-plan {:plan {:sections [{:id :a :length {:ms 500}}]}})))))
          (is (re-find #"No saved cue :ghost"
                       (str (:message (submit! rt 0.0 :auto-plan {:plan {:sections [{:id :a :length {:bars 1}
                                                                                         :config {:scope {:cue-ids [:ghost]}}}]}})))))
          (is (re-find #"Palette needs"
                       (str (:message (submit! rt 0.0 :auto-plan {:plan {:palette [{:color [2 0 0]}]}})))))
          (is (re-find #"Unknown auto plan setting" (str (:message (submit! rt 0.0 :auto-plan {:plan {:count 3}})))))
          (is (= before (config rt)))))
      (testing "Start with keep-plan, as Studio's Start auto sends, plays the authored sections"
        (is (= :accepted (:status (submit! rt 0.0 :auto (assoc base :keep-plan? true))))))
      (play! rt frame-ms 4000.0)
      (is (#{:lift :hush} (keyword (get-in @(:state rt) [:model :auto :state :section :id]))))
      (finally (show/stop! rt)))))

(def three-sections
  [{:id :a :weight 1 :length {:bars 1} :transition :release :config {:scope {:cue-ids [:wash-color]}}}
   {:id :b :weight 2 :length {:bars 1} :transition :release :config {:scope {:cue-ids [:accent]}}}
   {:id :c :weight 1 :length {:bars 1} :transition :release :config {:scope {:cue-ids [:sweep]}}}])

(defn- sections-by-bar
  "The auto section playing at each bar line from bar 0 to bar end."
  [rt t end-beat edit!]
  (loop [t t seen {} edited? false]
    (let [beat (get-in @(:state rt) [:model :transport :beat])
          bar (long (Math/floor (/ beat 4)))]
      (if (> beat end-beat) seen
          (let [edited? (or edited? (when (and edit! (>= beat 16.0)) (edit! t) true))]
            (show/tick! rt t)
            (recur (+ t frame-ms)
                   (update seen bar #(or % (some-> (get-in @(:state rt) [:model :auto :state :section :id]) keyword)))
                   edited?))))))

(deftest a-plan-edited-while-auto-runs-goes-on-with-the-running-session
  ;; Review of #823: the edit restarted the session, so changing the lit limit
  ;; mid-set replayed the plan's opening look for look.
  (let [start (assoc base :seed 99 :sections three-sections)
        baseline (runtime)
        edited (runtime)
        before (atom nil)]
    (try
      (submit! baseline 0.0 :auto start)
      (submit! edited 0.0 :auto start)
      (let [expected (sections-by-bar baseline frame-ms 40.0 nil)
            seen (sections-by-bar edited frame-ms 40.0
                                  (fn [t]
                                    (reset! before (get-in @(:state edited) [:model :auto :state]))
                                    (let [ack (submit! edited t :auto-plan {:plan {:max-lit 5}})]
                                      (is (= {:status :accepted :enabled? true} (select-keys ack [:status :enabled?])))
                                      (is (= {:auto-session (str (:owner-id @before) "+1")} (:basis ack))
                                          "the session's basis names the plan edit"))))
            after (get-in @(:state edited) [:model :auto :state])]
        (is (= 5 (:max-lit (config edited))))
        (is (= expected seen) "the section sequence goes on exactly as without the edit")
        (is (= #{:a :b :c} (set (vals seen))) "the plan moved through its sections")
        (is (= (:owner-id @before) (:owner-id after)) "the same session")
        (is (> (:tick after) (:tick @before)) "the decision count went on, not from zero"))
      (finally (show/stop! baseline) (show/stop! edited)))))

(deftest a-plan-edit-keeps-the-looks-it-owns-and-can-wait-for-a-line
  (let [rt (runtime)]
    (try
      (submit! rt 0.0 :auto (assoc base :seed 5 :sections three-sections :launch {:mode :timed :duration {:bars 2}}))
      (let [t (play! rt frame-ms 1500.0)
            owned (get-in @(:state rt) [:model :auto :state :owned-instance-ids])
            _ (is (seq owned))
            ack (submit! rt t :auto-plan {:plan {:categories {:exclude [:strobe]}}})]
        (is (= :accepted (:status ack)))
        (is (every? #(= :running (get-in @(:state rt) [:model :instances % :phase])) owned) "no look was ended")
        (testing "on a line"
          (let [ack (submit! rt t :auto-plan {:plan {:max-lit 3} :at "next-bar" :source {:adapter :web :session-id "studio"}})]
            (is (= {:status :accepted :scheduled? true} (select-keys ack [:status :scheduled?])))
            (is (nil? (:max-lit (config rt))) "nothing changes before the line")
            (play! rt (+ t frame-ms) 4000.0)
            (is (= 3 (:max-lit (config rt))))
            (is (re-matches #"auto/0/\d+\+2" (get-in (show/snapshot rt) [:auto :session])) "the same session, two edits on"))))
      (testing "a section the edit removes plays out its line, then the plan goes on"
        (let [section (keyword (get-in @(:state rt) [:model :auto :state :section :id]))
              others (vec (remove #(= section (:id %)) three-sections))
              _ (submit! rt 4000.0 :auto-plan {:plan {:sections others}})
              timer (get-in @(:state rt) [:model :auto :state :section :timer])
              line (+ (:started-at timer) (get-in timer [:duration :value]))]
          (is (= section (keyword (get-in @(:state rt) [:model :auto :state :section :id]))) "it keeps its line")
          (play! rt 4125.0 (+ 4125.0 (* 500.0 (+ 1 (- line (get-in @(:state rt) [:model :transport :beat]))))))
          (is (some #{(keyword (get-in @(:state rt) [:model :auto :state :section :id]))} (map :id others)))))
      (finally (show/stop! rt)))))

(deftest a-line-is-refused-for-a-plan-edit-while-auto-is-off
  (let [rt (runtime)]
    (try
      (let [ack (submit! rt 0.0 :auto-plan {:plan {:max-lit 2} :at "next-bar"})]
        (is (= :rejected (:status ack)))
        (is (re-find #"Auto is off" (:message ack)))
        (is (nil? (:max-lit (config rt)))))
      (finally (show/stop! rt)))))
