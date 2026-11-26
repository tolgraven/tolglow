(ns tolglow.automation.sections
  "Musical section plans for the auto planner (#217).

  A plan is a vector of sections, each with a musical :length (beats, bars or
  phrases, never milliseconds), a positive :weight and a :config that overrides
  the planner's settings while it plays: palette, launch mode, interval, count,
  intent, category filters or a lit-fixture limit. At each boundary the next
  section is a weighted seeded choice; :max-run caps how many times in a row one
  section may repeat. Boundaries follow the transport beat, so a stopped
  transport holds the section in place and play resumes it without catching up.
  The choice at boundary n reads its own substream, so editing cues or
  parameters never changes the section sequence."
  (:require [afterglow.time :as time]
            [tolglow.automation.random :as random]
            [tolglow.registry :as registry]))

(def transitions #{:overlap :release :cut})

(def ^:private section-keys #{:id :weight :length :config :max-run :transition})

(defn- identifier? [x] (or (keyword? x) (and (string? x) (seq x))))

(defn validate
  "Path/message errors for a :sections vector. validate-config checks each
  section's :config with the planner's own validator."
  [sections validate-config]
  (if-not (and (vector? sections) (<= 1 (count sections) 32))
    [{:path [] :message "Sections must be a vector of 1-32 sections"}]
    (vec
     (concat
      (when-not (= (count sections) (count (distinct (map :id sections))))
        [{:path [] :message "Section IDs must be distinct"}])
      (mapcat
       (fn [i section]
         (let [path [i]]
           (if-not (map? section)
             [{:path path :message "Section must be a map"}]
             (concat
              (when-not (every? section-keys (keys section))
                [{:path path :message "Unknown section setting"}])
              (when-not (identifier? (:id section))
                [{:path (conj path :id) :message "Section needs a stable ID"}])
              (when-not (and (registry/finite-number? (:weight section 1)) (pos? (:weight section 1)))
                [{:path (conj path :weight) :message "Section weight must be positive and finite"}])
              (let [length (:length section)]
                (when-not (and (empty? (time/validate-duration length))
                               (not= :ms (ffirst length)) (pos? (val (first length))))
                  [{:path (conj path :length) :message "Section length needs positive beats, bars or phrases"}]))
              (when-not (or (nil? (:max-run section)) (pos-int? (:max-run section)))
                [{:path (conj path :max-run) :message "Maximum run must be a positive integer"}])
              (when-not (transitions (:transition section :overlap))
                [{:path (conj path :transition) :message "Transition is :overlap, :release or :cut"}])
              (let [config (:config section {})]
                (if (and (map? config) (not (contains? config :sections)))
                  (map #(update % :path (fn [p] (into (conj path :config) p))) (validate-config config))
                  [{:path (conj path :config) :message "Section config must be a map without nested sections"}]))))))
       (range) sections)))))

(defn- choose-section [sections seed index previous]
  (let [blocked (when-let [{:keys [id run]} previous]
                  (let [max-run (:max-run (some #(when (= id (:id %)) %) sections))]
                    (when (and max-run (>= run max-run)) id)))
        eligible (or (seq (remove #(= blocked (:id %)) sections)) sections)]
    (second (random/choose-weighted (random/derive-seed seed :section index) eligible #(:weight % 1)))))

(defn- start-timer [section clock boundary]
  (let [timer (time/start (:length section) clock)
        length (get-in timer [:duration :value])]
    ;; Stay on the bar grid unless a whole section was skipped (a long gap
    ;; between ticks); then start from now rather than catching up.
    (if (and boundary (< (- (:started-at timer) boundary) length))
      (assoc timer :started-at boundary)
      timer)))

(defn current
  "The section definition a section state refers to, or nil."
  [sections state]
  (some #(when (= (:id state) (:id %)) %) sections))

(defn step
  "Advance the section state at clock. Returns {:state .. :entered section-or-nil
  :changed? bool}. :entered is the section begun on this tick; :changed? is
  true when its ID differs from the previous section."
  [sections state seed clock]
  (let [section (current sections state)]
    (cond
      (nil? section)
      (let [chosen (choose-section sections seed 0 nil)]
        {:state {:id (:id chosen) :index 0 :run 1 :timer (start-timer chosen clock nil)}
         :entered chosen :changed? true})

      (not (:done? (time/sample (:timer state) clock)))
      {:state state :entered nil :changed? false}

      :else
      (let [index (inc (:index state))
            chosen (choose-section sections seed index state)
            boundary (+ (get-in state [:timer :started-at]) (get-in state [:timer :duration :value]))
            same? (= (:id chosen) (:id state))]
        {:state {:id (:id chosen) :index index :run (if same? (inc (:run state)) 1)
                 :timer (start-timer chosen clock boundary)}
         :entered chosen :changed? (not same?)}))))

(defn effective-config
  "Planner settings while section plays: its :config over the base settings."
  [config section]
  (merge (dissoc config :sections) (:config section)))
