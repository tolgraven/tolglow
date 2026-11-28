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
            [tolglow.automation.grid :as grid]
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

(defn- start-timer
  "The section's timer at clock, on the grid of its length's unit (a beat,
  bar or phrase line) by the planner's shared rule: it follows the previous
  boundary when that is on the grid and less than one length ago; otherwise
  it ends on the first grid line at least its length less half a step away."
  [section clock boundary]
  (let [timer (time/start (:length section) clock)
        {:keys [value meter]} (:duration timer)]
    (assoc timer :started-at (grid/start value (grid/step (:length section) value meter) (:started-at timer) boundary))))

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
      ;; A section a plan edit removed plays out its line (under the base
      ;; settings), then the plan goes on at the next index (#217).
      (and (nil? section) (:timer state) (not (:done? (time/sample (:timer state) clock))))
      {:state state :entered nil :changed? false}

      (nil? section)
      (let [index (if (:timer state) (inc (:index state 0)) 0)
            boundary (when-let [{:keys [started-at duration]} (:timer state)] (+ started-at (:value duration)))
            chosen (choose-section sections seed index nil)]
        {:state {:id (:id chosen) :index index :run 1 :timer (start-timer chosen clock boundary)}
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
  "Planner settings while section plays: its :config over the base settings.
  :randomize and :launch merge one level deep (a section palette keeps the
  base ranges; a section launch keeps base launch :params, merged); a section
  :interval or :interval-ms replaces both base interval forms."
  [config section]
  (let [base (dissoc config :sections)
        own (:config section)
        base (cond-> base (or (contains? own :interval) (contains? own :interval-ms)) (dissoc :interval :interval-ms))
        deep (fn [a b] (if (and (map? a) (map? b)) (merge a b) (if (some? b) b a)))]
    (cond-> (merge base own)
      (contains? own :randomize) (assoc :randomize (deep (:randomize base) (:randomize own)))
      (contains? own :launch)
      (assoc :launch (let [own-launch (:launch own)
                           base-launch (cond-> (:launch base)
                                         (and (map? own-launch) (or (contains? own-launch :duration) (contains? own-launch :duration-ms)))
                                         (dissoc :duration :duration-ms))
                           launch (deep base-launch own-launch)]
                       (cond-> launch
                         (and (map? launch) (or (get-in base [:launch :params]) (get-in own [:launch :params])))
                         (assoc :params (merge (get-in base [:launch :params]) (get-in own [:launch :params])))))))))
