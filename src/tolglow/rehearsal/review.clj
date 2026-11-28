(ns tolglow.rehearsal.review
  "Measure a rehearsal take and write a report the operator can judge from
  (#670). Pure: frame records and logs from tolglow.rehearsal/run-take in,
  data and text out. The numbers locate places to look at and listen to;
  they do not say whether a passage is musical.

  Club: light changes against the bar and phrase grid, the agent's response
  to each section line, repetition and novelty phrase by phrase, darkness and
  strobe contrast per section, and what a transport stop held. Concert: per
  take the section anchors, fills, stem hits, Push gestures, overwritten
  gestures and ramps that yielded to the performer; across two takes what
  stayed anchored and what varied."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [tolglow.rehearsal.passage :as passage]))

(def ^:private eps 1e-6)

(defn- fmt-number [x] (if (number? x) (str (/ (Math/rint (* 100.0 x)) 100.0)) "–"))

(def repeat-threshold
  "A phrase whose look signature differs from the previous phrase's by less
  than this (Jaccard distance) counts as a repeat."
  0.2)

(def late-threshold-beats
  "An agent change accepted more than this after its section line is late.
  One 125 ms frame is a quarter beat at 120 BPM."
  0.5)

(defn- on-grid? [beat step] (< (Math/abs (- beat (* step (Math/rint (/ beat step))))) eps))

(defn- mean [xs] (if (seq xs) (/ (reduce + xs) (double (count xs))) 0.0))

(defn- round [x places] (when x (let [f (Math/pow 10 places)] (/ (Math/rint (* x f)) f))))

(defn- bar-label
  "Zero-based bar and beat, as the scheduler reports positions."
  [beat]
  (let [bar (long (Math/floor (/ (+ beat eps) passage/beats-per-bar)))
        within (- beat (* bar passage/beats-per-bar))]
    (if (< (Math/abs within) eps) (str "bar " bar)
        (str "bar " bar " beat " (round within 2)))))

(defn launches
  "Every instance once, from the first frame it appears in, with the music
  and auto sections that frame was in."
  [frames]
  (vals (reduce (fn [seen {:keys [instances music auto-section beat auto-interval]}]
                  (reduce-kv (fn [seen id i]
                               (if (contains? seen id) seen
                                   (assoc seen id (assoc (select-keys i [:cue-id :adapter :session :params :launch-mode :duration :started-at-beat])
                                                         :id id :music music :auto-section auto-section :seen-beat beat
                                                         :interval auto-interval))))
                             seen instances))
                (sorted-map-by (fn [a b] (compare [(count a) a] [(count b) b])))
                frames)))

(defn- start-beat [launch] (or (:started-at-beat launch) (:seen-beat launch)))

(defn- phrase-of [beat] (long (Math/floor (/ (+ beat eps) passage/beats-per-phrase))))

(defn- signature
  "A look as a set of [cue colour] pairs: what was launched, in which colour."
  [launches]
  (into #{} (map (fn [l] [(:cue-id l) (get-in l [:params :color])])) launches))

(defn- jaccard-distance
  "Distance between two sets, or nil when both are empty: two silent
  phrases are not a repeat of each other."
  [a b]
  (let [union (set/union a b)]
    (when (seq union) (- 1.0 (/ (count (set/intersection a b)) (double (count union)))))))

(defn- frame-light [frames]
  (let [playing (filter :playing? frames)]
    {:frames (count playing)
     :mean-lit (round (mean (map (comp count :lit) playing)) 2)
     :mean-brightness (round (mean (map :brightness playing)) 3)
     :dark-fraction (round (mean (map #(if (<= (count (:lit %)) 1) 1.0 0.0) playing)) 3)
     :strobe-fraction (round (mean (map #(if (:strobe? %) 1.0 0.0) playing)) 3)}))

(defn- phrase-table [passage frames auto-launches]
  (let [phrases (range (long (Math/ceil (/ (passage/length-beats passage) passage/beats-per-phrase))))
        by-phrase (group-by (comp phrase-of start-beat) auto-launches)
        frames-by (group-by (comp phrase-of :beat) (filter :playing? frames))]
    (:rows
     (reduce (fn [{:keys [rows seen previous]} p]
               (let [ls (get by-phrase p [])
                     sig (signature ls)
                     cues (set (map :cue-id ls))
                     fs (get frames-by p [])]
                 {:rows (conj rows (merge {:phrase p :first-bar (* p (:bars-per-phrase passage/meter))
                                           :music (vec (distinct (keep :music fs)))
                                           :auto-sections (vec (distinct (keep :auto-section fs)))
                                           :launches (count ls)
                                           :cues (vec (sort-by str cues))
                                           :colours (count (distinct (keep #(get-in % [:params :color]) ls)))
                                           :new-cues (vec (sort-by str (set/difference cues seen)))
                                           :retained-cues (vec (sort-by str (set/intersection cues (set (map first previous)))))
                                           :novelty (when (some? previous) (some-> (jaccard-distance sig previous) (round 3)))}
                                          (dissoc (frame-light fs) :frames)))
                  :seen (into seen cues) :previous sig}))
             {:rows [] :seen #{} :previous nil} phrases))))

(defn- repetition [rows]
  (let [repeats (map #(and (some? (:novelty %)) (< (:novelty %) repeat-threshold)) rows)
        runs (->> (map vector rows repeats) (partition-by second) (filter (comp second first)))
        longest (first (sort-by (comp - count) runs))]
    {:threshold repeat-threshold
     :mean-novelty (round (mean (keep :novelty rows)) 3)
     :repeat-phrases (vec (keep (fn [[row r]] (when r (:phrase row))) (map vector rows repeats)))
     :longest-repeat-run (if longest {:phrases (mapv (comp :phrase first) longest) :length (count longest)} {:phrases [] :length 0})}))

(defn- applied-beat
  "The beat an agent change applied, or nil when it never did. A logged
  outcome (:applied-status) is authoritative; a hand-built log without one
  applied an unscheduled change when the owner accepted it."
  [{:keys [applied-beat accepted-beat scheduled? applied-status]}]
  (if (or scheduled? (some? applied-status)) applied-beat (or applied-beat accepted-beat)))

(defn- outcome
  "What became of an agent suggestion: :applied, :refused-stale (the show
  refused it because its basis or window had passed, #670), :agent-failed (a
  disconnected agent sent nothing), :timed-out (a silent agent the host gave
  up on), or the scheduler's status for a change that never fired."
  [entry]
  (if (applied-beat entry) :applied
      (case (:applied-status entry)
        :stale-suggestion :refused-stale
        :not-sent :agent-failed
        :timed-out :timed-out
        (:applied-status entry :never-applied))))

(defn- changes
  "Where each agent change took effect, as the review reads it: the beat it
  applied and whether it applied before the planner decided on that frame (a
  change on its line through configure_auto's :at) or after (a change applied
  on arrival, whose frame the previous settings had already decided on). A
  change that never applied is left out."
  [agent-log]
  (vec (keep (fn [{:keys [scheduled?] :as entry}]
               (when-let [beat (applied-beat entry)]
                 {:beat beat :scheduled? (boolean scheduled?)}))
             agent-log)))

(defn- frame-beats [bpm frame-ms] (/ (* frame-ms bpm) 60000.0))

(defn- with-change
  "The agent change a section seen first at seen-beat began with, if any: a
  change on its line enters the new plan on its own frame, one applied on
  arrival on the next frame, and the first frame recorded follows the first
  change."
  [changes frame seen-beat]
  (some (fn [{:keys [beat] :as change}] (when (<= (- beat eps) seen-beat (+ beat frame eps)) change)) changes))

(defn- auto-boundaries
  "Each auto section the planner entered. A section that follows another
  begins where its timer starts, on the grid. The first section of a new
  configuration begins with the agent's change: it is reported at the frame
  that entered it and marked :with-agent-change?, never counted as a plan
  boundary, wherever its timer starts (the first grid line at least one
  length away sets its end)."
  [frames changes frame]
  (->> frames
       (keep #(when (and (:auto-section %) (:auto-section-start %))
                [[(:auto-section %) (:auto-section-start %)] [(:beat %) (:auto-section-length %)]]))
       (reduce (fn [seen [k v]] (if (some #(= k (first %)) seen) seen (conj seen [k v]))) [])
       (mapv (fn [[[id start] [seen-beat length]]]
               (let [change (with-change changes frame seen-beat)
                     with-agent? (boolean (or change (> start (+ seen-beat eps))))
                     beat (cond change (:beat change) with-agent? seen-beat :else start)]
                 {:auto-section id :beat beat :position (bar-label beat) :timer-start start
                  :with-agent-change? with-agent?
                  :length length
                  :on-bar? (on-grid? beat passage/beats-per-bar)
                  :on-own-grid? (boolean (and length (on-grid? beat length)))
                  :on-phrase? (on-grid? beat passage/beats-per-phrase)})))))

(defn- session-before
  "The auto session playing just before beat."
  [frames beat]
  (:auto-session (last (filter #(< (:beat %) (- beat eps)) frames))))

(defn- session-after
  "The auto session a change installed: the first one recorded from the beat
  it applied that differs from the session it replaced. A change on its line
  shows on the frame it applied (it applies before the planner decides
  there); one applied on arrival shows on the next frame, since the frame it
  arrived on was recorded before it."
  [frames applied replaced]
  (when applied
    (some #(when (and (>= (:beat %) (- applied eps)) (some? (:auto-session %)) (not= replaced (:auto-session %)))
             (:auto-session %))
          frames)))

(defn- agent-responses
  "Each agent suggestion against its section line: when it was accepted and
  when it applied, the first launch of the settings it installed, launches
  the previous settings made from the line on (carry-over), launches of the
  new settings before the line (early), and whether it landed in a later
  section than the one it was for (stale). Each launch is attributed to the
  auto session that made it (its source session), not to when it was seen.
  auto-launches come from the take's committed launch receipts when it has
  them, so a launch the previous settings made on the change's own frame
  and ended in that tick (as installing the change does) still counts as
  carry-over; frame snapshots alone cannot show it."
  [passage agent-log auto-launches frames]
  (mapv (fn [{:keys [section section-beat accepted-beat suggested-beat status receipts tools scheduled?
                     applied-status late-beats-on-arrival] :as entry}]
          (let [scheduled? (boolean scheduled?)
                applied (applied-beat entry)
                result (outcome entry)
                previous (session-before frames (min section-beat (or applied section-beat)))
                session (session-after frames applied previous)
                mine (when session (filter #(= session (:session %)) auto-launches))
                early (filter #(< (start-beat %) (- section-beat eps)) mine)
                carry (when (and previous (not= previous session))
                        (filter #(and (= previous (:session %)) (>= (start-beat %) (- section-beat eps))) auto-launches))
                landed (when applied (:id (passage/section-at passage applied)))]
            (cond-> {:section section :section-beat section-beat :position (bar-label section-beat)
                     :suggested-beat suggested-beat :accepted-beat accepted-beat
                     :scheduled? scheduled? :applied-beat applied :session session
                     :late-beats (when applied (round (- applied section-beat) 3))
                     :first-launch-beat (first (sort (map start-beat mine)))
                     ;; A change that never applied leaves the previous mood playing by design:
                     ;; its launches are not carry-over.
                     :carry-over-launches (if applied (count carry) 0)
                     :early-launches (count early)
                     :outcome result
                     :landed-in landed :stale? (boolean (and (some? landed) (> applied section-beat) (not= landed section)))
                     :status status :tools tools
                     :receipts (update-vals receipts :status)}
              late-beats-on-arrival (assoc :arrived-late-beats late-beats-on-arrival)
              (nil? applied) (assoc :applied-status applied-status :previous-mood-held? true)
              (:error entry) (assoc :agent-error (:error entry))
              (:timed-out? entry) (assoc :timed-out? true)
              (#{:refused-stale} result) (assoc :refusal (get-in receipts ["configure_auto" :message])))))
        agent-log))

(defn continuity
  "Whether auto mode kept deciding while the transport played (#670). A
  decision is a frame where the planner's tick advanced or a new session
  began. A gap longer than
  two of its own intervals plus a frame, measured in transport beats (so a
  stop is not a gap), is a stall. held is what the take left waiting when it
  ended: auto changes still pending and agent runs the host had to close."
  [frames held frame-beats]
  (let [playing (filterv :playing? frames)
        decisions (->> (partition 2 1 playing)
                       ;; A new session starts its own tick count, so a session change is a decision too.
                       (keep (fn [[a b]] (when (and (:auto-session b)
                                                    (or (not= (:auto-session a) (:auto-session b))
                                                        (> (or (:auto-tick b) 0) (or (:auto-tick a) 0))))
                                           b)))
                       vec)
        end-beat (:beat (peek playing))
        gaps (mapv (fn [[a b]]
                     (let [interval (get-in a [:auto-interval :value])]
                       {:from (:beat a) :to (:beat b) :beats (round (- (:beat b) (:beat a)) 3)
                        :interval interval
                        :stall? (boolean (and interval (> (- (:beat b) (:beat a)) (+ (* 2 interval) frame-beats eps))))}))
                   (partition 2 1 (cond-> decisions
                                    ;; The last decision to the end of the passage counts too.
                                    (and (seq decisions) end-beat (< (:beat (peek decisions)) end-beat))
                                    (conj {:beat end-beat :auto-interval nil}))))
        longest (first (sort-by (comp - :beats) gaps))]
    (merge {:decisions (count decisions)
            :last-decision-beat (:beat (peek decisions))
            :longest-gap (when longest (select-keys longest [:from :to :beats :interval]))
            :stalls (mapv #(select-keys % [:from :to :beats :interval]) (filter :stall? gaps))}
           (select-keys held [:pending-auto-changes :agent-runs-open-at-end :auto-enabled?]))))

(defn- launch-timing
  "How auto launches sit on their own interval's grid. Each decision falls
  due on a grid point of its interval (a beat, bar or phrase line, or the
  interval's own length when shorter); the decision fires on the first
  frame at or after it. A decision taken when a new configuration arrives
  is immediate by design and is counted apart: on its own frame for a change
  on its line, recorded on the next frame for one applied on arrival. An
  interval that is not a whole number of steps (say {:beats 3/2} on a beat
  grid) need not fall due on a grid line, as the grid rule allows."
  [auto-launches changes bpm frame-ms]
  (let [frame-beats (frame-beats bpm frame-ms)
        configure? (fn [l] (some (fn [{:keys [beat scheduled?]}]
                                   (and (< (abs (- (start-beat l) beat)) eps)
                                        (or scheduled? (> (:seen-beat l (start-beat l)) (+ (start-beat l) eps)))))
                                 changes))
        timed (remove configure? (filter :interval auto-launches))
        rows (map (fn [l] (let [{:keys [started-at step value]} (:interval l)]
                            {:offset (- (start-beat l) started-at)
                             :on-grid? (or (and value (not (on-grid? value step))) (on-grid? started-at step))}))
                  timed)
        offsets (map :offset rows)]
    {:launches (count auto-launches)
     :at-configuration (count (filter configure? auto-launches))
     :measured (count rows)
     :due-on-own-grid (count (filter :on-grid? rows))
     :max-after-due (round (if (seq offsets) (apply max offsets) 0.0) 3)
     :before-due (count (filter #(< % (- eps)) offsets))
     :frame-beats (round frame-beats 3)
     :beyond-one-frame (count (filter #(> % (+ frame-beats eps)) offsets))}))

(defn- section-contrast [passage frames]
  (let [by-music (group-by :music (filter :playing? frames))]
    (mapv (fn [{:keys [id kind start-bar bars]}]
            (merge {:section id :kind kind :bars [start-bar (+ start-bar bars)]} (frame-light (get by-music id []))))
          (:sections passage))))

(defn- stops [frames transport]
  (let [indexed (map-indexed vector frames)]
    (vec
     (keep (fn [{:keys [op at-beat]}]
             (when (= :stop op)
               (let [stopped (filter (comp false? :playing? second) indexed)
                     held (filter #(>= (:beat (second %)) (- at-beat eps)) stopped)
                     first-held (first held)
                     run (take-while (comp false? :playing? second) (drop (first first-held) indexed))
                     last-index (first (last run))
                     before (when first-held (nth frames (max 0 (dec (first first-held)))))
                     after (when last-index (drop (inc last-index) frames))
                     resumed (first after)
                     first-beat (take-while #(< (:beat %) (+ (:beat resumed 0) 1.0)) after)
                     launched-during (set/difference (set (mapcat (comp keys :instances second) run))
                                                     (set (keys (:instances before))))]
                 (when (seq run)
                   {:stop-beat (:beat (second (first run))) :position (bar-label (:beat (second (first run))))
                    :frames-stopped (count run)
                    :beat-held? (apply = (map (comp :beat second) run))
                    :decisions-while-stopped (- (:auto-tick (second (last run)) 0) (:auto-tick before 0))
                    :launches-while-stopped (count launched-during)
                    :resume-beat (:beat resumed)
                    :decisions-in-first-resumed-beat (- (:auto-tick (last first-beat) 0) (:auto-tick (second (last run)) 0))}))))
           transport))))

(defn- carry-over-flags
  "Flags for the previous mood deciding on a section line before the
  agent's change, and for a change that arrived early enough that the new
  mood launched in the previous section."
  [take responses]
  (let [who (if take (str "Take " (name take) ": ") "")]
    (concat
     (for [r responses :when (and (:scheduled? r) (nil? (:applied-beat r))
                                  (not (#{:refused-stale :agent-failed :timed-out} (:outcome r))))]
       (str who "The agent's change for " (name (:section r)) " never applied (" (name (:applied-status r :pending)) ")."))
     (for [r responses :when (and (:late-beats r) (<= (:late-beats r) late-threshold-beats) (pos? (:carry-over-launches r)))]
       (str who "On the " (name (:section r)) " line the previous mood launched " (:carry-over-launches r)
            " looks before the agent's change landed, " (:late-beats r) " beats after the line."))
     (for [r responses :when (pos? (:early-launches r 0))]
       (str who "The agent's change for " (name (:section r)) " landed " (- (:late-beats r)) " beats before the line; the new mood launched "
            (:early-launches r) " looks in the previous section.")))))

(defn- agent-outage-flags
  "Flags for suggestions that never applied because the show refused them as
  stale or the agent did not answer. The previous mood played on."
  [take responses]
  (let [who (if take (str "Take " (name take) ": ") "")]
    (for [r responses :when (#{:refused-stale :agent-failed :timed-out} (:outcome r))]
      (str who
           (case (:outcome r)
             :refused-stale (str "The agent's suggestion for " (name (:section r)) " arrived at beat " (round (:accepted-beat r) 2)
                                 " and was refused as stale")
             :agent-failed (str "The agent did not answer for " (name (:section r))
                                (when (:agent-error r) (str " (" (name (:agent-error r)) ")")))
             :timed-out (str "The agent did not answer for " (name (:section r)) " and the host gave up"))
           "; the previous mood played on."))))

(defn- continuity-flags [take {:keys [stalls pending-auto-changes agent-runs-open-at-end]}]
  (let [who (if take (str "Take " (name take) ": ") "")]
    (concat
     (for [{:keys [from beats interval]} stalls]
       (str who "Auto made no decision for " beats " beats from beat " (round from 2) " (interval " (fmt-number interval) " beats)."))
     (when (pos? (or pending-auto-changes 0))
       [(str who pending-auto-changes " auto changes were still waiting when the passage ended.")])
     (when (pos? (or agent-runs-open-at-end 0))
       [(str who agent-runs-open-at-end " agent run was still open when the passage ended; the host closed it.")]))))

(defn- club-flags [{:keys [boundaries responses contrast repetition stops launch-timing continuity]}]
  (let [by-kind (group-by :kind contrast)
        brightness (fn [kind] (mean (map :mean-brightness (by-kind kind))))]
    (vec
     (concat
      (for [b boundaries :when (and (not (:on-bar? b)) (not (:with-agent-change? b)))]
        (str "Auto section " (name (:auto-section b)) " began off the bar grid at " (:position b) "."))
      (for [r responses :when (and (:late-beats r) (> (:late-beats r) late-threshold-beats))]
        (str "The agent's change for " (name (:section r)) " landed " (:late-beats r) " beats after the section line"
             (when (pos? (:carry-over-launches r)) (str "; the previous mood launched " (:carry-over-launches r) " looks into it"))
             "."))
      (carry-over-flags nil responses)
      (for [r responses :when (:stale? r)]
        (str "The agent's suggestion for " (name (:section r)) " landed in " (name (:landed-in r)) "."))
      (agent-outage-flags nil responses)
      (continuity-flags nil continuity)
      (when (pos? (:beyond-one-frame launch-timing))
        [(str (:beyond-one-frame launch-timing) " auto launches started more than one frame after their decision fell due.")])
      (when (pos? (:before-due launch-timing))
        [(str (:before-due launch-timing) " auto launches started before their decision fell due.")])
      (when (< (:due-on-own-grid launch-timing) (:measured launch-timing))
        [(str (- (:measured launch-timing) (:due-on-own-grid launch-timing)) " auto decisions fell due off their interval's grid.")])
      (when (>= (get-in repetition [:longest-repeat-run :length]) 3)
        [(str "Phrases " (str/join ", " (get-in repetition [:longest-repeat-run :phrases]))
              " repeat the same look; listen for whether that holds a motif or stalls.")])
      (for [c contrast :when (and (#{:quiet :breakdown} (:kind c)) (pos? (:strobe-fraction c)))]
        (str "Strobe ran in the " (name (:kind c)) " section " (name (:section c)) "."))
      (when (and (by-kind :quiet) (by-kind :groove) (>= (brightness :quiet) (brightness :groove)))
        ["The quiet section is not darker than the groove."])
      (for [s stops :when (or (pos? (:decisions-while-stopped s)) (pos? (:launches-while-stopped s))
                              (> (:decisions-in-first-resumed-beat s) 1) (not (:beat-held? s)))]
        (str "The stop at " (:position s) " did not hold cleanly: " (pr-str (dissoc s :position))))))))

(defn- fmt [x] (cond (nil? x) "–" (float? x) (str (round x 2)) (keyword? x) (name x)
                     (sequential? x) (str/join " " (map fmt x)) :else (str x)))

(defn- table [headers rows]
  (str "| " (str/join " | " headers) " |\n"
       "|" (str/join "|" (repeat (count headers) " --- ")) "|\n"
       (apply str (map #(str "| " (str/join " | " (map fmt %)) " |\n") rows))))

(defn- passage-line [passage frame-ms]
  (str "Seed " (:seed passage) (when (:take passage) (str ", take " (name (:take passage)) " (take seed " (:take-seed passage) ")"))
       ", " (fmt (:bpm passage)) " BPM, " (/ (passage/length-beats passage) passage/beats-per-bar) " bars; agent "
       (if (some? (:agent-lag-beats passage)) (str "lag " (fmt (:agent-lag-beats passage)))
           (str "lead " (fmt (:agent-lead-beats passage passage/default-agent-lead-beats))))
       " beats, latency " (fmt (:agent-latency-beats passage 0)) " beats; "
       (str/join ", " (map #(str (name (:id %)) " bars " (:start-bar %) "–" (+ (:start-bar %) (:bars %))) (:sections passage)))
       ". Frames every " (fmt frame-ms) " ms."))

(def ^:private agent-note
  (str "Each configure_auto names the auto session the agent read and the end of its section as its window; the show refuses "
       "a reply whose session has changed or whose window has passed (Outcome refused-stale) and the previous mood plays on, as it "
       "does when the agent does not answer (agent-failed, timed-out). "
       "The recorded agent hears of each section line a bar ahead, as a song map gives it (:agent-lead-beats; :agent-lag-beats "
       "makes it hear the line late or early), and its reply arrives :agent-latency-beats later. Heard before the line, its "
       "configure_auto carries :at the line and applies on the first frame at or past it, before the planner decides on that "
       "frame (On the line); a reply arriving less than a bar after its line applies at once, marked late; heard after the line, "
       "it applies on arrival. Each launch is attributed to the auto session that made it: carry-over launches are the previous "
       "settings' launches from the line on, early launches the new settings' launches before the line."))

(def ^:private headless-note
  (str "Headless rehearsal: the real show owner rendered every frame into a capture sink. No Room window, "
       "device, Live set or provider was used; the agent's rounds are recorded tool calls. Bars and beats count from zero."))

(defn- continuity-text [{:keys [decisions last-decision-beat longest-gap stalls pending-auto-changes agent-runs-open-at-end]}]
  (str decisions " auto decisions while the transport played, the last at beat " (fmt last-decision-beat)
       "; the longest gap between decisions was " (fmt (:beats longest-gap)) " beats"
       (when (:interval longest-gap) (str " (interval " (fmt (:interval longest-gap)) ")"))
       ", " (count stalls) " longer than two intervals. When the passage ended " (or pending-auto-changes 0)
       " auto changes were waiting and " (or agent-runs-open-at-end 0) " agent runs were still open.\n"))

(defn- club-report [{:keys [passage frame-ms boundaries responses phrases repetition contrast stops flags launch-timing continuity]}]
  (str "# Club rehearsal\n\n" headless-note "\n\n" (passage-line passage frame-ms) "\n\n"
       "## Agent responses to section lines\n\n"
       (table ["Section" "Line" "Accepted at beat" "On the line" "Outcome" "Applied at beat" "Late (beats)" "Carry-over launches" "Early launches" "First launch of the new mood" "Receipts"]
              (map (fn [r] [(:section r) (:position r) (:accepted-beat r) (:scheduled? r) (:outcome r) (:applied-beat r) (:late-beats r) (:carry-over-launches r) (:early-launches r) (:first-launch-beat r)
                            (str/join ", " (map (fn [[k v]] (str k " " (fmt v))) (:receipts r)))])
                   responses))
       "\n" agent-note "\n"
       "\n## Phrasing\n\n"
       (let [{:keys [launches at-configuration measured due-on-own-grid max-after-due before-due frame-beats beyond-one-frame]} launch-timing]
         (str launches " auto launches. " at-configuration " were immediate decisions when a configuration arrived. Of the other "
              measured ", " due-on-own-grid " fell due on their own interval's grid (beat, bar or phrase), and the latest started "
              max-after-due " beats after its decision fell due (one frame is " frame-beats " beats); " beyond-one-frame
              " later than one frame, " before-due " before it.\n\n"))
       (let [planned (remove :with-agent-change? boundaries)]
         (str (count boundaries) " auto sections began: " (- (count boundaries) (count planned))
              " with an agent change (timed by the agent row above) and " (count planned) " at a plan boundary, "
              (count (filter :on-bar? planned)) " of them on the bar grid, "
              (count (filter :on-own-grid? planned)) " on a multiple of their own length and "
              (count (filter :on-phrase? planned)) " on the phrase grid.\n\n"))
       (table ["Auto section" "Began" "Length (beats)" "With agent change" "On bar" "On own length" "On phrase"]
              (map (juxt :auto-section :position :length :with-agent-change? :on-bar? :on-own-grid? :on-phrase?) boundaries))
       "\n## Phrase by phrase\n\n"
       "Novelty is the Jaccard distance between a phrase's set of launched [cue, colour] pairs and the previous phrase's; 0 repeats it exactly.\n\n"
       (table ["Phrase" "First bar" "Music" "Auto sections" "Launches" "Cues" "New" "Novelty" "Lit" "Brightness" "Strobe" "Operator note"]
              (map (fn [r] [(:phrase r) (:first-bar r) (:music r) (:auto-sections r) (:launches r) (:cues r) (:new-cues r)
                            (:novelty r) (:mean-lit r) (:mean-brightness r) (:strobe-fraction r) ""])
                   phrases))
       "\nMean novelty " (fmt (:mean-novelty repetition)) "; repeats (novelty < " (:threshold repetition) ") in phrases "
       (fmt (:repeat-phrases repetition)) "; longest repeat run " (get-in repetition [:longest-repeat-run :length]) ".\n"
       "\n## Darkness and strobe contrast\n\n"
       (table ["Section" "Kind" "Bars" "Mean lit heads" "Mean brightness" "Dark frames" "Strobe frames"]
              (map (juxt :section :kind :bars :mean-lit :mean-brightness :dark-fraction :strobe-fraction) contrast))
       "\n## Continuity\n\n" (continuity-text continuity)
       "\n## Transport stops\n\n"
       (if (empty? stops) "No stop in this passage.\n"
           (table ["Stopped at" "Frames stopped" "Beat held" "Decisions while stopped" "Launches while stopped" "Decisions in first resumed beat"]
                  (map (juxt :position :frames-stopped :beat-held? :decisions-while-stopped :launches-while-stopped :decisions-in-first-resumed-beat) stops)))
       "\n## Measured flags\n\n"
       (if (empty? flags) "None.\n" (apply str (map #(str "- " % "\n") flags)))
       "\n## For the operator\n\n"
       "Watch the passage and fill the Operator note column: mark each phrase where a change felt early, "
       "repetitive or unrelated to the music. The numbers above say where to look, not whether it worked.\n"))

(defn club
  "Review an unattended club take."
  [{:keys [passage frames agent transport frame-ms] :as take}]
  (let [all (launches frames)
        auto-launches (filter #(= :auto (:adapter %)) all)
        changes (changes agent)
        frame (frame-beats (:bpm passage) frame-ms)
        boundaries (auto-boundaries frames changes frame)
        ;; Committed launches from receipts, when the take has them: a look the
        ;; previous settings launched and ended within one tick shows in no frame.
        responses (agent-responses passage agent (or (:auto-launches take) auto-launches) frames)
        phrases (phrase-table passage frames auto-launches)
        result {:id :club :passage (dissoc passage :onsets :levels) :frame-ms frame-ms
                :boundaries boundaries :responses responses :phrases phrases
                :repetition (repetition phrases)
                :launch-timing (launch-timing auto-launches changes (:bpm passage) frame-ms)
                :contrast (section-contrast passage frames)
                :stops (stops frames transport)
                :continuity (continuity frames (:held take) frame)
                :launch-count (count auto-launches)
                :error (get-in take [:status :error])}
        result (assoc result :flags (club-flags result))]
    (assoc result :report (club-report result) :take take)))

;; Concert

(defn- section-of [passage beat] (:id (passage/section-at passage beat)))

(defn- anchors [passage all]
  (mapv (fn [{:keys [id anchor start-bar]}]
          (let [planned (double (* passage/beats-per-bar start-bar))
                launch (first (filter #(and (= anchor (:cue-id %)) (= :agent (:adapter %))
                                            (<= (- planned eps) (start-beat %) (+ planned 1.0)))
                                      all))]
            {:section id :cue-id anchor :planned-beat planned :position (bar-label planned)
             :started-at-beat (some-> launch start-beat) :late-beats (some-> launch start-beat (- planned) (round 3))
             :missed? (nil? launch)}))
        (filter :anchor (:sections passage))))

(defn- overwritten
  "Push parameter writes whose value another writer replaced while the
  performer held that instance: from the frame after the gesture was
  accepted until the instance ended or the performer wrote it again."
  [frames gestures]
  (vec
   (keep (fn [[g next-g]]
           (when (and (= :param/set (:op g)) (= :accepted (:status g)))
             (let [until (or (when next-g (:accepted-beat next-g)) Double/POSITIVE_INFINITY)
                   held (filter #(and (> (:beat %) (:accepted-beat g)) (< (:beat %) until)
                                      (= :running (get-in % [:instances (:target g) :phase])))
                                frames)
                   changed (first (filter #(not= (:value g) (get-in % [:instances (:target g) :params (:param g)])) held))]
               (when changed {:label (:label g) :target (:target g) :param (:param g) :value (:value g)
                              :at-beat (:beat changed) :became (get-in changed [:instances (:target g) :params (:param g)])}))))
         (let [writes (vec (filter #(= :param/set (:op %)) gestures))]
           (map (fn [g] [g (first (filter #(and (= (:target %) (:target g)) (> (:beat %) (:beat g))) writes))]) writes)))))

(defn- agent-ramps
  "The final outcome of each agent ramp in the scheduler's recent history,
  with who ended it when it was cancelled."
  [schedule]
  (->> (:recent schedule)
       (filter #(and (= :param/ramp (:op %)) (= :agent (get-in % [:source :adapter]))))
       (reduce (fn [m e] (assoc m (:event-id e) e)) (sorted-map))
       vals
       (mapv #(cond-> (select-keys % [:event-id :status :reason :at-beat :from :to :value])
                (:by %) (assoc :by (get-in % [:by :source :adapter]))))))

(defn- concert-take [{:keys [passage frames gestures agent schedule] :as take}]
  (let [all (launches frames)
        by-adapter (group-by :adapter all)
        per-section (fn [ls] (group-by #(section-of passage (start-beat %)) ls))]
    {:take (:take passage) :passage (dissoc passage :onsets :levels)
     :anchors (anchors passage all)
     :fills (per-section (:auto by-adapter))
     :stem-hits (per-section (:event-subscriber by-adapter))
     :push-launches (vec (get by-adapter (:adapter passage/push)))
     :gestures (mapv #(dissoc % :delivered-beat) gestures)
     :overwritten (overwritten frames gestures)
     :ramps (agent-ramps schedule)
     :agent (agent-responses passage agent (or (:auto-launches take) (:auto by-adapter)) frames)
     :continuity (continuity frames (:held take) (frame-beats (:bpm passage) (:frame-ms take)))
     :light (section-contrast passage frames)
     :error (get-in take [:status :error])}))

(defn- fill-key [l] [(:cue-id l) (round (start-beat l) 3)])

(defn- compare-takes [a b]
  (let [sections (:sections (:passage a))]
    {:anchors (mapv (fn [x y] {:section (:section x) :cue-id (:cue-id x)
                               :a (:started-at-beat x) :b (:started-at-beat y)
                               :kept? (and (not (:missed? x)) (not (:missed? y))
                                           (= (:started-at-beat x) (:started-at-beat y)))})
                    (:anchors a) (:anchors b))
     :sections (mapv (fn [{:keys [id]}]
                       (let [fa (set (map fill-key (get-in a [:fills id])))
                             fb (set (map fill-key (get-in b [:fills id])))
                             params-a (into {} (map (juxt fill-key :params)) (get-in a [:fills id]))
                             params-b (into {} (map (juxt fill-key :params)) (get-in b [:fills id]))
                             shared (set/intersection fa fb)
                             sa (set (map fill-key (get-in a [:stem-hits id])))
                             sb (set (map fill-key (get-in b [:stem-hits id])))]
                         {:section id
                          :fills-a (count fa) :fills-b (count fb) :fills-shared (count shared)
                          :shared-fills-with-other-params (count (filter #(not= (params-a %) (params-b %)) shared))
                          :fill-distance (some-> (jaccard-distance fa fb) (round 3))
                          :stem-hits-a (count sa) :stem-hits-b (count sb) :stem-hits-shared (count (set/intersection sa sb))
                          :stem-distance (some-> (jaccard-distance sa sb) (round 3))
                          :gestures-a (vec (keep #(when (= id (section-of (:passage a) (:beat %))) (:label %)) (:gestures a)))
                          :gestures-b (vec (keep #(when (= id (section-of (:passage b) (:beat %))) (:label %)) (:gestures b)))}))
                     sections)}))

(defn- concert-flags [takes comparison]
  (vec
   (concat
    (mapcat #(carry-over-flags (:take %) (:agent %)) takes)
    (mapcat #(agent-outage-flags (:take %) (:agent %)) takes)
    (mapcat #(continuity-flags (:take %) (:continuity %)) takes)
    (for [t takes r (:agent t) :when (and (:late-beats r) (> (:late-beats r) late-threshold-beats))]
      (str "Take " (name (:take t)) ": the agent's change for " (name (:section r)) " landed " (:late-beats r)
           " beats after the section line" (when (pos? (:carry-over-launches r)) (str "; the previous mood launched " (:carry-over-launches r) " looks into it")) "."))
    (for [t takes a (:anchors t) :when (:missed? a)]
      (str "Take " (name (:take t)) " missed the " (name (:cue-id a)) " anchor at " (:position a) "."))
    (for [t takes a (:anchors t) :when (and (:late-beats a) (> (:late-beats a) eps))]
      (str "Take " (name (:take t)) " started the " (name (:cue-id a)) " anchor " (:late-beats a) " beats late."))
    (for [a (:anchors comparison) :when (not (:kept? a))]
      (str "The " (name (:section a)) " anchor differs between takes."))
    (for [t takes o (:overwritten t)]
      (str "Take " (name (:take t)) ": " (:label o) " was overwritten at beat " (fmt (:at-beat o)) "."))
    (for [t takes g (:gestures t) :when (not= :accepted (:status g))]
      (str "Take " (name (:take t)) ": gesture \"" (:label g) "\" was " (fmt (:status g)) (when (:message g) (str " (" (:message g) ")")) ".")))))

(defn- concert-report [{:keys [takes comparison flags frame-ms]}]
  (let [[a b] takes]
    (str "# Concert rehearsal: two takes\n\n" headless-note "\n\n"
         "Take a: " (passage-line (:passage a) frame-ms) "\n\nTake b: " (passage-line (:passage b) frame-ms) "\n\n"
         "## Section anchors\n\n"
         "Both takes post the same anchor events, so \"Kept\" checks that the scheduler started them identically; "
         "it says nothing about the agent's choices.\n\n"
         (table ["Section" "Anchor" "Take a starts" "Take b starts" "Kept"]
                (map (juxt :section :cue-id :a :b :kept?) (:anchors comparison)))
         "\n## What varied\n\n"
         "Fills are auto launches; stem hits are launches from stem onsets. Distance is the Jaccard distance between the two takes' [cue, start beat] sets.\n\n"
         "\"Other params\" counts shared fills whose seeded parameters (colour, span, level) differ.\n\n"
         (table ["Section" "Fills a" "Fills b" "Shared" "Other params" "Fill distance" "Stem hits a" "Stem hits b" "Shared" "Stem distance" "Push a" "Push b"]
                (map (juxt :section :fills-a :fills-b :fills-shared :shared-fills-with-other-params :fill-distance :stem-hits-a :stem-hits-b
                           :stem-hits-shared :stem-distance #(str/join "; " (:gestures-a %)) #(str/join "; " (:gestures-b %)))
                     (:sections comparison)))
         "\n## Push gestures\n\n"
         (table ["Take" "Recorded at" "Gesture" "Target" "Status" "Accepted at beat"]
                (for [t takes g (:gestures t)]
                  [(:take t) (bar-label (:beat g)) (:label g) (:target g) (:status g) (:accepted-beat g)]))
         "\n"
         (apply str (for [t takes]
                      (str "Take " (name (:take t)) ": " (count (:overwritten t)) " overwritten gestures; agent ramps "
                           (if (seq (:ramps t))
                             (str/join ", " (map #(str (:event-id %) " " (fmt (:status %)) " at beat " (fmt (:at-beat %))
                                                      (when (:reason %) (str " (" (fmt (:reason %)) (when (:by %) (str " by " (fmt (:by %)))) ")")))
                                                (:ramps t)))
                             "none recorded")
                           ".\n")))
         "\n## Agent\n\n"
         agent-note "\n\n"
         (table ["Take" "Section" "Line" "Accepted at beat" "On the line" "Outcome" "Applied at beat" "Late (beats)" "Carry-over launches" "Early launches" "Receipts"]
                (for [t takes r (:agent t)]
                  [(:take t) (:section r) (:position r) (:accepted-beat r) (:scheduled? r) (:outcome r) (:applied-beat r) (:late-beats r) (:carry-over-launches r) (:early-launches r)
                   (str/join ", " (map (fn [[k v]] (str k " " (fmt v))) (:receipts r)))]))
         "\n## Continuity\n\n"
         (apply str (for [t takes] (str "Take " (name (:take t)) ": " (continuity-text (:continuity t)))))
         "\n## Measured flags\n\n"
         (if (empty? flags) "None.\n" (apply str (map #(str "- " % "\n") flags)))
         "\n## For the operator\n\n"
         "Watch both takes side by side. Note where the anchors felt like the same song, where a fill, stem hit "
         "or Push gesture made a take better or worse, and any change that came early or late.\n")))

(defn concert
  "Review two takes of the concert passage and compare them."
  [take-a take-b]
  (let [takes [(concert-take take-a) (concert-take take-b)]
        comparison (compare-takes (first takes) (second takes))
        result {:id :concert :frame-ms (:frame-ms take-a) :takes takes :comparison comparison}
        result (assoc result :flags (concert-flags takes comparison))]
    (assoc result :report (concert-report result) :raw [take-a take-b])))

;; Artefact

(defn timeline
  "One row per whole beat while playing: the last frame within it. Enough to
  chart a take next to its music without every frame."
  [frames]
  (->> (filter :playing? frames)
       (partition-by #(long (Math/floor (+ (:beat %) eps))))
       (mapv (fn [fs] (let [f (last fs)]
                        {:beat (long (Math/floor (+ (:beat f) eps))) :music (:music f) :auto-section (:auto-section f)
                         :lit (count (:lit f)) :brightness (round (:brightness f) 3) :strobe? (:strobe? f)
                         :running (vec (sort-by str (keep (fn [i] (when (= :running (:phase i)) (:cue-id i))) (vals (:instances f)))))})))))

(defn artefact
  "The review as data for the operator's files: everything except frame
  records, which are summarized by a per-beat timeline and launch list."
  [result]
  (case (:id result)
    :club (-> result (dissoc :take :report)
              (assoc :timeline (timeline (get-in result [:take :frames]))
                     :launches (mapv #(dissoc % :music) (launches (get-in result [:take :frames])))
                     :agent-wall-ms (mapv :wall-ms (get-in result [:take :agent]))))
    :concert (-> result (dissoc :raw :report)
                 (assoc :timelines (mapv (comp timeline :frames) (:raw result))
                        :agent-wall-ms (mapv #(mapv :wall-ms (:agent %)) (:raw result))))))
