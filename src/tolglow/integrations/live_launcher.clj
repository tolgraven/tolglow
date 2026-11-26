(ns tolglow.integrations.live-launcher
  "Play a cue page from one Drum Rack track in Live (#228).

  The page's 8x8 cells become Drum Rack pads in Push's 64 Pads layout
  (ableton/pad-layouts), so the pad a performer hits on Push is the cell the
  cue sits in on the Tolglow page. Pad hits reach the show as Live bridge MIDI
  and launch cues through the shared control model (ableton/midi->commands);
  this namespace adds the pieces around that path:

  - `pads`: the 64 pads of a page under the bridge's note mappings, with
    each pad's cue definition, label and colour, and the page cells whose cue
    is not under its own pad.
  - `setup!`: the launcher track in the Set over a companion connection
    (live-connection): the companion's own track with a Drum Rack after it
    and one named, coloured chain per mapped pad, read back and verified.
  - `pad-clip` and `write-clip!`: a Session clip of many pad hits made from
    the run record (live-runs), one note per take at its cue's pad, so a
    performance can be replayed from the launcher track.

  Identity stays separate: a pad addresses a cue definition; a hit launches
  an instance (the show's instance ID); the run record keeps that run as a
  take. A pad clip holds timing only (note start and length); launch values
  and moves stay in the take.

  Recording without feedback: pad hits a performer plays while the launcher
  track plays no clip arrive as :live-input. They launch cues, the run record
  keeps them as takes and Live records them itself on an armed track; the
  bridge never writes them back. When a pad clip plays, the companion marks
  its notes :live-playback: they launch the same cues and make no takes and no
  clip writes (live-runs and ableton/record-command ignore that origin).
  Musical units are primary: a clip's beats are show-transport beats relative
  to its first bar."
  (:require [tolglow.integrations.ableton :as ableton]
            [tolglow.integrations.live-connection :as live]
            [tolglow.integrations.live-runs :as runs]))

(defn- fail! [message data] (throw (ex-info message data)))

(defn- wire-id
  "Registry IDs arrive as keywords or strings (bind-registry-ids); compare and
  display them by name."
  [x]
  (cond (keyword? x) (subs (str x) 1) (nil? x) nil :else (str x)))

(defn track-name
  "The launcher track for a cue page. Names come from the page ID, which stays
  fixed when the page is renamed, so a reopened Set reconnects."
  [page-id]
  (str "Tolglow launcher " (wire-id page-id)))

(defn pads
  "The launcher for a page under a Live bridge's settings (:mappings and
  :pad-layout, as live-sync-config normalizes them). Returns
    {:page-id :layout :scroll-row
     :pads [{:cell [x y] :note n :cue-id id|nil :label :live-color :mode
             :page-cue-id id|nil}]          ; 64 pads, top row first
     :elsewhere [{:cue-id :cell :note}]     ; a page cue mapped to another note
     :unmapped [{:cue-id :cell}]            ; a page cue with no enabled pad
     :off-grid [{:note :cue-id}]}           ; enabled notes outside the 64 pads
  A pad's :cue-id is what its note launches; :page-cue-id is what the page
  shows in that cell. They differ only for manual or older mappings."
  [live registry page]
  (let [layout (get live :pad-layout :push-64-pads)
        mappings (into {} (remove (comp :disabled? val)) (:mappings live))
        presented (into {} (map (juxt :note identity)) (ableton/mapped-pads registry mappings))
        cells (into {} (for [[xy cue] (:cells page)] [xy cue]))
        note-of-cue (fn [cue] (first (sort (for [[note m] mappings :when (= (wire-id cue) (wire-id (:cue-id m)))] note))))
        grid (for [y (range 8) x (range 8)]
               (let [note (ableton/pad-note layout [x y])
                     m (get mappings note)
                     shown (get presented note)]
                 (cond-> {:cell [x y] :note note :cue-id (:cue-id m) :page-cue-id (get cells [x y])}
                   m (assoc :mode (get m :mode :persistent))
                   ;; mapped-pads labels an unnamed cue with (str id); show its name.
                   shown (assoc :label (if (= (:label shown) (str (:cue-id m))) (wire-id (:cue-id m)) (:label shown))
                                :live-color (:live-color shown)))))
        grid-notes (set (map :note grid))]
    {:page-id (:id page) :layout layout :scroll-row ableton/push-64-pads-scroll-row
     :pads (vec grid)
     :elsewhere (vec (for [{:keys [cell page-cue-id note]} grid
                           :let [mapped (when page-cue-id (note-of-cue page-cue-id))]
                           :when (and mapped (not= mapped note))]
                       {:cue-id page-cue-id :cell cell :note mapped}))
     :unmapped (vec (for [{:keys [cell page-cue-id]} grid
                          :when (and page-cue-id (nil? (note-of-cue page-cue-id)))]
                      {:cue-id page-cue-id :cell cell}))
     :off-grid (vec (for [[note m] (sort-by key mappings) :when (not (grid-notes note))]
                      {:note note :cue-id (:cue-id m)}))}))

(defn desired
  "What the Set should hold for a launcher: its track name, the pad overview's
  scroll row and one named, coloured chain per pad that launches a cue."
  [{:keys [page-id scroll-row] :as launcher}]
  {:name (track-name page-id) :scroll-row scroll-row
   :pads (vec (for [{:keys [note cue-id label live-color]} (sort-by :note (:pads launcher)) :when cue-id]
                (let [n (str (or label (wire-id cue-id)))]
                  {:note note :name (subs n 0 (min 128 (count n))) :color (or live-color 0)})))})

(defn verify
  "Compare an :inspect-launcher read-back with the desired launcher. Live picks
  the nearest palette entry for a chain colour, so colours are reported
  (:color in each pad) but never a problem. Returns {:ready? :problems}."
  [read-back wanted]
  (let [rack (:rack read-back)
        by-note (into {} (map (juxt :note identity)) (:pads rack))
        problems (cond-> []
                   (not= (:name wanted) (:name read-back))
                   (conj {:code :not-launcher-track :expected (:name wanted) :actual (:name read-back)})
                   (nil? rack) (conj {:code :no-drum-rack})
                   (and rack (some? (:scroll-row wanted)) (not= (:scroll-row wanted) (:scroll-row rack)))
                   (conj {:code :scroll-row :expected (:scroll-row wanted) :actual (:scroll-row rack)}))
        problems (into problems
                       (when rack
                         (for [{:keys [note name]} (:pads wanted)
                               :let [actual (get by-note note)]
                               :when (not= name (:name actual))]
                           (if actual
                             {:code :pad-name :note note :expected name :actual (:name actual)}
                             {:code :empty-pad :note note :expected name}))))]
    {:ready? (empty? problems) :problems problems}))

(defn setup!
  "Bring the launcher track into the Set: :ensure-launcher on the companion's
  own track, then :write-pads, then read it back with :inspect-launcher and
  verify. Stops at the first receipt that is not executed and never retries;
  rerunning setup writes only what is missing. Returns
  {:ready? :problems :track-id :name :applied [[op status]] :skipped :launcher read-back}."
  [conn wanted]
  (let [receipt (live/request! conn :ensure-launcher (select-keys wanted [:name :scroll-row]))
        applied [[:ensure-launcher (:status receipt)]]
        track-id (get-in receipt [:result :track-id])
        pads (when (= :executed (:status receipt))
               (live/request! conn :write-pads {:track-id track-id :expected-name (:name wanted) :pads (:pads wanted)}))
        applied (cond-> applied pads (conj [:write-pads (:status pads)]))
        inspected (live/request! conn :inspect-launcher {})
        read-back (when (= :executed (:status inspected)) (:result inspected))
        verdict (if read-back
                  (verify read-back wanted)
                  {:ready? false :problems [{:code :not-inspected :receipt (select-keys inspected [:status :error])}]})
        refusal (some #(when-not (= :executed (:status %)) %) [receipt pads])]
    (cond-> (assoc verdict :name (:name wanted) :track-id (or track-id (:track-id read-back))
                   :applied applied :skipped (get-in pads [:result :skipped] []) :launcher read-back)
      refusal (-> (assoc :ready? false)
                  (update :problems conj {:code (or (get-in refusal [:error :code]) (:code refusal) (:status refusal))
                                          :status (:status refusal)
                                          :message (get-in refusal [:error :message])})))))

(defn- clip-range
  "Whole bars around the takes: from the bar holding the first start to the
  bar line at or after the last release."
  [takes beats-per-bar]
  (when (seq takes)
    (let [floor-bar #(* beats-per-bar (long (Math/floor (/ (double %) beats-per-bar))))
          ceil-bar #(* beats-per-bar (long (Math/ceil (/ (double %) beats-per-bar))))
          from (floor-bar (reduce min (map :start-beat takes)))
          to (ceil-bar (reduce max (map #(+ (:start-beat %) (runs/held-beats %)) takes)))]
      [from (if (> to from) to (+ from beats-per-bar))])))

(defn pad-clip
  "A Session clip of pad hits from the run record. Each released take of a cue
  that has a pad, starting in [from-beat to-beat), becomes one note at that
  pad's pitch: start relative to from-beat, length the take's held beats.
  Without a range the clip covers the whole bars around those takes.
  One pad plays one cue at a time, so a take that starts while an earlier
  take of the same cue still holds shortens the earlier note to end there
  (reported under :shortened). Notes past the clip end are cut at it.
  Returns {:clip clip :takes [take-id ..] :skipped [{:take-id :reason}]
  :shortened [take-id ..] :from-beat :to-beat}; the clip is nil when no take
  is in range. Running takes are skipped until they are released."
  ([record launcher] (pad-clip record launcher {}))
  ([record launcher {:keys [from-beat to-beat beats-per-bar] :or {beats-per-bar 4}}]
   (when-not (and (number? beats-per-bar) (pos? beats-per-bar)) (fail! "beats-per-bar must be positive" {}))
   (let [note-of (into {} (for [{:keys [note cue-id]} (:pads launcher) :when cue-id] [(wire-id cue-id) note]))
         candidates (filter #(and (or (nil? from-beat) (>= (:start-beat %) from-beat))
                                  (or (nil? to-beat) (< (:start-beat %) to-beat)))
                            (runs/takes record))
         skipped (vec (keep (fn [t] (cond (nil? (runs/held-beats t)) {:take-id (:take-id t) :reason :running}
                                          (nil? (note-of (wire-id (:cue-id t)))) {:take-id (:take-id t) :reason :no-pad}))
                            candidates))
         skip (set (map :take-id skipped))
         takes (sort-by (juxt :start-beat :take-id) (remove #(skip (:take-id %)) candidates))
         [auto-from auto-to] (clip-range takes beats-per-bar)
         from (or from-beat auto-from)
         to (or to-beat auto-to)]
     (if (empty? takes)
       {:clip nil :takes [] :skipped skipped :shortened [] :from-beat from :to-beat to}
       (let [length (- to from)
             _ (when-not (pos? length) (fail! "A pad clip needs to-beat after from-beat" {:from from :to to}))
             notes (mapv (fn [t] {:take-id (:take-id t) :pitch (note-of (wire-id (:cue-id t)))
                                  :start (- (:start-beat t) from)
                                  :duration (max runs/min-clip-beats (runs/held-beats t))
                                  :velocity 127})
                         takes)
             ;; One pad, one held cue: a later hit on the same pad ends the earlier note.
             [notes shortened] (reduce (fn [[out shortened] n]
                                         (let [i (last (keep-indexed (fn [i o] (when (= (:pitch o) (:pitch n)) i)) out))
                                               prev (when i (nth out i))]
                                           (if (and prev (> (+ (:start prev) (:duration prev)) (:start n)))
                                             [(conj (assoc-in out [i :duration] (max runs/min-clip-beats (- (:start n) (:start prev)))) n)
                                              (conj shortened (:take-id prev))]
                                             [(conj out n) shortened])))
                                       [[] []] notes)
             notes (mapv (fn [n] (-> n (update :duration #(min % (- length (:start n)))) (dissoc :take-id))) notes)]
         {:clip {:clip/version 1 :length-beats length :meter {:beats-per-bar beats-per-bar :bars-per-phrase 4}
                 :notes notes :controllers []}
          :takes (mapv :take-id takes) :skipped skipped :shortened shortened :from-beat from :to-beat to})))))

(defn write-clip!
  "Write a pad clip to the launcher track (setup! result) in its first empty
  slot, adding a scene when the track is full. options: :name (default
  \"Tolglow pads\"), :slot-index, :looping? (Live's default when omitted).
  Returns the :write-clip receipt; never retried."
  ([conn setup clip] (write-clip! conn setup clip {}))
  ([conn {:keys [track-id name ready?] :as setup} clip {:keys [slot-index looping?] :as options}]
   (when-not (and ready? track-id) (fail! "The launcher is not set up" {:problems (:problems setup)}))
   (when-not (map? clip) (fail! "No pad clip to write" {}))
   (live/request! conn :write-clip (cond-> {:track-id track-id :expected-name name :slot-index slot-index
                                            :name (get options :name "Tolglow pads") :clip clip :lanes []
                                            :create-scene? (nil? slot-index)}
                                     (some? looping?) (assoc :looping? looping?)))))
