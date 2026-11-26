(ns tolglow.integrations.fake-live
  "A simulated Live Set behind the live-connection transport boundary.

  This stands in for the Max for Live companion and Live itself. It keeps
  tracks, clip slots, arm/monitor/input state, the MIDI each monitored track
  received (Capture MIDI history) and clip playback on a song-beat timeline.
  It follows the documented Live Object Model where it is known and makes no
  claim about undocumented behaviour: Capture MIDI here takes whole bars ending
  at the nearest bar line, which real Live decides heuristically. Faults can
  be injected per operation to exercise lost acknowledgements.

  Like the real companion, it sits on one track (appended as \"MIDI\" unless
  :companion? is false); setup adopts that track for the host capture track.
  `(transport live {:json? true})` sends every payload and receipt through JSON,
  as the loopback HTTP companion does.
  Writing controller events with write-clip is refused unless the Set is
  created with :envelope-lanes? true, which models the companion's lane
  parameters on every capture track (the real companion has them only on its
  own track; see live-companion-contract-test).
  `json-transport` is the same as `(transport live {:json? true})` with the
  receipt read exactly as live-transport reads it.

  Group Tracks (#683): the documented Live Object Model reads membership
  (`Track.group_track`, `is_foldable`) but has no call that creates a Group
  Track or moves a track into one. Like the companion, `:ensure-group` here
  only finds a Group Track a performer or template placed (`add-group-track!`)
  and refuses with :group-creation-unavailable otherwise. `:ensure-track` with
  a :group-track-id inserts the new track after the group's last direct member
  that is not a group (or right after the Group Track); that the inserted
  track joins the group is an assumption to verify in Live, and
  `:insert-joins-group? false` models the opposite outcome.
  `:scenes n` bounds the clip slots per track (default unbounded);
  `:create-scene? true` on write-clip adds a scene when every slot is full."
  (:require [clojure.data.json :as json]))

(defn create
  "tracks: existing musician tracks, e.g. [{:name \"Bass\" :arm? true}]."
  ([] (create {}))
  ([{:keys [tracks tempo exclusive-arm? can-capture-midi? capture-bars companion? envelope-lanes? scenes
            insert-joins-group?]
     :or {tracks [] tempo 120.0 exclusive-arm? false can-capture-midi? true capture-bars 4 companion? true
          envelope-lanes? false insert-joins-group? true}}]
   (atom {:tracks (vec (map-indexed (fn [i t] (merge {:track-id (str "track:" (inc i)) :kind :midi :arm? false
                                                      :monitor :auto :input {:port "Keyboard" :channel 1}
                                                      :slots {} :history []} t))
                                    (cond-> (vec tracks) companion? (conj {:name "MIDI" :companion? true :arm? false
                                                                           :input {:port "All Ins" :channel nil}}))))
          :next-id 100 :tempo tempo :signature {:beats-per-bar 4 :bars-per-phrase 4}
          :exclusive-arm? exclusive-arm? :can-capture-midi? can-capture-midi? :capture-bars capture-bars
          :envelope-lanes? envelope-lanes? :scenes scenes :insert-joins-group? insert-joins-group?
          :watched [] :song-beat 0 :playing {} :selected nil :faults {} :requests []})))

(defn- next-id! [live prefix]
  (str prefix ":" (:next-id (swap! live update :next-id inc))))

(defn- track-index [state track-id]
  (first (keep-indexed (fn [i t] (when (= track-id (:track-id t)) i)) (:tracks state))))

(defn- public-track [track]
  (-> (if (= :group (:kind track))
        (assoc (select-keys track [:track-id :name :kind]) :arm? false :monitor nil :input nil)
        (select-keys track [:track-id :name :kind :arm? :monitor :input :companion?]))
      (assoc :group-track-id (:group-track-id track))
      (assoc :clips (vec (for [[slot c] (sort-by key (:slots track))]
                           {:clip-id (:clip-id c) :slot-index slot :name (:name c)
                            :length-beats (get-in c [:clip :length-beats])})))))

(defn- find-clip [state clip-id]
  (first (for [[ti t] (map-indexed vector (:tracks state)), [slot c] (:slots t) :when (= clip-id (:clip-id c))]
           {:track-index ti :slot slot :entry c})))

(defn- rejected [code] {:status :rejected :code code})

(defn- check-track [state {:keys [track-id expected-name]}]
  (let [i (track-index state track-id)]
    (cond (nil? i) [nil :stale-track]
          (and expected-name (not= expected-name (get-in state [:tracks i :name]))) [nil :track-renamed]
          :else [i nil])))

(defn- first-empty-slot [track] (first (remove (set (keys (:slots track))) (range))))

(defn- insert-index
  "Like the companion: after the group's last direct member that is not a
  group itself, or right after the Group Track."
  [state group-id]
  (inc (last (keep-indexed (fn [i t] (when (or (= group-id (:track-id t))
                                               (and (= group-id (:group-track-id t)) (not= :group (:kind t))))
                                       i))
                           (:tracks state)))))

(defn- overlaps? [arrangement start length]
  (some (fn [{s :start l :length}] (and (< start (+ s l)) (< s (+ start length)))) arrangement))

(defn- history->clip
  "Build a clip from received MIDI in [start end), as Capture MIDI would."
  [history start end]
  (let [in-window (filter #(and (<= start (:song-beat %)) (< (:song-beat %) end)) history)
        rel #(- (rationalize (:song-beat %)) start)
        result (reduce (fn [{:keys [open notes controllers] :as acc} {:keys [type note velocity controller value] :as m}]
                         (case type
                           :note-on (if (pos? velocity) (update-in acc [:open note] (fnil conj []) (rel m))
                                        (recur acc (assoc m :type :note-off)))
                           :note-off (if-let [s (first (get open note))]
                                       (-> acc (update-in [:open note] subvec 1)
                                           (update :notes conj {:pitch note :start s :duration (- (rel m) s) :velocity 127}))
                                       acc)
                           :cc (update acc :controllers conj {:beat (rel m) :controller controller :value value})
                           acc))
                       {:open {} :notes [] :controllers []} in-window)
        held (for [[pitch starts] (:open result), s starts]
               {:pitch pitch :start s :duration (- (- end start) s) :velocity 127})]
    {:clip/version 1 :length-beats (- end start) :meter {:beats-per-bar 4 :bars-per-phrase 4}
     :notes (vec (sort-by (juxt :start :pitch) (concat (:notes result) held)))
     :controllers (:controllers result)}))

(defn- with-lanes
  "Store compact lanes as the coarse/fine controller events a clip read returns."
  [clip lanes notes]
  (-> clip
      (update :notes #(into (vec %) notes))
      (update :controllers #(into (vec %) (for [{:keys [controller points]} lanes, [beat v14] points
                                                e [{:beat beat :controller controller :value (bit-shift-right v14 7)}
                                                   {:beat beat :controller (+ 32 controller) :value (bit-and v14 127)}]]
                                            e)))))

(defn move-companion!
  "Move the companion device to the track with this name."
  [live name]
  (swap! live update :tracks (fn [ts] (mapv #(assoc % :companion? (= name (:name %))) ts))))

(defn- execute [live op payload]
  (let [state @live]
    (case op
      :inspect-set {:status :executed
                    :result {:tracks (mapv public-track (:tracks state)) :tempo (:tempo state)
                             :signature (:signature state) :can-capture-midi? (:can-capture-midi? state)
                             :live-version "simulated"}}
      :ensure-track (if (:host? payload)
                      ;; Like the companion: the host is wherever the device is now, and
                      ;; only an empty or Tolglow-named track may be adopted.
                      (if-let [i (first (keep-indexed (fn [i t] (when (:companion? t) i)) (:tracks state)))]
                        (let [t (get-in state [:tracks i])]
                          (cond
                            (= (:name payload) (:name t)) {:status :executed :result {:track-id (:track-id t) :created? false}}
                            (not (or (re-find #"^Tolglow(\s|$)" (str (:name t))) (and (empty? (:slots t)) (not (:instrument? t)))))
                            (rejected :host-track-in-use)
                            :else (do (swap! live assoc-in [:tracks i :name] (:name payload))
                                      {:status :executed :result {:track-id (:track-id t) :created? false :adopted? true}})))
                        (rejected :no-companion))
                      (let [found (filter #(and (= (:name payload) (:name %)) (= :midi (:kind %))) (:tracks state))
                            group (:group-track-id payload)]
                        (cond
                          (> (count found) 1) (rejected :duplicate-tracks)
                          (= 1 (count found)) {:status :executed :result {:track-id (:track-id (first found)) :created? false
                                                                           :group-track-id (:group-track-id (first found))}}
                          (and group (not= :group (:kind (first (filter #(= group (:track-id %)) (:tracks state))))))
                          (rejected :not-a-group)
                          :else
                          (let [id (next-id! live "track")
                                joined (when (and group (:insert-joins-group? state)) group)
                                track {:track-id id :name (:name payload) :kind :midi :arm? false
                                       :monitor :auto :input {:port "All Ins" :channel nil}
                                       :slots {} :history [] :group-track-id joined}]
                            (swap! live update :tracks
                                   (fn [ts] (let [at (if group (insert-index {:tracks ts} group) (count ts))]
                                              (into (conj (subvec ts 0 at) track) (subvec ts at)))))
                            {:status :executed :result {:track-id id :created? true :group-track-id joined}}))))
      :ensure-group (let [found (filter #(and (= (:name payload) (:name %)) (= :group (:kind %))) (:tracks state))]
                      (case (count found)
                        0 (rejected :group-creation-unavailable)
                        1 {:status :executed :result {:track-id (:track-id (first found)) :created? false}}
                        (rejected :duplicate-tracks)))
      :watch-run-tracks (do (swap! live assoc :watched (vec (:track-ids payload)))
                            {:status :executed :result {:watching (count (:track-ids payload))}})
      :configure-track (let [[i code] (check-track state payload)]
                         (if code (rejected code)
                             (do (swap! live (fn [s]
                                               (let [s (update-in s [:tracks i] merge (cond-> (select-keys payload [:arm? :monitor :input])
                                                                                   (string? (:monitor payload)) (update :monitor keyword)))]
                                                 ;; Exclusive Arm as a preference: arming one track disarms the rest.
                                                 (if (and (:exclusive-arm? s) (:arm? payload))
                                                   (update s :tracks (fn [ts] (vec (map-indexed #(if (= %1 i) %2 (assoc %2 :arm? false)) ts))))
                                                   s))))
                                 {:status :executed :result {:track (public-track (get-in @live [:tracks i]))}})))
      :write-clip (let [[i code] (check-track state payload)]
                    (cond
                      code (rejected code)
                      ;; The LOM cannot write MIDI CC into a clip. Controllers become
                      ;; companion-parameter envelopes, which a test must opt into.
                      (and (or (seq (get-in payload [:clip :controllers])) (some (comp seq :points) (:lanes payload)))
                           (not (:envelope-lanes? state)))
                      (rejected :controllers-unsupported)
                      (and (:arrangement-beat payload)
                           (overlaps? (get-in state [:tracks i :arrangement]) (:arrangement-beat payload) (get-in payload [:clip :length-beats])))
                      (rejected :placement-overlap)
                      :else
                        (let [slot (or (:slot-index payload) (first-empty-slot (get-in state [:tracks i])))
                              scenes (:scenes state)
                              full? (and scenes (>= slot scenes))]
                          (cond
                            (get-in state [:tracks i :slots slot]) (rejected :slot-occupied)
                            (and full? (not (and (nil? (:slot-index payload)) (:create-scene? payload)))) (rejected :no-empty-slot)
                            :else
                            (let [id (next-id! live "clip")]
                              (when full? (swap! live update :scenes inc))
                              (swap! live assoc-in [:tracks i :slots slot]
                                     {:clip-id id :name (:name payload) :clip (with-lanes (:clip payload) (:lanes payload) nil)})
                              (when-let [beat (:arrangement-beat payload)]
                                (swap! live update-in [:tracks i :arrangement] (fnil conj [])
                                       {:clip-id id :start beat :length (get-in payload [:clip :length-beats])}))
                              {:status :executed :result (cond-> {:clip-id id :slot-index slot :scene-created? (boolean full?)}
                                                           (:arrangement-beat payload) (assoc :arrangement-beat (:arrangement-beat payload)))})))))
      :extend-clip (let [[i code] (check-track state payload)
                         found (find-clip state (:clip-id payload))]
                     (cond
                       code (rejected code)
                       (or (nil? found) (not= i (:track-index found))) (rejected :stale-clip)
                       (and (some (comp seq :points) (:lanes payload)) (not (:envelope-lanes? state))) (rejected :controllers-unsupported)
                       :else (do (swap! live update-in [:tracks i :slots (:slot found) :clip] with-lanes (:lanes payload) (:notes payload))
                                 {:status :executed :result {:notes (count (:notes payload))
                                                             :envelope-steps (reduce + 0 (map (comp count :points) (:lanes payload)))}})))
      :capture-midi (if-not (:can-capture-midi? state) (rejected :capture-unavailable)
                      (let [bpb (get-in state [:signature :beats-per-bar])
                            end (* bpb (Math/round (/ (double (:song-beat state)) bpb)))
                            start (- end (* bpb (:capture-bars state)))
                            clips (vec (for [[i t] (map-indexed vector (:tracks state))
                                             :when (and (or (:arm? t) (= :in (:monitor t)))
                                                        (some #(<= start (:song-beat %) end) (:history t)))]
                                         (let [id (next-id! live "clip") slot (first-empty-slot t)]
                                           (swap! live assoc-in [:tracks i :slots slot]
                                                  {:clip-id id :name "Captured" :clip (history->clip (:history t) start end)})
                                           {:clip-id id :track-id (:track-id t) :slot-index slot})))]
                        (if (seq clips) {:status :executed :result {:clips clips}} (rejected :nothing-to-capture))))
      :read-clip (if-let [{:keys [track-index entry]} (find-clip state (:clip-id payload))]
                   {:status :executed :result {:clip (:clip entry) :track-id (get-in state [:tracks track-index :track-id])}}
                   (rejected :stale-clip))
      :show-clip (if (find-clip state (:clip-id payload))
                   (do (swap! live assoc :selected (:clip-id payload))
                       {:status :executed :result {:clip-id (:clip-id payload) :view "Detail/Clip"}})
                   (rejected :stale-clip))
      :fire-clip (if-let [{:keys [track-index]} (find-clip state (:clip-id payload))]
                   (do (swap! live assoc-in [:playing (get-in state [:tracks track-index :track-id])]
                              {:clip-id (:clip-id payload) :from (:song-beat state)})
                       {:status :executed :result {:clip-id (:clip-id payload)}})
                   (rejected :stale-clip))
      :stop-clips (do (swap! live update :playing #(apply dissoc % (:track-ids payload)))
                      {:status :executed :result {}}))))

(defn- through-json [x] (json/read-str (json/write-str x) :key-fn keyword))

(defn- base-transport [live]
  {:open! (fn [_] {:live live})
   :request! (fn [_ id op payload]
               (swap! live update :requests conj [op payload])
               (let [fault (first (get-in @live [:faults op]))
                     _ (when fault (swap! live update-in [:faults op] rest))
                     receipt (case fault
                               :refuse {:status :rejected :code :injected}
                               :lose-ack (do (execute live op payload) {:status :indeterminate})
                               (execute live op payload))]
                 (assoc receipt :id id)))
   :close! (fn [_] nil)})

(defn transport
  "The live-connection transport for this simulated Set."
  ([live] (transport live {}))
  ([live {:keys [json?]}]
   (let [t (base-transport live)]
     (if json?
       (assoc t :request! (fn [handle id op payload]
                            (through-json ((:request! t) handle id op (through-json payload)))))
       t))))

(defn json-transport
  "transport through JSON both ways: the Set receives the payload as the
  companion would decode it, and the reply is read as live-transport reads it
  (keyword keys, keyword :status, every other value as JSON gave it)."
  [live]
  (let [inner (transport live)]
    (assoc inner :request!
           (fn [handle id op payload]
             (let [payload (json/read-str (json/write-str payload) :key-fn keyword)
                   receipt ((:request! inner) handle id op payload)]
               (update (json/read-str (json/write-str receipt) :key-fn keyword) :status keyword))))))

(defn inject-fault! [live op & faults] (swap! live update-in [:faults op] concat faults))

(defn set-song-beat! [live beat] (swap! live assoc :song-beat beat))

(defn midi-port
  "The MIDI port Tolglow feeds. Messages land in the Capture MIDI history of
  every armed or input-monitored track whose input is this port and channel."
  [live port]
  {:send! (fn [_ messages]
            (swap! live (fn [s]
                          (update s :tracks
                                  (fn [ts]
                                    (mapv (fn [t]
                                            (let [mine (filter #(and (= port (get-in t [:input :port]))
                                                                     (= (:channel %) (get-in t [:input :channel]))
                                                                     (or (:arm? t) (= :in (:monitor t))))
                                                               messages)]
                                              (update t :history into (map #(assoc % :song-beat (:song-beat s)) mine))))
                                          ts))))))})

(defn edit-clip!
  "Change a stored clip as a performer would in Live's clip editor."
  [live clip-id f]
  (let [{:keys [track-index slot]} (find-clip @live clip-id)]
    (swap! live update-in [:tracks track-index :slots slot :clip] f)))

(defn add-group-track!
  "Place a Group Track, as a performer or a packaged template would, at the
  end of the Set or inside parent-group-track-id. Returns its track ID."
  ([live name] (add-group-track! live name nil))
  ([live name parent]
   (let [id (next-id! live "track")]
     (swap! live update :tracks
            (fn [ts] (let [at (if parent (insert-index {:tracks ts} parent) (count ts))]
                       (into (conj (subvec ts 0 at) {:track-id id :name name :kind :group :slots {} :history []
                                                     :group-track-id parent})
                             (subvec ts at)))))
     id)))

(defn delete-clip! [live clip-id]
  (let [{:keys [track-index slot]} (find-clip @live clip-id)]
    (swap! live update-in [:tracks track-index :slots] dissoc slot)))

(defn rename-track! [live track-id name]
  (swap! live update :tracks (fn [ts] (mapv #(if (= track-id (:track-id %)) (assoc % :name name) %) ts))))

(defn delete-track! [live track-id]
  (swap! live update :tracks (fn [ts] (vec (remove #(= track-id (:track-id %)) ts)))))

(defn playback
  "MIDI that playing clips emit for song beats in [from to), each event
  carrying :song-beat and the playing track's input channel. Session clips
  loop from the beat they were fired."
  [live from to]
  (let [state @live]
    (->> (for [[track-id {:keys [clip-id] :as p}] (:playing state)
               :let [start (:from p)
                     {:keys [track-index entry]} (find-clip state clip-id)
                     ;; Live clips keep no channel: playback reaches the companion on
                     ;; channel 1, tagged with the track that played it.
                     channel 1
                     {:keys [length-beats notes controllers]} (:clip entry)]
               loop (range (long (Math/floor (/ (- from start) length-beats)))
                           (inc (long (Math/ceil (/ (- to start) length-beats)))))
               :let [base (+ start (* loop length-beats))]
               event (concat (for [{:keys [pitch start duration]} notes]
                               [{:type :note-on :note pitch :velocity 127 :song-beat (+ base start)}
                                {:type :note-off :note pitch :velocity 0 :song-beat (+ base start duration)}])
                             (for [{:keys [beat controller value]} controllers]
                               [{:type :cc :controller controller :value value :song-beat (+ base beat)}]))
               e event
               :when (and (<= from (:song-beat e)) (< (:song-beat e) to))]
           (assoc e :channel channel :track-id track-id))
         (sort-by :song-beat)
         vec)))
