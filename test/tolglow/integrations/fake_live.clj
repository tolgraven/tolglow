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
  `:create-scene? true` on write-clip adds a scene when every slot is full.

  Cue launcher (#228): the companion's track can hold a Drum Rack
  (`{:drum-rack {36 \"Kick\"}}` on a track spec: one chain per named pad).
  `:ensure-launcher` adopts the companion's track like `:ensure-track :host?`
  and inserts a Drum Rack when none follows the companion, and `:write-pads`
  inserts a chain for an empty pad; both need Live 12.3's insert calls, which
  `:device-insert? false` takes away (an older Live). Chain colours are kept
  exactly; real Live picks the nearest palette entry. `record-input!` and
  `session-record!` stand in for Live recording a performer's MIDI into a
  Session clip on an armed track, which Tolglow never does itself.

  Parameter devices (#625): `add-parameter-device!` drops a Tolglow
  parameters device on a track, as a performer or template must (the LOM
  inserts native devices only). It has 32 slot parameters after Live's
  Device On parameter. `:configure-parameter-device` applies names and ranges as the
  device's own patcher is assumed to; `:device-applies? false` models a Live
  that ignores the run-time attributes, so the read-back stays unchanged.
  `:relay? false` models a companion without the relay. Tolglow's
  `:set-parameter-values` writes are kept under `:parameter-writes`, and
  `:parameter-envelopes` on a run clip under that clip's `:envelopes`."
  (:require [clojure.data.json :as json]))

(defn create
  "tracks: existing musician tracks, e.g. [{:name \"Bass\" :arm? true}]."
  ([] (create {}))
  ([{:keys [tracks tempo exclusive-arm? can-capture-midi? capture-bars companion? envelope-lanes? scenes
            insert-joins-group? device-insert? relay? device-applies?]
     :or {tracks [] tempo 120.0 exclusive-arm? false can-capture-midi? true capture-bars 4 companion? true
          envelope-lanes? false insert-joins-group? true device-insert? true relay? true device-applies? true}}]
   (atom {:tracks (vec (map-indexed (fn [i t] (cond-> (merge {:track-id (str "track:" (inc i)) :kind :midi :arm? false
                                                              :monitor :auto :input {:port "Keyboard" :channel 1}
                                                              :slots {} :history []} (dissoc t :drum-rack))
                                                  (:drum-rack t) (assoc :rack {:scroll-row 9
                                                                               :chains (into (sorted-map) (for [[n name] (:drum-rack t)]
                                                                                                            [n {:name name :color 0x777777}]))})))
                                    (cond-> (vec tracks) companion? (conj {:name "MIDI" :companion? true :arm? false
                                                                           :input {:port "All Ins" :channel nil}}))))
          :next-id 100 :tempo tempo :signature {:beats-per-bar 4 :bars-per-phrase 4}
          :exclusive-arm? exclusive-arm? :can-capture-midi? can-capture-midi? :capture-bars capture-bars
          :envelope-lanes? envelope-lanes? :scenes scenes :insert-joins-group? insert-joins-group?
          :device-insert? device-insert? :relay? relay? :device-applies? device-applies? :parameter-writes []
          :watched [] :song-beat 0 :playing {} :selected nil :faults {} :requests []
          ;; The companion's surface mode (handoff), the capture host it adopted
          ;; in this session, and callbacks it would send (companion-events!).
          :mode :lighting :capture-host nil :outbox []})))

(defn- next-id! [live prefix]
  (str prefix ":" (:next-id (swap! live update :next-id inc))))

(defn- track-index [state track-id]
  (first (keep-indexed (fn [i t] (when (= track-id (:track-id t)) i)) (:tracks state))))

(defn- public-track [track]
  (-> (if (= :group (:kind track))
        (assoc (select-keys track [:track-id :name :kind]) :arm? false :monitor nil :input nil)
        (select-keys track [:track-id :name :kind :arm? :monitor :input :companion?]))
      (assoc :group-track-id (:group-track-id track))
      (assoc :arrangement-clips (vec (for [{:keys [clip-id start length]} (:arrangement track)]
                                       {:name (get-in (first (filter #(= clip-id (:clip-id %)) (vals (:slots track)))) [:name])
                                        :start-beat start :end-beat (+ start length)})))
      (assoc :clips (vec (for [[slot c] (sort-by key (:slots track))]
                           {:clip-id (:clip-id c) :slot-index slot :name (:name c)
                            :length-beats (get-in c [:clip :length-beats])
                            :envelopes? (boolean (seq (:envelopes c)))})))
      (cond-> (and (string? (:name track)) (re-find #"^Tolglow(\s|$)" (:name track)))
        (assoc :parameter-devices (vec (map-indexed (fn [i d] (assoc d :device-index (inc i))) (:parameter-devices track)))))))

(def parameter-slots 32)

(defn add-parameter-device!
  "Drop a \"Tolglow parameters\" device (ordinal 0, or n for \"Tolglow
  parameters n+1\") on the named track. Returns its device ID."
  ([live track-name] (add-parameter-device! live track-name 0))
  ([live track-name ordinal]
   (let [id (str "device:" (:next-id (swap! live update :next-id inc)))
         device {:device-id id :ordinal ordinal
                 :name (if (zero? ordinal) "Tolglow parameters" (str "Tolglow parameters " (inc ordinal)))
                 :parameters (vec (for [i (range parameter-slots)]
                                    {:index i :name (str "slot " (inc i)) :min 0.0 :max 1.0 :quantized? false
                                     :value 0.0 :automation-state 0}))}]
     (swap! live update :tracks (fn [ts] (mapv #(if (= track-name (:name %)) (update % :parameter-devices (fnil conj []) device) %) ts)))
     id)))

(defn parameter-device
  "The parameter device with this ordinal on the named track."
  [live track-name ordinal]
  (some #(when (= ordinal (:ordinal %)) %) (:parameter-devices (first (filter #(= track-name (:name %)) (:tracks @live))))))

(defn- device-on-track [track device-id]
  (first (keep-indexed (fn [i d] (when (= device-id (:device-id d)) i)) (:parameter-devices track))))

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
                           ;; Live keeps each note's played velocity.
                           :note-on (if (pos? velocity) (update-in acc [:open note] (fnil conj []) [(rel m) velocity])
                                        (recur acc (assoc m :type :note-off)))
                           :note-off (if-let [[s v] (first (get open note))]
                                       (-> acc (update-in [:open note] subvec 1)
                                           (update :notes conj {:pitch note :start s :duration (- (rel m) s) :velocity v}))
                                       acc)
                           :cc (update acc :controllers conj {:beat (rel m) :controller controller :value value})
                           acc))
                       {:open {} :notes [] :controllers []} in-window)
        held (for [[pitch starts] (:open result), [s v] starts]
               {:pitch pitch :start s :duration (- (- end start) s) :velocity v})]
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
      :inspect-set (do
                     ;; Like the companion: setup's capture host name lets a
                     ;; reloaded device know its track again, writing nothing.
                     (let [companion (first (filter :companion? (:tracks state)))]
                       (when (and companion (= (:capture-host-name payload) (:name companion)))
                         (swap! live assoc :capture-host (:track-id companion))))
                     {:status :executed
                      :result {:tracks (mapv public-track (:tracks state)) :tempo (:tempo state)
                               :signature (:signature state) :can-capture-midi? (:can-capture-midi? state)
                               :live-version "simulated"}})
      :ensure-track (if (:host? payload)
                      ;; Like the companion: the host is wherever the device is now. It
                      ;; is adopted only when it already has this exact name or is an
                      ;; empty track without a Tolglow name; another Tolglow track (the
                      ;; launcher, a run track) has another role and is refused.
                      (if-let [i (first (keep-indexed (fn [i t] (when (:companion? t) i)) (:tracks state)))]
                        (let [t (get-in state [:tracks i])
                              adopt! #(swap! live assoc :capture-host (:track-id t))]
                          (cond
                            (= (:name payload) (:name t)) (do (adopt!) {:status :executed :result {:track-id (:track-id t) :created? false}})
                            (re-find #"^Tolglow(\s|$)" (str (:name t))) (rejected :tolglow-track-in-use)
                            (not (and (empty? (:slots t)) (empty? (:arrangement t)) (not (:instrument? t)) (nil? (:rack t))))
                            (rejected :host-track-in-use)
                            :else (do (swap! live assoc-in [:tracks i :name] (:name payload)) (adopt!)
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
      :ensure-launcher
      (let [i (first (keep-indexed (fn [i t] (when (:companion? t) i)) (:tracks state)))
            t (when i (get-in state [:tracks i]))
            name (:name payload)]
        (cond
          (nil? i) (rejected :no-companion)
          (not (re-find #"^Tolglow launcher(\s|$)" (str name))) (rejected :track-name)
          ;; Like the companion: never another Tolglow track (Capture's cue track).
          (and (not (re-find #"^Tolglow launcher(\s|$)" (str (:name t)))) (re-find #"^Tolglow(\s|$)" (str (:name t))))
          (rejected :tolglow-track-in-use)
          (not (or (re-find #"^Tolglow launcher(\s|$)" (str (:name t)))
                   (and (empty? (:slots t)) (empty? (:arrangement t)) (not (:instrument? t)))))
          (rejected :host-track-in-use)
          (and (nil? (:rack t)) (not (:device-insert? state))) (rejected :drum-rack-creation-unavailable)
          :else (let [created? (nil? (:rack t))
                      renamed? (not= name (:name t))]
                  (swap! live update-in [:tracks i]
                         (fn [t] (cond-> (assoc t :name name)
                                   created? (assoc :rack {:scroll-row 9 :chains (sorted-map)})
                                   (:scroll-row payload) (assoc-in [:rack :scroll-row] (:scroll-row payload)))))
                  {:status :executed :result (cond-> {:track-id (:track-id t) :device-index 1}
                                               created? (assoc :rack-created? true)
                                               renamed? (assoc :renamed? true))})))
      :write-pads
      (let [[i code] (check-track state payload)
            t (when i (get-in state [:tracks i]))]
        (cond
          code (rejected code)
          (not (:companion? t)) (rejected :stale-track)
          (nil? (:rack t)) (rejected :no-drum-rack)
          :else (let [results (for [{:keys [note name color]} (:pads payload)
                                    :let [exists? (contains? (get-in t [:rack :chains]) note)]]
                                (cond exists? {:pad {:note note :name name :color color :chain-created? false}}
                                      (:device-insert? state) {:pad {:note note :name name :color color :chain-created? true}}
                                      :else {:skipped {:note note :code :chain-creation-unavailable}}))]
                  (swap! live update-in [:tracks i :rack :chains]
                         (fn [chains] (reduce (fn [c {:keys [note name color]}] (assoc c note {:name name :color color}))
                                              chains (keep :pad results))))
                  {:status :executed :result {:pads (vec (keep :pad results)) :skipped (vec (keep :skipped results))
                                              :color-policy "nearest-live-palette"}})))
      :inspect-launcher
      (if-let [t (first (filter :companion? (:tracks state)))]
        {:status :executed
         :result {:track-id (:track-id t) :name (:name t) :can-insert? (:device-insert? state)
                  :input (:input t) :routed-from-track? (boolean (:routed-from-track? t))
                  :arm? (:arm? t) :monitor (:monitor t)
                  :rack (when-let [rack (:rack t)]
                          {:device-index 1 :scroll-row (:scroll-row rack)
                           :pads (vec (for [[note {:keys [name color]}] (:chains rack)]
                                        {:note note :name name :color color :chains 1}))})}}
        (rejected :no-companion))
      :ensure-group (let [found (filter #(and (= (:name payload) (:name %)) (= :group (:kind %))) (:tracks state))]
                      (case (count found)
                        0 (rejected :group-creation-unavailable)
                        1 {:status :executed :result {:track-id (:track-id (first found)) :created? false}}
                        (rejected :duplicate-tracks)))
      :watch-run-tracks (do (swap! live assoc :watched (vec (:track-ids payload)))
                            {:status :executed
                             :result {:watching (count (:track-ids payload))
                                      :watching-slots (* parameter-slots
                                                         (reduce + (for [t (:tracks state) :when (some #{(:track-id t)} (:track-ids payload))]
                                                                     (count (:parameter-devices t)))))}})
      :configure-parameter-device
      (let [[i code] (check-track state payload)
            d (when i (device-on-track (get-in state [:tracks i]) (:device-id payload)))]
        (cond
          code (rejected code)
          (nil? d) (rejected :stale-device)
          (not= (:ordinal payload) (get-in state [:tracks i :parameter-devices d :ordinal])) (rejected :device-renamed)
          (not (:relay? state)) (rejected :relay-unavailable)
          :else (do (when (:device-applies? state)
                      (swap! live update-in [:tracks i :parameter-devices d :parameters]
                             (fn [ps] (reduce (fn [ps {:keys [index name type min max]}]
                                                (update ps index #(assoc % :name name :min (double min) :max (double max)
                                                                         :quantized? (not= :float (keyword type))
                                                                         :value (clojure.core/max (double min) (clojure.core/min (double max) (:value %))))))
                                              ps (:slots payload)))))
                    {:status :executed :result {:device-id (:device-id payload) :relayed (count (:slots payload))}})))
      :set-parameter-values
      (let [[i code] (check-track state payload)
            devices (when i (into {} (map-indexed (fn [n d] [(:ordinal d) n])) (get-in state [:tracks i :parameter-devices])))]
        (cond
          code (rejected code)
          (some #(not (contains? devices (:device %))) (:values payload)) (rejected :parameter-device-missing)
          :else (do (swap! live (fn [s]
                                  (reduce (fn [s {:keys [device index value]}]
                                            (-> s
                                                (update-in [:tracks i :parameter-devices (devices device) :parameters index]
                                                           (fn [p] (assoc p :value (clojure.core/max (:min p) (clojure.core/min (:max p) (double value)))
                                                                          :automation-state (if (= 1 (:automation-state p)) 2 (:automation-state p)))))
                                                (update :parameter-writes conj {:track-id (:track-id payload) :device device :index index :value value})))
                                          s (:values payload))))
                    {:status :executed :result {:written (count (:values payload))}})))
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
                      (some #(nil? (device-on-track (get-in state [:tracks i]) (:device-id %))) (:parameter-envelopes payload))
                      (rejected :stale-device)
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
                                     (cond-> {:clip-id id :name (:name payload) :looping? (get payload :looping? true)
                                              :clip (with-lanes (:clip payload) (:lanes payload) nil)}
                                       (seq (:parameter-envelopes payload))
                                       (assoc :envelopes (into {} (map (fn [{:keys [device-id index points]}] [[device-id index] (vec points)]))
                                                               (:parameter-envelopes payload)))))
                              {:status :executed :result {:clip-id id :slot-index slot :scene-created? (boolean full?)}})))))
      :place-clip (let [[i code] (check-track state payload)
                        found (find-clip state (:clip-id payload))
                        length (get-in found [:entry :clip :length-beats])]
                    (cond
                      code (rejected code)
                      (or (nil? found) (not= i (:track-index found))) (rejected :stale-clip)
                      (overlaps? (get-in state [:tracks i :arrangement]) (:arrangement-beat payload) length) (rejected :placement-overlap)
                      :else (do (swap! live update-in [:tracks i :arrangement] (fnil conj [])
                                       {:clip-id (:clip-id payload) :start (:arrangement-beat payload) :length length})
                                {:status :executed :result {:arrangement-beat (:arrangement-beat payload)}})))
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
      :stop-clips (do (swap! live (fn [s]
                                    (cond-> (update s :playing #(apply dissoc % (:track-ids payload)))
                                      ;; Like the companion's host watch: the capture host stopping.
                                      (and (:capture-host s) (contains? (:playing s) (:capture-host s))
                                           (some #{(:capture-host s)} (:track-ids payload)))
                                      (update :outbox conj {:kind "run-clip"
                                                            :event {:type "clip-stopped" :track-id (:capture-host s) :capture? true}}))))
                      {:status :executed :result {}})
      ;; Bridge operations, not Set ones: the status poll's capture fields
      ;; and the companion's surface mode.
      :inspect (let [host (:capture-host state)
                     companion (first (filter :companion? (:tracks state)))]
                 {:status :executed
                  :result {:capture-host (when (= host (:track-id companion)) host)
                           :capture-playing? (boolean (and host (= host (:track-id companion)) (contains? (:playing state) host)))}})
      :handoff (if (#{:live :lighting "live" "lighting"} (:mode payload))
                 (let [mode (keyword (name (:mode payload)))]
                   (swap! live assoc :mode mode)
                   {:status :executed :result {:mode (name mode) :midi-thru? (= :live mode)}})
                 (rejected :mode)))))

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

(defn companion-midi
  "What the companion forwards for one MIDI event its track received (as
  Adapter.midi does): only its own track's MIDI, never the feed channel.
  In lighting mode everything else, marked live-playback while a clip plays
  on the track and live-input otherwise. In Live mode only playback from the
  capture host it adopted, while a clip plays there. Returns the event to
  forward (with :track-id, :track-name and :origin) or nil."
  [live {:keys [track-id] :as event}]
  (let [state @live
        host (first (filter :companion? (:tracks state)))
        playing? (contains? (:playing state) (:track-id host))
        feed (get-in host [:input :channel])
        tagged (assoc event :track-id (:track-id host) :track-name (:name host)
                      :origin (if playing? :live-playback :live-input))]
    (when (and host (or (nil? track-id) (= track-id (:track-id host))) (not= feed (:channel event)))
      (case (:mode state)
        :lighting tagged
        :live (when (and playing? (= (:capture-host state) (:track-id host)))
                (assoc tagged :surface-mode :live))
        nil))))

(defn companion-events!
  "Callbacks the companion would have sent since the last call (clip stops
  on the capture host), and forget them."
  [live]
  (:outbox (first (swap-vals! live assoc :outbox []))))

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

(defn record-input!
  "MIDI a performer plays on a track at the current song beat: an armed track
  keeps it, as Live does while recording. messages: note/cc maps."
  [live track-id messages]
  (swap! live (fn [s] (update s :tracks (fn [ts] (mapv #(if (and (= track-id (:track-id %)) (:arm? %))
                                                             (update % :history into (map (fn [m] (assoc m :song-beat (:song-beat s))) messages))
                                                             %)
                                                          ts))))))

(defn session-record!
  "Live recording what an armed track received in song beats [start end) into
  a new Session clip in its first empty slot. Returns the clip ID."
  [live track-id start end]
  (let [i (track-index @live track-id)
        id (next-id! live "clip")]
    (swap! live (fn [s] (let [t (get-in s [:tracks i])]
                          (assoc-in s [:tracks i :slots (first-empty-slot t)]
                                    {:clip-id id :name "Recorded" :looping? true :clip (history->clip (:history t) start end)}))))
    id))

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
                     looping? (get entry :looping? true)
                     ;; Live clips keep no channel: playback reaches the companion on
                     ;; channel 1, tagged with the track that played it.
                     channel 1
                     {:keys [length-beats notes controllers]} (:clip entry)]
               ;; A clip with looping off (Clip.looping 0) plays once.
               loop (if looping? (range (long (Math/floor (/ (- from start) length-beats)))
                                        (inc (long (Math/ceil (/ (- to start) length-beats)))))
                        [0])
               :let [base (+ start (* loop length-beats))]
               event (concat (for [{:keys [pitch start duration velocity]} notes]
                               [{:type :note-on :note pitch :velocity (or velocity 127) :song-beat (+ base start)}
                                {:type :note-off :note pitch :velocity 0 :song-beat (+ base start duration)}])
                             (for [{:keys [beat controller value]} controllers]
                               [{:type :cc :controller controller :value value :song-beat (+ base beat)}]))
               e event
               :when (and (<= from (:song-beat e)) (< (:song-beat e) to))]
           (assoc e :channel channel :track-id track-id))
         (sort-by :song-beat)
         vec)))
