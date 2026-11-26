(ns tolglow.integrations.fake-live
  "A simulated Live Set behind the live-connection transport boundary.

  This stands in for the Max for Live companion and Live itself. It keeps
  tracks, clip slots, arm/monitor/input state, the MIDI each monitored track
  received (Capture MIDI history) and clip playback on a song-beat timeline.
  It follows the documented Live Object Model where it is known and makes no
  claim about undocumented behaviour: Capture MIDI here takes whole bars ending
  at the nearest bar line, which real Live decides heuristically. Faults can
  be injected per operation to exercise lost acknowledgements.")

(defn create
  "tracks: existing musician tracks, e.g. [{:name \"Bass\" :arm? true}]."
  ([] (create {}))
  ([{:keys [tracks tempo exclusive-arm? can-capture-midi? capture-bars]
     :or {tracks [] tempo 120.0 exclusive-arm? false can-capture-midi? true capture-bars 4}}]
   (atom {:tracks (vec (map-indexed (fn [i t] (merge {:track-id (str "track:" (inc i)) :kind :midi :arm? false
                                                      :monitor :auto :input {:port "Keyboard" :channel 1}
                                                      :slots {} :history []} t))
                                    tracks))
          :next-id 100 :tempo tempo :signature {:beats-per-bar 4 :bars-per-phrase 4}
          :exclusive-arm? exclusive-arm? :can-capture-midi? can-capture-midi? :capture-bars capture-bars
          :song-beat 0 :playing {} :selected nil :faults {} :requests []})))

(defn- next-id! [live prefix]
  (str prefix ":" (:next-id (swap! live update :next-id inc))))

(defn- track-index [state track-id]
  (first (keep-indexed (fn [i t] (when (= track-id (:track-id t)) i)) (:tracks state))))

(defn- public-track [track]
  (-> (select-keys track [:track-id :name :kind :arm? :monitor :input])
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

(defn- execute [live op payload]
  (let [state @live]
    (case op
      :inspect-set {:status :executed
                    :result {:tracks (mapv public-track (:tracks state)) :tempo (:tempo state)
                             :signature (:signature state) :can-capture-midi? (:can-capture-midi? state)
                             :live-version "simulated"}}
      :ensure-track (if-let [t (first (filter #(and (= (:name payload) (:name %)) (= :midi (:kind %))) (:tracks state)))]
                      {:status :executed :result {:track-id (:track-id t) :created? false}}
                      (let [id (next-id! live "track")]
                        (swap! live update :tracks conj {:track-id id :name (:name payload) :kind :midi :arm? false
                                                         :monitor :auto :input {:port "All Ins" :channel nil}
                                                         :slots {} :history []})
                        {:status :executed :result {:track-id id :created? true}}))
      :configure-track (let [[i code] (check-track state payload)]
                         (if code (rejected code)
                             (do (swap! live (fn [s]
                                               (let [s (update-in s [:tracks i] merge (select-keys payload [:arm? :monitor :input]))]
                                                 ;; Exclusive Arm as a preference: arming one track disarms the rest.
                                                 (if (and (:exclusive-arm? s) (:arm? payload))
                                                   (update s :tracks (fn [ts] (vec (map-indexed #(if (= %1 i) %2 (assoc %2 :arm? false)) ts))))
                                                   s))))
                                 {:status :executed :result {:track (public-track (get-in @live [:tracks i]))}})))
      :write-clip (let [[i code] (check-track state payload)]
                    (if code (rejected code)
                        (let [slot (or (:slot-index payload) (first-empty-slot (get-in state [:tracks i])))]
                          (if (get-in state [:tracks i :slots slot]) (rejected :slot-occupied)
                              (let [id (next-id! live "clip")]
                                (swap! live assoc-in [:tracks i :slots slot] {:clip-id id :name (:name payload) :clip (:clip payload)})
                                {:status :executed :result {:clip-id id :slot-index slot}})))))
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

(defn transport
  "The live-connection transport for this simulated Set."
  [live]
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
                     channel (get-in state [:tracks track-index :input :channel])
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
