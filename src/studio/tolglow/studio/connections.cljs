(ns tolglow.studio.connections
  "Explicit connection controls. Reading status never acquires a device; every
  start, stop, clock selection and Live handoff is a named re-frame event."
  (:require [clojure.string :as str]
            [afterglow.sync.peer :as pairing]
            [re-frame.core :as rf]
            [tolglow.studio.compute-client :as compute-client]
            [reagent.core :as r]
            [tolglow.audio.pcm :as pcm]
            [tolglow.studio.audience-invitation :as invitation]
            [tolglow.studio.patchbay :as patchbay]
            [tolglow.studio.sensors :as sensors]
            [tolglow.studio.audience :as audience]
            [tolglow.studio.lighting-capacity :as capacity]
            [tolglow.studio.fog-control :as fog-control]
            [tolglow.studio.audio-scope :as audio-scope]
            [tolglow.studio.audio-lfo :as audio-lfo]
            [tolglow.studio.stems :as stems]
            [tolglow.studio.events :as events]
            [tolglow.studio.projection :as projection]))

(def sync-defaults
  {:selected-source "manual"
   :midi {:enabled? false :input-index "" :output-index "" :control? false}
   :link {:enabled? false :port 17000 :follow-transport? true}
   :beat-link {:enabled? false :status? false :align-bars? false :peer nil}
   :live {:enabled? false :endpoint "http://127.0.0.1:8750/live" :bridge-id "tolglow-m4l" :record-route "bridge"
          :mode "lighting" :channel 1 :automap? true :observed-parameters {} :poll-ms 500 :timeout-ms 2000 :pairing nil
          :mappings {} :parameters {}}})

(def audio-defaults {:input "" :sample-rate "48000" :channels "1" :input-channel ""
                     :block-frames "480" :buffer-blocks "8" :big-endian? false :analyzer-options {} :stems []})

(def defaults {:visible? false :generation 0 :pending #{} :errors {} :sync nil :audio nil :section-reference nil
               :ports [] :inputs [] :sync-form sync-defaults :audio-form audio-defaults
               :sync-dirty? false :audio-dirty? false :notes [] :parameters []})

(defn state [db] (merge defaults (:connections db)))

(defn token [db] [(get-in db [:show :runtime-id]) (get-in db [:show :epoch]) (:generation (state db))])

(defn current? [db request-token] (= request-token (token db)))

(defn- req [db key path body success]
  [:studio/http (cond-> {:path path :on-success [success (token db) key]
                         :on-failure [:connections/failed (token db) key] :timeout-ms 6000}
                  (= key :configure) (assoc :raw-response? true)
                  (some? body) (assoc :body body))])

(defn- text [event] (.. event -target -value))

(defn- edit! [path value] (rf/dispatch-sync [:connections/edit path value]))

(defn- integer! [value label low high]
  (let [n (js/Number value)]
    (when-not (and (not (str/blank? (str value))) (js/Number.isInteger n) (<= low n high))
      (throw (js/Error. (str label " must be an integer from " low " to " high ".")))) n))

(defn- finite! [value label]
  (let [n (js/Number value)]
    (when-not (and (not (str/blank? (str value))) (js/Number.isFinite n))
      (throw (js/Error. (str label " must be a finite number.")))) n))

(defn- optional-port [ports value direction expected-name]
  (when-not (or (nil? value) (= "" value))
    (let [index (integer! value "MIDI port" 0 4096)
          port (some #(when (and (= index (:index %)) (get % direction)) %) ports)]
      (when-not port (throw (js/Error. "Refresh ports and choose an available MIDI port.")))
      (when (and expected-name (not= expected-name (:name port)))
        (throw (js/Error. "The selected MIDI port name changed. Select the device again."))) port)))

(defn- duration-parts [mapping]
  (if-let [[unit value] (first (:duration mapping))] [(str value) (name unit)]
    (if-let [ms (:duration-ms mapping)] [(str ms) "ms"] ["1" "beats"])))

(defn mapping-rows [config key]
  (mapv (fn [[control mapping]]
          (let [[value unit] (duration-parts mapping)]
            (merge mapping {:control (events/wire-id control) :duration-value value :duration-unit unit
                            :low (str (first (:bounds mapping [0 1]))) :high (str (second (:bounds mapping [0 1])))})))
        (sort-by (comp str first) (get-in config [:live key]))))

(defn parameter-rows [form typed-config]
  (let [values (into {} (keep (fn [[control mapping]] (when-let [choices (:values mapping)] [(events/wire-id control) choices])))
                     (get-in typed-config [:live :parameters]))]
    (mapv (fn [row] (if-let [choices (get values (:control row))] (assoc row :values choices) row))
          (mapping-rows form :parameters))))

(defn- mappings! [rows kind]
  (let [pairs (mapv (fn [row]
                      (let [control (integer! (:control row) "MIDI note/controller" 0 127)
                            cue-id (:cue-id row)]
                        (when (str/blank? cue-id) (throw (js/Error. "Choose a cue for every mapping.")))
                        [(str control)
                         (if (= kind :notes)
                           (let [mode (:mode row "one-shot") value (finite! (:duration-value row "1") "Mapping duration")]
                             (when-not (pos? value) (throw (js/Error. "Mapping duration must be positive.")))
                             (cond-> (merge (select-keys row [:disabled? :velocity-param]) {:cue-id cue-id :mode mode})
                               (contains? #{"one-shot" "timed"} mode)
                               (assoc :duration (events/musical-duration value (:duration-unit row "beats")))))
                           (let [low (finite! (:low row) "Lower bound") high (finite! (:high row) "Upper bound")]
                             (when (or (str/blank? (:param row)) (>= low high))
                               (throw (js/Error. "Choose a parameter and increasing bounds for each controller.")))
                             (cond-> (merge (select-keys row [:type :values :disabled?]) {:cue-id cue-id :param (:param row) :bounds [low high]})
                               (seq (str (:lane-note row)))
                               (assoc :lane-note (integer! (:lane-note row) "Parameter lane pitch" 0 127)))))])) rows)]
    (when-not (= (count pairs) (count (distinct (map first pairs)))) (throw (js/Error. "Each MIDI note/controller can have only one mapping.")))
    (into {} pairs)))

(defn sync-payload [db]
  (let [s (state db) form (:sync-form s)
        input (when (get-in form [:midi :enabled?])
                (optional-port (:ports s) (get-in form [:midi :input-index]) :input? (get-in form [:midi :expected-input-name])))
        output (when (get-in form [:midi :enabled?])
                 (optional-port (:ports s) (get-in form [:midi :output-index]) :output? (get-in form [:midi :expected-output-name])))
        midi (cond-> (reduce (fn [m key]
                                (if (or (nil? (get m key)) (= "" (get m key))) (dissoc m key)
                                  (update m key #(integer! % "MIDI port" 0 4096)))) (:midi form) [:input-index :output-index])
               input (assoc :input-index (:index input) :expected-input-name (:name input))
               output (assoc :output-index (:index output) :expected-output-name (:name output)))
        live (:live form) pairing (:pairing live)
        pairing (when pairing
                  (cond-> {:track-index (integer! (:track-index pairing 0) "Live track index" 0 9999)
                           (if (contains? pairing :arrangement-index) :arrangement-index :clip-slot-index)
                           (integer! (or (:arrangement-index pairing) (:clip-slot-index pairing) 0) "Live clip index" 0 9999)}
                    (seq (str (:rack-device-index pairing)))
                    (assoc :rack-device-index (integer! (:rack-device-index pairing) "Rack device index" 0 9999))
                    (seq (:expected-track-name pairing)) (assoc :expected-track-name (:expected-track-name pairing))
                    (seq (:expected-clip-name pairing)) (assoc :expected-clip-name (:expected-clip-name pairing))))]
    (when (and (:enabled? midi) (nil? input) (nil? output)) (throw (js/Error. "Choose at least one MIDI input or output before enabling it.")))
    (assoc form :midi midi :link (update (:link form) :port #(integer! % "Carabiner port" 1 65535))
           :live (assoc (dissoc live :paired?) :pairing pairing
                                      :channel (integer! (:channel live) "MIDI channel" 1 16)
                                      :observed-parameters (get-in s [:sync :config :live :observed-parameters] (:observed-parameters live {}))
                                      :poll-ms (integer! (:poll-ms live) "Live polling interval" 100 10000)
                                      :timeout-ms (integer! (:timeout-ms live) "Live request timeout" 50 10000)
                                      :mappings (mappings! (:notes s) :notes)
                                      :parameters (mappings! (:parameters s) :parameters)))))

(defn audio-payload [db]
  (let [s (state db) form (:audio-form s) selection (:input form)
        mixer (some #(when (= (str (:mixer-index %)) selection) %) (:inputs s))]
    (when-not (or (= "system" selection) mixer) (throw (js/Error. "Choose an audio input explicitly before starting capture.")))
    (when (and mixer (:expected-name form) (not= (:expected-name form) (:name mixer)))
      (throw (js/Error. "The audio input name changed. Select the device again.")))
    (let [channels (integer! (:channels form) "PCM channel count" 1 pcm/max-channels)
          input-channel (when-not (str/blank? (str (:input-channel form)))
                          (integer! (:input-channel form) "Input channel" 1 channels))]
      (when-not (pcm/valid-layout? channels input-channel)
        (throw (js/Error. "Choose an input channel for this PCM channel count.")))
      (cond-> (merge {:sample-rate (integer! (:sample-rate form) "Sample rate" 8000 192000)
                       :channels channels
                       :block-frames (integer! (:block-frames form) "Block size" 1 16384)
                       :buffer-blocks (integer! (:buffer-blocks form) "Buffer blocks" 2 64)
                       :big-endian? (:big-endian? form false) :analyzer-options (:analyzer-options form {})}
                      (if mixer {:mixer-index (:mixer-index mixer) :mixer-name (:name mixer)} {:default? true}))
        input-channel (assoc :input-channel input-channel)
        (seq (:stems form)) (assoc :stems (stems/payload (:stems form) channels))))))

(defn- audio-form [config]
  (merge audio-defaults (select-keys config [:sample-rate :channels :input-channel :block-frames :buffer-blocks :big-endian? :analyzer-options])
         {:expected-name (:mixer-name config) :stems (stems/form-rows config)
          :input (cond (:default? config) "system" (some? (:mixer-index config)) (str (:mixer-index config)) :else "")}))

(rf/reg-sub :connections/state (fn [db _] (state db)))
(rf/reg-event-fx :connections/open
  (fn [{:keys [db]} _]
    {:db (update db :connections #(cond-> (assoc (merge defaults %) :visible? true :devices-requested? false)
                                    (not (:sync-dirty? %)) (assoc :sync-form-loaded? false)
                                    (not (:audio-dirty? %)) (assoc :audio-form-loaded? false)))
     :studio/connections-interest true}))
(rf/reg-event-fx :connections/close
  (fn [{:keys [db]} _]
    {:db (update db :connections #(-> (merge defaults %) (assoc :visible? false :pending #{} :sync nil :audio nil :section-reference nil)
                                      (update :generation inc)))
     :studio/connections-interest false}))
(rf/reg-event-db :connections/runtime-change
  (fn [db _]
    (let [s (state db)]
      (assoc db :connections (assoc defaults :visible? (:visible? s) :generation (inc (:generation s)))))))
(rf/reg-event-db :connections/edit
  (fn [db [_ path value]]
    (cond-> (assoc-in db (into [:connections] path) value)
      (= :sync-form (first path)) (assoc-in [:connections :sync-dirty?] true)
      (= :audio-form (first path)) (assoc-in [:connections :audio-dirty?] true))))
(rf/reg-event-fx :connections/refresh-devices
  (fn [{:keys [db]} _]
    (when (:visible? (state db))
      {:fx [(req db :ports "/api/sync/ports" nil :connections/loaded)
            (req db :inputs "/api/audio/inputs" nil :connections/loaded)]})))
(rf/reg-event-fx :connections/pushed
  (fn [{:keys [db]} _]
    (let [s (state db)
          {:keys [sync audio section-reference]} (get-in db [:show :connection-status])
          config (:config sync)
          initialize-sync? (and config (not (:sync-dirty? s))
                                (or (not (:sync-form-loaded? s)) (not= config (get-in s [:sync :config]))))
          initialize-audio? (and (:config audio) (not (:audio-dirty? s))
                                 (or (not (:audio-form-loaded? s)) (not= (:config audio) (get-in s [:audio :config]))))
          enumerate? (and sync (not (:devices-requested? s)))]
      (when (:visible? s)
        (cond-> {:db (update db :connections
                       #(cond-> (assoc % :sync sync :audio audio :section-reference section-reference)
                          initialize-sync?
                          (merge {:sync-form-loaded? true
                                  :sync-form (reduce (fn [out k] (update out k (partial merge (get sync-defaults k))))
                                                     (merge sync-defaults config) [:midi :beat-link :link :live])
                                  :notes (mapping-rows config :mappings) :parameters (parameter-rows config (get-in db [:server-projection :view :connection-status :sync :config]))})
                          initialize-audio? (assoc :audio-form (audio-form (:config audio)) :audio-form-loaded? true)
                          enumerate? (assoc :devices-requested? true)))}
          enumerate? (assoc :dispatch [:connections/refresh-devices]))))))
(rf/reg-event-fx :connections/loaded
  (fn [{:keys [db]} [_ request-token key data]]
    (when (current? db request-token)
      {:db (-> db (assoc-in [:connections key] data) (update-in [:connections :errors] dissoc key))})))
(rf/reg-event-fx :connections/failed
  (fn [{:keys [db]} [_ request-token key error]]
    (when (current? db request-token)
      {:db (-> db (assoc-in [:connections :errors key] (str error))
               (update-in [:connections :pending] disj key)
               (cond-> (= key :pause) (update :connections dissoc :pause-save)
                       (= key :lighting-capacity) (update :connections dissoc :capacity-draft)))})))
(rf/reg-event-fx :connections/configure
  (fn [{:keys [db]} [_ start?]]
    (try
      {:db (-> db (update-in [:connections :pending] (fnil conj #{}) :configure)
                (update-in [:connections :errors] dissoc :configure))
       :fx [(req db :configure "/api/sync/configure" (cond-> {:config (sync-payload db)} (get-in db [:ui :page-id]) (assoc :page-id (get-in db [:ui :page-id]))) (if start? :connections/configured-start :connections/configured))]}
      (catch :default error {:db (assoc-in db [:connections :errors :configure] (events/error-message error))}))))
(defn configured [db request-token typed-data]
  (when (current? db request-token)
    (let [data (projection/presentation typed-data)]
      {:db (-> db (assoc-in [:connections :sync-dirty?] false)
                (assoc-in [:connections :sync-form-loaded?] true)
                (assoc-in [:connections :unmapped] (:unmapped data))
                (assoc-in [:connections :notes] (mapping-rows (:config data) :mappings))
                (assoc-in [:connections :parameters] (parameter-rows (:config data) (:config typed-data)))
                (update-in [:connections :pending] disj :configure))})))
(rf/reg-event-fx :connections/configured
  (fn [{:keys [db]} [_ request-token _ data]] (configured db request-token data)))
(rf/reg-event-fx :connections/configured-start
  (fn [{:keys [db]} [_ request-token _ data]]
    (when-let [result (configured db request-token data)] (assoc result :dispatch [:connections/action :start]))))
(rf/reg-event-fx :connections/action
  (fn [{:keys [db]} [_ operation value]]
    (let [[path body] (case operation
                        :start ["/api/sync/start" {}] :stop ["/api/sync/stop" {}]
                        :learn-parameter ["/api/sync/learn-parameter" {:id value}]
                        :forget-parameter ["/api/sync/forget-parameter" {:id value}]
                        :arrange ["/api/sync/arrange" {:request-id (str (random-uuid)) :song-beat (js/Number value)}]
                        :source ["/api/sync/source" {:source-id value}]
                        :handoff ["/api/sync/handoff" {:mode value}]
                        :reference-reconnect ["/api/section-reference/reconnect"
                                              {:runtime-id (get-in db [:show :runtime-id])
                                               :expected-epoch (get-in db [:show :epoch])}]
                        :audio-stop ["/api/audio/stop" {}])]
      {:db (-> db (update-in [:connections :pending] (fnil conj #{}) operation)
                (cond-> (= operation :source) (assoc-in [:connections :sync-form :selected-source] value))
                (update-in [:connections :errors] dissoc operation))
       :fx [(req db operation path body :connections/action-complete)]})))
(rf/reg-event-fx :connections/capture
  (fn [{:keys [db]} _]
    {:db (-> db (update-in [:connections :pending] (fnil conj #{}) :capture)
             (update-in [:connections :errors] dissoc :capture))
     :fx [(req db :capture "/api/capture/capture" {} :connections/captured)]}))
(rf/reg-event-fx :connections/captured
  (fn [{:keys [db]} [_ request-token _ result]]
    (when (current? db request-token)
      {:db (-> db (update-in [:connections :pending] disj :capture)
               (assoc-in [:connections :capture] result))})))
(defn capture-text
  "One line describing the server's capture summary (the same operation as
  show/capture-phrase! at the REPL)."
  [{:keys [status bars notes cues lane-points capture-id message]}]
  (if (= "captured" (some-> status name))
    (str "Capture " capture-id " · " bars (if (= 1 bars) " bar · " " bars · ") notes
         (if (= 1 notes) " activation of " " activations of ") (str/join ", " (map #(if (keyword? %) (name %) (str %)) cues))
         " · " lane-points (if (= 1 lane-points) " parameter move" " parameter moves"))
    (or message "Nothing ran in the captured bars")))
(rf/reg-event-fx :connections/live-runs-refresh
  (fn [{:keys [db]} _]
    {:db (-> db (update-in [:connections :pending] (fnil conj #{}) :live-runs)
             (update-in [:connections :errors] dissoc :live-runs))
     :fx [(req db :live-runs "/api/live-runs" nil :connections/live-runs-loaded)]}))

(rf/reg-event-fx :connections/live-runs-loaded
  (fn [{:keys [db]} [_ request-token _ result]]
    (when (current? db request-token)
      {:db (-> db (update-in [:connections :pending] disj :live-runs)
               (assoc-in [:connections :live-runs] result))})))

(defn- plural [n one many] (str n " " (if (= 1 n) one many)))

(defn live-runs-text
  "One line summarizing the run record (the same projection as show/live-runs
  at the REPL)."
  [{:keys [channels takes running mirror truncated? shown]}]
  (let [clips (mapcat :takes (mapcat :lanes channels))
        in-live (count (filter #(= "in-live" (some-> % :clip :status name)) clips))]
    (if (zero? (or takes 0))
      "Nothing has run yet"
      (str (plural (count channels) "launched cue" "launched cues") " · " (plural takes "take" "takes")
           (when (pos? (or running 0)) (str " (" running " running)"))
           " · " (if mirror (str in-live " of " (count clips) " in Live") "not synced with Live")
           (when truncated? (str " · newest " shown " shown"))))))

(defn- beat-text [b] (if (number? b) (.toFixed b 2) "—"))

(defn take-text
  "One take: run number, start beat, held beats and where its clip is."
  [{:keys [run start-beat length-beats phase clip]}]
  (str "run " run " · beat " (beat-text start-beat) " · "
       (if length-beats (str "held " (beat-text length-beats)) (name phase))
       " · " (case (some-> clip :status name)
               "in-live" "in Live"
               "recording" "recording"
               "not-in-live" "not in Live yet"
               (some-> clip :status name))))

(rf/reg-event-fx :connections/action-complete
  (fn [{:keys [db]} [_ request-token operation result]]
    (when (current? db request-token)
      {:db (-> db (update-in [:connections :pending] disj operation)
                 (cond-> (= operation :audio-start) (update :connections merge {:audio-dirty? false :audio-form-loaded? true})
                   (false? (:accepted? result)) (assoc-in [:connections :errors operation] "The connection could not accept that action. Check its current status.")))})))
(rf/reg-event-fx :connections/audio-start
  (fn [{:keys [db]} _]
    (try {:db (-> db (update-in [:connections :pending] (fnil conj #{}) :audio-start)
                  (update-in [:connections :errors] dissoc :audio-start))
          :fx [(req db :audio-start "/api/audio/start" (audio-payload db) :connections/action-complete)]}
         (catch :default error {:db (assoc-in db [:connections :errors :audio-start] (events/error-message error))}))))
(rf/reg-event-db :connections/select-port
  (fn [db [_ direction value]]
    (let [port (some #(when (= value (str (:index %))) %) (:ports (state db)))]
      (-> db (assoc-in [:connections :sync-form :midi (if (= direction :input) :input-index :output-index)] value)
          (assoc-in [:connections :sync-form :midi (if (= direction :input) :expected-input-name :expected-output-name)] (:name port))
          (assoc-in [:connections :sync-dirty?] true)))))
(rf/reg-event-db :connections/select-audio-input
  (fn [db [_ value]]
    (let [mixer (some #(when (= value (str (:mixer-index %))) %) (:inputs (state db)))]
      (-> db (assoc-in [:connections :audio-form :input] value)
          (assoc-in [:connections :audio-form :expected-name] (:name mixer))
          (assoc-in [:connections :audio-dirty?] true)))))
(rf/reg-event-db :connections/select-audio-width
  (fn [db [_ value]]
    (let [form (:audio-form (state db))
          width (js/Number value)
          channel (js/Number (:input-channel form))]
      (-> db
          (assoc-in [:connections :audio-form]
                    (cond-> (assoc form :channels value)
                      (or (= 1 width) (not (pcm/valid-layout? width channel))) (assoc :input-channel "")))
          (assoc-in [:connections :audio-dirty?] true)))))
(rf/reg-event-db :connections/pairing
  (fn [db [_ enabled?]]
    (-> db (assoc-in [:connections :sync-form :live :pairing]
                    (when enabled? {:track-index "0" :clip-slot-index "0" :rack-device-index "0"}))
        (assoc-in [:connections :sync-dirty?] true))))
(rf/reg-event-db :connections/peer
  (fn [db [_ index]]
    (assoc-in (assoc-in db [:connections :sync-dirty?] true) [:connections :sync-form :beat-link :peer]
              (when (seq index) (select-keys (get-in db [:connections :sync :peers (js/parseInt index 10)])
                                              [:device-number :address :hardware-address :device-name])))))
(rf/reg-event-db :connections/mapping-add
  (fn [db [_ kind]]
    (let [rows (get-in db [:connections kind] []) used (set (map #(js/parseInt (:control %) 10) rows))
          next-control (first (remove used (range (if (= kind :notes) 36 0) 128)))
          cue-id (:id (first (get-in db [:show :cues])))]
      (if (and next-control cue-id)
        (-> db (update-in [:connections kind] (fnil conj [])
                         {:control (str next-control) :cue-id cue-id :mode "one-shot" :duration-value "1" :duration-unit "beats"
                          :param "level" :low "0" :high "1"})
            (assoc-in [:connections :sync-dirty?] true)) db))))
(rf/reg-event-db :connections/mapping-edit
  (fn [db [_ kind index key value]]
    (-> db (assoc-in [:connections kind index key] value) (assoc-in [:connections :sync-dirty?] true))))
(rf/reg-event-db :connections/mapping-remove
  (fn [db [_ kind index]]
    (-> db (update-in [:connections kind] #(vec (concat (subvec % 0 index) (subvec % (inc index)))))
        (assoc-in [:connections :sync-dirty?] true))))

(defn- value-text [value & [digits]]
  (cond (nil? value) "—" (number? value) (.toFixed value (or digits 0)) :else (str value)))
(declare status-chip)

(defn output-payload [db]
  (let [form (merge {:mode "direct" :fade-ms 100 :latency-ms 25}
                    (get-in db [:show :settings :output :presentation])
                    (get-in db [:connections :output-form]))
        mode (:mode form) fade (finite! (:fade-ms form) "Fade duration")
        latency (finite! (:latency-ms form) "Buffer latency")]
    (when-not (and (contains? #{"direct" "hold-fade" "buffered-interpolation"} mode)
                   (pos? fade) (<= 0 latency (* 2 (:period-ms form 25))))
      (throw (js/Error. "Choose an output mode, a positive fade, and at most two frames of latency.")))
    (assoc form :fade-ms fade :latency-ms latency)))

(defn pause-form [db]
  (merge {:mode "show" :color [1.0 0.4 0.12] :level 0.08}
         (get-in db [:show :settings :output :pause-look])
         (let [{:keys [baseline form]} (get-in db [:connections :pause-save])]
           (when (= baseline (get-in db [:show :settings :output :pause-look])) form))
         (get-in db [:connections :pause-form])))

(rf/reg-sub :connections/pause-look (fn [db _] (pause-form db)))

(defn- color-hex [rgb]
  (str "#" (apply str (map #(.padStart (.toString (js/Math.round (* 255 %)) 16) 2 "0") rgb))))

(defn- color-components [hex]
  (mapv #(/ (js/parseInt (subs hex % (+ % 2)) 16) 255) [1 3 5]))

(rf/reg-event-fx :connections/save-pause
  (fn [{:keys [db]} _]
    {:db (-> db (update-in [:connections :pending] (fnil conj #{}) :pause)
             (assoc-in [:connections :pause-save] {:baseline (get-in db [:show :settings :output :pause-look])
                                                   :form (pause-form db) :draft (get-in db [:connections :pause-form])}))
     :fx [(req db :pause "/api/settings" {:scope "output" :values {:pause-look (pause-form db)}}
               :connections/settings-saved)]}))

(rf/reg-event-fx :connections/set-fog-control
  (fn [{:keys [db]} [_ values]]
    {:db (update-in db [:connections :pending] (fnil conj #{}) :fog-control)
     :fx [(req db :fog-control "/api/settings" {:scope "fog-control" :values values} :connections/settings-saved)]}))

(rf/reg-event-fx :connections/set-capacity
  (fn [{:keys [db]} [_ values]]
    {:db (-> db (update-in [:connections :pending] (fnil conj #{}) :lighting-capacity)
             (assoc-in [:connections :capacity-draft] values))
     :fx [(req db :lighting-capacity "/api/settings" {:scope "lighting-capacity" :values values}
               :connections/settings-saved)]}))

(rf/reg-event-fx :connections/save-output
  (fn [{:keys [db]} _]
    (try
      {:db (-> db (update-in [:connections :pending] (fnil conj #{}) :output)
                (update-in [:connections :errors] dissoc :output))
       :fx [(req db :output "/api/settings" {:scope "output" :values {:presentation (output-payload db)}} :connections/settings-saved)]}
      (catch :default error {:db (assoc-in db [:connections :errors :output] (events/error-message error))}))))
(rf/reg-event-fx :connections/push-mode
  (fn [{:keys [db]} [_ mode]]
    (when (contains? #{"parameters" "audio"} mode)
      {:db (update-in db [:connections :pending] (fnil conj #{}) :push-display)
       :fx [(req db :push-display "/api/settings"
                 {:scope "controllers" :values {:push-display (assoc (get-in db [:show :settings :controllers :push-display] {}) :mode mode)}}
                 :connections/settings-saved)]})))
(rf/reg-event-fx :connections/parameter-view
  (fn [{:keys [db]} [_ field value]]
    (when (case field
            :parameter-view (#{"auto" "spatial" "history" "controls"} value)
            :history-beats (#{0.5 1 2 4 8 16} value)
            false)
      (let [settings (assoc (get-in db [:show :settings :controllers :push-display] {}) field value)]
        {:db (update-in db [:connections :pending] (fnil conj #{}) :push-display)
         :fx [(req db :push-display "/api/settings" {:scope "controllers" :values {:push-display settings}}
                   :connections/settings-saved)]}))))
(rf/reg-event-fx :connections/settings-saved
  (fn [{:keys [db]} [_ request-token key _ack]]
    (when (current? db request-token)
      {:db (-> db (update-in [:connections :pending] disj key) (update-in [:connections :errors] dissoc key)
               (cond-> (and (= key :pause) (= (get-in db [:connections :pause-save :draft])
                                               (get-in db [:connections :pause-form])))
                       (update :connections dissoc :pause-form)
                       (= key :lighting-capacity) (update :connections dissoc :capacity-draft)))})))

(defn- pause-panel [pending?]
  (let [{:keys [mode color level]} @(rf/subscribe [:connections/pause-look])]
    [:section.panel
     [:div.panel-top [:span.section-label "PAUSED LIGHTING"]]
     [:div.form-body
      [:label "When playback stops"
       [:select#pause-mode {:value mode :disabled pending? :on-change #(edit! [:pause-form :mode] (text %))}
        [:option {:value "show"} "Use the running cues"]
        [:option {:value "blackout"} "Blackout"]
        [:option {:value "ambient"} "Ambient color"]]]
      (when (= mode "ambient")
        [:div.form-row
         [:label "Ambient color" [:input#pause-color {:type "color" :value (color-hex color) :disabled pending?
                                                      :on-change #(edit! [:pause-form :color] (color-components (text %)))}]]
         [:label (str "Brightness · " (js/Math.round (* 100 level)) "%")
          [:input#pause-level {:type "range" :min 0 :max 100 :step 1 :value (* 100 level) :disabled pending?
                               :on-change #(edit! [:pause-form :level] (/ (js/Number (text %)) 100))}]]])
      [:button#pause-save.button {:type "button" :disabled pending? :on-click #(rf/dispatch [:connections/save-pause])}
       "Apply pause lighting"]]]))

(defn- output-panel [db s]
  (let [form (merge {:mode "direct" :fade-ms 100 :latency-ms 25}
                    (get-in db [:show :settings :output :presentation]) (:output-form s))
        state (get-in db [:show :status :output-scheduler])
        pending? (contains? (:pending s) :output)
        mode (:mode form)]
    [:section.panel.output-connection
     [:div.panel-top [:span.section-label "FRAME DELIVERY"]
      [status-chip (:scheduled? state) (if (:scheduled? state) "Independent output clock" "Manual output")]]
     [:div.form-body
      [:label "When rendering falls behind"
       [:select#output-mode {:value mode :disabled pending? :on-change #(edit! [:output-form :mode] (text %))}
        [:option {:value "direct"} "Hold the last frame"]
        [:option {:value "hold-fade"} "Gently fade the last frame"]
        [:option {:value "buffered-interpolation"} "Buffer and interpolate"]]]
      [:p.small.muted
       (case mode "hold-fade" "After a missed frame, fade emitted light while the next frame finishes. Fixture movement and discrete controls hold."
                  "buffered-interpolation" "Play a little behind the render clock and blend completed frames. If a frame is late, fade the held light output."
                  "Send completed frames in order and keep the last one unchanged during a delay.")]
      (when (not= mode "direct")
        [:div.form-row
         [:label "Fallback fade · ms" [:input#output-fade {:type "number" :min 1 :step 1 :value (:fade-ms form) :disabled pending?
                                                          :on-change #(edit! [:output-form :fade-ms] (text %))}]]
         (when (= mode "buffered-interpolation")
           [:label "Buffer latency · ms" [:input#output-latency {:type "number" :min 0 :max (* 2 (:period-ms form 25)) :step 1
                                                                :value (:latency-ms form) :disabled pending?
                                                                :on-change #(edit! [:output-form :latency-ms] (text %))}]])])
      [:button#output-save.button {:type "button" :disabled pending? :on-click #(rf/dispatch [:connections/save-output])} "Apply output mode"]
      [:p.small.muted "These are physical output deadlines. Cue, envelope and sequence timing remains musical."]
      (when-let [last (:last state)]
        [:p.small.muted (cond (:blackout? last) "Blackout is active."
                              (:waiting? last) "Waiting for a completed frame."
                              (:fallback? last) "Fallback active while rendering catches up."
                              :else "Completed frames are arriving.")])
      [:label "Push display view"
       [:select#push-display-mode {:value (get-in db [:show :settings :controllers :push-display :mode] "parameters")
                                   :disabled (contains? (:pending s) :push-display)
                                   :on-change #(rf/dispatch [:connections/push-mode (text %)])}
        [:option {:value "parameters"} "Effect parameters"] [:option {:value "audio"} "Captured audio + encoder values"]]]
      [:label "Parameter visualization"
       [:select {:value (get-in db [:show :settings :controllers :push-display :parameter-view] "auto")
                 :disabled (contains? (:pending s) :push-display)
                 :on-change #(rf/dispatch [:connections/parameter-view :parameter-view (text %)])}
        [:option {:value "auto"} "Automatic by type"] [:option {:value "spatial"} "3D space"]
        [:option {:value "history"} "Time graph"] [:option {:value "controls"} "Control values"]]]
      [:label "History window"
       [:select {:value (get-in db [:show :settings :controllers :push-display :history-beats] 4)
                 :disabled (contains? (:pending s) :push-display)
                 :on-change #(rf/dispatch [:connections/parameter-view :history-beats (js/Number (text %))])}
        (for [n [0.5 1 2 4 8 16]] ^{:key n} [:option {:value n} (str n " beats")])]]]]))

(defn- field [s path title & [attrs]]
  [:label title [:input (merge {:type "text" :value (get-in s path "") :on-change #(edit! path (text %))} attrs)]])

(defn- checkbox [s path title disabled?]
  [:label.checkbox-label [:input {:type "checkbox" :checked (boolean (get-in s path)) :disabled (boolean disabled?)
                                  :on-change #(edit! path (.. % -target -checked))}] title])

(defn- status-chip [active? title] [:span.connection-chip {:class (when active? "active")} [:i] title])

(defn- section-reference-panel [s]
  (let [{:keys [status reason configured? override?]} (:section-reference s)
        pending? (contains? (:pending s) :reference-reconnect)
        title (get {"unavailable" "Unavailable" "acquiring" "Acquiring"
                    "aligned" "Aligned" "ambiguous" "Ambiguous" "stale" "Stale"}
                   status "Loading")
        message (case reason
                  "not-configured" "No local mpv reference is configured."
                  "not-attached" "Connect to the configured local mpv player."
                  "show-changed" "Reconnect for this show session."
                  "session-replaced" "The reference session changed."
                  "expired" "The last matching observation expired."
                  "error" "The configured mpv connection is unavailable."
                  (some-> reason name (str/replace "-" " ")))]
    [:section.panel {:aria-label "Track reference"}
     [:div.panel-top [:span.section-label "TRACK REFERENCE"] [status-chip (= "aligned" status) title]]
     [:div.form-body
      (when message [:p.small.muted message])
      (when override? [:p.small.muted "Patterns suspended"])
      [:div.button-row
       [:button#reference-reconnect.button {:type "button" :disabled (or (not configured?) pending?)
                                            :on-click #(rf/dispatch [:connections/action :reference-reconnect])}
        (if pending? "Reconnecting…" "Reconnect mpv")]]]]))

(defn- clock-panel [s transport]
  (let [status (:sync s) clock-projection (:clock status)
        clock (or (:clock clock-projection) (when (= "manual" (get-in status [:config :selected-source] "manual")) transport))
        source (or (:selected-source clock-projection) (get-in status [:config :selected-source]) "manual")]
    [:section.panel.clock-panel
     [:div.panel-top [:span.section-label "ONE SHOW CLOCK"] [status-chip (= "locked" (:status clock-projection)) (or (:status clock-projection) (:state status) "Loading")]]
     [:div.form-body
      [:label "Clock source" [:select#clock-source {:value source :disabled (or (nil? status) (boolean (get-in s [:pending :source])))
                                                   :on-change #(rf/dispatch [:connections/action :source (text %)])}
                              [:option {:value "manual"} "Manual · studio transport"]
                              (for [[section fallback title] [[:midi "midi-clock" "MIDI clock"] [:beat-link "beat-link" "Beat Link / Pro DJ Link"]
                                                             [:link "ableton-link" "Ableton Link"] [:live "ableton-live" "Ableton Live bridge"]]]
                                ^{:key section} [:option {:value (get-in status [:config section :source-id] fallback)} title])]]
      [:div.connection-metrics
       [:div [:span "Tempo"] [:strong (value-text (:bpm clock) 1) [:small " BPM"]]]
       [:div [:span "Position"] [:strong (value-text (:beat clock) 2) [:small " beats"]]]
       [:div [:span "Source age"] [:strong (value-text (:source-age-ms clock) 0) [:small " ms"]]]
       [:div [:span "Confidence"] [:strong (if (number? (:confidence clock)) (str (js/Math.round (* 100 (:confidence clock))) "%") "—")]]]
      [:p.small.muted "Choose a source, then connect its adapter."]]]))

(defn- mapping-editor [s kind cues disabled?]
  [:details.connection-mappings
   [:summary (if (= kind :notes) "Drum pads → cues" "MIDI controllers → parameters")]
   (for [[index row] (map-indexed vector (get s kind))]
     ^{:key index}
     [:div.connection-mapping-row
      [:label (if (= kind :notes) "Note" "CC") [:input {:type "text" :input-mode "numeric" :value (:control row) :disabled disabled?
                                                         :aria-label (str (name kind) " " (inc index) " number")
                                                         :on-change #(rf/dispatch-sync [:connections/mapping-edit kind index :control (text %)])}]]
      [:label.checkbox-label [:input {:type "checkbox" :checked (not (:disabled? row)) :disabled disabled?
                                      :on-change #(rf/dispatch-sync [:connections/mapping-edit kind index :disabled? (not (.. % -target -checked))])}] "Enabled"]
      [:label "Cue" [:select {:value (:cue-id row) :disabled disabled? :aria-label (str (name kind) " " (inc index) " cue")
                              :on-change #(rf/dispatch-sync [:connections/mapping-edit kind index :cue-id (text %)])}
                     (for [cue cues] ^{:key (:id cue)} [:option {:value (:id cue)} (or (:name cue) (:id cue))])]]
      (if (= kind :notes)
        [:<>
         [:label "Launch" [:select {:value (:mode row) :disabled disabled?
                                    :on-change #(rf/dispatch-sync [:connections/mapping-edit kind index :mode (text %)])}
                           [:option {:value "one-shot"} "One-shot"] [:option {:value "held"} "Held"]
                           [:option {:value "persistent"} "Persistent"] [:option {:value "timed"} "Timed"]]]
         [:label "Duration" [:input {:type "text" :input-mode "decimal" :value (:duration-value row) :disabled disabled?
                                     :aria-label (str "notes " (inc index) " duration")
                                     :on-change #(rf/dispatch-sync [:connections/mapping-edit kind index :duration-value (text %)])}]
          [:select {:aria-label "Mapping duration unit" :value (:duration-unit row) :disabled disabled?
                    :on-change #(rf/dispatch-sync [:connections/mapping-edit kind index :duration-unit (text %)])}
           (for [unit ["beats" "bars" "phrases" "ms"]] ^{:key unit} [:option {:value unit} unit])]]]
        [:<>
         [:label "Clip note" [:input {:value (str (:lane-note row)) :input-mode "numeric" :disabled disabled?
                                      :aria-label "Parameter velocity lane pitch"
                                      :on-change #(rf/dispatch-sync [:connections/mapping-edit kind index :lane-note (text %)])}]]
         [:label "Parameter" [:input {:value (:param row) :disabled disabled? :placeholder "level"
                                      :on-change #(rf/dispatch-sync [:connections/mapping-edit kind index :param (text %)])}]]
         [:label "Range" [:div.connection-bounds
                           (for [[key label] [[:low "Minimum"] [:high "Maximum"]]]
                             ^{:key key} [:input {:type "text" :input-mode "decimal" :value (get row key) :aria-label label :disabled disabled?
                                                  :on-change #(rf/dispatch-sync [:connections/mapping-edit kind index key (text %)])}])]]])
      [:button.icon-button {:type "button" :aria-label (str "Remove mapping " (inc index)) :disabled disabled?
                            :on-click #(rf/dispatch [:connections/mapping-remove kind index])} "×"]])
   [:button.text-button {:type "button" :disabled disabled? :on-click #(rf/dispatch [:connections/mapping-add kind])}
    (if (= kind :notes) "Add pad mapping +" "Add parameter mapping +")]])

(defn- midi-panel [s disabled?]
  [:section.panel
   [:div.panel-top [:span.section-label "MIDI"] [checkbox s [:sync-form :midi :enabled?] "Enabled" disabled?]]
   [:div.form-body
    [:div.form-row
     (for [[direction title capability] [[:input "Input port" :input?] [:output "Output port" :output?]]
           :let [key (if (= direction :input) :input-index :output-index)
                 value (str (or (get-in s [:sync-form :midi key]) ""))
                 expected (get-in s [:sync-form :midi (if (= direction :input) :expected-input-name :expected-output-name)])
                 port (some #(when (and (= value (str (:index %))) (get % capability)) %) (:ports s))
                 missing? (and (seq value) (or (nil? port) (and expected (not= expected (:name port)))))]]
       ^{:key direction} [:label title
                           [:select {:id (str "midi-" (name direction)) :value (if missing? "missing" value) :disabled disabled?
                                     :on-change #(rf/dispatch-sync [:connections/select-port direction (text %)])}
                            [:option {:value ""} "Not connected"]
                            (when missing? [:option {:value "missing" :disabled true} "Saved port unavailable · choose again"])
                            (for [port (filter capability (:ports s))]
                              ^{:key (:index port)} [:option {:value (str (:index port))} (str (:index port) " · " (:name port))])]])]
    [checkbox s [:sync-form :midi :control?] "Use mapped notes and controllers to perform cues" disabled?]
    [:p.small.muted "Port selections include a name guard. If enumeration changes, reconnecting requires choosing the device again."]]])

(defn- beat-link-panel [s disabled?]
  (let [peers (get-in s [:sync :peers]) selected (get-in s [:sync-form :beat-link :peer])
        index (first (keep-indexed #(when (pairing/matches-pairing? selected %2) %1) peers))]
    [:section.panel
     [:div.panel-top [:span.section-label "PRO DJ LINK"] [checkbox s [:sync-form :beat-link :enabled?] "Enabled" disabled?]]
     [:div.form-body
      [:label "Paired player" [:select#beat-link-peer {:value (if (some? index) (str index) "") :disabled disabled?
                                                      :on-change #(rf/dispatch-sync [:connections/peer (text %)])}
                              [:option {:value ""} (if selected (str "Saved: " (:device-name selected) " · " (:address selected)) "No player selected")]
                              (for [[i peer] (map-indexed vector peers)] ^{:key i}
                                [:option {:value (str i)} (str (:device-number peer) " · " (:device-name peer) " · " (:address peer))])]]
      [checkbox s [:sync-form :beat-link :align-bars?] "Align to the player's bar position" disabled?]
      [checkbox s [:sync-form :beat-link :status?] "Request player status (announces a virtual device)" disabled?]
      [:p.small.muted "Connect to discover players. Disconnect before changing the pairing. Passive beat/device reception is the default; status mode also sends network announcements."]]]))

(defn- link-panel [s disabled?]
  [:section.panel
   [:div.panel-top [:span.section-label "ABLETON LINK"] [checkbox s [:sync-form :link :enabled?] "Enabled" disabled?]]
   [:div.form-body
    [field s [:sync-form :link :port] "Local Carabiner port"
     {:id "link-port" :type "number" :min 1 :max 65535 :disabled disabled?}]
    [checkbox s [:sync-form :link :follow-transport?] "Follow Link play and stop" disabled?]
    [:p.small.muted "Enable Link and Start Stop Sync in Live, then select Ableton Link as the show clock. Turn off transport following to operate the show’s play button independently."]]])

(defn- live-panel [s cues disabled?]
  (let [live (get-in s [:sync-form :live]) recording (get-in s [:sync :recording]) execution (get-in s [:sync :last-execution])
        active? (and (get-in s [:sync :active?]) (:enabled? live))]
    [:section.panel.live-connection
     [:div.panel-top [:span.section-label "ABLETON LIVE"] [checkbox s [:sync-form :live :enabled?] "Enabled" disabled?]]
     [:div.form-body
      [field s [:sync-form :live :endpoint] "Local bridge endpoint" {:id "live-endpoint" :type "url" :disabled disabled?}]
      [field s [:sync-form :live :bridge-id] "Bridge identity" {:id "live-bridge-id" :disabled disabled?}]
      [:p.small.muted "Connect the local Live bridge explicitly. This endpoint must use loopback HTTP on the computer running the studio."]
      [:div.form-row
       [:label "Recording route" [:select#live-record-route {:value (:record-route live) :disabled disabled?
                                                             :on-change #(edit! [:sync-form :live :record-route] (text %))}
                                  [:option {:value "bridge"} "Bridge · confirmed clip writes"] [:option {:value "midi"} "MIDI · delivery only"]]]
       [field s [:sync-form :live :channel] "MIDI channel · 1–16" {:id "live-channel" :input-mode "numeric" :disabled disabled?}]]
      [:label.checkbox-label [:input {:type "checkbox" :checked (:automap? live true) :disabled disabled?
                                      :on-change #(edit! [:sync-form :live :automap?] (.. % -target -checked))}]
       "Map this cue page and its parameters when saving"]
      [:label.checkbox-label [:input {:type "checkbox" :checked (boolean (:pairing live)) :disabled disabled?
                                       :on-change #(rf/dispatch-sync [:connections/pairing (.. % -target -checked)])}]
       "Pair a track, clip slot and drum rack"]
      (when (:pairing live)
        [:div.connection-pairing
         [:label "Destination" [:select {:value (if (contains? (:pairing live) :arrangement-index) "arrangement" "session") :disabled disabled?
                                         :on-change #(let [k (if (= "arrangement" (text %)) :arrangement-index :clip-slot-index)]
                                                       (edit! [:sync-form :live :pairing]
                                                              (assoc (dissoc (:pairing live) :clip-slot-index :arrangement-index :expected-clip-name) k "0")))}
                                [:option {:value "session"} "Session clip"] [:option {:value "arrangement"} "Arrangement clip"]]]
         [:div.form-row
          [field s [:sync-form :live :pairing :track-index] "Track index · from 0" {:id "live-track" :input-mode "numeric" :disabled disabled?}]
          [field s [:sync-form :live :pairing (if (contains? (:pairing live) :arrangement-index) :arrangement-index :clip-slot-index)] "Clip index · from 0" {:id "live-slot" :input-mode "numeric" :disabled disabled?}]
          [field s [:sync-form :live :pairing :rack-device-index] "Rack device index · optional" {:id "live-rack" :input-mode "numeric" :disabled disabled?}]]
         [:div.form-row
          [field s [:sync-form :live :pairing :expected-track-name] "Expected track name · optional" {:disabled disabled?}]
          [field s [:sync-form :live :pairing :expected-clip-name] "Expected clip name · optional" {:disabled disabled?}]]])
      [:div.button-row
       (for [[mode title] [["lighting" "Lighting mode"] ["live" "Live mode"]]]
         ^{:key mode} [:button.button {:type "button" :disabled (not active?) :aria-pressed (= mode (get-in s [:sync :config :live :mode]))
                                      :on-click #(rf/dispatch [:connections/action :handoff mode])} title])]
      [:p.small.muted (cond (true? (:recording? recording)) "Live is recording. Accepted lighting actions follow the configured recording route."
                           (map? recording) "Live status received; recording is inactive." :else "No Live recording status received yet.")]
      (when (and active? (:destination-id recording))
        [:div.form-row
         [field s [:arrangement-beat] "Arrangement start · song beats" {:id "live-arrangement-beat" :input-mode "decimal"}]
         [:button.button {:type "button" :disabled (or (:recording? recording) (str/blank? (:arrangement-beat s)))
                          :on-click #(rf/dispatch [:connections/action :arrange (:arrangement-beat s)])} "Place clip in Arrangement"]])
      (when execution [:p.small.muted (str "Last write: " (:status execution) (when (:clip-write-confirmed? execution) " · clip write confirmed"))])
      (when active?
        [:details.connection-mappings
         [:summary "Live automation sources"]
         [:p.small.muted "Select a knob, mixer fader or send in Live, name the source, then map it in the patchbay."]
         [:div.form-row
          [field s [:parameter-source-name] "Source name" {:placeholder "filter-cutoff" :max-length 64}]
          [:button.button {:type "button" :disabled (not (re-matches #"[A-Za-z0-9][A-Za-z0-9_.-]{0,63}" (:parameter-source-name s "")))
                           :on-click #(rf/dispatch [:connections/action :learn-parameter (:parameter-source-name s)])}
           "Learn selected parameter"]]
         (for [{:keys [id reason]} (get-in s [:sync :parameter-watch :skipped])]
           ^{:key id} [:p.connection-error {:role "status"} (str (events/wire-id id) " · " reason)])
         (for [[id spec] (sort-by (comp str key) (get-in s [:sync :config :live :observed-parameters]))]
           ^{:key id} [:div.form-row
                       [:span (str (events/wire-id id) " · " (:track-name spec) " / " (:device-name spec) " / " (:name spec))]
                       [:button.text-button {:type "button" :on-click #(rf/dispatch [:connections/action :forget-parameter (events/wire-id id)])}
                        "Forget"]])])
      (when (seq (:unmapped s))
        [:details.connection-mappings [:summary (str "Unassigned cue and parameter mappings · " (count (:unmapped s)))]
         (for [{:keys [cue-id param reason]} (:unmapped s)]
           ^{:key [cue-id param]} [:p.small (str cue-id (when param (str " / " param)) " · " reason)])])
      [mapping-editor s :notes cues disabled?] [mapping-editor s :parameters cues disabled?]]]))

(defn- audio-panel [s]
  (let [audio (:audio s) running? (contains? #{"starting" "running" "stopping"} (:state audio))
        pending? (boolean (some (:pending s) [:audio-start :audio-stop])) features (:features audio) metrics (:metrics audio)
        width (js/Number (get-in s [:audio-form :channels]))
        selected (get-in s [:audio-form :input]) mixer (some #(when (= selected (str (:mixer-index %))) %) (:inputs s))
        missing? (and (seq selected) (not= selected "system")
                      (or (nil? mixer) (and (get-in s [:audio-form :expected-name])
                                           (not= (:name mixer) (get-in s [:audio-form :expected-name])))))]
    [:section.panel.audio-connection
     [:div.panel-top [:span.section-label "AUDIO INPUT"] [status-chip (:running? audio) (or (:state audio) "Loading")]]
     [:div.form-body
      [:label "Capture device" [:select#audio-input {:value (if missing? "missing" selected) :disabled (or running? pending?)
                                                     :on-change #(rf/dispatch-sync [:connections/select-audio-input (text %)])}
                                [:option {:value ""} "Choose an input…"] [:option {:value "system"} "System default input"]
                                (when missing? [:option {:value "missing" :disabled true} "Saved device unavailable · choose again"])
                                (for [mixer (:inputs s)] ^{:key (:mixer-index mixer)}
                                  [:option {:value (str (:mixer-index mixer))} (:name mixer)])]]
      [:div.form-row
       [field s [:audio-form :sample-rate] "Sample rate · Hz" {:id "audio-rate" :input-mode "numeric" :disabled (or running? pending?)}]
       [:label "PCM channels" [:select#audio-channels {:value (str width) :disabled (or running? pending?)
                                                      :on-change #(rf/dispatch-sync [:connections/select-audio-width (text %)])}
                               [:option {:value "1"} "1 · Mono"] [:option {:value "2"} "2 · Stereo"]
                               (for [n (range 3 (inc pcm/max-channels))]
                                 ^{:key n} [:option {:value (str n)} n])]]
       (when (and (> width 1) (pcm/valid-layout? width 1))
         [:label "Input channel"
          [:select#audio-input-channel {:value (str (get-in s [:audio-form :input-channel]))
                                        :disabled (or running? pending?)
                                        :on-change #(edit! [:audio-form :input-channel] (text %))}
           (if (= 2 width)
             [:option {:value "" :aria-label "Mix both stereo channels"} "Mix"]
             [:option {:value "" :aria-label "Select an input channel"} "Select"])
           (for [n (range 1 (inc width))]
             ^{:key n} [:option {:value (str n) :aria-label (str "Channel " n)} n])]])]
      [:details [:summary "Capture buffering"] [:div.form-row
                                               [field s [:audio-form :block-frames] "Frames per block" {:input-mode "numeric" :disabled (or running? pending?)}]
                                               [field s [:audio-form :buffer-blocks] "Buffer blocks" {:input-mode "numeric" :disabled (or running? pending?)}]]]
      [:div.button-row
       [:button#audio-start.button.primary {:type "button" :disabled (or (nil? audio) running? pending? (str/blank? (get-in s [:audio-form :input])))
                                            :on-click #(rf/dispatch [:connections/audio-start])} "Start capture"]
       [:button#audio-stop.button {:type "button" :disabled (or (not running?) pending?)
                                  :on-click #(rf/dispatch [:connections/action :audio-stop])} "Stop capture"]]
      [:div.audio-level [:label {:html-for "audio-level"} "Signal energy · RMS"] [:meter#audio-level {:min 0 :max 1 :value (max 0 (min 1 (:rms features 0)))}]]
      [audio-scope/panel]
      [stems/panel s]
      [:div.connection-metrics
       [:div [:span "Level"] [:strong (value-text (:energy-dbfs features) 1) [:small " dBFS"]]]
       [:div [:span "Confidence"] [:strong (if (number? (:confidence features)) (str (js/Math.round (* 100 (:confidence features))) "%") "—")]]
       [:div [:span "Events"] [:strong (value-text (or (:received-event-count audio) (:events metrics)))]]
       [:div [:span "Blocks"] [:strong (value-text (:blocks metrics))]]]
      (when features [:p.small.muted (str "Energy: " (:energy-class features) " · " (if (:breakdown? features) "low-energy passage" "normal passage")
                                        " · input age " (value-text (:input-age-ms features)) " ms")])
      (when-let [error (or (:error audio) (:last-callback-error audio))] [:p.connection-error (:message error)])
      [:p.small.muted "Capture continues when you change views."]]]))

(defn- capture-panel [s]
  (let [result (:capture s) captured? (= "captured" (some-> (:status result) name))]
    [:section.panel {:aria-label "Capture"}
     [:div.panel-top [:span.section-label "CAPTURE"]]
     [:div.form-body
      [:p.small.muted "Keep the last bars that ran as an editable phrase: one note per cue run, paired 14-bit controller lanes for parameter moves."]
      [:div.button-row
       [:button#capture-phrase.button.primary {:type "button" :disabled (contains? (:pending s) :capture)
                                               :on-click #(rf/dispatch [:connections/capture])} "Capture last bars"]
       (when captured?
         [:a#capture-download.button {:href "/api/capture/last.mid" :download true} "Download MIDI clip"])]
      (when result [:p.small {:role "status"} (capture-text result)])]]))

(defn- live-runs-panel [s]
  (let [{:keys [channels mirror] :as runs} (:live-runs s)]
    [:section.panel {:aria-label "Live runs"}
     [:div.panel-top [:span.section-label "LIVE RUNS"]]
     [:div.form-body
      [:p.small.muted "What actually ran: one channel per launched cue, one take per run, a lane for each overlapping run, grouped as the effect groups are. Each take becomes one clip on its cue's run track in Live."]
      [:div.button-row
       [:button#live-runs-refresh.button {:type "button" :disabled (contains? (:pending s) :live-runs)
                                          :on-click #(rf/dispatch [:connections/live-runs-refresh])} "Show what ran"]]
      (when runs
        [:<>
         [:p.small {:role "status"} (live-runs-text runs)]
         (into [:ul.live-runs]
               (for [{:keys [cue-id name group-path lanes]} channels]
                 ^{:key (str cue-id)}
                 [:li [:strong name] (when (seq group-path) [:span.small.muted (str " · in " (str/join " / " (map #(if (keyword? %) (cljs.core/name %) (str %)) group-path)))])
                  (into [:ul]
                        (for [{:keys [lane track-name takes]} lanes]
                          ^{:key lane}
                          [:li [:span.small track-name]
                           (into [:ul] (for [t takes] ^{:key (:take-id t)} [:li.small (take-text t)]))]))]))
         (for [{:keys [code name]} (:problems mirror)]
           ^{:key (str code name)} [:p.connection-error.small (str (cljs.core/name code) (when name (str " · " name)))])
         (when-not mirror
           [:p.small.muted "Mirror the record into Live from the REPL with show/sync-live-runs!; this panel then shows each take's clip."])])]]))

(defn connection-view [db s]
  (let [active? (boolean (get-in s [:sync :active?])) pending? (boolean (seq (:pending s))) disabled? (or (nil? (:sync s)) active? pending?)]
    [:section#view-connections.view
     [:div.view-heading [:h1 "Connections"]
      [:button.button {:type "button" :on-click #(rf/dispatch [:connections/refresh-devices])} "Refresh device lists"]]
     (for [[key message] (:errors s)]
       ^{:key key} [:p.connection-error {:role "status"} message])
     [audience/panel]
     [invitation/panel]
     [clock-panel s (get-in db [:show :transport])]
     (when-let [error (get-in s [:sync :last-error])]
       [:p.connection-error {:role "status"} (or (:message error) (str error))])
     [:div.connection-actions.panel
      [:div [status-chip active? (str "Synchronization · " (or (get-in s [:sync :state]) "loading"))]
       [:p.small.muted "Save pairings without opening devices, or connect the enabled adapters explicitly."]]
      [:div.button-row
       [:button#sync-save.button {:type "button" :disabled disabled? :on-click #(rf/dispatch [:connections/configure false])} "Save configuration"]
       [:button#sync-start.button.primary {:type "button" :disabled (or disabled? (not-any? #(get-in s [:sync-form % :enabled?]) [:midi :beat-link :link :live]))
                                          :on-click #(rf/dispatch [:connections/configure true])} "Connect enabled adapters"]
       [:button#sync-stop.button {:type "button" :disabled (or (not active?) pending?) :on-click #(rf/dispatch [:connections/action :stop])} "Disconnect"]]]
     [:div.connections-layout [midi-panel s disabled?] [beat-link-panel s disabled?] [link-panel s disabled?]
      [live-panel s (get-in db [:show :cues]) disabled?] [capture-panel s] [live-runs-panel s] [audio-panel s] [section-reference-panel s]
      [capacity/panel (contains? (:pending s) :lighting-capacity)] [fog-control/panel (contains? (:pending s) :fog-control)] [output-panel db s] [pause-panel (contains? (:pending s) :pause)]]
     [audio-lfo/panel]
     [compute-client/panel]
     [sensors/panel db]
     [patchbay/panel db]]))

(def panel
  (r/create-class
    {:display-name "Connections"
     :component-did-mount (fn [_] (rf/dispatch [:connections/open]))
     :component-will-unmount (fn [_] (rf/dispatch [:connections/close]))
     :reagent-render (fn [db] [connection-view db @(rf/subscribe [:connections/state])])}))
