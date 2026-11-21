(ns tolglow.studio.connections
  "Explicit connection controls. Reading status never acquires a device; every
  start, stop, clock selection and Live handoff is a named re-frame event."
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [tolglow.studio.patchbay :as patchbay]
            [tolglow.studio.sensors :as sensors]
            [tolglow.studio.audio-scope :as audio-scope]
            [tolglow.studio.events :as events]))

(def sync-defaults
  {:selected-source "manual"
   :midi {:enabled? false :input-index "" :output-index "" :control? false}
   :beat-link {:enabled? false :status? false :align-bars? false :peer nil}
   :live {:enabled? false :endpoint "http://127.0.0.1:8750/live" :bridge-id "tolglow-m4l" :record-route "bridge"
          :mode "lighting" :channel 1 :automap? true :poll-ms 500 :timeout-ms 2000 :pairing nil
          :mappings {} :parameters {}}})
(def audio-defaults {:input "" :sample-rate "48000" :channels "1" :block-frames "480" :buffer-blocks "8" :big-endian? false :analyzer-options {}})
(def defaults {:visible? false :generation 0 :pending #{} :errors {} :sync nil :audio nil
               :ports [] :inputs [] :sync-form sync-defaults :audio-form audio-defaults
               :sync-dirty? false :audio-dirty? false :notes [] :parameters []})
(defn state [db] (merge defaults (:connections db)))
(defn token [db] [(get-in db [:show :runtime-id]) (get-in db [:show :epoch]) (:generation (state db))])
(defn current? [db request-token] (= request-token (token db)))
(defn- req [db key path body success]
  [:studio/http (cond-> {:path path :on-success [success (token db) key]
                         :on-failure [:connections/failed (token db) key] :timeout-ms 6000}
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
    (assoc form :midi midi :live (assoc (dissoc live :paired?) :pairing pairing
                                      :channel (integer! (:channel live) "MIDI channel" 1 16)
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
    (merge {:sample-rate (integer! (:sample-rate form) "Sample rate" 8000 192000)
            :channels (integer! (:channels form) "Channels" 1 2)
            :block-frames (integer! (:block-frames form) "Block size" 1 16384)
            :buffer-blocks (integer! (:buffer-blocks form) "Buffer blocks" 2 64)
            :big-endian? (:big-endian? form false) :analyzer-options (:analyzer-options form {})}
           (if mixer {:mixer-index (:mixer-index mixer) :mixer-name (:name mixer)} {:default? true}))))
(defn- audio-form [config]
  (merge audio-defaults (select-keys config [:sample-rate :channels :block-frames :buffer-blocks :big-endian? :analyzer-options])
         {:expected-name (:mixer-name config) :input (cond (:default? config) "system" (some? (:mixer-index config)) (str (:mixer-index config)) :else "")}))

(rf/reg-sub :connections/state (fn [db _] (state db)))
(rf/reg-event-fx :connections/open
  (fn [{:keys [db]} _]
    {:db (update db :connections #(cond-> (assoc (merge defaults %) :visible? true :polling? false :devices-requested? false)
                                    (not (:sync-dirty? %)) (assoc :sync-form-loaded? false)
                                    (not (:audio-dirty? %)) (assoc :audio-form-loaded? false)))
     :dispatch [:connections/poll]}))
(rf/reg-event-db :connections/close
  (fn [db _] (assoc db :connections (-> (state db) (assoc :visible? false :polling? false :pending #{}) (update :generation inc)))))
(rf/reg-event-fx :connections/runtime-change
  (fn [{:keys [db]} _]
    (let [s (state db)]
      {:db (assoc db :connections (-> defaults (assoc :visible? (:visible? s) :generation (inc (:generation s)))))
       :dispatch (when (:visible? s) [:connections/open])})))
(rf/reg-event-db :connections/edit
  (fn [db [_ path value]]
    (cond-> (assoc-in db (into [:connections] path) value)
      (= :sync-form (first path)) (assoc-in [:connections :sync-dirty?] true)
      (= :audio-form (first path)) (assoc-in [:connections :audio-dirty?] true))))
(rf/reg-event-fx :connections/refresh-devices
  (fn [{:keys [db]} _]
    {:fx [(req db :ports "/api/sync/ports" nil :connections/loaded)
          (req db :inputs "/api/audio/inputs" nil :connections/loaded)]}))
(rf/reg-event-fx :connections/poll
  (fn [{:keys [db]} _]
    (when (and (:visible? (state db)) (not (:polling? (state db))))
      (if-not (:show db)
        {:dispatch-later [{:ms 100 :dispatch [:connections/poll]}]}
        {:db (-> db (assoc-in [:connections :polling?] true) (assoc-in [:connections :devices-requested?] true)
                   (assoc-in [:connections :poll-waiting] #{:sync :audio}))
         :fx (cond-> [(req db :sync "/api/sync" nil :connections/loaded)
                      (req db :audio "/api/audio" nil :connections/loaded)]
               (not (:devices-requested? (state db)))
               (conj (req db :ports "/api/sync/ports" nil :connections/loaded)
                     (req db :inputs "/api/audio/inputs" nil :connections/loaded)))}))))
(defn- poll-complete [db key]
  (let [db (update-in db [:connections :poll-waiting] disj key)]
    (if (and (#{:sync :audio} key) (empty? (get-in db [:connections :poll-waiting])))
      {:db (assoc-in db [:connections :polling?] false)
       :dispatch-later (when (:visible? (state db)) [{:ms 750 :dispatch [:connections/poll]}])}
      {:db db})))
(rf/reg-event-fx :connections/loaded
  (fn [{:keys [db]} [_ request-token key data]]
    (when (current? db request-token)
      (let [s (state db)
            db (-> db (assoc-in [:connections key] data) (update-in [:connections :errors] dissoc key))
            db (cond-> db
                 (and (= key :sync) (:config data) (not (:sync-dirty? s)) (not (:sync-form-loaded? s)))
                 (update :connections merge {:sync-form-loaded? true
                                             :sync-form (reduce (fn [out k] (assoc out k (merge (get sync-defaults k) (get-in data [:config k]))))
                                                               (merge sync-defaults (:config data)) [:midi :beat-link :live])
                                             :notes (mapping-rows (:config data) :mappings) :parameters (mapping-rows (:config data) :parameters)})
                 (and (= key :audio) (:config data) (not (:audio-dirty? s)) (not (:audio-form-loaded? s)))
                 (update :connections merge {:audio-form (audio-form (:config data)) :audio-form-loaded? true}))]
        (poll-complete db key)))))
(rf/reg-event-fx :connections/failed
  (fn [{:keys [db]} [_ request-token key error]]
    (when (current? db request-token)
      (poll-complete (-> db (assoc-in [:connections :errors key] (str error))
                        (update-in [:connections :pending] disj key)) key))))
(rf/reg-event-fx :connections/configure
  (fn [{:keys [db]} [_ start?]]
    (try
      {:db (-> db (update-in [:connections :pending] (fnil conj #{}) :configure)
                (update-in [:connections :errors] dissoc :configure))
       :fx [(req db :configure "/api/sync/configure" (cond-> {:config (sync-payload db)} (get-in db [:ui :page-id]) (assoc :page-id (get-in db [:ui :page-id]))) (if start? :connections/configured-start :connections/configured))]}
      (catch :default error {:db (assoc-in db [:connections :errors :configure] (events/error-message error))}))))
(defn configured [db request-token data]
  (when (current? db request-token)
    {:db (-> db (assoc-in [:connections :sync] data) (assoc-in [:connections :sync-dirty?] false)
                (assoc-in [:connections :sync-form-loaded?] true)
                (assoc-in [:connections :unmapped] (:unmapped data))
                (assoc-in [:connections :notes] (mapping-rows (:config data) :mappings))
                (assoc-in [:connections :parameters] (mapping-rows (:config data) :parameters))
                (update-in [:connections :pending] disj :configure))}))
(rf/reg-event-fx :connections/configured
  (fn [{:keys [db]} [_ request-token _ data]] (configured db request-token data)))
(rf/reg-event-fx :connections/configured-start
  (fn [{:keys [db]} [_ request-token _ data]]
    (when-let [result (configured db request-token data)] (assoc result :dispatch [:connections/action :start]))))
(rf/reg-event-fx :connections/action
  (fn [{:keys [db]} [_ operation value]]
    (let [[path body] (case operation
                        :start ["/api/sync/start" {}] :stop ["/api/sync/stop" {}]
                        :arrange ["/api/sync/arrange" {:request-id (str (random-uuid)) :song-beat (js/Number value)}]
                        :source ["/api/sync/source" {:source-id value}]
                        :handoff ["/api/sync/handoff" {:mode value}]
                        :audio-stop ["/api/audio/stop" {}])]
      {:db (-> db (update-in [:connections :pending] (fnil conj #{}) operation)
                (cond-> (= operation :source) (assoc-in [:connections :sync-form :selected-source] value))
                (update-in [:connections :errors] dissoc operation))
       :fx [(req db operation path body :connections/action-complete)]})))
(rf/reg-event-fx :connections/action-complete
  (fn [{:keys [db]} [_ request-token operation result]]
    (when (current? db request-token)
      {:db (-> db (update-in [:connections :pending] disj operation)
                 (cond-> (= operation :audio-start) (update :connections merge {:audio-dirty? false :audio-form-loaded? true})
                   (false? (:accepted? result)) (assoc-in [:connections :errors operation] "The connection could not accept that action. Check its current status.")))
       :dispatch [:connections/poll]})))
(rf/reg-event-fx :connections/audio-start
  (fn [{:keys [db]} _]
    (try {:db (update-in db [:connections :pending] (fnil conj #{}) :audio-start)
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
(rf/reg-event-fx :connections/settings-saved
  (fn [{:keys [db]} [_ request-token key _ack]]
    (when (current? db request-token)
      {:db (-> db (update-in [:connections :pending] disj key) (update-in [:connections :errors] dissoc key))})))

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
        [:option {:value "parameters"} "Effect parameters"] [:option {:value "audio"} "Captured audio + encoder values"]]]]]))
(defn- field [s path title & [attrs]]
  [:label title [:input (merge {:type "text" :value (get-in s path "") :on-change #(edit! path (text %))} attrs)]])
(defn- checkbox [s path title disabled?]
  [:label.checkbox-label [:input {:type "checkbox" :checked (boolean (get-in s path)) :disabled (boolean disabled?)
                                  :on-change #(edit! path (.. % -target -checked))}] title])
(defn- status-chip [active? title] [:span.connection-chip {:class (when active? "active")} [:i] title])
(defn- error-list [s]
  (for [[key message] (:errors s)] ^{:key key} [:p.connection-error {:role "status"} message]))
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
                                                             [:live "ableton-live" "Ableton Live"]]]
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
        index (first (keep-indexed #(when (= (select-keys %2 [:device-number :address]) (select-keys selected [:device-number :address])) %1) peers))]
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
      (when (seq (:unmapped s))
        [:details.connection-mappings [:summary (str "Unassigned cue and parameter mappings · " (count (:unmapped s)))]
         (for [{:keys [cue-id param reason]} (:unmapped s)]
           ^{:key [cue-id param]} [:p.small (str cue-id (when param (str " / " param)) " · " reason)])])
      [mapping-editor s :notes cues disabled?] [mapping-editor s :parameters cues disabled?]]]))
(defn- audio-panel [s]
  (let [audio (:audio s) running? (contains? #{"starting" "running" "stopping"} (:state audio))
        pending? (boolean (some (:pending s) [:audio-start :audio-stop])) features (:features audio) metrics (:metrics audio)
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
       [:label "Channels" [:select#audio-channels {:value (str (get-in s [:audio-form :channels])) :disabled (or running? pending?)
                                                   :on-change #(edit! [:audio-form :channels] (text %))}
                            [:option {:value "1"} "Mono"] [:option {:value "2"} "Stereo · analyzed as mono"]]]]
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
      [:div.connection-metrics
       [:div [:span "Level"] [:strong (value-text (:energy-dbfs features) 1) [:small " dBFS"]]]
       [:div [:span "Confidence"] [:strong (if (number? (:confidence features)) (str (js/Math.round (* 100 (:confidence features))) "%") "—")]]
       [:div [:span "Events"] [:strong (value-text (or (:received-event-count audio) (:events metrics)))]]
       [:div [:span "Blocks"] [:strong (value-text (:blocks metrics))]]]
      (when features [:p.small.muted (str "Energy: " (:energy-class features) " · " (if (:breakdown? features) "low-energy passage" "normal passage")
                                        " · input age " (value-text (:input-age-ms features)) " ms")])
      (when-let [error (or (:error audio) (:last-callback-error audio))] [:p.connection-error (:message error)])
      [:p.small.muted "Capture continues when you change views."]]]))

(defn connection-view [db s]
  (let [active? (boolean (get-in s [:sync :active?])) pending? (boolean (seq (:pending s))) disabled? (or (nil? (:sync s)) active? pending?)]
    [:section#view-connections.view
     [:div.view-heading [:div [:div.eyebrow "PLAY IN TIME"] [:h1 "Bring the room together."]
                         [:p "Pair clocks, instruments and audio inputs with the show."]]
      [:button.button {:type "button" :on-click #(rf/dispatch [:connections/refresh-devices])} "Refresh device lists"]]
     [error-list s]
     [clock-panel s (get-in db [:show :transport])]
     (when-let [error (get-in s [:sync :last-error])]
       [:p.connection-error {:role "status"} (or (:message error) (str error))])
     [:div.connection-actions.panel
      [:div [status-chip active? (str "Synchronization · " (or (get-in s [:sync :state]) "loading"))]
       [:p.small.muted "Save pairings without opening devices, or connect the enabled adapters explicitly."]]
      [:div.button-row
       [:button#sync-save.button {:type "button" :disabled disabled? :on-click #(rf/dispatch [:connections/configure false])} "Save configuration"]
       [:button#sync-start.button.primary {:type "button" :disabled (or disabled? (not-any? #(get-in s [:sync-form % :enabled?]) [:midi :beat-link :live]))
                                          :on-click #(rf/dispatch [:connections/configure true])} "Connect enabled adapters"]
       [:button#sync-stop.button {:type "button" :disabled (or (not active?) pending?) :on-click #(rf/dispatch [:connections/action :stop])} "Disconnect"]]]
     [:div.connections-layout [midi-panel s disabled?] [beat-link-panel s disabled?]
      [live-panel s (get-in db [:show :cues]) disabled?] [audio-panel s] [output-panel db s]]
     [sensors/panel db]
     [patchbay/panel db]]))
(def panel
  (r/create-class
    {:display-name "Connections"
     :component-did-mount (fn [_] (rf/dispatch [:connections/open]))
     :component-will-unmount (fn [_] (rf/dispatch [:connections/close]))
     :reagent-render (fn [db] [connection-view db @(rf/subscribe [:connections/state])])}))
