(ns tolglow.controllers.content-browser-display
  "Read-only saved-show navigation for tethered Push display transports."
  (:require [afterglow.control.codecs.midi :as midi-codec]
            [afterglow.control.devices.transports :as device-transports]
            [tolglow.controllers.content-browser :as browser]
            [tolglow.controllers.content-browser-host :as host]
            [tolglow.show :as show]))

(def ^:private closed {:open? false :page 0})

(defn navigate
  "Apply a catalog action against the last presented page. A row press captures
  its exact identity. Paging and returning from a preview preserve selection."
  [{:keys [open? preview? page] :as state} {:keys [page-count items] presented-page :page} action]
  (cond
    (= :toggle action) (if open? closed (assoc closed :open? true))
    (not open?) state
    (= :back action) (if preview? (dissoc state :preview?) closed)
    (and (not preview?) (vector? action) (= :select (first action)))
    (if-let [entry (get items (second action))]
      (assoc state :page presented-page :selected (:selection entry) :preview? true)
      state)
    (and (not preview?) (pos? (or page-count 0)))
    (case action
      :previous (assoc state :page (max 0 (dec (min page (dec page-count)))))
      :next (assoc state :page (min (dec page-count) (inc page)))
      state)
    :else state))

(defn- sources [{:keys [sources]}]
  (into [] (keep (fn [[kind label]]
                   (when-let [source (get sources kind)]
                     (cond-> {:label label}
                       (and (#{:checkpoint/named :checkpoint/recovery} (:handle source))
                            (nat-int? (:revision source)) (pos? (:revision source)))
                       (assoc :modified-at-ms (:revision source))))))
        [[:recovery "Working recovery"] [:named "Latest named"]]))

(defn display-view
  "Publish only display fields. Selection identities, handles and file paths
  stay on the host. A preview reports current owner metadata without reading
  or importing the selected file."
  [{:keys [preview?]} {:keys [items selected-key page page-count total]} preview]
  (cond-> {:page page :page-count page-count :total total
           :rows (mapv (fn [entry] {:name (:name entry) :selected? (= selected-key (:key entry))
                                   :sources (mapv :label (sources entry))}) items)}
    preview? (assoc :preview (if-let [entry (:entry preview)]
                              {:name (:name entry) :sources (sources entry)}
                              {:unavailable? true}))))

(defn- valid-action? [action]
  (or (#{:toggle :back :previous :next} action)
      (and (vector? action) (= 2 (count action)) (= :select (first action))
           (nat-int? (second action)) (< (second action) browser/page-size))))

(defn- control-addresses [controls]
  (let [valid-address? (fn [{:keys [channel controller] :as address}]
                         (and (map? address) (= #{:channel :controller} (set (keys address)))
                              (nat-int? channel) (< channel 16)
                              (nat-int? controller) (< controller 128)))]
    (when-not (and (map? controls) (contains? controls :toggle)
                   (every? valid-action? (keys controls)) (every? valid-address? (vals controls))
                   (= (count controls) (count (set (vals controls)))))
      (throw (ex-info "Catalog controls need distinct captured button addresses and a toggle" {})))
    (into {} (map (fn [[action {:keys [channel controller]}]] [[channel controller] action])) controls)))

(defn- button-message [packet]
  (when (and (sequential? packet) (integer? (first packet)) (<= 0xb0 (first packet) 0xbf))
    (try (midi-codec/decode packet) (catch clojure.lang.ExceptionInfo _ nil))))

(defn- route-button! [state addresses generation packet]
  (when-let [{:keys [channel controller raw-value]} (button-message packet)]
    (let [address [channel controller]]
      (when-let [action (addresses address)]
        (locking state
          (let [{:keys [navigation presented held midi-generation display-generation] :as current} @state
                owned? (get held address)]
            (when (= generation midi-generation)
              (cond
                (zero? raw-value) (do (reset! state (update current :held dissoc address)) owned?)
                (some? owned?) owned?
                :else
                (let [handled? (boolean (and display-generation (or (:open? navigation) (= :toggle action))))
                      next-navigation (if handled? (navigate navigation presented action) navigation)
                      open-changed? (not= (:open? navigation) (:open? next-navigation))]
                  (reset! state (cond-> (assoc-in current [:held address] handled?)
                                  handled? (assoc :navigation next-navigation)
                                  open-changed? (dissoc :presented)
                                  open-changed? (assoc :catalog-generation (when (:open? next-navigation) (Object.)))))
                  handled?)))))))))

(defn- catalog-projection! [state project! preview! view]
  (let [{:keys [navigation display-generation catalog-generation]} @state]
    (if-not (and display-generation catalog-generation)
      view
      (let [{:keys [selected page preview?]} navigation
            projection (project! {:kind :show :page page :selected-key (:key selected)})
            preview (when (and preview? selected)
                      (preview! {:runtime-id (:runtime-id projection) :selection selected}))]
        (vary-meta (assoc view :display-mode :saved-shows
                               :saved-show-catalog (display-view navigation projection preview))
                   assoc ::presented {:scope [display-generation catalog-generation]
                                      :page (when-not preview? projection)})))))

(defn- present! [state view]
  (when-let [{:keys [scope page]} (::presented (meta view))]
    (locking state
      (when (= scope ((juxt :display-generation :catalog-generation) @state))
        (swap! state assoc :presented page)))))

(defn transports
  "Wrap an explicit MIDI transport and build a display around its worker's
  :pre-project! and :presented! hooks. `project!` returns a host catalog page
  and `preview!` receives its runtime ID and exact selection. Use a distinct
  wrapper for each controller. Only the supplied catalog buttons are routed here."
  [{:keys [midi display! controls project! preview!]}]
  (when-not (every? ifn? [display! project! preview!])
    (throw (ex-info "Catalog needs display, projection and preview callbacks" {})))
  (let [addresses (control-addresses controls)
        state (atom {:navigation closed :held {}})
        display (display! {:pre-project! (partial catalog-projection! state project! preview!)
                           :presented! (partial present! state)})]
    {:midi (assoc midi
                  :open! (fn [receive!]
                           (let [generation (Object.)]
                             (locking state (swap! state assoc :midi-generation generation :held {}))
                             ((:open! midi) (fn [packet]
                                              (when (= generation (:midi-generation @state))
                                                (when-not (route-button! state addresses generation packet)
                                                  (receive! packet)))))))
                  :close! (fn [handle]
                            (locking state (swap! state dissoc :midi-generation))
                            ((:close! midi) handle)))
     :display (assoc display
                     :open! (fn []
                              (let [handle ((:open! display))]
                                (locking state (swap! state assoc :display-generation (Object.)))
                                handle))
                     :close! (fn [handle]
                               (locking state (swap! state #(-> % (assoc :navigation closed)
                                                               (dissoc :display-generation :catalog-generation :presented))))
                               ((:close! display) handle)))}))

(defn connect-device!
  "Connect one dedicated Push 2 or Push 3 with a read-only saved-show display.
  Supply :catalog-controls {action {:channel c :controller n}} using unused
  User Mode buttons, :display-options for the browser display, and optionally
  :catalog-saves as the trusted descriptor vector used by content-browser-host."
  [runtime kind {:keys [midi transport catalog-controls catalog-saves display-options] :as options}]
  (when-not (#{:push-2 :push-3} kind)
    (throw (ex-info "Saved-show browser requires a dedicated Push 2 or Push 3" {:device kind})))
  (when (and (= kind :push-3) (= :mpe (:expression-mode options))
             (some (set (get-in options [:zone :member-channels])) (map :channel (vals catalog-controls))))
    (throw (ex-info "Catalog buttons need fixed User Mode channels outside MPE members" {})))
  (let [saves (or catalog-saves [])
        _ (browser/validate-configured-save-limit! saves)
        browser-options (transports {:midi (or midi transport (device-transports/midi-transport options))
                                     :controls catalog-controls
                                     :project! (partial host/project runtime saves)
                                     :preview! (partial host/preview runtime saves)
                                     :display! #(show/push-browser-display runtime
                                                   (merge display-options % {:device kind}))})]
    (show/connect-device! runtime kind (merge (dissoc options :catalog-controls :catalog-saves :display-options)
                                             browser-options))))
