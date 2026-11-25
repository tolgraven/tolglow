(ns tolglow.controllers.content-browser-display
  "Saved shows, registered library previews and checkpoint restore for tethered Push."
  (:require [afterglow.control.codecs.midi :as midi-codec]
            [afterglow.control.devices.transports :as device-transports]
            [tolglow.controllers.content-browser :as browser]
            [tolglow.controllers.content-browser-host :as host]
            [tolglow.show :as show]))

(def ^:private closed {:open? false :page 0})

(def ^:private next-kind {:show :effect :effect :cue :cue :show})

(def ^:private source-labels (array-map :recovery "Working recovery" :named "Latest named"))

(defn navigate
  "Apply a catalog action against the last presented page. A row press captures
  its exact identity. Paging and returning from a preview preserve selection.
  Changing library starts at its first page."
  [{:keys [open? preview? page source kind] :or {kind :show} :as state}
   {:keys [page-count items] presented-page :page} action]
  (cond
    (= :toggle action) (if open? closed (assoc closed :open? true))
    (not open?) state
    (= :back action) (cond source (dissoc state :source :name)
                           preview? (dissoc state :preview?)
                           :else closed)
    (and (= :next-kind action) (not preview?))
    (-> state (assoc :kind (next-kind kind) :page 0) (dissoc :selected))
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

(defn- source-view [{:keys [source-kind handle revision]}]
  (cond-> {:label (get source-labels source-kind)}
    (and (#{:checkpoint/named :checkpoint/recovery} handle) (nat-int? revision) (pos? revision))
    (assoc :modified-at-ms revision)))

(defn- sources [{:keys [sources]}]
  (into [] (keep (fn [[kind label]]
                   (when-let [source (get sources kind)]
                     (assoc (source-view source) :label label))))
        source-labels))

(defn display-view
  "Publish only display fields. Selection identities, handles and file paths
  stay on the host. A row preview reports current owner metadata."
  [{:keys [preview?]} {:keys [kind items selected-key page page-count total]} preview]
  (let [registered? (#{:effect :cue} kind)]
    (cond-> {:kind kind :page page :page-count page-count :total total
             :rows (mapv (fn [entry]
                           (merge {:name (:name entry) :selected? (= selected-key (:key entry))}
                                  (if registered?
                                    (select-keys entry [:version])
                                    {:sources (mapv :label (sources entry))})))
                         items)}
      preview? (assoc :preview (if-let [entry (:entry preview)]
                                 (if registered?
                                   (select-keys entry [:name :version :effect-version :categories :usage])
                                   {:name (:name entry) :sources (sources entry)})
                                 {:unavailable? true})))))

(defn- valid-action? [action]
  (or (#{:toggle :back :previous :next :next-kind :confirm} action)
      (and (vector? action) (= 2 (count action)) (= :select (first action))
           (nat-int? (second action)) (< (second action) browser/page-size))
      (and (vector? action) (= 2 (count action)) (= :checkpoint (first action))
           (#{:named :recovery} (second action)))))

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

(defn- apply-action [{:keys [navigation presented result request] :as state} action]
  (cond
    request state
    result (if (#{:back :toggle} action)
             (-> state (assoc :navigation closed) (dissoc :result :presented :catalog-generation))
             state)
    (= :confirm action)
    (if-let [confirmation (:confirmation presented)]
      (if (= confirmation (get-in state [:choice :confirmation]))
        (-> state (assoc :request {:basis (Object.) :confirmation confirmation
                                   :result (assoc (source-view (:source navigation)) :name (:name navigation))})
            (dissoc :presented))
        state)
      state)

    :else
    (let [next-navigation
          (if (and (vector? action) (= :checkpoint (first action)))
            (if-let [source (get-in presented [:entry :sources (second action)])]
              (assoc navigation :source source :name (get-in presented [:entry :name]))
              navigation)
            (navigate navigation (:page presented) action))
          scope-changed? (not= (select-keys navigation [:open? :kind])
                               (select-keys next-navigation [:open? :kind]))]
      (cond-> (assoc state :navigation next-navigation)
        scope-changed?
        (assoc :catalog-generation (when (:open? next-navigation) (Object.)))
        (or scope-changed?
            (not= (select-keys navigation [:preview? :source])
                  (select-keys next-navigation [:preview? :source])))
        (dissoc :presented :choice)))))

(defn- route-button! [state addresses generation packet]
  (when-let [{:keys [channel controller raw-value]} (button-message packet)]
    (let [address [channel controller]]
      (when-let [action (addresses address)]
        (locking state
          (let [{:keys [navigation held midi-generation display-generation] :as current} @state
                owned? (get held address)]
            (when (= generation midi-generation)
              (cond
                (zero? raw-value) (do (reset! state (update current :held dissoc address)) owned?)
                (some? owned?) owned?
                :else
                (let [handled? (boolean (and display-generation (or (:open? navigation) (= :toggle action))))]
                  (reset! state (cond-> (assoc-in current [:held address] handled?)
                                  handled? (apply-action action)))
                  handled?)))))))))

(defn- rejection [result]
  (case (or (:reason result) (:code result))
    :stale-save-source :changed
    (:stale-session :stale-selection :stale-device-session) :session-changed
    :refused))

(defn- record-restore! [state basis result]
  (locking state
    (when (identical? basis (get-in @state [:request :basis]))
      (if (= :submitted (:status result))
        (swap! state assoc-in [:request :receipt] (:receipt result))
        (when (= :rejected (:status result))
          (swap! state #(-> % (dissoc :request :presented)
                            (assoc :choice {:status (rejection result)}))))))))

(defn- advance-restore! [state confirm! {:keys [basis confirmation presented? receipt result]}]
  (cond
    (and receipt (realized? receipt)) (record-restore! state basis @receipt)
    (and presented? (nil? receipt))
    (let [submission (try (confirm! confirmation result)
                          (catch Exception _ {:status :rejected}))]
      (record-restore! state basis submission))))

(defn- read-choice! [state preview! navigation runtime-id]
  (let [{:keys [selected source]} navigation
        confirmation (try (:confirmation (preview! {:runtime-id runtime-id :selection selected :source source}))
                          (catch Exception _ nil))
        choice (if confirmation {:status :ready :confirmation confirmation} {:status :unavailable})]
    (locking state
      (when (identical? navigation (:navigation @state))
        (swap! state assoc :choice choice)))
    choice))

(defn- catalog-projection! [state {:keys [project! preview! confirm! source-buttons next-kind?]} view]
  (when-let [request (:request @state)] (advance-restore! state confirm! request))
  (let [{:keys [navigation midi-generation display-generation catalog-generation request choice result]} @state
        scope [midi-generation display-generation catalog-generation]]
    (cond
      (not (and display-generation catalog-generation)) view
      (:pad-overlay view) (vary-meta view assoc ::presented {:scope scope :navigation navigation})
      :else
      (let [{:keys [selected page preview? source name kind] :or {kind :show}} navigation
            projection (when (and (nil? result) (or (nil? source) (nil? choice)))
                         (project! {:kind kind :page page :selected-key (:key selected)}))
            preview (when (and preview? selected (nil? source))
                      (preview! {:runtime-id (:runtime-id projection) :selection selected}))
            choice (when source (or choice (read-choice! state preview! navigation (:runtime-id projection))))
            catalog (cond
                      result {:checkpoint result}
                      source {:checkpoint (assoc (source-view source) :name name
                                                  :status (if request :pending (:status choice)))}
                      :else (cond-> (display-view navigation projection preview)
                              next-kind? (assoc :next-kind? true)
                              (and (= :show kind) (seq source-buttons) (:entry preview))
                              (assoc-in [:preview :load-options]
                                        (into [] (keep (fn [[kind label]]
                                                         (when (and (source-buttons kind)
                                                                  (get-in preview [:entry :sources kind])) label)))
                                              source-labels))))]
        (vary-meta (assoc view :display-mode :saved-shows
                               :saved-show-catalog catalog)
                   assoc ::presented {:scope scope
                                      :navigation navigation
                                      :page (when-not (or preview? result) projection)
                                      :entry (:entry preview)
                                      :pending (:basis request)
                                      :confirmation (when-not request (:confirmation choice))})))))

(defn- present! [state view]
  (when-let [{:keys [scope page navigation pending] :as presented} (::presented (meta view))]
    (locking state
      (let [current @state]
        (when (and (= scope ((juxt :midi-generation :display-generation :catalog-generation) current))
                   (or (identical? navigation (:navigation current))
                       ;; A page redraw can finish after another page key is pressed.
                       (and page (not (get-in current [:navigation :preview?])))))
          (if (= :saved-shows (:display-mode view))
            (reset! state (cond-> (assoc current :presented presented)
                            (and pending (identical? pending (get-in current [:request :basis])))
                            (assoc-in [:request :presented?] true)))
            (reset! state (dissoc current :presented))))))))

(defn transports
  "Wrap an explicit MIDI transport and build a display around its worker's
  :pre-project! and :presented! hooks. `project!` returns a host catalog page
  and `preview!` receives its runtime ID and exact selection. A checkpoint
  confirmation is submitted only after the pending view has been presented."
  [{:keys [midi display! controls project! preview! confirm! result]}]
  (when-not (every? ifn? [display! project! preview!])
    (throw (ex-info "Catalog needs display, projection and preview callbacks" {})))
  (let [addresses (control-addresses controls)
        source-buttons (into #{} (keep (fn [action] (when (vector? action)
                                                      (when (= :checkpoint (first action)) (second action)))))
                             (keys controls))
        _ (when (or (not= (boolean (seq source-buttons)) (contains? controls :confirm))
                    (and (seq source-buttons) (not (ifn? confirm!))))
            (throw (ex-info "Restore requires checkpoint buttons, Confirm and an owner callback" {})))
        state (atom (cond-> {:navigation closed :held {}}
                      result (assoc :navigation (assoc closed :open? true) :catalog-generation (Object.) :result result)))
        project-options {:project! project! :preview! preview! :confirm! confirm!
                         :source-buttons source-buttons :next-kind? (contains? controls :next-kind)}
        display (display! {:pre-project! (partial catalog-projection! state project-options)
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
                                                               (dissoc :display-generation :catalog-generation :presented
                                                                       :choice :request :result))))
                               ((:close! display) handle)))}))

(defn connect-device!
  "Connect one dedicated Push 2 or Push 3 with a content library display.
  Supply :catalog-controls {action {:channel c :controller n}} using unused
  User Mode buttons. Optional :next-kind cycles saved shows, effects and cues.
  Restore adds [:checkpoint :named], [:checkpoint :recovery]
  (one or both), and :confirm. Supply :display-options for the browser and
  :catalog-saves as the trusted descriptor vector used by content-browser-host."
  [runtime kind {:keys [midi transport catalog-controls catalog-saves display-options] :as options}]
  (when-not (#{:push-2 :push-3} kind)
    (throw (ex-info "Saved-show browser requires a dedicated Push 2 or Push 3" {:device kind})))
  (when (and (= kind :push-3) (= :mpe (:expression-mode options))
             (some (set (get-in options [:zone :member-channels])) (map :channel (vals catalog-controls))))
    (throw (ex-info "Catalog buttons need fixed User Mode channels outside MPE members" {})))
  (let [saves (or catalog-saves [])
        options (dissoc options :catalog-controls :catalog-saves :display-options)]
    (browser/validate-configured-save-limit! saves)
    (letfn [(connect! [reconnect-lease result]
              (let [lease (Object.)
                    restore-device {:session-id (:session-id options) :lease lease}
                    browser-options
                    (transports {:midi (or midi transport (device-transports/midi-transport options))
                                 :controls catalog-controls :result result
                                 :project! (partial host/project runtime saves)
                                 :preview! (partial host/preview runtime saves)
                                 :confirm! (fn [confirmation checkpoint]
                                             (host/confirm-load! runtime saves confirmation
                                               (assoc restore-device :after-restore!
                                                      #(connect! % (assoc checkpoint :status :restored)))))
                                 :display! #(show/push-browser-display runtime
                                              (merge display-options % {:device kind :presentation-receipts? true}))})]
                (show/connect-device! runtime kind
                  (merge options browser-options {::show/restore-lease lease ::show/reconnect-lease reconnect-lease}))))]
      (connect! nil nil))))
