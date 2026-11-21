(ns tolglow.studio.sync
  "Explicit MIDI/Pro DJ Link/Live bridge resources around immutable clock and
  control transitions. Requiring or restoring this namespace opens no devices.
  One event owner and one bounded IO worker isolate capture from slow host IO."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [afterglow.control.clock :as clock]
            [afterglow.time :as musical-time]
            [afterglow.control.codecs.midi :as midi-codec]
            [afterglow.control.devices.transports :as device-transports]
            [tolglow.integrations.ableton :as ableton]
            [tolglow.integrations.providers :as providers])
  (:import [javax.sound.midi MidiSystem MidiDevice MidiMessage Receiver ShortMessage]
           [java.lang.reflect Proxy InvocationHandler]
           [java.net URI]
           [java.util UUID]
           [java.util.concurrent ArrayBlockingQueue ThreadPoolExecutor ThreadPoolExecutor$AbortPolicy
            ThreadFactory TimeUnit RejectedExecutionException]))

(defn- fail! [code message] (throw (ex-info message {:sync/error code})))
(defn- finite? [x] (and (number? x) (Double/isFinite (double x))))
(defn- identifier? [x] (or (keyword? x) (and (string? x) (<= 1 (count x) 128))))
(defn- source-id [id]
  (when-not (identifier? id) (fail! :invalid-config "Clock source requires a bounded identity"))
  (if (keyword? id) id (keyword id)))
(defn- now-ms [] (/ (double (System/nanoTime)) 1000000.0))
(defn- port? [value] (or (nil? value) (and (integer? value) (<= 0 value 4096))))
(defn- mapping-key [key]
  (let [key (if (keyword? key) (name key) key)
        key (if (and (string? key) (re-matches #"\d{1,3}" key)) (Long/parseLong key) key)]
    (when-not (and (integer? key) (<= 0 key 127)) (fail! :invalid-config "MIDI mapping keys must be 0..127")) key))
(defn- live-pairing? [pairing]
  (or (nil? pairing)
      (and (map? pairing)
           (every? #{:track-index :clip-slot-index :arrangement-index :rack-device-index :expected-track-name :expected-clip-name} (keys pairing))
           (= 1 (count (select-keys pairing [:clip-slot-index :arrangement-index])))
           (every? #(and (integer? %) (<= 0 % 9999))
                   [(:track-index pairing) (or (:clip-slot-index pairing) (:arrangement-index pairing))])
           (or (nil? (:rack-device-index pairing)) (and (integer? (:rack-device-index pairing)) (<= 0 (:rack-device-index pairing) 16383)))
           (every? #(or (nil? %) (and (string? %) (<= 1 (count %) 512)))
                   ((juxt :expected-track-name :expected-clip-name) pairing)))))
(defn- mappings [values kind]
  (when-not (and (map? values) (<= (count values) 128)) (fail! :invalid-config "Mappings must be bounded maps"))
  (into {} (map (fn [[key value]]
                 (when-not (and (map? value) (identifier? (:cue-id value))
                                (boolean? (:disabled? value false))
                                (if (= kind :notes)
                                  (and (#{:persistent :held :timed :one-shot} (get value :mode :persistent))
                                       (or (nil? (:duration-ms value)) (and (finite? (:duration-ms value)) (pos? (:duration-ms value)))))
                                  (and (identifier? (:param value)) (not (contains? value :instance-id))
                                       (or (nil? (:lane-note value)) (and (integer? (:lane-note value)) (<= 0 (:lane-note value) 127)))
                                       (#{:number :integer :boolean :enum} (:type value :number))
                                       (or (not= :enum (:type value)) (and (vector? (:values value)) (<= 1 (count (:values value)) 127)))
                                       (let [[low high] (get value :bounds [0 1])]
                                         (and (every? finite? [low high]) (< low high))))))
                   (fail! :invalid-config "Invalid logical cue/parameter mapping"))
                 (when (:duration value)
                   (when (or (:duration-ms value) (seq (musical-time/validate-duration (:duration value)))
                             (not (pos? (val (first (:duration value))))))
                     (fail! :invalid-config "Mapping duration must use one positive musical/explicit time unit")))
                 [(mapping-key key) value]) values)))

(defn validate-config
  "Pure settings validation. Disabled sections retain pairings. selected-source
  is explicit; selecting an unconnected source waits rather than choosing a peer.
  Live object IDs/held instances are runtime state, never durable mappings."
  [input]
  (when-not (and (map? input) (every? #{:selected-source :clock :midi :beat-link :live} (keys input)))
    (fail! :invalid-config "Unknown synchronization configuration field"))
  (doseq [[section allowed] [[:clock #{:bpm :beat :playing? :meter :timeout-ms :on-stale}]
                             [:midi #{:enabled? :source-id :input-index :output-index :control? :expected-input-name :expected-output-name}]
                             [:beat-link #{:enabled? :source-id :status? :align-bars? :peer}]
                             [:live #{:enabled? :source-id :bridge-id :endpoint :poll-ms :timeout-ms :record-route :mode :channel :mappings :parameters :pairing :automap?}]]]
    (when (contains? input section)
      (when-not (and (map? (get input section)) (every? allowed (keys (get input section))))
        (fail! :invalid-config "Unknown synchronization section field"))))
  (let [base {:selected-source :manual :clock {:bpm 120.0 :beat 0.0 :playing? false :meter {:beats-per-bar 4 :bars-per-phrase 4}
                                             :timeout-ms 1500.0 :on-stale :hold}
              :midi {:enabled? false :source-id :midi-clock :input-index nil :output-index nil :control? false}
              :beat-link {:enabled? false :source-id :beat-link :status? false :align-bars? false :peer nil}
              :live {:enabled? false :source-id :ableton-live :bridge-id "tolglow-m4l" :endpoint "http://127.0.0.1:8750/live"
                     :poll-ms 500 :timeout-ms 2000 :record-route :bridge :mode :lighting :channel 1
                     :mappings {} :parameters {} :pairing nil :automap? true}}
        config (reduce (fn [out key] (assoc out key (merge (get base key) (get input key))))
                       (assoc base :selected-source (source-id (get input :selected-source :manual))) [:clock :midi :beat-link :live])
        config (reduce #(update-in %1 [%2 :source-id] source-id) config [:midi :beat-link :live])
        config (-> config (update-in [:clock :on-stale] #(if (string? %) (keyword %) %))
                   (update-in [:live :mode] #(if (string? %) (keyword %) %))
                   (update-in [:live :record-route] #(if (string? %) (keyword %) %))
                   (update-in [:live :mappings] #(mappings (into {} (map (fn [[k v]] [k (cond-> v (string? (:mode v)) (update :mode keyword))]) %)) :notes))
                   (update-in [:live :parameters] #(mappings % :parameters)))
        midi (:midi config) dj (:beat-link config) live (:live config)
        ids (mapv #(get-in config [% :source-id]) [:midi :beat-link :live])
        peer (:peer dj)]
    (when-not (and (every? boolean? [(:enabled? midi) (:control? midi) (:enabled? dj) (:status? dj)
                                    (:align-bars? dj) (:enabled? live) (:automap? live)])
                   (every? port? ((juxt :input-index :output-index) midi))
                   (or (not (:enabled? midi)) (some some? ((juxt :input-index :output-index) midi)))
                   (every? #(or (nil? %) (and (string? %) (<= (count %) 512))) ((juxt :expected-input-name :expected-output-name) midi))
                   (= 3 (count (set ids))) (not (some #{:manual} ids))
                   (some #{(:selected-source config)} (conj ids :manual))
                   (#{:lighting :live} (:mode live)) (#{:bridge :midi} (:record-route live))
                   (string? (:bridge-id live)) (<= 1 (count (:bridge-id live)) 128)
                   (integer? (:channel live)) (<= 1 (:channel live) 16)
                   (integer? (:poll-ms live)) (<= 100 (:poll-ms live) 10000)
                   (integer? (:timeout-ms live)) (<= 50 (:timeout-ms live) 10000)
                   (let [notes (concat (keys (:mappings live)) (keep :lane-note (vals (:parameters live))))]
                     (= (count notes) (count (set notes))))
                   (live-pairing? (:pairing live))
                   (or (nil? peer)
                       (and (map? peer) (every? #{:device-number :address :hardware-address :device-name} (keys peer))
                            (integer? (:device-number peer)) (<= 1 (:device-number peer) 255)
                            (or (nil? (:device-name peer)) (and (string? (:device-name peer)) (<= 1 (count (:device-name peer)) 512)))
                            (or (and (string? (:address peer)) (re-matches #"[0-9a-fA-F:.%]+" (:address peer)))
                                (and (string? (:hardware-address peer)) (re-matches #"(?i)[0-9a-f]{2}(:[0-9a-f]{2}){5}" (:hardware-address peer)))))))
      (fail! :invalid-config "Invalid synchronization sources, peer pairing or Live settings"))
    (try (let [uri (URI. (:endpoint live))]
           (when-not (and (= "http" (.getScheme uri)) (#{"127.0.0.1" "localhost" "[::1]" "::1"} (.getHost uri))
                          (nil? (.getUserInfo uri)) (nil? (.getFragment uri)))
             (fail! :invalid-config "Live bridge endpoint must be an explicit loopback HTTP URL")))
         (catch Exception _ (fail! :invalid-config "Invalid Live bridge endpoint")))
    (clock/new-state (assoc (:clock config) :selected-source (:selected-source config)))
    config))

(defn validate-state [data]
  (when-not (and (map? data) (every? #{:version :config :wanted?} (keys data)) (= 1 (:version data))
                 (boolean? (get data :wanted? false)))
    (fail! :invalid-state "Invalid synchronization state document"))
  {:version 1 :config (validate-config (:config data)) :wanted? (get data :wanted? false)})

(defn midi-ports [] (device-transports/midi-ports))

(defn midi-transport
  "Explicit input/output acquisition using Java MIDI. Either direction may be
  omitted. Previously open devices remain open; only this adapter's handles and
  newly opened devices are closed. No enumeration/opening happens at creation."
  [config clock-ms]
  {:open!
   (fn [receive!]
     (let [infos (vec (MidiSystem/getMidiDeviceInfo)) acquired (atom []) opened (atom []) handles (atom {})]
       (try
         (doseq [[direction index expected] [[:input (:input-index config) (:expected-input-name config)]
                                             [:output (:output-index config) (:expected-output-name config)]]
                 :when (some? index)]
           (let [info (get infos index)]
             (when-not info (fail! :missing-port "Selected MIDI port is no longer available"))
             (when (and expected (not= expected (.getName info))) (fail! :changed-port "Selected MIDI port name changed"))
             (let [^MidiDevice device (or (some #(when (= index (:index %)) (:device %)) @acquired) (MidiSystem/getMidiDevice info))]
               (swap! acquired conj {:index index :device device})
               (when-not (.isOpen device) (.open device) (swap! opened conj device))
               (if (= direction :input)
                 (let [transmitter (.getTransmitter device)]
                   (swap! handles assoc :transmitter transmitter)
                   (.setReceiver transmitter
                     (reify Receiver
                       (send [_ message timestamp]
                         (receive! {:type :midi/message :source-id (:source-id config) :time-ms (clock-ms)
                                    :device-timestamp-us timestamp
                                    :bytes (mapv #(bit-and 255 %) (take (.getLength ^MidiMessage message) (.getMessage ^MidiMessage message)))}))
                       (close [_] nil))))
                 (swap! handles assoc :receiver (.getReceiver device))))))
         (assoc @handles :owned-devices @opened)
         (catch Exception error
           (doseq [resource (vals @handles)] (try (.close ^java.lang.AutoCloseable resource) (catch Exception _)))
           (doseq [^MidiDevice device @opened] (try (.close device) (catch Exception _)))
           (throw error)))))
   :send! (fn [{:keys [receiver]} messages]
            (when-not receiver (fail! :no-midi-output "No MIDI output is configured"))
            (doseq [bytes messages]
              (let [[status a b] bytes]
                (.send ^Receiver receiver (doto (ShortMessage.) (.setMessage (int status) (int (or a 0)) (int (or b 0)))) -1))))
   :close! (fn [{:keys [transmitter receiver owned-devices]}]
             (doseq [resource [transmitter receiver]] (when resource (try (.close ^java.lang.AutoCloseable resource) (catch Exception _))))
             (doseq [^MidiDevice device owned-devices] (.close device)))})

(defn announcement-data
  "Read real Beat Link announcement getters into portable peer data. Announcement
  timestamps are wall-clock milliseconds, so the host supplies observation time."
  [announcement observed-at-ms]
  {:device-number (.getDeviceNumber announcement) :device-name (.getDeviceName announcement)
   :address (.getHostAddress (.getAddress announcement))
   :hardware-address (str/join ":" (map #(format "%02x" (bit-and 255 %)) (.getHardwareAddress announcement)))
   :observed-at-ms observed-at-ms})

(defn beat-data
  "Read Beat Link 8 Beat getters. DeviceUpdate.getTimestamp is nanoTime, unlike
  DeviceAnnouncement; convert its age into the explicit host logical domain.
  Player-number bar significance is labeled heuristic until status is available."
  [beat observed-at-ms observed-nanos]
  (let [age (max 0.0 (/ (- observed-nanos (.getTimestamp beat)) 1000000.0))
        bpm (.getEffectiveTempo beat) number (.getDeviceNumber beat)]
    {:type :beat-link/beat :time-ms (max 0.0 (- observed-at-ms age)) :received-at-ms observed-at-ms :packet-age-ms age
     :bpm (when (and (not= 65535 (.getBpm beat)) (finite? bpm) (pos? bpm)) bpm)
     :beat-within-bar (.getBeatWithinBar beat) :bar-meaningful? (< number 33)
     :bar-meaning-source :player-number-heuristic
     :peer {:device-number number :device-name (.getDeviceName beat) :address (.getHostAddress (.getAddress beat))}}))

(defonce ^:private beatlink-lease (atom nil))
(defn- singleton [class-name] (clojure.lang.Reflector/invokeStaticMethod class-name "getInstance" (object-array 0)))
(defn- listener [class-name methods]
  (let [interface (Class/forName class-name)]
    (Proxy/newProxyInstance (.getClassLoader interface) (into-array Class [interface])
      (reify InvocationHandler
        (invoke [_ proxy method arguments]
          (case (.getName method)
            "hashCode" (System/identityHashCode proxy) "equals" (identical? proxy (aget arguments 0)) "toString" class-name
            (when-let [handler (get methods (.getName method))] (apply handler (seq arguments)))))))))

(defn beat-link-transport
  "Explicit Beat Link 8 adapter. Listener registration and socket acquisition
  happen only in open!. Owns an exclusive lease on the library singletons and
  refuses to stop/share components owned by another integration. Optional status
  mode starts VirtualCdj (network announcements); default is passive beat/device
  reception. Never requires afterglow.dj-link or its global listeners."
  [config clock-ms]
  {:open!
   (fn [receive!]
     (let [token (Object.) components (atom nil) closed? (atom false)
           close! (fn []
                    (when (compare-and-set! closed? false true)
                      (when-let [{:keys [finder beats virtual peer-listener beat-listener status-listener]} @components]
                        (try (.removeDeviceAnnouncementListener finder peer-listener) (catch Exception _))
                        (try (.removeBeatListener beats beat-listener) (catch Exception _))
                        (when status-listener (try (.removeUpdateListener virtual status-listener) (catch Exception _)))
                        (when (:status? config) (try (.stop virtual) (catch Exception _)))
                        (try (.stop beats) (catch Exception _)) (try (.stop finder) (catch Exception _)))
                      (locking beatlink-lease (when (identical? token @beatlink-lease) (reset! beatlink-lease nil)))))]
       (try
       (locking beatlink-lease
         (when @beatlink-lease (fail! :beat-link-busy "Beat Link is already owned by another session"))
         (let [finder (singleton "org.deepsymmetry.beatlink.DeviceFinder")
               beats (singleton "org.deepsymmetry.beatlink.BeatFinder")
               virtual (singleton "org.deepsymmetry.beatlink.VirtualCdj")]
           (when (some #(.isRunning %) [finder beats virtual])
             (fail! :beat-link-busy "Beat Link components are already running outside this owner"))
           (reset! beatlink-lease token)
           (let [send! (fn [event] (when-not @closed? (receive! (assoc event :source-id (:source-id config)))))
                 peer-listener (listener "org.deepsymmetry.beatlink.DeviceAnnouncementListener"
                                         {"deviceFound" #(send! {:type :peer/found :peer (announcement-data % (clock-ms))})
                                          "deviceLost" #(send! {:type :peer/lost :peer (announcement-data % (clock-ms))})})
                 beat-listener (listener "org.deepsymmetry.beatlink.BeatListener"
                                         {"newBeat" #(send! (beat-data % (clock-ms) (System/nanoTime)))})
                 status-listener (when (:status? config)
                                   (listener "org.deepsymmetry.beatlink.DeviceUpdateListener"
                                             {"received" (fn [update]
                                                           (when (= "org.deepsymmetry.beatlink.CdjStatus" (.getName (class update)))
                                                             (let [observed (clock-ms) age (max 0.0 (/ (- (System/nanoTime) (.getTimestamp update)) 1000000.0))
                                                                   bpm (.getEffectiveTempo update)]
                                                               (send! {:type :beat-link/status :time-ms (max 0.0 (- observed age))
                                                                       :bpm (when (and (not= 65535 (.getBpm update)) (finite? bpm) (pos? bpm)) bpm)
                                                                       :playing? (.isPlaying update) :tempo-master? (.isTempoMaster update)
                                                                       :bar-meaningful? (.isBeatWithinBarMeaningful update)
                                                                       :reported-beat-number (.getBeatNumber update)
                                                                       :peer {:device-number (.getDeviceNumber update) :device-name (.getDeviceName update)
                                                                              :address (.getHostAddress (.getAddress update))}}))))}))]
             (reset! components {:finder finder :beats beats :virtual virtual :peer-listener peer-listener
                                 :beat-listener beat-listener :status-listener status-listener}))))
         (let [{:keys [finder beats virtual peer-listener beat-listener status-listener]} @components]
           (.addDeviceAnnouncementListener finder peer-listener) (.addBeatListener beats beat-listener)
           (when status-listener (.addUpdateListener virtual status-listener))
           (.start finder) (.start beats)
           (when (and (:status? config) (not (.start virtual))) (fail! :no-dj-peers "No Pro DJ Link peers became available"))
           (doseq [peer (.getCurrentDevices finder)]
             (receive! {:type :peer/found :source-id (:source-id config) :peer (announcement-data peer (clock-ms))}))
           {:close! close!})
         (catch Throwable error (close!) (throw error)))))
   :close! (fn [handle] ((:close! handle)))})

(defn live-http-transport
  "Actual bounded localhost HTTP requests to the companion Max bridge. No HTTP
  request happens at factory/open time. The owner explicitly invokes inspect or
  writes; cancellation closes an active response through the shared transport."
  [config]
  {:open! (fn [_] {:cancelled? (atom false)})
   :request! (fn [handle id op payload]
               (let [result (providers/http-transport!
                             {:method :post :url (:endpoint config)
                              :headers {"content-type" "application/json" "accept" "application/json"}
                              :body (json/write-str {:id id :protocol-version 1 :op op
                                                     :payload (cond-> payload
                                                                (:bridge-id config) (assoc :expected-bridge-id (:bridge-id config))
                                                                (:sync-session-id config) (assoc :sync-session-id (:sync-session-id config)))})}
                             (fn [_] nil)
                             {:timeout-ms (:timeout-ms config) :connect-timeout-ms (min 1000 (:timeout-ms config))
                              :max-response-chars 1048576 :cancelled? (:cancelled? handle)})]
                 (when-not (= 200 (:status result)) (fail! :live-http "Live bridge returned a failing HTTP status"))
                 (let [reply (json/read-str (:body result) :key-fn keyword)]
                   (when-not (and (= id (:id reply)) (= 1 (:protocol-version reply))
                                  (#{"executed" "rejected" "indeterminate"} (:status reply)))
                     (fail! :live-protocol "Live bridge response has invalid identity or protocol"))
                   (update reply :status keyword))))
   :close! (fn [handle] (reset! (:cancelled? handle) true))})

(defn- bridge [config]
  (ableton/new-bridge (assoc (select-keys (:live config) [:mappings :parameters :channel :mode]) :id :live)))

(defn create-manager
  "Create inert resources/settings. Options inject :now-ms, :midi-factory
  [config clock-ms], :beat-link-factory [config clock-ms], :live-factory [config]
  and :queue-capacity (256). Factories return open!/close! plus optional
  send!/request!. No factory or device is invoked before explicit start!."
  ([] (create-manager {}))
  ([options]
   (let [options (merge {:now-ms now-ms :midi-factory midi-transport :beat-link-factory beat-link-transport
                        :live-factory live-http-transport :queue-capacity 256} options)
         config (validate-config (get options :config {}))]
     (when-not (and (every? ifn? ((juxt :now-ms :midi-factory :beat-link-factory :live-factory) options))
                    (integer? (:queue-capacity options)) (<= 8 (:queue-capacity options) 4096))
       (fail! :invalid-options "Invalid synchronization boundary options"))
     {:lock (Object.) :options options
      :state (atom {:closed? false :active nil :config config :clock (clock/new-state (assoc (:clock config) :selected-source (:selected-source config)))
                    :bridge (bridge config) :retired [] :peers {} :recording nil :status :stopped :metrics {}})})))

(defn export-state [manager] {:version 1 :config (:config @(:state manager)) :wanted? false})
(defn active-run-id [manager] (get-in @(:state manager) [:active :id]))
(defn invalidate-inputs!
  "Invalidate queued input/IO tokens for one captured run immediately. Pure
  ownership mutation only: no locks, callbacks, transport work or waits. Host
  publication uses this before rebind! installs the next callback boundary."
  [manager expected-run-id]
  (when-let [run (:active @(:state manager))]
    (when (= expected-run-id (:id run))
      (swap! (:binding-generation run) inc))))
(defn status [manager]
  (let [{:keys [closed? config status peers recording metrics last-error last-execution active]} @(:state manager)]
    {:closed? closed? :state status :running? (= :running status) :active? (boolean active)
     :config config :peers (vec (sort-by (juxt :device-number :address) (vals peers)))
     :recording recording :metrics metrics :last-error last-error :last-execution last-execution}))
(defn clock-at
  "Selected external clock projection for the next owner frame. Manual selection
  returns clock nil so this boundary cannot overwrite the host's manual transport."
  [manager at-ms]
  (let [result (clock/snapshot (:clock @(:state manager)) at-ms)]
    (if (= :manual (:selected-source result)) (assoc result :clock nil) result)))

(defn configure! [manager config]
  (let [config (validate-config config)]
    (locking (:lock manager)
      (when (:closed? @(:state manager)) (fail! :closed "Synchronization manager is closed"))
      (when (:active @(:state manager)) (fail! :busy "Stop synchronization before changing resource pairings"))
      (swap! (:state manager) assoc :config config :bridge (bridge config) :peers {} :recording nil
             :clock (clock/new-state (assoc (:clock config) :selected-source (:selected-source config))) :status :stopped :last-error nil)))
  (status manager))
(defn restore! [manager data] (configure! manager (:config (validate-state data))))
(defn select-source! [manager id]
  (let [id (source-id id)]
    (locking (:lock manager)
      (let [config (validate-config (assoc (:config @(:state manager)) :selected-source id))]
        (swap! (:state manager) #(-> % (assoc :config config) (update :clock clock/select-source id))))))
  (clock-at manager ((get-in manager [:options :now-ms]))))

(defn- error-data [error]
  {:code (or (:sync/error (ex-data error)) :transport-error) :message (or (.getMessage ^Throwable error) (str (class error)))})
(def ^:dynamic *input-generation* nil)
(defn- run-callbacks [run]
  (let [callbacks (:callbacks run)]
    (if (instance? clojure.lang.IDeref callbacks) @callbacks callbacks)))
(defn- emit! [manager run event]
  (when-let [handler (:events! (run-callbacks run))]
    (try (when (= false (handler event)) (swap! (:state manager) update-in [:metrics :dropped-host-events] (fnil inc 0)))
         (catch Exception error
           (swap! (:state manager) #(-> % (update-in [:metrics :callback-errors] (fnil inc 0))
                                         (assoc :last-error (error-data error))))))))
(defn- queue! [manager run event]
  (locking (:binding-lock run)
    (let [generation (or *input-generation* @(:binding-generation run))]
      (if (or @(:cancelled? run) (not (identical? run (:active @(:state manager))))
              (not= generation @(:binding-generation run))) false
        (if (.offer ^ArrayBlockingQueue (:queue run) (assoc event ::generation generation)) true
          (do (swap! (:state manager) update-in [:metrics :dropped-input-events] (fnil inc 0))
              (when (:source-id event) (swap! (:overflow-sources run) conj (:source-id event))) false))))))
(defn ingest!
  "Enqueue captured data. Returns false on a stopped/full boundary; never blocks
  a device listener. Bridge callbacks use :live/status or :live/midi events."
  [manager event]
  (if-let [run (:active @(:state manager))] (queue! manager run event) false))
(defn ingest-live!
  "Admit a companion bridge callback only for the configured bridge and current
  explicit run handshake. Queued callbacks from an older show/run cannot become
  commands in a new run. This is logical ownership, not network authentication.
  Return false for stale/duplicate/unconnected callbacks or full input queues."
  [manager payload]
  (let [run (:active @(:state manager)) config (get-in @(:state manager) [:config :live])
        kind (when (or (keyword? (:kind payload)) (string? (:kind payload))) (keyword (name (:kind payload))))
        valid? (and run (= 1 (:protocol-version payload)) (:enabled? config)
                    (:live @(:resources run)) (= (:bridge-id config) (:bridge-id payload))
                    (= (:id run) (:sync-session-id payload))
                    (string? (:event-id payload)) (<= 1 (count (:event-id payload)) 256)
                    (case kind :status (map? (:status payload)) :midi (map? (:event payload))
                          :handoff (#{:live :lighting "live" "lighting"} (:mode payload)) false))]
    (if-not valid? false
      (let [event (case kind
                    :status {:type :live/status :status (:status payload) :time-ms ((get-in manager [:options :now-ms]))}
                    :midi {:type :live/midi :event (-> (:event payload) (update :type #(if (string? %) (keyword %) %))
                                                                    (update :origin #(if (string? %) (keyword %) %)))}
                    :handoff (when-let [envelope! (:envelope! (run-callbacks run))]
                               {:type :live/handoff :mode (some-> (:mode payload) name keyword) :envelope (envelope!)}))]
        (locking (:lock manager)
          (let [seen @(:callback-ids run) id (:event-id payload)]
            (if (or (nil? event) (some #{id} seen) (not (identical? run (:active @(:state manager))))) false
                (if (queue! manager run event)
                  (do (reset! (:callback-ids run) (vec (take-last 256 (conj seen id)))) true)
                  false))))))))
(defn record! [manager command acknowledgement]
  (ingest! manager {:type :record/command :command command :ack acknowledgement}))
(defn handoff! [manager mode envelope]
  (ingest! manager {:type :live/handoff :mode (if (string? mode) (keyword mode) mode) :envelope envelope}))
(declare receive-ack!)
(defn acknowledge! [manager note acknowledgement envelope]
  (receive-ack! manager note acknowledgement envelope))

(defn- apply-clock! [manager run event]
  (let [result (locking (:lock manager)
                 (let [result (clock/apply-event (:clock @(:state manager)) event)]
                   (when (:accepted? result) (swap! (:state manager) assoc :clock (:state result))) result))]
    (when (:accepted? result)
      (emit! manager run {:type :sync/clock :snapshot (clock-at manager ((get-in manager [:options :now-ms])))}))))

(defn- commands! [manager run commands]
  (when (seq commands)
    (if-let [handler (:commands! (run-callbacks run))]
      (try (when (= false (handler commands)) (swap! (:state manager) update-in [:metrics :dropped-commands] (fnil + 0) (count commands)))
           (catch Exception error (swap! (:state manager) assoc :last-error (error-data error))))
      (emit! manager run {:type :sync/control-unavailable :commands commands}))))

(defn- bridge-step! [manager step]
  (locking (:lock manager)
    (let [result (step (:bridge @(:state manager)))]
      (swap! (:state manager) assoc :bridge (:state result)) result)))

(defn- receive-ack! [manager note acknowledgement envelope]
  (let [resolved
        (locking (:lock manager)
          (let [state @(:state manager)
                matches? (fn [entry run-id]
                           (and (some? (:command-id acknowledgement))
                                (get-in entry [:held note])
                                (= (:command-id acknowledgement) (get-in entry [:held note :command-id]))
                                (or (nil? (:sync-run-id acknowledgement)) (= run-id (:sync-run-id acknowledgement)))))
                current (:active state)
                retired-index (first (keep-indexed (fn [index entry] (when (matches? (:bridge entry) (:id entry)) index)) (:retired state)))]
            (cond
              (and current (matches? (:bridge state) (:id current)))
              (let [result (ableton/acknowledge-launch (:bridge state) note acknowledgement envelope)]
                (swap! (:state manager) assoc :bridge (:state result)) [current (:commands result)])
              (some? retired-index)
              (let [retired (get-in state [:retired retired-index])
                    result (ableton/acknowledge-launch (:bridge retired) note acknowledgement envelope)]
                (swap! (:state manager) update :retired
                       #(filterv (comp seq :held :bridge) (assoc % retired-index (assoc retired :bridge (:state result)))))
                [retired (:commands result)])
              :else nil)))]
    (when resolved (commands! manager (first resolved) (second resolved)))
    (boolean resolved)))

(defn- release-held! [manager run]
  (when-let [envelope! (:envelope! (run-callbacks run))]
    (try
      (let [envelope (envelope!)
            result (locking (:lock manager)
                     (let [state @(:state manager) result (ableton/handoff (:bridge state) :live envelope)]
                       (swap! (:state manager)
                              #(cond-> (assoc % :bridge (bridge (:config state)))
                                 (seq (get-in result [:state :held]))
                                 (update :retired conj {:id (:id run) :callbacks (run-callbacks run) :bridge (:state result)})))
                       result))]
        (commands! manager run (:commands result)))
      (catch Exception error (emit! manager run {:type :sync/error :error (error-data error)})))))

(defn- enqueue-io! [manager run task]
  (let [generation (or *input-generation* @(:binding-generation run))]
   (try (.execute ^ThreadPoolExecutor (:io run)
                  ^Runnable (fn [] (when (= generation @(:binding-generation run))
                                    (binding [*input-generation* generation] (task))))) true
       (catch RejectedExecutionException _
         (swap! (:state manager) update-in [:metrics :rejected-io] (fnil inc 0)) false))))
(defn- live-request! [manager run id op payload]
  (let [{:keys [transport handle]} (:live @(:resources run))]
    (if-not transport
      (do (emit! manager run {:type :live/execution :id id :op op :status :rejected :error {:code :not-connected}}) false)
      (enqueue-io! manager run
                   (fn []
                     (let [start ((get-in manager [:options :now-ms]))
                           response (try ((:request! transport) handle id op
                                          (cond-> payload (= op :inspect)
                                            (assoc :sync-session-id (:id run)
                                                   :expected-bridge-id (get-in @(:state manager) [:config :live :bridge-id]))))
                                         (catch Exception error {:id id :status :indeterminate :error (error-data error)}))
                           end ((get-in manager [:options :now-ms]))]
                       (queue! manager run {:type :live/response :op op :response response :payload payload
                                            :time-ms (/ (+ start end) 2.0) :received-at-ms end :round-trip-ms (- end start)})
                       (when (= op :inspect) (reset! (:poll-pending? run) false))))))))

(defn place-in-arrangement! [manager beat request-id]
  (when-not (and (finite? beat) (<= 0 beat) (string? request-id) (<= 1 (count request-id) 128))
    (fail! :invalid-placement "Placement requires a nonnegative song beat and a stable request ID"))
  (let [{:keys [active recording]} @(:state manager)]
    (when-not (and active (:destination-id recording)) (fail! :unpaired "Pair a MIDI clip before placing it"))
    (live-request! manager active request-id :place-in-arrangement
                   {:destination-id (:destination-id recording) :song-beat beat})))

(defn- midi-bytes [event]
  (midi-codec/encode
   (merge {:channel (dec (:channel event))}
          (case (:type event)
            :note-on (select-keys event [:type :note :velocity])
            :note-off (select-keys event [:type :note :velocity])
            :cc {:type :control-change :controller (:controller event) :raw-value (:value event)}
            (fail! :invalid-midi "Unsupported Live MIDI event")))))

(defn execute-plan!
  "Explicit outbound executor for a pure ableton/record-command plan. Exactly
  one route is used: :bridge writes via acknowledged LiveAPI; :midi sends bytes
  and reports delivery only. Call on an IO worker, never an effect/CAS transition.
  resources are owned handles; injected transports make this boundary testable."
  [resources route plan recording command acknowledgement]
  (let [id (str (UUID/nameUUIDFromBytes (.getBytes (pr-str [(:show-id command) (:epoch command) (:command-id command)]) "UTF-8")))]
    (case route
      :midi (let [{:keys [transport handle]} (:midi resources)]
              (when-not (:send! transport) (fail! :no-midi-output "MIDI recording output is not connected"))
              ((:send! transport) handle (mapv midi-bytes (:midi plan)))
              {:id id :status :delivered :route :midi :clip-write-confirmed? false})
      :bridge (let [{:keys [transport handle]} (:live resources)]
                (when-not (:request! transport) (fail! :not-connected "Live bridge is not connected"))
                (let [results (mapv (fn [intent]
                                      ((:request! transport) handle (if (= (:command-id intent) (:command-id command)) id
                                                                      (str (UUID/nameUUIDFromBytes
                                                                            (.getBytes (pr-str [id (:command-id intent)]) "UTF-8")))) :record-event
                                       (assoc intent :song-beat (:song-beat recording)
                                              :instance-id (or (:instance-id acknowledgement) (:instance-id command))))) (:intents plan))]
                  {:id id :route :bridge :status (if (every? #(= :executed (:status %)) results) :executed
                                                   (if (some #(= :indeterminate (:status %)) results) :indeterminate :rejected))
                   :results results :clip-write-confirmed? (boolean (and (seq results) (every? #(= :executed (:status %)) results)))}))
      (fail! :invalid-route "Unknown Live recording route"))))

(defn- recording-at [recording at-ms freshness-ms]
  (when (and recording (finite? (:sampled-at-ms recording)) (<= (Math/abs (double (- at-ms (:sampled-at-ms recording)))) freshness-ms))
    (let [bpm (get-in recording [:clock :bpm] 120) playing? (get-in recording [:clock :playing?])
          delta (if playing? (* (/ (- at-ms (:sampled-at-ms recording)) 60000.0) bpm) 0.0)
          beat (when (finite? (:beat recording)) (+ (:beat recording) delta))
          low (:loop-start recording) high (:loop-end recording)
          beat (if (and beat (:looping? recording) (every? finite? [low high]) (< low high)) (+ low (mod (- beat low) (- high low))) beat)]
      (cond-> (assoc recording :beat beat)
        (finite? (:song-beat recording)) (update :song-beat + delta)))))

(defn- peer-key [peer] [(:device-number peer) (:address peer)])
(defn- paired-peer? [config peers peer]
  (when-let [paired (get-in config [:beat-link :peer])]
    (and (= (:device-number paired) (:device-number peer))
         (if-let [mac (:hardware-address paired)]
           (= (str/lower-case mac) (some-> (get peers (peer-key peer)) :hardware-address str/lower-case))
           (= (:address paired) (:address peer))))))

(defn- process-status! [manager run result at-ms round-trip-ms]
  (let [config (:config @(:state manager)) source (get-in config [:live :source-id])
        transport (:clock result)
        recording (assoc result :sampled-at-ms at-ms :round-trip-ms round-trip-ms)]
    (when-not (and (map? result) (finite? at-ms) (<= 0 at-ms)
                   (finite? round-trip-ms) (<= 0 round-trip-ms)
                   (every? #(or (nil? %) (finite? %)) ((juxt :beat :song-beat :loop-start :loop-end) result))
                   (or (nil? transport) (and (map? transport) (finite? (:bpm transport)) (pos? (:bpm transport)))))
      (fail! :invalid-live-status "Invalid Live status timing"))
    (when transport
      (apply-clock! manager run (merge transport {:type :clock :kind :ableton-live :source-id source :time-ms at-ms
                                                :confidence (max 0.0 (- 1.0 (/ round-trip-ms (/ 60000.0 (:bpm transport 120)))))})))
    (when (and (:mode result) (compare-and-set! (:setup-mode? run) false true)
               (not= (keyword (:mode result)) (get-in config [:live :mode])))
      (live-request! manager run (str (UUID/randomUUID)) :handoff {:mode (get-in config [:live :mode])}))
    (when-let [rack (get-in result [:destination :rack-device-index])]
      (let [pair [(:destination-id result) rack]]
        (when (not= pair @(:mirrored-pair run))
          (when-let [pads (:pads (run-callbacks run))]
            (doseq [page (partition-all 64 (pads (get-in config [:live :mappings])))]
              (live-request! manager run (str (UUID/randomUUID)) :pad-page {:pads (vec page)})))
          (reset! (:mirrored-pair run) pair))))
    (swap! (:state manager) assoc :recording recording)
    (emit! manager run {:type :live/status :status recording})))

(defn- midi-control! [manager run event]
  (let [state @(:state manager) callbacks (run-callbacks run) envelope (when-let [provider (:envelope! callbacks)] (provider))]
    (when envelope
      (let [controller (ableton/parameter-controller (:bridge state) event)
            mapping (get-in state [:bridge :parameters controller])
            instance (when (and mapping (not (:disabled? mapping)) (:resolve-instance! callbacks))
                       ((:resolve-instance! callbacks) (:cue-id mapping)))
            result (bridge-step! manager
                                 (fn [current]
                                   (let [resolved (cond-> current instance
                                                    (assoc-in [:parameters controller :instance-id] instance))
                                         result (ableton/midi->commands resolved event envelope)]
                                     (update result :state assoc :parameters (:parameters current)))))
            commands (mapv #(update % :source merge
                                    (cond-> {:sync-run-id (:id run) :source-id (or (:source-id event) (get-in state [:config :live :source-id]))}
                                      (contains? event :note) (assoc :note (:note event)))) (:commands result))]
        (commands! manager run commands)))))

(defn- observe-midi! [manager run event transport at-ms]
  (when (and (#{:note-on :note-off :cc} (:type event)) (not= :tolglow-feedback (:origin event)))
    (let [source (or (:source-id event) (get-in @(:state manager) [:config (if (= transport :live) :live :midi) :source-id]))
          note? (contains? event :note) value (if note? (get event :velocity 0) (:value event))
          off? (or (= :note-off (:type event)) (and (= :note-on (:type event)) (zero? value)))
          source-name (if (keyword? source) (subs (str source) 1) (str source))
          port-id (str (name transport) "/" source-name "/ch/" (:channel event) "/"
                       (if note? "note/" "cc/") (if note? (:note event) (:controller event)))]
      (when-not (and (integer? (:channel event)) (<= 1 (:channel event) 16)
                     (every? #(and (integer? %) (<= 0 % 127)) [value (if note? (:note event) (:controller event))]))
        (fail! :invalid-midi "Malformed MIDI observation"))
      (emit! manager run
             {:type :sync/observation
              :observation {:source-id port-id :event-id (str (:id run) "/observation/" (swap! (:event-sequence run) inc))
                            :observed-at-ms at-ms :kind :midi :sync-run-id (:id run)
                            :port (merge {:transport transport :source-id source :channel (:channel event)
                                          :signal (if note? :velocity :cc)} (select-keys event [:note :controller]))
                            :value (if off? 0 value) :gate? (not off?) :confidence 1.0
                            :origin (if (= transport :live) :live-playback :midi-input)}}))))

(defn- process! [manager run event]
  (let [{:keys [config peers]} @(:state manager) at ((get-in manager [:options :now-ms]))]
    (case (:type event)
      ::rebind
      (locking (:lock manager)
        (when (and (identical? run (:active @(:state manager))) (not @(:cancelled? run)))
          (reset! (:callbacks run) (:callbacks event))
          (reset! (:callback-ids run) [])
          (reset! (:poll-pending? run) false)
          (swap! (:state manager) #(assoc % :bridge (bridge (:config %)) :retired [] :recording nil))))
      :barrier (deliver (:promise event) true)
      :midi/message
      (if-let [decoded (clock/decode-midi (:bytes event))]
        (apply-clock! manager run (merge decoded (select-keys event [:source-id :time-ms])))
        (when (< (first (:bytes event)) 0xf0)
          (let [decoded (midi-codec/decode (:bytes event))
                captured-at (:time-ms event at)
                event (merge decoded {:channel (inc (:channel decoded)) :source-id (:source-id event)}
                             (when (= :control-change (:type decoded)) {:type :cc :value (:raw-value decoded)}))]
            (when (#{:note-on :note-off :cc} (:type event))
              (observe-midi! manager run event :midi captured-at)
              (when (get-in config [:midi :control?]) (midi-control! manager run event))))))
      (:peer/found :peer/lost)
      (let [peer (:peer event) key (peer-key peer)]
        (if (= :peer/found (:type event))
          (swap! (:state manager) update :peers assoc key peer)
          (swap! (:state manager) update :peers dissoc key))
        (when (and (= :peer/lost (:type event)) (paired-peer? config peers peer))
          (apply-clock! manager run {:type :source/lost :source-id (get-in config [:beat-link :source-id]) :time-ms at}))
        (emit! manager run event))
      (:beat-link/beat :beat-link/status)
      (when (paired-peer? config peers (:peer event))
        (when (or (not= :beat-link/beat (:type event)) (:bpm event))
          (apply-clock! manager run (assoc event :align-bars? (get-in config [:beat-link :align-bars?])))))
      :live/status (process-status! manager run (:status event) (get event :time-ms at) (get event :round-trip-ms 0.0))
      :live/midi (do (observe-midi! manager run (:event event) :live (get event :time-ms at))
                    (midi-control! manager run (:event event)))
      :live/acknowledgement
      (receive-ack! manager (:note event) (:ack event) (:envelope event))
      :live/handoff
      (let [result (bridge-step! manager #(ableton/handoff % (:mode event) (:envelope event)))]
        (swap! (:state manager) assoc-in [:config :live :mode] (:mode event))
        (commands! manager run (:commands result))
        (doseq [event (:events result)] (emit! manager run event))
        (when (:live @(:resources run)) (live-request! manager run (str (UUID/randomUUID)) :handoff {:mode (:mode event)})))
      :record/command
      (let [cmd (:command event) ack (:ack event) recording (recording-at (:recording @(:state manager))
                                                                                       (get cmd :time-ms at) (max 1500 (* 3 (get-in config [:live :poll-ms]))))
            plan (bridge-step! manager #(ableton/record-command % cmd ack recording))]
        (when (seq (if (= :midi (get-in config [:live :record-route])) (:midi plan) (:intents plan)))
          (enqueue-io! manager run
                       (fn []
                         (let [result (try (execute-plan! @(:resources run) (get-in config [:live :record-route]) plan recording cmd ack)
                                           (catch Exception error {:status :indeterminate :error (error-data error) :command-id (:command-id cmd)}))]
                           (queue! manager run {:type :live/executed :result result}))))))
      :live/executed (do (swap! (:state manager) assoc :last-execution (:result event))
                         (emit! manager run (assoc (:result event) :type :live/execution)))
      :live/response
      (let [response (:response event)]
        (when (= :executed (:status response))
          (when (#{:inspect :configure-destination} (:op event))
            (process-status! manager run (:result response) (:time-ms event) (:round-trip-ms event))))
        (when (= :place-in-arrangement (:op event)) (swap! (:state manager) assoc :last-execution response))
        (when-not (= :inspect (:op event)) (emit! manager run (assoc response :type :live/execution :op (:op event))))
        (when (and (= :inspect (:op event)) (not= :executed (:status response)))
          (swap! (:state manager) assoc :last-error (:error response))))
      (fail! :invalid-event "Unknown synchronization event"))))

(defn flush!
  "Wait for earlier event-owner work only; outbound IO has separate execution
  acknowledgements. Useful to synchronize injected tests and REPL admission."
  ([manager] (flush! manager 1000))
  ([manager timeout-ms]
   (let [done (promise)]
     (and (ingest! manager {:type :barrier :promise done}) (true? (deref done timeout-ms false))))))

(defn- close-resources! [run]
  (doseq [[_ {:keys [transport handle closed?]}] (reverse (seq @(:resources run)))]
    (when (compare-and-set! closed? false true) (try ((:close! transport) handle) (catch Exception _)))))

(defn rebind!
  "Queue a new host callback boundary without reopening any transport. Old
  queued input and pending IO work are invalidated immediately; the event owner
  clears captured Live held/recording state before admitting subsequent input.
  No joins, transport calls, callbacks or waits run in this function."
  [manager callbacks]
  (when-not (and (ifn? (:events! callbacks))
                 (every? #(or (nil? %) (ifn? %)) ((juxt :commands! :envelope! :resolve-instance!) callbacks)))
    (fail! :invalid-callbacks "Synchronization needs event delivery and optional callable control callbacks"))
  (locking (:lock manager)
    (when (:closed? @(:state manager)) (fail! :closed "Synchronization manager is closed"))
    (if-let [run (:active @(:state manager))]
      (locking (:binding-lock run)
        (if @(:cancelled? run) {:rebound? false :reason :stopping}
          (let [generation (swap! (:binding-generation run) inc)]
            (reset! (:overflow-sources run) #{})
            (.clear ^ArrayBlockingQueue (:queue run))
            (.offer ^ArrayBlockingQueue (:queue run) {:type ::rebind :callbacks callbacks ::generation generation})
            {:rebound? true :pending? true :run-id (:id run)})))
      {:rebound? false :reason :inactive})))

(defn start!
  "Start configured resources explicitly. callbacks require :events! (quick,
  bounded host enqueue); optional :commands!, :envelope!, :resolve-instance!.
  Returns run-id/result promise. start/open failures are reported asynchronously.
  Beat Link status acquisition can wait for peers; stop interrupts that wait."
  [manager callbacks]
  (when-not (and (ifn? (:events! callbacks)) (every? #(or (nil? %) (ifn? %)) ((juxt :commands! :envelope! :resolve-instance!) callbacks)))
    (fail! :invalid-callbacks "Synchronization needs event delivery and optional callable control callbacks"))
  (let [run (locking (:lock manager)
              (when (:closed? @(:state manager)) (fail! :closed "Synchronization manager is closed"))
              (when (:active @(:state manager)) (fail! :busy "Synchronization resources are already active"))
              (when (>= (count (:retired @(:state manager))) 16)
                (fail! :pending-acknowledgements "Resolve pending launch acknowledgements before opening more synchronization sessions"))
              (let [run {:id (str (UUID/randomUUID)) :cancelled? (atom false) :resources (atom {})
                         :queue (ArrayBlockingQueue. (int (get-in manager [:options :queue-capacity])))
                         :setup-mode? (atom false) :mirrored-pair (atom nil)
                         :poll-pending? (atom false) :overflow-sources (atom #{}) :callback-ids (atom []) :event-sequence (atom 0)
                         :callbacks (atom callbacks) :binding-lock (Object.) :binding-generation (atom 0)
                         :thread (atom nil) :result (promise)
                         :io (ThreadPoolExecutor. 1 1 30 TimeUnit/SECONDS (ArrayBlockingQueue. 32)
                               (reify ThreadFactory (newThread [_ task] (doto (Thread. task "tolglow-sync-io") (.setDaemon true))))
                               (ThreadPoolExecutor$AbortPolicy.))}]
                (swap! (:state manager) #(assoc % :active run :status :starting :metrics {} :last-error nil
                                                :bridge (bridge (:config %)) :recording nil)) run))
        worker (doto
                (Thread.
                 (fn []
                   (let [outcome
                         (try
                           (let [config (:config @(:state manager)) options (:options manager)]
                             (doseq [kind [:beat-link :midi :live] :when (and (get-in config [kind :enabled?]) (not @(:cancelled? run)))]
                               (let [factory (get options (keyword (str (name kind) "-factory")))
                                     transport (if (= kind :live) (factory (assoc (get config kind) :sync-session-id (:id run)))
                                                   (factory (get config kind) (:now-ms options)))
                                     handle ((:open! transport) #(queue! manager run %))]
                                 (swap! (:resources run) assoc kind {:transport transport :handle handle :closed? (atom false)})))
                             (when-not @(:cancelled? run)
                               (swap! (:state manager) assoc :status :running)
                               (when (:live @(:resources run))
                                 (reset! (:poll-pending? run) true)
                                 (when-not (live-request! manager run (str (UUID/randomUUID)) :inspect {})
                                   (reset! (:poll-pending? run) false)))
                               (when (and (:live @(:resources run)) (get-in config [:live :pairing]))
                                 (live-request! manager run (str (UUID/randomUUID)) :configure-destination (get-in config [:live :pairing]))))
                             (loop [next-poll 0.0]
                               (when-not @(:cancelled? run)
                                 (let [now ((:now-ms options))
                                       poll? (and (:live @(:resources run)) (>= now next-poll)
                                                  (compare-and-set! (:poll-pending? run) false true))]
                                   (when poll?
                                     (when-not (live-request! manager run (str (UUID/randomUUID)) :inspect {}) (reset! (:poll-pending? run) false)))
                                   (let [[lost _] (swap-vals! (:overflow-sources run) (constantly #{}))]
                                     (doseq [source lost]
                                       (apply-clock! manager run {:type :source/lost :source-id source :time-ms now})
                                       (emit! manager run {:type :sync/input-overflow :source-id source :requires-resync? true})))
                                   (when-let [event (.poll ^ArrayBlockingQueue (:queue run) 20 TimeUnit/MILLISECONDS)]
                                     (try (when (= (::generation event) @(:binding-generation run))
                                            (binding [*input-generation* (::generation event)] (process! manager run event)))
                                          (catch Exception error
                                            (swap! (:state manager) assoc :last-error (error-data error))
                                            (emit! manager run {:type :sync/error :error (error-data error)}))))
                                   (recur (if poll? (+ now (get-in config [:live :poll-ms])) next-poll)))))
                             {:status :stopped})
                           (catch Throwable error (if @(:cancelled? run) {:status :stopped}
                                                      {:status :error :error (error-data error)})))]
                     (release-held! manager run)
                     (reset! (:cancelled? run) true)
                     (close-resources! run)
                     (.shutdownNow ^ThreadPoolExecutor (:io run))
                     (let [terminated? (try (.awaitTermination ^ThreadPoolExecutor (:io run) 1000 TimeUnit/MILLISECONDS)
                                            (catch InterruptedException _ (.isTerminated ^ThreadPoolExecutor (:io run))))]
                       (locking (:lock manager)
                         (when (identical? run (:active @(:state manager)))
                           (swap! (:state manager) assoc :active (when-not terminated? run)
                                  :status (if terminated? (:status outcome) :stopping) :last-error (:error outcome))))
                       (deliver (:result run) (assoc outcome :terminated? terminated?)))))
                 "tolglow-sync-owner") (.setDaemon true))]
    (reset! (:thread run) worker) (.start worker)
    {:run-id (:id run) :result (:result run)}))

(defn stop!
  ([manager] (stop! manager 1500))
  ([manager timeout-ms] (stop! manager timeout-ms ::any))
  ([manager timeout-ms expected-run-id]
   (when-not (and (integer? timeout-ms) (<= 0 timeout-ms 10000)) (fail! :invalid-timeout "Stop timeout must be 0..10000 ms"))
   (if-let [run (let [run (:active @(:state manager))]
                 (when (or (= ::any expected-run-id) (= expected-run-id (:id run))) run))]
     (do (reset! (:cancelled? run) true)
         (let [at ((get-in manager [:options :now-ms]))]
           (locking (:lock manager)
             (let [state (:clock @(:state manager))
                   frozen (reduce (fn [state [id source]]
                                    (if (= id :manual) state
                                        (:state (clock/apply-event state {:type :source/lost :source-id id
                                                                          :time-ms (max at (:last-event-ms source))}))))
                                  state (:sources state))]
               (swap! (:state manager) assoc :clock frozen))))
         (close-resources! run) (.shutdownNow ^ThreadPoolExecutor (:io run))
         (when-let [thread @(:thread run)] (.interrupt ^Thread thread))
         (let [result (try (deref (:result run) timeout-ms nil) (catch InterruptedException _ nil))]
           (when (and result (.isTerminated ^ThreadPoolExecutor (:io run)))
             (locking (:lock manager)
               (when (identical? run (:active @(:state manager))) (swap! (:state manager) assoc :active nil :status (:status result)))))
           {:stopped? (nil? (:active @(:state manager))) :run-id (:id run)}))
     {:stopped? true})))
(defn close! [manager]
  (locking (:lock manager) (swap! (:state manager) assoc :closed? true))
  (assoc (stop! manager) :closed? true))
