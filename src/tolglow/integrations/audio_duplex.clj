(ns tolglow.integrations.audio-duplex
  "Explicit, bounded multichannel PCM connection. Mixer IO, sine generation and
  analysis have dedicated workers. A slow analyzer drops old input windows."
  (:require [tolglow.integrations.audio-return :as returns])
  (:import [javax.sound.sampled AudioFormat AudioSystem DataLine$Info SourceDataLine TargetDataLine]
           [java.util UUID]
           [java.util.concurrent ArrayBlockingQueue TimeUnit]))

(defn mixers []
  (mapv (fn [info] (let [mixer (AudioSystem/getMixer info)]
                     {:name (.getName info) :send? (pos? (count (.getSourceLineInfo mixer)))
                      :return? (pos? (count (.getTargetLineInfo mixer)))}))
        (AudioSystem/getMixerInfo)))

(defn- mixer [label]
  (let [matching (filter #(= label (.getName %)) (AudioSystem/getMixerInfo))]
    (when-not (= 1 (count matching)) (throw (ex-info "Choose an unambiguous available mixer" {:name label})))
    (AudioSystem/getMixer (first matching))))

(defn open-java! [{:keys [sample-rate channels block-frames input-mixer output-mixer]}]
  (let [format (AudioFormat. (float sample-rate) 16 (int channels) true false)
        size (int (* block-frames channels 2 6))
        ^SourceDataLine send (.getLine (mixer output-mixer) (DataLine$Info. SourceDataLine format))]
    (try
      (.open send format size)
      (let [^TargetDataLine receive (.getLine (mixer input-mixer) (DataLine$Info. TargetDataLine format))]
        (try
          (.open receive format size)
          (.start receive)
          (.start send)
          {:write! (fn [^bytes data offset n] (.write send data offset n))
           :read! (fn [^bytes data offset n] (.read receive data offset n))
           :close! #(try (.close receive) (finally (.close send)))}
          (catch Throwable e (.close receive) (throw e))))
      (catch Throwable e (.close send) (throw e)))))

(defn encode-channel! [^bytes buffer ^doubles samples channels channel]
  (dotimes [i (alength samples)]
    (let [offset (* 2 (+ (* i channels) (dec channel)))
          signal (long (Math/round (double (* 32767.0 (max -1.0 (min 1.0 (aget samples i)))))))]
      (aset-byte buffer offset (unchecked-byte signal))
      (aset-byte buffer (inc offset) (unchecked-byte (bit-shift-right signal 8)))))
  buffer)

(defn decode-channel [^bytes buffer channels channel]
  (let [samples (double-array (quot (alength buffer) (* channels 2)))]
    (dotimes [i (alength samples)]
      (let [offset (* 2 (+ (* i channels) (dec channel)))
            signed (+ (bit-and 255 (aget buffer offset)) (bit-shift-left (int (aget buffer (inc offset))) 8))]
        (aset-double samples i (/ signed 32768.0))))
    samples))

(defn- transfer! [transfer ^bytes buffer]
  (let [length (alength buffer)]
    (loop [offset 0]
      (when (< offset length)
        (let [n (transfer buffer offset (- length offset))]
          (when-not (and (integer? n) (<= 1 n (- length offset)))
            (throw (ex-info "Audio stream closed during PCM transfer" {})))
          (recur (+ offset n)))))))

(defn create [] (atom {:status :stopped}))

(defn status [manager] (select-keys @manager [:status :session :error :dropped-windows]))

(defn- close-once [endpoint]
  (when-let [e (first (swap-vals! endpoint (constantly nil)))]
    ((:close! e))))

(defn stop!
  ([manager] (stop! manager (:session @manager)))
  ([manager expected-session]
   (let [prior (locking manager (when (= expected-session (:session @manager))
                                  (first (reset-vals! manager {:status :stopped}))))]
     (when-let [stop (:stop prior)] (stop))
     (status manager))))

(defn- daemon! [name f]
  (doto (Thread. ^Runnable f name) (.setDaemon true) (.start)))

(defn- run-session!
  [{:keys [manager config session current-model identity now-ms active? endpoint open!]}]
  (let [{:keys [block-frames channels sample-rate lanes]} config
        size (* 2 block-frames channels)
        queue (ArrayBlockingQueue. 1)
        sent (atom {}) drained (atom {})
        failed! (fn [error]
                  (when (= session (:session @manager))
                    (swap! manager #(if (= session (:session %))
                                      (assoc % :status :failed :error (.getMessage ^Throwable error) :packet nil) %))))
        worker! (fn [label f]
                  (daemon! (str "tolglow-audio-return-" label)
                           (fn [] (try (f)
                                       (catch Exception e (when (active?) (failed! e)))
                                       (finally (close-once endpoint))))))]
    (try
      (let [io (open! config)]
        (reset! endpoint io)
        (when (active?)
          (swap! manager #(if (= session (:session %)) (assoc % :status :running) %))
          (worker! "carrier"
            (fn []
              (loop [states {}]
                (when (active?)
                  (let [inputs (returns/dry-instances (current-model)) out (byte-array size)
                        next-states
                        (reduce (fn [acc lane]
                                  (let [id (:id lane)
                                        block (returns/render-dry (get inputs (:dry-cue lane)) lane (get states id) (get @drained id) sample-rate block-frames)]
                                    (encode-channel! out (:samples block) channels (:send-channel lane))
                                    (let [state (:state block)]
                                      (assoc acc id (cond-> state
                                                      (and (:binding state) (nil? (:source-id state)))
                                                      (assoc :released-at-ms
                                                             (or (get-in states [id :released-at-ms]) (now-ms)))))))) {} lanes)]
                    (transfer! (:write! io) out)
                    (reset! sent next-states)
                    (recur next-states))))))
          (worker! "input"
            (fn []
              (loop [sequence 1]
                (when (active?)
                  (let [bytes (byte-array size)]
                    (transfer! (:read! io) bytes)
                    (let [block {:bytes bytes :dry @sent :sequence sequence :observed-at-ms (now-ms)}]
                      (when-not (.offer queue block)
                        (when (.poll queue)
                          (swap! manager #(if (= session (:session %)) (update % :dropped-windows inc) %)))
                        (.offer queue block)))
                    (recur (inc sequence)))))))
          (loop [states {}]
            (when (active?)
              (if-let [{:keys [bytes dry sequence observed-at-ms]} (.poll queue 100 TimeUnit/MILLISECONDS)]
                (let [model (current-model)
                      results (mapv (fn [lane]
                                      (let [id (:id lane) pcm (decode-channel bytes channels (:return-channel lane))]
                                        [id (returns/analyze-return model lane (get states id) (get dry id) pcm sample-rate
                                                                    observed-at-ms)])) lanes)
                      packet (merge identity
                                    {:observed-at-ms observed-at-ms :fresh? true
                                     :windows (into {} (map (fn [[id r]] [id (assoc (:window r) :sequence sequence)])) results)})]
                  (reset! drained (into {} (map (fn [[id r]] [id (:drained-id r)])) results))
                  (swap! manager #(if (and (= session (:session %)) (= :running (:status %)))
                                    (assoc % :packet packet) %))
                  (recur (into {} (map (fn [[id r]] [id (:state r)])) results)))
                (recur states))))))
      (catch Exception e (when (active?) (failed! e)))
      (finally
        (close-once endpoint)
        (swap! manager #(if (and (= session (:session %)) (not= :failed (:status %))) {:status :stopped} %))))))

(defn start! [manager model current-model now-ms open!]
  (let [config (returns/validate-config model (get-in model [:settings :audio-returns]))
        _ (when (empty? (:lanes config)) (throw (ex-info "Add an audio lane before connecting" {})))
        identity (select-keys model [:epoch :show-id])
        settings (get-in model [:settings :audio-returns])
        session (str (UUID/randomUUID)) endpoint (atom nil)
        current? (fn [] (let [m (current-model)]
                          (and (= (:epoch identity) (:epoch m)) (= (:show-id identity) (:show-id m))
                               (= settings (get-in m [:settings :audio-returns])))))
        active? #(and (= session (:session @manager)) (#{:starting :running} (:status @manager)) (current?))]
    (locking manager
      (when (#{:starting :running} (:status @manager)) (throw (ex-info "Audio return already running" {})))
      (reset! manager {:session session :status :starting :config config :settings settings :dropped-windows 0
                       :stop #(close-once endpoint)})
      (daemon! "tolglow-audio-duplex"
               #(run-session! {:manager manager :session session :config config :current-model current-model
                               :identity identity :now-ms now-ms :open! open! :active? active? :endpoint endpoint})))
    (status manager)))

(defn frame-input! [manager]
  (let [s @manager]
    (when (= :running (:status s))
      (when (get-in s [:packet :fresh?])
        (swap! manager #(if (identical? (:packet s) (:packet %)) (assoc-in % [:packet :fresh?] false) %)))
      (select-keys s [:session :config :settings :packet]))))
