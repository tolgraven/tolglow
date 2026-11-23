(ns rehearse
  (:require [clojure.data.json :as json]
            [tolglow.linux.audience.bluez :as bluez]
            [tolglow.linux.audience.stage :as stage]
            [tolglow.linux.audience.main :as gateway]
            [tolglow.linux.audience.wire :as wire]
            [tolglow.sensors :as sensors])
  (:import [java.io BufferedReader InputStreamReader]
           [java.net DatagramSocket InetSocketAddress]
           [java.nio ByteBuffer]
           [java.security SecureRandom]
           [java.util Arrays UUID]
           [java.util.concurrent Callable ExecutorService Executors TimeUnit]
           [javax.crypto Cipher]
           [javax.crypto.spec GCMParameterSpec SecretKeySpec]
           [org.bluez GattCharacteristic1 GattManager1 LEAdvertisingManager1]
           [org.freedesktop.dbus DBusPath]
           [org.freedesktop.dbus.connections.base AbstractConnectionBase]
           [org.freedesktop.dbus.interfaces Properties$PropertiesChanged]
           [org.freedesktop.dbus.types Variant]))

(def adapter "/org/bluez/hci0")
(def source "rehearsal-floor")
(def random (SecureRandom.))
(def guide-rev "e486dd9ef7377a8239aed4844500cf2edf16c9b3")

(defn managers [owner]
  (reify GattManager1
    (getObjectPath [_] adapter)
    (RegisterApplication [_ _ _] (deliver owner (.getSource (AbstractConnectionBase/getCallInfo))))
    (UnregisterApplication [_ _])
    LEAdvertisingManager1
    (RegisterAdvertisement [_ _ _])
    (UnregisterAdvertisement [_ _])))

(defn batch [^ExecutorService pool f items]
  (let [tasks (mapv (fn [item] (reify Callable (call [_] (f item)))) items)]
    (mapv #(.get ^java.util.concurrent.Future %) (.invokeAll pool tasks))))

(defn join [chars i]
  (let [path (format "%s/dev_02_00_00_00_00_%02X" adapter i)
        opts {"device" (Variant. (DBusPath. path))}
        local (wire/keypair)
        identity (.ReadValue ^GattCharacteristic1 (:identity chars) opts)
        key (wire/session-key local (Arrays/copyOfRange identity 0 65) (Arrays/copyOfRange identity 65 81))]
    (.WriteValue ^GattCharacteristic1 (:hello chars) (wire/public-key local) opts)
    {:index i :path path :opts opts :key key}))

(defn sealed [{:keys [index key]} serial steps share?]
  (let [axes? (and share? (zero? (mod index 3)))
        pulse (if (and share? (zero? (mod index 4))) (+ 90 (mod index 5)) 0)
        strength (if (zero? index) 255 0)
        b (doto (ByteBuffer/allocate (if axes? 35 11)) (.put (byte (if axes? 2 1)))
            (.putInt serial) (.putInt steps) (.put (unchecked-byte strength)) (.put (byte pulse)))
        nonce (byte-array 12)]
    (when axes? (doseq [v [2.5 -1.75 0.5 -0.25 0.125 3.0]] (.putFloat b (float v))))
    (.nextBytes random nonce)
    (let [cipher (doto (Cipher/getInstance "AES/GCM/NoPadding")
                   (.init Cipher/ENCRYPT_MODE (SecretKeySpec. key "AES") (GCMParameterSpec. 128 nonce)))]
      (wire/join nonce (.doFinal cipher (.array b))))))

(defn share [chars peer serial steps share?]
  (.WriteValue ^GattCharacteristic1 (:samples chars) (sealed peer serial steps share?) (:opts peer)))

(defn leave [bus peer]
  (.sendMessage bus (Properties$PropertiesChanged. (:path peer) "org.bluez.Device1" {"Connected" (Variant. false)} [])))

(defn until [label f]
  (let [deadline (+ (System/nanoTime) 4000000000)]
    (loop [] (or (f) (if (< (System/nanoTime) deadline) (do (Thread/sleep 5) (recur))
                          (throw (ex-info (str "Timed out waiting for " label) {})))))))

(defn reading [manager n]
  (until (str n " dancers")
    #(let [values (:values (sensors/latest manager source))]
       (when (and (number? (:audience-count values)) (== n (:audience-count values))) values))))

(defn verify [expected actual]
  (when-not (= expected actual)
    (throw (ex-info "Audience rehearsal failed" {:expected expected :actual actual}))))

(defn observer [manager initial]
  (let [stop (promise)
        state (atom {:model initial :total 0 :frames 0})
        epoch (System/nanoTime)
        task (future
               (loop [due epoch]
                 (when-not (realized? stop)
                   (let [ms (/ (- (System/nanoTime) epoch) 1e6)
                         result (sensors/advance! manager (:model @state)
                                  {:time-ms ms :beat (/ ms 500.0) :meter {:beats-per-bar 4 :bars-per-phrase 4}})
                         next-tick (max (+ due 25000000) (System/nanoTime))]
                     (verify true (every? #(= :accent (:cue-id %)) (:commands result)))
                     (swap! state #(-> % (assoc :model (:model result))
                                         (update :frames inc) (update :total + (count (:commands result)))))
                     (deref stop (long (max 1 (/ (- next-tick (System/nanoTime)) 1e6))) nil)
                     (recur next-tick)))))]
    {:state state :stop! #(do (deliver stop true) @task)}))

(defn onsets [chars pool peers state beat]
  (let [start (System/nanoTime) expected (* beat (count peers))]
    (batch pool #(share chars % (+ beat 2) beat true) peers)
    (until (str expected " admitted cue launches") #(>= (:total @state) expected))
    (verify expected (:total @state))
    (let [elapsed (/ (- (System/nanoTime) start) 1e6)]
      (Thread/sleep (long (max 0 (- 500 elapsed))))
      {:beat beat :cumulative-cues expected :all-cues-ms elapsed})))

(defn check-crowd [manager]
  (let [crowd (reading manager 96)]
    (verify [96 24 32] (mapv #(long (crowd %)) [:audience-count :pulse-count :motion-count]))
    (verify true (< (abs (- (/ 1.0 96) (:dance-effort crowd))) 1e-7))
    (verify true (< (abs (+ (:accel-y crowd) 1.75)) 1e-7))
    crowd))

(defn withdraw [manager bus chars pool peers]
  (batch pool #(leave bus %) (rest peers))
  (let [solo (first peers)]
    (share chars solo 11 8 false)
    (let [returned (reading manager 1)]
      (verify [1 0 0] (mapv #(long (returned %)) [:audience-count :pulse-count :motion-count]))
      (verify [nil nil] ((juxt :pulse-bpm :accel-y) returned))
      (leave bus solo)
      {:returned-to-one returned :last (reading manager 0)})))

(def mapping
  {:version 1 :timeout-ms 1200 :channels []
   :gestures [{:id :accent :type :dance-step
               :command {:op :cue/launch :cue-id :accent
                         :launch {:mode :one-shot :duration {:beats 1/8}}}}]})

(defn input-report [manager pairing bus chars pool]
  (let [peers (batch pool (partial join chars) (range 96))
        solo (first peers)]
    (share chars solo 1 0 true)
    (let [first-reading (reading manager 1)
          configured (:model (sensors/configure {:time-ms 0 :transport {:beat 0}} source mapping pairing))
          session (:session-id (sensors/latest manager source))
          armed (:model (sensors/arm! manager configured source session true))
          {:keys [state stop!]} (observer manager armed)]
      (try
        (batch pool #(share chars % 2 0 true) peers)
        (let [crowd (check-crowd manager)
              rounds (mapv (partial onsets chars pool peers state) (range 1 9))
              exit-readings (withdraw manager bus chars pool peers)
              metrics (:metrics (sensors/status manager))]
          (verify 768 (:total @state))
          (verify [0 0] ((juxt :rejected :overflow) metrics))
          (merge exit-readings
                 {:tolglow guide-rev :afterglow "14abdf9630baba74bbf70029a07609a14686eb01"
                  :transport "private dbus-daemon, real exported GATT characteristics, encrypted samples and loopback paired UDP"
                  :simulated-clients 96 :parallel-writes 4 :radio-tested false :tempo-bpm 120
                  :solo first-reading :crowd crowd :rounds rounds :host-frames (:frames @state)
                  :admission metrics :expected-cue-launches 768 :admitted-cue-launches (:total @state)}))
        (finally (stop!))))))

(defn run [out]
  (let [process (.start (ProcessBuilder. ^java.util.List ["dbus-daemon" "--session" "--nofork" "--print-address=1"]))
        pool (Executors/newFixedThreadPool 4)
        secret (atom {}) store {:put! (fn [v] (let [k (str "credential-" (UUID/randomUUID))] (swap! secret assoc k v) k))
                                :get! #(get @secret %) :delete! #(swap! secret dissoc %)}
        manager (sensors/create-manager {:credential-store store})]
    (try
      (let [enrolled (sensors/pair! manager source "Floor")
            port (:port (sensors/start-osc! manager {:host "127.0.0.1" :port 0}))
            to (InetSocketAddress. "127.0.0.1" (int port))]
        (with-open [lines (BufferedReader. (InputStreamReader. (.getInputStream process)))]
          (let [address (.readLine lines)]
            (with-open [blue (bluez/open-bus address) app (bluez/open-bus address) socket (DatagramSocket.)]
              (.requestBusName blue "org.bluez")
              (let [owner (promise) m (managers owner) host (stage/create) stopped (promise)]
                (.exportObject blue adapter m)
                (let [{:keys [stop!]} (bluez/register! app adapter host "Floor" stopped)
                      chars (update-vals bluez/char-paths #(.getRemoteObject blue @owner % GattCharacteristic1))
                      publisher (future (gateway/publish-loop! host stopped {:source source :token (:pairing-token enrolled)} socket to))]
                  (try (let [result (input-report manager (:pairing enrolled) blue chars pool)]
                         (spit out (json/write-str result :indent true)) (println "Verified" (:admitted-cue-launches result) "cue launches"))
                       (finally (deliver stopped :done) @publisher (stop!)))))))))
      (finally (sensors/close! manager) (.shutdownNow pool) (.destroy process) (.waitFor process 2 TimeUnit/SECONDS)))))

(try (run (or (System/getenv "AUDIENCE_REPORT") "/tmp/gatt-crowd.json")) (finally (shutdown-agents)))
