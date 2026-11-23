(ns tolglow.linux.audience.bluez-test
  (:require [clojure.test :refer [deftest is]]
            [tolglow.linux.audience.bluez :as bluez]
            [tolglow.linux.audience.stage :as stage]
            [tolglow.linux.audience.wire :as wire])
  (:import [java.io BufferedReader InputStreamReader]
           [java.nio ByteBuffer]
           [java.security SecureRandom]
           [java.util Arrays]
           [java.util.concurrent TimeUnit]
           [javax.crypto Cipher]
           [javax.crypto.spec SecretKeySpec GCMParameterSpec]
           [org.bluez GattCharacteristic1 GattManager1 LEAdvertisingManager1]
           [org.freedesktop.dbus DBusPath]
           [org.freedesktop.dbus.connections.base AbstractConnectionBase]
           [org.freedesktop.dbus.interfaces ObjectManager Properties Properties$PropertiesChanged]
           [org.freedesktop.dbus.types UInt16 Variant]))

(def adapter "/org/bluez/hci0")
(defn- mock-managers [receipt]
  (reify GattManager1
    (getObjectPath [_] adapter)
    (RegisterApplication [_ path _] (swap! receipt assoc :app (.getPath path) :owner (.getSource (AbstractConnectionBase/getCallInfo))))
    (UnregisterApplication [_ _] (swap! receipt dissoc :app))
    LEAdvertisingManager1
    (RegisterAdvertisement [_ path _] (swap! receipt assoc :ad (.getPath path)))
    (UnregisterAdvertisement [_ _] (swap! receipt dissoc :ad))))

(defn- sealed [key serial steps]
  (let [sample (-> (ByteBuffer/allocate 11) (.put (byte 1)) (.putInt serial) (.putInt steps)
                   (.put (unchecked-byte 150)) (.put (byte 88)) .array)
        nonce (.generateSeed (SecureRandom.) 12)
        cipher (doto (Cipher/getInstance "AES/GCM/NoPadding")
                 (.init Cipher/ENCRYPT_MODE (SecretKeySpec. key "AES") (GCMParameterSpec. 128 nonce)))]
    (wire/join nonce (.doFinal cipher sample))))

(defn exercise-stage! [on-stage]
  (let [process (.start (ProcessBuilder. ^java.util.List ["dbus-daemon" "--session" "--nofork" "--print-address=1"]))]
    (try
      (with-open [lines (BufferedReader. (InputStreamReader. (.getInputStream process)))]
        (let [address (.readLine lines)]
          (with-open [blue (bluez/open-bus address) app (bluez/open-bus address) other (bluez/open-bus address)]
            (.requestBusName blue "org.bluez")
            (let [calls (atom {}) stage (stage/create) manager (mock-managers calls)
                  _ (.exportObject blue adapter manager)
                  {:keys [stop!]} (bluez/register! app adapter stage "Test floor" (promise))
                  owner (:owner @calls) tree (.getRemoteObject blue owner bluez/root ObjectManager)
                  nodes (.GetManagedObjects ^ObjectManager tree)
                  props (.getRemoteObject blue owner bluez/advert Properties)
                  remote #(.getRemoteObject %1 owner (bluez/char-paths %2) GattCharacteristic1)
                  opts {"device" (Variant. (DBusPath. (str adapter "/dev_AA_BB_CC_DD_EE_FF")))}
                  phone (wire/keypair)
                  public (.ReadValue ^GattCharacteristic1 (remote blue :identity) opts)
                  nonce (.ReadValue ^GattCharacteristic1 (remote blue :identity) (assoc opts "offset" (Variant. (UInt16. 65))))
                  key (wire/session-key phone (Arrays/copyOf public 65) nonce)]
              (try
                (is (= 4 (count nodes)))
                (is (= (:service wire/uuids) (-> nodes (get (DBusPath. bluez/service)) (get "org.bluez.GattService1") (get "UUID") .getValue)))
                (is (= "Test floor" (-> (.GetAll ^Properties props "org.bluez.LEAdvertisement1") (get "LocalName") .getValue)))
                (is (thrown? Exception (.ReadValue ^GattCharacteristic1 (remote other :identity) opts)))
                (.WriteValue ^GattCharacteristic1 (remote blue :hello) (wire/public-key phone) opts)
                (let [char (remote blue :samples) packet (sealed key 2 7)]
                  (.WriteValue ^GattCharacteristic1 char (sealed key 1 6) opts)
                  (.WriteValue ^GattCharacteristic1 char packet opts)
                  (is (thrown? Exception (.WriteValue ^GattCharacteristic1 char packet opts)))
                  (let [[levels steps] (stage/drain! stage (bluez/now))]
                    (is (= [1 88.0 1] [(:audience-count levels) (:pulse-bpm levels) steps]))
                    (on-stage stage #(do (.WriteValue ^GattCharacteristic1 char (sealed key %1 %2) opts)))
                    (.sendMessage blue (Properties$PropertiesChanged.
                                        (str adapter "/dev_AA_BB_CC_DD_EE_FF") "org.bluez.Device1"
                                        {"Connected" (Variant. false)} []))
                    (loop [remaining 100]
                      (when (and (pos? remaining) (seq (:peers @(:state stage))))
                        (Thread/sleep 10)
                        (recur (dec remaining))))
                    (is (empty? (:peers @(:state stage))))
                    (is (thrown? Exception (.WriteValue ^GattCharacteristic1 char (sealed key 4 9) opts)))
                    (println "BlueZ private-bus trace:" (count nodes) "GATT objects, 81-byte identity, 65-byte hello, 39-byte encrypted samples, 1 admitted step")))
                (finally (stop!)))
              (is (nil? (:app @calls))) (is (nil? (:ad @calls)))))))
      (finally (.destroy process) (.waitFor process 2 TimeUnit/SECONDS)))))

(deftest private-dbus-gatt-registration-enrollment-and-withdrawal
  (exercise-stage! (fn [_ _])))
