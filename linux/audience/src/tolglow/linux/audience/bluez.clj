(ns tolglow.linux.audience.bluez
  "A GATT application and advertisement exported on the system bus only on start."
  (:require [tolglow.linux.audience.wire :as wire]
            [tolglow.linux.audience.stage :as stage])
  (:import [org.bluez GattCharacteristic1 GattService1 GattManager1 LEAdvertisement1 LEAdvertisingManager1]
           [org.freedesktop.dbus DBusPath]
           [org.freedesktop.dbus.connections.base AbstractConnectionBase]
           [org.freedesktop.dbus.connections.impl DBusConnection DBusConnectionBuilder]
           [org.freedesktop.dbus.errors AccessDenied NotSupported PropertyReadOnly UnknownProperty]
           [org.freedesktop.dbus.interfaces DBusSigHandler ObjectManager Properties Properties$PropertiesChanged]
           [org.freedesktop.dbus.types Variant]))

(def root "/org/tolglow/audience")

(def service (str root "/service0"))

(def advert "/org/tolglow/advertisement")

(def char-paths {:identity (str service "/char0") :hello (str service "/char1") :samples (str service "/char2")})

(defn now [] (/ (System/nanoTime) 1.0e9))

(defn- value [m k] (some-> ^Variant (get m k) .getValue))

(defn- deny [] (throw (AccessDenied. "Audience gateway rejected the request")))

(defn- unsupported [] (throw (NotSupported. "Operation unavailable")))

(defn- readonly [] (throw (PropertyReadOnly. "GATT properties are read-only")))

(defn- props [properties iface] (get properties iface {}))

(defn- prop [properties iface k] (or (get-in properties [iface k]) (throw (UnknownProperty. k))))

(defn- sender [] (some-> (AbstractConnectionBase/getCallInfo) .getSource))

(defn- device [owner adapter options]
  (let [path (some-> ^DBusPath (value options "device") .getPath)]
    (when-not (and (= owner (sender)) path (.startsWith path (str adapter "/dev_"))
                   (re-matches #"/org/bluez/hci[0-9]+/dev_[0-9A-Fa-f]{2}(?:_[0-9A-Fa-f]{2}){5}" path))
      (deny))
    path))

(defn- flags [values] (Variant. (into-array String values) "as"))

(defn- characteristic [owner adapter host kind properties]
  (let [path (char-paths kind)]
    (reify GattCharacteristic1
      (getObjectPath [_] path)
      (ReadValue [_ opts]
        (let [id (device owner adapter opts) offset (long (or (value opts "offset") 0))]
          (when-not (= kind :identity) (unsupported))
          (try (stage/identity! host id offset (now)) (catch Exception _ (deny)))))
      (WriteValue [_ bytes opts]
        (let [id (device owner adapter opts)]
          (when-not (and (zero? (long (or (value opts "offset") 0))) (not (value opts "prepare-authorize")))
            (unsupported))
          (try
            (case kind :hello (stage/hello! host id bytes (now))
                       :samples (stage/receive! host id bytes (now))
                       (unsupported))
            (catch Exception _ (deny)))))
      (AcquireWrite [_ _] (unsupported)) (AcquireNotify [_ _] (unsupported))
      (StartNotify [_] (unsupported)) (StopNotify [_] (unsupported)) (Confirm [_] (unsupported))
      Properties
      (Get [_ iface k] (prop properties iface k))
      (GetAll [_ iface] (props properties iface))
      (Set [_ _ _ _] (readonly)))))

(defn- objects [owner adapter host name stopped]
  (let [gatt {"org.bluez.GattService1" {"UUID" (Variant. (:service wire/uuids)) "Primary" (Variant. true)}}
        char-props (fn [k] {"org.bluez.GattCharacteristic1"
                            {"UUID" (Variant. (k wire/uuids)) "Service" (Variant. (DBusPath. service))
                             "Flags" (flags [(if (= k :identity) "read" "write")])}})
        nodes (into {(DBusPath. service) gatt} (map (fn [[k p]] [(DBusPath. p) (char-props k)]) char-paths))
        adv {"org.bluez.LEAdvertisement1" {"Type" (Variant. "peripheral") "LocalName" (Variant. name)
                                           "ServiceUUIDs" (flags [(:service wire/uuids)])}}
        app (reify ObjectManager (getObjectPath [_] root) (GetManagedObjects [_] nodes))
        srv (reify GattService1 (getObjectPath [_] service)
              Properties (Get [_ i k] (prop gatt i k)) (GetAll [_ i] (props gatt i)) (Set [_ _ _ _] (readonly)))
        ad (reify LEAdvertisement1 (getObjectPath [_] advert)
             (Release [_] (when (= owner (sender)) (deliver stopped :released)))
             Properties (Get [_ i k] (prop adv i k)) (GetAll [_ i] (props adv i)) (Set [_ _ _ _] (readonly)))]
    (into {root app service srv advert ad}
          (map (fn [[k path]] [path (characteristic owner adapter host k (char-props k))]) char-paths))))

(defn open-bus
  ([] (.build (.withShared (DBusConnectionBuilder/forSystemBus) false)))
  ([address] (.build (.withShared (DBusConnectionBuilder/forAddress ^String address) false))))

(defn- radio-events [^DBusConnection bus owner adapter host stopped]
  (.addSigHandler bus Properties$PropertiesChanged ^String owner
    (reify DBusSigHandler
      (handle [_ s]
        (let [^Properties$PropertiesChanged signal s
              path (.getPath signal) iface (.getInterfaceName signal) changes (.getPropertiesChanged signal)]
          (cond
            (and (= adapter path) (= "org.bluez.Adapter1" iface) (false? (value changes "Powered")))
            (deliver stopped :radio-unavailable)
            (and (= "org.bluez.Device1" iface) (.startsWith path (str adapter "/dev_"))
                 (false? (value changes "Connected")))
            (stage/leave! host path)))))))

(defn register!
  "The caller owns the bus. Closing it also withdraws every exported object.
  BlueZ must own both manager interfaces on the chosen powered peripheral adapter."
  [^DBusConnection bus adapter host name stopped]
  (when-not (re-matches #"/org/bluez/hci[0-9]+" adapter) (throw (ex-info "Invalid BlueZ adapter" {})))
  (let [owner (.getDBusOwnerName bus "org.bluez")
        gatt (.getRemoteObject bus owner adapter GattManager1)
        ads (.getRemoteObject bus owner adapter LEAdvertisingManager1)
        root-path (DBusPath. root) ad-path (DBusPath. advert)
        ;; Hold strong references even when the transport uses weak export registries.
        all (objects owner adapter host name stopped)
        events (radio-events bus owner adapter host stopped)]
    (try
      (doseq [[path obj] all] (.exportObject bus ^String path obj))
      (.RegisterApplication ^GattManager1 gatt root-path {})
      (try (.RegisterAdvertisement ^LEAdvertisingManager1 ads ad-path {})
           (catch Exception e (.UnregisterApplication ^GattManager1 gatt root-path) (throw e)))
      {:owner owner :objects all
       :stop! (fn []
                (try (.UnregisterAdvertisement ^LEAdvertisingManager1 ads ad-path)
                     (finally
                       (try (.UnregisterApplication ^GattManager1 gatt root-path)
                            (finally (.close ^java.lang.AutoCloseable events)
                                     (doseq [path (keys all)] (.unExportObject bus path)))))))}
      (catch Exception e (.close ^java.lang.AutoCloseable events) (doseq [path (keys all)] (.unExportObject bus path)) (throw e)))))
