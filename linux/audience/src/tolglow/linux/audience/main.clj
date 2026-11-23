(ns tolglow.linux.audience.main
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [tolglow.linux.audience.bluez :as bluez]
            [tolglow.linux.audience.stage :as stage]
            [tolglow.sensors.osc :as osc])
  (:import [com.google.zxing BarcodeFormat]
           [com.google.zxing.qrcode QRCodeWriter]
           [java.awt.image BufferedImage]
           [java.net DatagramPacket DatagramSocket InetSocketAddress URI]
           [java.nio.charset StandardCharsets]
           [java.util UUID]
           [javax.imageio ImageIO]
           [org.freedesktop.dbus.connections IDisconnectCallback]
           [org.freedesktop.dbus.interfaces DBus$NameOwnerChanged DBusSigHandler]))

(defn invitation [origin room]
  (let [u (URI. origin)]
    (when-not (and (= "https" (.getScheme u)) (.getHost u) (nil? (.getUserInfo u))
                   (nil? (.getQuery u)) (nil? (.getFragment u)) (#{"" "/"} (.getPath u)))
      (throw (ex-info "Use the HTTPS invocation origin of the signed app" {})))
    (str (.resolve u (str "/audience/" room)))))

(defn write-invitation! [origin room file]
  (let [url (invitation origin room) matrix (.encode (QRCodeWriter.) url BarcodeFormat/QR_CODE 512 512)
        img (BufferedImage. 512 512 BufferedImage/TYPE_BYTE_BINARY)]
    (dotimes [y 512]
      (dotimes [x 512] (.setRGB img x y (if (.get matrix x y) 0 0xffffff))))
    (ImageIO/write img "png" (io/file file))
    url))

(defn osc-config [env]
  (let [token (get env "TOLGLOW_AUDIENCE_OSC_TOKEN") raw (get env "TOLGLOW_AUDIENCE_OSC_PORT")
        port (some-> raw parse-long) source (get env "TOLGLOW_AUDIENCE_OSC_ID" "audience")]
    (when (or raw token)
      (when-not (and port (<= 1 port 65535) (string? token) (<= 16 (count token) 512)
                     (<= 1 (count source) 128))
        (throw (ex-info "Set the enrolled OSC source, its token and a valid port" {})))
      {:source source :token token :port (int port) :host (get env "TOLGLOW_AUDIENCE_OSC_HOST" "127.0.0.1")})))

(defn packets [{:keys [source token]} session first-id now levels steps]
  (loop [id first-id remaining steps out []]
    (let [n (min remaining 8)
          gesture #(hash-map :id (str (UUID/randomUUID)) :type :dance-step :phase :pulse)
          packet (osc/encode {:version 1 :device-id source :session-id session :sample-id id
                              :time-ms (* now 1000) :values (assoc levels :dance-steps n)
                              :gestures (vec (repeatedly n gesture)) :pairing-token token})]
      (when (> (alength ^bytes packet) 8192) (throw (ex-info "Oversize OSC observation" {})))
      (if (> remaining n) (recur (inc id) (- remaining n) (conj out packet)) (conj out packet)))))

(defn publication!
  "Advance continuous levels at 5 Hz. Between those updates, only pending
  events are consumed and carry the last published levels for the host mailbox."
  [host {:keys [due levels] :or {due 0}} now]
  (if (>= now due)
    (let [[updated steps] (stage/drain! host now)]
      {:due (+ now 0.2) :levels updated :steps steps
       :send? (or (pos? (:audience-count updated)) (pos? (:audience-count levels 0)) (pos? steps))})
    (let [steps (stage/take-steps! host)]
      {:due due :levels levels :steps steps :send? (pos? steps)})))

(defn publish-loop! [host stopped config ^DatagramSocket socket destination]
  (let [session (str (UUID/randomUUID))]
    (loop [prior {} id 0]
      (when-not (realized? stopped)
        (let [now (bluez/now) {:keys [levels steps send?] :as tick} (publication! host prior now)
              n (:audience-count levels)
              outgoing (when (and config send?) (packets config session id now levels steps))]
          (when (not= n (get-in prior [:levels :audience-count] 0)) (println "Participants" n))
          (doseq [^bytes packet outgoing]
            (.send socket (DatagramPacket. packet (alength packet) ^InetSocketAddress destination)))
          (deref stopped (if config 25 200) nil)
          (recur tick (+ id (count outgoing))))))))

(defn- advertise! [name origin png adapter]
  (when (or (str/blank? name) (> (alength (.getBytes ^String name StandardCharsets/UTF_8)) 248))
    (throw (ex-info "Use a stage name of 1–248 UTF-8 bytes" {})))
  (let [host (stage/create) config (osc-config (System/getenv))
        _ (invitation origin (:room host))
        destination (when config (InetSocketAddress. ^String (:host config) (int (:port config))))
        stopped (promise) hook (Thread. ^Runnable #(deliver stopped :shutdown))]
    (with-open [bus (bluez/open-bus)]
      (.setDisconnectCallback bus (reify IDisconnectCallback
                                    (disconnectOnError [_ _] (deliver stopped :radio-unavailable))))
      (let [owner (.getDBusOwnerName bus "org.bluez")]
        (with-open [observer (.addSigHandler bus DBus$NameOwnerChanged
                              (reify DBusSigHandler
                                (handle [_ signal]
                                  (when (and (= "org.freedesktop.DBus" (.getSource ^DBus$NameOwnerChanged signal))
                                             (= "org.bluez" (.-name ^DBus$NameOwnerChanged signal)) (= owner (.-oldOwner ^DBus$NameOwnerChanged signal)))
                                    (deliver stopped :radio-unavailable)))))]
          (let [{:keys [stop!]} (bluez/register! bus adapter host name stopped)]
            (try
              (.addShutdownHook (Runtime/getRuntime) hook)
              (println "Stage invitation" (write-invitation! origin (:room host) png) "\nQR image" png)
              (if config
                (with-open [socket (DatagramSocket.)] (publish-loop! host stopped config socket destination))
                (publish-loop! host stopped nil nil nil))
              (when (#{:released :radio-unavailable} @stopped)
                (throw (ex-info "Bluetooth stage became unavailable" {})))
              (finally
                (try (.removeShutdownHook (Runtime/getRuntime) hook) (catch IllegalStateException _))
                (stop!)))))))))

(defn -main [& [flag name origin png hci & more]]
  (if-not (and (= "--advertise" flag) name origin png (empty? more) (re-matches #"hci[0-9]+" (or hci "hci0")))
    (binding [*out* *err*]
      (println "Usage: lein run --advertise STAGE HTTPS_INVOCATION_ORIGIN QR.png [hci0]")
      (System/exit 64))
    (try (advertise! name origin png (str "/org/bluez/" (or hci "hci0")))
         (catch Exception e
           (binding [*out* *err*] (println "Stage stopped." (.getMessage e)))
           (System/exit 1)))))
