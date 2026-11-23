(ns tolglow.preview.airplay
  "A short-lived, read-only HLS listener for receivers on a selected network interface."
  (:require [clojure.string :as str]
            [reitit.ring :as ring]
            [ring.adapter.jetty :as jetty])
  (:import [java.io ByteArrayInputStream InputStream]
           [java.net Inet4Address NetworkInterface]
           [java.util UUID Timer TimerTask]
           [org.eclipse.jetty.server NetworkConnector]))

(def ^:private segment-pattern #"segment-1-([1-9][0-9]{0,9})\.ts")
(def ^:private playlists #{"master.m3u8" "playlist-1.m3u8"})
(def ^:private idle-ms 45000)
(defn create [] (atom nil))

(defn interfaces []
  (->> (enumeration-seq (NetworkInterface/getNetworkInterfaces))
       (filter #(and (.isUp %) (not (.isLoopback %))))
       (mapcat (fn [interface]
                 (for [address (enumeration-seq (.getInetAddresses interface))
                       :when (and (instance? Inet4Address address) (not (.isLinkLocalAddress address)))]
                   {:address (.getHostAddress address) :interface (.getName interface)})))
       (sort-by (juxt :interface :address)) vec))

(defn- fail! [status reason] (throw (ex-info reason {:http/status status})))
(defn- segment-number [file] (some->> (re-matches segment-pattern file) second parse-long))
(defn- file-type [file]
  (cond (playlists file) "application/vnd.apple.mpegurl"
        (segment-number file) "video/mp2t"))

(defn- media-response [files capability {:keys [request-method path-params]}]
  (let [{:keys [token file]} path-params
        bytes (when (= capability token) (get @files file))]
    (if bytes
      {:status 200 :headers {"Content-Type" (file-type file)
                             "Content-Length" (str (alength ^bytes bytes))
                             "Cache-Control" "no-store" "X-Content-Type-Options" "nosniff"
                             "Cross-Origin-Resource-Policy" "cross-origin"}
       :body (when (= :get request-method) (ByteArrayInputStream. bytes))}
      {:status 404 :body ""})))

(defn studio-media [service request]
  (let [{:keys [files capability]} @service]
    (if files (media-response files capability request) {:status 404 :body ""})))

(defn- receiver [files capability]
  (let [fetch (partial media-response files capability)]
    (ring/ring-handler
     (ring/router [["/:token/:file" {:get fetch :head fetch}]]
                  {::ring/default-options-endpoint nil})
     (ring/create-default-handler))))

(defn- end-session! [service next-state]
  (let [{:keys [timer server files]} @service]
    (reset! service next-state)
    (when server
      (.cancel ^Timer timer)
      (reset! files {})
      (.stop server))))

(defn close! [service]
  (locking service
    (end-session! service {:disposed? true})))

(defn- check-id! [session writer]
  (when-not (and session (string? writer) (= writer (:writer session)))
    (fail! 404 "Preview receiver session has ended.")))

(defn stop! [service writer]
  (locking service
    (check-id! @service writer)
    (end-session! service nil)
    {:status :closed}))

(defn- expire-task [service session]
  (proxy [TimerTask] []
    (run []
      (locking service
        (when (and (identical? session @service)
                   (> (- (System/nanoTime) @(:last-write session)) (* idle-ms 1000000)))
          (end-session! service nil))))))

(defn start! [service address]
  (let [addresses (interfaces)]
    (when-not (some #(= address (:address %)) addresses)
      (fail! 422 "Select a current network interface reachable from the receiver.")))
  (locking service
    (when (:disposed? @service) (fail! 410 "This Studio has closed."))
    (when @service (fail! 409 "A preview is already being shared from this Studio."))
    (let [capability (str (UUID/randomUUID))
          writer (str (UUID/randomUUID))
          files (atom {})
          web (receiver files capability)
          server (jetty/run-jetty web {:host address :port 0 :join? false
                                       :min-threads 2 :max-threads 12 :acceptors 1 :selectors 1})
          timer (Timer. "Tolglow preview receiver expiry" true)
          ^NetworkConnector connector (first (.getConnectors server))
          port (.getLocalPort connector)
          session {:server server :timer timer :files files :writer writer :capability capability :last-write (atom (System/nanoTime))}]
      (.setStopTimeout server 0)
      (reset! service session)
      (.schedule timer ^TimerTask (expire-task service session) idle-ms idle-ms)
      {:writer writer :url (str "http://" address ":" port "/" capability "/master.m3u8")
       :playback-path (str "/api/preview/airplay/watch/" capability "/master.m3u8")})))

(defn- retain-segments [files]
  (let [expired (->> (keys files) (keep #(when-let [n (segment-number %)] [n %]))
                     (sort-by first >) (drop 8) (map second))]
    (apply dissoc files expired)))

(defn- local-playlist? [bytes]
  (let [[header & rows] (str/split-lines (String. ^bytes bytes java.nio.charset.StandardCharsets/UTF_8))]
    (and (= header "#EXTM3U")
         (every? #(or (str/blank? %)
                      (= % "playlist-1.m3u8")
                      (segment-number %)
                      (re-matches #"#EXT-X-(VERSION|TARGETDURATION|MEDIA-SEQUENCE):[0-9]+" %)
                      (= % "#EXT-X-INDEPENDENT-SEGMENTS") (= % "#EXT-X-ENDLIST")
                      (re-matches #"#EXTINF:[0-9]+(?:\.[0-9]+)?," %)
                      (and (str/starts-with? % "#EXT-X-STREAM-INF:")
                           (re-matches #"[A-Za-z0-9=:.,\x22-]+" (subs % (count "#EXT-X-STREAM-INF:")))
                           (not (str/includes? % "URI="))))
                 rows))))

(defn publish! [service writer file ^InputStream input]
  (let [session @service
        content-type (file-type file)]
    (check-id! session writer)
    (when-not content-type (fail! 404 "Unknown preview media file."))
    (let [limit (if (playlists file) 8192 (* 4 1024 1024))
          bytes (.readNBytes input (int (inc limit)))]
      (when (> (alength bytes) limit) (fail! 413 "Preview segment exceeds its byte limit."))
      (when (zero? (alength bytes)) (fail! 422 "Empty preview media file."))
      (when (and (playlists file) (not (local-playlist? bytes)))
        (fail! 422 "Playlist must reference only this receiver’s preview segments."))
      (locking service
        (check-id! @service writer)
        (reset! (:last-write session) (System/nanoTime))
        (swap! (:files session) #(retain-segments (assoc % file bytes))))
      {:status 204 :body nil})))
