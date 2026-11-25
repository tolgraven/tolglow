(ns tolglow.preview.airplay-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [tolglow.preview.airplay :as relay]
            [tolglow.studio.web :as web])
  (:import [java.io ByteArrayInputStream]
           [java.net HttpURLConnection URI]))

(defn- fetch [url method]
  (let [^HttpURLConnection conn (.openConnection (.toURL (URI. url)))]
    (try
      (.setConnectTimeout conn 2000)
      (.setReadTimeout conn 2000)
      (.setRequestMethod conn method)
      (let [code (.getResponseCode conn)]
        [code (when (and (= 200 code) (= "GET" method))
                (with-open [body (.getInputStream conn)] (slurp body)))])
      (finally (.disconnect conn)))))

(defn- buffer [s] (ByteArrayInputStream. (.getBytes ^String s "UTF-8")))

(defn- scenario! [service]
  (let [{:keys [url writer playback-path]} (relay/start! service "127.0.0.1")
        handler (web/app {:airplay-media (partial relay/studio-media service)})
        request (fn [method path]
                  (handler {:request-method method :uri path :scheme :https
                            :headers {"host" "studio.example"}}))
        root (subs url 0 (.lastIndexOf ^String url "/"))
        host (subs root 0 (.lastIndexOf ^String root "/"))
        manifest (str "#EXTM3U\n\n#EXT-X-STREAM-INF:BANDWIDTH=71720,AVERAGE-BANDWIDTH=12072,"
                      "CODECS=\"avc1.640c1e\",RESOLUTION=990x280,FRAME-RATE=30\nplaylist-1.m3u8\n")]
    (is (thrown? Exception (relay/start! service "127.0.0.1")))
    (is (= [404 nil] (fetch url "GET")))
    (is (thrown? Exception (relay/publish! service "stale" "master.m3u8" (buffer manifest))))
    (is (thrown? Exception (relay/publish! service writer "show.clj" (buffer "private"))))
    (is (thrown? Exception (relay/publish! service writer "master.m3u8" (buffer "#EXTM3U\nhttps://example.org/other.m3u8"))))
    (is (= 204 (:status (relay/publish! service writer "master.m3u8" (buffer manifest)))))
    (is (= [200 manifest] (fetch url "GET")))
    (is (str/starts-with? playback-path "/api/preview/airplay/watch/"))
    (is (= manifest (slurp (:body (request :get playback-path)))))
    (is (= [200 nil] ((juxt :status :body) (request :head playback-path))))
    (is (= 404 (:status (request :get (str "/api/preview/airplay/watch/" writer "/master.m3u8")))))
    (is (= 405 (:status (request :put playback-path))))
    (is (= [200 nil] (fetch url "HEAD")))
    (is (= [405 nil] (fetch url "POST")))
    (is (= [404 nil] (fetch (str host "/api/state") "GET")))
    (is (= [404 nil] (fetch (str host "/" writer "/master.m3u8") "GET")))
    (doseq [n (range 1 12)]
      (relay/publish! service writer (str "segment-1-" n ".ts") (buffer (str n))))
    (is (= [404 nil] (fetch (str root "/segment-1-1.ts") "GET")))
    (is (= [200 "11"] (fetch (str root "/segment-1-11.ts") "GET")))
    (is (thrown? Exception (relay/stop! service "stale")))
    (is (= {:status :closed} (relay/stop! service writer)))
    (is (nil? @service))
    (is (= 404 (:status (request :get playback-path))))
    (is (thrown? java.io.IOException (fetch url "GET")))))

(deftest receiver-is-bounded-revocable-and-has-no-show-control-routes
  (let [service (relay/create)]
    (with-redefs [relay/interfaces (constantly [{:interface "lo" :address "127.0.0.1"}])]
      (try
        (is (thrown? Exception (relay/start! service "0.0.0.0")))
        (scenario! service)
        (finally (relay/close! service))))))

(deftest stopped-shares-can-restart-but-a-closed-studio-cannot-reopen
  (let [service (relay/create)
        address "127.0.0.1"
        interface [{:address address}]]
    (with-redefs [relay/interfaces (constantly interface)]
      (let [first-share (relay/start! service address)]
        (relay/stop! service (:writer first-share)))
      (let [{:keys [url writer]} (relay/start! service address)]
        (relay/close! service)
        (relay/close! service)
        (is (= 404 (:status (relay/studio-media service {}))))
        (is (thrown? java.io.IOException (fetch url "GET")))
        (is (thrown? clojure.lang.ExceptionInfo (relay/publish! service writer "segment-1-1.ts" (buffer "x"))))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"This Studio has closed" (relay/start! service address)))))))
