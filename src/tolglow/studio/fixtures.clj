(ns tolglow.studio.fixtures
  "Explicit OFL catalog/fetch boundary with a portable, bounded offline cache.
  Search and import never publish definitions, patch a rig or activate output.
  API schemas verified against OFL ui/api/openapi.json and routes on 2026-11-21."
  (:refer-clojure :exclude [bytes])
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [tolglow.integrations.fixtures :as fixtures]
            [tolglow.integrations.gdtf :as gdtf])
  (:import [java.io ByteArrayOutputStream InputStream]
           [java.net URI InetSocketAddress ProxySelector]
           [java.net.http HttpClient HttpClient$Redirect HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers]
           [java.nio.charset StandardCharsets]
           [java.security MessageDigest]
           [java.time Duration]
           [java.util.concurrent Executors ThreadFactory TimeUnit]))

(def ofl-base "https://open-fixture-library.org")
(def ofl-api-reference "https://github.com/OpenLightingProject/open-fixture-library/blob/master/docs/rest-api.md")
(def ofl-license
  {:spdx "MIT" :url "https://github.com/OpenLightingProject/open-fixture-library/blob/master/LICENSE"
   :notice "MIT License\n\nCopyright (c) 2017 Florian & Felix Edelmann\n\nPermission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the \"Software\"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:\n\nThe above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.\n\nTHE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE."})

(defn- fail! [message data] (throw (ex-info message data)))
(defn- bytes [text] (.getBytes ^String text StandardCharsets/UTF_8))
(defn- digest [text]
  (apply str (map #(format "%02x" (bit-and % 255))
                  (.digest (MessageDigest/getInstance "SHA-256") (bytes text)))))
(defn- key! [value fixture?]
  (when-not (and (string? value)
                 (re-matches (if fixture? #"[a-z0-9-]+/[a-z0-9-]+" #"[a-z0-9-]+") value))
    (fail! "Invalid OFL catalog key" {:key value :fixture? fixture?})) value)

(defn new-library
  ([] (new-library {}))
  ([options]
   (let [limits (merge {:max-catalog-entries 20000 :max-documents 64 :max-cache-bytes (* 16 1024 1024)
                        :max-response-bytes (* 4 1024 1024)} options)]
     (doseq [[key value] limits]
       (when-not (and (integer? value) (pos? value)) (fail! "Cache limits must be positive integers" {:key key})))
     {:version 1 :limits limits :manufacturers {} :entries {} :documents {} :revision 0})))

(defn- read-bounded [^InputStream stream limit]
  (let [buffer (byte-array 8192) output (ByteArrayOutputStream.)]
    (loop [size 0]
      (let [n (.read stream buffer)]
        (if (= -1 n) (.toByteArray output)
            (let [size (+ size n)]
              (when (> size limit) (fail! "Fixture response exceeds byte limit" {:limit limit}))
              (.write output buffer 0 n)
              (recur size)))))))

(defn ofl-provider
  "Create an explicit bounded HTTP provider. No request occurs until its returned
  function is invoked. Request maps contain :method, :path, optional :body and
  :max-bytes. Redirects are rejected, and all paths stay on the fixed OFL origin.
  :proxy-url defaults to HTTPS_PROXY; explicit nil disables it. A whole-call
  deadline closes a stalled response body. Mutable resources are request-owned."
  ([] (ofl-provider {}))
  ([{:keys [timeout-ms] :or {timeout-ms 10000} :as options}]
   (when-not (and (integer? timeout-ms) (<= 1 timeout-ms 60000))
     (fail! "HTTP timeout must be 1..60000 milliseconds" {}))
   (let [proxy-url (if (contains? options :proxy-url) (:proxy-url options)
                       (or (System/getenv "HTTPS_PROXY") (System/getenv "https_proxy")))
         proxy-uri (when proxy-url (URI/create proxy-url))
         _ (when (and proxy-uri (or (not= "http" (.getScheme proxy-uri)) (nil? (.getHost proxy-uri))
                                    (.getUserInfo proxy-uri)))
             (fail! "OFL proxy must be an HTTP origin without embedded credentials" {}))
         builder (-> (HttpClient/newBuilder) (.connectTimeout (Duration/ofMillis timeout-ms))
                     (.followRedirects HttpClient$Redirect/NEVER))
         _ (when proxy-uri
             (.proxy builder (ProxySelector/of (InetSocketAddress. (.getHost proxy-uri)
                                                                   (if (neg? (.getPort proxy-uri)) 80 (.getPort proxy-uri))))))
         client (.build builder)]
     (with-meta
       (fn [{:keys [method path body max-bytes] :or {method :get max-bytes (* 4 1024 1024)}}]
       (when-not (and (#{:get :post} method) (string? path)
                      (integer? max-bytes) (pos? max-bytes)
                      (if (= method :post) (= path "/api/v1/get-search-results")
                          (not= path "/api/v1/get-search-results"))
                      (or (= path "/api/v1/manufacturers") (= path "/api/v1/get-search-results")
                          (re-matches #"/api/v1/manufacturers/[a-z0-9-]+" path)
                          (re-matches #"/[a-z0-9-]+/[a-z0-9-]+\.json" path)))
         (fail! "Unsupported OFL provider request" {:method method :path path}))
       (let [builder (-> (HttpRequest/newBuilder (URI/create (str ofl-base path)))
                         (.timeout (Duration/ofMillis timeout-ms))
                         (.header "Accept" "application/json")
                         (.header "User-Agent" "Tolglow-Fixture-Library/1"))
             request (if (= method :post)
                       (-> builder (.header "Content-Type" "application/json")
                           (.POST (HttpRequest$BodyPublishers/ofString (json/write-str body))) .build)
                       (-> builder .GET .build))
             pending (.sendAsync client request (HttpResponse$BodyHandlers/ofInputStream))
             stream (atom nil) timed-out? (atom false)
             abort! #(do (.cancel pending true)
                         (when-let [^InputStream body @stream] (try (.close body) (catch Exception _))))
             scheduler (Executors/newSingleThreadScheduledExecutor
                        (reify ThreadFactory
                          (newThread [_ runnable] (doto (Thread. runnable "tolglow-fixture-deadline") (.setDaemon true)))))
             deadline (.schedule scheduler ^Runnable #(do (reset! timed-out? true) (abort!))
                                 (long timeout-ms) TimeUnit/MILLISECONDS)]
         (try
           (let [response (.get pending) body (.body response)]
             (reset! stream body)
             (when @timed-out? (abort!) (fail! "OFL request timed out" {:code :timeout}))
             (with-open [body body]
               (let [content (read-bounded body max-bytes)]
                 (when @timed-out? (fail! "OFL request timed out" {:code :timeout}))
                 {:status (.statusCode response) :headers (into {} (.map (.headers response)))
                  :body (String. ^bytes content StandardCharsets/UTF_8)})))
           (catch Exception cause
             (if @timed-out? (throw (ex-info "OFL request timed out" {:code :timeout} cause)) (throw cause)))
           (finally (.cancel deadline true) (.shutdownNow scheduler) (abort!)))))
       {:close! #(when (instance? java.lang.AutoCloseable client) (.close ^java.lang.AutoCloseable client))}))))

(defn- request-json [library provider request]
  (let [limit (get-in library [:limits :max-response-bytes])
        response (provider (assoc request :max-bytes limit))
        body (:body response)]
    (when-not (= 200 (:status response))
      (fail! "OFL request failed" {:status (:status response) :path (:path request)}))
    (when-not (and (string? body) (<= (alength (bytes body)) limit))
      (fail! "OFL response must be bounded JSON text" {:path (:path request) :limit limit}))
    {:data (json/read-str body) :text body :headers (:headers response)}))

(defn- provenance [path text opts]
  {:provider :ofl :source (str ofl-base path) :sha256 (digest text) :license ofl-license
   :retrieved-at (:retrieved-at opts) :checksum-kind :source-text})

(defn- merge-entries [library entries]
  (let [next (merge (:entries library) (into {} (map (juxt :key identity)) entries))]
    (when (> (count next) (get-in library [:limits :max-catalog-entries]))
      (fail! "Fixture catalog exceeds entry limit" {:count (count next)}))
    (-> library (assoc :entries next) (update :revision inc))))

(defn refresh-manufacturers
  ([library provider] (refresh-manufacturers library provider {}))
  ([library provider opts]
   (let [path "/api/v1/manufacturers" {:keys [data text]} (request-json library provider {:path path})]
     (when-not (and (map? data) (<= (count data) (get-in library [:limits :max-catalog-entries])))
       (fail! "Invalid OFL manufacturer index" {}))
     (doseq [[key manufacturer] data]
       (key! key false)
       (when-not (string? (get manufacturer "name")) (fail! "Manufacturer needs a name" {:key key})))
     {:library (-> library (assoc :manufacturers data :manufacturer-provenance (provenance path text opts))
                    (update :revision inc))
      :manufacturers data})))

(defn refresh-manufacturer
  ([library provider key] (refresh-manufacturer library provider key {}))
  ([library provider key opts]
   (key! key false)
   (let [path (str "/api/v1/manufacturers/" key)
         {:keys [data text]} (request-json library provider {:path path})
         _ (when-not (and (map? data) (string? (get data "name")) (vector? (get data "fixtures")))
             (fail! "Invalid OFL manufacturer catalog" {:key key}))
         provenance (provenance path text opts)
         entries (mapv (fn [fixture]
                         (let [fixture-key (str key "/" (key! (get fixture "key") false))]
                           (when-not (and (string? (get fixture "name")) (vector? (get fixture "categories")))
                             (fail! "Invalid fixture catalog entry" {:key fixture-key}))
                           {:key fixture-key :name (get fixture "name") :manufacturer-key key
                            :manufacturer (get data "name") :categories (get fixture "categories")
                            :provenance provenance})) (get data "fixtures"))]
     {:library (-> (merge-entries library entries) (assoc-in [:manufacturers key] (dissoc data "fixtures")))
      :results entries :redirects (get data "redirects" [])})))

(defn search
  "Pure offline search over fetched catalog entries. Never invokes a provider.
  Results are deterministic and contain cache status, without loading fixtures."
  [library {:keys [query manufacturer category limit] :or {query "" limit 50}}]
  (when-not (and (string? query) (<= (count query) 512) (integer? limit) (<= 1 limit 500))
    (fail! "Search needs a short query and limit 1..500" {}))
  (let [words (remove str/blank? (str/split (str/lower-case query) #"\s+"))
        matches (->> (:entries library) vals
                     (filter (fn [entry]
                               (and (or (nil? manufacturer) (= manufacturer (:manufacturer-key entry)))
                                    (or (nil? category) (some #{category} (:categories entry)))
                                    (let [text (str/lower-case (str (:key entry) " " (:manufacturer entry) " " (:name entry)))]
                                      (every? #(str/includes? text %) words)))))
                     (sort-by (juxt (comp str/lower-case :name) :key)) vec)]
    {:results (mapv #(assoc % :cached? (contains? (:documents library) (:key %))) (take limit matches))
     :total (count matches) :offline? true :catalog-count (count (:entries library))}))

(defn search-remote
  "Explicit OFL search that also seeds the offline index. Filters use OFL's
  manufacturer/category arrays; fetched search entries have unknown categories
  until a manufacturer catalog or fixture document is fetched."
  ([library provider query] (search-remote library provider query {}))
  ([library provider query opts]
   (when-not (and (string? query) (<= (count query) 512)) (fail! "Search query is too long" {}))
   (let [path "/api/v1/get-search-results"
         {:keys [data text]} (request-json library provider
                              {:method :post :path path
                               :body {:searchQuery query :manufacturersQuery (:manufacturers opts [])
                                      :categoriesQuery (:categories opts [])}})
         _ (when-not (vector? data) (fail! "Invalid OFL search response" {}))
         source (provenance path text opts)
         entries (mapv (fn [entry]
                         (let [key (key! (get entry "key") true)]
                           (when-not (string? (get entry "name")) (fail! "Search entry needs a name" {:key key}))
                           (merge (get-in library [:entries key])
                                  {:key key :name (get entry "name") :manufacturer-key (first (str/split key #"/"))
                                   :provenance source}))) data)
         next (merge-entries library entries)]
     {:library next :results (mapv #(assoc % :cached? (contains? (:documents next) (:key %))) entries)})))

(defn- cache-document [library key document]
  (let [revision (inc (:revision library))
        library (-> library (assoc :revision revision)
                    (assoc-in [:documents key] (assoc document :cache-order revision)))]
    (loop [library library]
      (let [documents (:documents library)]
        (if (and (<= (count documents) (get-in library [:limits :max-documents]))
                 (<= (reduce + 0 (map :byte-count (vals documents))) (get-in library [:limits :max-cache-bytes])))
          library
          (let [oldest (->> documents (sort-by (comp :cache-order val)) ffirst)]
            (recur (update library :documents dissoc oldest))))))))

(defn fetch-fixture
  "Explicit fetch by validated OFL key. Cached documents need no network unless
  :refresh? is true. Returns mode choices; selecting/importing one is separate."
  ([library provider key] (fetch-fixture library provider key {}))
  ([library provider key opts]
   (key! key true)
   (if-let [cached (when-not (:refresh? opts) (get-in library [:documents key]))]
     {:library library :fixture cached :cached? true}
     (let [path (str "/" key ".json") {:keys [data text]} (request-json library provider {:path path})
           _ (when-not (and (map? data) (string? (get data "name")) (vector? (get data "modes")))
               (fail! "Invalid OFL fixture document" {:key key}))
           byte-count (alength (bytes text))
           _ (when (> byte-count (get-in library [:limits :max-cache-bytes]))
               (fail! "Fixture document exceeds cache capacity" {:key key :byte-count byte-count}))
           manufacturer-key (first (str/split key #"/"))
           manufacturer (get-in library [:manufacturers manufacturer-key "name"] manufacturer-key)
           document {:key key :format :ofl :text text :byte-count byte-count :name (get data "name")
                     :manufacturer manufacturer :modes (mapv #(get % "name") (get data "modes"))
                     :provenance (provenance path text opts)}
           next (-> library
                    (merge-entries [{:key key :name (:name document) :manufacturer manufacturer
                                     :manufacturer-key manufacturer-key :categories (get data "categories" [])
                                     :provenance (:provenance document)}])
                    (cache-document key document))]
       {:library next :fixture (get-in next [:documents key]) :cached? false}))))

(defn import-fixture
  "Import an explicitly fetched OFL mode into editor data only. Publication and
  rig patching are separate shared-model commands owned by the application."
  [library key opts]
  (key! key true)
  (let [document (get-in library [:documents key])]
    (when-not document (fail! "Fetch the fixture before importing it" {:key key}))
    (let [result (fixtures/import-ofl (:text document)
                   (merge {:manufacturer (:manufacturer document)} opts
                          (select-keys (:provenance document) [:source :license])))
          provenance (merge (:provenance document) (:provenance result))]
      (cond-> (-> result (assoc :provenance provenance) (assoc-in [:candidate :provenance] provenance))
        (:definition result) (assoc-in [:definition :provenance] provenance)))))

(defn import-upload
  "Import user-supplied data without network access. OFL/QXF use text, GDTF uses
  ZIP bytes. Returns a candidate/diagnostics; never installs or activates it."
  [format data opts]
  (case format
    :ofl (fixtures/import-ofl data opts)
    :qxf (fixtures/import-qxf data opts)
    :gdtf (gdtf/import-gdtf data opts)
    (fail! "Supported import formats are OFL, QXF and bounded GDTF" {:format format})))
