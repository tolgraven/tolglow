(ns tolglow.studio.origin
  "Browser request boundaries shared by the Ring routes and WebSocket upgrades."
  (:require [clojure.string :as str])
  (:import [java.net URI URISyntaxException]))

(defn- http-origin [text]
  (when (string? text)
    (try
      (let [u (URI. text)
            scheme (some-> (.getScheme u) str/lower-case)
            host (some-> (.getHost u) str/lower-case)
            port (.getPort u)]
        (when (and (#{"http" "https"} scheme) host
                   (nil? (.getRawUserInfo u)) (empty? (.getRawPath u))
                   (nil? (.getRawQuery u)) (nil? (.getRawFragment u))
                   (not (str/ends-with? (.getRawAuthority u) ":"))
                   (or (= port -1) (<= 1 port 65535)))
          [scheme host (if (= port -1) (if (= scheme "https") 443 80) port)]))
      (catch URISyntaxException _ nil))))

(defn same-origin?
  "Native clients can omit Origin. When a browser supplies it, require the
  request's HTTP scheme, DNS name and effective port."
  [{:keys [scheme headers]}]
  (and (not= "cross-site" (get headers "sec-fetch-site"))
       (if-let [origin (get headers "origin")]
         (let [client (http-origin origin)
               scheme (or (some-> scheme name) (first client))
               host (get headers "host")]
           (and client host (= client (http-origin (str scheme "://" host)))))
         true)))

(def ^:private loopback-names #{"localhost" "127.0.0.1" "[::1]" "[0:0:0:0:0:0:0:1]"})
(def ^:private wildcard-names #{"0.0.0.0" "[::]" "[0:0:0:0:0:0:0:0]"})

(defn- hostname [s]
  (let [name (str/lower-case s)]
    (if (and (str/includes? name ":") (not (str/starts-with? name "[")))
      (str "[" name "]") name)))

(defn wrap-listener-host
  "Bind the HTTP Host to a named listener. Wildcard listeners accept venue
  hostnames unless the operator sets :allowed-hosts. Local proxy aliases can
  also be listed explicitly. This comparison never resolves client DNS."
  [handler bind-host allowed-hosts]
  (let [bound (hostname bind-host)
        allowed (cond-> (into #{} (map hostname) allowed-hosts)
                  (loopback-names bound) (into loopback-names)
                  (not (wildcard-names bound)) (conj bound))]
    (fn [request]
      (let [authority (get-in request [:headers "host"])
            [_ host] (when authority (http-origin (str "http://" authority)))]
        (if (and host (or (empty? allowed) (contains? allowed host)))
          (handler request)
          {:status 403 :headers {"Content-Type" "text/plain"} :body "Unrecognized Studio host"})))))

(defn loopback-host? [host]
  (contains? loopback-names (hostname host)))

(defn https-proxy [uri]
  (let [parsed (http-origin uri)]
    (when-not (= "https" (first parsed))
      (throw (ex-info "The public Studio origin must be a complete HTTPS origin" {})))
    parsed))

(defn wrap-proxy
  "Only the operator-configured authority receives the TLS scheme. This
  middleware is installed exclusively behind an explicitly loopback listener."
  [handler public]
  (fn [request]
    (handler (if (= public (http-origin (str "https://" (get-in request [:headers "host"]))))
               (assoc request :scheme :https ::proxied? true)
               request))))
