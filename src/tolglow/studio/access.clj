(ns tolglow.studio.access
  "Operator admission for the entire Ring listener, including WebSocket upgrades."
  (:require [clojure.string :as str]
            [tolglow.studio.origin :as origin])
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest SecureRandom]
           [java.util Base64]
           [javax.crypto Mac]
           [javax.crypto.spec SecretKeySpec]))

(def ^:private cookie-name "tolglow-studio-session")

(def ^:private session-seconds (* 12 3600))

(defn operator-authority
  "Return the authority assigned by the trusted listener to this Ring request."
  [request]
  (::operator-authority request))

(defn- admit-operator [handler]
  ;; The listener's access policy identifies a shared operator, independently of HTTP payloads.
  (let [authority {:operator-id (str "studio/" (random-uuid))}]
    #(handler (assoc % ::operator-authority authority))))

(defn- bytes-of [s] (.getBytes ^String s StandardCharsets/UTF_8))

(defn- equal-text? [a b] (MessageDigest/isEqual (bytes-of a) (bytes-of b)))

(defn- sign [key text]
  (let [mac (doto (Mac/getInstance "HmacSHA256") (.init key))]
    (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) (.doFinal mac (bytes-of text)))))

(defn- session-cookie [request]
  (some (fn [field]
          (let [[name value] (str/split (str/trim field) #"=" 2)]
            (when (= name cookie-name) value)))
        (some-> (get-in request [:headers "cookie"]) (str/split #";"))))

(defn- valid-session? [key now value]
  (when-let [[_ stamp signature] (and (string? value) (re-matches #"([0-9]{1,12})\.([A-Za-z0-9_-]{43})" value))]
    (and (< now (Long/parseLong stamp) (inc (+ now session-seconds)))
         (equal-text? signature (sign key stamp)))))

(defn- header-authorized? [token header]
  (when (and (string? header) (< (count header) 8192))
    (let [[scheme param] (str/split header #" " 2)]
      (case (str/lower-case scheme)
        "bearer" (and param (equal-text? token param))
        "basic" (when param
                  (try (equal-text? (str "studio:" token)
                                    (String. (.decode (Base64/getDecoder) ^String param) StandardCharsets/UTF_8))
                       (catch IllegalArgumentException _ false)))
        false))))

(defn authenticated
  "Issue a signed, HTTP-only browser session after successful HTTP Basic login.
  Native clients may instead send a Bearer header on HTTP or a WS handshake.
  Keys are unique to this listener and never enter portable show state."
  [handler token]
  (when-not (and (string? token) (<= 24 (count token) 1024) (not (re-find #"[\r\n]" token)))
    (throw (ex-info "Studio operator token must contain 24 to 1024 characters" {})))
  (let [key-bytes (byte-array 32)
        _ (.nextBytes (SecureRandom.) key-bytes)
        key (SecretKeySpec. key-bytes "HmacSHA256")
        admitted (admit-operator handler)]
    (fn [request]
      (let [now (quot (System/currentTimeMillis) 1000)]
        (cond
          (valid-session? key now (session-cookie request))
          (admitted request)

          (header-authorized? token (get-in request [:headers "authorization"]))
          (let [stamp (str (+ now session-seconds))]
            (-> (admitted request)
                (assoc-in [:headers "Set-Cookie"]
                          (str cookie-name "=" stamp "." (sign key stamp)
                               "; Path=/; HttpOnly; SameSite=Strict; Max-Age=" session-seconds
                               (when (= :https (:scheme request)) "; Secure")))))

          :else {:status 401 :headers {"WWW-Authenticate" "Basic realm=\"Tolglow Studio\", charset=\"UTF-8\""
                                       "Cache-Control" "no-store" "Content-Type" "text/plain"
                                       "X-Frame-Options" "SAMEORIGIN"}
                 :body "Studio requires an operator sign-in."})))))

(defn- secret-setting [env-var]
  (when-not (and (string? env-var) (re-matches #"[A-Za-z_][A-Za-z0-9_]*" env-var))
    (throw (ex-info "Studio needs the name of a local environment variable" {})))
  (or (System/getenv env-var) (throw (ex-info "Studio credential environment variable is unset" {:variable env-var}))))

(defn jetty-options [host port tls]
  (if-not tls {:host host :port port :join? false}
    (let [{:keys [keystore password-env]} tls]
      (when-not (and (string? keystore) (not (str/blank? keystore)))
        (throw (ex-info "TLS requires a PKCS12 keystore path" {})))
      {:host host :join? false :http? false :ssl? true :ssl-port port :keystore keystore
       :keystore-type "PKCS12" :key-password (secret-setting password-env)})))

(defn listener-handler
  [handler {:keys [host auth trusted-lan? public-origin allowed-hosts tls] :or {host "127.0.0.1"}}]
  (let [local? (origin/loopback-host? host)
        public (when public-origin (origin/https-proxy public-origin))]
    (when (and public (not local?))
      (throw (ex-info "The TLS proxy adapter requires a loopback bind" {})))
    (when (and (or public (not local?)) (nil? auth) (not (true? trusted-lan?)))
      (throw (ex-info "A LAN listener requires :auth or :trusted-lan? true" {})))
    (when (and (some? auth) (not (map? auth)))
      (throw (ex-info "Studio :auth requires {:token-env ...}" {})))
    (when (and auth (not local?) (not tls))
      (throw (ex-info "Authenticated LAN access requires TLS or a loopback TLS proxy" {})))
    (let [app (if (some? auth) (authenticated handler (secret-setting (:token-env auth))) (admit-operator handler))
          app (if public (origin/wrap-proxy app public) app)]
      (origin/wrap-listener-host app host (cond-> (set allowed-hosts) public (conj (second public)))))))
