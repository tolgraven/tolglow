(ns tolglow.studio.access-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [tolglow.show :as show]
            [tolglow.studio :as studio]
            [tolglow.studio.access :as access])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers WebSocket$Listener]
           [java.util Base64]
           [java.util.concurrent ExecutionException TimeUnit]))

(def token "test-only-studio-admission-credential")
(def basic (str "Basic " (.encodeToString (Base64/getEncoder) (.getBytes (str "studio:" token) "UTF-8"))))

(deftest lan-listeners-require-an-explicit-operator-policy
  (let [handler (constantly {:status 200})]
    (is (ifn? (access/listener-handler handler {:host "127.0.0.1"})))
    (doseq [options [{:host "0.0.0.0"} {:host "192.0.2.1"} {:host "0.0.0.0" :auth {:token-env "TEST_TOKEN"}}
                     {:public-origin "https://show.local"} {:host "0.0.0.0" :public-origin "https://show.local"}]]
      (is (thrown? clojure.lang.ExceptionInfo (access/listener-handler handler options))))
    (is (ifn? (access/listener-handler handler {:host "0.0.0.0" :trusted-lan? true})))
    (let [app (with-redefs-fn {#'access/secret-setting (constantly token)}
                #(access/listener-handler (fn [r] {:status 200 :body (:scheme r)})
                                         {:auth {:token-env "TEST_TOKEN"} :public-origin "https://show.local"}))
          res (app {:scheme :http :headers {"host" "show.local" "authorization" basic}})]
      (is (= :https (:body res)))
      (is (str/includes? (get-in res [:headers "Set-Cookie"]) "; Secure"))
      (is (= 403 (:status (app {:headers {"host" "other.local" "authorization" basic}})))))))

(deftest one-sign-in-admits-ring-requests-and-both-websockets
  (let [runtime (show/create-runtime)
        {:keys [port stop!]} (with-redefs-fn {#'access/secret-setting (constantly token)}
                               #(studio/listen! runtime {:port 0 :auth {:token-env "TEST_TOKEN"}}))
        client (HttpClient/newHttpClient)
        root (str "127.0.0.1:" port)
        request! (fn [method path headers]
                   (let [req (HttpRequest/newBuilder (URI/create (str "http://" root path)))]
                     (doseq [[k v] headers] (.header req k v))
                     (when (= method :post) (.POST req (HttpRequest$BodyPublishers/ofString "{}")))
                     (.send client (.build req) (HttpResponse$BodyHandlers/discarding))))
        connect! (fn [path cookie]
                   (let [ws (.newWebSocketBuilder client)]
                     (when cookie (.header ws "Cookie" cookie))
                     (.get (.buildAsync ws (URI/create (str "ws://" root path)) (reify WebSocket$Listener)) 3 TimeUnit/SECONDS)))]
    (try
      (doseq [[method path] [[:get "/"] [:get "/api/state"] [:get "/api/chat/history"]
                             [:post "/api/command"] [:post "/api/repl/eval"] [:post "/api/workspaces/write"]]]
        (is (= 401 (.statusCode (request! method path {})))))
      (let [signed (request! :get "/" {"Authorization" basic})
            cookie-header (.get (.firstValue (.headers signed) "Set-Cookie"))
            cookie (first (str/split cookie-header #";"))]
        (is (= 200 (.statusCode signed)))
        (is (str/includes? cookie-header "HttpOnly"))
        (is (not (str/includes? cookie token)))
        (is (= 200 (.statusCode (request! :get "/api/state" {"Cookie" cookie}))))
        (is (= 200 (.statusCode (request! :get "/api/state" {"Authorization" (str "Bearer " token)}))))
        (is (= 401 (.statusCode (request! :get "/api/state" {"Cookie" (str cookie "x")}))))
        (doseq [path ["/api/state/stream?session=test" "/api/debug/events?session=test"]]
          (is (thrown? ExecutionException (connect! path nil)))
          (let [socket (connect! path cookie)] (is (some? socket)) (.abort socket))))
      (finally (stop!) (show/stop! runtime)))))
