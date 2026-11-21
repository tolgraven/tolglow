(ns tolglow.studio.http-transit-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.data.json :as json]
            [thi.ng.color.core :as color]
            [tolglow.transit :as transit]
            [tolglow.studio.web :as web])
  (:import [java.io ByteArrayInputStream]))

(def mime "application/transit+json")

(defn- request [handler method headers data]
  (handler {:uri "/api/command" :request-method method
            :headers (merge {"host" "localhost" "accept" mime "content-type" mime} headers)
            :body (ByteArrayInputStream. (.getBytes (transit/encode data) "UTF-8"))}))

(deftest typed-commands-and-acknowledgements-share-the-same-codec
  (let [handler (web/app {:heartbeat! (constantly nil)
                          :submit! (fn [_ payload] (doto (promise) (deliver {:status :accepted :received payload})))})
        color (color/hsla 0.7 0.5 0.25 0.4)
        payload {:op :param/set :id "looks/blue" :param :tint :value color :tags #{:a} :division 1/3}
        response (request handler :post {} payload)
        received (:received (transit/decode (:body response)))
        form (request handler :post {"x-tolglow-form" "studio-v1"} (assoc payload :op "param/set"))]
    (is (= 200 (:status response)))
    (is (= "application/transit+json; charset=utf-8" (get-in response [:headers "Content-Type"])))
    (is (= payload received))
    (is (= (type color) (type (:value received))))
    (is (= [:param/set :looks/blue color]
           ((juxt :op :id :value) (:received (transit/decode (:body form))))))
    (is (= "accepted" (:status (json/read-str (:body (request handler :post {"accept" "application/json"} payload))
                                               :key-fn keyword))))
    (let [forbidden (request handler :post {"origin" "https://elsewhere.test"} payload)]
      (is (= 403 (:status forbidden)))
      (is (string? (:message (transit/decode (:body forbidden))))))))
