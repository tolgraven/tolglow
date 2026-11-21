(ns tolglow.integrations.live-transport
  "Bounded Live bridge request/response and outbound MIDI or clip execution."
  (:require [clojure.data.json :as json]
            [afterglow.control.codecs.midi :as midi-codec]
            [tolglow.integrations.providers :as providers]
            [afterglow.sync.session :refer [fail!]])
  (:import [java.util UUID]))

(defn- request! [config cancelled? id op payload]
  (let [result (providers/http-transport!
                {:method :post :url (:endpoint config)
                 :headers {"content-type" "application/json" "accept" "application/json"}
                 :body (json/write-str {:id id :protocol-version 1 :op op
                                        :payload (cond-> payload
                                                   (:bridge-id config) (assoc :expected-bridge-id (:bridge-id config))
                                                   (:sync-session-id config) (assoc :sync-session-id (:sync-session-id config)))})}
                (fn [_] nil)
                {:timeout-ms (:timeout-ms config) :connect-timeout-ms (min 1000 (:timeout-ms config))
                 :max-response-chars 1048576 :cancelled? cancelled?})]
    (when-not (= 200 (:status result)) (fail! :live-http "Live bridge returned a failing HTTP status"))
    (let [reply (json/read-str (:body result) :key-fn keyword)]
      (when-not (and (= id (:id reply)) (= 1 (:protocol-version reply))
                     (#{"executed" "rejected" "indeterminate"} (:status reply)))
        (fail! :live-protocol "Live bridge response has invalid identity or protocol"))
      (update reply :status keyword))))

(defn transport
  "Open an inert handle. Shutdown cancels outstanding IO and releases the
  session's native parameter observers if any may still be installed."
  [config]
  {:open! (fn [_] {:cancelled? (atom false) :watching? (atom false)})
   :request! (fn [{:keys [cancelled? watching?]} id op payload]
               (let [watch? (= :watch-parameters op)
                     sources? (seq (:sources payload))]
                 ;; A lost acknowledgement can still have installed callbacks.
                 (when (and watch? sources?) (reset! watching? true))
                 (let [result (request! config cancelled? id op payload)]
                   (when (and watch? (not sources?) (= :executed (:status result))) (reset! watching? false))
                   result)))
   :close! (fn [{:keys [cancelled? watching?]}]
             (reset! cancelled? true)
             (when (compare-and-set! watching? true false)
               (let [id (str (UUID/randomUUID))]
                 (request! (update config :timeout-ms min 250) (atom false) id :watch-parameters
                           {:sources {} :generation id}))))})

(defn- midi-bytes [event]
  (midi-codec/encode
   (merge {:channel (dec (:channel event))}
          (case (:type event)
            :note-on (select-keys event [:type :note :velocity])
            :note-off (select-keys event [:type :note :velocity])
            :cc {:type :control-change :controller (:controller event) :raw-value (:value event)}
            (fail! :invalid-midi "Unsupported Live MIDI event")))))

(defn execute-plan!
  "Explicit outbound executor for a pure ableton/record-command plan. Exactly
  one route is used: :bridge writes via acknowledged LiveAPI; :midi sends bytes
  and reports delivery only. Call on an IO worker, never an effect/CAS transition.
  resources are owned handles; injected transports make this boundary testable."
  [resources route plan recording command acknowledgement]
  (let [id (str (UUID/nameUUIDFromBytes (.getBytes (pr-str [(:show-id command) (:epoch command) (:command-id command)]) "UTF-8")))]
    (case route
      :midi (let [{:keys [transport handle]} (:midi resources)]
              (when-not (:send! transport) (fail! :no-midi-output "MIDI recording output is not connected"))
              ((:send! transport) handle (mapv midi-bytes (:midi plan)))
              {:id id :status :delivered :route :midi :clip-write-confirmed? false})
      :bridge (let [{:keys [transport handle]} (:live resources)]
                (when-not (:request! transport) (fail! :not-connected "Live bridge is not connected"))
                (let [results (mapv (fn [intent]
                                      ((:request! transport) handle (if (= (:command-id intent) (:command-id command)) id
                                                                      (str (UUID/nameUUIDFromBytes
                                                                            (.getBytes (pr-str [id (:command-id intent)]) "UTF-8")))) :record-event
                                       (assoc intent :song-beat (:song-beat recording)
                                              :instance-id (or (:instance-id acknowledgement) (:instance-id command))))) (:intents plan))]
                  {:id id :route :bridge :status (if (every? #(= :executed (:status %)) results) :executed
                                                   (if (some #(= :indeterminate (:status %)) results) :indeterminate :rejected))
                   :results results :clip-write-confirmed? (boolean (and (seq results) (every? #(= :executed (:status %)) results)))}))
      (fail! :invalid-route "Unknown Live recording route"))))
