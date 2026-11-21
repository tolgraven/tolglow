(ns tolglow.integrations.live-sync-config
  "Portable Live bridge settings, MIDI mappings and recording destination."
  (:require [afterglow.sync.session :refer [fail!]]
            [afterglow.time :as musical-time]
            [tolglow.integrations.live-parameters :as live-params])
  (:import [java.net URI]))

(def defaults
  {:enabled? false :source-id :ableton-live :bridge-id "tolglow-m4l" :endpoint "http://127.0.0.1:8750/live"
   :poll-ms 500 :timeout-ms 2000 :record-route :bridge :mode :lighting :channel 1
   :mappings {} :parameters {} :pairing nil :automap? true :observed-parameters {}})

(defn- bounded-int? [value low high] (and (integer? value) (<= low value high)))
(defn- text? [value limit] (and (string? value) (<= 1 (count value) limit)))
(defn- identifier? [value] (or (keyword? value) (text? value 128)))

(defn- check! [condition message]
  (when-not condition (fail! :invalid-config message)))

(defn- mapping-key [key]
  (let [key (if (keyword? key) (name key) key)
        key (if (and (string? key) (re-matches #"\d{1,3}" key)) (Long/parseLong key) key)]
    (when-not (and (integer? key) (<= 0 key 127)) (fail! :invalid-config "MIDI mapping keys must be 0..127")) key))
(defn- live-pairing? [pairing]
  (or (nil? pairing)
      (and (map? pairing)
           (every? #{:track-index :clip-slot-index :arrangement-index :rack-device-index :expected-track-name :expected-clip-name} (keys pairing))
           (= 1 (count (select-keys pairing [:clip-slot-index :arrangement-index])))
           (every? #(and (integer? %) (<= 0 % 9999))
                   [(:track-index pairing) (or (:clip-slot-index pairing) (:arrangement-index pairing))])
           (or (nil? (:rack-device-index pairing)) (and (integer? (:rack-device-index pairing)) (<= 0 (:rack-device-index pairing) 16383)))
           (every? #(or (nil? %) (and (string? %) (<= 1 (count %) 512)))
                   ((juxt :expected-track-name :expected-clip-name) pairing)))))
(defn- mappings [values kind]
  (when-not (and (map? values) (<= (count values) 128)) (fail! :invalid-config "Mappings must be bounded maps"))
  (into {} (map (fn [[key value]]
                 (when-not (and (map? value) (identifier? (:cue-id value))
                                (boolean? (:disabled? value false))
                                (if (= kind :notes)
                                  (and (#{:persistent :held :timed :one-shot} (get value :mode :persistent))
                                       (or (nil? (:duration-ms value)) (and (musical-time/finite-number? (:duration-ms value)) (pos? (:duration-ms value)))))
                                  (and (identifier? (:param value)) (not (contains? value :instance-id))
                                       (or (nil? (:lane-note value)) (and (integer? (:lane-note value)) (<= 0 (:lane-note value) 127)))
                                       (#{:number :integer :boolean :enum} (:type value :number))
                                       (or (not= :enum (:type value)) (and (vector? (:values value)) (<= 1 (count (:values value)) 127)))
                                       (let [[low high] (get value :bounds [0 1])]
                                         (and (every? musical-time/finite-number? [low high]) (< low high))))))
                   (fail! :invalid-config "Invalid logical cue/parameter mapping"))
                 (when (:duration value)
                   (when (or (:duration-ms value) (seq (musical-time/validate-duration (:duration value)))
                             (not (pos? (val (first (:duration value))))))
                     (fail! :invalid-config "Mapping duration must use one positive musical/explicit time unit")))
                 [(mapping-key key) value]) values)))

(defn- check-endpoint! [endpoint]
  (let [uri (try (URI. endpoint) (catch Exception _ nil))]
    (check! (and uri (= "http" (.getScheme uri)) (#{"127.0.0.1" "localhost" "[::1]" "::1"} (.getHost uri))
                 (nil? (.getUserInfo uri)) (nil? (.getFragment uri)))
            "Live bridge endpoint must be an explicit loopback HTTP URL")))

(defn normalize [settings]
  (let [live (-> settings
                 (update :record-route keyword)
                 (update :mode keyword)
                 (update :mappings #(mappings (into {} (map (fn [[k v]] [k (cond-> v (string? (:mode v)) (update :mode keyword))]) %)) :notes))
                 (update :parameters mappings :parameters)
                 (update :observed-parameters live-params/sources))
        {:keys [automap? mode record-route bridge-id channel poll-ms timeout-ms pairing endpoint]} live
        notes (concat (keys (:mappings live)) (keep :lane-note (vals (:parameters live))))]
    (check! (and (boolean? automap?) (#{:lighting :live} mode) (#{:bridge :midi} record-route)
                 (text? bridge-id 128) (bounded-int? channel 1 16)
                 (bounded-int? poll-ms 100 10000) (bounded-int? timeout-ms 50 10000)
                 (= (count notes) (count (set notes))) (live-pairing? pairing))
            "Invalid Live pairing, recording route or mapping")
    (check-endpoint! endpoint)
    live))

