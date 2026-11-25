(ns tolglow.integrations.mpv-observations
  "Read-only mpv JSON IPC observations for one trusted local player session."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [tolglow.integrations.reference-position :as position])
  (:import [java.nio.file InvalidPathException Paths]))

;; mpv properties and lifecycle events are documented at https://mpv.io/manual/stable/
(def ^:private watched
  {1 "path" 2 "pause" 3 "speed" 4 "seeking" 5 "paused-for-cache" 6 "eof-reached"
   7 "time-pos" 8 "ab-loop-a" 9 "ab-loop-b" 10 "remaining-ab-loops" 11 "stream-open-filename"})

(def ^:private sample-properties
  [[:path "path"] [:stream-path "stream-open-filename"] [:pause "pause"] [:speed "speed"] [:seeking "seeking"]
   [:cache-paused "paused-for-cache"] [:eof "eof-reached"]
   [:ab-a "ab-loop-a"] [:ab-b "ab-loop-b"]
   [:ab-loops "remaining-ab-loops"] [:file-loops "remaining-file-loops"]
   [:position "time-pos"] [:path-after "path"] [:stream-path-after "stream-open-filename"] [:pause-after "pause"]
   [:speed-after "speed"] [:seeking-after "seeking"]])

(def ^:private max-line 16384)
(def ^:private max-chunk (* 16 max-line))
(defn- finite? [x] (and (number? x) (Double/isFinite (double x))))
(defn- identity? [x] (or (keyword? x) (and (string? x) (not (str/blank? x))) (integer? x) (uuid? x)))

(defn- local-path [value]
  (when (and (string? value) (not (str/includes? value "://"))
             (not (str/starts-with? value "//")) (not (str/starts-with? value "\\\\")))
    (try
      (let [path (Paths/get value (make-array String 0))]
        (when (and (.isAbsolute path) (not-any? #(= ".." (str %)) (iterator-seq (.iterator path))))
          (str (.normalize path))))
      (catch InvalidPathException _ nil))))

(defn- catalog-entries [catalog]
  (when-not (map? catalog)
    (throw (ex-info "mpv requires an explicit local reference catalog" {})))
  (reduce-kv (fn [entries path {:keys [reference duration-ms]}]
               (let [path (local-path path)]
                 (when-not (and path (not (contains? entries path))
                                (identity? (:id reference)) (identity? (:version reference))
                                (finite? duration-ms) (pos? duration-ms))
                   (throw (ex-info "Invalid or duplicate local reference catalog entry" {})))
                 (assoc entries path {:reference (select-keys reference [:id :version])
                                      :duration-ms (double duration-ms)})))
             {} catalog))

(defn- line [message] (str (json/write-str message) "\n"))

(defn connect
  "Return {:state :lines} for a new player connection. Send the subscription
  lines on the same ordered connection used for samples and notifications.
  Use a new :session-id on reconnect. Catalog entries map absolute local path
  strings to {:reference {:id :version} :duration-ms} from trusted track profiles.
  The reported file and opened stream must agree with the catalog path.
  Refresh the catalog if the file at a registered path is replaced."
  ([provenance catalog] (connect provenance catalog {}))
  ([provenance catalog {:keys [max-rtt-ms alignment-options]
                        :or {max-rtt-ms 50.0 alignment-options {}}}]
   (when-not (and (finite? max-rtt-ms) (pos? max-rtt-ms))
     (throw (ex-info "mpv sample timeout must be positive" {})))
   {:state {:catalog (catalog-entries catalog) :alignment (position/new-state provenance alignment-options)
            :connected? true :phase :settled :max-rtt-ms (double max-rtt-ms)
            :next-request-id (inc (reduce max (keys watched)))
            :subscribing (set (keys watched)) :fragment ""}
    :lines (mapv (fn [[id property]]
                  (line {:command ["observe_property" id property] :request_id id}))
                (sort-by key watched))}))

(defn- observation [state now-ms observed-at-ms type data]
  (let [{:keys [alignment]} state]
    (assoc state :alignment
           (position/advance alignment now-ms
             (merge {:type type :observed-at-ms observed-at-ms :occurrence (:occurrence alignment)
                     :provenance (:provenance alignment)} data)))))

(defn- fence [state now-ms reason]
  (-> (observation state now-ms now-ms :unavailable {})
      (dissoc :pending)
      (assoc :reason reason)))

(defn- tick [state now-ms]
  (when-not (and (finite? now-ms) (<= 0 now-ms) (<= (get state :now-ms now-ms) now-ms))
    (throw (ex-info "mpv observations require monotonic host time in milliseconds" {})))
  (let [state (-> state (assoc :now-ms now-ms) (update :alignment position/advance now-ms))]
    (if-let [sent (get-in state [:pending :sent-at-ms])]
      (if (> (- now-ms sent) (:max-rtt-ms state)) (fence state now-ms :sample-timeout) state)
      state)))

(defn request-sample
  "Return {:state :lines} for at most one outstanding bounded sample group.
  The caller supplies actual send time on the same monotonic clock as receive.
  Run periodically to expire unanswered requests and age the alignment."
  [state now-ms]
  (let [state (tick state now-ms)]
    (if (and (:connected? state) (= :settled (:phase state)) (empty? (:subscribing state))
             (nil? (:pending state)))
      (let [id (:next-request-id state)
            commands (mapv (fn [offset [key property]] [(+ id offset) key property])
                           (range) sample-properties)]
        {:state (assoc state :pending {:sent-at-ms now-ms
                                      :requests (into {} (map (juxt first second)) commands)}
                             :next-request-id (+ id (count commands)))
         :lines (mapv (fn [[request-id _ property]] (line {:command ["get_property" property]
                                                           :request_id request-id})) commands)})
      {:state state :lines []})))

(defn- disconnect-at [state now-ms reason]
  (-> (fence state now-ms reason) (assoc :connected? false :fragment "")))

(defn disconnect
  "Revoke the session at transport EOF. A fresh connection needs a new state."
  [state now-ms]
  (let [state (tick state now-ms)]
    (disconnect-at state now-ms (if (seq (:fragment state)) :truncated-line :disconnected))))

(defn- file-transition [state now-ms]
  (assoc (fence state now-ms :file-change) :phase :loading :transition :file))

(defn- seek-transition [state now-ms]
  (cond-> (fence state now-ms :seek)
    (not= :file (:transition state)) (assoc :phase :seeking :transition :seek)))

(defn- position-ms [value] (when (and (finite? value) (<= 0 value) (finite? (* 1000.0 value))) (* 1000.0 value)))

(defn- player-sample [{:keys [path path-after stream-path stream-path-after pause pause-after speed speed-after
                              seeking seeking-after cache-paused eof position ab-a ab-b ab-loops file-loops]}]
  (let [path (local-path path) cursor (position-ms position)]
    (when (and path (= path (local-path path-after) (local-path stream-path) (local-path stream-path-after)) cursor
               (boolean? pause) (= pause pause-after) (finite? speed) (pos? speed) (finite? speed-after) (== speed speed-after)
               (boolean? seeking) (= seeking seeking-after) (boolean? cache-paused) (boolean? eof))
      {:path path :position-ms cursor :rate (double speed)
       :playing? (not (or pause seeking cache-paused eof))
       :ab-a-ms (position-ms ab-a) :ab-b-ms (position-ms ab-b) :ab-loops ab-loops :file-loops file-loops})))

(defn- loop-count-decreased? [before after]
  (and (integer? before) (integer? after) (<= 0 after) (< after before)))

(defn- occurrence-transition [previous sample marker]
  (when previous
    (let [rewound? (< (+ (:position-ms sample) 75.0) (:position-ms previous))]
      (cond
        (not= (:path previous) (:path sample)) :cut
        (= marker :file) :loop
        (and rewound? (or (loop-count-decreased? (:file-loops previous) (:file-loops sample))
                          (loop-count-decreased? (:ab-loops previous) (:ab-loops sample)))) :loop
        (or (= marker :seek) rewound?) :seek))))

(defn- playback-bound [sample duration-ms]
  (let [{:keys [ab-a-ms ab-b-ms ab-loops position-ms]} sample]
    (if (and ab-a-ms ab-b-ms (not= ab-a-ms ab-b-ms) (not= 0 ab-loops)
             (< position-ms (max ab-a-ms ab-b-ms)))
      (min duration-ms (max ab-a-ms ab-b-ms))
      duration-ms)))

(defn- apply-sample [state now-ms {:keys [sent-at-ms values]}]
  (let [state (dissoc state :pending)
        elapsed (- now-ms sent-at-ms)]
    (if-let [{:keys [path position-ms rate playing?] :as sample} (player-sample values)]
      (let [{:keys [reference duration-ms]} (get-in state [:catalog path])
            transition (occurrence-transition (:last-player state) sample (:transition state))
            controls (into {} (map (fn [[key property]] [property (get values key)])) sample-properties)
            state (-> state (assoc :last-player sample :controls controls) (dissoc :transition))]
        (cond
          ;; Confirm the occurrence before a separate sample can reacquire it.
          transition (observation (assoc state :reason transition) now-ms now-ms transition {})
          (not reference) (fence state now-ms :unknown-reference)
          (not playing?) (fence state now-ms :not-playing)
          :else
          ;; IPC supplies neither a sample timestamp nor the exact sampling phase.
          ;; Age from send time, project no further than the measured position on
          ;; receipt, and cap prediction early by the same uncertainty interval.
          (let [uncertainty (* rate elapsed)
                cursor (- position-ms uncertainty)
                until (- (playback-bound sample duration-ms) uncertainty)]
            (if (and (pos? elapsed) (<= 0 cursor) (< position-ms until))
              (observation (dissoc state :reason) now-ms sent-at-ms :match
                           {:match {:reference reference :position-ms cursor :rate rate :score 1.0
                                    :until-position-ms until}})
              (fence state now-ms :unusable-sample)))))
      (fence state now-ms :incoherent-sample))))

(defn- property-change [state now-ms message]
  (let [id (get message "id") name (get message "name") value (get message "data")]
    (if-not (= name (get watched id ::unknown))
      state
      (cond
        (= name "time-pos") (if (some? value) state (fence state now-ms :position-unavailable))
        (= value (get-in state [:controls name] ::unknown)) state
        :else
        (cond-> (-> (fence state now-ms :properties-changed) (assoc-in [:controls name] value))
          (and (= name "path") (:last-player state)
               (not= (local-path value) (get-in state [:last-player :path])))
          (assoc :transition :file))))))

(defn- response [state now-ms message]
  (let [id (get message "request_id") success? (= "success" (get message "error"))]
    (cond
      (contains? (:subscribing state) id)
      (if success? (update state :subscribing disj id) (disconnect-at state now-ms :subscription-failed))

      :else
      (if-let [key (get-in state [:pending :requests id])]
        (let [pending (-> (:pending state) (update :requests dissoc id)
                          (assoc-in [:values key] (when success? (get message "data"))))]
          (if (empty? (:requests pending)) (apply-sample state now-ms pending) (assoc state :pending pending)))
        state))))

(defn- handle-message [state now-ms message]
  (case (get message "event")
    "start-file" (file-transition state now-ms)
    "end-file" (file-transition state now-ms)
    "file-loaded" (file-transition state now-ms)
    "seek" (seek-transition state now-ms)
    "playback-restart" (assoc (fence state now-ms :playback-restart) :phase :settled)
    "shutdown" (disconnect-at state now-ms :shutdown)
    "property-change" (property-change state now-ms message)
    nil (response state now-ms message)
    state))

(defn- read-line-message [line]
  (try
    (let [message (json/read-str line :extra-data-fn json/on-extra-throw)]
      (when (map? message) message))
    (catch Exception _ nil)))

(defn receive
  "Consume an ordered UTF-8 transport's decoded text chunk at receipt time.
  Complete newline-delimited records update :alignment for reference-position
  consumers. Same-file reloads create repeat occurrences. An in-file rewind
  without loop evidence creates a seek occurrence. Incomplete final records
  invalidate the session on disconnect."
  [state now-ms chunk]
  (let [state (tick state now-ms)]
    (cond
      (not (:connected? state)) state
      (not (and (string? chunk) (<= (count chunk) max-chunk))) (disconnect-at state now-ms :oversized-chunk)
      :else
      (let [records (str/split (str (:fragment state) chunk) #"\n" -1)
            tail (peek records)]
        (if (> (count tail) max-line)
          (disconnect-at state now-ms :oversized-line)
          (reduce (fn [state record]
                    (cond
                      (not (:connected? state)) (reduced state)
                      (> (count record) max-line) (reduced (disconnect-at state now-ms :oversized-line))
                      :else (if-let [message (read-line-message record)]
                              (handle-message state now-ms message)
                              (reduced (disconnect-at state now-ms :malformed-json)))))
                  (assoc state :fragment tail) (pop records)))))))
